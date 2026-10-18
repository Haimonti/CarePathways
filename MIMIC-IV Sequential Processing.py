# Imports

from __future__ import annotations

import argparse
import gc
import hashlib
import json
import logging
import math
import re
import zipfile
from collections import defaultdict
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable

import numpy as np
import pandas as pd
import scipy.sparse as sparse

# Configuration

LOGGER = logging.getLogger("mimic_iv_sequence_processing")
SPLITS = ("train", "val", "test")
APPROACHES = ("aprA", "aprB", "aprC")
TEXT_SECTIONS = ("cc_adm_text", "hpi_adm_text", "bhc_hist_text")
ROLE_IDS = {"current": 0, "hist": 1, "short_hist": 2, "prol_hist": 3}
FEATURE_GROUPS = (
    "physical_characteristics",
    "diagnoses_icd10_cm",
    "procedures_icd10_pcs",
    "medication_frequency",
    "chief_complaint",
    "history_of_present_illness",
    "brief_hospital_course",
)


@dataclass(frozen=True)
class Config:

    root: Path
    output: Path
    raw: Path
    tables: Path
    roles: Path
    code_maps: Path
    medication_map: Path
    rxnav_cache: Path
    encoder: Path
    pretraining_roles: Path
    rebuild_split: bool = False
    current_text_policy: str = "retrospective_proxy"
    window: int = 16
    seed: int = 42
    chunk_size: int = 200_000
    note_chunk_size: int = 20_000
    minimum_category_count: int = 50
    minimum_code_count: int = 20
    minimum_medication_count: int = 50
    text_window: int = 8192
    text_overlap: int = 1024
    text_token_budget: int = 32768
    text_shard_size: int = 4096
    device: str = "auto"


# File Utilities


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def json_read(path: Path) -> dict:
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def json_write(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    with temporary.open("w", encoding="utf-8") as handle:
        json.dump(value, handle, indent=2, default=str, allow_nan=False)
    temporary.replace(path)


def parquet_write(path: Path, frame: pd.DataFrame) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    frame.to_parquet(temporary, index=False)
    temporary.replace(path)


def digest_file(path: Path) -> str:
    result = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(8 << 20), b""):
            result.update(block)
    return result.hexdigest()


def digest_frame(frame: pd.DataFrame) -> str:
    values = pd.util.hash_pandas_object(frame, index=False).to_numpy()
    return hashlib.sha256(values.tobytes()).hexdigest()


def source_signature(path: Path) -> dict:
    stat = path.stat()
    return {
        "path": str(path.resolve()),
        "size": stat.st_size,
        "mtime_ns": stat.st_mtime_ns,
    }


def locate_table(config: Config, name: str) -> Path:
    candidates = [
        base / filename
        for base in (
            config.tables,
            config.tables / "hosp",
            config.tables / "note",
        )
        for filename in (name + ".csv.gz", name + ".csv")
    ]
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    raise FileNotFoundError(
        f"Missing {name}.csv[.gz]; searched {config.tables} and its hosp/note directories."
    )


def read_subset(
    path: Path,
    columns: Iterable[str],
    ids: set[int],
    chunk_size: int,
    key: str = "hadm_id",
    dtype: dict | None = None,
) -> pd.DataFrame:
    parts = []
    for chunk in pd.read_csv(
        path, usecols=list(columns), chunksize=chunk_size, dtype=dtype
    ):
        selected = chunk.loc[chunk[key].isin(ids)].copy()
        if not selected.empty:
            parts.append(selected)
    return (
        pd.concat(parts, ignore_index=True)
        if parts
        else pd.DataFrame(columns=list(columns))
    )


# Cohort Construction and Patient Splitting


def assign_patient_roles(encounters: pd.DataFrame, seed: int) -> pd.Series:
    first = (
        encounters.sort_values(["subject_id", "admittime", "hadm_id"])
        .groupby("subject_id")
        .head(1)
    )
    first = first.set_index("subject_id").sort_index()
    patients = first.index.to_numpy(np.int64)
    drawn_test = np.random.default_rng(seed).permutation(patients)[
        : round(0.20 * len(patients))
    ]
    original = pd.Series("train", index=patients)
    original.loc[drawn_test] = "test"
    pool = np.sort(original.index[original.eq("train")].to_numpy())
    drawn_val = np.random.default_rng(seed + 1).permutation(pool)[
        : round(0.10 * len(pool))
    ]
    original.loc[drawn_val] = "val"
    counts = pd.crosstab(encounters["subject_id"], encounters["era"]).reindex(
        index=patients, columns=["train", "straddle", "test", "late"], fill_value=0
    )
    has_target = (original.isin(["train", "val"]) & counts["train"].gt(0)) | (
        original.eq("test") & (counts["test"] + counts["late"]).gt(0)
    )
    orphan = counts.loc[~has_target]
    recovered = pd.Series(
        np.where(
            orphan["train"].gt(0),
            "train",
            np.where((orphan["test"] + orphan["late"]).gt(0), "test", "drop"),
        ),
        index=orphan.index,
    )
    recovered = recovered.loc[recovered.ne("drop")]
    pool = np.sort(recovered.index[recovered.eq("train")].to_numpy())
    recovered_val = np.random.default_rng(seed + 2).permutation(pool)[
        : round(0.10 * len(pool))
    ]
    recovered.loc[recovered_val] = "val"
    original.loc[recovered.index] = recovered
    group_high = first["anchor_year_group"].str.extract(r"(\d{4})\s*$")[0].astype(int)
    early = first["real_hi"].le(2016) | (
        first["era"].eq("straddle") & group_high.le(2016)
    )
    roles = pd.Series(np.where(early, "train", "test"), index=patients)
    roles.loc[early & original.eq("val")] = "val"
    pool = np.sort(original.index[early & original.eq("test")].to_numpy())
    extra_val = np.random.default_rng(seed + 3).permutation(pool)[
        : round(0.10 * len(pool))
    ]
    roles.loc[extra_val] = "val"
    return roles


def prepare_cohort(config: Config) -> tuple[pd.DataFrame, dict]:
    if config.raw.is_file():
        columns = ["subject_id", "hadm_id", "admittime", "LOS"] + list(
            PHYSICAL_CATEGORIES
        )
        encounters = pd.read_csv(config.raw, usecols=columns)
    else:
        columns = [
            "subject_id",
            "hadm_id",
            "admittime",
            "dischtime",
            "race",
            "marital_status",
            "admission_type",
        ]
        encounters = pd.read_csv(locate_table(config, "admissions"), usecols=columns)
        admissions = pd.to_datetime(encounters["admittime"], errors="coerce")
        discharges = pd.to_datetime(encounters.pop("dischtime"), errors="coerce")
        encounters["LOS"] = (discharges - admissions).dt.total_seconds() / 86400.0
        demographics = pd.read_csv(
            locate_table(config, "patients"), usecols=["subject_id", "gender"]
        )
        encounters = encounters.merge(
            demographics, on="subject_id", how="left", validate="many_to_one"
        )
    require(
        encounters["hadm_id"].notna().all() and encounters["hadm_id"].is_unique,
        "The source must have one nonmissing hadm_id per encounter.",
    )
    require(encounters["subject_id"].notna().all(), "Missing subject_id in the source.")
    encounters[["subject_id", "hadm_id"]] = encounters[
        ["subject_id", "hadm_id"]
    ].astype(np.int64)
    encounters["admittime"] = pd.to_datetime(
        encounters["admittime"], errors="coerce"
    ).astype("datetime64[ns]")
    encounters["los_days"] = pd.to_numeric(encounters.pop("LOS"), errors="coerce")
    require(encounters["admittime"].notna().all(), "Unparseable admission timestamps.")
    valid = np.isfinite(encounters["los_days"]) & encounters["los_days"].gt(0)
    parquet_write(
        config.output / "invalid_los.parquet",
        encounters.loc[~valid, ["subject_id", "hadm_id", "admittime", "los_days"]],
    )
    encounters = encounters.loc[valid].copy()
    patients = pd.read_csv(
        locate_table(config, "patients"),
        usecols=["subject_id", "anchor_age", "anchor_year", "anchor_year_group"],
    )
    require(patients["subject_id"].is_unique, "Duplicate subject_id in patients.")
    encounters = encounters.merge(
        patients, on="subject_id", how="left", validate="many_to_one"
    )
    require(
        encounters[["anchor_age", "anchor_year", "anchor_year_group"]]
        .notna()
        .all()
        .all(),
        "Patient anchor information is incomplete.",
    )
    years = (
        encounters["anchor_year_group"]
        .astype(str)
        .str.extract(r"^(\d{4})\s*-\s*(\d{4})$")
    )
    require(years.notna().all().all(), "Unexpected anchor_year_group format.")
    offset = encounters["admittime"].dt.year - encounters["anchor_year"]
    encounters["real_lo"] = years[0].astype(int) + offset
    encounters["real_hi"] = years[1].astype(int) + offset
    encounters["era"] = np.select(
        [
            encounters["real_hi"].le(2016),
            encounters["real_lo"].ge(2017) & encounters["real_hi"].le(2019),
            encounters["real_lo"].ge(2020),
        ],
        ["train", "test", "late"],
        default="straddle",
    )
    encounters = encounters.sort_values(
        ["subject_id", "admittime", "hadm_id"]
    ).reset_index(drop=True)
    if config.rebuild_split or not config.roles.is_file():
        roles = assign_patient_roles(encounters, config.seed)
        encounters["role"] = encounters["subject_id"].map(roles)
    else:
        saved = pd.read_parquet(config.roles)
        require(saved["hadm_id"].is_unique, "Duplicate encounters in the saved split.")
        require(
            set(saved["hadm_id"]) == set(encounters["hadm_id"]),
            "Saved split and valid source encounters differ.",
        )
        saved = saved.set_index("hadm_id").reindex(encounters["hadm_id"])
        require(
            np.array_equal(
                saved["subject_id"].to_numpy(), encounters["subject_id"].to_numpy()
            ),
            "Subject identities disagree with the saved split.",
        )
        require(
            np.array_equal(
                pd.to_datetime(saved["admittime"]).to_numpy(),
                encounters["admittime"].to_numpy(),
            ),
            "Admission timestamps disagree with the saved split.",
        )
        saved_los = saved["los"] if "los" in saved else saved["los_days"]
        require(
            np.allclose(saved_los, encounters["los_days"], rtol=0, atol=1 / 86400),
            "LOS labels disagree with the saved split.",
        )
        encounters["role"] = saved["role"].to_numpy()
    require(
        encounters["role"].isin(SPLITS).all(),
        "Encounter roles must contain only train, val, and test.",
    )
    require(
        encounters.groupby("subject_id")["role"].nunique().eq(1).all(),
        "Patients cross data splits.",
    )
    require(
        set(encounters["role"]) == set(SPLITS), "All three patient splits are required."
    )
    first = encounters.groupby("subject_id", sort=False).head(1)
    upper = first["anchor_year_group"].str[-4:].astype(int)
    early = first["real_hi"].le(2016) | (first["era"].eq("straddle") & upper.le(2016))
    require(
        np.array_equal(early.to_numpy(), first["role"].ne("test").to_numpy()),
        "Saved roles do not match the final patient-entry temporal split.",
    )
    if config.pretraining_roles.is_file():
        old = pd.read_parquet(config.pretraining_roles, columns=["subject_id", "role"])
        train_patients = set(
            encounters.loc[encounters["role"].eq("train"), "subject_id"]
        )
        require(
            set(old.loc[old["role"].eq("train"), "subject_id"]) <= train_patients,
            "The adapted text encoder's training patients would cross the new split.",
        )
    train_los = encounters.loc[encounters["role"].eq("train"), "los_days"].to_numpy(
        np.float64
    )
    require(len(train_los) > 1, "At least two train encounters are required.")
    threshold = float(train_los.mean() + 2 * train_los.std(ddof=1))
    encounters["prolonged"] = encounters["los_days"].gt(threshold)
    encounters["z"] = np.log1p(encounters["los_days"])
    encounters["dischtime"] = encounters["admittime"] + pd.to_timedelta(
        encounters["los_days"], unit="D"
    )
    manifest = {
        "split_signature": digest_frame(
            encounters[["hadm_id", "role"]].sort_values("hadm_id")
        ),
        "split_rule": "Patient-disjoint first-entry temporal split; later stays remain with their patient.",
        "T_plos_days": threshold,
        "threshold_rule": "Train LOS mean + 2 sample standard deviations.",
        "counts": {split: int(encounters["role"].eq(split).sum()) for split in SPLITS},
        "target_transform": "z = log1p(los_days)",
        "invalid_los_excluded": int((~valid).sum()),
    }
    parquet_write(
        config.output / "encounters.parquet",
        encounters[
            [
                "subject_id",
                "hadm_id",
                "admittime",
                "dischtime",
                "role",
                "era",
                "los_days",
                "z",
                "prolonged",
            ]
        ],
    )
    json_write(config.output / "cohort_manifest.json", manifest)
    return encounters, manifest


# Pre-Processing of Physical Characteristics

PHYSICAL_CATEGORIES = ("gender", "race", "marital_status", "admission_type")
PHYSICAL_NUMERIC = (
    "age",
    "height_cm",
    "weight_kg",
    "bmi",
    "sbp",
    "dbp",
    "height_days",
    "weight_days",
    "bmi_days",
    "bp_days",
)
PHYSICAL_FLAGS = ("age_capped", "miss_height", "miss_weight", "miss_bmi", "miss_bp")
BOUNDS = {
    "height_cm": (120.0, 230.0),
    "weight_kg": (25.0, 350.0),
    "bmi": (10.0, 90.0),
    "sbp": (50.0, 280.0),
    "dbp": (20.0, 180.0),
}


def prepare_physical(
    config: Config, encounters: pd.DataFrame
) -> tuple[np.ndarray, np.ndarray, dict]:
    frame = encounters.copy()
    train = frame["role"].eq("train").to_numpy()
    frame["age_capped"] = frame["anchor_age"].gt(89).astype(np.float32)
    frame["age"] = np.where(
        frame["age_capped"].gt(0),
        91.0,
        frame["anchor_age"] + frame["admittime"].dt.year - frame["anchor_year"],
    )
    require(
        frame["age"].between(0, 120).all(),
        "Implausible age after anchor-year adjustment.",
    )
    columns = ["subject_id", "chartdate", "result_name", "result_value"]
    omr = read_subset(
        locate_table(config, "omr"),
        columns,
        set(frame["subject_id"]),
        config.chunk_size,
        key="subject_id",
        dtype={"result_name": str, "result_value": str},
    )
    omr["chartdate"] = pd.to_datetime(omr["chartdate"], errors="coerce").astype(
        "datetime64[ns]"
    )
    omr = omr.dropna(subset=["chartdate"])
    names = omr["result_name"].fillna("").str.strip().str.lower()
    omr["kind"] = np.select(
        [
            names.str.contains("height"),
            names.str.contains("weight"),
            names.str.contains("bmi"),
            names.isin(["blood pressure", "blood pressure sitting"]),
        ],
        ["height", "weight", "bmi", "bp"],
        default="excluded",
    )
    train_patients = set(frame.loc[train, "subject_id"])
    left = frame[["hadm_id", "subject_id", "admittime"]].copy()
    left["adate"] = left["admittime"].dt.normalize()
    left = left.sort_values("adate")
    unit_choices = {}
    for kind, values in (
        ("height", ["height_cm"]),
        ("weight", ["weight_kg"]),
        ("bmi", ["bmi"]),
        ("bp", ["sbp", "dbp"]),
    ):
        subset = omr.loc[omr["kind"].eq(kind)].copy()
        if kind == "bp":
            parts = (
                subset["result_value"]
                .str.split("/", n=1, expand=True)
                .reindex(columns=[0, 1])
            )
            subset["sbp"] = pd.to_numeric(parts[0], errors="coerce")
            subset["dbp"] = pd.to_numeric(parts[1], errors="coerce")
        else:
            subset[values[0]] = pd.to_numeric(
                subset["result_value"], errors="coerce"
            ).astype(np.float64)
            if kind in ("height", "weight"):
                for name, group in subset.groupby("result_name"):
                    lower = name.lower()
                    fitting = group.loc[
                        group["subject_id"].isin(train_patients), values[0]
                    ].dropna()
                    if kind == "height" and ("inch" in lower or "inches" in lower):
                        factor = 2.54
                    elif kind == "height" and ("cm" in lower or "centimeter" in lower):
                        factor = 1.0
                    elif kind == "weight" and ("lb" in lower or "pound" in lower):
                        factor = 0.45359237
                    elif kind == "weight" and ("kg" in lower or "kilogram" in lower):
                        factor = 1.0
                    else:
                        require(
                            not fitting.empty,
                            f"Cannot infer units from train patients for OMR field {name!r}.",
                        )
                        factor = (
                            (2.54 if fitting.median() < 100 else 1.0)
                            if kind == "height"
                            else (0.45359237 if fitting.median() >= 120 else 1.0)
                        )
                    subset.loc[group.index, values[0]] *= factor
                    unit_choices[name] = factor
        valid = pd.Series(True, index=subset.index)
        for value in values:
            valid &= subset[value].between(*BOUNDS[value])
        if kind == "bp":
            valid &= subset["sbp"].gt(subset["dbp"])
        aggregate = (
            subset.loc[valid]
            .groupby(["subject_id", "chartdate"], as_index=False)[values]
            .mean()
        )
        if aggregate.empty:
            for value in values:
                frame[value] = np.nan
            frame[f"{kind}_days"] = np.nan
            continue
        aggregate["subject_id"] = aggregate["subject_id"].astype(np.int64)
        joined = (
            pd.merge_asof(
                left,
                aggregate.sort_values("chartdate"),
                by="subject_id",
                left_on="adate",
                right_on="chartdate",
                direction="backward",
                allow_exact_matches=False,
            )
            .set_index("hadm_id")
            .reindex(frame["hadm_id"])
        )
        age = (joined["adate"] - joined["chartdate"]).dt.days
        require(
            age.dropna().gt(0).all(),
            "An OMR measurement is not strictly before admission.",
        )
        for value in values:
            frame[value] = joined[value].to_numpy()
        frame[f"{kind}_days"] = age.to_numpy(np.float64)
    calculated = frame["weight_kg"] / (frame["height_cm"] / 100) ** 2
    use_calculated = calculated.between(*BOUNDS["bmi"])
    frame.loc[use_calculated, "bmi"] = calculated[use_calculated]
    frame.loc[use_calculated, "bmi_days"] = frame.loc[
        use_calculated, ["height_days", "weight_days"]
    ].max(axis=1)
    for kind, column in (
        ("height", "height_cm"),
        ("weight", "weight_kg"),
        ("bmi", "bmi"),
        ("bp", "sbp"),
    ):
        frame[f"miss_{kind}"] = frame[column].isna().astype(np.float32)
    numeric, transforms = [], {}
    for column in PHYSICAL_NUMERIC:
        values = frame[column].to_numpy(np.float64)
        if column.endswith("_days"):
            values = np.log1p(np.maximum(values, 0))
        fitting = values[train & np.isfinite(values)]
        low, high = np.quantile(fitting, [0.005, 0.995]) if len(fitting) else (0.0, 0.0)
        fitting = np.clip(fitting, low, high)
        mean = float(fitting.mean()) if len(fitting) else 0.0
        std = float(fitting.std()) if len(fitting) else 1.0
        std = std if std > 1e-8 else 1.0
        numeric.append(
            np.where(np.isfinite(values), (np.clip(values, low, high) - mean) / std, 0)
        )
        transforms[column] = {
            "log1p": column.endswith("_days"),
            "clip": [float(low), float(high)],
            "mean": mean,
            "std": std,
            "n_fit": int(len(fitting)),
        }
    numeric.extend(frame[column].to_numpy(np.float32) for column in PHYSICAL_FLAGS)
    categories, vocabularies = [], {}
    for column in PHYSICAL_CATEGORIES:
        values = (
            frame[column]
            .fillna("UNKNOWN")
            .astype(str)
            .str.strip()
            .replace("", "UNKNOWN")
        )
        counts = values.loc[train].value_counts()
        kept = sorted(counts.index[counts.ge(config.minimum_category_count)])
        require(
            "__OTHER__" not in kept,
            "Reserved category __OTHER__ occurs in source data.",
        )
        vocab = {
            "__OTHER__": 0,
            **{value: index + 1 for index, value in enumerate(kept)},
        }
        categories.append(values.map(vocab).fillna(0).to_numpy(np.int32))
        vocabularies[column] = vocab
    numeric_array = np.column_stack(numeric).astype(np.float32)
    categorical_array = np.column_stack(categories).astype(np.int32)
    require(np.isfinite(numeric_array).all(), "Nonfinite physical characteristics.")
    metadata = {
        "numeric_columns": list(PHYSICAL_NUMERIC + PHYSICAL_FLAGS),
        "categorical_columns": list(PHYSICAL_CATEGORIES),
        "numeric_transforms": transforms,
        "categorical_vocabularies": vocabularies,
        "unit_multipliers": unit_choices,
        "bounds": BOUNDS,
        "fit_role": "train",
    }
    json_write(config.output / "physical_transform.json", metadata)
    return numeric_array, categorical_array, metadata


# Pre-Processing of Diagnosis and Procedure Codes


def read_gem(path: Path, kind: str) -> dict[str, list[str]]:
    require(path.is_file(), f"Missing cached CMS GEM archive: {path}")
    pattern = r"2018_I9gem\.txt$" if kind == "diagnoses" else r"gem_i9pcs\.txt$"
    with zipfile.ZipFile(path) as archive:
        matches = [
            name
            for name in archive.namelist()
            if re.search(pattern, name, re.IGNORECASE)
        ]
        require(
            len(matches) == 1, f"Cannot identify the ICD-9-to-ICD-10 mapping in {path}."
        )
        with archive.open(matches[0]) as handle:
            table = pd.read_csv(
                handle,
                sep=r"\s+",
                header=None,
                names=["icd9", "icd10", "flags"],
                dtype=str,
            )
    mapped = (
        table["flags"].str[1].ne("1")
        & table["icd10"].ne("NoDx")
        & table["icd10"].ne("NoPCS")
    )
    table = table.loc[mapped].dropna().drop_duplicates(["icd9", "icd10"])
    return (
        table.groupby("icd9")["icd10"].agg(lambda values: sorted(set(values))).to_dict()
    )


def prepare_codes(
    config: Config, encounters: pd.DataFrame, kind: str
) -> tuple[sparse.csr_matrix, dict]:
    table_name = "diagnoses_icd" if kind == "diagnoses" else "procedures_icd"
    path = locate_table(config, table_name)
    columns = ["hadm_id", "icd_code", "icd_version"]
    codes = read_subset(
        path,
        columns,
        set(encounters["hadm_id"]),
        config.chunk_size,
        dtype={"icd_code": str},
    )
    codes["icd_code"] = (
        codes["icd_code"]
        .fillna("")
        .str.upper()
        .str.replace(".", "", regex=False)
        .str.strip()
    )
    codes["icd_version"] = pd.to_numeric(codes["icd_version"], errors="coerce")
    require(
        codes["icd_version"].isin([9, 10]).all(), f"Unexpected ICD version in {path}."
    )
    codes = codes.loc[codes["icd_code"].ne("")].drop_duplicates(columns)
    gem_path = config.code_maps / (
        "gem_cm.zip" if kind == "diagnoses" else "gem_pcs.zip"
    )
    mapping = read_gem(gem_path, kind) if codes["icd_version"].eq(9).any() else {}
    codes["tokens"] = [
        [code] if version == 10 else mapping.get(code, ["__UNMAPPED_ICD9__"])
        for code, version in zip(codes["icd_code"], codes["icd_version"])
    ]
    train_ids = set(encounters.loc[encounters["role"].eq("train"), "hadm_id"])
    expanded = codes[["hadm_id", "tokens"]].explode("tokens").drop_duplicates()
    counts = (
        expanded.loc[expanded["hadm_id"].isin(train_ids)]
        .groupby("tokens")["hadm_id"]
        .nunique()
    )
    reserved = ["__OTHER__", "__UNMAPPED_ICD9__"]
    vocabulary = reserved + sorted(
        set(counts.index[counts.ge(config.minimum_code_count)]) - set(reserved)
    )
    token_index = {token: index for index, token in enumerate(vocabulary)}
    encounter_index = pd.Series(np.arange(len(encounters)), index=encounters["hadm_id"])
    row = expanded["hadm_id"].map(encounter_index).to_numpy(np.int64)
    column = expanded["tokens"].map(token_index).fillna(0).to_numpy(np.int64)
    matrix = sparse.csr_matrix(
        (np.ones(len(row), np.float32), (row, column)),
        shape=(len(encounters), len(vocabulary)),
    )
    matrix.sum_duplicates()
    matrix.data[:] = 1.0
    matrix.sort_indices()
    icd9 = codes["icd_version"].eq(9)
    unmatched = sum(
        tokens == ["__UNMAPPED_ICD9__"] for tokens in codes.loc[icd9, "tokens"]
    )
    metadata = {
        "vocabulary": vocabulary,
        "minimum_train_encounters": config.minimum_code_count,
        "encoding": "Binary multi-hot; repeated and overlapping mapped codes count once.",
        "icd9_mapping": "Union of CMS GEM candidates, including approximate and one-to-many mappings; not exact equivalence.",
        "unmapped_icd9": "Reserved missing-mapping indicator; never relabeled as an ICD-10 code.",
        "source": source_signature(path),
        "icd9_encounter_code_pairs": int(icd9.sum()),
        "unmapped_icd9_encounter_code_pairs": int(unmatched),
        "fit_role": "train",
        "gem_sha256": digest_file(gem_path) if mapping else None,
    }
    json_write(config.output / f"{kind}_vocabulary.json", metadata)
    return matrix, metadata


# Pre-Processing of Medication Frequency


def normalize_ndc(value: Any) -> str:
    if pd.isna(value):
        return ""
    value = re.sub(r"\.0+$", "", str(value).strip())
    if "-" in value:
        parts = value.split("-")
        if len(parts) == 3 and tuple(map(len, parts)) in (
            (4, 4, 2),
            (5, 3, 2),
            (5, 4, 1),
            (5, 4, 2),
        ):
            value = parts[0].zfill(5) + parts[1].zfill(4) + parts[2].zfill(2)
        else:
            return ""
    if not value.isdigit() or not 9 <= len(value) <= 11 or len(set(value)) == 1:
        return ""
    return value.zfill(11)


def medication_mapping(config: Config) -> tuple[dict[str, list[str]], dict]:
    mapping = {}
    provenance = {
        "source": None,
        "fallback": "Normalized drug name when no cached ATC4 mapping exists.",
    }
    if config.medication_map.is_file():
        table = pd.read_parquet(config.medication_map)
        require(
            table["ndc11"].is_unique, "Duplicate NDC entries in the medication map."
        )
        for row in table.itertuples(index=False):
            codes = str(row.atc).split(";") if pd.notna(row.atc) else []
            mapping[normalize_ndc(row.ndc11)] = sorted(
                {
                    code[:5]
                    for code in codes
                    if re.fullmatch(r"[A-Z]\d{2}[A-Z]{2}", code[:5])
                }
            )
        provenance["source"] = source_signature(config.medication_map)
        provenance["sha256"] = digest_file(config.medication_map)
    elif config.rxnav_cache.is_dir():
        ndc, atc = {}, {}
        for path in sorted(config.rxnav_cache.glob("ndc_*.json")):
            ndc.update(json_read(path))
        for path in sorted(config.rxnav_cache.glob("atc_*.json")):
            atc.update(json_read(path))
        for code, information in ndc.items():
            classes = atc.get(information.get("rxcui", ""), {}).get("atc", [])
            mapping[normalize_ndc(code)] = sorted(
                {
                    value[:5]
                    for value in classes
                    if re.fullmatch(r"[A-Z]\d{2}[A-Z]{2}", value[:5])
                }
            )
        provenance["source"] = str(config.rxnav_cache)
    mapping.pop("", None)
    return mapping, provenance


def prepare_medications(
    config: Config, encounters: pd.DataFrame
) -> tuple[sparse.csr_matrix, dict]:
    path = locate_table(config, "prescriptions")
    mapping, provenance = medication_mapping(config)
    ids = set(encounters["hadm_id"])
    parts, total_orders, mapped_orders = [], 0, 0
    for chunk in pd.read_csv(
        path,
        usecols=["hadm_id", "drug", "ndc", "doses_per_24_hrs"],
        dtype={"ndc": str, "drug": str},
        chunksize=config.chunk_size,
    ):
        chunk = chunk.loc[chunk["hadm_id"].isin(ids)].copy()
        if chunk.empty:
            continue
        total_orders += len(chunk)
        chunk["ndc11"] = chunk["ndc"].map(normalize_ndc)
        names = (
            chunk["drug"]
            .fillna("")
            .str.upper()
            .str.replace(r"\s+", " ", regex=True)
            .str.strip()
        )
        chunk["token"] = [
            (
                ["ATC:" + value for value in mapping[ndc]]
                if mapping.get(ndc)
                else ["NAME:" + name if name else "NAME:__MISSING__"]
            )
            for ndc, name in zip(chunk["ndc11"], names)
        ]
        mapped_orders += sum(bool(mapping.get(code)) for code in chunk["ndc11"])
        chunk["frequency"] = pd.to_numeric(chunk["doses_per_24_hrs"], errors="coerce")
        chunk.loc[
            ~np.isfinite(chunk["frequency"]) | chunk["frequency"].lt(0), "frequency"
        ] = np.nan
        expanded = chunk[["hadm_id", "token", "frequency"]].explode("token")
        parts.append(
            expanded.groupby(["hadm_id", "token"], as_index=False).agg(
                n_orders=("token", "size"), max_doses_per_24h=("frequency", "max")
            )
        )
    if parts:
        aggregated = (
            pd.concat(parts, ignore_index=True)
            .groupby(["hadm_id", "token"], as_index=False)
            .agg(
                n_orders=("n_orders", "sum"),
                max_doses_per_24h=("max_doses_per_24h", "max"),
            )
        )
    else:
        aggregated = pd.DataFrame(
            columns=["hadm_id", "token", "n_orders", "max_doses_per_24h"]
        )
    train_ids = set(encounters.loc[encounters["role"].eq("train"), "hadm_id"])
    counts = (
        aggregated.loc[aggregated["hadm_id"].isin(train_ids)]
        .groupby("token")["hadm_id"]
        .nunique()
    )
    reserved = ["ATC:__OTHER__", "NAME:__OTHER__", "NAME:__MISSING__"]
    vocabulary = reserved + sorted(
        set(counts.index[counts.ge(config.minimum_medication_count)]) - set(reserved)
    )
    positions = {name: index for index, name in enumerate(vocabulary)}
    aggregated["code_id"] = [
        positions.get(name, 0 if name.startswith("ATC:") else 1)
        for name in aggregated["token"]
    ]
    grouped = aggregated.groupby(["hadm_id", "code_id"], as_index=False).agg(
        n_orders=("n_orders", "sum"), max_doses_per_24h=("max_doses_per_24h", "max")
    )
    encounter_index = pd.Series(np.arange(len(encounters)), index=encounters["hadm_id"])
    row = grouped["hadm_id"].map(encounter_index).to_numpy(np.int64)
    column = grouped["code_id"].to_numpy(np.int64)
    count = sparse.csr_matrix(
        (np.log1p(grouped["n_orders"].to_numpy(np.float32)), (row, column)),
        shape=(len(encounters), len(vocabulary)),
    )
    doses = sparse.csr_matrix(
        (
            np.log1p(grouped["max_doses_per_24h"].fillna(0).to_numpy(np.float32)),
            (row, column),
        ),
        shape=count.shape,
    )
    matrix = sparse.hstack([count, doses], format="csr", dtype=np.float32)
    matrix.eliminate_zeros()
    require(np.isfinite(matrix.data).all(), "Nonfinite medication frequencies.")
    metadata = {
        "vocabulary": vocabulary,
        "channels": ["log1p_n_orders", "log1p_max_doses_per_24h"],
        "minimum_train_encounters": config.minimum_medication_count,
        "fit_role": "train",
        "scope": "Completed history stays only; both frequency channels are zero for current stays.",
        "total_orders": total_orders,
        "orders_with_cached_atc": mapped_orders,
        "mapping": provenance,
        "source": source_signature(path),
    }
    json_write(config.output / "medication_vocabulary.json", metadata)
    return matrix, metadata


# Pre-Processing of Clinical Texts

SECTION_HEADERS = {
    "admin": ["name", "admission date", "date of birth", "service", "attending"],
    "allergy": ["allergies", "allergy", "allergies/adverse drug reactions"],
    "cc": ["chief complaint", "chief complaints"],
    "procedure": [
        "major surgical or invasive procedure",
        "major surgical or invasive procedures",
    ],
    "hpi": [
        "history of present illness",
        "history of the present illness",
        "present illness",
    ],
    "pmh": [
        "past medical history",
        "past medical/surgical history",
        "past medical and surgical history",
        "past medical history/past surgical history",
        "past surgical history",
        "pmh/psh",
    ],
    "shx": ["social history"],
    "fhx": ["family history"],
    "exam": [
        "physical exam",
        "physical examination",
        "admission physical exam",
        "admission physical examination",
        "admission exam",
        "physical exam on admission",
        "physical examination on admission",
        "exam on admission",
        "exam at admission",
        "physical exam at admission",
        "physical examination at admission",
        "admission pe",
        "admission physical",
        "discharge physical exam",
        "discharge physical examination",
        "discharge exam",
        "physical exam on discharge",
        "physical examination on discharge",
        "exam on discharge",
        "exam at discharge",
        "physical exam at discharge",
        "physical examination at discharge",
        "discharge pe",
        "discharge physical",
    ],
    "results": ["pertinent results", "laboratory data", "pertinent labs"],
    "bhc": [
        "brief hospital course",
        "hospital course",
        "summary of hospital course",
        "brief summary of hospital course",
    ],
    "admmeds": [
        "medications on admission",
        "admission medications",
        "admission meds",
        "home medications",
        "home meds",
        "preadmission medications",
        "pre-admission medications",
        "medications prior to admission",
        "outpatient medications",
        "meds on admission",
        "medications at home",
    ],
    "dismeds": [
        "discharge medications",
        "discharge medication",
        "discharge meds",
        "medications on discharge",
        "medications at discharge",
    ],
    "discharge": [
        "discharge disposition",
        "discharge diagnosis",
        "discharge diagnoses",
        "discharge condition",
        "discharge instructions",
        "followup instructions",
        "follow-up instructions",
        "follow up instructions",
    ],
    "transitional": ["transitional issues"],
}


def normalize_header(value: str) -> str:
    return re.sub(r"\s*/\s*", "/", re.sub(r"\s+", " ", value.strip().lower()))


HEADER_GROUPS = {
    normalize_header(header): group
    for group, headers in SECTION_HEADERS.items()
    for header in headers
}
HEADER_ALTERNATIVES = "|".join(
    sorted(
        (
            re.escape(value).replace(r"\ ", r"\s+").replace("/", r"\s*/\s*")
            for value in HEADER_GROUPS
        ),
        key=len,
        reverse=True,
    )
)
HEADER_PATTERN = re.compile(
    rf"(?im)^[ \t]*(?P<header>{HEADER_ALTERNATIVES})[ \t]*(?::|$)"
)
CURRENT_STAY_PATTERN = re.compile(
    r"(?i)\b(?:during (?:this|the|her|his|their) (?:admission|hospitalization|hospital stay|stay)|"
    r"hospital course|post[- ]?op(?:erative)? day|pod\s*#?\s*\d|hospital day\s*#?\s*\d|hd\s*#\s*\d|"
    r"prior to discharge|day of discharge)"
)


def clean_text(value: Any) -> str:
    if value is None or pd.isna(value):
        return ""
    text = str(value).replace("\r\n", "\n").replace("\r", "\n")
    text = re.sub(r"\n{3,}", "\n\n", re.sub(r"[ \t]+\n", "\n", text)).strip()
    return text if re.sub(r"_{2,}|[^A-Za-z0-9]", "", text) else ""


def parse_sections(value: Any) -> dict[str, str]:
    text = clean_text(value)
    headers = list(HEADER_PATTERN.finditer(text))
    sections = defaultdict(list)
    for index, header in enumerate(headers):
        group = HEADER_GROUPS[normalize_header(header.group("header"))]
        if group in ("cc", "hpi", "bhc"):
            end = headers[index + 1].start() if index + 1 < len(headers) else len(text)
            sections[group].append(text[header.end() : end])
    return {key: clean_text("\n".join(sections[key])) for key in ("cc", "hpi", "bhc")}


def section_tokens(text: str) -> set[str]:
    return set(re.sub(r"[^a-z0-9 ]", " ", text.lower()).split())


def section_is_contained(text: str, group: str) -> bool:
    return all(
        HEADER_GROUPS[normalize_header(match.group("header"))] == group
        for match in HEADER_PATTERN.finditer(text)
    )


def select_section(source: Any, parsed: Any, group: str) -> str:
    source, parsed = clean_text(source), clean_text(parsed)
    if source and not section_is_contained(source, group):
        source = parse_sections(source)[group]
    if not parsed:
        return source
    if not source:
        return parsed
    source_tokens, parsed_tokens = section_tokens(source), section_tokens(parsed)
    contained = len(source_tokens & parsed_tokens) / max(len(parsed_tokens), 1) >= 0.95
    return source if contained and len(source.split()) > len(parsed.split()) else parsed


def scrub_current_stay(text: str) -> str:
    segments = re.split(r"((?<=[.;!?])[ \t]+|\n)", text)
    output = []
    for index in range(0, len(segments), 2):
        segment = segments[index]
        if not CURRENT_STAY_PATTERN.search(segment):
            output.append(
                segment + (segments[index + 1] if index + 1 < len(segments) else "")
            )
    cleaned = clean_text("".join(output))
    if CURRENT_STAY_PATTERN.search(cleaned) and cleaned != text:
        return scrub_current_stay(cleaned)
    return cleaned


def prepare_sections(config: Config, encounters: pd.DataFrame) -> pd.DataFrame:
    note_path = locate_table(config, "discharge")
    ids = set(encounters["hadm_id"])
    key = hashlib.sha256(
        json.dumps(
            {
                "parser": "clinical-sections-v1",
                "notes": source_signature(note_path),
                "raw": source_signature(config.raw) if config.raw.is_file() else None,
                "ids": digest_frame(encounters[["hadm_id"]]),
                "chunk": config.note_chunk_size,
            },
            sort_keys=True,
        ).encode()
    ).hexdigest()[:20]
    cache = config.output / "cache" / f"sections_{key}"
    cache.mkdir(parents=True, exist_ok=True)
    columns = ["hadm_id", "note_id", "note_seq", "charttime", "storetime", "text"]
    filenames = []
    for index, chunk in enumerate(
        pd.read_csv(note_path, usecols=columns, chunksize=config.note_chunk_size)
    ):
        destination = cache / f"part_{index:06d}.parquet"
        filenames.append(destination)
        if destination.is_file():
            continue
        chunk = chunk.loc[chunk["hadm_id"].isin(ids)]
        rows = []
        for row in chunk.itertuples(index=False):
            sections = parse_sections(row.text)
            rows.append(
                {
                    "hadm_id": int(row.hadm_id),
                    "note_id": str(row.note_id),
                    "note_seq": row.note_seq,
                    "charttime": row.charttime,
                    "storetime": row.storetime,
                    **sections,
                }
            )
        parquet_write(
            destination, pd.DataFrame(rows, columns=columns[:-1] + ["cc", "hpi", "bhc"])
        )
    notes = (
        pd.concat([pd.read_parquet(path) for path in filenames], ignore_index=True)
        if filenames
        else pd.DataFrame(columns=columns[:-1] + ["cc", "hpi", "bhc"])
    )
    notes["charttime"] = pd.to_datetime(notes["charttime"], errors="coerce")
    notes["storetime"] = pd.to_datetime(notes["storetime"], errors="coerce")
    notes["note_seq"] = pd.to_numeric(notes["note_seq"], errors="coerce").fillna(-1)
    available = (
        notes[["storetime", "charttime"]].max(axis=1).where(notes["storetime"].notna())
    )
    notes["available_time"] = available
    availability = notes.groupby("hadm_id")["available_time"].max()
    unknown = notes["available_time"].isna().groupby(notes["hadm_id"]).any()
    availability.loc[unknown] = pd.NaT
    notes = notes.sort_values(
        ["hadm_id", "charttime", "note_seq", "note_id"], na_position="first"
    ).drop_duplicates("hadm_id", keep="last")
    source = encounters[["hadm_id"]].copy()
    text_columns = ["cc_text", "hpi_text", "bhc_text"]
    if config.raw.is_file():
        available_columns = set(pd.read_csv(config.raw, nrows=0).columns)
        supplied_columns = [
            column for column in text_columns if column in available_columns
        ]
        if supplied_columns:
            source = read_subset(
                config.raw,
                ["hadm_id"] + supplied_columns,
                ids,
                config.chunk_size,
                dtype={column: str for column in supplied_columns},
            )
    for column in text_columns:
        if column not in source:
            source[column] = ""
    require(source["hadm_id"].is_unique, "Duplicate source text encounters.")
    frame = encounters[["hadm_id"]].merge(
        source, on="hadm_id", how="left", validate="one_to_one"
    )
    frame = frame.merge(
        notes[["hadm_id", "note_id", "cc", "hpi", "bhc"]],
        on="hadm_id",
        how="left",
        validate="one_to_one",
    )
    for group, target in zip(("cc", "hpi", "bhc"), TEXT_SECTIONS):
        selected = [
            select_section(raw, parsed, group)
            for raw, parsed in zip(frame[f"{group}_text"], frame[group])
        ]
        frame[target] = (
            [scrub_current_stay(value) for value in selected]
            if group != "bhc"
            else selected
        )
    frame["note_available_time"] = frame["hadm_id"].map(availability)
    frame["note_id"] = frame["note_id"].fillna("")
    frame = frame[["hadm_id", "note_id", "note_available_time"] + list(TEXT_SECTIONS)]
    require(
        not any(
            CURRENT_STAY_PATTERN.search(value)
            for section in TEXT_SECTIONS[:2]
            for value in frame[section]
        ),
        "Current-stay markers remain in admission-proxy text.",
    )
    parquet_write(config.output / "text_sections.parquet", frame)
    return frame


def content_windows(
    length: int, window: int, overlap: int
) -> list[tuple[int, int, int, int]]:
    capacity = window - 2
    require(
        capacity > 0 and 0 <= overlap < capacity,
        "Text overlap must be smaller than the content window.",
    )
    if length <= capacity:
        return [(0, length, 0, length)] if length else []
    starts = sorted(
        set(list(range(0, length - capacity, capacity - overlap)) + [length - capacity])
    )
    result = []
    for index, start in enumerate(starts):
        low = 0 if index == 0 else (start + starts[index - 1] + capacity) // 2
        high = (
            length
            if index == len(starts) - 1
            else (starts[index + 1] + start + capacity) // 2
        )
        result.append((start, start + capacity, low, high))
    require(
        sum(high - low for _, _, low, high in result) == length,
        "Token window coverage is incomplete.",
    )
    return result


class FrozenTextEncoder:

    def __init__(self, config: Config):
        import torch
        from transformers import AutoModel, AutoTokenizer

        self.torch = torch
        self.config = config
        self.device = (
            torch.device("cuda" if torch.cuda.is_available() else "cpu")
            if config.device == "auto"
            else torch.device(config.device)
        )
        self.tokenizer = AutoTokenizer.from_pretrained(
            str(config.encoder), local_files_only=True
        )
        self.tokenizer.model_max_length = 10**9
        self.model = (
            AutoModel.from_pretrained(
                str(config.encoder), local_files_only=True, attn_implementation="sdpa"
            )
            .float()
            .to(self.device)
            .eval()
        )
        self.model.requires_grad_(False)
        self.dimension = int(self.model.config.hidden_size)
        require(
            all(
                value is not None
                for value in (
                    self.tokenizer.cls_token_id,
                    self.tokenizer.sep_token_id,
                    self.tokenizer.pad_token_id,
                )
            ),
            "The text encoder must supply CLS, SEP, and PAD tokens.",
        )
        maximum = int(
            getattr(self.model.config, "max_position_embeddings", config.text_window)
        )
        require(
            config.text_window <= maximum,
            "Requested text window exceeds the encoder context length.",
        )
        if self.device.type == "cuda":
            torch.backends.cuda.matmul.allow_tf32 = False
            torch.backends.cudnn.allow_tf32 = False

    def _forward(
        self, documents: list[list[int]], windows: list[tuple[int, int, int, int, int]]
    ) -> np.ndarray:
        torch = self.torch
        width = max(end - start for _, start, end, _, _ in windows) + 2
        ids = np.full(
            (len(windows), width), self.tokenizer.pad_token_id, dtype=np.int64
        )
        attention = np.zeros_like(ids)
        content = np.zeros(ids.shape, dtype=np.float32)
        for index, (document, start, end, low, high) in enumerate(windows):
            tokens = documents[document][start:end]
            ids[index, 0] = self.tokenizer.cls_token_id
            ids[index, 1 : len(tokens) + 1] = tokens
            ids[index, len(tokens) + 1] = self.tokenizer.sep_token_id
            attention[index, : len(tokens) + 2] = 1
            content[index, 1 + low - start : 1 + high - start] = 1
        with torch.inference_mode():
            output = self.model(
                input_ids=torch.from_numpy(ids).to(self.device),
                attention_mask=torch.from_numpy(attention).to(self.device),
            )
            mask = torch.from_numpy(content).to(self.device).unsqueeze(-1)
            return (
                (output.last_hidden_state.float() * mask).sum(1).double().cpu().numpy()
            )

    def _run(
        self, documents: list[list[int]], windows: list[tuple[int, int, int, int, int]]
    ) -> np.ndarray:
        try:
            return self._forward(documents, windows)
        except self.torch.cuda.OutOfMemoryError:
            if len(windows) == 1:
                raise RuntimeError(
                    "One text window exceeds GPU memory; rerun with a smaller --text-window."
                ) from None
        gc.collect()
        self.torch.cuda.empty_cache()
        middle = len(windows) // 2
        return np.concatenate(
            [
                self._run(documents, windows[:middle]),
                self._run(documents, windows[middle:]),
            ]
        )

    def encode(self, texts: list[str]) -> np.ndarray:
        documents = self.tokenizer(
            texts, add_special_tokens=False, truncation=False, padding=False
        )["input_ids"]
        windows = [
            (index, *part)
            for index, tokens in enumerate(documents)
            for part in content_windows(
                len(tokens), self.config.text_window, self.config.text_overlap
            )
        ]
        windows.sort(key=lambda item: item[2] - item[1])
        sums = np.zeros((len(texts), self.dimension), dtype=np.float64)
        counts = np.zeros(len(texts), dtype=np.int64)
        start = 0
        while start < len(windows):
            end, width = start, 0
            while end < len(windows):
                next_width = max(width, windows[end][2] - windows[end][1] + 2)
                if (
                    end > start
                    and (end - start + 1) * next_width > self.config.text_token_budget
                ):
                    break
                width = next_width
                end += 1
            batch = windows[start:end]
            values = self._run(documents, batch)
            for item, vector in zip(batch, values):
                document, _, _, low, high = item
                sums[document] += vector
                counts[document] += high - low
            start = end
        require(
            np.array_equal(counts, np.asarray([len(tokens) for tokens in documents])),
            "Embedding token counts disagree.",
        )
        result = (sums / np.maximum(counts[:, None], 1)).astype(np.float32)
        require(np.isfinite(result).all(), "Text encoder produced nonfinite features.")
        return result


def validate_encoder(config: Config) -> tuple[int, str, dict]:
    configuration = config.encoder / "config.json"
    require(configuration.is_file(), f"Missing encoder configuration: {configuration}")
    weights = sorted(config.encoder.glob("*.safetensors"))
    if not weights:
        weights = sorted(config.encoder.glob("pytorch_model*.bin"))
    require(bool(weights), f"No pretrained encoder weights found in {config.encoder}.")
    vocabulary_files = [
        config.encoder / name for name in ("tokenizer.json", "vocab.txt", "vocab.json")
    ]
    require(
        any(path.is_file() for path in vocabulary_files),
        "The encoder directory must include its tokenizer vocabulary.",
    )
    signature = hashlib.sha256()
    for path in [configuration] + weights:
        signature.update(path.name.encode())
        signature.update(digest_file(path).encode())
    dimension = int(json_read(configuration)["hidden_size"])
    require(dimension > 0, "Encoder hidden_size must be positive.")
    return (
        dimension,
        signature.hexdigest(),
        {
            "encoder_directory": str(config.encoder),
            "frozen": True,
            "training_patients_checked": config.pretraining_roles.is_file(),
        },
    )


def prepare_embeddings(
    config: Config, encounters: pd.DataFrame, sections: pd.DataFrame
) -> tuple[dict[str, Path], np.ndarray, dict]:
    dimension, encoder_sha, provenance = validate_encoder(config)
    cache = config.output / "cache" / "text_embeddings"
    cache.mkdir(parents=True, exist_ok=True)
    train = encounters["role"].eq("train").to_numpy()
    has = np.column_stack(
        [sections[name].str.len().gt(0).to_numpy() for name in TEXT_SECTIONS]
    )
    paths, counts, encoder = {}, {}, None
    for section_number, section in enumerate(TEXT_SECTIONS):
        values = sections[section].tolist()
        key = hashlib.sha256(
            json.dumps(
                {
                    "encoder": encoder_sha,
                    "tokenizer": {
                        path.name: digest_file(path)
                        for path in sorted(config.encoder.iterdir())
                        if path.name
                        in {
                            "tokenizer.json",
                            "tokenizer_config.json",
                            "vocab.txt",
                            "vocab.json",
                            "merges.txt",
                            "special_tokens_map.json",
                        }
                    },
                    "window": config.text_window,
                    "overlap": config.text_overlap,
                    "pooling": "last",
                    "section": section,
                    "content": digest_frame(sections[["hadm_id", section]]),
                },
                sort_keys=True,
            ).encode()
        ).hexdigest()[:20]
        raw_path = cache / f"raw_{section}_{key}.npy"
        raw = np.lib.format.open_memmap(
            raw_path, mode="w+", dtype=np.float16, shape=(len(encounters), dimension)
        )
        for start in range(0, len(encounters), config.text_shard_size):
            end = min(start + config.text_shard_size, len(encounters))
            shard = cache / f"{section}_{key}_{start:09d}_{end:09d}.npy"
            if shard.is_file():
                block = np.load(shard, allow_pickle=False)
                require(
                    block.shape == (end - start, dimension)
                    and np.isfinite(block).all(),
                    "Invalid cached embedding shard.",
                )
            else:
                block = np.zeros((end - start, dimension), dtype=np.float32)
                pending = np.flatnonzero(has[start:end, section_number])
                if len(pending):
                    if encoder is None:
                        encoder = FrozenTextEncoder(config)
                    block[pending] = encoder.encode(
                        [values[start + index] for index in pending]
                    )
                require(
                    np.isfinite(block).all()
                    and np.abs(block).max(initial=0) <= np.finfo(np.float16).max,
                    "Embedding values exceed finite float16 range.",
                )
                temporary = shard.with_name(shard.name + ".tmp")
                with temporary.open("wb") as handle:
                    np.save(handle, block.astype(np.float16), allow_pickle=False)
                temporary.replace(shard)
            raw[start:end] = block
        raw.flush()
        fitting = train & has[:, section_number]
        number, total, squared = (
            0,
            np.zeros(dimension, np.float64),
            np.zeros(dimension, np.float64),
        )
        for start in range(0, len(encounters), config.text_shard_size):
            end = min(start + config.text_shard_size, len(encounters))
            block = np.asarray(raw[start:end][fitting[start:end]], dtype=np.float64)
            number += len(block)
            total += block.sum(axis=0)
            squared += np.square(block).sum(axis=0)
        mean = total / max(number, 1)
        std = np.sqrt(np.maximum(squared / max(number, 1) - mean**2, 0))
        std[std < 1e-8] = 1.0
        np.savez(
            config.output / f"text_transform_{section}.npz",
            mean=mean.astype(np.float32),
            std=std.astype(np.float32),
            n_fit=number,
        )
        destination = config.output / f"encounter_{section}.npy"
        normalized = np.lib.format.open_memmap(
            destination, mode="w+", dtype=np.float32, shape=raw.shape
        )
        for start in range(0, len(encounters), config.text_shard_size):
            end = min(start + config.text_shard_size, len(encounters))
            block = (np.asarray(raw[start:end], dtype=np.float32) - mean) / std
            block[~has[start:end, section_number]] = 0
            require(np.isfinite(block).all(), "Normalized text features are nonfinite.")
            normalized[start:end] = block
        normalized.flush()
        paths[section] = destination
        counts[section] = {
            "present": int(has[:, section_number].sum()),
            "train_fit": number,
        }
        del raw, normalized
        LOGGER.info("Prepared %s: %s texts.", section, counts[section]["present"])
    if encoder is not None:
        torch = encoder.torch
        del encoder
        gc.collect()
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
    metadata = {
        "encoder_sha256": encoder_sha,
        "dimension": dimension,
        "provenance": provenance,
        "sections": list(TEXT_SECTIONS),
        "pooling": "Last-layer content-token mean; each token counted once.",
        "window": config.text_window,
        "overlap": config.text_overlap,
        "transform": "Per-dimension standardization fit only on present train sections.",
        "counts": counts,
    }
    json_write(config.output / "embedding_manifest.json", metadata)
    return paths, has, metadata


# Construction of Chronological Encounter Sequences


def prepare_sequences(
    config: Config, encounters: pd.DataFrame, sections: pd.DataFrame
) -> tuple[pd.DataFrame, dict, dict]:
    records = {"aprA": [], "aprC": []}
    selected = {"aprA": [], "aprC": []}
    note_times = pd.to_datetime(sections["note_available_time"]).to_numpy(
        dtype="datetime64[ns]"
    )
    columns = [
        "t_hadm",
        "subject_id",
        "split",
        "hadm_id",
        "is_current",
        "seq_role",
        "seq_pos",
        "seq_len",
        "gap_days_log",
        "view",
    ]
    for subject, group in encounters.groupby("subject_id", sort=False):
        indices = group.index.to_numpy(np.int64)
        admissions = group["admittime"].to_numpy(dtype="datetime64[ns]")
        discharges = group["dischtime"].to_numpy(dtype="datetime64[ns]")
        hadms = group["hadm_id"].to_numpy(np.int64)
        prolonged = group["prolonged"].to_numpy(bool)
        split = str(group["role"].iloc[0])
        for position, target in enumerate(hadms):
            if split != "train" and not prolonged[position]:
                continue
            prior = np.flatnonzero(
                (admissions[:position] < admissions[position])
                & (discharges[:position] <= admissions[position])
            )
            for approach in ("aprA", "aprC"):
                if approach == "aprC" and not prolonged[position]:
                    continue
                history = prior[prolonged[prior]] if approach == "aprC" else prior
                history = (
                    history[-(config.window - 1) :]
                    if config.window > 1
                    else history[:0]
                )
                selected[approach].append(int(indices[position]))
                length = len(history) + 1
                for sequence_position, past in enumerate(history):
                    available = note_times[indices[past]]
                    readable = (
                        not np.isnat(available) and available <= admissions[position]
                    )
                    view = "history" if readable else "history_note_masked"
                    gap = float(
                        (admissions[position] - discharges[past])
                        / np.timedelta64(1, "D")
                    )
                    role = "prol_hist" if approach == "aprC" else "hist"
                    records[approach].append(
                        (
                            int(target),
                            int(subject),
                            split,
                            int(hadms[past]),
                            False,
                            role,
                            sequence_position,
                            length,
                            math.log1p(max(gap, 0)),
                            view,
                        )
                    )
                records[approach].append(
                    (
                        int(target),
                        int(subject),
                        split,
                        int(target),
                        True,
                        "current",
                        length - 1,
                        length,
                        0.0,
                        "current",
                    )
                )
    sequences = {name: pd.DataFrame(records[name], columns=columns) for name in records}
    sequences["aprB"] = sequences["aprA"].copy()
    prolonged_by_id = encounters.set_index("hadm_id")["prolonged"]
    history = ~sequences["aprB"]["is_current"].astype(bool)
    sequences["aprB"].loc[history, "seq_role"] = np.where(
        sequences["aprB"].loc[history, "hadm_id"].map(prolonged_by_id),
        "prol_hist",
        "short_hist",
    )
    selected["aprB"] = selected["aprA"].copy()
    all_rows = pd.concat(
        [frame[["hadm_id", "view"]] for frame in sequences.values()], ignore_index=True
    ).drop_duplicates()
    view_order = {"current": 0, "history": 1, "history_note_masked": 2}
    all_rows["view_order"] = all_rows["view"].map(view_order)
    rows = (
        all_rows.sort_values(["view_order", "hadm_id"])
        .drop(columns="view_order")
        .reset_index(drop=True)
    )
    rows["row_id"] = np.arange(len(rows), dtype=np.int64)
    position_by_id = pd.Series(np.arange(len(encounters)), index=encounters["hadm_id"])
    rows["encounter_index"] = rows["hadm_id"].map(position_by_id).astype(np.int64)
    row_lookup = pd.MultiIndex.from_frame(rows[["hadm_id", "view"]])
    targets = {}
    for approach in APPROACHES:
        frame = (
            sequences[approach]
            .sort_values(["t_hadm", "seq_pos"])
            .reset_index(drop=True)
        )
        frame["row_id"] = row_lookup.get_indexer(
            pd.MultiIndex.from_frame(frame[["hadm_id", "view"]])
        )
        require(frame["row_id"].ge(0).all(), "An encounter view has no feature row.")
        current = frame.loc[frame["is_current"].astype(bool)]
        require(
            current["t_hadm"].is_unique
            and current["hadm_id"].eq(current["t_hadm"]).all(),
            "Invalid current encounter.",
        )
        require(
            (current["seq_pos"] + 1).eq(current["seq_len"]).all(),
            "Current encounter must end each sequence.",
        )
        require(
            frame["seq_len"].between(1, config.window).all(), "Invalid sequence length."
        )
        target = encounters.loc[
            selected[approach],
            ["hadm_id", "subject_id", "role", "los_days", "z", "prolonged"],
        ].copy()
        target = (
            target.rename(columns={"hadm_id": "t_hadm", "role": "split"})
            .sort_values("t_hadm")
            .reset_index(drop=True)
        )
        target["current_row_id"] = (
            target["t_hadm"].map(current.set_index("t_hadm")["row_id"]).astype(np.int64)
        )
        target["seq_len"] = (
            target["t_hadm"]
            .map(current.set_index("t_hadm")["seq_len"])
            .astype(np.int32)
        )
        require(len(target) == len(current), "Targets and current encounters disagree.")
        targets[approach] = target
        sequences[approach] = frame
    for split in ("val", "test"):
        reference = (
            targets["aprA"].loc[targets["aprA"]["split"].eq(split), "t_hadm"].to_numpy()
        )
        for approach in APPROACHES:
            actual = targets[approach].loc[targets[approach]["split"].eq(split)]
            require(
                np.array_equal(reference, actual["t_hadm"].to_numpy())
                and actual["prolonged"].all(),
                "Evaluation targets differ between approaches.",
            )
    return rows, sequences, targets


# Feature Fusion and Availability Masking


def save_feature_blocks(
    config: Config,
    encounters: pd.DataFrame,
    sections: pd.DataFrame,
    rows: pd.DataFrame,
    numeric: np.ndarray,
    categorical: np.ndarray,
    physical_metadata: dict,
    diagnoses: sparse.csr_matrix,
    procedures: sparse.csr_matrix,
    medications: sparse.csr_matrix,
    text_has: np.ndarray,
    embedding_metadata: dict,
) -> dict:
    indices = rows["encounter_index"].to_numpy(np.int64)
    current = rows["view"].eq("current").to_numpy()
    note_masked = rows["view"].eq("history_note_masked").to_numpy()
    np.save(
        config.output / "physical_numeric.npy", numeric[indices], allow_pickle=False
    )
    np.save(
        config.output / "physical_categorical.npy",
        categorical[indices],
        allow_pickle=False,
    )
    sparse_matrices = {
        "diagnoses_icd10": diagnoses,
        "procedures_icd10": procedures,
        "medication_frequency": medications,
    }
    sizes = {}
    keep = sparse.diags((~current).astype(np.float32))
    for name, base in sparse_matrices.items():
        matrix = sparse.csr_matrix(keep @ base[indices])
        matrix.eliminate_zeros()
        require(
            matrix[current].nnz == 0,
            f"Current encounter contains unavailable {name} features.",
        )
        require(np.isfinite(matrix.data).all(), f"Nonfinite {name} features.")
        sparse.save_npz(config.output / f"{name}.npz", matrix)
        sizes[name] = matrix.shape[1]
    availability = text_has[indices].copy()
    availability[note_masked] = False
    availability[current, 2] = False
    if config.current_text_policy == "available_only":
        stored = pd.to_datetime(sections["note_available_time"]).to_numpy(
            dtype="datetime64[ns]"
        )[indices]
        admitted = encounters["admittime"].to_numpy(dtype="datetime64[ns]")[indices]
        allowed = ~np.isnat(stored) & (stored <= admitted)
        availability[current & ~allowed, :2] = False
    require(
        not availability[current, 2].any() and not availability[note_masked].any(),
        "Invalid text availability mask.",
    )
    np.save(
        config.output / "text_has.npy",
        availability.astype(np.uint8),
        allow_pickle=False,
    )
    parquet_write(config.output / "rows.parquet", rows)
    cards = [
        len(physical_metadata["categorical_vocabularies"][name])
        for name in PHYSICAL_CATEGORIES
    ]
    widths = [
        numeric.shape[1] + sum(cards),
        sizes["diagnoses_icd10"],
        sizes["procedures_icd10"],
        sizes["medication_frequency"],
    ] + [embedding_metadata["dimension"]] * 3
    offset, groups = 0, {}
    for name, width in zip(FEATURE_GROUPS, widths):
        groups[name] = {"start": offset, "stop": offset + width, "width": width}
        offset += width
    schema = {
        "feature_groups": groups,
        "dimension": offset,
        "n_rows": len(rows),
        "n_encounters": len(encounters),
        "categorical_cardinalities": cards,
        "physical_numeric_columns": physical_metadata["numeric_columns"],
        "physical_categorical_columns": list(PHYSICAL_CATEGORIES),
        "text_sections": list(TEXT_SECTIONS),
        "text_dimension": embedding_metadata["dimension"],
        "fusion": "Concatenate the seven ordered groups when loading a batch.",
        "text_storage": "One vector per encounter and section; row-view masks are applied by EncounterFeatureStore.gather.",
        "current_text_policy": config.current_text_policy,
        "current_text_provenance": "CC/HPI are extracted retrospectively from discharge summaries; section scrubbing cannot establish prospective availability.",
        "current_masks": [
            "diagnoses_icd10",
            "procedures_icd10",
            "medication_frequency",
            "bhc_hist_text",
        ],
        "history_text_rule": "Mask all note text if any note availability timestamp is unknown or later than target admission.",
        "labels": "LOS, log-LOS, and target prolonged status are stored only in target tables, outside feature fusion.",
        "sequence_metadata": "Role IDs and log gaps are separate metadata; no prior-LOS scalar enters the fused features.",
    }
    json_write(config.output / "feature_schema.json", schema)
    return schema


# Export of Sequence Arrays


def save_sequence_arrays(config: Config, sequences: dict, targets: dict) -> None:
    for approach in APPROACHES:
        for split in SPLITS:
            target = (
                targets[approach]
                .loc[targets[approach]["split"].eq(split)]
                .reset_index(drop=True)
            )
            frame = (
                sequences[approach]
                .loc[sequences[approach]["split"].eq(split)]
                .reset_index(drop=True)
            )
            row_ids = np.full((len(target), config.window), -1, dtype=np.int64)
            roles = np.full((len(target), config.window), -1, dtype=np.int8)
            gaps = np.zeros((len(target), config.window), dtype=np.float32)
            target_position = pd.Series(np.arange(len(target)), index=target["t_hadm"])
            ii = frame["t_hadm"].map(target_position).to_numpy(np.int64)
            jj = frame["seq_pos"].to_numpy(np.int64)
            row_ids[ii, jj] = frame["row_id"].to_numpy(np.int64)
            roles[ii, jj] = frame["seq_role"].map(ROLE_IDS).to_numpy(np.int8)
            gaps[ii, jj] = frame["gap_days_log"].to_numpy(np.float32)
            lengths = target["seq_len"].to_numpy(np.int32)
            mask = np.arange(config.window)[None, :] < lengths[:, None]
            require(
                np.array_equal(row_ids >= 0, mask),
                "Padded sequence masks are inconsistent.",
            )
            require(
                np.all(
                    roles[np.arange(len(target)), lengths - 1] == ROLE_IDS["current"]
                ),
                "Current role must end each padded sequence.",
            )
            parquet_write(config.output / f"targets_{approach}_{split}.parquet", target)
            parquet_write(
                config.output / f"sequences_{approach}_{split}.parquet",
                frame.drop(columns="view"),
            )
            np.savez_compressed(
                config.output / f"sequence_arrays_{approach}_{split}.npz",
                t_hadm=target["t_hadm"].to_numpy(np.int64),
                row_ids=row_ids,
                mask=mask,
                role_ids=roles,
                gap_days_log=gaps,
                lengths=lengths,
            )


# Loading of Encounter and Sequence Features


class EncounterFeatureStore:

    def __init__(self, directory: str | Path):
        self.directory = Path(directory)
        self.schema = json_read(self.directory / "feature_schema.json")
        self.rows = pd.read_parquet(self.directory / "rows.parquet")
        self.numeric = np.load(
            self.directory / "physical_numeric.npy", mmap_mode="r", allow_pickle=False
        )
        self.categorical = np.load(
            self.directory / "physical_categorical.npy",
            mmap_mode="r",
            allow_pickle=False,
        )
        self.text_has = np.load(
            self.directory / "text_has.npy", mmap_mode="r", allow_pickle=False
        )
        self.text = [
            np.load(
                self.directory / f"encounter_{section}.npy",
                mmap_mode="r",
                allow_pickle=False,
            )
            for section in TEXT_SECTIONS
        ]
        self.sparse_blocks = [
            sparse.load_npz(self.directory / f"{name}.npz").tocsr()
            for name in ("diagnoses_icd10", "procedures_icd10", "medication_frequency")
        ]

    def gather(self, row_ids: np.ndarray, max_bytes: int = 1 << 30) -> np.ndarray:
        ids = np.asarray(row_ids, dtype=np.int64).reshape(-1)
        require(
            np.all((ids >= 0) & (ids < len(self.rows))),
            "Feature row IDs are out of range.",
        )
        needed = len(ids) * self.schema["dimension"] * np.dtype(np.float32).itemsize
        require(
            needed <= max_bytes,
            "The requested dense feature batch exceeds max_bytes; use a smaller batch.",
        )
        features = np.zeros((len(ids), self.schema["dimension"]), dtype=np.float32)
        cursor = self.numeric.shape[1]
        features[:, :cursor] = self.numeric[ids]
        for column, cardinality in enumerate(self.schema["categorical_cardinalities"]):
            features[np.arange(len(ids)), cursor + self.categorical[ids, column]] = 1
            cursor += cardinality
        for matrix in self.sparse_blocks:
            width = matrix.shape[1]
            features[:, cursor : cursor + width] = matrix[ids].toarray()
            cursor += width
        encounter_ids = self.rows["encounter_index"].to_numpy(np.int64)[ids]
        for section, matrix in enumerate(self.text):
            width = matrix.shape[1]
            features[:, cursor : cursor + width] = (
                matrix[encounter_ids] * self.text_has[ids, section, None]
            )
            cursor += width
        require(
            cursor == features.shape[1] and np.isfinite(features).all(),
            "Invalid fused feature batch.",
        )
        return features

    def sequence_batch(
        self,
        approach: str,
        split: str,
        indices: Iterable[int],
        max_bytes: int = 1 << 30,
    ) -> dict[str, np.ndarray]:
        require(
            approach in APPROACHES and split in SPLITS, "Unknown approach or split."
        )
        indices = np.asarray(list(indices), dtype=np.int64)
        with np.load(
            self.directory / f"sequence_arrays_{approach}_{split}.npz",
            allow_pickle=False,
        ) as archive:
            require(
                np.all((indices >= 0) & (indices < len(archive["t_hadm"]))),
                "Target indices are out of range.",
            )
            batch = {key: archive[key][indices] for key in archive.files}
        pointers, mask = batch["row_ids"], batch["mask"]
        shape = (*pointers.shape, self.schema["dimension"])
        needed = math.prod(shape) * np.dtype(np.float32).itemsize
        require(
            needed <= max_bytes,
            "The dense sequence batch exceeds max_bytes; use a smaller batch.",
        )
        features = np.zeros(shape, dtype=np.float32)
        features[mask] = self.gather(pointers[mask], max_bytes=max_bytes)
        target = pd.read_parquet(
            self.directory / f"targets_{approach}_{split}.parquet",
            columns=["t_hadm", "los_days", "z"],
        ).iloc[indices]
        require(
            np.array_equal(batch["t_hadm"], target["t_hadm"].to_numpy()),
            "Target arrays and labels disagree.",
        )
        return {
            **batch,
            "features": features,
            "los_days": target["los_days"].to_numpy(np.float32),
            "z": target["z"].to_numpy(np.float32),
        }


# Validation of Processed Data


def validate_package(config: Config, schema: dict, targets: dict) -> dict:
    store = EncounterFeatureStore(config.output)
    checks = {"seven_feature_groups": tuple(schema["feature_groups"]) == FEATURE_GROUPS}
    columns = schema["feature_groups"]
    for view in ("current", "history", "history_note_masked"):
        rows = store.rows.loc[store.rows["view"].eq(view), "row_id"].to_numpy(np.int64)
        if not len(rows):
            continue
        chosen = rows[np.linspace(0, len(rows) - 1, min(32, len(rows))).astype(int)]
        features = store.gather(chosen)
        checks[f"{view}_fused_features_finite"] = bool(np.isfinite(features).all())
        masked = (
            [
                "diagnoses_icd10_cm",
                "procedures_icd10_pcs",
                "medication_frequency",
                "brief_hospital_course",
            ]
            if view == "current"
            else (list(FEATURE_GROUPS[-3:]) if view == "history_note_masked" else [])
        )
        for name in masked:
            checks[f"{view}_{name}_masked"] = not bool(
                features[:, columns[name]["start"] : columns[name]["stop"]].any()
            )
    for approach in APPROACHES:
        for split in SPLITS:
            count = int(targets[approach]["split"].eq(split).sum())
            if count:
                batch = store.sequence_batch(
                    approach, split, [0, count - 1] if count > 1 else [0]
                )
                checks[f"{approach}_{split}_batch"] = bool(
                    np.isfinite(batch["features"]).all()
                    and not batch["features"][~batch["mask"]].any()
                )
    require(
        all(checks.values()),
        f"Preprocessing validation failed: {[key for key, value in checks.items() if not value]}",
    )
    return checks


# Execution of the Pre-Processing Pipeline


def run(config: Config) -> Path:
    config.output.mkdir(parents=True, exist_ok=True)
    marker = config.output / "preprocessing_manifest.json"
    marker.unlink(missing_ok=True)
    source_paths = [
        config.raw if config.raw.is_file() else locate_table(config, "admissions"),
        locate_table(config, "patients"),
        locate_table(config, "omr"),
        locate_table(config, "diagnoses_icd"),
        locate_table(config, "procedures_icd"),
        locate_table(config, "prescriptions"),
        locate_table(config, "discharge"),
    ]
    validate_encoder(config)
    LOGGER.info("1/8: Cohort, patient splits, and train-only targets.")
    encounters, cohort_metadata = prepare_cohort(config)
    LOGGER.info("2/8: Physical characteristics and categorical encoding.")
    numeric, categorical, physical_metadata = prepare_physical(config, encounters)
    LOGGER.info("3/8: ICD-10-CM and ICD-10-PCS multi-hot encoding.")
    diagnoses, diagnosis_metadata = prepare_codes(config, encounters, "diagnoses")
    procedures, procedure_metadata = prepare_codes(config, encounters, "procedures")
    LOGGER.info("4/8: Medication-frequency vectors.")
    medications, medication_metadata = prepare_medications(config, encounters)
    LOGGER.info("5/8: Chief complaint, present illness, and brief hospital course.")
    sections = prepare_sections(config, encounters)
    LOGGER.info("6/8: Frozen text embeddings and train-only normalization.")
    _, text_has, embedding_metadata = prepare_embeddings(config, encounters, sections)
    LOGGER.info("7/8: Chronological A/B/C sequences and encounter feature fusion.")
    rows, sequences, targets = prepare_sequences(config, encounters, sections)
    schema = save_feature_blocks(
        config,
        encounters,
        sections,
        rows,
        numeric,
        categorical,
        physical_metadata,
        diagnoses,
        procedures,
        medications,
        text_has,
        embedding_metadata,
    )
    save_sequence_arrays(config, sequences, targets)
    del diagnoses, procedures, medications, numeric, categorical, sequences
    gc.collect()
    LOGGER.info("8/8: Feature availability and sequence validation.")
    checks = validate_package(config, schema, targets)
    manifest = {
        "status": "complete",
        "pipeline": "MIMIC-IV sequential preprocessing",
        "feature_groups": list(FEATURE_GROUPS),
        "config": asdict(config),
        "cohort": cohort_metadata,
        "n_feature_rows": len(rows),
        "n_fused_features": schema["dimension"],
        "targets": {
            approach: {
                split: int(targets[approach]["split"].eq(split).sum())
                for split in SPLITS
            }
            for approach in APPROACHES
        },
        "approaches": {
            "aprA": "All train targets and full eligible history with one history role.",
            "aprB": "All train targets and full eligible history tagged short or prolonged.",
            "aprC": "Prolonged train targets and prolonged eligible history only.",
        },
        "evaluation": "Identical prolonged validation and test stays for all approaches.",
        "current_text_policy": config.current_text_policy,
        "source_signatures": [source_signature(path) for path in source_paths],
        "diagnosis_vocabulary_size": len(diagnosis_metadata["vocabulary"]),
        "procedure_vocabulary_size": len(procedure_metadata["vocabulary"]),
        "medication_vocabulary_size": len(medication_metadata["vocabulary"]),
        "assertions": checks,
    }
    json_write(marker, manifest)
    LOGGER.info("Sequence preprocessing complete: %s", config.output)
    return config.output


# Command-Line Configuration


def parse_arguments(argv: list[str] | None = None) -> Config:
    parser = argparse.ArgumentParser(
        description="MIMIC-IV feature preprocessing and chronological encounter-sequence construction."
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=Path("data/mimic_iv"),
        help="MIMIC-IV directory containing hosp and note subdirectories.",
    )
    for name in (
        "output",
        "raw",
        "tables",
        "roles",
        "code-maps",
        "medication-map",
        "rxnav-cache",
        "encoder",
        "pretraining-roles",
    ):
        parser.add_argument("--" + name, type=Path)
    parser.add_argument(
        "--rebuild-split",
        action="store_true",
        help="Recompute patient roles even when a saved split is available.",
    )
    parser.add_argument(
        "--current-text-policy",
        choices=("retrospective_proxy", "available_only"),
        default="retrospective_proxy",
        help="Use retrospective admission-text sections or require note availability at admission.",
    )
    parser.add_argument("--window", type=int, default=16)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--chunk-size", type=int, default=200_000)
    parser.add_argument("--note-chunk-size", type=int, default=20_000)
    parser.add_argument("--minimum-category-count", type=int, default=50)
    parser.add_argument("--minimum-code-count", type=int, default=20)
    parser.add_argument("--minimum-medication-count", type=int, default=50)
    parser.add_argument("--text-window", type=int, default=8192)
    parser.add_argument("--text-overlap", type=int, default=1024)
    parser.add_argument("--text-token-budget", type=int, default=32768)
    parser.add_argument("--text-shard-size", type=int, default=4096)
    parser.add_argument("--device", default="auto", help="auto, cpu, cuda, or cuda:N")
    arguments = vars(parser.parse_args(argv))
    root = arguments["root"].expanduser().resolve()
    defaults = {
        "root": root,
        "output": root / "processed",
        "raw": root / "encounters.csv",
        "tables": root,
        "roles": root / "splits" / "encounter_roles.parquet",
        "code_maps": root / "mappings",
        "medication_map": root / "mappings" / "ndc_atc_map.parquet",
        "rxnav_cache": root / "mappings" / "rxnav_cache",
        "encoder": root / "text_encoder",
        "pretraining_roles": root / "splits" / "pretraining_roles.parquet",
    }
    for name, default in defaults.items():
        arguments[name] = (arguments.get(name) or default).expanduser().resolve()
    config = Config(**arguments)
    for name in (
        "window",
        "chunk_size",
        "note_chunk_size",
        "minimum_category_count",
        "minimum_code_count",
        "minimum_medication_count",
        "text_token_budget",
        "text_shard_size",
    ):
        require(getattr(config, name) > 0, f"{name} must be positive.")
    content_windows(1, config.text_window, config.text_overlap)
    require(
        config.output != config.root,
        "The output directory must differ from the input directory.",
    )
    for directory in (
        config.tables,
        config.encoder,
        config.roles.parent,
        config.code_maps,
        config.rxnav_cache,
    ):
        require(
            config.output != directory, "Output must not overwrite a source directory."
        )
    return config


def main(argv: list[str] | None = None) -> None:
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    run(parse_arguments(argv))


if __name__ == "__main__":
    main()
