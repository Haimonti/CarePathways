# Imports

from __future__ import annotations

import argparse
import copy
import hashlib
import importlib.util
import json
import logging
import math
import os
import random
import sys
import time
from dataclasses import asdict, dataclass, fields
from pathlib import Path

import numpy as np
import pandas as pd
import scipy.sparse as sparse
from scipy.stats import rankdata, spearmanr
import torch
from torch import nn
import torch.nn.functional as F

# Configuration

LOGGER = logging.getLogger("mimic_iv_sequential_model")
FEATURES = (
    "physical_characteristics",
    "diagnoses_icd10_cm",
    "procedures_icd10_pcs",
    "medication_frequency",
    "chief_complaint",
    "history_of_present_illness",
    "brief_hospital_course",
)
TEXT_SECTIONS = ("cc_adm_text", "hpi_adm_text", "bhc_hist_text")
SPLITS = ("train", "val", "test")
APPROACHES = ("aprA", "aprB", "aprC")
ROLES = {"current": 0, "hist": 1, "short_hist": 2, "prol_hist": 3}
VIEWS = {"current": 0, "history": 1, "history_note_masked": 2}
BANDS = np.asarray([0.0, 7.0, 14.0, 30.0, 60.0, 90.0, 150.0, np.inf])
REFERENCES = {
    "rank_n_contrast": "https://proceedings.neurips.cc/paper_files/paper/2023/hash/39e9c5913c970e3e49c2df629daff636-Abstract-Conference.html",
    "ordered_proxy_inspiration": "https://proceedings.mlr.press/v267/lim25a.html",
}


@dataclass
class Config:
    data: str = "data/mimic_iv/processed_all_stays"
    output: str = "runs/mimic_iv_sequential"
    approach: str = "aprB"
    admissions: str | None = None
    landmarks: tuple = (
        0.0,
        3.0,
        7.0,
        14.0,
        21.0,
        28.0,
        30.0,
        35.0,
        42.0,
        56.0,
        70.0,
        90.0,
        120.0,
    )
    include_prolonged_landmark: bool = True
    allow_prolonged_evaluation: bool = False
    require_prospective_text: bool = False
    use_prior_los: bool = True
    d_model: int = 192
    encoder_layers: int = 2
    encoder_heads: int = 4
    memory_heads: int = 3
    fusion_dim: int = 256
    adapters: int = 4
    adapter_rank: int = 16
    representation_dim: int = 32
    proxy_count: int = 12
    dropout: float = 0.1
    head_dropout: float = 0.2
    modality_dropout: float = 0.05
    history_dropout: float = 0.05
    tail_start: float = 60.0
    xi_max: float = 0.45
    xi_initial: float = 0.15
    batch_size: int = 128
    balanced_batch_size: int = 32
    eval_batch_size: int = 256
    epochs: int = 30
    minimum_epochs: int = 2
    patience: int = 6
    evaluations_per_epoch: int = 2
    learning_rate: float = 3e-4
    weight_decay: float = 1e-2
    gradient_clip: float = 1.0
    ema_decay: float = 0.995
    admission_sampling_share: float = 0.5
    landmark_tempering: float = 0.5
    crps_weight: float = 0.25
    rank_weight: float = 0.05
    proxy_weight: float = 0.05
    temporal_weight: float = 0.5
    router_weight: float = 0.01
    orthogonality_weight: float = 1e-3
    balance_weight: float = 1e-3
    contrastive_temperature: float = 0.2
    quadrature_points: int = 96
    selection_tail_weight: float = 0.25
    minimum_selection_cases: int = 20
    checkpoint_seconds: int = 120
    bootstrap_samples: int = 200
    seed: int = 42
    device: str = "auto"
    threads: int = 4
    ablate: tuple = ()


# Reproducibility and File Utilities


def require(condition, message):
    if not condition:
        raise ValueError(message)


def read_json(path):
    with Path(path).open(encoding="utf-8") as handle:
        return json.load(handle)


def plain(value):
    if isinstance(value, dict):
        return {str(key): plain(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [plain(item) for item in value]
    if isinstance(value, np.ndarray):
        return plain(value.tolist())
    if isinstance(value, np.generic):
        return plain(value.item())
    if isinstance(value, float) and not math.isfinite(value):
        return None
    if isinstance(value, Path):
        return str(value)
    return value


def write_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(
        json.dumps(plain(value), indent=2, allow_nan=False), encoding="utf-8"
    )
    temporary.replace(path)


def save_checkpoint(path, value):
    path = Path(path)
    temporary = path.with_name(path.name + ".tmp")
    torch.save(value, temporary)
    temporary.replace(path)


def seed_everything(seed, threads):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.set_num_threads(threads)
    torch.backends.cudnn.benchmark = False
    torch.backends.cudnn.deterministic = True


def device_for(name):
    if name == "auto":
        return torch.device("cuda" if torch.cuda.is_available() else "cpu")
    return torch.device(name)


def snapshot(model):
    return {
        name: value.detach().cpu().clone() for name, value in model.state_dict().items()
    }


def random_state(generator):
    return {
        "python": random.getstate(),
        "numpy": np.random.get_state(),
        "generator": generator.bit_generator.state,
        "torch": torch.get_rng_state(),
        "cuda": torch.cuda.get_rng_state_all() if torch.cuda.is_available() else None,
    }


def restore_random_state(state, generator):
    random.setstate(state["python"])
    np.random.set_state(state["numpy"])
    generator.bit_generator.state = state["generator"]
    torch.set_rng_state(state["torch"].cpu())
    if torch.cuda.is_available() and state["cuda"] is not None:
        torch.cuda.set_rng_state_all([value.cpu() for value in state["cuda"]])


# Construction of Complete Short-Stay and Long-Stay Evaluation Cohorts


def complete_sequences(config, encounters, sections):
    records = {"aprA": [], "aprC": []}
    chosen = {"aprA": [], "aprC": []}
    times = pd.to_datetime(sections["note_available_time"]).to_numpy(
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
        ix = group.index.to_numpy(np.int64)
        admission = group["admittime"].to_numpy(dtype="datetime64[ns]")
        discharge = group["dischtime"].to_numpy(dtype="datetime64[ns]")
        ids = group["hadm_id"].to_numpy(np.int64)
        prolonged = group["prolonged"].to_numpy(bool)
        split = str(group["role"].iloc[0])
        for position, target in enumerate(ids):
            prior = np.flatnonzero(
                (admission[:position] < admission[position])
                & (discharge[:position] <= admission[position])
            )
            for approach in records:
                if approach == "aprC" and not prolonged[position]:
                    continue
                history = prior[prolonged[prior]] if approach == "aprC" else prior
                history = (
                    history[-(config.window - 1) :]
                    if config.window > 1
                    else history[:0]
                )
                chosen[approach].append(int(ix[position]))
                length = len(history) + 1
                for seq_pos, past in enumerate(history):
                    available = times[ix[past]]
                    view = (
                        "history"
                        if not np.isnat(available) and available <= admission[position]
                        else "history_note_masked"
                    )
                    gap = float(
                        (admission[position] - discharge[past]) / np.timedelta64(1, "D")
                    )
                    role = "prol_hist" if approach == "aprC" else "hist"
                    records[approach].append(
                        (
                            int(target),
                            int(subject),
                            split,
                            int(ids[past]),
                            False,
                            role,
                            seq_pos,
                            length,
                            math.log1p(gap),
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
    sequences = {
        key: pd.DataFrame(value, columns=columns) for key, value in records.items()
    }
    sequences["aprB"] = sequences["aprA"].copy()
    history = ~sequences["aprB"]["is_current"].astype(bool)
    prolonged_by_id = encounters.set_index("hadm_id")["prolonged"]
    sequences["aprB"].loc[history, "seq_role"] = np.where(
        sequences["aprB"].loc[history, "hadm_id"].map(prolonged_by_id),
        "prol_hist",
        "short_hist",
    )
    chosen["aprB"] = chosen["aprA"].copy()
    rows = pd.concat(
        [frame[["hadm_id", "view"]] for frame in sequences.values()], ignore_index=True
    ).drop_duplicates()
    rows["order"] = rows["view"].map(VIEWS)
    rows = (
        rows.sort_values(["order", "hadm_id"])
        .drop(columns="order")
        .reset_index(drop=True)
    )
    rows["row_id"] = np.arange(len(rows), dtype=np.int64)
    position = pd.Series(np.arange(len(encounters)), index=encounters["hadm_id"])
    rows["encounter_index"] = rows["hadm_id"].map(position).astype(np.int64)
    lookup = pd.MultiIndex.from_frame(rows[["hadm_id", "view"]])
    targets = {}
    for approach in APPROACHES:
        frame = (
            sequences[approach]
            .sort_values(["t_hadm", "seq_pos"])
            .reset_index(drop=True)
        )
        frame["row_id"] = lookup.get_indexer(
            pd.MultiIndex.from_frame(frame[["hadm_id", "view"]])
        )
        require(frame["row_id"].ge(0).all(), "Missing encounter feature view.")
        current = frame.loc[frame["is_current"].astype(bool)].set_index("t_hadm")
        target = encounters.loc[
            chosen[approach],
            ["hadm_id", "subject_id", "role", "los_days", "z", "prolonged"],
        ].copy()
        target = (
            target.rename(columns={"hadm_id": "t_hadm", "role": "split"})
            .sort_values("t_hadm")
            .reset_index(drop=True)
        )
        target["current_row_id"] = (
            target["t_hadm"].map(current["row_id"]).astype(np.int64)
        )
        target["seq_len"] = target["t_hadm"].map(current["seq_len"]).astype(np.int32)
        targets[approach], sequences[approach] = target, frame
    return rows, sequences, targets


def prepare_data(script, arguments):
    script = Path(script).expanduser().resolve()
    require(script.is_file(), f"Preprocessing script not found: {script}")
    spec = importlib.util.spec_from_file_location(
        "mimic_preprocessing_components", script
    )
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    require(
        all(
            hasattr(module, name)
            for name in ("parse_arguments", "prepare_sequences", "run")
        ),
        "Use the seven-feature preprocessing script with parse_arguments, prepare_sequences, and run.",
    )
    arguments = list(arguments)
    if arguments and arguments[0] == "--":
        arguments.pop(0)
    config = module.parse_arguments(arguments)
    require(
        hasattr(config, "window") and hasattr(config, "current_text_policy"),
        "Incompatible preprocessing configuration.",
    )
    if not any(
        value == "--output" or value.startswith("--output=") for value in arguments
    ):
        from dataclasses import replace

        config = replace(config, output=config.root / "processed_all_stays")
    module.prepare_sequences = complete_sequences
    directory = module.run(config)
    manifest = read_json(directory / "preprocessing_manifest.json")
    manifest["evaluation"] = (
        "All valid validation/test stays for A/B; prolonged stays only for C."
    )
    manifest["complete_evaluation_cohort"] = True
    write_json(directory / "preprocessing_manifest.json", manifest)
    LOGGER.info("Complete-cohort feature package: %s", directory)


# Loading and Validation of the Seven Clinical Feature Groups


class FeatureStore:
    def __init__(self, config):
        self.config = config
        self.path = Path(config.data).expanduser().resolve()
        require(
            (self.path / "feature_schema.json").is_file(),
            "--data must point to a completed seven-feature preprocessing package containing feature_schema.json.",
        )
        self.schema = read_json(self.path / "feature_schema.json")
        require(
            tuple(self.schema["feature_groups"]) == FEATURES,
            "The package must contain exactly the seven selected feature groups.",
        )
        self.manifest = read_json(self.path / "preprocessing_manifest.json")
        require(
            self.manifest.get("status") == "complete",
            "The preprocessing package is incomplete.",
        )
        require(
            all(self.manifest.get("assertions", {}).values()),
            "Preprocessing checks did not all pass.",
        )
        self.rows = (
            pd.read_parquet(self.path / "rows.parquet")
            .sort_values("row_id")
            .reset_index(drop=True)
        )
        require(
            np.array_equal(self.rows["row_id"], np.arange(len(self.rows))),
            "Feature row IDs must be contiguous.",
        )
        self.encounters = pd.read_parquet(self.path / "encounters.parquet").reset_index(
            drop=True
        )
        require(self.encounters["hadm_id"].is_unique, "Duplicate encounters.")
        require(
            self.encounters.groupby("subject_id")["role"].nunique().eq(1).all(),
            "Patients cross train/validation/test splits.",
        )
        self.position = pd.Series(
            np.arange(len(self.encounters)), index=self.encounters["hadm_id"]
        )
        self.row_encounter = self.rows["encounter_index"].to_numpy(np.int64)
        require(
            np.all(
                (self.row_encounter >= 0) & (self.row_encounter < len(self.encounters))
            ),
            "Encounter feature indices are invalid.",
        )
        require(
            np.array_equal(
                self.encounters["hadm_id"].to_numpy()[self.row_encounter],
                self.rows["hadm_id"].to_numpy(),
            ),
            "Encounter and feature indices disagree.",
        )
        require(self.rows["view"].isin(VIEWS).all(), "Unknown encounter view.")
        self.views = self.rows["view"].map(VIEWS).to_numpy(np.int64)
        self.numeric = np.load(
            self.path / "physical_numeric.npy", mmap_mode="r", allow_pickle=False
        )
        self.categorical = np.load(
            self.path / "physical_categorical.npy", mmap_mode="r", allow_pickle=False
        )
        self.text_has = np.load(
            self.path / "text_has.npy", mmap_mode="r", allow_pickle=False
        )
        self.cards = self.schema["categorical_cardinalities"]
        self.codes = [
            sparse.load_npz(self.path / f"{name}.npz").tocsr()
            for name in ("diagnoses_icd10", "procedures_icd10", "medication_frequency")
        ]
        self.text = [
            np.load(
                self.path / f"encounter_{name}.npy", mmap_mode="r", allow_pickle=False
            )
            for name in TEXT_SECTIONS
        ]
        self.widths = [
            int(self.schema["feature_groups"][name]["width"]) for name in FEATURES
        ]
        require(
            self.numeric.shape[0] == len(self.rows)
            and self.categorical.shape == (len(self.rows), len(self.cards)),
            "Physical feature dimensions disagree.",
        )
        require(
            self.text_has.shape == (len(self.rows), 3),
            "Text availability dimensions disagree.",
        )
        require(
            self.widths[0] == self.numeric.shape[1] + sum(self.cards),
            "Physical schema width disagrees.",
        )
        for index, (matrix, width) in enumerate(zip(self.codes, self.widths[1:4])):
            require(
                matrix.shape == (len(self.rows), width)
                and np.isfinite(matrix.data).all(),
                f"Invalid structured feature block {index}.",
            )
            require(
                matrix[self.views == 0].nnz == 0,
                "Current stays contain unavailable diagnosis, procedure, or medication information.",
            )
        for matrix, width in zip(self.text, self.widths[4:]):
            require(
                matrix.shape == (len(self.encounters), width),
                "Text embedding dimensions disagree.",
            )
        require(
            not self.text_has[self.views == 0, 2].any(),
            "Current hospital-course text must be masked.",
        )
        require(
            not self.text_has[self.views == 2].any(),
            "Unavailable historical notes must be masked.",
        )
        self.text_policy = self.schema.get("current_text_policy", "unknown")
        if config.require_prospective_text:
            require(
                self.text_policy == "available_only",
                "Prospective mode requires preprocessing with --current-text-policy available_only.",
            )
        if self.text_policy != "available_only":
            LOGGER.warning(
                "Current CC/HPI use retrospective discharge-summary sections; this run is not a prospective admission-time evaluation."
            )
        self.threshold = float(self.manifest["cohort"]["T_plos_days"])
        self.death, self.cause_count = self.load_exit_labels(config.admissions)
        signatures = []
        for path in sorted(self.path.iterdir()):
            if path.is_file() and path.suffix in (".json", ".parquet", ".npy", ".npz"):
                stat = path.stat()
                signatures.append((path.name, stat.st_size, stat.st_mtime_ns))
        label_signature = hashlib.sha256(self.death.tobytes()).hexdigest()
        self.signature = hashlib.sha256(
            json.dumps([signatures, label_signature]).encode()
        ).hexdigest()

    def load_exit_labels(self, explicit):
        candidates = [Path(explicit).expanduser()] if explicit else []
        if not explicit:
            source = self.manifest.get("config", {})
            for key in ("tables", "root"):
                if source.get(key):
                    base = Path(source[key])
                    candidates.extend(
                        [
                            base / "admissions.csv.gz",
                            base / "hosp" / "admissions.csv.gz",
                            base / "admissions.csv",
                            base / "hosp" / "admissions.csv",
                        ]
                    )
        path = next((path for path in candidates if path.is_file()), None)
        if explicit:
            require(path is not None, f"Admissions table not found: {explicit}")
        if path is None:
            LOGGER.warning(
                "No admissions outcome labels found: training one all-exit cause; no death labels are inferred."
            )
            return np.zeros(len(self.encounters), dtype=np.int64), 1
        frame = pd.read_csv(path, usecols=["hadm_id", "hospital_expire_flag"])
        require(frame["hadm_id"].is_unique, "Duplicate admissions outcomes.")
        labels = frame.set_index("hadm_id")["hospital_expire_flag"].reindex(
            self.encounters["hadm_id"]
        )
        require(
            labels.notna().all() and labels.isin([0, 1]).all(),
            "Missing or invalid in-hospital exit labels.",
        )
        return labels.to_numpy(np.int64), 2

    def gather(self, ids, device):
        ids = np.asarray(ids, np.int64)
        require(
            np.all((ids >= 0) & (ids < len(self.rows))), "Feature index out of bounds."
        )
        physical = np.zeros((len(ids), self.widths[0]), np.float32)
        physical[:, : self.numeric.shape[1]] = self.numeric[ids]
        cursor = self.numeric.shape[1]
        for j, card in enumerate(self.cards):
            values = self.categorical[ids, j].astype(np.int64)
            require(
                np.all((values >= 0) & (values < card)), "Invalid categorical index."
            )
            physical[np.arange(len(ids)), cursor + values] = 1.0
            cursor += card
        groups = [physical] + [
            matrix[ids].toarray().astype(np.float32, copy=False)
            for matrix in self.codes
        ]
        encounter = self.row_encounter[ids]
        for j, matrix in enumerate(self.text):
            groups.append(
                np.asarray(matrix[encounter], np.float32) * self.text_has[ids, j, None]
            )
        presence = np.ones((len(ids), 7), dtype=bool)
        presence[:, 1:4] = np.column_stack(
            [np.any(group != 0, axis=1) for group in groups[1:4]]
        )
        presence[:, 4:] = self.text_has[ids].astype(bool)
        require(
            all(np.isfinite(group).all() for group in groups),
            "Nonfinite clinical feature values.",
        )
        return {
            "groups": [
                torch.from_numpy(np.array(group, copy=True)).to(device)
                for group in groups
            ],
            "present": torch.from_numpy(presence).to(device),
            "views": torch.from_numpy(self.views[ids]).to(device),
        }


# Chronological Sequence Metadata and Landmark Risk Sets


class SequenceData:
    def __init__(self, store, config, split):
        self.split = split
        self.store = store
        path = store.path
        self.targets = pd.read_parquet(
            path / f"targets_{config.approach}_{split}.parquet"
        ).reset_index(drop=True)
        with np.load(
            path / f"sequence_arrays_{config.approach}_{split}.npz", allow_pickle=False
        ) as archive:
            arrays = {name: archive[name] for name in archive.files}
        require(
            np.array_equal(arrays["t_hadm"], self.targets["t_hadm"]),
            "Sequence arrays and targets disagree.",
        )
        self.ids = self.targets["t_hadm"].to_numpy(np.int64)
        self.subjects = self.targets["subject_id"].to_numpy(np.int64)
        self.days = self.targets["los_days"].to_numpy(np.float64)
        self.row_ids = arrays["row_ids"].astype(np.int64)
        self.mask = arrays["mask"].astype(bool)
        self.roles = arrays["role_ids"].astype(np.int64)
        self.lengths = arrays["lengths"].astype(np.int64)
        require(
            self.row_ids.shape == self.mask.shape == self.roles.shape,
            "Sequence shapes disagree.",
        )
        require(
            np.all(self.lengths >= 1)
            and np.array_equal(self.mask.sum(1), self.lengths),
            "Invalid sequence lengths.",
        )
        require(
            np.array_equal(
                self.mask, np.arange(self.mask.shape[1])[None] < self.lengths[:, None]
            ),
            "Padding must follow the final current stay.",
        )
        require(
            np.array_equal(self.row_ids >= 0, self.mask),
            "Padding row IDs disagree with masks.",
        )
        safe = np.maximum(self.row_ids, 0)
        require(np.all(safe < len(store.rows)), "Unknown feature row in sequence.")
        positions = store.row_encounter[safe]
        target_positions = store.position.reindex(self.ids).to_numpy()
        require(np.isfinite(target_positions).all(), "Target encounter is missing.")
        target_positions = target_positions.astype(np.int64)
        cohort = store.encounters
        require(
            np.all(cohort["role"].to_numpy()[target_positions] == split),
            "A target belongs to the wrong split.",
        )
        require(
            np.all(
                cohort["subject_id"].to_numpy()[positions][self.mask]
                == np.broadcast_to(self.subjects[:, None], self.mask.shape)[self.mask]
            ),
            "Sequence history belongs to another patient.",
        )
        require(
            np.allclose(cohort["los_days"].to_numpy()[target_positions], self.days),
            "Outcome labels disagree.",
        )
        current = np.zeros_like(self.mask)
        current[np.arange(len(self.ids)), self.lengths - 1] = True
        require(np.all(self.roles[current] == 0), "Current encounter must be last.")
        require(
            np.all(store.views[safe[current]] == 0),
            "Current target must use its current feature view.",
        )
        require(
            np.array_equal(cohort["hadm_id"].to_numpy()[positions[current]], self.ids),
            "Current encounter ID does not match target.",
        )
        history = self.mask & ~current
        require(
            np.all((self.roles[history] >= 1) & (self.roles[history] <= 3)),
            "Invalid history role.",
        )
        admitted = pd.to_datetime(cohort["admittime"]).to_numpy(dtype="datetime64[ns]")
        discharged = pd.to_datetime(cohort["dischtime"]).to_numpy(
            dtype="datetime64[ns]"
        )
        target_time = admitted[target_positions, None]
        require(
            np.all((admitted[positions] < target_time)[history])
            and np.all((discharged[positions] <= target_time)[history]),
            "History contains concurrent or future encounters.",
        )
        note = pd.read_parquet(path / "text_sections.parquet")
        note_time = pd.to_datetime(
            note.set_index("hadm_id")["note_available_time"].reindex(cohort["hadm_id"])
        ).to_numpy(dtype="datetime64[ns]")
        readable = ~np.isnat(note_time[positions]) & (
            note_time[positions] <= target_time
        )
        require(
            not np.any(
                history & ~readable & np.any(store.text_has[safe].astype(bool), axis=-1)
            ),
            "Historical text was unavailable at prediction origin.",
        )
        gap = (target_time - discharged[positions]) / np.timedelta64(1, "D")
        self.gap = np.where(history, np.maximum(gap, 0.0), 0.0).astype(np.float32)
        self.prior_los = np.where(
            history, np.log1p(cohort["los_days"].to_numpy()[positions]), 0.0
        ).astype(np.float32)
        self.delta = np.zeros_like(self.gap)
        if self.row_ids.shape[1] > 1:
            difference = (
                admitted[positions[:, 1:]] - admitted[positions[:, :-1]]
            ) / np.timedelta64(1, "D")
            consecutive = history[:, 1:] & history[:, :-1]
            require(
                np.all(difference[consecutive] >= 0),
                "Historical encounters are not chronological.",
            )
            self.delta[:, 1:] = np.where(consecutive, np.maximum(difference, 0.0), 0.0)
        self.causes = store.death[target_positions]
        expected = cohort.loc[cohort["role"].eq(split)]
        if config.approach == "aprC":
            expected = expected.loc[expected["prolonged"]]
        self.complete = set(self.ids) == set(expected["hadm_id"])
        if split != "train" and config.approach != "aprC" and not self.complete:
            require(
                config.allow_prolonged_evaluation,
                "Validation/test omit short stays. Run the prepare subcommand with the seven-feature preprocessing script, or explicitly use --allow-prolonged-evaluation for a prolonged-only study.",
            )
        require(
            len(self.ids) > 0
            and np.isfinite(self.days).all()
            and np.all(self.days > 0),
            "Empty or invalid target cohort.",
        )


class LandmarkData:
    def __init__(self, sequence, landmarks):
        self.sequence = sequence
        rows, values, groups = [], [], []
        for group, landmark in enumerate(landmarks):
            eligible = np.flatnonzero(sequence.days > landmark)
            rows.append(eligible)
            values.append(np.full(len(eligible), landmark, np.float32))
            groups.append(np.full(len(eligible), group, np.int64))
        self.index = np.concatenate(rows)
        self.s = np.concatenate(values)
        self.group = np.concatenate(groups)
        self.total = sequence.days[self.index]
        self.remaining = self.total - self.s
        self.subject = sequence.subjects[self.index]
        self.cause = sequence.causes[self.index]
        self.band = np.searchsorted(BANDS[1:-1], self.total, side="left")
        self.landmarks = tuple(landmarks)
        require(len(self.index) > 0, "No stays are at risk at the requested landmarks.")

    def __len__(self):
        return len(self.index)

    def batch(self, ids, device, prior_los=True):
        ids = np.asarray(ids, np.int64)
        seq = self.sequence
        target = self.index[ids]
        row_ids = seq.row_ids[target]
        mask = seq.mask[target]
        unique, inverse = np.unique(row_ids[mask], return_inverse=True)
        pointers = np.zeros(row_ids.shape, np.int64)
        pointers[mask] = inverse
        inputs = seq.store.gather(unique, device)
        for name, array in {
            "pointers": pointers,
            "mask": mask,
            "roles": np.maximum(seq.roles[target], 0),
            "lengths": seq.lengths[target],
            "gap": seq.gap[target],
            "delta": seq.delta[target],
            "prior_los": (
                seq.prior_los[target]
                if prior_los
                else np.zeros_like(seq.prior_los[target])
            ),
            "s": self.s[ids],
        }.items():
            inputs[name] = torch.from_numpy(np.array(array, copy=True)).to(device)
        labels = {
            "remaining": torch.tensor(
                self.remaining[ids], device=device, dtype=torch.float32
            ),
            "total": torch.tensor(self.total[ids], device=device, dtype=torch.float32),
            "group": torch.tensor(self.group[ids], device=device, dtype=torch.long),
            "cause": torch.tensor(self.cause[ids], device=device, dtype=torch.long),
            "subject": torch.tensor(self.subject[ids], device=device, dtype=torch.long),
        }
        return inputs, labels


# Natural-Landmark and Balanced-Representation Sampling


class TrainingSampler:
    def __init__(self, data, config):
        self.data, self.config = data, config
        self.groups = [
            np.flatnonzero(data.group == k) for k in range(len(data.landmarks))
        ]
        counts = np.asarray([len(group) for group in self.groups], dtype=float)
        probability = counts**config.landmark_tempering
        probability[counts == 0] = 0.0
        probability /= probability.sum()
        zero = next((k for k, value in enumerate(data.landmarks) if value == 0), None)
        if zero is not None and counts[zero] and (counts > 0).sum() > 1:
            probability[zero] = 0.0
            probability *= (1 - config.admission_sampling_share) / probability.sum()
            probability[zero] = config.admission_sampling_share
        self.probability = probability
        self.cells = [
            [group[data.band[group] == band] for band in range(len(BANDS) - 1)]
            for group in self.groups
        ]

    def draw(self, generator):
        cfg = self.config
        selected = generator.choice(
            len(self.groups), cfg.batch_size, p=self.probability
        )
        natural = np.asarray(
            [self.groups[k][generator.integers(len(self.groups[k]))] for k in selected],
            np.int64,
        )
        focus = int(generator.choice(len(self.groups), p=self.probability))
        cells = [cell for cell in self.cells[focus] if len(cell)]
        probability = np.sqrt([len(cell) for cell in cells])
        probability /= probability.sum()
        selected = generator.choice(len(cells), cfg.balanced_batch_size, p=probability)
        balanced = np.asarray(
            [cells[k][generator.integers(len(cells[k]))] for k in selected], np.int64
        )
        return np.concatenate([natural, balanced])


# Seven-Token Encounter Representation Learning


class EncounterEncoder(nn.Module):
    def __init__(self, widths, config):
        super().__init__()
        d = config.d_model
        self.dropout_probability = config.modality_dropout
        self.projections = nn.ModuleList(
            [
                nn.Sequential(nn.Linear(width, d), nn.GELU(), nn.LayerNorm(d))
                for width in widths
            ]
        )
        self.types = nn.Parameter(torch.randn(7, d) * 0.02)
        self.states = nn.Parameter(torch.randn(7, 9, d) * 0.02)
        self.cls = nn.Parameter(torch.randn(1, 1, d) * 0.02)
        self.layers = nn.ModuleList(
            [
                nn.TransformerEncoderLayer(
                    d,
                    config.encoder_heads,
                    4 * d,
                    config.dropout,
                    activation="gelu",
                    batch_first=True,
                    norm_first=True,
                )
                for _ in range(config.encoder_layers)
            ]
        )
        self.norm = nn.LayerNorm(d)

    def forward(self, batch):
        present = batch["present"]
        dropped = torch.zeros_like(present)
        if self.training and self.dropout_probability:
            dropped = (
                torch.rand_like(present, dtype=torch.float32) < self.dropout_probability
            ) & present
            dropped[:, 0] = False
        keep = present & ~dropped
        tokens = torch.stack(
            [
                projection(value)
                for projection, value in zip(self.projections, batch["groups"])
            ],
            dim=1,
        )
        states = batch["views"][:, None] * 3 + torch.where(dropped, 2, present.long())
        tokens = (
            tokens * keep[..., None]
            + self.types
            + self.states[torch.arange(7, device=tokens.device)[None], states]
        )
        value = torch.cat([self.cls.expand(len(tokens), -1, -1), tokens], dim=1)
        for layer in self.layers:
            value = layer(value)
        return self.norm(value[:, 0])


# Time-Aware Recurrent Memory and Multi-Scale History Attention


class HistoryEncoder(nn.Module):
    def __init__(self, config):
        super().__init__()
        d = config.d_model
        self.config = config
        self.role_film = nn.Embedding(4, 2 * d)
        nn.init.zeros_(self.role_film.weight)
        self.time = nn.Sequential(nn.Linear(3, d), nn.GELU(), nn.Linear(d, d))
        self.decay = nn.Parameter(torch.full((d,), -2.0))
        self.gate = nn.Linear(3 * d + 1, d)
        self.candidate = nn.Linear(3 * d, d)
        self.query, self.key, self.value = (
            nn.Linear(d, d),
            nn.Linear(d, d),
            nn.Linear(d, d),
        )
        self.time_bias = nn.Linear(8, config.memory_heads)
        self.null = nn.Linear(d, config.memory_heads)
        self.memory_out = nn.Linear(d, d, bias=False)
        self.fusion_gate = nn.Linear(3 * d, 2)
        self.post = nn.Sequential(
            nn.LayerNorm(d),
            nn.Linear(d, 2 * d),
            nn.GELU(),
            nn.Dropout(config.dropout),
            nn.Linear(2 * d, d),
        )
        self.norm = nn.LayerNorm(d)
        self.register_buffer("rbf_centres", torch.linspace(0, 9, 8))

    def forward(self, encoded, batch):
        d = self.config.d_model
        values = encoded[batch["pointers"]]
        gamma, bias = self.role_film(batch["roles"]).chunk(2, dim=-1)
        values = values * (1 + gamma) + bias
        count, length, _ = values.shape
        rows = torch.arange(count, device=values.device)
        current = values[rows, batch["lengths"] - 1]
        history = batch["mask"].clone()
        history[rows, batch["lengths"] - 1] = False
        if "history" in self.config.ablate:
            history[:] = False
        if self.training and self.config.history_dropout:
            history &= (
                torch.rand(count, 1, device=values.device)
                >= self.config.history_dropout
            )
        log_gap = torch.log1p(batch["gap"])
        time_features = self.time(
            torch.stack(
                [
                    log_gap / 5.0,
                    batch["prior_los"] / 5.0,
                    torch.log1p(batch["delta"]) / 5.0,
                ],
                dim=-1,
            )
        )
        state, previous = torch.zeros_like(current), torch.zeros_like(current)
        rate = F.softplus(self.decay)
        for position in range(length - 1):
            mask = history[:, position, None]
            value = values[:, position]
            decayed = state * torch.exp(
                -rate * torch.log1p(batch["delta"][:, position, None]) / 5.0
            )
            novelty = (
                1 - F.cosine_similarity(value, previous, dim=-1, eps=1e-6)[:, None]
            )
            gate = torch.sigmoid(
                self.gate(
                    torch.cat(
                        [value, decayed, time_features[:, position], novelty], dim=-1
                    )
                )
            )
            candidate = torch.tanh(
                self.candidate(
                    torch.cat([value, decayed, time_features[:, position]], dim=-1)
                )
            )
            state = torch.where(mask, (1 - gate) * decayed + gate * candidate, state)
            previous = torch.where(mask, value, previous)
        has_history = history.any(1)
        recency = torch.where(history, batch["gap"], torch.inf).min(1).values
        recency = torch.where(has_history, recency, torch.zeros_like(recency))
        state = state * torch.exp(-rate * torch.log1p(recency[:, None]) / 5.0)
        heads = self.config.memory_heads
        width = d // heads
        query = self.query(current).view(count, heads, width)
        key = self.key(values + time_features).view(count, length, heads, width)
        value = self.value(values + time_features).view(count, length, heads, width)
        radial = torch.exp(-(((log_gap[..., None] - self.rbf_centres) / (9 / 7)) ** 2))
        scores = torch.einsum("bhd,blhd->blh", query, key) / math.sqrt(
            width
        ) + self.time_bias(radial)
        scores = scores.masked_fill(~history[..., None], -torch.inf)
        weights = torch.softmax(
            torch.cat([scores, self.null(current)[:, None]], dim=1), dim=1
        )
        memory = self.memory_out(
            torch.einsum("blh,blhd->bhd", weights[:, :length], value).reshape(count, d)
        )
        gates = (
            torch.sigmoid(self.fusion_gate(torch.cat([current, state, memory], dim=-1)))
            * has_history[:, None]
        )
        state = current + gates[:, :1] * state + gates[:, 1:] * memory
        return self.norm(state + self.post(state))


# Landmark Conditioning and Adaptive Low-Rank Experts


class LandmarkAdapters(nn.Module):
    def __init__(self, config):
        super().__init__()
        self.config = config
        d = config.fusion_dim
        self.embedding = nn.Sequential(nn.Linear(6, 32), nn.GELU(), nn.Linear(32, 32))
        self.projection = nn.Sequential(
            nn.Linear(config.d_model + 32, d),
            nn.GELU(),
            nn.Dropout(config.head_dropout),
            nn.Linear(d, d),
            nn.LayerNorm(d),
        )
        self.film = nn.Linear(32, 2 * d)
        nn.init.zeros_(self.film.weight)
        nn.init.zeros_(self.film.bias)
        self.down = nn.Parameter(
            torch.randn(config.adapters, config.adapter_rank, d) / math.sqrt(d)
        )
        self.up = nn.Parameter(torch.zeros(config.adapters, d, config.adapter_rank))
        self.router = nn.Linear(d + 32, config.adapters)

    def forward(self, state, landmark):
        time_value = torch.log1p(landmark) / math.log1p(365.0)
        features = torch.stack(
            [
                time_value,
                landmark / 365.0,
                torch.sin(2 * math.pi * time_value),
                torch.cos(2 * math.pi * time_value),
                torch.sin(4 * math.pi * time_value),
                torch.cos(4 * math.pi * time_value),
            ],
            dim=-1,
        )
        embedded = self.embedding(features)
        latent = self.projection(torch.cat([state, embedded], dim=-1))
        gamma, bias = self.film(embedded).chunk(2, dim=-1)
        latent = latent * (1 + gamma) + bias
        logits = self.router(torch.cat([latent, embedded], dim=-1))
        probability = logits.softmax(-1)
        if "adapters" not in self.config.ablate:
            residual = torch.einsum(
                "edr,ber->bed",
                self.up,
                F.gelu(torch.einsum("erd,bd->ber", self.down, latent)),
            )
            latent = latent + (probability[..., None] * residual).sum(1)
        return latent, logits, probability


# Competing-Exit Hazard Distribution and Generalized Pareto Tail


def hazard_edges(upper):
    values = np.concatenate(
        [
            [0.0, 0.25, 0.5],
            np.arange(1.0, 8.0, 1.0),
            np.arange(8.0, 31.0, 2.0),
            np.arange(35.0, upper, 5.0),
            [upper],
        ]
    )
    return np.unique(values[(values >= 0) & (values <= upper)]).astype(np.float32)


class ExitDistribution:
    def __init__(self, output, edges):
        self.edges = edges
        self.widths = edges[1:] - edges[:-1]
        self.log_hazard = output["hazard"].float().log_softmax(-1)
        stay = self.log_hazard[:, :, 0]
        self.log_survival_edges = torch.cat(
            [torch.zeros_like(stay[:, :1]), stay.cumsum(1)], dim=1
        )
        self.log_cause_mass = (
            self.log_survival_edges[:, :-1, None] + self.log_hazard[:, :, 1:]
        )
        self.log_mass = self.log_cause_mass.logsumexp(-1)
        self.mass = self.log_mass.exp()
        self.log_tail_mass = self.log_survival_edges[:, -1]
        self.tail_mass = self.log_tail_mass.exp()
        self.scale = output["scale"].float()
        self.xi = output["xi"].float()
        self.tail_causes = output["tail_causes"].float().log_softmax(-1)

    def nll(self, remaining, cause=None):
        remaining = remaining.float().clamp_min(1e-6)
        index = torch.searchsorted(
            self.edges[1:].contiguous(), remaining.contiguous(), right=True
        ).clamp_max(len(self.widths) - 1)
        row = torch.arange(len(remaining), device=remaining.device)
        if cause is None:
            body = self.log_mass[row, index] - self.widths[index].log()
            tail_cause = torch.zeros_like(remaining)
        else:
            body = self.log_cause_mass[row, index, cause] - self.widths[index].log()
            tail_cause = self.tail_causes[row, cause]
        excess = (remaining - self.edges[-1]).clamp_min(0)
        tail = (
            self.log_tail_mass
            - self.scale.log()
            - (1 + 1 / self.xi) * torch.log1p(self.xi * excess / self.scale)
            + tail_cause
        )
        return -torch.where(remaining >= self.edges[-1], tail, body)

    def log_survival(self, times):
        times = times.float()
        if times.ndim == 1:
            times = times[:, None]
        require(
            times.shape[0] == len(self.mass),
            "Survival times must have one row per patient.",
        )
        index = torch.searchsorted(
            self.edges[1:].contiguous(), times.contiguous(), right=True
        ).clamp_max(len(self.widths) - 1)
        fraction = ((times - self.edges[index]) / self.widths[index]).clamp(0, 1)
        probability_exit = -torch.expm1(self.log_hazard[:, :, 0]).gather(1, index)
        body = self.log_survival_edges[:, :-1].gather(1, index) + torch.log1p(
            -(fraction * probability_exit).clamp(max=1 - 1e-7)
        )
        tail = (
            self.log_tail_mass[:, None]
            - torch.log1p(
                self.xi * (times - self.edges[-1]).clamp_min(0) / self.scale[:, None]
            )
            / self.xi
        )
        result = torch.where(times >= self.edges[-1], tail, body)
        return torch.where(times <= 0, torch.zeros_like(result), result)

    def survival(self, times):
        return self.log_survival(times).exp()

    def mean(self):
        midpoints = (self.edges[1:] + self.edges[:-1]) / 2
        return (self.mass * midpoints).sum(1) + self.tail_mass * (
            self.edges[-1] + self.scale / (1 - self.xi)
        )

    def quantile(self, q):
        require(0 < q < 1, "Quantile must lie strictly between zero and one.")
        cdf = -torch.expm1(self.log_survival_edges)
        index = torch.sum(cdf[:, 1:] < q, dim=1).clamp_max(len(self.widths) - 1)
        rows = torch.arange(len(self.mass), device=self.mass.device)
        body = self.edges[index] + self.widths[index] * (
            (q - cdf[rows, index]) / self.mass[rows, index].clamp_min(1e-30)
        ).clamp(0, 1)
        log_ratio = (self.log_tail_mass - math.log1p(-q)).clamp_min(0)
        tail = self.edges[-1] + self.scale / self.xi * torch.expm1(self.xi * log_ratio)
        return torch.where(q <= cdf[:, -1], body, tail)

    def death_probability(self):
        if self.log_cause_mass.shape[-1] == 1:
            return None
        return (
            self.log_cause_mass[:, :, 1].exp().sum(1)
            + self.tail_mass * self.tail_causes[:, 1].exp()
        )


class SequentialExitModel(nn.Module):
    def __init__(self, widths, config, cause_count, init):
        super().__init__()
        self.config = config
        self.encounter = EncounterEncoder(widths, config)
        self.history = HistoryEncoder(config)
        self.adapters = LandmarkAdapters(config)
        edges = torch.tensor(hazard_edges(config.tail_start))
        self.register_buffer("edges", edges)
        self.head = nn.Sequential(
            nn.Dropout(config.head_dropout),
            nn.Linear(config.fusion_dim, (len(edges) - 1) * (cause_count + 1)),
        )
        nn.init.normal_(self.head[-1].weight, std=0.001)
        self.head[-1].bias.data.copy_(torch.as_tensor(init["hazard"]).flatten())
        self.scale_head = nn.Linear(config.fusion_dim, 1)
        nn.init.zeros_(self.scale_head.weight)
        scale = max(float(init["scale"]) - 0.1, 0.01)
        self.scale_head.bias.data.fill_(scale + math.log(-math.expm1(-scale)))
        self.tail_cause_head = nn.Linear(config.fusion_dim, cause_count)
        nn.init.zeros_(self.tail_cause_head.weight)
        self.tail_cause_head.bias.data.copy_(torch.as_tensor(init["tail_causes"]))
        self.xi_raw = nn.Parameter(
            torch.tensor(
                math.log(config.xi_initial / (config.xi_max - config.xi_initial))
            )
        )
        self.representation = nn.Sequential(
            nn.Linear(config.fusion_dim, config.fusion_dim),
            nn.GELU(),
            nn.Linear(config.fusion_dim, config.representation_dim),
        )
        angle = torch.linspace(0, math.pi, config.proxy_count)
        arc = torch.zeros(config.proxy_count, config.representation_dim)
        arc[:, 0], arc[:, 1] = angle.cos(), angle.sin()
        self.register_buffer("proxy_arc", arc)
        self.proxy_offset = nn.Parameter(torch.zeros_like(arc))
        self.cause_count = cause_count

    def forward(self, batch):
        state = self.history(self.encounter(batch), batch)
        latent, router, probability = self.adapters(state, batch["s"])
        output = {
            "hazard": self.head(latent).view(
                -1, len(self.edges) - 1, self.cause_count + 1
            ),
            "scale": F.softplus(self.scale_head(latent).squeeze(-1)) + 0.1,
            "xi": self.config.xi_max * self.xi_raw.sigmoid().clamp_min(1e-4),
            "tail_causes": self.tail_cause_head(latent),
            "representation": F.normalize(self.representation(latent), dim=-1),
            "router": router,
            "router_probability": probability,
        }
        return output

    def distribution(self, output):
        return ExitDistribution(output, self.edges)


# Train-Only Distribution Initialization and Ordered Target Anchors


def training_statistics(data, sampler, config, cause_count):
    edges = hazard_edges(config.tail_start).astype(np.float64)
    weights = np.zeros(len(data), np.float64)
    centres = []
    constants = {}
    for group, members in enumerate(sampler.groups):
        if len(members):
            weights[members] = sampler.probability[group] / len(members)
            remaining = data.remaining[members]
            centres.append(
                np.quantile(
                    np.log1p(remaining), np.linspace(0.02, 0.98, config.proxy_count)
                )
            )
            constants[str(group)] = {
                "median": float(np.median(remaining)),
                "mean": float(np.mean(remaining)),
                "n": len(members),
            }
        else:
            centres.append(
                np.linspace(0, math.log1p(config.tail_start), config.proxy_count)
            )
    weights *= len(data)
    hazard = []
    for low, high in zip(edges[:-1], edges[1:]):
        risk = weights[data.remaining >= low].sum()
        event = (data.remaining >= low) & (data.remaining < high)
        count = np.asarray(
            [
                weights[event & (data.cause == cause)].sum()
                for cause in range(cause_count)
            ]
        )
        probabilities = np.concatenate(
            [[max(risk - count.sum(), 0.0) + 1.0], count + 0.25]
        )
        probabilities /= probabilities.sum()
        hazard.append(np.log(probabilities))
    tail = data.remaining >= config.tail_start
    excess = data.remaining[tail] - config.tail_start
    scale = (
        float(np.average(excess, weights=weights[tail]) * (1 - config.xi_initial))
        if np.any(tail) and weights[tail].sum() > 0
        else 30.0
    )
    counts = np.asarray(
        [
            weights[tail & (data.cause == cause)].sum() + 0.5
            for cause in range(cause_count)
        ]
    )
    counts /= counts.sum()
    return {
        "hazard": np.asarray(hazard, np.float32),
        "scale": max(scale, 1.0),
        "tail_causes": np.log(counts).astype(np.float32),
        "proxy_centres": np.asarray(centres, np.float32),
        "constants": constants,
    }


# Threshold-Weighted Proper Scoring and Rank-Aware Representation Learning


def rank_contrastive_loss(embedding, outcome, group, subject, temperature):
    if len(embedding) < 3:
        return embedding.sum() * 0.0
    similarity = -torch.cdist(embedding.float(), embedding.float(), p=2) / temperature
    distance = (outcome[:, None] - outcome[None]).abs()
    valid = (group[:, None] == group[None]) & (subject[:, None] != subject[None])
    losses = []
    for row in range(len(embedding)):
        index = valid[row].nonzero(as_tuple=True)[0]
        if len(index) < 2:
            continue
        order = torch.argsort(distance[row, index], stable=True)
        index = index[order]
        distances = distance[row, index].contiguous()
        logits = similarity[row, index]
        denominator = torch.logcumsumexp(logits.flip(0), 0).flip(0)
        first_equal = torch.searchsorted(distances, distances, right=False)
        losses.append((denominator[first_equal] - logits).mean())
    return torch.stack(losses).mean() if losses else embedding.sum() * 0.0


class TrainingObjective(nn.Module):
    def __init__(self, config, statistics):
        super().__init__()
        self.config = config
        nodes, weights = np.polynomial.legendre.leggauss(config.quadrature_points)
        unit = (nodes + 1.0) / 2.0
        times = 30.0 * unit / (1.0 - unit)
        weights = weights / 2.0 * 30.0 / (1.0 - unit) ** 2
        self.register_buffer("times", torch.tensor(times, dtype=torch.float32))
        self.register_buffer("weights", torch.tensor(weights, dtype=torch.float32))
        self.register_buffer("centres", torch.as_tensor(statistics["proxy_centres"]))
        self.register_buffer(
            "temporal_horizons", torch.tensor([1.0, 3.0, 7.0, 14.0, 30.0, 60.0])
        )

    def crps(self, distribution, landmark, remaining, weighted=True):
        times = self.times[None].expand(len(remaining), -1)
        survival = distribution.survival(times)
        observed = (remaining[:, None] > times).float()
        weight = torch.ones_like(times)
        if weighted:
            total = times + landmark[:, None]
            weight = (
                weight
                + (total > 30).float()
                + (total > 60).float()
                + (total > 100).float()
            )
        return ((survival - observed).square() * self.weights * weight).sum(1)

    def forward(self, model, teacher, batch, labels, natural_count, ramp):
        cfg = self.config
        output = model(batch)
        distribution = model.distribution(output)
        zero = output["representation"].sum() * 0.0
        nll = distribution.nll(labels["remaining"], labels["cause"])
        crps = self.crps(distribution, batch["s"], labels["remaining"])
        loss = (
            nll[:natural_count].mean()
            + cfg.crps_weight * crps[:natural_count].mean() / 30.0
        )
        rank, proxy, route, temporal, orthogonality, balance = [zero] * 6
        if len(nll) > natural_count and "ranking" not in cfg.ablate:
            embedding = output["representation"][natural_count:]
            value = torch.log1p(labels["remaining"][natural_count:])
            groups = labels["group"][natural_count:]
            subjects = labels["subject"][natural_count:]
            if cfg.rank_weight:
                rank = rank_contrastive_loss(
                    embedding, value, groups, subjects, cfg.contrastive_temperature
                )
            centres = self.centres[groups]
            distance = (centres - value[:, None]).abs()
            tier = distance.argmin(-1)
            if cfg.proxy_weight:
                proxies = F.normalize(
                    model.proxy_arc + 0.1 * model.proxy_offset.tanh(), dim=-1
                )
                logits = embedding @ proxies.T / cfg.contrastive_temperature
                target = torch.exp(
                    -0.5
                    * (
                        (
                            torch.arange(cfg.proxy_count, device=value.device)[None]
                            - tier[:, None]
                        )
                        / 0.75
                    ).square()
                )
                target = target / target.sum(1, keepdim=True)
                proxy = -(target * logits.log_softmax(-1)).sum(1).mean()
                proxy = proxy + 0.01 * model.proxy_offset.square().mean()
            if cfg.router_weight and "adapters" not in cfg.ablate:
                route = F.cross_entropy(
                    output["router"][natural_count:],
                    (tier * cfg.adapters) // cfg.proxy_count,
                )
            loss = loss + ramp * (
                cfg.rank_weight * rank
                + cfg.proxy_weight * proxy
                + cfg.router_weight * route
            )
        if "adapters" not in cfg.ablate:
            probability = output["router_probability"][:natural_count].mean(0)
            balance = (
                probability * torch.log((probability * cfg.adapters).clamp_min(1e-8))
            ).sum()
            vectors = F.normalize(model.adapters.down.flatten(1), dim=-1)
            gram = vectors @ vectors.T
            if cfg.adapters > 1:
                mask = ~torch.eye(cfg.adapters, device=gram.device, dtype=torch.bool)
                orthogonality = gram[mask].square().mean()
            loss = (
                loss
                + cfg.balance_weight * balance
                + cfg.orthogonality_weight * orthogonality
            )
        if cfg.temporal_weight and "temporal" not in cfg.ablate and ramp > 0:
            delta = torch.randint(1, 15, (len(nll),), device=nll.device).float()
            eligible = labels["remaining"] > delta
            eligible[natural_count:] = False
            eligible &= distribution.log_survival(delta).squeeze(1).detach() > math.log(
                1e-4
            )
            if bool(eligible.any()):
                late = dict(batch)
                late["s"] = batch["s"] + delta
                with torch.no_grad():
                    target = teacher.distribution(teacher(late)).survival(
                        self.temporal_horizons[None].expand(len(nll), -1)
                    )
                log_conditional = distribution.log_survival(
                    delta[:, None] + self.temporal_horizons
                ) - distribution.log_survival(delta)
                conditional = log_conditional.clamp_max(0).exp()
                temporal = (conditional[eligible] - target[eligible]).square().mean()
                loss = loss + ramp * cfg.temporal_weight * temporal
        require(bool(torch.isfinite(loss)), "Nonfinite training objective.")
        components = {
            "loss": loss,
            "nll": nll[:natural_count].mean(),
            "twcrps": crps[:natural_count].mean(),
            "rank": rank,
            "proxy": proxy,
            "router": route,
            "temporal": temporal,
            "orthogonality": orthogonality,
            "balance": balance,
        }
        return loss, {name: float(value.detach()) for name, value in components.items()}


# Exponential Moving Average of Model Parameters


@torch.no_grad()
def update_teacher(teacher, model, step, maximum):
    decay = min(maximum, (1 + step) / (10 + step))
    for average, value in zip(teacher.parameters(), model.parameters()):
        average.lerp_(value.detach(), 1 - decay)
    for average, value in zip(teacher.buffers(), model.buffers()):
        average.copy_(value)


# Distribution Prediction and Point Readouts


def predict_dataset(model, data, objective, config, device):
    model.eval()
    outputs = []
    with torch.inference_mode():
        for start in range(0, len(data), config.eval_batch_size):
            ids = np.arange(start, min(start + config.eval_batch_size, len(data)))
            batch, labels = data.batch(ids, device, config.use_prior_los)
            distribution = model.distribution(model(batch))
            values = {
                "mean_remaining": distribution.mean(),
                "median_remaining": distribution.quantile(0.5),
                "q10_remaining": distribution.quantile(0.1),
                "q90_remaining": distribution.quantile(0.9),
                "nll": distribution.nll(labels["remaining"]),
                "joint_nll": distribution.nll(labels["remaining"], labels["cause"]),
                "crps": objective.crps(
                    distribution, batch["s"], labels["remaining"], weighted=False
                ),
                "twcrps": objective.crps(distribution, batch["s"], labels["remaining"]),
            }
            for threshold in (30.0, 60.0, 100.0):
                values[f"p_total_gt_{threshold:g}"] = distribution.survival(
                    (threshold - batch["s"]).clamp_min(0)
                ).squeeze(1)
            death = distribution.death_probability()
            if death is not None:
                values["p_death"] = death
            require(
                all(bool(torch.isfinite(value).all()) for value in values.values()),
                "Nonfinite model predictions.",
            )
            outputs.append(
                pd.DataFrame(
                    {name: value.cpu().numpy() for name, value in values.items()}
                )
            )
    result = pd.concat(outputs, ignore_index=True)
    result.insert(0, "subject_id", data.subject)
    result.insert(0, "t_hadm", data.sequence.ids[data.index])
    result["split"] = data.sequence.split
    result["s"] = data.s
    result["group"] = data.group
    result["true_total"] = data.total
    result["true_remaining"] = data.remaining
    result["cause"] = data.cause
    require(
        np.all(result["q10_remaining"] <= result["median_remaining"])
        and np.all(result["median_remaining"] <= result["q90_remaining"]),
        "Predicted quantiles cross.",
    )
    return result


def error_metrics(prediction, outcome):
    prediction, outcome = np.asarray(prediction, float), np.asarray(outcome, float)
    if not len(outcome):
        return {"n": 0}
    error = prediction - outcome
    pv, yv = float(np.var(prediction)), float(np.var(outcome))
    covariance = float(
        np.mean((prediction - prediction.mean()) * (outcome - outcome.mean()))
    )
    r = covariance / math.sqrt(pv * yv) if pv > 0 and yv > 0 else None
    return plain(
        {
            "n": len(outcome),
            "MAE": np.abs(error).mean(),
            "RMSE": np.sqrt(np.mean(error**2)),
            "R2": 1 - np.mean(error**2) / yv if yv > 0 else None,
            "r": r,
            "spearman": (
                float(spearmanr(prediction, outcome).statistic)
                if pv > 0 and yv > 0 and len(outcome) > 2
                else None
            ),
            "CCC": (
                2 * covariance / (pv + yv + error.mean() ** 2)
                if pv + yv + error.mean() ** 2 > 0
                else 1.0
            ),
            "slope_pred_on_true": covariance / yv if yv > 0 else None,
            "spread_ratio": math.sqrt(pv / yv) if yv > 0 else None,
            "bias": error.mean(),
        }
    )


def macro_mae(prediction, outcome, minimum=1):
    group = np.searchsorted(BANDS[1:-1], outcome, side="left")
    scores = [
        np.abs(prediction[group == k] - outcome[group == k]).mean()
        for k in range(len(BANDS) - 1)
        if np.sum(group == k) >= minimum
    ]
    return float(np.mean(scores)) if scores else None


def validation_score(frame, remaining, statistics, config, mask=None):
    selected = np.ones(len(frame), bool) if mask is None else np.asarray(mask, bool)
    terms = {}
    group_values = frame["group"].to_numpy()
    truth = frame["true_total"].to_numpy()
    landmark = frame["s"].to_numpy()
    prediction = np.asarray(remaining) + landmark
    for group in np.unique(group_values[selected]):
        keep = selected & (group_values == group)
        if (
            keep.sum() < config.minimum_selection_cases
            or str(int(group)) not in statistics["constants"]
        ):
            continue
        baseline = statistics["constants"][str(int(group))]
        true = truth[keep]
        predicted = prediction[keep]
        med = np.full(len(true), baseline["median"]) + landmark[keep]
        mean = np.full(len(true), baseline["mean"]) + landmark[keep]
        mae = np.abs(predicted - true).mean() / max(np.abs(med - true).mean(), 1e-6)
        rmse = np.sqrt(np.mean((predicted - true) ** 2)) / max(
            np.sqrt(np.mean((mean - true) ** 2)), 1e-6
        )
        macro = macro_mae(predicted, true, minimum=2)
        constant_macro = macro_mae(med, true, minimum=2)
        tail = macro / max(constant_macro, 1e-6) if macro is not None else mae
        w = config.selection_tail_weight
        terms[int(group)] = (1 - w) * (0.65 * mae + 0.35 * rmse) + w * tail
    require(
        bool(terms),
        "Validation has too few cases at every landmark; reduce --minimum-selection-cases or provide a larger cohort.",
    )
    zero_groups = [
        int(group)
        for group in terms
        if np.allclose(landmark[group_values == group], 0.0)
    ]
    if zero_groups and len(terms) > 1:
        first = zero_groups[0]
        score = 0.5 * terms[first] + 0.5 * np.mean(
            [value for group, value in terms.items() if group != first]
        )
    else:
        score = np.mean(list(terms.values()))
    return float(score), terms


def select_readout(validation, statistics, config):
    mean = validation["mean_remaining"].to_numpy()
    median = validation["median_remaining"].to_numpy()
    groups = {
        "admission": validation["s"].eq(0).to_numpy(),
        "landmark": validation["s"].gt(0).to_numpy(),
    }
    decisions = {}
    for name, mask in groups.items():
        if not mask.any():
            continue
        candidates = []
        for alpha in (0.0, 0.25, 0.5, 0.75, 1.0):
            try:
                score, _ = validation_score(
                    validation,
                    (1 - alpha) * median + alpha * mean,
                    statistics,
                    config,
                    mask,
                )
            except ValueError:
                continue
            candidates.append({"mean_weight": alpha, "score": score})
        if candidates:
            best = min(candidates, key=lambda value: value["score"])
            decisions[name] = {**best, "candidates": candidates}
        else:
            decisions[name] = {
                "mean_weight": 1.0,
                "score": None,
                "candidates": [],
                "reason": "Too few validation cases; retain the checkpoint mean readout.",
            }
    return decisions


def apply_readout(frame, decisions):
    frame = frame.copy()
    alpha = np.where(
        frame["s"].eq(0),
        decisions.get("admission", {}).get("mean_weight", 1.0),
        decisions.get("landmark", {}).get("mean_weight", 1.0),
    )
    frame["pred_remaining"] = (1 - alpha) * frame[
        "median_remaining"
    ].to_numpy() + alpha * frame["mean_remaining"].to_numpy()
    frame["pred_total"] = frame["s"] + frame["pred_remaining"]
    return frame


# Short-Stay, Long-Stay, and Distribution Evaluation


def auc(score, positive):
    positive = np.asarray(positive, bool)
    n1, n0 = int(positive.sum()), int((~positive).sum())
    if not n1 or not n0:
        return None
    ranks = rankdata(score)
    return float((ranks[positive].sum() - n1 * (n1 + 1) / 2) / (n1 * n0))


def evaluate_frame(frame, statistics):
    reports = {}
    for group, part in frame.groupby("group", sort=True):
        true, predicted = part["true_total"].to_numpy(), part["pred_total"].to_numpy()
        value = error_metrics(predicted, true)
        value["band_macro_MAE"] = macro_mae(predicted, true)
        value["nll"] = float(part["nll"].mean())
        value["joint_nll"] = float(part["joint_nll"].mean())
        value["crps"] = float(part["crps"].mean())
        value["twcrps"] = float(part["twcrps"].mean())
        value["coverage80"] = float(
            (
                (part["true_remaining"] >= part["q10_remaining"])
                & (part["true_remaining"] <= part["q90_remaining"])
            ).mean()
        )
        value["interval80_width"] = float(
            (part["q90_remaining"] - part["q10_remaining"]).mean()
        )
        baseline = statistics["constants"].get(str(int(group)))
        if baseline:
            value["constant_median_MAE"] = float(
                np.abs(part["true_remaining"] - baseline["median"]).mean()
            )
            value["constant_mean_RMSE"] = float(
                np.sqrt(np.mean((part["true_remaining"] - baseline["mean"]) ** 2))
            )
        for name, mask in {
            "short_le_30": true <= 30,
            "long_gt_30": true > 30,
            "long_gt_60": true > 60,
            "long_gt_100": true > 100,
        }.items():
            value[name] = error_metrics(predicted[mask], true[mask])
        value["bands"] = []
        for low, high in zip(BANDS[:-1], BANDS[1:]):
            mask = (true > low) & (true <= high)
            if mask.any():
                value["bands"].append(
                    {
                        "low": float(low),
                        "high": float(high),
                        "mean_true": float(true[mask].mean()),
                        "mean_predicted": float(predicted[mask].mean()),
                        **error_metrics(predicted[mask], true[mask]),
                    }
                )
        for threshold in (30, 60, 100):
            probability = part[f"p_total_gt_{threshold}"].to_numpy()
            positive = true > threshold
            value[f"AUROC_gt_{threshold}"] = auc(probability, positive)
            value[f"Brier_gt_{threshold}"] = float(
                np.mean((probability - positive) ** 2)
            )
        if "p_death" in part:
            value["death_AUROC"] = auc(part["p_death"], part["cause"].eq(1))
        reports[f"{part['s'].iloc[0]:g}"] = value
    return plain(reports)


def patient_bootstrap(frame, repetitions, seed):
    if not repetitions or len(frame) < 3:
        return {}
    patients, inverse = np.unique(frame["subject_id"], return_inverse=True)
    if len(patients) < 2:
        return {}
    generator = np.random.default_rng(seed)
    error = frame["pred_total"].to_numpy() - frame["true_total"].to_numpy()
    masks = {
        "all": np.ones(len(frame), bool),
        "short_le_30": frame["true_total"].le(30).to_numpy(),
        "long_gt_30": frame["true_total"].gt(30).to_numpy(),
    }
    results = {}
    for name, mask in masks.items():
        if mask.sum() < 2:
            continue
        count = np.bincount(inverse[mask], minlength=len(patients))
        absolute = np.bincount(
            inverse[mask], weights=np.abs(error[mask]), minlength=len(patients)
        )
        squared = np.bincount(
            inverse[mask], weights=error[mask] ** 2, minlength=len(patients)
        )
        mae, rmse = [], []
        for _ in range(repetitions):
            chosen = generator.integers(len(patients), size=len(patients))
            n = count[chosen].sum()
            if n:
                mae.append(absolute[chosen].sum() / n)
                rmse.append(math.sqrt(squared[chosen].sum() / n))
        if mae:
            results[name] = {
                "MAE_95CI": np.quantile(mae, [0.025, 0.975]),
                "RMSE_95CI": np.quantile(rmse, [0.025, 0.975]),
                "patient_clusters": len(patients),
            }
    return plain(results)


# True-versus-Predicted Figures


def save_figures(frame, directory):
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    available = frame["s"].unique()
    selected = [0.0] if 0.0 in available else [float(np.min(available))]
    if 30.0 in available and 30.0 not in selected:
        selected.append(30.0)
    for landmark in selected:
        part = frame.loc[np.isclose(frame["s"], landmark)]
        true = part["true_total"].to_numpy()
        predicted = part["pred_total"].to_numpy()
        fig, axes = plt.subplots(1, 2, figsize=(13, 6))
        for axis, logarithmic in zip(axes, (False, True)):
            axis.hexbin(
                true,
                predicted,
                gridsize=65,
                mincnt=1,
                bins="log",
                cmap="Blues",
                xscale="log" if logarithmic else "linear",
                yscale="log" if logarithmic else "linear",
            )
            low = (
                max(min(true.min(), predicted.min()) * 0.9, 1e-4)
                if logarithmic
                else 0.0
            )
            high = max(true.max(), predicted.max()) * 1.03
            axis.plot([low, high], [low, high], "k--", linewidth=1.0)
            axis.set_xlim(low, high)
            axis.set_ylim(low, high)
            axis.set_aspect("equal", adjustable="box")
            axis.set_xlabel("True total length of stay (days)")
            axis.set_ylabel("Predicted total length of stay (days)")
            axis.set_title(
                f"Day {landmark:g}; {'log' if logarithmic else 'linear'} axes; n={len(part):,}"
            )
        fig.tight_layout()
        fig.savefig(directory / f"true_vs_predicted_day_{landmark:g}.png", dpi=220)
        plt.close(fig)


# Training, Validation Selection, and Resumable Checkpoints


def resolve_landmarks(store, config):
    landmarks = set(float(value) for value in config.landmarks)
    if config.include_prolonged_landmark:
        landmarks.add(store.threshold)
    if config.approach == "aprC":
        landmarks = {value for value in landmarks if value >= store.threshold}
        LOGGER.warning(
            "Approach C is prolonged-only and is evaluated at or after the prolonged threshold; it cannot establish short-stay performance."
        )
    require(
        bool(landmarks) and min(landmarks) >= 0,
        "Landmarks must be finite nonnegative days.",
    )
    return tuple(sorted(landmarks))


def report_scope(sequences, store, config):
    return {
        "feature_groups": list(FEATURES),
        "sequence_metadata": {
            "prior_completed_los": config.use_prior_los,
            "time_gaps": True,
            "landmark_day": True,
        },
        "dynamic_current_stay_measurements": False,
        "current_text_policy": store.text_policy,
        "prospective_admission_evaluation": store.text_policy == "available_only",
        "complete_cohorts": {name: value.complete for name, value in sequences.items()},
        "cause_count": store.cause_count,
        "exit_labels": "discharge/death" if store.cause_count == 2 else "all exits",
        "novelty_or_state_of_the_art_established": False,
        "representation_methods": "Rank-aware contrastive loss and ordered-proxy adaptation; not exact paper reproductions.",
        "references": REFERENCES,
    }


def train_model(config, resume=False):
    seed_everything(config.seed, config.threads)
    device = device_for(config.device)
    directory = Path(config.output).expanduser().resolve() / config.approach
    directory.mkdir(parents=True, exist_ok=True)
    last_path, best_path = directory / "last.pt", directory / "best.pt"
    require(
        resume or not last_path.exists(),
        f"A training checkpoint already exists at {last_path}; use --resume or another --output.",
    )
    store = FeatureStore(config)
    sequences = {split: SequenceData(store, config, split) for split in SPLITS}
    landmarks = resolve_landmarks(store, config)
    training = LandmarkData(sequences["train"], landmarks)
    validation = LandmarkData(sequences["val"], landmarks)
    sampler = TrainingSampler(training, config)
    statistics = training_statistics(training, sampler, config, store.cause_count)
    model = SequentialExitModel(store.widths, config, store.cause_count, statistics).to(
        device
    )
    teacher = copy.deepcopy(model).eval()
    teacher.requires_grad_(False)
    objective = TrainingObjective(config, statistics).to(device)
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=config.learning_rate, weight_decay=config.weight_decay
    )
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode="min", factor=0.5, patience=2, min_lr=1e-6
    )
    generator = np.random.default_rng(config.seed)
    steps_per_epoch = math.ceil(len(training) / config.batch_size)
    evaluate_every = max(1, steps_per_epoch // config.evaluations_per_epoch)
    warmup = max(1, min(500, steps_per_epoch // 4))
    limit = config.epochs * steps_per_epoch
    identity_config = asdict(config)
    for name in (
        "device",
        "threads",
        "output",
        "checkpoint_seconds",
        "bootstrap_samples",
    ):
        identity_config.pop(name)
    signature = hashlib.sha256(
        json.dumps(
            {
                "config": identity_config,
                "data": store.signature,
                "causes": store.cause_count,
            },
            sort_keys=True,
        ).encode()
    ).hexdigest()
    state = {
        "step": 0,
        "updates": 0,
        "best_score": float("inf"),
        "best_step": 0,
        "since": 0,
        "history": [],
        "finished": False,
    }
    common = {
        "config": asdict(config),
        "signature": signature,
        "data_signature": store.signature,
        "widths": store.widths,
        "cause_count": store.cause_count,
        "statistics": statistics,
        "landmarks": landmarks,
        "scope": report_scope(sequences, store, config),
    }
    if resume:
        require(last_path.is_file(), f"No checkpoint to resume: {last_path}")
        checkpoint = torch.load(last_path, map_location="cpu", weights_only=False)
        require(
            checkpoint["signature"] == signature,
            "Checkpoint configuration or feature package differs from this run.",
        )
        model.load_state_dict(checkpoint["model"])
        teacher.load_state_dict(checkpoint["teacher"])
        optimizer.load_state_dict(checkpoint["optimizer"])
        scheduler.load_state_dict(checkpoint["scheduler"])
        for value in optimizer.state.values():
            for key, tensor in value.items():
                if torch.is_tensor(tensor) and key != "step":
                    value[key] = tensor.to(device)
        state = checkpoint["state"]
        restore_random_state(checkpoint["random"], generator)
        LOGGER.info(
            "Resumed step %s, best validation score %.4f.",
            state["step"],
            state["best_score"],
        )
    LOGGER.info(
        "Seven feature groups; %s parameters; train/val/test targets: %s/%s/%s; device: %s.",
        f"{sum(parameter.numel() for parameter in model.parameters()):,}",
        len(sequences["train"].ids),
        len(sequences["val"].ids),
        len(sequences["test"].ids),
        device,
    )
    LOGGER.info("Landmarks: %s", ", ".join(f"{value:g}" for value in landmarks))
    write_json(
        directory / "configuration.json",
        {**asdict(config), "scope": common["scope"], "landmarks": landmarks},
    )
    last_save = time.monotonic()
    accumulated, accumulated_count = {}, 0

    def save_last():
        save_checkpoint(
            last_path,
            {
                **common,
                "state": state,
                "model": snapshot(model),
                "teacher": snapshot(teacher),
                "optimizer": optimizer.state_dict(),
                "scheduler": scheduler.state_dict(),
                "random": random_state(generator),
            },
        )

    while state["step"] < limit and not state["finished"]:
        model.train()
        teacher.eval()
        if state["step"] < warmup:
            for group in optimizer.param_groups:
                group["lr"] = config.learning_rate * (state["step"] + 1) / warmup
        ids = sampler.draw(generator)
        batch, labels = training.batch(ids, device, config.use_prior_los)
        ramp = min(1.0, state["step"] / max(steps_per_epoch, 1))
        optimizer.zero_grad(set_to_none=True)
        loss, components = objective(
            model, teacher, batch, labels, config.batch_size, ramp
        )
        loss.backward()
        norm = nn.utils.clip_grad_norm_(
            model.parameters(), config.gradient_clip, error_if_nonfinite=True
        )
        optimizer.step()
        update_teacher(teacher, model, state["updates"], config.ema_decay)
        state["updates"] += 1
        state["step"] += 1
        for name, value in components.items():
            accumulated[name] = accumulated.get(name, 0.0) + value
        accumulated_count += 1
        if state["step"] % evaluate_every == 0 or state["step"] == limit:
            predicted = predict_dataset(teacher, validation, objective, config, device)
            score, terms = validation_score(
                predicted, predicted["mean_remaining"].to_numpy(), statistics, config
            )
            scheduler.step(score)
            improved = score < state["best_score"] - 1e-5
            if improved:
                state["best_score"], state["best_step"], state["since"] = (
                    score,
                    state["step"],
                    0,
                )
                save_checkpoint(
                    best_path,
                    {
                        **common,
                        "state": copy.deepcopy(state),
                        "model": snapshot(teacher),
                    },
                )
            else:
                state["since"] += 1
            record = {
                "step": state["step"],
                "epoch": state["step"] / steps_per_epoch,
                "validation_score": score,
                "landmark_scores": terms,
                "selected": improved,
                "lr": optimizer.param_groups[0]["lr"],
                "gradient_norm": float(norm),
                **{
                    f"train_{name}": value / accumulated_count
                    for name, value in accumulated.items()
                },
            }
            state["history"].append(record)
            write_json(directory / "training_history.json", state["history"])
            LOGGER.info(
                "Epoch %.2f | validation %.4f | best %.4f | NLL %.4f | %s",
                record["epoch"],
                score,
                state["best_score"],
                record["train_nll"],
                "selected" if improved else "",
            )
            accumulated, accumulated_count = {}, 0
            if (
                state["step"] >= config.minimum_epochs * steps_per_epoch
                and state["since"] >= config.patience
            ):
                state["finished"] = True
            save_last()
            last_save = time.monotonic()
        elif time.monotonic() - last_save >= config.checkpoint_seconds:
            save_last()
            last_save = time.monotonic()
    state["finished"] = True
    save_last()
    require(best_path.is_file(), "No validation-selected model was saved.")
    best = torch.load(best_path, map_location="cpu", weights_only=False)
    model.load_state_dict(best["model"])
    validation_predictions = predict_dataset(
        model, validation, objective, config, device
    )
    decisions = select_readout(validation_predictions, statistics, config)
    write_json(directory / "readout.json", decisions)
    best["readout"] = decisions
    save_checkpoint(best_path, best)
    report = {
        "scope": common["scope"],
        "best_validation_score": state["best_score"],
        "best_step": state["best_step"],
        "steps_run": state["step"],
        "readout": decisions,
        "xi": float((config.xi_max * model.xi_raw.sigmoid()).detach().cpu()),
        "metrics": {},
    }
    for split in ("val", "test"):
        data = (
            validation if split == "val" else LandmarkData(sequences["test"], landmarks)
        )
        prediction = (
            validation_predictions
            if split == "val"
            else predict_dataset(model, data, objective, config, device)
        )
        prediction = apply_readout(prediction, decisions)
        prediction.to_csv(directory / f"predictions_{split}.csv", index=False)
        report["metrics"][split] = evaluate_frame(prediction, statistics)
        if split == "test":
            first = 0.0 if prediction["s"].eq(0).any() else float(prediction["s"].min())
            report["patient_bootstrap"] = {
                "landmark": first,
                "intervals": patient_bootstrap(
                    prediction.loc[prediction["s"].eq(first)],
                    config.bootstrap_samples,
                    config.seed + 1,
                ),
            }
            save_figures(prediction, directory)
    write_json(directory / "results.json", report)
    LOGGER.info("Training and held-out evaluation complete: %s", directory)
    return directory


# Evaluation of a Saved Model


def evaluate_saved(
    checkpoint_path,
    data_path,
    output,
    split,
    device_name,
    admissions,
    allow_changed_data=False,
):
    checkpoint_path = Path(checkpoint_path).expanduser().resolve()
    checkpoint = torch.load(checkpoint_path, map_location="cpu", weights_only=False)
    values = dict(checkpoint["config"])
    values["data"], values["device"] = data_path, device_name
    if admissions:
        values["admissions"] = admissions
    config = Config(**values)
    seed_everything(config.seed, config.threads)
    store = FeatureStore(config)
    require(
        allow_changed_data or store.signature == checkpoint["data_signature"],
        "Feature package differs from training; use --allow-changed-data only after verifying identical preprocessing transforms.",
    )
    require(
        store.widths == checkpoint["widths"]
        and store.cause_count == checkpoint["cause_count"],
        "Feature dimensions or event causes disagree with the saved model.",
    )
    require(
        "readout" in checkpoint,
        "Training has not yet completed validation readout selection.",
    )
    sequence = SequenceData(store, config, split)
    data = LandmarkData(sequence, checkpoint["landmarks"])
    device = device_for(device_name)
    model = SequentialExitModel(
        store.widths, config, store.cause_count, checkpoint["statistics"]
    ).to(device)
    model.load_state_dict(checkpoint["model"])
    objective = TrainingObjective(config, checkpoint["statistics"]).to(device)
    prediction = apply_readout(
        predict_dataset(model, data, objective, config, device), checkpoint["readout"]
    )
    directory = Path(output).expanduser().resolve()
    directory.mkdir(parents=True, exist_ok=True)
    prediction.to_csv(directory / f"predictions_{split}.csv", index=False)
    write_json(
        directory / f"metrics_{split}.json",
        evaluate_frame(prediction, checkpoint["statistics"]),
    )
    save_figures(prediction, directory)
    LOGGER.info("Saved-model evaluation complete: %s", directory)


# Command-Line Configuration and Execution


def validate_config(config):
    require(config.approach in APPROACHES, "Unknown sequence approach.")
    require(
        config.d_model % config.encoder_heads == 0
        and config.d_model % config.memory_heads == 0,
        "d-model must be divisible by encoder-heads and memory-heads.",
    )
    for name in (
        "d_model",
        "encoder_layers",
        "encoder_heads",
        "memory_heads",
        "fusion_dim",
        "adapters",
        "adapter_rank",
        "representation_dim",
        "proxy_count",
        "batch_size",
        "eval_batch_size",
        "epochs",
        "minimum_epochs",
        "patience",
        "evaluations_per_epoch",
        "quadrature_points",
        "minimum_selection_cases",
        "checkpoint_seconds",
        "threads",
    ):
        require(getattr(config, name) > 0, f"{name} must be positive.")
    require(
        config.representation_dim >= 2 and config.proxy_count >= 2,
        "Representations and proxy lattice need at least two dimensions/anchors.",
    )
    require(
        config.balanced_batch_size >= 0 and config.bootstrap_samples >= 0,
        "Batch/bootstrap counts cannot be negative.",
    )
    require(
        config.epochs >= config.minimum_epochs,
        "epochs must be at least minimum-epochs.",
    )
    require(
        config.tail_start > 0 and 0 < config.xi_initial < config.xi_max < 0.5,
        "Tail parameters must satisfy 0 < xi-initial < xi-max < 0.5.",
    )
    require(
        all(math.isfinite(float(value)) and value >= 0 for value in config.landmarks),
        "Invalid landmark day.",
    )
    for name in (
        "dropout",
        "head_dropout",
        "modality_dropout",
        "history_dropout",
        "ema_decay",
        "admission_sampling_share",
        "selection_tail_weight",
    ):
        require(0 <= getattr(config, name) < 1, f"{name} must be in [0, 1).")
    for name in (
        "crps_weight",
        "rank_weight",
        "proxy_weight",
        "temporal_weight",
        "router_weight",
        "orthogonality_weight",
        "balance_weight",
        "weight_decay",
        "landmark_tempering",
    ):
        require(getattr(config, name) >= 0, f"{name} must be nonnegative.")
    require(
        config.learning_rate > 0
        and config.gradient_clip > 0
        and config.contrastive_temperature > 0,
        "Learning rate, gradient clip, and temperature must be positive.",
    )
    require(
        Path(config.output).expanduser().resolve()
        != Path(config.data).expanduser().resolve(),
        "Training outputs must be separate from preprocessing inputs.",
    )


def parse_arguments(argv=None):
    parser = argparse.ArgumentParser(
        description="Seven-feature sequential length-of-stay modeling with history memory, landmark conditioning, and an unbounded exit distribution.",
        epilog="Examples: %(prog)s train --data data/mimic_iv/processed_all_stays --output runs/sequential --approach aprB; %(prog)s prepare --preprocessing-script mimic_iv_sequence_processing.py -- --root data/mimic_iv",
    )
    commands = parser.add_subparsers(dest="command", required=True)
    preparation = commands.add_parser(
        "prepare",
        help="Run the existing seven-feature preprocessor with complete A/B evaluation cohorts.",
    )
    preparation.add_argument("--preprocessing-script", required=True)
    preparation.add_argument("arguments", nargs=argparse.REMAINDER)
    train = commands.add_parser(
        "train",
        help="Train on training patients; select checkpoints/readouts on validation; evaluate test once.",
    )
    defaults = Config()
    for field in fields(Config):
        name, value = field.name, getattr(defaults, field.name)
        option = "--" + name.replace("_", "-")
        if isinstance(value, bool):
            train.add_argument(
                option, action=argparse.BooleanOptionalAction, default=value
            )
        elif name == "landmarks":
            train.add_argument(option, type=float, nargs="+", default=value)
        elif name == "ablate":
            train.add_argument(
                option,
                nargs="*",
                choices=("history", "adapters", "ranking", "temporal"),
                default=value,
            )
        elif name == "approach":
            train.add_argument(option, choices=APPROACHES, default=value)
        else:
            train.add_argument(
                option, type=type(value) if value is not None else str, default=value
            )
    train.add_argument("--resume", action="store_true")
    evaluate = commands.add_parser(
        "evaluate",
        help="Evaluate a trusted local model checkpoint with its frozen validation readout.",
    )
    evaluate.add_argument("--checkpoint", required=True)
    evaluate.add_argument("--data", required=True)
    evaluate.add_argument("--output", required=True)
    evaluate.add_argument("--split", choices=SPLITS, default="test")
    evaluate.add_argument("--device", default="auto")
    evaluate.add_argument("--admissions")
    evaluate.add_argument("--allow-changed-data", action="store_true")
    return parser.parse_args(argv)


def main(argv=None):
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s | %(message)s", datefmt="%H:%M:%S"
    )
    arguments = vars(parse_arguments(argv))
    command = arguments.pop("command")
    if command == "prepare":
        prepare_data(arguments["preprocessing_script"], arguments["arguments"])
    elif command == "train":
        resume = arguments.pop("resume")
        config = Config(**arguments)
        validate_config(config)
        train_model(config, resume)
    else:
        evaluate_saved(
            arguments["checkpoint"],
            arguments["data"],
            arguments["output"],
            arguments["split"],
            arguments["device"],
            arguments["admissions"],
            arguments["allow_changed_data"],
        )


if __name__ == "__main__":
    main()
