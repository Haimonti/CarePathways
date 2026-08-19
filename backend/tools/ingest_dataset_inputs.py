from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Any

from backend.app.repositories.sqlite import SQLiteRepository
from backend.app.settings import load_settings


REQUIRED_COLUMNS = {"subject_id", "hadm_id", "input", "target"}


def _to_float(value: Any) -> float | None:
    if value in (None, ""):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _clean(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def ingest_dataset(
    config_path: str | Path,
    dataset_path: str | Path,
) -> int:
    settings = load_settings(config_path)
    repository = SQLiteRepository(settings.database.path)
    repository.initialize()

    resolved_dataset_path = Path(dataset_path).expanduser().resolve()
    with resolved_dataset_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        fieldnames = set(reader.fieldnames or [])
        missing = sorted(REQUIRED_COLUMNS - fieldnames)
        if missing:
            raise RuntimeError(
                f"Dataset is missing required column(s): {', '.join(missing)}"
            )

        count = 0
        for uuid, row in enumerate(reader, start=1):
            subject_id = _clean(row.get("subject_id"))
            hadm_id = _clean(row.get("hadm_id"))
            input_text = _clean(row.get("input"))
            if not subject_id or not hadm_id or not input_text:
                continue

            metadata = {
                key: value
                for key, value in row.items()
                if key not in {
                    "subject_id",
                    "hadm_id",
                    "input",
                    "target",
                    "admittime",
                    "dischtime",
                    "anchor_age",
                    "gender",
                    "race",
                    "ethnicity",
                    "birthdate",
                    "admission_type",
                }
            }
            repository.upsert_dataset_record(
                uuid=uuid,
                subject_id=subject_id,
                hadm_id=hadm_id,
                input_text=input_text,
                actual_los_days=_to_float(row.get("target")),
                admittime=_clean(row.get("admittime")),
                dischtime=_clean(row.get("dischtime")),
                anchor_age=_to_float(row.get("anchor_age")),
                gender=_clean(row.get("gender")),
                race=_clean(row.get("race")),
                ethnicity=_clean(row.get("ethnicity")),
                birthdate=_clean(row.get("birthdate")),
                admission_type=_clean(row.get("admission_type")),
                metadata=metadata,
                source_path=str(resolved_dataset_path),
            )
            count += 1

    return count


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Seed SQLite from Dataset.csv serialized clinical input rows."
    )
    parser.add_argument("--config", default=None, help="Path to backend/config.yaml")
    parser.add_argument("--dataset", required=True, help="Path to Dataset.csv")
    args = parser.parse_args()

    count = ingest_dataset(config_path=args.config, dataset_path=args.dataset)
    print(f"Ingested {count} dataset row(s) into SQLite.")


if __name__ == "__main__":
    main()
