from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class DataPaths:
    raw_csv_dir: Path
    notes_dir: Path
    processed_dir: Path


def _repo_dir() -> Path:
    return Path(__file__).resolve().parent


def _config_path(filename: str) -> Path:
    return _repo_dir() / filename


def _load_yaml_config(config_path: Path) -> dict:
    try:
        import yaml
    except ImportError as exc:
        raise RuntimeError(
            "PyYAML is required to load config.yaml. Install it with `pip install PyYAML`."
        ) from exc

    with config_path.open("r", encoding="utf-8") as handle:
        config = yaml.safe_load(handle) or {}

    if not isinstance(config, dict):
        raise RuntimeError(f"Config file must contain a YAML mapping: {config_path}")

    return config


def load_data_paths(
    config_filename: str = "config.yaml",
    example_filename: str = "config.example.yaml",
) -> DataPaths:
    config_path = _config_path(config_filename)
    example_path = _config_path(example_filename)

    if not config_path.exists():
        raise FileNotFoundError(
            f"Missing {config_path.name}. Copy {example_path.name} to {config_path.name} "
            "and set data.root_dir before running preprocessing or training."
        )

    config = _load_yaml_config(config_path)
    data_config = config.get("data")
    if not isinstance(data_config, dict):
        raise RuntimeError("config.yaml must define a `data` mapping.")

    root_dir_value = data_config.get("root_dir")
    if not isinstance(root_dir_value, str) or not root_dir_value.strip():
        raise RuntimeError("config.yaml must set a non-empty `data.root_dir` value.")

    root_dir = Path(root_dir_value).expanduser().resolve()
    raw_csv_subdir = data_config.get("raw_csv_subdir", "csv")
    notes_subdir = data_config.get("notes_subdir", "notes")
    processed_subdir = data_config.get("processed_subdir", "synthea_processed")

    raw_csv_dir = (root_dir / raw_csv_subdir).resolve()
    notes_dir = (root_dir / notes_subdir).resolve()
    processed_dir = (root_dir / processed_subdir).resolve()
    processed_dir.mkdir(parents=True, exist_ok=True)

    return DataPaths(
        raw_csv_dir=raw_csv_dir,
        notes_dir=notes_dir,
        processed_dir=processed_dir,
    )
