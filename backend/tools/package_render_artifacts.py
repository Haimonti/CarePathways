from __future__ import annotations

import argparse
import tarfile
from pathlib import Path

from backend.app.settings import load_settings


DEFAULT_DATASET_PATH = Path("/Users/william/Documents/LLOS/data/processed/Dataset.csv")


def _required_path(value: object, label: str) -> Path:
    if not isinstance(value, Path):
        raise RuntimeError(f"Missing required path in config: {label}")
    if not value.is_file():
        raise FileNotFoundError(f"Missing required file for {label}: {value}")
    return value


def package_artifacts(
    *,
    config_path: str | Path,
    dataset_path: str | Path,
    output_path: str | Path,
) -> Path:
    settings = load_settings(config_path)
    bundle = settings.model_bundles[settings.default_model_key]

    model_weights_path = _required_path(
        bundle.adapter_settings.get("model_weights_path"),
        "model_weights_path",
    )
    lgbm_model_path = _required_path(
        bundle.adapter_settings.get("lgbm_model_path"),
        "lgbm_model_path",
    )
    extractor_config_path = _required_path(
        bundle.adapter_settings.get("extractor_config_path"),
        "extractor_config_path",
    )

    resolved_dataset_path = Path(dataset_path).expanduser().resolve()
    if not resolved_dataset_path.is_file():
        raise FileNotFoundError(f"Missing Dataset.csv: {resolved_dataset_path}")

    resolved_output_path = Path(output_path).expanduser().resolve()
    resolved_output_path.parent.mkdir(parents=True, exist_ok=True)

    with tarfile.open(resolved_output_path, "w:gz") as archive:
        archive.add(model_weights_path, arcname="models/los_model_weights.pt")
        archive.add(lgbm_model_path, arcname="models/lgbm_los_model.txt")
        archive.add(extractor_config_path, arcname="models/t5_extractor_config.json")
        archive.add(resolved_dataset_path, arcname="Dataset.csv")

    return resolved_output_path


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Package model artifacts and Dataset.csv for Render disk bootstrap."
    )
    parser.add_argument(
        "--config",
        default="backend/config.yaml",
        help="Local backend config that points to the trained artifacts.",
    )
    parser.add_argument(
        "--dataset",
        default=str(DEFAULT_DATASET_PATH),
        help="Path to local Dataset.csv.",
    )
    parser.add_argument(
        "--output",
        default="render_artifacts.tar.gz",
        help="Output tar.gz path.",
    )
    args = parser.parse_args()

    output_path = package_artifacts(
        config_path=args.config,
        dataset_path=args.dataset,
        output_path=args.output,
    )
    print(f"Wrote Render artifact bundle: {output_path}")


if __name__ == "__main__":
    main()
