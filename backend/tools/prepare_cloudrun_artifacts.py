"""Stage model weights + Dataset.csv inside the repo so Docker can COPY them.

Docker's build context can't reach outside the directory containing the
Dockerfile, but the canonical model/data files live on this machine at
/Users/william/Documents/LLOS/data/. This copies the ~23MB of artifacts the
backend needs into backend/cloudrun_artifacts/ (gitignored — regenerate
locally before each build rather than committing large binaries to git).

Run from CarePathways/ before building:
    python -m backend.tools.prepare_cloudrun_artifacts
"""
from __future__ import annotations

import shutil
from pathlib import Path

SOURCE_MODELS = Path("/Users/william/Documents/LLOS/data/models")
SOURCE_DATASET = Path("/Users/william/Documents/LLOS/data/processed/Dataset.csv")
DEST_DIR = Path(__file__).resolve().parents[1] / "cloudrun_artifacts"

FILES_TO_COPY = [
    "los_model_weights.pt",
    "lgbm_los_model.txt",
    "t5_extractor_config.json",
]
DEEP_PATIENT_FILES = [
    "word2vec.model",
    "scaler.npz",
    "dae_state.pt",
    "mlp_state.pt",
    "bundle_meta.json",
]


def main() -> None:
    dest_models = DEST_DIR / "models"
    dest_dp = dest_models / "deep_patient"
    dest_dp.mkdir(parents=True, exist_ok=True)

    for name in FILES_TO_COPY:
        shutil.copy2(SOURCE_MODELS / name, dest_models / name)
    for name in DEEP_PATIENT_FILES:
        shutil.copy2(SOURCE_MODELS / "deep_patient" / name, dest_dp / name)
    shutil.copy2(SOURCE_DATASET, DEST_DIR / "Dataset.csv")

    total_bytes = sum(f.stat().st_size for f in DEST_DIR.rglob("*") if f.is_file())
    print(f"Staged {total_bytes / 1_000_000:.1f} MB into {DEST_DIR}")


if __name__ == "__main__":
    main()
