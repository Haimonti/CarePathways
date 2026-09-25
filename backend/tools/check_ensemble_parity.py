"""Check the backend ensemble port against Isaac's original LOSPredictor.

Runs both implementations on the same rows of Isaac's Dataset.csv and asserts
every member prediction matches. Needs spaCy installed (Isaac's code imports
it), which also lets the vendored stop-word list be compared.

Run from CarePathways/ (the env vars avoid a macOS libomp clash, see run.sh):
    OMP_NUM_THREADS=1 KMP_DUPLICATE_LIB_OK=TRUE python -m backend.tools.check_ensemble_parity \
        --esembles-dir ../esembles --weights-dir /Users/william/Documents/LLOS/data/models/ensemble
"""
from __future__ import annotations

import argparse
import importlib.util
import sys
from pathlib import Path

import numpy as np
import pandas as pd

from backend.app.pipelines.ensemble.features import TEXT_COLUMNS
from backend.app.pipelines.ensemble.pipeline import MEMBERS, EnsemblePipeline
from backend.app.pipelines.ensemble.stop_words import STOP_WORDS

ISAAC_KEYS = {
    "random_forest": "RandomForest",
    "xgboost": "XGBoost",
    "lightgbm": "LightGBM",
    "svr": "SVR",
}
# Dataset.csv stores gender/race already encoded; Isaac's predictor expects labels.
GENDER_LABELS = {1: "F", 2: "M"}
RACE_LABELS = {0: "unknown", 1: "white", 2: "black", 3: "asian", 4: "native", 5: "other"}


def _load_isaac_module(esembles_dir: Path):
    spec = importlib.util.spec_from_file_location("isaac_inference", esembles_dir / "inference.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _sample_rows(dataset_path: Path, rows: int) -> pd.DataFrame:
    df = pd.read_csv(dataset_path)
    df = df.sample(n=min(rows, len(df)), random_state=0).reset_index(drop=True)
    df["gender"] = df["gender"].map(GENDER_LABELS)
    df["race"] = df["race"].map(RACE_LABELS)
    return df[TEXT_COLUMNS + ["gender", "race", "ethnicity"]]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--esembles-dir", type=Path, required=True)
    parser.add_argument("--weights-dir", type=Path, required=True)
    parser.add_argument("--rows", type=int, default=25)
    parser.add_argument("--tolerance", type=float, default=1e-6)
    args = parser.parse_args()

    from spacy.lang.en.stop_words import STOP_WORDS as SPACY_STOP_WORDS

    if set(SPACY_STOP_WORDS) != set(STOP_WORDS):
        print("FAIL: vendored stop words differ from the installed spaCy list")
        return 1

    raw_df = _sample_rows(args.esembles_dir / "Dataset.csv", args.rows)
    isaac = _load_isaac_module(args.esembles_dir).LOSPredictor(str(args.weights_dir))
    expected = isaac.predict(raw_df)
    actual = EnsemblePipeline(args.weights_dir).predict_frame(raw_df)

    failed = False
    for member in MEMBERS:
        diff = np.max(np.abs(np.asarray(actual[member]) - np.asarray(expected[ISAAC_KEYS[member]])))
        status = "ok" if diff <= args.tolerance else "FAIL"
        failed |= status == "FAIL"
        print(f"{member:>14}: max |diff| = {diff:.3e} over {len(raw_df)} rows [{status}]")
    print(f"sample predictions (row 0): " + ", ".join(f"{m}={actual[m][0]:.2f}" for m in MEMBERS))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
