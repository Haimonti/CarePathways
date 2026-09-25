"""Inference pipeline for Isaac's tabular LOS ensemble (RF + XGBoost + LightGBM + SVR).

All four regressors share one feature matrix, so the pipeline is loaded once per
weights directory and reused by every ensemble bundle (mean and each member).
"""
from __future__ import annotations

import threading
from functools import lru_cache
from pathlib import Path

import joblib
import lightgbm as lgb
import pandas as pd
import torch
import xgboost as xgb
from transformers import AutoTokenizer, T5EncoderModel

from ..demographics import Demographics
from .features import PHYSICAL_COLUMNS, T5_MODEL_NAME, TEXT_COLUMNS, build_features

REQUIRED_FILES = (
    "text_embedding_scaler.joblib",
    "text_pca.joblib",
    "rf_model.joblib",
    "xgb_model.json",
    "lgb_model.txt",
    "svr_model.joblib",
    "svr_scaler.joblib",
)
MEMBERS = ("random_forest", "xgboost", "lightgbm", "svr")


class EnsemblePipeline:
    def __init__(self, weights_dir: Path):
        self.weights_dir = weights_dir
        self.device = torch.device("cpu")
        self._lock = threading.Lock()

        self.t5_tokenizer = AutoTokenizer.from_pretrained(T5_MODEL_NAME)
        self.t5_model = T5EncoderModel.from_pretrained(T5_MODEL_NAME).to(self.device)
        self.t5_model.eval()

        self.text_embedding_scaler = joblib.load(weights_dir / "text_embedding_scaler.joblib")
        self.text_pca = joblib.load(weights_dir / "text_pca.joblib")

        self.rf_model = joblib.load(weights_dir / "rf_model.joblib")

        self.xgb_model = xgb.XGBRegressor()
        self.xgb_model.load_model(weights_dir / "xgb_model.json")

        self.lgb_model = lgb.Booster(model_file=str(weights_dir / "lgb_model.txt"))

        self.svr_model = joblib.load(weights_dir / "svr_model.joblib")
        self.svr_scaler = joblib.load(weights_dir / "svr_scaler.joblib")

    def build_features(self, raw_df: pd.DataFrame) -> pd.DataFrame:
        return build_features(
            raw_df,
            self.t5_tokenizer,
            self.t5_model,
            self.device,
            self.text_embedding_scaler,
            self.text_pca,
        )

    def predict_frame(self, raw_df: pd.DataFrame) -> dict[str, list[float]]:
        with self._lock:
            X = self.build_features(raw_df)
            X_scaled = self.svr_scaler.transform(X)
            return {
                "random_forest": self.rf_model.predict(X).tolist(),
                "xgboost": self.xgb_model.predict(X).tolist(),
                "lightgbm": self.lgb_model.predict(X).tolist(),
                "svr": self.svr_model.predict(X_scaled).tolist(),
            }

    def predict_components(self, sections: dict[str, str], demographics: Demographics) -> dict[str, float]:
        row = {column: sections.get(column, "") for column in TEXT_COLUMNS}
        # Empty strings fall through Isaac's maps to 0 ("unknown"), as in training.
        row.update(
            {
                "gender": demographics.gender or "",
                "race": demographics.race or "",
                "ethnicity": demographics.ethnicity or "",
            }
        )
        raw_df = pd.DataFrame([row], columns=TEXT_COLUMNS + PHYSICAL_COLUMNS)
        predictions = self.predict_frame(raw_df)
        return {member: float(values[0]) for member, values in predictions.items()}


@lru_cache(maxsize=None)
def load_ensemble_pipeline(weights_dir: Path) -> EnsemblePipeline:
    return EnsemblePipeline(weights_dir)
