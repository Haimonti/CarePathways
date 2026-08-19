"""Inference pipeline for the Deep Patient (DAE + MLP) LOS model.

Mirrors the feature construction in DEEP_PATIENT.ipynb: six clinical text
sections are tokenized with gensim's simple_preprocess, mean-pooled over a
Word2Vec vocabulary, and concatenated after three integer-coded demographic
features. The 1803-dim vector is standardized, passed through the frozen DAE
encoder, and regressed to LOS days by the MLP head.

Demographics are not part of the serialized_clinical_input contract; they are
recovered from the demographic preamble present in stored records
("... nonhispanic white male ...") and fall back to training-set modes when
absent. They are 3 of 1803 standardized features, so the fallback impact is
minimal.
"""
from __future__ import annotations

import json
import re
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import torch
from gensim.models import Word2Vec
from gensim.utils import simple_preprocess

from ..manual.utils import parse_sections
from .models import DenoisingAutoencoder, MLPRegressor

# Matches the demographic preamble kept by the original preprocessing,
# e.g. "24 year-old nonhispanic white male". Bare word search is avoided
# because race terms collide with clinical text ("white blood cells").
DEMOGRAPHIC_PHRASE = re.compile(
    r"\b(nonhispanic|hispanic)\s+(white|black|asian|native|other)\b"
)
FEMALE_PATTERN = re.compile(r"\bfemale\b")
MALE_PATTERN = re.compile(r"\bmale\b")


@dataclass(frozen=True)
class DeepPatientPipelineSettings:
    word2vec_path: Path
    scaler_path: Path
    dae_weights_path: Path
    mlp_weights_path: Path
    meta_path: Path


class DeepPatientPipeline:
    def __init__(self, settings: DeepPatientPipelineSettings):
        self.settings = settings
        self.meta = json.loads(settings.meta_path.read_text(encoding="utf-8"))
        self.text_columns: list[str] = list(self.meta["text_columns"])
        self.embedding_dim: int = int(self.meta["embedding_dim"])
        self.encodings: dict = self.meta["encodings"]
        self.demographic_defaults: dict = self.meta["demographic_defaults"]

        self.word2vec = Word2Vec.load(str(settings.word2vec_path))

        scaler = np.load(settings.scaler_path)
        self._scaler_mean = scaler["mean"]
        self._scaler_scale = scaler["scale"]

        input_dim = int(self.meta["input_dim"])
        self.dae = DenoisingAutoencoder(input_dim=input_dim)
        self.dae.load_state_dict(
            torch.load(settings.dae_weights_path, map_location="cpu", weights_only=True)
        )
        self.dae.eval()

        self.mlp = MLPRegressor(input_dim=self.dae.encoder[-1].out_features)
        self.mlp.load_state_dict(
            torch.load(settings.mlp_weights_path, map_location="cpu", weights_only=True)
        )
        self.mlp.eval()

    def _tokens_to_vector(self, tokens: list[str]) -> np.ndarray:
        vectors = [self.word2vec.wv[t] for t in tokens if t in self.word2vec.wv]
        if not vectors:
            return np.zeros(self.embedding_dim)
        return np.mean(vectors, axis=0)

    def _extract_demographics(self, text: str) -> list[float]:
        lowered = text.lower()

        gender = float(self.demographic_defaults["gender"])
        if FEMALE_PATTERN.search(lowered):
            gender = float(self.encodings["gender"]["F"])
        elif MALE_PATTERN.search(lowered):
            gender = float(self.encodings["gender"]["M"])

        race = float(self.demographic_defaults["race"])
        ethnicity = float(self.demographic_defaults["ethnicity"])
        match = DEMOGRAPHIC_PHRASE.search(lowered)
        if match:
            ethnicity = float(self.encodings["ethnicity"][match.group(1)])
            race = float(self.encodings["race"][match.group(2)])

        return [gender, race, ethnicity]

    def predict_input_text(self, input_text: str) -> float:
        sections = parse_sections(input_text)

        field_vectors = [
            self._tokens_to_vector(simple_preprocess(sections.get(column, "")))
            for column in self.text_columns
        ]
        physical = self._extract_demographics(input_text)
        features = np.concatenate([np.asarray(physical), *field_vectors]).astype(np.float64)

        scaled = (features - self._scaler_mean) / self._scaler_scale
        with torch.no_grad():
            encoded = self.dae.encoder(torch.tensor(scaled, dtype=torch.float32).unsqueeze(0))
            prediction = self.mlp(encoded).squeeze().item()
        return float(prediction)
