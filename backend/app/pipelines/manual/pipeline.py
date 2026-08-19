"""
ManualPipeline — class-based wrapper around the T5 + BiLSTM + LightGBM stack.

Key differences from the original los_predictor/inference.py:
- No module-level model loading (models load on first call or explicit .load()).
- Paths come from backend config, not hardcoded CWD-relative strings.
- Prompt is a pure parameter — no global mutation (thread-safe).
- Cache directory is configurable.
"""
from __future__ import annotations

import json
import threading
from typing import Any
from pathlib import Path

from .config import DEFAULT_T5_PROMPT, HIDDEN_DIM, SECTION_LIST
from .utils import build_structured_input, parse_sections


class ManualPipeline:
    """
    Lazy-loading inference pipeline for manual clinical form predictions.

    Usage:
        pipeline = ManualPipeline(settings)
        pipeline.load()          # optional — called automatically on first predict()
        days = pipeline.predict(form_data)
    """

    def __init__(self, settings: "ManualPipelineSettings") -> None:  # noqa: F821
        self._settings = settings
        self._lock = threading.Lock()
        self._loaded = False

        self._bilstm: Any | None = None
        self._lgbm: Any | None = None
        self._t5: Any | None = None
        self._device: Any | None = None
        self._torch: Any | None = None

    # ------------------------------------------------------------------
    # Loading
    # ------------------------------------------------------------------

    def load(self) -> None:
        """Load all three model components. Idempotent."""
        with self._lock:
            if self._loaded:
                return
            self._load_unsafe()
            self._loaded = True

    def _load_unsafe(self) -> None:
        s = self._settings
        self._validate_paths()

        import torch
        import lightgbm as lgb

        from .models import LOSModel, T5Extractor

        self._torch = torch
        self._device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

        # BiLSTM
        checkpoint = torch.load(str(s.model_weights_path), map_location=self._device)
        bilstm = LOSModel()
        bilstm.load_state_dict(checkpoint["model_state_dict"])
        bilstm.to(self._device).eval()
        self._bilstm = bilstm
        print(f"[ManualPipeline] Loaded BiLSTM   → {s.model_weights_path}")

        # LightGBM
        self._lgbm = lgb.Booster(model_file=str(s.lgbm_model_path))
        print(f"[ManualPipeline] Loaded LightGBM → {s.lgbm_model_path}")

        # T5 Extractor
        with open(s.extractor_config_path, encoding="utf-8") as fh:
            extractor_cfg = json.load(fh)
        self._t5 = T5Extractor(
            model_name=extractor_cfg["model_name"],
            device=self._device,
            cache_dir=str(s.cache_dir) if s.cache_dir else None,
        )
        print(f"[ManualPipeline] Loaded T5       → {extractor_cfg['model_name']}")
        print("[ManualPipeline] Ready for inference.")

    def _validate_paths(self) -> None:
        s = self._settings
        missing = [
            str(p)
            for p in [s.model_weights_path, s.lgbm_model_path, s.extractor_config_path]
            if not Path(p).is_file()
        ]
        if missing:
            raise FileNotFoundError(
                "ManualPipeline: missing model file(s):\n  " + "\n  ".join(missing)
            )

    # ------------------------------------------------------------------
    # Inference
    # ------------------------------------------------------------------

    def predict(self, form_data: dict, prompt: str = DEFAULT_T5_PROMPT) -> float:
        """
        Predict LOS in days from a dict of clinical form fields.

        Args:
            form_data: keys from SECTION_LIST, values are free-text strings.
                       Empty / missing keys are silently skipped.
            prompt:    T5 prefix prompt (defaults to training prompt).

        Returns:
            Predicted LOS in days, clipped to >= 0.

        Raises:
            ValueError: if no valid section fields are present.
            FileNotFoundError: if model files are missing on first call.
        """
        structured = build_structured_input(form_data)
        if not structured:
            raise ValueError(
                f"No valid section fields found. Expected one or more of: {SECTION_LIST}"
            )
        return self.predict_input_text(input_text=structured, prompt=prompt)

    def predict_input_text(self, input_text: str, prompt: str = DEFAULT_T5_PROMPT) -> float:
        """Predict LOS in days from serialized notebook-style clinical input text."""
        if not self._loaded:
            self.load()

        if not isinstance(input_text, str) or not input_text.strip():
            raise ValueError("input_text must be a non-empty serialized clinical input string.")

        sections = parse_sections(input_text)
        if not any(value.strip() for value in sections.values()):
            raise ValueError(f"input_text must include at least one known section tag: {SECTION_LIST}")

        torch = self._torch
        pooled = torch.zeros(len(SECTION_LIST), HIDDEN_DIM)

        with torch.no_grad():
            # Temporarily swap the extractor's prompt for this request only.
            original_prompt = self._t5.prompt
            self._t5.prompt = prompt
            try:
                for i, section in enumerate(SECTION_LIST):
                    text = sections.get(section, "").strip()
                    if text:
                        embedding = self._t5.encode(text)
                        pooled[i] = torch.tensor(embedding, dtype=torch.float32)
            finally:
                self._t5.prompt = original_prompt

            feature_vector = (
                self._bilstm.encode_document([pooled]).cpu().numpy().reshape(1, -1)
            )
        prediction = self._lgbm.predict(feature_vector)[0]
        return max(0.0, round(float(prediction), 2))

    # ------------------------------------------------------------------
    # Health
    # ------------------------------------------------------------------

    def is_ready(self) -> bool:
        return self._loaded
