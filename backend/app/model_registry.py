from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict

from .pipelines.manual.config import DEFAULT_T5_PROMPT
from .pipelines.manual.pipeline import ManualPipeline
from .schemas import ModelInfo
from .settings import ModelBundleSettings, Settings


@dataclass(frozen=True)
class T5LightGbmPipelineSettings:
    model_weights_path: Path
    lgbm_model_path: Path
    extractor_config_path: Path
    cache_dir: Path | None = None


class T5BilstmLightGbmAdapter:
    input_contract = "serialized_clinical_input"

    def __init__(self, settings: ModelBundleSettings):
        self.settings = settings
        pipeline_settings = self._build_pipeline_settings(settings)
        self._validate_required_files(pipeline_settings)
        self.pipeline = ManualPipeline(pipeline_settings)

    @staticmethod
    def _required_path(settings: ModelBundleSettings, key: str) -> Path:
        value = settings.adapter_settings.get(key)
        if not isinstance(value, Path):
            raise RuntimeError(f"Model `{settings.key}` must define `{key}`.")
        return value

    @classmethod
    def _build_pipeline_settings(cls, settings: ModelBundleSettings) -> T5LightGbmPipelineSettings:
        cache_dir_value = settings.adapter_settings.get("cache_dir")
        cache_dir = cache_dir_value if isinstance(cache_dir_value, Path) else None
        return T5LightGbmPipelineSettings(
            model_weights_path=cls._required_path(settings, "model_weights_path"),
            lgbm_model_path=cls._required_path(settings, "lgbm_model_path"),
            extractor_config_path=cls._required_path(settings, "extractor_config_path"),
            cache_dir=cache_dir,
        )

    @staticmethod
    def _validate_required_files(pipeline_settings: T5LightGbmPipelineSettings) -> None:
        missing = [
            path
            for path in (
                pipeline_settings.model_weights_path,
                pipeline_settings.lgbm_model_path,
                pipeline_settings.extractor_config_path,
            )
            if not path.is_file()
        ]
        if missing:
            missing_text = ", ".join(str(path) for path in missing)
            raise FileNotFoundError(f"Missing T5 + LightGBM artifacts: {missing_text}")

    def predict(self, input_text: str, prompt: str = DEFAULT_T5_PROMPT) -> float:
        return self.pipeline.predict_input_text(input_text=input_text, prompt=prompt)


class DeepPatientAdapter:
    input_contract = "serialized_clinical_input"

    _REQUIRED_PATH_KEYS = (
        "word2vec_path",
        "scaler_path",
        "dae_weights_path",
        "mlp_weights_path",
        "meta_path",
    )

    def __init__(self, settings: ModelBundleSettings):
        from .pipelines.deep_patient.pipeline import (
            DeepPatientPipeline,
            DeepPatientPipelineSettings,
        )

        self.settings = settings
        paths = {}
        for key in self._REQUIRED_PATH_KEYS:
            value = settings.adapter_settings.get(key)
            if not isinstance(value, Path):
                raise RuntimeError(f"Model `{settings.key}` must define `{key}`.")
            if not value.is_file():
                raise FileNotFoundError(f"Missing Deep Patient artifact: {value}")
            paths[key] = value
        self.pipeline = DeepPatientPipeline(DeepPatientPipelineSettings(**paths))

    def predict(self, input_text: str, prompt: str = DEFAULT_T5_PROMPT) -> float:
        # `prompt` is part of the shared adapter interface; Deep Patient has no prompt.
        return self.pipeline.predict_input_text(input_text=input_text)


ADAPTERS = {
    "t5_bilstm_lightgbm": T5BilstmLightGbmAdapter,
    "deep_patient": DeepPatientAdapter,
}


@dataclass
class RegisteredModel:
    settings: ModelBundleSettings
    adapter_instance: Any

    @property
    def input_contract(self) -> str:
        return str(self.adapter_instance.input_contract)

    def predict(self, input_text: str, prompt: str = DEFAULT_T5_PROMPT) -> float:
        return self.adapter_instance.predict(input_text=input_text, prompt=prompt)

    def to_model_info(self, default_model_key: str) -> ModelInfo:
        return ModelInfo(
            model_key=self.settings.key,
            display_name=self.settings.display_name,
            description=self.settings.description,
            adapter=self.settings.adapter,
            input_contract=self.input_contract,
            is_default=self.settings.key == default_model_key,
        )


class ModelRegistry:
    def __init__(self, settings: Settings):
        self._settings = settings
        self._models: Dict[str, RegisteredModel] = {}
        self._load_models()

    def _load_models(self) -> None:
        for bundle in self._settings.model_bundles.values():
            adapter_cls = ADAPTERS.get(bundle.adapter)
            if adapter_cls is None:
                supported = ", ".join(sorted(ADAPTERS))
                raise RuntimeError(
                    f"Unsupported adapter `{bundle.adapter}` for model `{bundle.key}`. "
                    f"Supported adapters: {supported}."
                )
            self._models[bundle.key] = RegisteredModel(
                settings=bundle,
                adapter_instance=adapter_cls(bundle),
            )

    def get(self, model_key: str) -> RegisteredModel | None:
        return self._models.get(model_key)

    def list_models(self) -> list[ModelInfo]:
        return [
            model.to_model_info(self._settings.default_model_key)
            for model in self._models.values()
        ]

    @property
    def default_model_key(self) -> str:
        return self._settings.default_model_key

    def is_ready(self) -> bool:
        return bool(self._models)
