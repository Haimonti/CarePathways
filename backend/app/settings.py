from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict
import os


@dataclass(frozen=True)
class DatabaseSettings:
    path: Path
    default_results_limit: int


@dataclass(frozen=True)
class ModelBundleSettings:
    key: str
    display_name: str
    description: str
    adapter: str
    artifact_path: Path | None
    adapter_settings: Dict[str, Any]


@dataclass(frozen=True)
class Settings:
    config_path: Path
    database: DatabaseSettings
    model_bundles: Dict[str, ModelBundleSettings]
    default_model_key: str


def _load_yaml(config_path: Path) -> dict:
    try:
        import yaml
    except ImportError as exc:
        raise RuntimeError(
            "PyYAML is required for backend config loading. Install dependencies from backend/requirements.txt."
        ) from exc

    with config_path.open("r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle) or {}

    if not isinstance(data, dict):
        raise RuntimeError(f"Invalid backend config format: {config_path}")

    return data


def resolve_backend_config_path(config_path: str | Path | None = None) -> Path:
    if config_path is not None:
        return Path(config_path).expanduser().resolve()

    env_value = os.getenv("LLOS_BACKEND_CONFIG")
    if env_value:
        return Path(env_value).expanduser().resolve()

    return (Path(__file__).resolve().parents[1] / "config.yaml").resolve()


def _resolve_config_path(config_dir: Path, value: str) -> Path:
    path = Path(value).expanduser()
    if not path.is_absolute():
        path = config_dir / path
    return path.resolve()


def load_settings(config_path: str | Path | None = None) -> Settings:
    resolved_path = resolve_backend_config_path(config_path)
    if not resolved_path.exists():
        raise FileNotFoundError(
            f"Missing backend config: {resolved_path}. Copy backend/config.example.yaml to backend/config.yaml."
        )

    config = _load_yaml(resolved_path)
    config_dir = resolved_path.parent

    database_cfg = config.get("database")
    if not isinstance(database_cfg, dict):
        raise RuntimeError("Backend config must define a `database` section.")

    db_path_value = database_cfg.get("path")
    if not isinstance(db_path_value, str) or not db_path_value.strip():
        raise RuntimeError("Backend config must define database.path.")

    database = DatabaseSettings(
        path=_resolve_config_path(config_dir, db_path_value),
        default_results_limit=int(database_cfg.get("default_results_limit", 20)),
    )

    models_cfg = config.get("models")
    if not isinstance(models_cfg, dict):
        raise RuntimeError("Backend config must define a `models` section.")

    default_model_key = models_cfg.get("default_model_key")
    bundles_cfg = models_cfg.get("bundles")
    if not isinstance(default_model_key, str) or not default_model_key.strip():
        raise RuntimeError("Backend config must define models.default_model_key.")
    if not isinstance(bundles_cfg, dict) or not bundles_cfg:
        raise RuntimeError("Backend config must define at least one model bundle.")

    model_bundles: Dict[str, ModelBundleSettings] = {}
    for key, bundle in bundles_cfg.items():
        if not isinstance(bundle, dict):
            raise RuntimeError(f"Model bundle `{key}` must be a mapping.")
        artifact_path_value = bundle.get("artifact_path")
        resolved_artifact_path: Path | None = None
        if isinstance(artifact_path_value, str) and artifact_path_value.strip():
            resolved_artifact_path = _resolve_config_path(config_dir, artifact_path_value)
        elif artifact_path_value is not None:
            raise RuntimeError(f"Model bundle `{key}` artifact_path must be a string when provided.")
        adapter_settings = {
            setting_key: value
            for setting_key, value in bundle.items()
            if setting_key
            not in {"display_name", "description", "adapter", "artifact_path"}
        }
        for path_key in (
            "model_weights_path",
            "lgbm_model_path",
            "extractor_config_path",
            "cache_dir",
            "dataset_path",
            "word2vec_path",
            "scaler_path",
            "dae_weights_path",
            "mlp_weights_path",
            "meta_path",
        ):
            path_value = adapter_settings.get(path_key)
            if isinstance(path_value, str) and path_value.strip():
                adapter_settings[path_key] = _resolve_config_path(config_dir, path_value)

        model_bundles[key] = ModelBundleSettings(
            key=key,
            display_name=str(bundle.get("display_name", key)),
            description=str(bundle.get("description", "")),
            adapter=str(bundle.get("adapter", "")),
            artifact_path=resolved_artifact_path,
            adapter_settings=adapter_settings,
        )

    if default_model_key not in model_bundles:
        raise RuntimeError(f"Default model `{default_model_key}` is not declared in models.bundles.")

    return Settings(
        config_path=resolved_path,
        database=database,
        model_bundles=model_bundles,
        default_model_key=default_model_key,
    )
