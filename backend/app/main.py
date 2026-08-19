from __future__ import annotations

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from .api.routes import router
from .model_registry import ModelRegistry
from .repositories.sqlite import SQLiteRepository
from .services.inference import InferenceService
from .settings import load_settings


def create_app(config_path: str | None = None) -> FastAPI:
    settings = load_settings(config_path)

    repository = SQLiteRepository(settings.database.path)
    repository.initialize()
    model_registry = ModelRegistry(settings)
    inference_service = InferenceService(repository=repository, model_registry=model_registry)

    app = FastAPI(
        title="LLOS Unified API",
        version="1.0.0",
        description=(
            "Unified inference backend for the LLOS mobile application. "
            "Predictions can use a stored Dataset.csv row by uuid or manual clinical input."
        ),
    )
    app.state.settings = settings
    app.state.repository = repository
    app.state.model_registry = model_registry
    app.state.inference_service = inference_service

    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    app.include_router(router)
    return app


def _create_startup_error_app(exc: Exception) -> FastAPI:
    app = FastAPI(
        title="LLOS Backend API",
        version="0.1.0",
        description="FastAPI inference backend for the LLOS mobile application.",
    )

    @app.on_event("startup")
    async def _raise_startup_error() -> None:
        raise exc

    return app


try:
    app = create_app()
except Exception as exc:  # pragma: no cover - exercised implicitly during import
    app = _create_startup_error_app(exc)
