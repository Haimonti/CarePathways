from __future__ import annotations

from datetime import datetime

from fastapi import APIRouter, HTTPException, Query, Request, status

from ..schemas import (
    HealthResponse,
    ModelsResponse,
    PredictionRequest,
    PredictionResponse,
    RecordDetailResponse,
    RecordsResponse,
    ResultsResponse,
)

router = APIRouter()


@router.post("/predictions", response_model=PredictionResponse)
def create_prediction(request_body: PredictionRequest, request: Request) -> PredictionResponse:
    service = request.app.state.inference_service
    try:
        return service.predict(request_body)
    except KeyError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc
    except LookupError as exc:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=str(exc)) from exc
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_422_UNPROCESSABLE_ENTITY, detail=str(exc)) from exc


@router.get("/records", response_model=RecordsResponse)
def list_records(
    request: Request,
    query: str | None = Query(default=None, description="Search by uuid, subject_id, or hadm_id"),
    limit: int = Query(default=20, ge=1, le=100),
    offset: int = Query(default=0, ge=0),
) -> RecordsResponse:
    repository = request.app.state.repository
    items, total = repository.list_records(query=query, limit=limit, offset=offset)
    return RecordsResponse(items=items, total=total, limit=limit, offset=offset)


@router.get("/records/{uuid}", response_model=RecordDetailResponse)
def get_record(uuid: int, request: Request) -> RecordDetailResponse:
    repository = request.app.state.repository
    record = repository.get_dataset_record(uuid=uuid)
    if record is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Dataset record not found: uuid={uuid}",
        )
    return RecordDetailResponse(
        uuid=record["uuid"],
        subject_id=record["subject_id"],
        hadm_id=record["hadm_id"],
        admittime=record.get("admittime"),
        dischtime=record.get("dischtime"),
        actual_los_days=record.get("actual_los_days"),
        anchor_age=record.get("anchor_age"),
        gender=record.get("gender"),
        race=record.get("race"),
        ethnicity=record.get("ethnicity"),
        admission_type=record.get("admission_type"),
        input_text=record.get("input_text"),
        source_path=record.get("source_path"),
        ingested_at=datetime.fromisoformat(str(record["ingested_at"])),
    )


@router.get("/results", response_model=ResultsResponse)
def list_results(
    request: Request,
    limit: int | None = Query(default=None, ge=1, le=100),
    offset: int = Query(default=0, ge=0),
) -> ResultsResponse:
    repository = request.app.state.repository
    if limit is None:
        limit = request.app.state.settings.database.default_results_limit
    items = repository.list_predictions(limit=limit, offset=offset)
    return ResultsResponse(items=items, limit=limit, offset=offset)


@router.get("/models", response_model=ModelsResponse)
def list_models(request: Request) -> ModelsResponse:
    registry = request.app.state.model_registry
    return ModelsResponse(items=registry.list_models())


@router.get("/health", response_model=HealthResponse)
def health_check(request: Request) -> HealthResponse:
    repository = request.app.state.repository
    registry = request.app.state.model_registry

    database_ok = repository.is_ready()
    model_registry_ok = registry.is_ready()
    if not database_ok or not model_registry_ok:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Backend dependencies are not ready.",
        )

    return HealthResponse(
        status="ok",
        database_ok=database_ok,
        model_registry_ok=model_registry_ok,
        available_models=[item.model_key for item in registry.list_models()],
    )
