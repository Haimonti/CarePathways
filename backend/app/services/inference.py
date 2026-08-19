from __future__ import annotations

from ..model_registry import ModelRegistry
from ..pipelines.manual.utils import build_structured_input
from ..repositories.sqlite import SQLiteRepository
from ..schemas import LLOS_THRESHOLD_DAYS, PredictionRequest, PredictionResponse, SECTION_FIELDS


class InferenceService:
    def __init__(self, repository: SQLiteRepository, model_registry: ModelRegistry):
        self.repository = repository
        self.model_registry = model_registry

    def predict(self, request_body: PredictionRequest) -> PredictionResponse:
        resolved_model_key = request_body.model_key or self.model_registry.default_model_key
        model = self.model_registry.get(resolved_model_key)
        if model is None:
            raise KeyError(f"Unknown model_key: {resolved_model_key}")

        if request_body.uuid is not None:
            record = self.repository.get_dataset_record(uuid=request_body.uuid)
            if record is None:
                raise LookupError(f"Dataset record not found: uuid={request_body.uuid}")

            input_text = str(record["input_text"])
            prediction_mode = "stored"
            uuid = int(record["uuid"])
            subject_id = str(record["subject_id"])
            hadm_id = str(record["hadm_id"])
            actual_los_days = record.get("actual_los_days")
        else:
            input_text = self._manual_input_text(request_body)
            prediction_mode = "manual"
            uuid = None
            subject_id = None
            hadm_id = None
            actual_los_days = None

        predicted_los_days = model.predict(input_text=input_text, prompt=request_body.prompt)
        is_llos = predicted_los_days > LLOS_THRESHOLD_DAYS
        stored_result = self.repository.insert_prediction(
            uuid=uuid,
            subject_id=subject_id,
            hadm_id=hadm_id,
            model_key=resolved_model_key,
            prediction_mode=prediction_mode,
            predicted_los_days=predicted_los_days,
            actual_los_days=float(actual_los_days) if actual_los_days is not None else None,
            is_llos=is_llos,
        )

        return PredictionResponse(
            prediction_id=stored_result.prediction_id,
            uuid=stored_result.uuid,
            subject_id=stored_result.subject_id,
            hadm_id=stored_result.hadm_id,
            model_key=stored_result.model_key,
            predicted_los_days=stored_result.predicted_los_days,
            actual_los_days=stored_result.actual_los_days,
            is_llos=stored_result.is_llos,
            created_at=stored_result.created_at,
        )

    @staticmethod
    def _manual_input_text(request_body: PredictionRequest) -> str:
        if isinstance(request_body.input, str) and request_body.input.strip():
            return request_body.input.strip()

        form_data = {
            field_name: getattr(request_body, field_name)
            for field_name in SECTION_FIELDS
            if isinstance(getattr(request_body, field_name), str)
            and getattr(request_body, field_name).strip()
        }
        structured = build_structured_input(form_data)
        if not structured:
            raise ValueError("Manual prediction requires raw input or at least one clinical section field.")
        return structured
