from __future__ import annotations

from datetime import datetime
from typing import List

from pydantic import BaseModel, Field, model_validator


LLOS_THRESHOLD_DAYS = 14.0
SECTION_FIELDS = (
    "hpi",
    "social_history",
    "chief_complaint",
    "vitals",
    "labs",
    "conditions",
    "medications",
    "admission_type",
    "admitted_time",
    "admitted_date",
)


class PredictionRequest(BaseModel):
    uuid: int | None = Field(default=None, ge=1)
    model_key: str | None = None
    input: str | None = None
    hpi: str = ""
    social_history: str = ""
    chief_complaint: str = ""
    vitals: str = ""
    labs: str = ""
    conditions: str = ""
    medications: str = ""
    admission_type: str = ""
    admitted_time: str = ""
    admitted_date: str = ""
    prompt: str = "predict patient length of stay: "

    @model_validator(mode="after")
    def validate_prediction_mode(self) -> "PredictionRequest":
        has_uuid = self.uuid is not None
        has_raw_input = isinstance(self.input, str) and bool(self.input.strip())
        has_section_input = any(
            isinstance(getattr(self, field_name), str)
            and bool(getattr(self, field_name).strip())
            for field_name in SECTION_FIELDS
        )
        has_manual_input = has_raw_input or has_section_input

        if has_uuid and has_manual_input:
            raise ValueError("Send either uuid or manual input fields, not both.")
        if not has_uuid and not has_manual_input:
            raise ValueError("Prediction requires uuid, raw input, or at least one clinical section field.")
        return self


class PredictionResponse(BaseModel):
    prediction_id: int
    uuid: int | None = None
    subject_id: str | None = None
    hadm_id: str | None = None
    model_key: str
    predicted_los_days: float
    actual_los_days: float | None = None
    is_llos: bool
    created_at: datetime


class ResultItem(BaseModel):
    prediction_id: int
    uuid: int | None = None
    subject_id: str | None = None
    hadm_id: str | None = None
    model_key: str
    prediction_mode: str
    predicted_los_days: float
    actual_los_days: float | None = None
    is_llos: bool
    created_at: datetime


class ResultsResponse(BaseModel):
    items: List[ResultItem]
    limit: int
    offset: int


class ModelInfo(BaseModel):
    model_key: str
    display_name: str
    description: str
    adapter: str
    input_contract: str
    is_default: bool


class ModelsResponse(BaseModel):
    items: List[ModelInfo]


class HealthResponse(BaseModel):
    status: str
    database_ok: bool
    model_registry_ok: bool
    available_models: List[str]


class RecordItem(BaseModel):
    uuid: int
    subject_id: str
    hadm_id: str
    admittime: str | None = None
    dischtime: str | None = None
    actual_los_days: float | None = None
    anchor_age: float | None = None
    gender: str | None = None
    race: str | None = None
    ethnicity: str | None = None
    admission_type: str | None = None


class RecordsResponse(BaseModel):
    items: List[RecordItem]
    total: int
    limit: int
    offset: int


class RecordDetailResponse(RecordItem):
    input_text: str | None = None
    source_path: str | None = None
    ingested_at: datetime
