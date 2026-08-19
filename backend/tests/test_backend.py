from __future__ import annotations

import csv
import sys
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from backend.app.main import create_app
from backend.app.model_registry import ModelRegistry
from backend.app.repositories.sqlite import SQLiteRepository
from backend.app.settings import load_settings
from backend.tools.ingest_dataset_inputs import ingest_dataset, main as ingest_main


class FakeManualPipeline:
    calls: list[tuple[str, str]] = []

    def __init__(self, settings: object) -> None:
        self.settings = settings

    def load(self) -> None:
        return None

    def predict_input_text(self, input_text: str, prompt: str) -> float:
        self.calls.append((input_text, prompt))
        return 16.25 if "long stay" in input_text else 4.5


def _write_dataset(path: Path) -> None:
    rows = [
        {
            "subject_id": "subject-1",
            "hadm_id": "enc-1",
            "input": "<hpi> long stay risk </s> <vitals> BP 150/90 </s>",
            "target": "19",
            "admittime": "2024-01-01 09:00:00",
            "dischtime": "2024-01-20 09:00:00",
            "anchor_age": "67",
            "gender": "M",
            "race": "white",
            "ethnicity": "nonhispanic",
            "birthdate": "1957-01-01",
            "admission_type": "inpatient",
        },
        {
            "subject_id": "subject-2",
            "hadm_id": "enc-2",
            "input": "<hpi> short admission </s> <labs> No abnormal labs </s>",
            "target": "4",
            "admittime": "2024-01-02 09:00:00",
            "dischtime": "2024-01-06 09:00:00",
            "anchor_age": "42",
            "gender": "F",
            "race": "black",
            "ethnicity": "nonhispanic",
            "birthdate": "1982-01-01",
            "admission_type": "urgentcare",
        },
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def _write_config(tmp_path: Path, missing_artifact: bool = False) -> Path:
    models_dir = tmp_path / "models"
    models_dir.mkdir()
    model_weights = models_dir / "los_model_weights.pt"
    lgbm_model = models_dir / "lgbm_los_model.txt"
    extractor_config = models_dir / "t5_extractor_config.json"
    if not missing_artifact:
        model_weights.write_text("fake torch weights", encoding="utf-8")
        lgbm_model.write_text("fake lightgbm model", encoding="utf-8")
        extractor_config.write_text('{"model_name": "t5-small"}', encoding="utf-8")

    config_path = tmp_path / "config.yaml"
    config_path.write_text(
        "\n".join(
            [
                "database:",
                "  path: ./test_backend.db",
                "  default_results_limit: 20",
                "",
                "models:",
                "  default_model_key: lightgbm_t5",
                "  bundles:",
                "    lightgbm_t5:",
                "      display_name: T5 + BiLSTM + LightGBM",
                "      description: Test notebook prediction model",
                "      adapter: t5_bilstm_lightgbm",
                "      model_weights_path: ./models/los_model_weights.pt",
                "      lgbm_model_path: ./models/lgbm_los_model.txt",
                "      extractor_config_path: ./models/t5_extractor_config.json",
                "      cache_dir: ./cache/t5_cache",
            ]
        ),
        encoding="utf-8",
    )
    return config_path


@pytest.fixture()
def configured_backend(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> dict[str, Path]:
    monkeypatch.setattr("backend.app.model_registry.ManualPipeline", FakeManualPipeline)
    FakeManualPipeline.calls = []

    dataset_path = tmp_path / "Dataset.csv"
    _write_dataset(dataset_path)
    config_path = _write_config(tmp_path)
    ingest_dataset(config_path=config_path, dataset_path=dataset_path)
    return {"config_path": config_path, "dataset_path": dataset_path}


@pytest.fixture()
def app_client(configured_backend: dict[str, Path]) -> TestClient:
    app = create_app(str(configured_backend["config_path"]))
    return TestClient(app)


def test_load_settings_resolves_t5_adapter_paths(tmp_path: Path) -> None:
    config_path = _write_config(tmp_path)
    settings = load_settings(str(config_path))
    bundle = settings.model_bundles["lightgbm_t5"]

    assert settings.default_model_key == "lightgbm_t5"
    assert bundle.adapter == "t5_bilstm_lightgbm"
    assert bundle.adapter_settings["model_weights_path"] == tmp_path / "models" / "los_model_weights.pt"
    assert bundle.adapter_settings["cache_dir"] == tmp_path / "cache" / "t5_cache"


def test_model_registry_loads_lightgbm_t5_model(
    configured_backend: dict[str, Path],
) -> None:
    settings = load_settings(str(configured_backend["config_path"]))
    registry = ModelRegistry(settings)
    models = registry.list_models()

    assert len(models) == 1
    assert models[0].model_key == "lightgbm_t5"
    assert models[0].adapter == "t5_bilstm_lightgbm"
    assert models[0].input_contract == "serialized_clinical_input"
    assert models[0].is_default is True


def test_model_registry_missing_artifact_fails_clearly(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr("backend.app.model_registry.ManualPipeline", FakeManualPipeline)
    settings = load_settings(str(_write_config(tmp_path, missing_artifact=True)))

    with pytest.raises(FileNotFoundError, match="Missing T5 \\+ LightGBM artifacts"):
        ModelRegistry(settings)


def test_model_registry_rejects_unknown_adapter(tmp_path: Path) -> None:
    config_path = _write_config(tmp_path)
    text = config_path.read_text(encoding="utf-8").replace(
        "adapter: t5_bilstm_lightgbm",
        "adapter: deep_patient_future",
    )
    config_path.write_text(text, encoding="utf-8")
    settings = load_settings(str(config_path))

    with pytest.raises(RuntimeError, match="Unsupported adapter"):
        ModelRegistry(settings)


def test_ingest_dataset_inputs_imports_uuid_rows_idempotently(tmp_path: Path) -> None:
    dataset_path = tmp_path / "Dataset.csv"
    _write_dataset(dataset_path)
    config_path = _write_config(tmp_path)

    assert ingest_dataset(config_path=config_path, dataset_path=dataset_path) == 2
    assert ingest_dataset(config_path=config_path, dataset_path=dataset_path) == 2

    repository = SQLiteRepository(load_settings(str(config_path)).database.path)
    record = repository.get_dataset_record(uuid=1)
    items, total = repository.list_records(query=None, limit=10, offset=0)

    assert total == 2
    assert len(items) == 2
    assert record is not None
    assert record["uuid"] == 1
    assert record["subject_id"] == "subject-1"
    assert record["hadm_id"] == "enc-1"
    assert record["input_text"].startswith("<hpi>")
    assert record["actual_los_days"] == 19.0
    assert record["source_path"] == str(dataset_path.resolve())


def test_ingest_cli_runs(configured_backend: dict[str, Path]) -> None:
    previous_argv = sys.argv[:]
    try:
        sys.argv = [
            "ingest_dataset_inputs",
            "--config",
            str(configured_backend["config_path"]),
            "--dataset",
            str(configured_backend["dataset_path"]),
        ]
        ingest_main()
    finally:
        sys.argv = previous_argv


def test_post_predictions_by_uuid(app_client: TestClient) -> None:
    response = app_client.post("/predictions", json={"uuid": 1})

    assert response.status_code == 200
    payload = response.json()
    assert payload["uuid"] == 1
    assert payload["subject_id"] == "subject-1"
    assert payload["hadm_id"] == "enc-1"
    assert payload["model_key"] == "lightgbm_t5"
    assert payload["predicted_los_days"] == 16.25
    assert payload["actual_los_days"] == 19.0
    assert payload["is_llos"] is True


def test_post_predictions_missing_uuid_returns_404(app_client: TestClient) -> None:
    response = app_client.post("/predictions", json={"uuid": 999})

    assert response.status_code == 404
    assert "uuid=999" in response.json()["detail"]


def test_post_predictions_unknown_model_returns_400(app_client: TestClient) -> None:
    response = app_client.post(
        "/predictions",
        json={"uuid": 1, "model_key": "not-a-model"},
    )

    assert response.status_code == 400
    assert "Unknown model_key" in response.json()["detail"]


def test_post_predictions_manual_raw_input(app_client: TestClient) -> None:
    response = app_client.post(
        "/predictions",
        json={"input": "<hpi> short admission </s>"},
    )

    assert response.status_code == 200
    payload = response.json()
    assert payload["uuid"] is None
    assert payload["subject_id"] is None
    assert payload["predicted_los_days"] == 4.5
    assert payload["actual_los_days"] is None
    assert payload["is_llos"] is False


def test_post_predictions_manual_section_fields(app_client: TestClient) -> None:
    response = app_client.post(
        "/predictions",
        json={
            "hpi": "long stay risk",
            "vitals": "BP 150/90",
            "prompt": "predict patient length of stay: ",
        },
    )

    assert response.status_code == 200
    payload = response.json()
    assert payload["predicted_los_days"] == 16.25
    called_input, called_prompt = FakeManualPipeline.calls[-1]
    assert "<hpi> long stay risk </s>" in called_input
    assert "<vitals> BP 150/90 </s>" in called_input
    assert called_prompt == "predict patient length of stay: "


def test_post_predictions_invalid_empty_body_returns_422(app_client: TestClient) -> None:
    response = app_client.post("/predictions", json={})

    assert response.status_code == 422


def test_get_records_lists_and_searches(app_client: TestClient) -> None:
    all_records = app_client.get("/records").json()
    searched = app_client.get("/records", params={"query": "subject-2"}).json()

    assert all_records["total"] == 2
    assert all_records["items"][0]["uuid"] == 1
    assert searched["total"] == 1
    assert searched["items"][0]["uuid"] == 2


def test_get_record_detail_returns_input_text(app_client: TestClient) -> None:
    response = app_client.get("/records/1")

    assert response.status_code == 200
    payload = response.json()
    assert payload["uuid"] == 1
    assert payload["input_text"].startswith("<hpi>")
    assert payload["actual_los_days"] == 19.0


def test_get_results_returns_newest_first(app_client: TestClient) -> None:
    app_client.post("/predictions", json={"uuid": 1})
    app_client.post("/predictions", json={"input": "<hpi> short admission </s>"})

    response = app_client.get("/results", params={"limit": 10})

    assert response.status_code == 200
    payload = response.json()
    assert len(payload["items"]) == 2
    assert payload["items"][0]["prediction_mode"] == "manual"
    assert payload["items"][1]["prediction_mode"] == "stored"


def test_get_models_and_health(app_client: TestClient) -> None:
    models = app_client.get("/models")
    health = app_client.get("/health")

    assert models.status_code == 200
    assert models.json()["items"][0]["model_key"] == "lightgbm_t5"
    assert models.json()["items"][0]["input_contract"] == "serialized_clinical_input"
    assert health.status_code == 200
    assert health.json()["available_models"] == ["lightgbm_t5"]


def test_public_schemas_do_not_expose_legacy_branch_fields(app_client: TestClient) -> None:
    prediction = app_client.post("/predictions", json={"uuid": 1}).json()
    model = app_client.get("/models").json()["items"][0]

    assert "branch" not in prediction
    assert set(model) == {
        "model_key",
        "display_name",
        "description",
        "adapter",
        "input_contract",
        "is_default",
    }
