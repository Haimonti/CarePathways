# LLOS FastAPI Backend

This backend serves the `predict.ipynb` flow end to end. It stores rows from
`Dataset.csv` in SQLite, looks them up by generated integer `uuid`, runs the
serialized clinical `input` text through T5 + BiLSTM, and predicts LOS days
with the selected model.

## What It Serves

- Stored-row prediction by `uuid`
- Manual prediction from either raw serialized `input` text or section fields
- Model selection through `model_key`
- Default model adapter: `t5_bilstm_lightgbm`
- Primary output: `predicted_los_days`
- Observed output for stored rows: `actual_los_days`

## Local Testing Flow

1. Confirm these model files exist:

```text
/Users/william/Documents/LLOS/data/models/los_model_weights.pt
/Users/william/Documents/LLOS/data/models/lgbm_los_model.txt
/Users/william/Documents/LLOS/data/models/t5_extractor_config.json
```

2. Seed SQLite from `Dataset.csv`:

```bash
python -m backend.tools.ingest_dataset_inputs \
  --config backend/config.yaml \
  --dataset /Users/william/Documents/LLOS/data/processed/Dataset.csv
```

3. Start the API from the `CarePathways/` root:

```bash
python -m uvicorn backend.app.main:app --reload --app-dir .
```

4. Predict from a stored row:

```bash
curl -X POST http://127.0.0.1:8000/predictions \
  -H "Content-Type: application/json" \
  -d '{"uuid": 1, "model_key": "lightgbm_t5"}'
```

5. Predict from manual section fields:

```bash
curl -X POST http://127.0.0.1:8000/predictions \
  -H "Content-Type: application/json" \
  -d '{
    "model_key": "lightgbm_t5",
    "hpi": "67-year-old male with chest tightness and exertional dyspnea.",
    "chief_complaint": "Chest tightness - Shortness of breath",
    "vitals": "BP 158/94 HR 88 RR 18 SpO2 94%",
    "conditions": "Hypertension; Type 2 diabetes; CKD stage 3",
    "medications": "Lisinopril; Metformin; Atorvastatin",
    "admission_type": "inpatient",
    "admitted_time": "09:15am",
    "admitted_date": "14 March, 2024"
  }'
```

## Endpoints

- `POST /predictions`
- `GET /records`
- `GET /records/{uuid}`
- `GET /results`
- `GET /models`
- `GET /health`

## Config

Use [config.example.yaml](/Users/william/Documents/Cowork/LLOS/CarePathways/backend/config.example.yaml)
as the template for [config.yaml](/Users/william/Documents/Cowork/LLOS/CarePathways/backend/config.yaml).

The default bundle uses:

- `adapter: t5_bilstm_lightgbm`
- `model_weights_path`
- `lgbm_model_path`
- `extractor_config_path`
- optional `cache_dir`

Future models should be added as new model bundles and adapters so the app can
keep using the same `/predictions` request shape.

## Verification

Run tests from the workspace root:

```bash
pytest CarePathways/backend/tests -q
```

## Netlify

Netlify should host the Flutter web app and proxy `/api/*` to this FastAPI
service. The Python inference backend itself should still run on a Python host.
See [docs/netlify.md](/Users/william/Documents/Cowork/LLOS/CarePathways/backend/docs/netlify.md).

## Render

Render is the recommended Python host for this backend. The repo includes a
Docker-based Render Blueprint plus persistent-disk config. See
[docs/render.md](/Users/william/Documents/Cowork/LLOS/CarePathways/backend/docs/render.md).
