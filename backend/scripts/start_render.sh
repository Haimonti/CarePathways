#!/usr/bin/env bash
set -euo pipefail

CONFIG_PATH="${LLOS_BACKEND_CONFIG:-backend/config.render.yaml}"
DATA_DIR="${LLOS_RENDER_DATA_DIR:-/var/data}"
DATASET_PATH="${LLOS_DATASET_PATH:-${DATA_DIR}/Dataset.csv}"
PORT_VALUE="${PORT:-10000}"

MODEL_WEIGHTS_PATH="${DATA_DIR}/models/los_model_weights.pt"
LGBM_MODEL_PATH="${DATA_DIR}/models/lgbm_los_model.txt"
EXTRACTOR_CONFIG_PATH="${DATA_DIR}/models/t5_extractor_config.json"

mkdir -p "${DATA_DIR}/models" "${DATA_DIR}/t5_cache"

if [[ -n "${LLOS_ARTIFACT_BUNDLE_URL:-}" ]]; then
  if [[ ! -f "${MODEL_WEIGHTS_PATH}" || ! -f "${LGBM_MODEL_PATH}" || ! -f "${EXTRACTOR_CONFIG_PATH}" ]]; then
    echo "Downloading LLOS artifact bundle into ${DATA_DIR}..."
    curl -fL "${LLOS_ARTIFACT_BUNDLE_URL}" -o /tmp/llos_render_artifacts.tar.gz
    tar -xzf /tmp/llos_render_artifacts.tar.gz -C "${DATA_DIR}"
  else
    echo "Model artifacts already exist on the Render disk; skipping artifact download."
  fi
fi

missing=()
for path in "${MODEL_WEIGHTS_PATH}" "${LGBM_MODEL_PATH}" "${EXTRACTOR_CONFIG_PATH}"; do
  if [[ ! -f "${path}" ]]; then
    missing+=("${path}")
  fi
done

if (( ${#missing[@]} > 0 )); then
  echo "Missing required model artifact(s) on the Render disk:" >&2
  printf '  - %s\n' "${missing[@]}" >&2
  echo "Upload them to ${DATA_DIR}/models or set LLOS_ARTIFACT_BUNDLE_URL to a tar.gz bundle." >&2
  exit 1
fi

if [[ -f "${DATASET_PATH}" ]]; then
  echo "Ingesting dataset rows from ${DATASET_PATH}..."
  python -m backend.tools.ingest_dataset_inputs \
    --config "${CONFIG_PATH}" \
    --dataset "${DATASET_PATH}"
else
  echo "No Dataset.csv found at ${DATASET_PATH}; stored-record predictions will be unavailable until it is uploaded."
fi

exec python -m uvicorn backend.app.main:app \
  --host 0.0.0.0 \
  --port "${PORT_VALUE}"
