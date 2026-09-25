#!/usr/bin/env bash
# Start the unified LLOS backend.
#
# Usage:
#   ./run.sh              # default port 8000
#   ./run.sh 9000         # custom port
#
# Prerequisites:
#   1. Copy config.example.yaml → config.yaml and fill in model paths.
#   2. pip install -r requirements.txt
#   3. Seed SQLite with backend.tools.ingest_dataset_inputs.

set -euo pipefail

PORT="${1:-8000}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Starting LLOS unified backend on http://localhost:${PORT}"
echo "  Predict: POST /predictions"
echo "  Records: GET /records"
echo ""

# torch and xgboost/lightgbm each ship their own libomp on macOS; loading both
# segfaults in xgboost's load_model without these (Dockerfile.cloudrun sets them too).
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export KMP_DUPLICATE_LIB_OK=TRUE

cd "$SCRIPT_DIR"
uvicorn app.main:app --host 0.0.0.0 --port "$PORT" --reload
