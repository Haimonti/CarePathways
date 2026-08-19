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

cd "$SCRIPT_DIR"
uvicorn app.main:app --host 0.0.0.0 --port "$PORT" --reload
