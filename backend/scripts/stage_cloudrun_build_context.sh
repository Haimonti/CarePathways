#!/usr/bin/env bash
# Stage a clean build context for `gcloud builds submit`.
#
# Three things `docker build` tolerates that `gcloud builds submit` does not,
# discovered by hitting each one during the first real deploy:
#   1. `gcloud builds submit --tag` has no -f/--dockerfile flag — it only
#      ever looks for a file literally named `Dockerfile` at the context
#      root, so backend/Dockerfile.cloudrun has to be copied there.
#   2. .gcloudignore doesn't support negating a .gitignore rule reliably —
#      backend/cloudrun_artifacts/ is gitignored (keeps binaries out of git)
#      but that means it also silently drops out of the gcloud upload.
#      Simplest fix: build the context somewhere with no ignore files to
#      negotiate at all.
#   3. backend/models/manual/*.{pt,txt,json} are dangling symlinks left over
#      from an old remote dev session (target: /sessions/eloquent-brave-
#      shannon/...) — unrelated to this deploy, but gcloud's file scanner
#      crashes with FileNotFoundError trying to stat them. Excluded below.
#
# Usage: ./stage_cloudrun_build_context.sh [output_dir]
# Prints the staged context path on success.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="${1:-$(mktemp -d /tmp/llos_build_ctx.XXXXXX)}"

rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

rsync -a \
  --exclude='.git' \
  --exclude='.DS_Store' \
  --exclude='__pycache__' \
  --exclude='.pytest_cache' \
  --exclude='models/manual/' \
  "$BACKEND_DIR/" "$OUTPUT_DIR/backend/"

cp "$OUTPUT_DIR/backend/Dockerfile.cloudrun" "$OUTPUT_DIR/Dockerfile"

if find "$OUTPUT_DIR" -type l | grep -q .; then
  echo "WARNING: staged context still contains symlinks — gcloud builds submit will crash on any dangling ones:" >&2
  find "$OUTPUT_DIR" -type l >&2
fi

echo "$OUTPUT_DIR"
