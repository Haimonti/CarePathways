# Render Deployment

This backend is configured for Render as a Docker web service with a persistent
disk mounted at `/var/data`.

## What Render Runs

The root [render.yaml](/Users/william/Documents/Cowork/LLOS/render.yaml)
defines:

- Service name: `llos-backend`
- Runtime: Docker
- Dockerfile: `CarePathways/backend/Dockerfile.render`
- Health check: `/health`
- Persistent disk: `/var/data`
- Startup script: `backend/scripts/start_render.sh`

The Render config file is
[config.render.yaml](/Users/william/Documents/Cowork/LLOS/CarePathways/backend/config.render.yaml).
It expects these files on the Render disk:

```text
/var/data/models/los_model_weights.pt
/var/data/models/lgbm_los_model.txt
/var/data/models/t5_extractor_config.json
/var/data/Dataset.csv
```

`Dataset.csv` is optional at boot, but stored-record predictions by `uuid` will
not work until it exists and has been ingested. The three model artifacts are
required.

## Recommended Setup

1. Commit and push the Render files.

2. In Render, create a new Blueprint from the repository root. Render will use
   `render.yaml`.

3. Use at least the `standard` instance type for the first deployment. The T5,
   PyTorch, and LightGBM stack is too heavy for very small instances.

4. Put the artifacts on the persistent disk. The easiest path is to package
   them locally:

```bash
cd /Users/william/Documents/Cowork/LLOS/CarePathways

python -m backend.tools.package_render_artifacts \
  --config backend/config.yaml \
  --dataset /Users/william/Documents/LLOS/data/processed/Dataset.csv \
  --output /tmp/llos_render_artifacts.tar.gz
```

5. Upload that tarball somewhere Render can download it from, such as a private
   release asset or temporary signed URL.

6. Set this Render environment variable:

```text
LLOS_ARTIFACT_BUNDLE_URL=https://your-download-url/llos_render_artifacts.tar.gz
```

On startup, `start_render.sh` downloads the bundle into `/var/data`, verifies
the model files, ingests `Dataset.csv` idempotently, and starts Uvicorn.

## Manual Disk Layout

If you use Render Shell or SSH instead of `LLOS_ARTIFACT_BUNDLE_URL`, arrange
the disk like this:

```bash
mkdir -p /var/data/models /var/data/t5_cache

# copy files into:
/var/data/models/los_model_weights.pt
/var/data/models/lgbm_los_model.txt
/var/data/models/t5_extractor_config.json
/var/data/Dataset.csv
```

Then redeploy or restart the service. The startup script will ingest the CSV.

## Verify

After the service is live:

```bash
curl https://your-render-service.onrender.com/health
curl https://your-render-service.onrender.com/models
curl "https://your-render-service.onrender.com/records?limit=5"
curl -X POST https://your-render-service.onrender.com/predictions \
  -H "Content-Type: application/json" \
  -d '{"uuid": 1, "model_key": "lightgbm_t5"}'
```

For the Netlify-hosted Flutter app, set:

```text
BACKEND_ORIGIN=https://your-render-service.onrender.com
```
