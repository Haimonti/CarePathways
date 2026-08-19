# Google Cloud Run Deployment

The backend runs as a Docker container on Cloud Run. Chosen over Hugging Face
Spaces (Docker/Gradio now require a PRO subscription, since July 2026) and
over Render's free tier (512MB RAM is a real risk for the torch + transformers
+ lightgbm + gensim stack loaded together). Cloud Run's Always Free tier gives
2M requests/month and enough configurable memory per revision to comfortably
fit this stack, in exchange for needing a billing account on file (you won't
be charged while under the free quota, but Google requires a card to enable
the API as of Feb 2026).

## One-Time Setup

Done as of 2026-08-19 — recorded here so it doesn't need repeating, and so a
fresh project can be stood up the same way if this one is ever recreated.

- **Project:** `llos-backend`, created via `gcloud projects create llos-backend`.
- **Billing:** linked to account `014B0C-C5D7EB-E7E00E` ("Firebase Payment" —
  Google's auto-generated label, not Firebase-specific; this is the billing
  account that also covers `projekt-david-foundation`). Chosen over the other
  open billing account on file, which already covers three unrelated
  projects (`tickix-6ad94`, `goalique1`, `premier-specialist-hospital`).
- **Region:** `us-central1`, set as the default (`gcloud config set run/region us-central1`)
  — one of the three regions (`us-central1`, `us-east1`, `us-west1`) the
  Always Free tier applies to. Deploying elsewhere means paying from the
  first request.
- **APIs enabled:** `run.googleapis.com`, `artifactregistry.googleapis.com`,
  `cloudbuild.googleapis.com` (needed by `gcloud builds submit`), and
  `cloudresourcemanager.googleapis.com` (a handful of other gcloud commands,
  e.g. `gcloud billing projects describe`, assume this is on).
- **Artifact Registry repo:** `llos` (Docker format) in `us-central1`, created
  via `gcloud artifacts repositories create llos --repository-format=docker --location=us-central1`.

**Gotcha hit during setup, worth knowing if this is ever repeated:** right
after creating a brand-new project, `gcloud artifacts repositories create`
failed with `PERMISSION_DENIED` even though the account correctly held
`roles/owner` (confirmed via `gcloud projects get-iam-policy`). This is IAM/API
propagation lag on freshly created projects, not a real permissions problem —
it resolved on its own after about 20 seconds. If it happens again, just
retry after a short wait rather than debugging IAM.

Reusable commands for setting this up on a different project:

```bash
gcloud projects create PROJECT_ID --name="Display Name"
gcloud billing projects link PROJECT_ID --billing-account=BILLING_ACCOUNT_ID
gcloud config set project PROJECT_ID
gcloud config set run/region us-central1
gcloud services enable run.googleapis.com artifactregistry.googleapis.com \
  cloudbuild.googleapis.com cloudresourcemanager.googleapis.com
gcloud artifacts repositories create llos --repository-format=docker --location=us-central1
```

`gcloud` itself: install via `brew install --cask google-cloud-sdk` on macOS,
or skip local install entirely and use
[Cloud Shell](https://console.cloud.google.com/?cloudshell=true) in the
browser — it comes with `gcloud` and `docker` preinstalled. Either way,
`gcloud auth login` opens a browser to sign in — that step needs to be done
interactively by whoever's account it is.

## Deploy

**First deploy: 2026-08-19.** Live at
`https://llos-backend-658435340196.us-central1.run.app` — verified `/health`,
`/models` (both `lightgbm_t5` and `deep_patient` listed), and predictions with
both models (cold start ~18s once, then ~0.7–1.4s warm; matches local timing).

From `CarePathways/`:

```bash
# 1. Stage model weights + Dataset.csv into the build context (gitignored,
#    regenerate whenever the models are retrained).
python -m backend.tools.prepare_cloudrun_artifacts

# 2. Stage a CLEAN build context, then submit it. Do not run
#    `gcloud builds submit .` directly from CarePathways/ — see the three
#    gotchas below, all hit on the first real deploy.
BUILD_CTX=$(backend/scripts/stage_cloudrun_build_context.sh)
gcloud builds submit "$BUILD_CTX" --tag us-central1-docker.pkg.dev/llos-backend/llos/backend

# 3. Deploy it.
gcloud run deploy llos-backend \
  --image us-central1-docker.pkg.dev/llos-backend/llos/backend \
  --region us-central1 \
  --platform managed \
  --allow-unauthenticated \
  --memory 2Gi \
  --cpu 1 \
  --min-instances 0 \
  --max-instances 2
```

`--min-instances 0` means it scales to zero and costs nothing when idle (with
a cold-start delay on the next request, similar to HF's free-tier sleep
behavior). `--allow-unauthenticated` makes the API publicly reachable, which
the Next.js frontend needs — Cloud Run's own auth would otherwise require
service-to-service credentials.

### Why step 2 needs a staging script, not a plain `gcloud builds submit .`

Three real failures hit in sequence getting the first deploy working, in case
they resurface:

1. **`gcloud builds submit --tag` has no `-f`/`--dockerfile` flag.** Unlike
   plain `docker build`, the `--tag` shorthand only ever looks for a file
   literally named `Dockerfile` at the build context root — passing
   `-f backend/Dockerfile.cloudrun` fails with "unrecognized arguments." The
   staging script copies `backend/Dockerfile.cloudrun` to the context root as
   `Dockerfile`.
2. **`.gcloudignore` doesn't reliably override `.gitignore`.** `backend/cloudrun_artifacts/`
   is gitignored on purpose (keeps ~25MB of binaries out of git), but that
   also silently dropped it from the `gcloud builds submit` upload —
   confirmed with `gcloud meta list-files-for-upload`. Negation patterns
   (`!backend/cloudrun_artifacts/**`) in `.gcloudignore` didn't fix it either.
   Building the context somewhere with no ignore files to negotiate (a temp
   directory, via `rsync`) sidesteps the whole problem.
3. **Dangling symlinks crash gcloud's file scanner.** `backend/models/manual/*.{pt,txt,json}`
   are leftover symlinks from an old remote dev session
   (target: `/sessions/eloquent-brave-shannon/...`), unrelated to this deploy
   but present in the repo. `gcloud builds submit` crashes with
   `FileNotFoundError` trying to stat a dangling symlink rather than skipping
   it. The staging script excludes `models/manual/`.

`backend/scripts/stage_cloudrun_build_context.sh` handles all three. Pass it
an output directory, or let it use a fresh `mktemp -d` (default).

## Verify

```bash
SERVICE_URL=$(gcloud run services describe llos-backend --region us-central1 --format='value(status.url)')

curl "$SERVICE_URL/health"
curl "$SERVICE_URL/models"
curl -X POST "$SERVICE_URL/predictions" -H "Content-Type: application/json" \
  -d '{"uuid": 1, "model_key": "deep_patient"}'
```

`/models` should list both `lightgbm_t5` and `deep_patient`.

## Point the frontend at it

Set in Vercel's project env vars (or `llos_next_app/.env.local` for local
testing):

```text
BACKEND_ORIGIN=https://llos-backend-658435340196.us-central1.run.app
```

## Redeploying after changes

Re-run steps 1–3 above. Cloud Run keeps the previous revision live until the
new one passes its health check, so there's no downtime window.

## Known limitation: prediction history isn't durable

`Dataset.csv` and the model artifacts are baked into the image at build time,
so stored-record lookups and predictions always work correctly. But new rows
written to the `predictions` table at runtime live in the container's local
SQLite file, which Cloud Run does not persist across instance restarts or
share across concurrent instances. Fine for a demo/research tool where the
live prediction result is what matters; if a durable audit log of past
predictions becomes a requirement, that table should move to Cloud SQL rather
than local SQLite.
