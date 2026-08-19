# LLOS Next.js App

Next.js port of the Flutter `llos_app` client. It keeps the same UB-branded
manual-entry and dataset-record prediction workflows while calling the existing
FastAPI backend through a same-origin `/api/*` proxy.

## Local Development

```bash
npm install
cp .env.local.example .env.local
npm run dev
```

Set `BACKEND_ORIGIN` in `.env.local` to the FastAPI backend, for example:

```text
BACKEND_ORIGIN=http://localhost:8000
```

The browser calls `/api/models`, `/api/records`, and `/api/predictions`; the
Next route handler forwards those requests to the configured backend origin.
