# Netlify Deployment Notes

Netlify is a good fit for the Flutter web app, but not for running this FastAPI
inference service directly. Netlify Functions currently run JavaScript,
TypeScript, or Go, while this backend needs Python plus PyTorch, Transformers,
LightGBM, SQLite, and model artifacts.

The supported setup in this repo is therefore:

1. Deploy the Python FastAPI backend to Render.
2. Deploy `llos_app` to Netlify.
3. Set the Netlify environment variable `BACKEND_ORIGIN` to the FastAPI origin,
   for example `https://llos-backend.onrender.com`.
4. The Flutter app calls `/api/*` on the Netlify site.
5. `llos_app/netlify/functions/api.mjs` proxies `/api/*` to the FastAPI backend.

## Netlify Setup

In Netlify, create a site with:

- Base directory: `llos_app`
- Build command: `bash netlify/build.sh`
- Publish directory: `llos_app/build/web` if configured from repo root, or
  `build/web` if the site base directory is `llos_app`

Set this environment variable in Netlify:

```text
BACKEND_ORIGIN=https://your-fastapi-backend.example.com
```

After deploy, these URLs should work through Netlify:

```text
https://your-netlify-site.netlify.app/api/health
https://your-netlify-site.netlify.app/api/models
https://your-netlify-site.netlify.app/api/records?limit=5
```

## Why This Is A Proxy

The FastAPI app is a long-running Python inference service with large ML
dependencies and local model artifacts. Netlify's serverless runtime is not a
Python ASGI runtime, so deploying the actual model-serving backend there would
require rewriting the backend into a supported Netlify function runtime or
calling a separate Python inference service anyway.
