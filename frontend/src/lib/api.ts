import type {
  DatasetRecord,
  ModelsResponse,
  PredictionResult,
  RecordsResponse,
} from "./types";

export class LlosApiError extends Error {
  statusCode?: number;

  constructor(message: string, statusCode?: number) {
    super(message);
    this.name = "LlosApiError";
    this.statusCode = statusCode;
  }
}

async function request<T>(path: string, init?: RequestInit): Promise<T> {
  let response: Response;
  try {
    response = await fetch(`/api${path}`, {
      ...init,
      headers: {
        "Content-Type": "application/json",
        ...init?.headers,
      },
    });
  } catch {
    throw new LlosApiError(
      "Could not reach the backend server. Check that FastAPI is running.",
    );
  }

  if (response.ok) {
    return response.json() as Promise<T>;
  }

  let message = "Request failed.";
  try {
    const body = (await response.json()) as { detail?: unknown };
    message = body.detail?.toString() ?? message;
  } catch {
    message = response.statusText || message;
  }

  throw new LlosApiError(message, response.status);
}

export function getModels() {
  return request<ModelsResponse>("/models");
}

export function searchRecords(query: string, limit = 30, offset = 0) {
  const params = new URLSearchParams({
    limit: String(limit),
    offset: String(offset),
  });
  if (query.trim()) params.set("query", query.trim());
  return request<RecordsResponse>(`/records?${params.toString()}`);
}

export function predictRecord(uuid: number, modelKey?: string) {
  return request<PredictionResult>("/predictions", {
    method: "POST",
    body: JSON.stringify({
      uuid,
      ...(modelKey ? { model_key: modelKey } : {}),
    }),
  });
}

export function predictManual(
  payload: Record<string, string>,
  modelKey?: string,
) {
  return request<PredictionResult>("/predictions", {
    method: "POST",
    body: JSON.stringify({
      ...payload,
      ...(modelKey ? { model_key: modelKey } : {}),
    }),
  });
}

export type { DatasetRecord };
