import type { DatasetRecord, PredictionResult } from "./types";

export function formatDate(value?: string | null): string {
  if (!value) return "Unknown";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return date.toISOString().slice(0, 10);
}

export function shortId(value: string): string {
  return value.length > 8 ? value.slice(0, 8) : value;
}

export function predictionWeeks(result: PredictionResult): string {
  return (result.predicted_los_days / 7).toFixed(1);
}

export function predictionHours(result: PredictionResult): number {
  return Math.round(result.predicted_los_days * 24);
}

export function absoluteError(result: PredictionResult): number | null {
  if (result.actual_los_days == null) return null;
  return Math.abs(result.predicted_los_days - result.actual_los_days);
}

export function recordDisplay(record: DatasetRecord): string {
  return `Subject ${shortId(record.subject_id)}  ·  Admission ${shortId(record.hadm_id)}`;
}
