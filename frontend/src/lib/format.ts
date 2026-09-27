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

// Clinical sections of a stored record's serialized input, in display order.
// Admission type/date/time are left out: the record summary already shows them.
const clinicalSections: Array<[string, string, "text" | "list" | "bullets"]> = [
  ["chief_complaint", "Chief Complaint", "bullets"],
  ["hpi", "History of Present Illness", "text"],
  ["social_history", "Social History", "text"],
  ["vitals", "Vital Signs", "list"],
  ["labs", "Laboratory Results", "list"],
  ["conditions", "Medical Conditions", "list"],
  ["medications", "Current Medications", "list"],
];

export type ClinicalSection = {
  key: string;
  label: string;
  items: string[];
};

// Mirrors the backend's parse_sections: "<section> value </s> <next> ...".
export function parseClinicalSections(inputText: string): ClinicalSection[] {
  const values: Record<string, string> = {};
  for (const match of inputText.matchAll(/<([^>/][^>]*)>\s*([\s\S]*?)\s*(?=<[^>/][^>]*>|$)/g)) {
    values[match[1].trim()] = match[2].replace(/<\/s>/g, " ").trim();
  }

  return clinicalSections.flatMap(([key, label, kind]) => {
    const value = values[key];
    if (!value) return [];
    let items = [value];
    if (kind === "list") items = value.split(";");
    if (kind === "bullets" && value.startsWith("-")) items = value.split(/(?:^|\s)-\s/);
    items = items.map((item) => item.trim()).filter(Boolean);
    return items.length > 0 ? [{ key, label, items }] : [];
  });
}

export function recordDisplay(record: DatasetRecord): string {
  return `Subject ${shortId(record.subject_id)}  ·  Admission ${shortId(record.hadm_id)}`;
}
