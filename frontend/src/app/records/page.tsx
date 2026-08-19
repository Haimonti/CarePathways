"use client";

import { useEffect, useMemo, useState } from "react";
import { BarChart3, CheckCircle, Info, Search } from "lucide-react";
import { ErrorBanner } from "@/components/ErrorBanner";
import { PredictionCard } from "@/components/PredictionCard";
import { SectionLabel } from "@/components/SectionLabel";
import { UbHeader } from "@/components/UbHeader";
import { getModels, predictRecord, searchRecords } from "@/lib/api";
import {
  absoluteError,
  formatDate,
  predictionHours,
  predictionWeeks,
  recordDisplay,
} from "@/lib/format";
import type { DatasetRecord, ModelInfo, PredictionResult } from "@/lib/types";

export default function RecordsPage() {
  const [query, setQuery] = useState("");
  const [models, setModels] = useState<ModelInfo[]>([]);
  const [records, setRecords] = useState<DatasetRecord[]>([]);
  const [recordTotal, setRecordTotal] = useState(0);
  const [selectedRecord, setSelectedRecord] = useState<DatasetRecord | null>(
    null,
  );
  const [selectedModelKey, setSelectedModelKey] = useState("");
  const [result, setResult] = useState<PredictionResult | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const [loadingModels, setLoadingModels] = useState(true);
  const [loadingRecords, setLoadingRecords] = useState(false);
  const [predicting, setPredicting] = useState(false);

  const selectedModel = useMemo(
    () => models.find((model) => model.model_key === selectedModelKey) ?? null,
    [models, selectedModelKey],
  );

  useEffect(() => {
    let cancelled = false;

    async function loadModels() {
      try {
        const response = await getModels();
        if (cancelled) return;
        setModels(response.items);
        const defaultModel =
          response.items.find((model) => model.is_default) ?? response.items[0];
        setSelectedModelKey(defaultModel?.model_key ?? "");
      } catch (error) {
        if (!cancelled) {
          setErrorMessage(
            error instanceof Error ? error.message : "Failed to load models.",
          );
        }
      } finally {
        if (!cancelled) setLoadingModels(false);
      }
    }

    loadModels();
    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    const timeout = window.setTimeout(() => {
      void runSearch(query);
    }, 250);

    return () => window.clearTimeout(timeout);
  }, [query]);

  async function runSearch(searchQuery: string) {
    setLoadingRecords(true);
    setSelectedRecord(null);
    setResult(null);
    setErrorMessage(null);

    try {
      const response = await searchRecords(searchQuery, 30);
      setRecords(response.items);
      setRecordTotal(response.total);
    } catch (error) {
      setErrorMessage(
        error instanceof Error
          ? error.message
          : "Failed to search dataset records.",
      );
    } finally {
      setLoadingRecords(false);
    }
  }

  async function runPrediction() {
    if (!selectedRecord) {
      setErrorMessage("Please select a dataset record first.");
      return;
    }

    setPredicting(true);
    setResult(null);
    setErrorMessage(null);

    try {
      setResult(await predictRecord(selectedRecord.uuid, selectedModelKey));
    } catch (error) {
      setErrorMessage(
        error instanceof Error ? error.message : "Prediction failed.",
      );
    } finally {
      setPredicting(false);
    }
  }

  return (
    <main className="min-h-screen bg-ub-background">
      <UbHeader
        title="Patient Record Lookup"
        subtitle="Dataset.csv-backed LOS prediction"
        showBack
      />

      <div className="mx-auto max-w-[720px] px-5 py-5">
        <SectionLabel>1. Search & Select Record</SectionLabel>
        <div className="relative mt-2.5">
          <Search
            className="pointer-events-none absolute left-3 top-3.5 text-ub-textMuted"
            size={20}
          />
          <input
            value={query}
            onChange={(event) => setQuery(event.target.value)}
            placeholder="Search by UUID, subject ID, or admission ID"
            className="input pl-10 pr-11"
          />
          {loadingRecords ? (
            <span className="absolute right-3 top-3.5 h-4 w-4 animate-spin rounded-full border-2 border-ub-blue/25 border-t-ub-blue" />
          ) : null}
        </div>

        <p className="mt-2.5 text-xs text-ub-textMuted">
          {recordTotal} record{recordTotal === 1 ? "" : "s"} in dataset
        </p>

        <RecordList
          records={records}
          selectedRecord={selectedRecord}
          onSelect={(record) => {
            setSelectedRecord(record);
            setResult(null);
            setErrorMessage(null);
          }}
        />

        {selectedRecord ? (
          <div className="mt-6">
            <SectionLabel>2. Confirm Record</SectionLabel>
            <RecordSummary record={selectedRecord} />

            <div className="mt-6">
              <SectionLabel>3. Configure Model</SectionLabel>
              <ModelConfig
                models={models}
                selectedModelKey={selectedModelKey}
                selectedModel={selectedModel}
                loading={loadingModels}
                onChange={(modelKey) => {
                  setSelectedModelKey(modelKey);
                  setResult(null);
                }}
              />
            </div>

            <div className="mt-6">
              {errorMessage ? <ErrorBanner message={errorMessage} /> : null}
              {result ? (
                <div className="mb-5">
                  <PredictionCard
                    predictedDays={result.predicted_los_days}
                    isLlos={result.is_llos}
                    actualDays={result.actual_los_days}
                    modelLabel={selectedModel?.display_name ?? result.model_key}
                    statRows={{
                      "Dataset UUID": `${result.uuid}`,
                      "Equivalent Weeks": `${predictionWeeks(result)} wks`,
                      "Estimated Hours": `${predictionHours(result)} hrs`,
                      ...(absoluteError(result) != null
                        ? {
                            "Error vs Actual": `${absoluteError(result)!.toFixed(
                              1,
                            )} days`,
                          }
                        : {}),
                    }}
                  />
                </div>
              ) : null}

              <button
                type="button"
                disabled={predicting}
                onClick={runPrediction}
                className="btn-primary w-full text-base"
              >
                {predicting ? (
                  <span className="h-[18px] w-[18px] animate-spin rounded-full border-2 border-white/40 border-t-white" />
                ) : (
                  <BarChart3 size={20} />
                )}
                {predicting ? "Predicting..." : "Run LOS Prediction"}
              </button>
            </div>
          </div>
        ) : (
          <div className="mt-5">
            {errorMessage ? <ErrorBanner message={errorMessage} /> : null}
          </div>
        )}
      </div>
    </main>
  );
}

function RecordList({
  records,
  selectedRecord,
  onSelect,
}: {
  records: DatasetRecord[];
  selectedRecord: DatasetRecord | null;
  onSelect: (record: DatasetRecord) => void;
}) {
  if (records.length === 0) {
    return (
      <div className="card mt-2.5 p-5 text-center text-sm text-ub-textMuted">
        No dataset records found.
      </div>
    );
  }

  return (
    <div className="card mt-2.5 max-h-80 overflow-auto">
      {records.map((record, index) => {
        const isSelected = selectedRecord?.uuid === record.uuid;
        return (
          <button
            key={record.uuid}
            type="button"
            onClick={() => onSelect(record)}
            className={[
              "flex w-full items-center gap-3 border-b border-ub-border px-4 py-3 text-left transition last:border-b-0",
              isSelected ? "bg-ub-blue/5" : "hover:bg-ub-surfaceVariant/60",
            ].join(" ")}
          >
            <span
              className={[
                "flex h-[42px] w-[42px] shrink-0 items-center justify-center rounded-xl text-[11px] font-extrabold",
                isSelected
                  ? "bg-ub-blue text-white"
                  : "bg-ub-surfaceVariant text-ub-textSecondary",
              ].join(" ")}
            >
              #{record.uuid}
            </span>
            <span className="min-w-0 flex-1">
              <span
                className={[
                  "block truncate text-[13px]",
                  isSelected
                    ? "font-bold text-ub-blue"
                    : "font-medium text-ub-textPrimary",
                ].join(" ")}
              >
                {recordDisplay(record)}
              </span>
              <span className="block truncate text-[11px] text-ub-textMuted">
                Admitted {formatDate(record.admittime)}
                {record.actual_los_days != null
                  ? `  ·  Actual LOS ${record.actual_los_days.toFixed(1)} days`
                  : ""}
              </span>
            </span>
            {isSelected ? (
              <CheckCircle className="text-ub-blue" size={18} />
            ) : (
              <span className="text-xs text-ub-textMuted">{index + 1}</span>
            )}
          </button>
        );
      })}
    </div>
  );
}

function RecordSummary({ record }: { record: DatasetRecord }) {
  return (
    <div className="card mt-2.5 p-[18px]">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <span className="rounded-full bg-ub-blue/10 px-2.5 py-1 text-[11px] font-extrabold text-ub-blue">
          UUID {record.uuid}
        </span>
        {record.actual_los_days != null ? (
          <span className="text-xs font-bold text-ub-goldDark">
            Actual LOS {record.actual_los_days.toFixed(1)} days
          </span>
        ) : null}
      </div>
      <div className="mt-3.5 space-y-1.5">
        <InfoRow label="Subject ID" value={record.subject_id} />
        <InfoRow label="Admission ID" value={record.hadm_id} />
        <InfoRow label="Admitted" value={formatDate(record.admittime)} />
        <InfoRow label="Discharged" value={formatDate(record.dischtime)} />
        {record.anchor_age != null ? (
          <InfoRow label="Age" value={record.anchor_age.toFixed(0)} />
        ) : null}
        {record.gender ? <InfoRow label="Gender" value={record.gender} /> : null}
        {record.race ? <InfoRow label="Race" value={record.race} /> : null}
        {record.admission_type ? (
          <InfoRow label="Admission Type" value={record.admission_type} />
        ) : null}
      </div>
    </div>
  );
}

function InfoRow({ label, value }: { label: string; value: string }) {
  return (
    <div className="grid gap-1 text-xs sm:grid-cols-[110px_1fr]">
      <span className="text-[11px] font-bold text-ub-textMuted">{label}</span>
      <span className="text-ub-textPrimary">{value}</span>
    </div>
  );
}

function ModelConfig({
  models,
  selectedModelKey,
  selectedModel,
  loading,
  onChange,
}: {
  models: ModelInfo[];
  selectedModelKey: string;
  selectedModel: ModelInfo | null;
  loading: boolean;
  onChange: (modelKey: string) => void;
}) {
  return (
    <div className="card mt-2.5 p-5">
      <div className="text-[10px] font-bold uppercase tracking-[1.2px] text-ub-textMuted">
        Model
      </div>
      <div className="mt-2">
        {loading ? (
          <div className="h-1 overflow-hidden rounded-full bg-ub-surfaceVariant">
            <div className="h-full w-1/2 animate-pulse rounded-full bg-ub-blue" />
          </div>
        ) : models.length === 0 ? (
          <p className="text-xs text-ub-textMuted">
            No model metadata loaded. The backend default model will be used.
          </p>
        ) : (
          <select
            value={selectedModelKey}
            onChange={(event) => onChange(event.target.value)}
            className="input"
          >
            {models.map((model) => (
              <option key={model.model_key} value={model.model_key}>
                {model.display_name}
              </option>
            ))}
          </select>
        )}
      </div>
      {selectedModel ? (
        <div className="mt-2.5 flex items-start gap-1.5 text-[11px] text-ub-textMuted">
          <Info className="mt-0.5 shrink-0" size={14} />
          <span>Input contract: {selectedModel.input_contract}</span>
        </div>
      ) : null}
    </div>
  );
}
