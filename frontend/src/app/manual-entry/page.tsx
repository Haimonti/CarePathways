"use client";

import { useEffect, useMemo, useState } from "react";
import {
  ArrowLeft,
  ArrowRight,
  BarChart3,
  Calendar,
  Clock,
  Info,
} from "lucide-react";
import { ErrorBanner } from "@/components/ErrorBanner";
import { PredictionCard } from "@/components/PredictionCard";
import { SectionLabel } from "@/components/SectionLabel";
import { StepIndicator } from "@/components/StepIndicator";
import { UbHeader } from "@/components/UbHeader";
import { TextInput } from "@/components/FormControls";
import { getModels, predictManual } from "@/lib/api";
import {
  emptyPatientForm,
  labelledPatientFields,
  patientFormToPayload,
} from "@/lib/patient-form";
import type { ModelInfo, PatientForm, PredictionResult } from "@/lib/types";

const stepLabels = [
  "Admission",
  "Presentation",
  "Examination",
  "History",
  "Review & Predict",
];

const admissionTypes = [
  "Inpatient",
  "Emergency",
  "Elective",
  "Outpatient",
  "Observation",
];

export default function ManualEntryPage() {
  const [form, setForm] = useState<PatientForm>(emptyPatientForm);
  const [currentStep, setCurrentStep] = useState(0);
  const [models, setModels] = useState<ModelInfo[]>([]);
  const [selectedModelKey, setSelectedModelKey] = useState("");
  const [result, setResult] = useState<PredictionResult | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [loadingModels, setLoadingModels] = useState(true);

  const labelledFields = useMemo(() => labelledPatientFields(form), [form]);
  const filledFieldCount = labelledFields.length;
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

  function updateForm(patch: Partial<PatientForm>) {
    setForm((current) => ({ ...current, ...patch }));
    setResult(null);
    setErrorMessage(null);
  }

  function reset() {
    setForm(emptyPatientForm);
    setCurrentStep(0);
    setResult(null);
    setErrorMessage(null);
    setIsLoading(false);
  }

  async function runPrediction() {
    const payload = patientFormToPayload(form);
    if (Object.keys(payload).length === 0) {
      setErrorMessage(
        "Please fill in at least one clinical field before predicting.",
      );
      return;
    }

    setIsLoading(true);
    setResult(null);
    setErrorMessage(null);
    try {
      setResult(await predictManual(payload, selectedModelKey));
    } catch (error) {
      setErrorMessage(
        error instanceof Error
          ? error.message
          : "An unexpected error occurred. Please try again.",
      );
    } finally {
      setIsLoading(false);
    }
  }

  return (
    <main className="min-h-screen bg-ub-background">
      <UbHeader
        title="Clinical Form"
        subtitle="Manual patient data entry"
        showBack
        onReset={reset}
      />

      <div className="mx-auto max-w-[700px]">
        <StepIndicator
          currentStep={currentStep}
          totalSteps={stepLabels.length}
          labels={stepLabels}
        />
        <div className="border-t border-ub-border">
          {currentStep === 0 ? (
            <StepShell
              eyebrow="Step 01 — Admission"
              title="How was the patient admitted?"
              description="Admission context signals acuity and care pathway. Select the type and enter the date and time of arrival."
              showBack={false}
              onNext={() => setCurrentStep(1)}
            >
              <div className="card p-5">
                <div className="mb-3.5 text-[10px] font-bold uppercase tracking-[1.2px] text-ub-textMuted">
                  Admission Type
                </div>
                <div className="flex flex-wrap gap-2">
                  {admissionTypes.map((type) => {
                    const value = type.toLowerCase();
                    const selected = form.admissionType === value;
                    return (
                      <button
                        key={type}
                        type="button"
                        onClick={() => updateForm({ admissionType: value })}
                        className={[
                          "rounded-full border px-3.5 py-2 text-[13px] font-medium transition",
                          selected
                            ? "border-ub-blue bg-ub-blue text-white"
                            : "border-ub-border bg-ub-surfaceVariant text-ub-textSecondary hover:border-ub-blue/40",
                        ].join(" ")}
                      >
                        {type}
                      </button>
                    );
                  })}
                </div>
              </div>
              <div className="card mt-3 p-5">
                <div className="mb-3.5 text-[10px] font-bold uppercase tracking-[1.2px] text-ub-textMuted">
                  Date & Time of Admission
                </div>
                <div className="grid gap-3 sm:grid-cols-2">
                  <TextInput
                    label="Date of Admission"
                    value={form.admittedDate}
                    onChange={(admittedDate) => updateForm({ admittedDate })}
                    placeholder="e.g. 2024-03-15"
                    icon={<Calendar size={18} />}
                  />
                  <TextInput
                    label="Time of Admission"
                    value={form.admittedTime}
                    onChange={(admittedTime) => updateForm({ admittedTime })}
                    placeholder="e.g. 14:30"
                    icon={<Clock size={18} />}
                  />
                </div>
              </div>
            </StepShell>
          ) : null}

          {currentStep === 1 ? (
            <StepShell
              eyebrow="Step 02 — Presentation"
              title="Clinical Presentation"
              description="Describe why the patient presented and the full context of their current illness. Rich narrative significantly improves prediction accuracy."
              onBack={() => setCurrentStep(0)}
              onNext={() => setCurrentStep(2)}
            >
              <div className="space-y-3.5">
                <TextInput
                  label="Chief Complaint"
                  value={form.chiefComplaint}
                  onChange={(chiefComplaint) => updateForm({ chiefComplaint })}
                  placeholder="e.g. Chest tightness and shortness of breath for 2 days"
                />
                <TextInput
                  label="History of Present Illness (HPI)"
                  value={form.hpi}
                  onChange={(hpi) => updateForm({ hpi })}
                  placeholder="Describe onset, duration, character, progression, associated symptoms, aggravating and relieving factors..."
                  rows={5}
                />
                <TextInput
                  label="Social History"
                  value={form.socialHistory}
                  onChange={(socialHistory) => updateForm({ socialHistory })}
                  placeholder="e.g. Retired teacher, non-smoker, occasional alcohol use. Lives alone. Fully independent prior to this admission..."
                  rows={3}
                />
              </div>
            </StepShell>
          ) : null}

          {currentStep === 2 ? (
            <StepShell
              eyebrow="Step 03 — Examination"
              title="Examination & Investigations"
              description="Objective clinical data — vitals and labs — are among the strongest predictors. Include abnormal flags and units where possible."
              onBack={() => setCurrentStep(1)}
              onNext={() => setCurrentStep(3)}
            >
              <div className="space-y-3.5">
                <TextInput
                  label="Vital Signs"
                  value={form.vitals}
                  onChange={(vitals) => updateForm({ vitals })}
                  placeholder="e.g. BP 158/94 mmHg  HR 88 bpm  RR 18/min  Temp 37.4°C  SpO2 94% room air  GCS 15/15"
                  rows={3}
                />
                <TextInput
                  label="Laboratory Results"
                  value={form.labs}
                  onChange={(labs) => updateForm({ labs })}
                  placeholder="e.g. Pain score (0-10): 6.0..."
                  rows={6}
                />
              </div>
            </StepShell>
          ) : null}

          {currentStep === 3 ? (
            <StepShell
              eyebrow="Step 04 — Background"
              title="History & Medications"
              description="Comorbid conditions and medication burden are strong independent predictors of prolonged hospital stays. Be as complete as possible."
              nextLabel="Review & Predict"
              onBack={() => setCurrentStep(2)}
              onNext={() => setCurrentStep(4)}
            >
              <div className="space-y-3.5">
                <TextInput
                  label="Medical Conditions & Diagnoses"
                  value={form.conditions}
                  onChange={(conditions) => updateForm({ conditions })}
                  placeholder="e.g. Hypertension; Type 2 diabetes mellitus; Chronic kidney disease stage 3; Atrial fibrillation; Previous MI (2019); Osteoarthritis bilateral knees"
                  rows={4}
                />
                <TextInput
                  label="Current Medications"
                  value={form.medications}
                  onChange={(medications) => updateForm({ medications })}
                  placeholder="e.g. Lisinopril 10mg OD; Metformin 500mg BD; Atorvastatin 40mg nocte; Warfarin 3mg OD; Bisoprolol 5mg OD; Furosemide 40mg OD"
                  rows={4}
                />
              </div>
            </StepShell>
          ) : null}

          {currentStep === 4 ? (
            <section className="px-5 py-7 pb-10">
              <SectionLabel>Step 05 — Predict</SectionLabel>
              <h2 className="mt-2.5 text-[22px] font-semibold text-ub-textPrimary">
                Review & Run
              </h2>
              <p className="mt-2 text-sm leading-6 text-ub-textSecondary">
                Confirm the fields below, then run the prediction. Only filled
                fields are submitted.
              </p>

              <div className="card mt-6 overflow-hidden">
                <div className="flex items-center justify-between px-5 pt-4">
                  <div className="text-[10px] font-bold uppercase tracking-[1.2px] text-ub-textMuted">
                    Completed Fields
                  </div>
                  <span className="rounded-full bg-ub-blue/10 px-2.5 py-1 text-[11px] font-bold text-ub-blue">
                    {filledFieldCount} / 10
                  </span>
                </div>
                <div className="mt-3">
                  {labelledFields.length === 0 ? (
                    <p className="px-5 pb-4 text-[13px] italic text-ub-textMuted">
                      No fields completed yet. Go back and enter patient
                      information.
                    </p>
                  ) : (
                    labelledFields.map(([label, value]) => (
                      <div
                        key={label}
                        className="grid gap-2 border-t border-ub-border px-5 py-2.5 text-[13px] sm:grid-cols-[160px_1fr]"
                      >
                        <span className="text-[11px] font-semibold tracking-[0.3px] text-ub-textMuted">
                          {label}
                        </span>
                        <span className="text-ub-textPrimary">
                          {value.length > 120
                            ? `${value.slice(0, 120)}…`
                            : value}
                        </span>
                      </div>
                    ))
                  )}
                </div>
              </div>

              <div className="card mt-5 p-5">
                <div className="text-[10px] font-bold uppercase tracking-[1.2px] text-ub-textMuted">
                  Model
                </div>
                <div className="mt-2">
                  {loadingModels ? (
                    <div className="h-1 overflow-hidden rounded-full bg-ub-surfaceVariant">
                      <div className="h-full w-1/2 animate-pulse rounded-full bg-ub-blue" />
                    </div>
                  ) : models.length === 0 ? (
                    <p className="text-xs text-ub-textMuted">
                      No model metadata loaded. The backend default model will
                      be used.
                    </p>
                  ) : (
                    <select
                      value={selectedModelKey}
                      onChange={(event) => {
                        setSelectedModelKey(event.target.value);
                        setResult(null);
                      }}
                      className="input"
                    >
                      {models.map((model) => (
                        <option key={model.model_key} value={model.model_key}>
                          {model.display_name} - {model.description}
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

              <div className="mt-5">
                {errorMessage ? <ErrorBanner message={errorMessage} /> : null}
                {result ? (
                  <div className="mb-5">
                    <PredictionCard
                      predictedDays={result.predicted_los_days}
                      isLlos={result.is_llos}
                      modelLabel={selectedModel?.display_name ?? result.model_key}
                      statRows={{
                        "Fields Used": `${filledFieldCount}/10`,
                        "Equivalent Weeks": `${(
                          result.predicted_los_days / 7
                        ).toFixed(1)} wks`,
                        "Estimated Hours": `${Math.round(
                          result.predicted_los_days * 24,
                        )} hrs`,
                      }}
                    />
                  </div>
                ) : null}
              </div>

              <div className="mt-8 flex flex-wrap items-center justify-between gap-3">
                <button
                  type="button"
                  className="btn-secondary"
                  disabled={isLoading}
                  onClick={() => setCurrentStep(3)}
                >
                  <ArrowLeft size={14} />
                  Back
                </button>
                <div className="ml-auto flex flex-wrap justify-end gap-2.5">
                  {result ? (
                    <button
                      type="button"
                      className="btn-secondary border-ub-danger/40 text-ub-danger hover:bg-ub-danger/5"
                      onClick={reset}
                    >
                      Clear All
                    </button>
                  ) : null}
                  <button
                    type="button"
                    className="btn-primary"
                    disabled={isLoading}
                    onClick={runPrediction}
                  >
                    {isLoading ? (
                      <span className="h-4 w-4 animate-spin rounded-full border-2 border-white/40 border-t-white" />
                    ) : (
                      <BarChart3 size={18} />
                    )}
                    {isLoading ? "Analysing..." : "Run Prediction"}
                  </button>
                </div>
              </div>
            </section>
          ) : null}
        </div>
      </div>
    </main>
  );
}

function StepShell({
  eyebrow,
  title,
  description,
  children,
  onNext,
  onBack,
  showBack = true,
  nextLabel = "Continue",
}: {
  eyebrow: string;
  title: string;
  description: string;
  children: React.ReactNode;
  onNext: () => void;
  onBack?: () => void;
  showBack?: boolean;
  nextLabel?: string;
}) {
  return (
    <section className="px-5 py-7 pb-10">
      <SectionLabel>{eyebrow}</SectionLabel>
      <h2 className="mt-2.5 text-[22px] font-semibold leading-tight text-ub-textPrimary">
        {title}
      </h2>
      <p className="mt-2 text-sm leading-6 text-ub-textSecondary">
        {description}
      </p>
      <div className="mt-7">{children}</div>
      <div className="mt-8 flex items-center justify-between gap-3">
        {showBack ? (
          <button type="button" className="btn-secondary" onClick={onBack}>
            <ArrowLeft size={14} />
            Back
          </button>
        ) : (
          <span />
        )}
        <button type="button" className="btn-primary" onClick={onNext}>
          {nextLabel}
          <ArrowRight size={14} />
        </button>
      </div>
    </section>
  );
}
