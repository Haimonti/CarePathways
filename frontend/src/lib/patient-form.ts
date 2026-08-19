import type { PatientForm } from "./types";

export const emptyPatientForm: PatientForm = {
  hpi: "",
  socialHistory: "",
  chiefComplaint: "",
  vitals: "",
  labs: "",
  conditions: "",
  medications: "",
  admissionType: "",
  admittedTime: "",
  admittedDate: "",
};

export function patientFormToPayload(form: PatientForm): Record<string, string> {
  const payload: Record<string, string> = {};
  const add = (key: string, value: string) => {
    if (value.trim()) payload[key] = value.trim();
  };

  add("hpi", form.hpi);
  add("social_history", form.socialHistory);
  add("chief_complaint", form.chiefComplaint);
  add("vitals", form.vitals);
  add("labs", form.labs);
  add("conditions", form.conditions);
  add("medications", form.medications);
  add("admission_type", form.admissionType);
  add("admitted_time", form.admittedTime);
  add("admitted_date", form.admittedDate);

  return payload;
}

export function labelledPatientFields(form: PatientForm): Array<[string, string]> {
  const fields: Array<[string, string]> = [];
  const add = (label: string, value: string) => {
    if (value.trim()) fields.push([label, value.trim()]);
  };

  add("Admission Type", form.admissionType);
  add("Admission Date", form.admittedDate);
  add("Admission Time", form.admittedTime);
  add("Chief Complaint", form.chiefComplaint);
  add("History of Present Illness", form.hpi);
  add("Social History", form.socialHistory);
  add("Vital Signs", form.vitals);
  add("Laboratory Results", form.labs);
  add("Medical Conditions", form.conditions);
  add("Current Medications", form.medications);

  return fields;
}
