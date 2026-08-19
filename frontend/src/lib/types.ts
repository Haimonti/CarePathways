export type PatientForm = {
  hpi: string;
  socialHistory: string;
  chiefComplaint: string;
  vitals: string;
  labs: string;
  conditions: string;
  medications: string;
  admissionType: string;
  admittedTime: string;
  admittedDate: string;
};

export type ModelInfo = {
  model_key: string;
  display_name: string;
  description: string;
  adapter: string;
  input_contract: string;
  is_default: boolean;
};

export type DatasetRecord = {
  uuid: number;
  subject_id: string;
  hadm_id: string;
  admittime?: string | null;
  dischtime?: string | null;
  actual_los_days?: number | null;
  anchor_age?: number | null;
  gender?: string | null;
  race?: string | null;
  ethnicity?: string | null;
  admission_type?: string | null;
};

export type PredictionResult = {
  prediction_id: number;
  uuid?: number | null;
  subject_id?: string | null;
  hadm_id?: string | null;
  model_key: string;
  predicted_los_days: number;
  actual_los_days?: number | null;
  is_llos: boolean;
  created_at: string;
};

export type RecordsResponse = {
  items: DatasetRecord[];
  total: number;
};

export type ModelsResponse = {
  items: ModelInfo[];
};
