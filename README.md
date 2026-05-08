This notebook handles the full preprocessing of Synthea EHR data to build the dataset for the LLOS prediction.

It starts by filtering inpatient encounters and identifying the long-stay cohort using a threshold of the mean LOS + 2 standard deviations. From there, it pulls vitals from the first 12 hours and lab results from the first 2 hours after admission.

For conditions and medications, the notebook looks at what was active at the time of admission and aggregates them per encounter. SNOMED CT codes are also included as a binary feature matrix. Clinical notes are parsed into separate sections; HPI, social history and chief complaint and are kept as individual columns or features.

The final output is saved as Dataset_Tabular.csv and includes physical characteristics; demographic info, admission timestamps, all the text columns and the SNOMED features. 
