
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(forcats)
library(readr)
library(data.table)

data <- read_excel("Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx")

Canada_Hosp1_COVID_InpatientData <- read_excel("Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx")

medication_data <- data.frame(id = character(),
                              medications = character(),
                              dosage = character(),
                              frequency = character(),
                              stringsAsFactors = FALSE)


for (i in 1:nrow(Canada_Hosp1_COVID_InpatientData)) {
  meds_string <- Canada_Hosp1_COVID_InpatientData$medications[i]
  meds_string <- gsub('^"(.*)"$', '\\1', meds_string)  
  meds_string <- gsub('\\\\', '', meds_string)  
  
  meds_list <- tryCatch(fromJSON(meds_string), error = function(e) NULL)
  
  if (!is.null(meds_list) && length(meds_list) > 0) {
    for (j in 1:length(meds_list)) {
      medication <- ifelse(!is.null(meds_list[[j]]$medications), meds_list[[j]]$medications, NA)
      dosage <- ifelse(!is.null(meds_list[[j]]$dosage), meds_list[[j]]$dosage, NA)
      frequency <- ifelse(!is.null(meds_list[[j]]$frequency), meds_list[[j]]$frequency, NA)
      
      medication_data <- rbind(medication_data, data.frame(
        id = Canada_Hosp1_COVID_InpatientData$id[i],
        medications = medication,
        dosage = dosage,
        frequency = frequency,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    medication_data <- rbind(medication_data, data.frame(
      id = Canada_Hosp1_COVID_InpatientData$id[i],
      medications = "",
      dosage = "",
      frequency = "",
      stringsAsFactors = FALSE
    ))
  }
}

write.csv(medication_data, "medication_data.csv", row.names = FALSE)



data <- read_excel("Documents/UB/Spring 2025/Research/correct_medication_data.xlsx")

# Delet medications that is NA or Please Select...
data <- data %>%
  filter(!is.na(medications) & str_trim(medications) != "")

# Clean frequency
invalid_freqs <- c("Please Select an option", "NA", "", NA)
data <- data %>%
  mutate(frequency = ifelse(toupper(frequency) %in% toupper(invalid_freqs), "", frequency))

# Standardize medications
formats_to_remove <- c(
  "TABLETS?", "CAPLETS?", "CAPSULES?", "TAB", "CAP", "TABLET", "CAPLET",
  "XL", "DR", "SR", "XR", "CR", "MR", "AEM", "PF", "CD", "HD", "LA", "XC",
  "PQ", "N NPH", "R", "TEARS", "EZ", "S", "DS", "F TAB", "INJ", 
  "ENTERICCOATED", "EXTRA STRENGTH", "HFA","TABS", "SYSTEM", "FASTAB", "RD", "SELECT",
  "\\(.*?\\)", ",\\s*$", "\\s+$", "USP", "PLUS", "ANTACID", "CHEWABLE", 
  "LIQUID", "SANDOZ FORTE", " - .*", "\"", "&AMP;"
)

format_regex <- paste0("(?i)\\b(", paste(formats_to_remove, collapse = "|"), ")\\b")

data <- data %>%
  mutate(medications = toupper(medications),
         medications = gsub(format_regex, "", medications),
         medications = gsub("\\s+", " ", medications),
         medications = trimws(medications),
         
         medications = ifelse(str_detect(medications, "TYLENOL"), "TYLENOL", medications),
         medications = ifelse(str_detect(medications, "VITAMIN B|VIT B"), "VITAMIN B", medications),
         medications = ifelse(str_detect(medications, "VITAMIN C"), "VITAMIN C", medications),
         medications = ifelse(str_detect(medications, "VITAMIN D|VIT D|VIT -D|VIT-D-3"), "VITAMIN D", medications),
         medications = ifelse(medications == "ACETYLSALICYCLIC ACID", "ACETYLSALICYLIC ACID", medications),
         medications = ifelse(str_detect(medications, "AMLODIPINE"), "AMLODIPINE", medications),
         medications = ifelse(str_detect(medications, "ASCORBIC ACID"), "ASCORBIC ACID", medications),
         medications = ifelse(medications %in% c("ASPRIN", "ASPRIRIN"), "ASPIRIN", medications),
         medications = ifelse(str_detect(medications, "ASPIRIN"), "ASPIRIN", medications),
         medications = ifelse(str_detect(medications, "ATORVASTATIN"), "ATORVASTATIN", medications),
         medications = ifelse(str_detect(medications, "ATROVENT"), "ATROVENT", medications),
         medications = ifelse(str_detect(medications, "CANDESARTAN"), "CANDESARTAN", medications),
         medications = ifelse(medications == "CELEBRIX", "CELEBREX", medications),
         medications = ifelse(str_detect(medications, "CENTRUM"), "CENTRUM", medications),
         medications = ifelse(str_detect(medications, "COLACE"), "COLACE", medications),
         medications = ifelse(str_detect(medications, "COREG"), "COREG", medications),
         medications = ifelse(medications == "DOXECIN", "DOXEPIN", medications),
         medications = ifelse(str_detect(medications, "EMPAGLIFLOZIN"), "EMPAGLIFLOZIN", medications),
         medications = ifelse(medications == "EXETIMIBE", "EZETIMIBE", medications),
         medications = ifelse(medications %in% c("HYDROCHLOROTHIZIDE", "HYDROCHLROTHIAZIDE"), "HYDROCHLOROTHIAZIDE", medications),
         medications = ifelse(str_detect(medications, "LANTUS"), "LANTUS", medications),
         medications = ifelse(str_detect(medications, "METFORMIN"), "METFORMIN", medications),
         medications = ifelse(str_detect(medications, "METHYLPREDNISOLONE ACETATE"), "METHYLPREDNISOLONE ACETATE", medications),
         medications = ifelse(str_detect(medications, "MULTIVITAMIN"), "MULTIVITAMINS", medications),
         medications = ifelse(str_detect(medications, "OMEGA 3"), "OMEGA 3", medications),
         medications = ifelse(str_detect(medications, "PANTOPRAZOLE"), "PANTOPRAZOLE", medications),
         medications = ifelse(str_detect(medications, "PERINDOPRIL"), "PERINDOPRIL", medications),
         medications = ifelse(str_detect(medications, "PRENATAL VIT(AMIN)?"), "PRENATAL VIT", medications),
         medications = ifelse(medications == "PROCLORPERAZINE", "PROCHLORPERAZINE", medications),
         medications = ifelse(medications == "PROPANOLOL", "PROPRANOLOL", medications),
         medications = ifelse(str_detect(medications, "SALBUTAMOL"), "SALBUTAMOL", medications),
         medications = ifelse(str_detect(medications, "TELMISARTAN"), "TELMISARTAN", medications),
         medications = ifelse(str_detect(medications, "TRAVATAN"), "TRAVATAN", medications),
         medications = ifelse(medications == "TREHALOSE)", "TREHALOSE", medications),
         medications = ifelse(medications == "ULTIBRO BEEZHALER", "ULTIBRO BREEZHALER", medications),
         medications = ifelse(str_detect(medications, "VENTOLIN"), "VENTOLIN", medications),
         medications = ifelse(str_detect(medications, "VITALUX"), "VITALUX", medications),
         medications = ifelse(str_detect(medications, "ACETAMIOPHEN"), "ACETAMINOPHEN", medications)
  )


# Frequency report
freq_report <- data %>%
  filter(frequency != "") %>%
  count(frequency, name = "count")
write.csv(freq_report, "frequency_report.csv", row.names = FALSE)

# Medication report
med_report <- data %>%
  count(medications, name = "count")
write.csv(med_report, "medications_report.csv", row.names = FALSE)

# One-hot encoding matrix
one_hot <- data %>%
  distinct(id, medications) %>%
  mutate(value = 1L) %>%
  pivot_wider(
    id_cols = id,
    names_from = medications,
    values_from = value,
    values_fill = list(value = 0L)
  )
write.csv(one_hot, "one_hot_matrix.csv", row.names = FALSE)

# Dosage report
dosage_report <- data %>%
  filter(!is.na(dosage) & str_trim(dosage) != "") %>%
  group_by(medications) %>%
  summarise(dosages = paste(sort(unique(dosage)), collapse = ", "), .groups = "drop")
write.csv(dosage_report, "medication_dosages.csv", row.names = FALSE)

# Frequency matrix（medication_dosage columns）
data <- data %>%
  mutate(
    dosage_clean = ifelse(is.na(dosage) | str_trim(dosage) == "", "UNKNOWN", str_trim(dosage)),
    med_dose = paste(medications, dosage_clean, sep = "_")
  )
# Turn empty freq to NA
data <- data %>%
  mutate(
    frequency = ifelse(is.na(frequency) | str_trim(frequency) == "", "NA", frequency)
  )
# turn text to numbers
freq_map <- c(
  "OD" = 1, "BID" = 2, "TID" = 3, "QUID" = 4,
  "PRN" = 5, "WEEKLY" = 6, "EVERY OTHER DAY" = 7,
  "MWF" = 8, "MONTHLY" = 9, "NA" = 0
)
data <- data %>%
  mutate(
    frequency_clean = toupper(str_trim(frequency)),
    freq_value = freq_map[frequency_clean]
  )
# Create a frequency matrix
freq_matrix <- data %>%
  select(id, med_dose, freq_value) %>%
  group_by(id, med_dose) %>%
  summarise(freq_value = max(freq_value, na.rm = TRUE), .groups = "drop") %>%  
  pivot_wider(names_from = med_dose, values_from = freq_value, values_fill = 0)
write.csv(freq_matrix, "frequency_matrix.csv", row.names = FALSE)


# merge dataset
merged_new <- read_csv("Documents/UB/Spring 2025/Research/csv file extract/merged_new.csv")
merged_data <- merge(merged_new, freq_matrix, by = "id", all.x = TRUE)

merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[, -2]
write.csv(merged_data, "final_data.csv", row.names = FALSE)
