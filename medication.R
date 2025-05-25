#1.
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(forcats)
library(readr)
library(data.table)

#2.
file_path <- "Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx"

df <- read_excel(file_path) %>%
  mutate(id = as.character(id))

#3.
medication_data <- tibble(
  id = character(),
  medications = character(),
  dosage = character(),
  frequency = character()
)

#4.
`%||%` <- function(a, b) if (!is.null(a)) a else b
for (i in seq_len(nrow(df))) {
  row_id <- df$id[i]
  meds_string <- df$medications[i]
  
  if (is.na(meds_string) || meds_string == "") {
    medication_data <- bind_rows(medication_data, tibble(
      id = row_id, medications = "", dosage = "", frequency = ""
    ))
    next
  }
  
  meds_string_clean <- meds_string %>%
    str_replace_all("[\r\n]", "") %>%
    str_replace_all("\\\\", "") %>%
    str_replace_all('^"|"$', "")
  
  meds_list <- fromJSON(meds_string_clean)
  
  if (is.null(meds_list) || length(meds_list) == 0) {
    medication_data <- bind_rows(medication_data, tibble(
      id = row_id, medications = "", dosage = "", frequency = ""
    ))
    next
  }
  
  if (is.data.frame(meds_list)) {
    for (j in seq_len(nrow(meds_list))) {
      medication_data <- bind_rows(medication_data, tibble(
        id = row_id,
        medications = meds_list$medications[j] %||% "",
        dosage = meds_list$dosage[j] %||% "",
        frequency = meds_list$frequency[j] %||% ""
      ))
    }
  } else if (is.list(meds_list)) {
    for (j in seq_along(meds_list)) {
      entry <- meds_list[[j]]
      if (is.list(entry)) {
        medication_data <- bind_rows(medication_data, tibble(
          id = row_id,
          medications = entry$medications %||% "",
          dosage = entry$dosage %||% "",
          frequency = entry$frequency %||% ""
        ))
      } else {
        medication_data <- bind_rows(medication_data, tibble(
          id = row_id,
          medications = as.character(entry) %||% "",
          dosage = "",
          frequency = ""
        ))
      }
    }
  } else {
    medication_data <- bind_rows(medication_data, tibble(
      id = row_id,
      medications = as.character(meds_list) %||% "",
      dosage = "",
      frequency = ""
    ))
  }
}

write_csv(medication_data, "medication_data.csv")

data <- medication_data

#5. 
data <- data %>%
  filter(!is.na(medications) & str_trim(medications) != "")

invalid_freqs <- c("Please Select an option", "NA", "", NA)
data <- data %>%
  mutate(frequency = ifelse(toupper(frequency) %in% toupper(invalid_freqs), "", frequency))

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


#6.
freq_report <- data %>%
  filter(frequency != "") %>%
  count(frequency, name = "count")
write.csv(freq_report, "frequency_report.csv", row.names = FALSE)

med_report <- data %>%
  count(medications, name = "count")
write.csv(med_report, "medications_report.csv", row.names = FALSE)

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

dosage_report <- data %>%
  filter(!is.na(dosage) & str_trim(dosage) != "") %>%
  group_by(medications) %>%
  summarise(dosages = paste(sort(unique(dosage)), collapse = ", "), .groups = "drop")
write.csv(dosage_report, "medication_dosages.csv", row.names = FALSE)

#7.
data <- data %>%
  mutate(
    dosage_clean = ifelse(is.na(dosage) | str_trim(dosage) == "", "UNKNOWN", str_trim(dosage)),
    med_dose = paste(medications, dosage_clean, sep = "_")
  )

data <- data %>%
  mutate(
    frequency = ifelse(is.na(frequency) | str_trim(frequency) == "", "NA", frequency)
  )

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

freq_matrix <- data %>%
  select(id, med_dose, freq_value) %>%
  group_by(id, med_dose) %>%
  summarise(freq_value = max(freq_value, na.rm = TRUE), .groups = "drop") %>%  
  pivot_wider(names_from = med_dose, values_from = freq_value, values_fill = 0)
write.csv(freq_matrix, "frequency_matrix.csv", row.names = FALSE)



#8.
merged_new <- read_csv("Documents/UB/Spring 2025/Research/csv file extract/merged_new.csv")
merged_data <- merge(merged_new, freq_matrix, by = "id", all.x = TRUE)

merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[, -2]
write.csv(merged_data, "final_data.csv", row.names = FALSE)
