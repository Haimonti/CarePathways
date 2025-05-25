# 1.
library(readxl)
library(dplyr)
library(readr)
smoking_df <- read_excel("Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx")

# 2. 
smoking_counts <- smoking_df %>%
  count(smoking_history)
print(smoking_counts)
write.csv(smoking_counts, "smoking_counts.csv", row.names = FALSE)

# 3.
ex_smokers <- smoking_df %>%
  filter(smoking_history == "Ex-smoker") %>%
  mutate(
    year_they_quit = ifelse(is.na(year_they_quit) | year_they_quit == "", NA, year_they_quit),
    years_since_quit = ifelse(!is.na(year_they_quit), 2025 - as.numeric(year_they_quit), NA)
  ) %>%
  select(id, years_since_quit)
write.csv(ex_smokers, "ex_smokers_years_quit.csv", row.names = FALSE)

# 4. 
all_patients_smoking <- smoking_df %>%
  mutate(
    smoking_history_numeric = case_when(
      smoking_history == "Non-smoker" ~ 1,
      smoking_history == "Ex-smoker" ~ 2,
      smoking_history == "Smoker: < 30 pack years" ~ 3,
      smoking_history == "Smoker: pack years unknown" ~ 4,
      is.na(smoking_history) | smoking_history == "" ~ 0,
      TRUE ~ 0
    ),
    years_since_quit = case_when(
      smoking_history == "Ex-smoker" ~ 2025 - as.numeric(year_they_quit),
      TRUE ~ NA_real_
    )
  ) %>%
  select(id, smoking_history_numeric, years_since_quit)
write.csv(all_patients_smoking, "all_patients_smoking_history.csv", row.names = FALSE, na = "")

# Non-smoker = 1
# Ex-smoker = 2
# Smoker: < 30 pack years = 3
# Smoker: pack years unknown = 4
# if it is empty, then it will be 0

# smoking_counts.csv
# ex_smokers_years_quit.csv
# all_patients_smoking_history.csv

# convert to numbers


# 5. 
new_data <- read_csv("Downloads/new.csv")
new_data_modified <- new_data %>% select(-smoking_history)
merged_data <- left_join(new_data_modified, all_patients_smoking, by = "id")
merged_data <- merged_data %>%
  mutate(years_since_quit = ifelse(is.na(years_since_quit), 0, years_since_quit))
if ("smoking_history_numeric" %in% colnames(merged_data) & "years_since_quit" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    relocate(smoking_history_numeric, years_since_quit, .after = weight)
}
merged_data <- merge(merged_data, df_result, by = "id", all.x = TRUE)
if ("years_since_medical_events" %in% colnames(merged_data) & "years_since_quit" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    relocate(years_since_medical_events, .after = years_since_quit)
}
merged_data <- merge(merged_data, binary_df, by = "id", all.x = TRUE)
write.csv(merged_data, "merged_new.csv", row.names = FALSE)



# 6. 
df_additional <- read_excel("Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx", sheet = "Hospital-length-of-stay")
columns_to_keep <- c("id", "days_in_hospital_prior_to_expiration", "hospital_length_of_stay", 
                     "icu_length_of_stay", "days_in_hospital_prior_to_icu_admission", 
                     "time_on_mechanical_ventilation", "days_in_hospital_prior_to_mechanical_ventilation", 
                     "days_to_first_covid19_test_negative")

df_additional <- df_additional %>% select(all_of(columns_to_keep))
merged_data <- merge(merged_data, df_additional, by = "id", all.x = TRUE)
merged_data <- merged_data %>% mutate_all(~replace(., is.na(.), 0))
write.csv(merged_data, "merged_new.csv", row.names = FALSE)



