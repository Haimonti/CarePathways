# ---------------------------
# 1.
# ---------------------------
library(readxl)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(qdapDictionaries)
data(GradyAugmented)

# ---------------------------
# 2. 
# ---------------------------
df <- read_excel("Documents/UB/Spring 2025/Research/Canada_Hosp1_COVID_InpatientData.xlsx")
df$comorbidities_other <- as.character(df$comorbidities_other)
df$comorbidities_other[is.na(df$comorbidities_other)] <- ""

# ---------------------------
# 3. 
# ---------------------------
removed_numbers <- 0
removed_typos <- 0
removed_special_chars <- 0
removed_non_english <- 0
year_list <- list()          # All 4-digit years (for counts_of_year_extraction_report.csv)
special_chars_list <- list()  # All removed special characters
typos_removed <- character(0) # Removed typos (only 1-letter words, 2 repeated letters like "aa", or 3 repeated letters like "aaa")
non_english_removed_list <- character(0)  # Removed words not in allowed lists
stop_words_removed <- character(0)        # Stop words that were removed

# ---------------------------
# 4. Medical Terms Dictionary
# ---------------------------
base_medical_terms <- c(
  "gerd", "gastroesophageal", "reflux", "copd", "dementia", "barrett",
  "esophagus", "pneumonia", "schizophrenia", "hiatus", "hernia",
  "diverticulosis", "irritable", "bowel", "syndrome", "parkinsons",
  "hyperlipidemia", "osteoporosis", "dyslipidemia", "cholecystitis",
  "neuroencephalopathy", "prostatic", "nephropathy", "tricuspid",
  "regurgitation", "pulmonary", "hypertension", "cholecystectomy",
  "hypercholesteremia", "osteomyelitis", "gout", "hypothyroidism",
  "glaucoma", "tachycardia", "anemia", "cataract", "diverticulitis",
  "thrombocytopenia", "osteopenia", "cardiomyopathy", "arrhythmia",
  "neuropathy", "colitis", "steatohepatitis", "meningioma", "sarcoidosis",
  "lymphoma", "carcinoma", "thromboembolism", "embolism", "stenosis",
  "hyperplasia", "dyskinesia", "encephalopathy", "nephritis", "cholangitis",
  "pancreatitis", "cirrhosis", "fibromyalgia", "vasculitis", "myopathy",
  "retinopathy", "thyroidectomy", "adenocarcinoma", "leukemia",
  "lymphadenopathy", "myeloma", "neuroblastoma", "osteogenesis",
  "spondylitis", "tendinitis", "ulcerative", "valvulopathy"
)

additional_terms <- c(
  "osteoarthritis", "hypothyroid", "uti", "dvt", "osa", "infarction",
  "myocardial", "cabg", "chf", "arthroplasty", "diverticular", "endocarditis",
  "gestational", "mellitus", "orthostatic", "afib", "alzheimer", "choledocholithiasis",
  "gastroenteritis", "gastrointestinal", "hydronephrosis", "hypercholesterolemia",
  "nephrectomy", "prostatectomy", "rheumatoid", "rvsp", "sinusitis", "thalassemia",
  "angiodysplasia", "angioplasty", "antiglobulin", "bipap", "bronch", "calcaneous",
  "ckd", "discitis", "echocardiogram", "erythematosus", "esophagitis", "follicular",
  "gammopathy", "glioblastoma", "hyperlipemia", "hypocholesteremia", "hyponatremia",
  "hypoventilation", "idiopathic", "lymphocytic", "mediastinal", "monoclonal",
  "noncardiac", "normocytic", "obsessivecompulsive", "parkinson", "polycythemia",
  "resection", "scoliosis", "stent", "stenting", "trigeminal", "urolithiasis",
  "abu", "adhd", "affective", "akinesis", "alzheimers", "amiodarone", "amputationsite",
  "angio", "anticoagulation", "antiinflammatory", "arteritis", "arthroplasties", "asa",
  "asystole", "atrioventricular", "attrophy", "autonomic", "avoidant", "azathioprine",
  "bacteremia", "bedbound", "bleedafter", "bradycardia", "bronchiectasis", "bronchogenic",
  "calcaneal", "cancerprior", "cancerrecurrent", "cancerremote", "cellulitis", "chemo",
  "chilhood", "cholangiocarcinoma", "cholelithiasis", "cholesterolemia", "colectomy",
  "colonoscopy", "crohns", "csection", "decompression", "dermatomyositis", "diskitis",
  "ductal", "dyslipedemia", "dyslipidaemia", "dysphasia", "embolectomy", "endarterectomy",
  "endometrial", "eosinophilic", "etoposide", "exacerbation", "extendedspectrum",
  "feedings", "followup", "foraminal", "gastrooesophageal", "gramnegative", "gravis",
  "groundglass", "hemiparesis", "hemiplegia", "hemo", "hemodialysis", "hydrocele",
  "hydroureter", "hydroxychloroquine", "hyper", "hypercapnic", "hypercholesremia",
  "hypercholesterlemia", "hypercholestrermia", "hyperlipidaemia", "hypernatremia",
  "hyperopia", "hyperparathyroidism", "hypertelorism", "hypogammaglobulinemia",
  "hypogonadism", "hypokinesis", "hypothroidism", "hypotonic", "impaction",
  "impairement", "implantation", "indeterminant", "intracerebral", "intradermal",
  "klebsiella", "kyphoscoliosis", "lactamase", "laparoscopic", "leftsided",
  "lithotripsy", "lobectomy", "lumbosacral", "lumpectomy", "lvot", "macrocytic",
  "mammogram", "menorrhagia", "metatarsal", "meth", "methicillin", "multifocal",
  "multiforme", "multinodular", "myasthenia", "myelodysplasia", "myelofibrosis",
  "myelopathy", "myopericarditis", "myositis", "nash", "nephrolithiasis",
  "nephrosclerosis", "nephrostomy", "neurocognitive", "neurodegenerative",
  "neurosurgery", "neutropenia", "noncompliant", "noninsulin", "nonischemic",
  "nonsteroidal", "obdtuctive", "occlusion", "paraneoplastic",
  "paraproteinemia", "parathyroidectomy", "parkinsonism", "pectoris",
  "pemphigus", "perianal", "peritoneal", "persantine", "pfts", "phlebotomies",
  "pneumothorax", "polymyalgia", "polypectomy", "polysubstance", "popliteal",
  "postoperatively", "posttraumatic", "pseudogout", "psoriatic", "pyelonephritis",
  "pyrophosphate", "radiculopathy", "raynauds", "recurrant", "retension",
  "retropubic", "rheumatica", "rhizotomy", "richters", "rima", "risperidone",
  "rituxan", "sacroilitis", "schizoaffective", "splenectomy", "splenomegaly",
  "staghorn", "stented", "stents", "stimulator", "subarachnoid", "supranuclear",
  "tachybrady", "tardive", "thrombocytopenic", "thrombotic", "thymoma",
  "tracheomalacia", "tracheostomy", "ultrasound", "uncomplicated", "uropathy",
  "utivs", "variceal", "venormal", "ventricular", "viralalcoholicfatty",
  "volvulussigmoid"
)

allowed_medical_terms <- unique(c(base_medical_terms, additional_terms))

# ---------------------------
# 5. Data Cleaning Function
# ---------------------------
clean_text <- function(text) {
  text <- tolower(text)
  text <- trimws(text)
  phrases <- unlist(strsplit(text, ",\\s*"))
  all_words <- character(0)
  for (phrase in phrases) {
    phrase <- trimws(phrase)
    # --- Year Extraction ---
    years <- str_extract_all(phrase, "\\b(19|20)\\d{2}\\b")[[1]]
    year_list <<- c(year_list, years)  # store all extracted years
    phrase <- gsub("\\b(19|20)\\d{2}\\b", "", phrase)
    # --- Remove Non-Year Digits ---
    non_year_digits <- str_extract_all(phrase, "\\d+")[[1]]
    removed_numbers <<- removed_numbers + length(non_year_digits)
    phrase <- gsub("\\d+", "", phrase)
    # --- Special Characters ---
    special_chars <- str_extract_all(phrase, "[^a-z ]")[[1]]
    special_chars <- special_chars[special_chars != "" & !is.na(special_chars)]
    special_chars_list <<- c(special_chars_list, special_chars)
    phrase <- gsub("[^a-z ]", "", phrase)
    # --- Split into Words ---
    words_list <- unlist(strsplit(phrase, "\\s+"))
    words_list <- words_list[words_list != ""]
    # --- Track and Remove Typos ---
    # Only remove words that are exactly 1 letter,
    # OR exactly 2 letters that are the same (e.g. "aa"),
    # OR exactly 3 letters that are the same (e.g. "aaa").
    remove_condition <- (nchar(words_list) == 1) |
      ((nchar(words_list) == 2) & grepl("^([a-z])\\1$", words_list)) |
      ((nchar(words_list) == 3) & grepl("^([a-z])\\1{2}$", words_list))
    removed_typos_words <- words_list[remove_condition]
    typos_removed <<- c(typos_removed, removed_typos_words)
    removed_typos <<- removed_typos + length(removed_typos_words)
    words_list <- words_list[!remove_condition]
    # --- Validate Words (Non-English Check) ---
    # Allowed words are those in GradyAugmented or in allowed_medical_terms.
    words_before_valid <- words_list
    valid_words <- words_list[words_list %in% c(GradyAugmented, allowed_medical_terms)]
    removed_non_eng <- setdiff(words_before_valid, valid_words)
    non_english_removed_list <<- c(non_english_removed_list, removed_non_eng)
    removed_non_english <<- removed_non_english + length(removed_non_eng)
    # --- Remove Stop Words (but keep if in allowed_medical_terms) ---
    current_stop_words <- valid_words[valid_words %in% stop_words$word & !(valid_words %in% allowed_medical_terms)]
    stop_words_removed <<- c(stop_words_removed, current_stop_words)
    
    words_df <- data.frame(word = valid_words, stringsAsFactors = FALSE)
    words_df <- words_df %>% filter(!(word %in% stop_words$word) | (word %in% allowed_medical_terms))
    
    all_words <- c(all_words, words_df$word)
  }
  
  paste(all_words, collapse = " ")
}

# ---------------------------
# 6. Cleanned Data
# ---------------------------
df$cleaned_comorbidities <- sapply(df$comorbidities_other, clean_text)

# ---------------------------
# 7. CSV Files Outputs
# ---------------------------

# (a) cleaned_dictionary.csv: The unique cleaned dictionary.
all_words_unique <- unlist(strsplit(df$cleaned_comorbidities, "\\s+")) %>% 
  .[. != ""] %>% 
  unique()
dictionary_df <- data.frame(
  term = all_words_unique,
  stringsAsFactors = FALSE
)
write.csv(dictionary_df, "cleaned_dictionary.csv", row.names = FALSE)

# (b) counts_of_year_extraction_report.csv: Frequency counts for all extracted years.
year_report <- data.frame(
  Year = unlist(year_list),
  stringsAsFactors = FALSE
) %>% 
  count(Year, name = "Count") %>%
  arrange(desc(Count))
write.csv(year_report, "counts_of_year_extraction_report.csv", row.names = FALSE)

# (c) counts_of_years_of_medical_events.csv: Extract "event year" pairs.
medical_events_list <- list()
for(i in seq_along(df$comorbidities_other)) {
  text <- tolower(df$comorbidities_other[i])
  # Regex: one or more letters/spaces (non-greedy) then whitespace and a 4-digit year.
  matches <- str_match_all(text, "([a-z ]+?)\\s+(\\b(?:19|20)\\d{2}\\b)")[[1]]
  if(nrow(matches) > 0){
    for(j in 1:nrow(matches)){
      event <- trimws(matches[j,2])
      year <- matches[j,3]
      if(event != ""){
        medical_events_list <- append(medical_events_list, list(c(year, event)))
      }
    }
  }
}
if(length(medical_events_list) > 0){
  medical_events_df <- do.call(rbind, medical_events_list)
  colnames(medical_events_df) <- c("year", "medical_event")
  medical_events_df <- as.data.frame(medical_events_df, stringsAsFactors = FALSE)
} else {
  medical_events_df <- data.frame(year = character(0), medical_event = character(0))
}
write.csv(medical_events_df, "counts_of_years_of_medical_events.csv", row.names = FALSE)

# (d) years_since_medical_events.csv: calculate years passed since last medical event.
df$year_extracted <- as.numeric(str_extract(df$comorbidities_other, "\\b(19|20)\\d{2}\\b"))
df <- df %>%
  mutate(years_since_medical_events = ifelse(!is.na(year_extracted), 2025 - year_extracted, 0))
df_result <- df %>% select(id, years_since_medical_events)
write.csv(df_result, "years_since_medical_events.csv", row.names = FALSE)

# (e) special_characters_report.csv: Frequency counts for removed special characters.
special_char_report <- data.frame(
  Character = unlist(special_chars_list),
  stringsAsFactors = FALSE
) %>% 
  filter(Character != "" & !is.na(Character)) %>%
  count(Character, name = "Count") %>%
  arrange(desc(Count))
write.csv(special_char_report, "special_characters_report.csv", row.names = FALSE)

# (f) typo_analysis_report.csv: Frequency table for removed typos grouped by letter count (only lengths 1, 2, 3).
typo_lengths <- nchar(typos_removed)
typo_analysis_summary <- data.frame(length = typo_lengths) %>% 
  group_by(length) %>% 
  summarise(count = n()) %>%
  filter(length %in% c(1, 2, 3))
write.csv(typo_analysis_summary, "typo_analysis_report.csv", row.names = FALSE)

# (g) list_of_removed_typos_with_counts.csv: List each removed typo and its frequency.
typo_report <- data.frame(
  Typo = typos_removed,
  stringsAsFactors = FALSE
) %>%
  count(Typo, name = "Count") %>%
  arrange(desc(Count))
write.csv(typo_report, "list_of_removed_typos_with_counts.csv", row.names = FALSE)

# (h) non_English_words_report.csv: Frequency table for removed non-English words.
non_eng_report <- data.frame(
  NonEnglishWord = non_english_removed_list,
  stringsAsFactors = FALSE
) %>%
  count(NonEnglishWord, name = "Count") %>%
  arrange(desc(Count))
write.csv(non_eng_report, "non_English_words_report.csv", row.names = FALSE)

# (i) stop_words_count.csv: List all removed stop words and their counts.
stop_words_report <- data.frame(
  StopWord = stop_words_removed,
  stringsAsFactors = FALSE
) %>%
  count(StopWord, name = "Count") %>%
  arrange(desc(Count))
write.csv(stop_words_report, "stop_words_count.csv", row.names = FALSE)

# (j) cleanup_summary_report.csv: Summary of cleaning metrics.
cleanup_summary <- data.frame(
  Metric = c(
    "Total Numbers Removed (non-years)",
    "Total Years Extracted",
    "Total Typos Removed",
    "Total Special Characters Removed",
    "Total Non-English Words Removed",
    "Total Stop Words Removed"
  ),
  Count = c(
    removed_numbers,
    sum(year_report$Count),
    sum(typo_report$Count),
    sum(special_char_report$Count),
    removed_non_english,
    sum(stop_words_report$Count)
  )
)
write.csv(cleanup_summary, "cleanup_summary_report.csv", row.names = FALSE)

# ---------------------------
# 8. Binary Matrix
# ---------------------------

create_binary_matrix <- function(cleaned_texts, dictionary_terms) {
  t(sapply(cleaned_texts, function(text) {
    words <- unlist(strsplit(text, "\\s+"))
    as.integer(dictionary_terms %in% words)
  }))
}
binary_matrix <- create_binary_matrix(df$cleaned_comorbidities, all_words_unique)
binary_df <- data.frame(id = df$id, binary_matrix, stringsAsFactors = FALSE)
colnames(binary_df) <- c("id", all_words_unique)
print(paste("Matrix dimensions:", nrow(binary_df), "x", ncol(binary_df)))
write.csv(binary_df, "binary_matrix.csv", row.names = FALSE)



# The following CSV files are produced:
# 1. counts_of_year_extraction_report.csv
# 2. counts_of_years_of_medical_events.csv
# 3. years_since_medical_events.csv
# 4. special_characters_report.csv
# 5. typo_analysis_report.csv
# 6. list_of_removed_typos_with_counts.csv
# 7. non_English_words_report.csv
# 8. stop_words_count.csv
# 9. cleanup_summary_report.csv
# 10. cleaned_dictionary.csv
# 11. binary_matrix.csv







