
# Urine data cleaning
# X. Peng
#
# This script processes daily urine collection data.
#
# The source Excel sheet contains several tables placed side by side.
# Only the first table, containing the re-measured raw urine results,is used for the clean dataset.




# Libraries
library(tidyverse)
library(readxl)

# Output directories
urine_output_dir <- "data/processed_data/05_urine"

dir.create(
  urine_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# open data 

urine_raw <- read_excel("data/Original_data/01 - GENERAL DATA/06 - URINE/Urine_Remeasured.xlsx")
lookup_cleanlookup_clean <- readRDS(
  "data/processed_data/lookup/lookup_table.RDS"
)



# function
normalize_stage_key <- function(stage) {
  stage |>
    str_replace_all("[-+]", "") |>
    str_replace_all("\\s+", "") |>
    str_to_upper()
}

make_study_day <- function(stage_key) {
  case_when(
    str_detect(stage_key, "BDC") ~ str_replace(stage_key, "BDC", "Baseline_min"),
    str_detect(stage_key, "HDT") ~ str_replace(stage_key, "HDT", "Intervention_"),
    str_detect(stage_key, "R") ~ str_replace(stage_key, "R", "Recovery_")
  )
}


 
# clean urine data

str(urine_raw)
urine_main <- urine_raw[-1, 1:20]

names(urine_main) <- as.character(
  urine_raw[1, 1:20]
)


## U   HDT46 HDT49  No evening urine     
urine_main |>
  filter(
    !is.na(Volume),
    is.na(suppressWarnings(as.numeric(Volume)))
  ) |>
  select(Subject, Day, Date, Period, Volume, Comment)



urine_clean <- urine_main |>
  transmute(
    ID = as.factor(str_c("BRACE_", Subject)),

    StudyDay = make_study_day(
      normalize_stage_key(Day)
    ),

    Date = as.Date(
      as.numeric(Date),
      origin = "1899-12-30"
    ),

    period = Period,
    urine_volume = as.numeric(na_if(Volume, "na")),
    comments = Comment,

    urea_mg_dl = as.numeric(`Urea (mg/dl)`),
    urea_mmol_l = as.numeric(`Urea (mmol/L)`),

    creatinine_mg_dl = as.numeric(`Creatinine (mg/dl)`),
    creatinine_mmol_l = as.numeric(`Creatinin (mmol/L)`),

    total_protein_mg_l = as.numeric(`Total protein (mg/L)`),

    glucose_mg_dl = as.numeric(`Glucose (mg/dl)`),
    glucose_mmol_l = as.numeric(`Glucose (mmol/L)`),

    sodium_mmol_l = as.numeric(`Sodium (mmol/L)`),
    potassium_mmol_l = as.numeric(`Potassium (mmol/L)`),
    chloride_mmol_l = as.numeric(`Chloride (mmol/L)`)
  ) |>
  left_join(
    lookup_clean |>
      distinct(ID, StudyDay, Group),
    by = c("ID", "StudyDay")
  )

str(urine_clean)


# Save 
write_csv(
  urine_clean,
  file.path(urine_output_dir, "urine_clean.csv")
)

saveRDS(
  urine_clean,
  file.path(urine_output_dir, "urine_clean.RDS")
)
