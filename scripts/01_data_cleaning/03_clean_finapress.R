# Finapres data cleaning
# X. Peng
# This script processes two Finapres data sources:
# 1. Arm cuff blood pressure measurements from all available CSV files
# 2. Session-level hemodynamic measurements from Finapress.xlsx


# Libraries
library(tidyverse)
library(readxl)


##combine fina arm_cuff

##open data 
arm_cuff_files <- list.files( "data/Original_data/03 - FINAPRESS",
  pattern = "Arm Cuff\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

#### Output directories ####
finapress_output_dir <- "data/processed_data/03_finapress"
dir.create(finapress_output_dir, recursive = TRUE, showWarnings = FALSE)


length(arm_cuff_files)

# Read and combine all Arm Cuff CSV files.
# Extract cohort, subject, and study stage from each file path.
read_arm_cuff <- function(file) {
  read_delim(
    file,
    delim = ";",
    skip = 7,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    mutate(source_file = file)
}
arm_cuff_all <- map_dfr(arm_cuff_files, read_arm_cuff)

dim(arm_cuff_all)
str(arm_cuff_all)

bp_fina_clean <- arm_cuff_all |>
  mutate(
    cohort = str_match(source_file, "/(C[12])/")[, 2],

    Subject = str_match(
      source_file,
      "/(?:COMBINED|RawData)/([A-Z])/"
    )[, 2],

    ID = as.factor(str_c("BRACE_", Subject)),

    Stage = str_extract(
      source_file,
      "(?:BDC-?\\d+|HDT\\d+|R\\+?\\d+)"
    ),

    stage_key = normalize_stage_key(Stage),

    StudyDay = as.factor(make_study_day(stage_key)),
    systolic= as.numeric(`SYS Arm(mmHg)`),
mean_bp = as.numeric(`MAP Arm(mmHg)`),
diastolic = as.numeric(`DIA Arm(mmHg)`),
heart_rate= as.numeric(`HR Arm(bpm)`)

  )|>
  select(source_file, cohort, Subject, ID, Stage, stage_key, StudyDay,systolic,mean_bp,diastolic ,heart_rate) 

dim(bp_fina_clean)
str(bp_fina_clean)

write_csv(
  bp_fina_clean,
  file.path(finapress_output_dir, "finapress_blood_pressure_clean.csv")
)

saveRDS(
  bp_fina_clean,
  file.path(finapress_output_dir, "finapress_blood_pressure_clean.RDS")
)


## clean hemodynamic measurements

## Open data ##

finapress_hemodynamics <- read_excel(file.path( "data/Original_data/03 - FINAPRESS/Finapress.xlsx")
 )


str(finapress_hemodynamics)
finapress_hemodynamics$Group

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



finapress_hemodynamic_clean <- finapress_hemodynamics |> 
 transmute(
ID = as.factor(str_c("BRACE_",Subject)),
  stage_key = normalize_stage_key(Stage),
  StudyDay = make_study_day(stage_key),
  Group= case_when(
        Group == "ExAG" ~ "Exercise_Gravity",
        Group == "Ex" ~ "Exercise",
        TRUE ~ Group
  ), 
  across(-c(    Subject, Stage, stage_key,Nr,Day2,Date
  )) )


str(finapress_hemodynamic_clean)

# Save clean data
write_csv(
  finapress_hemodynamic_clean,
  file.path(finapress_output_dir , "finapress_hemodynamic_clean.csv")
)

saveRDS(
  finapress_hemodynamic_clean,
  file.path(finapress_output_dir , "finapress_hemodynamic_clean.RDS")
)

  
