# Body weight data cleaning: daily weight, DEXA, and MRI muscle
# X. Peng
#
# This script processes three body-weight/body-composition data sources:
# 1. Daily body weight
# 2. DEXA body composition
# 3. MRI thigh muscle
#
# Workflow:
# raw data -> source-specific clean tables -> lookup-matched tables -> QC outputs
# Bit more details
# Raw data in data/Original_data/ are read only and never modified.

# Libraries
library(tidyverse)
library(readxl)

# Functions
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

#### Output directories ####
processed_dir <- "data/processed_data/body_weight"
daily_output_dir <- file.path(processed_dir, "daily_body_weight")
dexa_output_dir <- file.path(processed_dir, "dexa_body_composition")
mri_output_dir <- file.path(processed_dir, "mri_muscle")
lookup_output_dir <- "data/processed_data/lookup"
qc_dir <- "results/qc/body_weight"

dir.create(daily_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dexa_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mri_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

## Open data ##
lookup <- read_excel("data/processed_data/lookup/BRACE_subject_group_stage_day_lookup.xlsx")
daily_c1 <- read_excel("data/Original_data/01 - GENERAL DATA/03 - BODY WEIGHT/BRACE_GEN_BODY-WEIGHT_ALLC1.xlsx")
daily_c2 <- read_excel("data/Original_data/01 - GENERAL DATA/03 - BODY WEIGHT/BRACE_GEN_BODY-WEIGHT_ALLC2.xlsx")
dexa <- read_excel("data/Original_data/05 - METABOLISM/01 - RMR & DEXA BODY COMPOSITION/RAW DATA/BRACE_BSM_DEXA_BODYCOMP_ALL.xlsx")
mri <- read_excel("data/Original_data/05 - METABOLISM/02 - MRI MUSCLE/RAW DATA/BRACE_BSM_MRI_MUSCLE_ALL.xlsx")

#### Lookup table with subject IDs ####
head(lookup)
names(lookup)
colnames(lookup)
dim(lookup)
str(lookup)

lookup_clean <- lookup |> 
    mutate(
      ID = str_c("BRACE_", Subject), .after = Subject,
      Group = as.factor(Group),
      Stage = as.factor(Stage),
      Subject = as.factor(Subject),
      StudyDay = make_study_day(stage_key),
      StudyDay = as.factor(StudyDay),
      Date = as.Date(Date),
      DateTime = as.POSIXct(DateTime),
      Group= case_when(
        Group == "ExAG" ~ "Exercise_Gravity",
        Group == "Ex" ~ "Exercise",
        TRUE ~ Group
      )
    )
write.csv(lookup_clean, file.path(lookup_output_dir, "lookup_table.csv"))
saveRDS(lookup_clean, file.path(lookup_output_dir, "lookup_table.RDS"))

head(lookup_clean)
dim(lookup_clean)
str(lookup_clean)
unique(lookup_clean$Group)

#### Daily body weight ####
head(daily_c1)
dim(daily_c1)
str(daily_c1)

daily_c1_clean <- daily_c1 |>
  mutate(
    cohort = "C1",
    ID = as.factor(str_c("BRACE_",Record)),
    Stage = str_extract(`Event Name`, "^[^ ]+"),
    stage_key = normalize_stage_key(Stage),
    StudyDay = make_study_day(stage_key),
    StudyDay = as.factor(StudyDay),
    DateTime = as.POSIXct(`Date - Heure - Première prise de constantes`),
    Date = as.Date(DateTime),
    daily_body_weight_kg = as.numeric(`Poids - Weight (Kg):`),
    measurement = "weighing scale",
    )|> 
  select(
    cohort, ID, StudyDay,daily_body_weight_kg,Subject = Record,stage_key,
    Date, DateTime, measurement)

head(daily_c1_clean)
str(daily_c1_clean)


head(daily_c2)
str(daily_c2)
tail(daily_c2[which(daily_c2$`Volunteer ID` == "Z" & daily_c2$`Study day - Jour d'étude` == "HDT37"),])

daily_c2_clean <- daily_c2 |> 
  mutate(
    cohort = "C2",
    Subject = as.factor(`Volunteer ID`),
    ID = as.factor(str_c("BRACE_",Subject)),
    stage_key = normalize_stage_key(`Study day - Jour d'étude`),
    StudyDay = make_study_day(stage_key),
    DateTime = as.POSIXct(`Date et heure - Date and Time`),
    Date = as.Date(DateTime),
    daily_body_weight_kg = as.numeric(`Poids - Weight (Kg)`), # two missings ND Z HDT37; Y HDT37
    measurement = "weighing scale"
  ) |> 
  select(
    cohort, ID, StudyDay, daily_body_weight_kg, Subject, stage_key,
    Date, DateTime, measurement)

head(daily_c2_clean)
names(daily_c2_clean)
names(daily_c1_clean)
str(daily_c1_clean)
str(daily_c2)

daily_bw_merged <- bind_rows(daily_c1_clean, daily_c2_clean)
names(daily_bw_merged)

daily_bw_merged <- daily_bw_merged |> 
left_join(
    lookup_clean |> 
      select(ID, stage_key, Group),
    by = c("ID","stage_key")
  )

names(daily_bw_merged)
head(daily_bw_merged)
dim(daily_bw_merged)
dim(daily_bw_merged)
str(daily_bw_merged)

write_csv(daily_bw_merged, file.path(daily_output_dir, "daily_body_weight_clean.csv"))
saveRDS(daily_bw_merged, file.path(daily_output_dir,"daily_body_weight_clean.RDS"))

#### DEXA body composition ####
colnames(dexa) <- colnames(dexa) |> str_to_lower() |> str_replace_all( " ", "_")

dim(dexa)


dexa_merged <- dexa |> 
  mutate(
    ID = str_c("BRACE_", last_name),
    cohort = str_remove_all(first_name, "BRACE "),
    stage_key = normalize_stage_key(study_day),
  ) |> 
  select(-last_name, -first_name, -birthdate, -sex, -pfile_name) |> 
  left_join(lookup_clean)
head(dexa_merged)
names(dexa_merged)
dim(dexa_merged)

write_csv(dexa_merged, file.path(dexa_output_dir, "dexa_clean.csv"))
saveRDS(dexa_merged, file.path(dexa_output_dir, "dexa_clean.RDS"))
 
#### MRI thigh muscle ####
colnames(mri) <- colnames(mri) |> str_replace_all("%", "percent") |> str_replace_all(" ", "_")
colnames(mri)

mri_clean <- mri |> 
  mutate(ID = str_c("BRACE_", subject_long),
          Group= case_when(
            group_long == "AGB" ~ "Exercise_Gravity",
            group_long == "B" ~ "Exercise",
            group_long == "CTRL" ~ "Control",
            TRUE ~ group_long
          ),
          Timepoint = timepoint_long,
          total_thigh_fat_free_muscle_volume_l = tot_volume,
          total_thigh_muscle_fat_infiltration_percent = tot_percent
) |> 
  select(-subject_long, -group_long, -timepoint_long)

write_csv(mri_clean, file.path(mri_output_dir, "body_weight_mri_clean.csv"))
saveRDS(mri_clean, file.path(mri_output_dir, "body_weight_mri_clean.RDS"))

