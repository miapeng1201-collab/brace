
# Urine data cleaning
# X. Peng
#
# This script processes daily urine collection data.
#
# The source Excel sheet contains several tables placed side by side.
# Only the first table, containing the re-measured raw urine results,is used for the clean dataset.
#
# Manual corrections and 24-hour calculations:
# - Correct D's visit on 13 April 2023 to BDC7.
# - U HDT46 and HDT49 have no evening urine collection; retain the
#   available period data but exclude these days from 24-hour calculations.
# - Calculate 24-hour urine volume as the sum of mo, dt, and ev volumes,
#   and calculate 24-hour concentrations using volume-weighted means.




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
lookup_clean <- readRDS("data/processed_data/lookup/lookup_table.RDS")


 
# clean urine data

str(urine_raw)
urine_main <- urine_raw[-1, 1:20]

names(urine_main) <- as.character(
  urine_raw[1, 1:20]
)

# Remove rows with no data in any column
urine_main <- urine_main |>
  filter(!if_all(everything(), is.na))


## U   HDT46 HDT49  No evening urine     
urine_main |>
  filter(
    !is.na(Volume),
    is.na(suppressWarnings(as.numeric(Volume)))
  ) |>
  select(Subject, Day, Date, Period, Volume, Comment)



urine_clean <- urine_main |>
  filter(!is.na(Subject), !is.na(Period)) |>
  # Correct D's visit label on 13 April 2023
  mutate(
    Day = if_else(
      Subject == "D" &
        as.Date(as.numeric(Date), origin = "1899-12-30") == as.Date("2023-04-13"),
      "BDC7",
      Day
    ),
    stage_key = Day |>
      str_replace_all("[-+]", "") |>
      str_replace_all("\\s+", "") |>
      str_to_upper()
  ) |>
  transmute(
    ID = as.factor(str_c("BRACE_", Subject)),

    StudyDay = case_when(
      str_detect(stage_key, "BDC") ~ str_replace(stage_key, "BDC", "Baseline_min"),
      str_detect(stage_key, "HDT") ~ str_replace(stage_key, "HDT", "Intervention_"),
      str_detect(stage_key, "R") ~ str_replace(stage_key, "R", "Recovery_")
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

# Exclude U HDT46 and HDT49 from 24-hour calculations
urine_24h <- urine_clean |>
  filter(
    period %in% c("mo", "dt", "ev"),
    !(ID == "BRACE_U" &
        StudyDay %in% c("Intervention_46", "Intervention_49"))
  ) |>
  group_by(ID, StudyDay, Date, Group) |>
  summarise(
    across(
      urea_mg_dl:chloride_mmol_l,
      ~ sum(.x * urine_volume) / sum(urine_volume)
    ),
    urine_volume = sum(urine_volume),
    period = "24h",
    comments = "Calculated from mo, dt, and ev.",
    .groups = "drop"
  )

# Replace existing 24-hour records for recalculated days
recalculated_days <- urine_clean |>
  filter(period %in% c("mo", "dt", "ev")) |>
  distinct(ID, StudyDay)

urine_clean <- bind_rows(
  urine_clean |>
    filter(period != "24h"),
  urine_clean |>
    filter(period == "24h") |>
    anti_join(recalculated_days, by = c("ID", "StudyDay")),
  urine_24h
)

# Check record counts by collection period
urine_clean |>
  count(period)
# Save 
write_csv(
  urine_clean,
  file.path(urine_output_dir, "urine_clean.csv")
)

saveRDS(
  urine_clean,
  file.path(urine_output_dir, "urine_clean.RDS")
)

