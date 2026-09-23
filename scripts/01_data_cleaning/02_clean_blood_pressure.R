# Blood pressure data cleaning
# X. Peng
#

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

# Average morning and evening; return NA if both are missing
daily_mean <- function(am, pm) {
  result <- rowMeans(cbind(am, pm), na.rm = TRUE)
  result[is.nan(result)] <- NA_real_
  result
}

mean_available <- function(...) {
  result <- rowMeans(cbind(...), na.rm = TRUE)
  result[is.nan(result)] <- NA_real_
  result
}

n_available <- function(...) {
  rowSums(!is.na(cbind(...)))
}

#### Output directories ####
bp_output_dir <- "data/processed_data/blood_pressure"
dir.create(bp_output_dir, recursive = TRUE, showWarnings = FALSE)

## Open data ##
# excel_sheets(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC1.xlsx"))
bp_daily_C1 <- read_excel(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC1.xlsx"),
                            sheet = "original data")
# excel_sheets(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC2.xlsx"))
bp_daily_C2 <- read_excel(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC2.xlsx"),
                            sheet = "VITAL SIGNS C2")
lookup_clean <- readRDS("data/processed_data/lookup/lookup_table.RDS")

names(bp_daily_C1)
names(bp_daily_C2)
names(lookup_clean)

head(bp_daily_C1)

bp_daily_C1_clean <- bp_daily_C1 |> 
  mutate(
    cohort = "C1",
    ID = as.factor(str_c("BRACE_",Record)),
    Stage = str_extract(`Event Name`, "^[^ ]+"),
    stage_key = normalize_stage_key(Stage),
    StudyDay = make_study_day(stage_key),
    StudyDay = as.factor(StudyDay)
     ) |> 
  select(
    contains(c("Systolic PA", "Diastolic PA", "Mean BP", "Heart Rate")),
  ) |> 
  rename_with(
    \(x) {
      measure <- case_when(
        str_detect(x, "Systolic")  ~ "systolic",
        str_detect(x, "Diastolic") ~ "diastolic",
        str_detect(x, "Mean BP")   ~ "mean_bp",
        str_detect(x, "Heart Rate") ~ "heart_rate"
      )

      suffix <- coalesce(as.integer(str_extract(x, "\\d+$")), 0L)
      occasion <- case_when(
        suffix == 0       ~ "AM_1",
        between(suffix, 1, 4)   ~ "AM_2",
        between(suffix, 5, 8)   ~ "AM_3",
        between(suffix, 10, 13) ~ "PM_1",
        between(suffix, 14, 17) ~ "PM_2",
        between(suffix, 18, 21) ~ "PM_3"
      )

      paste(measure, occasion, sep = "_")
    }
  )

head(bp_daily_C1_clean)
names(bp_daily_C1_clean)
dim(bp_daily_C1_clean)

timepoint_means <- bp_daily_C1_clean |> 
  transmute(
    # Morning means
    systolic_AM_mean   = mean_available(systolic_AM_1, systolic_AM_2, systolic_AM_3),
    diastolic_AM_mean  = mean_available(diastolic_AM_1, diastolic_AM_2, diastolic_AM_3),
    mean_bp_AM_mean    = mean_available(mean_bp_AM_1, mean_bp_AM_2, mean_bp_AM_3),
    heart_rate_AM_mean = mean_available(heart_rate_AM_1, heart_rate_AM_2, heart_rate_AM_3),

    # Morning counts
    systolic_AM_n   = n_available(systolic_AM_1, systolic_AM_2, systolic_AM_3),
    diastolic_AM_n  = n_available(diastolic_AM_1, diastolic_AM_2, diastolic_AM_3),
    mean_bp_AM_n    = n_available(mean_bp_AM_1, mean_bp_AM_2, mean_bp_AM_3),
    heart_rate_AM_n = n_available(heart_rate_AM_1, heart_rate_AM_2, heart_rate_AM_3),

    # Evening means
    systolic_PM_mean   = mean_available(systolic_PM_1, systolic_PM_2),
    diastolic_PM_mean  = mean_available(diastolic_PM_1, diastolic_PM_2),
    mean_bp_PM_mean    = mean_available(mean_bp_PM_1, mean_bp_PM_2),
    heart_rate_PM_mean = mean_available(heart_rate_PM_1, heart_rate_PM_2),

    # Evening counts
    systolic_PM_n   = n_available(systolic_PM_1, systolic_PM_2),
    diastolic_PM_n  = n_available(diastolic_PM_1, diastolic_PM_2),
    mean_bp_PM_n    = n_available(mean_bp_PM_1, mean_bp_PM_2),
    heart_rate_PM_n = n_available(heart_rate_PM_1, heart_rate_PM_2)
  )

daily_means <- timepoint_means |> 
  transmute(
    systolic_day_mean   = daily_mean(systolic_AM_mean, systolic_PM_mean),
    diastolic_day_mean  = daily_mean(diastolic_AM_mean, diastolic_PM_mean),
    mean_bp_day_mean    = daily_mean(mean_bp_AM_mean, mean_bp_PM_mean),
    heart_rate_day_mean = daily_mean(heart_rate_AM_mean, heart_rate_PM_mean),

    systolic_day_n   = systolic_AM_n + systolic_PM_n,
    diastolic_day_n  = diastolic_AM_n + diastolic_PM_n,
    mean_bp_day_n    = mean_bp_AM_n + mean_bp_PM_n,
    heart_rate_day_n = heart_rate_AM_n + heart_rate_PM_n
  )

head(timepoint_means)
head(daily_means)



names(bp_daily_C2)
bp_daily_C2_AM <- bp_daily_C2 |>
  filter(
    as.integer(
      format(`Date - Heure - 1ere prise de constantes`, "%H")
    ) < 12
  )

bp_daily_C2_PM <- bp_daily_C2 |>
  filter(
    as.integer(
      format(`Date - Heure - 1ere prise de constantes`, "%H")
    ) >= 12
  )

dim(bp_daily_C2_AM)
dim(bp_daily_C2_PM)

bp_daily_C2_AM_clean <- bp_daily_C2_AM |>
  mutate(
    cohort = "C2",
    ID = as.factor(str_c("BRACE_", `Volunteer ID`)),
    Stage = str_extract(`Event Name`, "^[^ ]+"),
    stage_key = normalize_stage_key(Stage),
    StudyDay = as.factor(make_study_day(stage_key)),

    datetime_AM_1 = `Date - Heure - 1ere prise de constantes`,
    temperature_AM = `Temperature (Celsius)`,
    systolic_AM_1 = `Systolic PA (mmHg)`,
    diastolic_AM_1 = `Diastolic PA (mmHg)`,
    mean_bp_AM_1 = `PA moyenne - Mean BP (mmHg)`,
    heart_rate_AM_1 = `Rythme Cardiaque - Heart Rate (bpm)`,

    datetime_AM_2 = `Date - Heure - 2nd prise de constantes`,
    systolic_AM_2 = `Systolic PA (mmHg) 2`,
    diastolic_AM_2 = `Diastolic PA (mmHg) 2`,
    mean_bp_AM_2 = `PA moyenne - Mean BP (mmHg) 2`,
    heart_rate_AM_2 = `Rythme Cardiaque - Heart Rate (bpm) 2`,

    datetime_AM_3 = `Date - Heure - 3ème prise de constantes (Optionnelle)`,
    systolic_AM_3 = `Systolic PA (mmHg) 3`,
    diastolic_AM_3 = `Diastolic PA (mmHg) 3`,
    mean_bp_AM_3 = `PA moyenne - Mean BP (mmHg) 3`,
    heart_rate_AM_3 = `Rythme Cardiaque - Heart Rate (bpm) 3`,
    comments_AM = `Commentaires - Comments`
  ) |>
  select(
    cohort, ID, Stage, stage_key, StudyDay,
    matches("_AM")
  )


bp_daily_C2_PM_clean <- bp_daily_C2_PM |>
  mutate(
    cohort = "C2",
    ID = as.factor(str_c("BRACE_", `Volunteer ID`)),
    Stage = str_extract(`Event Name`, "^[^ ]+"),
    stage_key = normalize_stage_key(Stage),
    StudyDay = as.factor(make_study_day(stage_key)),

    datetime_PM_1 = `Date - Heure - 1ere prise de constantes`,
    temperature_PM = `Temperature (Celsius)`,
    systolic_PM_1 = `Systolic PA (mmHg)`,
    diastolic_PM_1 = `Diastolic PA (mmHg)`,
    mean_bp_PM_1 = `PA moyenne - Mean BP (mmHg)`,
    heart_rate_PM_1 = `Rythme Cardiaque - Heart Rate (bpm)`,

    datetime_PM_2 = `Date - Heure - 2nd prise de constantes`,
    systolic_PM_2 = `Systolic PA (mmHg) 2`,
    diastolic_PM_2 = `Diastolic PA (mmHg) 2`,
    mean_bp_PM_2 = `PA moyenne - Mean BP (mmHg) 2`,
    heart_rate_PM_2 = `Rythme Cardiaque - Heart Rate (bpm) 2`,

    datetime_PM_3 = `Date - Heure - 3ème prise de constantes (Optionnelle)`,
    systolic_PM_3 = `Systolic PA (mmHg) 3`,
    diastolic_PM_3 = `Diastolic PA (mmHg) 3`,
    mean_bp_PM_3 = `PA moyenne - Mean BP (mmHg) 3`,
    heart_rate_PM_3 = `Rythme Cardiaque - Heart Rate (bpm) 3`,
    comments_PM = `Commentaires - Comments`
  ) |>
  select(
    cohort, ID, Stage, stage_key, StudyDay,
    matches("_PM")
  )


bp_daily_C2_clean <- full_join(
  bp_daily_C2_AM_clean,
  bp_daily_C2_PM_clean,
  by = c("cohort", "ID", "Stage", "stage_key", "StudyDay")
)


names(bp_daily_C2_clean)

dim(bp_daily_C2)

dim(bp_daily_C1_clean)
dim(bp_daily_C2_clean)

## find the differences
setdiff(names(bp_daily_C1_clean), names(bp_daily_C2_clean))

bp_daily_clean <- full_join(
bp_daily_C1_clean,bp_daily_C2_clean,
by = c("cohort", "ID", "Stage", "stage_key", "StudyDay")
)
dim(bp_daily_clean)

names(bp_daily_clean)