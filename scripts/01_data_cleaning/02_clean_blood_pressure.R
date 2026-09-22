
# Libraries
library(tidyverse)
library(readxl)


#### Output directories ####


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


## Open data ##
excel_sheets(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC1.xlsx"))
bp_daily_C1 <- read_excel(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC1.xlsx"),sheet = "original data")
bp_daily_C2 <- read_excel(file.path("data/Original_data/01 - GENERAL DATA/07 - VITAL SIGNS/BRACE_GEN_VITAL-SIGNS_ALLC2.xlsx"))
lookup_clean <- readRDS(file.path(lookup_output_dir, "lookup_table.RDS"))


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
    StudyDay = as.factor(StudyDay),
    
    datetime_AM_1 = `Date - Heure - Première prise de constantes`,
    temperature_AM = `Temperature (Celsius):`,
    systolic_AM_1 = `Systolic PA (mmHg):`,
    diastolic_AM_1 = `Diastolic PA (mmHg):`,
    mean_bp_AM_1 = `PA moyenne - Mean BP (mmHg)`,
    heart_rate_AM_1 = `Rythme Cardiaque - Heart Rate (bpm):`,

    datetime_AM_2 = `Date - Heure - Seconde prise de constantes`,
    systolic_AM_2 = `Systolic PA (mmHg):_1`,
    diastolic_AM_2 = `Diastolic PA (mmHg):_2`,
    mean_bp_AM_2 = `PA moyenne - Mean BP (mmHg)_3`,
    heart_rate_AM_2 = `Rythme Cardiaque - Heart Rate (bpm):_4`,

    datetime_AM_3 = `Date - Heure - Troisième prise de constantes`,
    systolic_AM_3 = `Systolic PA (mmHg):_5`,
    diastolic_AM_3 = `Diastolic PA (mmHg):_6`,
    mean_bp_AM_3 = `PA moyenne - Mean BP (mmHg)_7`,
    heart_rate_AM_3 = `Rythme Cardiaque - Heart Rate (bpm):_8`,
    comments_AM = `Commentaires - Comments`,

    datetime_PM_1 = `Date - Heure - 1ere prise de constantes`,
    temperature_PM = `Temperature (Celsius):_9`,
    systolic_PM_1 = `Systolic PA (mmHg):_10`,
    diastolic_PM_1 = `Diastolic PA (mmHg):_11`,
    mean_bp_PM_1 = `PA moyenne - Mean BP (mmHg)_12`,
    heart_rate_PM_1 = `Rythme Cardiaque - Heart Rate (bpm):_13`,

    datetime_PM_2 = `Date - Heure - 2nd prise de constantes`,
    diastolic_PM_2 = `Diastolic PA (mmHg):_14`,
    systolic_PM_2 = `Systolic PA (mmHg):_15`,
    mean_bp_PM_2 = `PA moyenne - Mean BP (mmHg)_16`,
    heart_rate_PM_2 = `Rythme Cardiaque - Heart Rate (bpm):_17`,

    datetime_PM_3 = `Date - Heure - 3ème prise de constantes (Optionnelle)`,
    systolic_PM_3 = `Systolic PA (mmHg):_18`,
    diastolic_PM_3 = `Diastolic PA (mmHg):_19`,
    mean_bp_PM_3 = `PA moyenne - Mean BP (mmHg)_20`,
    heart_rate_PM_3 = `Rythme Cardiaque - Heart Rate (bpm):_21`,
    comments_PM = `Commentaires - Comments_22`  ) |> 
  select(cohort,ID,Stage,stage_key,StudyDay,matches("_(AM|PM)"))


head(bp_daily_C1_clean)
names(bp_daily_C1_clean)
dim(bp_daily_C1_clean)


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



##combine fina arm_cuff
arm_cuff_files <- list.files(
  "data/Original_data/03 - FINAPRESS",
  pattern = "Arm Cuff\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

length(arm_cuff_files)

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
