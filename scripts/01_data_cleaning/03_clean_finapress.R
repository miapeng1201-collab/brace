# Finapres data cleaning
# X. Peng
# This script extracts arm-cuff blood pressure measurements from the
# participant-level Finapres overview workbooks for cohorts 1 and 2.




# Libraries
library(tidyverse)
library(readxl)

#Output directory 
finapress_output_dir <- "data/processed_data/03_finapress"
dir.create(finapress_output_dir, recursive = TRUE, showWarnings = FALSE)

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


#Open data
finapress_overview_files <- list.files(
  "data/Original_data/03 - FINAPRESS",
  pattern = "^[A-Z]_Overview_Finapress[.]xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

length(finapress_overview_files)

lookup_clean <- readRDS(
  "data/processed_data/lookup/lookup_table.RDS"
)

#combine 

read_armcuff_overview <- function(file) {
  read_excel(
    file,
    sheet = 1,

    # 只读取 Measurement、Time(sec) 和 Marker
    col_types = c(
      "skip",          # Source.Name
      "text",          # Measurement
      "text",          # Time(sec)
      rep("skip", 16), # 中间的血流动力学变量
      "text",          # Marker
      "skip"           # Region
    )
  ) |>
    filter(
      str_detect(
        coalesce(Marker, ""),
        "^ArmCuff:"
      )
    ) |>
    transmute(
      source_file = file,

      cohort = str_match(
        file,
        "/(C[12])/"
      )[, 2],

      Subject = str_match(
        basename(file),
        "^([A-Z])_"
      )[, 2],

      Measurement,

      `Time(sec)` = parse_double(`Time(sec)`),

      Marker
    )
}


armcuff_all <- map_dfr(
  finapress_overview_files,
  read_armcuff_overview
)

str(armcuff_all)

armcuff_all$Marker

bp_fina_clean <- armcuff_all |>
  mutate(
    ID = str_c("BRACE_", Subject),

    Stage = str_extract(
      Measurement,
      "(?:BDC-?[0-9]+|HDT[0-9]+|R[+]?[0-9]+)"
    ),

    stage_key = normalize_stage_key(Stage),
    StudyDay = make_study_day(stage_key),

    armcuff_value = str_extract_all(
      Marker,
      "[0-9.]+/[0-9.]+"
    )
  ) |>
  unnest_longer(
    armcuff_value,
    indices_to = "armcuff_reading"
  ) |>
  separate_wider_delim(
    armcuff_value,
    delim = "/",
    names = c("systolic", "diastolic")
  ) |>
  mutate(
    systolic = as.numeric(systolic),
    diastolic = as.numeric(diastolic),
    mean_bp = (systolic + 2 * diastolic) / 3
  ) |>
  select(
    source_file,
    cohort,
    Subject,
    ID,
    Stage,
    stage_key,
    StudyDay,
    Measurement,
    `Time(sec)`,
    armcuff_reading,
    Marker,
    systolic,
    mean_bp,
    diastolic
  )

# str(bp_fina_clean)






#### Save clean data ####
write_csv(
  bp_fina_clean,
  file.path(
    finapress_output_dir,
    "finapress_blood_pressure_clean.csv"
  )
)

saveRDS(
  bp_fina_clean,
  file.path(
    finapress_output_dir,
    "finapress_blood_pressure_clean.RDS"
  )
)
