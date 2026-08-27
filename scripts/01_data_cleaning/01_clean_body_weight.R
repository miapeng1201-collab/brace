# Clean body weight and body-composition-related data
#
# This script processes three body-weight/body-composition data sources:
# 1. Daily body weight
# 2. DEXA body composition
# 3. MRI thigh muscle
#
# Workflow:
# raw data -> source-specific clean tables -> lookup-matched tables -> QC outputs
#
# Raw data in data/Original_data/ are read only and never modified.

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

# Setup ----

root_dir <- normalizePath(
  file.path(dirname(getwd()), "BRACE"),
  mustWork = FALSE
)

if (!dir.exists(root_dir)) {
  root_dir <- normalizePath(getwd(), mustWork = TRUE)
}

raw_dir <- file.path(root_dir, "data", "Original_data")
processed_dir <- file.path(root_dir, "data", "processed_data", "body_weight")
qc_dir <- file.path(root_dir, "results", "qc", "body_weight")

lookup_file <- file.path(
  root_dir,
  "data",
  "processed_data",
  "lookup",
  "BRACE_subject_group_stage_day_lookup.xlsx"
)

daily_output_dir <- file.path(processed_dir, "daily_body_weight")
dexa_output_dir <- file.path(processed_dir, "dexa_body_composition")
mri_output_dir <- file.path(processed_dir, "mri_muscle")

dir.create(daily_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dexa_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mri_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

# Helper functions ----

normalize_stage_key <- function(stage) {
  stage %>%
    str_replace_all("[-+]", "") %>%
    str_replace_all("\\s+", "") %>%
    str_to_upper()
}

read_lookup <- function(path) {
  read_excel(path, sheet = "subject_group_stage_day_lookup") %>%
    mutate(
      Subject = as.character(Subject),
      Group = as.character(Group),
      Stage = as.character(Stage),
      stage_key = as.character(stage_key),
      Day = as.integer(Day),
      Date = as.Date(Date),
      DateTime = as.POSIXct(DateTime)
    )
}

write_missingness_heatmap <- function(data, output_file, title) {
  group_order <- c("Control", "Ex", "ExAG")
  subject_order <- data %>%
    distinct(Group, Subject) %>%
    mutate(Group = factor(Group, levels = group_order)) %>%
    arrange(Group, Subject) %>%
    pull(Subject)

  plot_data <- data %>%
    mutate(
      has_value = !is.na(value_present),
      Group = factor(Group, levels = group_order),
      Subject = factor(Subject, levels = rev(subject_order)),
      Stage = factor(Stage, levels = unique(Stage[order(Day, Stage)]))
    )

  heatmap_plot <- ggplot(plot_data, aes(x = Day, y = Subject, fill = has_value)) +
    geom_tile(color = "white", linewidth = 0.15) +
    geom_vline(xintercept = c(14.5, 74.5), linewidth = 0.5, color = "black") +
    scale_fill_manual(
      values = c(`TRUE` = "#2166AC", `FALSE` = "#D9D9D9"),
      labels = c(`TRUE` = "Present", `FALSE` = "Missing"),
      name = "Data"
    ) +
    scale_x_continuous(
      breaks = c(0, 1, 14, 15, 44, 45, 74, 75, 89, 104, 105),
      labels = c("BDC-15", "BDC-14", "BDC-1", "HDT1", "HDT30", "HDT31", "HDT60", "R+0", "R+14", "R+29", "R+30")
    ) +
    labs(
      title = title,
      x = "Continuous study day",
      y = "Subject"
    ) +
    facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      strip.text.y = element_text(angle = 0),
      panel.spacing.y = unit(0.08, "lines")
    )

  ggsave(output_file, heatmap_plot, width = 11, height = 6, device = "pdf")
}

write_timepoint_missingness_heatmap <- function(data, output_file, title) {
  group_order <- c("Control", "Ex", "ExAG")
  subject_order <- data %>%
    distinct(Group, Subject) %>%
    mutate(Group = factor(Group, levels = group_order)) %>%
    arrange(Group, Subject) %>%
    pull(Subject)

  plot_data <- data %>%
    mutate(
      has_value = !is.na(value_present),
      Group = factor(Group, levels = group_order),
      Subject = factor(Subject, levels = rev(subject_order)),
      timepoint = factor(timepoint, levels = unique(timepoint))
    )

  heatmap_plot <- ggplot(plot_data, aes(x = timepoint, y = Subject, fill = has_value)) +
    geom_tile(color = "white", linewidth = 0.25) +
    scale_fill_manual(
      values = c(`TRUE` = "#2166AC", `FALSE` = "#D9D9D9"),
      labels = c(`TRUE` = "Present", `FALSE` = "Missing"),
      name = "Data"
    ) +
    labs(
      title = title,
      x = "Timepoint",
      y = "Subject"
    ) +
    facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      strip.text.y = element_text(angle = 0),
      panel.spacing.y = unit(0.08, "lines")
    )

  ggsave(output_file, heatmap_plot, width = 6, height = 6, device = "pdf")
}

standardize_group <- function(group) {
  case_when(
    group %in% c("CTRL", "Control", "Contr") ~ "Control",
    group %in% c("B", "Bike", "Ex") ~ "Ex",
    group %in% c("AGB", "AG-Bike", "Ex-Ag", "ExAG") ~ "ExAG",
    TRUE ~ group
  )
}

clean_column_names <- function(names) {
  names %>%
    str_replace_all("%", "percent") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
}

# Lookup ----

subject_day_lookup <- read_lookup(lookup_file)

# 1. Daily body weight ----

daily_c1_file <- file.path(
  raw_dir,
  "01 - GENERAL DATA",
  "03 - BODY WEIGHT",
  "BRACE_GEN_BODY-WEIGHT_ALLC1.xlsx"
)

daily_c2_file <- file.path(
  raw_dir,
  "01 - GENERAL DATA",
  "03 - BODY WEIGHT",
  "BRACE_GEN_BODY-WEIGHT_ALLC2.xlsx"
)

daily_c1_raw <- read_excel(daily_c1_file, sheet = "BRACE-Body Weight")
daily_c2_raw <- read_excel(daily_c2_file, sheet = "BRACEC2-Poids_DATA_LABELS_2024-")

daily_c1_clean <- daily_c1_raw %>%
  mutate(
    cohort = "C1",
    Subject = as.character(Record),
    Stage = str_extract(`Event Name`, "^[^ ]+"),
    stage_key = normalize_stage_key(Stage),
    arm_number = as.integer(str_match(`Event Name`, "Arm ([0-9]+):")[, 2]),
    arm_label = str_match(`Event Name`, "Arm [0-9]+: ([^)]+)")[, 2],
    measured_datetime = as.POSIXct(`Date - Heure - Première prise de constantes`),
    measured_date = as.Date(measured_datetime),
    daily_body_weight_kg = as.numeric(`Poids - Weight (Kg):`),
    comments = NA_character_,
    source_file = basename(daily_c1_file),
    source_sheet = "BRACE-Body Weight",
    source_row = row_number() + 1
  ) %>%
  select(
    cohort, Subject, Stage, stage_key, arm_number, arm_label,
    measured_date, measured_datetime, daily_body_weight_kg, comments,
    source_file, source_sheet, source_row
  )

daily_c2_clean <- daily_c2_raw %>%
  mutate(
    cohort = "C2",
    Subject = as.character(`Volunteer ID`),
    Stage = as.character(`Study day - Jour d'étude`),
    stage_key = normalize_stage_key(Stage),
    arm_number = NA_integer_,
    arm_label = NA_character_,
    measured_datetime = as.POSIXct(`Date et heure - Date and Time`),
    measured_date = as.Date(measured_datetime),
    daily_body_weight_kg = as.numeric(`Poids - Weight (Kg)`),
    comments = as.character(Comments),
    source_file = basename(daily_c2_file),
    source_sheet = "BRACEC2-Poids_DATA_LABELS_2024-",
    source_row = row_number() + 1
  ) %>%
  select(
    cohort, Subject, Stage, stage_key, arm_number, arm_label,
    measured_date, measured_datetime, daily_body_weight_kg, comments,
    source_file, source_sheet, source_row
  )

daily_body_weight_merged <- bind_rows(daily_c1_clean, daily_c2_clean) %>%
  arrange(Subject, measured_datetime, Stage)

daily_body_weight_lookup_matched <- daily_body_weight_merged %>%
  left_join(
    subject_day_lookup,
    by = c("Subject", "Stage", "stage_key"),
    suffix = c("_measured", "_lookup")
  ) %>%
  mutate(
    Group = case_when(
      !is.na(Group) ~ Group,
      arm_label == "AG-Bike" ~ "ExAG",
      arm_label == "Bike" ~ "Ex",
      arm_label == "Control" ~ "Control",
      TRUE ~ NA_character_
    ),
    date_matches_lookup = is.na(Date) | is.na(measured_date) | Date == measured_date
  ) %>%
  select(
    cohort, Subject, Group, Stage, stage_key, Day, Date, DateTime,
    measured_date, measured_datetime, daily_body_weight_kg,
    date_matches_lookup, arm_number, arm_label, comments,
    source_file, source_sheet, source_row
  ) %>%
  arrange(Subject, Day, Stage)

write_csv(
  daily_body_weight_lookup_matched,
  file.path(daily_output_dir, "daily_body_weight_lookup_matched.csv"),
  na = ""
)

daily_missingness <- daily_body_weight_lookup_matched %>%
  transmute(
    Subject, Group, Stage, Day,
    value_present = daily_body_weight_kg
  )

write_missingness_heatmap(
  daily_missingness,
  file.path(qc_dir, "daily_body_weight_missingness_heatmap.pdf"),
  "Daily body weight missingness"
)

daily_summary <- tibble(
  data_source = "daily_body_weight",
  metric = c(
    "merged_rows",
    "lookup_matched_rows",
    "subjects",
    "rows_with_valid_weight",
    "missing_group",
    "missing_day",
    "date_mismatch_with_lookup"
  ),
  value = c(
    nrow(daily_body_weight_merged),
    nrow(daily_body_weight_lookup_matched),
    n_distinct(daily_body_weight_lookup_matched$Subject),
    sum(!is.na(daily_body_weight_lookup_matched$daily_body_weight_kg)),
    sum(is.na(daily_body_weight_lookup_matched$Group)),
    sum(is.na(daily_body_weight_lookup_matched$Day)),
    sum(!daily_body_weight_lookup_matched$date_matches_lookup, na.rm = TRUE)
  )
)

# 2. DEXA body composition ----

dexa_file <- file.path(
  raw_dir,
  "05 - METABOLISM",
  "01 - RMR & DEXA BODY COMPOSITION",
  "RAW DATA",
  "BRACE_BSM_DEXA_BODYCOMP_ALL.xlsx"
)

dexa_raw <- read_excel(dexa_file, sheet = "BRACE BODY COMP")
names(dexa_raw) <- clean_column_names(names(dexa_raw))

dexa_body_composition_lookup_matched <- dexa_raw %>%
  transmute(
    Subject = as.character(last_name),
    Stage = as.character(study_day),
    stage_key = normalize_stage_key(Stage),
    scan_date = as.Date(scan_date),
    sex = as.character(sex),
    pfile_name = as.character(pfile_name),
    brain_fat,
    water_lbm,
    head_fat,
    head_lean,
    head_mass,
    head_pfat,
    larm_fat,
    larm_lean,
    larm_mass,
    larm_pfat,
    rarm_fat,
    rarm_lean,
    rarm_mass,
    rarm_pfat,
    trunk_fat,
    trunk_lean,
    trunk_mass,
    trunk_pfat,
    l_leg_fat,
    l_leg_lean,
    l_leg_mass,
    l_leg_pfat,
    r_leg_fat,
    r_leg_lean,
    r_leg_mass,
    r_leg_pfat,
    subtot_fat,
    subtot_lean,
    subtot_mass,
    subtot_pfat,
    wbtot_fat,
    wbtot_lean,
    wbtot_mass,
    wbtot_pfat,
    roi_type,
    roi_width,
    roi_height,
    tissue_analysis_method
  ) %>%
  left_join(
    subject_day_lookup,
    by = c("Subject", "Stage", "stage_key")
  ) %>%
  select(
    Subject, Group, Stage, stage_key, Day, Date, DateTime,
    scan_date, sex, pfile_name,
    everything()
  ) %>%
  arrange(Subject, Day, Stage)

write_csv(
  dexa_body_composition_lookup_matched,
  file.path(dexa_output_dir, "dexa_body_composition_lookup_matched.csv"),
  na = ""
)

dexa_missingness <- dexa_body_composition_lookup_matched %>%
  transmute(
    Subject, Group, Stage, Day,
    value_present = wbtot_mass
  )

write_missingness_heatmap(
  dexa_missingness,
  file.path(qc_dir, "dexa_body_composition_missingness_heatmap.pdf"),
  "DEXA body composition missingness"
)

dexa_summary <- tibble(
  data_source = "dexa_body_composition",
  metric = c(
    "lookup_matched_rows",
    "subjects",
    "rows_with_valid_whole_body_mass",
    "missing_group",
    "missing_day",
    "missing_scan_date"
  ),
  value = c(
    nrow(dexa_body_composition_lookup_matched),
    n_distinct(dexa_body_composition_lookup_matched$Subject),
    sum(!is.na(dexa_body_composition_lookup_matched$wbtot_mass)),
    sum(is.na(dexa_body_composition_lookup_matched$Group)),
    sum(is.na(dexa_body_composition_lookup_matched$Day)),
    sum(is.na(dexa_body_composition_lookup_matched$scan_date))
  )
)

# 3. MRI thigh muscle ----

mri_file <- file.path(
  raw_dir,
  "05 - METABOLISM",
  "02 - MRI MUSCLE",
  "RAW DATA",
  "BRACE_BSM_MRI_MUSCLE_ALL.xlsx"
)

mri_raw <- read_excel(mri_file, sheet = "LONG")
names(mri_raw) <- clean_column_names(names(mri_raw))

mri_muscle_clean <- mri_raw %>%
  transmute(
    Subject = as.character(subject_long),
    Group = standardize_group(as.character(group_long)),
    timepoint = as.character(timepoint_long),
    stage_mapping_status = "TBD: MRI PRE/POST has no confirmed Stage/Day mapping yet",
    left_anterior_thigh_fat_free_muscle_volume_l,
    left_anterior_thigh_muscle_fat_infiltration_percent,
    left_posterior_thigh_fat_free_muscle_volume_l,
    left_posterior_thigh_muscle_fat_infiltration_percent,
    right_anterior_thigh_fat_free_muscle_volume_l,
    right_anterior_thigh_muscle_fat_infiltration_percent,
    right_posterior_thigh_fat_free_muscle_volume_l,
    right_posterior_thigh_muscle_fat_infiltration_percent,
    total_thigh_fat_free_muscle_volume_l = tot_volume,
    total_thigh_muscle_fat_infiltration_percent = tot_percent
  ) %>%
  arrange(Group, Subject, timepoint)

write_csv(
  mri_muscle_clean,
  file.path(mri_output_dir, "mri_muscle_clean.csv"),
  na = ""
)

mri_missingness <- mri_muscle_clean %>%
  transmute(
    Subject, Group, timepoint,
    value_present = total_thigh_fat_free_muscle_volume_l
  )

write_timepoint_missingness_heatmap(
  mri_missingness,
  file.path(qc_dir, "mri_muscle_missingness_heatmap.pdf"),
  "MRI thigh muscle missingness"
)

mri_summary <- tibble(
  data_source = "mri_muscle",
  metric = c(
    "clean_rows",
    "subjects",
    "timepoints",
    "rows_with_valid_total_volume",
    "missing_group",
    "stage_day_mapping"
  ),
  value = c(
    as.character(nrow(mri_muscle_clean)),
    as.character(n_distinct(mri_muscle_clean$Subject)),
    as.character(n_distinct(mri_muscle_clean$timepoint)),
    as.character(sum(!is.na(mri_muscle_clean$total_thigh_fat_free_muscle_volume_l))),
    as.character(sum(is.na(mri_muscle_clean$Group))),
    "TBD"
  )
)

# 4. QC outputs ----

# TODO:
# - create missingness heatmap for daily body weight
# - create missingness heatmap for DEXA body composition
# - create missingness heatmap for MRI muscle
# - write cleaning summary to:
#   results/qc/body_weight/body_weight_cleaning_summary.csv

cleaning_summary <- bind_rows(
  daily_summary %>% mutate(value = as.character(value)),
  dexa_summary %>% mutate(value = as.character(value)),
  mri_summary
)

write_csv(
  cleaning_summary,
  file.path(qc_dir, "body_weight_cleaning_summary.csv"),
  na = ""
)

message("Wrote daily body weight lookup-matched output to: ", daily_output_dir)
message("Wrote DEXA body composition lookup-matched output to: ", dexa_output_dir)
message("Wrote MRI muscle clean output to: ", mri_output_dir)
message("Wrote body weight QC outputs to: ", qc_dir)
