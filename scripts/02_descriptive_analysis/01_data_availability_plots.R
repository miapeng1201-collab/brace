# Data availability plots
# X. Peng



# Libraries
#library(Amelia)
library(readxl)
library(tidyverse)

#output directions
availability_output_dir <- "results/02_descriptive_analysis/data_availability"
dir.create(
  availability_output_dir ,
  recursive = TRUE,
  showWarnings = FALSE
)


## Open data ##
processed_data_dir <- "data/processed_data"

lookup_clean <- readRDS(
  file.path(processed_data_dir, "lookup/lookup_table.RDS")
)

daily_bw_clean <- readRDS(
  file.path(processed_data_dir, "01body_weight/daily_body_weight/daily_body_weight_clean.RDS")
)
dexa_bw_clean<- readRDS(
  file.path(processed_data_dir, "01body_weight/dexa_body_composition/dexa_clean.RDS")
)
mri_bw_clean <- readRDS(
  file.path(processed_data_dir, "01body_weight/mri_muscle/body_weight_mri_clean.RDS")
)

bp_daily_clean <- readRDS(
  file.path(processed_data_dir, "02_blood_pressure/daily_blood_pressure_clean.RDS")
)
finapress_bp_clean <- readRDS(
  file.path(processed_data_dir, "03_finapress/finapress_blood_pressure_clean.RDS")
)
finapress_hemodynamic_clean <- readRDS(
  file.path(processed_data_dir, "03_finapress/finapress_hemodynamic_clean.RDS")
)

tewameter_clean <- readRDS(
  file.path(processed_data_dir, "04_tewameter/tewameter_clean.RDS")
)

urine_clean <- readRDS(
  file.path(processed_data_dir, "05_urine/urine_clean.RDS")
)
anthropometry_clean <- readRDS(
  file.path(processed_data_dir, "06_anthropometry/anthropometry_clean.RDS")
)
nutrition_clean <- readRDS(
  file.path(processed_data_dir, "08_intake/nutrition_clean.RDS")
)
liquid_balance_clean <- readRDS(
  file.path(processed_data_dir, "08_intake/liquid_balance_clean.RDS")
)

#function
make_daily_availability <- function(data, value_column) {
  lookup_clean |>
    select(ID, Group, StudyDay, Day) |>
    left_join(
      data |>
        select(ID, StudyDay, {{ value_column }}),
      by = c("ID", "StudyDay")
    ) |>
    mutate(
      data_status = if_else(
        is.na({{ value_column }}),
        "Missing",
        "Available"
      )
    )
}


plot_daily_availability <- function(availability_data, title) {
  ggplot(
    availability_data,
    aes(x = Day, y = ID, fill = data_status)
  ) +
    geom_tile() +
    geom_vline(
      xintercept = c(14.5, 74.5),
      color = "white",
      linewidth = 0.5
    ) +
    facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
    scale_fill_manual(
      values = c(
        Available = "#2166AC",
        Missing = "#D9D9D9"
      )
    ) +
    labs(
      title = title,
      x = "Study day",
      y = "Participant",
      fill = "Data status"
    ) +
    theme_minimal()
}


check_duplicate_days <- function(data) {
  data |>
    count(ID, StudyDay) |>
    filter(n > 1)
}

#body_weight

daily_bw_availability <- make_daily_availability(
  daily_bw_clean,
  daily_body_weight_kg
)

p_daily_bw <- plot_daily_availability(
  daily_bw_availability,
  "Daily body weight availability"
)

#dexa_weight
str(dexa_bw_clean)
dexa_bw_availability <- make_daily_availability(
  dexa_bw_clean,
  trunk_lean
)


p_dexa <- plot_daily_availability(
  dexa_bw_availability,
  "DEXA body weight availability"
)

#mri_weight
str(mri_bw_clean )

mri_availability <- make_daily_availability(
  mri_bw_clean ,
  total_thigh_fat_free_muscle_volume_l
)

p_mri <- plot_daily_availability(
  mri_availability,
  "MRI muscle availability"
)


# Daily blood pressure
str(bp_daily_clean)
bp_daily_availability <- make_daily_availability(
  bp_daily_clean,
  systolic_day_mean 
)

p_bp <- plot_daily_availability(
  bp_daily_availability,
  "Daily blood pressure availability"
)


# Urine volume
urine_availability <- make_daily_availability(
  urine_clean,
  urine_volume
)

  p_urine <- plot_daily_availability(
  urine_availability,
  "Urine volume availability"
)

# Nutrition energy intake
nutrition_availability <- make_daily_availability(
  nutrition_clean,
  energy_kcal
)

p_nutrition <- plot_daily_availability(
  nutrition_availability,
  "Nutrition energy availability"
)


# Liquid balance
liquid_balance_availability <- make_daily_availability(
  liquid_balance_clean,
  liquid_balance_g
)

p_liquid_balance <- plot_daily_availability(
  liquid_balance_availability,
  "Liquid balance availability"
)


# Finapress blood pressure
str(finapress_bp_clean)

finapress_bp_availability <- make_daily_availability(
  finapress_bp_clean,
  systolic_mean
)

p_finapress_bp <- plot_daily_availability(
  finapress_bp_availability,
  "Finapress blood pressure availability"
)

# Finapress haemodynamics
finapress_hemodynamic_availability <- make_daily_availability(
  finapress_hemodynamic_clean,
  `CO(l/min)`
)

p_finapress_hemodynamic <- plot_daily_availability(
  finapress_hemodynamic_availability,
  "Finapress cardiac output availability"
)


library(patchwork)
combine_availability_plots <- function(plot_list) {
  wrap_plots(plot_list, ncol = 3)
}


availability_plots <- list(
  p_daily_bw,
  p_dexa,
  p_mri,
  p_bp,
  p_finapress_bp,
  p_finapress_hemodynamic,
  p_urine,
  p_nutrition,
  p_liquid_balance
)

all_availability_plots <- combine_availability_plots(
  availability_plots
)


check_duplicate_days(daily_bw_clean)

check_duplicate_days(dexa_bw_clean)

check_duplicate_days(mri_bw_clean)

check_duplicate_days(bp_daily_clean)

check_duplicate_days(finapress_bp_clean)

check_duplicate_days(finapress_hemodynamic_clean)

check_duplicate_days(urine_clean)

check_duplicate_days(nutrition_clean)

check_duplicate_days(liquid_balance_clean)
