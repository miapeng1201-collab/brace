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

library(tidyverse)
library(readxl)


#### Helper functions ####
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


write_missingness_heatmap <- function(data, output_file, title) {
  group_order <- c("Control","Exercise", "Exercise_Gravity")
  subject_order <- data |> 
    distinct(Group, ID) |> 
    arrange(Group,ID) |> 
    pull(ID)

  plot_data <- data |> 
    mutate(  has_value = !is.na(value_present),
      Group = factor(Group, levels = group_order),
      ID = factor(ID, levels = rev(subject_order)),
      StudyDay = factor(StudyDay, levels = unique(StudyDay)))
    

  heatmap_plot <- ggplot(plot_data, aes(x = StudyDay, y = ID, fill = has_value)) +
    geom_tile(color = "white", linewidth = 0.15) +
    geom_vline(xintercept = c(14.5, 74.5), linewidth = 0.5, color = "black") +
    scale_fill_manual(
      values = c(`TRUE` = "#2166AC", `FALSE` = "#D9D9D9"),
      labels = c(`TRUE` = "Present", `FALSE` = "Missing"),
      name = "Data"
    ) +
    scale_x_continuous(
      breaks = c(0, 1, 14, 15, 44, 45, 74, 75, 89, 104, 105),
    ) +
    labs(
      title = title,
      x = "Continuous study day",
      y = "ID"
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



clean_column_names <- function(names) {
  names %>%
    str_replace_all("%", "percent") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
}





#### Output directories ####
processed_dir <- "data/processed_data/body_weight"
daily_output_dir <- file.path(processed_dir, "daily_body_weight")
dexa_output_dir <- file.path(processed_dir, "dexa_body_composition")
mri_output_dir <- file.path(processed_dir, "mri_muscle")
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

#### Lookup ####



head(lookup)
names(lookup)
colnames(lookup)
dim(lookup)
str(lookup)

subject_day_lookup$Group

subject_day_lookup <- lookup |> 
    mutate(
      Subject = str_c("BRACE_", Subject), .after = Subject,
      Group = as.factor(Group),
      Stage = as.factor(Stage),
      Subject = as.factor(Subject),
      StudyDay = make_study_day(stage_key),
      StudyDay = as.factor(StudyDay),
      Date = as.Date(Date),
      DateTime = as.POSIXct(DateTime),
      Group= case_when(
      Group = Group == "ExAG" ~ "Exercise_Gravity",
      Group == "Ex" ~ "Exercise",
      TRUE ~ Group
     )
    )

head(subject_day_lookup)
dim(subject_day_lookup)
str(subject_day_lookup)
unique(subject_day_lookup$Group)


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
    cohort, ID, StudyDay,daily_body_weight_kg,Subject, stage_key,
    Date, DateTime, measurement)

head(daily_c2_clean)
names(daily_c2_clean)
names(daily_c1_clean)
str(daily_c1_clean)
str(daily_c2)


daily_body_weight_merged <- bind_rows(daily_c1_clean, daily_c2_clean)
names(daily_body_weight_merged)

daily_body_weight_lookup_matched <- daily_body_weight_merged |> 
left_join(
    subject_day_lookup |> 
      select(ID, stage_key,Group),
    by = c("ID","stage_key")
  )

names(daily_body_weight_lookup_matched)


dim(daily_body_weight_merged)
dim(daily_body_weight_lookup_matched)

str(daily_body_weight_lookup_matched)

write_csv(
  daily_body_weight_lookup_matched,
  file.path(daily_output_dir, "daily_body_weight_lookup_matched.csv")
)

## add  value_present

daily_body_weight_lookup_matched <- daily_body_weight_lookup_matched |>
  mutate(
    value_present = !is.na(daily_body_weight_kg)
  )
str(daily_body_weight_lookup_matched)

## how many false
daily_body_weight_lookup_matched |>  count(value_present)

## which day false 
daily_body_weight_lookup_matched |>
  filter(value_present == FALSE) |>
  select(ID, StudyDay)
str(daily_body_weight_lookup_matched)

studyday_order <- c(
  paste0("Baseline_min", 15:1),
  paste0("Intervention_", 1:60),
  paste0("Recovery_", c(0:14, 29, 30))
)

plot_data <- daily_body_weight_lookup_matched |>
  mutate(
    StudyDay = factor(StudyDay, levels = studyday_order),
      Phase = case_when(
      grepl("^Baseline", StudyDay) ~ "Baseline",
      grepl("^Intervention", StudyDay) ~ "Intervention",
      grepl("^Recovery", StudyDay) ~ "Recovery"
    )
  )


## draw 
ggplot(plot_data,aes(x= StudyDay,y= ID,fill= Phase,alpha = value_present)
)+
  geom_tile()+scale_alpha_manual(
  values = c(`TRUE` = 1, `FALSE` = 0.25)
)+
  scale_x_discrete(
  breaks = c(
     "Baseline_min14", "Baseline_min7",
    "Intervention_1", "Intervention_15", "Intervention_30","Intervention_45", "Intervention_60",
    "Recovery_7", "Recovery_14", "Recovery_29"
  )
)+theme(
  axis.text.x = element_text(angle = 45, hjust = 1)
)
 +scale_fill_manual(
    values = c(
      Baseline = "#8DA0CB",
      Intervention = "#66C2A5",
      Recovery = "#FC8D62"
    )
  )

###save pdf
ggsave(file)





#### DEXA body composition ####
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
  file.path(dexa_output_dir, "body_weight_dexa_clean_lookup_matched.csv"),
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

#### MRI thigh muscle ####
names(mri_raw) <- clean_column_names(names(mri_raw))

mri_muscle_clean <- mri_raw |> 
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
  file.path(mri_output_dir, "body_weight_mri_clean.csv"),
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

#### QC outputs ####

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