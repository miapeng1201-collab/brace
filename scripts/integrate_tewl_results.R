library(readxl)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")

main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
lmm_after_file <- file.path(tewl_dir, "TEWL_LMM_temperature_after_HDT19.xlsx")
lmm_phase_file <- file.path(tewl_dir, "TEWL_LMM_temperature_by_phase.xlsx")
ambient_only_file <- file.path(tewl_dir, "TEWL_LMM_ambient_temperature_only.xlsx")
bdc_all_groups_file <- file.path(tewl_dir, "TEWL_LMM_BDC_all_groups_ambient_only.xlsx")
skin_ambient_file <- file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation.xlsx")
ambient_adjusted_file <- file.path(tewl_dir, "TEWL_ambient_temperature_adjusted_values.xlsx")
environment_summary_file <- file.path(tewl_dir, "TEWL_test_day_environment_summary.xlsx")
unadjusted_group_diff_file <- file.path(tewl_dir, "TEWL_unadjusted_group_differences.xlsx")
countermeasure_file <- file.path(tewl_dir, "TEWL_countermeasure_group_results.xlsx")
out_file <- file.path(tewl_dir, "TEWL_integrated_results.xlsx")

stage_levels <- c(
  "BDC-12", "BDC-6",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)

as_missing <- function(x) {
  if (is.numeric(x)) {
    return(is.na(x))
  }
  is.na(x) | trimws(as.character(x)) == "" | toupper(trimws(as.character(x))) %in% c("NA", "N/A")
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop("Missing expected column: ", paste(candidates, collapse = " / "))
  }
  hit[[1]]
}

clean_data <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(clean_data) <- trimws(names(clean_data))

subject_col <- pick_col(clean_data, c("subject", "Subject", "participant_id"))
group_col <- pick_col(clean_data, c("group", "Group"))
stage_col <- pick_col(clean_data, c("stage", "Stage"))
day_col <- pick_col(clean_data, c("day", "Day"))
final_col <- pick_col(clean_data, c("final_clean_tewl"))
raw_col <- pick_col(clean_data, c("raw_window_tewl_mean_of_measurements_g_m2_h"))
ambient_col <- pick_col(clean_data, c("ambient_temperature_take_c"))
skin_col <- pick_col(clean_data, c("avg_temperature_skin_robust_c"))

clean_data[[stage_col]] <- as.character(clean_data[[stage_col]])
clean_data[[group_col]] <- as.character(clean_data[[group_col]])
clean_data[[subject_col]] <- as.character(clean_data[[subject_col]])

lmm_after <- read_excel(lmm_after_file, sheet = "LMM_results")
lmm_after$analysis_window <- "HDT_after_HDT19"
lmm_after$phase <- "HDT25_to_HDT55"

lmm_phase <- read_excel(lmm_phase_file, sheet = "LMM_results")
lmm_phase$analysis_window <- lmm_phase$phase

common_cols <- union(names(lmm_after), names(lmm_phase))
pad_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  for (col in missing) df[[col]] <- NA
  df[, cols]
}
lmm_all <- rbind(
  pad_cols(lmm_after, common_cols),
  pad_cols(lmm_phase, common_cols)
)

key_cols <- c(
  "analysis_window", "phase", "group", "n", "n_subjects", "stages",
  "ambient_beta", "ambient_se", "ambient_df", "ambient_t",
  "ambient_p_lmerTest", "ambient_p_lrt",
  "skin_beta", "skin_p_lmerTest",
  "subject_random_intercept_sd", "residual_sd", "singular_fit", "note"
)
key_cols <- key_cols[key_cols %in% names(lmm_all)]
lmm_key_results <- lmm_all[, key_cols]

ambient_only_results <- if (file.exists(ambient_only_file)) {
  read_excel(ambient_only_file, sheet = "LMM_results")
} else {
  data.frame(note = "TEWL_LMM_ambient_temperature_only.xlsx not found")
}

bdc_all_groups <- if (file.exists(bdc_all_groups_file)) {
  read_excel(bdc_all_groups_file, sheet = "Summary")
} else {
  data.frame(note = "TEWL_LMM_BDC_all_groups_ambient_only.xlsx not found")
}

skin_ambient_pearson <- if (file.exists(skin_ambient_file)) {
  read_excel(skin_ambient_file, sheet = "Pearson_correlations")
} else {
  data.frame(note = "TEWL_skin_ambient_temperature_correlation.xlsx not found")
}

skin_ambient_mixed <- if (file.exists(skin_ambient_file)) {
  read_excel(skin_ambient_file, sheet = "Mixed_model")
} else {
  data.frame(note = "TEWL_skin_ambient_temperature_correlation.xlsx not found")
}

ambient_adjustment_models <- if (file.exists(ambient_adjusted_file)) {
  read_excel(ambient_adjusted_file, sheet = "Ambient_adjustment_models")
} else {
  data.frame(note = "TEWL_ambient_temperature_adjusted_values.xlsx not found")
}

tewl_ambient_adjusted <- if (file.exists(ambient_adjusted_file)) {
  read_excel(ambient_adjusted_file, sheet = "TEWL_ambient_adjusted")
} else {
  data.frame(note = "TEWL_ambient_temperature_adjusted_values.xlsx not found")
}

skin_sensitivity_models <- if (file.exists(ambient_adjusted_file)) {
  read_excel(ambient_adjusted_file, sheet = "Skin_sensitivity_models")
} else {
  data.frame(note = "TEWL_ambient_temperature_adjusted_values.xlsx not found")
}

environment_by_test_day <- if (file.exists(environment_summary_file)) {
  read_excel(environment_summary_file, sheet = "By_subject_test_day")
} else {
  data.frame(note = "TEWL_test_day_environment_summary.xlsx not found")
}

environment_by_subject <- if (file.exists(environment_summary_file)) {
  read_excel(environment_summary_file, sheet = "By_subject_overall")
} else {
  data.frame(note = "TEWL_test_day_environment_summary.xlsx not found")
}

environment_by_group_stage <- if (file.exists(environment_summary_file)) {
  read_excel(environment_summary_file, sheet = "By_group_stage")
} else {
  data.frame(note = "TEWL_test_day_environment_summary.xlsx not found")
}

unadjusted_group_tests <- if (file.exists(unadjusted_group_diff_file)) {
  read_excel(unadjusted_group_diff_file, sheet = "Model_tests")
} else {
  data.frame(note = "TEWL_unadjusted_group_differences.xlsx not found")
}

unadjusted_delta_groups <- if (file.exists(unadjusted_group_diff_file)) {
  read_excel(unadjusted_group_diff_file, sheet = "Delta_group_emmeans")
} else {
  data.frame(note = "TEWL_unadjusted_group_differences.xlsx not found")
}

unadjusted_delta_pairs <- if (file.exists(unadjusted_group_diff_file)) {
  read_excel(unadjusted_group_diff_file, sheet = "Delta_pairwise_groups")
} else {
  data.frame(note = "TEWL_unadjusted_group_differences.xlsx not found")
}

countermeasure_group_tests <- if (file.exists(countermeasure_file)) {
  read_excel(countermeasure_file, sheet = "Group_tests")
} else {
  data.frame(note = "TEWL_countermeasure_group_results.xlsx not found")
}

countermeasure_delta_groups <- if (file.exists(countermeasure_file)) {
  read_excel(countermeasure_file, sheet = "Delta_group_emmeans")
} else {
  data.frame(note = "TEWL_countermeasure_group_results.xlsx not found")
}

countermeasure_delta_pairwise <- if (file.exists(countermeasure_file)) {
  read_excel(countermeasure_file, sheet = "Delta_pairwise")
} else {
  data.frame(note = "TEWL_countermeasure_group_results.xlsx not found")
}

countermeasure_adjustment_models <- if (file.exists(countermeasure_file)) {
  read_excel(countermeasure_file, sheet = "Ambient_adjustment_models")
} else {
  data.frame(note = "TEWL_countermeasure_group_results.xlsx not found")
}

stage_day_check <- unique(clean_data[, c(stage_col, day_col)])
names(stage_day_check) <- c("stage", "day")
stage_day_check$stage_order <- match(stage_day_check$stage, stage_levels)
stage_day_check <- stage_day_check[order(stage_day_check$stage_order, stage_day_check$day), ]

summary_rows <- list()
row_i <- 1
for (grp in sort(unique(clean_data[[group_col]]))) {
  for (stg in stage_levels[stage_levels %in% unique(clean_data[[stage_col]])]) {
    subset_idx <- clean_data[[group_col]] == grp & clean_data[[stage_col]] == stg
    if (!any(subset_idx)) next
    sub <- clean_data[subset_idx, ]
    final_missing <- as_missing(sub[[final_col]])
    raw_missing <- as_missing(sub[[raw_col]])
    score_col <- if ("Rik_score" %in% names(sub)) "Rik_score" else NA
    score_1 <- if (!is.na(score_col)) sum(sub[[score_col]] == 1, na.rm = TRUE) else NA_integer_
    score_2 <- if (!is.na(score_col)) sum(sub[[score_col]] == 2, na.rm = TRUE) else NA_integer_
    summary_rows[[row_i]] <- data.frame(
      group = grp,
      stage = stg,
      n_records = nrow(sub),
      final_clean_missing_n = sum(final_missing),
      final_clean_present_n = sum(!final_missing),
      raw_window_missing_n = sum(raw_missing),
      raw_window_present_n = sum(!raw_missing),
      rik_score_1_n = score_1,
      rik_score_2_n = score_2,
      stringsAsFactors = FALSE
    )
    row_i <- row_i + 1
  }
}
missing_summary <- do.call(rbind, summary_rows)

subjects <- sort(unique(clean_data[[subject_col]]))
baseline_rows <- vector("list", length(subjects))
for (i in seq_along(subjects)) {
  sid <- subjects[[i]]
  sub <- clean_data[clean_data[[subject_col]] == sid & clean_data[[stage_col]] %in% c("BDC-12", "BDC-6"), ]
  final_vals <- suppressWarnings(as.numeric(sub[[final_col]]))
  raw_vals <- suppressWarnings(as.numeric(sub[[raw_col]]))
  final_vals <- final_vals[!is.na(final_vals)]
  raw_vals <- raw_vals[!is.na(raw_vals)]
  use_raw_fallback <- length(final_vals) == 0 && length(raw_vals) > 0
  baseline <- if (length(final_vals) > 0) mean(final_vals) else if (use_raw_fallback) mean(raw_vals) else NA_real_
  baseline_rows[[i]] <- data.frame(
    subject = sid,
    group = unique(sub[[group_col]])[[1]],
    bdc_baseline_tewl = baseline,
    baseline_source = ifelse(use_raw_fallback, "raw_window_BDC_fallback", "final_clean_BDC"),
    n_final_bdc_values = length(final_vals),
    n_raw_bdc_values = length(raw_vals),
    stringsAsFactors = FALSE
  )
}
bdc_baseline <- do.call(rbind, baseline_rows)

percent_change <- merge(
  clean_data,
  bdc_baseline[, c("subject", "bdc_baseline_tewl", "baseline_source")],
  by.x = subject_col,
  by.y = "subject",
  all.x = TRUE,
  sort = FALSE
)
percent_change$percent_change_from_BDC <- (
  (suppressWarnings(as.numeric(percent_change[[final_col]])) - percent_change$bdc_baseline_tewl) /
    percent_change$bdc_baseline_tewl
) * 100
percent_change <- percent_change[, c(
  subject_col, group_col, stage_col, day_col, final_col,
  "bdc_baseline_tewl", "baseline_source", "percent_change_from_BDC"
)]
names(percent_change)[1:5] <- c("subject", "group", "stage", "day", "final_clean_tewl")

figures <- data.frame(
  result = c(
    "Raw-window missing heatmap",
    "Final-clean subject trend plot",
    "Final-clean percent change from BDC plot",
    "Final-clean percent change from BDC by group plot"
    , "Ambient-temperature adjusted TEWL by group plot"
    , "Ambient-temperature adjusted TEWL delta percent from BDC plot"
    , "Countermeasure final-clean TEWL delta percent plot"
    , "Countermeasure ambient-adjusted TEWL value plot"
    , "Countermeasure ambient-adjusted TEWL delta percent plot"
    , "Skin vs ambient temperature correlation plot"
    , "Test-day ambient temperature by subject plot"
    , "Test-day ambient humidity by subject plot"
  ),
  file_path = c(
    file.path(tewl_dir, "TEWL_raw_window_mean_missing_heatmap_R.png"),
    file.path(tewl_dir, "TEWL_final_clean_subject_trends_R.png"),
    file.path(tewl_dir, "TEWL_final_percent_change_from_BDC_R.png"),
    file.path(tewl_dir, "TEWL_final_percent_change_from_BDC_by_group_R.png"),
    file.path(tewl_dir, "TEWL_ambient_adjusted_values_by_group_R.png"),
    file.path(tewl_dir, "TEWL_ambient_adjusted_delta_percent_from_BDC_by_group_R.png"),
    file.path(tewl_dir, "TEWL_countermeasure_final_delta_percent_from_BDC_R.png"),
    file.path(tewl_dir, "TEWL_countermeasure_ambient_adjusted_values_R.png"),
    file.path(tewl_dir, "TEWL_countermeasure_ambient_adjusted_delta_percent_from_BDC_R.png"),
    file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation_R.png"),
    file.path(tewl_dir, "TEWL_test_day_ambient_temperature_by_subject_R.png"),
    file.path(tewl_dir, "TEWL_test_day_ambient_humidity_by_subject_R.png")
  ),
  note = c(
    "Missing reason labeled as no record; score 1/2 marked by color.",
    "Each subject trajectory by group using final_clean_tewl.",
    "BDC baseline is mean of BDC-12 and BDC-6; raw-window BDC fallback used where final BDC is unavailable.",
    "Three groups in one panel; thin lines are subjects, thick lines are group means with SE.",
    "Ambient-temperature adjusted TEWL absolute values; no R stage shown.",
    "Ambient-temperature adjusted TEWL percent change from adjusted BDC baseline; no R stage shown.",
    "Ex and ExAG merged as Countermeasure; final_clean_tewl percent change from BDC; no R stage shown.",
    "Ex and ExAG merged as Countermeasure; ambient-adjusted TEWL values; no R stage shown.",
    "Ex and ExAG merged as Countermeasure; ambient-adjusted percent change from adjusted BDC; no R stage shown.",
    "Scatter and linear trends for skin temperature versus ambient temperature by phase and group.",
    "Each subject's test-day ambient temperature across stages, grouped by intervention group.",
    "Each subject's test-day ambient relative humidity across stages, grouped by intervention group."
  ),
  stringsAsFactors = FALSE
)

overview <- data.frame(
  item = c(
    "Main cleaned data",
    "Final TEWL value",
    "BDC baseline rule",
  "LMM model",
  "Ambient-only LMM model",
  "BDC all-groups ambient-only model",
  "Ambient-adjusted TEWL values",
  "Test-day environment summary",
  "Unadjusted TEWL group comparison",
  "LMM windows",
    "Stage source",
    "Workbook created"
  ),
  detail = c(
    main_file,
    "final_clean_tewl = Rik data cleaning when available; otherwise raw_window_tewl_mean_of_measurements_g_m2_h. Text NA/N/A treated as blank.",
    "Mean of BDC-12 and BDC-6 final_clean_tewl; if final BDC unavailable, use raw-window BDC values as fallback.",
  "final_clean_tewl ~ ambient_temperature_take_c + avg_temperature_skin_robust_c + (1 | subject), fitted separately by group/window.",
  "final_clean_tewl ~ ambient_temperature_take_c + (1 | subject), fitted separately by group/window.",
  "BDC only: final_clean_tewl ~ ambient_temperature_take_c + group + (1 | subject); interaction with group also checked.",
  "TEWL corrected to phase/model-group mean ambient temperature. BDC uses all groups together; other phases use group-specific models. Skin temperature included only in sensitivity models.",
  "Subject test-day temperature and relative humidity summarized from ambient_temperature_take_c and ambient_relative_humidity_take_pct.",
  "HDT stages only, no R: final_clean_tewl and BDC delta percent compared across groups using mixed models with subject random intercept.",
  "HDT_after_HDT19, BDC, HDT_before_HDT25, R",
    "D-column stage in RawWindowSummaryClean; real stage ignored.",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ),
  stringsAsFactors = FALSE
)

out_sheets <- list(
  Overview = overview,
  LMM_key_results = lmm_key_results,
  LMM_ambient_only = ambient_only_results,
  LMM_BDC_all_groups = bdc_all_groups,
  Skin_Ambient_Pearson = skin_ambient_pearson,
  Skin_Ambient_Mixed = skin_ambient_mixed,
  Ambient_Adjustment_Models = ambient_adjustment_models,
  TEWL_Ambient_Adjusted = tewl_ambient_adjusted,
  Skin_Sensitivity_Models = skin_sensitivity_models,
  Environment_Test_Day = environment_by_test_day,
  Environment_By_Subject = environment_by_subject,
  Environment_Group_Stage = environment_by_group_stage,
  Unadjusted_Group_Tests = unadjusted_group_tests,
  Unadjusted_Delta_Groups = unadjusted_delta_groups,
  Unadjusted_Delta_Pairs = unadjusted_delta_pairs,
  Countermeasure_Group_Tests = countermeasure_group_tests,
  Countermeasure_Delta_Groups = countermeasure_delta_groups,
  Countermeasure_Delta_Pairwise = countermeasure_delta_pairwise,
  Countermeasure_Adjust_Models = countermeasure_adjustment_models,
  Missing_summary = missing_summary,
  BDC_baseline = bdc_baseline,
  Percent_change_from_BDC = percent_change,
  Stage_day_check = stage_day_check,
  Figures = figures,
  Clean_TEWL_data = clean_data
)

write_xlsx(out_sheets, out_file)
cat(out_file, "\n")
