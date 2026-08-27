library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_skin_temperature_ambient_adjusted_by_phase.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_skin_temperature_ambient_adjusted_LMM_absolute_delta_HDT_only.xlsx")

stage_levels <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19",
  "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)
model_stage_levels <- stage_levels[stage_levels != "BDC"]

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

extract_lmm <- function(model, model_name) {
  anova_results <- as.data.frame(anova(model, type = 3))
  anova_results$effect <- rownames(anova_results)
  rownames(anova_results) <- NULL
  anova_results$model <- model_name
  anova_results <- anova_results[, c("model", "effect", setdiff(names(anova_results), c("model", "effect")))]

  coef_table <- as.data.frame(coef(summary(model)))
  coef_results <- data.frame(
    model = model_name,
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    se = coef_table[, "Std. Error"],
    df = if ("df" %in% names(coef_table)) coef_table[, "df"] else NA_real_,
    t = coef_table[, "t value"],
    p = coef_table[, "Pr(>|t|)"],
    row.names = NULL
  )

  vc <- as.data.frame(VarCorr(model))
  fit <- data.frame(
    model = model_name,
    n = nobs(model),
    n_subjects = length(unique(model@frame$subject)),
    subject_random_intercept_sd = vc$sdcor[vc$grp == "subject"][1],
    residual_sd = sigma(model),
    singular_fit = isSingular(model),
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    stringsAsFactors = FALSE
  )

  list(anova = anova_results, coefficients = coef_results, fit = fit)
}

extract_emmeans_pairs <- function(model, model_name) {
  emm <- emmeans(model, ~ group_cm | stage_plot)
  pairs_df <- as.data.frame(pairs(emm))
  pairs_df$model <- model_name
  pairs_df <- pairs_df[, c("model", setdiff(names(pairs_df), "model"))]
  pairs_df
}

summarize_values <- function(data, y_col) {
  out <- aggregate(
    as.formula(paste(y_col, "~ group_cm + stage_plot")),
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
  )
  out <- do.call(data.frame, out)
  names(out) <- c("group", "stage", "mean", "sd", "sem", "n")
  out
}

df <- read_excel(input_file, sheet = "Adjusted_data")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group.x), levels = c("Control", "Ex", "ExAG"))
df$group_cm <- ifelse(as.character(df$group) == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$stage_raw <- as.character(df$stage)
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$stage_plot <- factor(df$stage_plot, levels = stage_levels)
df$skin_adjusted <- suppressWarnings(as.numeric(df$avg_temperature_skin_ambient_adjusted_by_phase_c))

analysis_data <- df[complete.cases(df[, c("subject", "group_cm", "stage_plot", "skin_adjusted")]), ]
analysis_data <- analysis_data[analysis_data$stage_plot %in% stage_levels, ]

subject_stage <- aggregate(
  skin_adjusted ~ subject + group_cm + stage_plot,
  data = analysis_data,
  FUN = mean
)
subject_stage$stage_plot <- factor(subject_stage$stage_plot, levels = stage_levels)

baseline <- subject_stage[subject_stage$stage_plot == "BDC", c("subject", "skin_adjusted")]
names(baseline)[names(baseline) == "skin_adjusted"] <- "baseline_bdc_skin_adjusted"

model_source <- merge(subject_stage, baseline, by = "subject", all.x = TRUE, sort = FALSE)
model_source <- model_source[complete.cases(model_source[, c("skin_adjusted", "baseline_bdc_skin_adjusted")]), ]
model_source$delta_percent_from_BDC <- (model_source$skin_adjusted - model_source$baseline_bdc_skin_adjusted) /
  model_source$baseline_bdc_skin_adjusted * 100
model_source$stage_plot <- factor(model_source$stage_plot, levels = stage_levels)

model_data <- model_source[model_source$stage_plot %in% model_stage_levels, ]
model_data <- model_data[complete.cases(model_data[, c(
  "subject", "group_cm", "stage_plot", "skin_adjusted",
  "baseline_bdc_skin_adjusted", "delta_percent_from_BDC"
)]), ]
model_data$subject <- factor(model_data$subject)
model_data$group_cm <- factor(model_data$group_cm, levels = c("Control", "Countermeasure"))
model_data$stage_plot <- factor(as.character(model_data$stage_plot), levels = model_stage_levels)

absolute_model <- lmer(
  skin_adjusted ~ baseline_bdc_skin_adjusted + group_cm * stage_plot + (1 | subject),
  data = model_data,
  REML = FALSE
)

delta_model <- lmer(
  delta_percent_from_BDC ~ group_cm * stage_plot + (1 | subject),
  data = model_data,
  REML = FALSE
)

res_abs <- extract_lmm(absolute_model, "Two_group_HDT_only_Absolute_with_baseline")
res_delta <- extract_lmm(delta_model, "Two_group_HDT_only_Delta_no_baseline")

model_note <- data.frame(
  item = c("outcome", "subjects", "groups", "absolute_model", "delta_model", "baseline", "stages", "excluded_phase", "data_level"),
  detail = c(
    "avg_temperature_skin_ambient_adjusted_by_phase_c",
    "All available subjects retained, including P and G.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Absolute: adjusted skin temperature ~ BDC baseline + group * stage + (1 | subject).",
    "Delta: adjusted skin temperature delta percent from BDC ~ group * stage + (1 | subject).",
    "baseline_bdc_skin_adjusted = subject-level mean of adjusted BDC-12 and BDC-6.",
    "Models fitted on HDT stages only: HDT1, HDT7, HDT13, HDT19, HDT25, HDT37, HDT43, HDT49, HDT55.",
    "R+1, R+6, and R+12 excluded.",
    "Rows were averaged to subject-stage before model fitting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = rbind(res_abs$anova, res_delta$anova),
    Coefficients = rbind(res_abs$coefficients, res_delta$coefficients),
    Model_fit = rbind(res_abs$fit, res_delta$fit),
    EMM_group_pairs_by_stage = rbind(
      extract_emmeans_pairs(absolute_model, "Two_group_HDT_only_Absolute_with_baseline"),
      extract_emmeans_pairs(delta_model, "Two_group_HDT_only_Delta_no_baseline")
    ),
    Two_group_absolute_summary = summarize_values(model_source, "skin_adjusted"),
    Two_group_delta_summary = summarize_values(model_source, "delta_percent_from_BDC"),
    Model_data_HDT = model_data,
    Subject_stage_all = model_source
  ),
  output_xlsx
)

print(rbind(res_abs$anova, res_delta$anova))
print(rbind(res_abs$fit, res_delta$fit))
cat(output_xlsx, "\n")
