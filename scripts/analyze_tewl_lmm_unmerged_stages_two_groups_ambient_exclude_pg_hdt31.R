library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_LMM_unmerged_stages_two_groups_ambient_exclude_PG_exclude_HDT31.xlsx")

stage_levels <- c("BDC-12", "BDC-6", "HDT1", "HDT7", "HDT13", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55")
hdt_levels <- c("HDT1", "HDT7", "HDT13", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

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
  emm <- emmeans(model, ~ group_cm | stage)
  pairs_df <- as.data.frame(pairs(emm))
  pairs_df$model <- model_name
  pairs_df <- pairs_df[, c("model", setdiff(names(pairs_df), "model"))]
  pairs_df
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df <- df[!(df$subject %in% c("P", "G")), ]
df$group_original <- as.character(df$group)
df$group_cm <- ifelse(df$group_original == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$stage <- as.character(df[[4]]) # Excel column D: stage
df <- df[df$stage %in% stage_levels, ]
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$ambient_temperature <- suppressWarnings(as.numeric(to_missing(df$ambient_temperature_take_c)))

analysis_data <- df[complete.cases(df[, c("subject", "group_cm", "stage", "final_clean_tewl", "ambient_temperature")]), ]

subject_stage <- aggregate(
  cbind(final_clean_tewl, ambient_temperature) ~ subject + group_cm + stage,
  data = analysis_data,
  FUN = mean
)
subject_stage$stage <- factor(subject_stage$stage, levels = stage_levels)

baseline <- subject_stage[subject_stage$stage %in% c("BDC-12", "BDC-6"), ]
baseline <- aggregate(
  baseline_bdc_tewl ~ subject,
  data = transform(baseline, baseline_bdc_tewl = final_clean_tewl),
  FUN = mean
)

model_source <- merge(subject_stage, baseline, by = "subject", all.x = TRUE, sort = FALSE)
model_source <- model_source[complete.cases(model_source[, c("final_clean_tewl", "baseline_bdc_tewl")]), ]
model_source$delta_percent_from_BDC <- (model_source$final_clean_tewl - model_source$baseline_bdc_tewl) /
  model_source$baseline_bdc_tewl * 100

hdt_data <- model_source[model_source$stage %in% hdt_levels, ]
hdt_data <- hdt_data[complete.cases(hdt_data[, c(
  "subject", "group_cm", "stage", "final_clean_tewl", "baseline_bdc_tewl", "delta_percent_from_BDC", "ambient_temperature"
)]), ]
hdt_data$subject <- factor(hdt_data$subject)
hdt_data$group_cm <- factor(hdt_data$group_cm, levels = c("Control", "Countermeasure"))
hdt_data$stage <- factor(as.character(hdt_data$stage), levels = hdt_levels)

absolute_model <- lmer(
  final_clean_tewl ~ baseline_bdc_tewl + ambient_temperature + group_cm * stage + (1 | subject),
  data = hdt_data,
  REML = FALSE
)

delta_model <- lmer(
  delta_percent_from_BDC ~ ambient_temperature + group_cm * stage + (1 | subject),
  data = hdt_data,
  REML = FALSE
)

res_abs <- extract_lmm(absolute_model, "Absolute_with_baseline_ambient_unmerged")
res_delta <- extract_lmm(delta_model, "Delta_ambient_unmerged")
emm_abs_pairs <- extract_emmeans_pairs(absolute_model, "Absolute_with_baseline_ambient_unmerged")
emm_delta_pairs <- extract_emmeans_pairs(delta_model, "Delta_ambient_unmerged")

absolute_summary <- aggregate(
  final_clean_tewl ~ group_cm + stage,
  data = model_source,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
absolute_summary <- do.call(data.frame, absolute_summary)
names(absolute_summary) <- c("group", "stage", "mean", "sd", "sem", "n")

delta_summary <- aggregate(
  delta_percent_from_BDC ~ group_cm + stage,
  data = model_source,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
delta_summary <- do.call(data.frame, delta_summary)
names(delta_summary) <- c("group", "stage", "mean", "sd", "sem", "n")

ambient_summary <- aggregate(
  ambient_temperature ~ group_cm + stage,
  data = model_source,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
ambient_summary <- do.call(data.frame, ambient_summary)
names(ambient_summary) <- c("group", "stage", "mean", "sd", "sem", "n")

model_note <- data.frame(
  item = c("excluded_subjects", "excluded_stage", "groups", "models", "baseline", "ambient_covariate", "delta_formula", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "HDT31 excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Absolute model: final_clean_tewl ~ baseline_bdc_tewl + ambient_temperature + group_cm * stage + (1 | subject). Delta model: delta_percent_from_BDC ~ ambient_temperature + group_cm * stage + (1 | subject). Models fitted on unmerged HDT stages only.",
    "baseline_bdc_tewl = subject-level mean of BDC-12 and BDC-6.",
    "ambient_temperature = subject-stage mean of ambient_temperature_take_c.",
    "Delta percent = (subject-stage TEWL - subject BDC TEWL) / subject BDC TEWL * 100.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "If duplicate rows existed for a subject-stage, they were averaged before model fitting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = rbind(res_abs$anova, res_delta$anova),
    Coefficients = rbind(res_abs$coefficients, res_delta$coefficients),
    Model_fit = rbind(res_abs$fit, res_delta$fit),
    EMM_group_pairs_by_stage = rbind(emm_abs_pairs, emm_delta_pairs),
    Absolute_summary = absolute_summary,
    Delta_summary = delta_summary,
    Ambient_summary = ambient_summary,
    Model_data_HDT = hdt_data,
    Subject_stage_all = model_source
  ),
  output_xlsx
)

print(rbind(res_abs$anova, res_delta$anova))
print(rbind(emm_abs_pairs, emm_delta_pairs))
print(rbind(res_abs$fit, res_delta$fit))
cat(output_xlsx, "\n")
