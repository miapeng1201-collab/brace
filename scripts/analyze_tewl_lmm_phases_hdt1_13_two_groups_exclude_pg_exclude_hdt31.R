library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_LMM_BDC_HDTE1_13_HDTM_HDTL_two_groups_exclude_PG_exclude_HDT31.xlsx")

phase_levels <- c("BDC", "HDT-E", "HDT-M", "HDT-L")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
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

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df <- df[!(df$subject %in% c("P", "G")), ]
df$group_original <- as.character(df$group)
df$group_cm <- ifelse(df$group_original == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$ambient_temperature <- suppressWarnings(as.numeric(to_missing(df$ambient_temperature_take_c)))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13")] <- "HDT-E"
df$phase[df$stage_d %in% c("HDT25", "HDT37")] <- "HDT-M"
df$phase[df$stage_d %in% c("HDT43", "HDT49", "HDT55")] <- "HDT-L"
df$phase <- factor(df$phase, levels = phase_levels)

analysis_data <- df[complete.cases(df[, c("subject", "group_cm", "phase", "final_clean_tewl", "ambient_temperature")]), ]

subject_phase <- aggregate(
  cbind(final_clean_tewl, ambient_temperature) ~ subject + group_cm + phase,
  data = analysis_data,
  FUN = mean
)
subject_phase$phase <- factor(subject_phase$phase, levels = phase_levels)

baseline <- subject_phase[subject_phase$phase == "BDC", c("subject", "final_clean_tewl")]
names(baseline)[names(baseline) == "final_clean_tewl"] <- "baseline_bdc_tewl"

model_source <- merge(subject_phase, baseline, by = "subject", all.x = TRUE, sort = FALSE)
model_source <- model_source[complete.cases(model_source[, c("final_clean_tewl", "baseline_bdc_tewl")]), ]
model_source$delta_percent_from_BDC <- (model_source$final_clean_tewl - model_source$baseline_bdc_tewl) /
  model_source$baseline_bdc_tewl * 100

hdt_data <- model_source[model_source$phase %in% c("HDT-E", "HDT-M", "HDT-L"), ]
hdt_data <- hdt_data[complete.cases(hdt_data[, c(
  "subject", "group_cm", "phase", "final_clean_tewl", "delta_percent_from_BDC", "ambient_temperature"
)]), ]
hdt_data$subject <- factor(hdt_data$subject)
hdt_data$group_cm <- factor(hdt_data$group_cm, levels = c("Control", "Countermeasure"))
hdt_data$phase <- factor(hdt_data$phase, levels = c("HDT-E", "HDT-M", "HDT-L"))

absolute_model <- lmer(
  final_clean_tewl ~ baseline_bdc_tewl + group_cm * phase + (1 | subject),
  data = hdt_data,
  REML = FALSE
)

delta_model <- lmer(
  delta_percent_from_BDC ~ group_cm * phase + (1 | subject),
  data = hdt_data,
  REML = FALSE
)

res_abs <- extract_lmm(absolute_model, "Absolute_with_baseline")
res_delta <- extract_lmm(delta_model, "Delta_no_ambient")

extract_emmeans_pairs <- function(model, model_name) {
  emm <- emmeans(model, ~ group_cm | phase)
  pairs_df <- as.data.frame(pairs(emm))
  pairs_df$model <- model_name
  pairs_df <- pairs_df[, c("model", setdiff(names(pairs_df), "model"))]
  pairs_df
}

emm_abs_pairs <- extract_emmeans_pairs(absolute_model, "Absolute_with_baseline")
emm_delta_pairs <- extract_emmeans_pairs(delta_model, "Delta_no_ambient")

absolute_summary <- aggregate(
  final_clean_tewl ~ group_cm + phase,
  data = model_source,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
absolute_summary <- do.call(data.frame, absolute_summary)
names(absolute_summary) <- c("group", "phase", "mean", "sd", "sem", "n")

delta_summary <- aggregate(
  delta_percent_from_BDC ~ group_cm + phase,
  data = model_source,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
delta_summary <- do.call(data.frame, delta_summary)
names(delta_summary) <- c("group", "phase", "mean", "sd", "sem", "n")

model_note <- data.frame(
  item = c("excluded_subjects", "groups", "models", "delta_formula", "phase_definition", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Absolute model: final_clean_tewl ~ baseline_bdc_tewl + group_cm * phase + (1 | subject). Delta model: delta_percent_from_BDC ~ group_cm * phase + (1 | subject). Models fitted on HDT-E/HDT-M/HDT-L only. Ambient temperature is not adjusted in this version.",
    "Delta percent = (subject-phase TEWL - subject BDC TEWL) / subject BDC TEWL * 100.",
    "BDC = BDC-12 and BDC-6; HDT-E = HDT1, HDT7, HDT13; HDT-M = HDT25, HDT37; HDT-L = HDT43, HDT49, HDT55. HDT31 excluded.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "Each subject was averaged within phase before model fitting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = rbind(res_abs$anova, res_delta$anova),
    Coefficients = rbind(res_abs$coefficients, res_delta$coefficients),
    Model_fit = rbind(res_abs$fit, res_delta$fit),
    EMM_group_pairs_by_phase = rbind(emm_abs_pairs, emm_delta_pairs),
    Absolute_summary = absolute_summary,
    Delta_summary = delta_summary,
    Model_data_HDT = hdt_data,
    Subject_phase_all = model_source
  ),
  output_xlsx
)

print(rbind(res_abs$anova, res_delta$anova))
print(rbind(res_abs$fit, res_delta$fit))
cat(output_xlsx, "\n")
