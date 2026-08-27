library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_delta_percent_LMM_two_groups_with_without_ambient_exclude_PG.xlsx")

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
df$phase[df$stage_d %in% c("HDT7", "HDT13", "HDT19")] <- "HDT-E"
df$phase[df$stage_d %in% c("HDT25", "HDT31", "HDT37")] <- "HDT-M"
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

delta_data <- merge(subject_phase, baseline, by = "subject", all.x = TRUE, sort = FALSE)
delta_data <- delta_data[complete.cases(delta_data[, c("final_clean_tewl", "baseline_bdc_tewl")]), ]
delta_data$delta_percent_from_BDC <- (delta_data$final_clean_tewl - delta_data$baseline_bdc_tewl) /
  delta_data$baseline_bdc_tewl * 100

model_data <- delta_data[delta_data$phase %in% c("HDT-E", "HDT-M", "HDT-L"), ]
model_data <- model_data[complete.cases(model_data[, c(
  "subject", "group_cm", "phase", "delta_percent_from_BDC", "ambient_temperature"
)]), ]
model_data$subject <- factor(model_data$subject)
model_data$group_cm <- factor(model_data$group_cm, levels = c("Control", "Countermeasure"))
model_data$phase <- factor(model_data$phase, levels = c("HDT-E", "HDT-M", "HDT-L"))

model_no_ambient <- lmer(
  delta_percent_from_BDC ~ group_cm * phase + (1 | subject),
  data = model_data,
  REML = FALSE
)

model_with_ambient <- lmer(
  delta_percent_from_BDC ~ ambient_temperature + group_cm * phase + (1 | subject),
  data = model_data,
  REML = FALSE
)

res_no <- extract_lmm(model_no_ambient, "Delta_LMM_no_ambient")
res_amb <- extract_lmm(model_with_ambient, "Delta_LMM_with_ambient")

delta_summary <- aggregate(
  delta_percent_from_BDC ~ group_cm + phase,
  data = delta_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
delta_summary <- do.call(data.frame, delta_summary)
names(delta_summary) <- c("group", "phase", "mean", "sd", "sem", "n")

ambient_summary <- aggregate(
  ambient_temperature ~ group_cm + phase,
  data = delta_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
ambient_summary <- do.call(data.frame, ambient_summary)
names(ambient_summary) <- c("group", "phase", "mean", "sd", "sem", "n")

model_note <- data.frame(
  item = c("excluded_subjects", "groups", "outcome", "models", "ambient_covariate", "phase_definition", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "delta_percent_from_BDC = (subject-phase TEWL - subject BDC TEWL) / subject BDC TEWL * 100.",
    "No ambient: delta_percent_from_BDC ~ group_cm * phase + (1 | subject). With ambient: delta_percent_from_BDC ~ ambient_temperature + group_cm * phase + (1 | subject). Models fitted on HDT-E/HDT-M/HDT-L only.",
    "ambient_temperature = subject-phase mean of ambient_temperature_take_c.",
    "BDC = BDC-12 and BDC-6; HDT-E = HDT7, HDT13, HDT19; HDT-M = HDT25, HDT31, HDT37; HDT-L = HDT43, HDT49, HDT55.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "Each subject was averaged within phase before delta calculation and model fitting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = rbind(res_no$anova, res_amb$anova),
    Coefficients = rbind(res_no$coefficients, res_amb$coefficients),
    Model_fit = rbind(res_no$fit, res_amb$fit),
    Delta_summary = delta_summary,
    Ambient_summary = ambient_summary,
    Model_data = model_data,
    Delta_data_all_phases = delta_data,
    Subject_phase_all = subject_phase
  ),
  output_xlsx
)

print(rbind(res_no$anova, res_amb$anova))
print(rbind(res_no$coefficients, res_amb$coefficients))
print(rbind(res_no$fit, res_amb$fit))
cat(output_xlsx, "\n")
