library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_absolute_LMM_two_groups_baseline_ambient_exclude_PG.xlsx")

phase_levels <- c("BDC", "HDT-E", "HDT-M", "HDT-L")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

term_row <- function(coef_table, term_name) {
  if (!term_name %in% rownames(coef_table)) {
    return(data.frame(
      term = term_name,
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      t = NA_real_,
      p = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    term = term_name,
    estimate = coef_table[term_name, "Estimate"],
    se = coef_table[term_name, "Std. Error"],
    df = if ("df" %in% names(coef_table)) coef_table[term_name, "df"] else NA_real_,
    t = coef_table[term_name, "t value"],
    p = coef_table[term_name, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
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

model_data <- merge(subject_phase, baseline, by = "subject", all.x = TRUE, sort = FALSE)
model_data <- model_data[model_data$phase %in% c("HDT-E", "HDT-M", "HDT-L"), ]
model_data <- model_data[complete.cases(model_data[, c(
  "subject", "group_cm", "phase", "final_clean_tewl", "baseline_bdc_tewl", "ambient_temperature"
)]), ]
model_data$subject <- factor(model_data$subject)
model_data$group_cm <- factor(model_data$group_cm, levels = c("Control", "Countermeasure"))
model_data$phase <- factor(model_data$phase, levels = c("HDT-E", "HDT-M", "HDT-L"))

model <- lmer(
  final_clean_tewl ~ baseline_bdc_tewl + ambient_temperature + group_cm * phase + (1 | subject),
  data = model_data,
  REML = FALSE
)

coef_table <- as.data.frame(coef(summary(model)))
coef_results <- do.call(rbind, lapply(rownames(coef_table), function(term_name) {
  term_row(coef_table, term_name)
}))

anova_results <- as.data.frame(anova(model, type = 3))
anova_results$effect <- rownames(anova_results)
rownames(anova_results) <- NULL
anova_results <- anova_results[, c("effect", setdiff(names(anova_results), "effect"))]

vc <- as.data.frame(VarCorr(model))
model_fit <- data.frame(
  n = nrow(model_data),
  n_subjects = length(unique(model_data$subject)),
  subject_random_intercept_sd = vc$sdcor[vc$grp == "subject"][1],
  residual_sd = sigma(model),
  singular_fit = isSingular(model),
  AIC = AIC(model),
  BIC = BIC(model),
  logLik = as.numeric(logLik(model)),
  stringsAsFactors = FALSE
)

group_phase_summary <- aggregate(
  cbind(final_clean_tewl, ambient_temperature) ~ group_cm + phase,
  data = model_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sd(x) / sqrt(length(x)), n = length(x))
)
group_phase_summary <- do.call(data.frame, group_phase_summary)

baseline_summary <- aggregate(
  baseline_bdc_tewl ~ group_cm,
  data = unique(model_data[, c("subject", "group_cm", "baseline_bdc_tewl")]),
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sd(x) / sqrt(length(x)), n = length(x))
)
baseline_summary <- do.call(data.frame, baseline_summary)

model_note <- data.frame(
  item = c("excluded_subjects", "groups", "outcome", "baseline", "ambient_covariate", "model", "phase_definition", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "final_clean_tewl absolute value in each HDT phase.",
    "baseline_bdc_tewl = subject-level mean of BDC-12 and BDC-6, one value per subject.",
    "ambient_temperature = subject-phase mean of ambient_temperature_take_c.",
    "final_clean_tewl ~ baseline_bdc_tewl + ambient_temperature + group_cm * phase + (1 | subject); model fitted on HDT-E/HDT-M/HDT-L only.",
    "HDT-E = HDT7, HDT13, HDT19; HDT-M = HDT25, HDT31, HDT37; HDT-L = HDT43, HDT49, HDT55.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "Each subject was averaged within phase before model fitting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = anova_results,
    Coefficients = coef_results,
    Model_fit = model_fit,
    Group_phase_summary = group_phase_summary,
    Baseline_summary = baseline_summary,
    Model_data = model_data,
    Subject_phase_all = subject_phase
  ),
  output_xlsx
)

print(anova_results)
print(coef_results)
print(model_fit)
cat(output_xlsx, "\n")
