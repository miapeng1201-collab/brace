# Analyze plot TEWL delta and LMM no baseline

library(readxl)
library(writexl)
library(ggplot2)
library(lme4)
library(lmerTest)

tewl_dir <- "data/processed_data/04_tewameter"
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")

output_delta_plot <- file.path(tewl_dir, "TEWL_delta_percent_BDC_HDTE_HDTM_HDTL_two_groups_exclude_PG_R.png")
output_lmm_xlsx <- file.path(tewl_dir, "TEWL_absolute_LMM_two_groups_no_baseline_exclude_PG.xlsx")

phase_levels <- c("BDC", "HDT-E", "HDT-M", "HDT-L")
colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
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
df$group_cm <- factor(df$group_cm, levels = names(colors_two))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT7", "HDT13", "HDT19")] <- "HDT-E"
df$phase[df$stage_d %in% c("HDT25", "HDT31", "HDT37")] <- "HDT-M"
df$phase[df$stage_d %in% c("HDT43", "HDT49", "HDT55")] <- "HDT-L"
df$phase <- factor(df$phase, levels = phase_levels)

analysis_data <- df[complete.cases(df[, c("subject", "group_cm", "phase", "final_clean_tewl")]), ]

subject_phase <- aggregate(
  final_clean_tewl ~ subject + group_cm + phase,
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

delta_summary <- aggregate(
  delta_percent_from_BDC ~ group_cm + phase,
  data = delta_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
delta_summary <- do.call(data.frame, delta_summary)
names(delta_summary) <- c("group", "phase", "mean", "sd", "sem", "n")
delta_summary$phase <- factor(delta_summary$phase, levels = phase_levels)
delta_summary$group <- factor(delta_summary$group, levels = names(colors_two))

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "22", color = "grey55", linewidth = 0.45) +
  geom_line(
    data = delta_data,
    aes(x = phase, y = delta_percent_from_BDC, group = interaction(subject, group_cm), color = group_cm),
    alpha = 0.18,
    linewidth = 0.35
  ) +
  geom_point(
    data = delta_data,
    aes(x = phase, y = delta_percent_from_BDC, color = group_cm),
    alpha = 0.35,
    size = 1.4,
    position = position_jitter(width = 0.055, height = 0)
  ) +
  geom_line(
    data = delta_summary,
    aes(x = phase, y = mean, group = group, color = group),
    linewidth = 0.95
  ) +
  geom_point(
    data = delta_summary,
    aes(x = phase, y = mean, color = group),
    size = 2.7
  ) +
  geom_errorbar(
    data = delta_summary,
    aes(x = phase, ymin = mean - sem, ymax = mean + sem, color = group),
    width = 0.12,
    linewidth = 0.65
  ) +
  scale_color_manual(values = colors_two) +
  labs(
    x = NULL,
    y = "Delta TEWL from BDC (%)",
    color = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(face = "bold"),
    plot.margin = margin(8, 10, 8, 8)
  )
ggsave(output_delta_plot, p, width = 6.4, height = 4.4, dpi = 320, bg = "white")

model_data <- subject_phase[subject_phase$phase %in% c("HDT-E", "HDT-M", "HDT-L"), ]
model_data <- model_data[complete.cases(model_data[, c("subject", "group_cm", "phase", "final_clean_tewl")]), ]
model_data$subject <- factor(model_data$subject)
model_data$group_cm <- factor(model_data$group_cm, levels = names(colors_two))
model_data$phase <- factor(model_data$phase, levels = c("HDT-E", "HDT-M", "HDT-L"))

model <- lmer(
  final_clean_tewl ~ group_cm * phase + (1 | subject),
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

absolute_summary <- aggregate(
  final_clean_tewl ~ group_cm + phase,
  data = model_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
absolute_summary <- do.call(data.frame, absolute_summary)
names(absolute_summary) <- c("group", "phase", "mean", "sd", "sem", "n")

model_note <- data.frame(
  item = c("excluded_subjects", "groups", "delta_baseline", "lmm_model", "phase_definition", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Delta percent = (subject-phase TEWL - subject BDC TEWL) / subject BDC TEWL * 100.",
    "final_clean_tewl ~ group_cm * phase + (1 | subject); no baseline covariate; model fitted on HDT-E/HDT-M/HDT-L only.",
    "BDC = BDC-12 and BDC-6; HDT-E = HDT7, HDT13, HDT19; HDT-M = HDT25, HDT31, HDT37; HDT-L = HDT43, HDT49, HDT55.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "Each subject was averaged within phase before delta plot and LMM."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    ANOVA_type3 = anova_results,
    Coefficients = coef_results,
    Model_fit = model_fit,
    Absolute_summary = absolute_summary,
    Delta_summary = delta_summary,
    Delta_data = delta_data,
    Model_data = model_data,
    Subject_phase_all = subject_phase
  ),
  output_lmm_xlsx
)

cat(output_delta_plot, "\n")
cat(output_lmm_xlsx, "\n")
print(delta_summary)
print(anova_results)
print(coef_results)
print(model_fit)
