library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_unadjusted_group_differences.xlsx")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)
hdt_order <- stage_order[stage_order != "BDC"]

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
tewl_col <- pick_col(df, c("final_clean_tewl"))
raw_col <- pick_col(df, c("raw_window_tewl_mean_of_measurements_g_m2_h"))

df$subject <- as.character(df[[subject_col]])
df$group <- factor(as.character(df[[group_col]]), levels = c("Control", "Ex", "ExAG"))
df$stage_raw <- as.character(df[[stage_col]])
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df[[tewl_col]])))
df$raw_window_tewl_mean_of_measurements_g_m2_h <- suppressWarnings(as.numeric(to_missing(df[[raw_col]])))

plot_source <- df[df$stage_plot %in% stage_order, ]

absolute_values <- aggregate(
  final_clean_tewl ~ group + subject + stage_plot,
  data = plot_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline_source <- df[df$stage_raw %in% c("BDC-12", "BDC-6"), ]
baseline_source$baseline_value <- ifelse(
  is.na(baseline_source$final_clean_tewl),
  baseline_source$raw_window_tewl_mean_of_measurements_g_m2_h,
  baseline_source$final_clean_tewl
)
baseline <- aggregate(
  baseline_value ~ group + subject,
  data = baseline_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)
names(baseline)[names(baseline) == "baseline_value"] <- "BDC"

delta_values <- merge(absolute_values, baseline, by = c("group", "subject"), all.x = TRUE)
delta_values$delta_percent_from_BDC <- (delta_values$final_clean_tewl - delta_values$BDC) / delta_values$BDC * 100
delta_values$stage_plot <- factor(delta_values$stage_plot, levels = stage_order)

absolute_values$stage_plot <- factor(absolute_values$stage_plot, levels = stage_order)
absolute_hdt <- absolute_values[absolute_values$stage_plot %in% hdt_order & !is.na(absolute_values$final_clean_tewl), ]
absolute_hdt$subject <- factor(absolute_hdt$subject)
absolute_hdt$group <- droplevels(absolute_hdt$group)
absolute_hdt$stage_plot <- droplevels(absolute_hdt$stage_plot)

delta_hdt <- delta_values[delta_values$stage_plot %in% hdt_order & !is.na(delta_values$delta_percent_from_BDC), ]
delta_hdt$subject <- factor(delta_hdt$subject)
delta_hdt$group <- droplevels(delta_hdt$group)
delta_hdt$stage_plot <- droplevels(delta_hdt$stage_plot)

fit_absolute <- lmer(final_clean_tewl ~ group * stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)
fit_absolute_additive <- lmer(final_clean_tewl ~ group + stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)
fit_absolute_no_group <- lmer(final_clean_tewl ~ stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)
fit_absolute_no_interaction <- fit_absolute_additive

fit_delta <- lmer(delta_percent_from_BDC ~ group * stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_delta_additive <- lmer(delta_percent_from_BDC ~ group + stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_delta_no_group <- lmer(delta_percent_from_BDC ~ stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_delta_no_interaction <- fit_delta_additive

model_tests <- rbind(
  data.frame(
    outcome = "final_clean_tewl_absolute_HDT",
    test = c("group_main_effect_additive_LRT", "group_by_stage_interaction_LRT"),
    chi_sq = c(
      anova(fit_absolute_no_group, fit_absolute_additive)$Chisq[2],
      anova(fit_absolute_no_interaction, fit_absolute)$Chisq[2]
    ),
    df = c(
      anova(fit_absolute_no_group, fit_absolute_additive)$Df[2],
      anova(fit_absolute_no_interaction, fit_absolute)$Df[2]
    ),
    p = c(
      anova(fit_absolute_no_group, fit_absolute_additive)[["Pr(>Chisq)"]][2],
      anova(fit_absolute_no_interaction, fit_absolute)[["Pr(>Chisq)"]][2]
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    outcome = "delta_percent_from_BDC_HDT",
    test = c("group_main_effect_additive_LRT", "group_by_stage_interaction_LRT"),
    chi_sq = c(
      anova(fit_delta_no_group, fit_delta_additive)$Chisq[2],
      anova(fit_delta_no_interaction, fit_delta)$Chisq[2]
    ),
    df = c(
      anova(fit_delta_no_group, fit_delta_additive)$Df[2],
      anova(fit_delta_no_interaction, fit_delta)$Df[2]
    ),
    p = c(
      anova(fit_delta_no_group, fit_delta_additive)[["Pr(>Chisq)"]][2],
      anova(fit_delta_no_interaction, fit_delta)[["Pr(>Chisq)"]][2]
    ),
    stringsAsFactors = FALSE
  )
)

absolute_emm_group <- as.data.frame(emmeans(fit_absolute_additive, ~ group))
absolute_pairs_group <- as.data.frame(pairs(emmeans(fit_absolute_additive, ~ group), adjust = "tukey"))
delta_emm_group <- as.data.frame(emmeans(fit_delta_additive, ~ group))
delta_pairs_group <- as.data.frame(pairs(emmeans(fit_delta_additive, ~ group), adjust = "tukey"))

delta_stage_group_means <- aggregate(
  delta_percent_from_BDC ~ group + stage_plot,
  data = delta_hdt,
  FUN = function(x) c(mean = mean(x), sd = sd(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
delta_stage_group_means <- do.call(data.frame, delta_stage_group_means)
names(delta_stage_group_means) <- c("group", "stage", "mean_delta_percent", "sd", "se", "n")

absolute_stage_group_means <- aggregate(
  final_clean_tewl ~ group + stage_plot,
  data = absolute_hdt,
  FUN = function(x) c(mean = mean(x), sd = sd(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
absolute_stage_group_means <- do.call(data.frame, absolute_stage_group_means)
names(absolute_stage_group_means) <- c("group", "stage", "mean_final_clean_tewl", "sd", "se", "n")

model_note <- data.frame(
  item = c("data", "absolute_model", "delta_percent_model", "BDC_rule", "included_stages"),
  detail = c(
    "Unadjusted final_clean_tewl from RawWindowSummaryClean.",
    "HDT stages only: final_clean_tewl ~ group * stage + (1 | subject); additive model used for overall group emmeans.",
    "HDT stages only: delta_percent_from_BDC ~ group * stage + (1 | subject); additive model used for overall group emmeans.",
    "Subject BDC baseline is mean of BDC-12 and BDC-6 final_clean_tewl; if final BDC unavailable, raw-window BDC fallback is used.",
    paste(hdt_order, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Model_note = model_note,
  Model_tests = model_tests,
  Absolute_group_emmeans = absolute_emm_group,
  Absolute_pairwise_groups = absolute_pairs_group,
  Delta_group_emmeans = delta_emm_group,
  Delta_pairwise_groups = delta_pairs_group,
  Absolute_stage_group_means = absolute_stage_group_means,
  Delta_stage_group_means = delta_stage_group_means,
  Delta_values = delta_values
), out_file)

print(model_tests)
print(delta_emm_group)
print(delta_pairs_group)
cat(out_file, "\n")
