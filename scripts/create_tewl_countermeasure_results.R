library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")

out_xlsx <- file.path(tewl_dir, "TEWL_countermeasure_group_results.xlsx")
plot_unadjusted_delta <- file.path(tewl_dir, "TEWL_countermeasure_final_delta_percent_from_BDC_R.png")
plot_adjusted_values <- file.path(tewl_dir, "TEWL_countermeasure_ambient_adjusted_values_R.png")
plot_adjusted_delta <- file.path(tewl_dir, "TEWL_countermeasure_ambient_adjusted_delta_percent_from_BDC_R.png")

stage_order <- c("BDC", "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55")
hdt_order <- stage_order[stage_order != "BDC"]
phase_map <- list(
  BDC = c("BDC-12", "BDC-6"),
  HDT1_7_13_19 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  HDT25_37_43_49_55 = c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"),
  R = c("R+1", "R+6", "R+12")
)

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

stage_to_phase <- function(stage) {
  out <- rep(NA_character_, length(stage))
  for (phase in names(phase_map)) out[stage %in% phase_map[[phase]]] <- phase
  out
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

fit_ambient_model <- function(data, formula_text, ambient_col, subject_col) {
  model <- lmer(as.formula(formula_text), data = data, REML = FALSE)
  coef_table <- as.data.frame(coef(summary(model)))
  vc <- as.data.frame(VarCorr(model))
  list(
    ambient_beta = coef_table[ambient_col, "Estimate"],
    ambient_se = coef_table[ambient_col, "Std. Error"],
    ambient_df = if ("df" %in% names(coef_table)) coef_table[ambient_col, "df"] else NA_real_,
    ambient_t = coef_table[ambient_col, "t value"],
    ambient_p = coef_table[ambient_col, "Pr(>|t|)"],
    subject_sd = vc$sdcor[vc$grp == subject_col][1],
    residual_sd = sigma(model),
    singular_fit = isSingular(model)
  )
}

summarize_plot <- function(data, y_col) {
  f <- as.formula(paste(y_col, "~ group_cm + stage_plot"))
  out <- aggregate(
    f,
    data = data,
    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
  )
  out <- do.call(data.frame, out)
  names(out) <- c("group_cm", "stage_plot", "mean", "se", "n")
  out
}

make_line_plot <- function(plot_df, summary_df, y_col, y_label, out_file) {
  colors <- c(Control = "#2F855A", Countermeasure = "#D62728")
  p <- ggplot() +
    geom_hline(yintercept = ifelse(grepl("percent", y_col), 0, NA), color = "grey35", linewidth = 0.35, na.rm = TRUE) +
    geom_line(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], group = interaction(group_cm, subject), color = group_cm),
      alpha = 0.18,
      linewidth = 0.45,
      na.rm = TRUE
    ) +
    geom_point(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], color = group_cm),
      alpha = 0.28,
      size = 1.2,
      na.rm = TRUE
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = stage_plot, ymin = mean - se, ymax = mean + se, color = group_cm),
      width = 0.18,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_line(
      data = summary_df,
      aes(x = stage_plot, y = mean, group = group_cm, color = group_cm),
      linewidth = 1.15,
      na.rm = TRUE
    ) +
    geom_point(
      data = summary_df,
      aes(x = stage_plot, y = mean, color = group_cm),
      size = 2.4,
      na.rm = TRUE
    ) +
    scale_color_manual(values = colors) +
    labs(x = NULL, y = y_label, color = "Group") +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  ggsave(out_file, p, width = 10.5, height = 6.5, dpi = 300)
}

df <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
day_col <- pick_col(df, c("day", "Day"))
tewl_col <- pick_col(df, c("final_clean_tewl"))
raw_col <- pick_col(df, c("raw_window_tewl_mean_of_measurements_g_m2_h"))
ambient_col <- pick_col(df, c("ambient_temperature_take_c"))
skin_col <- pick_col(df, c("avg_temperature_skin_robust_c"))

df$subject <- as.character(df[[subject_col]])
df$group_original <- as.character(df[[group_col]])
df$group_cm <- ifelse(df$group_original == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$stage <- as.character(df[[stage_col]])
df$day <- df[[day_col]]
df$stage_plot <- ifelse(df$stage %in% c("BDC-12", "BDC-6"), "BDC", df$stage)
df$stage_plot <- factor(df$stage_plot, levels = stage_order)
df$adjustment_window <- stage_to_phase(df$stage)
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df[[tewl_col]])))
df$raw_window_tewl_mean_of_measurements_g_m2_h <- suppressWarnings(as.numeric(to_missing(df[[raw_col]])))
df$ambient_temperature_take_c <- suppressWarnings(as.numeric(df[[ambient_col]]))
df$avg_temperature_skin_robust_c <- suppressWarnings(as.numeric(df[[skin_col]]))

stage_values <- aggregate(
  final_clean_tewl ~ group_cm + subject + stage_plot,
  data = df[!is.na(df$stage_plot), ],
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline_source <- df[df$stage %in% c("BDC-12", "BDC-6"), ]
baseline_source$baseline_value <- ifelse(
  is.na(baseline_source$final_clean_tewl),
  baseline_source$raw_window_tewl_mean_of_measurements_g_m2_h,
  baseline_source$final_clean_tewl
)
baseline <- aggregate(
  baseline_value ~ group_cm + subject,
  data = baseline_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)
names(baseline)[names(baseline) == "baseline_value"] <- "BDC"

delta_values <- merge(stage_values, baseline, by = c("group_cm", "subject"), all.x = TRUE)
delta_values$delta_percent_from_BDC <- (delta_values$final_clean_tewl - delta_values$BDC) / delta_values$BDC * 100
delta_values <- delta_values[!is.na(delta_values$stage_plot), ]

absolute_hdt <- stage_values[stage_values$stage_plot %in% hdt_order & !is.na(stage_values$final_clean_tewl), ]
absolute_hdt$subject <- factor(absolute_hdt$subject)
absolute_hdt$group_cm <- droplevels(absolute_hdt$group_cm)
absolute_hdt$stage_plot <- droplevels(absolute_hdt$stage_plot)

delta_hdt <- delta_values[delta_values$stage_plot %in% hdt_order & !is.na(delta_values$delta_percent_from_BDC), ]
delta_hdt$subject <- factor(delta_hdt$subject)
delta_hdt$group_cm <- droplevels(delta_hdt$group_cm)
delta_hdt$stage_plot <- droplevels(delta_hdt$stage_plot)

fit_abs_inter <- lmer(final_clean_tewl ~ group_cm * stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)
fit_abs_add <- lmer(final_clean_tewl ~ group_cm + stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)
fit_abs_no_group <- lmer(final_clean_tewl ~ stage_plot + (1 | subject), data = absolute_hdt, REML = FALSE)

fit_delta_inter <- lmer(delta_percent_from_BDC ~ group_cm * stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_delta_add <- lmer(delta_percent_from_BDC ~ group_cm + stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_delta_no_group <- lmer(delta_percent_from_BDC ~ stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)

model_tests <- rbind(
  data.frame(
    outcome = "final_clean_tewl_absolute_HDT",
    test = c("group_main_effect_additive_LRT", "group_by_stage_interaction_LRT"),
    chi_sq = c(anova(fit_abs_no_group, fit_abs_add)$Chisq[2], anova(fit_abs_add, fit_abs_inter)$Chisq[2]),
    df = c(anova(fit_abs_no_group, fit_abs_add)$Df[2], anova(fit_abs_add, fit_abs_inter)$Df[2]),
    p = c(anova(fit_abs_no_group, fit_abs_add)[["Pr(>Chisq)"]][2], anova(fit_abs_add, fit_abs_inter)[["Pr(>Chisq)"]][2]),
    stringsAsFactors = FALSE
  ),
  data.frame(
    outcome = "delta_percent_from_BDC_HDT",
    test = c("group_main_effect_additive_LRT", "group_by_stage_interaction_LRT"),
    chi_sq = c(anova(fit_delta_no_group, fit_delta_add)$Chisq[2], anova(fit_delta_add, fit_delta_inter)$Chisq[2]),
    df = c(anova(fit_delta_no_group, fit_delta_add)$Df[2], anova(fit_delta_add, fit_delta_inter)$Df[2]),
    p = c(anova(fit_delta_no_group, fit_delta_add)[["Pr(>Chisq)"]][2], anova(fit_delta_add, fit_delta_inter)[["Pr(>Chisq)"]][2]),
    stringsAsFactors = FALSE
  )
)

absolute_emm <- as.data.frame(emmeans(fit_abs_add, ~ group_cm))
absolute_pairwise <- as.data.frame(pairs(emmeans(fit_abs_add, ~ group_cm)))
delta_emm <- as.data.frame(emmeans(fit_delta_add, ~ group_cm))
delta_pairwise <- as.data.frame(pairs(emmeans(fit_delta_add, ~ group_cm)))

df$tewl_for_ambient_adjustment <- df$final_clean_tewl
bdc_fallback_idx <- !is.na(df$adjustment_window) & df$adjustment_window == "BDC" &
  is.na(df$tewl_for_ambient_adjustment) & !is.na(df$raw_window_tewl_mean_of_measurements_g_m2_h)
df$tewl_for_ambient_adjustment[bdc_fallback_idx] <- df$raw_window_tewl_mean_of_measurements_g_m2_h[bdc_fallback_idx]

adjusted <- df
adjusted$model_group <- NA_character_
adjusted$reference_ambient_temperature_c <- NA_real_
adjusted$ambient_beta_for_adjustment <- NA_real_
adjusted$tewl_ambient_adjusted <- NA_real_

model_rows <- list()
row_i <- 1
for (phase in names(phase_map)) {
  phase_data <- adjusted[!is.na(adjusted$adjustment_window) & adjusted$adjustment_window == phase, ]
  if (phase == "BDC") {
    model_sets <- list(All_groups = phase_data)
  } else {
    model_sets <- split(phase_data, phase_data$group_cm)
  }
  for (mg in names(model_sets)) {
    sub <- model_sets[[mg]]
    sub <- sub[complete.cases(sub[, c("subject", "group_cm", "tewl_for_ambient_adjustment", "ambient_temperature_take_c")]), ]
    sub$subject <- droplevels(factor(sub$subject))
    sub$group_cm <- droplevels(factor(sub$group_cm))
    stages <- paste(sort(unique(sub$stage)), collapse = ", ")
    ref_ambient <- mean(sub$ambient_temperature_take_c, na.rm = TRUE)
    model_label <- if (phase == "BDC") {
      "tewl_for_ambient_adjustment ~ ambient_temperature_take_c + group_cm + (1 | subject)"
    } else {
      "tewl_for_ambient_adjustment ~ ambient_temperature_take_c + (1 | subject)"
    }
    if (nrow(sub) < 6 || length(unique(sub$subject)) < 3 || length(unique(sub$ambient_temperature_take_c)) < 2) {
      model_rows[[row_i]] <- data.frame(
        adjustment_window = phase, model_group = mg, n = nrow(sub), n_subjects = length(unique(sub$subject)),
        stages = stages, model = model_label, reference_ambient_temperature_c = ref_ambient,
        ambient_beta = NA_real_, ambient_se = NA_real_, ambient_df = NA_real_, ambient_t = NA_real_,
        ambient_p_lmerTest = NA_real_, singular_fit = NA, note = "insufficient data",
        stringsAsFactors = FALSE
      )
      row_i <- row_i + 1
      next
    }
    formula_text <- if (phase == "BDC") model_label else "tewl_for_ambient_adjustment ~ ambient_temperature_take_c + (1 | subject)"
    fit <- fit_ambient_model(sub, formula_text, "ambient_temperature_take_c", "subject")
    model_rows[[row_i]] <- data.frame(
      adjustment_window = phase, model_group = mg, n = nrow(sub), n_subjects = length(unique(sub$subject)),
      stages = stages, model = model_label, reference_ambient_temperature_c = ref_ambient,
      ambient_beta = fit$ambient_beta, ambient_se = fit$ambient_se, ambient_df = fit$ambient_df,
      ambient_t = fit$ambient_t, ambient_p_lmerTest = fit$ambient_p,
      singular_fit = fit$singular_fit, note = "",
      stringsAsFactors = FALSE
    )
    row_i <- row_i + 1

    apply_idx <- !is.na(adjusted$adjustment_window) & adjusted$adjustment_window == phase
    if (phase != "BDC") apply_idx <- apply_idx & adjusted$group_cm == mg
    ok <- apply_idx & !is.na(adjusted$tewl_for_ambient_adjustment) & !is.na(adjusted$ambient_temperature_take_c)
    adjusted$model_group[apply_idx] <- mg
    adjusted$reference_ambient_temperature_c[apply_idx] <- ref_ambient
    adjusted$ambient_beta_for_adjustment[apply_idx] <- fit$ambient_beta
    adjusted$tewl_ambient_adjusted[ok] <- adjusted$tewl_for_ambient_adjustment[ok] -
      fit$ambient_beta * (adjusted$ambient_temperature_take_c[ok] - ref_ambient)
  }
}
ambient_models <- do.call(rbind, model_rows)

adjusted_plot_source <- adjusted[!is.na(adjusted$stage_plot) & !is.na(adjusted$tewl_ambient_adjusted), ]
adjusted_values <- aggregate(
  tewl_ambient_adjusted ~ group_cm + subject + stage_plot,
  data = adjusted_plot_source,
  FUN = function(x) mean(x, na.rm = TRUE)
)
adjusted_baseline <- adjusted_values[adjusted_values$stage_plot == "BDC", c("group_cm", "subject", "tewl_ambient_adjusted")]
names(adjusted_baseline)[names(adjusted_baseline) == "tewl_ambient_adjusted"] <- "BDC_adjusted"
adjusted_delta <- merge(adjusted_values, adjusted_baseline, by = c("group_cm", "subject"), all.x = TRUE)
adjusted_delta$delta_percent_from_adjusted_BDC <- (adjusted_delta$tewl_ambient_adjusted - adjusted_delta$BDC_adjusted) /
  adjusted_delta$BDC_adjusted * 100

make_line_plot(
  delta_values[delta_values$stage_plot %in% stage_order & !is.na(delta_values$delta_percent_from_BDC), ],
  summarize_plot(delta_values[delta_values$stage_plot %in% stage_order & !is.na(delta_values$delta_percent_from_BDC), ], "delta_percent_from_BDC"),
  "delta_percent_from_BDC",
  "Change from subject BDC baseline (%)",
  plot_unadjusted_delta
)
make_line_plot(
  adjusted_values[adjusted_values$stage_plot %in% stage_order & !is.na(adjusted_values$tewl_ambient_adjusted), ],
  summarize_plot(adjusted_values[adjusted_values$stage_plot %in% stage_order & !is.na(adjusted_values$tewl_ambient_adjusted), ], "tewl_ambient_adjusted"),
  "tewl_ambient_adjusted",
  "Ambient-temperature adjusted TEWL (g/m2/h)",
  plot_adjusted_values
)
make_line_plot(
  adjusted_delta[adjusted_delta$stage_plot %in% stage_order & !is.na(adjusted_delta$delta_percent_from_adjusted_BDC), ],
  summarize_plot(adjusted_delta[adjusted_delta$stage_plot %in% stage_order & !is.na(adjusted_delta$delta_percent_from_adjusted_BDC), ], "delta_percent_from_adjusted_BDC"),
  "delta_percent_from_adjusted_BDC",
  "Change from adjusted BDC baseline (%)",
  plot_adjusted_delta
)

model_note <- data.frame(
  item = c("group_recode", "excluded_stage", "plot_stages", "unadjusted_models", "ambient_adjustment"),
  detail = c(
    "Ex and ExAG were combined as Countermeasure; Control remains Control.",
    "HDT31 excluded.",
    paste(stage_order, collapse = ", "),
    "HDT stages only: final_clean_tewl or delta_percent_from_BDC ~ group_cm * stage + (1 | subject). Additive model used for overall emmeans.",
    "BDC uses all groups together; other phases use Control vs Countermeasure model groups."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Model_note = model_note,
  Group_tests = model_tests,
  Absolute_group_emmeans = absolute_emm,
  Absolute_pairwise = absolute_pairwise,
  Delta_group_emmeans = delta_emm,
  Delta_pairwise = delta_pairwise,
  Ambient_adjustment_models = ambient_models,
  Unadjusted_delta_values = delta_values,
  Ambient_adjusted_values = adjusted[, c(
    "subject", "group_original", "group_cm", "stage", "day", "adjustment_window", "model_group",
    "final_clean_tewl", "tewl_for_ambient_adjustment", "ambient_temperature_take_c",
    "reference_ambient_temperature_c", "ambient_beta_for_adjustment", "tewl_ambient_adjusted"
  )],
  Ambient_adjusted_delta = adjusted_delta
), out_xlsx)

print(model_tests)
print(delta_emm)
print(delta_pairwise)
print(ambient_models)
cat(out_xlsx, "\n")
cat(plot_unadjusted_delta, "\n")
cat(plot_adjusted_values, "\n")
cat(plot_adjusted_delta, "\n")
