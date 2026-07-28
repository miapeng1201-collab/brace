library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_skin_temperature_correlation_two_groups_exclude_PG.xlsx")
output_plot <- file.path(tewl_dir, "TEWL_skin_temperature_vs_TEWL_two_groups_three_phases_exclude_PG_R.png")

phase_levels <- c("BDC", "HDT1 7 13 19", "HDT37 43 49 55")
colors <- c(Control = "#2F855A", Countermeasure = "#D62728")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

safe_cor <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name & data$group_cm == group_name, ]
  sub <- sub[complete.cases(sub[, c("final_clean_tewl", "skin_temperature")]), ]
  out <- data.frame(
    phase = phase_name,
    group = group_name,
    n = nrow(sub),
    n_subjects = length(unique(sub$subject)),
    pearson_r = NA_real_,
    pearson_p = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    stringsAsFactors = FALSE
  )
  if (nrow(sub) < 4 || length(unique(sub$final_clean_tewl)) < 2 || length(unique(sub$skin_temperature)) < 2) {
    return(out)
  }
  ct <- cor.test(sub$skin_temperature, sub$final_clean_tewl, method = "pearson")
  out$pearson_r <- unname(ct$estimate)
  out$pearson_p <- ct$p.value
  out$ci_low <- ct$conf.int[1]
  out$ci_high <- ct$conf.int[2]
  out
}

fit_lmm <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name & data$group_cm == group_name, ]
  sub <- sub[complete.cases(sub[, c("subject", "final_clean_tewl", "skin_temperature")]), ]
  sub$subject <- droplevels(factor(sub$subject))
  out <- data.frame(
    phase = phase_name,
    group = group_name,
    n = nrow(sub),
    n_subjects = length(unique(sub$subject)),
    beta = NA_real_,
    se = NA_real_,
    df = NA_real_,
    t = NA_real_,
    p_lmerTest = NA_real_,
    subject_random_intercept_sd = NA_real_,
    residual_sd = NA_real_,
    singular_fit = NA,
    note = "",
    stringsAsFactors = FALSE
  )
  if (nrow(sub) < 6 || length(unique(sub$subject)) < 3 || length(unique(sub$skin_temperature)) < 2) {
    out$note <- "insufficient data or no skin-temperature variation"
    return(out)
  }
  tryCatch({
    model <- lmer(final_clean_tewl ~ skin_temperature + (1 | subject), data = sub, REML = FALSE)
    coef_table <- as.data.frame(coef(summary(model)))
    vc <- as.data.frame(VarCorr(model))
    out$beta <- coef_table["skin_temperature", "Estimate"]
    out$se <- coef_table["skin_temperature", "Std. Error"]
    out$df <- if ("df" %in% names(coef_table)) coef_table["skin_temperature", "df"] else NA_real_
    out$t <- coef_table["skin_temperature", "t value"]
    out$p_lmerTest <- coef_table["skin_temperature", "Pr(>|t|)"]
    out$subject_random_intercept_sd <- vc$sdcor[vc$grp == "subject"][1]
    out$residual_sd <- sigma(model)
    out$singular_fit <- isSingular(model)
    out
  }, error = function(e) {
    out$note <- paste("model error:", conditionMessage(e))
    out
  })
}

fit_lm_label <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name & data$group_cm == group_name, ]
  sub <- sub[complete.cases(sub[, c("final_clean_tewl", "skin_temperature")]), ]
  if (nrow(sub) < 3 || length(unique(sub$skin_temperature)) < 2) {
    return(c(beta = NA_real_, p = NA_real_))
  }
  fit <- lm(final_clean_tewl ~ skin_temperature, data = sub)
  coef_table <- as.data.frame(coef(summary(fit)))
  c(
    beta = coef_table["skin_temperature", "Estimate"],
    p = coef_table["skin_temperature", "Pr(>|t|)"]
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
df$skin_temperature <- suppressWarnings(as.numeric(df$avg_temperature_skin_robust_c))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13", "HDT19")] <- "HDT1 7 13 19"
df$phase[df$stage_d %in% c("HDT37", "HDT43", "HDT49", "HDT55")] <- "HDT37 43 49 55"
df$phase <- factor(df$phase, levels = phase_levels)

analysis_data <- df[!is.na(df$phase) & !is.na(df$final_clean_tewl) & !is.na(df$skin_temperature), ]

pearson_rows <- list()
lmm_rows <- list()
label_rows <- list()
idx <- 1
for (phase_name in phase_levels) {
  for (group_name in levels(analysis_data$group_cm)) {
    pearson_rows[[idx]] <- safe_cor(analysis_data, phase_name, group_name)
    lmm_rows[[idx]] <- fit_lmm(analysis_data, phase_name, group_name)
    lm_stat <- fit_lm_label(analysis_data, phase_name, group_name)
    label_rows[[idx]] <- data.frame(
      phase = phase_name,
      group = group_name,
      beta = lm_stat[["beta"]],
      p = lm_stat[["p"]],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}

pearson_results <- do.call(rbind, pearson_rows)
lmm_results <- do.call(rbind, lmm_rows)
label_df <- do.call(rbind, label_rows)
label_df$phase <- factor(label_df$phase, levels = phase_levels)
label_df$group <- factor(label_df$group, levels = levels(analysis_data$group_cm))
label_df$label <- ifelse(
  is.na(label_df$beta),
  paste0(label_df$group, ": beta=NA, p=NA"),
  paste0(
    label_df$group,
    ": beta=", sprintf("%.2f", label_df$beta),
    ", p=", ifelse(label_df$p < 0.001, "<0.001", sprintf("%.3f", label_df$p))
  )
)

global_x_min <- min(analysis_data$skin_temperature, na.rm = TRUE)
global_x_max <- max(analysis_data$skin_temperature, na.rm = TRUE)
global_y_min <- min(analysis_data$final_clean_tewl, na.rm = TRUE)
global_y_max <- max(analysis_data$final_clean_tewl, na.rm = TRUE)
global_y_range <- global_y_max - global_y_min
label_df$x <- global_x_min + 0.03 * (global_x_max - global_x_min)
label_df$y <- global_y_max - (as.numeric(label_df$group) - 1) * 0.065 * global_y_range

p <- ggplot(analysis_data, aes(x = skin_temperature, y = final_clean_tewl, color = group_cm)) +
  geom_point(alpha = 0.7, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.75) +
  facet_wrap(~ phase, ncol = 2) +
  scale_color_manual(values = colors) +
  labs(
    x = "Skin temperature (C)",
    y = "Final clean TEWL (g/m2/h)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey92", color = "grey75"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(output_plot, p, width = 10, height = 7.2, dpi = 300)

model_note <- data.frame(
  item = c("excluded_subjects", "group_recode", "phases", "plot_model", "lmm_model", "stage_source"),
  detail = c(
    "P and G excluded.",
    "Ex and ExAG merged as Countermeasure; Control remains Control.",
    "BDC; HDT1 7 13 19; HDT37 43 49 55.",
    "Per phase/two-group simple linear regression used for plotted beta and p.",
    "final_clean_tewl ~ avg_temperature_skin_robust_c + (1 | subject), fitted separately by phase and two-group.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Pearson_correlations = pearson_results,
    LMM_skin_temperature = lmm_results,
    Plot_lm_labels = label_df,
    Analysis_data = analysis_data
  ),
  output_xlsx
)

print(pearson_results)
print(lmm_results)
cat(output_xlsx, "\n")
cat(output_plot, "\n")
