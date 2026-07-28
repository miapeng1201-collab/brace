library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_skin_temperature_vs_final_clean_tewl_analysis.xlsx")
output_plot <- file.path(tewl_dir, "TEWL_skin_temperature_vs_final_clean_tewl_four_phases_R.png")

colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

fit_lmm <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name, ]
  if (group_name != "All_groups") {
    sub <- sub[sub$group == group_name, ]
  }
  sub <- sub[complete.cases(sub[, c("subject", "final_clean_tewl", "skin_temperature")]), ]
  sub$subject <- droplevels(factor(sub$subject))
  sub$group <- droplevels(factor(sub$group))
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
    formula <- if (group_name == "All_groups") {
      final_clean_tewl ~ skin_temperature + group + (1 | subject)
    } else {
      final_clean_tewl ~ skin_temperature + (1 | subject)
    }
    model <- lmer(formula, data = sub, REML = FALSE)
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

fit_lm_stat <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name, ]
  if (group_name != "All_groups") {
    sub <- sub[sub$group == group_name, ]
  }
  sub <- sub[complete.cases(sub[, c("final_clean_tewl", "skin_temperature")]), ]
  if (nrow(sub) < 3 || length(unique(sub$skin_temperature)) < 2) {
    return(c(beta = NA_real_, p = NA_real_))
  }
  fit <- if (group_name == "All_groups") {
    lm(final_clean_tewl ~ skin_temperature + group, data = sub)
  } else {
    lm(final_clean_tewl ~ skin_temperature, data = sub)
  }
  coef_table <- as.data.frame(coef(summary(fit)))
  c(
    beta = coef_table["skin_temperature", "Estimate"],
    p = coef_table["skin_temperature", "Pr(>|t|)"]
  )
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$skin_temperature <- suppressWarnings(as.numeric(df$avg_temperature_skin_robust_c))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13", "HDT19")] <- "HDT1 7 13 19"
df$phase[df$stage_d %in% c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55")] <- "HDT25 37 43 49 55"
df$phase[df$stage_d %in% c("R+1", "R+6", "R+12")] <- "R"
df$phase <- factor(df$phase, levels = c("BDC", "HDT1 7 13 19", "HDT25 37 43 49 55", "R"))

plot_df <- df[!is.na(df$phase) & !is.na(df$final_clean_tewl) & !is.na(df$skin_temperature), ]

lmm_rows <- list()
lm_rows <- list()
idx <- 1
for (phase_name in levels(plot_df$phase)) {
  group_names <- if (phase_name == "BDC") "All_groups" else levels(plot_df$group)
  for (group_name in group_names) {
    lmm_rows[[idx]] <- fit_lmm(plot_df, phase_name, group_name)
    lm_stat <- fit_lm_stat(plot_df, phase_name, group_name)
    lm_rows[[idx]] <- data.frame(
      phase = phase_name,
      group = group_name,
      beta = lm_stat[["beta"]],
      p = lm_stat[["p"]],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}
lmm_results <- do.call(rbind, lmm_rows)
lm_results <- do.call(rbind, lm_rows)
lm_results$phase <- factor(lm_results$phase, levels = levels(plot_df$phase))
lm_results$group <- factor(lm_results$group, levels = c("All_groups", levels(plot_df$group)))
lm_results$label <- ifelse(
  is.na(lm_results$beta),
  paste0(lm_results$group, ": beta=NA, p=NA"),
  paste0(
    ifelse(lm_results$group == "All_groups", "All", as.character(lm_results$group)),
    ": beta=", sprintf("%.2f", lm_results$beta),
    ", p=", ifelse(lm_results$p < 0.001, "<0.001", sprintf("%.3f", lm_results$p))
  )
)

global_x_min <- min(plot_df$skin_temperature, na.rm = TRUE)
global_x_max <- max(plot_df$skin_temperature, na.rm = TRUE)
global_y_min <- min(plot_df$final_clean_tewl, na.rm = TRUE)
global_y_max <- max(plot_df$final_clean_tewl, na.rm = TRUE)
global_y_range <- global_y_max - global_y_min
lm_results$x <- global_x_min + 0.03 * (global_x_max - global_x_min)
label_order <- ifelse(lm_results$group == "All_groups", 1, match(lm_results$group, levels(plot_df$group)))
lm_results$y <- global_y_max - (label_order - 1) * 0.055 * global_y_range

p <- ggplot(plot_df, aes(x = skin_temperature, y = final_clean_tewl, color = group)) +
  geom_point(alpha = 0.7, size = 2.0) +
  geom_smooth(
    data = subset(plot_df, phase != "BDC"),
    method = "lm",
    se = TRUE,
    linewidth = 0.75
  ) +
  geom_smooth(
    data = subset(plot_df, phase == "BDC"),
    aes(x = skin_temperature, y = final_clean_tewl, group = 1),
    inherit.aes = FALSE,
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 0.75
  ) +
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
  item = c("plot_model", "lmm_model", "phases", "stage_source"),
  detail = c(
    "Per phase/group simple linear regression used for plotted beta and p; BDC combines all groups and adjusts for group in the label model.",
    "BDC: final_clean_tewl ~ avg_temperature_skin_robust_c + group + (1 | subject). Other phases: final_clean_tewl ~ avg_temperature_skin_robust_c + (1 | subject), fitted separately by group.",
    "BDC; HDT1 7 13 19; HDT25 37 43 49 55; R. HDT31 excluded.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    LMM_skin_temperature = lmm_results,
    Plot_lm_labels = lm_results,
    Analysis_data = plot_df
  ),
  output_xlsx
)

print(lmm_results)
cat(output_xlsx, "\n")
cat(output_plot, "\n")
