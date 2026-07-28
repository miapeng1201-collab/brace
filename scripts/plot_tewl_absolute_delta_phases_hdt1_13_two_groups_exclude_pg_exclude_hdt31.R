library(readxl)
library(ggplot2)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")

output_absolute_plot <- file.path(tewl_dir, "TEWL_absolute_BDC_HDTE1_13_HDTM_HDTL_two_groups_exclude_PG_exclude_HDT31_R.png")
output_delta_plot <- file.path(tewl_dir, "TEWL_delta_percent_BDC_HDTE1_13_HDTM_HDTL_two_groups_exclude_PG_exclude_HDT31_R.png")
output_xlsx <- file.path(tewl_dir, "TEWL_absolute_delta_BDC_HDTE1_13_HDTM_HDTL_two_groups_exclude_PG_exclude_HDT31_summary.xlsx")

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

summarize_metric <- function(data, metric_col) {
  out <- aggregate(
    data[[metric_col]] ~ group_cm + phase,
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
  )
  names(out)[names(out) == "data[[metric_col]]"] <- metric_col
  unpacked <- do.call(data.frame, out)
  names(unpacked) <- c("group", "phase", "mean", "sd", "sem", "n")
  unpacked$phase <- factor(unpacked$phase, levels = phase_levels)
  unpacked$group <- factor(unpacked$group, levels = names(colors_two))
  unpacked
}

plot_metric <- function(data, summary_df, metric_col, y_label, output_file, zero_line = FALSE) {
  p <- ggplot()
  if (zero_line) {
    p <- p + geom_hline(yintercept = 0, linetype = "22", color = "grey55", linewidth = 0.45)
  }
  p <- p +
    geom_line(
      data = data,
      aes(x = phase, y = .data[[metric_col]], group = interaction(subject, group_cm), color = group_cm),
      alpha = 0.18,
      linewidth = 0.35
    ) +
    geom_point(
      data = data,
      aes(x = phase, y = .data[[metric_col]], color = group_cm),
      alpha = 0.35,
      size = 1.4,
      position = position_jitter(width = 0.055, height = 0)
    ) +
    geom_line(
      data = summary_df,
      aes(x = phase, y = mean, group = group, color = group),
      linewidth = 0.95
    ) +
    geom_point(
      data = summary_df,
      aes(x = phase, y = mean, color = group),
      size = 2.7
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = phase, ymin = mean - sem, ymax = mean + sem, color = group),
      width = 0.12,
      linewidth = 0.65
    ) +
    scale_color_manual(values = colors_two) +
    labs(x = NULL, y = y_label, color = NULL) +
    theme_classic(base_size = 12) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(face = "bold"),
      plot.margin = margin(8, 10, 8, 8)
    )

  ggsave(output_file, p, width = 6.4, height = 4.4, dpi = 320, bg = "white")
  p
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
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13")] <- "HDT-E"
df$phase[df$stage_d %in% c("HDT25", "HDT37")] <- "HDT-M"
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

plot_data <- merge(subject_phase, baseline, by = "subject", all.x = TRUE, sort = FALSE)
plot_data <- plot_data[complete.cases(plot_data[, c("final_clean_tewl", "baseline_bdc_tewl")]), ]
plot_data$delta_percent_from_BDC <- (plot_data$final_clean_tewl - plot_data$baseline_bdc_tewl) /
  plot_data$baseline_bdc_tewl * 100

absolute_summary <- summarize_metric(plot_data, "final_clean_tewl")
delta_summary <- summarize_metric(plot_data, "delta_percent_from_BDC")

plot_metric(
  plot_data,
  absolute_summary,
  "final_clean_tewl",
  expression(TEWL~"(g/m"^2*"/h)"),
  output_absolute_plot
)
plot_metric(
  plot_data,
  delta_summary,
  "delta_percent_from_BDC",
  "Delta TEWL from BDC (%)",
  output_delta_plot,
  zero_line = TRUE
)

model_note <- data.frame(
  item = c("excluded_subjects", "groups", "outcome", "delta_formula", "phase_definition", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Absolute plot uses final_clean_tewl. Delta plot uses delta_percent_from_BDC.",
    "Delta percent = (subject-phase TEWL - subject BDC TEWL) / subject BDC TEWL * 100.",
    "BDC = BDC-12 and BDC-6; HDT-E = HDT1, HDT7, HDT13; HDT-M = HDT25, HDT37; HDT-L = HDT43, HDT49, HDT55. HDT31 excluded.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "Each subject was averaged within phase before plotting and summary."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Subject_phase_values = plot_data,
    Absolute_summary = absolute_summary,
    Delta_summary = delta_summary
  ),
  output_xlsx
)

cat(output_absolute_plot, "\n")
cat(output_delta_plot, "\n")
cat(output_xlsx, "\n")
print(absolute_summary)
print(delta_summary)
