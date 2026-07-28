library(readxl)
library(ggplot2)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_skin_temperature_ambient_adjusted_by_phase.xlsx")

three_actual_output <- file.path(tewl_dir, "TEWL_skin_temperature_unadjusted_actual_three_groups_R.png")
three_delta_output <- file.path(tewl_dir, "TEWL_skin_temperature_unadjusted_delta_percent_three_groups_R.png")
two_actual_output <- file.path(tewl_dir, "TEWL_skin_temperature_unadjusted_actual_two_groups_R.png")
two_delta_output <- file.path(tewl_dir, "TEWL_skin_temperature_unadjusted_delta_percent_two_groups_R.png")
summary_output <- file.path(tewl_dir, "TEWL_skin_temperature_unadjusted_actual_delta_summary.xlsx")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19",
  "HDT25", "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)

colors_three <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")
colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

summarize_plot <- function(data, group_col, y_col) {
  out <- aggregate(
    as.formula(paste(y_col, "~", group_col, "+ stage_plot")),
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
  )
  out <- do.call(data.frame, out)
  names(out) <- c("group", "stage_plot", "mean", "sd", "sem", "n")
  out$stage_plot <- factor(out$stage_plot, levels = stage_order)
  out
}

make_plot <- function(plot_df, summary_df, group_col, y_col, y_label, out_file, colors, add_zero_line = FALSE) {
  p <- ggplot() +
    geom_line(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], group = interaction(.data[[group_col]], subject), color = .data[[group_col]]),
      alpha = 0.16,
      linewidth = 0.42,
      na.rm = TRUE
    ) +
    geom_point(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], color = .data[[group_col]]),
      alpha = 0.28,
      size = 1.15,
      na.rm = TRUE
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = stage_plot, ymin = mean - sem, ymax = mean + sem, color = group),
      width = 0.18,
      linewidth = 0.65,
      na.rm = TRUE
    ) +
    geom_line(
      data = summary_df,
      aes(x = stage_plot, y = mean, group = group, color = group),
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    geom_point(
      data = summary_df,
      aes(x = stage_plot, y = mean, color = group),
      size = 2.35,
      na.rm = TRUE
    ) +
    scale_color_manual(values = colors, drop = FALSE) +
    labs(x = NULL, y = y_label, color = "Group") +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1, face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 10, 8, 8)
    )
  if (add_zero_line) {
    p <- p + geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35)
  }
  ggsave(out_file, p, width = 11.2, height = 6.8, dpi = 320, bg = "white")
}

df <- read_excel(input_file, sheet = "Adjusted_data")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group.x), levels = names(colors_three))
df$group_cm <- ifelse(as.character(df$group) == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = names(colors_two))
df$stage_raw <- as.character(df$stage)
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$stage_plot <- factor(df$stage_plot, levels = stage_order)
df$skin_unadjusted <- suppressWarnings(as.numeric(df$avg_temperature_skin_robust_c))

plot_source <- df[!is.na(df$stage_plot) & !is.na(df$skin_unadjusted), ]

stage_values <- aggregate(
  skin_unadjusted ~ group + group_cm + subject + stage_plot,
  data = plot_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)
stage_values$stage_plot <- factor(stage_values$stage_plot, levels = stage_order)
stage_values$group <- factor(stage_values$group, levels = names(colors_three))
stage_values$group_cm <- factor(stage_values$group_cm, levels = names(colors_two))

baseline <- stage_values[stage_values$stage_plot == "BDC", c("subject", "skin_unadjusted")]
names(baseline)[names(baseline) == "skin_unadjusted"] <- "BDC_skin_unadjusted"

delta_df <- merge(stage_values, baseline, by = "subject", all.x = TRUE, sort = FALSE)
delta_df$delta_percent_from_BDC <- (delta_df$skin_unadjusted - delta_df$BDC_skin_unadjusted) /
  delta_df$BDC_skin_unadjusted * 100
delta_df <- delta_df[!is.na(delta_df$delta_percent_from_BDC), ]
delta_df$stage_plot <- factor(delta_df$stage_plot, levels = stage_order)
delta_df$group <- factor(delta_df$group, levels = names(colors_three))
delta_df$group_cm <- factor(delta_df$group_cm, levels = names(colors_two))

three_actual_summary <- summarize_plot(stage_values, "group", "skin_unadjusted")
three_delta_summary <- summarize_plot(delta_df, "group", "delta_percent_from_BDC")
two_actual_summary <- summarize_plot(stage_values, "group_cm", "skin_unadjusted")
two_delta_summary <- summarize_plot(delta_df, "group_cm", "delta_percent_from_BDC")

make_plot(stage_values, three_actual_summary, "group", "skin_unadjusted", "Skin temperature (C)", three_actual_output, colors_three)
make_plot(delta_df, three_delta_summary, "group", "delta_percent_from_BDC", "Change from subject BDC skin temperature baseline (%)", three_delta_output, colors_three, TRUE)
make_plot(stage_values, two_actual_summary, "group_cm", "skin_unadjusted", "Skin temperature (C)", two_actual_output, colors_two)
make_plot(delta_df, two_delta_summary, "group_cm", "delta_percent_from_BDC", "Change from subject BDC skin temperature baseline (%)", two_delta_output, colors_two, TRUE)

model_note <- data.frame(
  item = c("outcome", "three_groups", "two_groups", "delta_baseline", "stage_source"),
  detail = c(
    "avg_temperature_skin_robust_c, before ambient-temperature correction",
    "Control, Ex, and ExAG shown separately.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "Delta percent uses each subject's mean unadjusted BDC value from BDC-12 and BDC-6.",
    "Same records and stage order as the ambient-adjusted skin-temperature plots."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Subject_stage_values = stage_values,
    Delta_values = delta_df,
    Three_group_absolute_summary = three_actual_summary,
    Three_group_delta_summary = three_delta_summary,
    Two_group_absolute_summary = two_actual_summary,
    Two_group_delta_summary = two_delta_summary
  ),
  summary_output
)

cat(three_actual_output, "\n")
cat(three_delta_output, "\n")
cat(two_actual_output, "\n")
cat(two_delta_output, "\n")
cat(summary_output, "\n")
