library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_raw_window_HL_total_35_55s.xlsx")
actual_output <- file.path(tewl_dir, "TEWL_HL_total_35_55s_actual_by_group_R.png")
delta_output <- file.path(tewl_dir, "TEWL_HL_total_35_55s_delta_percent_from_BDC_by_group_R.png")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)
colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

df <- read_excel(input_file, sheet = "HLTotalSummary35_55s")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_raw <- as.character(df$stage)
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$stage_plot <- factor(df$stage_plot, levels = stage_order)
df$hl_total <- suppressWarnings(as.numeric(df$raw_window_hl_total_mean_of_measurements_w_m2))

plot_source <- df[!is.na(df$stage_plot) & !is.na(df$hl_total), ]

stage_values <- aggregate(
  hl_total ~ group + subject + stage_plot,
  data = plot_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline <- stage_values[stage_values$stage_plot == "BDC", c("group", "subject", "hl_total")]
names(baseline)[names(baseline) == "hl_total"] <- "BDC_hl_total"

delta_df <- merge(stage_values, baseline, by = c("group", "subject"), all.x = TRUE)
delta_df$delta_percent_from_BDC <- (delta_df$hl_total - delta_df$BDC_hl_total) / delta_df$BDC_hl_total * 100
delta_df <- delta_df[!is.na(delta_df$delta_percent_from_BDC), ]

summarize_plot <- function(data, y_col) {
  out <- aggregate(
    as.formula(paste(y_col, "~ group + stage_plot")),
    data = data,
    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
  )
  out <- do.call(data.frame, out)
  names(out) <- c("group", "stage_plot", "mean", "se", "n")
  out
}

make_plot <- function(plot_df, summary_df, y_col, y_label, out_file, add_zero_line = FALSE) {
  p <- ggplot() +
    geom_line(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], group = interaction(group, subject), color = group),
      alpha = 0.18,
      linewidth = 0.45,
      na.rm = TRUE
    ) +
    geom_point(
      data = plot_df,
      aes(x = stage_plot, y = .data[[y_col]], color = group),
      alpha = 0.28,
      size = 1.2,
      na.rm = TRUE
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = stage_plot, ymin = mean - se, ymax = mean + se, color = group),
      width = 0.18,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_line(
      data = summary_df,
      aes(x = stage_plot, y = mean, group = group, color = group),
      linewidth = 1.15,
      na.rm = TRUE
    ) +
    geom_point(
      data = summary_df,
      aes(x = stage_plot, y = mean, color = group),
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
  if (add_zero_line) {
    p <- p + geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35)
  }
  ggsave(out_file, p, width = 11, height = 6.8, dpi = 300)
}

make_plot(
  stage_values,
  summarize_plot(stage_values, "hl_total"),
  "hl_total",
  "HL Total 35-55 s (W/m2)",
  actual_output
)

make_plot(
  delta_df,
  summarize_plot(delta_df, "delta_percent_from_BDC"),
  "delta_percent_from_BDC",
  "Change from subject BDC HL Total baseline (%)",
  delta_output,
  add_zero_line = TRUE
)

cat(actual_output, "\n")
cat(delta_output, "\n")
