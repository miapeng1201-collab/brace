library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_file <- file.path(tewl_dir, "TEWL_final_percent_change_from_BDC_by_group_R.png")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$raw_window_tewl_mean_of_measurements_g_m2_h <- suppressWarnings(
  as.numeric(to_missing(df$raw_window_tewl_mean_of_measurements_g_m2_h))
)

stage_values <- aggregate(
  final_clean_tewl ~ group + subject + stage_d,
  data = df,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline_source <- df[df$stage_d %in% c("BDC-12", "BDC-6"), ]
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

plot_df <- merge(stage_values, baseline, by = c("group", "subject"), all.x = TRUE)
plot_df$percent_change <- (plot_df$final_clean_tewl - plot_df$BDC) / plot_df$BDC * 100
plot_df <- plot_df[!(plot_df$stage_d %in% c("BDC-12", "BDC-6")), ]

baseline_rows <- baseline
baseline_rows$stage_d <- "BDC"
baseline_rows$final_clean_tewl <- baseline_rows$BDC
baseline_rows$percent_change <- 0

plot_df <- rbind(
  baseline_rows[, c("group", "subject", "stage_d", "final_clean_tewl", "BDC", "percent_change")],
  plot_df[, c("group", "subject", "stage_d", "final_clean_tewl", "BDC", "percent_change")]
)
plot_df$stage_axis <- factor(plot_df$stage_d, levels = stage_order)
plot_df <- plot_df[!is.na(plot_df$stage_axis) & !is.na(plot_df$percent_change), ]

summary_df <- aggregate(
  percent_change ~ group + stage_axis,
  data = plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "stage_axis", "mean_percent_change", "se", "n")

colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
  geom_line(
    data = plot_df,
    aes(x = stage_axis, y = percent_change, group = interaction(group, subject), color = group),
    alpha = 0.18,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = plot_df,
    aes(x = stage_axis, y = percent_change, color = group),
    alpha = 0.28,
    size = 1.2,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = summary_df,
    aes(
      x = stage_axis,
      ymin = mean_percent_change - se,
      ymax = mean_percent_change + se,
      color = group
    ),
    width = 0.18,
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  geom_line(
    data = summary_df,
    aes(x = stage_axis, y = mean_percent_change, group = group, color = group),
    linewidth = 1.15,
    na.rm = TRUE
  ) +
  geom_point(
    data = summary_df,
    aes(x = stage_axis, y = mean_percent_change, color = group),
    size = 2.4,
    na.rm = TRUE
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = NULL,
    y = "Change from subject BDC baseline (%)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(output_file, p, width = 11, height = 6.8, dpi = 300)
cat(output_file, "\n")
