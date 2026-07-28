library(ggplot2)
library(readxl)

args_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(args_file), ".."), mustWork = TRUE)
input <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_processed_raw_window.xlsx")
output <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_final_percent_change_from_BDC_R.png")

df <- read_excel(input, sheet = "RawWindowSummaryClean")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$raw_window_tewl_mean_of_measurements_g_m2_h <- suppressWarnings(
  as.numeric(to_missing(df$raw_window_tewl_mean_of_measurements_g_m2_h))
)
df$subject <- as.character(df$subject)
df$group <- factor(df$group, levels = c("Control", "Ex", "ExAG"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage

stage_values <- aggregate(
  final_clean_tewl ~ group + subject + stage_d,
  data = df,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline_source <- subset(df, stage_d %in% c("BDC-12", "BDC-6"))
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

baseline_rows <- baseline
baseline_rows$stage_d <- "BDC"
baseline_rows$final_clean_tewl <- baseline_rows$BDC
baseline_rows$percent_change <- 0

plot_df <- subset(plot_df, !(stage_d %in% c("BDC-12", "BDC-6")))
plot_df <- rbind(
  baseline_rows[, c("group", "subject", "stage_d", "final_clean_tewl", "BDC", "percent_change")],
  plot_df[, c("group", "subject", "stage_d", "final_clean_tewl", "BDC", "percent_change")]
)

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)
plot_df$stage_axis <- factor(plot_df$stage_d, levels = stage_order)
plot_df <- plot_df[!is.na(plot_df$stage_axis), ]

p <- ggplot(plot_df, aes(x = stage_axis, y = percent_change, group = subject, color = subject)) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.35) +
  geom_line(linewidth = 0.75, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 1.8, alpha = 0.95, na.rm = TRUE) +
  facet_wrap(~ group, ncol = 1) +
  labs(
    x = NULL,
    y = "Change from subject BDC baseline (%)",
    color = "subject"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.spacing.y = unit(0.9, "lines")
  )

ggsave(output, p, width = 10, height = 9, dpi = 300)
cat(output, "\n")
