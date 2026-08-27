library(ggplot2)
library(readxl)

args_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(args_file), ".."), mustWork = TRUE)
input <- file.path(root, "processed_data", "04_tewameter", "TEWL_processed_raw_window.xlsx")
output <- file.path(root, "processed_data", "04_tewameter", "TEWL_final_clean_subject_trends_R.png")

df <- read_excel(input, sheet = "RawWindowSummaryClean")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$subject <- as.character(df$subject)
df$group <- factor(df$group, levels = c("Control", "Ex", "ExAG"))
df$plot_stage <- as.character(df[[4]]) # Excel column D: stage

stage_order <- data.frame(plot_stage = unique(df$plot_stage), stringsAsFactors = FALSE)
stage_order$stage_id <- seq_len(nrow(stage_order))
df <- merge(df, stage_order, by = "plot_stage", all.x = TRUE)
df$stage_axis <- factor(df$plot_stage, levels = stage_order$plot_stage)

p <- ggplot(df, aes(x = stage_axis, y = final_clean_tewl, group = subject, color = subject)) +
  geom_line(linewidth = 0.75, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 1.8, alpha = 0.95, na.rm = TRUE) +
  facet_wrap(~ group, ncol = 1) +
  labs(
    x = NULL,
    y = expression("final_clean_tewl (g/m"^2*"/h)"),
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
