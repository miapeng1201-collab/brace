#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

root <- getwd()
source_csv <- "/Users/ree/Documents/New project/data_clean/Fina_cleaned.csv"
out_dir <- file.path(root, "Processed data", "03 - FINAPRESS")
figure_dir <- file.path(out_dir, "Figures")
source_dir <- file.path(out_dir, "Source data")
qa_dir <- file.path(out_dir, "QA")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

palette_contract <- c(
  Control = "#2E9D55",
  Ex = "#D64B45",
  ExAG = "#7B4BB2",
  Countermeasure = "#D64B45"
)

theme_svri <- function(base_size = 7, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 0.6),
      panel.grid.major.y = element_line(linewidth = 0.2, colour = "#E7E7E7"),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle = element_text(size = base_size - 0.2, colour = "#444444"),
      plot.margin = margin(6, 8, 6, 8)
    )
}

parse_stage <- function(stage) {
  phase <- sub("^([A-Za-z]+).*$", "\\1", stage)
  day <- suppressWarnings(as.numeric(sub("^[A-Za-z]+(-?[0-9]+)$", "\\1", stage)))
  data.frame(phase = toupper(phase), stage_day = day)
}

raw <- read_csv(source_csv, show_col_types = FALSE)

stage_source <- raw$stage
parsed <- parse_stage(stage_source)

raw_plot_data <- raw %>%
  mutate(
    stage_clean = stage_source,
    phase = parsed$phase,
    stage_day = parsed$stage_day,
    svri = svri_mm_hg_s_ml_m_2
  ) %>%
  filter(phase %in% c("BDC", "HDT"), !is.na(svri), !is.na(group), !is.na(subject))

bdc_data <- raw_plot_data %>%
  filter(phase == "BDC") %>%
  group_by(subject, group) %>%
  summarise(
    svri = mean(svri, na.rm = TRUE),
    source_stages = paste(sort(unique(stage_clean)), collapse = ";"),
    .groups = "drop"
  ) %>%
  mutate(
    stage_clean = "BDC",
    phase = "BDC",
    stage_day = 0
  )

hdt_data <- raw_plot_data %>%
  filter(phase == "HDT") %>%
  mutate(source_stages = stage_clean)

plot_data <- bind_rows(
  bdc_data %>% select(subject, group, stage_clean, phase, stage_day, svri, source_stages),
  hdt_data %>% select(subject, group, stage_clean, phase, stage_day, svri, source_stages)
)

stage_levels <- plot_data %>%
  distinct(stage_clean, phase, stage_day) %>%
  arrange(
    factor(phase, levels = c("BDC", "HDT")),
    stage_day
  ) %>%
  pull(stage_clean)

plot_data <- plot_data %>%
  mutate(
    stage_clean = factor(stage_clean, levels = stage_levels),
    group = factor(group, levels = intersect(c("Control", "Ex", "ExAG"), unique(group)))
  )

summary_data <- plot_data %>%
  group_by(group, stage_clean) %>%
  summarise(
    n = n(),
    mean_svri = mean(svri, na.rm = TRUE),
    se_svri = sd(svri, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

source_data <- plot_data %>%
  select(subject, group, stage = stage_clean, phase, stage_day, svri, source_stages)

write_csv(source_data, file.path(source_dir, "SVRI_BDC_HDT_source_data.csv"))
write_csv(summary_data, file.path(source_dir, "SVRI_BDC_HDT_group_summary.csv"))

make_svri_plot <- function(data, summary, title, subtitle, palette_values) {
  ggplot(data, aes(x = stage_clean, y = svri, group = subject, colour = group)) +
  geom_line(alpha = 0.18, linewidth = 0.35) +
  geom_point(alpha = 0.28, size = 0.9) +
  geom_errorbar(
    data = summary,
    aes(x = stage_clean, y = mean_svri, ymin = mean_svri - se_svri, ymax = mean_svri + se_svri, group = group),
    inherit.aes = FALSE,
    width = 0.18,
    linewidth = 0.35,
    colour = "#222222"
  ) +
  geom_line(
    data = summary,
    aes(x = stage_clean, y = mean_svri, group = group, colour = group),
    inherit.aes = FALSE,
    linewidth = 0.9
  ) +
  geom_point(
    data = summary,
    aes(x = stage_clean, y = mean_svri, colour = group),
    inherit.aes = FALSE,
    size = 1.9,
    stroke = 0.2
  ) +
  scale_colour_manual(values = palette_values, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.08))) +
  labs(
    title = title,
    subtitle = subtitle,
    x = NULL,
    y = expression("SVRI (mmHg"*"\u00b7"*"s/ml/m"^2*")")
  ) +
  theme_svri()
}

plot_subtitle <- "BDC is the subject-level mean of available baseline days; bold lines show group mean +/- SE."

p <- make_svri_plot(
  plot_data,
  summary_data,
  "SVRI during baseline and head-down tilt",
  plot_subtitle,
  palette_contract[c("Control", "Ex", "ExAG")]
)

base <- file.path(figure_dir, "SVRI_BDC_HDT_by_group_R")
pdf_path <- paste0(base, ".pdf")
if (file.exists(pdf_path)) {
  unlink(pdf_path)
}

png(
  paste0(base, ".png"),
  width = 183 / 25.4,
  height = 105 / 25.4,
  units = "in",
  res = 600,
  type = "quartz",
  bg = "white"
)
print(p)
dev.off()

two_group_data <- plot_data %>%
  mutate(
    group = if_else(as.character(group) == "Control", "Control", "Countermeasure"),
    group = factor(group, levels = c("Control", "Countermeasure"))
  )

two_group_summary <- two_group_data %>%
  group_by(group, stage_clean) %>%
  summarise(
    n = n(),
    mean_svri = mean(svri, na.rm = TRUE),
    se_svri = sd(svri, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

write_csv(two_group_data, file.path(source_dir, "SVRI_BDC_HDT_two_group_source_data.csv"))
write_csv(two_group_summary, file.path(source_dir, "SVRI_BDC_HDT_two_group_summary.csv"))

p_two <- make_svri_plot(
  two_group_data,
  two_group_summary,
  "SVRI during baseline and head-down tilt",
  paste0(plot_subtitle, " Countermeasure = Ex + ExAG."),
  palette_contract[c("Control", "Countermeasure")]
)

base_two <- file.path(figure_dir, "SVRI_BDC_HDT_two_groups_countermeasure_R")
pdf_path_two <- paste0(base_two, ".pdf")
if (file.exists(pdf_path_two)) {
  unlink(pdf_path_two)
}

png(
  paste0(base_two, ".png"),
  width = 183 / 25.4,
  height = 105 / 25.4,
  units = "in",
  res = 600,
  type = "quartz",
  bg = "white"
)
print(p_two)
dev.off()

qa <- list(
  rows_used = nrow(plot_data),
  subjects = dplyr::n_distinct(plot_data$subject),
  groups = paste(levels(plot_data$group), collapse = ", "),
  stages = paste(levels(plot_data$stage_clean), collapse = ", "),
  output_png = paste0(base, ".png"),
  output_two_group_png = paste0(base_two, ".png"),
  output_pdf = NA_character_,
  color_rule = "Control=green; Ex=red; ExAG=purple; Countermeasure=red"
)

writeLines(capture.output(str(qa)), file.path(qa_dir, "SVRI_BDC_HDT_by_group_R_QA.txt"))
print(qa)
