#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

root <- getwd()
source_csv <- "/Users/ree/Documents/New project/data_clean/Fina_cleaned.csv"
out_dir <- file.path(root, "processed_data", "03_finapress")
figure_dir <- file.path(out_dir, "Figures")
source_dir <- file.path(out_dir, "source_data")
qa_dir <- file.path(out_dir, "QA")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

palette_contract <- c(
  Control = "#2E9D55",
  Countermeasure = "#D64B45"
)

theme_svri_delta <- function(base_size = 7, base_family = "Helvetica") {
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

# Use CSV column D (`stage`) as the only source for time/stage labels.
stage_source <- raw$stage
parsed <- parse_stage(stage_source)

raw_plot_data <- raw %>%
  mutate(
    stage_clean = stage_source,
    phase = parsed$phase,
    stage_day = parsed$stage_day,
    svri = svri_mm_hg_s_ml_m_2,
    group_two = if_else(group == "Control", "Control", "Countermeasure")
  ) %>%
  filter(phase %in% c("BDC", "HDT"), !is.na(svri), !is.na(group_two), !is.na(subject))

bdc_baseline <- raw_plot_data %>%
  filter(phase == "BDC") %>%
  group_by(subject, group_two) %>%
  summarise(
    bdc_svri = mean(svri, na.rm = TRUE),
    bdc_source_stages = paste(sort(unique(stage_clean)), collapse = ";"),
    .groups = "drop"
  ) %>%
  filter(!is.na(bdc_svri), bdc_svri != 0)

hdt_delta <- raw_plot_data %>%
  filter(phase == "HDT") %>%
  inner_join(bdc_baseline, by = c("subject", "group_two")) %>%
  mutate(
    delta_percent_from_bdc = (svri - bdc_svri) / bdc_svri * 100,
    source_stages = stage_clean
  )

bdc_zero <- bdc_baseline %>%
  transmute(
    subject,
    group_two,
    stage_clean = "BDC",
    phase = "BDC",
    stage_day = 0,
    svri = bdc_svri,
    bdc_svri,
    delta_percent_from_bdc = 0,
    source_stages = bdc_source_stages
  )

plot_data <- bind_rows(
  bdc_zero,
  hdt_delta %>%
    select(subject, group_two, stage_clean, phase, stage_day, svri, bdc_svri, delta_percent_from_bdc, source_stages)
) %>%
  mutate(group = factor(group_two, levels = c("Control", "Countermeasure")))

stage_levels <- plot_data %>%
  distinct(stage_clean, phase, stage_day) %>%
  arrange(
    factor(phase, levels = c("BDC", "HDT")),
    stage_day
  ) %>%
  pull(stage_clean)

plot_data <- plot_data %>%
  mutate(stage_clean = factor(stage_clean, levels = stage_levels))

summary_data <- plot_data %>%
  group_by(group, stage_clean) %>%
  summarise(
    n = n(),
    mean_delta_percent = mean(delta_percent_from_bdc, na.rm = TRUE),
    se_delta_percent = sd(delta_percent_from_bdc, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

write_csv(
  plot_data %>%
    select(subject, group, stage = stage_clean, phase, stage_day, svri, bdc_svri, delta_percent_from_bdc, source_stages),
  file.path(source_dir, "SVRI_delta_percent_from_BDC_two_group_source_data.csv")
)
write_csv(summary_data, file.path(source_dir, "SVRI_delta_percent_from_BDC_two_group_summary.csv"))

p <- ggplot(plot_data, aes(x = stage_clean, y = delta_percent_from_bdc, group = subject, colour = group)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#555555") +
  geom_line(alpha = 0.18, linewidth = 0.35) +
  geom_point(alpha = 0.28, size = 0.9) +
  geom_errorbar(
    data = summary_data,
    aes(
      x = stage_clean,
      y = mean_delta_percent,
      ymin = mean_delta_percent - se_delta_percent,
      ymax = mean_delta_percent + se_delta_percent,
      group = group
    ),
    inherit.aes = FALSE,
    width = 0.18,
    linewidth = 0.35,
    colour = "#222222"
  ) +
  geom_line(
    data = summary_data,
    aes(x = stage_clean, y = mean_delta_percent, group = group, colour = group),
    inherit.aes = FALSE,
    linewidth = 0.9
  ) +
  geom_point(
    data = summary_data,
    aes(x = stage_clean, y = mean_delta_percent, colour = group),
    inherit.aes = FALSE,
    size = 1.9,
    stroke = 0.2
  ) +
  scale_colour_manual(values = palette_contract, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.1))) +
  labs(
    title = "SVRI change from baseline during head-down tilt",
    subtitle = "Delta% is relative to each subject's available BDC mean. Countermeasure = Ex + ExAG.",
    x = NULL,
    y = "SVRI delta from BDC (%)"
  ) +
  theme_svri_delta()

base <- file.path(figure_dir, "SVRI_delta_percent_from_BDC_two_groups_countermeasure_R")
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

qa <- list(
  rows_used = nrow(plot_data),
  subjects = dplyr::n_distinct(plot_data$subject),
  subjects_with_bdc = nrow(bdc_baseline),
  groups = paste(levels(plot_data$group), collapse = ", "),
  stages = paste(levels(plot_data$stage_clean), collapse = ", "),
  output_png = paste0(base, ".png"),
  output_pdf = NA_character_,
  color_rule = "Control=green; Countermeasure=red; Countermeasure=Ex+ExAG",
  stage_source = "CSV column D: stage"
)

writeLines(capture.output(str(qa)), file.path(qa_dir, "SVRI_delta_percent_from_BDC_two_groups_countermeasure_R_QA.txt"))
print(qa)
