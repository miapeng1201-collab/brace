library(readxl)
library(ggplot2)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")

output_plot <- file.path(tewl_dir, "TEWL_absolute_unmerged_stages_two_groups_exclude_PG_exclude_HDT31_R.png")
output_xlsx <- file.path(tewl_dir, "TEWL_absolute_unmerged_stages_two_groups_exclude_PG_exclude_HDT31_summary.xlsx")

stage_levels <- c("BDC-12", "BDC-6", "HDT1", "HDT7", "HDT13", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55")
colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df <- df[!(df$subject %in% c("P", "G")), ]
df$group_original <- as.character(df$group)
df$group_cm <- ifelse(df$group_original == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = names(colors_two))
df$stage <- as.character(df[[4]]) # Excel column D: stage
df <- df[df$stage %in% stage_levels, ]
df$stage <- factor(df$stage, levels = stage_levels)
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))

analysis_data <- df[complete.cases(df[, c("subject", "group_cm", "stage", "final_clean_tewl")]), ]

subject_stage <- aggregate(
  final_clean_tewl ~ subject + group_cm + stage,
  data = analysis_data,
  FUN = mean
)
subject_stage$stage <- factor(subject_stage$stage, levels = stage_levels)

summary_df <- aggregate(
  final_clean_tewl ~ group_cm + stage,
  data = subject_stage,
  FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "stage", "mean", "sd", "sem", "n")
summary_df$group <- factor(summary_df$group, levels = names(colors_two))
summary_df$stage <- factor(summary_df$stage, levels = stage_levels)

p <- ggplot() +
  geom_line(
    data = subject_stage,
    aes(x = stage, y = final_clean_tewl, group = interaction(subject, group_cm), color = group_cm),
    alpha = 0.16,
    linewidth = 0.32
  ) +
  geom_point(
    data = subject_stage,
    aes(x = stage, y = final_clean_tewl, color = group_cm),
    alpha = 0.32,
    size = 1.25,
    position = position_jitter(width = 0.05, height = 0)
  ) +
  geom_line(
    data = summary_df,
    aes(x = stage, y = mean, group = group, color = group),
    linewidth = 0.95
  ) +
  geom_point(
    data = summary_df,
    aes(x = stage, y = mean, color = group),
    size = 2.55
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = stage, ymin = mean - sem, ymax = mean + sem, color = group),
    width = 0.14,
    linewidth = 0.62
  ) +
  scale_color_manual(values = colors_two) +
  labs(
    x = NULL,
    y = expression(TEWL~"(g/m"^2*"/h)"),
    color = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(face = "bold", angle = 35, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(face = "bold"),
    plot.margin = margin(8, 10, 8, 8)
  )

ggsave(output_plot, p, width = 8.8, height = 4.6, dpi = 320, bg = "white")

model_note <- data.frame(
  item = c("excluded_subjects", "excluded_stage", "groups", "stage_source", "data_level"),
  detail = c(
    "P and G excluded.",
    "HDT31 excluded.",
    "Control vs Countermeasure; Countermeasure = Ex + ExAG.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "If duplicate rows existed for a subject-stage, they were averaged before plotting."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Subject_stage_values = subject_stage,
    Absolute_summary = summary_df
  ),
  output_xlsx
)

cat(output_plot, "\n")
cat(output_xlsx, "\n")
print(summary_df)
