library(readxl)
library(ggplot2)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")

output_three_group <- file.path(tewl_dir, "TEWL_absolute_BDC_HDTE_HDTM_HDTL_three_groups_exclude_PG_R.png")
output_two_group <- file.path(tewl_dir, "TEWL_absolute_BDC_HDTE_HDTM_HDTL_two_groups_exclude_PG_R.png")
output_xlsx <- file.path(tewl_dir, "TEWL_absolute_BDC_HDTE_HDTM_HDTL_exclude_PG_summary.xlsx")

phase_levels <- c("BDC", "HDT-E", "HDT-M", "HDT-L")
colors_three <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")
colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

summarize_group <- function(data, group_col) {
  out <- aggregate(
    final_clean_tewl ~ phase + data[[group_col]],
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), sem = sem(x), n = length(x))
  )
  names(out)[names(out) == "data[[group_col]]"] <- "group_plot"
  unpacked <- do.call(data.frame, out)
  names(unpacked) <- c("phase", "group", "mean", "sd", "sem", "n")
  unpacked$phase <- factor(unpacked$phase, levels = phase_levels)
  unpacked
}

plot_absolute <- function(subject_phase, summary_df, group_col, colors, output_file) {
  subject_phase$group_plot <- subject_phase[[group_col]]
  subject_phase$group_plot <- factor(subject_phase$group_plot, levels = names(colors))
  summary_df$group <- factor(summary_df$group, levels = names(colors))

  p <- ggplot() +
    geom_line(
      data = subject_phase,
      aes(x = phase, y = final_clean_tewl, group = interaction(subject, group_plot), color = group_plot),
      alpha = 0.18,
      linewidth = 0.35
    ) +
    geom_point(
      data = subject_phase,
      aes(x = phase, y = final_clean_tewl, color = group_plot),
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
    scale_color_manual(values = colors) +
    labs(
      x = NULL,
      y = expression(TEWL~"(g/m"^2*"/h)"),
      color = NULL
    ) +
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
df$group <- factor(as.character(df$group), levels = names(colors_three))
df$group_cm <- ifelse(df$group == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = names(colors_two))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT7", "HDT13", "HDT19")] <- "HDT-E"
df$phase[df$stage_d %in% c("HDT25", "HDT31", "HDT37")] <- "HDT-M"
df$phase[df$stage_d %in% c("HDT43", "HDT49", "HDT55")] <- "HDT-L"
df$phase <- factor(df$phase, levels = phase_levels)

analysis_data <- df[complete.cases(df[, c("subject", "group", "group_cm", "phase", "final_clean_tewl")]), ]

subject_phase <- aggregate(
  final_clean_tewl ~ subject + group + group_cm + phase,
  data = analysis_data,
  FUN = mean
)
subject_phase$phase <- factor(subject_phase$phase, levels = phase_levels)

three_summary <- summarize_group(subject_phase, "group")
two_summary <- summarize_group(subject_phase, "group_cm")

plot_absolute(subject_phase, three_summary, "group", colors_three, output_three_group)
plot_absolute(subject_phase, two_summary, "group_cm", colors_two, output_two_group)

model_note <- data.frame(
  item = c("excluded_subjects", "outcome", "stage_source", "phase_definition", "subject_level_summary"),
  detail = c(
    "P and G excluded.",
    "final_clean_tewl absolute values.",
    "D-column stage from RawWindowSummaryClean; real stage ignored.",
    "BDC = BDC-12 and BDC-6; HDT-E = HDT7, HDT13, HDT19; HDT-M = HDT25, HDT31, HDT37; HDT-L = HDT43, HDT49, HDT55.",
    "For each subject, repeated stages within each phase were averaged first; group mean and SEM were then calculated across subjects."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Subject_phase_values = subject_phase,
    Three_group_summary = three_summary,
    Two_group_summary = two_summary
  ),
  output_xlsx
)

cat(output_three_group, "\n")
cat(output_two_group, "\n")
cat(output_xlsx, "\n")
print(three_summary)
print(two_summary)
