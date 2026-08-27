library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
input_file <- file.path(root_dir, "Generated outputs", "Corneometer_CM825_Single_Avg_summary.xlsx")
output_dir <- file.path(root_dir, "processed_data", "04_tewameter")
output_file <- file.path(output_dir, "Corneometer_Hydration_Avg_absolute_by_group_R.png")
subject_output_file <- file.path(output_dir, "Corneometer_Hydration_Avg_absolute_each_subject_R.png")
two_group_output_file <- file.path(output_dir, "Corneometer_Hydration_Avg_absolute_two_groups_R.png")
two_group_subject_output_file <- file.path(output_dir, "Corneometer_Hydration_Avg_absolute_each_subject_two_groups_R.png")

stage_order <- c(
  "BDC-12", "BDC-6",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT31",
  "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)
colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")
two_group_colors <- c(Control = "#2F855A", Countermeasure = "#D62728")

df <- read_excel(input_file, sheet = "Clean Data")
names(df) <- trimws(names(df))

df$Subject <- as.character(df$Subject)
df$Group <- factor(as.character(df$Group), levels = c("Control", "Ex", "ExAG"))
df$Group2 <- ifelse(as.character(df$Group) %in% c("Ex", "ExAG"), "Countermeasure", as.character(df$Group))
df$Group2 <- factor(df$Group2, levels = c("Control", "Countermeasure"))
df$Stage <- factor(as.character(df$Stage), levels = stage_order)
df$Hydration_Avg <- suppressWarnings(as.numeric(df$`Hydration Avg`))

plot_df <- df[!is.na(df$Subject) & !is.na(df$Group) & !is.na(df$Stage) & !is.na(df$Hydration_Avg), ]

summary_df <- aggregate(
  Hydration_Avg ~ Group + Stage,
  data = plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("Group", "Stage", "mean", "se", "n")

p <- ggplot() +
  geom_line(
    data = plot_df,
    aes(x = Stage, y = Hydration_Avg, group = interaction(Group, Subject), color = Group),
    alpha = 0.22,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = plot_df,
    aes(x = Stage, y = Hydration_Avg, color = Group),
    alpha = 0.35,
    size = 1.35,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = Stage, ymin = mean - se, ymax = mean + se, color = Group),
    width = 0.18,
    linewidth = 0.75,
    na.rm = TRUE
  ) +
  geom_line(
    data = summary_df,
    aes(x = Stage, y = mean, group = Group, color = Group),
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  geom_point(
    data = summary_df,
    aes(x = Stage, y = mean, color = Group),
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_color_manual(values = colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Hydration Avg (absolute value)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(output_file, p, width = 12, height = 7, dpi = 300)

subject_plot <- ggplot(
  plot_df,
  aes(x = Stage, y = Hydration_Avg, group = Subject, color = Group)
) +
  geom_line(linewidth = 0.65, na.rm = TRUE) +
  geom_point(size = 1.7, na.rm = TRUE) +
  facet_wrap(~ Subject, ncol = 4) +
  scale_color_manual(values = colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Hydration Avg (absolute value)",
    color = "Group"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 7),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(subject_output_file, subject_plot, width = 13, height = 14, dpi = 300)

two_group_summary <- aggregate(
  Hydration_Avg ~ Group2 + Stage,
  data = plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
two_group_summary <- do.call(data.frame, two_group_summary)
names(two_group_summary) <- c("Group2", "Stage", "mean", "se", "n")

two_group_stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT31",
  "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)
two_group_plot_df <- plot_df
two_group_plot_df$Stage2 <- ifelse(
  as.character(two_group_plot_df$Stage) %in% c("BDC-12", "BDC-6"),
  "BDC",
  as.character(two_group_plot_df$Stage)
)
two_group_plot_df$Stage2 <- factor(two_group_plot_df$Stage2, levels = two_group_stage_order)
two_group_plot_df <- aggregate(
  Hydration_Avg ~ Group2 + Subject + Stage2,
  data = two_group_plot_df,
  FUN = function(x) mean(x, na.rm = TRUE)
)

two_group_summary <- aggregate(
  Hydration_Avg ~ Group2 + Stage2,
  data = two_group_plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
two_group_summary <- do.call(data.frame, two_group_summary)
names(two_group_summary) <- c("Group2", "Stage2", "mean", "se", "n")

two_group_plot <- ggplot() +
  geom_line(
    data = two_group_plot_df,
    aes(x = Stage2, y = Hydration_Avg, group = interaction(Group2, Subject), color = Group2),
    alpha = 0.22,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = two_group_plot_df,
    aes(x = Stage2, y = Hydration_Avg, color = Group2),
    alpha = 0.35,
    size = 1.35,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = two_group_summary,
    aes(x = Stage2, ymin = mean - se, ymax = mean + se, color = Group2),
    width = 0.18,
    linewidth = 0.75,
    na.rm = TRUE
  ) +
  geom_line(
    data = two_group_summary,
    aes(x = Stage2, y = mean, group = Group2, color = Group2),
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  geom_point(
    data = two_group_summary,
    aes(x = Stage2, y = mean, color = Group2),
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_color_manual(values = two_group_colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Hydration Avg (absolute value)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(two_group_output_file, two_group_plot, width = 12, height = 7, dpi = 300)

two_group_subject_plot <- ggplot(
  plot_df,
  aes(x = Stage, y = Hydration_Avg, group = Subject, color = Group2)
) +
  geom_line(linewidth = 0.65, na.rm = TRUE) +
  geom_point(size = 1.7, na.rm = TRUE) +
  facet_wrap(~ Subject, ncol = 4) +
  scale_color_manual(values = two_group_colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Hydration Avg (absolute value)",
    color = "Group"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 7),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(two_group_subject_output_file, two_group_subject_plot, width = 13, height = 14, dpi = 300)

cat("Rows plotted:", nrow(plot_df), "\n")
cat("Subjects:", length(unique(plot_df$Subject)), "\n")
cat(output_file, "\n")
cat(subject_output_file, "\n")
cat(two_group_output_file, "\n")
cat(two_group_subject_output_file, "\n")
