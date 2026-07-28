library(readxl)
library(ggplot2)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
input_file <- file.path(root_dir, "Generated outputs", "Corneometer_CM825_Single_Avg_summary.xlsx")
output_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
plot_file <- file.path(output_dir, "Corneometer_Hydration_Avg_delta_from_baseline_two_groups_R.png")
data_file <- file.path(output_dir, "Corneometer_Hydration_Avg_delta_from_baseline_two_groups.xlsx")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25",
  "HDT37", "HDT43", "HDT49", "HDT55"
)
colors <- c(Control = "#2F855A", Countermeasure = "#D62728")

df <- read_excel(input_file, sheet = "Clean Data")
names(df) <- trimws(names(df))

df$Subject <- as.character(df$Subject)
df$Group_original <- as.character(df$Group)
df$Group <- ifelse(df$Group_original %in% c("Ex", "ExAG"), "Countermeasure", df$Group_original)
df$Group <- factor(df$Group, levels = c("Control", "Countermeasure"))
df$Stage <- as.character(df$Stage)
df$Hydration_Avg <- suppressWarnings(as.numeric(df$`Hydration Avg`))

baseline_source <- df[df$Stage %in% c("BDC-12", "BDC-6") & !is.na(df$Hydration_Avg), ]
baseline <- aggregate(
  Hydration_Avg ~ Subject,
  data = baseline_source,
  FUN = mean
)
names(baseline)[names(baseline) == "Hydration_Avg"] <- "Baseline"

plot_df <- merge(df, baseline, by = "Subject", all.x = TRUE)
followup_df <- plot_df[
  plot_df$Stage %in% stage_order &
    !is.na(plot_df$Hydration_Avg) &
    !is.na(plot_df$Baseline) &
    !is.na(plot_df$Group),
]
followup_df$Delta_Hydration_Avg <- followup_df$Hydration_Avg - followup_df$Baseline

baseline_df <- unique(plot_df[, c("Subject", "Group", "Group_original", "Baseline")])
baseline_df <- baseline_df[!is.na(baseline_df$Subject) & !is.na(baseline_df$Group) & !is.na(baseline_df$Baseline), ]
baseline_df$Stage <- "BDC"
baseline_df$Hydration_Avg <- baseline_df$Baseline
baseline_df$Delta_Hydration_Avg <- 0

plot_df <- rbind(
  baseline_df[, c("Subject", "Group", "Group_original", "Stage", "Baseline", "Hydration_Avg", "Delta_Hydration_Avg")],
  followup_df[, c("Subject", "Group", "Group_original", "Stage", "Baseline", "Hydration_Avg", "Delta_Hydration_Avg")]
)
plot_df$Stage <- factor(plot_df$Stage, levels = stage_order)

summary_df <- aggregate(
  Delta_Hydration_Avg ~ Group + Stage,
  data = plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("Group", "Stage", "mean_delta", "se", "n")

p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
  geom_line(
    data = plot_df,
    aes(x = Stage, y = Delta_Hydration_Avg, group = interaction(Group, Subject), color = Group),
    alpha = 0.22,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = plot_df,
    aes(x = Stage, y = Delta_Hydration_Avg, color = Group),
    alpha = 0.35,
    size = 1.35,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = Stage, ymin = mean_delta - se, ymax = mean_delta + se, color = Group),
    width = 0.18,
    linewidth = 0.75,
    na.rm = TRUE
  ) +
  geom_line(
    data = summary_df,
    aes(x = Stage, y = mean_delta, group = Group, color = Group),
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  geom_point(
    data = summary_df,
    aes(x = Stage, y = mean_delta, color = Group),
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_color_manual(values = colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Delta Hydration Avg from subject baseline",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(plot_file, p, width = 11, height = 6.8, dpi = 300)

write_xlsx(
  list(
    Delta_by_subject_stage = plot_df[, c(
      "Subject", "Group", "Group_original", "Stage", "Baseline",
      "Hydration_Avg", "Delta_Hydration_Avg"
    )],
    Group_stage_summary = summary_df
  ),
  data_file
)

cat("Rows plotted:", nrow(plot_df), "\n")
cat("Subjects:", length(unique(plot_df$Subject)), "\n")
cat(plot_file, "\n")
cat(data_file, "\n")
