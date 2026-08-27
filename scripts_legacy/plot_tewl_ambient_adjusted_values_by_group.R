library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_ambient_temperature_adjusted_values.xlsx")
output_file <- file.path(tewl_dir, "TEWL_ambient_adjusted_values_by_group_R.png")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)

df <- read_excel(input_file, sheet = "TEWL_ambient_adjusted")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_raw <- as.character(df$stage)
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$stage_plot <- factor(df$stage_plot, levels = stage_order)
df$tewl_ambient_adjusted <- suppressWarnings(as.numeric(df$tewl_ambient_adjusted))

plot_source <- df[!is.na(df$stage_plot) & !is.na(df$tewl_ambient_adjusted), ]
plot_df <- aggregate(
  tewl_ambient_adjusted ~ group + subject + stage_plot,
  data = plot_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

summary_df <- aggregate(
  tewl_ambient_adjusted ~ group + stage_plot,
  data = plot_df,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "stage_plot", "mean_tewl", "se", "n")

colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

p <- ggplot() +
  geom_line(
    data = plot_df,
    aes(x = stage_plot, y = tewl_ambient_adjusted, group = interaction(group, subject), color = group),
    alpha = 0.18,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = plot_df,
    aes(x = stage_plot, y = tewl_ambient_adjusted, color = group),
    alpha = 0.28,
    size = 1.2,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = stage_plot, ymin = mean_tewl - se, ymax = mean_tewl + se, color = group),
    width = 0.18,
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  geom_line(
    data = summary_df,
    aes(x = stage_plot, y = mean_tewl, group = group, color = group),
    linewidth = 1.15,
    na.rm = TRUE
  ) +
  geom_point(
    data = summary_df,
    aes(x = stage_plot, y = mean_tewl, color = group),
    size = 2.4,
    na.rm = TRUE
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = NULL,
    y = "Ambient-temperature adjusted TEWL (g/m2/h)",
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
