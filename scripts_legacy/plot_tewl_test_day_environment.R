library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_test_day_environment_summary.xlsx")

temp_out <- file.path(tewl_dir, "TEWL_test_day_ambient_temperature_by_subject_R.png")
humidity_out <- file.path(tewl_dir, "TEWL_test_day_ambient_humidity_by_subject_R.png")

stage_levels <- c(
  "BDC-12", "BDC-6",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT31", "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)

df <- read_excel(input_file, sheet = "By_subject_test_day")
df$stage <- factor(df$stage, levels = stage_levels)
df$group <- factor(df$group, levels = c("Control", "Ex", "ExAG"))
df$subject <- factor(df$subject)

base_theme <- theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey92", color = "grey75"),
    strip.text = element_text(face = "bold"),
    panel.spacing.x = unit(1.1, "lines")
  )

make_plot <- function(y_col, y_label, out_file, color) {
  p <- ggplot(df, aes(x = stage, y = .data[[y_col]], group = subject, color = subject)) +
    geom_line(alpha = 0.72, linewidth = 0.65) +
    geom_point(alpha = 0.88, size = 1.8) +
    facet_wrap(~group, ncol = 1) +
    labs(x = "Stage", y = y_label, color = "Subject") +
    base_theme +
    theme(
      strip.text = element_text(size = 12),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.width = unit(0.7, "lines")
    )

  ggsave(out_file, p, width = 13, height = 10.5, dpi = 300)
}

make_plot("ambient_temperature_c", "Ambient temperature (C)", temp_out, "#2B6CB0")
make_plot("ambient_relative_humidity_pct", "Ambient relative humidity (%)", humidity_out, "#2F855A")

cat(temp_out, "\n")
cat(humidity_out, "\n")
