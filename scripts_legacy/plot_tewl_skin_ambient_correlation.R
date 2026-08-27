library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation.xlsx")
out_file <- file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation_R.png")

df <- read_excel(input_file, sheet = "Analysis_data")
names(df) <- trimws(names(df))

df$stage <- as.character(df$stage)
df$phase <- ifelse(df$stage %in% c("BDC-12", "BDC-6"), "BDC",
  ifelse(df$stage %in% c("HDT1", "HDT7", "HDT13", "HDT19"), "HDT1 7 13 19",
    ifelse(df$stage %in% c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"), "HDT25 37 43 49 55", "R")
  )
)
df$phase <- factor(df$phase, levels = c("BDC", "HDT1 7 13 19", "HDT25 37 43 49 55", "R"))

df$plot_group <- ifelse(df$phase == "BDC", "BDC pooled", as.character(df$group))
df$plot_group <- factor(df$plot_group, levels = c("BDC pooled", "Control", "Ex", "ExAG"))

p <- ggplot(df, aes(x = ambient_temperature_take_c, y = avg_temperature_skin_robust_c, color = plot_group)) +
  geom_point(alpha = 0.72, size = 2.1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_wrap(~phase, ncol = 2) +
  scale_color_manual(values = c(`BDC pooled` = "#333333", Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")) +
  labs(
    x = "Ambient temperature (C)",
    y = "Skin temperature (C)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey92", color = "grey75"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )

ggsave(out_file, p, width = 10, height = 7.2, dpi = 300)
cat(out_file, "\n")
