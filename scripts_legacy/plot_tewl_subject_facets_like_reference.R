library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_file <- file.path(tewl_dir, "TEWL_final_clean_subject_facets_reference_style_R.png")

stage_order <- c(
  "BDC-12", "BDC-6",
  "HDT1", "HDT7", "HDT13", "HDT19",
  "HDT25", "HDT31", "HDT37", "HDT43", "HDT49", "HDT55",
  "R+1", "R+6", "R+12"
)

colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage <- as.character(df[[4]]) # Excel column D: stage
df$stage <- factor(df$stage, levels = stage_order)
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))

plot_df <- df[!is.na(df$stage) & !is.na(df$final_clean_tewl), ]

subject_group <- unique(plot_df[, c("subject", "group")])
subject_group$group <- factor(subject_group$group, levels = c("Control", "Ex", "ExAG"))
subject_group <- subject_group[order(subject_group$group, subject_group$subject), ]
subject_group$within_group_order <- ave(
  seq_len(nrow(subject_group)),
  subject_group$group,
  FUN = seq_along
)
subject_group <- subject_group[order(subject_group$within_group_order, subject_group$group), ]
subject_levels <- subject_group$subject
plot_df$subject <- factor(plot_df$subject, levels = subject_levels)

p <- ggplot(plot_df, aes(x = stage, y = final_clean_tewl, group = subject, color = group)) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  geom_point(size = 1.25, na.rm = TRUE) +
  facet_wrap(~ subject, ncol = 3) +
  scale_color_manual(values = colors, drop = FALSE) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.10))) +
  labs(
    x = NULL,
    y = "Final clean TEWL (g/m2/h)",
    color = "Group"
  ) +
  theme_classic(base_size = 8) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.35),
    strip.text = element_text(face = "bold", size = 7, margin = margin(1, 0, 1, 0)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, size = 5.5),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(face = "bold", size = 8),
    axis.line = element_line(linewidth = 0.25),
    axis.ticks = element_line(linewidth = 0.25),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 7),
    legend.text = element_text(size = 6),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.45, "cm"),
    panel.spacing.x = unit(0.08, "in"),
    panel.spacing.y = unit(0.10, "in"),
    plot.margin = margin(6, 6, 6, 6)
  )

ggsave(output_file, p, width = 7.5, height = 12.5, dpi = 300)
cat(output_file, "\n")
