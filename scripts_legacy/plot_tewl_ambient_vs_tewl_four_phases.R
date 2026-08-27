library(readxl)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_file <- file.path(tewl_dir, "TEWL_ambient_temperature_vs_final_clean_tewl_four_phases_R.png")

colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$ambient_temperature_take_c <- suppressWarnings(as.numeric(df$ambient_temperature_take_c))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13", "HDT19")] <- "HDT1 7 13 19"
df$phase[df$stage_d %in% c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55")] <- "HDT25 37 43 49 55"
df$phase[df$stage_d %in% c("R+1", "R+6", "R+12")] <- "R"
df$phase <- factor(df$phase, levels = c("BDC", "HDT1 7 13 19", "HDT25 37 43 49 55", "R"))

plot_df <- df[!is.na(df$phase) & !is.na(df$final_clean_tewl) & !is.na(df$ambient_temperature_take_c), ]

stat_rows <- list()
idx <- 1
for (phase_name in levels(plot_df$phase)) {
  for (group_name in levels(plot_df$group)) {
    sub <- plot_df[plot_df$phase == phase_name & plot_df$group == group_name, ]
    sub <- sub[complete.cases(sub[, c("ambient_temperature_take_c", "final_clean_tewl")]), ]
    if (nrow(sub) < 3 || length(unique(sub$ambient_temperature_take_c)) < 2) {
      beta <- NA_real_
      p_value <- NA_real_
    } else {
      fit <- lm(final_clean_tewl ~ ambient_temperature_take_c, data = sub)
      coef_table <- as.data.frame(coef(summary(fit)))
      beta <- coef_table["ambient_temperature_take_c", "Estimate"]
      p_value <- coef_table["ambient_temperature_take_c", "Pr(>|t|)"]
    }
    stat_rows[[idx]] <- data.frame(
      phase = phase_name,
      group = group_name,
      label = ifelse(
        is.na(beta),
        paste0(group_name, ": beta=NA, p=NA"),
        paste0(group_name, ": beta=", sprintf("%.2f", beta), ", p=", ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))
      ),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}
stats_df <- do.call(rbind, stat_rows)
stats_df$phase <- factor(stats_df$phase, levels = levels(plot_df$phase))
stats_df$group <- factor(stats_df$group, levels = levels(plot_df$group))
global_x_min <- min(plot_df$ambient_temperature_take_c, na.rm = TRUE)
global_x_max <- max(plot_df$ambient_temperature_take_c, na.rm = TRUE)
global_y_min <- min(plot_df$final_clean_tewl, na.rm = TRUE)
global_y_max <- max(plot_df$final_clean_tewl, na.rm = TRUE)
global_y_range <- global_y_max - global_y_min
stats_df$x <- global_x_min + 0.03 * (global_x_max - global_x_min)
stats_df$y <- global_y_max - (as.numeric(stats_df$group) - 1) * 0.055 * global_y_range

p <- ggplot(plot_df, aes(x = ambient_temperature_take_c, y = final_clean_tewl, color = group)) +
  geom_point(alpha = 0.7, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.75) +
  geom_text(
    data = stats_df,
    aes(x = x, y = y, label = label, color = group),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ phase, ncol = 2) +
  scale_color_manual(values = colors) +
  labs(
    x = "Ambient temperature (C)",
    y = "Final clean TEWL (g/m2/h)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey92", color = "grey75"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(output_file, p, width = 10, height = 7.2, dpi = 300)
cat(output_file, "\n")
