library(readxl)
library(ggplot2)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_three_group <- file.path(tewl_dir, "TEWL_vs_skin_temperature_binned_three_groups_R.png")
output_two_group <- file.path(tewl_dir, "TEWL_vs_skin_temperature_binned_two_groups_R.png")

colors_three <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")
colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p))))
}

fit_pvalues <- function(data, group_col) {
  data <- data[complete.cases(data[, c("subject", "final_clean_tewl", "skin_temperature", group_col)]), ]
  data$subject <- factor(data$subject)
  data[[group_col]] <- factor(data[[group_col]])

  out <- c(temp = NA_real_, group = NA_real_, temp_group = NA_real_)
  if (nrow(data) < 12 || length(unique(data$subject)) < 4 || length(unique(data[[group_col]])) < 2) {
    return(out)
  }

  model <- lmer(
    as.formula(paste0("final_clean_tewl ~ skin_temperature * ", group_col, " + (1 | subject)")),
    data = data,
    REML = FALSE
  )
  a <- as.data.frame(anova(model, type = 3))
  rn <- rownames(a)
  out["temp"] <- a[rn == "skin_temperature", "Pr(>F)"]
  out["group"] <- a[rn == group_col, "Pr(>F)"]
  out["temp_group"] <- a[rn == paste0("skin_temperature:", group_col), "Pr(>F)"]
  out
}

make_summary <- function(data, group_col, bin_width = 0.5) {
  data$skin_bin <- round(data$skin_temperature / bin_width) * bin_width
  data <- data[complete.cases(data[, c("final_clean_tewl", "skin_bin", group_col)]), ]

  summary_df <- aggregate(
    final_clean_tewl ~ skin_bin + data[[group_col]],
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
  )
  names(summary_df)[names(summary_df) == "data[[group_col]]"] <- "group_plot"
  unpacked <- do.call(data.frame, summary_df)
  names(unpacked) <- c("skin_bin", "group_plot", "mean", "sd", "n")
  unpacked$sem <- unpacked$sd / sqrt(unpacked$n)
  unpacked <- unpacked[unpacked$n >= 2, ]
  unpacked
}

make_plot <- function(data, group_col, colors, output_file, panel_label) {
  pvals <- fit_pvalues(data, group_col)
  summary_df <- make_summary(data, group_col)
  summary_df$group_plot <- factor(summary_df$group_plot, levels = names(colors))

  label_text <- paste0(
    "P[temp] ", format_p(pvals["temp"]), "\n",
    "P[group] ", format_p(pvals["group"]), "\n",
    "P[temp*group] ", format_p(pvals["temp_group"])
  )

  x_range <- range(summary_df$skin_bin, na.rm = TRUE)
  y_range <- range(c(summary_df$mean - summary_df$sem, summary_df$mean + summary_df$sem), na.rm = TRUE)
  x_pos <- x_range[2] - 0.32 * diff(x_range)
  y_pos <- y_range[2] + 0.06 * diff(y_range)

  p <- ggplot(summary_df, aes(x = skin_bin, y = mean, color = group_plot, group = group_plot)) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.0) +
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.08, linewidth = 0.55) +
    annotate("text", x = -Inf, y = Inf, label = panel_label, hjust = -0.35, vjust = 1.25, size = 5, fontface = "bold") +
    annotate("text", x = x_pos, y = y_pos, label = label_text, hjust = 0, vjust = 1, size = 3.25, fontface = "bold") +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(28, 34, 0.5)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    labs(
      x = "Skin temperature (C)",
      y = expression(TEWL~"(g/m"^2*"/h)"),
      color = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.position = c(0.15, 0.78),
      legend.background = element_blank(),
      legend.text = element_text(size = 9, face = "bold"),
      legend.key.width = unit(0.45, "cm"),
      plot.margin = margin(10, 12, 8, 8)
    )

  ggsave(output_file, p, width = 5.2, height = 4.0, dpi = 320, bg = "white")
  list(plot = p, pvalues = pvals, summary = summary_df)
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))
df$subject <- as.character(df$subject)
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$group_cm <- ifelse(df$group == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$skin_temperature <- suppressWarnings(as.numeric(to_missing(df$avg_temperature_skin_robust_c)))

plot_data <- df[complete.cases(df[, c("subject", "group", "group_cm", "final_clean_tewl", "skin_temperature")]), ]

three <- make_plot(plot_data, "group", colors_three, output_three_group, "(A)")
two <- make_plot(plot_data, "group_cm", colors_two, output_two_group, "(B)")

cat(output_three_group, "\n")
cat(output_two_group, "\n")
cat("Three-group p-values:\n")
print(three$pvalues)
cat("Two-group p-values:\n")
print(two$pvalues)
