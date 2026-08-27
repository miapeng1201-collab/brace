library(readxl)
library(ggplot2)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_file <- file.path(tewl_dir, "TEWL_vs_skin_temperature_discrete_bins_three_phases_two_groups_R.png")

colors_two <- c(Control = "#2F855A", Countermeasure = "#D62728")
phase_levels <- c("BDC", "HDT1-19", "HDT37-55")
bin_breaks <- seq(28.5, 33.5, by = 0.5)
bin_labels <- paste0(head(bin_breaks, -1), "-", tail(bin_breaks, -1))

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p))))
}

fit_pvalues <- function(data) {
  data <- data[complete.cases(data[, c("subject", "final_clean_tewl", "skin_temperature", "group_cm")]), ]
  data$subject <- factor(data$subject)
  data$group_cm <- factor(data$group_cm, levels = names(colors_two))

  out <- c(temp = NA_real_, group = NA_real_, temp_group = NA_real_)
  if (nrow(data) < 12 || length(unique(data$subject)) < 4 || length(unique(data$group_cm)) < 2) {
    return(out)
  }

  model <- lmer(
    final_clean_tewl ~ skin_temperature * group_cm + (1 | subject),
    data = data,
    REML = FALSE
  )
  a <- as.data.frame(anova(model, type = 3))
  out["temp"] <- a[rownames(a) == "skin_temperature", "Pr(>F)"]
  out["group"] <- a[rownames(a) == "group_cm", "Pr(>F)"]
  out["temp_group"] <- a[rownames(a) == "skin_temperature:group_cm", "Pr(>F)"]
  out
}

make_summary <- function(data) {
  data$skin_bin <- cut(
    data$skin_temperature,
    breaks = bin_breaks,
    labels = bin_labels,
    include.lowest = TRUE,
    right = FALSE
  )
  data <- data[complete.cases(data[, c("final_clean_tewl", "skin_bin", "group_cm")]), ]

  summary_df <- aggregate(
    final_clean_tewl ~ phase + skin_bin + group_cm,
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
  )
  unpacked <- do.call(data.frame, summary_df)
  names(unpacked) <- c("phase", "skin_bin", "group", "mean", "sd", "n")
  unpacked$sem <- unpacked$sd / sqrt(unpacked$n)
  unpacked <- unpacked[unpacked$n >= 2, ]
  unpacked$phase <- factor(unpacked$phase, levels = phase_levels)
  unpacked$skin_bin <- factor(unpacked$skin_bin, levels = bin_labels)
  unpacked$group <- factor(unpacked$group, levels = names(colors_two))
  unpacked
}

build_label_df <- function(plot_data, summary_df) {
  labels <- lapply(phase_levels, function(phase_name) {
    sub <- plot_data[plot_data$phase == phase_name, ]
    sum_sub <- summary_df[summary_df$phase == phase_name, ]
    pvals <- fit_pvalues(sub)
    data.frame(
      phase = phase_name,
      x = 5.2,
      y = max(sum_sub$mean + sum_sub$sem, na.rm = TRUE) * 1.04,
      label = paste0(
        "P[temp] ", format_p(pvals["temp"]), "\n",
        "P[group] ", format_p(pvals["group"]), "\n",
        "P[temp*group] ", format_p(pvals["temp_group"])
      ),
      panel = paste0("(", LETTERS[match(phase_name, phase_levels)], ")"),
      stringsAsFactors = FALSE
    )
  })
  label_df <- do.call(rbind, labels)
  label_df$phase <- factor(label_df$phase, levels = phase_levels)
  label_df
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))
df$subject <- as.character(df$subject)
df$group <- as.character(df$group)
df$group_cm <- ifelse(df$group == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = names(colors_two))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$skin_temperature <- suppressWarnings(as.numeric(to_missing(df$avg_temperature_skin_robust_c)))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13", "HDT19")] <- "HDT1-19"
df$phase[df$stage_d %in% c("HDT37", "HDT43", "HDT49", "HDT55")] <- "HDT37-55"
df$phase <- factor(df$phase, levels = phase_levels)

plot_data <- df[complete.cases(df[, c("subject", "group_cm", "phase", "final_clean_tewl", "skin_temperature")]), ]
summary_df <- make_summary(plot_data)
label_df <- build_label_df(plot_data, summary_df)

p <- ggplot(summary_df, aes(x = skin_bin, y = mean, color = group, group = group)) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.9) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.15, linewidth = 0.48) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 2.65,
    fontface = "bold"
  ) +
  geom_text(
    data = label_df,
    aes(x = -Inf, y = Inf, label = panel),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.25,
    size = 4.4,
    fontface = "bold"
  ) +
  facet_wrap(~ phase, nrow = 1) +
  scale_color_manual(values = colors_two) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.24))) +
  labs(
    x = "Skin temperature bin (C)",
    y = expression(TEWL~"(g/m"^2*"/h)"),
    color = NULL
  ) +
  theme_classic(base_size = 10.5) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 8.2),
    legend.position = "bottom",
    legend.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(output_file, p, width = 13.2, height = 4.5, dpi = 320, bg = "white")

cat(output_file, "\n")
print(label_df[, c("phase", "label")])
