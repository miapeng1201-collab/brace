library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_final_delta_percent_exclude_PG.xlsx")
output_plot <- file.path(tewl_dir, "TEWL_final_delta_percent_from_BDC_by_group_exclude_PG_R.png")

stage_order <- c(
  "BDC",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55"
)
hdt_order <- stage_order[stage_order != "BDC"]
colors <- c(Control = "#2F855A", Ex = "#D62728", ExAG = "#7B2CBF")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))
df$subject <- as.character(df$subject)
df <- df[!(df$subject %in% c("P", "G")), ]
df$group <- factor(as.character(df$group), levels = c("Control", "Ex", "ExAG"))
df$stage_raw <- as.character(df[[4]]) # Excel column D: stage
df$stage_plot <- ifelse(df$stage_raw %in% c("BDC-12", "BDC-6"), "BDC", df$stage_raw)
df$stage_plot <- factor(df$stage_plot, levels = stage_order)
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))

plot_source <- df[!is.na(df$stage_plot) & !is.na(df$final_clean_tewl), ]

stage_values <- aggregate(
  final_clean_tewl ~ group + subject + stage_plot,
  data = plot_source,
  FUN = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  },
  na.action = na.pass
)

baseline <- stage_values[stage_values$stage_plot == "BDC", c("group", "subject", "final_clean_tewl")]
names(baseline)[names(baseline) == "final_clean_tewl"] <- "BDC"

delta_values <- merge(stage_values, baseline, by = c("group", "subject"), all.x = TRUE)
delta_values$delta_percent_from_BDC <- (delta_values$final_clean_tewl - delta_values$BDC) / delta_values$BDC * 100
delta_values <- delta_values[!is.na(delta_values$delta_percent_from_BDC), ]

delta_hdt <- delta_values[delta_values$stage_plot %in% hdt_order & !is.na(delta_values$delta_percent_from_BDC), ]
delta_hdt$subject <- droplevels(factor(delta_hdt$subject))
delta_hdt$group <- droplevels(delta_hdt$group)
delta_hdt$stage_plot <- droplevels(delta_hdt$stage_plot)

fit_interaction <- lmer(delta_percent_from_BDC ~ group * stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_additive <- lmer(delta_percent_from_BDC ~ group + stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)
fit_no_group <- lmer(delta_percent_from_BDC ~ stage_plot + (1 | subject), data = delta_hdt, REML = FALSE)

model_tests <- data.frame(
  outcome = "final_clean_tewl_delta_percent_HDT_exclude_PG",
  test = c("group_main_effect_additive_LRT", "group_by_stage_interaction_LRT"),
  chi_sq = c(
    anova(fit_no_group, fit_additive)$Chisq[2],
    anova(fit_additive, fit_interaction)$Chisq[2]
  ),
  df = c(
    anova(fit_no_group, fit_additive)$Df[2],
    anova(fit_additive, fit_interaction)$Df[2]
  ),
  p = c(
    anova(fit_no_group, fit_additive)[["Pr(>Chisq)"]][2],
    anova(fit_additive, fit_interaction)[["Pr(>Chisq)"]][2]
  ),
  stringsAsFactors = FALSE
)

group_emmeans <- as.data.frame(emmeans(fit_additive, ~ group))
pairwise_groups <- as.data.frame(pairs(emmeans(fit_additive, ~ group), adjust = "tukey"))

stage_group_means <- aggregate(
  delta_percent_from_BDC ~ group + stage_plot,
  data = delta_hdt,
  FUN = function(x) c(mean = mean(x), sd = sd(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
stage_group_means <- do.call(data.frame, stage_group_means)
names(stage_group_means) <- c("group", "stage", "mean_delta_percent", "sd", "se", "n")

summary_df <- aggregate(
  delta_percent_from_BDC ~ group + stage_plot,
  data = delta_values,
  FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x))
)
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "stage_plot", "mean", "se", "n")

p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
  geom_line(
    data = delta_values,
    aes(x = stage_plot, y = delta_percent_from_BDC, group = interaction(group, subject), color = group),
    alpha = 0.18,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(
    data = delta_values,
    aes(x = stage_plot, y = delta_percent_from_BDC, color = group),
    alpha = 0.28,
    size = 1.2,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = stage_plot, ymin = mean - se, ymax = mean + se, color = group),
    width = 0.18,
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  geom_line(
    data = summary_df,
    aes(x = stage_plot, y = mean, group = group, color = group),
    linewidth = 1.15,
    na.rm = TRUE
  ) +
  geom_point(
    data = summary_df,
    aes(x = stage_plot, y = mean, color = group),
    size = 2.4,
    na.rm = TRUE
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = NULL,
    y = "Change from subject BDC baseline (%)",
    color = "Group"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(output_plot, p, width = 11, height = 6.8, dpi = 300)

model_note <- data.frame(
  item = c("excluded_subjects", "BDC_rule", "stages", "model"),
  detail = c(
    "P and G excluded.",
    "Subject BDC baseline is mean of available BDC-12 and BDC-6 final_clean_tewl. No raw-window BDC fallback used after excluding P/G.",
    paste(stage_order, collapse = ", "),
    "HDT stages only: delta_percent_from_BDC ~ group * stage + (1 | subject); additive model used for group emmeans."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Model_tests = model_tests,
    Group_emmeans = group_emmeans,
    Pairwise_groups = pairwise_groups,
    Stage_group_means = stage_group_means,
    Delta_values = delta_values
  ),
  output_xlsx
)

print(model_tests)
print(group_emmeans)
print(pairwise_groups)
cat(output_xlsx, "\n")
cat(output_plot, "\n")
