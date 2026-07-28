library(readxl)
library(lmerTest)
library(emmeans)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
input_file <- file.path(root_dir, "Generated outputs", "Corneometer_CM825_Single_Avg_summary.xlsx")
output_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
output_file <- file.path(output_dir, "Corneometer_Hydration_Avg_LMM_two_groups_baseline_adjusted.xlsx")

stage_order <- c(
  "Baseline",
  "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37",
  "HDT43", "HDT49", "HDT55"
)

df <- read_excel(input_file, sheet = "Clean Data")
names(df) <- trimws(names(df))

df$Subject <- factor(as.character(df$Subject))
df$Group_original <- as.character(df$Group)
df$Group <- ifelse(df$Group_original %in% c("Ex", "ExAG"), "Countermeasure", df$Group_original)
df$Group <- factor(df$Group, levels = c("Control", "Countermeasure"))
df$Time_raw <- as.character(df$Stage)
df$Hydration_Avg <- suppressWarnings(as.numeric(df$`Hydration Avg`))

baseline_source <- df[df$Time_raw %in% c("BDC-12", "BDC-6") & !is.na(df$Hydration_Avg), ]
baseline <- aggregate(
  Hydration_Avg ~ Subject,
  data = baseline_source,
  FUN = mean
)
names(baseline)[names(baseline) == "Hydration_Avg"] <- "Baseline"

model_df <- merge(df, baseline, by = "Subject", all.x = TRUE)

baseline_rows <- unique(model_df[, c("Subject", "Group", "Baseline")])
baseline_rows <- baseline_rows[!is.na(baseline_rows$Baseline) & !is.na(baseline_rows$Group), ]
baseline_rows$Time <- "Baseline"
baseline_rows$Hydration_Avg <- baseline_rows$Baseline

followup_rows <- model_df[
  model_df$Time_raw %in% stage_order &
    model_df$Time_raw != "HDT31" &
    !is.na(model_df$Hydration_Avg) &
    !is.na(model_df$Baseline) &
    !is.na(model_df$Group),
  c("Subject", "Group", "Time_raw", "Hydration_Avg", "Baseline")
]
names(followup_rows)[names(followup_rows) == "Time_raw"] <- "Time"

model_df <- rbind(
  baseline_rows[, c("Subject", "Group", "Time", "Hydration_Avg", "Baseline")],
  followup_rows[, c("Subject", "Group", "Time", "Hydration_Avg", "Baseline")]
)
model_df$Time <- factor(model_df$Time, levels = stage_order)

options(contrasts = c("contr.sum", "contr.poly"))
fit <- lmer(Hydration_Avg ~ Group * Time + Baseline + (1 | Subject), data = model_df, REML = FALSE)

anova_type3 <- as.data.frame(anova(fit, type = 3))
anova_type3$term <- rownames(anova_type3)
anova_type3 <- anova_type3[, c("term", setdiff(names(anova_type3), "term"))]

fixed_effects <- as.data.frame(coef(summary(fit)))
fixed_effects$term <- rownames(fixed_effects)
fixed_effects <- fixed_effects[, c("term", setdiff(names(fixed_effects), "term"))]

emm <- emmeans(fit, ~ Group | Time)
emm_df <- as.data.frame(emm)
group_contrasts <- as.data.frame(contrast(emm, method = "revpairwise", by = "Time"))

model_counts <- aggregate(
  Hydration_Avg ~ Group + Time,
  data = model_df,
  FUN = length
)
names(model_counts)[names(model_counts) == "Hydration_Avg"] <- "n"

baseline_summary <- aggregate(
  Baseline ~ Group,
  data = unique(model_df[, c("Subject", "Group", "Baseline")]),
  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
)
baseline_summary <- do.call(data.frame, baseline_summary)
names(baseline_summary) <- c("Group", "baseline_mean", "baseline_sd", "n_subjects")

notes <- data.frame(
  item = c(
    "input",
    "outcome",
    "model",
    "group_recode",
    "baseline",
    "excluded_time",
    "baseline_in_time",
    "model_rows",
    "subjects",
    "REML"
  ),
  value = c(
    input_file,
    "Hydration Avg absolute value",
    "Hydration Avg ~ Group * Time + Baseline + (1 | Subject)",
    "Ex and ExAG merged as Countermeasure",
    "Subject mean of BDC-12 and BDC-6 Hydration Avg",
    "HDT31 and recovery stages R+1/R+6/R+12 excluded as requested",
    "Baseline included as a Time level; Baseline value is subject mean of BDC-12 and BDC-6",
    as.character(nrow(model_df)),
    as.character(length(unique(model_df$Subject))),
    "FALSE"
  )
)

write_xlsx(
  list(
    Notes = notes,
    Model_data_counts = model_counts,
    Baseline_summary = baseline_summary,
    ANOVA_type3 = anova_type3,
    Fixed_effects = fixed_effects,
    EMM_Group_by_Time = emm_df,
    Group_contrasts_by_Time = group_contrasts
  ),
  output_file
)

cat("Model rows:", nrow(model_df), "\n")
cat("Subjects:", length(unique(model_df$Subject)), "\n")
cat("Output:", output_file, "\n")
print(anova_type3)
