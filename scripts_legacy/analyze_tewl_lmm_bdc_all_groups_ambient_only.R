library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_LMM_BDC_all_groups_ambient_only.xlsx")

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

df <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
tewl_col <- pick_col(df, c("final_clean_tewl"))
ambient_col <- pick_col(df, c("ambient_temperature_take_c"))

bdc <- df[df[[stage_col]] %in% c("BDC-12", "BDC-6"), ]
bdc[[subject_col]] <- factor(as.character(bdc[[subject_col]]))
bdc[[group_col]] <- factor(as.character(bdc[[group_col]]), levels = c("Control", "Ex", "ExAG"))
bdc[[stage_col]] <- factor(as.character(bdc[[stage_col]]), levels = c("BDC-12", "BDC-6"))
bdc[[tewl_col]] <- suppressWarnings(as.numeric(bdc[[tewl_col]]))
bdc[[ambient_col]] <- suppressWarnings(as.numeric(bdc[[ambient_col]]))
bdc <- bdc[!is.na(bdc[[tewl_col]]) & !is.na(bdc[[ambient_col]]) & !is.na(bdc[[group_col]]), ]
bdc[[subject_col]] <- droplevels(bdc[[subject_col]])
bdc[[group_col]] <- droplevels(bdc[[group_col]])

full_formula <- as.formula(sprintf("%s ~ %s + %s + (1 | %s)", tewl_col, ambient_col, group_col, subject_col))
null_formula <- as.formula(sprintf("%s ~ %s + (1 | %s)", tewl_col, group_col, subject_col))
interaction_formula <- as.formula(sprintf("%s ~ %s * %s + (1 | %s)", tewl_col, ambient_col, group_col, subject_col))

model <- lmer(full_formula, data = bdc, REML = FALSE)
null_model <- lmer(null_formula, data = bdc, REML = FALSE)
interaction_model <- lmer(interaction_formula, data = bdc, REML = FALSE)

coef_table <- as.data.frame(coef(summary(model)))
coef_table$term <- rownames(coef_table)
coef_table <- coef_table[, c("term", setdiff(names(coef_table), "term"))]

interaction_coef <- as.data.frame(coef(summary(interaction_model)))
interaction_coef$term <- rownames(interaction_coef)
interaction_coef <- interaction_coef[, c("term", setdiff(names(interaction_coef), "term"))]

lrt_ambient <- anova(null_model, model)
lrt_interaction <- anova(model, interaction_model)
vc <- as.data.frame(VarCorr(model))

summary_result <- data.frame(
  window = "BDC_all_groups",
  n = nrow(bdc),
  n_subjects = length(unique(bdc[[subject_col]])),
  stages = paste(sort(unique(as.character(bdc[[stage_col]]))), collapse = ", "),
  groups = paste(levels(bdc[[group_col]]), collapse = ", "),
  model = "final_clean_tewl ~ ambient_temperature_take_c + group + (1 | subject)",
  ambient_beta = coef_table$Estimate[coef_table$term == ambient_col],
  ambient_se = coef_table[["Std. Error"]][coef_table$term == ambient_col],
  ambient_df = coef_table$df[coef_table$term == ambient_col],
  ambient_t = coef_table[["t value"]][coef_table$term == ambient_col],
  ambient_p_lmerTest = coef_table[["Pr(>|t|)"]][coef_table$term == ambient_col],
  ambient_p_lrt = lrt_ambient[["Pr(>Chisq)"]][2],
  interaction_p_lrt = lrt_interaction[["Pr(>Chisq)"]][2],
  subject_random_intercept_sd = vc$sdcor[vc$grp == subject_col][1],
  residual_sd = sigma(model),
  singular_fit = isSingular(model),
  stringsAsFactors = FALSE
)

group_counts <- as.data.frame(table(bdc[[group_col]]), stringsAsFactors = FALSE)
names(group_counts) <- c("group", "n")

write_xlsx(list(
  Summary = summary_result,
  Coefficients_group_adjusted = coef_table,
  Interaction_coefficients = interaction_coef,
  Group_counts = group_counts,
  BDC_model_data = bdc
), out_file)

print(summary_result)
cat(out_file, "\n")
