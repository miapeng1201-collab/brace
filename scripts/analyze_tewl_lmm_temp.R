library(lme4)
library(lmerTest)
library(readxl)
library(writexl)

args_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(args_file), ".."), mustWork = TRUE)
input <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_processed_raw_window.xlsx")
output <- file.path(root, "Processed data", "04 - TEWAMETER", "TEWL_LMM_temperature_after_HDT19.xlsx")

df <- read_excel(input, sheet = "RawWindowSummaryClean")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$ambient_temperature_take_c <- suppressWarnings(as.numeric(to_missing(df$ambient_temperature_take_c)))
df$avg_temperature_skin_robust_c <- suppressWarnings(as.numeric(to_missing(df$avg_temperature_skin_robust_c)))
df$subject <- as.factor(as.character(df$subject))
df$group <- as.factor(as.character(df$group))
df$stage <- as.character(df[[4]]) # Excel D column: stage

stages_after_hdt19 <- c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55")
model_df <- df[df$stage %in% stages_after_hdt19, ]
model_df <- model_df[
  complete.cases(model_df[, c("group", "subject", "stage", "final_clean_tewl", "ambient_temperature_take_c", "avg_temperature_skin_robust_c")]),
]

fit_one_group <- function(dat, group_name) {
  dat$subject <- droplevels(dat$subject)
  full <- lmer(
    final_clean_tewl ~ ambient_temperature_take_c + avg_temperature_skin_robust_c + (1 | subject),
    data = dat,
    REML = FALSE
  )
  reduced <- lmer(
    final_clean_tewl ~ avg_temperature_skin_robust_c + (1 | subject),
    data = dat,
    REML = FALSE
  )
  coef_table <- as.data.frame(summary(full)$coefficients)
  ambient <- coef_table["ambient_temperature_take_c", , drop = FALSE]
  skin <- coef_table["avg_temperature_skin_robust_c", , drop = FALSE]
  lrt <- anova(reduced, full)
  varcorr <- as.data.frame(VarCorr(full))
  random_sd <- varcorr$sdcor[varcorr$grp == "subject"][1]
  residual_sd <- sigma(full)
  data.frame(
    group = group_name,
    n = nrow(dat),
    n_subjects = length(unique(dat$subject)),
    stages = paste(sort(unique(dat$stage)), collapse = ", "),
    ambient_beta = ambient$Estimate,
    ambient_se = ambient$`Std. Error`,
    ambient_df = ambient$df,
    ambient_t = ambient$`t value`,
    ambient_p_lmerTest = ambient$`Pr(>|t|)`,
    ambient_p_lrt = lrt$`Pr(>Chisq)`[2],
    skin_beta = skin$Estimate,
    skin_p_lmerTest = skin$`Pr(>|t|)`,
    subject_random_intercept_sd = random_sd,
    residual_sd = residual_sd,
    singular_fit = isSingular(full),
    stringsAsFactors = FALSE
  )
}

results <- do.call(
  rbind,
  lapply(split(model_df, model_df$group), function(dat) fit_one_group(dat, as.character(unique(dat$group))))
)
rownames(results) <- NULL

model_data <- model_df[, c(
  "group",
  "subject",
  "stage",
  "day",
  "final_clean_tewl",
  "ambient_temperature_take_c",
  "avg_temperature_skin_robust_c",
  "source_file"
)]

write_xlsx(
  list(
    LMM_results = results,
    Model_data = model_data
  ),
  output
)

print(results)
cat(output, "\n")
