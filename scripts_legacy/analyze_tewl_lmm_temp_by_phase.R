library(lme4)
library(lmerTest)
library(readxl)
library(writexl)

args_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(args_file), ".."), mustWork = TRUE)
input <- file.path(root, "processed_data", "04_tewameter", "TEWL_processed_raw_window.xlsx")
output <- file.path(root, "processed_data", "04_tewameter", "TEWL_LMM_temperature_by_phase.xlsx")

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

phase_map <- list(
  BDC = c("BDC-12", "BDC-6"),
  HDT_before_HDT25 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  R = c("R+1", "R+6", "R+12")
)

fit_one <- function(dat, phase_name, group_name) {
  dat <- dat[complete.cases(dat[, c("group", "subject", "stage", "final_clean_tewl", "ambient_temperature_take_c", "avg_temperature_skin_robust_c")]), ]
  dat$subject <- droplevels(dat$subject)
  if (nrow(dat) < 6 || length(unique(dat$subject)) < 3) {
    return(data.frame(
      phase = phase_name,
      group = group_name,
      n = nrow(dat),
      n_subjects = length(unique(dat$subject)),
      stages = paste(sort(unique(dat$stage)), collapse = ", "),
      ambient_beta = NA_real_,
      ambient_se = NA_real_,
      ambient_df = NA_real_,
      ambient_t = NA_real_,
      ambient_p_lmerTest = NA_real_,
      ambient_p_lrt = NA_real_,
      skin_beta = NA_real_,
      skin_p_lmerTest = NA_real_,
      subject_random_intercept_sd = NA_real_,
      residual_sd = NA_real_,
      singular_fit = NA,
      note = "Insufficient data for LMM",
      stringsAsFactors = FALSE
    ))
  }

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
    phase = phase_name,
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
    note = "",
    stringsAsFactors = FALSE
  )
}

results <- do.call(
  rbind,
  unlist(
    lapply(names(phase_map), function(phase_name) {
      phase_df <- df[df$stage %in% phase_map[[phase_name]], ]
      lapply(split(phase_df, phase_df$group), function(dat) {
        fit_one(dat, phase_name, as.character(unique(dat$group)))
      })
    }),
    recursive = FALSE
  )
)
rownames(results) <- NULL

model_data <- do.call(
  rbind,
  lapply(names(phase_map), function(phase_name) {
    dat <- df[df$stage %in% phase_map[[phase_name]], ]
    dat$phase <- phase_name
    dat
  })
)
model_data <- model_data[
  complete.cases(model_data[, c("group", "subject", "stage", "final_clean_tewl", "ambient_temperature_take_c", "avg_temperature_skin_robust_c")]),
  c("phase", "group", "subject", "stage", "day", "final_clean_tewl", "ambient_temperature_take_c", "avg_temperature_skin_robust_c", "source_file")
]

write_xlsx(
  list(
    LMM_results = results,
    Model_data = model_data
  ),
  output
)

print(results)
cat(output, "\n")
