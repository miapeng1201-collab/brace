library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
output_xlsx <- file.path(tewl_dir, "TEWL_skin_temperature_within_between_LMM_two_groups_exclude_PG.xlsx")

phase_levels <- c("BDC", "HDT1 7 13 19", "HDT37 43 49 55")

to_missing <- function(x) {
  ifelse(is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "n/a"), NA, x)
}

decompose_skin_temperature <- function(data) {
  subject_mean <- aggregate(
    skin_temperature ~ subject,
    data = data,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(subject_mean)[names(subject_mean) == "skin_temperature"] <- "subject_mean_skin_temperature"
  data <- merge(data, subject_mean, by = "subject", all.x = TRUE, sort = FALSE)
  grand_mean <- mean(data$skin_temperature, na.rm = TRUE)
  data$grand_mean_skin_temperature <- grand_mean
  data$skin_temperature_within <- data$skin_temperature - data$subject_mean_skin_temperature
  data$skin_temperature_between <- data$subject_mean_skin_temperature - grand_mean
  data
}

fit_lmm_within_between <- function(data, phase_name, group_name) {
  sub <- data[data$phase == phase_name & data$group_cm == group_name, ]
  sub <- sub[complete.cases(sub[, c(
    "subject",
    "final_clean_tewl",
    "skin_temperature",
    "skin_temperature_within",
    "skin_temperature_between"
  )]), ]
  sub$subject <- droplevels(factor(sub$subject))

  base <- data.frame(
    phase = phase_name,
    group = group_name,
    n = nrow(sub),
    n_subjects = length(unique(sub$subject)),
    term = c("skin_temperature_within", "skin_temperature_between"),
    beta = NA_real_,
    se = NA_real_,
    df = NA_real_,
    t = NA_real_,
    p_lmerTest = NA_real_,
    subject_random_intercept_sd = NA_real_,
    residual_sd = NA_real_,
    singular_fit = NA,
    note = "",
    stringsAsFactors = FALSE
  )

  if (
    nrow(sub) < 6 ||
    length(unique(sub$subject)) < 3 ||
    length(unique(sub$skin_temperature_within)) < 2 ||
    length(unique(sub$skin_temperature_between)) < 2
  ) {
    base$note <- "insufficient data or no within/between skin-temperature variation"
    return(base)
  }

  tryCatch({
    model <- lmer(
      final_clean_tewl ~ skin_temperature_within + skin_temperature_between + (1 | subject),
      data = sub,
      REML = FALSE
    )
    coef_table <- as.data.frame(coef(summary(model)))
    vc <- as.data.frame(VarCorr(model))
    for (term_name in base$term) {
      row_idx <- base$term == term_name
      if (term_name %in% rownames(coef_table)) {
        base$beta[row_idx] <- coef_table[term_name, "Estimate"]
        base$se[row_idx] <- coef_table[term_name, "Std. Error"]
        base$df[row_idx] <- if ("df" %in% names(coef_table)) coef_table[term_name, "df"] else NA_real_
        base$t[row_idx] <- coef_table[term_name, "t value"]
        base$p_lmerTest[row_idx] <- coef_table[term_name, "Pr(>|t|)"]
      }
    }
    base$subject_random_intercept_sd <- vc$sdcor[vc$grp == "subject"][1]
    base$residual_sd <- sigma(model)
    base$singular_fit <- isSingular(model)
    base
  }, error = function(e) {
    base$note <- paste("model error:", conditionMessage(e))
    base
  })
}

df <- read_excel(input_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

df$subject <- as.character(df$subject)
df <- df[!(df$subject %in% c("P", "G")), ]
df$group_original <- as.character(df$group)
df$group_cm <- ifelse(df$group_original == "Control", "Control", "Countermeasure")
df$group_cm <- factor(df$group_cm, levels = c("Control", "Countermeasure"))
df$stage_d <- as.character(df[[4]]) # Excel column D: stage
df$final_clean_tewl <- suppressWarnings(as.numeric(to_missing(df$final_clean_tewl)))
df$skin_temperature <- suppressWarnings(as.numeric(to_missing(df$avg_temperature_skin_robust_c)))

df$phase <- NA_character_
df$phase[df$stage_d %in% c("BDC-12", "BDC-6")] <- "BDC"
df$phase[df$stage_d %in% c("HDT1", "HDT7", "HDT13", "HDT19")] <- "HDT1 7 13 19"
df$phase[df$stage_d %in% c("HDT37", "HDT43", "HDT49", "HDT55")] <- "HDT37 43 49 55"
df$phase <- factor(df$phase, levels = phase_levels)

analysis_data <- df[!is.na(df$phase) & !is.na(df$final_clean_tewl) & !is.na(df$skin_temperature), ]

decomposed_chunks <- list()
chunk_idx <- 1
for (phase_name in phase_levels) {
  for (group_name in levels(analysis_data$group_cm)) {
    sub <- analysis_data[analysis_data$phase == phase_name & analysis_data$group_cm == group_name, ]
    decomposed_chunks[[chunk_idx]] <- decompose_skin_temperature(sub)
    chunk_idx <- chunk_idx + 1
  }
}
analysis_decomposed <- do.call(rbind, decomposed_chunks)

model_rows <- list()
idx <- 1
for (phase_name in phase_levels) {
  for (group_name in levels(analysis_decomposed$group_cm)) {
    model_rows[[idx]] <- fit_lmm_within_between(analysis_decomposed, phase_name, group_name)
    idx <- idx + 1
  }
}
lmm_results <- do.call(rbind, model_rows)

model_note <- data.frame(
  item = c("excluded_subjects", "group_recode", "phases", "model", "within_definition", "between_definition", "decomposition_scope", "stage_source"),
  detail = c(
    "P and G excluded.",
    "Ex and ExAG merged as Countermeasure; Control remains Control.",
    "BDC; HDT1 7 13 19; HDT37 43 49 55.",
    "final_clean_tewl ~ skin_temperature_within + skin_temperature_between + (1 | subject), fitted separately by phase and two-group.",
    "skin_temperature_within = each observation skin temperature - that subject's mean skin temperature within the same phase and two-group analysis set.",
    "skin_temperature_between = that subject's mean skin temperature - grand mean skin temperature within the same phase and two-group analysis set.",
    "Within/between decomposition was recalculated separately for each phase x group model.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    LMM_within_between = lmm_results,
    Analysis_data_within_between = analysis_decomposed
  ),
  output_xlsx
)

print(lmm_results)
cat(output_xlsx, "\n")
