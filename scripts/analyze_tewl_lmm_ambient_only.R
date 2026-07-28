library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_LMM_ambient_temperature_only.xlsx")

windows <- list(
  HDT_after_HDT19 = c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"),
  BDC = c("BDC-12", "BDC-6"),
  HDT_before_HDT25 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  R = c("R+1", "R+6", "R+12")
)

as_missing <- function(x) {
  if (is.numeric(x)) return(is.na(x))
  is.na(x) | trimws(as.character(x)) == "" | toupper(trimws(as.character(x))) %in% c("NA", "N/A")
}

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

df[[subject_col]] <- as.factor(as.character(df[[subject_col]]))
df[[group_col]] <- as.character(df[[group_col]])
df[[stage_col]] <- as.character(df[[stage_col]])
df[[tewl_col]] <- suppressWarnings(as.numeric(df[[tewl_col]]))
df[[ambient_col]] <- suppressWarnings(as.numeric(df[[ambient_col]]))

fit_one <- function(data, window_name, grp, stages) {
  sub <- data[data[[group_col]] == grp & data[[stage_col]] %in% stages, ]
  sub <- sub[!is.na(sub[[tewl_col]]) & !is.na(sub[[ambient_col]]) & !as_missing(sub[[subject_col]]), ]
  sub[[subject_col]] <- droplevels(as.factor(sub[[subject_col]]))

  base <- data.frame(
    analysis_window = window_name,
    group = grp,
    n = nrow(sub),
    n_subjects = length(unique(sub[[subject_col]])),
    stages = paste(sort(unique(sub[[stage_col]])), collapse = ", "),
    ambient_beta = NA_real_,
    ambient_se = NA_real_,
    ambient_df = NA_real_,
    ambient_t = NA_real_,
    ambient_p_lmerTest = NA_real_,
    ambient_p_lrt = NA_real_,
    subject_random_intercept_sd = NA_real_,
    residual_sd = NA_real_,
    singular_fit = NA,
    note = "",
    stringsAsFactors = FALSE
  )

  if (nrow(sub) < 6 || length(unique(sub[[subject_col]])) < 3 || length(unique(sub[[ambient_col]])) < 2) {
    base$note <- "insufficient data or no ambient variation"
    return(base)
  }

  formula_full <- as.formula(sprintf("%s ~ %s + (1 | %s)", tewl_col, ambient_col, subject_col))
  formula_null <- as.formula(sprintf("%s ~ 1 + (1 | %s)", tewl_col, subject_col))

  tryCatch({
    model <- lmer(formula_full, data = sub, REML = FALSE)
    null_model <- lmer(formula_null, data = sub, REML = FALSE)
    sm <- summary(model)
    coef_table <- as.data.frame(coef(sm))
    ambient_row <- coef_table[ambient_col, , drop = FALSE]
    lrt <- anova(null_model, model)
    vc <- as.data.frame(VarCorr(model))
    base$ambient_beta <- ambient_row[["Estimate"]]
    base$ambient_se <- ambient_row[["Std. Error"]]
    base$ambient_df <- if ("df" %in% names(ambient_row)) ambient_row[["df"]] else NA_real_
    base$ambient_t <- ambient_row[["t value"]]
    base$ambient_p_lmerTest <- ambient_row[["Pr(>|t|)"]]
    base$ambient_p_lrt <- lrt[["Pr(>Chisq)"]][2]
    base$subject_random_intercept_sd <- vc$sdcor[vc$grp == as.character(subject_col)][1]
    base$residual_sd <- sigma(model)
    base$singular_fit <- isSingular(model)
    base
  }, error = function(e) {
    base$note <- paste("model error:", conditionMessage(e))
    base
  })
}

groups <- sort(unique(df[[group_col]]))
results <- list()
i <- 1
for (window_name in names(windows)) {
  for (grp in groups) {
    results[[i]] <- fit_one(df, window_name, grp, windows[[window_name]])
    i <- i + 1
  }
}

lmm_results <- do.call(rbind, results)

model_note <- data.frame(
  item = c("model", "outcome", "fixed_effect", "random_effect", "stage_source", "windows"),
  detail = c(
    "Linear mixed model fitted separately by group and analysis window",
    "final_clean_tewl",
    "ambient_temperature_take_c only; skin temperature not included",
    "subject random intercept",
    "D-column stage from RawWindowSummaryClean",
    paste(names(windows), collapse = "; ")
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Model_note = model_note,
  LMM_results = lmm_results
), out_file)

print(lmm_results)
cat(out_file, "\n")
