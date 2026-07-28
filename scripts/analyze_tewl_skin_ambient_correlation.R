library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation.xlsx")

windows <- list(
  All = c("BDC-12", "BDC-6", "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT37", "HDT43", "HDT49", "HDT55", "R+1", "R+6", "R+12"),
  BDC = c("BDC-12", "BDC-6"),
  HDT_before_HDT25 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  HDT_after_HDT19 = c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"),
  R = c("R+1", "R+6", "R+12")
)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

safe_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 4 || length(unique(x[ok])) < 2 || length(unique(y[ok])) < 2) {
    return(data.frame(n = sum(ok), r = NA_real_, p = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  ct <- cor.test(x[ok], y[ok], method = "pearson")
  data.frame(
    n = sum(ok),
    r = unname(ct$estimate),
    p = ct$p.value,
    ci_low = ct$conf.int[[1]],
    ci_high = ct$conf.int[[2]]
  )
}

fit_mixed <- function(data, window_name, grp = "All") {
  sub <- data
  if (grp != "All") sub <- sub[sub[[group_col]] == grp, ]
  sub <- sub[complete.cases(sub[, c(subject_col, skin_col, ambient_col)]), ]
  sub[[subject_col]] <- droplevels(factor(as.character(sub[[subject_col]])))
  out <- data.frame(
    window = window_name,
    group = grp,
    n = nrow(sub),
    n_subjects = length(unique(sub[[subject_col]])),
    ambient_beta = NA_real_,
    ambient_se = NA_real_,
    ambient_df = NA_real_,
    ambient_t = NA_real_,
    ambient_p = NA_real_,
    subject_random_intercept_sd = NA_real_,
    residual_sd = NA_real_,
    singular_fit = NA,
    note = "",
    stringsAsFactors = FALSE
  )
  if (nrow(sub) < 6 || length(unique(sub[[subject_col]])) < 3 || length(unique(sub[[ambient_col]])) < 2) {
    out$note <- "insufficient data or no ambient variation"
    return(out)
  }
  fm <- as.formula(sprintf("%s ~ %s + (1 | %s)", skin_col, ambient_col, subject_col))
  tryCatch({
    model <- lmer(fm, data = sub, REML = FALSE)
    coef_table <- as.data.frame(coef(summary(model)))
    vc <- as.data.frame(VarCorr(model))
    out$ambient_beta <- coef_table[ambient_col, "Estimate"]
    out$ambient_se <- coef_table[ambient_col, "Std. Error"]
    out$ambient_df <- if ("df" %in% names(coef_table)) coef_table[ambient_col, "df"] else NA_real_
    out$ambient_t <- coef_table[ambient_col, "t value"]
    out$ambient_p <- coef_table[ambient_col, "Pr(>|t|)"]
    out$subject_random_intercept_sd <- vc$sdcor[vc$grp == subject_col][1]
    out$residual_sd <- sigma(model)
    out$singular_fit <- isSingular(model)
    out
  }, error = function(e) {
    out$note <- paste("model error:", conditionMessage(e))
    out
  })
}

df <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
day_col <- pick_col(df, c("day", "Day"))
ambient_col <- pick_col(df, c("ambient_temperature_take_c"))
skin_col <- pick_col(df, c("avg_temperature_skin_robust_c"))

df[[subject_col]] <- as.character(df[[subject_col]])
df[[group_col]] <- as.character(df[[group_col]])
df[[stage_col]] <- as.character(df[[stage_col]])
df[[ambient_col]] <- suppressWarnings(as.numeric(df[[ambient_col]]))
df[[skin_col]] <- suppressWarnings(as.numeric(df[[skin_col]]))

analysis_data <- df[complete.cases(df[, c(subject_col, group_col, stage_col, ambient_col, skin_col)]), ]
analysis_data <- analysis_data[analysis_data[[stage_col]] %in% windows$All, ]

cor_rows <- list()
i <- 1
for (window_name in names(windows)) {
  wdat <- analysis_data[analysis_data[[stage_col]] %in% windows[[window_name]], ]
  vals <- safe_cor(wdat[[ambient_col]], wdat[[skin_col]])
  cor_rows[[i]] <- data.frame(window = window_name, group = "All", vals, stringsAsFactors = FALSE)
  i <- i + 1
  if (window_name == "BDC") next
  for (grp in sort(unique(wdat[[group_col]]))) {
    gdat <- wdat[wdat[[group_col]] == grp, ]
    vals <- safe_cor(gdat[[ambient_col]], gdat[[skin_col]])
    cor_rows[[i]] <- data.frame(window = window_name, group = grp, vals, stringsAsFactors = FALSE)
    i <- i + 1
  }
}
pearson_results <- do.call(rbind, cor_rows)

mixed_rows <- list()
i <- 1
for (window_name in names(windows)) {
  wdat <- analysis_data[analysis_data[[stage_col]] %in% windows[[window_name]], ]
  mixed_rows[[i]] <- fit_mixed(wdat, window_name, "All")
  i <- i + 1
  if (window_name == "BDC") next
  for (grp in sort(unique(wdat[[group_col]]))) {
    mixed_rows[[i]] <- fit_mixed(wdat, window_name, grp)
    i <- i + 1
  }
}
mixed_results <- do.call(rbind, mixed_rows)

stage_summary <- aggregate(
  analysis_data[, c(ambient_col, skin_col)],
  by = list(stage = analysis_data[[stage_col]], group = analysis_data[[group_col]]),
  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
)
stage_summary <- do.call(data.frame, stage_summary)
names(stage_summary) <- c("stage", "group", "ambient_mean", "ambient_sd", "ambient_n", "skin_mean", "skin_sd", "skin_n")

model_note <- data.frame(
  item = c("variables", "pearson", "mixed_model", "BDC_rule", "stage_source"),
  detail = c(
    paste(skin_col, "vs", ambient_col),
    "Pearson correlations calculated overall, by group, and by phase window; BDC is pooled across all three groups.",
    paste(skin_col, "~", ambient_col, "+ (1 | subject)"),
    "BDC window combines Control, Ex, and ExAG together; no BDC group-specific rows are reported.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Model_note = model_note,
  Pearson_correlations = pearson_results,
  Mixed_model = mixed_results,
  Stage_group_summary = stage_summary,
  Analysis_data = analysis_data
), out_file)

print(pearson_results)
print(mixed_results)
cat(out_file, "\n")
