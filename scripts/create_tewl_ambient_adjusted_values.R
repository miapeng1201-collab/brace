library(readxl)
library(writexl)
library(lme4)
library(lmerTest)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "Processed data", "04 - TEWAMETER")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_ambient_temperature_adjusted_values.xlsx")

phase_map <- list(
  BDC = c("BDC-12", "BDC-6"),
  HDT1_7_13_19 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  HDT25_37_43_49_55 = c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"),
  R = c("R+1", "R+6", "R+12")
)

stage_to_phase <- function(stage) {
  out <- rep(NA_character_, length(stage))
  for (phase in names(phase_map)) {
    out[stage %in% phase_map[[phase]]] <- phase
  }
  out
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

make_model_row <- function(window, group, n, n_subjects, stages, model, ref_ambient,
                           ambient_beta = NA_real_, ambient_se = NA_real_,
                           ambient_df = NA_real_, ambient_t = NA_real_,
                           ambient_p = NA_real_, subject_sd = NA_real_,
                           residual_sd = NA_real_, singular_fit = NA,
                           note = "") {
  data.frame(
    adjustment_window = window,
    model_group = group,
    n = n,
    n_subjects = n_subjects,
    stages = paste(stages, collapse = ", "),
    model = model,
    reference_ambient_temperature_c = ref_ambient,
    ambient_beta = ambient_beta,
    ambient_se = ambient_se,
    ambient_df = ambient_df,
    ambient_t = ambient_t,
    ambient_p_lmerTest = ambient_p,
    subject_random_intercept_sd = subject_sd,
    residual_sd = residual_sd,
    singular_fit = singular_fit,
    note = note,
    stringsAsFactors = FALSE
  )
}

fit_ambient_model <- function(data, formula_text, ambient_col, subject_col) {
  model <- lmer(as.formula(formula_text), data = data, REML = FALSE)
  coef_table <- as.data.frame(coef(summary(model)))
  vc <- as.data.frame(VarCorr(model))
  list(
    model = model,
    coef_table = coef_table,
    ambient_beta = coef_table[ambient_col, "Estimate"],
    ambient_se = coef_table[ambient_col, "Std. Error"],
    ambient_df = if ("df" %in% names(coef_table)) coef_table[ambient_col, "df"] else NA_real_,
    ambient_t = coef_table[ambient_col, "t value"],
    ambient_p = coef_table[ambient_col, "Pr(>|t|)"],
    subject_sd = vc$sdcor[vc$grp == subject_col][1],
    residual_sd = sigma(model),
    singular_fit = isSingular(model)
  )
}

df <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
day_col <- pick_col(df, c("day", "Day"))
tewl_col <- pick_col(df, c("final_clean_tewl"))
raw_col <- pick_col(df, c("raw_window_tewl_mean_of_measurements_g_m2_h"))
ambient_col <- pick_col(df, c("ambient_temperature_take_c"))
skin_col <- pick_col(df, c("avg_temperature_skin_robust_c"))
outcome_col <- "tewl_for_ambient_adjustment"

df[[subject_col]] <- as.character(df[[subject_col]])
df[[group_col]] <- as.character(df[[group_col]])
df[[stage_col]] <- as.character(df[[stage_col]])
df[[tewl_col]] <- suppressWarnings(as.numeric(df[[tewl_col]]))
df[[raw_col]] <- suppressWarnings(as.numeric(df[[raw_col]]))
df[[ambient_col]] <- suppressWarnings(as.numeric(df[[ambient_col]]))
df[[skin_col]] <- suppressWarnings(as.numeric(df[[skin_col]]))
df$adjustment_window <- stage_to_phase(df[[stage_col]])
df[[outcome_col]] <- df[[tewl_col]]
bdc_fallback_idx <- !is.na(df$adjustment_window) & df$adjustment_window == "BDC" & is.na(df[[outcome_col]]) & !is.na(df[[raw_col]])
df[[outcome_col]][bdc_fallback_idx] <- df[[raw_col]][bdc_fallback_idx]

adjusted <- df
adjusted$model_group <- NA_character_
adjusted$reference_ambient_temperature_c <- NA_real_
adjusted$ambient_beta_for_adjustment <- NA_real_
adjusted$tewl_ambient_adjusted <- NA_real_
adjusted$adjustment_note <- NA_character_

model_rows <- list()
sensitivity_rows <- list()
row_i <- 1
sens_i <- 1

for (phase in names(phase_map)) {
  phase_idx <- adjusted$adjustment_window == phase & !is.na(adjusted$adjustment_window)
  phase_data <- adjusted[phase_idx, ]

  if (phase == "BDC") {
    model_groups <- "All_groups"
    model_sets <- list(All_groups = phase_data)
    model_formula <- sprintf("%s ~ %s + %s + (1 | %s)", outcome_col, ambient_col, group_col, subject_col)
    sens_formula <- sprintf("%s ~ %s + %s + %s + (1 | %s)", outcome_col, ambient_col, skin_col, group_col, subject_col)
  } else {
    groups <- sort(unique(phase_data[[group_col]]))
    model_groups <- groups
    model_sets <- lapply(groups, function(g) phase_data[phase_data[[group_col]] == g, ])
    names(model_sets) <- groups
  }

  for (mg in names(model_sets)) {
    sub <- model_sets[[mg]]
    sub <- sub[complete.cases(sub[, c(subject_col, group_col, stage_col, outcome_col, ambient_col)]), ]
    sub[[subject_col]] <- droplevels(factor(as.character(sub[[subject_col]])))
    sub[[group_col]] <- droplevels(factor(as.character(sub[[group_col]])))

    ref_ambient <- mean(sub[[ambient_col]], na.rm = TRUE)
    stages <- sort(unique(as.character(sub[[stage_col]])))
    model_label <- if (phase == "BDC") {
      sprintf("%s ~ %s + %s + (1 | %s)", outcome_col, ambient_col, group_col, subject_col)
    } else {
      sprintf("%s ~ %s + (1 | %s)", outcome_col, ambient_col, subject_col)
    }

    if (nrow(sub) < 6 || length(unique(sub[[subject_col]])) < 3 || length(unique(sub[[ambient_col]])) < 2) {
      model_rows[[row_i]] <- make_model_row(
        phase, mg, nrow(sub), length(unique(sub[[subject_col]])), stages, model_label, ref_ambient,
        note = "insufficient data or no ambient variation"
      )
      row_i <- row_i + 1
      next
    }

    formula_text <- if (phase == "BDC") model_formula else sprintf("%s ~ %s + (1 | %s)", outcome_col, ambient_col, subject_col)
    fit <- fit_ambient_model(sub, formula_text, ambient_col, subject_col)

    model_rows[[row_i]] <- make_model_row(
      phase, mg, nrow(sub), length(unique(sub[[subject_col]])), stages, model_label, ref_ambient,
      fit$ambient_beta, fit$ambient_se, fit$ambient_df, fit$ambient_t, fit$ambient_p,
      fit$subject_sd, fit$residual_sd, fit$singular_fit
    )
    row_i <- row_i + 1

    apply_idx <- !is.na(adjusted$adjustment_window) & adjusted$adjustment_window == phase
    if (phase != "BDC") {
      apply_idx <- apply_idx & adjusted[[group_col]] == mg
    }
    ok <- apply_idx & !is.na(adjusted[[outcome_col]]) & !is.na(adjusted[[ambient_col]])
    adjusted$model_group[apply_idx] <- mg
    adjusted$reference_ambient_temperature_c[apply_idx] <- ref_ambient
    adjusted$ambient_beta_for_adjustment[apply_idx] <- fit$ambient_beta
    adjusted$tewl_ambient_adjusted[ok] <- adjusted[[outcome_col]][ok] - fit$ambient_beta * (adjusted[[ambient_col]][ok] - ref_ambient)
    adjusted$adjustment_note[apply_idx] <- "Adjusted to phase/model-group mean ambient temperature"

    sens_sub <- model_sets[[mg]]
    sens_sub <- sens_sub[complete.cases(sens_sub[, c(subject_col, group_col, stage_col, outcome_col, ambient_col, skin_col)]), ]
    sens_sub[[subject_col]] <- droplevels(factor(as.character(sens_sub[[subject_col]])))
    sens_sub[[group_col]] <- droplevels(factor(as.character(sens_sub[[group_col]])))
    if (nrow(sens_sub) >= 6 && length(unique(sens_sub[[subject_col]])) >= 3 &&
        length(unique(sens_sub[[ambient_col]])) >= 2 && length(unique(sens_sub[[skin_col]])) >= 2) {
      sens_formula_text <- if (phase == "BDC") {
        sens_formula
      } else {
        sprintf("%s ~ %s + %s + (1 | %s)", outcome_col, ambient_col, skin_col, subject_col)
      }
      sens <- tryCatch({
        smodel <- lmer(as.formula(sens_formula_text), data = sens_sub, REML = FALSE)
        ctab <- as.data.frame(coef(summary(smodel)))
        data.frame(
          adjustment_window = phase,
          model_group = mg,
          n = nrow(sens_sub),
          n_subjects = length(unique(sens_sub[[subject_col]])),
          model = sens_formula_text,
          ambient_beta = ctab[ambient_col, "Estimate"],
          ambient_p_lmerTest = ctab[ambient_col, "Pr(>|t|)"],
          skin_beta = ctab[skin_col, "Estimate"],
          skin_p_lmerTest = ctab[skin_col, "Pr(>|t|)"],
          singular_fit = isSingular(smodel),
          note = "",
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        data.frame(
          adjustment_window = phase,
          model_group = mg,
          n = nrow(sens_sub),
          n_subjects = length(unique(sens_sub[[subject_col]])),
          model = sens_formula_text,
          ambient_beta = NA_real_,
          ambient_p_lmerTest = NA_real_,
          skin_beta = NA_real_,
          skin_p_lmerTest = NA_real_,
          singular_fit = NA,
          note = paste("model error:", conditionMessage(e)),
          stringsAsFactors = FALSE
        )
      })
      sensitivity_rows[[sens_i]] <- sens
      sens_i <- sens_i + 1
    }
  }
}

model_results <- do.call(rbind, model_rows)
sensitivity_results <- if (length(sensitivity_rows) > 0) do.call(rbind, sensitivity_rows) else data.frame()

adjusted_export <- adjusted[, c(
  subject_col, group_col, stage_col, day_col, "adjustment_window", "model_group",
  tewl_col, raw_col, outcome_col, ambient_col, skin_col,
  "reference_ambient_temperature_c", "ambient_beta_for_adjustment",
  "tewl_ambient_adjusted", "adjustment_note"
)]
names(adjusted_export)[1:11] <- c(
  "subject", "group", "stage", "day", "adjustment_window", "model_group",
  "final_clean_tewl", "raw_window_tewl_mean_of_measurements_g_m2_h",
  "tewl_for_ambient_adjustment", "ambient_temperature_take_c", "avg_temperature_skin_robust_c"
)

model_note <- data.frame(
  item = c("adjustment_formula", "BDC_model", "HDT_R_models", "skin_temperature", "reference_temperature", "stage_source"),
  detail = c(
    "tewl_ambient_adjusted = tewl_for_ambient_adjustment - ambient_beta * (ambient_temperature_take_c - reference_ambient_temperature_c)",
    "BDC uses all three groups together: tewl_for_ambient_adjustment ~ ambient_temperature_take_c + group + (1 | subject). If BDC final_clean_tewl is missing, raw-window TEWL is used as fallback.",
    "HDT1 7 13 19, HDT25 37 43 49 55, and R are modeled separately by group: tewl_for_ambient_adjustment ~ ambient_temperature_take_c + (1 | subject)",
    "Skin temperature is not included in the correction value; sensitivity models with skin temperature are provided separately.",
    "Mean ambient temperature within the corresponding phase/model group; for BDC, mean ambient across all groups.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Model_note = model_note,
  Ambient_adjustment_models = model_results,
  TEWL_ambient_adjusted = adjusted_export,
  Skin_sensitivity_models = sensitivity_results
), out_file)

print(model_results)
cat(out_file, "\n")
