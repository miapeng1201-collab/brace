library(readxl)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
input_file <- file.path(tewl_dir, "TEWL_skin_ambient_temperature_correlation.xlsx")
out_file <- file.path(tewl_dir, "TEWL_skin_temperature_ambient_adjusted_by_phase.xlsx")

phase_map <- list(
  BDC = c("BDC-12", "BDC-6"),
  HDT_before_HDT25 = c("HDT1", "HDT7", "HDT13", "HDT19"),
  HDT_after_HDT19 = c("HDT25", "HDT37", "HDT43", "HDT49", "HDT55"),
  R = c("R+1", "R+6", "R+12")
)

phase_label <- function(stage) {
  out <- rep(NA_character_, length(stage))
  for (nm in names(phase_map)) {
    out[stage %in% phase_map[[nm]]] <- nm
  }
  out
}

data <- read_excel(input_file, sheet = "Analysis_data")
coefs <- read_excel(input_file, sheet = "Mixed_model")
names(data) <- trimws(names(data))
names(coefs) <- trimws(names(coefs))

data$stage <- as.character(data$stage)
data$phase_window <- phase_label(data$stage)
data$ambient_temperature_take_c <- suppressWarnings(as.numeric(data$ambient_temperature_take_c))
data$avg_temperature_skin_robust_c <- suppressWarnings(as.numeric(data$avg_temperature_skin_robust_c))

coefs$key_group <- ifelse(
  coefs$window == "BDC",
  "All",
  as.character(coefs$group)
)
phase_coefs <- coefs[
  coefs$window %in% names(phase_map) &
    (
      (coefs$window == "BDC" & coefs$group == "All") |
        (coefs$window %in% c("HDT_before_HDT25", "HDT_after_HDT19", "R") & coefs$group != "All")
    ),
]
phase_coefs <- phase_coefs[, c("window", "group", "key_group", "n", "n_subjects", "ambient_beta", "ambient_se", "ambient_df", "ambient_p")]

reference <- aggregate(
  ambient_temperature_take_c ~ phase_window,
  data = data[!is.na(data$phase_window) & !is.na(data$ambient_temperature_take_c), ],
  FUN = mean
)
names(reference)[names(reference) == "ambient_temperature_take_c"] <- "reference_ambient_temperature_c"

data$key_group <- ifelse(
  data$phase_window == "BDC",
  "All",
  as.character(data$group)
)

adjusted <- merge(
  data,
  phase_coefs,
  by.x = c("phase_window", "key_group"),
  by.y = c("window", "key_group"),
  all.x = TRUE,
  sort = FALSE
)
adjusted <- merge(adjusted, reference, by = "phase_window", all.x = TRUE, sort = FALSE)

adjusted$ambient_temperature_delta_from_phase_mean_c <-
  adjusted$ambient_temperature_take_c - adjusted$reference_ambient_temperature_c
adjusted$avg_temperature_skin_ambient_adjusted_by_phase_c <-
  adjusted$avg_temperature_skin_robust_c -
  adjusted$ambient_beta * adjusted$ambient_temperature_delta_from_phase_mean_c

summary_adjusted <- aggregate(
  cbind(
    avg_temperature_skin_robust_c,
    avg_temperature_skin_ambient_adjusted_by_phase_c,
    ambient_temperature_take_c
  ) ~ phase_window,
  data = adjusted,
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = sum(!is.na(x)))
)
summary_adjusted <- do.call(data.frame, summary_adjusted)
names(summary_adjusted) <- c(
  "phase_window",
  "skin_observed_mean", "skin_observed_sd", "skin_observed_n",
  "skin_adjusted_mean", "skin_adjusted_sd", "skin_adjusted_n",
  "ambient_mean", "ambient_sd", "ambient_n"
)

model_note <- data.frame(
  item = c("outcome", "coefficient_rule", "BDC_rule", "HDT_rule", "R_rule", "reference_temperature", "formula", "source_model"),
  detail = c(
    "avg_temperature_skin_ambient_adjusted_by_phase_c",
    "Uses phase-specific ambient beta from the mixed model.",
    "BDC coefficient is pooled across Control, Ex, and ExAG.",
    "HDT phase coefficients are group-specific: Control, Ex, and ExAG each use their own beta.",
    "R phase coefficients are group-specific: Control, Ex, and ExAG each use their own beta.",
    "Each phase is corrected to its own mean ambient_temperature_take_c.",
    "adjusted_skin_temp = observed_skin_temp - ambient_beta_phase * (ambient_temp - phase_mean_ambient_temp)",
    "avg_temperature_skin_robust_c ~ ambient_temperature_take_c + (1 | subject)"
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Model_note = model_note,
    Phase_coefficients = phase_coefs,
    Phase_reference_ambient = reference,
    Adjusted_data = adjusted,
    Phase_summary = summary_adjusted
  ),
  out_file
)

print(phase_coefs)
print(reference)
print(summary_adjusted)
cat(out_file, "\n")
