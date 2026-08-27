library(readxl)
library(writexl)

root_dir <- "/Users/ree/Documents/01 my research/01 bed rest study/BRACE Bed Rest"
tewl_dir <- file.path(root_dir, "processed_data", "04_tewameter")
main_file <- file.path(tewl_dir, "TEWL_processed_raw_window.xlsx")
out_file <- file.path(tewl_dir, "TEWL_test_day_environment_summary.xlsx")

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing expected column: ", paste(candidates, collapse = " / "))
  hit[[1]]
}

num <- function(x) suppressWarnings(as.numeric(x))
mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
sd_na <- function(x) if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

df <- read_excel(main_file, sheet = "RawWindowSummaryClean")
names(df) <- trimws(names(df))

subject_col <- pick_col(df, c("subject", "Subject", "participant_id"))
group_col <- pick_col(df, c("group", "Group"))
stage_col <- pick_col(df, c("stage", "Stage"))
day_col <- pick_col(df, c("day", "Day"))
date_col <- pick_col(df, c("stage_date"))
take_datetime_col <- pick_col(df, c("take_datetime"))
source_col <- pick_col(df, c("source_file"))
temp_col <- pick_col(df, c("ambient_temperature_take_c"))
humidity_col <- pick_col(df, c("ambient_relative_humidity_take_pct"))

df[[subject_col]] <- as.character(df[[subject_col]])
df[[group_col]] <- as.character(df[[group_col]])
df[[stage_col]] <- as.character(df[[stage_col]])
df[[day_col]] <- num(df[[day_col]])
df[[temp_col]] <- num(df[[temp_col]])
df[[humidity_col]] <- num(df[[humidity_col]])

keys <- c(subject_col, group_col, stage_col, day_col, date_col)
split_key <- interaction(df[, keys], drop = TRUE, lex.order = TRUE)
parts <- split(df, split_key)

rows <- lapply(parts, function(sub) {
  data.frame(
    subject = unique(sub[[subject_col]])[[1]],
    group = unique(sub[[group_col]])[[1]],
    stage = unique(sub[[stage_col]])[[1]],
    day = unique(sub[[day_col]])[[1]],
    stage_date = as.character(unique(sub[[date_col]])[[1]]),
    n_records = nrow(sub),
    n_source_files = length(unique(sub[[source_col]])),
    take_datetime = as.character(min(as.POSIXct(sub[[take_datetime_col]], tz = "UTC"), na.rm = TRUE)),
    ambient_temperature_c = mean_na(sub[[temp_col]]),
    ambient_relative_humidity_pct = mean_na(sub[[humidity_col]]),
    stringsAsFactors = FALSE
  )
})

summary_by_test_day <- do.call(rbind, rows)
stage_levels <- c("BDC-12", "BDC-6", "HDT1", "HDT7", "HDT13", "HDT19", "HDT25", "HDT31", "HDT37", "HDT43", "HDT49", "HDT55", "R+1", "R+6", "R+12")
summary_by_test_day$stage_order <- match(summary_by_test_day$stage, stage_levels)
summary_by_test_day <- summary_by_test_day[order(summary_by_test_day$group, summary_by_test_day$subject, summary_by_test_day$stage_order), ]
summary_by_test_day$stage_order <- NULL

subject_rows <- lapply(split(summary_by_test_day, summary_by_test_day$subject), function(sub) {
  data.frame(
    subject = unique(sub$subject)[[1]],
    group = unique(sub$group)[[1]],
    n_test_days = nrow(sub),
    ambient_temperature_mean_c = mean_na(sub$ambient_temperature_c),
    ambient_temperature_sd_across_test_days_c = sd_na(sub$ambient_temperature_c),
    ambient_relative_humidity_mean_pct = mean_na(sub$ambient_relative_humidity_pct),
    ambient_relative_humidity_sd_across_test_days_pct = sd_na(sub$ambient_relative_humidity_pct),
    stringsAsFactors = FALSE
  )
})
summary_by_subject <- do.call(rbind, subject_rows)
summary_by_subject <- summary_by_subject[order(summary_by_subject$group, summary_by_subject$subject), ]

stage_rows <- lapply(split(summary_by_test_day, list(summary_by_test_day$group, summary_by_test_day$stage), drop = TRUE), function(sub) {
  data.frame(
    group = unique(sub$group)[[1]],
    stage = unique(sub$stage)[[1]],
    n_subject_test_days = nrow(sub),
    ambient_temperature_mean_c = mean_na(sub$ambient_temperature_c),
    ambient_temperature_sd_c = sd_na(sub$ambient_temperature_c),
    ambient_relative_humidity_mean_pct = mean_na(sub$ambient_relative_humidity_pct),
    ambient_relative_humidity_sd_pct = sd_na(sub$ambient_relative_humidity_pct),
    stringsAsFactors = FALSE
  )
})
summary_by_group_stage <- do.call(rbind, stage_rows)
summary_by_group_stage$stage_order <- match(summary_by_group_stage$stage, stage_levels)
summary_by_group_stage <- summary_by_group_stage[order(summary_by_group_stage$group, summary_by_group_stage$stage_order), ]
summary_by_group_stage$stage_order <- NULL

model_note <- data.frame(
  item = c("temperature", "humidity", "unit", "stage_source"),
  detail = c(
    "ambient_temperature_take_c",
    "ambient_relative_humidity_take_pct",
    "Temperature in C; relative humidity in percent.",
    "D-column stage from RawWindowSummaryClean; real stage ignored."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(
  Notes = model_note,
  By_subject_test_day = summary_by_test_day,
  By_subject_overall = summary_by_subject,
  By_group_stage = summary_by_group_stage
), out_file)

cat(out_file, "\n")
cat("subject-test-day rows=", nrow(summary_by_test_day), "\n")
