# Finapres data cleaning
# X. Peng
# Purpose:
# Process individual raw Finapres CSV files from cohorts C1 and C2.
# Extract time-based analysis windows, calculate measurement means,
# identify recordings requiring manual review, and apply documented
# manual window selections and exclusions.

# Workflow:
# 1. Extract C1 files using the F BDC-6 backup, skipping misplaced G R1 files.
# 2. Select nine variables from C1 and C2; exclude N_Merger_AHD files.
# 3. Select 200–20 s before each file ends and check against the latest BraCal.
# 4. Calculate means and valid/missing counts without removing outliers.
# 5. Use manual windows for A HDT19 (330–390 s) and M HDT7 (510–540 s); 
# exclude N BDC-6 for poor recording quality
# 6. Standardize subject/visit labels, match lookup information, and save results.


# Libraries ----
library(tidyverse)
library(readxl)

# Output directories ----
finapress_output_dir <- "data/processed_data/03_finapress"
dir.create(finapress_output_dir, recursive = TRUE, showWarnings = FALSE)

# Open data ----
finapres_root <- "data/Original_data/03 - FINAPRESS"
lookup_clean <- readRDS("data/processed_data/lookup/lookup_table.RDS")

## Locate C1 raw CSV archives
c1_zip_files <- list.files(path = file.path(finapres_root, "C1", "RawData", "CSV RAW"),pattern = "\\.zip$",full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
# Replace the damaged F BDC-6 archive with its readable backup
damaged_zip <- file.path(
  finapres_root, "C1", "RawData", "CSV RAW", "F",
  "BRACE_SALT_FINAPRES_RAW_CSV_F_BDC-6_20230415.zip"
)

backup_zip <- file.path(
  finapres_root, "C1", "RawData", "CSV RAW", "F",
  "dubbel_BRACE_SALT_FINAPRES_RAW_CSV_F_BDC-6_20230415.zip"
)

c1_zip_files[c1_zip_files == damaged_zip] <- backup_zip
c1_zip_files <- unique(c1_zip_files)
# c1 extracted
c1_extract_dir <- file.path(finapres_root, "C1", "RawData", "CSV EXTRACTED")
dir.create(c1_extract_dir, recursive = TRUE, showWarnings = FALSE)

# Extract C1 files, excluding misplaced G R1 copies
for (zip_file in c1_zip_files) {
  extract_folder <- file.path(
    c1_extract_dir,
    tools::file_path_sans_ext(basename(zip_file))
  )
  files <- unzip(zip_file, list = TRUE)$Name
  if (basename(zip_file) ==
      "BRACE_SALT_FINAPRES_RAW_CSV_H_HDT55_20230615.zip") {
    files <- files[
      !startsWith(basename(files), "G SALT R+1 220623")
    ]
  }
  message("Extracting: ", basename(zip_file))
  unzip(
    zipfile = zip_file,
    files = files,
    exdir = extract_folder
  )
}

# Find the nine selected variables 
finapres_raw_files <- list.files(
  path = c(
    c1_extract_dir,
    file.path(finapres_root, "C2", "RawData")
  ),
  pattern = " (HR|reSYS|reDIA|SV|CO|SVI|CI|SVR|SVRI)\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
) |>
  str_subset("/N_Merger_AHD/", negate = TRUE)

length(finapres_raw_files)

# Read one raw CSV file and remove unnamed columns
read_finapres_raw <- function(file) {
  data <- read.csv2(
    file = file,
    skip = 7,
    dec = ".",
    na.strings = "",
    strip.white = TRUE,
    check.names = FALSE
  )

  data[, !is.na(names(data)) & names(data) != "", drop = FALSE]
}

# Test the reader on one file
finapres_test <- read_finapres_raw(finapres_raw_files[1])

names(finapres_test)
head(finapres_test)

# Count numeric BraCal results and find the latest timestamp
get_bracal_info <- function(data) {
  result_rows <- grepl(
    "BraCal:\\s*[0-9]",
    data[["Marker"]]
  )
  n_bracal <- sum(result_rows)
  bracal_time <- if (n_bracal == 0) {
    NA_real_
  } else {
    max(data[["Time(sec)"]][result_rows])
  }
  list(
    n_bracal = n_bracal,
    bracal_time = bracal_time
  )
}

bracal_info_test <- get_bracal_info(finapres_test)
bracal_info_test
t_end <- tail(finapres_test[["Time(sec)"]],1)
t_start <- t_end - 200
t_stop <- t_end - 20
finapres_window_test <- finapres_test |>
  filter(
    between(.data[["Time(sec)"]], t_start, t_stop)
  )
range(finapres_window_test[["Time(sec)"]])
nrow(finapres_window_test)

# Store calibration information for the test recording
window_after_bracal <- if (nrow(finapres_window_test) > 0) {
  all(
    finapres_window_test[["Time(sec)"]] >
      bracal_info_test$bracal_time
  )
} else {
  FALSE
}

window_after_bracal

# Assign automatic or manual window-review status
window_status <- if (bracal_info_test$n_bracal == 0) {
  "manual_no_bracal"
} else if (isTRUE(window_after_bracal)) {
  "after_bracal"
} else {
  "manual_window_review"
}

window_status

# Count valid and missing measurements in the selected window
n_valid_test <- sum(!is.na(finapres_window_test[[2]]))
n_missing_test <- sum(is.na(finapres_window_test[[2]]))

n_valid_test
n_missing_test 
n_valid_test + n_missing_test == nrow(finapres_window_test)

# Calculate the mean within the selected time window
mean_test <- mean(finapres_window_test[[2]],na.rm = TRUE)
mean_test


finapres_summary_test <- tibble(
  file = finapres_raw_files[1],
  Measurement = names(finapres_test)[2],
  t_end = t_end,
  t_start = t_start,
  t_stop = t_stop,
  n_bracal = bracal_info_test$n_bracal,
  bracal_time = bracal_info_test$bracal_time,
  window_status = window_status,
  n_valid = n_valid_test,
  n_missing = n_missing_test,
  Mean = mean_test
)

finapres_summary_test 
# Process all indexed Finapres files----
process_finapres_file <- function(file) {
  data <- read_finapres_raw(file)
  bracal_info <- get_bracal_info(data)
  t_end <- tail(data[["Time(sec)"]], 1)
  t_start<- t_end - 200
  t_stop<- t_end - 20
  window <- data |>
  filter(
    between(.data[["Time(sec)"]], t_start, t_stop)
  )
  window_after_bracal <- if (nrow(window) > 0) {
  all(window[["Time(sec)"]] > bracal_info$bracal_time)
} else {
  FALSE
}
  window_status <- if (bracal_info$n_bracal == 0) {
  "manual_no_bracal"
} else if (isTRUE(window_after_bracal)) {
  "after_bracal"
} else {
  "manual_window_review"
}
n_valid <- sum(!is.na(window[[2]]))
n_missing <- sum(is.na(window[[2]]))

mean_value <- if (n_valid > 0) {
  mean(window[[2]], na.rm = TRUE)
} else {
  NA_real_
}
  tibble(
  file = file,
  Measurement = names(data)[2],
  t_end = t_end,
  t_start = t_start,
  t_stop = t_stop,
  n_bracal = bracal_info$n_bracal,
  bracal_time = bracal_info$bracal_time,
  window_status = window_status,
  n_valid = n_valid,
  n_missing = n_missing,
  Mean = mean_value
)
}

finapres_processed_test <- process_finapres_file(finapres_raw_files[1])
finapres_processed_test


# Process all selected Finapres files
finapres_time_results <- map_dfr(
  finapres_raw_files,
  function(file) {
    message("Processing: ", basename(file))
    process_finapres_file(file)
  }
)

dim(finapres_time_results)

finapres_time_results |>
  count(window_status)

# Identify records requiring manual window selection

table(finapres_time_results$window_status, useNA = "ifany")
finapres_manual_review <- finapres_time_results |>
  filter(window_status != "after_bracal")

finapres_manual_review
# Manual review is required for 63 variable files across seven recordings.
# C1: A HDT19, B HDT25, I HDT19, I HDT31, and M HDT7.
# C2: N BDC-6 and N R6.
# Manual review decisions:----
# A HDT19: use 330–390 s.
# M HDT7: use 510–540 s.
# N BDC-6: exclude the recording because of poor signal quality.
# Define manual windows using recording folder identifiers
manual_windows <- tribble(
  ~recording, ~manual_start_s, ~manual_stop_s,
  "BRACE_SALT_FINAPRES_RAW_CSV_A_HDT19_", 330, 390,
  "BRACE_SALT_FINAPRES_RAW_CSV_M_HDT7_",  510, 540
)

# Identify files requiring manual window replacement
manual_files <- map_dfr(
  seq_len(nrow(manual_windows)),
  function(i) {
    finapres_time_results |>
      filter(
        str_detect(file, fixed(manual_windows$recording[i]))
      ) |>
      mutate(
        manual_start_s = manual_windows$manual_start_s[i],
        manual_stop_s = manual_windows$manual_stop_s[i]
      )
  }
)

# Check the number of selected files
nrow(manual_files)
# Recalculate results using the manually selected windows
manual_results <- map_dfr(
  seq_len(nrow(manual_files)),
  function(i) {
    record <- manual_files[i, ]
    data <- read_finapres_raw(record$file)
    window <- data |>
      filter(
        between(
          .data[["Time(sec)"]],
          record$manual_start_s,
          record$manual_stop_s
        )
      )
    n_valid <- sum(!is.na(window[[2]]))
    tibble(
      file = record$file,
      manual_start_s = record$manual_start_s,
      manual_stop_s = record$manual_stop_s,
      manual_n_valid = n_valid,
      manual_n_missing = sum(is.na(window[[2]])),
      manual_mean = if (n_valid > 0) {
        mean(window[[2]], na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  }
)

# Check manually recalculated results
manual_results |>
  select(
    file, manual_start_s, manual_stop_s,
    manual_n_valid, manual_n_missing, manual_mean
  )

# Apply manual results and preserve the original window information
finapres_final_results <- finapres_time_results |>
  anti_join(finapres_excluded, by = "file") |>
  left_join(
    manual_results,
    by = "file",
    relationship = "one-to-one"
  ) |>
  mutate(
    standard_start_s = t_start,
    standard_stop_s = t_stop,
    standard_mean = Mean,
    standard_window_status = window_status,

    manually_selected = !is.na(manual_start_s),

    t_start = if_else(manually_selected, manual_start_s, t_start),
    t_stop = if_else(manually_selected, manual_stop_s, t_stop),
    n_valid = if_else(manually_selected, manual_n_valid, n_valid),
    n_missing = if_else(manually_selected, manual_n_missing, n_missing),
    Mean = if_else(manually_selected, manual_mean, Mean),

    window_status = if_else(
      manually_selected, "manually_selected", window_status
    ),
    window_duration_s = t_stop - t_start
  ) |>
  select(
    -manual_start_s, -manual_stop_s,
    -manual_n_valid, -manual_n_missing, -manual_mean
  )

# Extract subject and visit identifiers
finapres_final_results <- finapres_final_results |>
  mutate(
    Subject = str_match(
      file,
      "BRACE_SALT_FINAPRES_RAW_CSV_([^_]+)_"
    )[, 2],
    Stage_original = str_match(
      file,
      "BRACE_SALT_FINAPRES_RAW_CSV_[^_]+_([^_]+)_"
    )[, 2],
    Stage = recode(
      Stage_original,
      "BDC-10" = "BDC-12",
      "BDC-11" = "BDC-12",
      "HDT56" = "HDT55"
    ),
    stage_key = str_remove_all(Stage, "[-+]")
  )
str(finapres_final_results)

# Check subject and visit labels
table(finapres_final_results$Subject, useNA = "ifany")
table(finapres_final_results$Stage, useNA = "ifany")

# Check for duplicate measurement records
finapres_final_results |>
  count(Subject, stage_key, Measurement) |>
  filter(n > 1)



# Prepare lookup information
finapres_final_results <- finapres_final_results |>
  mutate(stage_key = str_remove_all(Stage, "[-+]"))
finapres_lookup <- lookup_clean |>
  transmute(
    Subject = as.character(Subject),
    stage_key,
    ID,
    Group,
    StudyDay,
    Day,
    Date
  ) |>
  distinct()

# Save final results
saveRDS(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.RDS")
)

write_csv(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.csv")
)


