# Finapres data cleaning
# X. Peng
# Purpose:
# Process individual raw Finapres CSV files from cohorts C1 and C2.
# Extract time-based analysis windows, calculate measurement means,
# identify recordings requiring manual review, and apply documented
# manual window selections and exclusions.

# Workflow:
# 1. Locate nine variables: HR, reSYS, reDIA, SV, CO, SVI, CI, SVR, and SVRI.
# 2. Replace the damaged F BDC-6 archive with its readable backup,remove misplaced G R1 copies from the H HDT55 archive, and exclude files in the N_Merger_AHD folder.
# 3. Read timestamps, measurement values, and marker information.
# 4. Identify numeric BraCal results and select the latest result timestamp.
# 5. Select data from 200 to 20 seconds before each variable file's final timestamp.
# 6. Flag recordings without numeric BraCal results and windows
#    that are not entirely after the selected BraCal result.
# 7. Calculate means and valid/missing counts without removing numerical outliers.
# 8. Extract subject and visit identifiers, standardize visit labels,
#    and match preliminary results to the study lookup table.
# 9. Recalculate all nine variables using manually selected windows:
#    A HDT19: 330–390 s; M HDT7: 510–540 s.
# 10. Exclude N BDC-6 following manual assessment of signal quality.


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
c1_zip_files <- list.files(
  path = file.path(finapres_root, "C1", "RawData", "CSV RAW"),
  pattern = "\\.zip$",
  full.names = TRUE,
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

# Remove misplaced G R1 copies from the H HDT55 archive
c1_files <- c1_files |>
  filter(
    !(
      basename(zip) ==
        "BRACE_SALT_FINAPRES_RAW_CSV_H_HDT55_20230615.zip" &
        startsWith(basename(file), "G SALT R+1 220623")
    )
  )


c1_zip_files[c1_zip_files == damaged_zip] <- backup_zip
c1_zip_files <- unique(c1_zip_files)

# List the nine selected variables in each archive
c1_files <- lapply(c1_zip_files, function(z) {
  files <- unzip(z, list = TRUE)$Name

  files <- files[
    grepl(
      " (HR|reSYS|reDIA|SV|CO|SVI|CI|SVR|SVRI)\\.csv$",
      files,
      ignore.case = TRUE
    )
  ]

  data.frame(
    zip = rep(z, length(files)),
    file = files
  )
})

c1_files <- bind_rows(c1_files)


# Check archive and selected-file counts
length(c1_zip_files)
nrow(c1_files)

# Check for archives with an unexpected number of selected files
c1_files |>
  count(zip) |>
  filter(n != 9)

# Remove misplaced G R1 copies from the H HDT55 archive
c1_files <- c1_files |>
  filter(
    !(
      basename(zip) ==
        "BRACE_SALT_FINAPRES_RAW_CSV_H_HDT55_20230615.zip" &
        startsWith(basename(file), "G SALT R+1 220623")
    )
  )

# Check for duplicate archive-file entries
c1_files |>
  count(zip, file) |>
  filter(n > 1)

# Locate the nine selected variables in C2 raw folders
c2_raw_files <- list.files(
  path = file.path(finapres_root, "C2", "RawData"),
  pattern = " (HR|reSYS|reDIA|SV|CO|SVI|CI|SVR|SVRI)\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

length(c2_raw_files)
# Exclude files in the N_Merger_AHD folder
c2_raw_files <- c2_raw_files |>
  str_subset("/N_Merger_AHD/", negate = TRUE)

c2_files <- data.frame(
  folder = dirname(c2_raw_files),
  file = c2_raw_files
)
# Check
c2_files <- data.frame(
  folder = dirname(c2_raw_files),
  file = c2_raw_files
)

# Check the number of selected files in each C2 folder
nrow(c2_files)

c2_files |>
  count(folder) |>
  filter(n != 9)

# Combine C1 and C2 source-file indexes
fina_raw_files <- bind_rows(
  data.frame(
    cohort = "C1",
    zip = c1_files$zip,
    file = c1_files$file
  ),
  data.frame(
    cohort = "C2",
    zip = NA_character_,
    file = c2_files$file
  )
)
table(fina_raw_files$cohort)

fina_raw_files <- fina_raw_files |>
  mutate(
    recording_source = if_else(
      cohort == "C1",
      zip,
      dirname(file)
    )
  )

fina_raw_files |>
  count(cohort, recording_source) |>
  filter(n != 9)

# Read one raw file while retaining timestamps and markers
read_finapres_raw <- function(cohort, zip, file){if (cohort=="C1"){
  con <- unz(zip,file) 
  data <- read.csv2(file=con,skip=7,dec = ".",na.strings="",strip.white=TRUE,check.names=FALSE)

}else{

data <- read.csv2(file=file,skip=7,dec = ".",na.strings="",strip.white=TRUE,check.names=FALSE)
}
  data <- data[,!is.na(names(data)) & names(data) != "",
  drop = FALSE]
  return(data)}

# Test
finapres_test <- read_finapres_raw(
  fina_raw_files$cohort[1],
  fina_raw_files$zip[1],
  fina_raw_files$file[1]
)

names(finapres_test)

bracal_result_rows <- grepl(
  "BraCal:\\s*[0-9]",
  finapres_test[["Marker"]]
)

finapres_test[
  bracal_result_rows,
  c("Time(sec)", "Marker"),
  drop = FALSE
]
tail(finapres_test)
t_end <- tail(finapres_test[["Time(sec)"]],1)

t_start <- t_end - 200
t_stop <- t_end - 20

finapres_window_test <- finapres_test |>
  filter(
    between(.data[["Time(sec)"]], t_start, t_stop)
  )

range(finapres_window_test[["Time(sec)"]])

nrow(finapres_window_test)

finapres_test[, which(names(finapres_test) == ""), drop = FALSE]
names(finapres_test)

# Verify that all selected timestamps occur after the BraCal result

all(
finapres_window_test[["Time(sec)"]] >
    finapres_test[["Time(sec)"]][bracal_result_rows]

)


# Return the result count and latest numeric BraCal timestamp
get_bracal_info <- function(data) {
  result_rows <- grepl("BraCal:\\s*[0-9]", data[["Marker"]])
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

get_bracal_info(finapres_test)

# Store calibration information for the test recording
bracal_info_test <- get_bracal_info(finapres_test)

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
window_status_test <- if (bracal_info_test$n_bracal == 0) {
  "manual_no_bracal"
} else if (isTRUE(window_after_bracal)) {
  "after_bracal"
} else {
  "manual_window_review"
}

window_status_test

# Count valid and missing measurements in the selected window
n_valid_test <- sum(!is.na(finapres_window_test[[2]]))
n_missing_test <- sum(is.na(finapres_window_test[[2]]))

n_valid_test
n_missing_test 

n_valid_test + n_missing_test == nrow(finapres_window_test)

# Calculate the mean within the selected time window
mean_test <- mean(finapres_window_test[[2]],na.rm = TRUE)
mean_test

finapres_summary_test <- data.frame(
  t_end = t_end,
  t_start = t_start,
  t_stop = t_stop,
  n_bracal = bracal_info_test$n_bracal,
  bracal_time = bracal_info_test$bracal_time,
  window_status = window_status_test,
  n_valid = n_valid_test,
  n_missing = n_missing_test,
  Mean = mean_test
)

finapres_summary_test 

# Process all indexed Finapres files----
process_finapres_file <- function(cohort, zip, file) {
  data <- read_finapres_raw(cohort, zip, file)
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
  data.frame(
  cohort = cohort,
  zip = zip,
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

finapres_processed_test <- process_finapres_file(
  fina_raw_files$cohort[1],
  fina_raw_files$zip[1],
  fina_raw_files$file[1]
)

finapres_processed_test


finapres_time_results <- map_dfr(seq_len(nrow(fina_raw_files)),
function(i) { process_finapres_file(
  fina_raw_files$cohort[i],
  fina_raw_files$zip[i],
  fina_raw_files$file[i]
) }

)

dim(finapres_time_results)


# Identify records requiring manual window selection

table(finapres_time_results$window_status, useNA = "ifany")
finapres_manual_review <- finapres_time_results |>
  filter(window_status != "after_bracal")

finapres_manual_review
# Manual review is required for 63 variable files across seven recordings.
# C1: A HDT19, B HDT25, I HDT19, I HDT31, and M HDT7.
# C2: N BDC-6 and N R6.


finapres_time_results <- finapres_time_results |>
  mutate(
    recording_source = if_else(
      cohort == "C1",
      zip,
      dirname(file)
    )
  )

head(finapres_time_results$recording_source)

finapres_subject <- str_match(
  finapres_time_results$recording_source,
  "BRACE_SALT_FINAPRES_RAW_CSV_([^_]+)_"
)[, 2]

table(finapres_subject, useNA = "ifany")
finapres_stage <- str_match(
  finapres_time_results$recording_source,
  "BRACE_SALT_FINAPRES_RAW_CSV_[^_]+_([^_]+)_"
)[, 2]

table(finapres_stage, useNA = "ifany")

# Add identifiers and standardize analysis visit labels
finapres_time_results <- finapres_time_results |>
  mutate(
    Subject = finapres_subject,
    Stage_original = finapres_stage,
    Stage = recode(
      Stage_original,
      "BDC-10" = "BDC-12",
      "BDC-11" = "BDC-12",
      "HDT56" = "HDT55"
    ),
    Stage = gsub("-", "", Stage)
  ) 

table(finapres_time_results$Stage)

finapres_time_results |>
  count(cohort, Subject, Stage, Measurement) |>
  filter(n > 1)


names(lookup_clean)
str(finapres_time_results)



finapres_matched <- left_join(
  finapres_time_results,
  lookup_clean |> select(Subject, stage_key, StudyDay, ID, Group, Day),
  by = c("Subject" = "Subject", "Stage" = "stage_key")
) |> select(-zip,-file)

str(finapres_matched)
finapres_time_results |> anti_join(lookup_clean,by = c("Subject" = "Subject", "Stage" = "stage_key")) |> distinct(Subject, Stage)


# Manual review decisions:----
# A HDT19: use the 330–390 s window (60 s).
# M HDT7: use the 510–540 s window (30 s).
# These manually selected windows are shorter than the standard 180-s window.
# N BDC-6: exclude this recording because marked signal instability,abrupt changes, and interruptions prevent selection of a reliable analysis window.


# Define manually selected analysis windows
manual_windows <- tribble(
  ~Subject, ~Stage, ~manual_start_s, ~manual_stop_s,
  "A",      "HDT19", 330,            390,
  "M",      "HDT7",  510,            540,
)

# Verify that source-file identities are unique
duplicate_sources <- finapres_time_results |>
  count(cohort, zip, file) |>
  filter(n > 1)

# Identify files requiring manual window replacement
manual_files <- finapres_time_results |>
  inner_join(
    manual_windows,
    by = c("Subject", "Stage"),
    relationship = "many-to-one"
  )

# Recalculate means using the manually selected windows
manual_results <- map_dfr(seq_len(nrow(manual_files)), function(i) {
  record <- manual_files[i, ]
  print(message(
  "Processing ", i, "/", nrow(manual_files),
  ": ", basename(record$file)
))
  data <- read_finapres_raw(
    record$cohort,
    record$zip,
    record$file
  )
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
    cohort = record$cohort,
    zip = record$zip,
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
})

# Retain excluded records and document the reason
finapres_excluded <- finapres_time_results |>
  filter(Subject == "N", Stage == "BDC6") |>
  mutate(
    exclusion_reason = paste(
      "Marked signal instability, abrupt changes, and interruptions;",
      "no reliable analysis window identified."
    )
  )

# Apply manual results while preserving standard-window results
finapres_final_results <- finapres_time_results |>
  filter(!(Subject == "N" & Stage == "BDC6")) |>
  left_join(
    manual_results,
    by = c("cohort", "zip", "file"),
    relationship = "one-to-one",
    na_matches = "na"
  ) |>
  mutate(
    standard_start_s = t_start,
    standard_stop_s = t_stop,
    standard_mean = Mean,
    standard_n_valid = n_valid,
    standard_n_missing = n_missing,
    standard_window_status = window_status,

    manually_selected = !is.na(manual_start_s),

    t_start = if_else(
      manually_selected, manual_start_s, t_start
    ),
    t_stop = if_else(
      manually_selected, manual_stop_s, t_stop
    ),
    n_valid = if_else(
      manually_selected, manual_n_valid, n_valid
    ),
    n_missing = if_else(
      manually_selected, manual_n_missing, n_missing
    ),
    Mean = if_else(
      manually_selected, manual_mean, Mean
    ),

    window_status = if_else(
      manually_selected,
      "manually_selected",
      window_status
    ),

    review_reason = case_when(
      Subject == "A" & Stage == "HDT19" ~
        "Manually selected 330–390 s window.",
      Subject == "M" & Stage == "HDT7" ~
        "Manually selected 510–540 s window.",
      TRUE ~ NA_character_
    ),

    window_duration_s = t_stop - t_start
  ) |>
  select(
    -manual_start_s, -manual_stop_s,
    -manual_n_valid, -manual_n_missing, -manual_mean
  )

# Inspect manually selected results
finapres_final_results |>
  filter(manually_selected) |>
  select(
    Subject, Stage, Measurement,
    t_end,
    t_start, t_stop, window_duration_s,
    n_valid, n_missing, Mean
  )



# Verify that only A HDT19 and M HDT7 have manual windows
stopifnot(
  nrow(finapres_final_results) == 3231,
  nrow(finapres_excluded) == 9,
  sum(finapres_final_results$manually_selected) == 18
)

# Save final results
saveRDS(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.RDS")
)

write_csv(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.csv")
)

