# Finapres data cleaning
# X. Peng
# Purpose:
# Process individual raw Finapres CSV files from cohorts C1 and C2.
# Select analysis windows and calculate descriptive statistics.
# Inspect recording quality before saving the final results.

# Workflow:
# Select 10 variables from C1 and C2; exclude N_Merger_AHD files.
# Select 200–20 s before each file ends and check against the latest BraCal.
# Use manual windows for A HDT19 (330–390 s) and M HDT7 (510–540 s); 
# Calculate mean, median, SD, IQR, and valid/missing counts.
# exclude N BDC-6 for poor recording quality
# Plot HR, reSYS, and reDIA records with absolute mean–median differences > 3.


# Libraries ----
library(tidyverse)


# Functions ----
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

# Output directories ----
finapress_output_dir <- "data/processed_data/03_finapress"
dir.create(finapress_output_dir, recursive = TRUE, showWarnings = FALSE)

# Open data ----
lookup_clean <- readRDS("data/processed_data/lookup/lookup_table.RDS")
c1_extract_dir <- file.path(
  "data/Original_data/03 - FINAPRESS", "C1", "RawData", "CSV EXTRACTED"
)
# Find the nine selected variables 
finapres_raw_files <- list.files(
  path = c(
    c1_extract_dir,
    file.path( "data/Original_data/03 - FINAPRESS", "C2", "RawData")
  ),
  pattern = " (HR|reSYS|reDIA|SV|CO|SVI|CI|SVR|SVRI|BRS)\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
) |>
  str_subset("/N_Merger_AHD/", negate = TRUE)

length(finapres_raw_files)

finapres_file_index <- tibble(file= finapres_raw_files ) |> mutate(
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
  ) |> left_join(lookup_clean |> select("Subject","stage_key", "ID", "Group", "StudyDay", "Day", "Date", "DateTime" ),
    by=c("Subject", "stage_key"),relationship="many-to-one"

  )

head(finapres_file_index)

manual_windows <- tribble(
  ~Subject,~Stage                            , ~manual_start_s , ~manual_stop_s ,
  "A" , "HDT19",            330 ,            390 ,
  "M" ,   "HDT7"  ,        510 ,            540
)

finapres_file_index <- left_join(finapres_file_index,manual_windows,by =c("Subject", "Stage"),relationship="many-to-one")

# Test the reader on one file
finapres_test <- read_finapres_raw(finapres_raw_files[1])

names(finapres_test)
head(finapres_test)

# Count numeric BraCal results and find the latest timestamp
result_rows <- grepl(
  "BraCal:\\s*[0-9]",
  finapres_test[["Marker"]]
)

n_bracal <- sum(result_rows)

bracal_time <- if (n_bracal == 0) {
  NA_real_
} else {
  max(finapres_test[["Time(sec)"]][result_rows])
}

bracal_info_test <- list(
  n_bracal = n_bracal,
  bracal_time = bracal_time
)



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


# Process all selected Finapres files
finapres_time_results <- map_dfr(
  seq_len(nrow(finapres_file_index)),
  function(i) {
    record <- finapres_file_index[i, ]
    file <- record$file
    data <- read_finapres_raw(file)
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
    t_end <- tail(data[["Time(sec)"]], 1)
    manually_selected <- !is.na(record$manual_start_s)
    t_start <- coalesce(record$manual_start_s,t_end - 200) 
    t_stop <- coalesce(record$manual_stop_s,t_end - 20) 
    window <- data |>
      filter(
        between(.data[["Time(sec)"]], t_start, t_stop)
      )
    window_after_bracal <- if (nrow(window) > 0) {
      all(window[["Time(sec)"]] > bracal_time)
    } else {
      FALSE
    }
window_status <- if (manually_selected) {
  "manually_selected"
} else if (n_bracal == 0) {
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
    median_value <- if (n_valid > 0) {
      median(window[[2]], na.rm = TRUE)
    } else {
      NA_real_
    }

    sd_value <- if (n_valid > 1) {
      sd(window[[2]], na.rm = TRUE)
    } else {
      NA_real_
    }

    iqr_value <- if (n_valid > 0) {
      IQR(window[[2]], na.rm = TRUE)
    } else {
      NA_real_
    }

    bind_cols(record,tibble(
      Measurement = names(data)[2] |> str_replace_all("[^A-Za-z0-9]+","_") |>  str_remove("_$"),
      t_end = t_end,
      t_start = t_start,
      t_stop = t_stop,
      n_bracal = n_bracal,
      bracal_time = bracal_time,
      window_after_bracal = window_after_bracal,
      window_status = window_status,
      n_valid = n_valid,
      n_missing = n_missing,
      Mean = mean_value,
      Median = median_value,
      SD = sd_value,
      IQR = iqr_value,
      manually_selected = manually_selected
    ))
  }
)

dim(finapres_time_results)

# Identify records requiring manual window selection
finapres_time_results |>
  count(window_status)


# Manual review is required for 63 variable files across seven recordings.
# C1: A HDT19, B HDT25, I HDT19, I HDT31, and M HDT7.
# C2: N BDC-6 and N R6.
# Manual review decisions:----
# N BDC-6: exclude the recording because of poor signal quality.
# Define manual windows using recording folder identifiers


# Identify N BDC-6 records excluded after manual review
finapres_final_results <- finapres_time_results |>
  filter(!(Subject == "N" & Stage == "BDC-6")) |>
  mutate(window_duration_s = t_stop - t_start)


str(finapres_final_results)

# Check subject and visit labels
table(finapres_final_results$Subject, useNA = "ifany")
table(finapres_final_results$Stage, useNA = "ifany")

# Check for duplicate measurement records
finapres_final_results |>
  count(Subject, stage_key, Measurement) |>
  filter(n > 1)

# Order result columns and rows
finapres_final_results <- finapres_final_results |>relocate(Subject, ID, Group, Stage, stage_key, StudyDay, Day, Date, DateTime) |>
  arrange(ID, Day, Stage, Measurement)

# Finapres window quality checks ----
table(finapres_time_results$Measurement)
finapres_time_results |>
  filter(Measurement %in% c("HR_bpm", "reSYS_mmHg","reDIA_mmHg")) |>
  mutate(mean_median_gap = abs(Mean - Median)) |>
  group_by(Measurement) |>
  slice_max(mean_median_gap, n = 10, with_ties = FALSE) |>
  ungroup() |>
  select(Subject, Stage, Measurement, Mean, Median, SD, IQR,
         n_valid, mean_median_gap) |>
  print(n = 30)

review_records <- finapres_time_results |>
  filter(Measurement %in% c("HR_bpm", "reSYS_mmHg","reDIA_mmHg")) |>
  mutate(mean_median_gap = abs(Mean - Median)) |>
  filter(mean_median_gap > 3) |>
  arrange(Measurement, desc(mean_median_gap))

review_records |> count(Measurement)

review_plot_data <- map_dfr(
  seq_len(nrow(review_records)),
  function(i) {
    record <- review_records[i, ]
    raw <- read_finapres_raw(record$file)

    tibble(
      Subject = record$Subject,
      Stage = record$Stage,
      Measurement = record$Measurement,
      Time_s = raw[["Time(sec)"]],
      Value = raw[[2]],
      t_start = record$t_start,
      t_stop = record$t_stop
    )
  }
)


ggplot(review_plot_data, aes(x = Time_s, y = Value)) +
  geom_line() +  geom_vline(
    data = review_records,
    aes(xintercept = t_start),
    linetype = "dashed", colour = "blue"
  ) +
  geom_vline(
    data = review_records,
    aes(xintercept = t_stop),
    linetype = "dashed", colour = "red"
  ) +
  facet_wrap(
    ~ Subject + Stage + Measurement,
    scales = "free_y",
    ncol = 2
  ) +
  labs(x = "Time (s)", y = NULL) +
  theme_bw()


table(finapres_final_results$Measurement)




# Save final results
saveRDS(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.RDS")
)

write_csv(
  finapres_final_results,
  file.path(finapress_output_dir, "finapres_final_results.csv")
)


