# Tewameter data cleaning
# X. Peng
# This script processes raw Tewameter Excel files from C1 and C2.
#
# Workflow:
# 1. Find all raw Tewameter files and exclude test and Overview files.
# 2. Read each file's Raw Data sheet.
# 3. Extract TEWL measurements recorded between 35 and 55 seconds.
# 4. Calculate the mean TEWL value for each measurement.
# 5. Combine TEWL results with the Summary sheet from the same source file.



# Libraries
library(tidyverse)
library(readxl)

# Output directories
tewameter_output_dir <- "data/processed_data/04_tewameter"
dir.create(
  tewameter_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# open data 
tewameter_files <- c(
  list.files(
    "data/Original_data/04 - TEWAMETER/C1/Tewameter",
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  ),
  list.files(
    "data/Original_data/04 - TEWAMETER/C2/Tewameter",
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )
) |>
  str_subset("/test001/", negate = TRUE)

length(tewameter_files)




# # C2 includes Overview Excel files that are not raw Tewameter measurements.
# Exclude them before processing to avoid errors and duplicate summaries.
tewameter_files |>
  str_subset("Overview")

tewameter_files <- tewameter_files |>
  str_subset("Overview", negate = TRUE)
length(tewameter_files)

#function
extract_tewl_35_55 <- function(file) {
  raw_data <- read_excel(file, sheet = "Raw Data")

  measurement_starts <- which(
    str_detect(names(raw_data), "^Measurement_")
  )

  measurement_ends <- c(
    measurement_starts[-1] - 1,
    ncol(raw_data)
  )

  map2_dfr(
    measurement_starts,
    measurement_ends,
    \(start, end) {
      one_measurement <- raw_data[-1, start:end]
      names(one_measurement) <- as.character(raw_data[1, start:end])

      one_measurement |>
        transmute(
          elapsed_time_s = as.numeric(`Elapsed Time [s]`),
          tewl_g_m2_h = as.numeric(`TEWL [g/m²/h]`)
        ) |>
        filter(between(elapsed_time_s, 35, 55)) |>
        summarise(
          tewl_g_m2_h = mean(tewl_g_m2_h, na.rm = TRUE)
        )
    }
  )
}

#test function
#tewl_one_file <- extract_tewl_35_55("data/Original_data/04 - TEWAMETER/C1/Tewameter/A/0/2023_04_09_17_06_58_[02].xlsx")
#tewl_one_file

tewameter_35_55 <- map_dfr(
  tewameter_files,
  \(file) {
    extract_tewl_35_55(file) |>
      mutate(source_file = file)
  }
)

dim(tewameter_35_55)
head(tewameter_35_55)
names(
tewameter_35_55
)


tewameter_35_55 <- tewameter_35_55 |>
  group_by(source_file) |>
  mutate(measurement_number = row_number()) |>
  ungroup()

tewameter_35_55_wide <- tewameter_35_55 |>
  select(source_file, measurement_number, tewl_g_m2_h) |>
  pivot_wider(
    names_from = measurement_number,
    values_from = tewl_g_m2_h,
    names_glue = "tewl_measurement_{measurement_number}"
  )

dim(tewameter_35_55_wide )
names(tewameter_35_55_wide 
)

tewameter_summary <- map_dfr(
  tewameter_files,
  \(file) {
    read_excel(
      file,
      sheet = 1,
      col_types = "text"
    ) |>
      mutate(source_file = file)
  }
)

dim(tewameter_summary)


tewameter_clean <- tewameter_summary |>
  left_join(
    tewameter_35_55_wide ,
    by = "source_file"
  )

dim(tewameter_summary)

dim(tewameter_clean)

write_csv(
  tewameter_35_55,
  file.path(tewameter_output_dir, "tewameter_35_55.csv")
)

saveRDS(
  tewameter_35_55,
  file.path(tewameter_output_dir, "tewameter_35_55.RDS")
)

write_csv(
  tewameter_clean,
  file.path(tewameter_output_dir, "tewameter_clean.csv")
)

saveRDS(
  tewameter_clean,
  file.path(tewameter_output_dir, "tewameter_clean.RDS")
)
