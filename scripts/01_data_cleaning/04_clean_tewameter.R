# Tewameter data cleaning
# X. Peng
# This script processes raw Tewameter Excel files from C1 and C2.
#
# Workflow:
# 1. Find all raw Tewameter files and exclude test and Overview files.
# 2. Read each file's Raw Data sheet.
# 3. Extract TEWL measurements recorded between 35 and 55 seconds.
# 4. Calculate the mean, median, SD, CV, and IQR within each measurement's 35–55 s window.
# 5. Inspect the distribution of within-measurement CV using a histogram.
# 6. Flag measurements with CV >7.5% and plot their original TEWL time-series curves for manual inspection.
# 7. Exclude measurements with CV >7.5% from subsequent aggregation, while retaining their results and exclusion flags for traceability.
# 8. Count the remaining valid measurements for each subject and date.
# 9. For subject-days with at least 3 valid measurements,
#    calculate the median of the retained 35–55 s measurement means.
# 10. Match daily summaries to the lookup table using subject and date
#     to obtain ID, Group, Stage, StudyDay, and other study information.



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
  str_subset("/test001/", negate = TRUE) |>
  str_subset(
    regex("Overview", ignore_case = TRUE),
    negate = TRUE
  )

length(tewameter_files)

lookup_clean <- readRDS("data/processed_data/lookup/lookup_table.RDS")


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
names(tewameter_35_55_wide )

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

## Inspect one raw Tewameter file for quality assessment ##

assess_tewl_35_55 <- function(file) {
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
          measurement_id = names(raw_data)[start],
          n_total = n(),
          n_valid = sum(!is.na(tewl_g_m2_h)),
          tewl_mean = if (n_valid > 0) {
            mean(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          tewl_median = if (n_valid > 0) {
            median(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          tewl_sd = if (n_valid >= 2) {
            sd(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          tewl_iqr = if (n_valid > 0) {
            IQR(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          tewl_min = if (n_valid > 0) {
            min(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          tewl_max = if (n_valid > 0) {
            max(tewl_g_m2_h, na.rm = TRUE)
          } else NA_real_,
          cv_pct = if_else(tewl_mean > 0 & n_valid >= 2,100 * tewl_sd / tewl_mean,NA_real_),
          upper_dev_pct = if_else(tewl_mean > 0 & n_valid >= 2,100 * (tewl_max - tewl_mean) / tewl_mean,NA_real_),
          lower_dev_pct = if_else(tewl_mean > 0 & n_valid >= 2,100 * (tewl_mean - tewl_min) / tewl_mean,NA_real_),
          mean_median_diff_pct=if_else(tewl_mean > 0 & n_valid >= 2,100 * abs(tewl_mean - tewl_median) / tewl_mean,NA_real_  )
          
        )
    }
  )
}

tewl_qc_one<- assess_tewl_35_55(tewameter_files[1])


tewl_qc_one

## Assess all Tewameter files while retaining source paths ##


tewameter_qc <- map_dfr(
  tewameter_files,
  function(file) {
    assess_tewl_35_55(file) |>
      mutate(source_file = file)
  }
)
dim(tewameter_qc)



## Distinguish empty windows from entirely missing measurements ##
tewameter_qc |>
  filter(n_valid == 0) |>
  count(n_total)

tewl_empty_windows <- tewameter_qc |>
  filter(n_total == 0) |>
  select(source_file, measurement_id, n_total, n_valid)

tewameter_qc <- tewameter_qc |>
  mutate(
    across(
      c(tewl_mean, tewl_median, tewl_sd, tewl_iqr, tewl_min, tewl_max,
        cv_pct, upper_dev_pct, lower_dev_pct, mean_median_diff_pct),
      ~ replace(.x, n_valid == 0, NA_real_)
    ),
    qc_status = case_when(
      n_total == 0 ~ "no_data_in_window",
      n_valid == 0 ~ "all_values_missing",
      n_valid < 2 ~ "insufficient_for_cv",
      TRUE ~ "pending_review"
    )
  )

table(tewameter_qc$qc_status, useNA = "ifany")


# Inspect the complete within-measurement CV distribution
cv_histogram <- tewameter_qc |>
  filter(is.finite(cv_pct)) |>
  ggplot(aes(x = cv_pct)) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    fill = "#4C78A8",
    color = "white"
  ) +
  labs(
    x = "Within-measurement CV (%)",
    y = "Number of measurements"
  ) +
  theme_classic()

cv_histogram

# Find the maximum finite CV  66.50418
max(
  tewameter_qc$cv_pct[is.finite(tewameter_qc$cv_pct)],
  na.rm = TRUE
)

# Zoom into the main CV distribution with finer axis ticks
cv_histogram +
  coord_cartesian(xlim = c(0, 15)) +
  scale_x_continuous(breaks = seq(0, 15, by = 1))


# Classify measurements using the 7.5% within-window CV threshold
tewameter_qc <- tewameter_qc |>
  mutate(
    cv_status = case_when(
      qc_status == "no_data_in_window" ~ "no_data_in_window",
      !is.finite(cv_pct) ~ "cv_unavailable",
      cv_pct > 7.5 ~ "excluded_cv_above7.5",
      TRUE ~ "retained"
    )
  )

# Count measurements in each category
tewameter_qc |>
  count(cv_status)

# Classify measurements using the within-window CV threshold
tewameter_qc <- tewameter_qc |>
  mutate(
    cv_status = case_when(
      n_valid == 0 ~ "no_valid_data",
      n_valid < 2 ~ "insufficient_for_cv",
      !is.finite(cv_pct) ~ "cv_unavailable",
      cv_pct > 7.5 ~ "excluded_cv_over7.5",
      TRUE ~ "retained"
    )
  )

# Check classification counts
tewameter_qc |>
  count(cv_status)

# Identify measurements requiring manual curve review
tewl_cv_review <- tewameter_qc |>
  filter(is.finite(cv_pct), cv_pct > 7.5)

# Extract the complete time series for one selected measurement
extract_tewl_curve <- function(file, measurement_id) {
  raw_data <- read_excel(file, sheet = "Raw Data")

  starts <- which(str_detect(names(raw_data), "^Measurement_"))
  ends <- c(starts[-1] - 1, ncol(raw_data))

  block <- match(measurement_id, names(raw_data)[starts])

  columns <- starts[block]:ends[block]
  measurement <- raw_data[-1, columns]
  names(measurement) <- as.character(raw_data[1, columns])

  measurement |>
    transmute(
      elapsed_time_s = as.numeric(`Elapsed Time [s]`),
      tewl_g_m2_h = as.numeric(`TEWL [g/m²/h]`)
    ) |>
    filter(!is.na(elapsed_time_s)) |>
    mutate(
      source_file = file,
      measurement_id = measurement_id
    )
}

# Test extraction on the measurement with the highest CV
tewl_curve_test <- extract_tewl_curve(
  tewl_cv_review$source_file[1],
  tewl_cv_review$measurement_id[1]
)

head(tewl_curve_test)
range(tewl_curve_test$elapsed_time_s)

# Inspect the complete curve and highlight the target analysis window
ggplot(tewl_curve_test, aes(x = elapsed_time_s, y = tewl_g_m2_h)) +
  annotate(
    "rect",
    xmin = 35, xmax = 55,
    ymin = -Inf, ymax = Inf,
    fill = "grey80", alpha = 0.5
  ) +
  geom_line(color = "#4C78A8", linewidth = 0.6) +
  geom_point(size = 1) +
  geom_vline(
    xintercept = c(35, 55),
    linetype = "dashed", color = "grey50"
  ) +
  coord_cartesian(
    xlim = c(0, max(60, max(tewl_curve_test$elapsed_time_s)))
  ) +
  labs(
    title = "N — Measurement 6",
    subtitle = paste0(
      "Within-window CV: ",
      round(tewl_cv_review$cv_pct[1], 2), "%"
    ),
    x = "Elapsed time (s)",
    y = "TEWL (g/m²/h)"
  ) +
  theme_classic()

# Extract complete curves for all measurements exceeding CV 7.5%
tewl_review_curves <- map_dfr(
  seq_len(nrow(tewl_cv_review)),
  function(i) {
    extract_tewl_curve(
      tewl_cv_review$source_file[i],
      tewl_cv_review$measurement_id[i]
    ) |>
      mutate(
        review_id = i,
        cv_pct = tewl_cv_review$cv_pct[i]
      )
  }
) |>
  mutate(
    Subject = str_match(source_file, "/Tewameter/([^/]+)/")[, 2],
    visit_folder = str_match(
      source_file, "/Tewameter/[^/]+/([^/]+)/"
    )[, 2],
    panel_label = paste0(
      review_id, ". ", Subject,
      " | Folder ", visit_folder,
      " | ", measurement_id,
      "\nCV = ", round(cv_pct, 2), "%"
    ),
    panel_label = factor(panel_label, levels = unique(panel_label))
  )



# Save six measurement curves per PDF page
pdf(
  file.path(tewameter_output_dir, "tewl_cv_over7.5_curve_review.pdf"),
  width = 11, height = 8
)

for (page in seq_len(ceiling(nrow(tewl_cv_review) / 6))) {
  page_ids <- seq.int(
    (page - 1) * 6 + 1,
    min(page * 6, nrow(tewl_cv_review))
  )

  p <- tewl_review_curves |>
    filter(review_id %in% page_ids) |>
    ggplot(aes(x = elapsed_time_s, y = tewl_g_m2_h)) +
    annotate(
      "rect",
      xmin = 35, xmax = 55,
      ymin = -Inf, ymax = Inf,
      fill = "grey80", alpha = 0.5
    ) +
    geom_line(color = "#4C78A8", linewidth = 0.5) +
    geom_point(size = 0.7, na.rm = TRUE) +
    geom_vline(
      xintercept = c(35, 55),
      linetype = "dashed", color = "grey50"
    ) +
    facet_wrap(~ panel_label, ncol = 2, scales = "free_y") +
    scale_x_continuous(
      limits = NULL,
      breaks = seq(0, 120, 20)
    ) +
    expand_limits(x = c(0, 60)) +
    labs(
      x = "Elapsed time (s)",
      y = "TEWL (g/m²/h)",
      caption = "Shaded area: target analysis window, 35–55 s."
    ) +
    theme_classic() +
    theme(strip.text = element_text(size = 9))

  print(p)
}

dev.off()

# Count measurements per subject and date
tewl_daily_counts <- tewameter_qc |>
  mutate(
    cohort = str_match(source_file, "/(C[12])/Tewameter/")[, 2],
    Subject = str_match(source_file, "/Tewameter/([^/]+)/")[, 2],
    measurement_date = as.Date(
      str_extract(basename(source_file), "\\d{4}_\\d{2}_\\d{2}"),
      format = "%Y_%m_%d"
    )
  ) |>
  group_by(cohort, Subject, measurement_date) |>
  summarise(
    n_total = n(),
    n_with_data = sum(n_valid > 0),
    n_retained = sum(is.finite(cv_pct) & cv_pct <= 7.5),
    .groups = "drop"
  ) |>
  arrange(cohort, Subject, measurement_date)

# Summarize remaining measurement counts
table(tewl_daily_counts$n_retained, useNA = "ifany")

# Identify subject-days with fewer than three eligible measurements
tewl_daily_counts |>
  filter(n_retained < 3)

# Calculate daily medians using all measurements with CV <=7.5%
tewl_daily_median <- tewameter_qc |>
  mutate(
    cohort = str_match(source_file, "/(C[12])/Tewameter/")[, 2],
    Subject = str_match(source_file, "/Tewameter/([^/]+)/")[, 2],
    measurement_date = as.Date(
      str_extract(basename(source_file), "\\d{4}_\\d{2}_\\d{2}"),
      format = "%Y_%m_%d"
    ),
    retained = is.finite(cv_pct) & cv_pct <= 7.5
  ) |>
  group_by(cohort, Subject, measurement_date) |>
  summarise(
    n_retained = sum(retained),
    daily_median = if (n_retained > 0) {
      median(tewl_mean[retained], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  arrange(cohort, Subject, measurement_date)

head(tewl_daily_median)

# Prepare lookup information for subject-date matching
tewl_lookup <- lookup_clean |>
  transmute(
    Subject = as.character(Subject),
    measurement_date = as.Date(Date),
    ID, Group, Stage, stage_key, StudyDay, Day
  ) |>
  distinct()

# Match daily TEWL summaries to the lookup table
tewl_daily_matched <- tewl_daily_median |>
  left_join(
    tewl_lookup,
    by = c("Subject", "measurement_date"),
    relationship = "many-to-one",
    na_matches = "never"
  )

# Identify subject-days without a lookup match
tewl_daily_matched |>
  filter(is.na(ID)) |>
  select(cohort, Subject, measurement_date, n_retained)

# Check lookup matching
tewl_daily_matched |>
  summarise(
    n_subject_days = n(),
    n_unmatched = sum(is.na(ID))
  )

# Standardize analysis visit labels
tewl_daily_matched <- tewl_daily_matched |>
  mutate(
    Stage_original = as.character(Stage),
    stage_key_original = stage_key,

    Stage = recode(
      Stage_original,
      "BDC-10" = "BDC-12",
      "BDC-11" = "BDC-12",
      "BDC10" = "BDC12",
      "BDC11" = "BDC12",
      "HDT50" = "HDT49",
      "HDT56" = "HDT55"
    ),

    stage_key = recode(
      stage_key,
      "BDC10" = "BDC12",
      "BDC11" = "BDC12",
      "HDT50" = "HDT49",
      "HDT56" = "HDT55"
    )
  )

# Check standardized visit labels
table(tewl_daily_matched$Stage, useNA = "ifany")

# Check whether relabeling created duplicate subject-visit records
tewl_daily_matched |>
  count(cohort, Subject, stage_key) |>
  filter(n > 1)


# Save daily TEWL medians with lookup information
saveRDS(
  tewl_daily_matched,
  file.path(tewameter_output_dir, "tewl_daily_matched.RDS")
)

write_csv(
  tewl_daily_matched,
  file.path(tewameter_output_dir, "tewl_daily_matched.csv")
)

