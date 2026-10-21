# Anthropometry data cleaning
# X. Peng
#
# This script processes baseline age, height, and weight data
# collected at BDC-14.



# Libraries
library(tidyverse)
library(readxl)

# Output directory
anthropometry_output_dir <- "data/processed_data/06_anthropometry"
dir.create(
  anthropometry_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Open data
# The C1 Excel file already contains C2 data.
anthropometry_C1 <- read_excel(
  "data/Original_data/01 - GENERAL DATA/01 - ANTHROMORPHOMETRY/BRACE_GEN_SUB-CHARACTERITICS_ALLC1.xlsx"
)
lookup_clean <- readRDS(
  "data/processed_data/lookup/lookup_table.RDS"
)

names(anthropometry_C1)

anthropometry_clean <- anthropometry_C1 |>
  transmute(
    ID = as.factor(str_c("BRACE_", Subjects)),
    StudyDay = "Baseline_min14",
    age_years = `Age\n(yrs)`,
    height_cm = `Height (cm)`,
    weight_kg = `Weight (kg)`
  )|>
  left_join(
    lookup_clean |>
      distinct(ID, Group),
    by = "ID"
  )

str(anthropometry_clean)

write_csv(
  anthropometry_clean,
  file.path(
    anthropometry_output_dir,
    "anthropometry_clean.csv"
  )
)

saveRDS(
  anthropometry_clean,
  file.path(
    anthropometry_output_dir,
    "anthropometry_clean.RDS"
  )
)
