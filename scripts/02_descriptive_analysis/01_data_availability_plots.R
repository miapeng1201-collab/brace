# Exploratory plots


# Libraries
library(Amelia)

## Open data ##
daily_output_dir <- "data/processed_data/body_weight/daily_body_weight"
dexa_output_dir <- "data/processed_data/body_weight/dexa_body_composition"
mri_output_dir <- "data/processed_data/body_weight/mri_muscle"
daily_bw_clean <- readRDS(file.path(daily_output_dir,"/daily_body_weight_clean.RDS"))
dexa_clean <- readRDS(file.path(dexa_output_dir,"dexa_clean.RDS"))
mri_clean <-  readRDS(file.path(mri_output_dir, "body_weight_mri_clean.RDS"))

## Check the data
names(daily_bw_clean)
names(dexa_clean)
names(mri_clean)

mergeddf <- daily_bw_clean |> left_join(dexa_clean)
names(mergeddf)

mergeddf2 <- mergeddf |>
  select(ID, daily_body_weight_kg, 13:49) |> 
  as.data.frame()

missmap(
  mergeddf2,
  csvar = "ID",
  rank.order = FALSE,
  main = "Daily body weight and DEXA missingness"
)

mri <- as.data.frame(mri_clean)

missmap(
  mri,
  csvar = "ID",
  rank.order = FALSE,
  main = "MRI missingness"
)


