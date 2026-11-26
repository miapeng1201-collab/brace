# Finapres raw data extraction
# X. Peng
#
# Purpose:
# Extract C1 ZIP files so C1 and C2 CSV files can be read in the same way.
# Use the readable F BDC-6 backup and skip misplaced G R1 files
# in the H HDT55 archive. skipping misplaced G R1 files.

# Output directories ----
c1_extract_dir <- file.path("data/Original_data/03 - FINAPRESS", "C1", "RawData", "CSV EXTRACTED")
dir.create(c1_extract_dir, recursive = TRUE, showWarnings = FALSE)

# Open data ----
finapres_root <- "data/Original_data/03 - FINAPRESS"




## Locate C1 raw CSV archives
c1_zip_files <- list.files(path = file.path(finapres_root, "C1", "RawData", "CSV RAW"),pattern = "\\.zip$",full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE)

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

