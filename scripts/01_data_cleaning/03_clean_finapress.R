




##combine fina arm_cuff
arm_cuff_files <- list.files(
  "data/Original_data/03 - FINAPRESS",
  pattern = "Arm Cuff\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
arm_cuff_files

length(arm_cuff_files)

read_arm_cuff <- function(file) {
  read_delim(
    file,
    delim = ";",
    skip = 7,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    mutate(source_file = file)
}
arm_cuff_all <- map_dfr(arm_cuff_files, read_arm_cuff)

dim(arm_cuff_all)
str(arm_cuff_all)

bp_fina_clean <- arm_cuff_all |>
  mutate(
    cohort = str_match(source_file, "/(C[12])/")[, 2],

    Subject = str_match(
      source_file,
      "/(?:COMBINED|RawData)/([A-Z])/"
    )[, 2],

    ID = as.factor(str_c("BRACE_", Subject)),

    Stage = str_extract(
      source_file,
      "(?:BDC-?\\d+|HDT\\d+|R\\+?\\d+)"
    ),

    stage_key = normalize_stage_key(Stage),

    StudyDay = as.factor(make_study_day(stage_key)),
    systolic= as.numeric(`SYS Arm(mmHg)`),
mean_bp = as.numeric(`MAP Arm(mmHg)`),
diastolic = as.numeric(`DIA Arm(mmHg)`),
heart_rate= as.numeric(`HR Arm(bpm)`)

  )|>
  select(source_file, cohort, Subject, ID, Stage, stage_key, StudyDay,systolic,mean_bp,diastolic ,heart_rate) 

dim(bp_fina_clean)
str(bp_fina_clean)
