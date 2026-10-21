# Nutrition intake and liquid balance data cleaning
# X. Peng
#
# This script processes daily nutrition intake and liquid balance data
#
# Workflow:
# 1. Read and combine C1 and C2 nutrition data.
# 2. Create standard ID, StudyDay, and Group variables.
# 3. Convert nutrition variables to numeric values.
# 4. Standardize and combine C1 and C2 liquid balance data.
# 5. Save clean nutrition and liquid balance datasets.
#
# Outputs:
# - nutrition_clean: daily energy, macronutrient, vitamin, and mineral intake.
# - liquid_balance_clean: daily liquid intake, urine output, and liquid balance.
#
# Data note:
# Review the comments column before analysis



# Libraries
library(tidyverse)
library(readxl)

#output
intake_output_dir <- "data/processed_data/08_intake"
dir.create(
  intake_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

#open data

intake_input_dir <- "data/Original_data/01 - GENERAL DATA/04 - NUTRITION INTAKE & LIQUID BALANCE"
nutrition_raw <- bind_rows(
  read_excel(
    file.path(intake_input_dir, "BRACE_GEN_NUTRITION_ALL.xlsx"),
    sheet = "NUTRITION_BRACEC1",
    col_types = "text"
  ) |>
    mutate(cohort = "C1"),

  read_excel(
    file.path(intake_input_dir, "BRACE_GEN_NUTRITION_ALL.xlsx"),
    sheet = "NUTRITION_BRACEC2",
    col_types = "text"
  ) |>
    mutate(cohort = "C2")
)

liquid_balance_C1 <- read_excel(
  file.path(
    intake_input_dir,
    "BRACE_GEN_LIQUID-BALANCE_ALLC1.xlsx"
  )
)

liquid_balance_C2 <- read_excel(
  file.path(
    intake_input_dir,
    "BRACE_GEN_LIQUID-BALANCE_ALLC2.xlsx"
  )
)


lookup_clean <- readRDS(
  "data/processed_data/lookup/lookup_table.RDS"
)

str(nutrition_raw)
str(liquid_balance_C1)
str(liquid_balance_C2)




# function
normalize_stage_key <- function(stage) {
  stage |>
    str_replace_all("[-+]", "") |>
    str_replace_all("\\s+", "") |>
    str_to_upper()
}

make_study_day <- function(stage_key) {
  case_when(
    str_detect(stage_key, "BDC") ~ str_replace(stage_key, "BDC", "Baseline_min"),
    str_detect(stage_key, "HDT") ~ str_replace(stage_key, "HDT", "Intervention_"),
    str_detect(stage_key, "R") ~ str_replace(stage_key, "R", "Recovery_")
  )
}




## cleaned intake data.

nutrition_clean <- nutrition_raw |> 
  transmute(
  cohort,
  ID=as.factor(str_c("BRACE_", str_extract(Subjects, "^[A-Z]"))),
  StudyDay = make_study_day(
  normalize_stage_key(`Study day`)
),
  `Prescribed Energy (Kcal)` = parse_number(
      `Prescribed Energy (Kcal)`
    ),
    across(
      `ENER (Kcal)`:`Zn (mg)`,
      \(x) parse_number(x, na = c("", "NA", "ND"))# "ND" values are treated as missing values.
    )
  ) |>
  left_join(
    lookup_clean |>
      distinct(ID, StudyDay, Group),
    by = c("ID", "StudyDay")
  )  |>
  rename(
    prescribed_energy_kcal = `Prescribed Energy (Kcal)`,
    energy_kcal = `ENER (Kcal)`,

    protein_g = `PRO (g)`,
    protein_pct_energy = `PRO (%AET)`,
    protein_g_kg_bw = `PRO (g/kgBW)`,

    carbohydrate_g = `GLU (g)`,
    carbohydrate_pct_energy = `GLU (%AET)`,

    fat_g = `LIP (g)`,
    fat_pct_energy = `LIP (%AET)`,

    saturated_fat_g = `AGsat (g)`,
    monounsaturated_fat_g = `AGmi (g)`,
    polyunsaturated_fat_g = `AGpi (g)`,

    water_g = `Eau (g)`,
    sodium_mg = `Na (mg)`,
    chloride_mg = `Cld (mg)`,
    potassium_mg = `K (mg)`,
    calcium_mg = `Ca (mg)`,
    fiber_g = `FIB (g)`,

    biotin_ug = `Biot (µg)`,
    pantothenic_acid_mg = `aPant (mg)`,
    folate_ug = `Fol (µg)`,
    niacin_mg = `NIA (mg)`,
    riboflavin_mg = `Ribf (mg)`,
    thiamin_mg = `Thia (mg)`,
    vitamin_b6_mg = `B6 (mg)`,
    vitamin_b12_ug = `B12 (µg)`,
    vitamin_d_ug = `D (µg)`,
    vitamin_e_mg = `E (mg)`,
    vitamin_a_ug_re = `A-er (µg_RE)`,
    vitamin_k_ug = `Vit_K (µg)`,
    vitamin_c_mg = `C (mg)`,

    magnesium_mg = `Mg (mg)`,
    phosphorus_mg = `P (mg)`,
    iron_mg = `Fe (mg)`,
    fd_ug = `Fd (µg)`,
    iodine_ug = `Id (µg)`,
    copper_mg = `Cu (mg)`,
    zinc_mg = `Zn (mg)`,
    comments = Comments
  )
  
str(nutrition_clean)


# clean liquid balance data.

str(liquid_balance_C1)
str(liquid_balance_C2)


liquid_balance_C1_clean <- liquid_balance_C1 |>
  transmute(
    cohort = "C1",
    ID = as.factor(str_c("BRACE_", Subjects)),
    StudyDay = make_study_day(
      normalize_stage_key(`Study day`)
    ),
    total_liquid_intake_g = `Total Liquid Intake (g)`,
    urine_output_g = `Diuresis (g)`,
    liquid_balance_g = `Liquid Balance (g)`,
    comments = `Comments - Commentaires`
  )

liquid_balance_C2_clean <- liquid_balance_C2 |>
  transmute(
    cohort = "C2",
    ID = as.factor(str_c("BRACE_", `Volunteer ID`)),
    StudyDay = make_study_day(
      normalize_stage_key(`Study day - Jour d'étude`)
    ),
    total_liquid_intake_g = `Liquid Intake - Liquide ingéré`,
    urine_output_g = `Urine Output - Diurèse (24h)`,
    liquid_balance_g = `Liquid Balance - Balance hydrique`,
    comments = `Comments - Commentaires`
  )

liquid_balance_clean <- bind_rows(
  liquid_balance_C1_clean,
  liquid_balance_C2_clean
) |>
  left_join(
    lookup_clean |>
      distinct(ID, StudyDay, Group),
    by = c("ID", "StudyDay")
  )

str(liquid_balance_clean)

liquid_balance_clean$comments


# Save cleaned nutrition and liquid balance datasets as CSV and RDS files.
write_csv(
  nutrition_clean,
  file.path(intake_output_dir, "nutrition_clean.csv")
)

saveRDS(
  nutrition_clean,
  file.path(intake_output_dir, "nutrition_clean.RDS")
)

write_csv(
  liquid_balance_clean,
  file.path(intake_output_dir, "liquid_balance_clean.csv")
)

saveRDS(
  liquid_balance_clean,
  file.path(intake_output_dir, "liquid_balance_clean.RDS")
)
