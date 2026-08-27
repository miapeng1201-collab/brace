# BRACE analysis scripts

This folder contains the new, clean analysis workflow for the BRACE project.

Older exploratory and legacy scripts are preserved in:

```text
scripts_legacy/
```

The legacy scripts are kept for reference and traceability, but they are not part of the new primary analysis workflow unless specific code is copied and adapted into the numbered scripts below.

## Script order

Run scripts in numerical order:

```text
01_data_cleaning/
02_descriptive_analysis/
03_auc_analysis/
04_repeated_measures_anova/
05_lmm_analysis/
06_gamm_analysis/
07_figures/
```

Shared helper functions should be placed in:

```text
functions/
```

Unused new scripts should be moved to:

```text
archive/
```

## Principles

- Raw data are read from `data/Original_data/` and should not be modified.
- Cleaned or analysis-ready data should be written to `data/processed_data/`.
- Tables should be written to `results/tables/`.
- Figures should be written to `results/figures/`.
- Each script should have one clear purpose.
- File names should be numbered and descriptive.

