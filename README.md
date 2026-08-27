# BRACE-SALT Longitudinal Bed Rest Analysis

## Overview

This repository contains the analysis workflow for the BRACE/SALT bed rest project. The project investigates physiological adaptations during prolonged head-down tilt bed rest, with a focus on fluid balance, electrolyte handling, body weight, skin water loss/barrier-related measurements, and cardiovascular regulation.

The repository is organized as a reproducible analysis project: raw data are kept separate from processed data, scripts are organized by analysis step, and results are saved in dedicated output folders.

## Research question

**Overarching question**

How does prolonged head-down tilt bed rest affect fluid balance, electrolyte handling, body weight, skin barrier/water loss, and cardiovascular regulation, and do these responses differ between intervention and control groups?

**Main sub-questions**

- How does body weight change during baseline, bed rest, and recovery?
- Does total liquid balance change during bed rest and recovery?
- Does electrolyte handling change during bed rest?
- Are cardiovascular variables affected by bed rest?
- Do skin water loss/barrier-related measurements change over time?
- Are fluid and electrolyte changes associated with body weight or cardiovascular changes?
- Do outcomes recover toward baseline after bed rest?

## Repository structure

```text
.
├── README.md
├── pixi.toml
├── pixi.lock
├── data/
│   ├── Original_data/
│   └── processed_data/
├── docs/
│   └── papers/
├── results/
│   ├── figures/
│   ├── tables/
│   ├── body_weight_change_plots/
│   ├── finapress/
│   └── urine/
├── scripts/
│   ├── README.md
│   ├── 01_data_cleaning/
│   ├── 02_descriptive_analysis/
│   ├── 03_auc_analysis/
│   ├── 04_repeated_measures_anova/
│   ├── 05_lmm_analysis/
│   ├── 06_gamm_analysis/
│   ├── 07_figures/
│   ├── functions/
│   └── archive/
└── scripts_legacy/
```

## Data

Raw data are stored in:

```text
data/Original_data/
```

This folder contains the original project data and must not be modified. Scripts should read from this folder but should never overwrite or manually edit files inside it.

Processed or analysis-ready data should be written to:

```text
data/processed_data/
```

The current processed-data folders include:

```text
data/processed_data/03_finapress/
data/processed_data/04_tewameter/
data/processed_data/Databases/
```

Raw data are not intended to be pushed to GitHub or shared publicly because they may contain sensitive research data and are subject to project-specific data-sharing restrictions.

## Scripts

The new primary analysis workflow is located in:

```text
scripts/
```

Scripts should be added in numerical order according to the analysis step:

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
scripts/functions/
```

Unused new scripts should be moved to:

```text
scripts/archive/
```

Older exploratory scripts are preserved in:

```text
scripts_legacy/
```

The legacy scripts are kept for reference and traceability. They are not part of the new primary analysis workflow unless specific code is copied, reviewed, and adapted into the numbered scripts.

## Planned analysis workflow

The planned workflow is:

1. Clean raw data and create analysis-ready datasets.
2. Create descriptive tables and inspect missingness.
3. Generate individual and group-level longitudinal plots.
4. Calculate AUC summary measures for selected phases.
5. Run repeated-measures ANOVA for selected key time points.
6. Run linear mixed models as the main longitudinal analysis.
7. Run exploratory GAMMs for nonlinear trajectories when relevant.
8. Export final tables and figures.

## Statistical methods

The planned statistical methods include:

- Descriptive statistics and visualization
- Area under the curve (AUC) summary analyses
- Repeated-measures ANOVA for selected key time points
- Linear mixed models (LMMs) for primary longitudinal analyses
- Generalized additive mixed models (GAMMs) for exploratory nonlinear time trends
- Correlation or regression analyses for exploratory associations between fluid, electrolyte, body weight, and cardiovascular outcomes

LMMs are expected to be the main analysis method because the data are longitudinal repeated-measures data with multiple observations per participant.

## Results

New result tables should be saved in:

```text
results/tables/
```

New result figures should be saved in:

```text
results/figures/
```

Recommended formats:

- Tables: `.csv` or `.tsv`
- Figures: `.pdf`

Existing topic-specific output folders, such as `results/finapress/`, `results/urine/`, and `results/body_weight_change_plots/`, are retained for previous outputs.

## Environment

This project uses `pixi` for environment management.

The environment files are:

```text
pixi.toml
pixi.lock
```

To install the environment:

```bash
pixi install
```

Current R dependencies include:

- `r-base`
- `r-tidyverse`
- `r-readxl`
- `r-writexl`
- `r-lme4`
- `r-lmertest`
- `r-emmeans`

Additional packages may be added as the analysis workflow develops.

## Documentation

Project documents, protocols, SOPs, manuals, and related background files are stored in:

```text
docs/papers/
```

This includes protocol documents, reports, manuals, SOPs, and reference materials.

## Responsibilities

| Person | Responsibility |
|---|---|
| Xinyue Peng | Data cleaning, R scripts, exploratory plots, statistical analyses, project documentation |
| Barbara Verhaar | Supervision on project organization, reproducibility, and statistical analysis strategy |
| Rik H.G. Olde Engberink | Scientific scope, final interpretation, and manuscript-level decisions |
| Collaborators | Data checking, physiological interpretation, and domain-specific feedback |

Responsibilities should be updated when project roles are finalized.

## Notes for contributors

- Do not edit files in `data/Original_data/`.
- Keep generated data in `data/processed_data/`.
- Keep new scripts in the numbered `scripts/` workflow.
- Keep old exploratory scripts in `scripts_legacy/`.
- Save final tables and figures in `results/tables/` and `results/figures/`.
- Document important analysis decisions in `docs/` or in script comments.
