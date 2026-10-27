# Type 2 Diabetes complications — statistical analysis pipeline (R)

Reproducible R pipeline for a cross-sectional study of complications and comorbidities in adults with type 2 diabetes (T2D) attended at a hospital in Mexico (n = 228 of 531 patients seen in the study semester). It covers data cleaning and QC, descriptive statistics, bivariate analysis, multivariable logistic regression and single-biomarker ROC analysis, plus an interactive **Shiny** explorer and a parametrised **R Markdown** figure report.

> **Privacy.** Patient-level data are **not** in this repository (`data/raw/`, `data/processed/`, `*.xlsx`, `*.csv`, `*.rds` and `output/` are git-ignored). The `data/demo/` folder contains **synthetic** data (random numbers with plausible relationships, fixed seed) so anyone can run the pipeline and the app. Results on synthetic data have no clinical meaning.

## What it does

| Step | Script | Output |
|---|---|---|
| Setup | `00_setup.R` | installs/loads packages, creates output folders, writes `output/models/sessionInfo.txt` |
| Cleaning & QC | `01_limpieza.R` | `data/processed/df_clean_tesis.rds`; CKD-EPI 2009 eGFR, KDIGO stage, TyG index, BMI class, complication/comorbidity indices; NA and IQR×3 outlier checks |
| Descriptive | `02_descriptivo.R` | Table 1, prevalence tables (Clopper–Pearson 95% CI), Figure 1 panels, population pyramid, BMI/CKD-stage plots |
| Bivariate | `03_bivariado.R` | Supplementary Tables S1–S10 (Shapiro-driven t / Mann–Whitney, χ²/Fisher with OR, Spearman correlations with Holm adjustment) |
| Regression | `04_regresion.R` | Six logistic models (CKD, hypertension, dialysis, retinopathy, heart failure, amputation): backward-AIC selection, VIF, Hosmer–Lemeshow, Nagelkerke R², AUC; forest plots; Figure 2 |
| ROC | `05_roc.R` | 8 biomarkers × 4 outcomes (CKD, dialysis, retinopathy, amputation): AUC (DeLong 95% CI), Youden cut-offs, Table 9, AUC heatmap, Figure 3 |
| Shiny app | `app/app.R` | interactive version of the figures, PNG download |
| Report | `report/figures_report.Rmd` | static HTML figure report (see `docs/demo_report.html`, built on synthetic data) |

All paths, sheet name, study constants (`N_TOTAL_HOSPITAL`, `N_DM2_ESPERADO`), `ALPHA` and the random seed live in **`config.R`** — the only file to edit.

## Repository layout

```
config.R                 paths + constants (env-var overridable)
00_setup.R … 05_roc.R    analysis pipeline (run in order)
R/load_data.R, R/plots.R plotting/analysis functions shared by app and report
app/app.R                Shiny explorer
report/figures_report.Rmd  parametrised R Markdown report
demo/make_demo_data.R    generates synthetic data and runs the real 01_limpieza.R on it
data/demo/               synthetic Excel + cleaned RDS (safe to publish)
docs/demo_report.html    rendered demo report (synthetic data)
legacy/                  previous Spanish version of the bivariate script (kept for reference)
```

## Quick start

Requires R ≥ 4.2. From the project root:

```r
# 1) Try it with synthetic data (no patient data needed)
source("00_setup.R")                 # installs missing packages
source("demo/make_demo_data.R")      # creates data/demo/*
shiny::runApp("app")                 # interactive explorer
rmarkdown::render("report/figures_report.Rmd")   # static report
```

```r
# 2) Full pipeline on the real dataset (kept locally)
# put the workbook at data/raw/Datos.xlsx (sheet "DIABETICOS"; first row skipped — see config.R)
for (s in c("01_limpieza", "02_descriptivo", "03_bivariado", "04_regresion", "05_roc"))
  source(paste0(s, ".R"))
shiny::runApp("app")    # the app automatically uses data/processed/df_clean_tesis.rds when it exists
```

The app and report use the real cleaned data if `data/processed/df_clean_tesis.rds` exists and fall back to the synthetic demo otherwise (a banner says which). They show aggregates and plots only — there is no row-level view.

To run the pipeline on other files without editing `config.R`, set `TD2_DATA_FILE`, `TD2_DIR_TABLES`, `TD2_DIR_FIGURES`, `TD2_DIR_MODELS`, `TD2_DIR_DATA` as environment variables.

## Reproducibility

Each run records R and package versions in `output/models/sessionInfo.txt`. To pin the environment:

```r
install.packages("renv"); renv::init()      # first time
renv::snapshot()                            # commit renv.lock
renv::restore()                             # on another machine
```

## Methods summary

eGFR by CKD-EPI 2009 (no race coefficient), KDIGO G1–G5; TyG = ln(TG × glucose / 2); prevalence CIs by Clopper–Pearson; group comparisons by Shapiro–Wilk-guided t-test or Mann–Whitney, χ²/Fisher; Spearman correlation with Holm correction; logistic regression with backward AIC selection (minimum 10 events, 10:1 rule), VIF, Hosmer–Lemeshow, Nagelkerke R²; ROC by pROC with DeLong CIs and Youden optimum. Creatinine, BUN and urea are excluded as predictors of CKD (consequence, not predictor).

## License

MIT — see `LICENSE`.
