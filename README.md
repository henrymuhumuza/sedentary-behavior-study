# NHANES Sedentary Behaviour and Cardiometabolic Multimorbidity

This project is a clean Quarto/R workflow for:

**Sedentary Behaviour and Cardiometabolic Multimorbidity: A Cross-sectional Analysis of NHANES August 2021-August 2023**

The workflow regenerates the cleaned NHANES adult master dataset from raw XPT files:

```text
data/processed/nhanes_master.csv
data/processed/nhanes_master.rds
```

Raw XPT files are preserved and should not be overwritten during analysis.

## Project Structure

```text
data/raw/          Original NHANES XPT files
data/processed/    Master dataset and derived sedentary analysis dataset
R/                 Shared helper functions
scripts/           Reproducible analysis scripts
figures/           Reserved for manuscript-facing figure exports
tables/            Reserved for manuscript-facing table exports
results/           Reserved for model/result objects
manuscript/        Quarto manuscript
references/        BibTeX and Vancouver CSL
outputs/           Generated tables, figures, logs, rendered manuscript
logs/              Additional run logs
```

## Run Order

If `Rscript` is not on PATH, use the installed R executable directly:

```powershell
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\00_setup.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\00_build_master_from_raw.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\01_data_preparation.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\02_descriptive_analysis.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\03_sedentary_exposure.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\04_regression_models.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\05_joint_exposure_analysis.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\06_spline_analysis.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\07_sensitivity_analyses.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\08_tables.R
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\09_figures.R
& 'C:\Program Files\Quarto\bin\quarto.exe' render manuscript/manuscript.qmd
```

`scripts/08_tables.R` sources the main analysis scripts as a convenience runner for tables and intermediate outputs.

## Analysis Notes

The master dataset has 7,809 adult participants and 64 columns. The workflow excludes participants missing sedentary time, cardiometabolic multimorbidity, or core MEC survey design variables for the primary analytic sample.

Physical activity is derived as:

```text
moderate minutes/week = weekly moderate frequency * moderate_pa_minutes
vigorous minutes/week = weekly vigorous frequency * vigorous_pa_minutes
moderate-equivalent minutes/week = moderate minutes/week + 2 * vigorous minutes/week
meeting guideline = moderate-equivalent minutes/week >= 150
```

The current master dataset retains PA frequency and duration variables but not `PAD790U`/`PAD810U` unit fields. The analysis helper recovers these fields from raw `data/raw/PAQ_L.xpt` and converts day, week, month, and year frequencies to weekly equivalents before deriving moderate-equivalent minutes/week.
