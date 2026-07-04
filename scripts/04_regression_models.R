source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- analysis_sample(derive_analysis_data(load_master()))

models <- list(
  "Model 1" = c("cardiometabolic_multimorbidity", "sitting_per_60min"),
  "Model 2" = c("cardiometabolic_multimorbidity", "sitting_per_60min", "age", "sex", "race_ethnicity"),
  "Model 3" = c("cardiometabolic_multimorbidity", "sitting_per_60min", "age", "sex", "race_ethnicity", "education", "pir"),
  "Model 4" = c("cardiometabolic_multimorbidity", "sitting_per_60min", "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline"),
  "Model 5" = c("cardiometabolic_multimorbidity", "sitting_per_60min", "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline", "smoking_status", "alcohol_ever", "sleep_hours_weekday"),
  "Model 6" = c("cardiometabolic_multimorbidity", "sitting_per_60min", "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline", "smoking_status", "alcohol_ever", "sleep_hours_weekday", "bmi")
)

fit_one <- function(name, vars, exposure) {
  des <- complete_design(data, vars)
  rhs <- paste(vars[-1], collapse = " + ")
  fit <- svyglm(as.formula(paste(vars[1], "~", rhs)), design = des, family = quasibinomial())
  out <- tidy_svyglm_or(fit, name, exposure)
  out$n_unweighted <- nrow(model.frame(fit))
  out
}

continuous_results <- do.call(rbind, Map(function(nm, vars) fit_one(nm, vars, "Sedentary time per 60 min/day"), names(models), models))

cat_vars <- gsub("sitting_per_60min", "sedentary_category", models[["Model 5"]])
cat_des <- complete_design(data, cat_vars)
cat_fit <- svyglm(cardiometabolic_multimorbidity ~ sedentary_category + age + sex + race_ethnicity + education + pir + meets_pa_guideline + smoking_status + alcohol_ever + sleep_hours_weekday,
                  design = cat_des, family = quasibinomial())
categorical_results <- tidy_svyglm_or(cat_fit, "Model 5 categorical", "Sedentary category")
categorical_results$n_unweighted <- nrow(model.frame(cat_fit))

write_csv_base(continuous_results, root_path("outputs", "tables", "table_3_multimorbidity_regression_continuous.csv"))
write_csv_base(categorical_results, root_path("outputs", "tables", "table_3_multimorbidity_regression_categorical.csv"))

secondary_outcomes <- c("hypertension", "diabetes", "ckd")
secondary_results <- do.call(rbind, lapply(secondary_outcomes, function(outcome) {
  vars <- c(outcome, "sitting_per_60min", "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline", "smoking_status", "alcohol_ever", "sleep_hours_weekday")
  des <- complete_design(data, vars)
  fit <- svyglm(as.formula(paste(outcome, "~ sitting_per_60min + age + sex + race_ethnicity + education + pir + meets_pa_guideline + smoking_status + alcohol_ever + sleep_hours_weekday")),
                design = des, family = quasibinomial())
  out <- tidy_svyglm_or(fit, paste("Model 5:", outcome), "Sedentary time per 60 min/day")
  out$outcome <- outcome
  out$n_unweighted <- nrow(model.frame(fit))
  out
}))
write_csv_base(secondary_results, root_path("outputs", "tables", "table_4_secondary_outcome_models.csv"))
