source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- analysis_sample(derive_analysis_data(load_master()))

fit_sensitivity <- function(label, dat, exposure_var = "sitting_per_60min", add_bmi = FALSE) {
  rhs <- c(exposure_var, "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline", "smoking_status", "alcohol_ever", "sleep_hours_weekday")
  rhs <- rhs[!(rhs == "sex" & length(unique(na.omit(dat$sex))) < 2)]
  if (add_bmi) rhs <- c(rhs, "bmi")
  vars <- c("cardiometabolic_multimorbidity", rhs)
  des <- complete_design(dat, vars)
  fit <- svyglm(as.formula(paste("cardiometabolic_multimorbidity ~", paste(rhs, collapse = " + "))), design = des, family = quasibinomial())
  out <- tidy_svyglm_or(fit, label, exposure_var)
  out$n_unweighted <- nrow(model.frame(fit))
  out
}

data$high_sedentary_6h <- factor(ifelse(data$sitting_hours_day >= 6, "≥6 h/day", "<6 h/day"))
data$high_sedentary_10h <- factor(ifelse(data$sitting_hours_day >= 10, "≥10 h/day", "<10 h/day"))

sens <- rbind(
  fit_sensitivity("Primary Model 5", data),
  fit_sensitivity("Excluding implausible sitting >20 h/day", subset(data, !implausible_sitting)),
  fit_sensitivity("Model 5 + BMI", data, add_bmi = TRUE),
  fit_sensitivity("Alternative cutoff ≥6 h/day", data, "high_sedentary_6h"),
  fit_sensitivity("Alternative cutoff ≥10 h/day", data, "high_sedentary_10h")
)

strata_out <- rbind(
  fit_sensitivity("Female-stratified", subset(data, sex == "Female")),
  fit_sensitivity("Male-stratified", subset(data, sex == "Male")),
  fit_sensitivity("Age 20-39", subset(data, age_group == "20-39")),
  fit_sensitivity("Age 40-59", subset(data, age_group == "40-59")),
  fit_sensitivity("Age ≥60", subset(data, age_group == "≥60"))
)

write_csv_base(sens, root_path("outputs", "tables", "supplementary_table_3_sensitivity_cutoffs.csv"))
write_csv_base(strata_out, root_path("outputs", "tables", "supplementary_table_4_stratified_models.csv"))
