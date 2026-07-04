source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- derive_analysis_data(load_master())
data <- analysis_sample(data)
des <- survey_design(data)

continuous <- c("age", "pir", "bmi", "waist_cm", "sitting_hours_day", "total_meq_minutes_week", "sleep_hours_weekday")
binary <- c("hypertension", "diabetes", "ckd", "cardiometabolic_multimorbidity")

overall_cont <- do.call(rbind, lapply(continuous, function(v) {
  m <- svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)
  q <- svyquantile(as.formula(paste0("~", v)), des, quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE, ci = FALSE)
  data.frame(variable = v, mean = as.numeric(coef(m)), se = as.numeric(SE(m)), q25 = q[[1]][1], median = q[[1]][2], q75 = q[[1]][3])
}))

overall_binary <- do.call(rbind, lapply(binary, function(v) {
  m <- svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)
  data.frame(variable = v, percent = as.numeric(coef(m)) * 100, se = as.numeric(SE(m)) * 100)
}))

cat_vars <- c("sex", "race_ethnicity", "education", "smoking_status", "sleep_category", "meets_pa_guideline", "sedentary_category")
overall_cat <- do.call(rbind, lapply(cat_vars, function(v) {
  x <- svy_percent(des, v)
  x$variable <- v
  x
}))

table1_by_sedentary <- do.call(rbind, lapply(c("age", "pir", "bmi", "waist_cm", "total_meq_minutes_week"), function(v) {
  out <- svyby(as.formula(paste0("~", v)), ~sedentary_category, des, svymean, na.rm = TRUE, keep.names = FALSE)
  data.frame(
    variable = v,
    sedentary_category = out$sedentary_category,
    mean = out[[v]],
    se = out$se,
    row.names = NULL
  )
}))

prevalence_by_sedentary <- do.call(rbind, lapply(binary, function(v) {
  out <- svyby(as.formula(paste0("~", v)), ~sedentary_category, des, svymean, na.rm = TRUE, keep.names = FALSE)
  names(out)[2] <- "proportion"
  data.frame(outcome = v, sedentary_category = out$sedentary_category, percent = out$proportion * 100, se = out$se * 100)
}))

write_csv_base(overall_cont, root_path("outputs", "tables", "descriptive_continuous_overall.csv"))
write_csv_base(overall_binary, root_path("outputs", "tables", "descriptive_binary_overall.csv"))
write_csv_base(overall_cat, root_path("outputs", "tables", "descriptive_categorical_overall.csv"))
write_csv_base(table1_by_sedentary, root_path("outputs", "tables", "table_1_weighted_characteristics_by_sedentary.csv"))
write_csv_base(prevalence_by_sedentary, root_path("outputs", "tables", "table_2_prevalence_by_sedentary.csv"))
