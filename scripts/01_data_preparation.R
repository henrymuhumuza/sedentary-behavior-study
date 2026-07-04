source(file.path("R", "analysis_helpers.R"))
init_project()

master <- derive_analysis_data(load_master())
analytic <- analysis_sample(master)

write_csv_base(analytic, root_path("data", "processed", "sedentary_analysis_dataset.csv"))
saveRDS(analytic, root_path("data", "processed", "sedentary_analysis_dataset.rds"))

key_vars <- c(
  "sitting_minutes_day", "cardiometabolic_multimorbidity", "wtmec2yr", "sdmvpsu",
  "sdmvstra", "age", "sex", "race_ethnicity", "education", "pir", "bmi",
  "smoking_status", "alcohol_ever", "sleep_hours_weekday", "meets_pa_guideline"
)
missingness <- data.frame(
  variable = key_vars,
  missing_n = sapply(key_vars, function(v) sum(is.na(master[[v]]))),
  missing_percent = sapply(key_vars, function(v) mean(is.na(master[[v]])) * 100),
  row.names = NULL
)

flow <- data.frame(
  step = c("Adults in derived adult analytic file", "Excluded missing sedentary time", "Excluded missing multimorbidity outcome", "Excluded missing/zero MEC design variables", "Primary analytic sample"),
  n = c(
    nrow(master),
    sum(is.na(master$sitting_minutes_day)),
    sum(is.na(master$cardiometabolic_multimorbidity)),
    sum(is.na(master$wtmec2yr) | is.na(master$sdmvpsu) | is.na(master$sdmvstra) | master$wtmec2yr <= 0),
    nrow(analytic)
  )
)

write_csv_base(missingness, root_path("outputs", "tables", "supplementary_table_2_missingness.csv"))
write_csv_base(flow, root_path("outputs", "tables", "figure_1_study_flow_data.csv"))
