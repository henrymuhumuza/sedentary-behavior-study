source(file.path("R", "analysis_helpers.R"))
init_project()

definitions <- data.frame(
  construct = c("Sedentary behaviour", "Physical activity guideline", "Hypertension", "Diabetes", "Chronic kidney disease", "Cardiometabolic multimorbidity"),
  operational_definition = c(
    "Conceptually defined by WHO as waking sitting, reclining, or lying behaviour with energy expenditure ≤1.5 METs, where 1 MET approximates resting energy expenditure. Operationalized as usual sitting time in minutes/day from PAD680; analyzed per 60-minute/day and in categories <4, 4 to <6, 6 to <8, and ≥8 hours/day.",
    "WHO defines physical activity as bodily movement produced by skeletal muscles that requires energy expenditure. Operationalized as at least 150 moderate-equivalent minutes/week; vigorous minutes counted double. Frequency units were recovered from raw PAQ_L PAD790U and PAD810U fields and converted to weekly equivalents.",
    "Mean SBP ≥140 mmHg, mean DBP ≥90 mmHg, self-reported hypertension, or current antihypertensive medication.",
    "Self-reported physician diagnosis, HbA1c ≥6.5%, insulin use, or diabetes medication use.",
    "eGFR <60 mL/min/1.73m2 or urine albumin-creatinine ratio ≥30 mg/g.",
    "At least two of hypertension, diabetes, and chronic kidney disease."
  ),
  missing_value_criteria = c(
    "Missing if usual sitting time from PAD680 was missing or outside the retained analytic range after cleaning.",
    "Missing if moderate-equivalent minutes/week could not be derived because required self-reported frequency, duration, or frequency-unit fields were unavailable after cleaning.",
    "Missing if blood pressure measurements, self-reported hypertension diagnosis, and antihypertensive medication use were all unavailable after cleaning.",
    "Missing if HbA1c, self-reported diabetes diagnosis, insulin use, and diabetes medication use were all unavailable after cleaning.",
    "Missing if both eGFR and urine albumin-creatinine ratio were unavailable after cleaning.",
    "Missing if hypertension, diabetes, and CKD status were all missing."
  )
)
write_csv_base(definitions, root_path("outputs", "tables", "supplementary_table_1_variable_definitions.csv"))

scripts <- c(
  "scripts/01_data_preparation.R",
  "scripts/02_descriptive_analysis.R",
  "scripts/03_sedentary_exposure.R",
  "scripts/04_regression_models.R",
  "scripts/05_joint_exposure_analysis.R",
  "scripts/06_spline_analysis.R",
  "scripts/07_sensitivity_analyses.R"
)
for (s in scripts) source(s, local = new.env(parent = globalenv()))
