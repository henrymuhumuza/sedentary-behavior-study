source(file.path("R", "analysis_helpers.R"))
init_project()

if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Package 'haven' is required to read NHANES XPT files.")
}

read_raw <- function(name) {
  haven::read_xpt(root_path("data", "raw", paste0(name, ".xpt")))
}

num <- function(x) suppressWarnings(as.numeric(x))

clean_code <- function(x, special = c(7, 9, 77, 99, 777, 999, 7777, 9999)) {
  x <- num(x)
  x[x %in% special] <- NA_real_
  x
}

yes_no <- function(x, yes = 1, no = 2) {
  x <- num(x)
  out <- rep(NA_real_, length(x))
  out[x == yes] <- 1
  out[x == no] <- 0
  out
}

merge_left <- function(x, y) {
  merge(x, y, by = "seqn", all.x = TRUE, sort = FALSE)
}

mean_available <- function(...) {
  vals <- data.frame(..., check.names = FALSE)
  rowMeans(vals, na.rm = TRUE)
}

any_available <- function(...) {
  vals <- data.frame(..., check.names = FALSE)
  has_one <- rowSums(vals == 1, na.rm = TRUE) > 0
  has_any <- rowSums(!is.na(vals)) > 0
  out <- rep(NA_real_, nrow(vals))
  out[has_any] <- 0
  out[has_one] <- 1
  out
}

egfr_2021 <- function(scr, age, sex) {
  female <- sex == "Female"
  kappa <- ifelse(female, 0.7, 0.9)
  alpha <- ifelse(female, -0.241, -0.302)
  sex_factor <- ifelse(female, 1.012, 1)
  142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^(-1.200) * 0.9938^age * sex_factor
}

demo <- read_raw("DEMO_L")
master <- data.frame(
  seqn = num(demo$SEQN),
  wtint2yr = clean_code(demo$WTINT2YR),
  wtmec2yr = clean_code(demo$WTMEC2YR),
  sdmvpsu = clean_code(demo$SDMVPSU),
  sdmvstra = clean_code(demo$SDMVSTRA),
  age = num(demo$RIDAGEYR),
  sex = ifelse(num(demo$RIAGENDR) == 1, "Male", ifelse(num(demo$RIAGENDR) == 2, "Female", NA)),
  race_ethnicity = factor(
    num(demo$RIDRETH3),
    levels = c(1, 2, 3, 4, 6, 7),
    labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi-racial")
  ),
  education = factor(
    clean_code(demo$DMDEDUC2),
    levels = 1:5,
    labels = c("Less than 9th grade", "9th-11th grade", "High school/GED", "Some college/AA", "College graduate or above")
  ),
  marital_status = factor(
    clean_code(demo$DMDMARTZ),
    levels = c(1, 2, 3),
    labels = c("Married/Living with partner", "Widowed/Divorced/Separated", "Never married")
  ),
  pir = clean_code(demo$INDFMPIR),
  stringsAsFactors = FALSE
)
master <- master[master$age >= 20 & !is.na(master$age), ]

bmx <- read_raw("BMX_L")
bmx_out <- data.frame(
  seqn = num(bmx$SEQN),
  bmi = clean_code(bmx$BMXBMI),
  waist_cm = clean_code(bmx$BMXWAIST),
  hip_cm = clean_code(bmx$BMXHIP)
)
master <- merge_left(master, bmx_out)
master$bmi_category <- cut(
  master$bmi,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  right = FALSE,
  labels = c("Underweight", "Normal", "Overweight", "Obese")
)
master$obesity <- ifelse(is.na(master$bmi), NA_real_, ifelse(master$bmi >= 30, 1, 0))
master$central_obesity <- ifelse(
  is.na(master$waist_cm) | is.na(master$sex),
  NA_real_,
  ifelse((master$sex == "Male" & master$waist_cm >= 102) | (master$sex == "Female" & master$waist_cm >= 88), 1, 0)
)

bpxo <- read_raw("BPXO_L")
bpxo_out <- data.frame(
  seqn = num(bpxo$SEQN),
  mean_sbp = mean_available(clean_code(bpxo$BPXOSY1), clean_code(bpxo$BPXOSY2), clean_code(bpxo$BPXOSY3)),
  mean_dbp = mean_available(clean_code(bpxo$BPXODI1), clean_code(bpxo$BPXODI2), clean_code(bpxo$BPXODI3))
)
bpxo_out$mean_sbp[is.nan(bpxo_out$mean_sbp)] <- NA_real_
bpxo_out$mean_dbp[is.nan(bpxo_out$mean_dbp)] <- NA_real_
master <- merge_left(master, bpxo_out)

bpq <- read_raw("BPQ_L")
bpq_out <- data.frame(
  seqn = num(bpq$SEQN),
  htn_diagnosis = yes_no(bpq$BPQ020),
  htn_med = yes_no(bpq$BPQ150),
  high_chol_diagnosis = yes_no(bpq$BPQ080),
  chol_med = yes_no(bpq$BPQ101D)
)
master <- merge_left(master, bpq_out)
bp_available <- !is.na(master$mean_sbp) | !is.na(master$mean_dbp)
bp_high <- (!is.na(master$mean_sbp) & master$mean_sbp >= 140) | (!is.na(master$mean_dbp) & master$mean_dbp >= 90)
master$hypertension <- any_available(
  ifelse(bp_available, ifelse(bp_high, 1, 0), NA_real_),
  master$htn_diagnosis,
  master$htn_med
)

ghb <- read_raw("GHB_L")
master <- merge_left(master, data.frame(seqn = num(ghb$SEQN), hba1c = clean_code(ghb$LBXGH)))

diq <- read_raw("DIQ_L")
diq_out <- data.frame(
  seqn = num(diq$SEQN),
  diabetes_diagnosis = ifelse(num(diq$DIQ010) %in% c(1, 3), 1, ifelse(num(diq$DIQ010) == 2, 0, NA)),
  insulin_use = yes_no(diq$DIQ050),
  diabetes_pills = yes_no(diq$DIQ070)
)
master <- merge_left(master, diq_out)
master$diabetes <- any_available(
  ifelse(!is.na(master$hba1c), ifelse(master$hba1c >= 6.5, 1, 0), NA_real_),
  master$diabetes_diagnosis,
  master$insulin_use,
  master$diabetes_pills
)

biopro <- read_raw("BIOPRO_L")
master <- merge_left(master, data.frame(
  seqn = num(biopro$SEQN),
  serum_creatinine_mgdl = clean_code(biopro$LBXSCR)
))
master$egfr <- egfr_2021(master$serum_creatinine_mgdl, master$age, master$sex)

acr <- read_raw("ALB_CR_L")
master <- merge_left(master, data.frame(seqn = num(acr$SEQN), acr_mg_g = clean_code(acr$URDACT)))
master$albuminuria <- ifelse(is.na(master$acr_mg_g), NA_real_, ifelse(master$acr_mg_g >= 30, 1, 0))
master$ckd <- any_available(
  ifelse(!is.na(master$egfr), ifelse(master$egfr < 60, 1, 0), NA_real_),
  master$albuminuria
)

tchol <- read_raw("TCHOL_L")
hdl <- read_raw("HDL_L")
trig <- read_raw("TRIGLY_L")
master <- merge_left(master, data.frame(seqn = num(tchol$SEQN), total_cholesterol_mgdl = clean_code(tchol$LBXTC)))
master <- merge_left(master, data.frame(seqn = num(hdl$SEQN), hdl_mgdl = clean_code(hdl$LBDHDD)))
master <- merge_left(master, data.frame(
  seqn = num(trig$SEQN),
  triglycerides_mgdl = clean_code(trig$LBXTLG),
  ldl_mgdl = clean_code(trig$LBDLDL)
))
lipid_high <- any_available(
  ifelse(!is.na(master$total_cholesterol_mgdl), ifelse(master$total_cholesterol_mgdl >= 240, 1, 0), NA_real_),
  ifelse(!is.na(master$ldl_mgdl), ifelse(master$ldl_mgdl >= 160, 1, 0), NA_real_),
  ifelse(!is.na(master$hdl_mgdl), ifelse(master$hdl_mgdl < 40, 1, 0), NA_real_),
  ifelse(!is.na(master$triglycerides_mgdl), ifelse(master$triglycerides_mgdl >= 200, 1, 0), NA_real_),
  master$high_chol_diagnosis,
  master$chol_med
)
master$dyslipidemia <- lipid_high

smq <- read_raw("SMQ_L")
smq_out <- data.frame(seqn = num(smq$SEQN), ever_smoked = yes_no(smq$SMQ020), now_smoke = clean_code(smq$SMQ040))
master <- merge_left(master, smq_out)
master$smoking_status <- ifelse(
  master$ever_smoked == 0,
  "Never",
  ifelse(master$ever_smoked == 1 & master$now_smoke %in% c(1, 2), "Current", ifelse(master$ever_smoked == 1 & master$now_smoke == 3, "Former", NA))
)

alq <- read_raw("ALQ_L")
master <- merge_left(master, data.frame(
  seqn = num(alq$SEQN),
  alcohol_ever = yes_no(alq$ALQ111),
  alcohol_drinks_per_day = clean_code(alq$ALQ130),
  binge_drinking_days = clean_code(alq$ALQ142)
))

paq <- read_raw("PAQ_L")
master <- merge_left(master, data.frame(
  seqn = num(paq$SEQN),
  moderate_pa_freq = clean_code(paq$PAD790Q),
  moderate_pa_minutes = clean_code(paq$PAD800),
  vigorous_pa_freq = clean_code(paq$PAD810Q),
  vigorous_pa_minutes = clean_code(paq$PAD820),
  sitting_minutes_day = clean_code(paq$PAD680)
))

slq <- read_raw("SLQ_L")
master <- merge_left(master, data.frame(
  seqn = num(slq$SEQN),
  sleep_hours_weekday = clean_code(slq$SLD012),
  sleep_hours_weekend = clean_code(slq$SLD013)
))
master$sleep_category <- cut(
  master$sleep_hours_weekday,
  breaks = c(-Inf, 7, 9, Inf),
  right = FALSE,
  labels = c("Short", "Normal", "Long")
)

diet <- read_raw("DR1TOT_L")
master <- merge_left(master, data.frame(
  seqn = num(diet$SEQN),
  wtdrd1 = clean_code(diet$WTDRD1),
  total_energy_kcal = clean_code(diet$DR1TKCAL),
  protein_g = clean_code(diet$DR1TPROT),
  carbohydrate_g = clean_code(diet$DR1TCARB),
  total_sugar_g = clean_code(diet$DR1TSUGR),
  fiber_g = clean_code(diet$DR1TFIBE),
  total_fat_g = clean_code(diet$DR1TTFAT),
  saturated_fat_g = clean_code(diet$DR1TSFAT),
  cholesterol_mg = clean_code(diet$DR1TCHOL),
  sodium_mg = clean_code(diet$DR1TSODI),
  potassium_mg = clean_code(diet$DR1TPOTA),
  alcohol_g_diet = clean_code(diet$DR1TALCO)
))

cm <- data.frame(hypertension = master$hypertension, diabetes = master$diabetes, ckd = master$ckd)
master$cardiometabolic_count_available <- rowSums(!is.na(cm))
master$cardiometabolic_missing_count <- rowSums(is.na(cm))
master$cardiometabolic_multimorbidity <- ifelse(
  master$cardiometabolic_count_available == 0,
  NA_real_,
  ifelse(rowSums(cm == 1, na.rm = TRUE) >= 2, 1, 0)
)
master$cardiometabolic_multimorbidity[master$cardiometabolic_missing_count == 3] <- NA_real_

drop_cols <- c("high_chol_diagnosis", "chol_med", "ever_smoked", "now_smoke")
master <- master[, setdiff(names(master), drop_cols)]

ordered_cols <- c(
  "seqn", "wtint2yr", "wtmec2yr", "wtdrd1", "sdmvpsu", "sdmvstra", "age", "sex",
  "race_ethnicity", "education", "marital_status", "pir", "bmi", "waist_cm", "hip_cm",
  "bmi_category", "obesity", "central_obesity", "mean_sbp", "mean_dbp", "htn_diagnosis",
  "htn_med", "hypertension", "hba1c", "diabetes_diagnosis", "insulin_use",
  "diabetes_pills", "diabetes", "serum_creatinine_mgdl", "egfr", "acr_mg_g",
  "albuminuria", "ckd", "total_cholesterol_mgdl", "hdl_mgdl", "triglycerides_mgdl",
  "ldl_mgdl", "dyslipidemia", "smoking_status", "alcohol_ever",
  "alcohol_drinks_per_day", "binge_drinking_days", "moderate_pa_freq",
  "moderate_pa_minutes", "vigorous_pa_freq", "vigorous_pa_minutes",
  "sitting_minutes_day", "sleep_hours_weekday", "sleep_hours_weekend", "sleep_category",
  "total_energy_kcal", "protein_g", "carbohydrate_g", "total_sugar_g", "fiber_g",
  "total_fat_g", "saturated_fat_g", "cholesterol_mg", "sodium_mg", "potassium_mg",
  "alcohol_g_diet", "cardiometabolic_count_available", "cardiometabolic_missing_count",
  "cardiometabolic_multimorbidity"
)
master <- master[, ordered_cols]
master <- master[order(master$seqn), ]

write_csv_base(master, root_path("data", "processed", "nhanes_master.csv"))
saveRDS(master, root_path("data", "processed", "nhanes_master.rds"))

cat("Wrote nhanes_master with dimensions:", paste(dim(master), collapse = " x "), "\n")
