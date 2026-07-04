`%||%` <- function(x, y) if (is.null(x)) y else x

root_path <- function(...) {
  normalizePath(file.path(getOption("nhanes_project_root", "."), ...), winslash = "/", mustWork = FALSE)
}

init_project <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  root <- if (dir.exists(file.path(cwd, "data", "processed"))) {
    cwd
  } else if (dir.exists(file.path(cwd, "..", "data", "processed"))) {
    normalizePath(file.path(cwd, ".."), winslash = "/", mustWork = FALSE)
  } else {
    cwd
  }
  options(nhanes_project_root = root)
  renv_lib <- file.path(root, "renv", "library", "R-4.4", "x86_64-w64-mingw32")
  if (dir.exists(renv_lib)) .libPaths(c(renv_lib, .libPaths()))
  dirs <- c(
    "R", "scripts", "figures", "tables", "results", "manuscript", "references",
    "outputs", "outputs/figures", "outputs/tables", "outputs/logs", "logs"
  )
  invisible(lapply(file.path(root, dirs), dir.create, recursive = TRUE, showWarnings = FALSE))
  options(survey.lonely.psu = "adjust")
  invisible(root)
}

load_master <- function() {
  rds <- root_path("data", "processed", "nhanes_master.rds")
  csv <- root_path("data", "processed", "nhanes_master.csv")
  if (file.exists(rds)) return(readRDS(rds))
  if (file.exists(csv)) return(read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE))
  stop("Could not find nhanes_master.rds or nhanes_master.csv in data/processed.")
}

load_paq_units <- function() {
  paq <- root_path("data", "raw", "PAQ_L.xpt")
  if (!file.exists(paq) || !requireNamespace("haven", quietly = TRUE)) return(NULL)
  raw <- haven::read_xpt(paq)
  needed <- c("SEQN", "PAD790U", "PAD810U")
  if (!all(needed %in% names(raw))) return(NULL)
  data.frame(
    seqn = as.numeric(raw$SEQN),
    moderate_pa_unit = trimws(as.character(haven::as_factor(raw$PAD790U))),
    vigorous_pa_unit = trimws(as.character(haven::as_factor(raw$PAD810U))),
    stringsAsFactors = FALSE
  )
}

required_columns <- c(
  "seqn", "wtmec2yr", "sdmvpsu", "sdmvstra", "age", "sex", "race_ethnicity",
  "education", "pir", "bmi", "waist_cm", "smoking_status", "alcohol_ever",
  "alcohol_drinks_per_day", "binge_drinking_days", "moderate_pa_freq",
  "moderate_pa_minutes", "vigorous_pa_freq", "vigorous_pa_minutes",
  "sitting_minutes_day", "sleep_hours_weekday", "sleep_category",
  "hypertension", "diabetes", "ckd", "cardiometabolic_multimorbidity"
)

check_columns <- function(data, cols = required_columns) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

to_num <- function(x) suppressWarnings(as.numeric(x))

clean_nhanes_numeric <- function(x) {
  x <- to_num(x)
  x[x %in% c(7777, 9999)] <- NA_real_
  x
}

weekly_frequency <- function(freq, unit) {
  unit <- trimws(as.character(unit))
  out <- rep(NA_real_, length(freq))
  out[!is.na(freq) & freq == 0] <- 0
  out[!is.na(freq) & freq > 0 & unit == "D"] <- freq[!is.na(freq) & freq > 0 & unit == "D"] * 7
  out[!is.na(freq) & freq > 0 & unit == "W"] <- freq[!is.na(freq) & freq > 0 & unit == "W"]
  out[!is.na(freq) & freq > 0 & unit == "M"] <- freq[!is.na(freq) & freq > 0 & unit == "M"] * 12 / 52
  out[!is.na(freq) & freq > 0 & unit == "Y"] <- freq[!is.na(freq) & freq > 0 & unit == "Y"] / 52
  out
}

derive_analysis_data <- function(data) {
  check_columns(data)
  data$wtmec2yr <- clean_nhanes_numeric(data$wtmec2yr)
  data$sdmvpsu <- clean_nhanes_numeric(data$sdmvpsu)
  data$sdmvstra <- clean_nhanes_numeric(data$sdmvstra)
  data$age <- clean_nhanes_numeric(data$age)
  data$pir <- clean_nhanes_numeric(data$pir)
  data$bmi <- clean_nhanes_numeric(data$bmi)
  data$waist_cm <- clean_nhanes_numeric(data$waist_cm)
  data$sitting_minutes_day <- clean_nhanes_numeric(data$sitting_minutes_day)
  data$sitting_minutes_day[data$sitting_minutes_day < 0 | data$sitting_minutes_day > 1200] <- NA_real_
  data$sitting_hours_day <- data$sitting_minutes_day / 60
  data$sitting_per_60min <- data$sitting_minutes_day / 60
  if (!all(c("moderate_pa_unit", "vigorous_pa_unit") %in% names(data))) {
    units <- load_paq_units()
    if (!is.null(units)) {
      data <- merge(data, units, by = "seqn", all.x = TRUE, sort = FALSE)
    }
  }
  if (!"moderate_pa_unit" %in% names(data)) data$moderate_pa_unit <- NA_character_
  if (!"vigorous_pa_unit" %in% names(data)) data$vigorous_pa_unit <- NA_character_
  data$moderate_pa_freq <- clean_nhanes_numeric(data$moderate_pa_freq)
  data$moderate_pa_minutes <- clean_nhanes_numeric(data$moderate_pa_minutes)
  data$vigorous_pa_freq <- clean_nhanes_numeric(data$vigorous_pa_freq)
  data$vigorous_pa_minutes <- clean_nhanes_numeric(data$vigorous_pa_minutes)
  data$moderate_pa_minutes[data$moderate_pa_freq == 0 & !is.na(data$moderate_pa_freq)] <- 0
  data$vigorous_pa_minutes[data$vigorous_pa_freq == 0 & !is.na(data$vigorous_pa_freq)] <- 0
  data$moderate_pa_freq_week <- weekly_frequency(data$moderate_pa_freq, data$moderate_pa_unit)
  data$vigorous_pa_freq_week <- weekly_frequency(data$vigorous_pa_freq, data$vigorous_pa_unit)
  data$moderate_minutes_week <- data$moderate_pa_freq_week * data$moderate_pa_minutes
  data$vigorous_minutes_week <- data$vigorous_pa_freq_week * data$vigorous_pa_minutes
  data$total_meq_minutes_week <- data$moderate_minutes_week + 2 * data$vigorous_minutes_week
  data$meets_pa_guideline <- factor(
    ifelse(
      is.na(data$total_meq_minutes_week),
      NA_character_,
      ifelse(data$total_meq_minutes_week >= 150, "Meets guideline", "Does not meet guideline")
    ),
    levels = c("Does not meet guideline", "Meets guideline")
  )
  data$sedentary_category <- cut(
    data$sitting_hours_day,
    breaks = c(-Inf, 4, 6, 8, Inf),
    right = FALSE,
    labels = c("<4 h/day", "4 to <6 h/day", "6 to <8 h/day", "≥8 h/day")
  )
  data$high_sedentary_8h <- factor(ifelse(data$sitting_hours_day >= 8, "High sedentary", "Low sedentary"))
  data$joint_sedentary_pa <- interaction(data$high_sedentary_8h, data$meets_pa_guideline, sep = " + ", drop = TRUE)
  data$joint_sedentary_pa <- relevel(data$joint_sedentary_pa, ref = "Low sedentary + Meets guideline")
  data$age_group <- cut(data$age, breaks = c(20, 40, 60, Inf), right = FALSE, labels = c("20-39", "40-59", "≥60"))
  data$implausible_sitting <- !is.na(data$sitting_minutes_day) & (data$sitting_minutes_day < 0 | data$sitting_minutes_day > 1200)
  data
}

analysis_sample <- function(data) {
  subset(
    data,
    age >= 20 &
      !is.na(sitting_minutes_day) &
      !is.na(cardiometabolic_multimorbidity) &
      !is.na(wtmec2yr) & !is.na(sdmvpsu) & !is.na(sdmvstra) &
      wtmec2yr > 0
  )
}

survey_design <- function(data) {
  survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    nest = TRUE,
    data = data
  )
}

write_csv_base <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

svy_percent <- function(design, var) {
  form <- as.formula(paste0("~factor(", var, ")"))
  est <- survey::svymean(form, design, na.rm = TRUE)
  data.frame(
    level = sub("^factor\\([^)]*\\)", "", names(coef(est))),
    percent = as.numeric(coef(est)) * 100,
    se = as.numeric(SE(est)) * 100,
    row.names = NULL
  )
}

tidy_svyglm_or <- function(model, model_name, exposure_label = NULL) {
  beta <- coef(model)
  se <- sqrt(diag(vcov(model)))
  ci <- cbind(beta - 1.96 * se, beta + 1.96 * se)
  p <- 2 * pnorm(abs(beta / se), lower.tail = FALSE)
  out <- data.frame(
    model = model_name,
    term = names(beta),
    odds_ratio = exp(beta),
    conf_low = exp(ci[, 1]),
    conf_high = exp(ci[, 2]),
    p_value = p,
    row.names = NULL
  )
  if (!is.null(exposure_label)) out$exposure <- exposure_label
  out
}

complete_design <- function(data, vars) {
  keep <- complete.cases(data[, vars, drop = FALSE])
  survey_design(data[keep, , drop = FALSE])
}
