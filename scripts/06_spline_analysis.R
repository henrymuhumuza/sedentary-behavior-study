source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)
library(splines)

data <- analysis_sample(derive_analysis_data(load_master()))
vars <- c("cardiometabolic_multimorbidity", "sitting_hours_day", "age", "sex", "race_ethnicity", "education", "pir", "meets_pa_guideline")
des <- complete_design(data, vars)
fit <- svyglm(cardiometabolic_multimorbidity ~ ns(sitting_hours_day, df = 4) + age + sex + race_ethnicity + education + pir + meets_pa_guideline,
              design = des, family = quasibinomial())

grid <- data.frame(
  sitting_hours_day = seq(1, 16, by = 0.25),
  age = weighted.mean(data$age, data$wtmec2yr, na.rm = TRUE),
  sex = names(sort(table(data$sex), decreasing = TRUE))[1],
  race_ethnicity = names(sort(table(data$race_ethnicity), decreasing = TRUE))[1],
  education = names(sort(table(data$education), decreasing = TRUE))[1],
  pir = weighted.mean(data$pir, data$wtmec2yr, na.rm = TRUE),
  meets_pa_guideline = "Meets guideline"
)
pred <- predict(fit, newdata = grid, type = "link", se.fit = TRUE)
if (is.list(pred) && !is.null(pred$fit)) {
  fit_link <- pred$fit
  se_link <- pred$se.fit
} else {
  fit_link <- as.numeric(pred)
  se_link <- rep(NA_real_, length(fit_link))
}
grid$predicted_probability <- plogis(fit_link)
grid$conf_low <- plogis(fit_link - 1.96 * se_link)
grid$conf_high <- plogis(fit_link + 1.96 * se_link)
write_csv_base(grid, root_path("outputs", "tables", "figure_6_spline_dose_response_data.csv"))

png(root_path("outputs", "figures", "figure_6_spline_dose_response.png"), width = 1800, height = 1200, res = 200)
plot(grid$sitting_hours_day, grid$predicted_probability * 100, type = "l", lwd = 2, col = "#2171B5",
     ylim = range(grid$predicted_probability, grid$conf_low, grid$conf_high, na.rm = TRUE) * 100,
     xlab = "Sitting time, hours/day", ylab = "Adjusted predicted probability, %",
     main = "")
if (any(!is.na(grid$conf_low))) {
  lines(grid$sitting_hours_day, grid$conf_low * 100, lty = 2, col = "#6BAED6")
  lines(grid$sitting_hours_day, grid$conf_high * 100, lty = 2, col = "#6BAED6")
}
dev.off()
