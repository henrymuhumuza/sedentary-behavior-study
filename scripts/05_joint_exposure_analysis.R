source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- analysis_sample(derive_analysis_data(load_master()))
vars <- c("cardiometabolic_multimorbidity", "joint_sedentary_pa", "age", "sex", "race_ethnicity", "education", "pir")
des <- complete_design(data, vars)
fit <- svyglm(cardiometabolic_multimorbidity ~ joint_sedentary_pa + age + sex + race_ethnicity + education + pir,
              design = des, family = quasibinomial())
joint <- tidy_svyglm_or(fit, "Joint sedentary-PA model", "Joint sedentary behaviour and PA")
joint$n_unweighted <- nrow(model.frame(fit))
write_csv_base(joint, root_path("outputs", "tables", "table_5_joint_sedentary_pa.csv"))

prev <- svyby(~cardiometabolic_multimorbidity, ~joint_sedentary_pa, des, svymean, na.rm = TRUE, keep.names = FALSE)
plot_data <- data.frame(joint_group = prev$joint_sedentary_pa, percent = prev$cardiometabolic_multimorbidity * 100, se = prev$se * 100)
write_csv_base(plot_data, root_path("outputs", "tables", "figure_5_joint_sedentary_pa_data.csv"))

png(root_path("outputs", "figures", "figure_5_joint_sedentary_pa.png"), width = 1800, height = 1200, res = 200)
op <- par(mar = c(6.5, 5, 1, 1))
joint_labels <- c(
  "High sedentary + Does not meet guideline" = "High sedentary\nDoes not meet PA",
  "Low sedentary + Does not meet guideline" = "Low sedentary\nDoes not meet PA",
  "High sedentary + Meets guideline" = "High sedentary\nMeets PA",
  "Low sedentary + Meets guideline" = "Low sedentary\nMeets PA"
)
plot_labels <- ifelse(plot_data$joint_group %in% names(joint_labels), joint_labels[plot_data$joint_group], plot_data$joint_group)
y_top <- ceiling(max(plot_data$percent, na.rm = TRUE) * 1.2 / 5) * 5
barplot(plot_data$percent, names.arg = plot_labels, las = 1, col = "#74C476",
        ylab = "Weighted prevalence, %", ylim = c(0, y_top), cex.names = 0.82, main = "")
par(op)
dev.off()
