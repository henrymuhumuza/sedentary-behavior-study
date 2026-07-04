source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- analysis_sample(derive_analysis_data(load_master()))
des <- survey_design(data)

flow_file <- root_path("outputs", "tables", "figure_1_study_flow_data.csv")
if (file.exists(flow_file)) {
  flow <- read.csv(flow_file, stringsAsFactors = FALSE)
  write_csv_base(flow, root_path("outputs", "tables", "figure_1_study_flow_plot_data.csv"))

  png(root_path("outputs", "figures", "figure_1_study_flow.png"), width = 1800, height = 1200, res = 200)
  op <- par(mar = c(3, 8, 3, 2))
  y <- rev(seq_len(nrow(flow)))
  plot(NA, xlim = c(0, max(flow$n, na.rm = TRUE) * 1.18), ylim = c(0.5, nrow(flow) + 0.5),
       axes = FALSE, xlab = "", ylab = "", main = "Study Population Flow")
  rect(0, y - 0.28, flow$n, y + 0.28, col = "#9ECAE1", border = NA)
  axis(2, at = y, labels = flow$step, las = 1, tick = FALSE)
  axis(1)
  text(flow$n, y, labels = format(flow$n, big.mark = ","), pos = 4, cex = 0.9)
  box(bty = "l")
  par(op)
  dev.off()
}

prev <- svyby(~cardiometabolic_multimorbidity, ~sedentary_category, des, svymean, na.rm = TRUE, keep.names = FALSE)
plot_data <- data.frame(sedentary_category = prev$sedentary_category, percent = prev$cardiometabolic_multimorbidity * 100, se = prev$se * 100)
write_csv_base(plot_data, root_path("outputs", "tables", "figure_3_multimorbidity_by_sedentary_data.csv"))

png(root_path("outputs", "figures", "figure_3_multimorbidity_by_sedentary.png"), width = 1800, height = 1200, res = 200)
barplot(plot_data$percent, names.arg = plot_data$sedentary_category, col = "#9ECAE1",
        ylab = "Weighted prevalence, %", xlab = "Sedentary time category",
        main = "")
dev.off()

reg_file <- root_path("outputs", "tables", "table_3_multimorbidity_regression_categorical.csv")
if (file.exists(reg_file)) {
  reg <- read.csv(reg_file, stringsAsFactors = FALSE)
  reg <- reg[grepl("^sedentary_category", reg$term), ]
  png(root_path("outputs", "figures", "figure_4_adjusted_odds_ratios.png"), width = 1800, height = 1200, res = 200)
  plot(reg$odds_ratio, seq_len(nrow(reg)), xlim = range(c(reg$conf_low, reg$conf_high, 1), na.rm = TRUE),
       yaxt = "n", xlab = "Adjusted odds ratio", ylab = "", pch = 19,
       main = "Adjusted Odds Ratios by Sedentary Time Category")
  axis(2, at = seq_len(nrow(reg)), labels = sub("^sedentary_category", "", reg$term), las = 1)
  abline(v = 1, lty = 2)
  segments(reg$conf_low, seq_len(nrow(reg)), reg$conf_high, seq_len(nrow(reg)))
  dev.off()
}

box_data <- data[!is.na(data$cardiometabolic_multimorbidity) & !is.na(data$sitting_hours_day), ]
box_data$multimorbidity_group <- ifelse(box_data$cardiometabolic_multimorbidity == 1, "Multimorbidity", "No multimorbidity")
box_summary <- do.call(rbind, lapply(split(box_data$sitting_hours_day, box_data$multimorbidity_group), function(x) {
  data.frame(
    n = length(x),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE)
  )
}))
box_summary$group <- row.names(box_summary)
row.names(box_summary) <- NULL
write_csv_base(box_summary[, c("group", "n", "q25", "median", "q75", "mean")], root_path("outputs", "tables", "figure_7_sitting_boxplot_data.csv"))

png(root_path("outputs", "figures", "figure_7_sitting_boxplot_by_multimorbidity.png"), width = 1800, height = 1200, res = 200)
boxplot(sitting_hours_day ~ multimorbidity_group, data = box_data, col = c("#A1D99B", "#FC9272"),
        ylab = "Sitting time, hours/day", xlab = "", main = "Sitting Time by Cardiometabolic Multimorbidity")
stripchart(sitting_hours_day ~ multimorbidity_group, data = box_data, method = "jitter",
           vertical = TRUE, pch = 16, col = grDevices::adjustcolor("#252525", alpha.f = 0.12), add = TRUE)
dev.off()

scatter_data <- data[!is.na(data$age) & !is.na(data$sitting_hours_day), c("seqn", "age", "sitting_hours_day", "cardiometabolic_multimorbidity")]
write_csv_base(scatter_data, root_path("outputs", "tables", "figure_8_age_sedentary_scatter_data.csv"))

png(root_path("outputs", "figures", "figure_8_age_sedentary_scatter.png"), width = 1800, height = 1200, res = 200)
cols <- ifelse(scatter_data$cardiometabolic_multimorbidity == 1, "#CB181D", "#2171B5")
plot(scatter_data$age, scatter_data$sitting_hours_day, pch = 16,
     col = grDevices::adjustcolor(cols, alpha.f = 0.25),
     xlab = "Age, years", ylab = "Sitting time, hours/day",
     main = "Age and Daily Sitting Time")
lines(stats::lowess(scatter_data$age, scatter_data$sitting_hours_day, f = 0.35), col = "#238B45", lwd = 3)
legend("topleft", legend = c("No multimorbidity", "Multimorbidity", "Lowess trend"),
       col = c("#2171B5", "#CB181D", "#238B45"), pch = c(16, 16, NA), lty = c(NA, NA, 1), bty = "n")
dev.off()

age_sit <- svyby(~sitting_hours_day, ~age_group, des, svymean, na.rm = TRUE, keep.names = FALSE)
line_data <- data.frame(age_group = age_sit$age_group, mean_hours = age_sit$sitting_hours_day, se = age_sit$se)
write_csv_base(line_data, root_path("outputs", "tables", "figure_9_sitting_by_age_group_data.csv"))

png(root_path("outputs", "figures", "figure_9_sitting_by_age_group_line.png"), width = 1800, height = 1200, res = 200)
x <- seq_len(nrow(line_data))
ylim <- range(c(line_data$mean_hours - 1.96 * line_data$se, line_data$mean_hours + 1.96 * line_data$se), na.rm = TRUE)
plot(x, line_data$mean_hours, type = "b", pch = 19, lwd = 3, col = "#756BB1",
     xaxt = "n", ylim = ylim, xlab = "Age group", ylab = "Weighted mean sitting time, hours/day",
     main = "Weighted Mean Sitting Time by Age Group")
axis(1, at = x, labels = line_data$age_group)
segments(x, line_data$mean_hours - 1.96 * line_data$se, x, line_data$mean_hours + 1.96 * line_data$se, col = "#756BB1", lwd = 2)
arrows(x, line_data$mean_hours - 1.96 * line_data$se, x, line_data$mean_hours + 1.96 * line_data$se,
       angle = 90, code = 3, length = 0.05, col = "#756BB1")
dev.off()

pa_tab <- svytable(~sedentary_category + meets_pa_guideline, des)
pa_prop <- prop.table(pa_tab, margin = 1) * 100
pa_data <- as.data.frame(pa_prop)
names(pa_data) <- c("sedentary_category", "pa_guideline", "percent")
write_csv_base(pa_data, root_path("outputs", "tables", "figure_10_pa_by_sedentary_data.csv"))

png(root_path("outputs", "figures", "figure_10_pa_by_sedentary_stacked_bar.png"), width = 1800, height = 1200, res = 200)
mat <- t(pa_prop)
barplot(mat, beside = FALSE, col = c("#FDD0A2", "#74C476"), ylim = c(0, 100),
        ylab = "Weighted percent within sedentary category", xlab = "Sedentary time category",
        main = "Physical Activity Guideline Status by Sedentary Time")
legend("topright", legend = rownames(mat), fill = c("#FDD0A2", "#74C476"), bty = "n")
dev.off()
