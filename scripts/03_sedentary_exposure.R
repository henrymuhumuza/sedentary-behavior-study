source(file.path("R", "analysis_helpers.R"))
init_project()
library(survey)

data <- analysis_sample(derive_analysis_data(load_master()))
des <- survey_design(data)

q <- svyquantile(~sitting_minutes_day, des, quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE, ci = FALSE)
dist <- data.frame(
  measure = c("Weighted Q1", "Weighted median", "Weighted Q3"),
  minutes_day = as.numeric(q[[1]])
)
categories <- svy_percent(des, "sedentary_category")
names(categories)[names(categories) == "level"] <- "sedentary_category"

write_csv_base(dist, root_path("outputs", "tables", "sedentary_time_weighted_distribution.csv"))
write_csv_base(categories, root_path("outputs", "tables", "sedentary_time_category_distribution.csv"))

png(root_path("outputs", "figures", "figure_2_sedentary_time_distribution.png"), width = 1800, height = 1200, res = 200)
max_break <- ceiling(max(data$sitting_minutes_day, na.rm = TRUE) / 60) * 60
hist(data$sitting_minutes_day, breaks = seq(0, max_break, by = 60),
     col = "#6BAED6", border = "white", xlab = "Sitting time, minutes/day",
     main = "")
abline(v = as.numeric(q[[1]][2]), col = "#CB181D", lwd = 2)
dev.off()
