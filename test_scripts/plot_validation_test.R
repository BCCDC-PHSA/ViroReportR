devtools::load_all(".")
library(ViroReportR)
library(dplyr)


disease_type <- "rsv"
test_data <- simulate_data()
formatted_data <- get_aggregated_data(
  test_data,
  number_column = disease_type,
  date_column = "date",
  start_date = "2024-04-01",
  end_date = "2024-05-01"
)

head(formatted_data)

time_period_result_daily <- generate_validation(
  data = formatted_data,
  start_date = "2024-04-01",
  n_days = 7,
  type = "rsv",
  smooth_data = TRUE,
  validate_window_size = 1,
  window_size = 7
)

plot_validation(data= formatted_data, time_period_result_daily, pred_plot = "ribbon")


# plot_validation(time_period_result_daily, pred_plot = "error_bar") +
#   ggplot2::coord_cartesian(ylim=c(0,70),expand=FALSE)
