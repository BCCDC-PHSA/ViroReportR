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

# TODO: something wrong with the modelling
time_period_result_daily <- forecast_time_period(
  data = formatted_data, 
  start_date = "2024-04-10",
  n_days = 7, 
  type = "rsv", 
  time_period = "daily", 
  algorithm = "EpiEstim"
)

plot_validation(time_period_result_daily, pred_plot = "ribbon") +
  ggplot2::coord_cartesian(ylim=c(0,500),expand=FALSE)

plot_validation(time_period_result_daily, pred_plot = "violin") +
  ggplot2::coord_cartesian(ylim=c(0,500),expand=FALSE)
