create_test_forecast_time_period <- function(){
  set.seed(123)
  daily_data <- simulate_data(days = 30, peaks = c(flua = 30),
                              amplitudes = c(flua = 60),
                              scales = c(flua = -0.01))

  disease_type <- "flua"
  daily_flua <- data.frame(date = daily_data[["date"]],
                           confirm = daily_data[[disease_type]])

  # Run forecast_time_period
  result <- forecast_time_period(
    data = daily_flua,
    start_date = "2024-01-11",
    n_days = 7,
    type = "flu_a",
    time_period = "daily",
    algorithm = "EpiEstim"
  )

  return(result)
}
