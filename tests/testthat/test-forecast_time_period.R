test_that("forecast_time_period works for flu_a with EpiEstim", {
  # Simulate or load the data
  set.seed(123)
  daily_data <- simulate_data(days = 30, peaks = c(flua = 30),
                              amplitudes = c(flua = 60),
                              scales = c(flua = -0.01))

  disease_type <- "flua"
  daily_flua <- data.frame(date = daily_data$date, confirm = daily_data[[disease_type]])

  # Run forecast_time_period
  result <- forecast_time_period(
    data = daily_flua,
    start_date = "2022-10-02",
    n_days = 14,
    type = "flu_a",
    time_period = "daily",
    algorithm = "EpiEstim"
  )

  # Basic checks
  expect_s3_class(result[[1]]$R, "data.frame")
  expect_true( "R" %in% names(result[[1]]))
  expect_true("dates" %in% names(result[[1]]))

})
