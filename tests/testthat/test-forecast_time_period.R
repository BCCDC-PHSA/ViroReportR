test_that("forecast_time_period works for flu_a with EpiEstim", {
  # Simulate or load the data
  daily_data <- simulate_data()  # Or use load("data/daily_data.rda") if pre-saved

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
  expect_s3_class(result, "data.frame")
  expect_true("date" %in% names(result))
  expect_true("mean_R" %in% names(result) || "R" %in% names(result))  # Adjust depending on output
  expect_equal(nrow(result), 14)
})
