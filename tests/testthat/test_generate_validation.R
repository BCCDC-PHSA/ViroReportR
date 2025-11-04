test_that("generate_validation works for flu_a", {
  # Simulate or load the data
  set.seed(123)
  daily_data <- simulate_data(days = 30, peaks = c(flua = 30),
                              amplitudes = c(flua = 60),
                              scales = c(flua = -0.01))

  disease_type <- "flua"
  daily_flua <- data.frame(date = daily_data[["date"]],
                           confirm = daily_data[[disease_type]])

  # Run forecast_time_period
  validation_smooth <- generate_validation(
    data = daily_flua,
    start_date = "2024-01-11",
    n_days = 7,
    type = "flu_a",
    validate_window_size = 7,
    window_size = 7,
    smooth_data = T,
    smoothing_cutoff = 10
  )

  # Basic checks
  expect_true( "original_data" %in% names(validation_smooth[[1]]))
  expect_true( "smoothed_data" %in% names(validation_smooth[[1]]))
  expect_true( "smoothed_error" %in% names(validation_smooth[[1]]))
  expect_true("forecast_res_quantiles" %in% names(validation_smooth[[1]]))
  expect_true("estimate_R" %in% names(validation_smooth[[1]]))

  expect_true(nrow(validation_smooth[[1]]$forecast_res_quantiles) == 7)

  expect_error(generate_validation(
    data = daily_flua,
    start_date = "2024-01-11",
    n_days = 14,
    type = "flu_a",
    validate_window_size = 7,
    window_size = 7,
    smooth_data = T,
    smoothing_cutoff = 10
  ),
  "Not enough data for number of days to forecast in validation 14")
})
