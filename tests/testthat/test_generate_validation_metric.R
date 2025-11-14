test_that("generate_validation_metric returns expected list structure", {
  set.seed(123)
  daily_data <- simulate_data(days = 30, peaks = c(flu_a = 60),
                              amplitudes = c(flu_a = 90),
                              scales = c(flu_a = -0.01),
                              time_offset = 45)

  disease_type <- "flu_a"
  daily_flua <- data.frame(date = daily_data[["date"]],
                           confirm = daily_data[[disease_type]])

  # Run forecast_time_period
  forecast_obj <- generate_validation(
    data = daily_flua,
    start_date = "2024-01-10",
    n_days = 7,
    type = "flu_a",
    validate_window_size = 7,
    window_size = 7,
    smooth_data = T,
    smoothing_cutoff = 10
  )
  result <- generate_validation_metric(data = daily_flua,
                                       validation_res = forecast_obj)
  expect_type(result, "list")
  expect_named(result, c("train_period","forecast_period", "smape","mase"))
})
