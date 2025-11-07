test_that("generate_forecast returns expected output structure", {
  skip_if_not_installed("EpiEstim")
  skip_if_not_installed("projections")
  skip_if_not_installed("incidence")

  # create test data
  set.seed(123)
  test_data <- simulate_data(days = 30,
                             peaks = c(flua = 60),
                             amplitudes = c(flua = 90),
                             scales = c(flua = -0.01),
                             time_offset = 45)

  names(test_data) <- c("date","confirm")

  # run function
  res <- generate_forecast(
    data = test_data,
    start_date = as.Date("2024-01-07"),
    n_days = 7,
    type = "flu_a"
  )

  # check output type and columns
  expect_s3_class(res$forecast_res_quantiles, "data.frame")
  expect_true(all(c("date", "p50","p10", "p25", "p75","p90", "p025", "p975", "min_sim", "max_sim") %in% names(res$forecast_res_quantiles)))

  # check that forecast includes requested horizon
  expect_true(min(res$forecast_res_quantiles$date) > max(test_data$date))
  expect_true(max(res$forecast_res_quantiles$date) == max(test_data$date) + 7)
})

test_that("generate_forecast respects smoothing option", {
  test_data <- simulate_data(days = 30,
                             peaks = c(flu_a = 60),
                             amplitudes = c(flu_a = 90),
                             scales = c(flu_a = -0.01),
                             time_offset = 45)

  names(test_data) <- c("date","confirm")

  res_no_smooth <- generate_forecast(
    data = test_data,
    start_date = as.Date("2024-01-07"),
    type = "flu_a",
    smooth_data = FALSE,
    n_days = 3
  )

  res_smooth <- generate_forecast(
    data = test_data,
    start_date = as.Date("2024-01-07"),
    type = "flu_a",
    smooth_data = TRUE,
    n_days = 3
  )

  expect_s3_class(res_no_smooth$forecast_res_quantiles, "data.frame")
  expect_s3_class(res_smooth$forecast_res_quantiles, "data.frame")
  expect_true(all(c("date", "p50","p10", "p25", "p75","p90", "p025", "p975", "min_sim", "max_sim") %in% names(res_smooth$forecast_res_quantiles)))
  expect_true(all(c("date", "p50","p10", "p25", "p75","p90", "p025", "p975", "min_sim", "max_sim") %in% names(res_no_smooth$forecast_res_quantiles)))
})

test_that("generate_forecast errors with invalid input", {
  bad_data <- data.frame(
    time = seq(as.Date("2024-01-07"), by = "day", length.out = 10),
    cases = rpois(10, 5)
  )

  expect_error(
    generate_forecast(
      data = bad_data,
      start_date = as.Date("2024-01-07"),
      type = "rsv"
    ),
    "Data needs columns: date, confirm"
  )

  expect_error(
    generate_forecast(
      data = data.frame(date = Sys.Date(), confirm = 1),
      start_date = Sys.Date(),
      type = "rsv"
    ),
    "At least 14 days of data are needed."
  )
})
