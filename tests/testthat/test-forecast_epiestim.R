test_that("forecast_epiestim returns expected output structure", {
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
  res <- forecast_epiestim(
    data = test_data,
    start_date = as.Date("2024-01-01"),
    n_days = 7,
    type = "flu_a"
  )

  # check output type and columns
  expect_s3_class(res, "data.frame")
  expect_true(all(c("date", "incidence", "sim") %in% names(res)))

  # check that forecast includes requested horizon
  expect_true(min(res$date) > max(test_data$date))
  expect_true(max(res$date) == max(test_data$date) + 7)
})

test_that("forecast_epiestim respects smoothing option", {
  test_data <- simulate_data(days = 30,
                             peaks = c(flua = 60),
                             amplitudes = c(flua = 90),
                             scales = c(flua = -0.01),
                             time_offset = 45)

  names(test_data) <- c("date","confirm")

  res_no_smooth <- forecast_epiestim(
    data = test_data,
    start_date = as.Date("2024-01-01"),
    type = "flu_a",
    smooth_data = FALSE,
    n_days = 3
  )

  res_smooth <- forecast_epiestim(
    data = test_data,
    start_date = as.Date("2024-01-01"),
    type = "flu_a",
    smooth_data = TRUE,
    n_days = 3
  )

  expect_s3_class(res_no_smooth, "data.frame")
  expect_s3_class(res_smooth, "data.frame")
  expect_true(all(c("date", "incidence", "sim") %in% names(res_smooth)))
  expect_true(all(c("date", "incidence", "sim") %in% names(res_no_smooth)))
})

test_that("forecast_epiestim errors with invalid input", {
  bad_data <- data.frame(
    time = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    cases = rpois(10, 5)
  )

  expect_error(
    forecast_epiestim(
      data = bad_data,
      start_date = as.Date("2024-01-01"),
      type = "rsv"
    ),
    "Data needs columns: date, confirm"
  )

  expect_error(
    forecast_epiestim(
      data = data.frame(date = Sys.Date(), confirm = 1),
      start_date = Sys.Date(),
      type = "rsv"
    ),
    "At least 14 days of data are needed."
  )
})
