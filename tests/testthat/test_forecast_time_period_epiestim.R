#################### Test forecast_time_period_epiestim function and all associated helper functions ################################
set.seed(123)
test_data <- simulate_data(days = 15, peaks = c(flua = 30), amplitudes = c(flua = 60),
                           scales = c(flua = -0.01),
                           start_date = "2024-01-01")
names(test_data) <- c("date","confirm")
# Test function error handling  --------------------------------


test_that("Start date that does not match dataset dates throws an error", {
  expect_error(
    forecast_time_period_epiestim(data = test_data,
     start_date = "2022-10-03", n_days = 14, type = "flu_a", time_period = "daily"),
    "Data must include the `start_date`"
  )
})

test_that("Start date greater than or equal to end date throws an error", {
  expect_error(
    forecast_time_period_epiestim(data = test_data,
     start_date = "2025-01-01", n_days = 14, type = "flu_a", time_period = "daily"),
    "Data must include the `start_date`"
  )
})



# Test function output --------------------------------


test_that("time_period = daily argument works correctly to produce daily quantiles", {
  result <- forecast_time_period_epiestim(
    data = test_data, start_date = "2024-01-01", n_days = 14, type = "flu_a",
    time_period = "daily"
  )
  expect_equal(length(result[[1]]$quantile_date), 14)
})

test_that("n_days works correctly to produce number of daily quantiles (even if not multiple of 7)", {
  result <- forecast_time_period_epiestim(
    data = test_data, start_date = "2024-01-01", n_days = 3, type = "flu_a",
    time_period = "daily"
  )
  expect_equal(length(result[[1]]$quantile_date), 3)
})

test_that("extend_model_data works correctly and dimension of list produced is correct", {
  result <- forecast_time_period_epiestim(
    data = test_data, start_date = "2024-01-01", n_days = 14, type = "flu_a",
    time_period = "daily"
  )
  expect_equal(length(result), nrow(test_data) - 3)
})

test_that("extend_model_data indexes properly when start date is not first observation in dataset", {
  expect_equal(length(forecast_time_period_epiestim(
    data = test_data, start_date = "2024-01-05", n_days = 14, type = "flu_a",
    time_period = "daily"
  )), nrow(test_data) - 5 - 1)
})


