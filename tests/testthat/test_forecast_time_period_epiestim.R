#################### Test forecast_time_period_epiestim function and all associated helper functions ################################
# Test function error handling  --------------------------------

test_that("Start date that does not match dataset dates throws an error", {
  expect_error(
    forecast_time_period_epiestim(data = weekly_transformed_plover_data, start_date = "2022-10-03", n_days = 14, type = "flu_a", time_period = "weekly"),
    "Start date not present in dataset. Please check your input"
  )
})

test_that("Start date greater than or equal to end date throws an error", {
  expect_error(
    forecast_time_period_epiestim(data = weekly_transformed_plover_data, start_date = "2022-11-27", n_days = 14, type = "flu_a", time_period = "weekly"),
    "Start date greater than max date in dataset! Please check your input"
  )
})

test_that("Error is thrown when n_days is not a multiple of 7 and aggregate_week is set to true", {
  expect_error(forecast_time_period_epiestim(data = weekly_transformed_plover_data, start_date = "2022-10-02", n_days = 3, type = "flu_a", time_period = "weekly"),
    "n_days must be a multiple of 7 to aggregate by week",
    fixed = TRUE
  )
})


# Test function output --------------------------------

test_that("Mean of samples from rtrunc_norm function converges to specified mean", {
  expect_equal(mean(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 5, tolerance = 1e-2)
})

test_that("SD of samples from rtrunc_norm function converges to specified sd", {
  expect_equal(sd(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 1, tolerance = 1e-1)
})

test_that("rtrunc_norm function does not produce any negative numbers even with very high SD", {
  expect_true(all(rtrunc_norm(10000, mean = 0, sd = 50, lower_lim = 0) > 0))
})

# test_that("mean of rtrunc_norm function equal to mean of rtruncnorm function", {
#  expect_true(t.test(truncnorm::rtruncnorm(10000, mean = 5, sd = 1, a = 0), rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0))$p.value > 0.05)
# })

test_that("time_period = weekly argument works correctly to produce weekly quantiles", {
  expect_equal(length(forecast_time_period_epiestim(
    data = weekly_transformed_plover_data, start_date = "2022-10-02", n_days = 14, type = "flu_a",
    time_period = "weekly"
  )[[1]]$quantile_date), 2)
})


test_that("time_period = daily argument works correctly to produce daily quantiles", {
  expect_equal(length(forecast_time_period_epiestim(
    data = weekly_transformed_plover_data, start_date = "2022-10-02", n_days = 14, type = "flu_a",
    time_period = "daily"
  )[[1]]$quantile_date), 14)
})

test_that("n_days works correctly to produce number of daily quantiles (even if not multiple of 7)", {
  expect_equal(length(forecast_time_period_epiestim(
    data = weekly_transformed_plover_data, start_date = "2022-10-02", n_days = 3, type = "flu_a",
    time_period = "daily"
  )[[1]]$quantile_date), 3)
})

test_that("extend_model_data works correctly and dimension of list produced is correct", {
  expect_equal(length(forecast_time_period_epiestim(
    data = weekly_transformed_plover_data, start_date = "2022-10-02", n_days = 14, type = "flu_a",
    time_period = "weekly"
  )), nrow(weekly_transformed_plover_data) - 1)
})

test_that("extend_model_data indexes properly when start date is not first observation in dataset", {
  expect_equal(length(forecast_time_period_epiestim(
    data = weekly_transformed_plover_data, start_date = "2022-10-09", n_days = 14, type = "flu_a",
    time_period = "weekly"
  )), nrow(weekly_transformed_plover_data) - 2)
})

