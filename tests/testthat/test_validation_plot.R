#################### Test validation_plot function ################################

# Test function error handling  --------------------------------

test_that("Empty prediction horizon throws an error", {
  expect_error(plotValidation(time_period_result_weekly),
               "Must specify prediction time horizon for validation plot")
})

test_that("Daily aggregated data throws an error", {
  expect_error(plotValidation(time_period_result_daily, pred_horizon_str = "1 week ahead"),
               "Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE")
})

test_that("Incorrect prediction horizon throws an error", {
  expect_error(plotValidation(time_period_result_weekly, pred_horizon_str = "3 week ahead"),
               "Prediction horizon not found in time_period_result, please check input")
})

