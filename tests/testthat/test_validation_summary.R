#################### Test validation summary function ################################
bad_pred_example <- weekly_time_period_result

bad_pred_example[[length(bad_pred_example)]]$confirm[3] <- 60

# Test function error handling  --------------------------------

test_that("Empty prediction horizon throws an error", {
  expect_error(
    summary(weekly_time_period_result),
    "Must specify prediction time horizon for validation summary"
  )
})

test_that("Daily aggregated data throws an error", {
  expect_error(
    summary(daily_time_period_result, pred_horizon_str = "1 week ahead"),
    "Only weekly aggregated data suitable for validation summary. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE"
  )
})


test_that("Incorrect prediction horizon throws an error", {
  expect_error(
    summary(weekly_time_period_result, pred_horizon_str = "3 week ahead"),
    "Prediction horizon not found in time_period_result, please check input"
  )
})

# Test function output and warning handling --------------------------------

test_that("Warning is thrown if confirmed case value is not within prediction quantile", {
  expect_warning(
    summary(bad_pred_example, pred_horizon_str = "1 week ahead"),
    "Prediction quantiles do not cover some data-points. Some forecasts may not be reliable"
  )
})

test_that("One week ahead predictions are correct dimension (skipping first two dates in data)", {
  expect_equal(length(intersect(summary(weekly_time_period_result, pred_horizon_str = "1 week ahead")$individual_quantiles$weekly_date, c(
    weekly_transformed_plover_data$date[1],
    weekly_transformed_plover_data$date[2]
  ))), 0)
})


rm(bad_pred_example)
