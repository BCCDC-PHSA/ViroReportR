#################### Test validation summary function ################################
test_that("Empty prediction horizon throws an error", {
  expect_error(
    summary(weekly_time_period_result),
    "Must specify prediction time horizon for validation summary"
  )
})

test_that("Daily aggregated data throws an error", {
  expect_error(
    summary(daily_time_period_result, pred_horizon_str = "1 week ahead"),
    "Only weekly aggregated data suitable for validation summary. Please re-run forecast_time_period_epiestim with time_period = weekly"
  )
})


test_that("Incorrect prediction horizon throws an error", {
  expect_error(
    summary(weekly_time_period_result, pred_horizon_str = "3 week ahead"),
    "Prediction horizon not found in time_period_result, please check input"
  )
})

# Test function output and warning handling --------------------------------

test_that("Warning is thrown if confirmed case value is not within prediction percentile interval", {
  expect_warning(
    summary(weekly_time_period_result, pred_horizon_str = "2 week ahead"),
    "Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable"
  )
})

test_that("One week ahead predictions are correct dimension (skipping first two dates in data)", {
  expect_equal(length(intersect(suppressWarnings(summary(weekly_time_period_result, pred_horizon_str = "1 week ahead"))$individual_quantiles$weekly_date, c(
    weekly_transformed_plover_data$date[1],
    weekly_transformed_plover_data$date[2]
  ))), 0)
})


rm(bad_pred_example)
