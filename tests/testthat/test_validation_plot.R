#################### Test validation_plot function ################################

p_test_oneweek <- plot_validation(weekly_time_period_result, pred_horizon_str = "1 week ahead")

p_test_twoweek <- plot_validation(weekly_time_period_result, pred_horizon_str = "2 week ahead")

# Test function error handling  --------------------------------

test_that("Empty prediction horizon throws an error", {
  expect_error(
    plot_validation(weekly_time_period_result),
    "Must specify prediction time horizon for validation plot"
  )
})

test_that("Daily aggregated data throws an error", {
  expect_error(
    plot_validation(daily_time_period_result, pred_horizon_str = "1 week ahead"),
    "Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with time_period = weekly"
  )
})


test_that("Incorrect prediction horizon throws an error", {
  expect_error(
    plot_validation(weekly_time_period_result, pred_horizon_str = "3 week ahead"),
    "Prediction horizon not found in time_period_result, please check input"
  )
})

# Test function output --------------------------------

test_that("Violin plot option contains two layers: violin and points of confirmed cases", {
  expect_equal(
    length(plot_validation(weekly_time_period_result, pred_horizon_str = "1 week ahead")$layers),
    2
  )
})

test_that("Ribbon plot option contains four layers: points of confirmed cases, median ribbon, 5% and 95% and 25 and 75% bands", {
  expect_equal(
    length(plot_validation(weekly_time_period_result, pred_horizon_str = "1 week ahead", pred_plot = "ribbon")$layers),
    4
  )
})

test_that("Violin plot option contains two layers: violin and points of confirmed cases", {
  expect_equal(
    length(plot_validation(weekly_time_period_result, pred_horizon_str = "1 week ahead")$layers),
    2
  )
})

test_that("pred_horizon_str is correctly passed on to ribbon plot for 1 week ahead predictions", {
  expect_true(any(p_test_oneweek$data$pred_horizon == "1 week ahead"))
})

test_that("pred_horizon_str is correctly passed on to ribbon plot for 2 week ahead predictions", {
  expect_true(any(p_test_twoweek$data$pred_horizon == "2 week ahead"))
})

rm(p_test_oneweek, p_test_twoweek)
