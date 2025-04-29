test_time_period <- create_test_forecast_time_period()

test_that("pred_interval_forecast works", {
  pred_horizon_str <- "7 days ahead"
  expect_no_error(
    pred_interval_forecast(time_period_result = test_time_period,
                           pred_horizon = pred_horizon_str)
  )
})

test_that("bad pred_horizon string", {
  expect_error(
    pred_interval_forecast(time_period_result = test_time_period,
                           pred_horizon = "1 week ahead"),
    "`pred_horizon_str` not in forecast data"
  )
})
