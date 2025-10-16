forecast_obj <- create_test_forecast_time_period()

test_that("forecast_validation_metric throws error for incorrect object class", {
  fake_obj <- list()
  class(fake_obj) <- "not_forecast_time_period"
  expect_error(
    forecast_validation_metric(fake_obj),
    "input must be validation_res of class forecast_time_period"
  )
})

test_that("forecast_validation_metric returns expected list structure", {
  forecast_obj <- create_test_forecast_time_period()
  result <- forecast_validation_metric(forecast_obj)
  expect_type(result, "list")
  expect_named(result, c("train_period","forecast_period", "smape","mase"))
})
