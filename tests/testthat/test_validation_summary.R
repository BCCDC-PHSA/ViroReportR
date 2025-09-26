forecast_obj <- create_test_forecast_time_period()

test_that("summary.forecast_time_period throws error for incorrect object class", {
  fake_obj <- list()
  class(fake_obj) <- "not_forecast_time_period"
  expect_error(
    summary.forecast_time_period(fake_obj, pred_horizon_str = "7 days ahead"),
    "input must be object of class forecast_time_period"
  )
})

test_that("summary.forecast_time_period throws error if pred_horizon_str is missing", {
  forecast_obj <- create_test_forecast_time_period()
  expect_error(
    summary(forecast_obj),
    "Must specify prediction time horizon for validation summary"
  )
})

test_that("summary.forecast_time_period returns expected list structure", {
  forecast_obj <- create_test_forecast_time_period()
  result <- summary(forecast_obj, pred_horizon_str = "7 days ahead")
  expect_type(result, "list")
  expect_named(result, c("individual_quantiles", "quantile_summary"))
})

test_that("individual_quantiles contains expected columns", {
  forecast_obj <- create_test_forecast_time_period()
  result <- summary(forecast_obj, pred_horizon_str = "7 days ahead")
  df <- result$individual_quantiles
  expect_s3_class(df, "data.frame")
  expect_true(all(c("date", "coverage", "Confirmed cases", "Predicted cases",
                    "50 percentile interval bounds", "95 percentile interval bounds") %in% names(df)))
})

test_that("quantile_summary contains expected columns and factor levels", {
  forecast_obj <- create_test_forecast_time_period()
  result <- summary(forecast_obj, pred_horizon_str = "7 days ahead")
  summ <- result$quantile_summary
  expect_s3_class(summ, "data.frame")
  expect_true(all(c("coverage", "counts", "proportion") %in% names(summ)))
  expect_true(is.factor(summ$coverage))
  expect_true(all(levels(summ$coverage) %in% c("50 percentile interval", "95 percentile interval", "Outside 95 percentile interval")))
})






