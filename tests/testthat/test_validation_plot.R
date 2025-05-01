#################### Test validation_plot function ################################
forecast_obj <- create_test_forecast_time_period()
# Test function error handling  --------------------------------

test_that("Empty prediction horizon throws an error", {
  expect_error(
    plot_validation(forecast_obj),
    "Must specify prediction time horizon for validation plot"
  )
})


test_that("Incorrect prediction horizon throws an error", {
  expect_error(
    plot_validation(forecast_obj, pred_horizon_str = "200 days ahead"),
    "`pred_horizon_str` not in forecast data"
  )
})

# Test function output --------------------------------

test_that("Violin plot option contains four layers: violin and points of confirmed cases", {
  expect_equal(
    length(plot_validation(forecast_obj, pred_horizon_str = "7 days ahead")$layers),
    4
  )
})

test_that("Ribbon plot option contains five layers: points of confirmed cases, median ribbon, 5% and 95% and 25 and 75% bands", {
  expect_equal(
    length(plot_validation(forecast_obj, pred_horizon_str = "7 days ahead", pred_plot = "ribbon")$layers),
    5
  )
})




