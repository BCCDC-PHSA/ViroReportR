#################### Test validation_plot function ################################
forecast_obj <- create_test_forecast_time_period()

test_that("plot_validation errors on wrong class", {
  expect_error(plot_validation(list(), pred_plot = "ribbon"),
               "must be object of class forecast_time_period")
})

test_that("plot_validation errors on invalid pred_plot", {

  expect_error(plot_validation(forecast_obj, pred_plot = "not_a_plot"),
               "Supported plot types are 'error_bar' and 'ribbon'")
})

test_that("plot_validation returns a ggplot for error_bar and ribbon", {
  dummy_obj <- structure(list(list(
    quantile_unit = "day",
    model_data_date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3,
    smoothed_date = as.Date("2020-01-01") + 0:2,
    smoothed_confirm = 1:3
  )), class = "forecast_time_period")


  p1 <- plot_validation(forecast_obj, pred_plot = "error_bar")
  p2 <- plot_validation(forecast_obj, pred_plot = "ribbon")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # check expected geoms
  geoms_p1 <- vapply(p1$layers, function(l) class(l$geom)[1], character(1))
  geoms_p2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomErrorbar" %in% geoms_p1)
  expect_true("GeomRibbon" %in% geoms_p2)
})




