#################### Test validation_plot function ################################
set.seed(123)
daily_data <- simulate_data(days = 30, peaks = c(flua = 60),
                            amplitudes = c(flua = 90),
                            scales = c(flua = -0.01),
                            time_offset = 45)

disease_type <- "flua"
daily_flua <- data.frame(date = daily_data[["date"]],
                         confirm = daily_data[[disease_type]])

# Run forecast_time_period
forecast_obj <- generate_validation(
  data = daily_flua,
  start_date = "2024-01-10",
  n_days = 7,
  type = "flu_a",
  validate_window_size = 7,
  window_size = 7,
  smooth_data = T,
  smoothing_cutoff = 10
)

test_that("plot_validation errors on invalid pred_plot", {
  expect_error(plot_validation(data = daily_flua,
                               validation_res = forecast_obj,
                               pred_plot = "not_a_plot"),
               "Supported plot types are 'error_bar' and 'ribbon'")
})

test_that("plot_validation returns a ggplot for error_bar and ribbon", {

  p1 <- plot_validation(data = daily_flua,
                        validation_res = forecast_obj,
                        pred_plot = "error_bar")
  p2 <- plot_validation(data = daily_flua,
                        validation_res = forecast_obj,
                        pred_plot = "ribbon")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # check expected geoms
  geoms_p1 <- vapply(p1$layers, function(l) class(l$geom)[1], character(1))
  geoms_p2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomErrorbar" %in% geoms_p1)
  expect_true("GeomRibbon" %in% geoms_p2)
})




