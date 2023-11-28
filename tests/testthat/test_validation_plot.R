#################### Test validation_plot function ################################

weekly_plover_data <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")

time_period_result <- forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-09", n_days = 14, type = "flu_a",
                              aggregate_week = TRUE)

time_period_result_daily <- forecast_time_period_epiestim(data = weekly_plover_date_type,
                                                          start_date_str = "2022-10-09", n_days = 14, type = "flu_a")

# Test function error handling  --------------------------------

test_that("Empty prediction horizon throws an error", {
  expect_error(plotValidation(time_period_result),
               "Must specify prediction time horizon for validation plot")
})

test_that("Daily aggregated data throws an error", {
  expect_error(plotValidation(time_period_result_daily, pred_horizon_str = "1 week ahead"),
               "Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE")
})

test_that("Incorrect prediction horizon throws an error", {
  expect_error(plotValidation(time_period_result, pred_horizon_str = "3 week ahead"),
               "Prediction horizon not found in time_period_result, please check input")
})

