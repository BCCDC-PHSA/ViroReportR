#################### Test forecast_time_period_epiestim function and all associated helper functions ################################

# Run Package Data Transformation Functions for PLOVER data  --------------------------------

weekly_plover_data <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")

# Test function error handling  --------------------------------

test_that("Start date that does not match dataset dates throws an error", {
 expect_error(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-03", n_days = 14, type = "flu_a", aggregate_week = TRUE),
    "Start date not present in dataset. Please check your input")
})

test_that("Start date greater than or equal to end date throws an error", {
  expect_error(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-11-27", n_days = 14, type = "flu_a", aggregate_week = TRUE),
               "Start date greater than max date in dataset! Please check your input")
})

test_that("Error is thrown when n_days is not a multiple of 7 and aggregate_week is set to true", {
  expect_error(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = 3, type = "flu_a", aggregate_week = TRUE),
               "n_days must be a multiple of 7 to aggregate by week", fixed = TRUE)
})


# Test function output --------------------------------

test_that("Mean of samples from rtrunc_norm function converges to specified mean", {
  expect_equal(mean(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 5, tolerance = 1e-3)
})

test_that("SD of samples from rtrunc_norm function converges to specified sd", {
  expect_equal(sd(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 1, tolerance = 1e-2)
})

test_that("rtrunc_norm function does not produce any negative numbers even with very high SD", {
  expect_true(all(rtrunc_norm(10000, mean = 0, sd = 50, lower_lim = 0) > 0))
})

test_that("mean of rtrunc_norm function equal to mean of rtruncnorm function", {
  expect_true(t.test(truncnorm::rtruncnorm(10000, mean = 5, sd = 1, a = 0), rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0))$p.value > 0.05)
})

test_that("aggregate_week = TRUE argument works correctly to produce weekly quantiles", {
  expect_equal(length(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = 14, type = "flu_a",
                                                    aggregate_week = TRUE)[[1]]$quantile_date), 2)
})


test_that("aggregate_week = FALSE argument works correctly to produce daily quantiles", {
  expect_equal(length(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = 14, type = "flu_a",
                                                    aggregate_week = FALSE)[[1]]$quantile_date), 14)
})

test_that("n_days works correctly to produce number of daily quantiles (even if not multiple of 7)", {
  expect_equal(length(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = 3, type = "flu_a",
                                             aggregate_week = FALSE)[[1]]$quantile_date), 3)
})

test_that("extend_model_data works correctly and dimension of list produced is correct", {
  expect_equal(length(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = 14, type = "flu_a",
                                             aggregate_week = TRUE)), nrow(weekly_plover_date_type) - 1)
})

test_that("extend_model_data indexes properly when start date is not first observation in dataset", {
  expect_equal(length(forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-09", n_days = 14, type = "flu_a",
                                                    aggregate_week = TRUE)), nrow(weekly_plover_date_type) - 2)
})


# Cleaning up environment
rm(weekly_plover_data, weekly_plover_date_type, disease_type)
