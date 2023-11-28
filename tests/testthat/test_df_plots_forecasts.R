#################### Test forecast_time_period_epiestim function and all associated helper functions ################################

# Run Package Data Transformation Functions for PLOVER data  --------------------------------

n_sims <- 1000
n_days <- 14
n_weeks <- 14/7

# Test error handling of plot_all_time_period_forecast_data --------------------------------
test_that("Error is correctly thrown when time_period specified is out of range", {
  expect_error(plot(daily_time_period_result, time_period = 11),
        "Time period index out of bounds. Please cross-check the time_period input with the length of your time_period_result object")
})


# Test function output of plot_all_time_period_forecast_data --------------------------------

test_that("Correct number of plots are produced for daily plot", {
  expect_equal(length(plot(daily_time_period_result)), nrow(weekly_plover_date_type) -1)
})


test_that("Correct number of plots are produced for weekly plot", {
  expect_equal(length(plot(weekly_time_period_result)), nrow(weekly_plover_date_type) -1)
})


# Cleaning up environment
rm(n_days, n_weeks, n_sims)
