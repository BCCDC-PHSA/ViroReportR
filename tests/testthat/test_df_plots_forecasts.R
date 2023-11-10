#################### Test forecast_time_period_epiestim function and all associated helper functions ################################

# Run Package Data Transformation Functions for PLOVER data  --------------------------------

weekly_plover_data <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")

n_sims <- 1000
n_days <- 14
n_weeks <- 14/7
tp_test_daily <- forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = n_days, type = "flu_a",
                             aggregate_week = FALSE)

tp_test_weekly <- forecast_time_period_epiestim(data = weekly_plover_date_type, start_date_str = "2022-10-02", n_days = n_days, type = "flu_a",
                                               aggregate_week = TRUE)


# Test function output of plot_all_time_period_forecast_data --------------------------------

test_that("Correct number of plots are produced for daily plot", {
  expect_equal(length(plot(tp_test_daily)), nrow(weekly_plover_date_type) -1)
})

test_that("Correct number of plots are produced for weekly plot", {
  expect_equal(length(plot(tp_test_weekly)), nrow(weekly_plover_date_type) -1)
})

# Test function output of create_forecast_df --------------------------------

test_that("Correct number of quantiles of samples are present in daily df", {
  expect_equal(nrow(create_forecast_df(tp_test_daily)), n_sims*(nrow(weekly_plover_date_type)-1)*n_days)
})

test_that("Correct number of quantiles of samples are present in weekly df", {
  expect_equal(nrow(create_forecast_df(tp_test_weekly)), n_sims*(nrow(weekly_plover_date_type)-1)*n_weeks)
})


# Cleaning up environment
rm(weekly_plover_data, weekly_plover_date_type, disease_type, tp_test_daily, tp_test_weekly, n_days, n_weeks, n_sims)
