test_that("combine_df_pred_case works", {
  test_time_period <- create_test_forecast_time_period()
  pred_horizon_str <- "7 days ahead"
  smoothed_model_data <- data.frame(
    date = test_time_period[[length(test_time_period)]]$smoothed_date,
    confirm = test_time_period[[length(test_time_period)]]$smoothed_confirm
  )
  forecast_dat <- pred_interval_forecast(time_period_result = test_time_period,
                           pred_horizon = pred_horizon_str)
  expect_no_error({
    combine_df_pred_case(forecast_dat,
                         pred_horizon_str = pred_horizon_str)
  })

})
