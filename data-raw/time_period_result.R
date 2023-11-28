# Code to prepare weekly_time_period_result
weekly_time_period_result <-  forecast_time_period_epiestim(
 data = forecast_to_weekly_dat_example(),
  start_date_str = "2022-10-02", n_days = 14, type = "flu_a", aggregate_week = TRUE
  )

usethis::use_data(weekly_time_period_result, overwrite = TRUE, compress = "bzip2")


# Code to prepare daily_time_period_result
daily_time_period_result <-  forecast_time_period_epiestim(
  data = forecast_to_weekly_dat_example(),
  start_date_str = "2022-10-02", n_days = 14, type = "flu_a")

usethis::use_data(daily_time_period_result, overwrite = TRUE)
