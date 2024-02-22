# Code to prepare weekly_time_period_result
weekly_time_period_result <- forecast_time_period(
  data = weekly_transformed_plover_data,
  start_date = "2022-10-02", n_days = 14, type = "flu_a", time_period = "weekly", algorithm = "EpiEstim"
)

usethis::use_data(weekly_time_period_result, overwrite = TRUE, compress = "bzip2")


# Code to prepare daily_time_period_result
daily_time_period_result <- forecast_time_period(
  data = weekly_transformed_plover_data,
  start_date = "2022-10-02", n_days = 14, type = "flu_a",
  time_period = "daily", algorithm = "EpiEstim"
)

usethis::use_data(daily_time_period_result, overwrite = TRUE)
