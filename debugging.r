disease_type <- "flu_a"
load("data/daily_data.rda")

daily_flua <- data.frame(date = daily_data$date, confirm = daily_data[[disease_type]])

head(daily_flua)

time_period_result <- forecast_time_period(
  data = daily_flua, 
start_date = "2022-10-02", n_days = 14, type = "flu_a", 
time_period = "daily" , algorithm = "EpiEstim"
)