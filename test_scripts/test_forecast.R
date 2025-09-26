devtools::load_all(".")
library(ViroReportR)
library(dplyr)


disease_type <- "rsv"
test_data <- simulate_data()
formatted_data <- get_aggregated_data(
  test_data,
  number_column = disease_type,
  date_column = "date",
  start_date = "2024-04-01",
  end_date = "2024-05-01"
)


# TODO: something wrong with the modelling
res_smooth <- forecast_epiestim(
  data = formatted_data,
  start_date = "2024-04-01",
  n_days = 7,
  type = "rsv",
  smooth_data = T
)

res_non_smooth <- forecast_epiestim(
  data = formatted_data,
  start_date = "2024-04-01",
  n_days = 7,
  type = "rsv",
  smooth_data = F
)

# cur_daily_samples <- res_non_smooth %>%
#   dplyr::rename(daily_date = date, sim = sim, daily_incidence = incidence)
#
#
# cur_samples_agg_quantiles <- cur_daily_samples %>%
#   create_quantiles(daily_date, variable = "daily_incidence") %>%
#   dplyr::rename(quantile_date = daily_date)


plot_forecast_comparison(smooth = res_smooth, non_smooth = res_non_smooth)+
  geom_point(ggplot2::aes(x=date,y=confirm),data=formatted_data) +
  ggplot2::ggtitle(glue::glue("7 day windows"))+
  ggplot2::coord_cartesian(ylim=c(0,70),expand=FALSE)


# plot_validation(time_period_result = cur_samples_agg_quantiles, pred_plot = "ribbon")
