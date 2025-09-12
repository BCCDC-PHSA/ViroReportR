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
plot_forecast_comparison(smooth = res_smooth, non_smooth = res_non_smooth)+
  geom_point(ggplot2::aes(x=date,y=confirm),data=formatted_data) +
  ggplot2::ggtitle(glue::glue("7 day windows"))+
  ggplot2::coord_cartesian(ylim=c(0,70),expand=FALSE)



# 
# window_size <- 7
# for(p in seq(0.3,1,by=0.1)){
#   max_date <- quantile(formatted_data$date,p,type=1)
#   filtered_formatted_data <-
#     formatted_data |>
#     dplyr::filter(date < max_date)
#   # change window size to custom window_size
#   # starting at 2 as conditional on the past observations
#   n_t <- nrow(filtered_formatted_data)
#   t_start <- seq(2, max(n_t-(window_size - 1),2))
#   t_end <- pmin(t_start+window_size - 1,n_t)
#   
#   res <- estimate_R(filtered_formatted_data$confirm,
#                     method="parametric_si",
#                     config = make_config(list(
#                       t_start = t_start,
#                       t_end = t_end,
#                       mean_si = 2.6,
#                       std_si = 1.5))
#   )
#   forecast_res[[scales::percent(p)]] <- generate_forecasts(filtered_formatted_data,res,n_days=7)
#   
# }
# do.call(plot_forecast_comparison,forecast_res) +
#   geom_point(ggplot2::aes(x=date,y=confirm),data=formatted_data) +
#   ggplot2::ggtitle(glue::glue("{window_size} day windows"))
