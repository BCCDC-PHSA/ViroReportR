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

plot_R_fit_comparison(smooth = res_smooth, non_smooth = res_non_smooth)

plot_forecast_comparison(smooth = res_smooth, non_smooth = res_non_smooth)+
  geom_point(ggplot2::aes(x=date,y=confirm),data=formatted_data) +
  ggplot2::ggtitle(glue::glue("7 day windows"))+
  ggplot2::coord_cartesian(ylim=c(0,70),expand=FALSE)

# params <- list()
# params$filepath <- "u:/project/test_vri.csv"
# params$start_date <- "2024-04-01"
# params$n_days <- 7
# params$smooth <- TRUE
# params$validate_window_size <- 7

# generate_forecast_report(input_data_dir = "u:/project/test_vri.csv",
#                          output_dir = "inst/",
#                          n_days = 7,
#                          validate_window_size = 1,
#                          smooth = TRUE)
#
#
# forecast_epiestim(
#   data = data,
#   start_date = "2024-04-01",
#   n_days = 7,
#   type = "flu_a",
#   smooth_data = T
# )
