
library(ViroReportR)
library(EpiEstim)
library(ggplot2)
library(dplyr)

# create data
disease_type <- "rsv"
test_data <- simulate_data(noise_sd=5)
formatted_data <- get_aggregated_data(
  test_data,
  number_column = disease_type,
  date_column = "date",
  start_date = "2024-04-01",
  end_date = "2024-05-01"
)

head(formatted_data)
formatted_data %>% ggplot(aes(x=date,y=confirm)) +
  geom_point()


# estimate R using Epiestim vignette approach
# see:
res_one_week <- estimate_R(formatted_data$confirm,
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 2.6,
                                  std_si = 1.5))
)


# change window size to every two weeks
n_t <- nrow(formatted_data)
t_start <- seq(2, n_t-13) # starting at 2 as conditional on the past observations
t_end <- t_start + 13 # adding 13 to get 14-day windows as bounds included in window

res_two_week <- estimate_R(formatted_data$confirm,
                           method="parametric_si",
                           config = make_config(list(
                             t_start = t_start,
                             t_end = t_end,
                             mean_si = 2.6,
                             std_si = 1.5))
)




# compare R fit for one and two week sliding window
plot_R_fit_comparison("one week"=res_one_week,"two week"=res_two_week)

# check forecasts from incidence and projections package
date_list <- formatted_data |>
  tidyr::uncount(confirm) |>
  dplyr::pull(date)

incidence_obj <- incidence::incidence(date_list)
R_list <- rnorm(100,
                mean = res_one_week$R$`Mean(R)`[nrow(res_one_week$R)],
                sd = res_one_week$R$`Std(R)`[nrow(res_one_week$R)])
proj_one_week <- projections::project(x=incidence_obj,R = R_list,
                                      si = res_one_week$si_distr[-1],
                                      n_days = 60)

plot(proj_one_week)

# compute forecasts from Epiestim objects
forecast_one_week <- generate_forecasts(formatted_data,res_one_week,n_days=28)
forecast_two_week <- generate_forecasts(formatted_data,res_two_week,n_days=28)

plot_forecast_comparison("one week"=forecast_one_week,"two week"=forecast_two_week)

forecast_one_week |>
  mutate(type="one week") |>
  bind_rows(
  forecast_two_week |>
  mutate(type = "two week")
  ) |>
  filter(sim < 30) |>
  ggplot(aes(x=date,y=incidence,group=interaction(sim,type),color=type)) +
  geom_line(alpha=0.2)

# compare forecasts using different proportions of data throughout the
# whole time series
forecast_res <- list()
# p is the quantile of data starting at 30% of the time-series
# and proceeding to 100% of the time-series in increments of 10%
window_size <- 3
for(p in seq(0.3,1,by=0.1)){
  max_date <- quantile(formatted_data$date,p,type=1)
  filtered_formatted_data <-
    formatted_data |>
    dplyr::filter(date < max_date)
  # change window size to custom window_size
  # starting at 2 as conditional on the past observations
  n_t <- nrow(filtered_formatted_data)
  t_start <- seq(2, max(n_t-(window_size - 1),2))
  t_end <- pmin(t_start+window_size - 1,n_t)

  res <- estimate_R(filtered_formatted_data$confirm,
                             method="parametric_si",
                             config = make_config(list(
                               t_start = t_start,
                               t_end = t_end,
                               mean_si = 2.6,
                               std_si = 1.5))
  )
  forecast_res[[scales::percent(p)]] <- generate_forecasts(filtered_formatted_data,res,n_days=7)

}
do.call(plot_forecast_comparison,forecast_res) +
  geom_point(ggplot2::aes(x=date,y=confirm),data=formatted_data) +
  ggplot2::ggtitle(glue::glue("{window_size} day windows"))
