#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic from weekly data
#'
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#'
#'
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19. The serial intervals for the estimation of R were retrieved from
#' Cowling et al., 2011, Vink et al., 2014 and Madewell et al., 2023 for Influenza, RSV and COVID (BA.5 Omicron variant) respectively
#'
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param dt *Integer* 	length of temporal aggregations of the incidence data. This should be an integer or vector of integers. The default value is 7 time units (1 week).
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "covid" or "other"
#' @param mean_si *Numeric* User specification of mean of parametric serial interval
#' @param std_si *Numeric* User specification of standard deviation of parametric serial interval
#' @param recon_opt One of "naive" or "match" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param method One of "non_parametric_si", "parametric_si", "uncertain_si", "si_from_data" or "si_from_sample" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param ... Other optional parameters to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page) to control estimation of reproduction number
#'
#'
#' @return Object of class {\code{\link[EpiEstim]{estimate_R}}} (see \code{EpiEstim} help page)
#' @export
#'
#' @examples
#' plover_data <- data.frame(
#'   epiWeek_date = as.Date(c(
#'     "2022-10-02", "2022-10-09",
#'     "2022-10-16", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-12-04"
#'   )),
#'   epiWeek_year = c(
#'     2022, 2022, 2022, 2022,
#'     2022, 2022, 2022, 2022, 2022, 2022
#'   ),
#'   Epiweek = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
#'   flu_a = c(17, 19, 32, 38, 43, 45, 73, 88, 94, 105),
#'   flu_b = c(24, 31, 39, 45, 50, 52, 68, 83, 89, 97)
#' )
#'
#' weekly_plover_data <- get_weekly_plover(plover_data)
#'
#' plover_dat_clean <- get_weekly_plover_by_date_type(
#'   weekly_plover_data,
#'   "flu_a",
#'   "2022-10-01",
#'   "2022-12-05"
#' )
#'
#' fit_epiestim_model(data = plover_dat_clean, type = "flu_a")
fit_epiestim_model <- function(data, dt = 7L, type = NULL, mean_si = NULL, std_si = NULL, recon_opt = "match",
                               method = "parametric_si", ...) {
  confirm <- NULL
  if (!is.data.frame(data) || !all(colnames(data) %in% c("date", "confirm"))) {
    stop("Must pass a data frame with two columns: date and confirm")
  }
  if (missing(type) || !(type %in% c("flu_a", "flu_b", "covid", "rsv", "other"))) {
    stop("Must specify the type of epidemic (flu_a, flu_b, covid, rsv or other)")
  }
  if (type == "other" && is.null(mean_si) && is.null(std_si)) {
    stop("Must specify mean and standard deviation of parametric serial interval for type other")
  }
  data_lag <- as.numeric(difftime(data$date[2], data$date[1]))
  if (data_lag != 7 && dt == 7L) {
    warning("Your data may not be weekly data. Please check input and consider changing dt argument (dt = 1L for daily data)")
  }

  incid <- data$confirm
  if (is.null(mean_si) && is.null(std_si)) {
    if (type == "flu_a" | type == "flu_b") {
      config <- EpiEstim::make_config(list(
        mean_si = 3.6,
        std_si = 1.6
      ))
    } else if (type == "rsv") {
      config <- EpiEstim::make_config(list(
        mean_si = 7.5,
        std_si = 2.1
      ))
    } else if (type == "covid") {
      config <- EpiEstim::make_config(list(
        mean_si = 2.3,
        std_si = 1.4
      ))
    }
  } else {
    config <- EpiEstim::make_config(list(
      mean_si = mean_si,
      std_si = std_si
    ))
  }
  a_prior <- (config$mean_prior / config$std_prior)^2
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)

  epiestim_estimates <- NULL
  epiestim_estimates <- suppressWarnings(EpiEstim::estimate_R(
    incid = incid,
    dt = dt,
    recon_opt = recon_opt,
    method = method,
    config = config, ...
  ))
  if (data$confirm[1] < min_nb_cases_per_time_period) {
    min_reliable_date <- data %>%
      dplyr::filter(confirm >= min_nb_cases_per_time_period) %>%
      dplyr::pull(date)
    warning(
      "Incidence is too low on the current start date. Consider starting R estimation from ", min_reliable_date[1],
      " for an accurate estimate of the reproduction number with EpiEstim"
    )
  }

  return(epiestim_estimates)
}


#' Iterate through a time-period as a sliding window to produce short-term forecasts with the EpiEstim model fit
#'
#'
#' @description Function to produce short-term forecasts from objects of class {\code{\link[EpiEstim]{estimate_R}}}
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param start_date_str Initial starting time-point. Must match a timepoint in the input dataset
#' @param n_days The number of days to run simulations for. Defaults to 7
#' @param type *character* Specifies type of epidemic. Must be one of "Influenza", "RSV" and "COVID"
#' @param aggregate_week *logical* argument specifying whether to aggregate forecasts by weekly quantiles. Default is to return daily quantiles
#' @param ... Pass on optional arguments from \code{fit_epiestim_model}
#'
#'
#'
#' @return List of class \code{forecast_time_period_epiestim}
#' storing quantiles of both daily and 2 week ahead weekly forecasts from each sliding window
#' @export
#'
#' @examples
#' plover_data <- data.frame(
#'   epiWeek_date = as.Date(c(
#'     "2022-10-02", "2022-10-09",
#'     "2022-10-16", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-12-04"
#'   )),
#'   epiWeek_year = c(
#'     2022, 2022, 2022, 2022,
#'     2022, 2022, 2022, 2022, 2022, 2022
#'   ),
#'   Epiweek = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
#'   flu_a = c(17, 19, 32, 38, 43, 45, 73, 88, 94, 105),
#'   flu_b = c(24, 31, 39, 45, 50, 52, 68, 83, 89, 97)
#' )
#'
#' weekly_plover_data <- get_weekly_plover(plover_data)
#'
#' plover_dat_clean <- get_weekly_plover_by_date_type(
#'   weekly_plover_data,
#'   "flu_a",
#'   "2022-10-01",
#'   "2022-12-05"
#' )
#'
#' #  Daily forecast
#' forecast_time_period_epiestim(
#'   data = plover_dat_clean,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a"
#' )
#'
# weekly aggregated forecast
#' forecast_time_period_epiestim(
#'   data = plover_dat_clean,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a", aggregate_week = TRUE
#' )
forecast_time_period_epiestim <- function(data, start_date_str, n_days = 7, aggregate_week = FALSE,
                                          type = NULL, ...) {
  sim <- week_date <- daily_date <- NULL
  if (!(lubridate::ymd(start_date_str) %in% data$date)) {
    stop("Start date not present in dataset. Please check your input")
  }

  if (lubridate::ymd(start_date_str) >= max(data$date)) {
    stop("Start date greater than max date in dataset! Please check your input")
  }
  start_index <- which(data$date == lubridate::ymd(start_date_str))
  time_length <- nrow(data) - start_index
  time_period <- seq_len(time_length)
  time_period_result <- lapply(time_period, function(tp) {
    model_data <- extend_rows_model_data(
      data = data, min_model_date_str = start_date_str,
      extension_interval = tp
    )
    print(paste0("Current time period: ", tp, " ", "(", max(model_data$date), ")"))
    cur_model <- fit_epiestim_model(model_data, type = type, ...)
    cur_daily_samples <- extract_daily_samples_epiestim_fit(data = model_data, model_fit = cur_model, n_days = n_days)
    cur_daily_samples <- cur_daily_samples %>%
      rename(daily_date = date, sim = sim, daily_incidence = incidence)

    model_data <- model_data %>%
      dplyr::rename(model_data_date = date)

    if (isTRUE(aggregate_week)) {
      if (isFALSE(n_days %% 7 == 0)) {
        stop("n_days must be a multiple of 7 to aggregate by week")
      }
      cur_samples <- extract_agg_samples_epiestim_fit(cur_daily_samples)
      message("Note: Weekly quantiles were calculated across simulated epicurves")
      cur_samples_agg_quantiles <- cur_samples %>%
        create_quantiles(week_date, variable = "weekly_incidence") %>%
        dplyr::rename(quantile_date = week_date)
      quantile_unit <- "weekly"
      row <- c(cur_model, tp, model_data, cur_samples, cur_samples_agg_quantiles, quantile_unit = quantile_unit)
    } else {
      message("Note: Daily quantiles were calculated across simulated epicurves")
      cur_samples_agg_quantiles <- cur_daily_samples %>%
        create_quantiles(daily_date, variable = "daily_incidence") %>%
        dplyr::rename(quantile_date = daily_date)
      quantile_unit <- "daily"
      row <- c(cur_model, tp, model_data, cur_daily_samples, cur_samples_agg_quantiles, quantile_unit = quantile_unit)
    }

    return(row)
  })
  class(time_period_result) <- c("forecast_time_period_epiestim", class(time_period_result))
  return(time_period_result)
}


#' Plot forecasts at each iteration with uncertainty quantile ranges
#'
#' @param x object of class \code{forecast_time_period_epiestim}
#' @param time_period optional parameter to show only plot at a specific time-point
#' @param ... pass optional parameters to plot method
#' @return Multiple plots with forecasts at each sliding window
#'
#' @export
#' @examples
#' plover_data <- data.frame(
#'   epiWeek_date = as.Date(c(
#'     "2022-10-02", "2022-10-09",
#'     "2022-10-16", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-12-04"
#'   )),
#'   epiWeek_year = c(
#'     2022, 2022, 2022, 2022,
#'     2022, 2022, 2022, 2022, 2022, 2022
#'   ),
#'   Epiweek = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
#'   flu_a = c(17, 19, 32, 38, 43, 45, 73, 88, 94, 105),
#'   flu_b = c(24, 31, 39, 45, 50, 52, 68, 83, 89, 97)
#' )
#'
#' weekly_plover_data <- get_weekly_plover(plover_data)
#'
#' plover_dat_clean <- get_weekly_plover_by_date_type(
#'   weekly_plover_data,
#'   "flu_a",
#'   "2022-10-01",
#'   "2022-12-05"
#' )
#'
#' time_period_result <- forecast_time_period_epiestim(
#'   data = plover_dat_clean,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a"
#' )
#'
#'
#' plot(time_period_result)
plot.forecast_time_period_epiestim <- function(x, time_period = NULL, ...) {
  if (is.null(time_period)) {
    times_plots <- lapply(x, plot_all_time_period_forecast_data_helper)
    times_plots
  } else {
    if (time_period > length(x)) {
      stop("Time period index out of bounds. Please cross-check the time_period input with the length of your time_period_result object")
    }
    one_time_plot <- plot_all_time_period_forecast_data_helper(x[[time_period]])
    one_time_plot
  }
}
