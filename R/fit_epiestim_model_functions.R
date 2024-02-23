#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic
#'
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#'
#'
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19. The default serial intervals for the estimation of R were retrieved from
#' Cowling et al., 2011, Vink et al., 2014 and Madewell et al., 2023 for Influenza A, Influenza B, RSV and COVID (BA.5 Omicron variant) respectively
#'
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases)
#' @param dt *Integer* 	length of temporal aggregations of the incidence data. This should be an integer or vector of integers. The default value is 7 time units (1 week).
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "custom"
#' @param mean_si *Numeric* User specification of mean of parametric serial interval
#' @param std_si *Numeric* User specification of standard deviation of parametric serial interval
#' @param recon_opt One of "naive" or "match" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param method One of "non_parametric_si", "parametric_si", "uncertain_si", "si_from_data" or "si_from_sample" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param mean_prior *Numeric* positive number giving the mean of the common prior distribution for all reproduction numbers
#' @param std_prior *Numeric* positive number giving the standard deviation of the common prior distribution for all reproduction numbers
#' @param ... Other optional parameters to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page) to control estimation of reproduction number
#'
#'
#' @return Object of class {\code{\link[EpiEstim]{estimate_R}}} (see \code{EpiEstim} help page)
#' @export
#'
#' @examples
#' fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a")
#'
fit_epiestim_model <- function(data, dt = 7L, type = NULL, mean_si = NULL, std_si = NULL, recon_opt = "match",
                               method = "parametric_si", mean_prior = NULL, std_prior = NULL, ...) {

  confirm <- NULL
  incid <- data$confirm
  if (!is.data.frame(data) || !all(colnames(data) %in% c("date", "confirm"))) {
    stop("Must pass a data frame with two columns: date and confirm")
  }
  if (missing(type) || !(type %in% c("flu_a", "flu_b", "sars_cov2", "rsv", "custom"))) {
    stop("Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)")
  }
  if (type == "custom" && any(is.null(mean_si), is.null(std_si), is.null(mean_prior), is.null(std_prior))) {
    stop("Must specify mean_si, std_si, mean_prior and std_prior for type custom")
  }

  if (type != "custom" && any(!is.null(mean_si), !is.null(std_si), !is.null(mean_prior), !is.null(std_prior))) {
    warning("Custom mean_si, std_s, mean_prior and std_prior can only be specified with type set to custom. Default config values were used")
  }

  data_lag <- as.numeric(difftime(data$date[2], data$date[1]))
  if (data_lag != 7 && dt == 7L) {
    warning("Your data may not be weekly data. Please check input. We recommend only weekly data be input into EpiEstim for optimal performance")
  }

  # 6. Providing default values
  if (is.null(mean_si)) mean_si <- switch(type,
                                          "flu_a" = 3.1,
                                          "flu_b" = 3.7,
                                          "rsv" = 7.5,
                                          "sars_cov2" = 2.75,
                                          "custom" = NULL)
  if (is.null(std_si)) std_si <- switch(type,
                                        "flu_a" = 1.6,
                                        "flu_b" = 2.1,
                                        "rsv" = 2.1,
                                        "sars_cov2" = 2.53,
                                        "custom" = NULL)
  if (is.null(mean_prior)) mean_prior <- switch(type,
                                                "flu_a" = 1,
                                                "flu_b" = 1,
                                                "rsv" = 1,
                                                "sars_cov2" = 2,
                                                "custom" = NULL)
  if (is.null(std_prior)) std_prior <- switch(type,
                                              "flu_a" = 1,
                                              "flu_b" = 1,
                                              "rsv" = 1,
                                              "sars_cov2" = 2,
                                              "custom" = NULL)

  # 7. Configuring based on type
  config <- EpiEstim::make_config(list(
                     mean_si = mean_si,
                     std_si = std_si,
                     mean_prior = mean_prior,
                     std_prior = std_prior
                   ))

  a_prior <- (config$mean_prior / config$std_prior)^2
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)
  if (data$confirm[1] < min_nb_cases_per_time_period) {
    reliable_date_data <- data %>%
      dplyr::filter(confirm >= min_nb_cases_per_time_period)
    incid <- reliable_date_data$confirm
    warning(
      "Incidence is too low on the current start date. R estimation started from ", reliable_date_data$date[1],
      " for an accurate estimate of the reproduction number with EpiEstim"
    )
  }
  epiestim_estimates <- NULL
  epiestim_estimates <- suppressWarnings(EpiEstim::estimate_R(
    incid = incid,
    dt = dt,
    recon_opt = recon_opt,
    method = method,
    config = config, ...
  ))


  return(epiestim_estimates)
}


#' Iterate through a time-period as a sliding window to produce short-term forecasts with the EpiEstim model fit
#'
#'
#' @description Function to produce short-term forecasts from objects of class {\code{\link[EpiEstim]{estimate_R}}}
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param start_date Initial starting time-point. Must match a timepoint in the input dataset
#' @param n_days Number of days to forecast ahead. Defaults to 7
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "other"
#' @param time_period time period string (e.g. 'daily', 'weekly'). Default is daily
#' @param verbose set to true to display progress output
#' @param ... Pass on optional arguments from \code{fit_epiestim_model}
#'
#'
#'
#' @return List of class \code{forecast_time_period}
#' storing quantiles of both daily and weekly forecasts from each sliding window
#' @export
#'
#' @examples
#'
#' #  Daily forecast
#' forecast_time_period_epiestim(
#'   data = weekly_transformed_plover_data,
#'   start_date = "2022-10-02", n_days = 14, type = "flu_a"
#' )
#'
# weekly aggregated forecast
#' forecast_time_period_epiestim(
#'   data = weekly_transformed_plover_data,
#'   start_date = "2022-10-02", n_days = 14, type = "flu_a", time_period = "weekly"
#' )
forecast_time_period_epiestim <- function(data, start_date, n_days = 7, time_period = "daily",
                                          type = NULL, verbose = FALSE, ...) {
  sim <- week_date <- daily_date <- NULL
  if (!(lubridate::ymd(start_date) %in% data$date)) {
    stop("Start date not present in dataset. Please check your input")
  }

  if (lubridate::ymd(start_date) >= max(data$date)) {
    stop("Start date greater than max date in dataset! Please check your input")
  }
  start_index <- which(data$date == lubridate::ymd(start_date))
  time_length <- nrow(data) - start_index
  time_index <- seq_len(time_length)
 time_period_result <- lapply(time_index, function(tp) {
  model_data <- extend_rows_model_data(
  data = data, min_model_date_str = start_date,
  extension_interval = tp
  )
  if (isTRUE(verbose)) {
  print(paste0("Current time period: ", tp, " ", "(", max(model_data$date), ")"))
  }
 cur_model <- fit_epiestim_model(model_data, type = type, ...)
    cur_daily_samples <- extract_daily_samples_epiestim_fit(data = model_data, model_fit = cur_model, n_days = n_days)
    cur_daily_samples <- cur_daily_samples %>%
    rename(daily_date = date, sim = sim, daily_incidence = incidence)

model_data <- model_data %>%
  dplyr::rename(model_data_date = date)


   if (time_period == "weekly") {
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
} else if (time_period == "daily") {
  message("Note: Daily quantiles were calculated across simulated epicurves")
 cur_samples_agg_quantiles <- cur_daily_samples %>%
 create_quantiles(daily_date, variable = "daily_incidence") %>%
 dplyr::rename(quantile_date = daily_date)
 quantile_unit <- "daily"
 row <- c(cur_model, tp, model_data, cur_daily_samples, cur_samples_agg_quantiles, quantile_unit = quantile_unit)
}

   return(row)
 })
}


