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
                               method = "parametric_si", mean_prior = NULL, std_prior = NULL, iter = 20L, ...) {
  confirm <- NULL
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
  if (data_lag <= 7 && dt == 7L) {
    warning("Your data may not be weekly data. Please set dt to 1L for daily data")
  }

  # 6. Providing default values
  if (is.null(mean_si)) {
    mean_si <- switch(type,
      "flu_a" = 3.1,
      "flu_b" = 3.7,
      "rsv" = 7.5,
      "sars_cov2" = 2.75,
      "custom" = NULL
    )
  }
  if (is.null(std_si)) {
    std_si <- switch(type,
      "flu_a" = 2.1,
      "flu_b" = 2.1,
      "rsv" = 2.1,
      "sars_cov2" = 2.53,
      "custom" = NULL
    )
  }
  if (is.null(mean_prior)) {
    mean_prior <- switch(type,
      "flu_a" = 1,
      "flu_b" = 1,
      "rsv" = 1,
      "sars_cov2" = 2,
      "custom" = NULL
    )
  }
  if (is.null(std_prior)) {
    std_prior <- switch(type,
      "flu_a" = 1,
      "flu_b" = 1,
      "rsv" = 1,
      "sars_cov2" = 1,
      "custom" = NULL
    )
  }

  # 7. Configuring based on type
  if (dt == 1L) {
    incid <- data.frame(I = data$confirm, dates = data$date)
    incid <- incid %>%
      dplyr::arrange(dates)
    t_start <- seq(2, nrow(incid))
    t_end <- t_start
    config <- EpiEstim::make_config(list(
      mean_si = mean_si,
      std_si = std_si,
      mean_prior = mean_prior,
      std_prior = std_prior,
      t_start = t_start,
      t_end = t_end
    ))
  } else if (dt > 1L) {
    incid <- data$confirm
    config <- EpiEstim::make_config(list(
      mean_si = mean_si,
      std_si = std_si,
      mean_prior = mean_prior,
      std_prior = std_prior
    ))
  }

  epiestim_estimates <- NULL
  epiestim_estimates <- suppressWarnings(EpiEstim::estimate_R(
    incid = incid,
    dt = dt,
    recon_opt = recon_opt,
    method = method,
    config = config, iter = iter, ...
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
#' @param smoothing_cutoff number of time periods windows after to start smoothing
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
forecast_time_period_epiestim <- function(data, start_date, n_days = 7, time_period = "weekly",
                                          type = NULL, verbose = FALSE, smoothing_cutoff = 10, ...) {
  data_lag <- as.numeric(difftime(data$date[2], data$date[1]))
  if (data_lag <= 7 && time_period == "weekly") {
    warning("Your data may not be weekly data. Please set time_period = daily for daily data")
  }
  sim <- week_date <- daily_date <- NULL


  model_non_zero_data <- data %>%
    dplyr::filter(confirm > 0) %>%
    pull(date)

  data <- data %>%
    dplyr::filter(date >= model_non_zero_data[1])


  start_index <- which(data$date == min(data$date))
  time_length <- nrow(data) - start_index
  time_index <- seq(from = start_index, to = time_length)

  time_period_result <- lapply(time_index, function(tp) {
    model_data <- extend_rows_model_data(
      data = data, min_model_date_str = min(data$date),
      extension_interval = tp
    )


    if (verbose) {
      message(paste0("Current time period: ", tp, " ", "(", max(model_data$date), ")"))
    }



    smoothed_output <- smooth_model_data(model_data, smoothing_cutoff = smoothing_cutoff)

    if (time_period == "weekly") {
      row <- calculate_weekly_fit_row(
        smoothed_output,
        tp,
        type = type, n_days = n_days, ...
      )
    } else if (time_period == "daily") {
      row <- calculate_daily_fit_row(
        smoothed_output,
        tp,
        type = type, n_days = n_days, ...
      )
    }

    return(row)
  })
  return(time_period_result)
}


#' Smooth Model Data Using P-Spline GAM
#'
#' Applies P-spline smoothing using a Generalized Additive Model (GAM) to the input model data.
#' If the number of rows in `model_data` is greater than or equal to `smoothing_cutoff`,
#' the function fits a GAM, estimates confidence intervals, and computes uncertainty.
#'
#' @param model_data A data frame containing the model data, including a column `confirm` representing observed values.
#' @param smoothing_cutoff minimum number of rows required to smooth
#' @param n_reps Number of replicates to calculate error
#'
#' @return A list with two elements:
#'  - `original_data`: A copy of `model_data`
#' 	- `data`: A data frame with smoothed `confirm` values.
#' 	- `error`: Estimated uncertainty of the smoothing process.
#'
#' @importFrom mgcv gam predict vcov
#' @importFrom stats coef qnorm rnorm quantile
#' @importFrom dplyr mutate
#' @noRd
smooth_model_data <- function(model_data, smoothing_cutoff = 10, n_reps = 10000) {
  ##### Add in P-spline smoothing with GAM at each time-step ###################
  smoothed_model_data <- model_data
  if (nrow(model_data) >= smoothing_cutoff) {
    index <- seq_len(nrow(model_data))
    model_data$index <- index
    model_smooth <- mgcv::gam(confirm ~ s(index, bs = "ps", k = round(length(index) / 2, 0)), data = model_data)
    beta <- coef(model_smooth)
    Vb <- vcov(model_smooth)
    Cv <- chol(Vb)

    nb <- length(beta)
    br <- t(Cv) %*% matrix(rnorm(n_reps * nb), nb, n_reps) + beta
    xp <- seq(0, 1, length.out = length(index))
    Xp <- suppressWarnings(predict(model_smooth, newdata = data.frame(index = index), type = "lpmatrix"))
    lp <- Xp %*% br
    fv <- lp
    yr <- matrix(rnorm(nrow(fv) * ncol(fv), mean = fv, sd = model_smooth$sig2), nrow = nrow(fv), ncol = ncol(fv))
    conf_int <- apply(yr, 1, quantile, prob = c(0.025, 0.975))
    diff <- conf_int[2, ] - conf_int[1, ]
    uncertainity_se <- diff / (qnorm(1 - 0.05 / 2) * 2)
    smoothed_estimates <- predict(model_smooth, type = "response", se.fit = TRUE)
    smoothed_model_data$confirm <- round(smoothed_estimates$fit, 0)
    smoothed_model_data <- smoothed_model_data %>%
      mutate(confirm = ifelse(confirm < 0, 0, confirm))
    smoothed_error <- data.frame(smoothed_error = smoothed_estimates$se.fit + uncertainity_se)
  } else {
    smoothed_error <- 0
  }

  return(list(original_data = model_data, data = smoothed_model_data, error = smoothed_error))
}

#' Calculate Weekly Fit Row from Smoothed Output
#'
#' Processes smoothed model data to fit an EpiEstim model, extract daily samples, aggregate them weekly,
#' and return a structured output containing relevant model and quantile information.
#'
#' @param smoothed_output A list containing:
#' 	- `data`: A data frame with smoothed model data, including a `date` and `confirm` column.
#' 	- `error`: The estimated smoothing error.
#' @param tp time period
#' @param type type of disease
#' @param n_days Number of days to forecast ahead. Defaults to 7
#' @param ... Additional arguments passed to `fit_epiestim_model()`.
#'
#' @return A named list containing:
#' 	- Fitted model results.
#' 	- Time period information.
#' 	- Original, smoothed, and aggregated model data.
#' 	- Weekly quantile estimates.
#' 	- Smoothed error values.
#'
#' @details The function fits an EpiEstim model using `fit_epiestim_model()`, extracts daily samples with `extract_daily_samples_epiestim_fit()`,
#' and renames key columns for consistency. It ensures `n_days` is a multiple of 7 before aggregating data to weekly intervals.
#'
#' @importFrom dplyr rename
#' @noRd
calculate_weekly_fit_row <- function(smoothed_output, tp, type = "sars_cov2",
                                     n_days = 7, ...) {
  smoothed_model_data <- smoothed_output$data
  smoothed_error <- smoothed_output$error
  quantile_unit <- "weekly"

  cur_model <- fit_epiestim_model(data = smoothed_model_data, type = type, ...)
  cur_daily_samples <- extract_daily_samples_epiestim_fit(
    data = smoothed_model_data,
    model_fit = cur_model,
    n_days = n_days
  )
  cur_daily_samples <- cur_daily_samples %>%
    dplyr::rename(daily_date = date, sim = sim, daily_incidence = incidence)

  smoothed_model_data <- smoothed_model_data %>%
    dplyr::rename(smoothed_date = date, smoothed_confirm = confirm)

  model_data <- smoothed_output$original_data %>%
    dplyr::rename(model_data_date = date)
  if (!(n_days %% 7 == 0)) {
    stop("n_days must be a multiple of 7 to aggregate by week")
  }
  cur_samples <- extract_agg_samples_epiestim_fit(cur_daily_samples)
  cur_samples_agg_quantiles <- cur_samples %>%
    create_quantiles(week_date, variable = "weekly_incidence") %>%
    dplyr::rename(quantile_date = week_date)

  row <- c(cur_model, tp, model_data, smoothed_model_data, cur_samples, cur_samples_agg_quantiles,
    quantile_unit = quantile_unit,
    smoothed_error
  )

  return(row)
}

#' Calculate Daily Fit Row from Smoothed Output
#'
#' Processes smoothed model data to fit an EpiEstim model, extract daily samples,
#' and return a structured output containing relevant model and quantile information.
#'
#' @param smoothed_output A list containing:
#' 	- `data`: A data frame with smoothed model data, including a `date` and `confirm` column.
#' 	- `error`: The estimated smoothing error.
#' @param tp time period
#' @param type type of disease
#' @param n_days Number of days to forecast ahead. Defaults to 7
#' @param ... Additional arguments passed to `fit_epiestim_model()`.
#'
#' @return A named list containing:
#' 	- Fitted model results.
#' 	- Time period information.
#' 	- Original, smoothed, and daily model data.
#' 	- Daily quantile estimates.
#' 	- Smoothed error values.
#'
#' @details The function fits an EpiEstim model using `fit_epiestim_model()`, extracts daily samples with `extract_daily_samples_epiestim_fit()`,
#' and renames key columns for consistency. It also generates daily quantile estimates using `create_quantiles()`.
#'
#' @importFrom dplyr rename
#' @noRd
calculate_daily_fit_row <- function(smoothed_output, tp, type = "sars_cov2",
                                    n_days = 7, ...) {
  smoothed_model_data <- smoothed_output$data
  smoothed_error <- smoothed_output$error
  quantile_unit <- "daily"

  cur_model <- fit_epiestim_model(data = smoothed_model_data, type = type, dt = 1L, ...)
  cur_daily_samples <- extract_daily_samples_epiestim_fit(
    data = smoothed_model_data,
    dt = 1L,
    model_fit = cur_model,
    n_days = n_days
  )
  cur_daily_samples <- cur_daily_samples %>%
    dplyr::rename(daily_date = date, sim = sim, daily_incidence = incidence)

  smoothed_model_data <- smoothed_model_data %>%
    dplyr::rename(smoothed_date = date, smoothed_confirm = confirm)

  model_data <- smoothed_output$original_data %>%
    dplyr::rename(model_data_date = date)
  cur_samples_agg_quantiles <- cur_daily_samples %>%
    create_quantiles(daily_date, variable = "daily_incidence") %>%
    dplyr::rename(quantile_date = daily_date)

  row <- c(cur_model, tp, model_data,
    smoothed_model_data, cur_daily_samples, cur_samples_agg_quantiles,
    quantile_unit = quantile_unit,
    smoothed_error
  )

  return(row)
}
