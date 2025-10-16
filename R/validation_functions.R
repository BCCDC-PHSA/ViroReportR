#' Iterate through a time-period as a sliding window to produce short-term forecasts
#'
#'
#' @description Function to produce short-term forecasts using either the EpiEstim or EpiFilter algorithm
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param start_date Initial starting time-point. Must match a timepoint in the input dataset
#' @param n_days The number of days to run simulations for. Defaults to 7
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "other"
#' @param time_period time period string (e.g. 'daily', 'weekly'). Default is daily
#' @param algorithm argument to control if model fitting and forecasting is done by `EpiEstim` or `EpiFilter`
#' @param ... optional arguments to control model fitting process from \code{fit_model_epiestim}
#'
#'
#' @return List of class \code{forecast_time_period}
#' storing quantiles of both daily and weekly forecasts from each sliding window
#' @export
#'
#' @examples
#'
#' #  Forecast using EpiEstim
#' forecast_time_period(
#'   data = weekly_transformed_plover_data,
#'   start_date = "2022-10-02", n_days = 14, type = "flu_a", algorithm = "EpiEstim"
#' )
#'
#' #  Forecast using EpiFilter
#' forecast_time_period(
#'   data = weekly_transformed_plover_data,
#'   start_date = "2022-10-02", n_days = 14, type = "flu_a", algorithm = "EpiFilter"
#' )
forecast_time_period <- function(data, start_date, n_days = 7, time_period = "weekly",
                                 type = NULL, algorithm = "EpiEstim", ...) {
  stopifnot(
    "Only EpiFilter and EpiEstim are currently supported as forecasting models. Please check input." =
      algorithm %in% c("EpiEstim", "EpiFilter")
  )
  if (algorithm == "EpiEstim") {
    time_period_result <- forecast_time_period_epiestim(
      data = data, start_date = start_date, n_days = n_days,
      time_period = eval(parse(text = "time_period")), type = eval(parse(text = "type")), ...
    )
  } else if (algorithm == "EpiFilter") {
    stop("EpiFilter is still in the process of being implemented. Please set algorithm to EpiEstim")
  }
  class(time_period_result) <- c("forecast_time_period", class(time_period_result))
  return(time_period_result)
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
forecast_time_period_epiestim <- function(data, start_date, n_days = 7, time_period = "daily",
                                          type = NULL, verbose = FALSE, smoothing_cutoff = 10, ...) {
  data_lag <- as.numeric(difftime(data$date[2], data$date[1]))
  if (data_lag <= 7 && time_period == "weekly") {
    warning("Your data may not be weekly data. Please set time_period = daily for daily data")
  }
  sim <- week_date <- daily_date <- date <- NULL

  check_epiestim_format(data)

  # check and filter on start date
  check_data_contains_start_date(data,start_date)
  data <- data %>%
    dplyr::filter(date > start_date)

  # remove days at the start with zero confirmed cases
  non_zero_dates <- data %>%
    dplyr::filter(confirm > 0) %>%
    pull(date)
  data <- data %>%
    dplyr::filter(date >= non_zero_dates[1])

  # check valid days
  check_min_days(data)

  # get time index after the 14th days
  time_index <- seq(from = 14, to = nrow(data))

  time_period_result <- lapply(time_index, function(tp) {

    # extract model data with extension during each iteration of loop
    model_data <- data %>%
      dplyr::arrange(date) %>%
      dplyr::filter(date <= data$date[tp])

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
#' @details The function fits an EpiEstim model using `fit_epiestim_model()`, extracts daily samples with `generate_forecasts()`,
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
  cur_daily_samples <- generate_forecasts(
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
#' @details The function fits an EpiEstim model using `fit_epiestim_model()`, extracts daily samples with `generate_forecasts()`,
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
  cur_daily_samples <- generate_forecasts(
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
  cur_samples_agg_quantiles <- cur_daily_samples %>%
    create_quantiles(daily_date, variable = "daily_incidence") %>%
    dplyr::rename(quantile_date = daily_date)

  row <- c(cur_model, tp, model_data,
           smoothed_model_data, cur_daily_samples, cur_samples_agg_quantiles,
           quantile_unit = quantile_unit,
           smoothed_error = smoothed_error
  )

  return(row)
}


#' Plot a ribbon plot with each time horizon predictions against true values for validation
#'
#' @param time_period_result object of class \code{forecast_time_period}
#' @param pred_plot either \code{"ribbon"} or \code{"error_bar"} (by default) to produce either ribbon prediction plots or error_bar plots respectively
#' @return error_bar validation plot or ribbon validation plot  for a specific prediction horizon
#'
#' @export
#' @examples
#' \dontrun{
#' plot_validation(daily_time_period_result)
#' }
plot_validation <- function(time_period_result, pred_plot = "ribbon") {

  if (!inherits(time_period_result, "forecast_time_period")) {
    stop("time_period_result input must be object of class forecast_time_period")
  }
  if (!(pred_plot %in% c("error_bar", "ribbon"))) {
    stop("Supported plot types are 'error_bar' and 'ribbon'")
  }

  # combine each time window forecast results
  forecast_dat <- purrr:::map_dfr(time_period_result,
                                  ~ data.frame(date = .x$quantile_date,
                                               p50 = .x$p50,
                                               p10 = .x$p10,
                                               p25 = .x$p25,
                                               p75 = .x$p75,
                                               p90 = .x$p90,
                                               p025 = .x$p025,
                                               p975 = .x$p975,
                                               group_id = as.character(nrow(.x$R))))

  smoothed_model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$smoothed_date,
    confirm = time_period_result[[length(time_period_result)]]$smoothed_confirm
  )

  model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$model_data_date,
    confirm = time_period_result[[length(time_period_result)]]$confirm
  )

  smoothed_model_data$point_type <- rep("Confirmed Case (Smoothed)", nrow(smoothed_model_data))
  model_data$point_type <- rep("Confirmed Case (Unsmoothed)", nrow(model_data))
  forecast_dat$point_type <- rep("Mean Prediction", nrow(forecast_dat))
  blue_grad_20 <- colorRampPalette(c("#08519c","#deebf7"))(20)

  base_plot <- ggplot2::ggplot(
    data = forecast_dat,
    ggplot2::aes(
      x = date, y = p50, group = group_id, color = group_id, fill = group_id
    )
  ) +
    ggplot2::geom_point(data = model_data, aes(x = date, y = confirm), colour = "black", inherit.aes = FALSE) +
    ggplot2::geom_line(data = smoothed_model_data, aes(x = date, y = confirm), colour = "red", linewidth = 1, inherit.aes = FALSE) +    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = paste0("Prediction of confirmed cases", fill = "", colour = "")) +
    ggplot2::ggtitle(paste0(gsub("_"," ",paste0(toupper(substr(pred_plot, 1, 1)), substr(pred_plot, 2, nchar(pred_plot)))), " plot of predictions")) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), legend.position = "None",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (pred_plot == "error_bar") {
    p <- base_plot +
      ggplot2::geom_point(ggplot2::aes(y = p50), size = 2) +
      ggplot2::geom_line(ggplot2::aes(y = p50)) +
      ggplot2::geom_errorbar(ggplot2::aes(group = date, ymin = p10,
                                          ymax = p90)) +
      ggplot2::scale_color_manual(values = blue_grad_20) +
      ggplot2::theme(legend.position = "None")
  } else if (pred_plot == "ribbon") {
    p <- base_plot +
      ggplot2::geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.3, color = NA) +
      ggplot2::geom_line(ggplot2::aes(y = p50), linewidth = 1) +
      ggplot2::scale_color_manual(values = blue_grad_20) +
      ggplot2::scale_fill_manual(values = blue_grad_20)
  }


  return(p)
}


#' Compute Forecast Validation Metrics (SMAPE & MASE)
#'
#' This function calculates two key forecast validation metrics:
#' - **Symmetric Mean Absolute Percentage Error (SMAPE)**: Handles zero values in actuals.
#' - **Mean Absolute Scaled Error (MASE)**: Scales the error relative to a naïve forecast.
#'
#' It also reports the training and forecast periods for each forecast result.
#' The computation stops once the forecast reaches the latest date in the model data.
#'
#' @param validation_res A list of forecast validation results. Each element should contain:
#'   - `quantile_date`: a vector of forecast dates,
#'   - `p50`: a vector of median forecasted values,
#'   - `model_data_date`: a vector of dates used for training the model,
#'   - `confirm`: a vector of actual observed values corresponding to `model_data_date`.
#'
#' @return A `tibble` containing one row per forecast result with the following columns:
#'   - `train_period`: Date range of the training data used for the forecast.
#'   - `forecast_period`: Date range of the forecasted period.
#'   - `smape`: Symmetric Mean Absolute Percentage Error between forecasted and actual values.
#'   - `mase`: Mean Absolute Scaled Error.
#'
#' @details
#' - SMAPE is preferred when actual values may contain zeros, avoiding division by zero.
#' - MASE scales the forecast error relative to the average change in the observed data.
#'
#' @export

forecast_validation_metric <- function(validation_res){

  if (class(validation_res)[1] != "forecast_time_period") {
    stop("input must be validation_res of class forecast_time_period")
  }

  # extract model data
  last_res <- validation_res[[length(validation_res)]]
  model_data <- data.frame(
    date = last_res$model_data_date,
    actual_confirm = last_res$confirm
  )

  # the max date of model data
  max_model_date <- max(model_data$date, na.rm = TRUE)

  metrics <- purrr::map_dfr(validation_res, function(forecast_res) {
    forecast_data <- data.frame(
      date = forecast_res$quantile_date,
      forecast_confirm = forecast_res$p50
    ) %>%
      left_join(model_data, by = "date")

    # SMAPE (symmetric MAPE) since actual values contain zero
    smape = mean(abs(forecast_data$forecast_confirm - forecast_data$actual_confirm) / ((abs(forecast_data$actual_confirm) + abs(forecast_data$forecast_confirm)) / 2))
    mase = round(mean(abs(forecast_data$actual_confirm - forecast_data$forecast_confirm)) / mean(abs(diff(forecast_data$actual_confirm))),2)

    data.frame(
      train_period = sprintf("%s - %s",
                             min(forecast_res$model_data_date, na.rm = TRUE),
                             max(forecast_res$model_data_date, na.rm = TRUE)),
      forecast_period = sprintf("%s - %s",
                                min(forecast_res$quantile_date, na.rm = TRUE),
                                max(forecast_res$quantile_date, na.rm = TRUE)),
      smape = round(smape, 2),
      mase = round(mase, 2),
      last_forecast_date = max(forecast_data$date, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  # stop when reaching the last model date (if applicable)
  cutoff <- which.max(metrics$last_forecast_date == max_model_date)
  if (cutoff > 0) metrics <- metrics[seq_len(cutoff), ]

  # drop helper column
  metrics$last_forecast_date <- NULL

  return(metrics)
}







