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

  non_zero_dates <- data %>%
    dplyr::filter(confirm > 0) %>%
    pull(date)

  data <- data %>%
    dplyr::filter(date >= non_zero_dates[1])


  start_index <- which(data$date == min(data$date)) + 14
  time_length <- nrow(data) - start_index
  time_index <- seq(from = start_index, to = time_length + 14)

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


  p025 <- p975 <- p25 <- p75 <- NULL
  smoothed_confirm <- confirm <- p50 <- point_type <- pred_horizon <- sim_draws <- date <- NULL
  pred_horizon_str_list <- paste0(seq(1, 7, 1), " days ahead")
  forecast_dat <- NULL
  aggregate_unit <- time_period_result[[length(time_period_result)]][["quantile_unit"]]



  model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$model_data_date,
    confirm = time_period_result[[length(time_period_result)]]$confirm
  )

  for (pred_horizon_str in pred_horizon_str_list) {
    forecast_dat <- rbind(
      forecast_dat,
      pred_interval_forecast(time_period_result,
        pred_horizon = pred_horizon_str
      ) %>%
        dplyr::mutate(pred_horizon_str = pred_horizon_str)
    )
  }

  forecast_dat <- forecast_dat %>%
    dplyr::arrange(date, pred_horizon_str) %>%
    dplyr::mutate(
      ahead_num = as.numeric(sub(" days ahead", "", pred_horizon_str)),
      anchor_date = as.Date(date) - (ahead_num - 1),
      group_id = as.character(factor(anchor_date))
    )

  smoothed_model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$smoothed_date,
    confirm = time_period_result[[length(time_period_result)]]$smoothed_confirm
  )

  smoothed_model_data$point_type <- rep("Confirmed Case (Smoothed)", nrow(smoothed_model_data))
  model_data$point_type <- rep("Confirmed Case (Unsmoothed)", nrow(model_data))
  forecast_dat$point_type <- rep("Mean Prediction", nrow(forecast_dat))
  blue_grad_20 <- colorRampPalette(c("#08519c","#deebf7"))(20)

  base_plot <- ggplot2::ggplot(
    data = forecast_dat,
    ggplot2::aes(
      x = date, y = mean_sim, group = group_id, color = group_id,
      fill = group_id
    )
  ) +
    ggplot2::geom_point(data = model_data, aes(x = date, y = confirm), colour = "black", inherit.aes = FALSE) +
    ggplot2::geom_line(data = smoothed_model_data, aes(x = date, y = confirm), colour = "red", linewidth = 1, inherit.aes = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 500), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = paste0("Prediction of confirmed cases", fill = "", colour = "")) +
    ggplot2::ggtitle(paste0(toupper(substr(pred_plot, 1, 1)), substr(pred_plot, 2, nchar(pred_plot)), " plot of predictions")) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (pred_plot == "error_bar") {
    p <- base_plot +
      ggplot2::geom_point(ggplot2::aes(y = mean_sim), size = 2) +
      ggplot2::geom_line(ggplot2::aes(y = mean_sim)) +
      ggplot2::geom_errorbar(ggplot2::aes(group = date, ymin = lower_bound90,
                                          ymax = upper_bound90)) +
      ggplot2::scale_color_manual(values = blue_grad_20) +
      ggplot2::theme(legend.position = "None")
  } else if (pred_plot == "ribbon") {
    p <- base_plot +
      ggplot2::geom_ribbon(aes(ymin = lower_bound90, ymax = upper_bound90), alpha = 0.3, color = NA) +
      ggplot2::scale_color_manual(values = blue_grad_20) +
      ggplot2::geom_line(size = 1)
  }


  return(p)
}



#' Generate a summary of model validation
#'
#' Outputs a summary indicating the prediction quantile in which the confirmed weekly case from the data falls.
#' The function warns when a confirmed case data-point is not within the predicted 95% confidence interval
#'
#' @param object An object of class \code{forecast_time_period}
#' @param pred_horizon_str A *string* indicating the prediction horizon time period to plot
#' @param ... Additional optional parameters to pass to the summary method
#' @return A list containing two dataframes: \code{individual_quantiles} and \code{quantiles_summary} respectively:
#' \describe{
#'   \item{date}{Daily or weekly date over which samples were aggregated}
#'   \item{data_quantile}{Prediction quantile in which the confirmed case from data is located}
#' }
#' \describe{
#'   \item{data_quantile}{Data quantiles}
#'   \item{count}{Number of data points in one of the prediction quantiles (Q1, Q2, Q3, Q4)}
#' }
#' @export

#' @examples
#' summary(weekly_time_period_result, pred_horizon_str = "1 week ahead")
summary.forecast_time_period <- function(object, pred_horizon_str = NULL, ...) {
  confirm <- p50 <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
  pred_horizon <- weekly_date <- coverage <- NULL
  if (class(object)[1] != "forecast_time_period") {
    stop("input must be object of class forecast_time_period")
  }
  aggregate_unit <- object[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation summary")
  }
  model_data <- data.frame(
    date = object[[length(object)]]$model_data_date,
    confirm = object[[length(object)]]$confirm
  )
  forecast_dat <- pred_interval_forecast(
    time_period_result = object,
    pred_horizon = pred_horizon_str
  )

  smoothed_model_data <- data.frame(
    date = object[[length(object)]]$smoothed_date,
    confirm = object[[length(object)]]$smoothed_confirm
  )
  forecast_cases_dat <- combine_df_pred_case(forecast_dat,
    pred_horizon_str = pred_horizon_str
  )
  forecast_cases_dat <- forecast_cases_dat %>%
    dplyr::left_join(model_data, by = "date") %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(coverage = dplyr::case_when(
      upper_bound90 < confirm || confirm < lower_bound90 ~ "Outside 95 percentile interval",
      lower_bound50 <= confirm && confirm <= upper_bound50 ~ "50 percentile interval",
      lower_bound90 <= confirm && confirm <= upper_bound90 ~ "95 percentile interval",
    )) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(`50 percentile interval bounds` = glue::glue("({lower_bound50}-{upper_bound50})")) %>%
    dplyr::mutate(`95 percentile interval bounds` = glue::glue("({lower_bound90}-{upper_bound90})")) %>%
    dplyr::select(date, coverage, confirm, median.prediction = p50, `50 percentile interval bounds`, `95 percentile interval bounds`) %>%
    dplyr::rename("Confirmed cases" = confirm, "Predicted cases" = median.prediction)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_sum <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::mutate(proportion = round(counts / sum(counts) * 100, 2)) %>%
    dplyr::mutate(proportion = ifelse(coverage == "95 percentile interval",
      sum(proportion[coverage == "50 percentile interval"]) + proportion,
      proportion
    ))
  forecast_cases_dat_sum$coverage <- factor(forecast_cases_dat_sum$coverage,
    levels = c(
      "50 percentile interval",
      "95 percentile interval",
      "Outside 95 percentile interval"
    )
  )

  forecast_cases_dat_sum <- forecast_cases_dat_sum[order(forecast_cases_dat_sum$coverage), ]

  forecast_cases_dat <- forecast_cases_dat
  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_sum
  ))
}


#' Plot current forecast at last chaining iteration with uncertainty quantile ranges
#'
#' @param x object of class \code{forecast_time_period}
#' @param ... pass optional parameters to plot method
#' @return Multiple plots with forecasts at each sliding window
#'
#' @export
#' @examples
#' plot(daily_time_period_result)
plot.forecast_time_period <- function(x, ...) {
  forecast_plot <- plot_all_time_period_forecast_data_helper(x[[length(x)]])
  return(forecast_plot)
}



#' Generate a summary of model validation by season
#'
#' Outputs a summary indicating the prediction quantile in which the confirmed weekly case from the data falls.
#' The function warns when a confirmed case data-point is not within the predicted 95% confidence interval
#'
#' @param object An object of class \code{forecast_time_period}
#' @param pred_horizon_str A *string* indicating the prediction horizon time period to plot
#' @param year_input *numeric* indicating the year to generate the validation summary
#' @param ... Additional optional parameters to pass to the summary method
#' @return A list containing two dataframes: \code{individual_quantiles} and \code{quantiles_summary} respectively:
#' \describe{
#'   \item{date}{Daily or weekly date over which samples were aggregated}
#'   \item{data_quantile}{Prediction quantile in which the confirmed case from data is located}
#' }
#' \describe{
#'   \item{data_quantile}{Data quantiles}
#'   \item{count}{Number of data points in one of the prediction quantiles (Q1, Q2, Q3, Q4)}
#' }
#' @export

#' @examples
#' summary(weekly_time_period_result, pred_horizon_str = "1 week ahead")
summary_season.forecast_time_period <- function(object, pred_horizon_str = NULL, year_input = NULL, ...) {
  confirm <- p50 <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
  pred_horizon <- weekly_date <- coverage <- NULL
  if (class(object)[1] != "forecast_time_period") {
    stop("input must be object of class forecast_time_period")
  }
  aggregate_unit <- object[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation summary")
  }
  if (aggregate_unit == "daily") {
    stop("Only weekly aggregated data suitable for validation summary. Please re-run forecast_time_period_epiestim with time_period = weekly")
  }
  model_data <- data.frame(
    date = object[[length(object)]]$model_data_date,
    confirm = object[[length(object)]]$confirm
  )
  forecast_dat <- pred_interval_forecast(time_period_result = object, pred_horizon = pred_horizon_str)
  forecast_dat <- forecast_dat %>%
    dplyr::filter(pred_horizon == pred_horizon_str)
  if (!(pred_horizon_str %in% forecast_dat$pred_horizon)) {
    stop("Prediction horizon not found in time_period_result, please check input")
  }
  smoothed_model_data <- data.frame(
    date = object[[length(object)]]$smoothed_date,
    confirm = object[[length(object)]]$smoothed_confirm
  )
  forecast_cases_dat <- combine_df_pred_case(forecast_dat,
    pred_horizon_str = eval(parse(text = "pred_horizon_str"))
  )
  forecast_cases_dat <- forecast_cases_dat %>%
    dplyr::mutate(year = lubridate::year(weekly_date)) %>%
    dplyr::filter(year == year_input)
  forecast_cases_dat <- forecast_cases_dat %>%
    dplyr::left_join(model_data, by = c("weekly_date" = "date")) %>%
    dplyr::group_by(weekly_date) %>%
    dplyr::mutate(coverage = dplyr::case_when(
      upper_bound90 < confirm || confirm < lower_bound90 ~ "Outside 95 percentile interval",
      lower_bound50 <= confirm && confirm <= upper_bound50 ~ "50 percentile interval",
      lower_bound90 <= confirm && confirm <= upper_bound90 ~ "95 percentile interval",
    )) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(`50 percentile interval bounds` = glue::glue("({lower_bound50}-{upper_bound50})")) %>%
    dplyr::mutate(`95 percentile interval bounds` = glue::glue("({lower_bound90}-{upper_bound90})")) %>%
    dplyr::select(weekly_date, coverage, confirm, median.prediction = p50, `50 percentile interval bounds`, `95 percentile interval bounds`) %>%
    dplyr::rename("Confirmed cases" = confirm, "Predicted cases" = median.prediction)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_sum <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::mutate(proportion = round(counts / sum(counts) * 100, 2)) %>%
    dplyr::mutate(proportion = ifelse(coverage == "95 percentile interval",
      sum(proportion[coverage == "50 percentile interval"]) + proportion,
      proportion
    ))
  forecast_cases_dat_sum$coverage <- factor(forecast_cases_dat_sum$coverage,
    levels = c(
      "50 percentile interval",
      "95 percentile interval",
      "Outside 95 percentile interval"
    )
  )

  forecast_cases_dat_sum <- forecast_cases_dat_sum[order(forecast_cases_dat_sum$coverage), ]

  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_sum
  ))
}



#' Generate a summary of model validation
#'
#' Outputs a summary indicating the prediction quantile in which the confirmed weekly case from the data falls.
#' The function warns when a confirmed case data-point is not within the predicted 95% confidence interval
#'
#' @param object An object of class \code{forecast_time_period}
#' @param pred_horizon_str A *string* indicating the prediction horizon time period to plot
#' @param ... Additional optional parameters to pass to the summary method
#' @return A list containing two dataframes: \code{individual_quantiles} and \code{quantiles_summary} respectively:
#' \describe{
#'   \item{date}{Daily or weekly date over which samples were aggregated}
#'   \item{data_quantile}{Prediction quantile in which the confirmed case from data is located}
#' }
#' \describe{
#'   \item{data_quantile}{Data quantiles}
#'   \item{count}{Number of data points in one of the prediction quantiles (Q1, Q2, Q3, Q4)}
#' }
#' @export

#' @examples
#' summary(weekly_time_period_result, pred_horizon_str = "1 week ahead")
summary_daily.forecast_time_period <- function(object, ...) {
  confirm <- p50 <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
  pred_horizon <- daily_date <- coverage <- NULL
  if (class(object)[1] != "forecast_time_period") {
    stop("input must be object of class forecast_time_period")
  }
  aggregate_unit <- object[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation summary")
  }
  model_data <- data.frame(
    date = object[[length(object)]]$model_data_date,
    confirm = object[[length(object)]]$confirm
  )
  forecast_dat <- create_forecast_df(time_period_result = object)
  forecast_dat <- forecast_dat %>%
    dplyr::group_by(daily_date) %>%
    dplyr::summarise(
      p50 = mean(p50), p25 = mean(p25), p75 = mean(p75),
      p025 = mean(p025), p975 = mean(p975), value = mean(value)
    )

  forecast_cases_dat <- forecast_dat %>%
    dplyr::left_join(model_data, by = c("daily_date" = "date")) %>%
    dplyr::group_by(daily_date) %>%
    dplyr::mutate(coverage = dplyr::case_when(
      agg_p975 < confirm || confirm < agg_p025 ~ "Outside 95 percentile interval",
      agg_p25 <= confirm && confirm <= agg_p75 ~ "50 percentile interval",
      agg_p025 <= confirm && confirm <= agg_p975 ~ "95 percentile interval",
    )) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(`50 percentile interval bounds` = glue::glue("({lower_bound50}-{upper_bound50})")) %>%
    dplyr::mutate(`95 percentile interval bounds` = glue::glue("({lower_bound90}-{upper_bound90})")) %>%
    dplyr::select(weekly_date, coverage, confirm, median.prediction = p50, `50 percentile interval bounds`, `95 percentile interval bounds`) %>%
    dplyr::rename("Confirmed cases" = confirm, "Predicted cases" = median.prediction)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_sum <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::mutate(proportion = round(counts / sum(counts) * 100, 2)) %>%
    tidyr::drop_na(coverage) %>%
    dplyr::mutate(proportion = ifelse(coverage == "95 percentile interval",
      sum(proportion[coverage == "50 percentile interval"]) + proportion,
      proportion
    ))
  forecast_cases_dat_sum$coverage <- factor(forecast_cases_dat_sum$coverage,
    levels = c(
      "50 percentile interval",
      "95 percentile interval",
      "Outside 95 percentile interval"
    )
  )

  forecast_cases_dat_sum <- forecast_cases_dat_sum[order(forecast_cases_dat_sum$coverage), ]

  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_sum
  ))
}
