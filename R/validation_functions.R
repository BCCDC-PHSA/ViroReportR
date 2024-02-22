#' Iterate through a time-period as a sliding window to produce short-term forecasts
#'
#'
#' @description Function to produce short-term forecasts using either the EpiEstim or EpiFilter algorithm
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param start_date_str Initial starting time-point. Must match a timepoint in the input dataset
#' @param n_days The number of days to run simulations for. Defaults to 7
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "other"
#' @param time_period time period string (e.g. 'daily', 'weekly'). Default is daily
#' @param algorithm argument to control if model fitting and forecasting is done by `EpiEstim` or `EpiFilter`
#'
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
                                          type = NULL, algorithm = "EpiEstim") {
  stopifnot(
    "Only EpiFilter and EpiEstim are currently supported as forecasting models. Please check input." =
      algorithm %in% c("EpiEstim", "EpiFilter")
  )
  if (algorithm == "EpiEstim") {
    time_period_result <- forecast_time_period_epiestim(data = data, start_date = start_date, n_days = n_days,
                                                        time_period = eval(parse(text = "time_period")), type = eval(parse(text = "type")))
  } else if (algorithm == "EpiFilter") {
    time_period_result <- forecast_time_period_epiestim(data = data, start_date = start_date, n_days = n_days,
                                                        time_period = eval(parse(text = "time_period")), type = eval(parse(text = "type")))
  }
  class(time_period_result) <- c("forecast_time_period", class(time_period_result))
  return(time_period_result)
}

#' Plot a violin plot with specific time horizon predictions against true values for validation
#'
#' @param time_period_result object of class \code{forecast_time_period}
#' @param pred_horizon_str *string* prediction horizon time period to plot
#' @param pred_plot either \code{"ribbon"} or \code{"violin"} (by default) to produce either ribbon prediction plots or violin plots respectively
#' @return violin validation plot or ribbon validation plot  for a specific prediction horizon
#'
#' @export
#' @examples
#' plot_validation(weekly_time_period_result, pred_horizon_str = "1 week ahead")
plot_validation <- function(time_period_result, pred_horizon_str = NULL, pred_plot = "violin") {
  p025 <- p975 <- p25 <- p75 <- NULL
  confirm <- p50 <- point_type <- pred_horizon <- sim_draws <- weekly_date <- NULL
  aggregate_unit <- time_period_result[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation plot")
  }
  if (class(time_period_result)[1] != "forecast_time_period") {
    stop("time_period_result input must be object of class forecast_time_period")
  }
  if (aggregate_unit == "daily") {
    stop("Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with time_period = weekly")
  }
  forecast_dat <- create_forecast_df(time_period_result)
  if (!(pred_horizon_str %in% forecast_dat$pred_horizon)) {
    stop("Prediction horizon not found in time_period_result, please check input")
  }
  if (!(pred_plot %in% c("violin", "ribbon"))) {
    stop("Supported plot types are 'violin' and 'ribbon'")
  }
  forecast_dat <- forecast_dat %>%
    dplyr::filter(pred_horizon == pred_horizon_str)
  model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$model_data_date,
    confirm = time_period_result[[length(time_period_result)]]$confirm
  )
  model_data$point_type <- rep("Confirmed Case", nrow(model_data))
  if (pred_plot == "violin") {
    p <- ggplot2::ggplot(forecast_dat, ggplot2::aes(x = factor(weekly_date), y = sim_draws)) +
      ggplot2::geom_violin(scale = "count", colour = "gray", fill = "blue", alpha = 0.1, draw_quantiles = 0.5) +
      ggplot2::ggtitle(paste0("Violin plot of ", pred_horizon_str, " prediction")) +
      ggplot2::geom_point(ggplot2::aes(x = factor(date), y = confirm, colour = point_type), data = model_data) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Time", y = paste0(pred_horizon_str, " projection of confirmed cases", fill = "", colour = "")) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(), legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::scale_colour_manual(values = c("#471164FF"))
  } else if (pred_plot == "ribbon") {
    p <- ggplot2::ggplot(forecast_dat, ggplot2::aes(x = weekly_date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p025, ymax = p975), fill = "#08519C", alpha = 0.25) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75), fill = "#08519C", alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = p50), color = "#08519C") +
      ggplot2::ggtitle(paste0("Ribbon plot of ", pred_horizon_str, " prediction")) +
      ggplot2::geom_point(ggplot2::aes(x = date, y = confirm, colour = point_type), data = model_data) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Time", y = paste0(pred_horizon_str, " projection of confirmed cases", fill = "", colour = "")) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(), legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::scale_colour_manual(values = c("#471164FF")) +
      ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
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
  confirm <- p50 <- weighted_diff <- `50 percentile interval` <- `95 percentile interval` <- NULL
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
  forecast_dat <- create_forecast_df(object)
  forecast_dat <- forecast_dat %>%
    dplyr::filter(pred_horizon == pred_horizon_str)
  if (!(pred_horizon_str %in% forecast_dat$pred_horizon)) {
    stop("Prediction horizon not found in time_period_result, please check input")
  }
  model_data <- data.frame(
    date = object[[length(object)]]$model_data_date,
    confirm = object[[length(object)]]$confirm
  )
  forecast_cases_dat <- combine_df_pred_case(forecast_dat, model_data,
    pred_horizon_str = eval(parse(text = "pred_horizon_str"))
  )
  forecast_cases_dat <- forecast_cases_dat %>%
    dplyr::left_join(model_data, by = c("weekly_date" = "date")) %>%
    dplyr::group_by(weekly_date) %>%
    dplyr::mutate(coverage = dplyr::case_when(
      (p75 < confirm || confirm < p25) && (p025 <= confirm & confirm <= p975)  ~ "only 95 percentile interval",
      p25 <= confirm && confirm <= p75 ~ "50 and 95 percentile interval",
      p975 < confirm || confirm < p025 ~ "Outside 95 percentile interval"
    )) %>%
    dplyr::mutate(weighted_diff = time_weighted_diff(confirm, p50, pred_horizon_str = eval(parse(text = "pred_horizon_str")))) %>%
    dplyr::mutate(`50 percentile interval` = glue::glue("({p25},{p75})")) %>%
    dplyr::mutate(`95 percentile interval` = glue::glue("({p025},{p975})")) %>%
    dplyr::select(weekly_date, coverage, weighted_diff, confirm, median.prediction = p50, `50 percentile interval`, `95 percentile interval`)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_summ <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(count = dplyr::n())
  time_weighted_mspe <- sqrt(mean(forecast_cases_dat$weighted_diff))
  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_summ, time_weighted_mspe = time_weighted_mspe
  ))
}


#' Plot forecasts at each iteration with uncertainty quantile ranges
#'
#' @param x object of class \code{forecast_time_period}
#' @param time_period optional parameter to show only plot at a specific time-point
#' @param ... pass optional parameters to plot method
#' @return Multiple plots with forecasts at each sliding window
#'
#' @export
#' @examples
#' plot(daily_time_period_result)
plot.forecast_time_period <- function(x, time_period = NULL, ...) {
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
