#' Plot a violin plot with specific time horizon predictions against true values for validation
#'
#' @param time_period_result object of class \code{forecast_time_period_epiestim}
#' @param pred_horizon_str *string* prediction horizon time period to plot
#' @param pred_plot either \code{"ribbon"} or \code{"violin"} (by default) to produce either ribbon prediction plots or violin plots respectively
#' @return violin validation plot or ribbon validation plot  for a specific prediction horizon
#'
#' @export
#' @examples
#' plotValidation(weekly_time_period_result, pred_horizon_str = "1 week ahead")
plotValidation <- function(time_period_result, pred_horizon_str = NULL, pred_plot = "violin") {
  p05 <- p95 <- p25 <- p75 <- NULL
  confirm <- p50 <- point_type <- pred_horizon <- sim_draws <- weekly_date <- NULL
  aggregate_unit <- time_period_result[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation plot")
  }
  if (class(time_period_result)[1] != "forecast_time_period_epiestim") {
    stop("time_period_result input must be object of class forecast_time_period_epiestim")
  }
  if (aggregate_unit == "daily") {
    stop("Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE")
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
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p05, ymax = p95), fill = "#08519C", alpha = 0.25) +
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
#' The function warns when a confirmed case data-point is not within the predicted quantiles.
#'
#' @param object An object of class \code{forecast_time_period_epiestim}
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
summary.forecast_time_period_epiestim <- function(object, pred_horizon_str = NULL, ...) {
  pred_horizon <- weekly_date <- data_quantile <- NULL
  if (class(object)[1] != "forecast_time_period_epiestim") {
    stop("input must be object of class forecast_time_period_epiestim")
  }
  aggregate_unit <- object[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation summary")
  }
  if (aggregate_unit == "daily") {
    stop("Only weekly aggregated data suitable for validation summary. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE")
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
    dplyr::mutate(data_quantile = dplyr::case_when(
      min_sim < confirm & confirm < p25 ~ "Q1",
      p25 < confirm & confirm < p50 ~ "Q2",
      p50 < confirm & confirm < p75 ~ "Q3",
      p75 < confirm & confirm < max_sim ~ "Q4",
      min_sim > confirm | confirm > max_sim ~ "Outside prediction quantiles"
    )) %>%
    dplyr::select(weekly_date, data_quantile)
  if ((any(forecast_cases_dat$data_quantile %in% "Outside prediction quantiles"))) {
    warning("Prediction quantiles do not cover some data-points. Some forecasts may not be reliable")
  }
  forecast_cases_dat_summ <- forecast_cases_dat %>%
    dplyr::group_by(data_quantile) %>%
    dplyr::summarise(count = dplyr::n())
  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_summ
  ))
}
