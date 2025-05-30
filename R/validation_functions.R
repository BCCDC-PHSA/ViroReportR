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
  confirm <- p50 <- weighted_diff <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
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
  # forecast_dat <- forecast_dat %>%
  # dplyr::filter(pred_horizon == pred_horizon_str)
  # if (!(pred_horizon_str %in% forecast_dat$pred_horizon)) {
  #  stop("Prediction horizon not found in time_period_result, please check input")
  # }
  # smoothed_model_data <- data.frame(
  #   date = object[[length(object)]]$smoothed_date,
  #   confirm = object[[length(object)]]$smoothed_confirm
  # )
  # forecast_cases_dat <- combine_df_pred_case(forecast_dat, smoothed_model_data,
  #                                            pred_horizon_str = eval(parse(text = "pred_horizon_str"))
  # )
  forecast_cases_dat <- forecast_dat %>%
    dplyr::left_join(model_data, by = c("daily_date" = "date")) %>%
    dplyr::group_by(daily_date) %>%
    dplyr::mutate(coverage = dplyr::case_when(
      agg_p975 < confirm || confirm < agg_p025 ~ "Outside 95 percentile interval",
      agg_p25 <= confirm && confirm <= agg_p75 ~ "50 percentile interval",
      agg_p025 <= confirm && confirm <= agg_p975 ~ "95 percentile interval",
    )) %>%
    dplyr::mutate(weighted_diff = time_weighted_diff(confirm, p50, pred_horizon_str = eval(parse(text = "pred_horizon_str")))) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(`50 percentile interval bounds` = glue::glue("({lower_bound50}-{upper_bound50})")) %>%
    dplyr::mutate(`95 percentile interval bounds` = glue::glue("({lower_bound90}-{upper_bound90})")) %>%
    dplyr::select(weekly_date, coverage, weighted_diff, confirm, median.prediction = p50, `50 percentile interval bounds`, `95 percentile interval bounds`) %>%
    dplyr::rename("Confirmed cases" = confirm, "Predicted cases" = median.prediction)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_summ <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::mutate(proportion = round(counts / sum(counts) * 100, 2)) %>%
    tidyr::drop_na(coverage) %>%
    dplyr::mutate(proportion = ifelse(coverage == "95 percentile interval",
      sum(proportion[coverage == "50 percentile interval"]) + proportion,
      proportion
    ))
  forecast_cases_dat_summ$coverage <- factor(forecast_cases_dat_summ$coverage,
    levels = c(
      "50 percentile interval",
      "95 percentile interval",
      "Outside 95 percentile interval"
    )
  )

  forecast_cases_dat_summ <- forecast_cases_dat_summ[order(forecast_cases_dat_summ$coverage), ]
  time_weighted_mspe <- sqrt(mean(forecast_cases_dat$weighted_diff))

  forecast_cases_dat <- forecast_cases_dat %>%
    select(-weighted_diff)
  return(list(
    individual_quantiles = forecast_cases_dat,
    quantile_summary = forecast_cases_dat_summ, time_weighted_mspe = round(time_weighted_mspe, 3)
  ))
}
