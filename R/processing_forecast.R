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
  confirm <- p50 <- weighted_diff <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
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
  forecast_cases_dat <- combine_df_pred_case(forecast_dat, smoothed_model_data,
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
    dplyr::mutate(weighted_diff = time_weighted_diff(confirm, p50, pred_horizon_str = eval(parse(text = "pred_horizon_str")))) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(`50 percentile interval bounds` = glue::glue("({lower_bound50}-{upper_bound50})")) %>%
    dplyr::mutate(`95 percentile interval bounds` = glue::glue("({lower_bound90}-{upper_bound90})")) %>%
    dplyr::select(date, coverage, weighted_diff, confirm, median.prediction = p50, `50 percentile interval bounds`, `95 percentile interval bounds`) %>%
    dplyr::rename("Confirmed cases" = confirm, "Predicted cases" = median.prediction)
  if ((any(forecast_cases_dat$coverage %in% "Outside 95 percentile interval"))) {
    warning("Prediction percentile intervals do not cover some data-points in validation fits. Some forecasts may not be reliable")
  }
  forecast_cases_dat_summ <- forecast_cases_dat %>%
    dplyr::group_by(coverage) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::mutate(proportion = round(counts / sum(counts) * 100, 2)) %>%
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
  confirm <- p50 <- weighted_diff <- `50 percentile interval bounds` <- `95 percentile interval bounds` <- counts <- median.prediction <- NULL
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
  forecast_cases_dat <- combine_df_pred_case(forecast_dat, smoothed_model_data,
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

#' Plot a violin plot with specific time horizon predictions against true values for validation
#'
#' @param time_period_result object of class \code{forecast_time_period}
#' @param pred_horizon_str *string* prediction horizon time period to plot
#' @param pred_plot either \code{"ribbon"} or \code{"violin"} (by default) to produce either ribbon prediction plots or violin plots respectively
#' @return violin validation plot or ribbon validation plot  for a specific prediction horizon
#'
#' @export
#' @examples
#' \dontrun{
#' plot_validation(daily_time_period_result, pred_horizon_str = "7 days ahead")
#' }
plot_validation <- function(time_period_result, pred_horizon_str = NULL, pred_plot = "violin") {
  p025 <- p975 <- p25 <- p75 <- NULL
  smoothed_confirm <- confirm <- p50 <- point_type <- pred_horizon <- sim_draws <- date <- NULL
  aggregate_unit <- time_period_result[[length(time_period_result)]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation plot")
  }
  if (!inherits(time_period_result, "forecast_time_period")) {
    stop("time_period_result input must be object of class forecast_time_period")
  }


  model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$model_data_date,
    confirm = time_period_result[[length(time_period_result)]]$confirm
  )
  forecast_dat <- pred_interval_forecast(time_period_result,
                                         pred_horizon = pred_horizon_str)

  if (!(pred_plot %in% c("violin", "ribbon"))) {
    stop("Supported plot types are 'violin' and 'ribbon'")
  }

  smoothed_model_data <- data.frame(
    date = time_period_result[[length(time_period_result)]]$smoothed_date,
    confirm = time_period_result[[length(time_period_result)]]$smoothed_confirm
  )

  smoothed_model_data$point_type <- rep("Confirmed Case (Smoothed)", nrow(smoothed_model_data))
  model_data$point_type <- rep("Confirmed Case (Unsmoothed)", nrow(model_data))
  forecast_dat$point_type <- rep("Mean Prediction", nrow(forecast_dat))
  if (pred_plot == "violin") {
    p <- ggplot2::ggplot(forecast_dat, ggplot2::aes(x = date)) +
      # ggplot2::geom_violin(aes(group = weekly_date), scale = "count", colour = "gray",
      #                   fill = "blue", alpha = 0.1, draw_quantiles = 0.5) +
      ggplot2::geom_point(ggplot2::aes(y = mean_sim), colour = "#08519C") +
      ggplot2::geom_errorbar(aes(group = date, ymin = lower_bound90, ymax = upper_bound90), colour = "gray") +
      ggplot2::ggtitle(paste0("Violin plot of ", pred_horizon_str, " prediction (blue)")) +
      ggplot2::geom_line(ggplot2::aes(x = date, y = confirm, colour = point_type), data = smoothed_model_data, linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(x = date, y = confirm, colour = point_type), data = model_data) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = paste0(pred_horizon_str, " mean prediction (blue) of confirmed cases", fill = "", colour = "")) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(), legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::scale_colour_manual(values = c("#FFC0CB", "#471164FF", "#08519C")) +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  } else if (pred_plot == "ribbon") {
    p <- ggplot2::ggplot(forecast_dat, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound90, ymax = upper_bound90), fill = "#08519C", alpha = 0.25) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound50, ymax = lower_bound50), fill = "#08519C", alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = mean_sim), colour = "#08519C") +
      ggplot2::ggtitle(paste0("Ribbon plot of ", pred_horizon_str, " prediction (blue)")) +
      ggplot2::geom_line(ggplot2::aes(x = date, y = confirm, colour = point_type), data = smoothed_model_data, linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(x = date, y = confirm, colour = point_type), data = model_data) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = paste0(pred_horizon_str, "Mean prediction (blue) of confirmed cases", fill = "", colour = "")) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(), legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::scale_colour_manual(values = c("#FFC0CB", "#471164FF", "#08519C")) +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  }
  return(p)
}


#' Calculate 95% prediction interval bounds for smoothed predictions
#' @param time_period_result output from  \code{forecast_time_period}
#' @param pred_horizon_str A *string* indicating the prediction horizon time period to plot
#' @importFrom stats qnorm
#' @return upper and lower bounds for 95% prediction interval for smoothed predictions
pred_interval_forecast <- function(time_period_result, pred_horizon_str = NULL) {
  p50 <- p75 <- p25 <- p025 <- p975 <- sim_draws <- weekly_date <- daily_date <- NULL
  forecast_dat <- create_forecast_df(time_period_result)

  if (!(pred_horizon_str %in% unique(forecast_dat$pred_horizon))) {
    stop("`pred_horizon_str` not in forecast data")
  }

  forecast_dat <- forecast_dat %>%
    dplyr::filter(pred_horizon == pred_horizon_str)
  aggregate_unit <- time_period_result[[1]][["quantile_unit"]]
  dates <- c()
  sd_smooth <- c()
  for (i in seq_along(time_period_result)) {
    sd_smooth[i] <- sqrt(mean(time_period_result[[i]]$smoothed_error))
  }

  smoothing_error_dat <- data.frame(sd_smooth = sd_smooth)
  if (aggregate_unit == "daily") {
    forecast_dat <- forecast_dat %>%
      cbind(smoothing_error_dat) %>%
      dplyr::group_by(daily_date) %>%
      dplyr::summarise(
        p50 = mean(p50), p25 = mean(p25), p75 = mean(p75),
        p025 = mean(p025), p975 = mean(p975),
        mean_sim = mean(sim_draws),
        sd_sim = sd(sim_draws) / sqrt(dplyr::n())
      ) %>%
      dplyr::mutate(
        upper_bound90 = mean_sim + qnorm(1 - 0.025) * (sd_sim + sd_smooth),
        lower_bound90 = mean_sim - qnorm(1 - 0.025) * (sd_sim + sd_smooth),
        upper_bound50 = mean_sim + qnorm(1 - 0.25) * (sd_sim + sd_smooth),
        lower_bound50 = mean_sim - qnorm(1 - 0.25) * (sd_sim + sd_smooth),
        lower_bound90 = ifelse(lower_bound90 < 0, 0, lower_bound90),
        lower_bound50 = ifelse(lower_bound50 < 0, 0, lower_bound50)
      ) %>%
      dplyr::rename(date = daily_date)

  } else if (aggregate_unit == "weekly") {
    forecast_dat <- forecast_dat %>%
      dplyr::left_join(smoothing_error_dat, by = "weekly_date") %>%
      dplyr::group_by(weekly_date) %>%
      dplyr::mutate(mean_sim = mean(sim_draws)) %>%
      dplyr::mutate(upper_bound90 = mean_sim + qnorm(1 - 0.025) * (sd(sim_draws) / sqrt(n()) + sd_smooth)) %>%
      dplyr::mutate(lower_bound90 = mean_sim - qnorm(1 - 0.025) * (sd(sim_draws) / sqrt(n()) + sd_smooth)) %>%
      dplyr::mutate(upper_bound50 = mean_sim + qnorm(1 - 0.25) * (sd(sim_draws) / sqrt(n()) + sd_smooth)) %>%
      dplyr::mutate(lower_bound50 = mean_sim - qnorm(1 - 0.25) * (sd(sim_draws) / sqrt(n()) + sd_smooth)) %>%
      dplyr::mutate(lower_bound90 = ifelse(lower_bound90 < 0, 0, lower_bound90)) %>%
      dplyr::mutate(lower_bound50 = ifelse(lower_bound50 < 0, 0, lower_bound50)) %>%
      dplyr::rename(date = weekly_date)
  } else {
    stop("Unknown `aggregate_unit`")
  }


  return(forecast_dat)
}

#' Extract data-frame with forecasts for validation violin plot
#'
#' @param time_period_result output from  \code{forecast_time_period}
#'
#' @return Data frame with forecasts at all time-points
#' #' \describe{
#'   \item{date}{daily date or weekly date over which samples were aggregated}
#'   \item{p50}{median quantile value}
#'   \item{p25}{quantile value of probability 0.25}
#'   \item{p75}{quantile value of probability 0.75}
#'   \item{p025}{quantile value of probability 0.05}
#'   \item{p975}{quantile value of probability 0.95}
#'   \item{pred_horizon}{Prediction time horizon (time period ahead for which prediction was made)}
#'   \item{value}{prediction values from simulated draws}
#' }
#'
create_forecast_df <- function(time_period_result) {
  results <- lapply(time_period_result, function(i) {
    pred_samples_with_quantile_helper(tp = i, aggregate_unit = time_period_result[[1]][["quantile_unit"]])
  })
  results <- do.call(rbind.data.frame, results)
  return(results)
}

#' Helper function to extract dataframe with week ahead forecasts for Violin plot
#'
#' @param tp element from list output generated by \code{forecast_time_period_epiestim}
#' @param aggregate_unit  Time forecasted samples are aggregated by (weekly or daily)
#'
#'
#' @return Dataframe of forecasts at a single time-point (single element from list input)
#' \describe{
#'   \item{date}{daily date or weekly date over which samples were aggregated}
#'   \item{p50}{median quantile value}
#'   \item{p25}{quantile value of probability 0.25}
#'   \item{p75}{quantile value of probability 0.75}
#'   \item{p025}{quantile value of probability 0.05}
#'   \item{p975}{quantile value of probability 0.95}
#'   \item{pred_horizon}{Prediction time horizon (time period ahead for which prediction was made)}
#'   \item{value}{prediction values from simulated draws}
#' }
#'
pred_samples_with_quantile_helper <- function(tp, aggregate_unit = NULL) {
  value <- quantile_date <- daily_value <- NULL
  cur_samples_with_quantile <- extract_quantile_epiestim(tp)
  seq_time_length <- seq_len(nrow(cur_samples_with_quantile))
  if (aggregate_unit == "weekly") {
    cur_samples_with_quantile$pred_horizon <- paste(seq_time_length, "week ahead")
    cur_samples_with_week_date <- extract_sim_samples_epiestim(tp, aggregate_unit = eval(parse(text = "aggregate_unit")))
    cur_samples_with_quantile <- cur_samples_with_quantile %>%
      dplyr::left_join(cur_samples_with_week_date, by = "quantile_date") %>%
      dplyr::rename(sim_draws = value, weekly_date = quantile_date)
  } else if (aggregate_unit == "daily") {
    cur_samples_with_quantile$pred_horizon <- paste(seq_time_length, "days ahead")
    cur_samples_with_daily_date <- extract_sim_samples_epiestim(tp, aggregate_unit = eval(parse(text = "aggregate_unit")))
    cur_samples_with_quantile <- cur_samples_with_quantile %>%
      dplyr::left_join(cur_samples_with_daily_date, by = "quantile_date") %>%
      dplyr::rename(sim_draws = value, daily_date = quantile_date)
  }

  return(cur_samples_with_quantile)
}


# Extract simulated samples

#' Extract simulated samples ideally 1000 samples per date
#'
#' @param tp element from list output generated by \code{forecast_time_period}
#' @param aggregate_unit Time forecasted samples are aggregated by (weekly or daily)
#'
extract_sim_samples_epiestim <- function(tp, aggregate_unit = NULL) {
  if (aggregate_unit == "weekly") {
    dat_samples <- tibble::tibble(
      quantile_date = tp$week_date,
      value = tp$weekly_incidence
    )
  } else if (aggregate_unit == "daily") {
    dat_samples <- tibble::tibble(
      quantile_date = tp$daily_date,
      value = tp$daily_incidence
    )
  }
  return(dat_samples)
}