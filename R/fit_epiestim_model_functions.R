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

