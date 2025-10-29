#' Validate forecast performance over multiple time windows
#'
#' This function performs rolling validation of short-term forecasts generated
#' by **EpiEstim** or similar models. It divides the input time series into
#' overlapping validation windows and repeatedly runs forecasts to assess model
#' performance across different time segments.
#'
#' @param data A data frame containing at least the columns `"date"` and
#'   `"confirm"`. The `"date"` column should be of class `Date`, and
#'   `"confirm"` should be numeric.
#' @param start_date A `Date` (or date-convertible string) specifying the
#'   starting point for validation Must exist in the `"date"` column.
#' @param validate_window_size Integer. The number of days between each
#'   validation window (default: `7`).
#' @param window_size Integer. The sliding window size (in days) used by the
#'   forecasting model (default: `7`).
#' @param n_days Integer. The number of future days to forecast in each
#'   validation iteration (default: `7`).
#' @param type *character*
#'   Type of epidemic. Must be one of `"flu_a"`, `"flu_b"`, `"rsv"`,
#'   `"sars_cov2"`, or `"custom"`. Passed to
#'   \code{\link{fit_epiestim_model}}.
#' @param smooth_data Logical. Whether to smooth the input case counts prior
#'   to forecasting (default: `FALSE`).
#' @param smoothing_cutoff Numeric. Threshold used for smoothing when
#'   `smooth_data = TRUE` (default: `10`).
#' @param ... Additional arguments passed to `generate_forecast()`.
#'
#' @return A list of forecast results, each element corresponding to one
#'   validation window. Each element contains the output returned by
#'   `generate_forecast()` for that particular window.
#'
#' @details
#' The validation procedure ensures that forecasts are evaluated under realistic
#' temporal conditions. Starting from the earliest date, the function repeatedly:
#'
#' 1. Takes a growing subset of data up to the current validation endpoint.
#' 2. Runs the forecast using `generate_forecast()`.
#' 3. Moves the validation window forward by `validate_window_size` days.
#'
#' This results in a set of forecasts that can be compared to observed data to
#' evaluate predictive performance across time.
#'
#' @seealso [clean_sample_data()], [generate_forecast()]
#' @export

generate_validation <- function(
    data,
    start_date,
    validate_window_size = 7,
    window_size = 7,
    n_days = 7,
    type = NULL,
    smooth_data = FALSE,
    smoothing_cutoff = 10,
    ...
) {
  # clean and validate input
  data <- clean_sample_data(data, start_date)
  n <- nrow(data)

  if (n - n_days < 14) {
    stop(glue::glue("Not enough data for number of days to forecast in validation {n_days}"))
  }

  if (n - validate_window_size < 14) {
    stop(glue::glue("Not enough data for validate window size {validate_window_size}"))
  }

  # define validation indices
  end_indx <- seq(14, n - n_days, by = validate_window_size)

  # run forecast on each validation window
  validation_res <- lapply(end_indx, function(i) {
    generate_forecast(
      data = data[1:i,],
      start_date = data$date[1],
      window_size = window_size,
      n_days = n_days,
      type = type,
      smooth_data = smooth_data,
      smoothing_cutoff = smoothing_cutoff,
      ...
    )
  })

  return(validation_res)
}

#' Compute Forecast Validation Metrics (SMAPE & MASE)
#'
#' This function evaluates forecast accuracy across multiple validation runs
#' by computing two key performance metrics:
#'
#' - **Symmetric Mean Absolute Percentage Error (SMAPE)**: Measures relative
#'   forecast accuracy while remaining robust to zero values in the actual data.
#' - **Mean Absolute Scaled Error (MASE)**: Scales forecast errors relative
#'   to the in-sample one-step naïve forecast, allowing comparison across
#'   series with different scales.
#'
#' For each forecast result, the function also reports the corresponding
#' training and forecast periods. Computation stops once the forecast period
#' reaches the maximum date in the model data.
#'
#' @param data A data frame used in [`generate_validation()`], containing the
#'   **original training data** for the model. It must include:
#'   - `date`: Dates of the observed case data (class `Date`).
#'   - `confirm`: Numeric values of observed confirmed cases.
#'
#' @param validation_res A list of forecast validation results, typically the
#'   output from [`generate_validation()`]. Each element should contain:
#'   - `forecast_res_quantiles`: A data frame with columns `date` and `p50`
#'     (median forecasted values).
#'   - `original_data`: A data frame representing the training data used for
#'     that forecast, with a `date` column.
#'
#' @return A `tibble` (data frame) with one row per forecast result and the
#'   following columns:
#'   - `train_period`: Date range of the training period used for the forecast.
#'   - `forecast_period`: Date range of the forecasted period.
#'   - `smape`: Symmetric Mean Absolute Percentage Error between forecasted and
#'     actual values, rounded to two decimals.
#'   - `mase`: Mean Absolute Scaled Error, rounded to two decimals.
#'
#' @details
#' - **SMAPE** is defined as:
#'   \deqn{SMAPE = mean( |F - A| / ((|A| + |F|) / 2) )}
#'   where \(A\) are actual values and \(F\) are forecasts.
#'   It avoids division by zero and is suitable for count data with zeros.
#'
#' - **MASE** compares the mean absolute forecast error against the mean
#'   absolute difference of successive actual:
#'   \deqn{MASE = mean(|A - F|) / mean(|diff(A)|)}
#'
#' The function automatically excludes forecasts extending beyond the latest
#' date in the observed model data.
#'
#' @seealso [generate_validation()], [generate_forecast()]
#' @export


forecast_validation_metric <- function(data,
                                       validation_res){

  # extract model data
  model_data <- data.frame(
    date = data$date,
    actual_confirm = data$confirm
  )

  # the max date of model data
  max_model_date <- max(model_data$date, na.rm = TRUE)

  metrics <- purrr::map_dfr(validation_res, function(forecast_res) {
    forecast_data <- data.frame(
      date = forecast_res$forecast_res_quantiles$date,
      forecast_confirm = forecast_res$forecast_res_quantiles$p50
    ) %>%
      left_join(model_data, by = "date")

    # SMAPE (symmetric MAPE) since actual values contain zero
    smape = mean(abs(forecast_data$forecast_confirm - forecast_data$actual_confirm) / ((abs(forecast_data$actual_confirm) + abs(forecast_data$forecast_confirm)) / 2))
    # MASE
    mase = round(mean(abs(forecast_data$actual_confirm - forecast_data$forecast_confirm)) / mean(abs(diff(forecast_data$actual_confirm))),2)

    data.frame(
      train_period = sprintf("%s to %s",
                             min(forecast_res$original_data$date, na.rm = TRUE),
                             max(forecast_res$original_data$date, na.rm = TRUE)),
      forecast_period = sprintf("%s to %s",
                                min(forecast_res$forecast_res_quantiles$date, na.rm = TRUE),
                                max(forecast_res$forecast_res_quantiles$date, na.rm = TRUE)),
      smape = round(smape, 2),
      mase = round(mase, 2),
      last_forecast_date = max(forecast_res$forecast_res_quantiles$date, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  # stop when reaching the last model date (if applicable)
  metrics <- metrics[metrics$last_forecast_date <= max_model_date, ]

  # drop helper column
  metrics$last_forecast_date <- NULL

  return(metrics)
}
