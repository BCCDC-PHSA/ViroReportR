#' summarise a data frame `d` by groups along a `variable`
#' @param d tibble data frame
#' @param ... group_by variables
#' @param variable string
#'
#'
#' @return Data frame containing sample quantiles at probabilities 0.05, 0.25, 0.50,
#' 0.75 and 0.95
create_quantiles <- function(d, ..., variable = NULL) {
  d %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      p50 = stats::quantile(.data[[variable]], 0.5),
      p10 = stats::quantile(.data[[variable]], 0.1),
      p25 = stats::quantile(.data[[variable]], 0.25),
      p75 = stats::quantile(.data[[variable]], 0.75),
      p90 = stats::quantile(.data[[variable]], 0.9),
      p025 = stats::quantile(.data[[variable]], 0.025),
      p975 = stats::quantile(.data[[variable]], 0.975),
      min_sim = min(.data[[variable]]),
      max_sim = max(.data[[variable]])
    )
}


#' Extract current forecast metrics: forecast prediction, percentile interval and Rt value
#' @param time_period_result output from  \code{forecast_time_period}
#' @param iter number of MCMC iterations used to generate Rt posterior
#' @return dataframe of current forecast metrics
#'
forecast_metrics <- function(time_period_result, iter = 10) {
  cur_time_period_result <- time_period_result[[length(time_period_result)]]

  Rt_mean <- round(cur_time_period_result$R$`Mean(R)`[length(cur_time_period_result$R$`Mean(R)`)], 2)
  Rt_std <- cur_time_period_result$R$`Std(R)`[length(cur_time_period_result$R$`Std(R)`)]
  R_lower <- round(Rt_mean - 1.96 * Rt_std / sqrt(iter), 2)
  R_upper <- round(Rt_mean + 1.96 * Rt_std / sqrt(iter), 2)

  if("forecast_res_quantiles" %in% names(time_period_result)){
    data_proj <- time_period_result$forecast_res_quantiles %>%
      dplyr::mutate_if(is.numeric, round) %>%
      dplyr::mutate(`95 percentile interval` = glue::glue("{p025}-{p975}")) %>%
      dplyr::mutate(`50 percentile interval` = glue::glue("{p25}-{p75}"))
  }else{
    data_proj <- tibble::tibble(
      date = cur_time_period_result$week_date,
      sim = cur_time_period_result$sim,
      incidence = cur_time_period_result$weekly_incidence,
    )
    data_proj <- data_proj %>%
      dplyr::mutate(incidence = incidence) %>%
      create_quantiles(date, variable = "incidence") %>%
      dplyr::mutate_if(is.numeric, round) %>%
      dplyr::mutate(`95 percentile interval` = glue::glue("{p025}-{p975}")) %>%
      dplyr::mutate(`50 percentile interval` = glue::glue("{p25}-{p75}"))
  }


  Rt_interval <- glue::glue("{R_lower}-{R_upper}")

  return(list(
    Rt_mean = Rt_mean,
    Rt_interval = Rt_interval,
    prediction = unname(data_proj$p50[1]),
    interval_90 = data_proj$`95 percentile interval`,
    interval_50 = data_proj$`50 percentile interval`,
    forecast_date = unname(format(as.Date(data_proj$date[1])))
  ))
}



#' Print out text output for ViroReportR report detailing current number of case visits, last value of Rt and corresponding intervals
#' @param time_period_result output from  \code{forecast_time_period}
#' @param ... optional arguments to be passed on to \code{forecast_metrics}
#' @return current forecast metrics
current_forecast_text <- function(time_period_result, ...) {
  forecast_metrics <- forecast_metrics(time_period_result, ...)
  cat(
    "The 1-week ahead forecast value of confirmed cases for", format(as.Date(forecast_metrics$forecast_date)),
    "is:", forecast_metrics$prediction, "cases/week \n\n",
    "The 95 % prediction interval for this forecast is", glue::glue("({forecast_metrics$interval_90[1]})"),
    "\n\n The 50 % prediction interval for this forecast is", glue::glue("({forecast_metrics$interval_50[1]})"),
    "\n\n The last estimated Rt value with the 95% confidence interval is:", forecast_metrics$Rt_mean, glue::glue("({forecast_metrics$Rt_interval})")
  )
}



#' @noRd
check_data_contains_start_date <- function(data,start_date){
  if(!(as.Date(start_date) %in% as.Date(data$date))){
    stop("Data must include the `start_date`")
  }
}

#' Validates that the input `data` contains the required columns for use with
#'  EpiEstim.
#'
#' This function checks that the input data frame has the required columns:
#' `"date"` and `"confirm"`.
#' If either of these columns is missing, the function will stop with an
#' error message.
#'
#' @param data A data frame
#'
#' @noRd
check_epiestim_format <- function(data){
  required_columns <- c("date","confirm")
  missing_columns <- setdiff(required_columns, colnames(data))
  extra_columns <- setdiff(colnames(data),required_columns)
  if(length(missing_columns) > 0){
    stop("Data needs columns: ", paste(missing_columns, collapse = ", "))
  }
  if(length(extra_columns) > 0){
    stop("Data has redundant columns: ", paste(extra_columns, collapse = ", "))
  }
}

#' Ensure that the input `data` includes at least 14 valid days.
#'
#' This function verifies that the input data frame contains a minimum of
#' 14 days of records. Days at the start with zero confirmed cases are not
#' included in the count.
#'
#' @param data A data frame
#'
#' @noRd
check_min_days <- function(data){
  if(nrow(data) < 14){
    stop("At least 14 days of data are needed.")
  }
}


#' Clean and validate case count data for EpiEstim
#'
#' This function prepares case count data for use with **EpiEstim** by
#' performing a series of validation and cleaning steps:
#'
#' 1. Ensures that the input data frame has the required columns:
#'    `"date"` and `"confirm"`.
#' 2. Confirms that the specified `start_date` exists in the data and filters
#'    the data to include only records on or after that date.
#' 3. Removes leading days before the first non-zero confirmed case.
#' 4. Verifies that the resulting dataset contains at least 14 valid days
#'    (as required for estimation).
#'
#' @param data A data frame containing at least the columns `"date"` and
#'   `"confirm"`. The `"date"` column should be of class `Date`, and
#'   `"confirm"` should be numeric.
#' @param start_date A `Date` (or date-convertible string) indicating the
#'   starting date for analysis. Must exist within the `"date"` column.
#'
#' @return A cleaned data frame filtered from `start_date`, starting at the
#'   first date with non-zero confirmed cases, and containing at least 14 days
#'   of data.
#'
#' @details
#' This function is primarily intended as a preprocessing step for EpiEstim
#' modeling. It combines validation checks for input structure and time coverage
#' with minimal data cleaning logic to ensure robust downstream estimation.
#'

clean_sample_data <- function(data,
                              start_date){
  check_epiestim_format(data)

  # check and filter on start date
  check_data_contains_start_date(data,start_date)
  data <- data %>%
    dplyr::filter(date >= start_date)

  # exclude the first date if there are no confirmed cases
  non_zero_dates <- data %>%
    dplyr::filter(.data$confirm > 0) %>%
    dplyr::pull(date)
  data <- data %>%
    dplyr::filter(date >= non_zero_dates[1]) %>%
    dplyr::arrange(date)

  # check valid days
  check_min_days(data)

  return(data)
}
