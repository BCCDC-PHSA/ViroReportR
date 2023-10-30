#' summarise a data frame `d` by groups along a `variable`
#' @param d tibble data frame
#' @param ... group_by variables
#' @param variable string 
create_quantiles <- function(d, ..., variable = NULL) {
  d %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      p50 = stats::quantile(.data[[variable]], 0.5),
      p25 = stats::quantile(.data[[variable]], 0.25),
      p75 = stats::quantile(.data[[variable]], 0.75),
      p05 = stats::quantile(.data[[variable]], 0.05),
      p95 = stats::quantile(.data[[variable]], 0.95)
    )
}


extract_agg_samples_epiestim_fit <- function(samples){
  
  samples <- samples %>% 
    mutate(week_date = lubridate::floor_date(date,unit ="weeks")) %>% 
    group_by(week_date, sim) %>%
    rename(value = incidence) %>%
    suppressMessages(summarize(value = sum(value)))
  
  return (samples)
}



#' extract model data with extension during each iteration of loop 
#'
#' @param min_model_date_str start date (in str)
#' @param extension_interval an integer (# of days) 
#'
#' @return model_data with # of days extension specified
#' @export
#'
#' @examples
extend_rows_model_data <- function(data, min_model_date_str,
                                              extension_interval=0){
  min_model_date <- lubridate::ymd(min_model_date_str)
  max_model_date <- data$date[which(data$date == min_model_date) + 1]

  if (extension_interval > 0){
    max_model_date <- max_model_date + extension_interval
  }
model_data <- data %>%
    filter(date >= min_model_date, date <= max_model_date)
  
  return(model_data)
}


# Extract 1w 2w sample distribution ---------------------------------------

#' Extract Weekly Samples (week_date & value) ideally 1000 samples per date
#'
#' @param time_period_result output from experiment_time_period_epiestim() i-th
#'
#' @return
#' @export
#'
#' @examples
extract_weekly_samples_epiestim_fit <- function(time_period_result){
  return(tibble(week_date = time_period_result$week_date,
                value = time_period_result$value))
}


#' Extract calculated quantiles from the weekly samples
#'
#' @param time_period_result output from experiment_time_period_epiestim() i-th
#'
#' @return
#' @export
#'
#' @examples
extract_weekly_quantile_epiestim <- function(time_period_result){
  # the samples are already generated/simulated
  return(tibble(week_date = time_period_result$quantile_week_date,
                p50 = time_period_result$p50,
                p25 = time_period_result$p25,
                p75 = time_period_result$p75,
                p05 = time_period_result$p05,
                p95 = time_period_result$p95))
}




