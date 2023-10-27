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
    mutate(week_date = lubridate::floor_date(date,unit ="weeks")) %>% # [**] test ceiling
    group_by(week_date, sim) %>%
    rename(value = incidence) %>%
    summarize(value = sum(value))
  
  return (samples)
}



#' extract model data with extension
#'
#' @param min_model_date_str start date (in str)
#' @param max_model_date_str end date (in str)
#' @param extension_interval an integer (# of days) 
#'
#' @return model_data with # of days extension specified
#' @export
#'
#' @examples
extend_rows_model_data <- function(data, min_model_date_str,
                                              extension_interval=0,
                                              type = NULL){
  min_model_date <- lubridate::ymd(min_model_date_str)
  max_model_date <- data$date[which(data$date == min_model_date) + 1]
  max_model_date <- lubridate::ymd(max_model_date_str)
  
  # extension condition
  if (extension_interval > 0){
    max_model_date <- max_model_date + extension_interval
  }
  model_data <- model_data %>%
    filter(date > min_model_date, date <= max_model_date)
  
  return(model_data)
}

