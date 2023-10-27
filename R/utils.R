#' summarise a data frame `d` by groups along a `variable`
#' @param d tibble data frame
#' @param ... group_by variables
#' @param variable string 
create_quantiles <- function(d, ..., variable = NULL) {
  d %>%
    dplyr::group_by(...) %>%
    # dplyr::group_by(region_name, HSDA_name, sit_type_all,
    #                 times,time_stamp_month) %>%
    dplyr::summarise(
      p50 = stats::quantile(.data[[variable]], 0.5),
      p25 = stats::quantile(.data[[variable]], 0.25),
      p75 = stats::quantile(.data[[variable]], 0.75),
      p05 = stats::quantile(.data[[variable]], 0.05),
      p95 = stats::quantile(.data[[variable]], 0.95)
    )
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
extract_model_data_with_extension <- function(min_model_date_str, 
                                              max_model_date_str,
                                              extension_interval=0,
                                              type = NULL){
  min_model_date <- lubridate::ymd(min_model_date_str)
  max_model_date <- lubridate::ymd(max_model_date_str)
  
  # extension condition
  if (extension_interval > 0){
    max_model_date <- max_model_date + extension_interval
  }
  model_data <- extract_model_data(min_model_date, max_model_date, type = type)
  
  return(model_data)
}

#' Extract Model Data
#'
#' @param min_model_date_str start date (in str)
#' @param max_model_date_str end date (in str)
#' @param type flu type name (in str, either "flu_a" or "flu_b")
#'
#' @return model data (dataframe) with flu cases & date specified
#' @export
#'
#' @examples
extract_model_data <- function(min_model_date_str, max_model_date_str, type="flu_a"){
  min_model_date <- lubridate::ymd(min_model_date_str)
  max_model_date <- lubridate::ymd(max_model_date_str)
  
  if(type == "RSV"){
    model_data <- phrdw_flu_weekly_data %>% # rdata version as of 2023-10-18 10:11 am
      filter(date > min_model_date,
             date <= max_model_date)
  }else{
    model_data <- plover_data %>% 
      filter(epiWeek_date > min_model_date,
             epiWeek_date <= max_model_date) %>%
      pivot_longer(cols = c("flu_a","flu_b"),
                   names_to = "type", values_to = "flu_cases") %>% 
      group_by(epiWeek_date,epiWeek_year,Epiweek,type) %>%
      summarise(flu_cases = sum(flu_cases)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = "type", values_from = "flu_cases") %>% 
      select(epiWeek_date,all_of(type)) %>% 
      rename("date" = "epiWeek_date", "confirm" = type)
  }
  

