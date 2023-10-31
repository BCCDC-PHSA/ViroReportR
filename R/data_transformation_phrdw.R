
#' get_weekly_phrdw - obtain weekly phrdw data group by date & age
#'
#' @param phrdw_flu_daily_count raw phrdw data
#'
#' @return aggregated phrdw data by date & age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' phrdw_data <- data.frame(
#' lis_date_collection = as.Date(c('2023-01-08','2023-01-08','2023-01-08','2023-01-09')),
#'     result_lab_name = c("lab_a", "lab_b", "lab_a", "lab_b"),
#'     age_years = c(0, 1, 2, 3),
#'     sars_cov2 = c(1,2,3,4),
#'     rsv = c(1,2,3,4),
#'     flu_a = c(1,2,3,4),
#'     flu_b = c(3,4,5,6))
#'
#' get_weekly_phrdw(phrdw_data)
get_weekly_phrdw <- function(phrdw_flu_daily_count) {

  stopifnot(c("lis_date_collection","result_lab_name","age_years",
              "sars_cov2","rsv","flu_a","flu_b") %in%
              colnames(phrdw_flu_daily_count))

  result_lab_name <- lis_date_collection <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- NULL

  agg_phrdw_data_date_type <- phrdw_flu_daily_count %>%
    dplyr::filter(result_lab_name != '*Missing') %>%
    dplyr::mutate(date = lubridate::floor_date(lis_date_collection, unit="week"),
                  age_years = as.numeric(age_years)) %>%
    dplyr::group_by(date, age_years) %>%
    dplyr::summarize(sars_cov2 = sum(sars_cov2),
                     rsv = sum(rsv),
                     flu_a = sum(flu_a),
                     flu_b = sum(flu_b)) %>%
    dplyr::ungroup()

  return(agg_phrdw_data_date_type)
}

#' get_weekly_phrdw_by_type_date_age - obtain weekly phrdw data filtered by disease type, date, age
#'
#' @param weekly_phrdw_data weekly phrdw data from get_weekly_phrdw(phrdw_flu_daily_count)
#' @param type disease type (e.g. 'sars_cov2', 'rsv', 'flu_a', 'flu_b')
#' @param start_date start date string (e.g. '2022-01-01')
#' @param end_date end date string (e.g. '2022-12-31')
#' @param start_age start age integer (default = 0)
#' @param end_age end age integer (default = 150)
#'
#' @return weekly phrdw data filtered by disease type, date, age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' phrdw_data <- data.frame(
#' lis_date_collection = as.Date(c('2023-01-08','2023-01-08','2023-01-08','2023-01-09')),
#'     result_lab_name = c("lab_a", "lab_b", "lab_a", "lab_b"),
#'     age_years = c(0, 1, 2, 3),
#'     sars_cov2 = c(1,2,3,4),
#'     rsv = c(1,2,3,4),
#'     flu_a = c(1,2,3,4),
#'     flu_b = c(3,4,5,6))
#'
#' weekly_phrdw_data <- get_weekly_phrdw(phrdw_data)
#'
#' get_weekly_phrdw_by_type_date_age(
#'                              weekly_phrdw_data = weekly_phrdw_data,
#'                              type='rsv',
#'                              start_date='2023-01-01',
#'                              end_date='2023-02-01',
#'                              start_age=0,
#'                              end_age=10)
get_weekly_phrdw_by_type_date_age <- function(weekly_phrdw_data, type,
                                              start_date, end_date,
                                              start_age=0, end_age=150){

  stopifnot("invalid disease type, available options: 'sars_cov2', 'rsv', 'flu_a', 'flu_b'" =
              type %in% c('sars_cov2', 'rsv', 'flu_a', 'flu_b'))
  stopifnot("start date is later than the end date" = (start_date < end_date))
  stopifnot("start age or end age is not numeric" = (is.numeric(start_age) && is.numeric(end_age)))
  stopifnot("start age is later than the end age" = (start_age <= end_age))

  date <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- NULL

  filtered_weekly_phrdw_data <- weekly_phrdw_data %>%
    dplyr::filter(date >= start_date,
                  date <= end_date,
                  age_years >= start_age,
                  age_years <= end_age) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sars_cov2 = sum(sars_cov2),
                     rsv = sum(rsv),
                     flu_a = sum(flu_a),
                     flu_b = sum(flu_b)) %>%
    dplyr::select(date, dplyr::all_of(type)) %>%
    dplyr::rename("confirm" = type) %>%
    dplyr::ungroup()

  return(filtered_weekly_phrdw_data)
}
