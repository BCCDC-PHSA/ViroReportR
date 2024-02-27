#' Obtain the weekly cases from plover data
#'
#' @description
#' `get_weekly_plover()` performs data transformation in the following steps:
#'
#' 1. Uses `pivot_longer()` to convert the `flu_a` & `flu_b` columns into `flu_cases`
#'  and its name into `type`.
#' 2. Aggregates the sum of `flu_cases` by `epiWeek_date`, `epiWeek_year`, `Epiweek`, `type`.
#' 3. Uses `pivot_wider()` to convert the `flu_cases` into the `flu_a` & `flu_b` columns.
#'
#' The input dataframe `plover_data` must have the following columns:
#' * `epiWeek_date`: Epiweek Date String (e.g. '2019-01-01')
#' * `epiWeek_year`: Epiweek Year Number (e.g. 2019)
#' * `Epiweek`: Epiweek Number (e.g. 1, 2, ..., 53)
#' * `flu_a`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_b`: Confirmed Cases Count (e.g. 1, 2, ...)
#'
#' @param plover_data raw plover data before any transformation
#'
#' @return plover data aggregated by epiweek date, year, week, flu type.
#'
#' @importFrom magrittr "%>%"
#'
#'
get_weekly_plover <- function(plover_data) {
  stopifnot(c("epiWeek_date", "epiWeek_year", "Epiweek", "flu_a", "flu_b") %in% colnames(plover_data))

  epiWeek_date <- epiWeek_year <- Epiweek <- type <- flu_cases <- NULL

  agg_plover_data_date_type <- plover_data %>%
    tidyr::pivot_longer(
      cols = c("flu_a", "flu_b"),
      names_to = "type", values_to = "flu_cases"
    ) %>%
    dplyr::group_by(epiWeek_date, epiWeek_year, Epiweek, type) %>%
    dplyr::summarise(flu_cases = sum(flu_cases)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "type", values_from = "flu_cases")

  return(agg_plover_data_date_type)
}


#' Filter the weekly plover data by date & disease type
#'
#' @description
#' `get_weekly_plover_by_date_type()` filter the weekly plover data by a given
#' disease type and the date range, it performs the following steps:
#' 1. Filters the weekly plover data by the given epiweek date range.
#' 2. Selects the epiweek date & flu type columns.
#' 3. Renames the epiweek date as `date` & the flu type as `confirm`.
#'
#' The input flu type must be 'flu_a' or 'flu_b'.
#'
#' The input start date must be earlier than the input end date.
#'
#' The input dataframe `plover_data` must have the following columns:
#' * `epiWeek_date`: Epiweek Date String (e.g. '2019-01-01')
#' * `epiWeek_year`: Epiweek Year Number (e.g. 2019)
#' * `Epiweek`: Epiweek Number (e.g. 1, 2, ..., 53)
#' * `flu_a`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_b`: Confirmed Cases Count (e.g. 1, 2, ...)
#'
#' @param plover_data raw plover data before any transformation
#' @param type disease type string (e.g. 'flu_a', 'flu_b')
#' @param start_date start date string (e.g. '2022-01-01')
#' @param end_date end date string (e.g. '2022-12-31')
#'
#' @return filtered weekly_plover_data
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' get_weekly_plover_by_date_type(
#'   plover_data,
#'   "flu_a",
#'   "2022-10-01",
#'   "2022-12-05"
#' )
get_weekly_plover_by_date_type <- function(plover_data, type, start_date, end_date) {
  # Include RSV and covid
  stopifnot(
    "invalid disease type, available options: 'flu_a', 'flu_b'" =
      type %in% c("flu_a", "flu_b")
  )
  stopifnot("start date is later than the end date" = (start_date < end_date))

  weekly_plover_data <- get_weekly_plover(plover_data)

  epiWeek_date <- NULL

  filtered_weekly_plover_data <- weekly_plover_data %>%
    dplyr::filter(
      epiWeek_date >= start_date,
      epiWeek_date <= end_date
    ) %>%
    dplyr::select(epiWeek_date, dplyr::all_of(type)) %>%
    dplyr::rename("date" = "epiWeek_date", "confirm" = type)

  return(filtered_weekly_plover_data)
}
