#' Extract Weekly Data from a Generic Dataframe
#'
#' @description
#' This is an internal function used in `get_weekly_aggregated_data()` to obtain the weekly data
#' and further extract its weekly aggregated cases count.
#'
#' `get_weekly_data()` performs data transformation in the following steps:
#'
#' 1. Create the week date column using `floor_date()`.
#' 2. Select only the week date and confirmed cases column.
#'
#' The input dataframe `generic_data` must have the following columns:
#' * `<date name>`: date column (e.g. as.Date('2022-01-01')).
#' * `<cases count name>`: Confirmed Cases Count (e.g. 1, 2, ...).
#'
#' Note that these columns can be defined in a generic name, and inputted as
#' the other two function parameters for data transformation (`date_column`,
#' `number_column`)
#'
#' @param generic_data data with a self defined column name for both date and cases count
#' @param date_column date column name str
#' @param number_column cases count column name str
#' @param unit aggregation unit "day" or "week"
#'
#' @importFrom data.table :=
#'
#' @keywords internal
#'
#' @return  data of the generic confirmed cases data
#' \describe{
#'   \item{date}{Either day or week date}
#'   \item{confirm}{number of confirmed cases}
#' }
#'
#' @examples
#' sim_data <- simulate_data()
#' weekly_data <- get_data(sim_data, "date", "flua")
#' @noRd
get_data <- function(generic_data, date_column, number_column,unit = "day") {

  stopifnot(
    "invalid date column name, not found in the input data" =
      date_column %in% names(generic_data)
  )

  stopifnot(
    "invalid number column name, not found in the input data" =
      number_column %in% names(generic_data)
  )

  aggregated_data <- generic_data %>%
    dplyr::mutate(
      date = lubridate::floor_date(!!dplyr::sym(date_column), unit = unit)
    ) %>%
    dplyr::mutate(!!date_column := date) %>%
    dplyr::select(!!date_column, !!number_column)

  return(aggregated_data)
}


#' Extract Aggregated Weekly Generic Data
#'
#' @description
#' `get_aggregated_data()` performs data transformation in the following steps:
#'
#' 1. Group the weekly or daily data by date.
#' 2. Aggregate the number of confirmed cases by either day or week.
#' 3. Select only the date and confirmed cases column.
#' 4. Filter the data by given start and end date
#'
#' The input dataframe `generic_data` must have the following columns:
#' * `<date name>`: date column (e.g. as.Date('2022-01-01')).
#' * `<cases count name>`: Confirmed Cases Count (e.g. 1, 2, ...).
#'
#' Note that these columns can be defined in a generic name, and inputted as
#' the other two function parameters for data transformation (`date_column`,
#' `number_column`)
#'
#' Assume the date column is the start of the epiweek.
#'
#' @param generic_data the weekly generic data from `get_data()`
#' @param date_column date column name str
#' @param number_column cases count column name str
#' @param start_date start date string (e.g. '2022-01-01')(optional, default is NULL)
#' @param end_date end date string (e.g. '2022-12-31')(optional, default is NULL)
#' @param unit aggregation unit "day" or "week"
#'
#' @return aggregated weekly data of the generic confirmed cases data (filtered by date if any)
#' \describe{
#'   \item{date}{Either day or week date}
#'   \item{confirm}{number of confirmed cases}
#' }
#' @export
#'
#' @examples
#' sim_data <- simulate_data()
#' aggregated_data <- get_aggregated_data(
#'   generic_data,
#'   "date_of_report", "flua", "2024-10-16", "2024-12-31"
#' )
get_aggregated_data <- function(generic_data, date_column, number_column,
                                start_date = NULL, end_date = NULL,
                                unit = "day") {
  confirm <- NULL

  if (!is.null(start_date) & !is.null(end_date)) {
    stopifnot("start date is later than the end date" = (start_date < end_date))
  }

  aggregated_data <- get_data(generic_data, date_column, number_column,
                                 unit = unit)

  stopifnot(
    "invalid date column name, not found in the input data" =
      date_column %in% names(aggregated_data)
  )

  stopifnot(
    "invalid number column name, not found in the input data" =
      number_column %in% names(aggregated_data)
  )

  aggregated_data <- aggregated_data %>%
    dplyr::group_by(!!dplyr::sym(date_column)) %>%
    dplyr::summarise(confirm = sum(!!dplyr::sym(number_column))) %>%
    dplyr::select(!!dplyr::sym(date_column), confirm) %>%
    dplyr::rename(date = !!dplyr::sym(date_column))



  if (!is.null(start_date)) {
    if (unit == "week" & start_date != lubridate::floor_date(as.Date(start_date), unit = "week")) {
      warning("The input `start_date` doesn't coincide with start of a week, the aggregated data might include partial number of dates in the start week")
    }

    aggregated_data <- aggregated_data %>%
      dplyr::filter(
        date >= start_date
      )
  }

  if (!is.null(end_date)) {
    if (unit == "week" & end_date != lubridate::floor_date(as.Date(end_date), unit = "week")) {
      warning("The input `end_date` doesn't coincide with start of a week, the aggregated data might include partial number of dates in the end week")
    }

    aggregated_data <- aggregated_data %>%
      dplyr::filter(
        date <= end_date
      )
  }

  return(aggregated_data)
}
