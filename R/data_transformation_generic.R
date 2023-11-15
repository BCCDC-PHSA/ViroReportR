#' Extract Weekly Data from a Generic Dataframe
#'
#' @description
#' `get_weekly_data()` performs data transformation in the following steps:
#'
#' 1. Create the week date column using `floor_date()`.
#' 2. Select only the week date and confirmed cases column.
#'
#' The input dataframe `generic_data` must have the following columns:
#' * `<date name>`: date column (e.g. as.Date('2022-01-01'))
#' * `<cases count name>`: Confirmed Cases Count (e.g. 1, 2, ...)
#' Note that these columns can be defined in a generic name, and inputted as
#' the other two function parameters for data transformation (`date_column`,
#' `number_column`)
#'
#' @param generic_data data with a self defined column name for both date and cases count
#' @param date_column date column name str
#' @param number_column cases count column name str
#'
#' @importFrom data.table :=
#'
#' @return weekly data of the generic confirmed cases data
#' @export
#'
#' @examples
#'
#' generic_data <- data.frame(
#'   date_of_report = as.Date(c(
#'     "2022-10-10", "2022-10-10",
#'     "2022-10-17", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-11-28"
#'   )),
#'   flu_a = c(7, 10, 32, 38, 43, 45, 73, 88, 44, 51),
#'   flu_b = c(6, 13, 31, 45, 50, 52, 68, 83, 45, 49)
#' )
#'
#' weekly_data <- get_weekly_data(generic_data, "date_of_report", "flu_a")
get_weekly_data <- function(generic_data, date_column, number_column) {
  # Unquote the quosures
  date_column <- dplyr::quo_name(dplyr::enquo(date_column))
  number_column <- dplyr::quo_name(dplyr::enquo(number_column))

  stopifnot(
    "invalid date column name, not found in the input data" =
      date_column %in% names(generic_data)
  )

  stopifnot(
    "invalid number column name, not found in the input data" =
      number_column %in% names(generic_data)
  )

  weekly_data <- generic_data %>%
    dplyr::mutate(
      date = lubridate::floor_date(!!dplyr::sym(date_column), unit = "week")
    ) %>%
    dplyr::mutate(!!date_column := date) %>%
    dplyr::select(!!date_column, !!number_column)

  return(weekly_data)
}


#' Extract Aggregated Weekly Generic Data
#'
#' @description
#' `get_weekly_aggregated_data()` performs data transformation in the following steps:
#'
#' 1. Group the weekly data by date (week date).
#' 2. Aggregate the number of confirmed cases by week date.
#' 3. Select only the week date and confirmed cases column.
#' 4. Filter the data by given start and end date
#'
#' The input dataframe `weekly_data` must have the following columns:
#' * `<date name>`: date column (e.g. as.Date('2022-01-01'))
#' * `<cases count name>`: Confirmed Cases Count (e.g. 1, 2, ...)
#' Note that these columns can be defined in a generic name, and inputted as
#' the other two function parameters for data transformation (`date_column`,
#' `number_column`)
#'
#' Assume the date column is the start of the epiweek.
#'
#' @param weekly_data the weekly generic data from `get_weekly_data()`
#' @param date_column date column name str
#' @param number_column cases count column name str
#' @param start_date start date string (e.g. '2022-01-01')(optional, default is NULL)
#' @param end_date end date string (e.g. '2022-12-31')(optional, default is NULL)
#'
#' @return aggregated weekly data of the generic confirmed cases data (filtered by date if any)
#' @export
#'
#' @examples
#'
#' generic_data <- data.frame(
#'   date_of_report = as.Date(c(
#'     "2022-10-10", "2022-10-10",
#'     "2022-10-17", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-11-28"
#'   )),
#'   flu_a = c(7, 10, 32, 38, 43, 45, 73, 88, 44, 51),
#'   flu_b = c(6, 13, 31, 45, 50, 52, 68, 83, 45, 49)
#' )
#'
#' weekly_data <- get_weekly_data(generic_data, "date_of_report", "flu_a")
#'
#' weekly_aggregated_data <- get_weekly_aggregated_data(
#'   weekly_data,
#'   "date_of_report", "flu_a", "2022-10-16", "2023-12-31"
#' )
get_weekly_aggregated_data <- function(weekly_data, date_column, number_column,
                                       start_date = NULL, end_date = NULL) {
  confirm <- NULL
  # Unquote the quosures
  date_column <- dplyr::quo_name(dplyr::enquo(date_column))
  number_column <- dplyr::quo_name(dplyr::enquo(number_column))

  stopifnot(
    "invalid date column name, not found in the input data" =
      date_column %in% names(weekly_data)
  )

  stopifnot(
    "invalid number column name, not found in the input data" =
      number_column %in% names(weekly_data)
  )

  weekly_aggregated_data <- weekly_data %>%
    dplyr::group_by(!!dplyr::sym(date_column)) %>%
    dplyr::summarise(confirm = sum(!!dplyr::sym(number_column))) %>%
    dplyr::select(!!dplyr::sym(date_column), confirm) %>%
    dplyr::rename(date = !!dplyr::sym(date_column))

  if (!is.null(start_date) & !is.null(end_date)) {
    stopifnot("start date is later than the end date" = (start_date < end_date))
    filtered_weekly_aggregated_data <- weekly_aggregated_data %>%
      dplyr::filter(
        date >= start_date,
        date <= end_date
      )
    return(filtered_weekly_aggregated_data)
  }

  if (!is.null(start_date)) {
    filtered_weekly_aggregated_data <- weekly_aggregated_data %>%
      dplyr::filter(
        date >= start_date
      )
    return(filtered_weekly_aggregated_data)
  }

  if (!is.null(end_date)) {
    filtered_weekly_aggregated_data <- weekly_aggregated_data %>%
      dplyr::filter(
        date <= end_date
      )
    return(filtered_weekly_aggregated_data)
  }

  return(weekly_aggregated_data)
}
