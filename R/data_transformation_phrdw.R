#' Obtain daily phrdw data group by date & age
#'
#' @description
#' `get_daily_phrdw()` performs data transformation in the following steps:
#'
#' 1. Uses `filter()` to remove those missing `result_lab_name`.
#' 2. set the `age_years` as numeric.
#' 3. Aggregates the confirmed cases for each diseases by `lis_date_collection`, `age_years`.
#'
#' The input dataframe `phrdw_flu_daily_count` must have the following columns:
#' * `lis_date_collection`: Date of collection (e.g. '2019-01-01')
#' * `result_lab_name`: The name of the result lab (e.g. 'BCCDC')
#' * `age_years`: Age (e.g. 1, 2, ...)
#' * `sars_cov2`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `rsv`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_a`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_b`: Confirmed Cases Count (e.g. 1, 2, ...)
#'
#' @param phrdw_flu_daily_count raw phrdw data
#'
#' @return An aggregated daily phrdw data by date & age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' phrdw_data <- data.frame(
#'   lis_date_collection = as.Date(c(
#'     "2022-10-02", "2022-10-02",
#'     "2022-10-09", "2022-10-09",
#'     "2022-10-16", "2022-10-16",
#'     "2022-10-23", "2022-10-23",
#'     "2022-10-30", "2022-10-30"
#'   )),
#'   result_lab_name = c(
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control"
#'   ),
#'   age_years = c(7, 8, 7, 8, 7, 8, 7, 8, 7, 8),
#'   sars_cov2 = c(15, 17, 30, 28, 41, 43, 71, 74, 80, 88),
#'   rsv = c(14, 16, 29, 27, 40, 42, 70, 73, 79, 87),
#'   flu_a = c(13, 15, 28, 26, 39, 41, 69, 72, 78, 86),
#'   flu_b = c(12, 14, 27, 25, 38, 40, 68, 71, 77, 85)
#' )
#'
#' get_daily_phrdw(phrdw_data)
get_daily_phrdw <- function(phrdw_flu_daily_count) {
  stopifnot(c(
    "lis_date_collection", "result_lab_name", "age_years",
    "sars_cov2", "rsv", "flu_a", "flu_b"
  ) %in%
    colnames(phrdw_flu_daily_count))

  result_lab_name <- lis_date_collection <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- NULL

  agg_phrdw_data_date_type <- phrdw_flu_daily_count %>%
    dplyr::filter(result_lab_name != "*Missing") %>%
    dplyr::mutate(
      age_years = as.numeric(age_years)
    ) %>%
    dplyr::group_by(lis_date_collection, age_years) %>%
    dplyr::summarize(
      sars_cov2 = sum(sars_cov2),
      rsv = sum(rsv),
      flu_a = sum(flu_a),
      flu_b = sum(flu_b)
    ) %>%
    dplyr::rename(date = lis_date_collection) %>%
    dplyr::ungroup()

  return(agg_phrdw_data_date_type)
}

#' Obtain weekly phrdw data group by date & age
#'
#' @description
#' `get_weekly_phrdw()` performs data transformation in the following steps:
#'
#' 1. Uses `filter()` to remove those missing `result_lab_name`.
#' 2. Creates a epiweek date column named `date` & set the `age_years` as numeric.
#' 3. Aggregates the confirmed cases for each diseases by `date`, `age_years`.
#'
#' The input dataframe `phrdw_flu_daily_count` must have the following columns:
#' * `lis_date_collection`: Date of collection (e.g. '2019-01-01')
#' * `result_lab_name`: The name of the result lab (e.g. 'BCCDC')
#' * `age_years`: Age (e.g. 1, 2, ...)
#' * `sars_cov2`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `rsv`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_a`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_b`: Confirmed Cases Count (e.g. 1, 2, ...)
#'
#' @param phrdw_flu_daily_count raw phrdw data
#'
#' @return An aggregated weekly phrdw data by date & age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' phrdw_data <- data.frame(
#'   lis_date_collection = as.Date(c(
#'     "2022-10-02", "2022-10-02",
#'     "2022-10-09", "2022-10-09",
#'     "2022-10-16", "2022-10-16",
#'     "2022-10-23", "2022-10-23",
#'     "2022-10-30", "2022-10-30"
#'   )),
#'   result_lab_name = c(
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control"
#'   ),
#'   age_years = c(7, 8, 7, 8, 7, 8, 7, 8, 7, 8),
#'   sars_cov2 = c(15, 17, 30, 28, 41, 43, 71, 74, 80, 88),
#'   rsv = c(14, 16, 29, 27, 40, 42, 70, 73, 79, 87),
#'   flu_a = c(13, 15, 28, 26, 39, 41, 69, 72, 78, 86),
#'   flu_b = c(12, 14, 27, 25, 38, 40, 68, 71, 77, 85)
#' )
#'
#' get_weekly_phrdw(phrdw_data)
get_weekly_phrdw <- function(phrdw_flu_daily_count) {
  stopifnot(c(
    "lis_date_collection", "result_lab_name", "age_years",
    "sars_cov2", "rsv", "flu_a", "flu_b"
  ) %in%
    colnames(phrdw_flu_daily_count))

  result_lab_name <- lis_date_collection <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- NULL

  agg_phrdw_data_date_type <- phrdw_flu_daily_count %>%
    dplyr::filter(result_lab_name != "*Missing") %>%
    dplyr::mutate(
      date = lubridate::floor_date(lis_date_collection, unit = "week"),
      age_years = as.numeric(age_years)
    ) %>%
    dplyr::group_by(date, age_years) %>%
    dplyr::summarize(
      sars_cov2 = sum(sars_cov2),
      rsv = sum(rsv),
      flu_a = sum(flu_a),
      flu_b = sum(flu_b)
    ) %>%
    dplyr::ungroup()

  return(agg_phrdw_data_date_type)
}

#' Filter weekly phrdw data filtered by disease type, date, age
#'
#' @description
#' `get_phrdw_by_type_date_age()` filter the weekly phrdw data by a given
#' disease type, date range, and age range, it performs the following steps:
#' 1. Filters the weekly phrdw data by the given epiweek date range and age range.
#' 2. Aggregates the confirmed cases count by the epiweek date.
#' 3. Selects the date & the corresponding disease type column.
#' 4. Renames the disease type as `confirm`.
#'
#' The input dataframe `weekly_phrdw_data` must be the output of `get_weekly_phrdw()`.
#'
#' The input disease type must be either 'sars_cov2', 'rsv', 'flu_a', or 'flu_b'.
#'
#' The input start date must be earlier than the input end date.
#'
#' The input start age must be earlier than the input end age.
#'
#' @seealso [vriforecasting::get_weekly_phrdw()] which produces the input dataframe of
#' this function.
#'
#' @param weekly_phrdw_data weekly phrdw data from get_weekly_phrdw(phrdw_flu_daily_count)
#' @param type disease type (e.g. 'sars_cov2', 'rsv', 'flu_a', 'flu_b')
#' @param start_date start date string (e.g. '2022-01-01')
#' @param end_date end date string (e.g. '2022-12-31')
#' @param start_age start age integer (default = 0)
#' @param end_age end age integer (default = 150)
#'
#' @return A weekly phrdw data filtered by disease type, date, age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' phrdw_data <- data.frame(
#'   lis_date_collection = as.Date(c(
#'     "2022-10-02", "2022-10-02",
#'     "2022-10-09", "2022-10-09",
#'     "2022-10-16", "2022-10-16",
#'     "2022-10-23", "2022-10-23",
#'     "2022-10-30", "2022-10-30"
#'   )),
#'   result_lab_name = c(
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control",
#'     "BC Centre for Disease Control", "BC Centre for Disease Control"
#'   ),
#'   age_years = c(7, 8, 7, 8, 7, 8, 7, 8, 7, 8),
#'   sars_cov2 = c(15, 17, 30, 28, 41, 43, 71, 74, 80, 88),
#'   rsv = c(14, 16, 29, 27, 40, 42, 70, 73, 79, 87),
#'   flu_a = c(13, 15, 28, 26, 39, 41, 69, 72, 78, 86),
#'   flu_b = c(12, 14, 27, 25, 38, 40, 68, 71, 77, 85)
#' )
#'
#' weekly_phrdw_data <- get_weekly_phrdw(phrdw_data)
#'
#' get_phrdw_by_type_date_age(
#'   weekly_phrdw_data = weekly_phrdw_data,
#'   type = "rsv",
#'   start_date = "2022-10-01",
#'   end_date = "2022-11-01",
#'   start_age = 0,
#'   end_age = 10
#' )
get_phrdw_by_type_date_age <- function(weekly_phrdw_data, type,
                                              start_date, end_date,
                                              start_age = 0, end_age = 150) {
  stopifnot(
    "invalid disease type, available options: 'sars_cov2', 'rsv', 'flu_a', 'flu_b'" =
      type %in% c("sars_cov2", "rsv", "flu_a", "flu_b")
  )
  stopifnot("start date is later than the end date" = (start_date < end_date))
  stopifnot("start age or end age is not numeric" = (is.numeric(start_age) && is.numeric(end_age)))
  stopifnot("start age is later than the end age" = (start_age <= end_age))

  date <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- NULL

  filtered_weekly_phrdw_data <- weekly_phrdw_data %>%
    dplyr::filter(
      date >= start_date,
      date <= end_date,
      age_years >= start_age,
      age_years <= end_age
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      sars_cov2 = sum(sars_cov2),
      rsv = sum(rsv),
      flu_a = sum(flu_a),
      flu_b = sum(flu_b)
    ) %>%
    dplyr::select(date, dplyr::all_of(type)) %>%
    dplyr::rename("confirm" = type) %>%
    dplyr::ungroup()

  return(filtered_weekly_phrdw_data)
}
