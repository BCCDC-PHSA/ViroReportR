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
#'
#' @importFrom magrittr "%>%"
#'

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
      flu_b = sum(flu_b),
      .groups = "keep"
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
#'
#' @importFrom magrittr "%>%"
#'

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
      flu_b = sum(flu_b),
      .groups = "keep"
    ) %>%
    dplyr::ungroup()

  return(agg_phrdw_data_date_type)
}

#' Filter phrdw data filtered by time period, disease type, date, age
#'
#' @description
#' `get_phrdw_by_type_date_age()` filter the phrdw data by a given
#' time_period, disease type, date range, and age range, it performs the following steps:
#' 1. Extracts the phrdw data by daily or weekly
#' 2. Filters the phrdw data by the given epiweek date range and age range.
#' 3. Aggregates the confirmed cases count by the epiweek date.
#' 4. Selects the date & the corresponding disease type column.
#' 5. Renames the disease type as `confirm`.
#'
#' The input dataframe `phrdw_data` must have the following columns:
#' * `lis_date_collection`: Date of collection (e.g. '2019-01-01')
#' * `result_lab_name`: The name of the result lab (e.g. 'BCCDC')
#' * `age_years`: Age (e.g. 1, 2, ...)
#' * `sars_cov2`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `rsv`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_a`: Confirmed Cases Count (e.g. 1, 2, ...)
#' * `flu_b`: Confirmed Cases Count (e.g. 1, 2, ...)
#'
#' The input disease type must be either 'sars_cov2', 'rsv', 'flu_a', or 'flu_b'.
#'
#' The input start date must be earlier than the input end date.
#'
#' The input start age must be earlier than the input end age.
#'
#' @param phrdw_data raw phrdw data
#' @param time_period time period string (e.g. 'daily', 'weekly')
#' @param type disease type (e.g. 'sars_cov2', 'rsv', 'flu_a', 'flu_b')
#' @param start_date start date string (e.g. '2022-01-01')
#' @param end_date end date string (e.g. '2022-12-31')
#' @param start_age start age integer (default = 0)
#' @param end_age end age integer (default = 150)
#'
#' @return phrdw data filtered by time period, disease type, date, age
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' get_phrdw_by_type_date_age(
#'   phrdw_data = phrdw_data,
#'   time_period = "weekly",
#'   type = "rsv",
#'   start_date = "2022-10-01",
#'   end_date = "2022-11-01",
#'   start_age = 0,
#'   end_age = 10
#' )
get_phrdw_by_type_date_age <- function(phrdw_data, time_period = "weekly",
                                       type, start_date = min(phrdw_data$lis_date_collection),
                                       end_date = max(phrdw_data$lis_date_collection),
                                       start_age = 0, end_age = 17) {
  stopifnot(
    "invalid time period, available options: 'daily', 'weekly'" =
      time_period %in% c("daily", "weekly")
  )
  stopifnot(
    "invalid disease type, available options: 'sars_cov2', 'rsv', 'flu_a', 'flu_b'" =
      type %in% c("sars_cov2", "rsv", "flu_a", "flu_b")
  )
  stopifnot("start date is later than the end date" = (start_date < end_date))
  stopifnot("start age or end age is not numeric" = (is.numeric(start_age) && is.numeric(end_age)))
  stopifnot("start age is later than the end age" = (start_age <= end_age))

  date <- age_years <- sars_cov2 <- rsv <- flu_a <- flu_b <- days_in_week <- NULL

  temp <- data.frame()

  if (time_period == "daily") {
    temp <- get_daily_phrdw(phrdw_data)
  } else if (time_period == "weekly") {
    temp <- get_weekly_phrdw(phrdw_data)

    check_temp <- temp %>%
      dplyr::filter(
        date >= start_date,
        date <= end_date,
        age_years >= start_age,
        age_years <= end_age
      ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        days_in_week = dplyr::n_distinct(date),
        .groups = "keep"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(days_in_week < 7) %>%
      dplyr::mutate(
        warning_message = paste("Warning: Less than 7 days in a week for date:", format(date, "%Y-%m-%d"), "\n")
      )

    if (!purrr::is_empty(check_temp$warning_message)) {
      warning(check_temp$warning_message)
    }
  }

  filtered_phrdw_data <- temp %>%
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
      flu_b = sum(flu_b),
      .groups = "keep"
    ) %>%
    dplyr::select(date, dplyr::all_of(type)) %>%
    dplyr::rename("confirm" = type) %>%
    dplyr::ungroup()


  return(filtered_phrdw_data)
}
