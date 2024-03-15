#' Weekly Flu Cases from Plover
#'
#' A dataset containing the flu cases in BC Canada.
#' The variables are as follows:
#'
#' @format A data frame with over 1000 rows and 5 variables:
#' \describe{
#'   \item{epiWeek_date}{the date of that epiweek (2019-09-08 -- 2023-10-15)}
#'   \item{epiWeek_year}{the year of that epiweek (2019 -- 2023)}
#'   \item{Epiweek}{the epiweek number (1 -- 53)}
#'   \item{flu_a}{number of confirmed flu A cases}
#'   \item{flu_b}{number of confirmed flu B cases}
#'   \item{corona}{number of confirmed SARS CoV2 cases}
#'   \item{rsv}{number of confirmed RSV cases}
#' }
#' @name plover_data
#'
#'
#' @docType data
"plover_data"



#' Daily/Weekly flu, RSV, sars_cov2 Cases from Phrdw
#'
#' A dataset containing the flu, RSV, sars_cov2 Cases for different
#' labs & ages in BC Canada.
#' The variables are as follows:
#'
#' @format A data frame with over 8000 rows and 7 variables:
#' \describe{
#'   \item{lis_date_collection}{the date of that epiweek (2021-08-31 -- 2023-10-17)}
#'   \item{result_lab_name}{the year of that epiweek (e.g. "BC Centre for Disease Control")}
#'   \item{age_years}{the epiweek number (0 -- 103)}
#'   \item{sars_cov2}{number of confirmed sars_cov2 cases}
#'   \item{rsv}{number of confirmed RSV cases}
#'   \item{flu_a}{number of confirmed flu A cases}
#'   \item{flu_b}{number of confirmed flu B cases}
#' }
#' @name phrdw_data
#'
#'
#' @docType data
"phrdw_data"


#' Time period result: weekly forecasts of PLOVER flu data in sliding windows as the output of \code{forecast_time_period_epiestim}
#'
#' @format List of class \code{forecast_time_period_epiestim}
#' storing quantiles of both daily and weekly forecasts from each sliding window
#' @name weekly_time_period_result
#'
#' @source data produced by script \code{time_period_result.R}
#'
#' @docType data
"weekly_time_period_result"



#' Time period result: daily forecasts of PLOVER flu data in sliding windows as the output of \code{forecast_time_period_epiestim}
#'
#' @format List of class \code{forecast_time_period_epiestim}
#' storing quantiles of 14 day ahead daily forecasts from each sliding window
#' @name daily_time_period_result
#'
#' @source data produced by script \code{time_period_result.R}
#'
#' @docType data
"daily_time_period_result"


#' Weekly aggregated and transformed Flu Cases from Plover
#'
#' A dataset containing the flu cases in BC Canada aggregated by week after running package PLOVER transformation functions
#' The variables are as follows:
#'
#' @format Data frame with
#' \describe{
#'   \item{date}{Weekly date over which samples were aggregated}
#'   \item{confirm}{confirmed weekly cases}
#' }
#' @name weekly_transformed_plover_data
#'
#' @source  data produced by script \code{weekly_transformed_plover_data.R}
#'
#' @docType data
"weekly_transformed_plover_data"

#' Generic Data with Date & Number of Cases Column
#'
#' A dataset containing a generic naming for the date & confirmed cases column
#'
#' The purpose of this generic data is to demonstrate the flexibility of a user-defined data
#' with generic date & confirmed cases column name. These generic column names will be used
#' as the input parameters in the generic data transformation function.
#'
#' Assume the generic data will have 1 column for the date and 1 column for the confirmed cases column.
#'
#' A sample variables are as follows:
#'
#' @format
#' \describe{
#'   \item{<date column>}{the date of the reported cases (e.g. as.Date('2022-01-01'))}
#'   \item{<confirmed cases column>}{number of confirmed cases}
#' }
#' @name generic_data
#'
#' @docType data
"generic_data"

