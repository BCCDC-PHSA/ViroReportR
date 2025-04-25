#' Weekly respirartory viral cases
#'
#' A dataset containing weekly cases of Flu-A, Flu-B, SARS-CoV2 and RSV
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
#' @name weekly_data
#'
#'
#' @docType data
"weekly_data"



#' Daily/Weekly flu, RSV, sars_cov2 Cases
#'
#' A simulated dataset containing COVID, flu A and RSV cases
#' The variables are as follows:
#'
#' @format A data frame with over 366 rows and 4 variables:
#' \describe{
#'   \item{date}{YMD format}
#'   \item{rsv}{number of confirmed RSV cases}
#'   \item{flua}{number of confirmed flu A cases}
#'   \item{covid}{number of confirmed COVID-19 cases}
#' }
#' @name daily_data
#'
#'
#' @docType data
"daily_data"


#' Time period result: weekly forecasts of weekly data in sliding windows as the output of \code{forecast_time_period_epiestim}
#'
#' @format List of class \code{forecast_time_period_epiestim}
#' storing quantiles of weekly forecasts from each sliding window
#' @name weekly_time_period_result
#'
#' @source data produced by script \code{time_period_result.R}
#'
#' @docType data
"weekly_time_period_result"


#' Time period result: daily forecasts of weekly data in sliding windows as the output of \code{forecast_time_period_epiestim}
#'
#' @format List of class \code{forecast_time_period_epiestim}
#' storing quantiles of daily forecasts from each sliding window
#' @name weekly_time_period_result
#'
#' @source data produced by script \code{time_period_result.R}
#'
#' @docType data
"daily_time_period_result"


