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
#' }
#' @name plover_data
#'
#' @source "O:/BCCDC/Groups/Analytics/Projects/covid_modeling/06 Projects/RSV Flu Modelling/Data/plover_weekly_resp.rds"
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
#' @source "O:/BCCDC/Groups/Analytics/Projects/covid_modeling/06 Projects/RSV Flu Modelling/Data/phrdw_flu_daily_count_vpp_only.rData"
#'
#' @docType data
"phrdw_data"


#' Daily Generic Confirmed Cases Data
#'
#' A dataset containing a generic naming for the date & confirmed cases column
#' The variables are as follows:
#'
#' @format
#' \describe{
#'   \item{date_of_report}{the date of the reported cases (e.g. as.Date('2022-01-01'))}
#'   \item{flu_a}{number of confirmed flu A cases}
#'   \item{flu_b}{number of confirmed flu B cases}
#' }
#' @name generic_data
#'
#' @docType data
"generic_data"
