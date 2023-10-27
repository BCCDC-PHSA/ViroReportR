
#' get_weekly_plover - calculate the weekly cases from plover data
#'
#' @param plover_data raw plover data before any transformation
#'
#' @return weekly_plover_data plover data aggregated by epiweek date, year, week type
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#'
#' @examples get_weekly_plover(data.frame(
#'epiWeek_date = as.Date(c('2019-09-08','2019-09-29','2019-10-06','2019-10-13')),
#'                              epiWeek_year = c(2019, 2019, 2019, 2019),
#'                              Epiweek = c(37,40,41,42),
#'                              flu_a = c(1,2,3,4),
#'                              flu_b = c(3,4,5,6)))
get_weekly_plover <- function(plover_data) {

  stopifnot(c("epiWeek_date","epiWeek_year","Epiweek","flu_a","flu_b") %in% colnames(plover_data))

  epiWeek_date <- epiWeek_year <- Epiweek <- type <- flu_cases <- NULL

  tryCatch(
    return(plover_data %>%
             tidyr::pivot_longer(cols = c("flu_a","flu_b"),
                                 names_to = "type", values_to = "flu_cases") %>%
             dplyr::group_by(epiWeek_date, epiWeek_year, Epiweek, type) %>%
             dplyr::summarise(flu_cases = sum(flu_cases)) %>%
             dplyr::ungroup() %>%
             tidyr::pivot_wider(names_from = "type", values_from = "flu_cases")
    ),
    error = function(e){
      message("An error occurred:\n", e)
    },
    warning = function(w){
      message("A warning occured:\n", w)
    }
  )
}


#' get_weekly_plover_by_date_type - filter the weekly plover data by date & disease type
#'
#' @param weekly_plover_data weekly plover data from get_weekly_plover
#' @param type disease type string (e.g. "flu_a", "flu_b")
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
#' @examples get_weekly_plover_by_date_type(
#'get_weekly_plover(data.frame(
#'epiWeek_date = as.Date(c('2019-09-08','2019-09-29','2019-10-06','2019-10-13')),
#'                              epiWeek_year = c(2019, 2019, 2019, 2019),
#'                              Epiweek = c(37,40,41,42),
#'                              flu_a = c(1,2,3,4),
#'                              flu_b = c(3,4,5,6))),
#'                              'flu_a',
#'                              '2019-09-01',
#'                              '2019-10-13')
get_weekly_plover_by_date_type <- function(weekly_plover_data, type, start_date, end_date){

  stopifnot("invalid disease type, available options: 'flu_a', 'flu_b'" =
              type %in% c('flu_a', 'flu_b'))
  stopifnot("start date is later than the end date" = (start_date < end_date))

  epiWeek_date <- NULL

  tryCatch(
    return(weekly_plover_data  %>%
             dplyr::filter(epiWeek_date >= start_date,
                           epiWeek_date <= end_date) %>%
             dplyr::select(epiWeek_date, dplyr::all_of(type)) %>%
             dplyr::rename("date" = "epiWeek_date", "confirm" = type)
           ),
           error = function(e){
             message("An error occurred:\n", e)
             },
           warning = function(w){
             message("A warning occured:\n", w)
             }
    )
}
