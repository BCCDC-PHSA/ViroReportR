
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
#' @examples
get_weekly_plover <- function(plover_data) {

  plover_data %>%
    tidyr::pivot_longer(cols = c("flu_a","flu_b"),
                 names_to = "type", values_to = "flu_cases") %>%
    dplyr::group_by(epiWeek_date, epiWeek_year, Epiweek, type) %>%
    dplyr::summarise(flu_cases = sum(flu_cases)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "type", values_from = "flu_cases")
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
#' @examples get_weekly_plover_by_date_type(weekly_plover_data, "flu_a", "2022-01-01", "2022-02-01")
get_weekly_plover_by_date_type <- function(weekly_plover_data, type, start_date, end_date){
  weekly_plover_data %>%
    dplyr::filter(epiWeek_date >= start_date,
           epiWeek_date <= end_date) %>%
    dplyr::select(epiWeek_date, all_of(type)) %>%
    dplyr::rename("date" = "epiWeek_date", "confirm" = type)
}
