#' Generate forecast report - Function to generate and knit report of forecasts for all supported viral respiratory diseases (Flu-A, Flu-B, RSV and SARS-CoV2)
#'
#' @description Function to render a full season viral respiratory report as an HTML document in the directory of your choice. The report includes plots of the observed trends and
#' model produced forecasts
#'
#' @param output_dir specify path to output directory that HTML report should be rendered in
generate_forecast_report <- function(output_dir = NULL) {
  rmarkdown::render(here::here("inst/vriforecasting_report.Rmd"), output_dir = output_dir)
}
