#' Generate Viral Respiratory Forecast Report
#'
#' Generates a full-season forecast report for viral respiratory diseases as an HTML document.
#'
#' @param input_data_dir Path to input CSV data. Must contain columns: `date`, `confirm`, `disease_type`.
#'   Allowed values for `disease_type`: `"flu_a"`, `"flu_b"`, `"rsv"`, `"sars_cov2"`, `"custom"`.
#' @param output_dir Path to output directory for the rendered HTML report.
#' @param n_days Number of days ahead to forecast. Default is 7.
#' @param smooth Logical indicating whether smoothing should be applied in the forecast. Default is `TRUE`.
#'
#' @return Invisibly returns the path to the rendered HTML report.
generate_forecast_report <- function(input_data_dir = NULL, output_dir = NULL, n_days = 7, smooth = TRUE) {

  # check that input_data_dir exists
  if (is.null(input_data_dir) || !file.exists(input_data_dir)) {
    stop("`input_data_dir` must be a valid path to a CSV file.")
  }

  # read data
  input_data <- read.csv(input_data_dir)

  # required columns
  required_cols <- c("date", "confirm", "disease_type")
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(paste("Input data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # check disease_type values
  allowed_disease_types <- c("flu_a", "flu_b", "rsv", "sars_cov2", "custom")
  invalid_types <- setdiff(unique(input_data$disease_type), allowed_disease_types)
  if (length(invalid_types) > 0) {
    stop(paste("Invalid disease_type values found:", paste(invalid_types, collapse = ", "),
               "\nAllowed values are:", paste(allowed_disease_types, collapse = ", ")))
  }

  # render report
  rmarkdown::render(
    here::here("inst/vriforecasting_report.Rmd"),
    output_dir = output_dir,
    params = list(n_days = n_days, filepath = input_data_dir)
  )
}

