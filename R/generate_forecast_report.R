#' Generate Viral Respiratory Forecast Report
#'
#' Generates a full-season forecast report for viral respiratory diseases as an HTML document.
#'
#' @param input_data_dir Path to input CSV data. Must contain columns: `date`, `confirm`, `disease_type`.
#'   Allowed values for `disease_type`: `"flu_a"`, `"flu_b"`, `"rsv"`, `"sars_cov2"`, `"custom"`.
#' @param output_dir Path to output directory for the rendered HTML report.
#' @param n_days Number of days ahead to forecast. Default is 7.
#' @param validate_window_size The number of days between each validation window. Default is 7.
#' @param smooth Logical indicating whether smoothing should be applied in the forecast. Default is `TRUE`.
#' @param disease_season An optional named list specifying the seasonal date
#'   ranges for each disease. Each element should be either:
#'   - NULL (indicating no defined season), or
#'   - a two-date vector in "YY-MM-DD" format (e.g., c("2024-09-01", "2025-03-01")) defining
#'     the start and end of the season for that disease.
#'
#'   For example:
#'   disease_season = list(
#'     flu_a     = c("2024-09-01", "2025-03-01"),
#'     rsv       = c("2024-09-01", "2025-03-01"),
#'     sars_cov2 = NULL
#'   )
#'
#'   This will produce a report where influenza A and RSV seasons run from
#'   September 1, 2024 to March 1, 2025, while no season is defined for SARS-CoV-2.
#' @import kableExtra cowplot
#' @return Invisibly returns the path to the rendered HTML report.
#' @export
#' @examples
#' \donttest{
#' data <- simulate_data(start_date = "2024-01-07", #starting Sunday
#' )
#' diseases <- c("flu_a", "rsv", "sars_cov2")
#' data$date <- lubridate::ymd(data$date)
#' vri_data_list <- purrr::set_names( purrr::map2( rep(list(data), length(diseases)),
#'                                   diseases,
#'                                   ~ get_aggregated_data(.x, "date", .y)
#'                                  ),
#'                             diseases
#' )

#' # Save the simulated data
#' df <- purrr::imap_dfr(
#' vri_data_list,
#' \(df, disease) dplyr::mutate(df, disease_type = disease)
#' )
#' tmp_dir <- tempdir() # temporary directory for example for saving data
#' data_path <- file.path(tmp_dir, "simulated_data.csv")
#' write.csv(df, data_path, row.names = FALSE)
#' 
#' output_path <- tempdir() # output directory for report (temporary as example)
#' generate_forecast_report(input_data_dir = data_path,
#'                          output_dir = output_path,
#'                          n_days = 7,
#'                          validate_window_size = 7,
#'                          smooth = FALSE)
#' }

generate_forecast_report <- function(input_data_dir = NULL,
                                     output_dir = NULL,
                                     n_days = 7,
                                     validate_window_size = 7,
                                     smooth = FALSE,
                                     disease_season = NULL) {

  # check that input_data_dir exists
  if (is.null(input_data_dir) || !file.exists(input_data_dir)) {
    stop("`input_data_dir` must be a valid path to a CSV file.")
  }

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Please install 'kableExtra' to generate this report.")
  }

  # read data
  input_data <- utils::read.csv(input_data_dir)

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

  # validate disease_season argument
  if (!is.null(disease_season)) {

    if (!is.list(disease_season) || is.null(names(disease_season))) {
      stop("disease_season must be a named list.")
    }

    diseases_in_data <- unique(input_data$disease_type)

    # names match the data
    invalid_names <- setdiff(names(disease_season), diseases_in_data)
    if (length(invalid_names) > 0) {
      stop(
        paste0(
          "Invalid disease name(s) in disease_season: ",
          paste(invalid_names, collapse = ", "),
          ". These disease types are not present in the data."
        )
      )
    }

    input_dates <- as.Date(input_data$date)

    # check each disease containing the disease season range
    for (d in names(disease_season)) {

      season <- disease_season[[d]]

      if (is.null(season)) next

      # must be a 2-element date vector
      if (!is.vector(season) || length(season) != 2) {
        stop(paste0("Season for '", d, "' must be a two-date vector or NULL."))
      }

      # Check both dates appear in the input data
      if (!all(as.Date(season) %in% input_dates)) {
        stop(
          paste0(
            "Invalid season dates for '", d, "': ",
            paste(season, collapse = ", "),
            ". All dates must be present in the input dataset."
          )
        )
      }
    }
  }

  template <- system.file("vriforecasting_report.Rmd", package = "ViroReportR") 
  stopifnot(nzchar(template)) 
  tmp_rmd <- tempfile(fileext = ".Rmd") 
  file.copy(template, tmp_rmd, overwrite = TRUE) 
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE) 
  out_file <- file.path(output_dir, "vriforecasting_report.html") 
  rendered_path <- rmarkdown::render( 
    input = tmp_rmd, 
    output_file = basename(out_file), 
    output_dir = dirname(out_file), 
    intermediates_dir = tempdir(), 
    clean = TRUE, 
    params = list( 
      n_days = n_days, 
      filepath = input_data_dir, 
      validate_window_size = validate_window_size, 
      smooth = smooth, 
      disease_season = disease_season 
      ), 
    envir = new.env(parent = globalenv()),
    quiet = TRUE ) 
  return(rendered_path) 
}

