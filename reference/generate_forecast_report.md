# Generate Viral Respiratory Forecast Report

Generates a full-season forecast report for viral respiratory diseases
as an HTML document.

## Usage

``` r
generate_forecast_report(
  input_data_dir = NULL,
  output_dir = NULL,
  n_days = 7,
  validate_window_size = 7,
  smooth = FALSE,
  disease_season = NULL
)
```

## Arguments

- input_data_dir:

  Path to input CSV data. Must contain columns: `date`, `confirm`,
  `disease_type`. Allowed values for `disease_type`: `"flu_a"`,
  `"flu_b"`, `"rsv"`, `"sars_cov2"`, `"custom"`.

- output_dir:

  Path to output directory for the rendered HTML report.

- n_days:

  Number of days ahead to forecast. Default is 7.

- validate_window_size:

  The number of days between each validation window. Default is 7.

- smooth:

  Logical indicating whether smoothing should be applied in the
  forecast. Default is `TRUE`.

- disease_season:

  An optional named list specifying the seasonal date ranges for each
  disease. Each element should be either:

  - NULL (indicating no defined season), or

  - a two-date vector in "YY-MM-DD" format (e.g., c("2024-09-01",
    "2025-03-01")) defining the start and end of the season for that
    disease.

  For example: disease_season = list( flu_a = c("2024-09-01",
  "2025-03-01"), rsv = c("2024-09-01", "2025-03-01"), sars_cov2 = NULL )

  This will produce a report where influenza A and RSV seasons run from
  September 1, 2024 to March 1, 2025, while no season is defined for
  SARS-CoV-2.

## Value

Invisibly returns the path to the rendered HTML report.

## Examples

``` r
# \donttest{
data <- simulate_data(start_date = "2024-01-07", #starting Sunday
)
diseases <- c("flu_a", "rsv", "sars_cov2")
data$date <- lubridate::ymd(data$date)
vri_data_list <- purrr::set_names( purrr::map2( rep(list(data), length(diseases)),
                                  diseases,
                                  ~ get_aggregated_data(.x, "date", .y)
                                 ),
                            diseases
)
# Save the simulated data
df <- purrr::imap_dfr(
vri_data_list,
\(df, disease) dplyr::mutate(df, disease_type = disease)
)
tmp_dir <- tempdir() # temporary directory for example for saving data
data_path <- file.path(tmp_dir, "simulated_data.csv")
write.csv(df, data_path, row.names = FALSE)

output_path <- tempdir() # output directory for report (temporary as example)
generate_forecast_report(input_data_dir = data_path,
                         output_dir = output_path,
                         n_days = 7,
                         validate_window_size = 7,
                         smooth = FALSE)
#> [1] "/tmp/RtmpWuLDzf/vriforecasting_report.html"
# }
```
