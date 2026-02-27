# Forecast daily epidemic cases using EpiEstim

This function prepares epidemic data, estimates the reproduction number
(\\R_t\\) using
[`fit_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/fit_epiestim_model.md),
and produces short-term forecasts of daily confirmed cases with
[`project_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/project_epiestim_model.md).

It removes early periods with no cases, checks data validity, optionally
smooths the epidemic curve, and then generates forward projections of
cases for a specified number of days.

## Usage

``` r
generate_forecast(
  data,
  start_date,
  window_size = 7,
  n_days = 7,
  type = NULL,
  smooth_data = FALSE,
  smoothing_cutoff = 10,
  ...
)
```

## Arguments

- data:

  *data frame* Must contain two columns:

  - `date`: observation dates

  - `confirm`: daily confirmed cases

- start_date:

  *Date* Date after which the epidemic is considered to have started.
  Data before this date is removed.

- window_size:

  *Integer* Length of the sliding window (in days) used for reproduction
  number estimation. Default is 7.

- n_days:

  *Integer* Number of future days to forecast. Default is 7.

- type:

  *character* Type of epidemic. Must be one of `"flu_a"`, `"flu_b"`,
  `"rsv"`, `"sars_cov2"`, or `"custom"`. Passed to
  [`fit_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/fit_epiestim_model.md).

- smooth_data:

  *logical* Whether to smooth the input daily case counts before
  estimation. Default is `FALSE`.

- smoothing_cutoff:

  *Integer* Cutoff parameter for smoothing. Only used if
  `smooth_data = TRUE`. Default is 10.

- ...:

  Additional arguments passed to
  [`fit_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/fit_epiestim_model.md).

## Value

A data frame of forecasted daily incidence with columns:

- `date`: date of forecast

- `p50`, `p25`, `p75`, `p025`, `p975`: forecast quantiles

- `min_sim`, `max_sim`: forecast range

## Details

- Data prior to the first non-zero `confirm` value is excluded.

- Input is checked for validity (sufficient days, proper format).

- If smoothing is enabled, case counts are adjusted before fitting.

- Forecasts are generated from the fitted EpiEstim model and returned
  with quantiles (2.5%, 25%, 50%, 75%, 97.5%), minimum, and maximum.

## See also

[`fit_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/fit_epiestim_model.md)
for reproduction number estimation,
[`project_epiestim_model`](https://bccdc-phsa.github.io/ViroReportR/reference/project_epiestim_model.md)
for forward simulations.

## Examples

``` r
# Create sample test rsv data
disease_type <- "rsv"
test_data <- simulate_data()
formatted_data <- get_aggregated_data(
  test_data,
  number_column = disease_type,
  date_column = "date",
  start_date = "2024-04-01",
  end_date = "2024-05-01"
)

# Run a 7 day forecast with smoothing
res_smooth <- generate_forecast(
  data = formatted_data,
  start_date = "2024-04-01",
  n_days = 7,
  type = "rsv",
  smooth_data = FALSE
)
```
