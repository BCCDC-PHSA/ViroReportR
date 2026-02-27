# Plot a ribbon plot with each time horizon predictions against true values for validation

Plot a ribbon plot with each time horizon predictions against true
values for validation

## Usage

``` r
plot_validation(data, validation_res, pred_plot = "ribbon")
```

## Arguments

- data:

  A data frame used in
  [`generate_validation()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_validation.md),
  containing the original training data used for model fitting. Must
  include:

  - `date`: Dates of the observed data (class `Date`).

  - `confirm`: Numeric values of confirmed cases.

- validation_res:

  A list of forecast validation results, typically produced by
  [`generate_validation()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_validation.md).
  Each element should include:

  - `forecast_res_quantiles`: A data frame containing forecasted
    quantiles (`p025`, `p10`, `p25`, `p50`, `p75`, `p90`, `p975`, and
    `date`).

  - `estimate_R`: A list with estimated reproduction numbers (used for
    grouping).

  - `smoothed_data`: (optional) A data frame of smoothed observations,
    if smoothing was applied before forecasting.

- pred_plot:

  either `"ribbon"` or `"error_bar"` (by default) to produce either
  ribbon prediction plots or error_bar plots respectively

## Value

error_bar validation plot or ribbon validation plot for a specific
prediction horizon

## Examples

``` r
data <- simulate_data()
formatted_data <- get_aggregated_data(data,"date", "flu_a", "2024-10-16", "2024-12-31")
start_date <- ("2024-10-16")
validation_results <- generate_validation(formatted_data, start_date, type ="flu_a")
plot_validation(formatted_data, validation_results)

```
