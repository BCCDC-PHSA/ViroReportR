# Compute Forecast Validation Metrics (SMAPE & MASE)

This function evaluates forecast accuracy across multiple validation
runs by computing two key performance metrics:

## Usage

``` r
generate_validation_metric(data, validation_res)
```

## Arguments

- data:

  A data frame used in
  [`generate_validation()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_validation.md),
  containing the **original training data** for the model. It must
  include:

  - `date`: Dates of the observed case data (class `Date`).

  - `confirm`: Numeric values of observed confirmed cases.

- validation_res:

  A list of forecast validation results, typically the output from
  [`generate_validation()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_validation.md).
  Each element should contain:

  - `forecast_res_quantiles`: A data frame with columns `date` and `p50`
    (median forecasted values).

  - `original_data`: A data frame representing the training data used
    for that forecast, with a `date` column.

## Value

A `tibble` (data frame) with one row per forecast result and the
following columns:

- `train_period`: Date range of the training period used for the
  forecast.

- `forecast_period`: Date range of the forecasted period.

- `smape`: Symmetric Mean Absolute Percentage Error between forecasted
  and actual values, rounded to two decimals.

- `mase`: Mean Absolute Scaled Error, rounded to two decimals.

## Details

- **Symmetric Mean Absolute Percentage Error (SMAPE)**: Measures
  relative forecast accuracy while remaining robust to zero values in
  the actual data.

- **Mean Absolute Scaled Error (MASE)**: Scales forecast errors relative
  to the in-sample one-step naïve forecast, allowing comparison across
  series with different scales.

For each forecast result, the function also reports the corresponding
training and forecast periods. Computation stops once the forecast
period reaches the maximum date in the model data.

- **SMAPE** is defined as: \$\$SMAPE = mean( \|F - A\| / ((\|A\| +
  \|F\|) / 2) )\$\$ where \\A\\ are actual values and \\F\\ are
  forecasts. It avoids division by zero and is suitable for count data
  with zeros.

- **MASE** compares the mean absolute forecast error against the mean
  absolute difference of successive actual: \$\$MASE = mean(\|A - F\|) /
  mean(\|diff(A)\|)\$\$

The function automatically excludes forecasts extending beyond the
latest date in the observed model data.

## See also

[`generate_validation()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_validation.md),
[`generate_forecast()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_forecast.md)

## Examples

``` r
data <- simulate_data()
formatted_data <- get_aggregated_data(data,"date", "flu_a", "2024-10-16", "2024-12-31")
start_date <- ("2024-10-16")
validation_results <- generate_validation(formatted_data, start_date, type ="flu_a")
generate_validation_metric(formatted_data, validation_results)
#>               train_period          forecast_period smape mase
#> 1 2024-10-19 to 2024-11-01 2024-11-02 to 2024-11-08  0.57 0.43
#> 2 2024-10-19 to 2024-11-08 2024-11-09 to 2024-11-15  1.85 9.43
#> 3 2024-10-19 to 2024-11-15 2024-11-16 to 2024-11-22  0.86 0.73
#> 4 2024-10-19 to 2024-11-22 2024-11-23 to 2024-11-29  1.71 0.61
#> 5 2024-10-19 to 2024-11-29 2024-11-30 to 2024-12-06  1.39 3.29
#> 6 2024-10-19 to 2024-12-06 2024-12-07 to 2024-12-13  1.78 0.71
#> 7 2024-10-19 to 2024-12-13 2024-12-14 to 2024-12-20  1.34 0.86
#> 8 2024-10-19 to 2024-12-20 2024-12-21 to 2024-12-27  1.19 0.46
```
