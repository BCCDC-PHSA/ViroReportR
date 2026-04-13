# Print out text output for ViroReportR report detailing current number of case visits, last value of Rt and corresponding intervals

Print out text output for ViroReportR report detailing current number of
case visits, last value of Rt and corresponding intervals

## Usage

``` r
current_forecast_text(time_period_result, n_days, ...)
```

## Arguments

- time_period_result:

  output from `forecast_time_period`

- n_days:

  The number of days to run simulations for. Defaults to 14

- ...:

  optional arguments to be passed on to `forecast_metrics`

## Value

current forecast metrics
