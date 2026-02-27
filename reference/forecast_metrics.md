# Extract current forecast metrics: forecast prediction, percentile interval and Rt value

Extract current forecast metrics: forecast prediction, percentile
interval and Rt value

## Usage

``` r
forecast_metrics(time_period_result, iter = 10)
```

## Arguments

- time_period_result:

  output from `forecast_time_period`

- iter:

  number of MCMC iterations used to generate Rt posterior

## Value

dataframe of current forecast metrics
