# Plot Mean Rt with time index (dates)

Plot Mean Rt with time index (dates)

## Usage

``` r
plot_rt(forecast_results)
```

## Arguments

- forecast_results:

  is the output of `generate_forecast`.

## Value

Mean Rt with time index plot

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
forecast_results <- generate_forecast(
  data = formatted_data,
  start_date = "2024-04-01",
  n_days = 7,
  type = "rsv",
  smooth_data = FALSE
)
plot_rt(forecast_results)
```
