# Compare Reproduction Number Estimates from Multiple EpiEstim Outputs

**\[deprecated\]**

This function is deprecated and may be removed in a future release.

This function creates a ggplot comparing estimated reproduction numbers
(\\R\\) over time from multiple EpiEstim outputs. Each input should be a
named object produced by EpiEstim containing an `estimate_R$R` data
frame.

## Usage

``` r
plot_R_fit_comparison(...)
```

## Arguments

- ...:

  Named EpiEstim output objects produced by
  [`generate_forecast()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_forecast.md).
  Each object should contain a data frame `estimate_R$R` with columns
  such as:

  - `t_start`, `t_end`: Time window indices

  - `Median(R)`: Median estimated reproduction number

  - `Quantile.0.025(R)` / `Quantile.0.975(R)`: 95% credible interval
    bounds

  - `Quantile.0.25(R)` / `Quantile.0.75(R)`: 50% credible interval
    bounds

## Value

A **ggplot2** object showing median \\R\\ estimates with 50% and 95%
credible intervals, colored by input source (i.e., name).

## See also

[`generate_forecast()`](https://bccdc-phsa.github.io/ViroReportR/reference/generate_forecast.md),
[`EpiEstim::estimate_R()`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html)
