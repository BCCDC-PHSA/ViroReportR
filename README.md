
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ViroReportR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ViroReportR)](https://CRAN.R-project.org/package=ViroReportR)
<!-- badges: end -->

<img src="man/figures/vri_forecasting_hexSticker.png" width="40%" />

The goal of `ViroReportR` is to provide a toolbox to conveniently
generate short-term forecasts (with accompanied diagnostics) for viral
respiratory diseases.

## Installation

`ViroReportR` depends on the latest version of the `EpiEstim` package
(2.4). Thus, this version of the package must be installed from GitHub
prior to installing the `ViroReportR` package using:

``` r
# install.packages("devtools")
install.packages('EpiEstim', repos = c('https://mrc-ide.r-universe.dev', 'https://cloud.r-project.org'))
```

You can then install the development version of `ViroReportR` from
[GitHub](https://github.com/) with:

``` r

devtools::install_github("BCCDC-PHSA/ViroReportR")
```

## Quick Start

`ViroReportR` can be used to generate short-term forecasts with
accompanied diagnostics in a few lines of code. We go through an example
here where the `EpiEstim` backend is used to generate forecasts of
Influenza-A.

``` r
library(ViroReportR)
#> ================================================
#> Welcome to ViroReportR! 
#> Please run `pkgdown::build_site(lazy = TRUE)` in your console 
#> to access documentation on the package website 
#> ================================================
```

We will use the PLOVER weekly data for Influenza A, which is included
with the `ViroReportR` package. We then use the `get_weekly_plover`
and `get_weekly_plover_by_date_type` functions to transform the PLOVER
data into a dataset with two columns: `date` and `confirm` in accordance
to format accepted by the model fitting functions.

``` r
disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  plover_data = plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")

head(weekly_plover_date_type)
#> # A tibble: 6 × 2
#>   date       confirm
#>   <date>       <dbl>
#> 1 2022-10-02      17
#> 2 2022-10-09      19
#> 3 2022-10-16      32
#> 4 2022-10-23      38
#> 5 2022-10-30      43
#> 6 2022-11-06      45
```

## Model fitting and forecasting over sliding windows

The `forecast_time_period_epiestim` can be used to produce both daily
and weekly forecasts using weekly sliding windows. For this example, we
produce forecasts using `EpiEstim` as the backend algorithm choice. The
other current choice for the forecasting algorithm is `EpiFilter` (WIP).

We can produce forecasts aggregated by week by setting
`time_period = weekly`. For the functionality, `n_days` must be a
multiple of 7. Thus, specifying `n_days = 14` when
`time_period = weekly` produces 14/7 i.e. 2-week ahead forecasts.

``` r
time_period_result_weekly <- forecast_time_period(data = weekly_plover_date_type, 
start_date = "2022-10-02", n_days = 14, type = "flu_a", time_period = "weekly" , algorithm = "EpiEstim")
#> [1] "Current time period: 1 (2022-10-09)"
#> [1] "Current time period: 2 (2022-10-16)"
#> [1] "Current time period: 3 (2022-10-23)"
#> [1] "Current time period: 4 (2022-10-30)"
#> [1] "Current time period: 5 (2022-11-06)"
#> [1] "Current time period: 6 (2022-11-13)"
#> [1] "Current time period: 7 (2022-11-20)"
#> [1] "Current time period: 8 (2022-11-27)"
```

Finally, we can plot a validation plot using the `plot_validation`
function. We can plot 2 week ahead forecasts for example by setting the
`pred_horizon` argument.

``` r
plot_validation(time_period_result_weekly, pred_horizon_str = "2 week ahead", pred_plot = "ribbon")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="80%" style="display: block; margin: auto;" />

The object of class `forecast_time_period` produced by the
`forecast_time_period` function also has a customized `summary`
function. This function checks to see if the weekly data inputted fall
into the ranges of the prediction quantiles and issues a warning if this
is not the case. This can be a useful check to assess the forecasts
produced and the model fit along with the validation plot. It takes in
the same arguments as the `plot_validation` function above:

``` r
summary(time_period_result_weekly, pred_horizon_str = "2 week ahead")
#> Warning in summary.forecast_time_period(time_period_result_weekly,
#> pred_horizon_str = "2 week ahead"): Prediction percentile intervals do not cover
#> some data-points in validation fits. Some forecasts may not be reliable
#> $individual_quantiles
#> # A tibble: 6 × 7
#> # Groups:   weekly_date [6]
#>   weekly_date coverage                   weighted_diff confirm median.prediction
#>   <date>      <chr>                              <dbl>   <dbl>             <dbl>
#> 1 2022-10-23  only 95 percentile interv…          1875      38                13
#> 2 2022-10-30  only 95 percentile interv…          3072      43                75
#> 3 2022-11-06  50 and 95 percentile inte…           363      45                34
#> 4 2022-11-13  only 95 percentile interv…          2352      73                45
#> 5 2022-11-20  only 95 percentile interv…          6912      88                40
#> 6 2022-11-27  Outside 95 percentile int…         43200      94               214
#> # ℹ 2 more variables: `50 percentile interval` <glue>,
#> #   `95 percentile interval` <glue>
#> 
#> $quantile_summary
#> # A tibble: 3 × 2
#>   coverage                       count
#>   <chr>                          <int>
#> 1 50 and 95 percentile interval      1
#> 2 Outside 95 percentile interval     1
#> 3 only 95 percentile interval        4
#> 
#> $time_weighted_mspe
#> [1] 98.12747
```

Finally, the `ViroReportR` package can conveniently generate an
automated report for the current season for all supported viral
respiratory diseases (Influenza-A, Influenza-B, RSV and SARS-CoV2) using
the `generate_forecast_report` function, which renders an HTML report.

``` r
generate_forecast_report(output_dir = "PATH OF DIRECTORY")
```

## Work flow

Try and use the following steps when working on features/issues and
fixing bugs (see this
[tutorial](https://rogerdudler.github.io/git-guide/) for using `git`):

- First run `git pull` to get the latest version of `master`
- Whenever possible, when working on a new feature or a bug make sure
  there is a corresponding issue open for it within Github. If it
  doesn’t exist then feel free to make one yourself.
- Create a new branch with a name related to the feature being worked on
  and then switch to it e.g. `git checkout -b feature_make_better_model`
- When working on the feature branch try and commit as often as possible
  and include references to the issue number being worked on
  e.g. `git commit -m "Re #42. Found answer to life, universe and everything."`
- Once feature is written or bug is fixed consider writing a test for it
  (see [tests](#tests) section for more details). Then run `pytest` in
  the console to check everything passes. If using a `conda`
  environment, first activate the environment and then run
  `python -m pytest`. If a test fails, try and resolve issue and re-run
  tests until everything passes.
- Once happy with the feature push the branch to Github by running a
  `git push` on the feature branch
  e.g. `git push origin feature_make_better_model`
- In Github create a pull request for the feature and assign a reviewer.
  Anyone else can review the code, choose whoever you think would be
  best to look over it.
- If the changes were substantial, document the changes along with a
  rationale on the [google document](#).
- As a reviewer, check the changes by performing a `git pull` on the
  branch. Check the differences and run the feature yourself. If
  satisfied, then perform the merge on Github referencing the issue.
  - If a merge conflict arises, then the reviewer can try to resolve or
    pass it back to the feature creator to resolve.
