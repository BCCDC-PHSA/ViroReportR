
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vriforecasting

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/vriforecasting)](https://CRAN.R-project.org/package=vriforecasting)
<!-- badges: end -->

The goal of `vriforecasting` is to provide a toolbox to conveniently
generate short-term forecasts (with accompanied diagnostics) for viral
respiratory diseases.

## Installation

`vriforecasting` depends on the latest version of the `EpiEstim` package
(2.4). Thus, this version of the package must be installed from GitHub
prior to installing the `vriforecasting` package using:

``` r
# install.packages("devtools")
install.packages('EpiEstim', repos = c('https://mrc-ide.r-universe.dev', 'https://cloud.r-project.org'))
```

You can then install the development version of `vriforecasting` from
[GitHub](https://github.com/) with:

``` r

devtools::install_github("sempwn/vriforecasting")
```

## Quick Start

`vriforecasting` can be used to generate short-term forecasts with
accompanied diagnostics in a few lines of code. We go through an example
here where the `EpiEstim` backend is used to generate forecasts of
Influenza-A.

``` r
library(vriforecasting)
```

We will use the PLOVER weekly data for Influenza A, which is included
with the `vriforecasting` package. We then use the `get_weekly_plover`
and `get_weekly_plover_by_date_type` functions to transform the PLOVER
data into a dataset with two columns: `date` and `confirm` in accordance
to format accepted by the model fitting functions.

``` r
weekly_plover_data <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
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
and weekly forecasts using weekly sliding windows using `EpiEstim`.

We can produce forecasts aggregated by week by setting
`aggregate_week = TRUE`. For the functionality, `n_days` must be a
multiple of 7. Thus, specifying `n_days = 14` when
`aggregate_week = TRUE` produces 14/7 i.e. 2-week ahead forecasts.

``` r
time_period_result_weekly <- forecast_time_period_epiestim(data = weekly_plover_date_type, 
start_date_str = "2022-10-02", n_days = 14, type = "flu_a",  aggregate_week = TRUE)
#> [1] "Current time period: 1 (2022-10-09)"
#> [1] "Current time period: 2 (2022-10-16)"
#> [1] "Current time period: 3 (2022-10-23)"
#> [1] "Current time period: 4 (2022-10-30)"
#> [1] "Current time period: 5 (2022-11-06)"
#> [1] "Current time period: 6 (2022-11-13)"
#> [1] "Current time period: 7 (2022-11-20)"
#> [1] "Current time period: 8 (2022-11-27)"
```

Finally, we can plot a validation plot using the `plotValidation`
function. We can plot 2 week ahead forecasts for example by setting the
`pred_horizon` argument.

``` r
plotValidation(time_period_result_weekly, pred_horizon_str = "2 week ahead", pred_plot = "ribbon")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="80%" style="display: block; margin: auto;" />

The object of class `forecast_time_period_epiestim` produced by the
`forecast_time_period_epiestim` function also has a customized `summary`
function. This function checks to see if the weekly data inputted fall
into the ranges of the prediction quantiles and issues a warning if this
is not the case. This can be a useful check to assess the forecasts
produced and the model fit along with the validation plot. It takes in
the same arguments as the `plotValidation` function above:

``` r
summary(time_period_result_weekly, pred_horizon_str = "2 week ahead")
#> Warning in summary.forecast_time_period_epiestim(time_period_result_weekly, :
#> Prediction percentile intervals do not cover some data-points in validation
#> fits. Some forecasts may not be reliable
#> $individual_quantiles
#> # A tibble: 6 × 7
#> # Groups:   weekly_date [6]
#>   weekly_date coverage                   weighted_diff confirm median.prediction
#>   <date>      <chr>                              <dbl>   <dbl>             <dbl>
#> 1 2022-10-23  50 and 95 percentile inte…           675      38                23
#> 2 2022-10-30  only 95 percentile interv…         15987      43               116
#> 3 2022-11-06  50 and 95 percentile inte…            75      45                50
#> 4 2022-11-13  50 and 95 percentile inte…           867      73                56
#> 5 2022-11-20  only 95 percentile interv…          4800      88                48
#> 6 2022-11-27  Outside 95 percentile int…         61347      94               237
#> # ℹ 2 more variables: `50 percentile interval` <glue>,
#> #   `95 percentile interval` <glue>
#> 
#> $quantile_summary
#> # A tibble: 3 × 2
#>   coverage                       count
#>   <chr>                          <int>
#> 1 50 and 95 percentile interval      3
#> 2 Outside 95 percentile interval     1
#> 3 only 95 percentile interval        2
#> 
#> $time_weighted_mspe
#> [1] 118.1461
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
