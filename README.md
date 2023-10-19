
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vriforecasting

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/vriforecasting)](https://CRAN.R-project.org/package=vriforecasting)
<!-- badges: end -->

The goal of vriforecasting is to …

## Installation

You can install the development version of vriforecasting from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sempwn/vriforecasting")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(vriforecasting)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

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
