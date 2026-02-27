# fit_epiestim_model - Function to estimate the reproduction number of an epidemic

A wrapper function for
[`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) from
the `EpiEstim` library to estimate the reproduction number of epidemics
to support short-term forecasts

## Usage

``` r
fit_epiestim_model(
  data,
  window_size = 7L,
  type = NULL,
  mean_si = NULL,
  std_si = NULL,
  recon_opt = "match",
  method = "parametric_si",
  mean_prior = NULL,
  std_prior = NULL
)
```

## Arguments

- data:

  *data frame* containing two columns: date and confirm (number of
  cases)

- window_size:

  *Integer* Length of the sliding windows used for R estimates.

- type:

  *character* Specifies type of epidemic. Must be one of "flu_a",
  "flu_b", "rsv", "sars_cov2" or "custom"

- mean_si:

  *Numeric* User specification of mean of parametric serial interval

- std_si:

  *Numeric* User specification of standard deviation of parametric
  serial interval

- recon_opt:

  Not implemented. One of "naive" or "match" to pass on to
  [`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) (see
  help page)

- method:

  One of "non_parametric_si", "parametric_si", "uncertain_si",
  "si_from_data" or "si_from_sample" to pass on to
  [`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) (see
  help page)

- mean_prior:

  *Numeric* positive number giving the mean of the common prior
  distribution for all reproduction numbers

- std_prior:

  *Numeric* positive number giving the standard deviation of the common
  prior distribution for all reproduction numbers

## Value

Object of class
[`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) (see
`EpiEstim` help page)

## Details

`fit_epiestim_model` currently supports the following epidemics:
Influenza, RSV and COVID-19. The default serial intervals for the
estimation of R were retrieved from Cowling et al., 2011, Vink et al.,
2014 and Madewell et al., 2023 for Influenza A, Influenza B, RSV and
COVID (BA.5 Omicron variant) respectively
