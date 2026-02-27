# summarise a data frame `d` by groups along a `variable`

summarise a data frame `d` by groups along a `variable`

## Usage

``` r
create_quantiles(d, ..., variable = NULL)
```

## Arguments

- d:

  tibble data frame

- ...:

  group_by variables

- variable:

  string

## Value

Data frame containing sample quantiles at probabilities 0.05, 0.25,
0.50, 0.75 and 0.95
