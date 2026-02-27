# Extract Aggregated Weekly Generic Data

`get_aggregated_data()` performs data transformation in the following
steps:

1.  Group the weekly or daily data by date.

2.  Aggregate the number of confirmed cases by either day or week.

3.  Select only the date and confirmed cases column.

4.  Filter the data by given start and end date

The input dataframe `generic_data` must have the following columns:

- `<date name>`: date column (e.g. as.Date('2022-01-01')).

- `<cases count name>`: Confirmed Cases Count (e.g. 1, 2, ...).

Note that these columns can be defined in a generic name, and inputted
as the other two function parameters for data transformation
(`date_column`, `number_column`)

Assume the date column is the start of the epiweek.

## Usage

``` r
get_aggregated_data(
  generic_data,
  date_column,
  number_column,
  start_date = NULL,
  end_date = NULL,
  unit = "day"
)
```

## Arguments

- generic_data:

  the weekly generic data from `get_data()`

- date_column:

  date column name str

- number_column:

  cases count column name str

- start_date:

  start date string (e.g. '2022-01-01')(optional, default is NULL)

- end_date:

  end date string (e.g. '2022-12-31')(optional, default is NULL)

- unit:

  aggregation unit "day" or "week"

## Value

aggregated weekly data of the generic confirmed cases data (filtered by
date if any)

- date:

  Either day or week date

- confirm:

  number of confirmed cases

## Examples

``` r
sim_data <- simulate_data()
aggregated_data <- get_aggregated_data(
  sim_data,
  "date", "flu_a", "2024-10-16", "2024-12-31"
)
```
