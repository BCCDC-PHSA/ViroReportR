# Clean and validate case count data for EpiEstim

This function prepares case count data for use with **EpiEstim** by
performing a series of validation and cleaning steps:

## Usage

``` r
clean_sample_data(data, start_date)
```

## Arguments

- data:

  A data frame containing at least the columns `"date"` and `"confirm"`.
  The `"date"` column should be of class `Date`, and `"confirm"` should
  be numeric.

- start_date:

  A `Date` (or date-convertible string) indicating the starting date for
  analysis. Must exist within the `"date"` column.

## Value

A cleaned data frame filtered from `start_date`, starting at the first
date with non-zero confirmed cases, and containing at least 14 days of
data.

## Details

1.  Ensures that the input data frame has the required columns: `"date"`
    and `"confirm"`.

2.  Confirms that the specified `start_date` exists in the data and
    filters the data to include only records on or after that date.

3.  Removes leading days before the first non-zero confirmed case.

4.  Verifies that the resulting dataset contains at least 14 valid days
    (as required for estimation).

This function is primarily intended as a preprocessing step for EpiEstim
modeling. It combines validation checks for input structure and time
coverage with minimal data cleaning logic to ensure robust downstream
estimation.
