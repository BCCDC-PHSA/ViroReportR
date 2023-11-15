# Run Package Data Transformation Function --------------------------------

weekly_data <- get_weekly_data(generic_data, "date_of_report", "flu_a")

weekly_aggregated_data <- get_weekly_aggregated_data(
  weekly_data,
  "date_of_report", "flu_a",
  "2022-10-16", "2023-12-31"
)

# test data transformation function ---------------------------------------

test_that("weekly generic column name correct", {
  expect_equal(colnames(weekly_data), c("date_of_report", "flu_a"))
})


test_that("weekly generic filtered column name correct", {
  expect_equal(colnames(weekly_aggregated_data), c("date", "confirm"))
})


# test function error handling --------------------------------------------

test_that("invalid date column name handling correct", {
  wrong_date_column <- "date_date"
  expect_error(
    get_weekly_aggregated_data(
      weekly_data,
      wrong_date_column, "flu_a",
      "2022-10-16", "2023-12-31"
    ),
    "invalid date column name, not found in the input data"
  )
})


test_that("invalid number column handling correct", {
  wrong_number_column <- "date_date"
  expect_error(
    get_weekly_aggregated_data(
      weekly_data,
      "date_of_report", wrong_number_column,
      "2022-10-16", "2023-12-31"
    ),
    "invalid number column name, not found in the input data"
  )
})

test_that("invalid start date handling correct", {
  wrong_start_date <- "2023-01-01"

  expect_error(
    get_weekly_aggregated_data(
      weekly_data,
      "date_of_report", "flu_a",
      wrong_start_date, "2022-11-20"
    ),
    "start date is later than the end date"
  )
})
