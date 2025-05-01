# Run Package Data Transformation Function --------------------------------
set.seed(123)
test_data <- simulate_data()


# test data transformation function ---------------------------------------
test_that("weekly generic filtered column name correct", {
  weekly_aggregated_data <- get_aggregated_data(
    test_data,
    "date", "rsv"
  )

  expect_equal(colnames(weekly_aggregated_data), c("date", "confirm"))
})


# test function error handling --------------------------------------------

test_that("invalid date column name handling correct", {
  wrong_date_column <- "date_date"
  expect_error(
    get_aggregated_data(
      test_data,
      wrong_date_column, "flua",
      "2024-01-07", "2024-12-08"
    ),
    "invalid date column name, not found in the input data"
  )
})


test_that("invalid number column handling correct", {
  wrong_number_column <- "date_date"
  expect_error(
    get_aggregated_data(
      test_data,
      "date", wrong_number_column,
      "2024-01-07", "2024-12-08"
    ),
    "invalid number column name, not found in the input data"
  )
})

test_that("invalid start date handling correct", {
  wrong_start_date <- "2024-06-01"

  expect_error(
    get_aggregated_data(
      test_data,
      "date", "flua",
      wrong_start_date, "2024-03-20"
    ),
    "start date is later than the end date"
  )
})


# test function warning handling ------------------------------------------

test_that("non Sunday input start_date warning correct", {
  non_sunday <- "2024-01-01"

  expect_warning(
    get_aggregated_data(
      test_data,
      "date", "flua",
      non_sunday, "2024-12-08",
      unit = "week"
    ),
    "The input `start_date` doesn't coincide with start of a week, the aggregated data might include partial number of dates in the start week"
  )
})

test_that("non Sunday input end_date warning correct", {
  # this is a wednesday use lubridate::wday to check
  non_sunday <- "2024-12-04"

  expect_warning(
    get_aggregated_data(
      test_data,
      "date", "flua",
      "2024-01-07", non_sunday,
      unit = "week"
    ),
    "The input `end_date` doesn't coincide with start of a week, the aggregated data might include partial number of dates in the end week"
  )
})
