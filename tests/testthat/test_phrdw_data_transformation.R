# Run Package Data Transformation Function --------------------------------

weekly_phrdw_data <- get_weekly_phrdw(phrdw_data)

disease_type <- "rsv"

weekly_phrdw_type_date_age <- get_weekly_phrdw_by_type_date_age(
  weekly_phrdw_data = weekly_phrdw_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-11-01",
  start_age = 0,
  end_age = 10
)

# test data transformation function ---------------------------------------

test_that("weekly phrdw column name correct", {
  expect_equal(colnames(weekly_phrdw_data), c("date", "age_years", "sars_cov2", "rsv", "flu_a", "flu_b"))
})


test_that("weekly phrdw filtered column name correct", {
  expect_equal(colnames(weekly_phrdw_type_date_age), c("date", "confirm"))
})


# test function error handling --------------------------------------------

test_that("invalid disease type handling correct", {
  wrong_disease_type <- "abc"

  expect_error(
    get_weekly_phrdw_by_type_date_age(
      weekly_phrdw_data = weekly_phrdw_data,
      type = wrong_disease_type,
      start_date = "2022-10-01",
      end_date = "2022-11-01",
      start_age = 0,
      end_age = 10
    ),
    "invalid disease type, available options: 'sars_cov2', 'rsv', 'flu_a', 'flu_b'"
  )
})


test_that("invalid start date handling correct", {
  wrong_start_date <- "2024-01-01"

  expect_error(
    get_weekly_phrdw_by_type_date_age(
      weekly_phrdw_data = weekly_phrdw_data,
      type = disease_type,
      start_date = wrong_start_date,
      end_date = "2023-02-01",
      start_age = 0,
      end_age = 10
    ),
    "start date is later than the end date"
  )
})


test_that("invalid start age handling correct", {
  wrong_start_age <- 200

  expect_error(
    get_weekly_phrdw_by_type_date_age(
      weekly_phrdw_data = weekly_phrdw_data,
      type = disease_type,
      start_date = "2023-01-01",
      end_date = "2023-02-01",
      start_age = wrong_start_age,
      end_age = 10
    ),
    "start age is later than the end age"
  )
})


test_that("invalid start age data type handling correct", {
  wrong_start_age <- "123"

  expect_error(
    get_weekly_phrdw_by_type_date_age(
      weekly_phrdw_data = weekly_phrdw_data,
      type = disease_type,
      start_date = "2023-01-01",
      end_date = "2023-02-01",
      start_age = wrong_start_age,
      end_age = 10
    ),
    "start age or end age is not numeric"
  )
})
