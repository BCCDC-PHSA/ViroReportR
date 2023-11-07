
# Run Package Data Transformation Function --------------------------------

weekly_plover_data <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")

# test data transformation function ---------------------------------------

test_that("weekly plover column name correct", {
  expect_equal(colnames(weekly_plover_data), c("epiWeek_date","epiWeek_year","Epiweek","flu_a","flu_b"))
})


test_that("weekly plover filtered column name correct", {
  expect_equal(colnames(weekly_plover_date_type), c("date", "confirm"))
})



# test function error handling --------------------------------------------

test_that("invalid disease type handling correct", {

  wrong_disease_type <- 'abc'

  expect_error(get_weekly_plover_by_date_type(
                  weekly_plover_data = weekly_plover_data,
                  type = wrong_disease_type,
                  start_date = "2022-10-01",
                  end_date = "2022-12-01"),
    "invalid disease type, available options: 'flu_a', 'flu_b'")
})


test_that("invalid start date handling correct", {

  wrong_start_date <- '2023-01-01'

  expect_error(get_weekly_plover_by_date_type(
    weekly_plover_data = weekly_plover_data,
    type = disease_type,
    start_date = wrong_start_date,
    end_date = "2022-02-01"),
    "start date is later than the end date")
})

