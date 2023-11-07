#################### Test fit_epiestim_model function ################################
weekly_phrdw_type_date_age <- get_weekly_phrdw_by_type_date_age(
  weekly_phrdw_data = weekly_phrdw_data,
  type = disease_type,
  start_date = "2023-01-01",
  end_date = "2023-02-01",
  start_date = "2022-10-01",
  end_date = "2022-11-01",
  start_age = 0,
  end_age = 10)

get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = wrong_disease_type,
  start_date = "2022-01-01",
  end_date = "2022-02-01"),
start_date = "2022-10-01",
end_date = "2022-12-01")

# Run for weekly data
plover_mod <- fit_epiestim_model(data = weekly_plover_data, type = "flu_a")

# Test function error handling

test_that("Data frame input error handling correct", {
 expect_error(fit_epiestim_model(data = plover_data, type = "rsv"),
    "Must pass a data frame with two columns: date and confirm")
})

test_that("Other and null mean and standard deviation arguments errors correct", {
  expect_error(fit_epiestim_model(data = weekly_plover_data, type = "other"),
               "Must specify mean and standard deviation of parametric serial interval for type other")
})

test_that("Typo in type error correct", {
  expect_error(fit_epiestim_model(data = weekly_plover_data, type = "coviad"))
})

test_that("Missing type error correct", {
  expect_error(fit_epiestim_model(data = weekly_plover_data))
})


# Test function warning handling

test_that("Function throws a warning when daily data is input and dt = 7L", {
  expect_error(fit_epiestim_model(data = weekly_plover_data))
})

test_that("Missing type error correct", {
  expect_error(fit_epiestim_model(data = weekly_plover_data))
})

# Test function output

test_that("Object of class estimate_R is obtained", {
  expect_equal(class(plover_mod), "estimate_R")
})
