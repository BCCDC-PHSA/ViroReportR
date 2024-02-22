#################### Test fit_epiestim_model function ################################
# Daily PHRDW data
daily_phrdw_data <- get_phrdw_by_type_date_age(
  phrdw_data = phrdw_data,
   time_period = "daily",
    type = "rsv",
   start_date = "2022-10-01",
   end_date = "2022-11-01",
   start_age = 0,
   end_age = 10
   )
# Low incidence dummy data
weekly_plover_dup <- weekly_transformed_plover_data

weekly_plover_dup$confirm[1] <- 1

# Test function error handling  --------------------------------

test_that("Data frame input error handling correct", {
  expect_error(
    fit_epiestim_model(data = plover_data, type = "rsv"),
    "Must pass a data frame with two columns: date and confirm"
  )
})


test_that("Typo in type error correct", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "coviad"),
    "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)",
    fixed = TRUE
  )
})

test_that("Missing type error correct", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data),
    "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)",
    fixed = TRUE
  )
})

test_that("Type custom correctly checks that all custom arguments must not be null", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "custom", mean_si = 5.5),
  "Must specify mean_si, std_si, mean_prior and std_prior for type custom")
})


# Test function warning handling --------------------------------


test_that("Function throws a warning when daily data is input", {
  expect_warning(fit_epiestim_model(data = daily_phrdw_data, type = "rsv"),
               "Your data may not be weekly data. Please check input. We recommend only weekly data be input into EpiEstim for optimal performance", fixed = TRUE)
})

test_that("Function throws warning when the incidence at the start date is too low", {
  expect_warning(fit_epiestim_model(data = weekly_plover_dup, type = "flu_a"),
                 "Incidence is too low on the current start date. R estimation started from 2022-10-09 for an accurate estimate of the reproduction number with EpiEstim")
})

test_that("Types other than custom do not use custom arguments", {
  expect_warning(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a", mean_prior = 5.5),
               "Custom mean_si, std_s, mean_prior and std_prior can only be specified with type set to custom. Default config values were used")
})


# Test function output --------------------------------

test_that("Function does not produce any errors with appropriate weekly data", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a"), NA)
})

test_that("Function produces estimate_R object as output with all columns", {
  expect_equal(names(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a")), c(
    "R", "method", "si_distr", "SI.Moments", "dates", "I",
    "I_local", "I_imported"
  ))
})

test_that("Function automatically movies to date with highest incidence if first date is not sufficient", {
  expect_equal(length(suppressWarnings(fit_epiestim_model(data = weekly_plover_dup, type = "flu_a"))$I), 56)
})

test_that("Custom mean_si and std_si are used in place of default  when type is custom", {
  expect_equal(fit_epiestim_model(data = weekly_transformed_plover_data, type = "custom", mean_si = 5.5, std_si = 0.2, mean_prior = 2, std_prior = 1)$SI.Moments$Mean, 5.5)
})


# Cleaning up environment
rm(weekly_plover_data, disease_type, daily_phrdw_data, weekly_plover_dup)
