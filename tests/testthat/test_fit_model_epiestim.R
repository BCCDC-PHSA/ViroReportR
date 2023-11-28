#################### Test fit_epiestim_model function ################################

# Run Package Data Transformation Function for PHRDW data --------------------------------

non_weekly_phrdw_data <- get_weekly_phrdw(phrdw_data) %>%
  group_by(date) %>%
  select(date, confirm = rsv)

# Low incidence dummy data
weekly_plover_dup <- weekly_transformed_plover_data

weekly_plover_dup$confirm[1] <- 8

# Test function error handling  --------------------------------

test_that("Data frame input error handling correct", {
 expect_error(fit_epiestim_model(data = plover_data, type = "rsv"),
    "Must pass a data frame with two columns: date and confirm")
})

test_that("Error is correctly thrown when type other is specified and mean and std of si are null", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "other"),
               "Must specify mean and standard deviation of parametric serial interval for type other")
})

test_that("Typo in type error correct", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "coviad"),
               "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or other)", fixed = TRUE)
})

test_that("Missing type error correct", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data),
               "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or other)", fixed = TRUE)
})


# Test function warning handling --------------------------------

test_that("Function throws a warning when daily data is input and dt = 7L", {
  expect_warning(fit_epiestim_model(data = non_weekly_phrdw_data, type = "rsv"),
                 "Your data may not be weekly data. Please check input and consider changing dt argument (dt = 1L for daily data)", fixed = TRUE)
})

test_that("Function throws the right warning when the incidence at the start date is too low", {
  expect_warning(fit_epiestim_model(data = weekly_plover_dup, type = "flu_a"))
})


# Test function output --------------------------------

test_that("Function does not produce any errors with appropriate weekly data", {
  expect_error(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a"), NA)
})

test_that("Function does not produce any warnings with appropriate weekly data", {
  expect_warning(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a"), NA)
})

test_that("Function produces estimate_R object as output with all columns", {
  expect_equal(names(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a")), c("R", "method", "si_distr", "SI.Moments", "dates", "I",
                                                                                     "I_local", "I_imported"))
})

test_that("Custom mean_si and std_si are used in place of default for covid, rsv and flu as well when args are not null", {
  expect_equal(fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a", mean_si = 5.5, std_si = 0.2)$SI.Moments$Mean, 5.5)
})

# Cleaning up environment
rm(weekly_plover_data, disease_type, non_weekly_phrdw_data, weekly_plover_dup)
