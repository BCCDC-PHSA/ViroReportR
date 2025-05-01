#################### Test fit_epiestim_model function ################################
set.seed(123)
daily_simulated_data <- simulate_data(days=60,peaks=c("rsv"=80),
                                  amplitudes=c("rsv"=100),
                                  scales=c("rsv"=-0.001))
formatted_simulated_data <- daily_simulated_data
names(formatted_simulated_data) <- c("date","confirm")


# Test function error handling  --------------------------------

test_that("Data frame input error handling correct", {
  expect_error(
    fit_epiestim_model(data = daily_simulated_data, type = "rsv"),
    "Must pass a data frame with two columns: date and confirm"
  )
})


test_that("Typo in type error correct", {
  expect_error(fit_epiestim_model(data = formatted_simulated_data, type = "coviad"),
    "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)",
    fixed = TRUE
  )
})

test_that("Missing type error correct", {
  expect_error(fit_epiestim_model(data = formatted_simulated_data),
    "Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)",
    fixed = TRUE
  )
})

test_that("Type custom correctly checks that all custom arguments must not be null", {
  expect_error(fit_epiestim_model(data = formatted_simulated_data, type = "custom", mean_si = 5.5),
  "Must specify mean_si, std_si, mean_prior and std_prior for type custom")
})

test_that("Tring to use weekly data throws error", {
  expect_error(
    fit_epiestim_model(data = formatted_simulated_data, type = "rsv", dt=7L),
    "Weekly data not currently implmented. Use only daily data."
  )
})


# Test function warning handling --------------------------------



test_that("Types other than custom do not use custom arguments", {
  expect_warning(fit_epiestim_model(data = formatted_simulated_data, type = "flu_a", mean_prior = 5.5),
               "Custom mean_si, std_s, mean_prior and std_prior can only be specified with type set to custom. Default config values were used")
})


# Test function output --------------------------------

test_that("Function does not produce any errors with appropriate daily data", {
  expect_error(fit_epiestim_model(data = formatted_simulated_data, type = "flu_a"), NA)
})

test_that("Function produces estimate_R object as output with all columns", {
  expect_equal(names(fit_epiestim_model(data = formatted_simulated_data, type = "flu_a")), c(
    "R", "method", "si_distr", "SI.Moments", "dates", "I",
    "I_local", "I_imported"
  ))
})


test_that("Custom mean_si and std_si are used in place of default  when type is custom", {
  expect_equal(fit_epiestim_model(data = formatted_simulated_data,
                                  type = "custom", mean_si = 5.5, std_si = 0.2,
                                  mean_prior = 2, std_prior = 1)$SI.Moments$Mean,
               5.5)
})



