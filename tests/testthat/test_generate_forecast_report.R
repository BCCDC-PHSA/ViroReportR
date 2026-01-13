##########################################
# Test generate_forecast_report function #
##########################################

set.seed(123)
daily_simulated_data <- simulate_data(days=60,peaks=c("rsv"=80),
                                      amplitudes=c("rsv"=100),
                                      scales=c("rsv"=-0.001))
formatted_simulated_data <- daily_simulated_data
test_col_names <- formatted_simulated_data

names(test_col_names) <- c("date","case")
names(formatted_simulated_data) <- c("date","confirm")

formatted_simulated_data <- formatted_simulated_data %>%
  mutate(disease_type = "rsv")

# wrong disease name for testing
test_disease_type_name <- formatted_simulated_data %>%
  mutate(disease_type = "covid")


# temporary file
vri_temp_file_path <- tempfile()
test_col_names_temp <- tempfile()
test_disease_type_temp <- tempfile()

write.csv(formatted_simulated_data, file = vri_temp_file_path, row.names = FALSE)
write.csv(test_col_names, file = test_col_names_temp, row.names = FALSE)
write.csv(test_disease_type_name, file = test_disease_type_temp, row.names = FALSE)

# Test function error handling  --------------------------------

test_that("Input file disease_type values correct", {
  expect_error({
    output_file <- tempfile(fileext = ".html")

    generate_forecast_report(input_data_dir = test_disease_type_temp,
                             output_dir = output_file,
                             n_days = 7,
                             validate_window_size = 1,
                             smooth = TRUE,
                             disease_season = NULL)

    unlink(output_file)}
  )
})

test_that("Input file required columns correct", {
  expect_error({
    output_file <- tempfile(fileext = ".html")

    generate_forecast_report(input_data_dir = test_col_names_temp,
                           output_dir = output_file,
                           n_days = 7,
                           validate_window_size = 1,
                           smooth = TRUE,
                           disease_season = NULL)

    unlink(output_file)},
    "Input data is missing required columns: confirm, disease_type"
  )
})

# Test Disease_season
test_that("Disease season dates must be present in the input dataset", {
  expect_error({
    output_file <- tempfile(fileext = ".html")

    generate_forecast_report(input_data_dir = vri_temp_file_path,
                             output_dir = output_file,
                             n_days = 7,
                             validate_window_size = 1,
                             smooth = TRUE,
                             disease_season = list("rsv" = c("2024-11-01","2025-03-01"))
    )

    unlink(output_file)},
    "Invalid season dates for 'rsv': 2024-11-01, 2025-03-01. All dates must be present in the input dataset."
  )
})

# Test output report file
test_that("Generate forecast report successfully",{

  output_file <- tempdir()

  expect_no_error(generate_forecast_report(input_data_dir = vri_temp_file_path,
                                           output_dir = output_file,
                                           n_days = 7,
                                           validate_window_size = 1,
                                           smooth = TRUE,
                                           disease_season = NULL))

  expect_true(file.exists(output_file))

  unlink(output_file)
})



unlink(vri_temp_file_path)
unlink(test_col_names_temp)
unlink(test_disease_type_temp)
