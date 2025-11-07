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

test_disease_type_name <- formatted_simulated_data %>%
  mutate(disease_type = "sars_cov2")


# temporary file
vri_temp_file_path <- tempfile()
test_col_names_temp <- tempfile()
test_disease_type_temp <- tempfile()

write.csv(formatted_simulated_data, file = vri_temp_file_path, row.names = FALSE)
write.csv(test_col_names, file = test_col_names_temp, row.names = FALSE)
write.csv(test_disease_type_name, file = test_disease_type_temp, row.names = FALSE)

# Test function error handling  --------------------------------

test_that("Input file required columns correct", {
  expect_error({
    output_file <- tempfile(fileext = ".html")

    generate_forecast_report(input_data_dir = test_col_names_temp,
                           output_dir = output_file,
                           n_days = 7,
                           validate_window_size = 1,
                           smooth = TRUE)

    unlink(output_file)},
    "Input data is missing required columns: confirm, disease_type"
  )
})

# Test output report file
test_that("Generate forecast report successfully",{

  output_file <- tempdir()

  expect_no_error(generate_forecast_report(input_data_dir = vri_temp_file_path,
                                           output_dir = output_file,
                                           n_days = 7,
                                           validate_window_size = 1,
                                           smooth = TRUE))

  expect_true(file.exists(output_file))

  unlink(output_file)
})

unlink(vri_temp_file_path)
unlink(test_col_names_temp)
unlink(test_disease_type_temp)
