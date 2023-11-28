## code to prepare `weekly_transformed_plover_data` dataset goes here
weekly_plover_data <- get_weekly_plover(plover_data)
disease_type <- "flu_a"
weekly_transformed_plover_data <- get_weekly_plover_by_date_type(
  weekly_plover_data = weekly_plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01")



usethis::use_data(weekly_transformed_plover_data, overwrite = TRUE)
