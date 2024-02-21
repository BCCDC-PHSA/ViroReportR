## code to prepare `weekly_transformed_plover_data` dataset goes here
disease_type <- "flu_a"
weekly_transformed_plover_data <- get_weekly_plover_by_date_type(
  plover_data = plover_data,
  type = disease_type,
  start_date = "2022-10-01",
  end_date = "2022-12-01"
)



usethis::use_data(weekly_transformed_plover_data, overwrite = TRUE)
