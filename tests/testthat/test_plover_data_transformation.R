

# load plover data --------------------------------------------------------

data_folder <- "O:/BCCDC/Groups/Analytics/Projects/covid_modeling/06 Projects/RSV Flu Modelling/Data"

data_filename <- "plover_weekly_resp.rds"

plover_data_file <- file.path(data_folder, data_filename)
plover_data <- readRDS(plover_data_file)


# Run Package Data Transformation Function --------------------------------

weekly_plover <- get_weekly_plover(plover_data)

disease_type <- "flu_a"
weekly_plover_date_type <- get_weekly_plover_by_date_type(
                              weekly_plover,
                               type = disease_type,
                               start_date = "2022-01-01",
                               end_date = "2022-02-01")

# test data transformation function ---------------------------------------

test_that("weekly plover column name correct", {
  expect_equal(colnames(weekly_plover), c("epiWeek_date","epiWeek_year","Epiweek","flu_a","flu_b"))
})


test_that("weekly plover filtered column name correct", {
  expect_equal(colnames(weekly_plover_date_type), c("date", "confirm"))
})
