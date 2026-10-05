# load package
library(ViroReportR)
library(dplyr)

# load in data
DATA_FOLDER_PATH <- "O:\\BCCDC\\Groups\\Data_Linked\\Archive\\PANDA\\nb0077\\New_Data"
files <- list.files(path = DATA_FOLDER_PATH, pattern = "*.csv")
disease_type <- stringr::str_extract(files, "^[a-z]+")
data_list <- list()

for (i in seq_along(files)) {
  data_list[[i]] <- readr::read_csv(file.path(DATA_FOLDER_PATH, files[i]))
  disease_type <- stringr::str_extract(files[i], "^[a-z]+")
  data_list[[i]]$disease_type <- disease_type
}

# merge data
data <- bind_rows(data_list)
data <- data %>%
  mutate(
    disease_type = if_else(disease_type == "flu", "flu_b",disease_type),
    confirm = if_else(is.na(cases), positive_cases, cases),
    date = if_else(is.na(surveillance_date), collection_date, surveillance_date)
  ) %>%
  select(date, confirm, disease_type)

readr::write_csv(data, file.path(DATA_FOLDER_PATH,"all_disease_data.csv"))

# run as report
# if(Sys.getenv("R_PLATFORM") == "x86_64-pc-linux-gnu"){
#   cdr_dir <- "/mnt/BCCDC/Depts"
# } else {
#   cdr_dir <- "O:/BCCDC/Groups"
# }
#
# surv_dir <- file.path(cdr_dir, "Lab/2019-nCoV/BC Labs Surveillance Indicators/surveillance")
#
# proj_dir <- file.path(surv_dir, "projects/vri_forecast_report")
#
#
# generate_forecast_report(
#   input_data_dir =  file.path(proj_dir, "data/vri_season_2025_2026.csv"),
#   # input filepath
#   output_dir = file.path(proj_dir, "output/test"),
#   # output directory
#   n_days = 7,
#   # number of days to forecast
#   validate_window_size = 7,
#   # number of days between each validation window
#   smooth = FALSE,
#   # logical indicating whether smoothing should be applied in the forecast
#   disease_season = list(
#     "flu_a" = c("2025-08-24", "2026-03-06"),
#     "rsv" = c("2025-08-24", "2026-03-06"),
#     "sars_cov2" = c("2025-08-24", "2026-03-06")
#   )
# )
