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
