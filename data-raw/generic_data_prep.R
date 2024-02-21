## code to prepare `generic_data` dataset goes here

generic_data <- data.frame(
 date_of_report = as.Date(c(
 "2022-10-10", "2022-10-10",
"2022-10-17", "2022-10-23", "2022-10-30",
 "2022-11-06", "2022-11-13", "2022-11-20",
"2022-11-27", "2022-11-28"
 )),
flu_a = c(7, 10, 32, 38, 43, 45, 73, 88, 44, 51),
flu_b = c(6, 13, 31, 45, 50, 52, 68, 83, 45, 49)
)

usethis::use_data(generic_data, overwrite = TRUE)
