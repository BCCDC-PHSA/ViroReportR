## code to prepare `plover_data` dataset goes here

plover_data <- data.frame(
 epiWeek_date = as.Date(c(
 "2022-10-02", "2022-10-09",
"2022-10-16", "2022-10-23", "2022-10-30",
"2022-11-06", "2022-11-13", "2022-11-20",
"2022-11-27", "2022-12-04"
 )),
epiWeek_year = c(
2022, 2022, 2022, 2022,
2022, 2022, 2022, 2022, 2022, 2022
 ),
Epiweek = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
flu_a = c(17, 19, 32, 38, 43, 45, 73, 88, 94, 105),
flu_b = c(24, 31, 39, 45, 50, 52, 68, 83, 89, 97)
 )


usethis::use_data(plover_data, overwrite = TRUE)
