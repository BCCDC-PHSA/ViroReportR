## code to prepare `phrdw_data` dataset goes here

phrdw_data <- data.frame(
lis_date_collection = as.Date(c(
"2022-10-02", "2022-10-02",
"2022-10-03", "2022-10-03",
"2022-10-04", "2022-10-05",
"2022-10-05", "2022-10-06",
"2022-10-06", "2022-10-07",
"2022-10-07", "2022-10-08",
"2022-10-08", "2022-10-09",
"2022-10-09", "2022-10-10",
"2022-10-10", "2022-10-11",
"2022-10-11", "2022-10-12",
"2022-10-12", "2022-10-13",
"2022-10-13"
 )),
result_lab_name = c(
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital",
"BC Centre for Disease Control", "Vancouver General Hospital"
 ),
age_years = c(7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7),
sars_cov2 = c(15, 17, 30, 28, 41, 43, 71, 74, 80, 88, 90, 92, 95, 97, 98, 100, 102, 90, 78, 90, 98, 88, 82),
rsv = c(14, 16, 29, 27, 40, 42, 70, 73, 79, 87, 14, 16, 29, 27, 40, 42, 70, 73, 79, 87, 88, 89, 90),
flu_a = c(13, 15, 28, 26, 39, 41, 69, 72, 78, 86, 13, 15, 28, 26, 39, 41, 69, 72, 78, 86, 77, 78, 79),
flu_b = c(12, 14, 27, 25, 38, 40, 68, 71, 77, 85, 12, 14, 27, 25, 38, 40, 68, 71, 77, 85, 78, 80, 81)
 )

usethis::use_data(phrdw_data, overwrite = TRUE)
