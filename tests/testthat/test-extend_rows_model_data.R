test_that("extend_rows_model_data returns correct rows for typical case", {
  data <- dplyr::tibble(
    date = lubridate::ymd("2023-01-01") + 0:9,
    confirm = 1:10
  )

  result <- extend_rows_model_data(data, "2023-01-03", extension_interval = 2)

  expect_equal(nrow(result), 3)
  expect_equal(result$date, ymd(c("2023-01-03", "2023-01-04", "2023-01-05")))
  expect_equal(result$confirm, 3:5)
})

test_that("extend_rows_model_data errors if min_model_date not in data", {
  data <- dplyr::tibble(
    date = lubridate::ymd("2023-01-01") + 0:9,
    confirm = 1:10
  )

  expect_error(extend_rows_model_data(data, "2022-12-31", extension_interval = 2))
})

test_that("extend_rows_model_data sorts data internally", {
  data <- dplyr::tibble(
    date = c(lubridate::ymd("2023-01-02"), lubridate::ymd("2023-01-01"), lubridate::ymd("2023-01-03")),
    confirm = c(5, 1, 3)
  )

  result <- extend_rows_model_data(data, "2023-01-01", extension_interval = 2)

  expect_equal(result$date, lubridate::ymd(c("2023-01-01", "2023-01-02", "2023-01-03")))
})
