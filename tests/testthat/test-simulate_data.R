test_that("simulate_data returns correct structure", {
  result <- simulate_data()

  expect_s3_class(result, "data.frame")
  expect_true("date" %in% names(result))
  expect_true(all(c("flua", "rsv", "covid") %in% names(result)))
  expect_equal(nrow(result), 366)  # days + 1
})

test_that("simulate_data works with custom parameters", {
  result <- simulate_data(days = 10,
                          peaks = c(flua = 5),
                          amplitudes = c(flua = 100),
                          scales = c(flua = -0.01))

  expect_equal(nrow(result), 11)
  expect_true("flua" %in% names(result))
  expect_false("rsv" %in% names(result))
  expect_false("covid" %in% names(result))
})

test_that("simulate_data throws error when names don't match", {
  expect_error(
    simulate_data(
      days = 10,
      peaks = c(flua = 5),
      amplitudes = c(flua = 100),
      scales = c(rsv = -0.01)
    ),
    regexp = "Missing name"
  )
})

test_that("output values are non-negative integers", {
  result <- simulate_data(days = 5)
  virus_data <- result[, c("flua", "rsv", "covid")]

  expect_true(all(virus_data >= 0))
  expect_true(all(virus_data == floor(virus_data)))
})
