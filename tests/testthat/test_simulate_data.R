test_that("simulate_data returns correct structure", {
  result <- simulate_data()

  expect_s3_class(result, "data.frame")
  expect_true("date" %in% names(result))
  expect_true(all(c("flu_a", "rsv", "sars_cov2") %in% names(result)))
  expect_equal(nrow(result), 366)  # days + 1
})

test_that("simulate_data works with custom parameters", {
  result <- simulate_data(days = 10,
                          peaks = c(flu_a = 5),
                          amplitudes = c(flu_a = 100),
                          scales = c(flu_a = -0.01))

  expect_equal(nrow(result), 11)
  expect_true("flu_a" %in% names(result))
  expect_false("rsv" %in% names(result))
  expect_false("sars_cov2" %in% names(result))
})

test_that("simulate_data throws error when names don't match", {
  expect_error(
    simulate_data(
      days = 10,
      peaks = c(flu_a = 5),
      amplitudes = c(flu_a = 100),
      scales = c(rsv = -0.01)
    ),
    regexp = "Missing name"
  )
})

test_that("output values are non-negative integers", {
  result <- simulate_data(days = 5)
  virus_data <- result[, c("flu_a", "rsv", "sars_cov2")]

  expect_true(all(virus_data >= 0))
  expect_true(all(virus_data == floor(virus_data)))
})
