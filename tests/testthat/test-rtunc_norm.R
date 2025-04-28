test_that("Mean of samples from rtrunc_norm function converges to specified mean", {
  expect_equal(mean(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 5, tolerance = 1e-2)
})

test_that("SD of samples from rtrunc_norm function converges to specified sd", {
  expect_equal(sd(rtrunc_norm(10000, mean = 5, sd = 1, lower_lim = 0)), 1, tolerance = 1e-1)
})

test_that("rtrunc_norm function does not produce any negative numbers even with very high SD", {
  expect_true(all(rtrunc_norm(10000, mean = 0, sd = 50, lower_lim = 0) > 0))
})


