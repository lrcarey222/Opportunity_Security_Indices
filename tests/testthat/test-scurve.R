test_that("median_scurve returns 0-1 values with NAs preserved", {
  x <- c(1, 2, 3, NA)
  out <- median_scurve(x)

  expect_length(out, length(x))
  expect_true(all(out[!is.na(out)] >= 0 & out[!is.na(out)] <= 1))
  expect_true(is.na(out[4]))
})

test_that("median_scurve handles single-value input", {
  out <- median_scurve(5)
  expect_equal(out, 0)
})
