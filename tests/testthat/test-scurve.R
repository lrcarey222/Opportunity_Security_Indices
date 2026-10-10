# Source the function under test, so the file also runs on its own (it relied on an earlier
# test file having sourced R/utils/scurve.R).
source(normalizePath(test_path("..", "..", "R", "utils", "scurve.R"), winslash = "/", mustWork = TRUE))

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

test_that("median_scurve_ref ranks the reference alone and places other values against it", {
  x <- c(3, 1, 2, 10, 2, NA, -5)
  in_ref <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA)
  out <- median_scurve_ref(x, in_ref)

  # Reference members: exactly median_scurve() over the reference values.
  expect_equal(out[1:3], median_scurve(c(3, 1, 2)))
  # Others: the percentile rank each would take if added alone to the reference
  # (below + half of ties) / reference size, then the same S-curve.
  s <- function(r) sqrt(r) / (sqrt(r) + sqrt(1 - r))
  expect_equal(out[4], 1)
  expect_equal(out[5], s(1.5 / 3))
  expect_equal(out[7], 0)
  expect_true(is.na(out[6]))
  # An NA flag counts as outside the reference.
  expect_false(is.na(out[7]))

  # Values outside the reference cannot move a reference value.
  moved <- median_scurve_ref(replace(x, 4:5, c(-100, 100)), in_ref)
  expect_equal(moved[1:3], out[1:3])

  # The same value as median_scurve() over reference + that one value.
  expect_equal(out[5], median_scurve(c(3, 1, 2, 2))[4])

  # With no reference value every result is NA; a bad flag vector is an error.
  expect_true(all(is.na(median_scurve_ref(c(1, 2), c(FALSE, FALSE)))))
  expect_error(median_scurve_ref(c(1, 2), TRUE), "logical vector")
})
