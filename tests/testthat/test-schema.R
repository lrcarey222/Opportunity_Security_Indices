test_that("validate_schema accepts valid data", {
  tbl <- data.frame(
    Country = "A",
    tech = "Solar",
    supply_chain = "Upstream",
    category = "Trade",
    variable = "Metric",
    data_type = "raw",
    value = 1.2,
    Year = 2024,
    source = "Test",
    explanation = "Example",
    stringsAsFactors = FALSE
  )

  expect_invisible(validate_schema(tbl))
})

test_that("validate_schema errors on missing columns", {
  tbl <- data.frame(
    Country = "A",
    stringsAsFactors = FALSE
  )

  expect_error(validate_schema(tbl), "Missing required columns")
})
