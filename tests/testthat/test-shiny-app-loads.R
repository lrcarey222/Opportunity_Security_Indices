test_that("shiny app loads", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")

  app_path <- file.path("shiny", "app.R")
  expect_true(file.exists(app_path))

  app_env <- new.env(parent = globalenv())
  source(app_path, local = app_env)

  expect_true(exists("ui", envir = app_env))
  expect_true(exists("server", envir = app_env))
  expect_true(exists("app", envir = app_env))
  expect_true(exists("dw_create_chart", envir = app_env))
  expect_s3_class(app_env$app, "shiny.appobj")
})
