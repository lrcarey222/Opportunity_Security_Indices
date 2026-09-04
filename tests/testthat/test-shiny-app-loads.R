# testthat runs with the working directory set to tests/testthat, so the repo root is
# resolved from this file rather than from the working directory.
repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

test_that("shiny app loads", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")

  app_path <- file.path(repo_root, "shiny", "app.R")
  expect_true(file.exists(app_path))

  app_env <- new.env(parent = globalenv())
  # app.R resolves app_dir from its own source path and falls back to the working directory,
  # which under testthat is tests/testthat - so it looked for tests/testthat/R/helpers.R.
  withr::with_dir(repo_root, source(app_path, local = app_env))

  expect_true(exists("ui", envir = app_env))
  expect_true(exists("server", envir = app_env))
  expect_true(exists("app", envir = app_env))
  expect_true(exists("dw_create_chart", envir = app_env))
  expect_s3_class(app_env$app, "shiny.appobj")
})
