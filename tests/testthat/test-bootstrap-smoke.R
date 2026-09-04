# testthat runs with the working directory set to tests/testthat, so the repo root is
# resolved from this file rather than from the working directory.
repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

testthat::test_that("bootstrap initializes repo and config options", {
  source(file.path(repo_root, "scripts", "utils", "bootstrap.R"))

  repo_root_opt <- getOption("opportunity_security.repo_root")
  testthat::expect_true(is.character(repo_root_opt) && nzchar(repo_root_opt))
  testthat::expect_true(dir.exists(repo_root_opt))
  testthat::expect_false(is.null(getOption("opportunity_security.config")))
  testthat::expect_true(exists("repo_root", envir = .GlobalEnv))
})
