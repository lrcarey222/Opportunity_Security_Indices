# Make the repo root and the pipeline config discoverable to any test that sources a
# pipeline script. Loaded automatically by testthat before the test files run.

local({
  repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

  # scripts/utils/bootstrap.R locates the repo by reading its own source path out of
  # sys.frame(1)$ofile, which is NULL when the file is sourced from inside a test_that()
  # block, so it stopped with "bootstrap.R could not determine its own source path".
  # resolve_repo_root() checks this option first, so setting it skips the fragile discovery
  # entirely. It also removes an ordering dependency: until now the tests that source a
  # pipeline script only passed if an earlier test file happened to set this first.
  if (is.null(getOption("opportunity_security.repo_root"))) {
    options(opportunity_security.repo_root = repo_root)
  }

  # The same bootstrap stops with "Config file not found" unless config/config.yml exists,
  # and that file is gitignored, so a fresh clone and CI can never have one. The bootstrap
  # honours OPSI_CONFIG, so point it at the tracked example; the other three configs it
  # reads (weights, missing_data, index_definition) are tracked and resolve on their own.
  #
  # Only set when there is no real config and no override already in the environment, so a
  # developer's own config.yml continues to win locally.
  if (!nzchar(Sys.getenv("OPSI_CONFIG")) &&
      !file.exists(file.path(repo_root, "config", "config.yml"))) {
    example <- file.path(repo_root, "config", "config.example.yml")
    if (file.exists(example)) {
      Sys.setenv(OPSI_CONFIG = example)
    }
  }
})
