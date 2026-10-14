fixture_path <- function(...) {
  # testthat runs with the working directory set to tests/testthat, so resolve fixtures
  # relative to the test file. The repo-relative form is kept as a fallback for callers
  # that source these helpers from the project root.
  from_test_file <- tryCatch(testthat::test_path("..", "fixtures", ...), error = function(e) NULL)
  if (!is.null(from_test_file) && file.exists(from_test_file)) {
    return(from_test_file)
  }

  file.path("tests", "fixtures", ...)
}

read_fixture_csv <- function(name) {
  read.csv(fixture_path(name), stringsAsFactors = FALSE)
}

read_fixture_yaml <- function(name) {
  yaml::read_yaml(fixture_path(name))
}

set_test_index_definition <- function() {
  index_definition <- read_fixture_yaml("index_definition.yml")
  options(opportunity_security.index_definition = index_definition)
  index_definition
}
