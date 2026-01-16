fixture_path <- function(...) {
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
