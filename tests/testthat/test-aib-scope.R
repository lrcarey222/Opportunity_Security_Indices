repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "aib", "aib_scope.R"))

coal <- tibble::tibble(iso3 = c("USA", "DEU", "JPN", "IND", "CHN", "RUS"), tier = c("core", "core", "core", "partner", "none", "none"))
econ <- c("USA", "DEU", "JPN", "IND", "CHN", "RUS", "TWN", "CHE", "PAN")

test_that("a coalition set takes its tiers, and treat_as_allied only with include_treat_as_allied", {
  s <- aib_resolve_set(list(mode = "coalition", tiers = list("core", "partner")), NULL, econ, coal, "PAN", c("TWN", "CHE"))
  expect_setequal(s$iso, c("USA", "DEU", "JPN", "IND"))
  expect_length(s$added, 0)
  t <- aib_resolve_set(list(mode = "coalition", tiers = list("core", "partner"), include_treat_as_allied = TRUE), NULL, econ, coal, "PAN",
                       c("TWN", "CHE", "USA", "PAN"))
  # the trusted network: tiers plus treat_as_allied; a code already in the tiers or excluded is not added
  expect_setequal(t$iso, c("USA", "DEU", "JPN", "IND", "TWN", "CHE"))
  expect_equal(t$added, c("TWN", "CHE"))
  expect_equal(t$id, "coalition")
})

test_that("global, configured and comma-list sets ignore treat_as_allied and drop the exclusions", {
  g <- aib_resolve_set(list(mode = "global", include_treat_as_allied = TRUE), NULL, econ, coal, "PAN", "TWN")
  expect_setequal(g$iso, setdiff(econ, "PAN"))
  expect_length(g$added, 0)
  o <- aib_resolve_set(list(mode = "coalition", include_treat_as_allied = TRUE), "usa, PAN,che", econ, coal, "PAN", "TWN")
  expect_equal(o$id, "configured")
  expect_equal(o$iso, c("USA", "CHE"))
  expect_equal(aib_resolve_set(list(mode = "configured", countries = list("jpn")), NULL, econ, coal)$iso, "JPN")
  expect_error(aib_resolve_set(list(mode = "coalition"), NULL, econ, NULL), "coalition_tbl")
  expect_error(aib_resolve_set(list(mode = "nope"), NULL, econ, coal), "Unknown set mode")
})

test_that("config/aib.yml protects the trusted network: coalition tiers plus the strategic layer's treat_as_allied", {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))
  layer <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_layer.yml"))
  expect_equal(cfg$demand_scope$mode, "coalition")
  expect_true(isTRUE(cfg$demand_scope$include_treat_as_allied))
  expect_setequal(unlist(layer$allied$treat_as_allied), c("TWN", "CHE", "SVN", "ISL", "ARE"))
})
