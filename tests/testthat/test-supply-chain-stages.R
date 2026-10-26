# Guards the draft six-stage taxonomy (config/supply_chain_stages.yml) and its two mapping files
# against drift from the HS6 master and from each other. Nothing in the pipeline reads them yet.

stages_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

stages_config <- function() {
  yaml::read_yaml(file.path(stages_repo_root(), "config", "supply_chain_stages.yml"))
}

stages_read_csv <- function(name) {
  utils::read.csv(file.path(stages_repo_root(), "data", "reference", name),
    colClasses = "character", check.names = FALSE
  )
}

test_that("the taxonomy has six ordered stages that roll up to the three old ones", {
  cfg <- stages_config()
  ids <- vapply(cfg$stages, function(s) s$id, character(1))
  rollups <- vapply(cfg$stages, function(s) s$rollup, character(1))

  expect_equal(anyDuplicated(ids), 0)
  expect_equal(vapply(cfg$stages, function(s) s$order, integer(1)), seq_along(ids))
  expect_setequal(unique(rollups), c("Upstream", "Midstream", "Downstream"))
  expect_setequal(unlist(cfg$rollup), ids)
  for (old in names(cfg$rollup)) {
    expect_true(all(rollups[match(cfg$rollup[[old]], ids)] == old), info = old)
  }
})

test_that("each stage's Strategic Index weight is an equal share of its roll-up's weight", {
  cfg <- stages_config()
  w <- cfg$strategic_index_weights
  expect_equal(w$split, "equal_share")
  for (old in names(cfg$rollup)) {
    children <- cfg$rollup[[old]]
    expected <- w$rollup[[old]] / length(children)
    for (s in children) {
      expect_equal(w$stage[[s]], expected, tolerance = 1e-3, info = s)
    }
  }
})

test_that("every in-basket HS6 master row has exactly one stage", {
  cfg <- stages_config()
  ids <- vapply(cfg$stages, function(s) s$id, character(1))
  rollup_of <- stats::setNames(vapply(cfg$stages, function(s) s$rollup, character(1)), ids)

  master <- stages_read_csv("energy_hs6_master.csv")
  master <- master[toupper(master$excluded) != "TRUE", ]
  master$hs6 <- formatC(as.integer(master$hs6), width = 6, flag = "0")
  map <- stages_read_csv("energy_hs6_stage_map.csv")

  key <- function(tbl) paste(tbl$tech, tbl$sub_sector, tbl$hs6, sep = "|")
  expect_equal(anyDuplicated(key(map)), 0)
  expect_setequal(key(map), key(master))

  expect_true(all(map$stage %in% ids))
  expect_equal(unname(rollup_of[map$stage]), map$stage_rollup)
  expect_equal(map$rollup_changed == "TRUE", map$stage_rollup != map$supply_chain)

  enabling <- map$enabling_equipment == "TRUE"
  expect_true(all(map$serves_stage[enabling] %in% ids))
  expect_true(all(map$serves_stage[!enabling] == ""))
})

test_that("the index variable map uses only known stages and fits", {
  cfg <- stages_config()
  ids <- vapply(cfg$stages, function(s) s$id, character(1))
  vars <- stages_read_csv("index_variable_stage_map.csv")

  stages <- unlist(strsplit(vars$proposed_stage, "|", fixed = TRUE))
  expect_true(all(stages %in% c(ids, "all", "inherited", "none")))
  expect_true(all(vars$fit %in% c(
    "exact", "splittable", "spans", "replicated", "mislabeled", "inherited", "not_stage_specific"
  )))
})
