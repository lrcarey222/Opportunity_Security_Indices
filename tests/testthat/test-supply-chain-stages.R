# Guards the six-stage taxonomy (config/supply_chain_stages.yml), the R constants that carry it
# (R/utils/supply_chain_stages.R), the stage column of the HS6 master and the index variable map.

stages_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(stages_repo_root(), "R", "utils", "supply_chain_stages.R"))

stages_config <- function() {
  yaml::read_yaml(file.path(stages_repo_root(), "config", "supply_chain_stages.yml"))
}

stages_read_csv <- function(name) {
  utils::read.csv(file.path(stages_repo_root(), "data", "reference", name),
    colClasses = "character", check.names = FALSE
  )
}

stage_title <- function(id) paste0(toupper(substring(id, 1, 1)), substring(id, 2))

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

test_that("the R constants equal the YAML", {
  cfg <- stages_config()
  ids <- vapply(cfg$stages, function(s) s$id, character(1))
  rollups <- vapply(cfg$stages, function(s) s$rollup, character(1))

  expect_identical(SUPPLY_CHAIN_STAGES, stage_title(ids))
  expect_identical(unname(SUPPLY_CHAIN_STAGE_ROLLUP[stage_title(ids)]), rollups)
  w <- cfg$strategic_index_weights$stage
  expect_equal(unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[stage_title(names(w))]), unname(unlist(w)), tolerance = 1e-3)
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
      expect_equal(unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[stage_title(s)]), expected, info = s)
    }
  }
})

test_that("legacy labels expand onto the stages they cover", {
  tbl <- tibble::tibble(
    Country = c("A", "A", "B"),
    supply_chain = c("Upstream", "Downstream", "Assembly"),
    value = c(1, 2, 3)
  )
  out <- supply_chain_expand_legacy(tbl)

  expect_setequal(out$supply_chain[out$value == 1], c("Extraction", "Refining", "Recovery"))
  expect_identical(out$supply_chain[out$value == 2], "Deployment")
  expect_identical(out$supply_chain[out$value == 3], "Assembly")
  expect_error(supply_chain_expand_legacy(tibble::tibble(supply_chain = "Sideways")), "unknown")

  all_six <- supply_chain_expand_all(tibble::tibble(Country = "A", value = 1))
  expect_identical(sort(all_six$supply_chain), sort(SUPPLY_CHAIN_STAGES))
  expect_identical(supply_chain_stage_rollup(c("Refining", "Assembly", "x")), c("Upstream", "Midstream", NA))
  expect_error(assert_supply_chain_stages(c("Extraction", "Midstream")), "Midstream")
})

test_that("every in-basket HS6 master row has exactly one stage", {
  master <- stages_read_csv("energy_hs6_master.csv")
  in_basket <- toupper(master$excluded) != "TRUE"

  expect_true(all(master$stage[in_basket] %in% SUPPLY_CHAIN_STAGES))
  expect_true(all(master$stage[!in_basket] == ""))

  live <- master[in_basket, ]
  enabling <- live$enabling_equipment == "TRUE"
  expect_true(all(live$enabling_equipment %in% c("TRUE", "FALSE")))
  expect_true(all(live$serves_stage[enabling] %in% SUPPLY_CHAIN_STAGES))
  expect_true(all(live$serves_stage[!enabling] == ""))
  # Enabling equipment is finished equipment (Assembly) or its parts (Components).
  expect_true(all(live$stage[enabling] %in% c("Assembly", "Components")))

  # No code sits in two stages of one technology.
  per_code <- tapply(live$stage, paste(live$tech, live$hs6), function(s) length(unique(s)))
  expect_true(all(per_code == 1))
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

test_that("a figure at one manufacturing stage is copied onto the other; a pair keeps its own", {
  tbl <- data.frame(
    Country = c("A", "A", "A", "B", "B", "A"),
    tech = c("Batteries", "Solar", "Solar", "Batteries", "Wind", "Batteries"),
    supply_chain = c("Components", "Components", "Assembly", "Components", "Assembly", "Extraction"),
    variable = "Market Share",
    value = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    explanation = "share",
    stringsAsFactors = FALSE
  )
  out <- supply_chain_fill_manufacturing_pair(tbl, by = c("Country", "tech", "variable"))
  val <- function(country, tech, stage) out$value[out$Country == country & out$tech == tech & out$supply_chain == stage]

  # Batteries (Components only) and Wind (Assembly only) gain the other stage, by country.
  expect_equal(val("A", "Batteries", "Assembly"), 0.1)
  expect_equal(val("B", "Batteries", "Assembly"), 0.4)
  expect_equal(val("B", "Wind", "Components"), 0.5)
  # Solar has its own figure at each stage, and stages outside the pair are left alone.
  expect_equal(val("A", "Solar", "Components"), 0.2)
  expect_equal(val("A", "Solar", "Assembly"), 0.3)
  expect_equal(sum(out$supply_chain == "Extraction"), 1L)
  expect_equal(nrow(out), nrow(tbl) + 3L)
  copies <- out[seq_len(nrow(out)) > nrow(tbl), ]
  expect_true(all(grepl("^share; copied from (Components|Assembly)", copies$explanation)))
  expect_identical(out[seq_len(nrow(tbl)), "value"], tbl$value)

  # Idempotent: once both stages are present nothing more is copied.
  expect_equal(nrow(supply_chain_fill_manufacturing_pair(out, by = c("Country", "tech", "variable"))), nrow(out))
  expect_error(supply_chain_fill_manufacturing_pair(tbl, by = "Year"), "missing column")
})

test_that("cells and categories made only of manufacturing-pair copies are found across tables", {
  copied <- function(stage) paste0("share", SUPPLY_CHAIN_PAIR_COPY_MARKER, stage, ": the source has no separate figure")
  fd <- data.frame(
    Country = "A", tech = c("Green Hydrogen", "Green Hydrogen", "Batteries", "Batteries"),
    supply_chain = c("Assembly", "Components", "Components", "Assembly"),
    category = "Foreign Dependency", variable = "Market Share", value = 0.1,
    explanation = c("share", copied("Assembly"), "share", copied("Components")),
    stringsAsFactors = FALSE
  )
  trade <- data.frame(Country = "A", tech = "Batteries", supply_chain = "Assembly", category = "Trade",
                      variable = "market_share", value = 0.5, explanation = "HS 850760", stringsAsFactors = FALSE)
  no_text <- data.frame(Country = "A", tech = "Wind", supply_chain = "Components", category = "Trade", value = 1)

  cells <- supply_chain_pair_copy_cells(list(fd, trade, no_text))
  # Batteries Assembly has its own trade beside the copy; a table without explanations counts as own.
  expect_equal(cells, data.frame(tech = "Green Hydrogen", supply_chain = "Components", stringsAsFactors = FALSE))
  cats <- supply_chain_pair_copy_cells(list(fd, trade, no_text), by_category = TRUE)
  expect_equal(nrow(cats), 2L)
  expect_setequal(paste(cats$tech, cats$supply_chain, cats$category),
                  c("Green Hydrogen Components Foreign Dependency", "Batteries Assembly Foreign Dependency"))
  expect_equal(nrow(supply_chain_pair_copy_cells(list(trade))), 0L)
})
