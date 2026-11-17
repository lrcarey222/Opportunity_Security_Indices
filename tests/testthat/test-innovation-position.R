# Guards the innovation pillar (INN) of Competitive Position: sp_innovation_contrib() repeats the
# technology-level indexes at the family's stages, and sp_competitive_position() scores them as one
# more equally weighted family, with fill na (a country without innovation data keeps its other
# families' mean) and an optional inn_contrib (NULL leaves the EO/ES-only result unchanged).

inn_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

suppressPackageStartupMessages(library(dplyr))
source(file.path(inn_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(inn_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(inn_repo_root(), "R", "strategic_position", "competitive_position.R"))
source(file.path(inn_repo_root(), "R", "strategic_position", "innovation_position.R"))

inn_theme <- function() {
  data.frame(
    Country = c("A", "B", "A", "A", "C"),
    tech = c("Solar", "Solar", "Solar", "Critical Minerals", "Solar"),
    supply_chain = c("All", "All", "All", "Refining", "All"),
    category = "Innovation",
    variable = c("Patent Innovation Index", "Patent Innovation Index", "Public RD&D Index", "Public RD&D Index",
                 "US patent grants (fractional, inventor country)"),
    data_type = c("index", "index", "index", "index", "raw"),
    value = c(0.9, 0.2, 0.6, 0.4, 120),
    stringsAsFactors = FALSE
  )
}

eo_rows <- function(Country, value, stage = "Components") {
  data.frame(Country = Country, tech = "Solar", supply_chain = stage, category = "Trade", variable = "market_share",
             component_value = value, imputed = FALSE, stringsAsFactors = FALSE)
}

inn_cfg <- function(with_inn = TRUE) {
  fams <- list(
    export_position = list(stages = list("Components", "Assembly"),
                           variables = list(list(pillar = "EO", category = "Trade", variable = "market_share", fill = "na")))
  )
  if (with_inn) {
    fams$innovation_position <- list(
      stages = list("Components"),
      variables = list(
        list(pillar = "INN", category = "Innovation", variable = "Patent Innovation Index", fill = "na"),
        list(pillar = "INN", category = "Innovation", variable = "Public RD&D Index", fill = "na")
      )
    )
  }
  list(min_observed_variables = 1, min_countries_per_cell = 2, families = fams)
}

test_that("contrib keeps index rows only and repeats technology rows at the given stages", {
  inn <- sp_innovation_contrib(inn_theme(), c("Components", "Assembly"))
  expect_false(any(grepl("grants", inn$variable)))
  solar <- inn[inn$tech == "Solar", ]
  expect_equal(nrow(solar), 3 * 2)
  expect_setequal(unique(solar$supply_chain), c("Components", "Assembly"))
  expect_equal(inn$supply_chain[inn$tech == "Critical Minerals"], "Refining")
  expect_true(all(!inn$imputed))
  expect_equal(nrow(sp_innovation_contrib(inn_theme(), "Components", techs = "Solar")), 3)
  expect_error(sp_innovation_contrib(inn_theme(), "Midstream"))
})

test_that("innovation is one more equal family, fill na, only at its stages", {
  eo <- rbind(eo_rows(c("A", "B", "C"), c(0.2, 0.8, 0.5)), eo_rows(c("A", "B", "C"), c(0.2, 0.8, 0.5), "Assembly"))
  es <- eo[0, ]
  inn <- sp_innovation_contrib(inn_theme(), "Components", techs = "Solar")
  cp <- sp_competitive_position(eo, es, inn_cfg(), inn_contrib = inn)$cells
  comp <- cp[cp$supply_chain == "Components", ]
  a <- comp[comp$Country == "A", ]
  expect_equal(a$cp_innovation_position, mean(c(0.9, 0.6)))
  expect_equal(a$competitive_position_score, mean(c(0.2, 0.75)))
  # C has no innovation index: its score is its export family alone, not dragged to zero.
  c_row <- comp[comp$Country == "C", ]
  expect_true(is.na(c_row$cp_innovation_position))
  expect_equal(c_row$competitive_position_score, 0.5)
  # Assembly is not an innovation stage in this config.
  asm <- cp[cp$supply_chain == "Assembly" & cp$Country == "A", ]
  expect_equal(asm$competitive_position_score, 0.2)
})

test_that("no inn_contrib reproduces the EO/ES-only result; a family naming INN without data stops", {
  eo <- eo_rows(c("A", "B", "C"), c(0.2, 0.8, 0.5))
  es <- eo[0, ]
  base <- sp_competitive_position(eo, es, inn_cfg(with_inn = FALSE))$cells
  same <- sp_competitive_position(eo, es, inn_cfg(with_inn = FALSE), inn_contrib = NULL)$cells
  expect_equal(base, same)
  expect_error(sp_competitive_position(eo, es, inn_cfg()), "absent from the data")
})
