# The partner-level PSI from the coalition's and from every reporter's viewpoint
# (build_partnership_strength_viewpoints(), R/indices/build_partnership_strength_index.R).

ppv_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(ppv_root, "R", "utils", "scurve.R"))
source(file.path(ppv_root, "R", "utils", "country.R"))
source(file.path(ppv_root, "R", "utils", "schema.R"))
source(file.path(ppv_root, "R", "utils", "levels.R"))
source(file.path(ppv_root, "R", "indices", "index_builder_core.R"))
source(file.path(ppv_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(ppv_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))

# Reporters USA and JPN (coalition) and CHN (not); partners IND, VNM, USA, JPN.
ppv_top5 <- function(category, variable, values) {
  tibble::tibble(
    reporter = c("United States", "Japan", "China", "United States", "Japan", "China", "United States", "China", "Japan", "China"),
    partner = c("India", "India", "India", "Viet Nam", "Viet Nam", "Viet Nam", "Japan", "Japan", "United States", "United States"),
    reporter_iso = c("USA", "JPN", "CHN", "USA", "JPN", "CHN", "USA", "CHN", "JPN", "CHN"),
    partner_iso = c("IND", "IND", "IND", "VNM", "VNM", "VNM", "JPN", "JPN", "USA", "USA"),
    category = category,
    variable = variable,
    data_type = "index",
    value = values,
    Year = 2024L,
    source = "test",
    explanation = "test"
  )
}

ppv_inputs <- function() {
  list(
    fs = ppv_top5("friendshore", "Friendshore Index", c(0.8, 0.6, 0.1, 0.5, 0.5, 0.9, 0.7, 0.2, 0.9, 0.3)),
    op = ppv_top5("opportunity", "Opportunity Index", c(0.4, 0.6, 0.2, 0.3, 0.5, 1.0, 0.6, 0.4, 0.8, 0.2)),
    dv = tibble::tibble(
      partner = c("India", "Viet Nam"),
      partner_iso = c("IND", "VNM"),
      category = "development",
      variable = "Development Potential Index",
      data_type = "index",
      value = c(0.5, 0.7),
      Year = 2024L,
      source = "test",
      explanation = "test"
    )
  )
}

ppv_w <- list(friendshore = 0.4, opportunity = 0.4, development = 0.2)

test_that("the coalition view averages the dyad pillars over coalition reporters only", {
  x <- ppv_inputs()
  views <- build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = c("USA", "JPN"))
  coal <- stats::setNames(views$coalition$index$Partnership_Strength_Index, views$coalition$index$Country)
  world <- stats::setNames(views$world$index$Partnership_Strength_Index, views$world$index$Country)

  # India: coalition FS mean(0.8, 0.6), OP mean(0.4, 0.6); world adds China's ratings.
  expect_equal(coal[["India"]], 0.4 * 0.7 + 0.4 * 0.5 + 0.2 * 0.5)
  expect_equal(world[["India"]], 0.4 * 0.5 + 0.4 * 0.4 + 0.2 * 0.5)
  # Viet Nam ranks above India from China's side, below it from the coalition's.
  expect_gt(world[["Viet Nam"]], world[["India"]])
  expect_lt(coal[["Viet Nam"]], coal[["India"]])
  # A coalition member is rated by the other members only (its self-dyad is not in the tables).
  expect_equal(coal[["Japan"]], (0.4 * 0.7 + 0.4 * 0.6) / 0.8)
  expect_equal(coal[["United States"]], (0.4 * 0.9 + 0.4 * 0.8) / 0.8)

  cc <- views$coalition$category_contributions
  expect_equal(cc$n_obs[cc$Country == "India" & cc$category == "friendshore"], 2)
  expect_equal(views$world$category_contributions$n_obs[
    views$world$category_contributions$Country == "India" & views$world$category_contributions$category == "friendshore"
  ], 3)
  expect_equal(views$coalition$reporters, c("USA", "JPN"))
  expect_null(views$world$reporters)
})

test_that("the world view is the PSI as built before the viewpoint split", {
  x <- ppv_inputs()
  views <- build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = "USA")
  old <- build_partnership_strength_index(
    list(
      friendshore = dplyr::select(psi_prepare_partner_top5(x$fs), -partner_iso),
      opportunity = dplyr::select(psi_prepare_partner_top5(x$op), -partner_iso),
      development = dplyr::select(psi_prepare_partner_top5(x$dv), -partner_iso)
    ),
    ppv_w
  )
  expect_equal(views$world$index, old$index)
})

test_that("coalition members missing from the dyad tables are named; none at all stops", {
  x <- ppv_inputs()
  expect_warning(
    views <- build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = c("USA", "JPN", "DEU")),
    "DEU"
  )
  expect_equal(nrow(views$coalition$index), 4)
  expect_error(
    build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = "DEU"),
    "No coalition member"
  )
  expect_error(build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = character()), "empty")
})

test_that("psi_prepare_partner_top5 filters reporters of dyad tables and passes partner tables through", {
  x <- ppv_inputs()
  p <- psi_prepare_partner_top5(x$fs, reporters = "CHN")
  expect_equal(nrow(p), 4)
  expect_true(all(p$tech == "All" & p$supply_chain == "All"))
  expect_equal(nrow(psi_prepare_partner_top5(x$dv, reporters = "CHN")), 2)
  # A partner without a name is keyed by its ISO3.
  x$fs$partner[1] <- NA
  expect_equal(psi_prepare_partner_top5(x$fs)$Country[1], "IND")
})

test_that("the full-coalition view averages over core and partner reporters when asked", {
  x <- ppv_inputs()
  views <- build_partnership_strength_viewpoints(
    x$fs, x$op, x$dv, ppv_w,
    coalition_iso3 = "USA",
    full_coalition_iso3 = c("USA", "JPN")
  )
  expect_setequal(names(views), c("coalition", "world", "full_coalition"))
  core <- stats::setNames(views$coalition$index$Partnership_Strength_Index, views$coalition$index$Country)
  full <- stats::setNames(views$full_coalition$index$Partnership_Strength_Index, views$full_coalition$index$Country)
  expect_equal(core[["India"]], 0.4 * 0.8 + 0.4 * 0.4 + 0.2 * 0.5)
  expect_equal(full[["India"]], 0.4 * 0.7 + 0.4 * 0.5 + 0.2 * 0.5)
  expect_equal(views$full_coalition$reporters, c("USA", "JPN"))
  # Without full_coalition_iso3 the result is unchanged (two views).
  expect_setequal(names(build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = "USA")), c("coalition", "world"))
  expect_error(
    build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppv_w, coalition_iso3 = "USA", full_coalition_iso3 = character()),
    "full_coalition_iso3 is empty"
  )
})

test_that("scripts/25 writes the core-coalition view as the headline and the full-coalition and world views beside it", {
  s25 <- paste(sub("#.*$", "", readLines(file.path(ppv_root, "scripts", "25_build_partner_indices.R"), warn = FALSE)), collapse = "\n")
  # Decision of 2026-10-08: the coalition is read from coalition_tbl.rds (scripts/13) through
  # coalition_members(), as scripts/20 and scripts/30 read it; the headline averages the core tier.
  expect_match(s25, "coalition_members(readRDS(coalition_path))", fixed = TRUE)
  expect_match(s25, "coalition_iso3 <- sort(coalition$iso3[coalition$tier == \"core\"])", fixed = TRUE)
  expect_match(s25, "full_coalition_iso3 = coalition_full_iso3", fixed = TRUE)
  expect_false(grepl("allied_coalition_iso3(", s25, fixed = TRUE))
  expect_match(s25, "write_processed_tbl(psi_tbl, \"psi_tbl\", processed_dir)", fixed = TRUE)
  expect_match(s25, "write_processed_tbl(psi_full_coalition_tbl, \"psi_full_coalition_tbl\", processed_dir)", fixed = TRUE)
  expect_match(s25, "write_processed_tbl(psi_world_tbl, \"psi_world_tbl\", processed_dir)", fixed = TRUE)
  expect_match(s25, "psi_views$coalition", fixed = TRUE)
  expect_match(s25, "psi_views$full_coalition", fixed = TRUE)
  expect_match(s25, "psi_flag_rankable(max_imputed_weight_share = psi_tau)", fixed = TRUE)
})

test_that("only sovereign partners are ranked; territories and SARs keep their score", {
  # The imputed-share condition is tested in test-partnership-psi-rankability.R; every share is 0 here.
  psi <- tibble::tibble(
    Country = c("Cayman Islands", "United States", "Hong Kong SAR, China", "Japan", "India", "Viet Nam"),
    partner_iso = c("CYM", "USA", "HKG", "JPN", "IND", "VNM"),
    value = c(0.95, 0.9, 0.85, 0.8, 0.8, NA),
    imputed_weight_share = 0
  )
  out <- psi_flag_rankable(psi, max_imputed_weight_share = 0.5)
  expect_equal(out$value, psi$value)
  # A partner without a value is not rankable (the pillars' "not scored"), and not ranked.
  expect_equal(out$rankable, c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(out$rank_exclusion[6], "not scored")
  # Ties take the lowest rank.
  expect_equal(out$rank, c(NA, 1L, NA, 2L, 2L, NA))
})
