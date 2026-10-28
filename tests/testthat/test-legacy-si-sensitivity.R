# Tests for the legacy Strategic Index sensitivity diagnostics
# (R/strategic_position/legacy_si_sensitivity.R): stage and technology prior scenarios, the ranking
# and allied-pick comparisons, and the CAT effect on the policy term. Small synthetic fixtures only.

lsi_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(lsi_repo_root(), "R", "utils", "schema.R"))
source(file.path(lsi_repo_root(), "R", "utils", "scurve.R"))
source(file.path(lsi_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(lsi_repo_root(), "R", "indices", "build_indices.R"))
source(file.path(lsi_repo_root(), "R", "indices", "build_policy_index.R"))
source(file.path(lsi_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(lsi_repo_root(), "R", "strategic_position", "legacy_si_sensitivity.R"))

lsi_scenarios <- function() {
  list(
    none = stats::setNames(as.list(rep(0, 6)), SUPPLY_CHAIN_STAGES),
    # the config's rounded copy of SUPPLY_CHAIN_STAGE_SI_WEIGHTS
    equal_share = list(Extraction = 0.1667, Refining = 0.1667, Components = 0.375, Assembly = 0.375,
                       Deployment = 0.25, Recovery = 0.1667),
    manufacturing_focused = list(Extraction = 0, Refining = 0.25, Components = 1, Assembly = 1,
                                 Deployment = 0, Recovery = 0)
  )
}

# A stored-SI-like table: strategic_index = base + 0.1 sc_weight + 0.1 tech_weight, one excluded
# cell and one Extraction cell barred by the resource screen.
lsi_si_fixture <- function() {
  cells <- expand.grid(
    Country = c("A", "B", "C"), tech = c("Solar", "Nuclear", "Oil"),
    supply_chain = c("Extraction", "Components", "Assembly", "Deployment"),
    stringsAsFactors = FALSE
  )
  ghg <- c(Solar = 0.967, Nuclear = 0.999, Oil = 0)
  cells$base <- 0.2 + 0.3 * ((seq_len(nrow(cells)) * 37) %% 23) / 23
  tibble::as_tibble(cells) %>%
    dplyr::mutate(
      sector = paste(.data$tech, .data$supply_chain, sep = " - "),
      sc_weight = unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[.data$supply_chain]),
      tech_weight = unname(ghg[.data$tech]),
      strategic_index = .data$base + 0.1 * .data$sc_weight + 0.1 * .data$tech_weight,
      si_exclusion = NA_character_,
      resource_base = dplyr::if_else(.data$supply_chain == "Extraction", TRUE, NA),
      pick_exclusion = NA_character_
    ) %>%
    dplyr::mutate(
      excluded = .data$Country == "B" & .data$sector == "Oil - Assembly",
      strategic_index = dplyr::if_else(.data$excluded, NA_real_, .data$strategic_index),
      si_exclusion = dplyr::if_else(.data$excluded, "missing energy security", .data$si_exclusion),
      barred = .data$Country == "C" & .data$sector == "Nuclear - Extraction",
      resource_base = dplyr::if_else(.data$barred, FALSE, .data$resource_base),
      pick_exclusion = dplyr::if_else(.data$barred, "no resource base", .data$pick_exclusion)
    ) %>%
    dplyr::select(-"base", -"excluded", -"barred")
}

lsi_variant <- function(long, v, si) {
  out <- long[long$variant == v, , drop = FALSE]
  out[match(paste(si$Country, si$sector), paste(out$Country, out$sector)), , drop = FALSE]
}

test_that("'none' removes the stage term exactly and equal_share reproduces strategic_index", {
  si <- lsi_si_fixture()
  long <- sp_si_stage_scenarios(si, lsi_scenarios())

  none <- lsi_variant(long, "none", si)
  expect_equal(none$si, si$strategic_index - 0.1 * si$sc_weight, tolerance = 1e-12)
  expect_true(all(none$stage_term == 0))

  eq <- lsi_variant(long, "equal_share", si)
  expect_true(all(eq$lambda_matches_stored))
  expect_identical(eq$si, si$strategic_index)

  # Without the snap to the stored prior, the config's rounding shifts three stages by 3e-6.
  raw <- lsi_variant(sp_si_stage_scenarios(si, lsi_scenarios(), match_stored_tol = 0), "equal_share", si)
  expect_false(any(raw$lambda_matches_stored))
  expect_equal(raw$si, si$strategic_index, tolerance = 1e-5)
  expect_gt(max(abs(raw$si - si$strategic_index), na.rm = TRUE), 0)

  mf <- lsi_variant(long, "manufacturing_focused", si)
  lam <- c(Extraction = 0, Components = 1, Assembly = 1, Deployment = 0)
  expect_equal(mf$si, si$strategic_index - 0.1 * si$sc_weight + 0.1 * unname(lam[si$supply_chain]), tolerance = 1e-12)
})

test_that("the technology prior can be switched off separately", {
  si <- lsi_si_fixture()
  long <- sp_si_stage_scenarios(si, lsi_scenarios())
  expect_setequal(
    unique(long$variant),
    c("none", "none_no_tech", "equal_share", "equal_share_no_tech", "manufacturing_focused", "manufacturing_focused_no_tech")
  )
  eq_nt <- lsi_variant(long, "equal_share_no_tech", si)
  expect_equal(eq_nt$si, si$strategic_index - 0.1 * si$tech_weight, tolerance = 1e-12)
  expect_true(all(eq_nt$tech_term == 0))
  both <- lsi_variant(long, "none_no_tech", si)
  expect_equal(both$si, si$strategic_index - 0.1 * si$sc_weight - 0.1 * si$tech_weight, tolerance = 1e-12)

  only_on <- sp_si_stage_scenarios(si, lsi_scenarios(), tech_prior = TRUE)
  expect_false(any(grepl("_no_tech$", only_on$variant)))
})

test_that("exclusions and the resource screen are carried unchanged into every scenario", {
  si <- lsi_si_fixture()
  long <- sp_si_stage_scenarios(si, lsi_scenarios())
  ex <- long[long$Country == "B" & long$sector == "Oil - Assembly", ]
  expect_equal(nrow(ex), 6)
  expect_true(all(is.na(ex$si)))
  expect_true(all(ex$si_exclusion == "missing energy security"))
  expect_true(all(is.na(ex$rank_within_country)))
  barred <- long[long$Country == "C" & long$sector == "Nuclear - Extraction", ]
  expect_true(all(barred$resource_base %in% FALSE))
  expect_true(all(barred$pick_exclusion == "no resource base"))
  expect_true(all(!is.na(barred$si)))
  expect_equal(sum(!is.na(long$si)), 6 * sum(!is.na(si$strategic_index)))
})

test_that("stage and technology priors cannot move the cross-country order within a cell", {
  si <- lsi_si_fixture()
  long <- sp_si_stage_scenarios(si, lsi_scenarios())
  ranks <- tidyr::pivot_wider(
    long[c("variant", "Country", "sector", "rank_cross_country")],
    names_from = "variant", values_from = "rank_cross_country"
  )
  for (v in setdiff(unique(long$variant), "equal_share")) {
    expect_identical(ranks[[v]], ranks[["equal_share"]], info = v)
  }
  cmp <- sp_si_scenario_comparison(long, "equal_share", min_n = 2)
  expect_equal(cmp$cell$spearman_cross, rep(1, nrow(cmp$cell)), tolerance = 1e-12)
})

test_that("scenarios must weight every stage and the table must use the six stages", {
  si <- lsi_si_fixture()
  bad <- list(partial = list(Extraction = 1, Refining = 1))
  expect_error(sp_si_stage_scenarios(si, bad), "missing Components")
  typo <- lsi_scenarios()
  typo$none$Assmbly <- 0
  expect_error(sp_si_stage_scenarios(si, typo), "unknown Assmbly")
  legacy <- si
  legacy$supply_chain[1] <- "Upstream"
  expect_error(sp_si_stage_scenarios(legacy, lsi_scenarios()), "not supply-chain stages")
})

test_that("the config stage scenarios parse, and equal_share is the stored prior", {
  cfg <- yaml::read_yaml(file.path(lsi_repo_root(), "config", "strategic_position.yml"))
  lam <- sp_lsi_scenario_lambdas(cfg$stage_scenarios)
  expect_true(all(c("none", "equal_share") %in% names(lam)))
  expect_true(all(lam$none == 0))
  expect_equal(lam$equal_share, SUPPLY_CHAIN_STAGE_SI_WEIGHTS[SUPPLY_CHAIN_STAGES], tolerance = 1e-3)
})

# Two countries, one technology, three stages, built so that dropping the stage prior moves A's
# top cell from Components to Extraction and leaves B's order alone.
lsi_small <- function() {
  base <- tibble::tribble(
    ~Country, ~supply_chain, ~base, ~resource_base,
    "A", "Extraction", 0.42, TRUE,
    "A", "Components", 0.40, NA,
    "A", "Deployment", 0.30, NA,
    "B", "Extraction", 0.10, TRUE,
    "B", "Components", 0.50, NA,
    "B", "Deployment", 0.20, NA
  )
  base %>%
    dplyr::mutate(
      tech = "Solar",
      sector = paste(.data$tech, .data$supply_chain, sep = " - "),
      sc_weight = unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[.data$supply_chain]),
      tech_weight = 0.967,
      strategic_index = .data$base + 0.1 * .data$sc_weight + 0.1 * .data$tech_weight,
      si_exclusion = NA_character_,
      pick_exclusion = dplyr::if_else(.data$resource_base %in% FALSE, "no resource base", NA_character_)
    ) %>%
    dplyr::select(-"base")
}

test_that("the ranking comparison counts rank agreement, top cells, overlaps and stage shares", {
  long <- sp_si_stage_scenarios(lsi_small(), lsi_scenarios()[c("none", "equal_share")], tech_prior = TRUE)
  cmp <- sp_si_scenario_comparison(long, baseline = "equal_share", top_n = 1, min_n = 2)

  a <- cmp$country[cmp$country$variant == "none" & cmp$country$Country == "A", ]
  # equal_share order Components, Extraction, Deployment; none: Extraction, Components, Deployment
  expect_equal(a$spearman_within, 0.5)
  expect_equal(a$top1_baseline, "Solar - Components")
  expect_equal(a$top1, "Solar - Extraction")
  expect_true(a$top1_changed)
  expect_equal(a$top_n_overlap, 0)
  b <- cmp$country[cmp$country$variant == "none" & cmp$country$Country == "B", ]
  expect_equal(b$spearman_within, 1)
  expect_false(b$top1_changed)
  expect_equal(b$top_n_overlap, 1)

  st <- cmp$stage[cmp$stage$variant == "none", ]
  expect_equal(sum(st$share_top), 1)
  expect_equal(st$share_top[st$supply_chain == "Extraction"], 0.5)
  expect_equal(st$share_top_baseline[st$supply_chain == "Components"], 1)
  expect_equal(st$change_share[st$supply_chain == "Components"], -0.5)
  expect_equal(cmp$cell$spearman_cross, rep(1, nrow(cmp$cell)), tolerance = 1e-12)

  s <- cmp$summary[cmp$summary$variant == "none", ]
  expect_equal(s$n_top1_changed, 1)
  expect_equal(s$spearman_within_median, 0.75)
  base_row <- cmp$summary[cmp$summary$variant == "equal_share", ]
  expect_equal(base_row$n_top1_changed, 0)
})

test_that("the pick comparison re-runs the allied selection with its rules and reports margins", {
  small <- lsi_small()
  long <- sp_si_stage_scenarios(small, lsi_scenarios()[c("none", "equal_share")], tech_prior = TRUE)
  allies <- tibble::tibble(Country = c("A", "B", "C"), iso3 = c("AAA", "BBB", "CCC"), tier = c("core", "core", "partner"))
  pc <- sp_si_pick_comparison(long, allies, baseline = "equal_share", focus_techs = "Solar")

  base_a <- pc$picks[pc$picks$variant == "equal_share" & pc$picks$Country == "A", ]
  expect_equal(base_a$pick, "Solar - Components")
  expect_equal(base_a$runner_up, "Solar - Extraction")
  expect_equal(base_a$margin, (0.40 + 0.0375) - (0.42 + 0.1 / 6), tolerance = 1e-12)
  expect_true(base_a$baseline_near)
  expect_equal(base_a$iso3, "AAA")

  none_a <- pc$picks[pc$picks$variant == "none" & pc$picks$Country == "A", ]
  expect_equal(none_a$pick, "Solar - Extraction")
  expect_true(none_a$changed)
  expect_true(none_a$stage_only_change)
  expect_equal(none_a$old_pick_deficit, 0.02 + 0, tolerance = 1e-12)

  # Deployment is never a pick; a member with no scored cell gets none and is not a change.
  expect_false(any(grepl("Deployment", pc$picks$pick)))
  c_rows <- pc$picks[pc$picks$Country == "C", ]
  expect_true(all(!c_rows$picked & !c_rows$changed))

  s <- pc$summary[pc$summary$variant == "none", ]
  expect_equal(c(s$n_members, s$n_picked, s$n_changed, s$n_changed_near_baseline), c(3L, 2L, 1L, 1L))
  f <- pc$focus[pc$focus$variant == "none", ]
  expect_equal(c(f$n_picks, f$n_lost, f$n_gained, f$n_stage_moves), c(2, 0, 0, 1))
  st <- pc$by_stage[pc$by_stage$variant == "none", ]
  expect_equal(st$n_picks[st$supply_chain == "Extraction"], 1L)
  expect_equal(st$change[st$supply_chain == "Components"], -1L)
  expect_equal(pc$transitions$from, "Solar - Components")

  # With A's Extraction barred by the resource screen, the stage prior no longer moves A's pick.
  barred <- small
  barred$resource_base[barred$Country == "A" & barred$supply_chain == "Extraction"] <- FALSE
  long_b <- sp_si_stage_scenarios(barred, lsi_scenarios()[c("none", "equal_share")], tech_prior = TRUE)
  pc_b <- sp_si_pick_comparison(long_b, allies, baseline = "equal_share")
  expect_equal(pc_b$summary$n_changed[pc_b$summary$variant == "none"], 0L)
  # ... unless the screen is off, as with OPSI_SI_RESOURCE_SCREEN=0.
  pc_off <- sp_si_pick_comparison(long_b, allies, baseline = "equal_share", require_resource_base = FALSE)
  expect_equal(pc_off$summary$n_changed[pc_off$summary$variant == "none"], 1L)
})

# Country A has a CAT rating, B is scored on IEA PAMS alone. A's Oil - Extraction has CAT but no
# IEA PAMS record, so its composite value comes from CAT alone.
lsi_policy_fixture <- function() {
  pams <- tibble::tribble(
    ~Country, ~tech, ~supply_chain, ~value,
    "A", "Solar", "Extraction", 0.3,
    "A", "Solar", "Deployment", 0.8,
    "A", "Oil", "Deployment", 0.5,
    "B", "Solar", "Extraction", 0.2,
    "B", "Solar", "Deployment", 0.6,
    "B", "Oil", "Extraction", 0.4,
    "B", "Oil", "Deployment", 0.1
  ) %>% dplyr::mutate(variable = "IEA PAMS Policy Index")
  cat <- tidyr::crossing(Country = "A", tech = c("Solar", "Oil"), supply_chain = c("Extraction", "Deployment")) %>%
    dplyr::mutate(value = dplyr::if_else(.data$tech == "Solar", 0.9, 0.1), variable = "CAT Policy Index")
  dplyr::bind_rows(pams, cat)
}

lsi_policy_si <- function(pct) {
  comp <- build_composite_policy_index(pct, techs = c("Solar", "Oil"))$index
  comp %>%
    dplyr::select("Country", "tech", "supply_chain", "value", "policy_basis") %>%
    dplyr::mutate(
      sector = paste(.data$tech, .data$supply_chain, sep = " - "),
      other = c(0.31, 0.29, 0.30, 0.28, 0.33, 0.27, 0.26, 0.32)[seq_len(dplyr::n())],
      si_exclusion = NA_character_
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(pol = scurve_in_sample(.data$value, TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(strategic_index = 0.2 * .data$pol + .data$other) %>%
    dplyr::select(-"other")
}

test_that("removing CAT leaves countries scored on IEA PAMS alone unchanged", {
  pct <- lsi_policy_fixture()
  si <- lsi_policy_si(pct)
  eff <- sp_cat_rank_effect(si, pct, techs = c("Solar", "Oil"))

  expect_equal(eff$check$max_abs_value_diff, 0)
  expect_equal(eff$check$max_abs_pol_diff, 0)

  b <- eff$cells[eff$cells$Country == "B", ]
  expect_identical(b$value_without_cat, b$value)
  expect_identical(b$pol_without_cat, b$pol)
  expect_identical(b$si_without_cat, b$strategic_index)
  bc <- eff$country[eff$country$Country == "B", ]
  expect_equal(bc$policy_basis, "IEA PAMS only")
  expect_equal(c(bc$rho_pol, bc$rho_si, bc$rho_policy_all), c(1, 1, 1))
  expect_equal(bc$n_third_changed, 0)
  expect_false(bc$top1_changed)
})

test_that("removing CAT recomputes pol within the country from IEA PAMS and drops CAT-only cells", {
  pct <- lsi_policy_fixture()
  si <- lsi_policy_si(pct)
  eff <- sp_cat_rank_effect(si, pct, techs = c("Solar", "Oil"))

  a <- eff$cells[eff$cells$Country == "A", ]
  lost <- a$sector == "Oil - Extraction"
  expect_true(is.na(a$value_without_cat[lost]))
  expect_false(a$in_sample_without_cat[lost])
  expect_equal(eff$check$n_cells_lost, 1)
  # composite (3 PAMS + 2 CAT) / 5 where both exist
  expect_equal(a$value[a$sector == "Solar - Extraction"], (3 * 0.3 + 2 * 0.9) / 5)
  expect_equal(a$value_without_cat[!lost], c(0.3, 0.8, 0.5)[match(a$sector[!lost], c("Solar - Extraction", "Solar - Deployment", "Oil - Deployment"))])
  pol_pams <- median_scurve(a$value_without_cat[!lost])
  expect_equal(a$pol_without_cat[!lost], pol_pams)
  expect_equal(a$si_without_cat[!lost], a$strategic_index[!lost] - 0.2 * a$pol[!lost] + 0.2 * pol_pams)

  ac <- eff$country[eff$country$Country == "A", ]
  expect_equal(ac$policy_basis, "IEA PAMS + CAT")
  expect_equal(ac$n_cells_lost, 1)
  # with CAT: Solar - Deployment > Solar - Extraction > Oil - Deployment; without: Solar - Dep > Oil - Dep > Solar - Ext
  expect_equal(ac$rho_pol, 0.5)

  wo <- eff$long[eff$long$variant == "without_cat" & eff$long$Country == "A" & eff$long$sector == "Oil - Extraction", ]
  expect_true(is.na(wo$si))
  expect_equal(wo$si_exclusion, "missing policy index without CAT")
  expect_setequal(unique(eff$long$variant), c("with_cat", "without_cat"))
  expect_identical(
    eff$long$si[eff$long$variant == "with_cat"],
    si$strategic_index[match(
      paste(eff$long$Country, eff$long$sector)[eff$long$variant == "with_cat"],
      paste(si$Country, si$sector)
    )]
  )
})

test_that("the CAT long table feeds the pick comparison", {
  pct <- lsi_policy_fixture()
  si <- lsi_policy_si(pct)
  eff <- sp_cat_rank_effect(si, pct, techs = c("Solar", "Oil"))
  pc <- sp_si_pick_comparison(eff$long, c("A", "B"), baseline = "with_cat", focus_techs = "Solar")
  expect_equal(pc$summary$n_members, c(2L, 2L))
  # Deployment is not eligible, so the picks are Extraction cells
  expect_true(all(grepl("Extraction", pc$picks$pick[pc$picks$picked])))
})
