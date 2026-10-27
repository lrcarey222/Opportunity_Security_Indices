repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

div_iso <- c("USA", "CAN", "MEX", "DEU", "FRA", "JPN", "AUS")
div_country_info <- tibble::tibble(
  iso3c = div_iso,
  country = c("United States", "Canada", "Mexico", "Germany", "France", "Japan", "Australia")
)

# Seven members whose raw scores differ modestly, in up to three stages (stage names of
# R/utils/supply_chain_stages.R), every component present.
make_div_inputs <- function(tech = "Solar", stages = c("Extraction", "Assembly", "Deployment")) {
  countries <- div_country_info$country
  grid <- tidyr::crossing(Country = countries, tech = tech, supply_chain = stages)
  eo_val <- stats::setNames(c(0.72, 0.70, 0.66, 0.69, 0.64, 0.67, 0.62), countries)
  es_val <- stats::setNames(c(0.60, 0.58, 0.55, 0.61, 0.57, 0.56, 0.59), countries)
  pol_val <- stats::setNames(c(0.65, 0.63, 0.58, 0.66, 0.62, 0.60, 0.61), countries)
  shift <- c(Extraction = 0, Assembly = 0.01, Deployment = -0.01)
  eo <- grid %>% dplyr::mutate(Economic_Opportunity_Index = unname(eo_val[Country]) + unname(shift[supply_chain]))
  es <- grid %>% dplyr::mutate(Energy_Security_Index = unname(es_val[Country]))
  pol <- grid %>%
    dplyr::mutate(category = "Policy", variable = "Overall Policy Index", data_type = "index", value = unname(pol_val[Country]), Year = 0)

  dyads <- tidyr::crossing(reporter_iso = div_iso, partner_iso = div_iso, tech = tech, supply_chain = stages) %>%
    dplyr::mutate(r = match(reporter_iso, div_iso), p = match(partner_iso, div_iso))
  fs <- dyads %>%
    dplyr::mutate(category = "friendshore", variable = "Friendshore Index", data_type = "index",
                  value = 0.55 + 0.05 * ((r + 2 * p) %% 4), Year = 2024L) %>%
    dplyr::select(-r, -p)
  op <- dyads %>%
    dplyr::mutate(category = "opportunity", variable = "Opportunity Index", data_type = "index",
                  value = 0.50 + 0.05 * ((2 * r + p) %% 5), Year = 2024L) %>%
    dplyr::select(-r, -p)
  size <- tibble::tibble(iso3c = div_iso, gdp_usd = c(3e13, 2.3e12, 1.8e12, 5e12, 3.4e12, 4.4e12, 1.8e12), gdp_imputed = FALSE)
  list(eo = eo, es = es, pol = pol, fs = fs, op = op, size = size)
}

run_div <- function(x, ...) {
  suppressWarnings(allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = x$pol,
    partner_friendshore_tbl = x$fs,
    partner_opportunity_tbl = x$op,
    country_info = div_country_info,
    iso3c_network = div_iso,
    country_size_tbl = x$size,
    demand_mode = "mixed",
    demand_weights = list(need = 0.5, size = 0.5),
    min_producers = 3,
    max_share = 0.40,
    min_share = 0.05,
    ...
  ))
}

by_iso <- function(res) res$specialization %>% dplyr::arrange(tech, supply_chain, iso3c)

# Every original constraint, checked on the realized outputs of one design. The greedy fallback treats
# the 10% supplier-share rule as soft (its flow fit drops a sliver only if the share caps still hold),
# so supplier_share_rule = FALSE is available for greedy designs where the rule cannot be met.
expect_design_constraints <- function(res, max_share = 0.40, min_share = 0.05, min_producers = 3, supplier_share_rule = TRUE) {
  sp <- res$specialization
  st <- sp %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(
      s_share = sum(production_share), s_demand = sum(demand_weight), max_s = max(production_share),
      min_sel = min(production_share[selected]), n_sel = sum(selected), n_pos = sum(production_share > 1e-9),
      .groups = "drop"
    )
  expect_equal(st$s_share, rep(1, nrow(st)), tolerance = 1e-6)
  expect_equal(st$s_demand, rep(1, nrow(st)), tolerance = 1e-9)
  expect_true(all(st$max_s <= max_share + 1e-6))
  expect_true(all(st$min_sel >= min_share - 1e-6))
  expect_true(all(st$n_sel >= min_producers & st$n_sel == st$n_pos))
  expect_true(all(res$diagnostics$min_suppliers_realized >= 2))
  if (supplier_share_rule) expect_true(all(res$diagnostics$min_supplier_share_realized >= 0.10 - 1e-6))

  fl <- res$flows
  expect_equal(anyDuplicated(fl[c("tech", "supply_chain", "reporter_iso", "partner_iso")]), 0L)
  supply <- fl %>% dplyr::group_by(tech, supply_chain, iso3c = reporter_iso) %>% dplyr::summarise(v = sum(flow_share), .groups = "drop") %>%
    dplyr::inner_join(sp, by = c("tech", "supply_chain", "iso3c"))
  expect_equal(supply$v, supply$production_share, tolerance = 1e-6)
  served <- fl %>% dplyr::group_by(tech, supply_chain, iso3c = partner_iso) %>% dplyr::summarise(v = sum(flow_share), .groups = "drop") %>%
    dplyr::inner_join(sp, by = c("tech", "supply_chain", "iso3c"))
  expect_equal(served$v, served$demand_weight, tolerance = 1e-6)
  flow_tot <- fl %>% dplyr::group_by(tech, supply_chain) %>% dplyr::summarise(v = sum(flow_share), .groups = "drop")
  expect_equal(flow_tot$v, rep(1, nrow(flow_tot)), tolerance = 1e-6)
}

test_that("tangent breakpoints and the closed-form shares behave as documented", {
  bp <- allied_network_hhi_breakpoints(0.40, 0.01)
  expect_equal(range(bp), c(0, 0.40))
  expect_length(bp, 41L)
  expect_equal(allied_network_hhi_breakpoints(0.35, 0.1), c(0, 0.1, 0.2, 0.3, 0.35))
  # The tangent envelope under-approximates x^2 by at most step^2 / 4, attained at the midpoints
  xs <- seq(0, 0.40, by = 0.0005)
  env <- vapply(xs, function(x) max(2 * bp * x - bp^2), numeric(1))
  expect_true(all(env <= xs^2 + 1e-15))
  expect_equal(max(xs^2 - env), 0.01^2 / 4, tolerance = 1e-12)

  # Unbounded interior solution: x = (v - mu) / (2 lambda) with sum x = 1
  x <- allied_network_waterfill(c(1, 0.9, 0.8), lambda = 1, lo = 0, hi = 1)
  expect_equal(x, c(1, 0.9, 0.8) / 2 - (2.7 / 2 - 1) / 3, tolerance = 1e-9)
  # Bounds bind: the leader is capped and the rest share the remainder by value
  x <- allied_network_waterfill(c(2, 0.5, 0.5, 0.5), lambda = 0.1, lo = 0.05, hi = 0.40)
  expect_equal(x, c(0.40, 0.20, 0.20, 0.20), tolerance = 1e-9)
  expect_null(allied_network_waterfill(c(1, 1), lambda = 1, lo = 0.05, hi = 0.40))

  # Equal values: more producers lower the penalty, up to the min_share limit (1 / 0.05 = 20)
  d <- allied_network_diversified_shares(rep(0.5, 25), lambda = 1, min_share = 0.05, max_share = 0.40, n_min = 3)
  expect_equal(d$k, 20L)
  expect_equal(d$shares, rep(0.05, 20), tolerance = 1e-9)
  # Large value gaps relative to lambda keep the minimum producer count
  d <- allied_network_diversified_shares(c(1, 0.9, 0.2, 0.1), lambda = 0.05, min_share = 0.05, max_share = 0.40, n_min = 3)
  expect_equal(d$k, 3L)
  expect_error(allied_network_check_lambda(-1), "diversity_lambda")
  expect_error(allied_network_check_lambda(NA_real_), "diversity_lambda")
})

test_that("lambda = 0 reproduces the pre-diversification design exactly", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_div_inputs(stages = c("Extraction", "Assembly"))
  # Golden values from the code before diversity_lambda existed (same fixture, same arguments),
  # rows ordered Assembly then Extraction, ISO3 alphabetical within a stage
  milp_shares <- rep(c(0, 0.2, 0.4, 0, 0, 0, 0.4), 2)
  greedy_shares <- rep(c(0, 0.2889754748, 0.3465742304, 0, 0, 0, 0.3644502948), 2)

  m0 <- run_div(x, method = "milp")
  m0_explicit <- run_div(x, method = "milp", diversity_lambda = 0)
  expect_equal(by_iso(m0)$production_share, milp_shares, tolerance = 1e-9)
  expect_equal(m0$diagnostics$objective, rep(1.1011712598, 2), tolerance = 1e-9)
  expect_equal(by_iso(m0_explicit)$production_share, by_iso(m0)$production_share)
  expect_equal(m0_explicit$flows, m0$flows)
  expect_equal(m0$diagnostics$diversity_penalty, c(0, 0))
  expect_true(all(is.na(m0$diagnostics$hhi_tangent_max_error)))
  expect_equal(m0$diagnostics$n_at_max_share, c(2L, 2L))

  g0 <- run_div(x, method = "greedy")
  expect_equal(by_iso(g0)$production_share, greedy_shares, tolerance = 1e-9)
  expect_equal(g0$diagnostics$objective, rep(1.0721802521, 2), tolerance = 1e-9)
})

test_that("lambda > 0 moves the MILP and greedy designs to interior, less concentrated shares", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_div_inputs(stages = c("Extraction", "Assembly"))
  m0 <- run_div(x, method = "milp")
  m1 <- run_div(x, method = "milp", diversity_lambda = 1)
  expect_true(all(m1$diagnostics$method == "milp"))
  expect_true(all(m1$diagnostics$hhi < m0$diagnostics$hhi - 0.05))
  expect_true(all(m1$diagnostics$n_at_max_share < m0$diagnostics$n_at_max_share))
  expect_true(all(m1$diagnostics$n_producers > m0$diagnostics$n_producers))
  expect_equal(m1$diagnostics$effective_producers, 1 / m1$diagnostics$hhi)
  # Interior: the selected shares are not all at a bound
  sel <- m1$specialization$production_share[m1$specialization$selected]
  expect_gt(sum(sel > 0.05 + 1e-6 & sel < 0.40 - 1e-6), 2)
  # lambda = 0 maximises the linear part over the same feasible set, so diversifying can only cost
  # node + edge score
  lin <- function(res) res$diagnostics$objective_node + res$diagnostics$objective_edge + res$diagnostics$objective_dev
  expect_true(all(lin(m1) <= lin(m0) + 1e-7))
  expect_true(all(m1$diagnostics$objective_node < m0$diagnostics$objective_node))
  expect_design_constraints(m1)

  # A higher lambda cannot raise the approximated HHI; the true HHI can differ from it by at most
  # (number of producers) * step^2 / 4
  m3 <- run_div(x, method = "milp", diversity_lambda = 3)
  expect_true(all(m3$diagnostics$hhi <= m1$diagnostics$hhi + length(div_iso) * 0.01^2 / 4))
  expect_design_constraints(m3)

  # The greedy fallback follows the same objective in closed form and lands close to the MILP
  g1 <- run_div(x, method = "greedy", diversity_lambda = 1)
  expect_true(all(g1$diagnostics$method == "greedy"))
  expect_equal(g1$diagnostics$n_producers, m1$diagnostics$n_producers)
  expect_equal(by_iso(g1)$selected, by_iso(m1)$selected)
  expect_lt(max(abs(by_iso(g1)$production_share - by_iso(m1)$production_share)), 0.02)
  # The stepwise prune lets the diversified greedy meet the 10% supplier rule too
  expect_design_constraints(g1)
})

test_that("a stage that falls back to greedy keeps the diversified objective and says why", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  # Eight suppliers per consumer cannot be met with seven members, so the MILP is infeasible
  res <- run_div(make_div_inputs(stages = "Assembly"), method = "milp", diversity_lambda = 1, min_suppliers_per_consumer = 8)
  d <- res$diagnostics
  expect_identical(d$method, "greedy")
  expect_identical(d$method_requested, "milp")
  expect_false(is.na(d$fallback_reason))
  expect_equal(d$diversity_penalty, 1 * d$hhi, tolerance = 1e-12)
  expect_equal(d$objective, d$objective_node + d$objective_edge + d$objective_dev - d$diversity_penalty, tolerance = 1e-9)
  expect_true(is.na(d$hhi_tangent_max_error) && is.na(d$hhi_tangent_error_bound))
  sel <- res$specialization$production_share[res$specialization$selected]
  expect_equal(sum(sel), 1, tolerance = 1e-9)
  expect_true(all(sel >= 0.05 - 1e-9 & sel <= 0.40 + 1e-9))
  expect_equal(d$n_at_max_share, 0L)
})

test_that("the tangent approximation error stays within the stated bound", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_div_inputs(stages = c("Extraction", "Assembly"))
  for (step in c(0.01, 0.02)) {
    res <- run_div(x, method = "milp", diversity_lambda = 1, diversity_breakpoint_step = step)
    d <- res$diagnostics
    expect_equal(d$hhi_tangent_error_bound, rep(step^2 / 4, 2))
    expect_equal(res$params$hhi_tangent_error_bound, step^2 / 4)
    expect_true(all(d$hhi_tangent_max_error <= step^2 / 4 + 1e-9))
    # The solver objective uses sum t <= HHI, so it exceeds the exact penalised objective by at most
    # lambda * (number of producers) * bound
    exact <- d$objective_node + d$objective_edge + d$objective_dev - d$diversity_penalty
    gap <- d$objective - exact
    expect_true(all(gap >= -1e-7 & gap <= 1 * d$n_producers * step^2 / 4 + 1e-7))
  }
  # Greedy is exact: no approximation, objective equals its decomposition
  g <- run_div(x, method = "greedy", diversity_lambda = 1)
  expect_true(all(is.na(g$diagnostics$hhi_tangent_max_error)))
  gd <- g$diagnostics
  expect_equal(gd$objective, gd$objective_node + gd$objective_edge + gd$objective_dev - gd$diversity_penalty, tolerance = 1e-9)
})

test_that("the stepwise prune meets the 10% supplier rule where the all-at-once prune cannot", {
  # Seven producers, four of them near min_share, spread over eight equal consumers by RAS: every
  # flow of a 5% or 8% producer is a sliver of its consumer's demand
  x <- c(0.30, 0.24, 0.20, 0.08, 0.08, 0.05, 0.05)
  d <- rep(1 / 8, 8)
  K <- matrix(1, 7, 8)
  K[1, 1] <- 0.5
  K[2, 3] <- 0.7
  rel <- function(f) sweep(f, 2, d, `/`)
  f0 <- allied_network_ras(K, x, d)
  f1 <- allied_network_prune_small_flows(f0, K, x, d, min_suppliers = 2, epsilon = 0.10)
  expect_gt(sum(f1 > 0 & rel(f1) < 0.10 - 1e-9), 0)
  f2 <- allied_network_prune_small_flows_stepwise(f1, K, x, d, min_suppliers = 2, epsilon = 0.10)
  expect_equal(sum(f2 > 0 & rel(f2) < 0.10 - 1e-9), 0L)
  expect_true(all(colSums(f2 > 0) >= 2))
  expect_equal(rowSums(f2), x, tolerance = 1e-9)
  expect_equal(colSums(f2), d, tolerance = 1e-9)
  # No consumer is left with fewer than min_suppliers: with eight required (of seven) nothing moves
  expect_identical(allied_network_prune_small_flows_stepwise(f0, K, x, d, min_suppliers = 8, epsilon = 0.10), f0)
})

test_that("diagnostics carry the diversification columns", {
  res <- run_div(make_div_inputs(stages = "Assembly"), method = "greedy", diversity_lambda = 0.5)
  cols <- c("diversity_lambda", "hhi", "effective_producers", "n_at_max_share", "objective_node",
            "objective_edge", "objective_dev", "diversity_penalty", "hhi_tangent_max_error", "hhi_tangent_error_bound")
  expect_true(all(cols %in% names(res$diagnostics)))
  expect_equal(res$diagnostics$diversity_lambda, 0.5)
  expect_equal(res$diagnostics$diversity_penalty, 0.5 * res$diagnostics$hhi, tolerance = 1e-12)
  expect_identical(res$params$diversity_lambda, 0.5)
  expect_error(run_div(make_div_inputs(stages = "Assembly"), method = "greedy", diversity_lambda = -0.1), "diversity_lambda")
})

test_that("the stage filter drops Deployment and the caps count solved stages only", {
  x <- make_div_inputs()
  # USA leads every stage; a cap of 2 binds over three stages but not over two
  caps <- tibble::tibble(iso3c = div_iso, portfolio_cap = c(2L, rep(5L, 6)))
  all3 <- run_div(x, method = "greedy", portfolio_enable = TRUE, portfolio_caps_tbl = caps, portfolio_top_k = 3)
  # scripts/30's filter: the five supply stages, three of which (Refining, Components, Recovery)
  # this fixture does not carry
  supply <- run_div(x, method = "greedy", portfolio_enable = TRUE, portfolio_caps_tbl = caps, portfolio_top_k = 3,
                    supply_chains = SUPPLY_CHAIN_SUPPLY_STAGES)

  expect_setequal(unique(all3$specialization$supply_chain), c("Extraction", "Assembly", "Deployment"))
  expect_setequal(unique(supply$specialization$supply_chain), c("Extraction", "Assembly"))
  expect_false(any(supply$diagnostics$supply_chain == "Deployment"))
  expect_false(any(supply$flows$supply_chain == "Deployment"))
  expect_false(any(supply$portfolio$topk$supply_chain == "Deployment"))
  expect_equal(supply$params$stages_excluded$supply_chain, "Deployment")
  expect_equal(nrow(all3$params$stages_excluded), 0L)

  usa_count <- function(res) res$portfolio$counts$topk_count[res$portfolio$counts$iso3c == "USA"]
  # Over three stages USA is forced out of one; over the two solved stages it fits under its cap
  expect_equal(usa_count(all3), 2L)
  expect_equal(sum(all3$specialization$selected & all3$specialization$iso3c == "USA"), 2L)
  # The diagnostics name the members the caps barred in each stage: USA in exactly the stage it left
  barred <- all3$diagnostics %>% dplyr::filter(producers_forbidden != "")
  expect_equal(barred$producers_forbidden, "USA")
  usa_out <- all3$specialization %>% dplyr::filter(iso3c == "USA", !selected)
  expect_equal(paste(barred$tech, barred$supply_chain), paste(usa_out$tech, usa_out$supply_chain))
  expect_true(all(supply$diagnostics$producers_forbidden == ""))
  expect_equal(usa_count(supply), 2L)
  expect_true(all(supply$specialization$selected[supply$specialization$iso3c == "USA"]))
  expect_equal(supply$portfolio$iters, 1)
  expect_true(supply$portfolio$converged)
  # Counts are the top-K appearances over the solved stages
  recount <- supply$portfolio$topk %>% dplyr::filter(in_top_k) %>% dplyr::count(iso3c)
  expect_equal(sum(supply$portfolio$counts$topk_count), sum(recount$n))
  expect_equal(sum(supply$portfolio$counts$topk_count), 2L * 3L)

  # Matching is case-insensitive; the solved stages are unchanged by the filter
  lower <- run_div(x, method = "greedy", supply_chains = c("extraction", "ASSEMBLY"))
  expect_setequal(unique(lower$specialization$supply_chain), c("Extraction", "Assembly"))
  free_all <- run_div(x, method = "greedy")
  same <- free_all$specialization %>% dplyr::filter(supply_chain != "Deployment") %>% dplyr::arrange(tech, supply_chain, iso3c)
  expect_equal(lower$specialization %>% dplyr::arrange(tech, supply_chain, iso3c), same)
  expect_error(run_div(x, method = "greedy", supply_chains = "Distribution"), "No \\(tech, supply_chain\\) stages")
  # The three-stage filter against six-stage inputs names both label sets
  expect_error(run_div(x, method = "greedy", supply_chains = c("Upstream", "Midstream")),
               "matches none of the input stages \\(Assembly, Deployment, Extraction\\)")
})

test_that("partner edges on other stage labels than the nodes stop instead of dropping the edge term", {
  x <- make_div_inputs(stages = c("Extraction", "Assembly"))
  legacy <- function(tbl) dplyr::mutate(tbl, supply_chain = dplyr::recode(supply_chain, Extraction = "Upstream", Assembly = "Midstream"))
  x3 <- x
  x3$fs <- legacy(x$fs)
  x3$op <- legacy(x$op)
  expect_error(run_div(x3, method = "greedy"),
               "partner edge tables share no \\(tech, supply_chain\\) stage with the nodes: edge stages Midstream, Upstream")
  # Edges for some solved stages only are allowed (a stage without edges has no edge term)
  part <- x
  part$fs <- dplyr::filter(x$fs, supply_chain == "Assembly")
  part$op <- dplyr::filter(x$op, supply_chain == "Assembly")
  expect_setequal(unique(run_div(part, method = "greedy")$diagnostics$supply_chain), c("Extraction", "Assembly"))
})

test_that("top-K membership uses a strict rank, so tied shares count at most K per stage", {
  sp <- tibble::tibble(
    tech = "Solar", supply_chain = "Assembly", iso3c = div_iso,
    production_share = c(0.25, 0.15, 0.15, 0.15, 0.10, 0.10, 0.10),
    producer_score = c(0.9, 0.5, 0.7, 0.6, 0.4, 0.8, 0.3)
  )
  topk <- allied_network_build_topk_tbl(sp, portfolio_top_k = 5)
  expect_equal(sum(topk$in_top_k), 5L)
  # Ties break on producer score: CAN (0.5) is third of the 0.15 group, JPN (0.8) leads the 0.10 group
  expect_equal(sort(topk$iso3c[topk$in_top_k]), sort(c("USA", "MEX", "DEU", "CAN", "JPN")))
  expect_equal(topk$rank_in_stage, c(1L, 2L, 2L, 2L, 3L, 3L, 3L))
  expect_equal(topk$iso3c, sp$iso3c)
})

test_that("the diversified MILP holds every original constraint, with and without self-supply", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_div_inputs(stages = c("Extraction", "Assembly"))
  res <- run_div(x, method = "milp", diversity_lambda = 0.5, allow_self = FALSE)
  expect_true(all(res$diagnostics$method == "milp"))
  expect_false(any(res$flows$reporter_iso == res$flows$partner_iso))
  expect_design_constraints(res)
  # Caps and diversification together: the portfolio loop still converges
  caps <- tibble::tibble(iso3c = div_iso, portfolio_cap = c(1L, rep(2L, 6)))
  capped <- run_div(x, method = "milp", diversity_lambda = 1, portfolio_enable = TRUE, portfolio_caps_tbl = caps, portfolio_top_k = 5)
  expect_true(capped$portfolio$converged)
  expect_true(all(capped$portfolio$counts$topk_count <= capped$portfolio$counts$portfolio_cap))
  expect_design_constraints(capped)
})
