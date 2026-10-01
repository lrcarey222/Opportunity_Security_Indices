repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

# Five real countries so the Country -> ISO3 mapping (and the "Viet Nam"/"Vietnam" split) is exercised.
net_iso <- c("USA", "CAN", "MEX", "VNM", "ARE")
net_country_info <- tibble::tibble(
  iso3c = net_iso,
  country = c("United States", "Canada", "Mexico", "Viet Nam", "United Arab Emirates")
)

# Clean inputs: one row per key everywhere. ARE has no policy index (as in the shipped data).
make_clean_inputs <- function(tech = "Solar", stages = c("Midstream", "Downstream")) {
  countries <- net_country_info$country
  grid <- tidyr::crossing(Country = countries, tech = tech, supply_chain = stages)
  eo_val <- c("United States" = 0.9, "Canada" = 0.7, "Mexico" = 0.5, "Viet Nam" = 0.4, "United Arab Emirates" = 0.6)
  es_val <- c("United States" = 0.8, "Canada" = 0.6, "Mexico" = 0.4, "Viet Nam" = 0.5, "United Arab Emirates" = 0.7)
  pol_val <- c("United States" = 0.7, "Canada" = 0.5, "Mexico" = 0.3, "Viet Nam" = 0.6)
  eo <- grid %>% dplyr::mutate(Economic_Opportunity_Index = unname(eo_val[Country]))
  es <- grid %>% dplyr::mutate(Energy_Security_Index = unname(es_val[Country]))
  pol <- grid %>%
    dplyr::filter(Country %in% names(pol_val)) %>%
    dplyr::mutate(category = "Policy", variable = "Overall Policy Index", data_type = "index", value = unname(pol_val[Country]), Year = 0)

  dyads <- tidyr::crossing(reporter_iso = net_iso, partner_iso = net_iso, tech = tech, supply_chain = stages) %>%
    dplyr::mutate(r = match(reporter_iso, net_iso), p = match(partner_iso, net_iso))
  fs <- dyads %>%
    dplyr::mutate(category = "friendshore", variable = "Friendshore Index", data_type = "index",
                  value = 0.3 + 0.1 * ((r + 2 * p) %% 6), Year = 2024L) %>%
    dplyr::select(-r, -p)
  op <- dyads %>%
    dplyr::mutate(category = "opportunity", variable = "Opportunity Index", data_type = "index",
                  value = 0.35 + 0.1 * ((2 * r + p) %% 5), Year = 2024L) %>%
    dplyr::select(-r, -p)

  size <- tibble::tibble(iso3c = net_iso, gdp_usd = c(3e13, 2e12, 1.8e12, 4.5e11, 5.5e11), gdp_imputed = FALSE)
  list(eo = eo, es = es, pol = pol, fs = fs, op = op, size = size)
}

# The same data with the defects seen in the shipped inputs: a second VNM spelling with a different
# value, exact duplicate friendshore rows, and a dyadic development table with within-dyad duplicates.
make_messy_inputs <- function(clean) {
  vn_eo <- clean$eo %>% dplyr::filter(Country == "Viet Nam") %>% dplyr::mutate(Country = "Vietnam", Economic_Opportunity_Index = 0.99)
  vn_es <- clean$es %>% dplyr::filter(Country == "Viet Nam") %>% dplyr::mutate(Country = "Vietnam", Energy_Security_Index = 0.01)
  list(
    eo = dplyr::bind_rows(clean$eo, vn_eo),
    es = dplyr::bind_rows(vn_es, clean$es),
    pol = clean$pol,
    fs = dplyr::bind_rows(clean$fs, clean$fs, clean$fs %>% dplyr::filter(reporter_iso == "USA")),
    op = dplyr::bind_rows(clean$op, clean$op %>% dplyr::filter(partner_iso == "MEX")),
    size = clean$size
  )
}

make_dyadic_dev <- function(tech = "Solar", stages = c("Midstream", "Downstream")) {
  # Development potential is partner-level: every reporter repeats the partner's values, and the
  # upstream join leaves two distinct values per dyad for MEX.
  base <- tidyr::crossing(reporter_iso = net_iso, partner_iso = net_iso, tech = tech, supply_chain = stages) %>%
    dplyr::mutate(partner = net_country_info$country[match(partner_iso, net_iso)])
  dplyr::bind_rows(
    base %>% dplyr::mutate(value = dplyr::case_when(partner_iso == "MEX" ~ 0.2, partner_iso == "VNM" ~ 0.7, TRUE ~ NA_real_)),
    base %>% dplyr::filter(partner_iso == "MEX") %>% dplyr::mutate(value = 0.6)
  ) %>%
    dplyr::mutate(category = "development", variable = "Development Potential Index", data_type = "index", Year = 2024L)
}

run_design <- function(x, dev = NULL, ...) {
  suppressWarnings(allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = x$pol,
    partner_friendshore_tbl = x$fs,
    partner_opportunity_tbl = x$op,
    partner_development_country_tbl = dev,
    country_info = net_country_info,
    iso3c_network = net_iso,
    country_size_tbl = x$size,
    demand_mode = "mixed",
    demand_weights = list(need = 0.5, size = 0.5),
    min_producers = 3,
    max_share = 0.40,
    min_share = 0.05,
    ...
  ))
}

test_that("node inputs collapse to one row per country and stage", {
  clean <- make_clean_inputs()
  messy <- make_messy_inputs(clean)
  nodes <- allied_network_prepare_nodes(
    economic_opportunity_index = messy$eo,
    energy_security_index = messy$es,
    policy_index = messy$pol,
    country_info = net_country_info,
    iso3c_network = net_iso,
    partner_development_country_tbl = make_dyadic_dev(),
    country_size_tbl = messy$size,
    demand_mode = "mixed"
  )

  expect_equal(nrow(nodes), length(net_iso) * 2)
  expect_equal(anyDuplicated(nodes[c("iso3c", "tech", "supply_chain")]), 0L)
  # The record spelled as in country_info wins over the variant
  vnm <- nodes %>% dplyr::filter(iso3c == "VNM")
  expect_equal(vnm$eo_raw, c(0.4, 0.4))
  expect_equal(vnm$es_raw, c(0.5, 0.5))
  # Dyadic development: mean of the partner's distinct values, not one row per reporter
  expect_equal(nodes$dev_potential[nodes$iso3c == "MEX"], c(0.4, 0.4))
  expect_equal(nodes$dev_potential[nodes$iso3c == "VNM"], c(0.7, 0.7))
  demand_sums <- nodes %>% dplyr::group_by(tech, supply_chain) %>% dplyr::summarise(s = sum(demand_weight), .groups = "drop")
  expect_equal(demand_sums$s, c(1, 1), tolerance = 1e-12)

  checks <- attr(nodes, "input_checks")
  expect_equal(checks$multivalued_keys[checks$input == "economic_opportunity_index"], 2L)
  expect_equal(checks$keys_out[checks$input == "partner_development_country_tbl"], 10L)
})

test_that("a variant spelling fills a value the canonical record lacks", {
  clean <- make_clean_inputs()
  eo <- clean$eo %>%
    dplyr::mutate(Economic_Opportunity_Index = dplyr::if_else(Country == "Viet Nam", NA_real_, Economic_Opportunity_Index)) %>%
    dplyr::bind_rows(clean$eo %>% dplyr::filter(Country == "Viet Nam") %>% dplyr::mutate(Country = "Vietnam", Economic_Opportunity_Index = 0.33))
  collapsed <- allied_network_collapse_country_index(eo, "Economic_Opportunity_Index", net_country_info, net_iso)
  expect_equal(collapsed$value[collapsed$iso3c == "VNM"], c(0.33, 0.33))
  expect_equal(anyDuplicated(collapsed[c("iso3c", "tech", "supply_chain")]), 0L)
})

test_that("duplicated inputs give exactly the clean-input design", {
  clean <- make_clean_inputs()
  messy <- make_messy_inputs(clean)
  a <- run_design(clean, method = "greedy")
  b <- run_design(messy, method = "greedy")

  key <- function(res) res$specialization %>% dplyr::arrange(tech, supply_chain, iso3c)
  expect_equal(nrow(b$specialization), length(net_iso) * 2)
  expect_equal(key(b)$production_share, key(a)$production_share, tolerance = 1e-12)
  expect_equal(key(b)$connectivity_out, key(a)$connectivity_out, tolerance = 1e-12)
  expect_true(all(b$specialization$connectivity_out <= 1 + 1e-12))
  expect_equal(anyDuplicated(b$flows[c("tech", "supply_chain", "reporter_iso", "partner_iso")]), 0L)
  expect_gt(sum(b$input_checks$dup_keys[b$input_checks$input == "partner_friendshore_tbl"]), 0)
})

test_that("edges are unique and oriented producer -> consumer", {
  iso <- c("USA", "CAN", "MEX")
  pairs <- tidyr::crossing(reporter_iso = iso, partner_iso = iso, tech = "Solar", supply_chain = "Midstream")
  fs <- pairs %>%
    dplyr::mutate(
      category = "friendshore", variable = "Friendshore Index", data_type = "index",
      value = dplyr::case_when(reporter_iso == "USA" & partner_iso == "CAN" ~ 0.9, reporter_iso == "CAN" & partner_iso == "USA" ~ 0.1, TRUE ~ 0.5)
    )
  op <- pairs %>%
    dplyr::mutate(
      category = "opportunity", variable = "Opportunity Index", data_type = "index",
      value = dplyr::case_when(reporter_iso == "CAN" & partner_iso == "USA" ~ 0.5, reporter_iso == "USA" & partner_iso == "CAN" ~ 0.3, TRUE ~ 0.5)
    )
  edges <- allied_network_extract_edges(dplyr::bind_rows(fs, fs), op, iso3c_network = iso)
  expect_equal(nrow(edges), 9L)
  can_to_usa <- edges %>% dplyr::filter(reporter_iso == "CAN", partner_iso == "USA")
  usa_to_can <- edges %>% dplyr::filter(reporter_iso == "USA", partner_iso == "CAN")
  # Producer CAN -> consumer USA: FS as reported by the importer USA about CAN, OP as reported by CAN
  expect_equal(can_to_usa$friendshore_index, 0.9)
  expect_equal(can_to_usa$edge_weight, 0.5 * 0.9 + 0.5 * 0.5)
  expect_equal(usa_to_can$edge_weight, 0.5 * 0.1 + 0.5 * 0.3)

  legacy <- allied_network_extract_edges(fs, op, iso3c_network = iso, friendshore_orientation = "reporter_is_producer")
  expect_equal(legacy$edge_weight[legacy$reporter_iso == "CAN" & legacy$partner_iso == "USA"], 0.5 * 0.1 + 0.5 * 0.5)
})

test_that("duplicate edge pairs and duplicate node rows are refused by the solvers", {
  nodes_stage <- tibble::tibble(
    iso3c = c("USA", "CAN", "MEX", "MEX"), country = c("United States", "Canada", "Mexico", "Mexico"),
    producer_score = c(0.8, 0.6, 0.4, 0.4), demand_weight = c(0.25, 0.25, 0.25, 0.25), dev_potential = 0.5
  )
  edges_stage <- tidyr::crossing(reporter_iso = c("USA", "CAN", "MEX"), partner_iso = c("USA", "CAN", "MEX")) %>%
    dplyr::mutate(edge_weight = 0.6)
  expect_error(allied_network_solve_stage_greedy(nodes_stage, edges_stage), "one row per iso3c")
  expect_error(
    allied_network_complete_edges_stage(dplyr::bind_rows(edges_stage, edges_stage), c("USA", "CAN", "MEX")),
    "duplicate"
  )
})

test_that("greedy selects distinct countries; shares, demand and flows are consistent", {
  res <- run_design(make_messy_inputs(make_clean_inputs()), method = "greedy")
  sp <- res$specialization
  by_stage <- sp %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(
      n_sel = sum(selected), n_sel_iso = dplyr::n_distinct(iso3c[selected]),
      s_share = sum(production_share), s_demand = sum(demand_weight), max_share = max(production_share),
      .groups = "drop"
    )
  expect_true(all(by_stage$n_sel == 3 & by_stage$n_sel_iso == 3))
  expect_equal(by_stage$s_share, rep(1, 2), tolerance = 1e-9)
  expect_equal(by_stage$s_demand, rep(1, 2), tolerance = 1e-9)
  expect_true(all(by_stage$max_share <= 0.40 + 1e-9))
  expect_equal(res$diagnostics$n_producers, c(3L, 3L))

  fl <- res$flows
  supply <- fl %>% dplyr::group_by(tech, supply_chain, iso3c = reporter_iso) %>% dplyr::summarise(supply = sum(flow_share), .groups = "drop")
  chk <- sp %>% dplyr::filter(selected) %>% dplyr::inner_join(supply, by = c("tech", "supply_chain", "iso3c"))
  # Realized supply equals the reported production share (RAS flows)
  expect_equal(chk$supply, chk$production_share, tolerance = 1e-9)
  served <- fl %>% dplyr::group_by(tech, supply_chain, iso3c = partner_iso) %>% dplyr::summarise(served = sum(flow_share), .groups = "drop") %>%
    dplyr::inner_join(sp, by = c("tech", "supply_chain", "iso3c"))
  expect_equal(served$served, served$demand_weight, tolerance = 1e-9)
})

test_that("portfolio counts distinct stages, not rows", {
  topk <- tibble::tibble(
    tech = c("Solar", "Solar", "Solar", "Wind"),
    supply_chain = c("Midstream", "Midstream", "Midstream", "Midstream"),
    iso3c = c("USA", "USA", "USA", "USA"),
    production_share = 0.4, rank_in_stage = 1L, in_top_k = TRUE
  )
  counts <- allied_network_count_topk(topk, c("USA", "CAN"), c(USA = 1L, CAN = 2L))
  expect_equal(counts$topk_count[counts$iso3c == "USA"], 2L)
  expect_equal(counts$over_by[counts$iso3c == "USA"], 1L)
  expect_equal(counts$topk_count[counts$iso3c == "CAN"], 0L)
})

test_that("ARE is the coalition code and its GDP uses the latest finite year", {
  expect_true("ARE" %in% allied_network_default_iso3c())
  expect_false("UAE" %in% allied_network_default_iso3c())

  gdp_raw <- data.frame(
    country = c("United Arab Emirates", "United Arab Emirates", "United States", "Canada"),
    iso2c = c("AE", "AE", "US", "CA"),
    iso3c = c("ARE", "ARE", "USA", "CAN"),
    year = c(2025, 2024, 2025, 2025),
    NY.GDP.MKTP.CD = c(NA, 5.5e11, 3e13, 2e12),
    stringsAsFactors = FALSE
  )
  size <- allied_network_country_size_tbl(gdp_raw, net_country_info, c("USA", "CAN", "ARE", "MEX"))
  are <- size %>% dplyr::filter(iso3c == "ARE")
  expect_equal(are$gdp_usd, 5.5e11)
  expect_equal(are$year, 2024)
  expect_false(are$gdp_imputed)
  mex <- size %>% dplyr::filter(iso3c == "MEX")
  expect_true(mex$gdp_imputed)
  expect_equal(mex$gdp_usd, stats::median(c(5.5e11, 3e13, 2e12)))
})

test_that("missing coalition policy is imputed at the stage median and flagged", {
  res <- run_design(make_clean_inputs(), method = "greedy")
  are <- res$specialization %>% dplyr::filter(iso3c == "ARE")
  expect_true(all(are$policy_imputed))
  expect_false(any(res$specialization$policy_imputed[res$specialization$iso3c != "ARE"]))
  expect_true(all(is.finite(res$specialization$producer_score)))
  expect_equal(res$diagnostics$n_policy_imputed, c(1L, 1L))
  expect_true(all(c("n_selected_policy_imputed", "share_policy_imputed") %in% names(res$diagnostics)))

  nodes <- allied_network_prepare_nodes(
    make_clean_inputs()$eo, make_clean_inputs()$es, make_clean_inputs()$pol, net_country_info,
    iso3c_network = net_iso
  )
  expect_equal(nodes$policy_raw[nodes$iso3c == "ARE"], rep(stats::median(c(0.7, 0.5, 0.3, 0.6)), 2))
  # With every component present the renormalized score is the plain weighted sum
  expect_equal(nodes$producer_score, 0.5 * nodes$eo_idx + 0.3 * nodes$pol_idx + 0.2 * nodes$es_idx, tolerance = 1e-12)

  legacy <- run_design(make_clean_inputs(), method = "greedy", policy_missing = "none", missing_components = "require_all")
  expect_true(all(is.na(legacy$specialization$producer_score[legacy$specialization$iso3c == "ARE"])))
})

test_that("a stage without ES ranks on the other components, and a stage without any signal is skipped", {
  x <- make_clean_inputs(stages = "Upstream")
  x$es <- x$es[0, ]
  eo_up <- c("United States" = 0.9, "Viet Nam" = 0.8, "Canada" = 0.7, "Mexico" = 0.2, "United Arab Emirates" = 0.1)
  x$eo <- x$eo %>% dplyr::mutate(Economic_Opportunity_Index = unname(eo_up[Country]))
  x$fs$value <- 0.6
  x$op$value <- 0.6
  # Wind Downstream: EO rows exist but carry no values, and there is no ES or policy either
  x$eo <- dplyr::bind_rows(x$eo, x$eo %>% dplyr::mutate(tech = "Wind", supply_chain = "Downstream", Economic_Opportunity_Index = NA_real_))

  res <- run_design(x, method = "greedy")
  sel <- res$specialization %>% dplyr::filter(tech == "Solar", selected) %>% dplyr::pull(iso3c) %>% sort()
  # Alphabetical order would have picked ARE, CAN, MEX
  expect_equal(sel, sort(c("USA", "VNM", "CAN")))
  expect_true(all(res$specialization$node_components_n[res$specialization$tech == "Solar"] == 2L))
  # Mixed demand falls back to the size term instead of zero when ES is missing
  solar <- res$specialization %>% dplyr::filter(tech == "Solar")
  expect_equal(solar$demand_weight, solar$size_idx / sum(solar$size_idx), tolerance = 1e-12)
  expect_gt(sum(solar$demand_weight > 0), 3)

  wind <- res$diagnostics %>% dplyr::filter(tech == "Wind")
  expect_equal(wind$method, "skipped_no_node_signal")
  expect_false(any(res$specialization$tech == "Wind"))
})

test_that("ties are broken deterministically, independent of row order", {
  x <- make_clean_inputs(stages = "Midstream")
  x$eo$Economic_Opportunity_Index <- 0.5
  x$es$Energy_Security_Index <- 0.5
  x$pol$value <- 0.5
  x$fs$value <- 0.6
  x$op$value <- 0.6
  a <- run_design(x, method = "greedy")
  y <- x
  y$eo <- y$eo[rev(seq_len(nrow(y$eo))), ]
  y$fs <- y$fs[rev(seq_len(nrow(y$fs))), ]
  b <- run_design(y, method = "greedy")
  pick <- function(res) sort(res$specialization$iso3c[res$specialization$selected])
  expect_equal(pick(a), pick(b))
  expect_equal(pick(a), c("ARE", "CAN", "MEX"))
})

test_that("MILP runs with a development table and reports the method per stage", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_messy_inputs(make_clean_inputs())
  res <- run_design(x, dev = make_dyadic_dev(), method = "milp", min_suppliers_per_consumer = 2, epsilon_supplier_share = 0.10)
  expect_true(all(res$diagnostics$method == "milp"))
  expect_identical(res$params$method, "milp")
  expect_identical(res$params$methods_used, "milp")
  expect_true("connectivity_out" %in% names(res$build_candidates))

  sp <- res$specialization
  by_stage <- sp %>% dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(s = sum(production_share), m = max(production_share), n = dplyr::n(), .groups = "drop")
  expect_equal(by_stage$s, c(1, 1), tolerance = 1e-6)
  expect_true(all(by_stage$m <= 0.40 + 1e-6))
  expect_true(all(by_stage$n == length(net_iso)))
  expect_true(all(res$diagnostics$min_suppliers_realized >= 2))
  expect_true(all(res$diagnostics$min_supplier_share_realized >= 0.10 - 1e-6))

  auto <- run_design(x, method = "auto")
  expect_true(all(auto$diagnostics$method == "milp"))
  expect_identical(auto$params$method, "auto")
})

test_that("an infeasible MILP stage falls back to greedy and says why", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  events <- list()
  cb <- function(info) if (identical(info$event, "fallback_greedy")) events[[length(events) + 1]] <<- info
  # Six suppliers per consumer cannot be met with five members
  res <- run_design(make_clean_inputs(stages = "Midstream"), method = "milp", min_suppliers_per_consumer = 6, progress_callback = cb)
  expect_identical(res$diagnostics$method, "greedy")
  expect_identical(res$diagnostics$method_requested, "milp")
  expect_false(is.na(res$diagnostics$fallback_reason))
  expect_length(events, 1)
  expect_identical(res$params$methods_used, "greedy")
  expect_equal(sum(res$specialization$production_share), 1, tolerance = 1e-9)
})

test_that("greedy flows follow the MILP supplier rules without breaking the share caps", {
  # Producer 2 barely reaches consumer 3, so the plain RAS fit leaves it a 1% sliver of that demand
  K <- matrix(c(1, 1, 1,
                1, 1, 0.01,
                1, 1, 1), nrow = 3, byrow = TRUE)
  row_target <- c(0.4, 0.3, 0.3)
  col_target <- rep(1 / 3, 3)
  f0 <- allied_network_ras(K, row_target, col_target)
  expect_lt(f0[2, 3] / col_target[3], 0.10)
  f1 <- allied_network_prune_small_flows(f0, K, row_target, col_target, min_suppliers = 2, epsilon = 0.10)
  expect_equal(f1[2, 3], 0)
  expect_equal(rowSums(f1), row_target, tolerance = 1e-9)
  expect_equal(colSums(f1), col_target, tolerance = 1e-9)
  expect_true(all(colSums(f1 > 0) >= 2))

  # A consumer already at the minimum supplier count keeps its small supplier
  f2 <- allied_network_prune_small_flows(f0, K, row_target, col_target, min_suppliers = 3, epsilon = 0.10)
  expect_equal(f2, f0)

  res <- run_design(make_clean_inputs(), method = "greedy", min_suppliers_per_consumer = 2, epsilon_supplier_share = 0.10)
  expect_true(all(res$diagnostics$min_suppliers_realized >= 2))
  expect_true(all(res$diagnostics$min_supplier_share_realized >= 0.10 - 1e-9))
})

test_that("greedy skips a producer that cannot reach any demand instead of breaking max_share", {
  # USA has the best node score but every edge out of it is 0 and self-supply is off, so the flow fit
  # could never give it a share; the other producers would absorb it past max_share.
  x <- make_clean_inputs(stages = "Midstream")
  x$fs <- x$fs %>% dplyr::mutate(value = dplyr::if_else(partner_iso == "USA", 0, value))
  x$op <- x$op %>% dplyr::mutate(value = dplyr::if_else(reporter_iso == "USA", 0, value))
  res <- run_design(x, method = "greedy", allow_self = FALSE)
  sp <- res$specialization
  expect_false(sp$selected[sp$iso3c == "USA"])
  expect_equal(sum(sp$selected), 3L)
  expect_true(all(sp$production_share[sp$selected] > 0))
  expect_lte(max(sp$production_share), 0.40 + 1e-9)
  expect_equal(sum(sp$production_share), 1, tolerance = 1e-9)
})
