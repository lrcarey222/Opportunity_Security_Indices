# A small synthetic bundle with every component of the real one (pure; no IO): 8 economies (one
# territory; two members sharing the network node "XEU"), 3 technologies (one fossil) x 3 stages,
# 4 ES categories (one never observed) and 3 EO categories with 2 variables each, three policy
# components (IEA PAMS, CAT and a stale NIPO), Friendshore / Opportunity dyad components with gaps,
# Development for half the partners and a 6-member network (5 nodes) over 2 technologies, and the
# endowment screen of the allied picks over two Upstream stages (Coal, Solar; explicit FALSE rows and
# countries without a row). The policy components' pipeline order (bundle$policy$components) is
# IEA PAMS, CAT, NIPO (not alphabetical), so the policy_basis labels exercise the reordering. The
# pipeline-format tables it was built from are attached as attr(bundle, "fixture_raw"), so tests
# can compute the pipeline's own results on the same data. Used by the tests and the synthetic fixtures.

# policy_required: the required Policy components (bundle$policy$required, dash_policy()); NULL = all.
dash_fixture_bundle <- function(seed = 1, policy_required = NULL) {
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)

  countries <- tibble::tibble(
    iso3c = c("BRA", "CHN", "DEU", "FRA", "HKG", "IND", "JPN", "USA"),
    country = c("Brazil", "China", "Germany", "France", "Hong Kong SAR, China", "India", "Japan", "United States"),
    region = c("Latin America & Caribbean", "East Asia & Pacific", "Europe & Central Asia", "Europe & Central Asia",
               "East Asia & Pacific", "South Asia", "East Asia & Pacific", "North America"),
    income_group = c("Upper middle income", "Upper middle income", "High income", "High income", "High income",
                     "Lower middle income", "High income", "High income"),
    sovereign = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    tier = c("partner", "none", "core", "core", "none", "partner", "core", "core"),
    network_node = c("BRA", NA, "XEU", "XEU", NA, "IND", "JPN", "USA"),
    eu_member = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    lon = c(-51.9, 104.2, 10.4, 2.2, 114.1, 78.9, 138.3, -98.6),
    lat = c(-14.2, 35.9, 51.2, 46.2, 22.3, 20.6, 36.2, 39.8),
    bloc = c("Nonaligned", "China-leaning", "U.S.-leaning", "U.S.-leaning", "China-leaning", "Nonaligned", "U.S.-leaning", "U.S.-leaning"),
    p_us = c(0.5, 0, 1, 1, 0, 0.5, 1, 1)
  )
  techs <- c("Solar", "Wind", "Coal")
  stages <- DASH_STAGES
  cells <- dash_partner_cells_tbl(techs, stages)
  cells$sector <- paste(cells$tech, cells$supply_chain, sep = " - ")
  grid <- expand.grid(Country = countries$country, cell = cells$cell_id, stringsAsFactors = FALSE)
  grid$tech <- cells$tech[grid$cell]
  grid$supply_chain <- cells$supply_chain[grid$cell]

  # ---- Pillars ----
  make_pillar <- function(categories, constant_cell = NULL) {
    cat_rows <- list()
    var_rows <- list()
    for (k in seq_len(nrow(grid))) {
      for (cg in categories) {
        if (stats::runif(1) < 0.15) next
        imp <- stats::runif(1) < 0.3
        score <- if (stats::runif(1) < 0.08) NA_real_ else round(stats::runif(1), 6)
        if (!is.null(constant_cell) && grid$tech[k] == constant_cell[1] && grid$supply_chain[k] == constant_cell[2]) score <- 0.3
        cat_rows[[length(cat_rows) + 1]] <- tibble::tibble(
          Country = grid$Country[k], tech = grid$tech[k], supply_chain = grid$supply_chain[k], category = cg,
          category_score = score, imputed = imp, missing_rule_applied = if (imp) "global_average" else NA_character_,
          sub_sector = "All"
        )
        v_imp <- c(imp, imp && stats::runif(1) < 0.5)
        var_rows[[length(var_rows) + 1]] <- tibble::tibble(
          Country = grid$Country[k], tech = grid$tech[k], supply_chain = grid$supply_chain[k], category = cg,
          score_variable = paste("Overall", cg, "Index"), variable = paste(cg, c("a", "b")),
          component_value = round(stats::runif(2), 6), component_weight_within_overall = c(0.5, 0.5),
          missing_rule_applied = ifelse(v_imp, "global_average", NA_character_), imputed = v_imp
        )
      }
    }
    list(category_scores = dplyr::bind_rows(cat_rows), variable_contributions = dplyr::bind_rows(var_rows))
  }
  es_raw <- make_pillar(c("Imports", "Reserves", "Trade"), constant_cell = c("Wind", "Upstream"))
  eo_raw <- make_pillar(c("Production", "Technological Readiness", "Trade"))
  es_weights <- c(Imports = 4, Prices = 2, Reserves = 3, Trade = 2)
  eo_weights <- c(Production = 3, `Technological Readiness` = 2, Trade = 3)
  country_tbl <- tibble::tibble(Country = countries$country, iso3c = countries$iso3c, sovereign = countries$sovereign)
  es_in_data <- dash_sort(es_raw$category_scores$category[!is.na(es_raw$category_scores$category_score)])
  eo_in_data <- dash_sort(eo_raw$category_scores$category[!is.na(eo_raw$category_scores$category_score)])
  pillars <- list(
    es = dash_bundle_pillar(es_raw$category_scores, es_raw$variable_contributions, es_in_data, country_tbl),
    eo = dash_bundle_pillar(eo_raw$category_scores, eo_raw$variable_contributions, eo_in_data, country_tbl)
  )

  # ---- Policy ----
  pol_rows <- list()
  for (k in seq_len(nrow(grid))) {
    ctry <- grid$Country[k]
    if (ctry != "Brazil" && stats::runif(1) < 0.9) {
      pol_rows[[length(pol_rows) + 1]] <- tibble::tibble(Country = ctry, tech = grid$tech[k], supply_chain = grid$supply_chain[k],
                                                         variable = "IEA PAMS Policy Index", value = round(stats::runif(1), 6))
    }
    if (!ctry %in% c("India")) {
      pol_rows[[length(pol_rows) + 1]] <- tibble::tibble(Country = ctry, tech = grid$tech[k], supply_chain = grid$supply_chain[k],
                                                         variable = "CAT Policy Index", value = c(0.25, 0.5, 0.75, 1)[(match(ctry, countries$country) %% 4) + 1])
    }
    if (grid$supply_chain[k] != "Downstream" && stats::runif(1) < 0.7) {
      pol_rows[[length(pol_rows) + 1]] <- tibble::tibble(Country = ctry, tech = grid$tech[k], supply_chain = grid$supply_chain[k],
                                                         variable = "NIPO Policy Index", value = round(stats::runif(1), 6))
    }
  }
  policy_component_tbl <- dplyr::bind_rows(pol_rows)
  # a duplicated component row (the pipeline averages duplicates)
  policy_component_tbl <- dplyr::bind_rows(policy_component_tbl, dplyr::mutate(policy_component_tbl[1, ], value = 0.123))
  policy_weights <- c(`CAT Policy Index` = 2, `IEA PAMS Policy Index` = 3, `NIPO Policy Index` = 4)
  policy <- dash_bundle_policy(policy_component_tbl, names(policy_weights), techs)
  if (!is.null(policy_required)) policy$required <- dash_sort(policy_required)
  policy$components <- c("IEA PAMS Policy Index", "CAT Policy Index", "NIPO Policy Index")

  # ---- Strategic Index inputs ----
  tech_ghg <- tibble::tibble(tech = c("Solar", "Coal"), ghg_index = c(0.97, 0))
  trl <- eo_raw$category_scores[eo_raw$category_scores$category == "Technological Readiness", c("Country", "tech", "supply_chain", "category_score")]
  names(trl)[4] <- "trl_index"
  members_tbl <- countries[countries$tier %in% c("core", "partner"), ]
  allies <- tibble::tibble(Country = members_tbl$country, iso3 = members_tbl$iso3c, tier = members_tbl$tier, network_node = members_tbl$network_node)
  # strategic_resource_base() output for the screened stages Coal - Upstream and Solar - Upstream:
  # explicit FALSE rows (a zero reserve) and countries without a row (no recorded resources: FALSE)
  resource_base <- tibble::tibble(
    Country = c("China", "India", "Japan", "United States", "Brazil", "China", "Germany", "United States"),
    tech = c("Coal", "Coal", "Coal", "Coal", "Solar", "Solar", "Solar", "Solar"),
    supply_chain = "Upstream",
    resource_base = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
    resource_evidence = c("Production; Reserves", "Reserves", NA, "Production; Reserves", "Reserves", "Production", NA, NA)
  )
  si <- list(tech_ghg = tech_ghg, trl = trl, allies = allies, resource_base = resource_base)

  # ---- Partner dyads ----
  U <- sort(countries$iso3c)
  Un <- length(U)
  Cn <- nrow(cells)
  mask_fs <- array(stats::runif(Un * Un * Cn) < 0.9, c(Un, Un, Cn))
  mask_po <- array(stats::runif(Un * Un * Cn) < 0.85, c(Un, Un, Cn))
  for (c in seq_len(Cn)) {
    diag(mask_fs[, , c]) <- FALSE
    diag(mask_po[, , c]) <- FALSE
  }
  rnd <- function(n, p_na = 0) {
    v <- round(stats::runif(n), 6)
    v[stats::runif(n) < p_na] <- NA_real_
    v
  }
  imp_trade <- array(rnd(Un * Un * Cn), c(Un, Un, Cn))
  econ_opp_raw <- matrix(rnd(Un * Cn, 0.05), Un, Cn)
  es_need <- matrix(rnd(Un * Cn, 0.1), Un, Cn)
  eo_partner <- matrix(rnd(Un * Cn, 0.05), Un, Cn)
  outbound <- matrix(rnd(Un * Un, 0.5), Un, Un)
  geo <- matrix(sample(c(0, 0.5, 1), Un * Un, replace = TRUE), Un, Un)
  geo[lower.tri(geo)] <- t(geo)[lower.tri(geo)]
  geo[sample(Un * Un, 4)] <- NA_real_
  trade <- array(rnd(Un * Un * Cn), c(Un, Un, Cn))
  econ_opp_index <- matrix(rnd(Un * Cn), Un, Cn)
  partner_es_need <- matrix(rnd(Un * Cn), Un, Cn)
  fs_long <- {
    idx <- which(mask_fs, arr.ind = TRUE)
    base <- tibble::tibble(reporter_iso = U[idx[, 1]], partner_iso = U[idx[, 2]], tech = cells$tech[idx[, 3]], supply_chain = cells$supply_chain[idx[, 3]])
    vals <- list(
      imp_trade_index = imp_trade[idx],
      econ_opp_raw = econ_opp_raw[idx[, c(1, 3)]],
      es_need = es_need[idx[, c(1, 3)]],
      eo_partner = eo_partner[idx[, c(2, 3)]],
      outbound_index = outbound[idx[, c(1, 2)]],
      geo_alignment = geo[idx[, c(1, 2)]],
      `Friendshore Index` = rep(NA_real_, nrow(idx))
    )
    dplyr::bind_rows(lapply(names(vals), function(v) dplyr::mutate(base, variable = v, value = vals[[v]])))
  }
  po_long <- {
    idx <- which(mask_po, arr.ind = TRUE)
    base <- tibble::tibble(reporter_iso = U[idx[, 1]], partner_iso = U[idx[, 2]], tech = cells$tech[idx[, 3]], supply_chain = cells$supply_chain[idx[, 3]])
    vals <- list(
      trade_index = trade[idx],
      econ_opp_index = econ_opp_index[idx[, c(1, 3)]],
      partner_es_need = partner_es_need[idx[, c(2, 3)]],
      `Opportunity Index` = rep(NA_real_, nrow(idx))
    )
    dplyr::bind_rows(lapply(names(vals), function(v) dplyr::mutate(base, variable = v, value = vals[[v]])))
  }
  fs_spec <- c(imp_trade_index = "rpc", econ_opp_raw = "rc", es_need = "rc", eo_partner = "pc", outbound_index = "rp", geo_alignment = "rp")
  po_spec <- c(trade_index = "rpc", econ_opp_index = "rc", partner_es_need = "pc")
  fsb <- dash_bundle_dyads(fs_long, U, techs, fs_spec, "Friendshore Index", stages)
  pob <- dash_bundle_dyads(po_long, U, techs, po_spec, "Opportunity Index", stages)
  tech_ghg_partner <- tibble::tibble(tech = c("Solar", "Wind", "Coal"), ghg_index = c(0.97, 0.99, 0))
  cpi_tbl <- tibble::tibble(iso3c = c("BRA", "DEU", "FRA", "IND", "JPN", "USA"), climate_policy_index = c(0.5, 0.75, 0.75, 0.5, 0.25, 0.5))
  gh <- dash_bundle_gh(tech_ghg_partner, cells)
  cpi <- dash_bundle_cpi(cpi_tbl, U)
  dp_iso <- c("BRA", "CHN", "IND", "HKG")
  dp <- tibble::tibble(partner_iso = dp_iso, development = round(stats::runif(length(dp_iso)), 6), imputed_weight_share = round(stats::runif(length(dp_iso)) * 0.6, 6))
  share_fs <- matrix(round(stats::runif(Un * Un) * 0.7, 6), Un, Un, dimnames = list(U, U))
  share_po <- matrix(round(stats::runif(Un * Un) * 0.7, 6), Un, Un, dimnames = list(U, U))
  partner <- list(
    U = U,
    names = countries$country[match(U, countries$iso3c)],
    sovereign = countries$sovereign[match(U, countries$iso3c)],
    cells = cells[c("cell_id", "tech", "supply_chain")],
    fs = list(imp_trade = fsb$imp_trade_index, econ_opp_raw = fsb$econ_opp_raw, es_need = fsb$es_need, eo_partner = fsb$eo_partner,
              outbound = fsb$outbound_index, geo = fsb$geo_alignment, mask = fsb$mask, cpi = cpi, gh = gh),
    po = list(trade = pob$trade_index, econ_opp_index = pob$econ_opp_index, partner_es_need = pob$partner_es_need,
              mask = pob$mask, cpi = cpi, gh = gh),
    dp = dp,
    top5_share = list(fs = share_fs, po = share_po),
    psi = list(coalition_reporters = members_tbl$iso3c, tau = 0.5)
  )

  # ---- Network ----
  net_members <- tibble::tibble(
    iso3c = members_tbl$iso3c, country = members_tbl$country, tier = members_tbl$tier,
    network_node = members_tbl$network_node, node_tier = members_tbl$tier, node_only = FALSE
  )
  net_techs <- c("Solar", "Coal")
  nr <- expand.grid(iso3c = net_members$iso3c, tech = net_techs, supply_chain = stages, stringsAsFactors = FALSE)
  gdp <- c(BRA = 2.1e12, DEU = 4.5e12, FRA = 3.0e12, IND = 3.6e12, JPN = 4.2e12, USA = 27e12)
  node_raw_member <- tibble::tibble(
    iso3c = nr$iso3c,
    country = net_members$country[match(nr$iso3c, net_members$iso3c)],
    tech = nr$tech,
    supply_chain = nr$supply_chain,
    eo_raw = rnd(nrow(nr)),
    es_raw = rnd(nrow(nr), 0.1),
    policy_raw = rnd(nrow(nr), 0.2),
    dev_potential = NA_real_,
    gdp_usd = unname(gdp[nr$iso3c]),
    gdp_imputed = FALSE
  )
  node_raw_member <- node_raw_member[order(node_raw_member$iso3c, node_raw_member$tech, node_raw_member$supply_chain, method = "radix"), ]
  nst <- expand.grid(tech = net_techs, supply_chain = stages, stringsAsFactors = FALSE)
  node_member_weights <- tibble::tibble(
    network_node = "XEU",
    iso3 = rep(c("DEU", "FRA"), times = nrow(nst)),
    tech = rep(nst$tech, each = 2),
    supply_chain = rep(nst$supply_chain, each = 2),
    weight = rep(c(0.6, 0.4), times = nrow(nst)),
    basis = "exports"
  )
  tr <- expand.grid(exporter_iso = net_members$iso3c, importer_iso = net_members$iso3c, tech = net_techs, supply_chain = stages, stringsAsFactors = FALSE)
  tr <- tr[tr$exporter_iso != tr$importer_iso, ]
  tr$value <- round(stats::runif(nrow(tr)) * 1e9)
  tr$value[stats::runif(nrow(tr)) < 0.3] <- NA_real_
  node_trade <- tibble::as_tibble(tr[!is.na(tr$value), ])
  country_size <- tibble::tibble(iso3c = net_members$iso3c, country = net_members$country, year = 2024, gdp_usd = unname(gdp[net_members$iso3c]), gdp_imputed = FALSE)
  network <- list(
    members = net_members,
    node_raw_member = node_raw_member,
    node_member_weights = node_member_weights,
    node_trade = node_trade,
    country_size = country_size,
    node_labels = c(XEU = "Euro bloc"),
    country_info = tibble::tibble(iso3c = countries$iso3c, country = countries$country),
    country_map = tibble::tibble(Country = countries$country, iso3c = countries$iso3c, is_canonical = TRUE),
    stored = NULL,
    greedy_baseline = NULL,
    observed_hhi = tibble::tibble(tech = c("Solar", "Coal"), supply_chain = c("Midstream", "Upstream"), n_nodes_exporting = c(4L, 3L),
                                  total_exports = c(1e10, 5e9), hhi = c(0.31, 0.45), top_node = c("USA", "BRA"),
                                  top_share = c(0.45, 0.6), effective_exporters = 1 / c(0.31, 0.45))
  )

  # ---- Dims and manifest ----
  node_tbl <- net_members %>%
    dplyr::group_by(.data$network_node) %>%
    dplyr::summarise(n_members = dplyr::n(), tier = dplyr::first(.data$node_tier), .groups = "drop")
  node_tbl$label <- ifelse(node_tbl$network_node == "XEU", "Euro bloc", countries$country[match(node_tbl$network_node, countries$iso3c)])
  node_tbl$lon <- vapply(node_tbl$network_node, function(n) mean(countries$lon[countries$network_node %in% n]), numeric(1))
  node_tbl$lat <- vapply(node_tbl$network_node, function(n) mean(countries$lat[countries$network_node %in% n]), numeric(1))
  dims <- list(
    countries = countries,
    nodes = node_tbl[c("network_node", "label", "lon", "lat", "n_members", "tier")],
    techs = techs,
    stages = stages,
    cells = cells,
    fossil_techs = "Coal",
    es_categories = names(es_weights),
    eo_categories = names(eo_weights),
    U = U
  )
  baseline_inputs <- list(
    es_weights = es_weights,
    eo_weights = eo_weights,
    tau = 0.5,
    allow_partial_categories = TRUE,
    policy_weights = policy_weights,
    policy_iea_var = "IEA PAMS Policy Index",
    policy_cat_var = "CAT Policy Index",
    si_weights = c(policy = 0.2, economic_opportunity = 0.3, energy_security_need = 0.3, supply_chain = 0.1, technology = 0.1),
    stage_weights = c(Upstream = 0.5, Midstream = 0.75, Downstream = 0.25),
    excluded_sectors = "Wind - Midstream",
    allied_excluded_techs = c("Geothermal", "Green Hydrogen"),
    require_resource_base = TRUE,
    fossil_techs = "Coal",
    fossil_threshold = 90,
    fs_weights = list(imp_trade_index = 2, econ_opp_raw = 0, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = 2),
    fs_penalty = 0.10,
    po_weights = list(trade_index = 3, econ_opp_index = 3, partner_es_need = 1),
    po_penalty = 0.05,
    psi_weights = c(friendshore = 0.4, opportunity = 0.4, development = 0.2),
    psi_view = "coalition",
    psi_tau = 0.5,
    top_k = 5L,
    node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
    node_policy = FALSE,
    node_policy_weight_max = 0.3,
    edge_weights = list(friendshore = 0.5, opportunity = 0.5),
    demand_weights = list(need = 0.5, size = 0.5),
    lambda = 1.5,
    max_share = 0.40,
    min_share = 0.05,
    min_producers = 3L,
    min_suppliers_per_consumer = 2L,
    epsilon_supplier_share = 0.10,
    w_node = 1,
    w_edge = 0.5,
    w_dev = 0,
    allow_self = TRUE,
    portfolio = list(enable = TRUE, top_k = 5L, min_cap = 2, max_cap = 10, transform = "log", max_iters = 30L),
    diversity_breakpoint_step = 0.01,
    supply_chains = c("Upstream", "Midstream"),
    milp_time_limit = 60,
    policy_missing = "coalition_median",
    missing_components = "renormalize",
    demand_mode = "mixed",
    size_transform = "log_scurve"
  )
  manifest <- list(
    bundle_version = 1L,
    created = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    git_commit = NA_character_,
    osi_branch = NA_character_,
    fixture = TRUE,
    sources = tibble::tibble(path = character(), mtime = as.POSIXct(character()), size = numeric(), md5 = character()),
    flags = list(pillar_rule = TRUE, geo_alignment_in_fs = TRUE, partner_pillar_consistent = FALSE,
                 network_aggregation = TRUE, network_consistent = FALSE, milp_baseline = FALSE),
    baseline_inputs = baseline_inputs,
    validation = tibble::tibble(check = character(), layer = character(), status = character(), n = integer(),
                                max_abs_diff = numeric(), note = character()),
    notes = "Synthetic fixture bundle (dash_fixture_bundle()); not real data."
  )
  bundle <- dash_bundle_assemble(manifest, dims, pillars, policy, si, partner, network)
  attr(bundle, "fixture_raw") <- list(
    es = es_raw, eo = eo_raw, es_categories_in_data = es_in_data, eo_categories_in_data = eo_in_data,
    country_tbl = country_tbl, policy_component_tbl = policy_component_tbl, tech_ghg = tech_ghg, trl = trl,
    resource_base = resource_base,
    fs_long = fs_long, po_long = po_long, tech_ghg_partner = tech_ghg_partner, cpi_tbl = cpi_tbl,
    fs_checks = fsb$checks, po_checks = pob$checks
  )
  bundle
}
