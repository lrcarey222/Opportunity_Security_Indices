# A small synthetic bundle with every component of the real one (pure; no IO): 8 economies (one
# territory; two members sharing the network node "XEU"), 3 technologies (one fossil) x 3 stages,
# 4 ES categories (one never observed) and 3 EO categories with 2 variables each, three policy
# components (IEA PAMS, CAT and a stale NIPO), Friendshore / Opportunity dyad components with gaps,
# Friendshore's CM-FDI parts (Solar and Wind Midstream are CM-FDI cells: investment ties with
# reporters that have no project there, and Chinese-parent ownership defined for some partners only,
# 1 for CHN and HKG, undefined for JPN), Development for half the partners, and the
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

  # ---- (retired) network inputs ----
  # The original allied network design is retired (2026-10-17): its synthetic inputs (member-level
  # node inputs and basket trade of the 6 members over 2 technologies) are no longer built, but their
  # 576 random draws are kept so the CM-FDI parts drawn after them keep their values.
  n_net <- nrow(members_tbl) * 2L * length(stages)
  invisible(stats::runif(3L * 2L * n_net + 2L * (n_net * nrow(members_tbl) - n_net)))

  # ---- Friendshore's CM-FDI parts (drawn last, so the other synthetic inputs keep their values) ----
  # In the CM-FDI cells the outbound rows are investment ties (NA for a reporter whose parents have no
  # project in the cell: FRA in Solar Midstream, IND in both) and their source names CM-FDI;
  # chinese_parent_exposure rows exist only where it is defined, as in the pipeline's long table.
  cm_cells <- cells$cell_id[cells$tech %in% c("Solar", "Wind") & cells$supply_chain == "Midstream"]
  tie <- array(rnd(Un * Un * length(cm_cells), 0.3), c(Un, Un, length(cm_cells)))
  tie[match("FRA", U), , 1] <- NA_real_
  tie[match("IND", U), , ] <- NA_real_
  cpe <- matrix(NA_real_, Un, Cn)
  cpe[, cm_cells] <- rnd(Un * length(cm_cells), 0.25)
  cpe[match(c("CHN", "HKG"), U), cm_cells] <- 1
  cpe[match("JPN", U), cm_cells] <- NA_real_
  fs_long$source <- "Author calculation"
  ci_long <- (match(fs_long$tech, techs) - 1L) * length(stages) + match(fs_long$supply_chain, stages)
  ob_cm <- fs_long$variable == "outbound_index" & ci_long %in% cm_cells
  fs_long$value[ob_cm] <- tie[cbind(match(fs_long$reporter_iso[ob_cm], U), match(fs_long$partner_iso[ob_cm], U),
                                    match(ci_long[ob_cm], cm_cells))]
  fs_long$source[ob_cm] <- "Cleantech Manufacturing FDI Dataset (CM-FDI); author calculation"
  cpe_rows <- fs_long[fs_long$variable == "imp_trade_index" & ci_long %in% cm_cells, c("reporter_iso", "partner_iso", "tech", "supply_chain")]
  cpe_rows$variable <- "chinese_parent_exposure"
  cpe_rows$value <- cpe[cbind(match(cpe_rows$partner_iso, U), (match(cpe_rows$tech, techs) - 1L) * length(stages) + match(cpe_rows$supply_chain, stages))]
  cpe_rows$source <- "Cleantech Manufacturing FDI Dataset (CM-FDI); author calculation"
  fs_long <- dplyr::bind_rows(fs_long, cpe_rows[!is.na(cpe_rows$value), ])
  cm <- dash_bundle_fs_cmfdi(fs_long, techs, stages)
  fs_spec <- c(fs_spec, outbound_index_cmfdi = "rpc", chinese_parent_exposure = "pc")
  fsb <- dash_bundle_dyads(cm$long, U, techs, fs_spec, "Friendshore Index", stages)
  cm_parts <- dash_bundle_fs_cmfdi_parts(fsb, cm)
  partner$fs <- list(imp_trade = fsb$imp_trade_index, econ_opp_raw = fsb$econ_opp_raw, es_need = fsb$es_need, eo_partner = fsb$eo_partner,
                     outbound = fsb$outbound_index, outbound_source = cm_parts$outbound_source, outbound_cmfdi = cm_parts$outbound_cmfdi,
                     cpe = cm_parts$cpe, geo = fsb$geo_alignment, mask = fsb$mask, cpi = cpi, gh = gh)

  # ---- Dims and manifest ----
  node_tbl <- members_tbl %>%
    dplyr::group_by(.data$network_node) %>%
    dplyr::summarise(n_members = dplyr::n(), tier = dplyr::first(.data$tier), .groups = "drop")
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
    fs_weights = list(imp_trade_index = 2, econ_opp_raw = 0, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = 2,
                      chinese_parent_exposure = 1),
    fs_penalty = 0.10,
    po_weights = list(trade_index = 3, econ_opp_index = 3, partner_es_need = 1),
    po_penalty = 0.05,
    psi_weights = c(friendshore = 0.4, opportunity = 0.4, development = 0.2),
    psi_view = "coalition",
    psi_tau = 0.5,
    top_k = 5L
  )
  manifest <- list(
    bundle_version = 1L,
    created = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    git_commit = NA_character_,
    osi_branch = NA_character_,
    fixture = TRUE,
    sources = tibble::tibble(path = character(), mtime = as.POSIXct(character()), size = numeric(), md5 = character()),
    flags = list(pillar_rule = TRUE, geo_alignment_in_fs = TRUE, fs_cmfdi = TRUE, partner_pillar_consistent = FALSE),
    baseline_inputs = baseline_inputs,
    validation = tibble::tibble(check = character(), layer = character(), status = character(), n = integer(),
                                max_abs_diff = numeric(), note = character()),
    notes = "Synthetic fixture bundle (dash_fixture_bundle()); not real data."
  )
  bundle <- dash_bundle_assemble(manifest, dims, pillars, policy, si, partner)
  attr(bundle, "fixture_raw") <- list(
    es = es_raw, eo = eo_raw, es_categories_in_data = es_in_data, eo_categories_in_data = eo_in_data,
    country_tbl = country_tbl, policy_component_tbl = policy_component_tbl, tech_ghg = tech_ghg, trl = trl,
    resource_base = resource_base,
    fs_long = fs_long, po_long = po_long, tech_ghg_partner = tech_ghg_partner, cpi_tbl = cpi_tbl,
    fs_checks = fsb$checks, po_checks = pob$checks, cm_fdi = cm[c("cells", "outbound_source", "n_rows", "n_mixed")]
  )
  bundle
}
