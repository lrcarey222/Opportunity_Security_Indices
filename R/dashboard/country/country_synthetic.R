# Deterministic synthetic inputs for Country Insights (tests and the dashboard fixture; spec_country.md
# A7.1 --synthetic). Ten economies chosen to exercise every path: DEU (core EU member with Upgrade
# sectors and the EU's Climate Action Tracker rating), USA (core, no Upgrade sector: the top_si_fallback
# path; since the country-median quadrant rule of 2026-10-15 its two pillars share their random draws, so
# its sectors rank alike on both), FRA, JPN, KOR (core), BRA (coalition partner), CHN, VNM (outside), NGA
# (outside, no Strategic Index: no policy records), PRI (a territory: not sovereign, nothing placed,
# alignment inherited from the United States). Pure: cfg and dict are passed in.

COUNTRY_SYNTH_ROSTER <- data.frame(
  iso3 = c("USA", "DEU", "FRA", "JPN", "KOR", "BRA", "CHN", "VNM", "NGA", "PRI"),
  name = c("United States", "Germany", "France", "Japan", "South Korea", "Brazil", "China", "Vietnam", "Nigeria", "Puerto Rico"),
  tier = c("core", "core", "core", "core", "core", "partner", "none", "none", "none", "none"),
  routes = c("core; agreement; supplier", "core; agreement; supplier", "core; agreement", "core; agreement; supplier", "core; agreement",
             "agreement; supplier", "none", "none", "none", "none"),
  region = c("North America", "Europe & Central Asia", "Europe & Central Asia", "East Asia & Pacific", "East Asia & Pacific",
             "Latin America & Caribbean", "East Asia & Pacific", "East Asia & Pacific", "Sub-Saharan Africa", "Latin America & Caribbean"),
  income_group = c("High income", "High income", "High income", "High income", "High income", "Upper middle income", "Upper middle income",
                   "Lower middle income", "Lower middle income", "High income"),
  sovereign = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  eu_member = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  network_node = c("USA", "EUU", "EUU", "JPN", "KOR", "BRA", NA, NA, NA, NA),
  bloc = c("U.S.-leaning", "U.S.-leaning", "U.S.-leaning", "U.S.-leaning", "U.S.-leaning", "Nonaligned", "China-leaning", "Nonaligned",
           "Nonaligned", "U.S.-leaning"),
  s_axis = c(1, 0.82, 0.8, 0.85, 0.83, 0.45, 0, 0.3, 0.35, 1),
  alignment_basis = c(rep("own_votes", 9), "inherited"),
  alignment_from = c(rep(NA, 9), "USA"),
  iso_n3 = c("840", "276", "250", "392", "410", "076", "156", "704", "566", "630"),
  stringsAsFactors = FALSE
)

country_synthetic_ctx <- function(cfg, dict, seed = 42L) {
  set.seed(seed)
  ro <- COUNTRY_SYNTH_ROSTER
  cells <- country_cells_tbl()
  isos <- ro$iso3
  # ---- category scores -> pillar values (sum of weight x score is the pillar, as in the pipeline)
  es_cats <- c("Energy Imports", "Reserves", "Trade")
  eo_cats <- c("Investment", "Production", "Trade")
  bias_es <- c(USA = 0.35, DEU = -0.1, FRA = -0.1, JPN = 0.1, KOR = -0.05, BRA = 0.05, CHN = 0.1, VNM = -0.1, NGA = 0, PRI = 0)
  bias_eo <- c(USA = 0.15, DEU = 0.25, FRA = 0.1, JPN = 0.15, KOR = 0.1, BRA = -0.05, CHN = 0.3, VNM = -0.1, NGA = -0.2, PRI = -0.1)
  clip <- function(x) pmin(0.98, pmax(0.02, x))
  cat_rows <- list(es = list(), eo = list())
  var_rows <- list(es = list(), eo = list())
  pillar <- list(es = list(), eo = list())
  for (iso in isos) for (i in seq_len(nrow(cells))) {
    k <- cells$key[i]
    u_es <- NULL
    for (pl in c("es", "eo")) {
      if (pl == "es" && k %in% COUNTRY_NO_ES_CELLS) next
      cats <- if (pl == "es") es_cats else eo_cats
      w <- c(0.4, 0.35, 0.25)
      base <- if (pl == "es") bias_es[[iso]] else bias_eo[[iso]]
      u <- stats::runif(3, -0.3, 0.3)
      if (pl == "es") u_es <- u else if (iso == "USA" && !is.null(u_es)) u <- u_es
      sc <- clip(0.5 + base + u)
      # the Trade category of ES carries a world-market variable (constant across economies)
      imp <- iso %in% c("NGA", "VNM") & c(FALSE, TRUE, FALSE)
      rule <- ifelse(imp, "global_average; zero", NA_character_)
      cat_rows[[pl]][[length(cat_rows[[pl]]) + 1]] <- data.frame(iso3 = iso, key = k, category = cats, category_score = sc, category_weight = w,
                                                                 imputed = imp, missing_rule_applied = rule, stringsAsFactors = FALSE)
      vr <- lapply(seq_along(cats), function(j) {
        d <- stats::runif(1, 0, 0.1)
        if (pl == "es" && cats[j] == "Trade") {
          hhi <- 0.2 + 0.01 * i
          v2 <- 2 * sc[j] - hhi
          return(data.frame(iso3 = iso, key = k, category = cats[j], variable = c("HHI", "market_share"), component_value = c(hhi, v2),
                            component_weight_within_overall = c(0.5, 0.5), imputed = FALSE, missing_rule_applied = NA_character_, stringsAsFactors = FALSE))
        }
        vn <- switch(cats[j], "Energy Imports" = c("Production surplus/deficit", "Import share"), "Reserves" = c("Oil Reserves", "Reserve life"),
                     "Investment" = c("Annual Investment Index", "Pipeline Capacity Index"), "Production" = c("size", "growth_abs"),
                     "Trade" = c("export_size", "rca"))
        data.frame(iso3 = iso, key = k, category = cats[j], variable = vn, component_value = c(sc[j] + d, sc[j] - d),
                   component_weight_within_overall = c(0.5, 0.5), imputed = imp[j], missing_rule_applied = rule[j], stringsAsFactors = FALSE)
      })
      var_rows[[pl]][[length(var_rows[[pl]]) + 1]] <- do.call(rbind, vr)
      pillar[[pl]][[length(pillar[[pl]]) + 1]] <- data.frame(iso3 = iso, key = k, value = sum(w * sc), stringsAsFactors = FALSE)
    }
  }
  bind <- function(x) do.call(rbind, x)
  mk_pillar <- function(pl) {
    d <- bind(pillar[[pl]])
    d$imputed_share <- ifelse(d$iso3 %in% c("NGA", "VNM"), 0.35, 0.05)
    d$rankable <- d$iso3 != "PRI"
    d$reason <- ifelse(d$iso3 == "PRI", "not a sovereign economy", NA_character_)
    d
  }
  es <- mk_pillar("es")
  eo <- mk_pillar("eo")
  cat_tbl <- list(es = bind(cat_rows$es), eo = bind(cat_rows$eo))
  var_tbl <- list(es = bind(var_rows$es), eo = bind(var_rows$eo))

  # ---- Strategic Index (level terms; NGA has no policy, PRI is not rankable)
  sc_w <- c(Upstream = 0.5, Midstream = 0.75, Downstream = 0.25)
  tech_w <- stats::setNames(round(seq(0.05, 0.99, length.out = 11), 4), unique(cells$tech))
  si_rows <- list()
  pol_index <- list()
  for (iso in isos) {
    e <- es[es$iso3 == iso, ]
    o <- eo[eo$iso3 == iso, ]
    pol <- round(stats::runif(nrow(cells), 0.05, 0.95), 4)
    pidx <- round(stats::runif(nrow(cells), 0.1, 0.9), 4)
    has_pol <- iso != "NGA"
    d <- data.frame(iso3 = iso, key = cells$key, tech = cells$tech, stage = cells$stage,
                    eo_level = o$value[match(cells$key, o$key)], need = 1 - e$value[match(cells$key, e$key)],
                    pol = if (has_pol) pol else NA_real_, sc_weight = unname(sc_w[cells$stage]), tech_weight = unname(tech_w[cells$tech]),
                    stringsAsFactors = FALSE)
    w <- c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tech = 0.1)
    d$si <- w[["eo"]] * d$eo_level + w[["need"]] * d$need + w[["pol"]] * d$pol + w[["sc"]] * d$sc_weight + w[["tech"]] * d$tech_weight
    ok <- is.finite(d$si) & iso != "PRI" & d$key != "geothermal-midstream"
    d$si_exclusion <- ifelse(ok, NA_character_, ifelse(!has_pol, "country not policy-eligible", ifelse(iso == "PRI", "not rankable", "missing energy security")))
    d$si[!ok] <- NA_real_
    d$resource_base <- ifelse(d$stage == "Upstream", d$tech %in% c("Oil", "Gas", "Coal", "Nuclear") & iso %in% c("USA", "BRA", "CHN"), NA)
    d$pick_exclusion <- ifelse(d$stage == "Upstream" & !(d$resource_base %in% TRUE), "no resource base", NA_character_)
    si_rows[[iso]] <- d
    if (has_pol) pol_index[[iso]] <- as.list(stats::setNames(pidx, cells$key))
  }
  si_w <- c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tech = 0.1)

  # ---- IEA PAMS (expanded rows: cross-cutting records repeat over technologies) and CAT
  pc <- list()
  pid <- 0L
  techs_all <- c(unique(cells$tech), "Critical Minerals")
  for (iso in c(setdiff(isos, c("NGA", "PRI")), "EUR")) {
    n_rec <- if (iso == "EUR") 6 else 12
    for (r in seq_len(n_rec)) {
      pid <- pid + 1L
      cross_t <- r %% 4 == 0
      cross_s <- r %% 3 == 0
      tt <- if (cross_t) techs_all else sample(techs_all, 1)
      ss <- if (cross_s) COUNTRY_STAGES else sample(COUNTRY_STAGES, 1)
      g <- expand.grid(tech = tt, supply_chain = ss, stringsAsFactors = FALSE)
      pc[[length(pc) + 1]] <- data.frame(policy_id = pid, iso3 = iso, tech = g$tech, supply_chain = g$supply_chain, tech_crosscut = cross_t,
                                         sc_crosscut = cross_s, status_norm = c("In force", "Ended", "Unknown")[r %% 3 + 1], year = 2010 + r,
                                         stringsAsFactors = FALSE)
    }
  }
  policy_clean <- bind(pc)
  cat_tbl_ratings <- data.frame(
    geo = c("USA", "EUU", "JPN", "KOR", "BRA", "CHN", "VNM"),
    overall = c("Insufficient", "Insufficient", "Highly insufficient", "Highly insufficient", "Insufficient", "Highly insufficient", "Critically insufficient"),
    policies_action = "Insufficient", domestic_target = "Insufficient", fair_share = "Insufficient", climate_finance = "Not assessed",
    ndc = "Poor", updated = c("Nov-24", "Jun-25", "Feb-25", "Mar-25", "Jul-23", "Dec-24", "Sep-24"), stringsAsFactors = FALSE)

  # ---- trade flows (baskets: Upstream and Midstream of the first eight technologies)
  basket_keys <- cells$key[cells$stage != "Downstream" & !cells$tech %in% c("Geothermal", "Green Hydrogen", "Electric Grid")]
  fl <- list()
  for (k in basket_keys) for (ex in isos) for (im in isos) {
    if (ex == im || stats::runif(1) < 0.45) next
    fl[[length(fl) + 1]] <- data.frame(key = k, exporter = ex, importer = im, value = round(stats::rlnorm(1, 18, 1.5)),
                                       status = if (stats::runif(1) < 0.15) "mirror" else "importer", stringsAsFactors = FALSE)
  }
  flows <- bind(fl)

  # ---- CM-FDI projects (four clean-tech manufacturing cells)
  fdi_keys <- c("batteries-midstream", "electric-vehicles-midstream", "solar-midstream", "wind-midstream")
  pr <- list()
  for (j in 1:40) {
    h <- sample(c("CHN", "KOR", "JPN", "DEU", "USA", "HKG"), 1)
    d <- sample(setdiff(isos, h), 1)
    pr[[j]] <- data.frame(pid = paste0("P", j), key = sample(fdi_keys, 1), home = h, dest = d, index_use = j %% 7 != 0, stringsAsFactors = FALSE)
  }
  fdi <- bind(pr)

  # ---- Partnership Strength from each reporter's viewpoint, with cell-level FS / PO for the best five
  dy <- list()
  cell_fs <- list()
  cell_po <- list()
  pk <- cells$key[!cells$key %in% COUNTRY_NO_ES_CELLS]
  for (rp in isos) {
    part <- setdiff(c(isos, "HKG"), rp)
    for (pt in part) {
      f <- round(stats::runif(length(pk), 0.2, 0.95), 6)
      p <- round(stats::runif(length(pk), 0.2, 0.95), 6)
      cell_fs[[length(cell_fs) + 1]] <- data.frame(reporter_iso = rp, partner_iso = pt, key = pk, value = f, stringsAsFactors = FALSE)
      cell_po[[length(cell_po) + 1]] <- data.frame(reporter_iso = rp, partner_iso = pt, key = pk, value = p, stringsAsFactors = FALSE)
    }
  }
  cell_fs <- bind(cell_fs)
  cell_po <- bind(cell_po)
  b5 <- rbind(country_best5(cell_fs, "fs", cells), country_best5(cell_po, "po", cells))
  income <- c(stats::setNames(ro$income_group, ro$iso3), HKG = "High income")
  for (rp in isos) {
    part <- setdiff(c(isos, "HKG"), rp)
    fsv <- b5$mean[b5$reporter_iso == rp & b5$side == "fs"][match(part, b5$partner_iso[b5$reporter_iso == rp & b5$side == "fs"])]
    pov <- b5$mean[b5$reporter_iso == rp & b5$side == "po"][match(part, b5$partner_iso[b5$reporter_iso == rp & b5$side == "po"])]
    dpv <- ifelse(income[part] == "High income", NA_real_, round(stats::runif(length(part), 0.3, 0.9), 6))
    psi <- ifelse(is.na(dpv), (0.4 * fsv + 0.4 * pov) / 0.8, 0.4 * fsv + 0.4 * pov + 0.2 * dpv)
    rk <- part != "PRI"
    rank <- rep(NA_real_, length(part))
    rank[rk] <- rank(-psi[rk], ties.method = "first")
    dy[[rp]] <- data.frame(reporter_iso = rp, partner_iso = part, friendshore = fsv, opportunity = pov, development = dpv, psi = psi,
                           rankable = rk, rank_exclusion = ifelse(rk, NA_character_, "not a sovereign economy"), rank = rank, stringsAsFactors = FALSE)
  }
  psi_dyad <- bind(dy)
  core <- ro$iso3[ro$tier == "core"]
  pp <- aggregate(psi ~ partner_iso, psi_dyad[psi_dyad$reporter_iso %in% core, ], mean)
  pp$rankable <- pp$partner_iso != "PRI"
  pp$rank <- NA_real_
  pp$rank[pp$rankable] <- rank(-pp$psi[pp$rankable], ties.method = "first")

  # ---- agreements, alignment
  agreements <- data.frame(
    iso3 = c("USA", "DEU", "FRA", "JPN", "EUU", "USA", "DEU", "FRA", "USA", "JPN", "KOR", "USA", "JPN", "KOR", "DEU", "FRA", "EUU", "KOR", "BRA"),
    agreement = c("G7", "G7", "G7", "G7", "G7", "NATO", "NATO", "NATO", "IPEF", "IPEF", "IPEF", "MSP", "MSP", "MSP", "MSP", "MSP", "MSP", "U.S. FTA", "MSP Forum"),
    membership_status = c(rep("member", 4), "participant", rep("member", 3), rep("partner", 9), "in_force", "forum_member"),
    stringsAsFactors = FALSE)
  bl <- stats::setNames(c(ro$bloc, "China-leaning"), c(ro$iso3, "HKG"))
  ga <- expand.grid(reporter_iso = names(bl), partner_iso = names(bl), stringsAsFactors = FALSE)
  ga <- ga[ga$reporter_iso != ga$partner_iso, ]
  b1 <- bl[ga$reporter_iso]
  b2 <- bl[ga$partner_iso]
  ga$geo_alignment <- ifelse(b1 == b2, 1, ifelse(b1 == "Nonaligned" | b2 == "Nonaligned", 0.5, 0))
  ga$bloc_partner <- unname(b2)
  ga$basis_partner <- ifelse(ga$partner_iso %in% c("PRI", "HKG"), "inherited", "own_votes")

  # ---- network design and allied picks
  ms <- data.frame(tech = c("Batteries", "Solar", "Oil", "Batteries", "Nuclear", "Solar", "Gas", "Oil"),
                   supply_chain = c("Midstream", "Midstream", "Upstream", "Midstream", "Upstream", "Midstream", "Upstream", "Upstream"),
                   iso3c = c("USA", "USA", "USA", "DEU", "FRA", "JPN", "BRA", "BRA"),
                   network_node = c("USA", "USA", "USA", "EUU", "EUU", "JPN", "BRA", "BRA"), selected = TRUE,
                   production_share_derived = c(0.3, 0.12, 0.25, 0.08, 0.05, 0.1, 0.07, 0.11),
                   share_is_derived = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), stringsAsFactors = FALSE)
  allied <- do.call(rbind, lapply(ro$iso3[ro$tier %in% c("core", "partner")], function(iso) {
    d <- si_rows[[iso]]
    el <- d[is.finite(d$si) & d$stage != "Downstream" & !d$tech %in% c("Geothermal", "Green Hydrogen") & is.na(d$pick_exclusion), ]
    el <- el[order(-el$si), ][1, ]
    data.frame(iso3 = iso, tech = el$tech, supply_chain = el$stage, strategic_index = el$si, stringsAsFactors = FALSE)
  }))

  # ---- NIPO measures (USA, DEU, JPN covered)
  # (the last USA row is announced, not yet in force: counted in n_not_in_force, never listed)
  nipo <- data.frame(
    iso3 = c("USA", "USA", "USA", "USA", "DEU", "DEU", "JPN", "USA", "USA"),
    title = c("Clean electricity production tax credit", "Advanced manufacturing production credit", "Battery materials grant programme",
              "Import duty on solar cells", "Hydrogen industrial strategy grant", "Battery cell investment aid", "Semiconductor and battery subsidy",
              "Expired wind tax credit", "Announced electric-vehicle charging grant"),
    impl_date = as.Date(c("2023-01-01", "2023-01-01", "2022-11-15", "2018-02-07", "2023-07-01", "2021-03-01", "2022-04-01", "2015-01-01", NA)),
    removal_date = as.Date(c(NA, NA, NA, "2030-01-01", NA, NA, NA, "2020-12-31", NA)),
    strength = c(2.4, 2.1, 1.7, 1.2, 1.5, 1.9, 1.6, 3, 2.8), strength_base = c(1.6, 1.4, 1.4, 1.2, 1.25, 1.6, 1.3, 2.5, 2),
    confidence = c(0.9, 0.95, 1, 1.1, 0.8, 0.9, 0.85, 1, 1.2),
    techs = c("Solar|Wind", "Batteries|Solar|Wind", "Batteries", "Solar", "Green Hydrogen", "Batteries", "Batteries", "Wind", "Electric Vehicles"),
    stages = c("Downstream", "Midstream", "Midstream|Upstream", "Midstream", "Midstream", "Midstream", "Midstream", "Downstream", "Downstream"),
    url = "https://www.globaltradealert.org/", source = "Synthetic record", stringsAsFactors = FALSE)
  # an EU-level measure recorded under both EU members (scope eu; eu_min_jurisdictions 2 in the synthetic meta)
  nipo <- rbind(nipo, data.frame(
    iso3 = c("DEU", "FRA"), title = "EU: Innovation Fund grants for net-zero manufacturing", impl_date = as.Date("2024-01-01"),
    removal_date = as.Date(NA), strength = 1.8, strength_base = 1.5, confidence = 1, techs = "Batteries|Solar", stages = "Midstream",
    url = "https://www.globaltradealert.org/", source = "Synthetic record", stringsAsFactors = FALSE))

  # ---- raw rows (the ES Energy Imports and Trade variables of fuels)
  rr <- list()
  for (iso in isos) for (t in c("Oil", "Gas", "Coal")) {
    rr[[length(rr) + 1]] <- data.frame(table = "import_dependence_tbl", iso3 = iso, tech = t, stage = "Upstream", category = "Energy Imports",
                                       variable = "Production surplus/deficit", value = round(stats::rnorm(1, 0, 3), 3), Year = 2025,
                                       source = "EI", explanation = "Production minus consumption", stringsAsFactors = FALSE)
    rr[[length(rr) + 1]] <- data.frame(table = "trade_concentration_tbl", iso3 = iso, tech = t, stage = "Upstream", category = "Trade",
                                       variable = "market_share", value = round(stats::runif(1, 0, 0.2), 4), Year = 2024,
                                       source = "Atlas", explanation = "Share of world exports [2023]", stringsAsFactors = FALSE)
  }
  raw_rows <- bind(rr)

  roster <- ro
  meta <- list(as_of = "2026-10-12", as_of_label = "12 October 2026", created = "2026-10-12T00:00:00Z", strategic_bundle_id = "synthetic",
               sector_bundle_id = "synthetic", trade_year = 2024L, un_window = "2019-2023", cmfdi_edition = "2026.10.05", rank_threshold = 0.5,
               pick_share_d = "37%", coalition_view = "coalition core (5 members)",
               nipo = list(vintage = "2026-06", vintage_label = "Global Trade Alert, New Industrial Policy Observatory, June 2026 extract (synthetic)",
                           jurisdictions = 4L, strength_words = cfg$nipo$strength_words, confidence_words = cfg$nipo$confidence_words,
                           eu_prefixes = cfg$nipo$eu_prefixes, eu_min_jurisdictions = 2L),
               label = country_fill(cfg$label, list(as_of_label = "12 October 2026")), validated = FALSE, synthetic = TRUE)
  party_fn <- country_party_fn(ro[, c("iso3", "name", "tier", "income_group")], c(HKG = "Hong Kong"))
  stats_c <- country_cell_stats(es, eo)
  list(
    cfg = cfg, dict = dict, meta = meta, roster = roster,
    es_by = split(es, es$iso3), eo_by = split(eo, eo$iso3), stats = stats_c,
    si_by = si_rows, si_w = si_w, pol_index_by = pol_index,
    policy_basis = as.list(stats::setNames(ifelse(isos %in% cat_tbl_ratings$geo | ro$eu_member, "IEA PAMS + CAT", "IEA PAMS only"), isos)),
    pams_counts = country_pams_counts_all(policy_clean), eu_records = length(unique(policy_clean$policy_id[policy_clean$iso3 == "EUR"])),
    cat_tbl = cat_tbl_ratings,
    dstats = list(es = country_driver_stats(cat_tbl$es, var_tbl$es, stats_c)$cat, eo = NULL),
    cat_by = list(es = split(cat_tbl$es, cat_tbl$es$iso3), eo = split(cat_tbl$eo, cat_tbl$eo$iso3)),
    var_by = list(es = split(var_tbl$es, var_tbl$es$iso3), eo = split(var_tbl$eo, var_tbl$eo$iso3)),
    .cat_tbl = cat_tbl, .var_tbl = var_tbl,
    raw_rows = raw_rows, raw_idx = country_raw_index(raw_rows),
    flows = flows, basket_keys = basket_keys, fdi = fdi,
    psi_by = split(psi_dyad, psi_dyad$reporter_iso), best5 = b5, psi_partner = pp,
    agreements = agreements, geo_alignment = ga, member_shares = ms, allied = allied, nipo = nipo,
    party_fn = party_fn,
    .cell_fs = cell_fs, .psi_dyad = psi_dyad
  ) |> country_ctx_finish()
}

# Driver statistics for both pillars (the synthetic builder and scripts/88 share this step); since
# 2026-10-14 also the EU members' shared values of the `eu_region` raw figures (country_eu_region_values())
# and where Global Trade Alert records each NIPO measure (country_nipo_scope(), over the whole run).
country_ctx_finish <- function(ctx) {
  ctx$dstats <- list(es = country_driver_stats(ctx$.cat_tbl$es, ctx$.var_tbl$es, ctx$stats),
                     eo = country_driver_stats(ctx$.cat_tbl$eo, ctx$.var_tbl$eo, ctx$stats))
  eu_iso <- ctx$roster$iso3[ctx$roster$eu_member %in% TRUE]
  if (!is.null(ctx$raw_rows)) ctx$eu_vals <- country_eu_region_values(ctx$raw_rows, ctx$cfg$raw_tables, eu_iso)
  if (!is.null(ctx$nipo) && is.null(ctx$nipo$scope)) {
    nm <- ctx$meta$nipo %||% list()
    ctx$nipo <- country_nipo_scope(ctx$nipo, unlist(nm$eu_prefixes %||% ctx$cfg$nipo$eu_prefixes),
                                   nm$eu_min_jurisdictions %||% ctx$cfg$nipo$eu_min_jurisdictions %||% 5L)
  }
  ctx
}
