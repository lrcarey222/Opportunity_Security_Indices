# Facts from data/processed (spec_sector.md 8.1, 8.9). Pure: every function takes a processed table
# and returns list(facts, ledger, excluded):
#   facts     var, item_key, geo, year, value, status, obs_year (true data years from the dictionary,
#             never the processed `Year`)
#   ledger    the parity ledger (not shipped): var, item_key, geo, year, prov, table, variable, key,
#             source_value, formula_value. prov P: the shipped value must be identical() to
#             source_value; D: identical() to formula_value = f(source values); X: within 1e-12 of
#             formula_value (a cross-check with processed).
#   excluded  (table, variable, reason, n): processed raw rows not shipped, by reason.
# Only data_type == "raw" rows are read; "Overall ..." variables never.

sector_raw_rows <- function(tbl) {
  tbl[tbl$data_type == "raw" & !grepl("^Overall", tbl$variable), , drop = FALSE]
}

sector_result <- function(facts = NULL, ledger = NULL, excluded = NULL) {
  list(facts = if (is.null(facts)) sector_facts_empty() else facts,
       ledger = if (is.null(ledger)) sector_ledger_empty() else ledger,
       excluded = if (is.null(excluded)) sector_excluded_empty() else excluded)
}
sector_facts_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), geo = character(), year = integer(), value = double(),
                 status = character(), obs_year = integer())
}
sector_ledger_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), geo = character(), year = integer(), prov = character(),
                 table = character(), variable = character(), key = character(), source_value = double(), formula_value = double())
}
sector_excluded_empty <- function() tibble::tibble(table = character(), variable = character(), reason = character(), n = integer())

# Bind a list of sector_result()s.
sector_bind_results <- function(rs) {
  list(facts = dplyr::bind_rows(lapply(rs, `[[`, "facts")),
       ledger = dplyr::bind_rows(lapply(rs, `[[`, "ledger")),
       excluded = dplyr::bind_rows(lapply(rs, `[[`, "excluded")))
}

# Facts and their ledger rows at once. src: the processed value (P) shipped verbatim.
sector_fx <- function(var, item_key, geo, year, value, status, table, variable, prov = "P", obs_year = NA_integer_,
                      formula_value = NA_real_, key = NA_character_) {
  n <- length(value)
  # as.character(): an empty input (ifelse() on zero rows gives logical(0)) keeps the column types.
  f <- tibble::tibble(var = as.character(rep_len(var, n)), item_key = as.character(rep_len(item_key, n)), geo = as.character(rep_len(geo, n)),
                      year = as.integer(rep_len(year, n)), value = as.double(value), status = as.character(rep_len(status, n)),
                      obs_year = as.integer(rep_len(obs_year, n)))
  shipped <- !is.na(f$value)
  l <- tibble::tibble(var = f$var, item_key = f$item_key, geo = f$geo, year = f$year, prov = as.character(rep_len(prov, n)),
                      table = as.character(rep_len(table, n)), variable = as.character(rep_len(variable, n)), key = as.character(rep_len(key, n)), source_value = f$value,
                      formula_value = as.double(rep_len(formula_value, n)))
  list(facts = f, ledger = l[shipped, , drop = FALSE])
}

sector_excl <- function(table, variable, reason, n) {
  if (!n) return(NULL)
  tibble::tibble(table = table, variable = variable, reason = reason, n = as.integer(n))
}

# Collapse copies: one row per `keys`; stops when a copy group holds two values (or two years).
sector_debroadcast <- function(rows, keys, label, value_cols = "value") {
  key <- do.call(paste, c(unname(as.list(rows[keys])), sep = "\r"))
  for (vc in value_cols) {
    v <- rows[[vc]]
    first <- !duplicated(key)
    ref <- v[first][match(key, key[first])]
    same <- (is.na(v) & is.na(ref)) | (!is.na(v) & !is.na(ref) & v == ref)
    if (!all(same)) {
      bad <- unique(key[!same])
      stop("sector_debroadcast(", label, "): copies differ in ", vc, " for ", length(bad), " group(s), e.g. ",
           gsub("\r", " / ", paste(utils::head(bad, 3), collapse = "; ")))
    }
  }
  rows[!duplicated(key), , drop = FALSE]
}

# major_supplier_shares_tbl repeats each mineral measure once per technology that uses the mineral:
# collapse the copies on (measure, mineral, iso3), asserting identical value and world_share.
sector_supplier_dedupe <- function(mss, measures = c("iea_mineral_mining", "iea_mineral_refining", "ei_mineral_mining")) {
  s <- mss[mss$measure %in% measures, , drop = FALSE]
  sector_debroadcast(s, c("measure", "mineral", "iso3"), "major_supplier_shares_tbl (technology copies)", c("value", "world_share"))
}

sector_check_iso3 <- function(x, label) {
  bad <- unique(x[!grepl("^[A-Z]{3}$", x)])
  if (length(bad)) stop(label, ": codes that are not ISO3: ", paste(utils::head(bad, 8), collapse = ", "))
  invisible(TRUE)
}

# ---- Trade (processed): Atlas shares, RCA, exporter-reported totals -----------------------------------

sector_facts_atlas <- function(tc) {
  r <- sector_raw_rows(tc)
  r <- r[r$variable == "market_share" & !is.na(r$value), , drop = FALSE]
  sector_check_iso3(r$iso3c, "trade_concentration_tbl market_share")
  x <- sector_fx("trade.atlas_share", paste0("cell:", sector_cell_slug(r$tech, r$supply_chain)), r$iso3c, 2023L,
                 r$value, ifelse(r$value == 0, "zero", "obs"), "trade_concentration_tbl", "market_share",
                 key = paste(r$iso3c, r$tech, r$supply_chain))
  w <- unique(x$facts[c("var", "item_key", "year")])
  w$value <- 1
  w$basis <- "one"
  c(sector_result(x$facts, x$ledger), list(world = w))
}

# RCA (exporter-reported): rows whose exporter-reported basket exports are below min_exports read
# "too small to compare" (status nq, value NaN; not in the ledger).
sector_facts_rca <- function(ef, min_exports = 5e7) {
  r <- sector_raw_rows(ef)
  rca <- r[r$variable == "export_rca" & !is.na(r$value), , drop = FALSE]
  ex <- r[r$variable == "exports", , drop = FALSE]
  base <- ex$value[match(paste(rca$iso3c, rca$tech, rca$supply_chain), paste(ex$iso3c, ex$tech, ex$supply_chain))]
  small <- is.na(base) | base < min_exports
  sector_check_iso3(rca$iso3c, "export_feasibility_tbl export_rca")
  x <- sector_fx("trade.rca", paste0("cell:", sector_cell_slug(rca$tech, rca$supply_chain)), rca$iso3c, 2024L,
                 ifelse(small, NaN, rca$value), ifelse(small, "nq", "obs"), "export_feasibility_tbl", "export_rca",
                 key = paste(rca$iso3c, rca$tech, rca$supply_chain))
  sector_result(x$facts, x$ledger, sector_excl("export_feasibility_tbl", "export_rca", "rca_below_min_exports (shipped as nq)", sum(small)))
}

# Exporter-reported totals per cell for the reconciliation table: total, China's share, the
# processed HHI (de-broadcast; one value per cell) and the number of reporters.
sector_reported_trade <- function(tc, china = "CHN") {
  r <- sector_raw_rows(tc)
  ex <- r[r$variable == "exports" & !is.na(r$value), , drop = FALSE]
  hhi <- sector_debroadcast(r[r$variable == "HHI", c("tech", "supply_chain", "value")], c("tech", "supply_chain"), "trade HHI")
  cells <- unique(ex[c("tech", "supply_chain")])
  out <- lapply(seq_len(nrow(cells)), function(i) {
    e <- ex[ex$tech == cells$tech[i] & ex$supply_chain == cells$supply_chain[i], , drop = FALSE]
    tot <- sum(e$value)
    h <- hhi$value[hhi$tech == cells$tech[i] & hhi$supply_chain == cells$supply_chain[i]]
    tibble::tibble(item_key = paste0("cell:", sector_cell_slug(cells$tech[i], cells$supply_chain[i])), year = 2024L,
                   total = tot, china_reported = sum(e$value[e$iso3c == china]) / tot,
                   hhi_reported = if (length(h)) h[1] else NA_real_, n_reporters = sum(e$value > 0))
  })
  dplyr::bind_rows(out)
}

# Mineral exports and export dependence (critical_minerals_trade_flows_tbl, one row per exporter x
# mineral). Status: mirror (partner-reported), zero, obs. World = the sum over every exporter,
# cross-checked against the processed market shares.
sector_facts_critmin <- function(fl) {
  sector_check_iso3(fl$iso3c, "critical_minerals_trade_flows_tbl")
  slug <- sector_mineral_slug(fl$mineral)
  if (anyNA(slug)) stop("critical minerals flows: unknown minerals ", paste(unique(fl$mineral[is.na(slug)]), collapse = ", "))
  year <- as.integer(fl$Year)
  item <- paste0("mineral:", slug)
  # A mirror row is from partners' declarations even when they declare nothing (a 0 there is no
  # report of zero by the economy itself): mirror, never zero.
  st <- ifelse(fl$export_source == "mirror", "mirror", ifelse(fl$exports == 0, "zero", "obs"))
  a <- sector_fx("trade.critmin_exports", item, fl$iso3c, year, fl$exports, st, "critical_minerals_trade_flows_tbl", "exports",
                 key = paste(fl$iso3c, fl$mineral))
  st2 <- ifelse(fl$export_source == "mirror", "mirror", ifelse(fl$exp_share == 0, "zero", "obs"))
  ok2 <- !is.na(fl$exp_share)
  b <- sector_fx("trade.critmin_dependence", item[ok2], fl$iso3c[ok2], year[ok2], fl$exp_share[ok2], st2[ok2],
                 "critical_minerals_trade_flows_tbl", "exp_share", key = paste(fl$iso3c, fl$mineral)[ok2])
  w <- stats::aggregate(list(value = fl$exports), list(item_key = item, year = year), sum)
  world <- tibble::tibble(var = "trade.critmin_exports", item_key = w$item_key, year = as.integer(w$year), value = w$value, basis = "sum_all")
  # Cross-check: exports / W = the processed market share.
  W <- w$value[match(paste(item, year), paste(w$item_key, w$year))]
  rel <- abs(fl$exports / W - fl$market_share) / pmax(abs(fl$market_share), 1e-300)
  rel[fl$exports == 0 & fl$market_share == 0] <- 0
  out <- sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                       sector_excl("critical_minerals_trade_flows_tbl", "exp_share", "NA", sum(!ok2)))
  c(out, list(world = world, check = list(max_rel = max(rel, na.rm = TRUE))))
}

# ---- Reserves and potentials ---------------------------------------------------------------------------

# reserves_tbl: fuels (end-2020), minerals (USGS end-2025) and uranium resources (1 Jan 2025).
# NA rows: listed-without-a-figure holders (unquantified: tibble(tech, ISO3166_alpha3)) ship as nq
# with NaN; the rest are the roster completion and are not shipped.
sector_facts_reserves <- function(res, unquantified) {
  r <- sector_raw_rows(res)
  r$tech_name <- sub(" Reserves$", "", r$variable)
  fuel <- r$tech_name %in% c("Oil", "Gas", "Coal")
  uran <- r$tech_name == "Nuclear"
  mineral <- !fuel & !uran
  out <- list()
  na <- is.na(r$value)
  nq <- mineral & na & paste(r$tech_name, r$iso3c) %in% paste(unquantified$tech, unquantified$ISO3166_alpha3)
  keep <- !na | nq
  st <- ifelse(nq, "nq", ifelse(r$value == 0, "zero", "obs"))
  sector_check_iso3(r$iso3c[keep], "reserves_tbl")
  sel <- keep & fuel
  out[[1]] <- sector_fx("resources.fuel_reserves", paste0("fuel:", tolower(r$tech_name[sel])), r$iso3c[sel], 2020L,
                        r$value[sel], st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  sel <- keep & mineral
  out[[2]] <- sector_fx("resources.mineral_reserves", paste0("mineral:", sector_mineral_slug(r$tech_name[sel])), r$iso3c[sel], 2025L,
                        ifelse(nq[sel], NaN, r$value[sel]), st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  sel <- keep & uran
  out[[3]] <- sector_fx("resources.uranium_resources", "cell:nuclear-upstream", r$iso3c[sel], 2025L,
                        r$value[sel], st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  ex <- dplyr::bind_rows(
    sector_excl("reserves_tbl", "*", "NA (roster completion: economy not listed by the source)", sum(na & !nq)),
    sector_excl("reserves_tbl", "<Mineral> Reserves", "NA listed without a figure (shipped as nq)", sum(nq))
  )
  sector_result(dplyr::bind_rows(lapply(out, `[[`, "facts")), dplyr::bind_rows(lapply(out, `[[`, "ledger")), ex)
}

# Solar, wind and geothermal potentials (static GIS layers; true year: the dataset label).
sector_facts_potential <- function(solar, wind, geo) {
  spec <- tibble::tribble(
    ~tbl, ~variable, ~var, ~cell,
    "solar_pv_potential_tbl", "Solar PV power potential per sq km", "resources.pv_quality", "solar-downstream",
    "solar_pv_potential_tbl", "Solar PV power potential", "resources.pv_area", "solar-downstream",
    "wind_potential_tbl", "Wind power potential per sq km", "resources.wind_density", "wind-downstream",
    "wind_potential_tbl", "Wind power potential", "resources.wind_area", "wind-downstream",
    "wind_potential_tbl", "Wind power potential share over threshold", "resources.wind_share", "wind-downstream",
    "geothermal_potential_tbl", "Geothermal total potential (MW)", "resources.geo_potential", "geothermal-downstream",
    "geothermal_potential_tbl", "Geothermal LCOE (500 MW)", "resources.geo_lcoe_500", "geothermal-downstream",
    "geothermal_potential_tbl", "Geothermal LCOE (5 GW)", "resources.geo_lcoe_5000", "geothermal-downstream"
  )
  src <- list(solar_pv_potential_tbl = sector_raw_rows(solar), wind_potential_tbl = sector_raw_rows(wind),
              geothermal_potential_tbl = sector_raw_rows(geo))
  rs <- lapply(seq_len(nrow(spec)), function(i) {
    r <- src[[spec$tbl[i]]]
    r <- r[r$variable == spec$variable[i], , drop = FALSE]
    na <- is.na(r$value)
    r <- r[!na, , drop = FALSE]
    sector_check_iso3(r$iso3c, spec$tbl[i])
    x <- sector_fx(spec$var[i], paste0("cell:", spec$cell[i]), r$iso3c, 2023L, r$value, ifelse(r$value == 0, "zero", "obs"),
                   spec$tbl[i], spec$variable[i], key = r$iso3c)
    sector_result(x$facts, x$ledger, sector_excl(spec$tbl[i], spec$variable[i], "NA (no tranche or not covered)", sum(na)))
  })
  sector_bind_results(rs)
}

# ---- Production -------------------------------------------------------------------------------------

# production_depth_momentum_tbl production_latest: fuels (Upstream), generation (Downstream) and
# uranium (Nuclear Upstream, 2024). ei_present: tibble(ei_var, iso3) of EI rows present in the raw
# CSV for the latest year; a processed 0 without one is a zero-completion (nsr). Mineral Upstream
# cells are a cross-mineral roll-up and are not shipped.
sector_facts_production <- function(pdm, ei_present, latest_year = 2025L, uranium_year = 2024L) {
  r <- sector_raw_rows(pdm)
  r <- r[r$variable == "production_latest", , drop = FALSE]
  cell <- paste(r$tech, r$supply_chain)
  fuel_map <- c("Oil Upstream" = "oilprod_kbd", "Gas Upstream" = "gasprod_ej", "Coal Upstream" = "coalprod_ej")
  gen_map <- c("Solar Downstream" = "solar_twh", "Wind Downstream" = "wind_twh", "Nuclear Downstream" = "nuclear_twh",
               "Coal Downstream" = "electbyfuel_coal", "Gas Downstream" = "electbyfuel_gas", "Oil Downstream" = "electbyfuel_oil")
  pres <- paste(ei_present$ei_var, ei_present$iso3)
  status_ei <- function(rr, ei_var) {
    present <- paste(ei_var, rr$iso3c) %in% pres
    ifelse(rr$value > 0, "obs", ifelse(present, "zero", "nsr"))
  }
  out <- list()
  f <- r[cell %in% names(fuel_map), , drop = FALSE]
  sector_check_iso3(f$iso3c, "production_depth_momentum_tbl")
  ev <- fuel_map[paste(f$tech, f$supply_chain)]
  out[[1]] <- sector_fx("production.fuels", paste0("fuel:", tolower(f$tech)), f$iso3c, latest_year, f$value, status_ei(f, ev),
                        "production_depth_momentum_tbl", "production_latest", key = paste(f$iso3c, f$tech, f$supply_chain))
  g <- r[cell %in% names(gen_map), , drop = FALSE]
  ev <- gen_map[paste(g$tech, g$supply_chain)]
  out[[2]] <- sector_fx("production.generation", paste0("fuel:", tolower(g$tech)), g$iso3c, latest_year, g$value, status_ei(g, ev),
                        "production_depth_momentum_tbl", "production_latest", key = paste(g$iso3c, g$tech, g$supply_chain))
  u <- r[cell == "Nuclear Upstream", , drop = FALSE]
  sector_check_iso3(u$iso3c, "production_depth_momentum_tbl uranium")
  out[[3]] <- sector_fx("production.uranium", "cell:nuclear-upstream", u$iso3c, uranium_year, u$value,
                        ifelse(u$value > 0, "obs", "zero"), "production_depth_momentum_tbl", "production_latest", key = u$iso3c)
  roll <- !cell %in% c(names(fuel_map), names(gen_map), "Nuclear Upstream")
  sector_result(dplyr::bind_rows(lapply(out, `[[`, "facts")), dplyr::bind_rows(lapply(out, `[[`, "ledger")),
                sector_excl("production_depth_momentum_tbl", "production_latest", "mineral Upstream roll-up (not a quantity of one mineral)", sum(roll)))
}

# critical_minerals_production_tbl: EI mine production of manganese, zinc and PGMs (2025). Zeros
# are the roster completion (nsr). World from major_supplier_shares_tbl (value / world share of the
# largest producer). The other EI minerals are covered by IEA and USGS and are not shipped.
sector_facts_mining_ei <- function(cmp, mss, minerals = c("Manganese", "Zinc", "PGMs"), year = 2025L) {
  r <- sector_raw_rows(cmp)
  r$tech_name <- sub(" Production$", "", r$variable)
  sel <- r$tech_name %in% minerals
  m <- r[sel, , drop = FALSE]
  sector_check_iso3(m$iso3c, "critical_minerals_production_tbl")
  x <- sector_fx("production.mining_ei", paste0("mineral:", sector_mineral_slug(m$tech_name)), m$iso3c, year, m$value,
                 ifelse(m$value > 0, "obs", "nsr"), "critical_minerals_production_tbl", m$variable, key = m$iso3c)
  s <- sector_supplier_dedupe(mss, "ei_mineral_mining")
  s <- s[s$mineral %in% minerals & s$world_share > 0, , drop = FALSE]
  s <- s[order(s$mineral, -s$value), , drop = FALSE]
  s <- s[!duplicated(s$mineral), , drop = FALSE]
  world <- tibble::tibble(var = "production.mining_ei", item_key = paste0("mineral:", sector_mineral_slug(s$mineral)), year = year,
                          value = s$value / s$world_share, basis = "source_world")
  other <- !sel & r$tech_name != "Nuclear"
  ex <- dplyr::bind_rows(
    sector_excl("critical_minerals_production_tbl", "<Mineral> Production", "minerals covered by IEA and USGS", sum(other)),
    sector_excl("critical_minerals_production_tbl", "Nuclear Production", "duplicate of production_depth_momentum_tbl (uranium)", sum(r$tech_name == "Nuclear"))
  )
  c(sector_result(x$facts, x$ledger, ex), list(world = world))
}

# ---- Consumption -------------------------------------------------------------------------------------

# energy_access_tbl: consumption per person and its 2020-2025 change (ship once; the EI rows of
# energy_consumption_tbl must be identical). ei_present as for production (<fuel>_tes_ej).
sector_facts_consumption <- function(ea, ec, ei_present, year = 2025L) {
  r <- sector_raw_rows(ea)
  k <- paste(r$iso3c, r$tech, r$variable)
  e <- sector_raw_rows(ec)
  e <- e[e$variable %in% unique(r$variable), , drop = FALSE]
  ke <- paste(e$iso3c, e$tech, e$variable)
  if (!setequal(k, ke) || !identical(e$value[match(k, ke)], r$value)) {
    stop("energy_consumption_tbl EI rows differ from energy_access_tbl: ship-once rule broken")
  }
  sector_check_iso3(r$iso3c, "energy_access_tbl")
  pres <- paste(ei_present$ei_var, ei_present$iso3)
  pc <- r[r$variable == "Energy consumption per capita", , drop = FALSE]
  present <- paste(paste0(tolower(pc$tech), "_tes_ej"), pc$iso3c) %in% pres
  st <- ifelse(pc$value > 0, "obs", ifelse(present, "zero", "nsr"))
  a <- sector_fx("consumption.per_capita", paste0("fuel:", tolower(pc$tech)), pc$iso3c, year, pc$value, st,
                 "energy_access_tbl", "Energy consumption per capita", key = paste(pc$iso3c, pc$tech))
  g <- r[r$variable == "Energy consumption per capita growth", , drop = FALSE]
  bad <- !is.finite(g$value)
  b <- sector_fx("consumption.pc_change", paste0("fuel:", tolower(g$tech)), g$iso3c, year, ifelse(bad, NaN, g$value),
                 ifelse(bad, "na_growth", "obs"), "energy_access_tbl", "Energy consumption per capita growth",
                 key = paste(g$iso3c, g$tech))
  sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                dplyr::bind_rows(sector_excl("energy_access_tbl", "Energy consumption per capita growth", "Inf or NaN (shipped as na_growth)", sum(bad)),
                                 sector_excl("energy_consumption_tbl", "Energy consumption per capita (+ growth)", "duplicate of energy_access_tbl", nrow(e))))
}

# import_dependence_tbl: net balance (EJ) and self-sufficiency (percent of consumption; NA dropped).
# ei_present as for production: where EI has no production row of the fuel for the economy, the
# pipeline's production is a zero-fill (production.fuels ships nsr), so the balance (-consumption)
# and the self-sufficiency (0%) are not observations: both ship as nsr with NaN ("nsr
# reclassification" in the validation report). Fossil is nsr when none of oil, gas and coal has an
# EI production row.
SECTOR_EI_PRODUCTION_VARS <- c(Oil = "oilprod_kbd", Gas = "gasprod_ej", Coal = "coalprod_ej")
sector_facts_balance <- function(idp, ei_present = NULL, year = 2025L) {
  r <- sector_raw_rows(idp)
  sector_check_iso3(r$iso3c, "import_dependence_tbl")
  no_prod <- function(rr) {
    if (is.null(ei_present)) return(rep(FALSE, nrow(rr)))
    pres <- paste(ei_present$ei_var, ei_present$iso3)
    has <- function(tech, iso3) paste(SECTOR_EI_PRODUCTION_VARS[tech], iso3) %in% pres
    fuel <- rr$tech %in% names(SECTOR_EI_PRODUCTION_VARS)
    fossil <- rr$tech == "Fossil"
    out <- fuel & !has(rr$tech, rr$iso3c)
    out[fossil] <- !(has("Oil", rr$iso3c[fossil]) | has("Gas", rr$iso3c[fossil]) | has("Coal", rr$iso3c[fossil]))
    out
  }
  b <- r[r$variable == "Production surplus/deficit" & !is.na(r$value), , drop = FALSE]
  nb <- no_prod(b)
  a <- sector_fx("consumption.balance", paste0("fuel:", tolower(b$tech)), b$iso3c, year, ifelse(nb, NaN, b$value),
                 ifelse(nb, "nsr", ifelse(b$value == 0, "zero", "obs")),
                 "import_dependence_tbl", "Production surplus/deficit", key = paste(b$iso3c, b$tech))
  s <- r[r$variable == "Self-sufficiency", , drop = FALSE]
  na <- is.na(s$value)
  s <- s[!na, , drop = FALSE]
  ns <- no_prod(s)
  c2 <- sector_fx("consumption.self_sufficiency", paste0("fuel:", tolower(s$tech)), s$iso3c, year, ifelse(ns, NaN, s$value),
                  ifelse(ns, "nsr", ifelse(s$value == 0, "zero", "obs")), "import_dependence_tbl", "Self-sufficiency",
                  key = paste(s$iso3c, s$tech))
  sector_result(dplyr::bind_rows(a$facts, c2$facts), dplyr::bind_rows(a$ledger, c2$ledger),
                dplyr::bind_rows(
                  sector_excl("import_dependence_tbl", "Self-sufficiency", "NA (no consumption)", sum(na)),
                  sector_excl("import_dependence_tbl", "Production surplus/deficit",
                              "nsr reclassification: no EI production row (shipped as nsr, NaN)", sum(nb)),
                  sector_excl("import_dependence_tbl", "Self-sufficiency",
                              "nsr reclassification: no EI production row (shipped as nsr, NaN)", sum(ns))))
}

# ---- Manufacturing and EV regions --------------------------------------------------------------------

SECTOR_ETP_TECH <- c(Solar = "solar-midstream", Wind = "wind-midstream", Batteries = "batteries-midstream",
                     Electrolyzers = "green-hydrogen-midstream", `Green Hydrogen` = "green-hydrogen-midstream")

sector_etp_entity <- function(iso3, eu_members) {
  ifelse(iso3 %in% c(eu_members, "EUU"), "E:iea_etp:eu",
         ifelse(iso3 == "CHN", "E:iea_etp:china", ifelse(iso3 == "USA", "E:iea_etp:us",
         ifelse(iso3 == "IND", "E:iea_etp:india", ifelse(iso3 == "VNM", "E:iea_etp:viet-nam", NA_character_)))))
}

# IEA ETP shares: current (major_supplier_shares_tbl iea_manufacturing_capacity world_share) and
# announced 2035 (foreign_dependency_tbl Market Share, EU members carry the EU value). A 0 means
# "under 1% or not reported" (status nq, value kept).
sector_facts_mfg <- function(fd, mss, eu_members) {
  s <- mss[mss$measure == "iea_manufacturing_capacity", , drop = FALSE]
  ent <- sector_etp_entity(s$iso3, eu_members)
  if (anyNA(ent)) stop("IEA manufacturing capacity: unknown entities ", paste(unique(s$iso3[is.na(ent)]), collapse = ", "))
  cell <- SECTOR_ETP_TECH[s$tech]
  if (anyNA(cell)) stop("IEA manufacturing capacity: unknown techs ", paste(unique(s$tech[is.na(cell)]), collapse = ", "))
  a <- sector_fx("manufacturing.share_now", paste0("cell:", cell), ent, as.integer(s$year), s$world_share,
                 ifelse(s$world_share == 0, "nq", "obs"), "major_supplier_shares_tbl", "iea_manufacturing_capacity",
                 key = paste(s$iso3, s$tech))
  r <- sector_raw_rows(fd)
  r <- r[r$variable == "Market Share", , drop = FALSE]
  heat <- !r$tech %in% names(SECTOR_ETP_TECH)
  r <- r[!heat, , drop = FALSE]
  r$entity <- sector_etp_entity(r$iso3c, eu_members)
  if (anyNA(r$entity)) stop("IEA 2035 shares: unknown economies ", paste(unique(r$iso3c[is.na(r$entity)]), collapse = ", "))
  d <- sector_debroadcast(r[c("entity", "tech", "value", "iso3c")], c("entity", "tech"), "IEA 2035 shares (EU members)")
  b <- sector_fx("manufacturing.share_2035", paste0("cell:", SECTOR_ETP_TECH[d$tech]), d$entity, 2035L, d$value,
                 ifelse(d$value == 0, "nq", "obs"), "foreign_dependency_tbl", "Market Share", key = paste(d$entity, d$tech))
  world <- unique(dplyr::bind_rows(a$facts, b$facts)[c("var", "item_key", "year")])
  world$value <- 1
  world$basis <- "one"
  c(sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                  dplyr::bind_rows(sector_excl("foreign_dependency_tbl", "Market Share", "Heat Pumps (no cell)", sum(heat)),
                                   sector_excl("foreign_dependency_tbl", "Market Share", "EU member copies of the EU value", nrow(r) - nrow(d)))),
    list(world = world))
}

# IEA EV regions (market_share_manufacturing_tbl): production, sales and the net-export ratio, one
# value per region (the region from the row's explanation; copies asserted identical).
sector_facts_ev_regions <- function(msm, year = 2024L) {
  r <- sector_raw_rows(msm)
  r <- r[r$tech == "Electric Vehicles" & r$variable %in% c("Domestic.Production", "Domestic.sales", "net_export_ratio"), , drop = FALSE]
  r$region <- sub("^.*IEA region: ([^(]+?) \\(.*$", "\\1", r$explanation)
  bad <- !r$region %in% names(SECTOR_EV_REGIONS)
  if (any(bad)) stop("EV regions: unknown region text ", paste(utils::head(unique(r$region[bad]), 3), collapse = "; "))
  d <- sector_debroadcast(r[c("region", "variable", "value")], c("region", "variable"), "IEA EV regions")
  vmap <- c(Domestic.Production = "production.ev_region", Domestic.sales = "production.ev_region_sales",
            net_export_ratio = "manufacturing.ev_net_export_ratio")
  x <- sector_fx(unname(vmap[d$variable]), "cell:electric-vehicles-midstream", paste0("R:iea_ev:", SECTOR_EV_REGIONS[d$region]),
                 year, d$value, ifelse(d$value == 0, "zero", "obs"), "market_share_manufacturing_tbl", d$variable, key = d$region)
  region_of <- stats::setNames(r$region, r$iso3c)
  region_of <- region_of[!duplicated(names(region_of))]
  c(sector_result(x$facts, x$ledger, sector_excl("market_share_manufacturing_tbl", "EV regional values", "copies of the region's value", nrow(r) - nrow(d))),
    list(region_of = region_of))
}

# ---- Investment -------------------------------------------------------------------------------------

# investment_momentum_tbl annual investment, 2018-2025. 2025 includes estimated Q4 construction
# starts for wind, solar and critical minerals (CIM README): those rows are `est`.
sector_facts_investment <- function(im, est_year = 2025L) {
  r <- sector_raw_rows(im)
  a <- r[r$variable == "Annual Investment (USD bn, 2024$)", , drop = FALSE]
  sector_check_iso3(a$iso3c, "investment_momentum_tbl")
  est <- a$Year == est_year & (a$tech %in% c("Solar", "Wind") | a$supply_chain == "Upstream")
  x <- sector_fx("investment.annual", paste0("cell:", sector_cell_slug(a$tech, a$supply_chain)), a$iso3c, as.integer(a$Year), a$value,
                 ifelse(est, "est", ifelse(a$value == 0, "zero", "obs")), "investment_momentum_tbl", "Annual Investment (USD bn, 2024$)",
                 key = paste(a$iso3c, a$tech, a$supply_chain, a$Year))
  other <- r$variable != "Annual Investment (USD bn, 2024$)"
  sector_result(x$facts, x$ledger, sector_excl("investment_momentum_tbl", "momentum and raw stage sums", "a transform / mixed units", sum(other)))
}

# ---- Future demand: WEO world supply, IEA GEVO EV cars ---------------------------------------------

SECTOR_WEO_FUEL <- c(Coal = "coal", Gas = "gas", `Green Hydrogen` = "hydrogen", Nuclear = "nuclear", Oil = "oil", Solar = "solar", Wind = "wind")

sector_facts_future <- function(fdm, ev_min_base = 1000) {
  r <- sector_raw_rows(fdm)
  weo <- r[r$source == "IEA World Energy Outlook" & r$variable %in% c("2035", "global_growth"), , drop = FALSE]
  d <- sector_debroadcast(weo[c("tech", "variable", "value")], c("tech", "variable"), "WEO world values")
  v35 <- d[d$variable == "2035", , drop = FALSE]
  gr <- d[d$variable == "global_growth", , drop = FALSE]
  g <- gr$value[match(v35$tech, gr$tech)]
  fuel <- paste0("fuel:", SECTOR_WEO_FUEL[v35$tech])
  a <- sector_fx("consumption.weo", fuel, "WLD", 2035L, v35$value, "obs", "future_demand_tbl", "2035", key = v35$tech)
  v24 <- v35$value - g
  b <- sector_fx("consumption.weo", fuel, "WLD", 2024L, v24, "obs", "future_demand_tbl", "2035 - global_growth", prov = "D",
                 formula_value = v24, key = v35$tech)
  ev <- r[r$source == "IEA Global EV Outlook 2026" & r$tech == "Electric Vehicles", , drop = FALSE]
  sector_check_iso3(ev$iso3c, "future_demand_tbl GEVO")
  cell <- "cell:electric-vehicles-downstream"
  pick <- function(vn) ev[ev$variable == vn & !is.na(ev$value), , drop = FALSE]
  lvl <- list()
  for (spec in list(c("sales_2025", "consumption.ev_sales", 2025), c("sales_2035", "consumption.ev_sales", 2035),
                    c("stock_2025", "consumption.ev_stock", 2025), c("stock_2035", "consumption.ev_stock", 2035),
                    c("share_2025", "consumption.ev_sales_share", 2025), c("share_2035", "consumption.ev_sales_share", 2035))) {
    p <- pick(spec[1])
    lvl[[length(lvl) + 1]] <- sector_fx(spec[2], cell, p$iso3c, as.integer(spec[3]), p$value, ifelse(p$value == 0, "zero", "obs"),
                                        "future_demand_tbl", spec[1], key = p$iso3c)
  }
  # Growth 2022-2025 only where the 2022 level (level_2025 / (1 + growth)) is at least ev_min_base.
  grow <- list()
  n_small <- 0L
  for (spec in list(c("sales_growth_2225", "sales_2025", "consumption.ev_sales_growth"),
                    c("stock_growth_2225", "stock_2025", "consumption.ev_stock_growth"))) {
    p <- pick(spec[1])
    lv <- ev$value[ev$variable == spec[2]][match(p$iso3c, ev$iso3c[ev$variable == spec[2]])]
    base <- lv / (1 + p$value)
    small <- is.na(base) | !is.finite(p$value) | base < ev_min_base
    n_small <- n_small + sum(small)
    grow[[length(grow) + 1]] <- sector_fx(spec[3], cell, p$iso3c, 2025L, ifelse(small, NaN, p$value), ifelse(small, "na_growth", "obs"),
                                          "future_demand_tbl", spec[1], key = p$iso3c)
  }
  all <- c(list(a, b), lvl, grow)
  other <- r$source %in% c("BNEF New Energy Outlook", "BCG") | (r$source == "IEA Global EV Outlook 2026" & grepl("forecast_growth|share_growth", r$variable))
  sector_result(dplyr::bind_rows(lapply(all, `[[`, "facts")), dplyr::bind_rows(lapply(all, `[[`, "ledger")),
                dplyr::bind_rows(sector_excl("future_demand_tbl", "BNEF / BCG", "licensed (never shipped)", sum(r$source %in% c("BNEF New Energy Outlook", "BCG"))),
                                 sector_excl("future_demand_tbl", "GEVO growth", "2022 base below the floor (shipped as na_growth)", n_small),
                                 sector_excl("future_demand_tbl", "GEVO forecast and share growth", "not in the measure catalogue",
                                             sum(r$source == "IEA Global EV Outlook 2026" & grepl("forecast_growth|share_growth", r$variable))),
                                 sector_excl("future_demand_tbl", "WEO", "copies of the world value", nrow(weo) - nrow(d))))
}

# ---- Prices, costs, TRL, alignment ----------------------------------------------------------------

sector_facts_prices <- function(ep, commodities, year = 2025L) {
  r <- sector_raw_rows(ep)
  vmap <- c(latest_price = "prices.latest", yoy_price_change_pct = "prices.yoy", price_volatility_1yr = "prices.vol_1y",
            price_volatility_5yr = "prices.vol_5y", price_volatility_10yr = "prices.vol_10y", price_volatility_20yr = "prices.vol_20y")
  r <- r[r$variable %in% names(vmap), , drop = FALSE]
  d <- sector_debroadcast(r[c("sub_sector", "variable", "value")], c("sub_sector", "variable"), "IMF prices (countries x techs)")
  d <- d[!is.na(d$value), , drop = FALSE]
  slug <- commodities$slug[match(d$sub_sector, commodities$sub_sector)]
  x <- sector_fx(unname(vmap[d$variable]), paste0("commodity:", slug), "WLD", year, d$value, "obs", "energy_prices_tbl", d$variable,
                 key = d$sub_sector)
  sector_result(x$facts, x$ledger, dplyr::bind_rows(
    sector_excl("energy_prices_tbl", "*", "copies of world values (countries x techs)", nrow(r) - nrow(d)),
    sector_excl("energy_prices_tbl", "mean_price_volatility", "a mean of the four windows (not shipped)", sum(sector_raw_rows(ep)$variable == "mean_price_volatility"))
  ))
}

# cost_competitiveness_tbl: wages per (economy, stage activity), the lending rate and PPI y/y per
# economy; copies across technologies (and stages) collapse; obs_year = the observation year.
sector_facts_costs <- function(cc, nominal_year = 2025L) {
  r <- sector_raw_rows(cc)
  sector_check_iso3(r$iso3c[!is.na(r$value)], "cost_competitiveness_tbl")
  w <- r[r$variable == "earnings_usd" & !is.na(r$value), c("iso3c", "supply_chain", "value", "Year")]
  w <- sector_debroadcast(w, c("iso3c", "supply_chain"), "wages (techs)", c("value", "Year"))
  a <- sector_fx("prices.wages", paste0("activity:", tolower(w$supply_chain)), w$iso3c, nominal_year, w$value, "obs",
                 "cost_competitiveness_tbl", "earnings_usd", obs_year = as.integer(w$Year), key = paste(w$iso3c, w$supply_chain))
  one <- function(vn, var) {
    x <- r[r$variable == vn & !is.na(r$value), c("iso3c", "value", "Year")]
    x <- sector_debroadcast(x, "iso3c", paste(vn, "(techs x stages)"), c("value", "Year"))
    sector_fx(var, "none:none", x$iso3c, nominal_year, x$value, ifelse(x$value == 0, "zero", "obs"), "cost_competitiveness_tbl", vn,
              obs_year = as.integer(x$Year), key = x$iso3c)
  }
  b <- one("nominal_rate", "prices.lending_rate")
  c3 <- one("ppi_yoy", "prices.ppi_yoy")
  sector_result(dplyr::bind_rows(a$facts, b$facts, c3$facts), dplyr::bind_rows(a$ledger, b$ledger, c3$ledger),
                dplyr::bind_rows(sector_excl("cost_competitiveness_tbl", "ppi", "PPI levels are not comparable across economies", sum(r$variable == "ppi")),
                                 sector_excl("cost_competitiveness_tbl", "earnings_usd, nominal_rate, ppi_yoy", "copies across technologies and stages",
                                             sum(r$variable %in% c("earnings_usd", "nominal_rate", "ppi_yoy") & !is.na(r$value)) - nrow(a$facts) - nrow(b$facts) - nrow(c3$facts))))
}

# technological_readiness_tbl: one value per cell (copies across economies collapse); fallback
# cells (the technology-wide value) from the explanation text.
sector_facts_trl <- function(tr, year = 2025L) {
  r <- sector_raw_rows(tr)
  vmap <- c("TRL 2025" = "technology.trl")
  dvar <- unique(r$variable[grepl("^TRL .*2020", r$variable)])
  if (length(dvar) != 1) stop("TRL change variable not found")
  vmap[dvar] <- "technology.trl_change"
  r <- r[r$variable %in% names(vmap), , drop = FALSE]
  d <- sector_debroadcast(r[c("tech", "supply_chain", "variable", "value", "explanation")], c("tech", "supply_chain", "variable"), "TRL (economies)")
  x <- sector_fx(unname(vmap[d$variable]), paste0("cell:", sector_cell_slug(d$tech, d$supply_chain)), "WLD", year, d$value, "obs",
                 "technological_readiness_tbl", d$variable, key = paste(d$tech, d$supply_chain))
  fb <- d[d$variable == "TRL 2025" & !grepl("Stage-specific", d$explanation), , drop = FALSE]
  c(sector_result(x$facts, x$ledger, sector_excl("technological_readiness_tbl", "*", "copies across economies", nrow(r) - nrow(d))),
    list(fallback = paste(fb$tech, fb$supply_chain)))
}

sector_facts_alignment <- function(ideal, year = 2023L) {
  x <- ideal[!is.na(ideal$s_axis) & grepl("^[A-Z]{3}$", ideal$iso3), , drop = FALSE]
  f <- sector_fx("development.alignment", "none:none", x$iso3, year, x$s_axis, "obs", "geo_ideal_points_tbl", "s_axis", key = x$iso3)
  sector_result(f$facts, f$ledger, sector_excl("geo_ideal_points_tbl", "s_axis", "NA (no votes, no sovereign)", sum(is.na(ideal$s_axis))))
}
