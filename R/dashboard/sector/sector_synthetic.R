# A small deterministic Sector Insights bundle with the real schema (spec_sector.md 8.3
# `--synthetic`): 10 economies (China, three coalition-core EU and non-EU members, an EU member
# outside the coalition, two partners, a non-reporter and a re-export hub), the EU, the world, three
# IEA EV regions, three IEA ETP entities and one residual; trade for three cells of two
# technologies over two years; two minerals; two fuels; two commodities; a thin cell
# (geothermal-upstream); every value state; policy records and CAT ratings; and every series table.
# Pure: the dictionary arrives as tables (sector_dictionary_tables()).

sector_synthetic_geo <- function() {
  g <- tibble::tribble(
    ~code, ~name, ~kind, ~iso_n3, ~tier, ~group, ~eu_member, ~re_export_hub, ~bloc, ~s_axis, ~alignment_basis, ~wb_region, ~income, ~lon, ~lat, ~reason,
    "BRA", "Brazil", "country", 76L, "partner", "partner", FALSE, FALSE, "Nonaligned", 0.31, "own_votes", "Latin America & Caribbean", "Upper middle income", -47.93, -15.78, "Nonaligned; supplier: reserves (lithium 10%)",
    "CHN", "China", "country", 156L, "none", "china", FALSE, FALSE, "China-leaning", 0, "own_votes", "East Asia & Pacific", "Upper middle income", 116.39, 39.91, NA,
    "DEU", "Germany", "country", 276L, "core", "core", TRUE, FALSE, "U.S.-leaning", 0.71, "own_votes", "Europe & Central Asia", "High income", 13.41, 52.52, "U.S.-leaning; agreements: G7; MSP; NATO; EU member: node EUU (core)",
    "EST", "Estonia", "country", 233L, "none", "rest", TRUE, FALSE, "U.S.-leaning", 0.72, "own_votes", "Europe & Central Asia", "High income", 24.75, 59.44, NA,
    "HKG", "Hong Kong SAR, China", "country", 344L, "none", "rest", FALSE, TRUE, "China-leaning", 0, "inherited", "East Asia & Pacific", "High income", 114.17, 22.28, NA,
    "IDN", "Indonesia", "country", 360L, "partner", "partner", FALSE, FALSE, "China-leaning", 0.14, "own_votes", "East Asia & Pacific", "Upper middle income", 106.83, -6.2, "Agreement: IPEF; major supplier (nickel)",
    "JPN", "Japan", "country", 392L, "core", "core", FALSE, FALSE, "U.S.-leaning", 0.62, "own_votes", "East Asia & Pacific", "High income", 139.77, 35.67, "U.S.-leaning; agreements: G7; IPEF; MSP",
    "POL", "Poland", "country", 616L, "core", "core", TRUE, FALSE, "U.S.-leaning", 0.7, "own_votes", "Europe & Central Asia", "High income", 21.02, 52.26, "U.S.-leaning; agreements: MSP; NATO; EU member: node EUU (core)",
    "RUS", "Russian Federation", "country", 643L, "none", "rest", FALSE, FALSE, "China-leaning", 0.05, "own_votes", "Europe & Central Asia", "Upper middle income", 37.62, 55.75, NA,
    "USA", "United States", "country", 840L, "core", "core", FALSE, FALSE, "U.S.-leaning", 1, "own_votes", "North America", "High income", -77.03, 38.89, "U.S.-leaning; agreements: G7; IPEF; MSP; NATO"
  )
  agg <- tibble::tribble(
    ~code, ~name, ~kind, ~tier, ~group,
    "EUU", "European Union (27)", "eu", "core", "core",
    "WLD", "World", "world", "none", "na",
    "R:iea_ev:china", "China (IEA EV region)", "region", "none", "china",
    "R:iea_ev:european-union", "European Union (IEA EV region)", "region", "none", "core",
    "R:iea_ev:rest-of-world", "Rest of world (IEA EV region)", "region", "none", "na",
    "E:iea_etp:china", "China (IEA)", "entity", "none", "china",
    "E:iea_etp:eu", "European Union (IEA)", "entity", "none", "core",
    "E:iea_etp:us", "United States (IEA)", "entity", "none", "core",
    "RES:ei", "Other economies (EI)", "residual", "none", "na"
  )
  agg$iso_n3 <- NA_integer_
  agg$eu_member <- FALSE
  agg$re_export_hub <- FALSE
  agg$bloc <- NA_character_
  agg$s_axis <- NA_real_
  agg$alignment_basis <- NA_character_
  agg$wb_region <- NA_character_
  agg$income <- NA_character_
  agg$lon <- NA_real_
  agg$lat <- NA_real_
  agg$reason <- NA_character_
  dplyr::bind_rows(g, agg[names(g)])
}

# Items: all 34 cells, plus the axes the synthetic data use.
sector_synthetic_items <- function(trade_cells = c("batteries-upstream", "batteries-midstream", "oil-upstream")) {
  cells <- sector_cell_items(trade_cells)
  other <- tibble::tribble(
    ~axis, ~slug, ~label, ~text,
    "mineral", "lithium", "Lithium", "lithium",
    "mineral", "graphite", "Graphite", "graphite",
    "fuel", "oil", "Oil", "oil",
    "fuel", "gas", "Natural gas", "gas",
    "fuel", "solar", "Solar", "solar",
    "commodity", "oil-brent", "Crude oil, Brent", "Brent crude",
    "commodity", "lithium", "Lithium", "lithium"
  )
  other$tech <- NA_character_
  other$stage <- NA_character_
  other$headline <- FALSE
  other$has_trade <- FALSE
  other$sort <- stats::ave(seq_len(nrow(other)), other$axis, FUN = seq_along)
  dplyr::bind_rows(cells, other[names(cells)], sector_fixed_items())
}

# The cell items (Strategic tech order x stage), has_trade for `trade_cells`: the 33 headline cells,
# Semiconductors Midstream, and any other Semiconductors stage the trade basket has (Upstream since
# the basket review of 2026-10-11: gallium, germanium and indium). Semiconductors cells are
# non-headline (trade only).
sector_cell_items <- function(trade_cells) {
  g <- expand.grid(stage = SECTOR_STAGE_SLUGS, tech = SECTOR_TECH_SLUGS, stringsAsFactors = FALSE)
  semi <- g$tech == "semiconductors"
  g <- g[!semi | g$stage == "midstream" | paste0(g$tech, "-", g$stage) %in% trade_cells, c("tech", "stage")]
  slug <- paste0(g$tech, "-", g$stage)
  tibble::tibble(
    axis = "cell", slug = slug,
    label = paste0(unname(SECTOR_TECH_LABELS[g$tech]), " — ", unname(SECTOR_STAGE_LABELS[g$stage])),
    tech = unname(SECTOR_TECH_LABELS[g$tech]), stage = unname(SECTOR_STAGE_LABELS[g$stage]),
    headline = g$tech != "semiconductors", sort = seq_along(slug), has_trade = slug %in% trade_cells,
    text = paste(unname(SECTOR_TECH_LABELS[g$tech]), tolower(unname(SECTOR_STAGE_LABELS[g$stage])))
  )
}

# Activity, policy-tag and none items (the same in the real export).
sector_fixed_items <- function() {
  act <- tibble::tibble(axis = "activity", slug = SECTOR_STAGE_SLUGS,
                        label = c("Mining and utilities", "Manufacturing", "Construction"),
                        text = c("mining and utilities", "manufacturing", "construction"))
  tags <- c(SECTOR_POLICY_TAGS, "Cross-cutting")
  pol <- tibble::tibble(axis = "policy_tag", slug = sector_policy_tag_slugs(tags),
                        label = ifelse(tags == "Cross-cutting", "Cross-cutting (economy-wide)", tags), text = tolower(tags))
  none <- tibble::tibble(axis = "none", slug = "none", label = "All", text = "all")
  out <- dplyr::bind_rows(act, pol, none)
  out$tech <- NA_character_
  out$stage <- NA_character_
  out$headline <- FALSE
  out$has_trade <- FALSE
  out$sort <- stats::ave(seq_len(nrow(out)), out$axis, FUN = seq_along)
  out
}

# Links that are the same in the real export: activities -> every cell of their stage; policy tags
# -> every cell of their technology (Critical Minerals -> the clean-tech Upstream cells).
sector_fixed_links <- function(cells) {
  act <- dplyr::bind_rows(lapply(SECTOR_STAGE_SLUGS, function(s) {
    cs <- cells$slug[tolower(cells$stage) == s]
    tibble::tibble(item_key = paste0("activity:", s), cell = cs, weight = 1)
  }))
  pol <- dplyr::bind_rows(lapply(SECTOR_POLICY_TAGS, function(t) {
    cs <- if (t == "Critical Minerals") {
      cells$slug[cells$stage == "Upstream" & cells$tech %in% c("Batteries", "Electric Vehicles", "Solar", "Wind", "Electric Grid", "Green Hydrogen")]
    } else cells$slug[cells$tech == t]
    tibble::tibble(item_key = paste0("policy_tag:", sector_policy_tag_slugs(t)), cell = cs, weight = 1)
  }))
  dplyr::bind_rows(act, pol)
}

# HS6-level dyads (the shape of partnership_strength_standardize_comtrade()) that reproduce a
# synthetic matrix: each observed positive flow split over the cell's codes by fixed fractions (the
# k-th code of a cell with n codes gets weight k / sum(1:n)), declared by the importer (source
# "importer") or by the exporter at value / 1.06 (source "mirror").
sector_synthetic_dyads <- function(tc, basket, cif_fob_factor = 1.06) {
  rows <- list()
  for (yi in seq_along(SECTOR_TRADE_YEARS)) {
    v <- if (yi == 1) tc$value_first else tc$value_last
    src <- if (yi == 1) tc$source_first else tc$source_last
    ok <- !is.na(v) & v > 0
    cell <- sector_cell_slug(tc$tech, tc$supply_chain)
    for (i in which(ok)) {
      codes <- basket$hs6[basket$cell == cell[i]]
      w <- seq_along(codes) / sum(seq_along(codes))
      imp <- src[i] == "importer"
      rows[[length(rows) + 1]] <- tibble::tibble(
        reporter_iso = if (imp) tc$importer_iso[i] else tc$exporter_iso[i],
        partner_iso = if (imp) tc$exporter_iso[i] else tc$importer_iso[i],
        period = SECTOR_TRADE_YEARS[yi], flow_direction = if (imp) "import" else "export",
        cmd_code = codes, primary_value = if (imp) v[i] * w else v[i] / cif_fob_factor * w
      )
    }
  }
  dplyr::bind_rows(rows)
}

# A synthetic matrix in the shape partnership_strength_build_trade_cells() returns.
sector_synthetic_trade_cells <- function() {
  econ <- c("BRA", "CHN", "DEU", "EST", "HKG", "IDN", "JPN", "POL", "RUS", "USA")
  spec <- list(
    list(tech = "Batteries", stage = "Midstream",
         s = c(BRA = 0.05, CHN = 40, DEU = 6, EST = 0.1, HKG = 0.3, IDN = 1, JPN = 5, POL = 8, RUS = 0.2, USA = 4),
         w = c(BRA = 0.02, CHN = 0.05, DEU = 0.2, EST = 0.01, HKG = 0.03, IDN = 0.02, JPN = 0.1, POL = 0.08, RUS = 0.04, USA = 0.3)),
    list(tech = "Batteries", stage = "Upstream",
         s = c(BRA = 2, CHN = 1, DEU = 0.05, EST = 0, HKG = 0.01, IDN = 1.5, JPN = 0.02, POL = 0, RUS = 0.1, USA = 0.5),
         w = c(BRA = 0.01, CHN = 0.6, DEU = 0.05, EST = 0.01, HKG = 0.01, IDN = 0.02, JPN = 0.15, POL = 0.03, RUS = 0.01, USA = 0.1)),
    list(tech = "Oil", stage = "Upstream",
         s = c(BRA = 30, CHN = 1, DEU = 0.5, EST = 0.1, HKG = 0, IDN = 10, JPN = 0.2, POL = 0.3, RUS = 60, USA = 100),
         w = c(BRA = 0.02, CHN = 0.3, DEU = 0.15, EST = 0.01, HKG = 0.02, IDN = 0.05, JPN = 0.15, POL = 0.05, RUS = 0, USA = 0.2))
  )
  growth <- c(BRA = 0.7, CHN = 0.45, DEU = 0.6, EST = 0.8, HKG = 0.9, IDN = 0.5, JPN = 0.75, POL = 0.55, RUS = 0.65, USA = 0.6)
  rows <- list()
  for (sp in spec) {
    for (e in econ) for (m in econ) {
      if (e == m) next
      w <- sp$w[setdiff(econ, e)]
      base <- if (sum(w) > 0) sp$s[[e]] * sp$w[[m]] / sum(w) * 1e9 else 0
      v24 <- round(base, -3)
      v20 <- round(base * growth[[e]], -3)
      # Patterns: an always-zero pair, a pair that stops, a non-reporting importer in 2024 (RUS:
      # mirror, x1.06) and a non-reporting exporter in 2024 (RUS: seen through importers).
      if (e == "EST" && m == "BRA") { v20 <- 0; v24 <- 0 }
      if (e == "BRA" && m == "EST") v24 <- 0
      s20 <- "importer"
      s24 <- "importer"
      if (m == "RUS") { v24 <- round(base, -3) * 1.06; s24 <- "mirror" }
      if (m == "RUS" && e == "HKG") { v24 <- NA_real_; s24 <- NA_character_ }
      rows[[length(rows) + 1]] <- tibble::tibble(exporter_iso = e, importer_iso = m, tech = sp$tech, supply_chain = sp$stage,
                                                 value_first = v20, value_last = v24, source_first = s20, source_last = s24)
    }
  }
  dplyr::bind_rows(rows)
}

sector_synthetic_reporters <- function(trade_cells) {
  econ <- sort(unique(c(trade_cells$exporter_iso, trade_cells$importer_iso)))
  cells <- unique(sector_cell_slug(trade_cells$tech, trade_cells$supply_chain))
  g <- expand.grid(reporter_iso = econ, period = SECTOR_TRADE_YEARS, flow_direction = c("import", "export"), cell = cells,
                   stringsAsFactors = FALSE)
  # RUS declares nothing in 2024; HKG declares no exports in 2024 (so HKG -> RUS is unobserved).
  drop <- (g$reporter_iso == "RUS" & g$period == 2024) | (g$reporter_iso == "HKG" & g$period == 2024 & g$flow_direction == "export")
  tibble::as_tibble(g[!drop, ])
}

# Every synthetic long table (the `parts` of sector_assemble()). dict: sector_dictionary_tables().
sector_synthetic_parts <- function(dict) {
  geo <- sector_synthetic_geo()
  items <- sector_synthetic_items()
  cells <- items[items$axis == "cell", , drop = FALSE]
  econ <- geo$code[geo$kind == "country"]
  f <- list()
  add <- function(var, item_key, geo, year, value, status = "obs", obs_year = NA_integer_) {
    n <- max(length(geo), length(value), length(year))
    f[[length(f) + 1]] <<- tibble::tibble(var = var, item_key = rep_len(item_key, n), geo = rep_len(geo, n),
                                          year = as.integer(rep_len(year, n)), value = as.double(rep_len(value, n)),
                                          status = rep_len(status, n), obs_year = as.integer(rep_len(obs_year, n)))
  }
  w <- list()
  addw <- function(var, item_key, year, value, basis) {
    w[[length(w) + 1]] <<- tibble::tibble(var = var, item_key = item_key, year = as.integer(year), value = as.double(value), basis = basis)
  }

  # Trade (the real helpers on a synthetic matrix).
  tc <- sector_synthetic_trade_cells()
  declared <- sector_trade_declared(sector_synthetic_reporters(tc))
  country <- sector_trade_country(tc, declared)
  f[[length(f) + 1]] <- tibble::tibble(var = country$var, item_key = country$item_key, geo = country$geo, year = country$year,
                                       value = country$value, status = country$status, obs_year = NA_integer_)
  tw <- sector_trade_world(country)
  w[[length(w) + 1]] <- tw
  flows <- sector_trade_flows(tc)
  basket <- sector_trade_basket(tibble::tribble(
    ~tech, ~supply_chain, ~sub_sector, ~hs6, ~note,
    "Batteries", "Midstream", "Cells", 850760, NA,
    "Batteries", "Midstream", "Lithium chemicals", 282520, NA,
    "Batteries", "Midstream", "Anode materials", 380110, NA,
    "Batteries", "Midstream", "Battery management", 850440, "kept with caveat (basket review 2026-10): static converters of every kind; general-purpose goods.",
    "Batteries", "Upstream", "Ores and concentrates", 253090, NA,
    "Batteries", "Upstream", "Lithium carbonate", 283691, NA,
    "Oil", "Upstream", "Crude oil", 270900, NA,
    "Electric Grid", "Midstream", "Converters", 850440, NA
  ))
  desc <- c(`850760` = "Lithium-ion accumulators", `282520` = "Lithium oxide and hydroxide", `380110` = "Artificial graphite",
            `850440` = "Static converters", `253090` = "Mineral substances n.e.c.", `283691` = "Lithium carbonates",
            `270900` = "Crude petroleum oils")
  ref <- tibble::tibble(tech = "Batteries", supply_chain = "Midstream", sub_sector = "Cells", hs6 = 850680, excluded = TRUE,
                        note = "excluded (basket review 2026-10): primary lithium cells are not rechargeable storage")
  codes_all <- sector_trade_codes(basket[basket$cell != "electric-grid-midstream", ], desc)
  excl_all <- sector_trade_excluded(ref, cells$slug)
  eu_members <- geo$code[geo$eu_member]
  eu_intra <- sector_eu_intra(tc, eu_members)
  # Trade by sub-component: the real helpers on HS6-level dyads that reproduce the synthetic matrix.
  basket_m <- basket[basket$cell %in% unique(sector_cell_slug(tc$tech, tc$supply_chain)), , drop = FALSE]
  sub <- sector_trade_sub(sector_synthetic_dyads(tc, basket_m), basket_m, declared, eu_members)
  cty_sub <- sector_trade_sub_table(sub, country)
  trade <- list()
  for (tech in c("batteries", "oil")) {
    ck <- paste0("cell:", cells$slug[startsWith(cells$slug, paste0(tech, "-"))])
    trade[[tech]] <- list(
      flows = flows[flows$item_key %in% ck, , drop = FALSE],
      declared = declared[declared$item_key %in% ck, , drop = FALSE],
      codes = codes_all[codes_all$item_key %in% ck, , drop = FALSE],
      excluded = excl_all[excl_all$item_key %in% ck, , drop = FALSE],
      cty_sub = cty_sub[cty_sub$item_key %in% ck, , drop = FALSE],
      sub_intra = sub$intra[sub$intra$item_key %in% ck, , drop = FALSE]
    )
  }

  bm <- "cell:batteries-midstream"
  # trade.rca (exporter-reported; HKG excluded; RUS did not report; small exporters nq).
  add("trade.rca", bm, c("CHN", "POL", "DEU", "JPN", "USA", "IDN", "EST", "BRA"), 2024,
      c(3.1, 4.2, 0.9, 1.1, 0.3, NaN, NaN, NaN), c("obs", "obs", "obs", "obs", "obs", "nq", "nq", "nq"))
  add("trade.atlas_share", bm, econ, 2023,
      c(0.0005, 0.409, 0.044, 0.0004, 0.003, 0.01, 0.04, 0.05, 0.001, 0.03))
  # Export feasibility (Atlas relatedness, 0-1); HKG has none (not in the Atlas HS4 panel here).
  add("trade.feasibility", bm, setdiff(econ, "HKG"), 2023, c(0.071, 0.513, 0.412, 0.205, 0.188, 0.397, 0.371, 0.096, 0.402))
  add("trade.feasibility", "cell:batteries-upstream", setdiff(econ, "HKG"), 2023, c(0.144, 0.162, 0.083, 0.051, 0.122, 0.064, 0.077, 0.139, 0.095))
  addw("trade.atlas_share", bm, 2023, 1, "one")
  add("trade.critmin_exports", "mineral:lithium", c("CHN", "BRA", "USA", "IDN", "RUS", "HKG"), 2024,
      c(2.3e9, 0.4e9, 0.1e9, 0, 0.05e9, 0.02e9), c("obs", "obs", "obs", "zero", "mirror", "obs"))
  add("trade.critmin_exports", "mineral:graphite", c("CHN", "DEU", "JPN", "BRA", "POL"), 2024,
      c(1.5e9, 0.2e9, 0.1e9, 0.05e9, 0), c("obs", "obs", "obs", "obs", "zero"))
  addw("trade.critmin_exports", "mineral:lithium", 2024, 2.87e9, "sum_all")
  addw("trade.critmin_exports", "mineral:graphite", 2024, 1.85e9, "sum_all")
  add("trade.critmin_dependence", "mineral:lithium", c("CHN", "BRA", "USA", "IDN", "RUS", "HKG"), 2024,
      c(0.00065, 0.0012, 0.00005, 0, 0.00012, 0.00003), c("obs", "obs", "obs", "zero", "mirror", "obs"))

  # Resources.
  add("resources.fuel_reserves", "fuel:oil", c("USA", "RUS", "BRA", "CHN", "IDN"), 2020, c(68.8, 107.8, 11.9, 26, 2.4))
  add("resources.fuel_reserves", "fuel:gas", c("RUS", "USA", "CHN", "IDN", "BRA"), 2020, c(37.4, 12.6, 8.4, 1.3, 0.3))
  add("resources.mineral_reserves", "mineral:lithium", c("CHN", "BRA", "USA", "RUS"), 2025, c(3000, 390, 1800, 1000))
  add("resources.mineral_reserves", "mineral:graphite", c("CHN", "BRA", "RUS", "USA", "DEU"), 2025,
      c(81000, 74000, 14000, NaN, NaN), c("obs", "obs", "obs", "nq", "nq"))
  addw("resources.mineral_reserves", "mineral:lithium", 2025, 30000, "source_world")
  addw("resources.mineral_reserves", "mineral:graphite", 2025, 290000, "source_world")
  sd <- "cell:solar-downstream"
  add("resources.pv_quality", sd, econ, 2023, c(4.88, 4.01, 2.83, 2.61, 3.68, 3.95, 3.52, 2.9, 2.75, 4.43))
  add("resources.pv_area", sd, econ, 2023, c(4.16e7, 3.85e7, 1.01e6, 1.18e5, 4.0e3, 7.5e6, 1.33e6, 9.07e5, 4.69e7, 4.2e7))
  gd <- "cell:geothermal-downstream"
  add("resources.geo_potential", gd, c("USA", "CHN", "BRA", "IDN", "JPN", "DEU"), 2023, c(1128988, 850000, 410000, 120000, 45000, 30000))
  add("resources.geo_lcoe_500", gd, c("USA", "CHN", "IDN", "JPN"), 2023, c(0.03, 0.035, 0.04, 0.068))

  # Production.
  add("production.fuels", "fuel:oil", c("USA", "RUS", "BRA", "CHN", "IDN", "DEU", "POL", "JPN", "EST"), 2025,
      c(21900, 10800, 3700, 4300, 600, 30, 20, 0, 0), c(rep("obs", 7), "zero", "nsr"))
  add("production.fuels", "fuel:gas", c("USA", "RUS", "CHN", "IDN", "BRA", "DEU", "POL", "JPN", "EST", "EUU"), 2025,
      c(37.7, 22.3, 9.1, 2.2, 0.9, 0.15, 0.15, 0.08, 0, 1.5), c(rep("obs", 8), "nsr", "obs"))
  addw("production.fuels", "fuel:oil", 2025, 96000, "source_world")
  addw("production.fuels", "fuel:gas", 2025, 151, "source_world")
  add("production.generation", "fuel:solar", c("CHN", "USA", "JPN", "DEU", "BRA", "POL", "IDN", "RUS", "EST", "HKG", "EUU"), 2025,
      c(1100, 300, 110, 70, 70, 20, 1, 3, 1, 0, 300), c(rep("obs", 9), "zero", "obs"))
  addw("production.generation", "fuel:solar", 2025, 2600, "source_world")
  for (yr in c(2025L, 2030L, 2035L, 2040L)) {
    k <- match(yr, c(2025L, 2030L, 2035L, 2040L))
    add("production.mining", "mineral:lithium", c("CHN", "BRA", "USA"), yr, c(c(40, 70, 90, 100)[k], c(20, 35, 45, 50)[k], c(5, 10, 25, 40)[k]))
    addw("production.mining", "mineral:lithium", yr, c(230, 400, 600, 700)[k], "source_world")
    add("production.mining", "mineral:graphite", c("CHN", "BRA", "RUS"), yr, c(1300, 1500, 1700, 1800)[k] * c(1, 0.08, 0.015))
    addw("production.mining", "mineral:graphite", yr, c(1600, 2000, 2400, 2700)[k], "source_world")
    add("production.refining", "mineral:lithium", c("CHN", "USA", "JPN"), yr, c(235, 300, 340, 360)[k] * c(1, 0.05, 0.02))
    addw("production.refining", "mineral:lithium", yr, c(319, 450, 560, 640)[k], "source_world")
    add("production.refining", "mineral:graphite", c("CHN", "JPN", "USA"), yr, c(2139, 2500, 2800, 3000)[k] * c(1, 0.02, 0.01 * k))
    addw("production.refining", "mineral:graphite", yr, c(2266, 2800, 3300, 3700)[k], "source_world")
  }
  add("production.mine_usgs", "mineral:lithium", c("CHN", "BRA", "USA"), 2025, c(22000, 10000, NaN), c("obs", "obs", "nq"))
  addw("production.mine_usgs", "mineral:lithium", 2025, 240000, "source_world")
  nu <- "cell:nuclear-upstream"
  for (yr in 2023:2024) {
    add("production.uranium", nu, econ, yr,
        c(50, 1700, 0, 0, 0, 0, 0, 0, 2600 + 100 * (yr - 2023), 600 + 77 * (yr - 2023)),
        ifelse(c(50, 1700, 0, 0, 0, 0, 0, 0, 1, 1) > 0, "obs", "zero"))
  }
  em <- "cell:electric-vehicles-midstream"
  reg <- c("R:iea_ev:china", "R:iea_ev:european-union", "R:iea_ev:rest-of-world")
  add("production.ev_region", em, reg, 2024, c(12.4047, 2.369, 2.5))
  add("production.ev_region_sales", em, reg, 2024, c(11.23, 2.226, 4.0))
  add("manufacturing.ev_net_export_ratio", em, reg, 2024, c(0.1045948, 0.064240791, -0.375))
  ent <- c("E:iea_etp:china", "E:iea_etp:eu", "E:iea_etp:us")
  add("manufacturing.share_now", bm, ent, 2024, c(0.76, 0.08, 0.07))
  add("manufacturing.share_now", "cell:solar-midstream", ent, 2024, c(0.79, 0, 0), c("obs", "nq", "nq"))
  add("manufacturing.share_2035", bm, ent, 2035, c(0.68, 0.11, 0.15))
  for (k in c(bm, "cell:solar-midstream")) {
    addw("manufacturing.share_now", k, 2024, 1, "one")
  }
  addw("manufacturing.share_2035", bm, 2035, 1, "one")

  # Investment (CIM), 2018-2025; 2025 rows are estimates.
  inv <- tibble::tribble(
    ~cell, ~geo, ~v2018, ~g,
    "batteries-midstream", "CHN", 9, 1.25, "batteries-midstream", "USA", 2.5, 1.4, "batteries-midstream", "DEU", 1.5, 1.2,
    "batteries-midstream", "POL", 1.2, 1.1, "batteries-midstream", "JPN", 0.8, 1.05, "batteries-midstream", "IDN", 0.1, 1.6,
    "solar-downstream", "CHN", 60, 1.2, "solar-downstream", "USA", 20, 1.15, "solar-downstream", "DEU", 9, 1.08,
    "solar-downstream", "JPN", 12, 0.95, "solar-downstream", "BRA", 2, 1.35, "solar-downstream", "POL", 1, 1.3
  )
  for (i in seq_len(nrow(inv))) {
    yrs <- 2018:2025
    v <- inv$v2018[i] * inv$g[i]^(yrs - 2018)
    add("investment.annual", paste0("cell:", inv$cell[i]), inv$geo[i], yrs, v, ifelse(yrs == 2025, "est", "obs"))
  }
  for (cl in unique(inv$cell)) for (yr in 2018:2025) {
    x <- dplyr::bind_rows(f)
    x <- x[x$var == "investment.annual" & x$item_key == paste0("cell:", cl) & x$year == yr, ]
    addw("investment.annual", paste0("cell:", cl), yr, sum(x$value), "sum_all")
  }

  # Consumption.
  add("consumption.per_capita", "fuel:oil", econ, 2025, c(0.025, 0.022, 0.055, 0.035, 0.08, 0.012, 0.065, 0.045, 0.042, 0.11))
  add("consumption.per_capita", "fuel:gas", econ, 2025, c(0.007, 0.0095, 0.03, 0, 0.11, 0.008, 0.028, 0.02, 0.21, 0.098),
      c("obs", "obs", "obs", "nsr", "obs", "obs", "obs", "obs", "obs", "obs"))
  add("consumption.pc_change", "fuel:oil", econ, 2025, c(0.04, 0.12, -0.06, -0.1, -0.02, 0.15, -0.08, 0.05, 0.03, -0.01))
  add("consumption.pc_change", "fuel:gas", econ, 2025, c(0.1, 0.35, -0.18, NaN, 0.02, 0.2, -0.05, 0.08, 0.06, 0.04),
      c("obs", "obs", "obs", "na_growth", "obs", "obs", "obs", "obs", "obs", "obs"))
  # Estonia's oil production is not reported separately (production.fuels nsr): assumed 0, so its
  # balance is minus consumption, nsr, with the note production_assumed_zero (fact_notes below).
  add("consumption.balance", "fuel:oil", c("USA", "RUS", "BRA", "CHN", "JPN", "DEU", "POL", "IDN", "EST"), 2025,
      c(3.3, 16, 2, -25, -7, -4, -1.2, -1, -0.05), c(rep("obs", 8), "nsr"))
  add("consumption.balance", "fuel:gas", c("USA", "RUS", "CHN", "JPN", "DEU", "POL", "IDN", "BRA", "EST"), 2025,
      c(5.2, 7.1, -6.5, -3.6, -2.9, -0.6, 0.9, -0.3, -0.02))
  add("consumption.self_sufficiency", "fuel:oil", c("USA", "RUS", "BRA", "CHN", "JPN", "DEU", "POL", "IDN"), 2025,
      c(115, 380, 130, 30, 0.3, 2, 4, 60))
  ed <- "cell:electric-vehicles-downstream"
  add("consumption.ev_sales", ed, c("CHN", "USA", "DEU", "JPN", "POL", "BRA", "IDN", "EST"), 2025,
      c(13300005, 1530000, 860000, 110000, 40000, 180000, 90000, 2270))
  add("consumption.ev_sales", ed, c("CHN", "USA", "JPN"), 2035, c(25600000, 4000000, 2000000))
  add("consumption.ev_stock", ed, c("CHN", "USA", "DEU", "JPN"), 2025, c(44000840, 7300000, 3100000, 600000))
  add("consumption.ev_sales_share", ed, c("CHN", "USA", "DEU", "JPN", "POL", "BRA", "IDN", "EST"), 2025, c(53, 10, 30, 3.5, 7, 8, 9, 6))
  add("consumption.ev_sales_growth", ed, c("CHN", "USA", "DEU", "JPN", "POL", "BRA", "IDN", "EST"), 2025,
      c(0.6, 0.55, 0.3, 0.2, 1.1, 2.3, NaN, NaN), c(rep("obs", 6), "na_growth", "na_growth"))
  add("consumption.weo", "fuel:oil", "WLD", c(2024, 2035), c(193.3, 191.5))
  add("consumption.weo", "fuel:gas", "WLD", c(2024, 2035), c(147.7, 165.5))

  # Prices (world).
  add("prices.latest", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(63.19, 95256.61))
  add("prices.yoy", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(-14.3, -24.9))
  add("prices.vol_1y", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(0.22, 0.46))
  add("prices.vol_5y", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(0.31, 0.81))
  add("prices.vol_10y", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(0.33, 0.73))
  add("prices.vol_20y", c("commodity:oil-brent", "commodity:lithium"), "WLD", 2025, c(0.35, 0.55))
  add("prices.wages", "activity:midstream", c("CHN", "USA", "DEU", "JPN", "POL", "BRA", "IDN"), 2025,
      c(833, 7055, 5545, 2687, 1900, 600, 250), "obs", c(2022, 2024, 2022, 2021, 2023, 2024, 2015))
  add("prices.wages", "activity:upstream", c("CHN", "USA", "BRA"), 2025, c(1100, 8100, 900), "obs", c(2022, 2024, 2024))
  add("prices.lending_rate", "none:none", c("CHN", "USA", "DEU", "JPN", "BRA", "POL"), 2025, c(3, 7.5, 4.1, 1.4, 46, 7.2), "obs",
      c(2026, 2021, 2025, 2017, 2025, 2025))
  add("prices.ppi_yoy", "none:none", c("DEU", "USA", "CHN", "POL"), 2025, c(-1.2, 2.1, -2.3, 0.5), "obs", c(2025, 2025, 2025, 2024))

  # Technology (cell_global, the WLD row).
  trl <- c(`batteries-upstream` = 6.9, `batteries-midstream` = 7.318182, `oil-upstream` = 9.5, `solar-downstream` = 9.4,
           `geothermal-upstream` = 5.444444, `nuclear-upstream` = 9, `electric-vehicles-midstream` = 9.666667)
  add("technology.trl", paste0("cell:", names(trl)), "WLD", 2025, unname(trl))
  add("technology.trl_change", paste0("cell:", names(trl)), "WLD", 2025, c(1.2, 1.35, 0.1, 0.4, 0, 0.2, 0.9))
  add("development.alignment", "none:none", econ, 2023, geo$s_axis[match(econ, geo$code)])

  facts <- dplyr::bind_rows(f)
  world <- dplyr::bind_rows(w)

  # Series: EI 2015-2025 (WLD in series.world, the residual as RES:ei), IEA EV 2021-2024, CIM power
  # additions 2018-2025, IMF monthly 2024-2025, CIM capacity by product.
  s <- list()
  sw <- list()
  yrs <- 2015:2025
  ser_fuel <- function(var, item, geos, last, wld_last, step) {
    for (j in seq_along(geos)) {
      v <- last[j] * (1 - step * (2025 - yrs))
      s[[length(s) + 1]] <<- tibble::tibble(var = var, item_key = item, geo = geos[j], year = yrs, value = v, status = "obs", obs_year = NA_integer_)
    }
    wv <- wld_last * (1 - step * (2025 - yrs))
    sw[[length(sw) + 1]] <<- tibble::tibble(var = var, item_key = item, year = yrs, value = wv, basis = "source_world")
    listed <- Reduce(`+`, lapply(seq_along(geos), function(j) last[j] * (1 - step * (2025 - yrs))))
    s[[length(s) + 1]] <<- tibble::tibble(var = var, item_key = item, geo = "RES:ei", year = yrs, value = wv - listed, status = "obs", obs_year = NA_integer_)
  }
  ser_fuel("production.fuels", "fuel:oil", c("USA", "RUS", "BRA", "CHN", "IDN", "DEU", "POL"), c(21900, 10800, 3700, 4300, 600, 30, 20), 96000, 0.02)
  ser_fuel("production.fuels", "fuel:gas", c("USA", "RUS", "CHN", "IDN", "BRA", "DEU", "POL", "JPN"), c(37.7, 22.3, 9.1, 2.2, 0.9, 0.15, 0.15, 0.08), 151, 0.025)
  ser_fuel("production.generation", "fuel:solar", c("CHN", "USA", "JPN", "DEU", "BRA", "POL", "IDN", "RUS", "EST"), c(1100, 300, 110, 70, 70, 20, 1, 3, 1), 2600, 0.085)
  ser_fuel("consumption.total_ej", "fuel:oil", c("USA", "CHN", "JPN", "DEU", "BRA", "RUS"), c(37, 32, 6.5, 4.4, 4.9, 6.6), 196, 0.01)
  # EI's own EU row (kind eu) in the series too: 2025 = the facts value.
  s[[length(s) + 1]] <- tibble::tibble(var = "production.fuels", item_key = "fuel:gas", geo = "EUU", year = yrs,
                                       value = 1.5 * (1 + 0.04 * (2025 - yrs)), status = "obs", obs_year = NA_integer_)
  s[[length(s) + 1]] <- tibble::tibble(var = "production.generation", item_key = "fuel:solar", geo = "EUU", year = yrs,
                                       value = 300 * (1 - 0.085 * (2025 - yrs)), status = "obs", obs_year = NA_integer_)
  for (j in seq_along(reg)) {
    s[[length(s) + 1]] <- tibble::tibble(var = "production.ev_region", item_key = em, geo = reg[j], year = 2021:2024,
                                         value = c(3.29, 6.45, 8.6, 12.4047)[c(1, 2, 3, 4)] * c(1, 0.191, 0.2015)[j], status = "obs", obs_year = NA_integer_)
    s[[length(s) + 1]] <- tibble::tibble(var = "production.ev_region_sales", item_key = em, geo = reg[j], year = 2021:2024,
                                         value = c(3.33, 6.0, 8.1, 11.23) * c(1, 0.1982, 0.3562)[j], status = "obs", obs_year = NA_integer_)
  }
  for (g in c("CHN", "USA", "DEU", "BRA")) {
    s[[length(s) + 1]] <- tibble::tibble(var = "investment.power_additions", item_key = sd, geo = g, year = 2018:2025,
                                         value = c(CHN = 45, USA = 10, DEU = 3, BRA = 0.5)[[g]] * 1.25^(0:7),
                                         status = c(rep("obs", 7), "est"), obs_year = NA_integer_)
  }
  series_obs <- dplyr::bind_rows(s)
  # Make the 2025 / 2024 cross-sections identical to facts (a contract rule).
  fx <- paste(facts$var, facts$item_key, facts$geo, facts$year)
  sx <- paste(series_obs$var, series_obs$item_key, series_obs$geo, series_obs$year)
  m <- match(sx, fx)
  series_obs$value[!is.na(m)] <- facts$value[m[!is.na(m)]]
  series_obs$status[!is.na(m)] <- facts$status[m[!is.na(m)]]
  wx <- paste(world$var, world$item_key, world$year)
  series_world <- dplyr::bind_rows(sw)
  swx <- paste(series_world$var, series_world$item_key, series_world$year)
  mw <- match(swx, wx)
  series_world$value[!is.na(mw)] <- world$value[mw[!is.na(mw)]]
  # Re-derive the residual from the aligned values.
  for (key in unique(paste(series_obs$var, series_obs$item_key)[series_obs$geo == "RES:ei"])) {
    for (yr in yrs) {
      sel <- paste(series_obs$var, series_obs$item_key) == key & series_obs$year == yr
      listed <- sel & geo$kind[match(series_obs$geo, geo$code)] == "country"
      wv <- series_world$value[paste(series_world$var, series_world$item_key) == key & series_world$year == yr]
      series_obs$value[sel & series_obs$geo == "RES:ei"] <- wv - sum(series_obs$value[listed])
    }
  }
  months <- expand.grid(month = 1:12, year = 2024:2025)
  monthly <- dplyr::bind_rows(
    tibble::tibble(var = "prices.monthly", item_key = "commodity:oil-brent", geo = "WLD", year = months$year, month = months$month,
                   value = round(80 - 0.7 * seq_len(24) + 3 * sin(seq_len(24)), 2), status = "obs"),
    tibble::tibble(var = "prices.monthly", item_key = "commodity:lithium", geo = "WLD", year = months$year, month = months$month,
                   value = round(160000 - 2700 * seq_len(24), 2), status = "obs")
  )
  for (cm in c("commodity:oil-brent", "commodity:lithium")) {
    monthly$value[monthly$item_key == cm & monthly$year == 2025 & monthly$month == 12] <- facts$value[facts$var == "prices.latest" & facts$item_key == cm]
  }
  capacity <- tibble::tribble(
    ~item_key, ~product, ~unit, ~geo, ~year, ~cap_status, ~value, ~status,
    bm, "Battery cells", "GWh", "CHN", 2025L, "operational", 1500, "obs",
    bm, "Battery cells", "GWh", "CHN", 2025L, "construction", 300, "obs",
    bm, "Battery cells", "GWh", "CHN", 2025L, "announced", 800, "obs",
    bm, "Battery cells", "GWh", "USA", 2025L, "operational", 120, "obs",
    bm, "Battery cells", "GWh", "USA", 2025L, "announced", 400, "obs",
    bm, "Battery cells", "GWh", "USA", 2025L, "paused", 30, "obs",
    bm, "Battery cells", "GWh", "DEU", 2025L, "operational", 60, "obs",
    bm, "Cathode active material", "kt", "CHN", 2025L, "operational", 4200, "obs",
    bm, "Cathode active material", "kt", "POL", 2025L, "operational", 90, "est"
  )
  series <- list(obs = series_obs, world = series_world, monthly = monthly, capacity = capacity)

  # Policy.
  pams <- tibble::tribble(
    ~geo, ~year, ~status, ~jurisdiction, ~type, ~tags, ~stages, ~tech_src, ~title,
    "USA", 2022L, "In force", "National", "Financial incentive", "Batteries|Electric Vehicles", "Midstream", "iea", "Advanced manufacturing production credit",
    "USA", 2023L, "In force", "National", "Regulation/standard", "Electric Vehicles", "Downstream", "iea", "Clean vehicle credit sourcing rules",
    "USA", 2027L, "Planned", "National", "Financial incentive", "Critical Minerals", "Upstream", "text", "Critical minerals stockpile programme",
    "CHN", 2021L, "In force", "National", "Regulation/standard", "Batteries", "Midstream", "iea", "Battery industry standard conditions",
    "CHN", 2016L, "Ended", "National", "Financial incentive", "Solar", "Downstream", "iea", "Solar feed-in tariff",
    "CHN", 2026L, "Planned", "National", "Other", "Oil", "", "text", "Strategic petroleum reserve expansion",
    "DEU", 2020L, "In force", "National", "R&D / Demonstration", "Batteries", "Midstream|Upstream", "iea", "Battery cell research programme",
    "DEU", 2019L, "In force", "National", "Regulation/standard", "", "", NA, "Climate action programme",
    "POL", 2021L, "In force", "National", "Financial incentive", "Electric Vehicles", "Downstream", "iea", "Clean transport fund",
    "EUU", 2023L, "In force", "International", "Regulation/standard", "Batteries", "Midstream|Downstream", "iea", "Batteries regulation",
    "EUU", 2024L, "In force", "International", "Regulation/standard", "Critical Minerals", "Upstream|Midstream", "iea", "Critical raw materials act",
    "EUU", 2028L, "Planned", "International", "Financial incentive", "", "", NA, "Clean industrial deal fund",
    "JPN", 2022L, "In force", "National", "Financial incentive", "Batteries", "Midstream", "iea", "Storage battery supply chain subsidy",
    "JPN", 2015L, "Unknown", "National", "Information/labeling", "Solar", "", "text", "Solar labelling scheme",
    "BRA", 2024L, "In force", "National", "Financial incentive", "Critical Minerals", "Upstream", "text", "Strategic minerals fund",
    "IDN", 2020L, "In force", "National", "Regulation/standard", "Critical Minerals", "Upstream|Midstream", "iea", "Nickel ore export ban",
    "IDN", 2029L, "Planned", "National", "Other", "Electric Vehicles", "Midstream", "text", "EV industrial estate plan",
    "RUS", 2018L, "In force", "National", "Other", "Oil|Gas", "Upstream", "iea", "Tax manoeuvre for oil and gas",
    "HKG", 2021L, "In force", "Subnational", "Voluntary/coordination", "", "", NA, "Climate action plan"
  )
  pams$tech_mask <- vapply(pams$tags, function(t) {
    if (!nzchar(t)) return(0L)
    b <- match(strsplit(t, "|", fixed = TRUE)[[1]], SECTOR_POLICY_TAGS) - 1L
    Reduce(bitwOr, bitwShiftL(1L, b), 0L)
  }, integer(1), USE.NAMES = FALSE)
  pams$stage_mask <- vapply(pams$stages, function(t) {
    if (!nzchar(t)) return(0L)
    b <- match(strsplit(t, "|", fixed = TRUE)[[1]], SECTOR_POLICY_STAGES) - 1L
    Reduce(bitwOr, bitwShiftL(1L, b), 0L)
  }, integer(1), USE.NAMES = FALSE)
  pams$id <- seq_len(nrow(pams)) - 1L
  pams$cross_cutting <- pams$tech_mask == 0L
  policy <- list(
    pams = pams[c("id", "geo", "year", "status", "jurisdiction", "type", "tech_mask", "tech_src", "stage_mask", "cross_cutting")],
    text = tibble::tibble(id = pams$id, title = pams$title,
                          url = ifelse(pams$id %% 4 == 3, NA_character_, paste0("https://www.iea.org/policies/", 1000 + pams$id, "-synthetic"))),
    tags = SECTOR_POLICY_TAGS,
    cat = tibble::tribble(
      ~geo, ~overall, ~policies_action, ~domestic_target, ~fair_share, ~climate_finance, ~ndc, ~updated,
      "BRA", "Insufficient", "Insufficient", "Almost sufficient", "Insufficient", NA, "Average", "Jun-25",
      "CHN", "Highly insufficient", "Insufficient", "Highly insufficient", "Highly insufficient", NA, "Average", "Dec-24",
      "EUU", "Insufficient", "Insufficient", "Almost sufficient", "Insufficient", "Insufficient", "Acceptable", "Apr-25",
      "IDN", "Critically insufficient", "Highly insufficient", "Critically insufficient", "Critically insufficient", NA, "Poor", "Jan-25",
      "JPN", "Insufficient", "Insufficient", "Almost sufficient", "Insufficient", "Insufficient", "Average", "Feb-25",
      "RUS", "Critically insufficient", "Critically insufficient", "Critically insufficient", "Critically insufficient", NA, "Poor", "Nov-22",
      "USA", "Insufficient", "Insufficient", "Almost sufficient", "Insufficient", "Critically insufficient", "Average", "Nov-24"
    )
  )

  # Links and IEA demand.
  item_links <- dplyr::bind_rows(
    tibble::tribble(
      ~item_key, ~cell, ~weight,
      "mineral:lithium", "batteries-upstream", 0.12,
      "mineral:lithium", "electric-vehicles-upstream", 0.85,
      "mineral:graphite", "batteries-upstream", 0.1,
      "mineral:graphite", "electric-vehicles-upstream", 0.82,
      "fuel:oil", "oil-upstream", 1, "fuel:oil", "oil-downstream", 1,
      "fuel:gas", "gas-upstream", 1, "fuel:gas", "gas-downstream", 1, "fuel:solar", "solar-downstream", 1,
      "commodity:oil-brent", "oil-upstream", 1,
      "commodity:lithium", "batteries-upstream", 1,
      "commodity:lithium", "electric-vehicles-upstream", 1
    ),
    sector_fixed_links(cells)
  )
  mineral_demand <- tibble::tribble(
    ~mineral, ~tech, ~label, ~scenario, ~year, ~kt,
    "lithium", "Electric Vehicles", "Electric vehicles", "observed", 2025L, 180,
    "lithium", "Batteries", "Battery storage", "observed", 2025L, 25,
    "lithium", NA, "Total clean technologies", "observed", 2025L, 210,
    "lithium", "Electric Vehicles", "Electric vehicles", "STEPS", 2030L, 400,
    "lithium", "Batteries", "Battery storage", "STEPS", 2030L, 60,
    "lithium", NA, "Total clean technologies", "STEPS", 2030L, 470,
    "lithium", "Electric Vehicles", "Electric vehicles", "STEPS", 2040L, 800,
    "lithium", "Batteries", "Battery storage", "STEPS", 2040L, 110,
    "lithium", NA, "Total clean technologies", "STEPS", 2040L, 930,
    "graphite", "Electric Vehicles", "Electric vehicles", "observed", 2025L, 1500,
    "graphite", "Batteries", "Battery storage", "observed", 2025L, 200,
    "graphite", NA, "Total clean technologies", "observed", 2025L, 1750,
    "graphite", "Electric Vehicles", "Electric vehicles", "STEPS", 2040L, 4200,
    "graphite", NA, "Total clean technologies", "STEPS", 2040L, 4800
  )
  item_units <- dict$item_units
  item_units$item_key <- sector_item_key(item_units$axis, item_units$item)
  item_units <- item_units[item_units$item_key %in% sector_item_key(items$axis, items$slug), c("var", "item_key", "unit")]
  pu <- tibble::tibble(var = c("prices.latest", "prices.latest", "prices.monthly", "prices.monthly"),
                       item_key = rep(c("commodity:oil-brent", "commodity:lithium"), 2), unit = rep(c("usd_bbl", "usd_t"), 2))
  item_units <- dplyr::bind_rows(item_units, pu)

  reported <- tibble::tibble(item_key = bm, year = 2024L, total = 6.2e10, china_reported = 0.61, hhi_reported = 0.41, n_reporters = 8L)

  fdi_clean <- sector_fdi_clean(sector_synthetic_cmfdi_raw(), sector_synthetic_iso3)
  fdi <- sector_fdi_part(fdi_clean, geo)

  # Development & finance (sector-dev and the development.* facts).
  dev <- sector_dev_synthetic(cells)
  items <- dplyr::bind_rows(items, dev$items[names(items)])
  item_links <- dplyr::bind_rows(item_links, dev$links)
  facts <- dplyr::bind_rows(facts, dev$facts)
  item_units <- dplyr::bind_rows(item_units, dev$item_units)

  list(
    geo = geo,
    region_members = tibble::tribble(
      ~geo, ~member,
      "EUU", "DEU", "EUU", "EST", "EUU", "POL",
      "R:iea_ev:china", "CHN",
      "R:iea_ev:european-union", "DEU", "R:iea_ev:european-union", "EST", "R:iea_ev:european-union", "POL",
      "R:iea_ev:rest-of-world", "BRA", "R:iea_ev:rest-of-world", "HKG", "R:iea_ev:rest-of-world", "IDN",
      "R:iea_ev:rest-of-world", "JPN", "R:iea_ev:rest-of-world", "RUS", "R:iea_ev:rest-of-world", "USA",
      "E:iea_etp:china", "CHN", "E:iea_etp:us", "USA",
      "E:iea_etp:eu", "DEU", "E:iea_etp:eu", "EST", "E:iea_etp:eu", "POL"
    ),
    items = items, item_links = item_links, mineral_demand = mineral_demand,
    eu_outside_coalition = "EST",
    item_units = item_units,
    roster_extra = dplyr::bind_rows(
      tibble::tibble(roster = c(rep("comtrade_matrix", length(econ)), rep("ei", 9)), geo = c(econ, setdiff(econ, "HKG"))),
      tibble::tibble(roster = "cmfdi_home", geo = sort(unique(fdi$projects$home)))
    ),
    facts = facts, world = world, series = series, trade = trade, eu_intra = eu_intra, policy = policy, fdi = fdi,
    dev = dev$part, dev_year = 2024L,
    # Per-fact notes (facts.notes): Japan's gas production as if from EIA, Estonia's oil balance assumed zero.
    fact_notes = tibble::tibble(
      var = c("production.fuels", "consumption.balance", "consumption.balance"),
      item_key = c("fuel:gas", "fuel:gas", "fuel:oil"), geo = c("JPN", "JPN", "EST"), year = 2025L,
      note = c("eia_production", "eia_production", "production_assumed_zero")
    ),
    reported = reported,
    licensed = tibble::tibble(item_key = c("cell:solar-downstream", "cell:wind-downstream"), family = "prices", sources = "BNEF LCOE"),
    headline_sources = sector_headline_sources()
  )
}

# The source line of each headline (manifest.headlines[].source).
sector_headline_sources <- function() {
  list(supply = "IEA Critical Minerals Dataset 2026", concentration = "Sector Insights summary (each sector's own source)",
       dependence = "UN Comtrade, reconciled matrix", investment = "Clean Investment Monitor (Rhodium Group and MIT CEEPR)",
       reserves = "USGS Mineral Commodity Summaries 2026", energy = "EI Statistical Review of World Energy 2026",
       policy = "IEA Policies and Measures", prices = "IMF Primary Commodity Prices")
}

# Placeholder values of the synthetic bundle (the CM-FDI counts from its synthetic projects).
sector_synthetic_fill <- function() {
  c(list(n_shared_codes = 1, n_codes = 7, n_rca_reporters = 8, critmin_year = 2024, pams_export_date = "2026-01-22",
         pams_year_max = 2030, trl_fallback_cells = "Geothermal Upstream", pct_cross_cutting = 16),
    sector_fdi_fill(sector_fdi_clean(sector_synthetic_cmfdi_raw(), sector_synthetic_iso3)),
    sector_dev_synthetic(sector_synthetic_items()[0, ])$fill)
}

# Country names of the synthetic CM-FDI rows -> ISO3 (the real export uses country_name_to_iso3()).
sector_synthetic_iso3 <- function(x) {
  map <- c(Brazil = "BRA", China = "CHN", Germany = "DEU", Estonia = "EST", `Hong Kong` = "HKG", Indonesia = "IDN",
           Japan = "JPN", Poland = "POL", Russia = "RUS", `United States` = "USA")
  unname(map[x])
}

# A CM-FDI extract in the CSV's own layout (every column text, as scripts/87 reads it): 14 projects
# covering the four technologies, every status, a non-manufacturing activity, undisclosed values, a
# joint venture, a Hong Kong parent, an expansion phase, a blank city, "N/A" and blank completion
# years, and source cells with dates, several links, "[UNVERIFIED]" and fDi Markets.
sector_synthetic_cmfdi_raw <- function() {
  p <- tibble::tribble(
    ~id, ~parent, ~hq, ~status, ~dest, ~city, ~ya, ~yc, ~tech, ~act, ~cap, ~exp, ~phase, ~jv, ~jvl, ~jvls, ~jvc, ~op,
    "EV1", "Toyota", "Japan", "Operational", "United States", "Liberty (North Carolina)", "2021", "2025", "Battery", "Manufacturing", "13900", "0", "", "0", "0", "", "", "Operational",
    "HAR0010", "CATL", "China", "Completed", "Germany", "Arnstadt (Thuringia)", "2018", "2022", "Battery", "Manufacturing", "1800", "0", "", "0", "0", "", "", "Operational",
    "HAR0011", "Gotion", "China", "Under Construction", "Indonesia", "Karawang (West Java)", "2024", "N/A", "Battery", "Manufacturing", "", "0", "All", "1", "1", "1", "Indonesia Battery Corporation (IBC)", "Under construction",
    "HKB1", "HK Battery Co", "Hong Kong", "Closed", "Poland", "Wroclaw", "2016", "2017", "Battery", "Manufacturing", "40", "0", "", "0", "0", "", "", "Ceased operations",
    "RUB1", "Rosatom", "Russia", "Announced", "Germany", "", "2025", "", "Battery", "Manufacturing", "", "1", "2", "0", "0", "", "", "Announced only",
    "SOL1", "LONGi", "China", "Announced", "United States", "Pataskala (Ohio)", "2024", "2027", "Solar", "Manufacturing", "600", "0", "", "1", "0", "", "Invenergy", "Announced only",
    "SOL2", "Sharp", "Japan", "Cancelled", "Poland", "Gdansk", "2019", "", "Solar", "Manufacturing", "120.5", "0", "", "0", "0", "", "", "Announced only",
    "SOL3", "Siemens", "Germany", "Operational", "Brazil", "Jundiai (Sao Paulo)", "2015", "2016", "Solar", "Research & Development", "15", "0", "", "0", "0", "", "", "Operational",
    "WND1", "Nordex", "Germany", "Completed", "Brazil", "Camaçari (Bahia)", "2012", "2013", "Wind", "Manufacturing", "80", "0", "", "0", "0", "", "", "Ceased operations",
    "WND2", "Goldwind", "China", "Paused", "Brazil", "Camaçari (Bahia)", "2023", "N/A", "Wind", "Manufacturing", "200", "0", "", "0", "0", "", "", "Paused",
    "WND3", "Goldwind", "China", "Operational", "Brazil", "Camaçari (Bahia)", "2019", "2020", "Wind", "Manufacturing", "60", "1", "1", "0", "0", "", "", "Operational",
    "EVX", "BYD", "China", "Under Construction", "Brazil", "Camaçari (Bahia)", "2023", "2026", "EV", "Manufacturing", "620", "0", "", "1", "0", "", "Ford (site seller)", "Under construction",
    "EVY", "Toyota", "Japan", "Operational", "Indonesia", "Karawang (West Java)", "2013", "2015", "EV", "Manufacturing", "", "0", "", "0", "0", "", "", "Operational",
    "EVZ", "BMW", "Germany", "Completed", "United States", "Spartanburg (South Carolina)", "2010", "2011", "EV", "Logistics, Distribution & Transportation", "50", "0", "", "0", "0", "", "", "Operational"
  )
  src <- function(id) paste0("https://example.org/cmfdi/", tolower(id))
  amount <- ifelse(p$cap == "", "Investment value not publicly disclosed", src(p$id))
  amount[p$id == "SOL1"] <- "[UNVERIFIED] https://example.org/cmfdi/sol1, https://example.com/news/sol1-plant (US$600m incl. phase 2)"
  amount[p$id == "WND1"] <- "fDi Markets (FT)"
  out <- data.frame(
    Sector = p$tech, `Project ID` = p$id, `Parent company` = p$parent, `Company Country (HQ)` = p$hq, `Project Status` = p$status,
    `Destination Country` = p$dest, `Destination city` = p$city, `Year Announced` = p$ya, `Year Completed` = p$yc,
    Technology = p$tech, `Industry activity` = p$act, `Capital Investment (US$m)` = p$cap, Expansion = p$exp, Phase = p$phase,
    `Joint Venture` = p$jv, `Joint Venture Local` = p$jvl, `Joint Venture Local State` = p$jvls, `Joint Venture Company` = p$jvc,
    `Source (Parent company name and HQ)` = src(p$id), `Source (Project Status)` = src(p$id),
    `Source (Project location, country and city)` = ifelse(p$id == "WND1", "fDi Markets (FT)", src(p$id)),
    `Source (Amount)` = amount, `Source (Ownership share, where available)` = ifelse(p$jv == "1", src(p$id), ""),
    `Source (Date Announced [MM/DD/YYYY])` = paste0(src(p$id), " 03/23/", p$ya),
    `Source (Date Completed [MM/DD/YYYY])` = ifelse(p$yc %in% c("", "N/A"), "", paste0(src(p$id), " (year only)")),
    `Source (Industry Activity)` = src(p$id), `Source (Joint Venture)` = ifelse(p$jv == "1", paste0("[UNVERIFIED] ", src(p$id)), ""),
    Operational = p$op,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out
}
