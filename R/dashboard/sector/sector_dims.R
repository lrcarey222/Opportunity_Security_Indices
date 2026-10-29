# dims of the Sector Insights bundle (spec_sector.md 3.1, 3.4, 8.6): economies and aggregates,
# items and their links to cells. Pure.

# Fixed aggregates: the EU, the world, the IEA EV regions, the IEA ETP entities and the EI residual.
SECTOR_EV_REGIONS <- c("China" = "china", "European Union" = "european-union", "North America" = "north-america",
                       "Other Asia Pacific" = "other-asia-pacific", "Rest of World" = "rest-of-world")
# Group of each IEA EV region. A region whose members span groups is "na" (mixed: North America
# holds MEX, a partner; Other Asia Pacific and Rest of World hold core members, partners and the
# rest): it stays a holder (ranked as a region) but its share counts as not attributed in the group
# split (sector_concentration()). The EU region is core, as the EU bloc is (spec 3.7).
SECTOR_EV_REGION_GROUPS <- c("china" = "china", "european-union" = "core", "north-america" = "na",
                             "other-asia-pacific" = "na", "rest-of-world" = "na")
SECTOR_ETP_ENTITIES <- tibble::tribble(
  ~id, ~name, ~group, ~member_rule,
  "china", "China (IEA)", "china", "CHN",
  "viet-nam", "Viet Nam (IEA)", "partner", "VNM",
  "india", "India (IEA)", "partner", "IND",
  "eu", "European Union (IEA)", "core", "EU",
  "us", "United States (IEA)", "core", "USA"
)

# Mineral names in the sources -> slugs, labels and running text, in IEA clean-energy demand order.
sector_mineral_table <- function() {
  tibble::tribble(
    ~slug, ~label, ~text, ~pattern,
    "copper", "Copper", "copper", "^copper",
    "lithium", "Lithium", "lithium", "^lithium",
    "nickel", "Nickel", "nickel", "^nickel",
    "cobalt", "Cobalt", "cobalt", "^cobalt",
    "graphite", "Graphite", "graphite", "graphite",
    "rare-earths", "Rare earths", "rare earths", "rare earth|^magnet rare|^neodymium",
    "manganese", "Manganese", "manganese", "^manganese",
    "zinc", "Zinc", "zinc", "^zinc",
    "pgms", "Platinum-group metals", "platinum-group metals", "^pgm|platinum",
    "silicon", "Silicon", "silicon", "^silicon",
    "silver", "Silver", "silver", "^silver",
    "chromium", "Chromium", "chromium", "^chrom",
    "molybdenum", "Molybdenum", "molybdenum", "^molybd"
  )
}

# The slug of a mineral name ("Rare Earths", "Magnet rare earth elements - Mining", "PGMs"); NA if none.
sector_mineral_slug <- function(x) {
  t <- sector_mineral_table()
  lx <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(lx))
  for (i in seq_len(nrow(t))) out[is.na(out) & grepl(t$pattern[i], lx)] <- t$slug[i]
  out
}

# Fuel items (EI and WEO).
sector_fuel_table <- function() {
  tibble::tribble(
    ~slug, ~label, ~text,
    "oil", "Oil", "oil",
    "gas", "Natural gas", "gas",
    "coal", "Coal", "coal",
    "nuclear", "Nuclear", "nuclear",
    "solar", "Solar", "solar",
    "wind", "Wind", "wind",
    "hydro", "Hydro", "hydro",
    "fossil", "Fossil fuels (oil, gas and coal)", "fossil fuels",
    "hydrogen", "Low-emissions hydrogen", "low-emissions hydrogen"
  )
}

# IMF commodity sub-sector -> item (slug, label, running text).
sector_commodity_items <- function(sub_sectors) {
  lab <- c(Oil_Brent = "Crude oil, Brent", Oil_WTI = "Crude oil, WTI", Dubai_Crude = "Crude oil, Dubai",
           Oil_APSP = "Crude oil, average spot", Natural_Gas_Henry_Hub = "Natural gas, Henry Hub",
           Natural_Gas_EU = "Natural gas, EU (TTF)", LNG = "LNG, Asia", Propane = "Propane", Coal = "Coal",
           Uranium = "Uranium", Copper = "Copper", Nickel = "Nickel", Cobalt = "Cobalt", Lithium = "Lithium",
           Manganese = "Manganese", Zinc = "Zinc", Molybdenum = "Molybdenum", Chromium = "Chromium", Silicon = "Silicon",
           Rare_Earths = "Rare earths", Vanadium = "Vanadium", Iron_Ore = "Iron ore", Diammonium_Phosphate = "Diammonium phosphate")
  ss <- sort(unique(sub_sectors))
  label <- ifelse(ss %in% names(lab), lab[ss], gsub("_", " ", ss))
  tibble::tibble(sub_sector = ss, slug = gsub("_", "-", tolower(ss)), label = unname(label),
                 text = ifelse(grepl("^Crude oil", label), sub("^Crude oil, (.*)$", "\\1 crude", label), sector_lower_first(label)))
}

# dims.geo. country_info: wdi_country_info.csv (iso3c, country, region, income, longitude, latitude);
# coalition: coalition_tbl; ideal: geo_ideal_points_tbl; hubs: ISO3 re-export hubs; extra: (code,
# name) of economies in the data but not in WDI (e.g. TWN). Countries ordered by code, then EUU,
# WLD, regions, entities, residuals.
sector_geo <- function(country_info, coalition, ideal, hubs, extra = NULL) {
  ci <- country_info[!is.na(country_info$region) & country_info$region != "Aggregates" &
                       grepl("^[A-Z]{3}$", country_info$iso3c), , drop = FALSE]
  codes <- sort(unique(c(ci$iso3c, coalition$iso3, if (!is.null(extra)) extra$code)))
  codes <- codes[grepl("^[A-Z]{3}$", codes) & !codes %in% c("EUU", "WLD")]
  nm <- ci$country[match(codes, ci$iso3c)]
  nm[is.na(nm)] <- coalition$country[match(codes[is.na(nm)], coalition$iso3)]
  if (!is.null(extra)) nm[is.na(nm)] <- extra$name[match(codes[is.na(nm)], extra$code)]
  nm[is.na(nm)] <- codes[is.na(nm)]
  tier <- coalition$tier[match(codes, coalition$iso3)]
  tier[is.na(tier) | !tier %in% c("core", "partner")] <- "none"
  group <- ifelse(codes == "CHN", "china", ifelse(tier == "core", "core", ifelse(tier == "partner", "partner", "rest")))
  eu <- coalition$eu_member[match(codes, coalition$iso3)] %in% TRUE
  ii <- match(codes, ideal$iso3)
  n3 <- suppressWarnings(countrycode::countrycode(codes, "iso3c", "iso3n", warn = FALSE))
  countries <- tibble::tibble(
    code = codes, name = nm, kind = "country", iso_n3 = as.integer(n3), tier = tier, group = group,
    eu_member = eu, re_export_hub = codes %in% hubs, bloc = ideal$bloc[ii], s_axis = as.double(ideal$s_axis[ii]),
    alignment_basis = ideal$alignment_basis[ii], wb_region = ci$region[match(codes, ci$iso3c)],
    income = ci$income[match(codes, ci$iso3c)],
    lon = suppressWarnings(as.double(ci$longitude[match(codes, ci$iso3c)])),
    lat = suppressWarnings(as.double(ci$latitude[match(codes, ci$iso3c)])),
    reason = coalition$reason[match(codes, coalition$iso3)]
  )
  countries$reason[countries$tier == "none"] <- NA_character_
  agg <- dplyr::bind_rows(
    tibble::tibble(code = "EUU", name = "European Union (27)", kind = "eu", tier = "core", group = "core"),
    tibble::tibble(code = "WLD", name = "World", kind = "world", tier = "none", group = "na"),
    tibble::tibble(code = paste0("R:iea_ev:", SECTOR_EV_REGIONS), name = paste0(names(SECTOR_EV_REGIONS), " (IEA EV region)"),
                   kind = "region", tier = "none", group = unname(SECTOR_EV_REGION_GROUPS[SECTOR_EV_REGIONS])),
    tibble::tibble(code = paste0("E:iea_etp:", SECTOR_ETP_ENTITIES$id), name = SECTOR_ETP_ENTITIES$name, kind = "entity",
                   tier = "none", group = SECTOR_ETP_ENTITIES$group),
    tibble::tibble(code = "RES:ei", name = "Other economies (EI)", kind = "residual", tier = "none", group = "na")
  )
  agg$iso_n3 <- NA_integer_
  agg$eu_member <- FALSE
  agg$re_export_hub <- FALSE
  for (k in c("bloc", "alignment_basis", "wb_region", "income", "reason")) agg[[k]] <- NA_character_
  for (k in c("s_axis", "lon", "lat")) agg[[k]] <- NA_real_
  dplyr::bind_rows(countries, agg[names(countries)])
}

# dims.region_members: EUU and the IEA ETP EU entity -> the EU members; the IEA EV regions -> the
# economies the processed EV table assigns to them (region_of: named vector iso3 -> region name);
# the other IEA ETP entities -> their country.
sector_region_members <- function(geo, region_of) {
  eu <- geo$code[geo$kind == "country" & geo$eu_member]
  ev <- tibble::tibble(geo = paste0("R:iea_ev:", unname(SECTOR_EV_REGIONS[region_of])), member = names(region_of))
  ent <- dplyr::bind_rows(lapply(seq_len(nrow(SECTOR_ETP_ENTITIES)), function(i) {
    m <- if (SECTOR_ETP_ENTITIES$member_rule[i] == "EU") eu else SECTOR_ETP_ENTITIES$member_rule[i]
    tibble::tibble(geo = paste0("E:iea_etp:", SECTOR_ETP_ENTITIES$id[i]), member = m)
  }))
  out <- dplyr::bind_rows(tibble::tibble(geo = "EUU", member = eu), ev, ent)
  out <- out[out$member %in% geo$code & out$geo %in% geo$code, , drop = FALSE]
  out[!duplicated(out), , drop = FALSE]
}

# The stages a mineral is shown at, and the stage of each critical-mineral trade basket
# (critical_minerals_trade_flows_tbl: ores and concentrates at Extraction, refined metal and chemicals
# at Refining, scrap at Recovery). Mineral items link to the Extraction and Refining cells of the
# technologies that use them (mining and reserves are shown at Extraction, refining at Refining, by the
# measures' link_stage); a mineral_stage item links to the cells of its own stage.
SECTOR_MINERAL_STAGES <- c("Extraction", "Refining")
SECTOR_CRITMIN_STAGES <- c("Extraction", "Refining", "Recovery")

# mineral_stage items (dims.items axis mineral_stage, schema 2) of the (mineral slug, stage) pairs in
# `pairs` (tibble(mineral, stage)): slug "<mineral>-<stage slug>", label "Lithium — Refining", running
# text "lithium (refining)"; stage = the stage label, tech NA. Mineral then stage order.
sector_mineral_stage_items <- function(pairs) {
  mt <- sector_mineral_table()
  p <- unique(tibble::tibble(mineral = as.character(pairs$mineral), stage = as.character(pairs$stage)))
  bad <- !p$mineral %in% mt$slug | !p$stage %in% SUPPLY_CHAIN_STAGES
  if (any(bad)) stop("sector_mineral_stage_items(): unknown minerals or stages ", paste(utils::head(paste(p$mineral, p$stage)[bad], 5), collapse = ", "))
  p <- p[order(match(p$mineral, mt$slug), match(p$stage, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
  m <- match(p$mineral, mt$slug)
  tibble::tibble(axis = "mineral_stage", slug = paste0(p$mineral, "-", tolower(p$stage)),
                 label = paste0(mt$label[m], " — ", p$stage), text = paste0(mt$text[m], " (", tolower(p$stage), ")"),
                 tech = NA_character_, stage = p$stage, headline = FALSE, has_trade = FALSE, sort = seq_len(nrow(p)))
}

# dims.items: the cells (has_trade for the basket cells: sector_cell_items()), the minerals / fuels /
# commodities present in the data, the mineral x stage items of the critical-mineral trade, activities,
# policy tags and none. cell_slugs: the dictionary's cells (NULL: the 66 headline cells and the
# Semiconductors trade cells); mineral_stages: tibble(mineral, stage) or NULL.
sector_items <- function(trade_cells, minerals, fuels, commodities, cell_slugs = NULL, mineral_stages = NULL) {
  cells <- sector_cell_items(trade_cells, cell_slugs)
  mt <- sector_mineral_table()
  mt <- mt[mt$slug %in% minerals, , drop = FALSE]
  ft <- sector_fuel_table()
  ft <- ft[ft$slug %in% fuels, , drop = FALSE]
  other <- dplyr::bind_rows(
    tibble::tibble(axis = "mineral", slug = mt$slug, label = mt$label, text = mt$text),
    tibble::tibble(axis = "fuel", slug = ft$slug, label = ft$label, text = ft$text),
    tibble::tibble(axis = "commodity", slug = commodities$slug, label = commodities$label, text = commodities$text)
  )
  other$tech <- NA_character_
  other$stage <- NA_character_
  other$headline <- FALSE
  other$has_trade <- FALSE
  other$sort <- stats::ave(seq_len(nrow(other)), other$axis, FUN = seq_along)
  ms <- if (is.null(mineral_stages) || !nrow(mineral_stages)) NULL else sector_mineral_stage_items(mineral_stages)
  dplyr::bind_rows(cells, other[names(cells)], if (!is.null(ms)) ms[names(cells)], sector_fixed_items())
}

# The fuel items -> cells: production and reserves at Extraction, use and generation at Deployment.
SECTOR_FUEL_LINKS <- tibble::tribble(
  ~item_key, ~cell,
  "fuel:oil", "oil-extraction", "fuel:oil", "oil-deployment",
  "fuel:gas", "gas-extraction", "fuel:gas", "gas-deployment",
  "fuel:coal", "coal-extraction", "fuel:coal", "coal-deployment",
  "fuel:nuclear", "nuclear-deployment",
  "fuel:solar", "solar-deployment",
  "fuel:wind", "wind-deployment",
  "fuel:hydrogen", "green-hydrogen-deployment"
)

# dims.item_links. mineral_links: coalition_mineral_links() (mineral, tech, mineral_demand_share);
# price_techs: distinct (sub_sector, tech, supply_chain) of the processed prices (each commodity is
# filed at the stage it prices: crude oil, gas, coal and ores at Extraction, metals and chemicals at
# Refining); cells: the cell items; mineral_stage_items: the mineral_stage items (NULL = none), linked
# with the mineral's demand share to the cells of their stage.
sector_item_links <- function(mineral_links, price_techs, commodities, cells, mineral_stage_items = NULL) {
  slug <- sector_mineral_slug(mineral_links$mineral)
  ml <- dplyr::bind_rows(lapply(SECTOR_MINERAL_STAGES, function(s) {
    tibble::tibble(item_key = paste0("mineral:", slug), cell = sector_cell_slug(mineral_links$tech, s),
                   weight = as.double(mineral_links$mineral_demand_share))
  }))
  ml <- ml[ml$item_key != "mineral:NA", , drop = FALSE]
  msl <- NULL
  if (!is.null(mineral_stage_items) && nrow(mineral_stage_items)) {
    ms <- mineral_stage_items
    base <- substr(ms$slug, 1, nchar(ms$slug) - nchar(ms$stage) - 1L)
    msl <- dplyr::bind_rows(lapply(seq_len(nrow(ms)), function(i) {
      hit <- !is.na(slug) & slug == base[i]
      tibble::tibble(item_key = rep(paste0("mineral_stage:", ms$slug[i]), sum(hit)), cell = sector_cell_slug(mineral_links$tech[hit], ms$stage[i]),
                     weight = as.double(mineral_links$mineral_demand_share[hit]))
    }))
  }
  fuel <- SECTOR_FUEL_LINKS
  fuel$weight <- 1
  if (!"supply_chain" %in% names(price_techs)) stop("sector_item_links(): price_techs needs the processed prices' supply_chain (the stage each commodity is filed at)")
  pt <- price_techs[!is.na(price_techs$tech), , drop = FALSE]
  cm <- tibble::tibble(item_key = paste0("commodity:", commodities$slug[match(pt$sub_sector, commodities$sub_sector)]),
                       cell = sector_cell_slug(pt$tech, pt$supply_chain), weight = 1)
  out <- dplyr::bind_rows(ml, msl, fuel, cm, sector_fixed_links(cells))
  out <- out[out$cell %in% cells$slug, , drop = FALSE]
  out[!duplicated(out[c("item_key", "cell")]), , drop = FALSE]
}
