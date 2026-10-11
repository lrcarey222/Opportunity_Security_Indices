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

# dims.items: the 34 cells (has_trade for the basket cells), the minerals / fuels / commodities
# present in the data, activities, policy tags and none.
sector_items <- function(trade_cells, minerals, fuels, commodities) {
  cells <- sector_cell_items(trade_cells)
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
  dplyr::bind_rows(cells, other[names(cells)], sector_fixed_items())
}

# dims.item_links. mineral_links: coalition_mineral_links() (mineral, tech, mineral_demand_share);
# price_techs: distinct (sub_sector, tech) of the processed prices; cells: the cell items.
sector_item_links <- function(mineral_links, price_techs, commodities, cells) {
  ml <- tibble::tibble(item_key = paste0("mineral:", sector_mineral_slug(mineral_links$mineral)),
                       cell = sector_cell_slug(mineral_links$tech, "Upstream"),
                       weight = as.double(mineral_links$mineral_demand_share))
  ml <- ml[ml$item_key != "mineral:NA", , drop = FALSE]
  fuel <- tibble::tribble(
    ~item_key, ~cell,
    "fuel:oil", "oil-upstream", "fuel:oil", "oil-downstream",
    "fuel:gas", "gas-upstream", "fuel:gas", "gas-downstream",
    "fuel:coal", "coal-upstream", "fuel:coal", "coal-downstream",
    "fuel:nuclear", "nuclear-downstream",
    "fuel:solar", "solar-downstream",
    "fuel:wind", "wind-downstream",
    "fuel:hydrogen", "green-hydrogen-downstream"
  )
  fuel$weight <- 1
  pt <- price_techs[!is.na(price_techs$tech), , drop = FALSE]
  cm <- tibble::tibble(item_key = paste0("commodity:", commodities$slug[match(pt$sub_sector, commodities$sub_sector)]),
                       cell = sector_cell_slug(pt$tech, "Upstream"), weight = 1)
  out <- dplyr::bind_rows(ml, fuel, cm, sector_fixed_links(cells))
  out <- out[out$cell %in% cells$slug, , drop = FALSE]
  out[!duplicated(out[c("item_key", "cell")]), , drop = FALSE]
}
