# Two-tier allied coalition and the EU network node.
#
# Built by scripts/13_build_coalition.R (after scripts/12, before scripts/20); settings in
# config/coalition.yml. Decisions of Oct 2026:
#
# Tiers. A country's tier is the highest of the routes it satisfies; every route it satisfies is
# recorded, not only the one that sets the tier.
# - core: the U.S.-leaning bloc of the UN General Assembly alignment (scripts/12,
#   geo_ideal_points_tbl.rds) with latest WDI GDP >= gdp_floor_usd (USD 100bn) and its own UN
#   votes: the alignment coalition of geo_coalition_tbl.rds, re-derived here so both tiers come
#   from one function.
# - partner: not core, and
#   (a) route "agreement": a member of a U.S. or allied economic-security agreement
#       (data/reference/economic_security_agreements.csv: U.S. FTA partners, IPEF, MSP partners,
#       G7, NATO; MSP Forum members only as a sensitivity) with GDP >= gdp_floor_usd, whatever
#       its bloc; or
#   (b) route "supplier": not China-leaning and a major supplier, i.e. a world share of at least
#       supplier_share (5%) of production, refining, manufacturing capacity or exports in at
#       least one measure of a supply stage (Extraction, Refining, Components, Assembly or
#       Recovery: SUPPLY_CHAIN_SUPPLY_STAGES, every stage but Deployment) of a headline
#       technology (major_supplier_shares()). No GDP floor, so small resource suppliers qualify.
#   (b') route "supplier: reserves" (user decision, 2026-10-09): not China-leaning and holding at
#       least supplier_reserves_share (10%) of world reserves of a headline mineral (Extraction of
#       the technologies that use it), of oil, gas or coal (their own Extraction) or of uranium
#       (Nuclear Extraction; Red Book identified resources < USD 260/kgU, the Reserves theme's
#       measure). Reserves are a stock, not supply (a holder supplies only once it mines), hence
#       a bar twice that of the flow measures. It is the same partner route as (b), with its own
#       threshold (coalition_supplier_threshold()), and is recorded separately. The sources are
#       the Reserves theme's (scripts/10): USGS Mineral Commodity Summaries for the minerals
#       (measure usgs_reserves), the EI Statistical Review for the fuels (ei_reserves) and the Red
#       Book for uranium (uranium_resources).
# - none: no route.
#
# Stages (since 2026-10-27; docs/supply_chain_stages.md). The supplier measures carry the six-stage
# supply_chain of R/utils/supply_chain_stages.R, each staged as the themes stage the same data:
# mine production (IEA mining, EI minerals, Red Book uranium), fuel production (EI oil, gas, coal)
# and every reserves measure are Extraction; IEA refining is Refining
# (iea_critical_minerals_supply_stage()); IEA manufacturing capacity is Components for batteries
# (cell capacity) and Assembly for solar, wind and electrolysers (COALITION_CLEANTECH_STAGES, as the
# Foreign Dependency theme stages it); the export shares are the trade table's own cells, each HS6
# code in its master stage. Before, the trade test read one Upstream and one Midstream basket per
# technology; split by stage, the baskets are smaller, and a country can now clear supplier_share
# in one of them (an Extraction basket of one or two ore codes, say) without having cleared it in
# the whole; the thresholds are unchanged.
#
# EU node. The 27 EU member states share one node of the allied network (eu_code, "EUU", the
# World Bank / WDI code of the European Union), whatever their own tier; the node's tier is set
# in config (core: all 27 members are U.S.-leaning on the 2026-10-09 run). eu_stage_weights()
# gives each member's weight within the node per (tech, stage) for stage-weighted aggregation.
# Only the network layer is bundled: the national indices stay national, and coalition_tbl keeps
# each member's own tier.
#
# Pure functions (no IO). They use R/utils/supply_chain_stages.R (SUPPLY_CHAIN_SUPPLY_STAGES,
# assert_supply_chain_stages), R/utils/country.R (country_name_to_iso3,
# standardize_country_names, is_aggregate_or_residual), R/utils/iea_critical_minerals.R
# (harmonize_mineral_names, iea_critical_minerals_supply_stage, iea_critical_minerals_base_year,
# iea_critical_minerals_year_col) and, for the EI mineral and reserves sheets and the USGS
# reserves, R/categories/production/critical_minerals_production.R
# (critical_minerals_production_resolve_val_col), R/categories/reserves/reserves.R
# (reserves_resolve_val_col) and R/categories/reserves/usgs_mcs.R (usgs_mcs_world_reserves),
# which the caller sources.

COALITION_TIERS <- c("core", "partner", "none")
# Route labels of coalition_tbl$routes. "supplier" is the supplier route through a production,
# refining, manufacturing or export measure, "supplier: reserves" through a reserves measure;
# an economy that passes both records both.
COALITION_ROUTES <- c("core", "agreement", "supplier", "supplier: reserves")
# Measures of major_supplier_shares() that are stocks (reserves, identified resources) rather
# than flows. They are tested at supplier_reserves_share instead of supplier_share.
COALITION_RESERVES_MEASURES <- c("usgs_reserves", "ei_reserves", "uranium_resources")
# Stage of the IEA manufacturing capacity of each technology (coalition_shares_cleantech()), the
# primary stage the Foreign Dependency theme gives the same data (foreign_dependency_cleantech_stage()):
# the IEA measures cell capacity for batteries, a component of the pack, and the end product
# (modules, nacelles, electrolysers) for the others. The theme also copies each figure onto the
# other manufacturing stage for scoring; the supplier test counts it once, here. Spelled out, not
# taken from R/utils/supply_chain_stages.R, so this file still sources on its own.
COALITION_CLEANTECH_STAGES <- c(
  Solar = "Assembly", Wind = "Assembly", Batteries = "Components", `Green Hydrogen` = "Assembly"
)

# Columns of a major_supplier_shares() table.
COALITION_SUPPLIER_COLUMNS <- c(
  "iso3", "country", "tech", "supply_chain", "measure", "mineral", "measure_label",
  "mineral_demand_share", "year", "value", "unit", "world_share", "is_aggregate", "source"
)

coalition_require_columns <- function(tbl, columns, label) {
  if (!inherits(tbl, "data.frame")) {
    stop(label, " must be a data.frame.")
  }
  missing_cols <- setdiff(columns, names(tbl))
  if (length(missing_cols) > 0) {
    stop(label, " lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  invisible(tbl)
}

coalition_clean_codes <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA")] <- NA_character_
  x
}

coalition_check_share <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x > 1) {
    stop(name, " must be a single number in (0, 1].")
  }
  invisible(x)
}

# World-share threshold of each supplier-table row: supplier_reserves_share for the reserves
# measures (COALITION_RESERVES_MEASURES), supplier_share for the production, refining,
# manufacturing and export measures. major_supplier_shares() (its `major` flag) and
# derive_coalition_tiers() (the supplier route) both apply this, so they agree row by row.
coalition_supplier_threshold <- function(measure, supplier_share = 0.05, supplier_reserves_share = 0.10) {
  coalition_check_share(supplier_share, "supplier_share")
  coalition_check_share(supplier_reserves_share, "supplier_reserves_share")
  ifelse(as.character(measure) %in% COALITION_RESERVES_MEASURES, supplier_reserves_share, supplier_share)
}

# One supplier-share block in the common layout; scalar arguments are recycled.
coalition_supplier_rows <- function(iso3, country, tech, supply_chain, measure, measure_label,
                                    year, value, unit, world_share, source,
                                    mineral = NA_character_, mineral_demand_share = NA_real_,
                                    is_aggregate = FALSE) {
  n <- length(iso3)
  if (n == 0) {
    return(coalition_empty_supplier_tbl())
  }
  tibble::tibble(
    iso3 = as.character(iso3),
    country = rep_len(as.character(country), n),
    tech = rep_len(as.character(tech), n),
    supply_chain = rep_len(as.character(supply_chain), n),
    measure = rep_len(as.character(measure), n),
    mineral = rep_len(as.character(mineral), n),
    measure_label = rep_len(as.character(measure_label), n),
    mineral_demand_share = rep_len(as.numeric(mineral_demand_share), n),
    year = rep_len(as.integer(year), n),
    value = rep_len(as.numeric(value), n),
    unit = rep_len(as.character(unit), n),
    world_share = rep_len(as.numeric(world_share), n),
    is_aggregate = rep_len(as.logical(is_aggregate), n),
    source = rep_len(as.character(source), n)
  )
}

coalition_empty_supplier_tbl <- function() {
  tibble::tibble(
    iso3 = character(), country = character(), tech = character(), supply_chain = character(),
    measure = character(), mineral = character(), measure_label = character(),
    mineral_demand_share = numeric(), year = integer(), value = numeric(), unit = character(),
    world_share = numeric(), is_aggregate = logical(), source = character()
  )
}

## Major-supplier shares ----------------------------------------------------

# Mineral -> technology links of the IEA demand sheet ("3.1", via
# reserves_build_mineral_demand_clean(): Mineral, tech, share_24): the technologies of `techs`
# that take at least `min_demand_share` of the mineral's clean-energy demand in the base year.
# One row per (mineral, tech): mineral (harmonize_mineral_names()), tech, mineral_demand_share.
coalition_mineral_links <- function(mineral_demand, techs, min_demand_share = 0.01) {
  coalition_require_columns(mineral_demand, c("Mineral", "tech", "share_24"), "mineral_demand")
  links <- tibble::tibble(
    mineral = harmonize_mineral_names(mineral_demand$Mineral),
    tech = as.character(mineral_demand$tech),
    mineral_demand_share = suppressWarnings(as.numeric(mineral_demand$share_24))
  )
  links <- links[!is.na(links$tech) & links$tech %in% techs &
                   !is.na(links$mineral_demand_share) &
                   links$mineral_demand_share >= min_demand_share, , drop = FALSE]
  dplyr::summarise(
    dplyr::group_by(links, .data$mineral, .data$tech),
    mineral_demand_share = max(.data$mineral_demand_share),
    .groups = "drop"
  )
}

# IEA Critical Minerals Dataset, sheet "2 Total supply for key minerals" (as flattened by
# read_iea_critical_minerals()): each country's share of world mining (Extraction) or refining
# (Refining; iea_critical_minerals_supply_stage(), as the mineral themes stage it) of a mineral in
# the release's base year, mapped to the technologies that use the mineral.
#
# The share is the country's supply over the block's country rows plus "Rest of world" (the
# denominator critical_minerals_processing uses); "Total" and "Top 3 share" rows are dropped.
# A mineral maps to a technology when the IEA demand sheet ("3.1", via
# reserves_build_mineral_demand_clean(): Mineral, tech, share_24) gives the technology at least
# `min_demand_share` of the mineral's clean-energy demand in the base year. The floor drops
# links such as copper to hydrogen (7e-7 of copper demand), which would otherwise make every
# large copper miner a hydrogen supplier. mineral_demand_share records the link.
coalition_shares_iea_minerals <- function(critical,
                                          mineral_demand,
                                          techs,
                                          min_demand_share = 0.01,
                                          year = NULL) {
  coalition_require_columns(critical, c("Pillar", "Mineral", "Sector.Country"), "critical")
  coalition_require_columns(mineral_demand, c("Mineral", "tech", "share_24"), "mineral_demand")
  if (is.null(year)) year <- iea_critical_minerals_base_year(critical)
  value_col <- iea_critical_minerals_year_col(critical, year, label = "IEA mineral supply")
  year <- as.integer(sub("^X", "", value_col))

  supply <- critical[grepl("Total supply", critical$Pillar), , drop = FALSE]
  label <- trimws(as.character(supply$`Sector.Country`))
  block <- trimws(as.character(supply$Mineral))
  keep <- nzchar(label) & nzchar(block) & !grepl("^(top 3|total)", label, ignore.case = TRUE)
  supply <- supply[keep, , drop = FALSE]
  label <- label[keep]
  block <- block[keep]
  if (nrow(supply) == 0) {
    stop("No rows of the IEA supply sheet ('Total supply for key minerals') in `critical`.")
  }
  value <- suppressWarnings(as.numeric(supply[[value_col]]))
  denominator <- stats::ave(ifelse(is.na(value), 0, value), block, FUN = sum)

  mineral_raw <- trimws(sub(" - .*$", "", block))
  stage_raw <- ifelse(grepl(" - ", block), trimws(sub("^[^-]* - ", "", block)), "")
  residual <- is_aggregate_or_residual(label)
  iso3 <- country_name_to_iso3(label)
  unmatched <- !residual & is.na(iso3)
  if (any(unmatched)) {
    stop("IEA supply rows with no ISO3 code: ", paste(unique(label[unmatched]), collapse = ", "))
  }

  rows <- tibble::tibble(
    iso3 = iso3,
    country = label,
    mineral = harmonize_mineral_names(mineral_raw),
    supply_chain = iea_critical_minerals_supply_stage(stage_raw, legacy = FALSE),
    block = block,
    value = value,
    world_share = ifelse(denominator > 0, value / denominator, NA_real_)
  )[!residual, , drop = FALSE]

  links <- coalition_mineral_links(mineral_demand, techs = techs, min_demand_share = min_demand_share)

  out <- dplyr::inner_join(rows, links, by = "mineral", relationship = "many-to-many")
  if (nrow(out) == 0) {
    return(coalition_empty_supplier_tbl())
  }
  coalition_supplier_rows(
    iso3 = out$iso3,
    country = out$country,
    tech = out$tech,
    supply_chain = out$supply_chain,
    measure = ifelse(out$supply_chain == "Extraction", "iea_mineral_mining", "iea_mineral_refining"),
    measure_label = paste0("IEA Critical Minerals Dataset: ", out$block),
    year = year,
    value = out$value,
    unit = "kt",
    world_share = out$world_share,
    source = "IEA Critical Minerals Dataset (Total supply for key minerals)",
    mineral = out$mineral,
    mineral_demand_share = out$mineral_demand_share
  )
}

# Energy Institute Statistical Review mine production (ei_stat_review_world_energy_wide.xlsx, the
# "P-R" sheets of critical_minerals_production_specs()): each country's share of world mining of a
# mineral, Extraction of the technologies that use it (coalition_mineral_links()). It covers the
# minerals the IEA supply sheet leaves out: the IEA lists copper, cobalt, lithium, nickel,
# graphite and magnet rare earths only, while its demand sheet also maps manganese (Wind, Electric
# Vehicles, Batteries), zinc (Wind, Solar) and PGMs (Green Hydrogen) to headline technologies.
# Without these sheets the largest manganese and platinum-group miners (South Africa, Gabon,
# Ghana) could not pass the supplier test. Minerals in `exclude_minerals` (those the IEA sheet
# already covers at the mining stage) are skipped, so no mineral is tested twice.
#
# `sheets`: a list with one element per sheet, list(sheet = name, mineral = the mineral (the
# spec's tech_name), nm_col = label column, data = the sheet as readxl reads it with the spec's
# skip). The share is the country's value over the sheet's "Total World" row, in the sheet's latest
# year column (critical_minerals_production_resolve_val_col()), or `year`. "Rest of World",
# footnotes and rows without a value are dropped.
coalition_shares_ei_minerals <- function(sheets,
                                         mineral_demand,
                                         techs,
                                         min_demand_share = 0.01,
                                         exclude_minerals = character(),
                                         year = NULL) {
  if (!is.list(sheets)) stop("sheets must be a list of list(sheet, mineral, nm_col, data).")
  links <- coalition_mineral_links(mineral_demand, techs = techs, min_demand_share = min_demand_share)
  exclude_minerals <- harmonize_mineral_names(exclude_minerals)
  out <- list(coalition_empty_supplier_tbl())
  for (sh in sheets) {
    mineral <- harmonize_mineral_names(sh$mineral)
    if (length(mineral) != 1 || is.na(mineral)) stop("An EI mineral sheet has no mineral name.")
    if (mineral %in% exclude_minerals || !mineral %in% links$mineral) next
    d <- sh$data
    if (!inherits(d, "data.frame") || ncol(d) < 2) stop("EI mineral sheet ", sh$sheet, " has no data.")
    nm_col <- if (!is.null(sh$nm_col) && sh$nm_col %in% names(d)) sh$nm_col else names(d)[1]
    val_col <- critical_minerals_production_resolve_val_col(d, year = year, sheet_id = sh$sheet)
    yr <- as.integer(substr(val_col, 1, 4))
    label <- trimws(as.character(d[[nm_col]]))
    value <- suppressWarnings(as.numeric(d[[val_col]]))
    is_world <- !is.na(label) & grepl("^total world$", label, ignore.case = TRUE)
    world <- value[is_world]
    if (length(world) != 1 || is.na(world) || world <= 0) {
      stop("EI mineral sheet ", sh$sheet, " has no single positive 'Total World' value for ", yr, ".")
    }
    keep <- !is.na(label) & nzchar(label) & !is.na(value) & !is_world &
      !grepl("of which", label, ignore.case = TRUE) & !is_aggregate_or_residual(label)
    iso3 <- country_name_to_iso3(label[keep])
    if (anyNA(iso3)) {
      stop("EI mineral sheet ", sh$sheet, " rows with no ISO3 code: ", paste(label[keep][is.na(iso3)], collapse = ", "))
    }
    kept_value <- value[keep]
    rows <- tibble::tibble(iso3 = iso3, country = label[keep], value = kept_value, world_share = kept_value / world)
    l <- links[links$mineral == mineral, , drop = FALSE]
    for (k in seq_len(nrow(l))) {
      out[[length(out) + 1L]] <- coalition_supplier_rows(
        iso3 = rows$iso3,
        country = rows$country,
        tech = l$tech[k],
        supply_chain = "Extraction",
        measure = "ei_mineral_mining",
        measure_label = paste0("EI Statistical Review: ", sh$sheet, " (mine production)"),
        year = yr,
        value = rows$value,
        unit = nm_col,
        world_share = rows$world_share,
        source = "Energy Institute, Statistical Review of World Energy (mineral production; includes USGS data)",
        mineral = mineral,
        mineral_demand_share = l$mineral_demand_share[k]
      )
    }
  }
  dplyr::bind_rows(out)
}

# Uranium mine production (OECD-NEA/IAEA Red Book Table 1.18, data/reference/uranium_production.csv):
# Nuclear Extraction. The table lists every producer, so the rows of a year sum to the world total.
# `year` defaults to the latest year in the table.
coalition_shares_uranium <- function(uranium_production, year = NULL) {
  coalition_require_columns(
    uranium_production, c("country", "iso3c", "variable", "value", "year"), "uranium_production"
  )
  prod <- uranium_production[uranium_production$variable == "mine_production", , drop = FALSE]
  if (nrow(prod) == 0) {
    stop("uranium_production has no mine_production rows.")
  }
  if (is.null(year)) year <- max(as.integer(prod$year), na.rm = TRUE)
  prod <- prod[as.integer(prod$year) == as.integer(year), , drop = FALSE]
  if (nrow(prod) == 0) {
    stop("uranium_production has no mine_production rows for ", year, ".")
  }
  value <- as.numeric(prod$value)
  world <- sum(value, na.rm = TRUE)
  coalition_supplier_rows(
    iso3 = coalition_clean_codes(prod$iso3c),
    country = prod$country,
    tech = "Nuclear",
    supply_chain = "Extraction",
    measure = "uranium_mine_production",
    measure_label = "Uranium mine production (Red Book Table 1.18)",
    year = year,
    value = value,
    unit = if ("unit" %in% names(prod)) paste(unique(prod$unit), collapse = "/") else "tU",
    world_share = if (world > 0) value / world else NA_real_,
    source = "OECD-NEA/IAEA, Uranium: Resources, Production and Demand (Red Book), Table 1.18"
  )
}

# Energy Institute Statistical Review reserves (ei_stat_review_world_energy_wide.xlsx, the sheets
# of reserves_specs() that the Reserves theme reads in scripts/10): each country's share of world
# reserves, Extraction, for the reserves leg of the supplier route (tested at
# supplier_reserves_share). A fuel in `fuels` (proved reserves at end-2020, the last year EI
# published them) maps to its own technology; a mineral (reserves "At end of <year>", USGS data)
# to the technologies that use it, through the same link and floor as the mining measures
# (coalition_mineral_links()). Since the Reserves theme moved its minerals to USGS MCS
# (coalition_shares_usgs_reserves()), reserves_specs() lists the fuel sheets only, so scripts/13
# passes fuels here; a mineral sheet is still read if passed.
#
# `sheets`: one element per sheet, list(sheet, resource = the spec's tech_name, nm_col, val_col,
# data_year (optional; else the year in the value column's name), unit (optional), data = the
# sheet as readxl reads it with the spec's skip). The value column is resolved as the theme does
# (reserves_resolve_val_col()).
#
# The denominator is the sheet's own "Total World" row. It includes the "Rest of World" (minerals)
# and "Other <region>" (fuels) residuals, which the Reserves theme drops: shares of the theme's
# country rows (reserves_tbl.rds) would overstate every holder, most for copper, where the
# residual is 43% of the world (Chile 32% of the listed countries, 18% of the world). Regional and
# group totals, residuals and component rows written "<name>: <part>" ("Venezuela: Orinoco Belt",
# "Canadian Oil Sands: Total", "of which: OECD"; the parts are already in the country's total)
# are dropped. Country rows that sum to more than the world total mean a part or a group was read
# as a country, and stop.
coalition_shares_ei_reserves <- function(sheets,
                                         mineral_demand,
                                         techs,
                                         min_demand_share = 0.01,
                                         fuels = c("Oil", "Gas", "Coal")) {
  if (!is.list(sheets)) stop("sheets must be a list of list(sheet, resource, nm_col, val_col, data).")
  links <- coalition_mineral_links(mineral_demand, techs = techs, min_demand_share = min_demand_share)
  out <- list(coalition_empty_supplier_tbl())
  for (sh in sheets) {
    resource <- as.character(sh$resource)
    if (length(resource) != 1 || is.na(resource) || !nzchar(resource)) {
      stop("An EI reserves sheet has no resource name.")
    }
    if (resource %in% fuels) {
      if (!resource %in% techs) next
      l <- tibble::tibble(tech = resource, mineral = NA_character_, mineral_demand_share = NA_real_)
    } else {
      l <- links[links$mineral == harmonize_mineral_names(resource), , drop = FALSE]
      if (nrow(l) == 0) next
    }
    d <- sh$data
    if (!inherits(d, "data.frame") || ncol(d) < 2) stop("EI reserves sheet ", sh$sheet, " has no data.")
    nm_col <- if (!is.null(sh$nm_col) && sh$nm_col %in% names(d)) sh$nm_col else names(d)[1]
    val_col <- reserves_resolve_val_col(d, sh$val_col, sheet_id = sh$sheet)
    if (!val_col %in% names(d)) stop("EI reserves sheet ", sh$sheet, " has no column '", val_col, "'.")
    yr <- if (!is.null(sh$data_year)) {
      as.integer(sh$data_year)
    } else {
      suppressWarnings(as.integer(sub("^.*?((19|20)[0-9]{2}).*$", "\\1", val_col, perl = TRUE)))
    }

    # EI marks a footnote with a trailing digit ("Brazil1", which country_name_to_iso3() strips) or
    # symbol ("European Union#" on the oil sheet), which would hide an aggregate from
    # is_aggregate_or_residual(); the symbol goes first.
    label <- trimws(sub("[#*^]+$", "", trimws(as.character(d[[nm_col]]))))
    value <- suppressWarnings(as.numeric(d[[val_col]]))
    is_world <- !is.na(label) & grepl("^total world$", label, ignore.case = TRUE)
    world <- value[is_world]
    if (length(world) != 1 || is.na(world) || world <= 0) {
      stop("EI reserves sheet ", sh$sheet, " has no single positive 'Total World' value in '", val_col, "'.")
    }
    keep <- !is.na(label) & nzchar(label) & !is.na(value) & !is_world &
      !grepl(":", label, fixed = TRUE) & !is_aggregate_or_residual(label)
    iso3 <- country_name_to_iso3(label[keep])
    if (anyNA(iso3)) {
      stop("EI reserves sheet ", sh$sheet, " rows with no ISO3 code: ", paste(label[keep][is.na(iso3)], collapse = ", "))
    }
    if (anyDuplicated(iso3)) {
      stop("EI reserves sheet ", sh$sheet, " has two rows for: ", paste(unique(iso3[duplicated(iso3)]), collapse = ", "))
    }
    kept_value <- value[keep]
    if (sum(kept_value) > world * (1 + 1e-6)) {
      stop("EI reserves sheet ", sh$sheet, ": the country rows sum to ", format(sum(kept_value)),
           ", more than the Total World row (", format(world), ").")
    }
    country <- standardize_country_names(label[keep]) # "Brazil1" (a footnote marker) -> "Brazil"
    for (k in seq_len(nrow(l))) {
      out[[length(out) + 1L]] <- coalition_supplier_rows(
        iso3 = iso3,
        country = country,
        tech = l$tech[k],
        supply_chain = "Extraction",
        measure = "ei_reserves",
        # The year, not the column name: readxl names the fuel columns "2020...42" and coal's "Total".
        measure_label = paste0("EI Statistical Review: ", sh$sheet, " (reserves",
                               if (is.na(yr)) "" else paste0(" at end-", yr), ")"),
        year = yr,
        value = kept_value,
        unit = if (is.null(sh$unit)) nm_col else sh$unit,
        world_share = kept_value / world,
        source = "Energy Institute, Statistical Review of World Energy (reserves; minerals include USGS data)",
        mineral = l$mineral[k],
        mineral_demand_share = l$mineral_demand_share[k]
      )
    }
  }
  dplyr::bind_rows(out)
}

# USGS Mineral Commodity Summaries reserves of the critical minerals (usgs_mcs_reserves() on the MCS
# commodity table, the rows the Reserves theme reads in scripts/10): each country's share of world
# reserves, Extraction of the technologies that use the mineral (coalition_mineral_links(), the link
# and floor of the mining measures), measure "usgs_reserves", for the reserves leg of the supplier
# route (tested at supplier_reserves_share).
#
# The world is usgs_mcs_world_reserves(): the larger of USGS's rounded "World total" and the sum
# of the listed rows, "Other countries" included, so it holds the residual the theme drops (as the
# EI reader's "Total World" row does). Holders USGS does not quantify ("NA", "W", "s") get no row:
# they have no share to test. "Other countries" and "World total" are never holders.
coalition_shares_usgs_reserves <- function(usgs_reserves,
                                           mineral_demand,
                                           techs,
                                           min_demand_share = 0.01) {
  coalition_require_columns(
    usgs_reserves,
    c("tech", "Country", "ISO3166_alpha3", "row_type", "value", "unit", "basis", "data_year", "edition"),
    "usgs_reserves"
  )
  links <- coalition_mineral_links(mineral_demand, techs = techs, min_demand_share = min_demand_share)
  world <- usgs_mcs_world_reserves(usgs_reserves)

  holders <- usgs_reserves[usgs_reserves$row_type == "country" & !is.na(usgs_reserves$value), , drop = FALSE]
  rows <- data.frame(
    iso3 = coalition_clean_codes(holders$ISO3166_alpha3),
    country = as.character(holders$Country),
    resource = as.character(holders$tech),
    mineral = harmonize_mineral_names(holders$tech),
    value = as.numeric(holders$value),
    world = world$world[match(holders$tech, world$tech)],
    unit = as.character(holders$unit),
    basis = as.character(holders$basis),
    year = as.integer(holders$data_year),
    edition = as.integer(holders$edition),
    stringsAsFactors = FALSE
  )
  if (anyNA(rows$iso3)) {
    stop("usgs_reserves country rows without an ISO3 code: ", paste(unique(rows$country[is.na(rows$iso3)]), collapse = ", "))
  }
  if (anyNA(rows$world) || any(rows$world <= 0)) {
    stop("usgs_reserves: no positive world reserves for ", paste(unique(rows$resource[is.na(rows$world) | rows$world <= 0]), collapse = ", "))
  }

  out <- dplyr::inner_join(rows, links, by = "mineral", relationship = "many-to-many")
  if (nrow(out) == 0) {
    return(coalition_empty_supplier_tbl())
  }
  coalition_supplier_rows(
    iso3 = out$iso3,
    country = out$country,
    tech = out$tech,
    supply_chain = "Extraction",
    measure = "usgs_reserves",
    measure_label = paste0(
      "USGS Mineral Commodity Summaries ", out$edition, ": ", out$resource, " reserves at end-",
      out$year, " (", out$basis, ")"
    ),
    year = out$year,
    value = out$value,
    unit = out$unit,
    world_share = out$value / out$world,
    source = paste0("U.S. Geological Survey, Mineral Commodity Summaries ", out$edition,
                    " (World Mine Production and Reserves)"),
    mineral = out$mineral,
    mineral_demand_share = out$mineral_demand_share
  )
}

# Identified recoverable uranium resources (OECD-NEA/IAEA Red Book Table 1.2a,
# data/reference/uranium_resources.csv) in one cost category, `variable`: Nuclear Extraction, for
# the reserves leg of the supplier route. scripts/13 passes the Reserves theme's category
# (uranium_resources_variable(URANIUM_DEFAULT_COST_CATEGORY): RAR plus inferred, < USD 260/kgU),
# so the test reads the theme's "Nuclear Reserves". The table lists every country with identified
# resources and the CSV has no world row, so the world total is the sum of the listed countries
# (the table's own total). `year` defaults to the latest reference year.
coalition_shares_uranium_resources <- function(uranium_resources, variable, year = NULL) {
  coalition_require_columns(
    uranium_resources, c("country", "iso3c", "variable", "value", "year"), "uranium_resources"
  )
  if (!is.character(variable) || length(variable) != 1 || is.na(variable)) {
    stop("variable must be one uranium_resources variable, e.g. uranium_resources_variable().")
  }
  res <- uranium_resources[uranium_resources$variable == variable, , drop = FALSE]
  if (nrow(res) == 0) {
    stop("uranium_resources has no ", variable, " rows; available: ",
         paste(sort(unique(uranium_resources$variable)), collapse = ", "))
  }
  if (is.null(year)) year <- max(as.integer(res$year), na.rm = TRUE)
  res <- res[as.integer(res$year) == as.integer(year), , drop = FALSE]
  if (nrow(res) == 0) {
    stop("uranium_resources has no ", variable, " rows for ", year, ".")
  }
  iso3 <- coalition_clean_codes(res$iso3c)
  if (anyNA(iso3) || anyDuplicated(iso3)) {
    stop("uranium_resources has missing or duplicate ISO3 codes for ", variable, " in ", year, ".")
  }
  value <- as.numeric(res$value)
  world <- sum(value, na.rm = TRUE)
  coalition_supplier_rows(
    iso3 = iso3,
    country = res$country,
    tech = "Nuclear",
    supply_chain = "Extraction",
    measure = "uranium_resources",
    measure_label = paste0("Uranium identified recoverable resources (Red Book Table 1.2a): ", variable),
    year = year,
    value = value,
    unit = if ("unit" %in% names(res)) paste(unique(res$unit), collapse = "/") else "tU",
    world_share = if (world > 0) value / world else NA_real_,
    source = "OECD-NEA/IAEA, Uranium: Resources, Production and Demand (Red Book), Table 1.2a"
  )
}

# Energy Institute Statistical Review production (ei_stat_review_world_energy.csv, long: Country,
# Year, ISO3166_alpha3, Var, Value), as the production themes read it: oilprod_kbd, gasprod_ej and
# coalprod_ej, Extraction of Oil, Gas and Coal. Shares are of the "Total World" row (WLD) in each
# variable's latest year (or `year`). Regional and residual rows are dropped; the EU aggregate
# ("Total EU", T-EU) is kept as eu_code with is_aggregate = TRUE, for eu_stage_weights()'s coverage
# check.
coalition_shares_ei_production <- function(ei,
                                           vars = c(Oil = "oilprod_kbd", Gas = "gasprod_ej", Coal = "coalprod_ej"),
                                           year = NULL,
                                           eu_code = "EUU",
                                           world_code = "WLD",
                                           eu_aggregate_code = "T-EU") {
  coalition_require_columns(ei, c("Country", "Year", "ISO3166_alpha3", "Var", "Value"), "ei")
  out <- list()
  for (tech in names(vars)) {
    var <- vars[[tech]]
    sub <- ei[ei$Var == var & !is.na(suppressWarnings(as.numeric(ei$Value))), , drop = FALSE]
    if (nrow(sub) == 0) {
      stop("EI data has no values for ", var, ".")
    }
    yr <- if (is.null(year)) max(as.integer(sub$Year)) else as.integer(year)
    sub <- sub[as.integer(sub$Year) == yr, , drop = FALSE]
    code <- trimws(as.character(sub$ISO3166_alpha3))
    value <- as.numeric(sub$Value)
    world <- unique(value[code == world_code])
    if (length(world) != 1 || is.na(world) || world <= 0) {
      stop("EI data has no single Total World (", world_code, ") value for ", var, " in ", yr, ".")
    }

    is_country <- grepl("^[A-Z]{3}$", code) & code != world_code & !is_aggregate_or_residual(sub$Country)
    countries <- unique(data.frame(iso3 = code[is_country], country = sub$Country[is_country],
                                   value = value[is_country], stringsAsFactors = FALSE))
    if (anyDuplicated(countries$iso3)) {
      stop("EI data has two ", var, " values for one ISO3 in ", yr, ": ",
           paste(unique(countries$iso3[duplicated(countries$iso3)]), collapse = ", "))
    }
    unit <- if (grepl("_kbd$", var)) "kb/d" else if (grepl("_ej$", var)) "EJ" else ""
    label <- paste0("EI Statistical Review: ", var)
    source <- "Energy Institute, Statistical Review of World Energy"
    out[[length(out) + 1L]] <- coalition_supplier_rows(
      iso3 = countries$iso3, country = countries$country, tech = tech, supply_chain = "Extraction",
      measure = "ei_production", measure_label = label, year = yr, value = countries$value,
      unit = unit, world_share = countries$value / world, source = source
    )
    eu_value <- unique(value[code == eu_aggregate_code])
    if (length(eu_value) == 1 && !is.na(eu_value)) {
      out[[length(out) + 1L]] <- coalition_supplier_rows(
        iso3 = eu_code, country = "European Union", tech = tech, supply_chain = "Extraction",
        measure = "ei_production", measure_label = label, year = yr, value = eu_value,
        unit = unit, world_share = eu_value / world, source = source, is_aggregate = TRUE
      )
    }
  }
  dplyr::bind_rows(out)
}

# IEA clean-technology manufacturing capacity (iea_cleantech_Midstream.csv, as
# market_share_manufacturing reads it): one row per economy, columns "<Technology> - Current" and
# "<Technology> - Pipeline" in percent of world capacity. `status` "Current" is the installed
# capacity (2024 in market_share_manufacturing's reading). For the technologies in `tech_map` (IEA
# label -> pipeline tech; electrolysers are Green Hydrogen), each in its stage of `tech_stages`
# (pipeline tech -> stage; COALITION_CLEANTECH_STAGES: Batteries Components, the others Assembly).
# The IEA reports the EU as one bloc: that row becomes eu_code with is_aggregate = TRUE (it is not a
# member-level share). Shares are each row over the column total including "Other", so rounding
# in the published percentages does not leave the total off 100.
coalition_shares_cleantech <- function(cleantech_midstream,
                                       tech_map = c(Solar = "Solar", Wind = "Wind",
                                                    Batteries = "Batteries", Electrolyzers = "Green Hydrogen"),
                                       status = "Current",
                                       year = 2024L,
                                       eu_code = "EUU",
                                       eu_labels = c("EU", "European Union"),
                                       tech_stages = COALITION_CLEANTECH_STAGES) {
  if (!inherits(cleantech_midstream, "data.frame") || ncol(cleantech_midstream) < 2) {
    stop("cleantech_midstream must be a data.frame with a label column and technology columns.")
  }
  unstaged <- setdiff(unname(tech_map), names(tech_stages))
  if (length(unstaged) > 0) {
    stop("coalition_shares_cleantech: no stage in tech_stages for ", paste(unstaged, collapse = ", "), ".")
  }
  label <- trimws(as.character(cleantech_midstream[[1]]))
  # "Solar - Current" (check.names = FALSE) or "Solar...Current" (read.csv default).
  col_key <- trimws(gsub("[^A-Za-z]+", " ", names(cleantech_midstream)))
  col_tech <- sub(" (Current|Pipeline)$", "", col_key)
  col_status <- ifelse(grepl(" (Current|Pipeline)$", col_key), sub("^.* ", "", col_key), NA_character_)

  is_eu <- label %in% eu_labels
  residual <- !is_eu & is_aggregate_or_residual(label)
  iso3 <- ifelse(is_eu, eu_code, country_name_to_iso3(label))
  unmatched <- !residual & is.na(iso3)
  if (any(unmatched)) {
    stop("Clean-tech manufacturing rows with no ISO3 code: ", paste(label[unmatched], collapse = ", "))
  }

  out <- list()
  for (iea_tech in names(tech_map)) {
    j <- which(tolower(col_tech) == tolower(iea_tech) & col_status == status)
    if (length(j) != 1) {
      stop("iea_cleantech_Midstream has no single '", iea_tech, " - ", status, "' column.")
    }
    value <- suppressWarnings(as.numeric(cleantech_midstream[[j]]))
    total <- sum(value, na.rm = TRUE)
    keep <- !residual & !is.na(value)
    out[[length(out) + 1L]] <- coalition_supplier_rows(
      iso3 = iso3[keep],
      country = ifelse(is_eu[keep], "European Union", label[keep]),
      tech = tech_map[[iea_tech]],
      supply_chain = tech_stages[[tech_map[[iea_tech]]]],
      measure = "iea_manufacturing_capacity",
      measure_label = paste0("IEA manufacturing capacity: ", iea_tech, " (", status, ")"),
      year = year,
      value = value[keep],
      unit = "% of world capacity",
      world_share = if (total > 0) value[keep] / total else NA_real_,
      source = "IEA Energy Technology Perspectives 2024 (manufacturing capacity)",
      is_aggregate = is_eu[keep]
    )
  }
  dplyr::bind_rows(out)
}

# Export shares of each technology x stage HS6 basket: the trade theme's raw `market_share`
# (trade_concentration_tbl / export_feasibility_tbl, data_type "raw"), each economy's share of
# world exports of the basket from the Atlas of Economic Complexity (trade_core_build_aec_market_share()).
# The whole basket (sub_sector "All") only. The data year is read from the explanation's "[YYYY]"
# when present (the Atlas leg is a year behind the table's Comtrade Year).
# The cells are the trade table's six-stage cells (each HS6 code in its master stage; Deployment
# has no basket), so the table must carry stage names: a three-stage table (one built with
# legacy_stages = TRUE, or before 2026-10-26) stops rather than matching none of `stages`.
coalition_shares_trade <- function(trade_tbl, techs, stages = SUPPLY_CHAIN_SUPPLY_STAGES,
                                   variable = "market_share") {
  coalition_require_columns(
    trade_tbl, c("iso3c", "tech", "supply_chain", "variable", "data_type", "value"), "trade_tbl"
  )
  assert_supply_chain_stages(stages, label = "coalition_shares_trade(): stages")
  keep <- trade_tbl$variable == variable & trade_tbl$data_type == "raw" & trade_tbl$tech %in% techs
  if ("sub_sector" %in% names(trade_tbl)) {
    keep <- keep & (is.na(trade_tbl$sub_sector) | trade_tbl$sub_sector == "All")
  }
  assert_supply_chain_stages(trade_tbl$supply_chain[keep], label = paste0("trade_tbl (", variable, ") supply_chain"))
  keep <- keep & trade_tbl$supply_chain %in% stages
  t <- trade_tbl[keep & !is.na(trade_tbl$iso3c) & !is.na(trade_tbl$value), , drop = FALSE]
  if (nrow(t) == 0) {
    return(coalition_empty_supplier_tbl())
  }
  t <- unique(t[intersect(c("iso3c", "Country", "tech", "supply_chain", "value", "Year", "source", "explanation"), names(t))])
  key <- paste(t$iso3c, t$tech, t$supply_chain)
  if (anyDuplicated(key)) {
    stop("trade_tbl has two ", variable, " values for one (iso3c, tech, supply_chain): ",
         paste(utils::head(unique(key[duplicated(key)]), 5), collapse = "; "))
  }
  year <- if ("explanation" %in% names(t)) {
    suppressWarnings(as.integer(sub("^.*\\[((19|20)\\d{2})\\].*$", "\\1", t$explanation)))
  } else {
    rep(NA_integer_, nrow(t))
  }
  if ("Year" %in% names(t)) year <- ifelse(is.na(year), as.integer(t$Year), year)
  coalition_supplier_rows(
    iso3 = coalition_clean_codes(t$iso3c),
    country = if ("Country" %in% names(t)) t$Country else t$iso3c,
    tech = t$tech,
    supply_chain = t$supply_chain,
    measure = "atlas_export_share",
    measure_label = "Share of world exports of the tech x stage HS6 basket (Atlas)",
    year = year,
    value = t$value,
    unit = "share of world exports",
    world_share = t$value,
    source = if ("source" %in% names(t)) t$source else "Harvard Atlas of Economic Complexity"
  )
}

# World shares of production and exports per (iso3, tech, supply_chain, measure[, mineral]) for
# the supplier test, from whichever inputs are given (NULL skips a source):
# - critical + mineral_demand: IEA mining (Extraction) and refining (Refining) of minerals, mapped
#   to technologies (coalition_shares_iea_minerals());
# - ei_minerals + mineral_demand: EI mine production (Extraction) of the minerals the IEA supply
#   sheet does not cover at the mining stage (manganese, zinc, PGMs; coalition_shares_ei_minerals());
# - uranium_production: Nuclear Extraction (coalition_shares_uranium());
# - ei: Oil, Gas and Coal Extraction (production; coalition_shares_ei_production());
# - cleantech_midstream: Solar, Wind, Batteries and Green Hydrogen manufacturing capacity, Batteries
#   Components and the others Assembly (cleantech_stages), EU as a bloc (coalition_shares_cleantech());
# - trade_tbl: export shares of every six-stage tech x stage HS6 basket (coalition_shares_trade());
# - usgs_reserves + mineral_demand: USGS MCS reserves of the critical minerals (usgs_mcs_reserves(),
#   Extraction of the technologies that use them; coalition_shares_usgs_reserves()), measure
#   "usgs_reserves";
# - ei_reserves + mineral_demand: EI reserves of oil, gas and coal (coalition_shares_ei_reserves();
#   mineral sheets too, if passed), measure "ei_reserves". A mineral both reserves inputs carry
#   stops: it would be tested twice, on two vintages;
# - uranium_resources + uranium_resources_variable: Red Book identified uranium resources, Nuclear
#   Extraction (coalition_shares_uranium_resources()), measure "uranium_resources".
# Restricted to `techs` and `stages` (stage names; by default the five supply stages,
# SUPPLY_CHAIN_SUPPLY_STAGES). `threshold` is the row's bar (coalition_supplier_threshold():
# supplier_reserves_share for the reserves measures, supplier_share otherwise) and `major` is
# TRUE where a country-level (not aggregate) world share reaches it. Aggregate rows (the EU in EI
# and the IEA manufacturing data) are kept for reference and the EU weights' coverage check, never
# for the test.
major_supplier_shares <- function(critical = NULL,
                                  mineral_demand = NULL,
                                  ei_minerals = NULL,
                                  uranium_production = NULL,
                                  ei = NULL,
                                  cleantech_midstream = NULL,
                                  trade_tbl = NULL,
                                  ei_reserves = NULL,
                                  uranium_resources = NULL,
                                  usgs_reserves = NULL,
                                  techs,
                                  stages = SUPPLY_CHAIN_SUPPLY_STAGES,
                                  supplier_share = 0.05,
                                  supplier_reserves_share = 0.10,
                                  mineral_min_demand_share = 0.01,
                                  eu_code = "EUU",
                                  ei_vars = c(Oil = "oilprod_kbd", Gas = "gasprod_ej", Coal = "coalprod_ej"),
                                  cleantech_tech_map = c(Solar = "Solar", Wind = "Wind",
                                                         Batteries = "Batteries", Electrolyzers = "Green Hydrogen"),
                                  cleantech_status = "Current",
                                  cleantech_year = 2024L,
                                  cleantech_stages = COALITION_CLEANTECH_STAGES,
                                  uranium_resources_variable = NULL) {
  coalition_check_share(supplier_share, "supplier_share")
  coalition_check_share(supplier_reserves_share, "supplier_reserves_share")
  assert_supply_chain_stages(stages, label = "major_supplier_shares(): stages")
  parts <- list(coalition_empty_supplier_tbl())
  iea_mined <- character()
  if (!is.null(critical)) {
    if (is.null(mineral_demand)) stop("critical needs mineral_demand (reserves_build_mineral_demand_clean()).")
    iea <- coalition_shares_iea_minerals(
      critical, mineral_demand, techs = techs, min_demand_share = mineral_min_demand_share
    )
    iea_mined <- unique(iea$mineral[iea$supply_chain == "Extraction"])
    parts[[length(parts) + 1L]] <- iea
  }
  if (!is.null(ei_minerals)) {
    if (is.null(mineral_demand)) stop("ei_minerals needs mineral_demand (reserves_build_mineral_demand_clean()).")
    parts[[length(parts) + 1L]] <- coalition_shares_ei_minerals(
      ei_minerals, mineral_demand, techs = techs, min_demand_share = mineral_min_demand_share,
      exclude_minerals = iea_mined
    )
  }
  if (!is.null(uranium_production)) {
    parts[[length(parts) + 1L]] <- coalition_shares_uranium(uranium_production)
  }
  if (!is.null(ei)) {
    parts[[length(parts) + 1L]] <- coalition_shares_ei_production(ei, vars = ei_vars, eu_code = eu_code)
  }
  if (!is.null(cleantech_midstream)) {
    parts[[length(parts) + 1L]] <- coalition_shares_cleantech(
      cleantech_midstream, tech_map = cleantech_tech_map, status = cleantech_status,
      year = cleantech_year, eu_code = eu_code, tech_stages = cleantech_stages
    )
  }
  if (!is.null(trade_tbl)) {
    parts[[length(parts) + 1L]] <- coalition_shares_trade(trade_tbl, techs = techs, stages = stages)
  }
  usgs_part <- ei_part <- coalition_empty_supplier_tbl()
  if (!is.null(usgs_reserves)) {
    if (is.null(mineral_demand)) stop("usgs_reserves needs mineral_demand (reserves_build_mineral_demand_clean()).")
    usgs_part <- coalition_shares_usgs_reserves(
      usgs_reserves, mineral_demand, techs = techs, min_demand_share = mineral_min_demand_share
    )
    parts[[length(parts) + 1L]] <- usgs_part
  }
  if (!is.null(ei_reserves)) {
    if (is.null(mineral_demand)) stop("ei_reserves needs mineral_demand (reserves_build_mineral_demand_clean()).")
    ei_part <- coalition_shares_ei_reserves(
      ei_reserves, mineral_demand, techs = techs, min_demand_share = mineral_min_demand_share
    )
    parts[[length(parts) + 1L]] <- ei_part
  }
  # A mineral read from both would be tested twice, on two vintages of the same USGS data.
  twice <- intersect(stats::na.omit(usgs_part$mineral), stats::na.omit(ei_part$mineral))
  if (length(twice) > 0) {
    stop("major_supplier_shares: reserves of ", paste(twice, collapse = ", "),
         " come from both usgs_reserves and ei_reserves; pass each mineral from one source.")
  }
  if (!is.null(uranium_resources)) {
    if (is.null(uranium_resources_variable)) {
      stop("uranium_resources needs uranium_resources_variable (uranium_resources_variable(), R/categories/reserves/uranium.R).")
    }
    parts[[length(parts) + 1L]] <- coalition_shares_uranium_resources(uranium_resources, variable = uranium_resources_variable)
  }
  out <- dplyr::bind_rows(parts)
  out <- out[out$tech %in% techs & out$supply_chain %in% stages & !is.na(out$iso3), , drop = FALSE]
  key <- paste(out$iso3, out$tech, out$supply_chain, out$measure, out$mineral)
  if (anyDuplicated(key)) {
    stop("major_supplier_shares: duplicate (iso3, tech, supply_chain, measure, mineral) rows: ",
         paste(utils::head(unique(key[duplicated(key)]), 5), collapse = "; "))
  }
  out$threshold <- as.numeric(coalition_supplier_threshold(out$measure, supplier_share, supplier_reserves_share))
  out$major <- !out$is_aggregate & !is.na(out$world_share) & out$world_share >= out$threshold
  out <- out[order(out$tech, out$supply_chain, out$measure, out$mineral, -out$world_share, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

## Tiers --------------------------------------------------------------------

# Latest non-missing GDP per ISO3 from a WDI-style long table (data/raw/wdi_gdp.csv: iso3c, year,
# NY.GDP.MKTP.CD, current USD). One row per ISO3: iso3, gdp_usd, gdp_year.
coalition_latest_gdp <- function(gdp_tbl, iso3_col = "iso3c", year_col = "year", value_col = "NY.GDP.MKTP.CD") {
  coalition_require_columns(gdp_tbl, c(iso3_col, year_col, value_col), "gdp_tbl")
  df <- data.frame(
    iso3 = coalition_clean_codes(gdp_tbl[[iso3_col]]),
    year = suppressWarnings(as.integer(gdp_tbl[[year_col]])),
    gdp_usd = suppressWarnings(as.numeric(gdp_tbl[[value_col]])),
    stringsAsFactors = FALSE
  )
  df <- unique(df[!is.na(df$iso3) & !is.na(df$year) & !is.na(df$gdp_usd), , drop = FALSE])
  if (anyDuplicated(df[c("iso3", "year")])) {
    stop("gdp_tbl has two GDP values for one ISO3 and year.")
  }
  df <- df[order(df$iso3, -df$year), , drop = FALSE]
  df <- df[!duplicated(df$iso3), , drop = FALSE]
  tibble::tibble(iso3 = df$iso3, gdp_usd = df$gdp_usd, gdp_year = df$year)
}

# Whether each row of a bloc table carries the economy's own UN votes. scripts/12 records how an
# economy without a vote got its position: alignment_basis ("own_votes", "inherited", "neutral",
# "none") in the current outputs, imputed_from / proxy_of (the sovereign it copies) in earlier
# ones, where an economy with neither a sovereign to copy nor a bloc (Taiwan) has no vote either.
# With none of these columns every economy with a bloc counts as voting.
coalition_own_vote <- function(bloc_tbl) {
  if ("alignment_basis" %in% names(bloc_tbl)) {
    return(!is.na(bloc_tbl$alignment_basis) & bloc_tbl$alignment_basis == "own_votes")
  }
  has_bloc <- !is.na(bloc_tbl$bloc)
  for (col in c("imputed_from", "proxy_of")) {
    if (col %in% names(bloc_tbl)) {
      return(has_bloc & (is.na(bloc_tbl[[col]]) | !nzchar(as.character(bloc_tbl[[col]]))))
    }
  }
  has_bloc
}

coalition_usd_bn <- function(x) {
  ifelse(is.na(x), "no GDP", paste0("USD ", formatC(x / 1e9, format = "f", digits = 1, big.mark = ","), "bn"))
}

# Network node of each economy: eu_code for the EU member states, the economy's own ISO3 otherwise.
eu_node_map <- function(iso3, eu_members, eu_code = "EUU") {
  iso3 <- coalition_clean_codes(iso3)
  eu_members <- coalition_clean_codes(eu_members)
  ifelse(!is.na(iso3) & iso3 %in% eu_members, eu_code, iso3)
}

# The two-tier coalition. One row per economy of bloc_tbl, agreements_tbl (country rows; the EU's
# own rows are skipped), the country rows of supplier_tbl and eu_members:
#   iso3, country, tier (core / partner / none), routes ("core; agreement; supplier; supplier:
#   reserves": every route the economy satisfies, COALITION_ROUTES, "none" if it satisfies none),
#   bloc, alignment_basis, own_vote, gdp_usd, gdp_year, eu_member, network_node (eu_node_map()),
#   node_tier (the node's tier: eu_node_tier for EU members, the country's own tier otherwise),
#   route_core, route_agreement, route_supplier (the supplier route by any measure: the partner
#   condition), route_supplier_reserves (by a reserves measure), agreements (every agreement listed
#   for the economy), qualifying_agreements (those in partner_agreements), supplier_cells (the
#   stages where it is a major supplier, each with the measure and share, largest first),
#   max_supplier_share (largest world share in a production, refining, manufacturing or export
#   measure: compare with supplier_share), max_reserves_share (largest share of world reserves:
#   compare with supplier_reserves_share), supplier_excluded (a major supplier by either kind of
#   measure kept off the supplier route because it is China-leaning or has no alignment) and reason.
#
# - bloc_tbl: geo_ideal_points_tbl (scripts/12): iso3, bloc and optionally country and
#   alignment_basis / imputed_from / proxy_of (coalition_own_vote()).
# - gdp_tbl: WDI GDP (coalition_latest_gdp() column arguments).
# - agreements_tbl: data/reference/economic_security_agreements.csv: iso3, agreement and
#   optionally country.
# - supplier_tbl: major_supplier_shares(): iso3, tech, supply_chain, measure, world_share and
#   optionally mineral, is_aggregate, country.
# Core needs the U.S.-leaning bloc, GDP >= gdp_floor_usd and (core_requires_own_vote) the
# economy's own UN votes, as derive_alignment_coalition() does. The agreement route needs GDP >=
# gdp_floor_usd and no bloc condition. The supplier route needs no GDP but a bloc other than
# china_bloc (and, with supplier_requires_alignment, a known bloc), and a world share at or above
# the row's threshold, coalition_supplier_threshold(measure, supplier_share,
# supplier_reserves_share): these two arguments decide, not supplier_tbl's own `major` column, so a
# sensitivity run can move either bar; pass the values major_supplier_shares() used to reproduce
# its flags.
derive_coalition_tiers <- function(bloc_tbl,
                                   gdp_tbl,
                                   agreements_tbl,
                                   supplier_tbl,
                                   gdp_floor_usd = 1e11,
                                   supplier_share = 0.05,
                                   supplier_reserves_share = 0.10,
                                   partner_agreements = c("U.S. FTA", "IPEF", "MSP", "G7", "NATO"),
                                   us_bloc = "U.S.-leaning",
                                   china_bloc = "China-leaning",
                                   core_requires_own_vote = TRUE,
                                   supplier_requires_alignment = TRUE,
                                   eu_members = character(),
                                   eu_code = "EUU",
                                   eu_node_tier = "core",
                                   gdp_iso3_col = "iso3c",
                                   gdp_year_col = "year",
                                   gdp_value_col = "NY.GDP.MKTP.CD") {
  coalition_require_columns(bloc_tbl, c("iso3", "bloc"), "bloc_tbl")
  coalition_require_columns(agreements_tbl, c("iso3", "agreement"), "agreements_tbl")
  coalition_require_columns(supplier_tbl, c("iso3", "tech", "supply_chain", "measure", "world_share"), "supplier_tbl")
  if (!is.numeric(gdp_floor_usd) || length(gdp_floor_usd) != 1 || is.na(gdp_floor_usd) || gdp_floor_usd < 0) {
    stop("gdp_floor_usd must be a single number >= 0.")
  }
  coalition_check_share(supplier_share, "supplier_share")
  coalition_check_share(supplier_reserves_share, "supplier_reserves_share")
  if (!eu_node_tier %in% COALITION_TIERS) stop("eu_node_tier must be one of ", paste(COALITION_TIERS, collapse = ", "))

  bloc_iso3 <- coalition_clean_codes(bloc_tbl$iso3)
  if (anyNA(bloc_iso3) || anyDuplicated(bloc_iso3)) stop("bloc_tbl has missing or duplicate ISO3 codes.")
  eu_members <- unique(coalition_clean_codes(eu_members))

  agreements <- data.frame(
    iso3 = coalition_clean_codes(agreements_tbl$iso3),
    agreement = trimws(as.character(agreements_tbl$agreement)),
    country = if ("country" %in% names(agreements_tbl)) as.character(agreements_tbl$country) else rep(NA_character_, nrow(agreements_tbl)),
    stringsAsFactors = FALSE
  )
  agreements <- agreements[!is.na(agreements$iso3) & agreements$iso3 != eu_code, , drop = FALSE]

  supplier <- data.frame(
    iso3 = coalition_clean_codes(supplier_tbl$iso3),
    country = if ("country" %in% names(supplier_tbl)) as.character(supplier_tbl$country) else NA_character_,
    tech = as.character(supplier_tbl$tech),
    supply_chain = as.character(supplier_tbl$supply_chain),
    measure = as.character(supplier_tbl$measure),
    mineral = if ("mineral" %in% names(supplier_tbl)) as.character(supplier_tbl$mineral) else NA_character_,
    world_share = as.numeric(supplier_tbl$world_share),
    is_aggregate = if ("is_aggregate" %in% names(supplier_tbl)) as.logical(supplier_tbl$is_aggregate) else FALSE,
    stringsAsFactors = FALSE
  )
  supplier <- supplier[!is.na(supplier$iso3) & supplier$iso3 != eu_code & !supplier$is_aggregate, , drop = FALSE]

  iso3 <- sort(unique(c(bloc_iso3, agreements$iso3, supplier$iso3, eu_members)))
  b <- match(iso3, bloc_iso3)
  bloc <- as.character(bloc_tbl$bloc)[b]
  own_vote <- coalition_own_vote(bloc_tbl)[b]
  own_vote[is.na(b)] <- FALSE
  basis <- if ("alignment_basis" %in% names(bloc_tbl)) {
    as.character(bloc_tbl$alignment_basis)[b]
  } else {
    ifelse(is.na(b), NA_character_, ifelse(own_vote, "own_votes", ifelse(is.na(bloc), "none", "inherited")))
  }

  country <- if ("country" %in% names(bloc_tbl)) as.character(bloc_tbl$country)[b] else rep(NA_character_, length(iso3))
  fill_country <- function(country, from_iso3, from_country) {
    k <- match(iso3, from_iso3)
    ifelse(is.na(country) | !nzchar(country), from_country[k], country)
  }
  country <- fill_country(country, agreements$iso3, agreements$country)
  country <- fill_country(country, supplier$iso3, supplier$country)
  country <- ifelse(is.na(country) | !nzchar(country), iso3, country)

  gdp <- coalition_latest_gdp(gdp_tbl, iso3_col = gdp_iso3_col, year_col = gdp_year_col, value_col = gdp_value_col)
  g <- match(iso3, gdp$iso3)
  gdp_usd <- gdp$gdp_usd[g]
  gdp_year <- gdp$gdp_year[g]
  big <- !is.na(gdp_usd) & gdp_usd >= gdp_floor_usd

  # Core.
  route_core <- !is.na(bloc) & bloc %in% us_bloc & big & (own_vote | !core_requires_own_vote)

  # Agreement route.
  agreements_all <- vapply(iso3, function(i) {
    a <- unique(agreements$agreement[agreements$iso3 == i])
    if (length(a) == 0) NA_character_ else paste(sort(a), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  qualifying <- vapply(iso3, function(i) {
    a <- unique(agreements$agreement[agreements$iso3 == i & agreements$agreement %in% partner_agreements])
    if (length(a) == 0) NA_character_ else paste(sort(a), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  route_agreement <- !is.na(qualifying) & big

  # Supplier route: each row against its own threshold (reserves at supplier_reserves_share,
  # every other measure at supplier_share).
  supplier$reserves <- supplier$measure %in% COALITION_RESERVES_MEASURES
  supplier$threshold <- coalition_supplier_threshold(supplier$measure, supplier_share, supplier_reserves_share)
  major <- supplier[!is.na(supplier$world_share) & supplier$world_share >= supplier$threshold, , drop = FALSE]
  major <- major[order(major$iso3, -major$world_share), , drop = FALSE]
  cell_text <- paste0(
    major$tech, " ", major$supply_chain, " (", major$measure,
    ifelse(is.na(major$mineral), "", paste0(" ", major$mineral)),
    " ", formatC(100 * major$world_share, format = "f", digits = 1), "%)"
  )
  supplier_cells <- vapply(iso3, function(i) {
    x <- unique(cell_text[major$iso3 == i])
    if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  # The largest share of each kind, each to be read against its own threshold: a 7% reserves
  # holder is below the reserves bar although 7% would pass for production.
  max_share_of <- function(rows) {
    vapply(iso3, function(i) {
      x <- supplier$world_share[rows & supplier$iso3 == i & !is.na(supplier$world_share)]
      if (length(x) == 0) NA_real_ else max(x)
    }, numeric(1), USE.NAMES = FALSE)
  }
  max_share <- max_share_of(!supplier$reserves)
  max_reserves_share <- max_share_of(supplier$reserves)
  major_flow <- iso3 %in% major$iso3[!major$reserves]
  major_reserves <- iso3 %in% major$iso3[major$reserves]
  is_major <- major_flow | major_reserves
  supplier_eligible <- !(bloc %in% china_bloc) & (!is.na(bloc) | !supplier_requires_alignment)
  route_supplier <- is_major & supplier_eligible
  route_supplier_reserves <- major_reserves & supplier_eligible
  supplier_excluded <- is_major & !supplier_eligible

  tier <- ifelse(route_core, "core", ifelse(route_agreement | route_supplier, "partner", "none"))
  routes <- vapply(seq_along(iso3), function(k) {
    r <- COALITION_ROUTES[c(route_core[k], route_agreement[k], major_flow[k] & supplier_eligible[k],
                            route_supplier_reserves[k])]
    if (length(r) == 0) "none" else paste(r, collapse = "; ")
  }, character(1))

  eu_member <- iso3 %in% eu_members
  network_node <- eu_node_map(iso3, eu_members, eu_code = eu_code)
  node_tier <- ifelse(eu_member, eu_node_tier, tier)

  bloc_txt <- ifelse(is.na(bloc), "no bloc", bloc)
  gdp_txt <- ifelse(is.na(gdp_usd), "no WDI GDP", paste0("GDP ", coalition_usd_bn(gdp_usd), " (", gdp_year, ")"))
  floor_txt <- coalition_usd_bn(gdp_floor_usd)
  reason <- paste0(
    bloc_txt,
    ifelse(!is.na(b) & !own_vote & !is.na(bloc), " (no own UN vote)", ""),
    "; ", gdp_txt, ifelse(big, " >= ", " < "), floor_txt,
    ifelse(is.na(qualifying), "", paste0("; agreements: ", qualifying)),
    ifelse(is_major, paste0("; ", ifelse(major_flow, ifelse(major_reserves, "major supplier and reserves holder", "major supplier"),
                                         "major reserves holder"),
                            ifelse(supplier_excluded, " (excluded: ", ""),
                            ifelse(supplier_excluded, ifelse(is.na(bloc), "no alignment)", paste0(bloc, ")")), "")), ""),
    ifelse(eu_member, paste0("; EU member: node ", eu_code, " (", eu_node_tier, ")"), "")
  )

  out <- tibble::tibble(
    iso3 = iso3,
    country = country,
    tier = tier,
    routes = routes,
    bloc = bloc,
    alignment_basis = basis,
    own_vote = own_vote,
    gdp_usd = gdp_usd,
    gdp_year = gdp_year,
    eu_member = eu_member,
    network_node = network_node,
    node_tier = node_tier,
    route_core = route_core,
    route_agreement = route_agreement,
    route_supplier = route_supplier,
    route_supplier_reserves = route_supplier_reserves,
    agreements = agreements_all,
    qualifying_agreements = qualifying,
    supplier_cells = supplier_cells,
    max_supplier_share = max_share,
    max_reserves_share = max_reserves_share,
    supplier_excluded = supplier_excluded,
    reason = reason
  )
  ord <- order(match(out$tier, COALITION_TIERS), -ifelse(is.na(out$gdp_usd), -Inf, out$gdp_usd), out$iso3)
  out[ord, , drop = FALSE]
}

## EU stage weights ---------------------------------------------------------

# Each EU member's weight within the EU node per (tech, supply_chain), for stage-weighted
# aggregation of the members' scores into the node (the allied network solves the five supply
# stages, so `stages` defaults to SUPPLY_CHAIN_SUPPLY_STAGES and supplier_tbl must carry stage
# names). One row per (member, tech, supply_chain):
#   network_node, iso3, country, tech, supply_chain, weight (sums to 1 over the members of a
#   stage), basis ("production", "exports" or "gdp"), measures, year, member_value (the member's
#   world share under the basis, or its GDP in USD), eu_world_share (the EU's world share under the
#   basis; NA for GDP), coverage (share of the EU total that the listed members cover).
#
# The bases are tried in the order of basis_priority (a named list: basis -> measures of
# supplier_tbl), then GDP. A measure is usable for a stage when the members' summed world share is
# positive, at least min_eu_world_share, and covers at least min_coverage of the EU total (the
# source's EU aggregate row, e.g. EI's "Total EU"; without one the listing is taken as complete).
# A member's weight is its share of the members' total, averaged over the usable measures of the
# first basis that has one; members a source does not list weigh 0. Where no basis is usable the
# weight is the member's share of the members' GDP (gdp_tbl, latest year).
#
# Only measures that list EU members individually and completely can carry the weights: the
# EI production series (with "Total EU" for the coverage check; Oil, Gas and Coal Extraction), the
# Red Book uranium table (Nuclear Extraction) and the Atlas export shares (every stage with an HS6
# basket; the other stages, most Recovery cells among them, fall to GDP). The IEA manufacturing
# data report the EU as one bloc and the IEA mineral
# supply sheet lists only the leading producers (the rest of the EU sits in "Rest of world"), so
# config/coalition.yml leaves them out of basis_priority.
eu_stage_weights <- function(supplier_tbl,
                             gdp_tbl,
                             eu_members,
                             techs,
                             stages = SUPPLY_CHAIN_SUPPLY_STAGES,
                             basis_priority = list(
                               production = c("ei_production", "uranium_mine_production"),
                               exports = "atlas_export_share"
                             ),
                             min_coverage = 0.5,
                             min_eu_world_share = 0.001,
                             eu_code = "EUU",
                             country_names = NULL,
                             gdp_iso3_col = "iso3c",
                             gdp_year_col = "year",
                             gdp_value_col = "NY.GDP.MKTP.CD") {
  coalition_require_columns(supplier_tbl, c("iso3", "tech", "supply_chain", "measure", "world_share"), "supplier_tbl")
  # Cells are matched on the stage name, so three-stage labels on either side would silently put
  # every cell on GDP.
  assert_supply_chain_stages(stages, label = "eu_stage_weights(): stages")
  assert_supply_chain_stages(supplier_tbl$supply_chain, label = "eu_stage_weights(): supplier_tbl$supply_chain")
  eu_members <- unique(coalition_clean_codes(eu_members))
  if (length(eu_members) == 0 || anyNA(eu_members)) stop("eu_members must be a non-empty vector of ISO3 codes.")
  if (!is.list(basis_priority) || is.null(names(basis_priority)) || any(!nzchar(names(basis_priority)))) {
    stop("basis_priority must be a named list of measures, e.g. list(production = ..., exports = ...).")
  }

  s <- data.frame(
    iso3 = coalition_clean_codes(supplier_tbl$iso3),
    tech = as.character(supplier_tbl$tech),
    supply_chain = as.character(supplier_tbl$supply_chain),
    measure = as.character(supplier_tbl$measure),
    mineral = if ("mineral" %in% names(supplier_tbl)) as.character(supplier_tbl$mineral) else NA_character_,
    year = if ("year" %in% names(supplier_tbl)) as.integer(supplier_tbl$year) else NA_integer_,
    world_share = as.numeric(supplier_tbl$world_share),
    is_aggregate = if ("is_aggregate" %in% names(supplier_tbl)) as.logical(supplier_tbl$is_aggregate) else FALSE,
    stringsAsFactors = FALSE
  )
  s$world_share[is.na(s$world_share)] <- 0

  gdp <- coalition_latest_gdp(gdp_tbl, iso3_col = gdp_iso3_col, year_col = gdp_year_col, value_col = gdp_value_col)
  member_gdp <- gdp$gdp_usd[match(eu_members, gdp$iso3)]
  member_gdp_year <- gdp$gdp_year[match(eu_members, gdp$iso3)]

  if (is.null(country_names)) country_names <- stats::setNames(eu_members, eu_members)
  member_country <- unname(country_names[eu_members])
  member_country[is.na(member_country)] <- eu_members[is.na(member_country)]

  cells <- expand.grid(tech = techs, supply_chain = stages, stringsAsFactors = FALSE)
  out <- vector("list", nrow(cells))
  for (k in seq_len(nrow(cells))) {
    tech <- cells$tech[k]
    stage <- cells$supply_chain[k]
    chosen <- NULL
    for (basis in names(basis_priority)) {
      rows <- s[s$tech == tech & s$supply_chain == stage & s$measure %in% basis_priority[[basis]], , drop = FALSE]
      if (nrow(rows) == 0) next
      groups <- unique(rows[c("measure", "mineral")])
      usable <- list()
      for (gi in seq_len(nrow(groups))) {
        gm <- rows[rows$measure == groups$measure[gi] &
                     (rows$mineral %in% groups$mineral[gi] | (is.na(rows$mineral) & is.na(groups$mineral[gi]))), , drop = FALSE]
        members <- gm[!gm$is_aggregate & gm$iso3 %in% eu_members, , drop = FALSE]
        member_sum <- sum(members$world_share)
        agg <- gm$world_share[gm$is_aggregate & gm$iso3 == eu_code]
        eu_total <- if (length(agg) == 1) agg else member_sum
        coverage <- if (eu_total > 0) member_sum / eu_total else NA_real_
        if (member_sum > 0 && eu_total >= min_eu_world_share && !is.na(coverage) && coverage >= min_coverage) {
          v <- members$world_share[match(eu_members, members$iso3)]
          v[is.na(v)] <- 0
          usable[[length(usable) + 1L]] <- list(
            label = if (is.na(groups$mineral[gi])) groups$measure[gi] else paste0(groups$measure[gi], ":", groups$mineral[gi]),
            within = v / member_sum, value = v, eu_total = eu_total, coverage = coverage,
            year = suppressWarnings(max(gm$year, na.rm = TRUE))
          )
        }
      }
      if (length(usable) > 0) {
        chosen <- list(
          basis = basis,
          weight = Reduce(`+`, lapply(usable, `[[`, "within")) / length(usable),
          measures = paste(vapply(usable, `[[`, character(1), "label"), collapse = "; "),
          member_value = Reduce(`+`, lapply(usable, `[[`, "value")) / length(usable),
          eu_world_share = mean(vapply(usable, `[[`, numeric(1), "eu_total")),
          coverage = mean(vapply(usable, `[[`, numeric(1), "coverage")),
          year = max(vapply(usable, function(u) as.numeric(u$year), numeric(1)))
        )
        break
      }
    }
    if (is.null(chosen)) {
      v <- member_gdp
      v[is.na(v)] <- 0
      if (sum(v) <= 0) stop("No GDP for the EU members: cannot weight ", tech, " ", stage, ".")
      chosen <- list(
        basis = "gdp", weight = v / sum(v), measures = "wdi_gdp", member_value = member_gdp,
        eu_world_share = NA_real_, coverage = sum(!is.na(member_gdp)) / length(member_gdp),
        year = suppressWarnings(max(member_gdp_year, na.rm = TRUE))
      )
    }
    out[[k]] <- tibble::tibble(
      network_node = eu_code,
      iso3 = eu_members,
      country = member_country,
      tech = tech,
      supply_chain = stage,
      weight = chosen$weight,
      basis = chosen$basis,
      measures = chosen$measures,
      year = as.integer(chosen$year),
      member_value = chosen$member_value,
      eu_world_share = chosen$eu_world_share,
      coverage = chosen$coverage
    )
  }
  out <- dplyr::bind_rows(out)
  out[order(out$tech, out$supply_chain, -out$weight, out$iso3), , drop = FALSE]
}
