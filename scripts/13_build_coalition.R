source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

# Two-tier allied coalition and the EU network node. Runs after scripts/12 (the alignment blocs)
# and before scripts/20 (which reads the coalition through coalition_members()).
#
# Inputs:
# - data/processed/geo_ideal_points_tbl.rds (scripts/12): blocs (U.S.-leaning / Nonaligned /
#   China-leaning) and how each economy got its position; geo_coalition_tbl.rds (scripts/12) is
#   read, when present, only to cross-check the core tier.
# - data/processed/trade_concentration_tbl.rds (scripts/10): Atlas export share of each tech x
#   stage HS6 basket (variable market_share, data_type raw).
# - data/raw: economic_security_agreements.csv (copied from data/reference by scripts/05, read from
#   data/reference otherwise), wdi_gdp.csv, the IEA Critical Minerals Dataset (newest vintage, as
#   scripts/10 resolves it), ei_stat_review_world_energy.csv, ei_stat_review_world_energy_wide.xlsx
#   (the mineral "P-R" sheets of critical_minerals_production_specs() with
#   supplier_measures$ei_mineral_production, and the fuel reserves sheets of reserves_specs() with
#   supplier_measures$reserves, read as scripts/10 reads them), iea_cleantech_Midstream.csv,
#   uranium_production.csv and, with supplier_measures$reserves, MCS2026_World_Data.csv (USGS
#   Mineral Commodity Summaries 2026 commodity data release: the critical-mineral reserves, read
#   as scripts/10 reads them) and uranium_resources.csv (both uranium files with the
#   data/reference fallback of scripts/10).
#   The reserves leg reads the raw sources rather than reserves_tbl.rds: the theme keeps country
#   rows only, without the world total a world share needs (coalition_shares_ei_reserves(),
#   coalition_shares_usgs_reserves()).
# Outputs (data/processed, or the directory in OPSI_COALITION_OUT_DIR):
# - coalition_tbl.rds: one row per economy: iso3, country, tier (core / partner / none), routes,
#   bloc, gdp_usd, eu_member, network_node (EUU for the 27 EU members) and node_tier, plus the
#   route flags and details (derive_coalition_tiers(); routes "supplier" and "supplier: reserves"
#   for the two legs of the supplier route), tier_with_sensitivity (the tier with the
#   sensitivity_agreements, the MSP Forum, as a partner route) and the thresholds used
#   (gdp_floor_usd, supplier_share, supplier_reserves_share).
# - eu_stage_weights_tbl.rds: each EU member's weight in the EU node per (tech, stage) and its
#   basis (eu_stage_weights()).
# - major_supplier_shares_tbl.rds: world shares per (iso3, tech, stage, measure[, mineral]) behind
#   the supplier route, with each row's threshold and major flag (major_supplier_shares()).
# Settings: config/coalition.yml. Functions: R/indices/coalition.R.

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "reserves", "uranium.R"))
source(file.path(repo_root, "R", "categories", "reserves", "usgs_mcs.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "indices", "coalition.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}

coalition_settings <- yaml::read_yaml(file.path(repo_root, "config", "coalition.yml"))
tier_cfg <- coalition_settings$coalition
measures_cfg <- coalition_settings$supplier_measures
eu_cfg <- coalition_settings$eu_node
weights_cfg <- coalition_settings$eu_weights
techs <- unlist(coalition_settings$headline_techs)
stages <- unlist(coalition_settings$stages)
eu_members <- unlist(eu_cfg$members)
if (length(eu_members) != 27 || anyDuplicated(eu_members)) {
  warning("config/coalition.yml eu_node$members lists ", length(unique(eu_members)), " distinct members, not 27.", call. = FALSE)
}

processed_dir <- file.path(repo_root, config$processed_dir)
out_dir <- Sys.getenv("OPSI_COALITION_OUT_DIR", processed_dir)
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
raw_data_path <- file.path(repo_root, config$raw_data_dir)
skip_data_downloads <- tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")

agreements_path <- file.path(raw_data_path, "economic_security_agreements.csv")
if (!file.exists(agreements_path)) {
  agreements_path <- file.path(raw_inputs_reference_dir(repo_root), "economic_security_agreements.csv")
}
uranium_production_path <- file.path(raw_data_path, "uranium_production.csv")
if (!file.exists(uranium_production_path)) {
  uranium_production_path <- file.path(raw_inputs_reference_dir(repo_root), "uranium_production.csv")
}
uranium_resources_path <- file.path(raw_data_path, "uranium_resources.csv")
if (!file.exists(uranium_resources_path)) {
  uranium_resources_path <- file.path(raw_inputs_reference_dir(repo_root), "uranium_resources.csv")
}
gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
ei_path <- file.path(raw_data_path, "ei_stat_review_world_energy.csv")
cleantech_midstream_path <- file.path(raw_data_path, "iea_cleantech_Midstream.csv")
use_ei_minerals <- isTRUE(measures_cfg$ei_mineral_production)
use_reserves <- isTRUE(measures_cfg$reserves)
supplier_share <- as.numeric(tier_cfg$supplier_share)
supplier_reserves_share <- as.numeric(tier_cfg$supplier_reserves_share)
if (length(supplier_reserves_share) != 1) {
  stop("config/coalition.yml has no coalition$supplier_reserves_share (the reserves bar of the supplier route).")
}
ei_wide_path <- file.path(raw_data_path, "ei_stat_review_world_energy_wide.xlsx")
# The critical-mineral reserves of the Reserves theme (scripts/10 reads the same file).
usgs_mcs_path <- file.path(raw_data_path, "MCS2026_World_Data.csv")
critical_minerals_path <- resolve_versioned_raw_input(
  raw_data_path,
  # Same pattern and fallback as scripts/10 (top-level alternation, no capture group, for the
  # manifest scanner).
  pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx",
  label = "IEA Critical Minerals Dataset"
)
bloc_path <- file.path(processed_dir, "geo_ideal_points_tbl.rds")
geo_coalition_path <- file.path(processed_dir, "geo_coalition_tbl.rds")
trade_path <- file.path(processed_dir, "trade_concentration_tbl.rds")

required <- c(
  agreements_path, uranium_production_path, gdp_path, ei_path, cleantech_midstream_path,
  critical_minerals_path, bloc_path, trade_path, if (use_ei_minerals || use_reserves) ei_wide_path,
  if (use_reserves) c(uranium_resources_path, usgs_mcs_path)
)
missing_inputs <- required[!file.exists(required)]
if (length(missing_inputs) > 0) {
  if (!skip_data_downloads) {
    stop(
      "Missing input(s) for the coalition: ", paste(missing_inputs, collapse = ", "), "\n",
      "geo_ideal_points_tbl.rds comes from scripts/12, trade_concentration_tbl.rds from scripts/10."
    )
  }
  message("Skipping the coalition; missing file(s): ", paste(missing_inputs, collapse = ", "))
} else {
  bloc_tbl <- readRDS(bloc_path)
  geo_coalition_tbl <- if (file.exists(geo_coalition_path)) readRDS(geo_coalition_path) else NULL
  trade_tbl <- readRDS(trade_path)
  agreements_tbl <- utils::read.csv(agreements_path, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
  gdp_raw <- utils::read.csv(gdp_path, stringsAsFactors = FALSE, check.names = FALSE)
  ei <- utils::read.csv(ei_path, stringsAsFactors = FALSE)
  cleantech_midstream <- utils::read.csv(cleantech_midstream_path, stringsAsFactors = FALSE, check.names = FALSE)
  uranium_production <- utils::read.csv(uranium_production_path, stringsAsFactors = FALSE)
  critical <- read_iea_critical_minerals(critical_minerals_path)
  mineral_demand <- reserves_build_mineral_demand_clean(critical)
  # EI mine production of the minerals the IEA supply sheet does not cover (manganese, zinc, PGMs):
  # the same sheets and skips as the critical minerals production theme (scripts/10).
  ei_minerals <- if (use_ei_minerals) {
    lapply(critical_minerals_production_specs(), function(spec) {
      list(
        sheet = spec$sheet, mineral = spec$tech_name, nm_col = spec$nm_col,
        data = readxl::read_excel(ei_wide_path, sheet = spec$sheet, skip = spec$skip, .name_repair = "unique_quiet")
      )
    })
  }
  # World reserves, from the Reserves theme's sources (scripts/10): USGS MCS for the critical
  # minerals, the EI sheets, skips and value columns of reserves_specs() for oil, gas and coal, and
  # the Red Book uranium resources in the theme's cost category. The USGS table is Windows-1252 and
  # prints "NA" as a value code, so it is read as text and parsed by usgs_mcs_reserves().
  usgs_reserves <- if (use_reserves) {
    usgs_mcs_reserves(readr::read_csv(
      usgs_mcs_path,
      col_types = readr::cols(.default = readr::col_character()),
      locale = readr::locale(encoding = "windows-1252"),
      na = character(),
      progress = FALSE
    ))
  }
  ei_reserves <- if (use_reserves) {
    lapply(reserves_specs(), function(spec) {
      list(
        sheet = spec$sheet, resource = spec$tech_name, nm_col = spec$nm_col, val_col = spec$val_col,
        data_year = spec$data_year, unit = spec$unit_desc,
        data = readxl::read_excel(ei_wide_path, sheet = spec$sheet, skip = spec$skip, .name_repair = "unique_quiet")
      )
    })
  }
  uranium_resources <- if (use_reserves) utils::read.csv(uranium_resources_path, stringsAsFactors = FALSE)

  # Membership rows must carry a source (the reference file's contract).
  no_source <- is.na(agreements_tbl$source) | is.na(agreements_tbl$url) | is.na(agreements_tbl$accessed)
  if (any(no_source)) {
    stop("economic_security_agreements.csv rows without source/url/accessed: ",
         paste(agreements_tbl$iso3[no_source], agreements_tbl$agreement[no_source], collapse = ", "))
  }
  unknown_agreements <- setdiff(c(tier_cfg$partner_agreements, tier_cfg$sensitivity_agreements), agreements_tbl$agreement)
  if (length(unknown_agreements) > 0) {
    stop("config/coalition.yml names agreements absent from economic_security_agreements.csv: ",
         paste(unknown_agreements, collapse = ", "))
  }

  eu_code <- eu_cfg$code
  major_supplier_shares_tbl <- major_supplier_shares(
    critical = critical,
    mineral_demand = mineral_demand,
    ei_minerals = ei_minerals,
    uranium_production = uranium_production,
    ei = ei,
    cleantech_midstream = cleantech_midstream,
    trade_tbl = trade_tbl,
    ei_reserves = ei_reserves,
    uranium_resources = uranium_resources,
    usgs_reserves = usgs_reserves,
    techs = techs,
    stages = stages,
    supplier_share = supplier_share,
    supplier_reserves_share = supplier_reserves_share,
    mineral_min_demand_share = as.numeric(measures_cfg$mineral_min_demand_share),
    eu_code = eu_code,
    ei_vars = unlist(measures_cfg$ei_production_vars),
    cleantech_tech_map = unlist(measures_cfg$cleantech_tech_map),
    cleantech_status = measures_cfg$cleantech_status,
    cleantech_year = as.integer(measures_cfg$cleantech_year),
    uranium_resources_variable = uranium_resources_variable(URANIUM_DEFAULT_COST_CATEGORY)
  )

  tier_args <- list(
    bloc_tbl = bloc_tbl,
    gdp_tbl = gdp_raw,
    agreements_tbl = agreements_tbl,
    supplier_tbl = major_supplier_shares_tbl,
    gdp_floor_usd = as.numeric(tier_cfg$gdp_floor_usd),
    supplier_share = supplier_share,
    supplier_reserves_share = supplier_reserves_share,
    partner_agreements = unlist(tier_cfg$partner_agreements),
    us_bloc = tier_cfg$us_bloc,
    china_bloc = tier_cfg$china_bloc,
    core_requires_own_vote = isTRUE(tier_cfg$core_requires_own_vote),
    supplier_requires_alignment = isTRUE(tier_cfg$supplier_requires_alignment),
    eu_members = eu_members,
    eu_code = eu_code,
    eu_node_tier = eu_cfg$tier
  )
  coalition_tbl <- do.call(derive_coalition_tiers, tier_args)
  # Names: the canonical name of each ISO3 where the inputs carry none.
  unnamed <- coalition_tbl$country == coalition_tbl$iso3
  coalition_tbl$country[unnamed] <- dplyr::coalesce(country_name_from_iso3(coalition_tbl$iso3[unnamed]), coalition_tbl$iso3[unnamed])

  # Sensitivity: the MSP Forum (sensitivity_agreements) as an additional partner route.
  sensitivity_args <- tier_args
  sensitivity_args$partner_agreements <- unique(c(unlist(tier_cfg$partner_agreements), unlist(tier_cfg$sensitivity_agreements)))
  sensitivity_tbl <- do.call(derive_coalition_tiers, sensitivity_args)
  coalition_tbl$tier_with_sensitivity <- sensitivity_tbl$tier[match(coalition_tbl$iso3, sensitivity_tbl$iso3)]
  coalition_tbl$gdp_floor_usd <- as.numeric(tier_cfg$gdp_floor_usd)
  coalition_tbl$supplier_share <- supplier_share
  coalition_tbl$supplier_reserves_share <- supplier_reserves_share

  country_names <- stats::setNames(coalition_tbl$country, coalition_tbl$iso3)
  eu_stage_weights_tbl <- eu_stage_weights(
    supplier_tbl = major_supplier_shares_tbl,
    gdp_tbl = gdp_raw,
    eu_members = eu_members,
    techs = techs,
    stages = stages,
    basis_priority = lapply(weights_cfg$basis_priority, unlist),
    min_coverage = as.numeric(weights_cfg$min_coverage),
    min_eu_world_share = as.numeric(weights_cfg$min_eu_world_share),
    eu_code = eu_code,
    country_names = country_names
  )

  saveRDS(coalition_tbl, file.path(out_dir, "coalition_tbl.rds"))
  saveRDS(eu_stage_weights_tbl, file.path(out_dir, "eu_stage_weights_tbl.rds"))
  saveRDS(major_supplier_shares_tbl, file.path(out_dir, "major_supplier_shares_tbl.rds"))

  ## Report ----------------------------------------------------------------
  core <- coalition_tbl[coalition_tbl$tier == "core", ]
  partners <- coalition_tbl[coalition_tbl$tier == "partner", ]
  nodes <- unique(coalition_tbl[coalition_tbl$node_tier %in% c("core", "partner"), c("network_node", "node_tier")])
  eu_rows <- coalition_tbl[coalition_tbl$eu_member, ]
  # "agreement (IPEF; MSP); supplier; supplier: reserves": the qualifying agreements go with their
  # route, so they do not read as part of the last one.
  partner_routes <- ifelse(
    is.na(partners$qualifying_agreements), partners$routes,
    mapply(function(r, a) sub("agreement", paste0("agreement (", a, ")"), r, fixed = TRUE),
           partners$routes, partners$qualifying_agreements, USE.NAMES = FALSE)
  )
  message(
    "Coalition (config/coalition.yml; GDP floor ", coalition_usd_bn(as.numeric(tier_cfg$gdp_floor_usd)),
    ", supplier share ", format(100 * supplier_share), "%",
    if (use_reserves) paste0(", reserves ", format(100 * supplier_reserves_share), "%"), "):",
    "\n  Core (", nrow(core), "): ", paste(core$iso3, collapse = " "),
    "\n  Partners (", nrow(partners), "): ",
    paste0(partners$iso3, " [", partner_routes, "]", collapse = ", "),
    "\n  Network nodes (", nrow(nodes), ", EU bundled as ", eu_code, "): ",
    paste0(nodes$network_node, ifelse(nodes$node_tier == "partner", "*", ""), collapse = " "), " (* partner)",
    "\n  EU members by own tier: ",
    paste0(names(table(eu_rows$tier)), " ", as.integer(table(eu_rows$tier)), collapse = ", "),
    "; by bloc: ", paste0(names(table(eu_rows$bloc, useNA = "ifany")), " ", as.integer(table(eu_rows$bloc, useNA = "ifany")), collapse = ", ")
  )
  if (use_reserves) {
    # The reserves leg: the partners it alone brings in (with their holdings), the members it also
    # covers, and the non-members holding between the flow bar and the reserves bar.
    reserves_major <- major_supplier_shares_tbl[major_supplier_shares_tbl$major &
                                                  major_supplier_shares_tbl$measure %in% COALITION_RESERVES_MEASURES, ]
    reserves_detail <- function(i) {
      r <- reserves_major[reserves_major$iso3 == i, ]
      what <- ifelse(!is.na(r$mineral), r$mineral, ifelse(r$measure == "uranium_resources", "Uranium", r$tech))
      r <- unique(data.frame(what = what, share = r$world_share))
      r <- r[order(-r$share), ]
      paste0(r$what, " ", formatC(100 * r$share, format = "f", digits = 1), "%", collapse = ", ")
    }
    reserves_only <- coalition_tbl$iso3[coalition_tbl$tier == "partner" & coalition_tbl$routes == "supplier: reserves"]
    reserves_also <- setdiff(coalition_tbl$iso3[coalition_tbl$route_supplier_reserves], reserves_only)
    below_bar <- coalition_tbl[coalition_tbl$tier == "none" & !is.na(coalition_tbl$max_reserves_share) &
                                 coalition_tbl$max_reserves_share >= supplier_share &
                                 coalition_tbl$max_reserves_share < supplier_reserves_share, ]
    below_bar <- below_bar[order(-below_bar$max_reserves_share), ]
    message(
      "  Reserves leg (>= ", format(100 * supplier_reserves_share), "% of world reserves): ",
      if (length(reserves_only) == 0) "no partner by reserves alone" else
        paste0("partners by reserves alone: ", paste0(reserves_only, " (", vapply(reserves_only, reserves_detail, character(1)), ")", collapse = ", ")),
      "; also passed by members on other routes: ", if (length(reserves_also) == 0) "none" else paste(reserves_also, collapse = " "),
      "; non-members between ", format(100 * supplier_share), "% and the bar: ",
      if (nrow(below_bar) == 0) "none" else
        paste0(below_bar$iso3, " ", formatC(100 * below_bar$max_reserves_share, format = "f", digits = 1), "%",
               ifelse(below_bar$bloc %in% tier_cfg$china_bloc, " (China-leaning)", ""), collapse = ", ")
    )
  }
  excluded <- coalition_tbl[coalition_tbl$supplier_excluded & coalition_tbl$tier == "none", ]
  if (nrow(excluded) > 0) {
    message("  Major suppliers kept off the supplier route (China-leaning or no alignment): ",
            paste0(excluded$iso3, " (", ifelse(is.na(excluded$bloc), "no bloc", excluded$bloc), ")", collapse = ", "))
  }
  china_partners <- partners[!is.na(partners$bloc) & partners$bloc == tier_cfg$china_bloc, ]
  if (nrow(china_partners) > 0) {
    message("  China-leaning partners (agreement route, which has no bloc condition): ",
            paste0(china_partners$iso3, " (", china_partners$qualifying_agreements, ")", collapse = ", "))
  }
  changed <- coalition_tbl[coalition_tbl$tier_with_sensitivity != coalition_tbl$tier, ]
  message("  Sensitivity (", paste(unlist(tier_cfg$sensitivity_agreements), collapse = ", "), " as a partner route): ",
          if (nrow(changed) == 0) "no change" else paste0(changed$iso3, " ", changed$tier, " -> ", changed$tier_with_sensitivity, collapse = ", "))
  if (!is.null(geo_coalition_tbl) && all(c("iso3", "member") %in% names(geo_coalition_tbl))) {
    geo_core <- sort(geo_coalition_tbl$iso3[geo_coalition_tbl$member])
    if (!identical(geo_core, sort(core$iso3))) {
      warning(
        "Core tier differs from geo_coalition_tbl members: only in core ",
        paste(setdiff(core$iso3, geo_core), collapse = " "), "; only in geo_coalition_tbl ",
        paste(setdiff(geo_core, core$iso3), collapse = " "),
        call. = FALSE
      )
    }
  }
  basis_counts <- unique(eu_stage_weights_tbl[c("tech", "supply_chain", "basis")])
  message("  EU stage weights: ", paste0(names(table(basis_counts$basis)), " ", as.integer(table(basis_counts$basis)), collapse = ", "),
          " of ", nrow(basis_counts), " (tech, stage) cells")
  message("  Written to ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))

  rm(
    bloc_tbl, geo_coalition_tbl, trade_tbl, agreements_tbl, gdp_raw, ei, cleantech_midstream,
    uranium_production, uranium_resources, critical, mineral_demand, ei_minerals, ei_reserves, usgs_reserves, tier_args,
    sensitivity_args, sensitivity_tbl, core, partners, partner_routes, nodes, eu_rows, excluded, china_partners, changed,
    basis_counts, country_names, unnamed, no_source, unknown_agreements
  )
}
rm(list = intersect(c(
  "coalition_settings", "tier_cfg", "measures_cfg", "eu_cfg", "weights_cfg", "techs", "stages",
  "eu_members", "eu_code", "out_dir", "agreements_path", "uranium_production_path", "uranium_resources_path",
  "gdp_path", "ei_path", "cleantech_midstream_path", "use_ei_minerals", "use_reserves", "supplier_share",
  "supplier_reserves_share", "ei_wide_path", "usgs_mcs_path", "critical_minerals_path", "bloc_path", "geo_coalition_path",
  "trade_path", "required", "missing_inputs", "geo_core", "reserves_major", "reserves_detail", "reserves_only",
  "reserves_also", "below_bar"
), ls()))
