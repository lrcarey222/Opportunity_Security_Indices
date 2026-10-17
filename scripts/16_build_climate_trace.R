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

# Climate TRACE sector emissions mapped to technology x stage (user request of 2026-10-17). Runs
# after scripts/10 (tech_ghg_tbl.rds) and the Climate TRACE fetch; scripts/87 reads its outputs.
#
#   Rscript scripts/16_build_climate_trace.R
#
# Inputs:
# - data/raw/climate_trace_country_emissions.csv, data/raw/climate_trace_asset_activity.csv
#   (scripts/fetchers/climate_trace.R; Climate TRACE, CC BY 4.0).
# - config/climate_trace_map.yml (what maps to which cell, with a match quality per cell) and the
#   cell list of config/sector_dictionary.yml.
# - data/processed/tech_ghg_tbl.rds (scripts/10) and data/raw/ipcc_ghg_intensity.csv: the IPCC
#   lifecycle intensity the views show for cells with no Climate TRACE match.
# Outputs (data/processed):
# - climate_trace_country_tbl.rds    iso3 (WLD = Climate TRACE's world total), sector, subsector, year,
#                                    co2e_100yr, co2, ch4, n2o, co2e_20yr (t), repeated (the world
#                                    total equals the previous year's: an estimate carried forward)
# - climate_trace_assets_tbl.rds     the tracked assets per (iso3, subsector, asset_group, year)
# - climate_trace_sources_tbl.rds    the sources the map uses (src_id, subsector, sector, label,
#                                    asset_group, basis)
# - climate_trace_map_tbl.rds        one row per (cell, source): role, match
# - climate_trace_cells_tbl.rds      one row per cell: tech, stage, match (best own source; none), note
# - climate_trace_emissions_tbl.rds  co2e_100yr per (src_id, iso3, year) - the values the views show
# - climate_trace_intensity_tbl.rds  the tracked assets' emissions over their activity per
#                                    (subsector, asset_group, iso3, year), for the subsectors mapped
# - climate_trace_lifecycle_tbl.rds  per technology: ghg_index (tech_ghg_tbl), basis (ipcc / proxy),
#                                    the IPCC gCO2e/kWh range and rows behind it
# IO only: the rules live in R/categories/climate/climate_trace.R.

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "climate", "climate_trace.R"))
# IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH, the oil correction tech_ghg_tbl was built with.
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))

raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)
read_raw <- function(p) {
  if (!file.exists(p)) stop("Missing ", p, ": run Rscript scripts/fetchers/climate_trace.R")
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
}

map <- yaml::read_yaml(file.path(repo_root, "config", "climate_trace_map.yml"))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))
problems <- climate_trace_map_validate(map, dict_cells)
if (length(problems)) stop("config/climate_trace_map.yml:\n- ", paste(problems, collapse = "\n- "))

country <- tibble::as_tibble(read_raw(file.path(raw_data_path, "climate_trace_country_emissions.csv")))
assets <- tibble::as_tibble(read_raw(file.path(raw_data_path, "climate_trace_asset_activity.csv")))
country$year <- as.integer(country$year)
assets$year <- as.integer(assets$year)
assets$asset_group <- as.character(assets$asset_group)

# Every (subsector, year) the config asks for must be in the staged files: a partial fetch stops here.
want <- expand.grid(subsector = vapply(map$fetch$country_subsectors, function(s) s$subsector, character(1)),
                    year = as.integer(unlist(map$fetch$years)), stringsAsFactors = FALSE)
have <- unique(country[country$iso3 == "WLD", c("subsector", "year")])
gap <- dplyr::anti_join(want, have, by = c("subsector", "year"))
if (nrow(gap)) stop("climate_trace_country_emissions.csv lacks: ", paste(gap$subsector, gap$year, collapse = ", "))
want_a <- expand.grid(subsector = vapply(map$fetch$asset_subsectors, function(s) s$subsector, character(1)),
                      year = as.integer(unlist(map$fetch$years)), stringsAsFactors = FALSE)
gap_a <- dplyr::anti_join(want_a, unique(assets[c("subsector", "year")]), by = c("subsector", "year"))
if (nrow(gap_a)) stop("climate_trace_asset_activity.csv lacks: ", paste(gap_a$subsector, gap_a$year, collapse = ", "))

# The world row is Climate TRACE's own total; it should be the sum of its countries.
wsum <- dplyr::summarise(dplyr::group_by(country[country$iso3 != "WLD", ], .data$subsector, .data$year),
                         s = sum(.data$co2e_100yr, na.rm = TRUE), .groups = "drop")
wchk <- dplyr::inner_join(wsum, country[country$iso3 == "WLD", c("subsector", "year", "co2e_100yr")], by = c("subsector", "year"))
off <- wchk[abs(wchk$s - wchk$co2e_100yr) > 1e-6 * pmax(wchk$co2e_100yr, 1), , drop = FALSE]
if (nrow(off)) message("Note: Climate TRACE's world total differs from the sum of its countries for ", nrow(off), " subsector-years (", paste(utils::head(paste(off$subsector, off$year), 6), collapse = ", "), ")")

country_tbl <- dplyr::left_join(country[CLIMATE_TRACE_COUNTRY_COLUMNS], climate_trace_repeats(country), by = c("subsector", "year"))
map_tbl <- climate_trace_map_tbl(map)
sources_tbl <- climate_trace_sources_tbl(map, map_tbl)
cells <- tibble::tibble(slug = dict_cells, stage = tools::toTitleCase(sub("^.*-", "", dict_cells)),
                        tech = tools::toTitleCase(gsub("-", " ", sub("-[a-z]+$", "", dict_cells))))
cells_tbl <- climate_trace_cells_tbl(map, map_tbl, cells)
emissions_tbl <- climate_trace_emissions_tbl(sources_tbl, country, assets)
# Intensity of the asset groups the sources use: every group of a subsector read as country totals
# (aluminium: smelters and refineries), the one group of an assets source (coal-fired plants).
used <- assets$subsector %in% sources_tbl$subsector[sources_tbl$basis == "country"] |
  climate_trace_src_id(assets$subsector, assets$asset_group) %in% sources_tbl$src_id[sources_tbl$basis == "assets"]
intensity_tbl <- climate_trace_intensity_tbl(assets[used, , drop = FALSE])
lifecycle_tbl <- climate_trace_lifecycle(
  utils::read.csv(file.path(raw_data_path, "ipcc_ghg_intensity.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
  readRDS(file.path(processed_dir, "tech_ghg_tbl.rds")),
  IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH
)

out <- list(
  climate_trace_country_tbl = country_tbl,
  climate_trace_assets_tbl = assets[CLIMATE_TRACE_ASSET_COLUMNS],
  climate_trace_sources_tbl = sources_tbl,
  climate_trace_map_tbl = map_tbl[c("cell", "src_id", "role", "match", "order")],
  climate_trace_cells_tbl = cells_tbl,
  climate_trace_emissions_tbl = emissions_tbl,
  climate_trace_intensity_tbl = intensity_tbl,
  climate_trace_lifecycle_tbl = lifecycle_tbl
)
for (nm in names(out)) saveRDS(out[[nm]], file.path(processed_dir, paste0(nm, ".rds")))

latest <- max(country$year)
rep_latest <- country_tbl$subsector[country_tbl$iso3 == "WLD" & country_tbl$year == latest & country_tbl$repeated]
message("Climate TRACE: ", length(unique(country$iso3)) - 1L, " countries, ", length(unique(country$subsector)), " subsectors, ",
        min(country$year), "-", latest, "; ", nrow(sources_tbl), " sources mapped to ", sum(cells_tbl$match != "none"), " of ",
        nrow(cells_tbl), " cells (", paste(names(table(cells_tbl$match)), table(cells_tbl$match), collapse = ", "), ").",
        if (length(rep_latest)) paste0(" ", latest, " repeats ", latest - 1L, " for: ", paste(rep_latest, collapse = ", "), ".") else "")
