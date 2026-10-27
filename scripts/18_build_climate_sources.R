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


# Climate sources for the Climate sections (user request of 2026-10-17: seven files added to data/raw).
# Runs after scripts/17 (it checks the NZE 2021 pairs against config/iea_pathways_map.yml); scripts/87
# reads its outputs.
#
#   Rscript scripts/18_build_climate_sources.R
#
# Inputs (data/raw; newest vintage of each pattern; licences in config/climate_sources_map.yml):
# - National_Fossil_Carbon_Emissions_<yyyy>_v<x>.xlsx and Global_Carbon_Budget_<yyyy>_v<x>.xlsx (Global
#   Carbon Project; cite Friedlingstein et al.): national fossil CO2 (territorial, consumption, transfers)
#   and World fossil CO2 by fuel.
# - <yyyy> Country Transition Tracker Data.xlsx (Energy Institute Statistical Review).
# - WEO<yyyy>_AnnexA_Free_Dataset_Regions.csv (IEA, CC BY-NC-SA 4.0): regional pathways.
# - NZE2021_AnnexA.csv (IEA, Net Zero by 2050, 2021; CC BY-NC-SA 3.0 IGO): 2030 milestones.
# - WorldEnergyInvestment<yyyy>_DataFile*.xlsx (IEA, CC BY 4.0): energy investment by region.
# - config/climate_sources_map.yml, config/iea_pathways_map.yml (series ids) and the cells of
#   config/sector_dictionary.yml.
# Not read: IEA_Energy_ValueAdded.xlsx (licence-restricted: external dissemination needs IEA permission).
# Outputs (data/processed, climate_src_<name>_tbl.rds):
# - gcp_national  iso3 (EUU, WLD for the EU27 and World), name, measure (territorial / consumption /
#                 transfers), year, value (Mt CO2), from 1990
# - gcp_fuel      fuel (total, coal, oil, gas, cement, flaring, other), year, value (Gt CO2, World), from 1960
# - ei            iso3, name, ind, year, value (Country Transition Tracker, as published)
# - ei_ind        id, label, unit, better, sheet, low_carbon
# - ei_median     ind, year, value (median of every economy the tracker lists), n
# - weo_series    id, label, unit, category, product, flow
# - weo_values    region, series, scenario (historical / stated / current / nze), year, value
# - weo_regions   region, kind (country / union / region / world)
# - weo_geo       iso3, region, match (own / member / by_subregion), for every ISO3 economy countrycode knows
# - nze21_series  id, label, unit, category, product, flow, weo (paired WEO 2025 World series), match, note
# - nze21_values  series, year, value (World, Net Zero Emissions by 2050, 2021 edition)
# - wei_rows      id, label (as published), name, parent, unit
# - wei_values    row, region, year, value, estimate (2026 = IEA estimate)
# - wei_regions   region (sheet name), label (as the sheet heads it), kind
# - map           cell, src (wei / gcp), key, role, match, order
# - cells         cell, match (best WEI row; none), note
# - meta          key, value: editions and files read
# IO only: the rules live in R/categories/climate/climate_sources.R.

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "climate", "climate_sources.R"))

raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)

map <- yaml::read_yaml(file.path(repo_root, "config", "climate_sources_map.yml"))
weo_ids <- vapply(yaml::read_yaml(file.path(repo_root, "config", "iea_pathways_map.yml"))$series, function(s) s$id, character(1))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))
problems <- climate_sources_map_validate(map, dict_cells, weo_ids)
if (length(problems)) stop("config/climate_sources_map.yml:\n- ", paste(problems, collapse = "\n- "))

need <- function(p) {
  if (!file.exists(p)) stop("Missing ", p)
  p
}
gcp_nat_path <- need(resolve_versioned_raw_input(raw_data_path, pattern = "^National_Fossil_Carbon_Emissions_\\d{4}_v[0-9.]+\\.xlsx$", label = "GCP national fossil CO2"))
gcp_gcb_path <- need(resolve_versioned_raw_input(raw_data_path, pattern = "^Global_Carbon_Budget_\\d{4}_v[0-9.]+\\.xlsx$", label = "GCP Global Carbon Budget"))
ei_path <- need(resolve_versioned_raw_input(raw_data_path, pattern = "^\\d{4} Country Transition Tracker Data\\.xlsx$", label = "EI Country Transition Tracker"))
weo_path <- need(resolve_versioned_raw_input(raw_data_path, pattern = "^WEO\\d{4}_AnnexA_Free_Dataset_Regions\\.csv$", label = "IEA WEO Annex A regions"))
nze_path <- need(file.path(raw_data_path, "NZE2021_AnnexA.csv"))
wei_path <- need(resolve_versioned_raw_input(raw_data_path, pattern = "^WorldEnergyInvestment\\d{4}_DataFile.*\\.xlsx$", label = "IEA World Energy Investment data file"))

# xlsx inputs are copied before reading: OneDrive and Excel lock files in data/raw.
read_sheets <- function(path, sheets = NULL) {
  cp <- file.path(tempdir(), paste0("cs_", gsub("[^A-Za-z0-9.]", "_", basename(path))))
  if (!file.copy(path, cp, overwrite = TRUE)) stop("Could not copy ", path)
  all <- readxl::excel_sheets(cp)
  sheets <- sheets %||% all
  miss <- setdiff(sheets, all)
  if (length(miss)) stop(basename(path), ": no sheet(s) ", paste(miss, collapse = ", "), " (renamed?)")
  out <- lapply(sheets, function(s) suppressMessages(readxl::read_excel(cp, sheet = s, col_names = FALSE, col_types = "list")))
  names(out) <- sheets
  out
}
to_iso3 <- function(x) suppressWarnings(country_name_to_iso3(x))

# Global Carbon Project.
gs <- read_sheets(gcp_nat_path, unname(unlist(map$gcp$national_sheets)))
names(gs) <- names(map$gcp$national_sheets)[match(names(gs), unlist(map$gcp$national_sheets))]
gcp_national <- climate_sources_gcp_national(gs, map, to_iso3)
gcp_fuel <- climate_sources_gcp_fuel(read_sheets(gcp_gcb_path, map$gcp$fuel_sheet)[[1]], map)

# Energy Institute Country Transition Tracker.
ei_sheets <- unique(c(vapply(map$ei$indicators, function(x) x$sheet, character(1)), map$ei$power$sheet))
ei <- climate_sources_ei(read_sheets(ei_path, ei_sheets), map, to_iso3)
ei_ind <- climate_sources_ei_indicators(map)
ei_median <- climate_sources_ei_median(ei$values)

# IEA WEO 2025 regions, mapped to every ISO3 economy (scripts/87 keeps those of dims.geo).
weo <- utils::read.csv(weo_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
wr <- climate_sources_weo_regions(weo, map)
geo_codes <- sort(unique(c(stats::na.omit(countrycode::codelist$iso3c), "XKX", gcp_national$iso3, ei$values$iso3)))
geo_codes <- geo_codes[!geo_codes %in% c("EUU", "WLD")]
subregion <- function(x) suppressWarnings(countrycode::countrycode(x, "iso3c", "un.regionsub.name", warn = FALSE))
weo_geo <- climate_sources_weo_geo(geo_codes, map, subregion)

# IEA Net Zero by 2050 (2021) and World Energy Investment.
nze <- utils::read.csv(nze_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
nz <- climate_sources_nze2021(nze, map)
wei <- climate_sources_wei(read_sheets(wei_path, names(map$wei$regions)), map)

map_tbl <- climate_sources_map_tbl(map)
cells_tbl <- climate_sources_cells_tbl(map, map_tbl, dict_cells)

meta <- tibble::tibble(
  key = c("gcp", "gcp_national_file", "gcp_budget_file", "ei", "ei_file", "weo_regions", "weo_regions_file", "nze2021", "nze2021_file", "wei", "wei_file"),
  value = c(sub("_v([0-9.]+)\\.xlsx$", " v\\1", sub("^National_Fossil_Carbon_Emissions_", "Global Carbon Budget ", basename(gcp_nat_path))),
            basename(gcp_nat_path), basename(gcp_gcb_path),
            sub(" Data\\.xlsx$", "", basename(ei_path)), basename(ei_path),
            map$weo_regions$publication, basename(weo_path),
            paste0(map$nze2021$publication, " (2021)"), basename(nze_path),
            sub("^WorldEnergyInvestment(\\d{4}).*$", "World Energy Investment \\1", basename(wei_path)), basename(wei_path))
)

out <- list(
  gcp_national = gcp_national, gcp_fuel = gcp_fuel, ei = ei$values, ei_ind = ei_ind, ei_median = ei_median,
  weo_series = wr$series, weo_values = wr$values, weo_regions = wr$regions, weo_geo = weo_geo,
  nze21_series = nz$series, nze21_values = nz$values, wei_rows = wei$rows, wei_values = wei$values, wei_regions = wei$regions,
  map = map_tbl, cells = cells_tbl, meta = meta
)
for (nm in names(out)) saveRDS(out[[nm]], file.path(processed_dir, paste0("climate_src_", nm, "_tbl.rds")))

tl <- function(x) paste(names(table(x)), table(x), collapse = ", ")
message("Climate sources: GCP ", nrow(gcp_national), " national values (", length(unique(gcp_national$iso3)), " geos, ",
        min(gcp_national$year), "-", max(gcp_national$year), "; ", tl(gcp_national$measure), "), ", nrow(gcp_fuel), " World fuel values; EI ",
        nrow(ei$values), " values (", length(unique(ei$values$iso3)), " economies, ", length(unique(ei$values$ind)), " indicators); sheet units as published: ",
        paste(sprintf("%s = %s", ei$units$sheet, ei$units$unit), collapse = "; "), ".")
message("WEO regions: ", nrow(wr$values), " values (", length(unique(wr$values$region)), " regions x ", nrow(wr$series), " series; ", tl(wr$values$scenario),
        "); economies mapped to a region: ", nrow(weo_geo), " (", tl(weo_geo$match), "). NZE 2021: ", nrow(nz$series), " paired series. WEI: ",
        nrow(wei$values), " values (", nrow(wei$rows), " rows x ", nrow(wei$regions), " regions, ", min(wei$values$year), "-", max(wei$values$year),
        "). Cells: ", tl(cells_tbl$match), "; GCP fuel context on ", length(unique(map_tbl$cell[map_tbl$src == "gcp"])), " cells.")

