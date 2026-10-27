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


# IEA scenario pathways (World) mapped to technology x stage (user request of 2026-10-17: IEA Net Zero
# data for the Climate section). Runs after scripts/16; scripts/87 reads its outputs.
#
#   Rscript scripts/17_build_iea_pathways.R
#
# Inputs (data/raw; both CC BY 4.0):
# - WEO<yyyy>_AnnexA_Free_Dataset_World.csv, newest vintage (manifest weo_annex_a): World Energy Outlook
#   Annex A, World: Historical and the Stated Policies, Current Policies and Net Zero Emissions by 2050
#   scenarios. The previous vintage, when staged, is read for a vintage check only (never shipped).
# - IEA Critical Minerals Dataset <yyyy>.xlsx, newest vintage (manifest iea_critical_minerals), read with
#   read_iea_critical_minerals(): mineral demand by technology and scenario (Current Policies, Stated
#   Policies, High Demand; the dataset has no Net Zero scenario).
# - config/iea_pathways_map.yml (series, cell map, match quality) and the cells of
#   config/sector_dictionary.yml.
# Outputs (data/processed):
# - iea_pathways_series_tbl.rds    one row per series: id, group, label, unit, category, product, flow
# - iea_pathways_values_tbl.rds    series, scenario (historical / stated / current / nze), year, value (World)
# - iea_pathways_map_tbl.rds       cell, series, role (own / demand / context), match (NA for context), order
# - iea_pathways_cells_tbl.rds     cell, match (best own / demand series; none), note, mineral_techs
# - iea_pathways_minerals_tbl.rds  sheet (1 / 3.1 / 3.2), mineral, tech, scenario (current / stated /
#                                  high_demand), year, kt
# - iea_pathways_meta_tbl.rds      the editions read (weo, weo_previous, minerals) and the base year
# - iea_pathways_vintage_tbl.rds   the vintage check: series, scenario, year, new, old, rel
# IO only: the rules live in R/categories/climate/iea_pathways.R.

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "climate", "iea_pathways.R"))

raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)

map <- yaml::read_yaml(file.path(repo_root, "config", "iea_pathways_map.yml"))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))

weo_pattern <- "^WEO\\d{4}_AnnexA_Free_Dataset_World\\.csv$"
weo_path <- resolve_versioned_raw_input(raw_data_path, pattern = weo_pattern, fallback = "WEO2025_AnnexA_Free_Dataset_World.csv", label = "IEA WEO Annex A")
if (!file.exists(weo_path)) stop("Missing ", weo_path)
read_weo <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
weo <- read_weo(weo_path)
weo_edition <- iea_pathways_weo_edition(weo)

# The previous vintage (vintage check only).
weo_all <- list.files(raw_data_path, pattern = weo_pattern)
weo_all <- weo_all[order(vapply(weo_all, raw_input_vintage_key, numeric(1)), decreasing = TRUE)]
weo_prev <- setdiff(weo_all, basename(weo_path))

cm_path <- resolve_versioned_raw_input(raw_data_path, pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
                                       fallback = "IEA Critical Minerals Dataset 2026.xlsx", label = "IEA Critical Minerals Dataset")
if (!file.exists(cm_path)) stop("Missing ", cm_path)
# xlsx inputs are copied before reading: OneDrive and Excel lock files in data/raw.
cm_copy <- file.path(tempdir(), basename(cm_path))
if (!file.copy(cm_path, cm_copy, overwrite = TRUE)) stop("Could not copy ", cm_path)
critical <- read_iea_critical_minerals(cm_copy, scenario = NULL, quiet = TRUE)

pw <- iea_pathways_weo(weo, map)
minerals <- iea_pathways_minerals(critical, map)
problems <- iea_pathways_map_validate(map, dict_cells, iea_pathways_mineral_techs(minerals))
if (length(problems)) stop("config/iea_pathways_map.yml:\n- ", paste(problems, collapse = "\n- "))
map_tbl <- iea_pathways_map_tbl(map)
cells_tbl <- iea_pathways_cells_tbl(map, map_tbl, dict_cells)

vintage <- tibble::tibble(series = character(), scenario = character(), year = integer(), new = double(), old = double(), rel = double())
prev_edition <- NA_character_
if (length(weo_prev)) {
  old <- read_weo(file.path(raw_data_path, weo_prev[1]))
  prev_edition <- iea_pathways_weo_edition(old)
  vintage <- iea_pathways_vintage(pw$values, old, map)
}

meta <- tibble::tibble(
  key = c("weo", "weo_file", "weo_previous", "minerals_file", "minerals_base_year"),
  value = c(weo_edition, basename(weo_path), prev_edition, basename(cm_path), as.character(iea_critical_minerals_base_year(critical)))
)

out <- list(
  iea_pathways_series_tbl = pw$series,
  iea_pathways_values_tbl = pw$values,
  iea_pathways_map_tbl = map_tbl,
  iea_pathways_cells_tbl = cells_tbl,
  iea_pathways_minerals_tbl = minerals,
  iea_pathways_meta_tbl = meta,
  iea_pathways_vintage_tbl = vintage
)
for (nm in names(out)) saveRDS(out[[nm]], file.path(processed_dir, paste0(nm, ".rds")))

message("IEA pathways: ", weo_edition, ", ", nrow(pw$series), " World series (", nrow(pw$values), " values; scenarios ",
        paste(unique(pw$values$scenario), collapse = ", "), "; years ", paste(sort(unique(pw$values$year)), collapse = ", "), "); ",
        sum(cells_tbl$match != "none"), " of ", nrow(cells_tbl), " cells matched (", paste(names(table(cells_tbl$match)), table(cells_tbl$match), collapse = ", "), "); ",
        nrow(minerals), " mineral demand rows (", paste(unique(minerals$scenario), collapse = ", "), ").")
if (nrow(vintage)) {
  big <- vintage[!is.na(vintage$rel) & abs(vintage$rel) > 0.25, , drop = FALSE]
  message("Vintage check against ", prev_edition, ": ", nrow(vintage), " shared (series, scenario, year) values; median |change| ",
          sprintf("%.1f%%", 100 * stats::median(abs(vintage$rel), na.rm = TRUE)), "; ", nrow(big), " move by more than 25%",
          if (nrow(big)) paste0(" (", paste(utils::head(sprintf("%s %s %d %+.0f%%", big$series, big$scenario, big$year, 100 * big$rel), 8), collapse = "; "), ")") else "", ".")
}
