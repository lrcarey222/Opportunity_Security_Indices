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


# Ember China Cleantech Export Data against the Comtrade view of imports from China (prototype).
#
#   Rscript scripts/36_build_ember_china_exports.R
#
# Rules: R/strategic_position/ember_china_exports.R. Settings by environment variable:
#   OPSI_EMBER_BASE_YEAR  Comtrade year to compare with and roll forward from (default 2024)
#   OPSI_EMBER_MONTHS     length of the latest Ember window in months (default 12)
#
# Inputs:
# - data/raw/ember/clean_tech_exports_full_release_monthly_lower.csv  Ember, China Cleantech Export Data
#   (CC BY 4.0; config/raw_inputs_manifest.yml, ember_china_cleantech_exports). Downloaded when missing unless
#   OPSI_SKIP_FETCH is set.
# - data/reference/energy_hs6_master.csv  the basket (tech x stage x HS6).
# - data/raw/comtrade_energy_dyads_all.csv  Comtrade reporter x partner x HS6 (scripts/98). Optional: without
#   it the comparison and nowcast are skipped.
# - data/processed/import_exposure_tbl.rds  (scripts/15) Optional: the cell-level Comtrade China share and
#   imports, for context and coverage.
#
# Outputs:
# - data/processed/ember_china_exports_cells_tbl.rds  destination x tech x stage: China's exports in the
#   latest window and base year, growth, codes used.
# - data/processed/ember_china_exports_nowcast_tbl.rds  importer x tech x stage: China share of the
#   Ember-covered codes in the base year and rolled forward, with the cell's full Comtrade China share.
# - diagnostics/ember_china_exports/: code_map.csv, comparison_by_code.csv, comparison_detail.csv,
#   nowcast_top_changes.csv.

library(dplyr)

for (f in c("scripts/utils/fetchers.R", "R/strategic_position/trade_restrictiveness.R",
            "R/strategic_position/ember_china_exports.R")) source(file.path(repo_root, f))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
processed_dir <- file.path(repo_root, config$processed_dir)
raw_data_path <- file.path(repo_root, config$raw_data_dir)
diag_dir <- file.path(repo_root, "diagnostics", "ember_china_exports")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

base_year <- as.integer(Sys.getenv("OPSI_EMBER_BASE_YEAR", "2024"))
months <- as.integer(Sys.getenv("OPSI_EMBER_MONTHS", "12"))
ember_url <- "https://files.ember-energy.org/public-downloads/customs/outputs/clean_tech_exports_full_release_monthly_lower.csv"
ember_path <- file.path(raw_data_path, "ember", "clean_tech_exports_full_release_monthly_lower.csv")
if (!file.exists(ember_path)) {
  if (!opsi_fetch_enabled()) stop(ember_path, " not found; stage it or unset OPSI_SKIP_FETCH to download it.")
  dir.create(dirname(ember_path), recursive = TRUE, showWarnings = FALSE)
  message("Downloading Ember China cleantech export data ...")
  utils::download.file(ember_url, ember_path, mode = "wb", quiet = TRUE)
}

raw <- utils::read.csv(ember_path, check.names = FALSE, colClasses = c("Commodity code" = "character"))
ember <- ember_cte_prepare(raw)
rm(raw)
unresolved <- unique(ember$area[!ember$is_region & is.na(ember$iso3)])
if (length(unresolved)) message("Ember destinations without ISO3 (dropped): ", paste(unresolved, collapse = ", "), ".")
message("Ember: ", length(unique(ember$iso3[!is.na(ember$iso3)])), " destinations, ", length(unique(ember$code)), " codes, ",
        format(min(ember$date), "%Y-%m"), " to ", format(max(ember$date), "%Y-%m"), ".")

master <- utils::read.csv(file.path(repo_root, "data", "reference", "energy_hs6_master.csv"), colClasses = "character",
                          check.names = FALSE, na.strings = character())
master$hs6 <- formatC(as.integer(master$hs6), width = 6, flag = "0")
basket <- tri_basket_cells(master, unique(master$tech))
code_cells <- ember_cte_code_cells(ember, basket)
unmatched <- attr(code_cells, "unmatched")
message("Basket: ", length(unique(code_cells$code)), " Ember codes map to ", nrow(unique(code_cells[c("tech", "stage")])),
        " cells; outside the basket: ", paste(unique(unmatched$sub_category), collapse = ", "), ".")
utils::write.csv(rbind(code_cells, data.frame(code = unmatched$code, hs6 = substr(unmatched$code, 1, 6),
                                              partial_hs6 = nchar(unmatched$code) > 6, category = unmatched$category,
                                              sub_category = unmatched$sub_category, tech = NA, stage = NA, n_cells = 0L)),
                 file.path(diag_dir, "code_map.csv"), row.names = FALSE)

windows <- ember_cte_code_windows(ember, base_year = base_year, months = months)
cells <- ember_cte_cells(windows, code_cells)
attr(cells, "source") <- "Ember, China Cleantech Export Data (CC BY 4.0)"
attr(cells, "base_year") <- base_year
saveRDS(cells, file.path(processed_dir, "ember_china_exports_cells_tbl.rds"))
message("Cells: ", nrow(cells), " destination x cell rows; latest window ends ", cells$latest_end[1], ".")

dyad_path <- file.path(raw_data_path, "comtrade_energy_dyads_all.csv")
if (!file.exists(dyad_path)) {
  warning(dyad_path, " not found: comparison and nowcast skipped.")
} else {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required to read the dyad file.")
  full_codes <- unique(code_cells$hs6[!code_cells$partial_hs6])
  d <- data.table::fread(dyad_path, select = c("period", "reporter_iso", "flow_code", "partner_iso", "cmd_code", "primary_value"),
                         colClasses = list(character = "cmd_code"))
  d <- as.data.frame(d[d$period == base_year & d$flow_code == "M" & !d$partner_iso %in% c("W00", "", NA), ])
  if (nrow(d) == 0) stop("No Comtrade ", base_year, " imports in ", dyad_path, ".")
  d$hs6 <- formatC(as.integer(d$cmd_code), width = 6, flag = "0")
  d <- d[d$hs6 %in% full_codes & d$reporter_iso != d$partner_iso, , drop = FALSE]
  d$china <- ifelse(d$partner_iso == "CHN", d$primary_value, 0)
  ct <- d %>% group_by(reporter_iso, hs6) %>%
    summarise(imports_from_china = sum(china, na.rm = TRUE), imports_total = sum(primary_value, na.rm = TRUE), .groups = "drop")
  rm(d)
  invisible(gc())

  cmp <- ember_cte_compare(windows, code_cells, ct)
  cmp_sum <- ember_cte_compare_summary(cmp)
  cmp_sum <- merge(unique(code_cells[c("hs6", "category", "sub_category")]), cmp_sum, by = "hs6")
  utils::write.csv(cmp, file.path(diag_dir, "comparison_detail.csv"), row.names = FALSE)
  utils::write.csv(cmp_sum[order(-cmp_sum$comtrade_usd), ], file.path(diag_dir, "comparison_by_code.csv"), row.names = FALSE)
  message("Comparison (", base_year, ", full HS6 codes): Ember / Comtrade = ",
          round(sum(cmp_sum$ember_usd) / sum(cmp_sum$comtrade_usd), 3), " over ",
          length(unique(cmp$iso3[cmp$comtrade_reported])), " reporters.")

  # Nowcast over the full-HS6 Ember codes of each cell: Comtrade base-year shares, Ember growth.
  full_cells <- code_cells[!code_cells$partial_hs6, , drop = FALSE]
  base <- merge(ct, unique(full_cells[c("hs6", "tech", "stage")]), by = "hs6") %>%
    group_by(iso3 = reporter_iso, tech, stage) %>%
    summarise(china_usd = sum(imports_from_china), total_usd = sum(imports_total), .groups = "drop")
  growth_full <- ember_cte_cells(windows[windows$code %in% full_cells$code, , drop = FALSE], full_cells)
  nowcast <- ember_cte_nowcast_share(as.data.frame(base), growth_full)
  exp_path <- file.path(processed_dir, "import_exposure_tbl.rds")
  if (file.exists(exp_path)) {
    ex <- readRDS(exp_path)
    ex <- data.frame(iso3 = ex$importer_iso, tech = ex$tech, stage = ex$supply_chain, cell_imports_usd = ex$imports_usd,
                     cell_china_share = ex$china_share, cell_trade_year = ex$trade_year)
    nowcast <- merge(nowcast, ex, by = c("iso3", "tech", "stage"), all.x = TRUE)
    nowcast$coverage <- ifelse(nowcast$cell_imports_usd > 0, pmin(nowcast$total_usd / nowcast$cell_imports_usd, 1), NA_real_)
  } else {
    message("import_exposure_tbl.rds not found: nowcast written without cell-level context.")
  }
  attr(nowcast, "base_year") <- base_year
  saveRDS(nowcast, file.path(processed_dir, "ember_china_exports_nowcast_tbl.rds"))
  big <- nowcast[nowcast$total_usd >= 1e8 & !is.na(nowcast$share_change), , drop = FALSE]
  utils::write.csv(big[order(-abs(big$share_change)), ], file.path(diag_dir, "nowcast_top_changes.csv"), row.names = FALSE)
  message("Nowcast: ", nrow(nowcast), " importer x cell rows; ", nrow(big), " with >= $100m base imports.")
}
