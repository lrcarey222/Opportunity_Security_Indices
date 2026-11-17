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


# Current import exposure: the Strategic Need / posture import shares (sp_import_exposure()) rebuilt on each
# importer's latest 12 months of Comtrade data and set against the 2024 table.
#
#   Rscript scripts/38_build_current_import_exposure.R
#
# Runs after scripts/37 (the monthly pull) and scripts/12 (geo alignment). Both years go through the same
# reconciled flow builder as scripts/15 (partnership_strength_build_trade_cells(): the importer's own
# declarations, else exporters' as mirrors), so the 2024 side reproduces data/processed/import_exposure_tbl.rds
# and any difference is the data, not the method. Rules: R/strategic_position/current_trade.R.
#
# Inputs: data/raw/comtrade_monthly_dyads.csv.gz and _windows.csv (scripts/37); data/raw/comtrade_energy_dyads_all.csv
# (scripts/98, its 2024 rows); data/raw/energy_hs6_master.csv (the stage crosswalk scripts/15 uses);
# data/raw/wdi_gdp.csv; data/processed/geo_alignment_tbl.rds; config/partnership.yml (cif_fob_factor) and
# config/strategic_position.yml (coercive powers). Optional: data/processed/import_exposure_tbl.rds, to
# check the 2024 side reproduces it.
# Settings: OPSI_CURRENT_TRADE_BASE_YEAR (2024), OPSI_CURRENT_TRADE_MIN_MONTHS (10).
#
# Outputs:
# - data/processed/current_import_exposure_tbl.rds  sp_import_exposure() on the T12M flows, with
#   window_start/window_end/months_used per importer (rows from mirrors are kept but flagged).
# - data/processed/import_exposure_compare_tbl.rds  ctm_exposure_compare(): base vs current per importer x cell.
# - data/processed/current_supplier_panel_tbl.rds  ctm_panel_supplier_shares(): exporter shares of supply into the
#   balanced importer panel, base year vs T12M (scripts/26 moves the Atlas export shares by the change).
# - data/processed/current_trade_dyads_tbl.rds  the T12M dyads (scripts/26 builds current import partners).
# - diagnostics/current_trade/: coverage_by_importer.csv, china_share_moves.csv, top_supplier_changes.csv,
#   base_reproduction.csv.

library(dplyr)

for (f in c("R/utils/schema.R", "R/utils/supply_chain_stages.R", "R/themes/partnership_strength/partnership_strength_helpers.R",
            "R/strategic_position/import_exposure.R", "R/strategic_position/supply_structure.R",
            "R/strategic_position/current_trade.R")) {
  source(file.path(repo_root, f))
}
config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)
diag_dir <- file.path(repo_root, "diagnostics", "current_trade")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
need_file <- function(path, hint) {
  if (!file.exists(path)) stop(path, " not found; ", hint)
  path
}
base_year <- as.integer(Sys.getenv("OPSI_CURRENT_TRADE_BASE_YEAR", "2024"))
min_months <- as.integer(Sys.getenv("OPSI_CURRENT_TRADE_MIN_MONTHS", "10"))
t12m_label <- 9999L

monthly <- as.data.frame(data.table::fread(
  need_file(file.path(raw_data_path, "comtrade_monthly_dyads.csv.gz"), "run scripts/37_pull_comtrade_monthly_dyads.R."),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code"))))
windows <- utils::read.csv(need_file(file.path(raw_data_path, "comtrade_monthly_dyads_windows.csv"), "run scripts/37."),
                           stringsAsFactors = FALSE)
completeness <- ctm_month_completeness(monthly)
t12m <- ctm_t12m_dyads(monthly, windows, completeness, min_months = min_months, period_label = t12m_label)
dropped <- attr(t12m, "dropped")
rm(monthly)
invisible(gc())
saveRDS(t12m, file.path(processed_dir, "current_trade_dyads_tbl.rds"))
message("T12M: ", length(unique(t12m$reporter_iso)), " reporters, ", format(nrow(t12m), big.mark = ","), " rows; ",
        sum(!completeness$complete), " partial reporter-months dropped; ", nrow(dropped), " reporter-flows under ",
        min_months, " complete months.")

annual <- data.table::fread(
  need_file(file.path(raw_data_path, "comtrade_energy_dyads_all.csv"), "run scripts/98_pull_comtrade_energy_dyads.R."),
  select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")), na.strings = c("", "NA"))
annual <- as.data.frame(annual[annual$period == base_year, ])
dyads <- rbind(annual, t12m[c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value")])
rm(annual)
invisible(gc())

subcat <- utils::read.csv(need_file(file.path(raw_data_path, "energy_hs6_master.csv"), "stage the HS6 master."))
subcat$supply_chain <- subcat$stage
partnership_cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))
cif_fob <- partnership_cfg$trade$cif_fob_factor %||% PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR
trade_cells <- partnership_strength_build_trade_cells(dyads, subcat, years = c(base_year, t12m_label), cif_fob_factor = cif_fob)
rm(dyads)
invisible(gc())

geo <- readRDS(need_file(file.path(processed_dir, "geo_alignment_tbl.rds"), "run scripts/12_build_geopolitical_alignment.R."))
powers <- unlist(yaml::read_yaml(file.path(repo_root, "config", "strategic_position.yml"))$strategic_posture$coercive_powers)
pairs <- sp_coercive_pairs(geo, coercive_powers = powers)
gdp_raw <- utils::read.csv(need_file(file.path(raw_data_path, "wdi_gdp.csv"), "stage WDI GDP."))
latest_gdp_year <- max(gdp_raw$year[!is.na(gdp_raw$NY.GDP.MKTP.CD)])
base <- sp_import_exposure(trade_cells, alignment = geo, gdp = sp_gdp_usd(gdp_raw, year = base_year),
                           value_col = "value_first", trade_year = base_year, coercive_pairs = pairs)
current <- sp_import_exposure(trade_cells, alignment = geo, gdp = sp_gdp_usd(gdp_raw, year = latest_gdp_year),
                              value_col = "value_last", trade_year = NULL, coercive_pairs = pairs)
panel <- ctm_panel_supplier_shares(trade_cells)
attr(panel, "base_year") <- base_year
saveRDS(panel, file.path(processed_dir, "current_supplier_panel_tbl.rds"))
message("Supplier panel: ", length(unique(paste(panel$tech, panel$supply_chain))), " cells; median ",
        stats::median(unique(panel[c("tech", "supply_chain", "panel_n_importers")])$panel_n_importers), " importers per cell.")
rm(trade_cells)
win <-unique(t12m[t12m$flow_direction == "import", c("reporter_iso", "window_end", "months_used")])
win <- merge(win, windows[c("reporter_iso", "window_start")], by = "reporter_iso")
current <- merge(current, setNames(win, c("importer_iso", "window_end", "months_used", "window_start")), by = "importer_iso", all.x = TRUE)
current$current_basis <- ifelse(current$import_source %in% "importer", "own_monthly", "no_monthly_report")
saveRDS(current, file.path(processed_dir, "current_import_exposure_tbl.rds"))

# The 2024 side should reproduce scripts/15's table.
old_path <- file.path(processed_dir, "import_exposure_tbl.rds")
if (file.exists(old_path)) {
  old <- readRDS(old_path)
  chk <- merge(old[c("importer_iso", "tech", "supply_chain", "china_share", "imports_usd")],
               base[c("importer_iso", "tech", "supply_chain", "china_share", "imports_usd")],
               by = c("importer_iso", "tech", "supply_chain"), suffixes = c("_tbl", "_rebuilt"))
  chk$china_diff <- abs(chk$china_share_tbl - chk$china_share_rebuilt)
  utils::write.csv(chk[order(-chk$china_diff), ][seq_len(min(200, nrow(chk))), ], file.path(diag_dir, "base_reproduction.csv"), row.names = FALSE)
  message("2024 reproduction: ", nrow(chk), " cells matched; max |China share difference| ",
          signif(max(chk$china_diff, na.rm = TRUE), 3), "; ", sum(chk$china_diff > 1e-9, na.rm = TRUE), " cells differ.")
}

cmp <- ctm_exposure_compare(base, current)
cmp <- merge(cmp, setNames(win, c("importer_iso", "window_end", "months_used", "window_start")), by = "importer_iso", all.x = TRUE)
saveRDS(cmp, file.path(processed_dir, "import_exposure_compare_tbl.rds"))

own <- cmp[cmp$current_basis == "own_monthly", , drop = FALSE]
cov <- own %>% group_by(importer_iso, window_start, window_end, months_used) %>%
  summarise(cells = n(), imports_base_bn = sum(imports_usd_base, na.rm = TRUE) / 1e9,
            imports_now_bn = sum(imports_usd_now, na.rm = TRUE) / 1e9, china_moves = sum(flag_china_move),
            .groups = "drop") %>% arrange(desc(imports_now_bn))
utils::write.csv(cov, file.path(diag_dir, "coverage_by_importer.csv"), row.names = FALSE)
moves <- own[own$flag_china_move, , drop = FALSE]
utils::write.csv(moves[order(-abs(moves$china_share_change)), ], file.path(diag_dir, "china_share_moves.csv"), row.names = FALSE)
tops <- own[own$top_supplier_changed & own$imports_usd_now >= 1e8, , drop = FALSE]
utils::write.csv(tops[order(-tops$imports_usd_now), ], file.path(diag_dir, "top_supplier_changes.csv"), row.names = FALSE)
message("Current exposure: ", length(unique(own$importer_iso)), " importers from their own monthly data (",
        nrow(own), " cells); ", nrow(moves), " cells with a China-share move of 10+ points on $100m+; ",
        nrow(tops), " top-supplier changes on $100m+. Imports-weighted mean China share ",
        round(stats::weighted.mean(own$china_share_base, own$imports_usd_base, na.rm = TRUE), 3), " -> ",
        round(stats::weighted.mean(own$china_share_now, own$imports_usd_now, na.rm = TRUE), 3), ".")
