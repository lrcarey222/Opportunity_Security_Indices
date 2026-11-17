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
message("T12M: ", length(unique(t12m$reporter_iso)), " reporters, ", format(nrow(t12m), big.mark = ","), " rows; ",
        sum(!completeness$complete), " partial reporter-months dropped; ", nrow(dropped), " reporter-flows under ",
        min_months, " complete months.")

annual <- data.table::fread(
  need_file(file.path(raw_data_path, "comtrade_energy_dyads_all.csv"), "run scripts/98_pull_comtrade_energy_dyads.R."),
  select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")), na.strings = c("", "NA"))
annual <- as.data.frame(annual[annual$period == base_year, ])
# Same economy universe as the annual dyads: the monthly data also carry aggregate partners ("_X" areas n.e.s.,
# X1, X2, XX, E19) that scripts/98 never keeps, and they would rank as suppliers.
economies <- unique(c(annual$reporter_iso, annual$partner_iso))
n_before <- sum(t12m$primary_value)
t12m <- t12m[t12m$partner_iso %in% economies & t12m$reporter_iso %in% economies, , drop = FALSE]
message("T12M: ", round(100 * (1 - sum(t12m$primary_value) / n_before), 2), "% of value dropped as non-economy partners.")
saveRDS(t12m, file.path(processed_dir, "current_trade_dyads_tbl.rds"))
dyads <- rbind(annual, t12m[c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value")])
# Ember fill inputs (base-year exports at HS6): China's, and those of the exporters that also report monthly.
annual_x <- annual[annual$flow_direction == "export", c("reporter_iso", "partner_iso", "cmd_code", "primary_value")]
annual_x$hs6 <- formatC(as.integer(annual_x$cmd_code), width = 6, flag = "0")
monthly_exporters <- setdiff(unique(t12m$reporter_iso[t12m$flow_direction == "export"]), "CHN")
china_x24 <- stats::aggregate(primary_value ~ partner_iso + hs6, data = annual_x[annual_x$reporter_iso == "CHN", ], FUN = sum)
names(china_x24) <- c("importer_iso", "hs6", "usd")
others_x24 <- stats::aggregate(primary_value ~ reporter_iso + partner_iso + hs6,
                               data = annual_x[annual_x$reporter_iso %in% monthly_exporters, ], FUN = sum)
rm(annual, annual_x)
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

# Ember fill: importers without monthly reports (China publishes none either, so their mirror rows lack
# China), from China's growth in Ember's customs data and the other suppliers' growth in monthly exporters'
# reports (ctm_ember_fill()).
ember_path <- file.path(raw_data_path, "ember", "clean_tech_exports_full_release_monthly_lower.csv")
if (file.exists(ember_path)) {
  source(file.path(repo_root, "R", "strategic_position", "ember_china_exports.R"))
  ember <- ember_cte_prepare(utils::read.csv(ember_path, check.names = FALSE, colClasses = c("Commodity code" = "character")))
  ember_end <- max(ember$date)
  ew <- ember_cte_code_windows(ember, base_year = base_year)
  ew <- merge(ew, unique(ember[!ember$partial_hs6, c("code", "hs6")]), by = "code")
  ew <- stats::aggregate(cbind(usd_latest, usd_base) ~ iso3 + hs6, data = ew, FUN = sum)
  names(ew)[1] <- "importer_iso"
  code_cells <- unique(data.frame(hs6 = formatC(as.integer(subcat$hs6), width = 6, flag = "0"), tech = subcat$tech,
                                  supply_chain = subcat$supply_chain, stringsAsFactors = FALSE))
  # Other suppliers: exporters with both base-year and T12M reports, by importer x cell.
  xn <- t12m[t12m$flow_direction == "export" & t12m$reporter_iso %in% monthly_exporters, ]
  xn$hs6 <- formatC(as.integer(xn$cmd_code), width = 6, flag = "0")
  xn <- stats::aggregate(primary_value ~ reporter_iso + partner_iso + hs6, data = xn, FUN = sum)
  ox <- merge(others_x24, xn, by = c("reporter_iso", "partner_iso", "hs6"), all = TRUE, suffixes = c("_24", "_now"))
  both <- unique(intersect(others_x24$reporter_iso, xn$reporter_iso))
  ox <- ox[ox$reporter_iso %in% both, ]
  ox[is.na(ox)] <- 0
  ox <- merge(ox, code_cells, by = "hs6")
  others <- stats::aggregate(cbind(primary_value_24, primary_value_now) ~ partner_iso + tech + supply_chain, data = ox, FUN = sum)
  names(others) <- c("importer_iso", "tech", "supply_chain", "x24", "xnow")
  chn_pairs <- pairs[pairs$partner_iso == "CHN", c("reporter_iso", "coercive", "trusted")]
  chn_geo <- geo[geo$partner_iso == "CHN", c("reporter_iso", "geo_alignment")]
  flags <- merge(chn_pairs, chn_geo, by = "reporter_iso", all = TRUE)
  flags <- data.frame(importer_iso = flags$reporter_iso, coercive = flags$coercive %in% TRUE, trusted = flags$trusted %in% TRUE,
                      opposite = flags$geo_alignment %in% 0)
  targets <- setdiff(unique(current$importer_iso), c(unique(current$importer_iso[current$current_basis %in% "own_monthly"]), "CHN", "HKG", "MAC"))
  fill <- ctm_ember_fill(base, targets, code_cells, ew, china_x24, others, flags, cif_fob = cif_fob,
                         window_end = as.integer(format(ember_end, "%Y%m")))
  if (!is.null(fill) && nrow(fill)) {
    k <- paste(current$importer_iso, current$tech, current$supply_chain)
    i <- match(paste(fill$importer_iso, fill$tech, fill$supply_chain), k)
    for (v in intersect(c("observed", "current_basis", "imports_usd", "china_share", "coercive_import_share", "opposite_bloc_share",
                          "trusted_import_share", "import_hhi", "top_supplier_iso3", "top_supplier_share", "window_end"), names(fill))) {
      current[[v]][i] <- fill[[v]]
    }
    current$imports_to_gdp[i] <- NA_real_
    current$window_start[i] <- as.integer(format(seq(ember_end, by = "-11 months", length.out = 2)[2], "%Y%m"))
    saveRDS(fill, file.path(processed_dir, "current_ember_fill_tbl.rds"))
    big <- fill[fill$imports_usd >= 1e8, ]
    message("Ember fill: ", nrow(fill), " cells for ", length(unique(fill$importer_iso)), " importers without monthly reports (",
            nrow(big), " with $100m+); ", length(targets) - length(unique(fill$importer_iso)), " importers had no cell passing coverage.")
  }
  rm(ember, ew, xn, ox, others)
} else {
  message("Ember file not staged (", ember_path, "): no fill for importers without monthly reports.")
}
saveRDS(current,file.path(processed_dir, "current_import_exposure_tbl.rds"))

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

if (exists("ember_end")) cmp$window_end[cmp$current_basis %in% "ember_fill"] <- as.integer(format(ember_end, "%Y%m"))
own <- cmp[cmp$current_basis %in% CTM_CURRENT_BASES, , drop = FALSE]
cov <- own %>% group_by(importer_iso, current_basis, window_end) %>%
  summarise(cells = n(), imports_base_bn = sum(imports_usd_base, na.rm = TRUE) / 1e9,
            imports_now_bn = sum(imports_usd_now, na.rm = TRUE) / 1e9, china_moves = sum(flag_china_move),
            .groups = "drop") %>% arrange(desc(imports_now_bn))
utils::write.csv(cov, file.path(diag_dir, "coverage_by_importer.csv"), row.names = FALSE)
moves <- own[own$flag_china_move, , drop = FALSE]
utils::write.csv(moves[order(-abs(moves$china_share_change)), ], file.path(diag_dir, "china_share_moves.csv"), row.names = FALSE)
tops <- own[own$top_supplier_changed & own$imports_usd_now >= 1e8, , drop = FALSE]
utils::write.csv(tops[order(-tops$imports_usd_now), ], file.path(diag_dir, "top_supplier_changes.csv"), row.names = FALSE)
message("Current exposure: ", length(unique(own$importer_iso[own$current_basis == "own_monthly"])), " importers from their own monthly data (",
        sum(own$current_basis == "own_monthly"), " cells), ", length(unique(own$importer_iso[own$current_basis == "ember_fill"])),
        " from the Ember fill (", sum(own$current_basis == "ember_fill"), " cells); ", nrow(moves), " cells with a China-share move of 10+ points on $100m+; ",
        nrow(tops), " top-supplier changes on $100m+. Imports-weighted mean China share ",
        round(stats::weighted.mean(own$china_share_base, own$imports_usd_base, na.rm = TRUE), 3), " -> ",
        round(stats::weighted.mean(own$china_share_now, own$imports_usd_now, na.rm = TRUE), 3), ".")
