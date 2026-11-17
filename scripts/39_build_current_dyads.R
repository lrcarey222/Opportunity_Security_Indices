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


# Current dyads for the AIB trade network: the scripts/98 dyad table with its trade-year rows replaced by each
# reporter's latest 12 months where it reports monthly, and carried forward (China rows scaled by Ember's
# growth) where it does not (ctm_current_dyads()).
#
#   Rscript scripts/39_build_current_dyads.R
#   Rscript scripts/31_build_aib_trade_network.R --dyads data/processed/current_dyads.csv --out-dir <outputs>/aib_current
#   (then scripts/32 and 33 with --out-dir / --in-dir <outputs>/aib_current)
#
# Inputs: the annual dyads (default data/raw/comtrade_energy_dyads_all.csv from scripts/98; OPSI_CURRENT_DYADS_ANNUAL
# overrides); data/processed/current_trade_dyads_tbl.rds
# (scripts/38); data/raw/ember/clean_tech_exports_full_release_monthly_lower.csv (Ember, CC BY 4.0).
# Settings: OPSI_CURRENT_DYADS_MIN_EMBER_USD (5e6: an Ember destination x HS6 needs this much in the base
# year for its growth to scale a carried row). The trade and history years come from config/aib.yml.
# Output: data/processed/current_dyads.csv (+ current_source) and diagnostics/current_trade/current_dyads_sources.csv.

for (f in c("R/strategic_position/current_trade.R", "R/strategic_position/ember_china_exports.R")) source(file.path(repo_root, f))
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
aib_trade <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))$trade
years <- as.integer(c(aib_trade$history_year, aib_trade$year))
min_ember_usd <- as.numeric(Sys.getenv("OPSI_CURRENT_DYADS_MIN_EMBER_USD", "5e6"))
# The annual dyads scripts/31 reads (the AIB dictionary taxonomy reaches beyond scripts/98 basket; point this at
# the dyad file that run used). The monthly pull asked for the non-excluded, current-HS basket codes only.
annual_path <- Sys.getenv("OPSI_CURRENT_DYADS_ANNUAL", file.path(raw_data_path, "comtrade_energy_dyads_all.csv"))
ref <- utils::read.csv(file.path(repo_root, "data", "reference", "energy_hs6_master.csv"), colClasses = "character")
is_true <- function(x) toupper(x) %in% c("TRUE", "T", "1")
pulled_codes <- unique(formatC(as.integer(ref$hs6[!is_true(ref$excluded) & !is_true(ref$legacy_hs)]), width = 6, flag = "0"))

annual <- data.table::fread(
  need_file(annual_path, "run scripts/98_pull_comtrade_energy_dyads.R or set OPSI_CURRENT_DYADS_ANNUAL."),
  select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")), na.strings = c("", "NA"))
annual <- as.data.frame(annual[annual$period %in% years, ])
t12m <- readRDS(need_file(file.path(processed_dir, "current_trade_dyads_tbl.rds"), "run scripts/38_build_current_import_exposure.R."))

ember <- ember_cte_prepare(utils::read.csv(
  need_file(file.path(raw_data_path, "ember", "clean_tech_exports_full_release_monthly_lower.csv"), "stage Ember's file (scripts/36 downloads it)."),
  check.names = FALSE, colClasses = c("Commodity code" = "character")))
ew <- ember_cte_code_windows(ember, base_year = max(years))
ew <- merge(ew, unique(ember[!ember$partial_hs6, c("code", "hs6")]), by = "code")
ew <- stats::aggregate(cbind(usd_latest, usd_base) ~ iso3 + hs6, data = ew, FUN = sum)
ew <- ew[ew$usd_base >= min_ember_usd, ]
ember_growth <- data.frame(importer_iso = ew$iso3, hs6 = ew$hs6, growth = ew$usd_latest / ew$usd_base)
message("Ember growth: ", nrow(ember_growth), " destination x HS6 pairs with $", format(min_ember_usd, big.mark = ","),
        "+ in ", max(years), "; window ends ", format(max(ember$date), "%Y-%m"), ".")
rm(ember, ew)

annual$cmd_code <- formatC(as.integer(annual$cmd_code), width = 6, flag = "0")
out <- ctm_current_dyads(annual, t12m, ember_growth, trade_year = max(years), pulled_codes = pulled_codes)
rm(annual, t12m)
invisible(gc())
data.table::fwrite(out, file.path(processed_dir, "current_dyads.csv"))
cur <- out[out$period == max(years), ]
src <- stats::aggregate(primary_value ~ flow_direction + current_source, data = cur, FUN = sum)
src$share <- src$primary_value / stats::ave(src$primary_value, src$flow_direction, FUN = sum)
src$reporters <- vapply(seq_len(nrow(src)), function(i) {
  length(unique(cur$reporter_iso[cur$flow_direction == src$flow_direction[i] & cur$current_source == src$current_source[i]]))
}, integer(1))
utils::write.csv(src, file.path(diag_dir, "current_dyads_sources.csv"), row.names = FALSE)
message("Current dyads: ", format(nrow(out), big.mark = ","), " rows; ", max(years), " rows by source (share of value):")
for (i in seq_len(nrow(src))) {
  message(sprintf("  %s %-14s %5.1f%% (%d reporters)", src$flow_direction[i], src$current_source[i], 100 * src$share[i], src$reporters[i]))
}
