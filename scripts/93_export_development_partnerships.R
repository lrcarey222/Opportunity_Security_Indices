# Development partnerships (docs/development_partnerships.md): the donor x recipient x technology x step
# pairs where a high-income economy's supply-chain or allied-capacity need meets a developing economy's
# credible Catch Up pathway in the same step, with the relationship evidence (OECD CRS aid by step, OCI
# export and lender finance, IMF direct investment, CM-FDI factory projects, AIB compacts and pairings) and
# the recommended tools. Rules: R/dashboard/country/country_devpart.R and config/development_partnerships.yml.
#
#   Rscript scripts/93_export_development_partnerships.R                 export every pair and the Country files
#   Rscript scripts/93_export_development_partnerships.R --audit AUS     also write the audit of one donor
#
# Options (repo-relative defaults): --posture-dir (dashboard/public/data/posture), --aib-dir
# (dashboard/public/data/aib), --country-dir (dashboard/public/data/country), --raw-dir (data/raw),
# --reference-dir (data/reference), --processed-dir (data/processed), --screen
# (<processed-dir>/outputs/aib/aib_host_screen.csv), --out-dir (the Country data directory, --country-dir) and
# --export-dir (<processed-dir>/development_partnerships). Inputs are the published posture, AIB and Country
# bundles (run after scripts/88 and 91) and the raw development tables (CRS purpose file, OCI, IMF DIP,
# CM-FDI projects, legacy Development Potential). Writes development_partnerships.{csv,rds} (every pair
# considered, excluded ones with the test they fail), development_partnership_steps.csv (every step with the
# donor signals and recipient tests), devpart-<ISO3>.<h8>.json per economy into the Country data directory, and
# each economy's Country manifest entry `devpart` (scripts/88 carries it while the file exists; run this after
# scripts/88 and 91).

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
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

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

for (f in c("R/utils/schema.R", "R/utils/country.R", "R/themes/partnership_strength/partnership_strength_helpers.R",
            "R/themes/partnership_strength/safer_friendshore.R", "R/themes/partnership_strength/stronger_development.R",
            "R/dashboard/sector/sector_dev.R", "R/strategic_position/pathway.R",
            "R/dashboard/country/country_util.R", "R/dashboard/country/country_brief.R", "R/dashboard/country/country_devpart.R")) source(file.path(repo_root, f))
suppressPackageStartupMessages(library(dplyr))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "development_partnerships.yml"))
pcfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))
posture_dir <- abs_path(arg_value("--posture-dir", "dashboard/public/data/posture"))
aib_dir <- abs_path(arg_value("--aib-dir", "dashboard/public/data/aib"))
country_dir <- abs_path(arg_value("--country-dir", "dashboard/public/data/country"))
raw_dir <- abs_path(arg_value("--raw-dir", "data/raw"))
ref_dir <- abs_path(arg_value("--reference-dir", "data/reference"))
processed_dir <- abs_path(arg_value("--processed-dir", "data/processed"))
screen_path <- abs_path(arg_value("--screen", file.path(processed_dir, "outputs", "aib", "aib_host_screen.csv")))
out_dir <- abs_path(arg_value("--out-dir", country_dir))
export_dir <- abs_path(arg_value("--export-dir", file.path(processed_dir, "development_partnerships")))
audit_iso <- toupper(arg_value("--audit", ""))
# the posture engine's pathway inputs (pw_*), to tell an observed constraint from an unobserved input
position_path <- abs_path(arg_value("--position-table", file.path(processed_dir, "outputs", "strategic_position_tbl.csv")))
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
need_file <- function(p) { if (!file.exists(p)) stop("input not found: ", p); p }

# ---- Bundles -----------------------------------------------------------------------------------------------

step("reading the posture, AIB and Country bundles")
pm <- read_json(need_file(file.path(posture_dir, "manifest.json")))
core <- read_json(file.path(posture_dir, pm$core))
details <- lapply(pm$details, function(f) read_json(file.path(posture_dir, f)))
am <- read_json(need_file(file.path(aib_dir, "manifest.json")))
aib <- read_json(file.path(aib_dir, am$files$compacts))
cm <- read_json(need_file(file.path(country_dir, "manifest.json")))
iso_all <- vapply(cm$countries, function(e) e$iso3, "")
name_of <- stats::setNames(vapply(cm$countries, function(e) e$name %||% e$iso3, ""), iso_all)
tier <- stats::setNames(vapply(cm$countries, function(e) e$tier %||% NA_character_, ""), iso_all)
income <- stats::setNames(vapply(cm$countries, function(e) {
  if (is.null(e$file) || !file.exists(file.path(country_dir, e$file))) return(NA_character_)
  read_json(file.path(country_dir, e$file))$meta$income_group %||% NA_character_
}, ""), iso_all)
screen <- read.csv(need_file(screen_path), stringsAsFactors = FALSE)

# ---- Development tables ------------------------------------------------------------------------------------

step("reading the development tables (CRS, OCI, IMF DIP, CM-FDI, Development Potential)")
aid_settings <- partnership_strength_development_aid_settings(pcfg$development_aid)
crs <- read.csv(need_file(file.path(raw_dir, "oecd_crs_purpose.csv")), stringsAsFactors = FALSE)
crs_cw <- read.csv(need_file(file.path(ref_dir, "oecd_crs_purpose_crosswalk.csv")), stringsAsFactors = FALSE, colClasses = c(purpose_code = "character"))
aid <- country_devpart_aid(crs, crs_cw, aid_settings$years, aid_settings$flow, cfg)
rm(crs)

oci_raw <- read.csv(need_file(file.path(raw_dir, "oci_energy_finance.csv")), stringsAsFactors = FALSE)
oci_inst <- read.csv(need_file(file.path(ref_dir, "oci_energy_finance_institutions.csv")), stringsAsFactors = FALSE)
oci_cw <- read.csv(need_file(file.path(ref_dir, "oci_energy_finance_crosswalk.csv")), stringsAsFactors = FALSE)
# key each transaction by lender and recipient, so the Development Potential helper sums them per pair
inst_key <- paste(oci_inst$institution_group, oci_inst$institution_abbr, sep = "\r")
lender <- oci_inst$lender_iso3[match(paste(oci_raw$institution_group, oci_raw$institution_abbr, sep = "\r"), inst_key)]
oci_lenders <- sort(unique(oci_inst$lender_iso3[oci_inst$source == "oci" & !oci_inst$lender_iso3 %in% aid_settings$energy_finance$exclude_lender_countries]))
oci_keyed <- oci_raw[!is.na(lender), , drop = FALSE]
oci_keyed$recipient_iso3 <- paste(lender[!is.na(lender)], oci_keyed$recipient_iso3, sep = "|")
oci <- country_devpart_oci_cells(suppressWarnings(partnership_strength_oci_amounts(oci_keyed, oci_inst, oci_cw, aid_settings)))
# the steps the OCI crosswalk maps at all: elsewhere OCI finance is not observed (NA), not zero
oci_steps <- unique(paste(oci_cw$tech, oci_cw$supply_chain, sep = "|"))
rm(oci_raw, oci_keyed)

dip_path <- need_file(file.path(raw_dir, "imf_dip.csv"))
dip_header <- names(data.table::fread(dip_path, nrows = 0))
dip_keep <- c(intersect(c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"), dip_header), grep("^X?[0-9]{4}$", dip_header, value = TRUE))
dip_raw <- as.data.frame(data.table::fread(dip_path, select = dip_keep, showProgress = FALSE))
country_info <- standardize_country_info(read.csv(need_file(file.path(raw_dir, "wdi_country_info.csv"))))
# the total outward direct investment position, equity and debt (reported official data); where the investor
# reports none, the IMF's position derived from the counterpart's reports, flagged. (Friendshore uses the
# debt-instrument series alone, a small fraction of the position: not a measure of the investment tie.)
dip_rep <- sector_dev_dip(dip_raw, country_info, SECTOR_DEV_DIP_YEARS, partnership_strength_conduit_centres()$iso3, SECTOR_DEV_DIP_TOTAL_INDICATOR)
dip_der_raw <- dip_raw[dip_raw$DV_TYPE == "Derived using counterparty information", , drop = FALSE]
dip_der_raw$DV_TYPE <- SECTOR_DEV_DIP_BASIS
dip_der <- sector_dev_dip(dip_der_raw, country_info, SECTOR_DEV_DIP_YEARS, partnership_strength_conduit_centres()$iso3, SECTOR_DEV_DIP_TOTAL_INDICATOR)
dip_rep$basis <- "reported"
dip_der$basis <- "derived"
dip <- rbind(dip_rep, dip_der[!paste(dip_der$reporter, dip_der$counterpart) %in% paste(dip_rep$reporter, dip_rep$counterpart), , drop = FALSE])
rm(dip_raw, dip_der_raw)

cmfdi <- country_devpart_cmfdi(readRDS(need_file(file.path(processed_dir, "cm_fdi_projects_tbl.rds"))))
dv <- readRDS(need_file(file.path(processed_dir, "partner_development_top5_tbl.rds")))
dv <- dv[dv$variable == "Development Potential Index", , drop = FALSE]
dp <- data.frame(partner_iso = as.character(dv$partner_iso), development = as.numeric(dv$value), stringsAsFactors = FALSE)
dp <- dp[!duplicated(dp$partner_iso), , drop = FALSE]
fl <- aib$investmentFlows$pairs %||% list()
flows <- data.frame(investor = vapply(fl, function(x) x$investor %||% NA_character_, ""), host = vapply(fl, function(x) x$host %||% NA_character_, ""),
                    node = vapply(fl, function(x) x$node %||% NA_character_, ""), usd = vapply(fl, function(x) as.numeric(x$usd %||% NA), 0), stringsAsFactors = FALSE)

# ---- Tests and pairs ---------------------------------------------------------------------------------------

step("donor signals and recipient tests on every step")
review <- do.call(rbind, lapply(cfg$review_cells %||% list(), function(r) data.frame(iso3 = r$iso3, tech = r$tech, stage = r$stage, reason = r$reason, stringsAsFactors = FALSE)))
nodes <- country_devpart_nodes(aib$nodes, aib$strategic)
steps <- country_devpart_steps(country_brief_core_cells(core), country_devpart_detail_table(details), income, tier, screen, unlist(core$meta$phrases))
pw_cols <- c("pw_feasibility_pct", "pw_input_cost_pct", "pw_operating_share", "pw_committed_share")
pos <- as.data.frame(data.table::fread(need_file(position_path), select = c("iso3", "tech", "supply_chain", pw_cols), showProgress = FALSE))
pos <- pos[!duplicated(paste(pos$iso3, pos$tech, pos$supply_chain)), , drop = FALSE]
mi <- match(paste(steps$iso3, steps$tech, steps$stage), paste(pos$iso3, pos$tech, pos$supply_chain))
for (k in pw_cols) steps[[k]] <- suppressWarnings(as.numeric(pos[[k]][mi]))
step(sprintf("pathway inputs joined for %d of %d steps", sum(!is.na(mi)), nrow(steps)))
rm(pos)
steps <- country_devpart_donor_signals(steps, nodes, core$meta$thresholds, cfg)
steps <- country_devpart_recipient_test(steps, cfg, review)
step("pairs")
brief_cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_brief.yml"))
rel <- list(aid = aid, oci = oci, oci_lenders = oci_lenders, oci_steps = oci_steps, dip = dip, cmfdi = cmfdi, dp = dp, flows = flows, roles = aib$strategic$compactRoles,
            compact_map = brief_cfg$compacts)
pairs <- country_devpart_pairs(steps, nodes, rel, cfg, name_of)
step(sprintf("%d pairs considered: %s", nrow(pairs), paste(names(table(pairs$route)), table(pairs$route), sep = " ", collapse = ", ")))

# ---- Outputs -----------------------------------------------------------------------------------------------

dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(pairs, file.path(export_dir, "development_partnerships.csv"), row.names = FALSE, na = "")
saveRDS(pairs, file.path(export_dir, "development_partnerships.rds"))
step_cols <- c("iso3", "tech", "stage", "key", "step_name", "choice", "fence", "need", "prize", "cp", "conf", "ev", "fl", "pw", "delivery", "aib_status",
               "trusted_alt", "income_group", "bloc", "in_demand_scope", "screened", "signals", "donor_tier", "anchors", "supports", "corroboration",
               "thin_anchor", "capability_split", "capability_note", "constraints", "unobserved", "artifact", "recipient_fail", "recipient_ok", "preserve_fail", pw_cols)
utils::write.csv(steps[step_cols], file.path(export_dir, "development_partnership_steps.csv"), row.names = FALSE, na = "")

meta <- list(created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), posture_core = pm$core, aib_compacts = am$files$compacts,
             as_of_label = cm$as_of_label %||% NA_character_, aid_years = range(aid_settings$years), aid_flow = aid_settings$flow,
             dip_years = range(SECTOR_DEV_DIP_YEARS), need_bar = as.numeric(core$meta$thresholds$need),
             sources = c("OECD Creditor Reporting System (purpose codes)", "Oil Change International public energy finance", "IMF Direct Investment Positions",
                         "fDi Markets clean-manufacturing projects (CM-FDI)", "Allied Industrial Base Explorer"))
compact_names <- stats::setNames(lapply(aib$strategic$compacts %||% list(), function(k) k$name %||% k$id),
                                 vapply(aib$strategic$compacts %||% list(), function(k) k$id %||% NA_character_, ""))
files <- list()
for (iso in sort(unique(steps$iso3))) {
  per <- country_devpart_perspective(steps, iso, cfg)
  # a "none" economy gets a short file too (its perspective and the reason), so its page says why
  obj <- country_devpart_country_file(iso, steps, pairs, aid, cfg, meta, name_of, compact_names)
  txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, na = "null", null = "null", digits = 6)
  # hashed from a temporary copy, then written once under its final name (a rename inside the served
  # directory can be missed by a running dev server's file index)
  tmp <- tempfile(fileext = ".json")
  writeLines(txt, tmp, useBytes = TRUE)
  h8 <- substr(unname(tools::md5sum(tmp)), 1, 8)
  f <- sprintf("devpart-%s.%s.json", iso, h8)
  if (!file.exists(file.path(out_dir, f))) file.copy(tmp, file.path(out_dir, f))
  unlink(tmp)
  files[[iso]] <- list(file = f, perspective = per$kind)
}
# every economy's Country manifest entry names its file (or none); files no entry names are removed
old <- list.files(out_dir, "^devpart-[A-Z0-9]{3}[.][0-9a-f]{8}[.]json$")
unlink(file.path(out_dir, setdiff(old, vapply(files, function(x) x$file, ""))))
mpath <- file.path(country_dir, "manifest.json")
cm2 <- read_json(mpath)
for (i in seq_along(cm2$countries)) {
  iso <- cm2$countries[[i]]$iso3
  cm2$countries[[i]]["devpart"] <- list(if (is.null(files[[iso]])) NULL else files[[iso]]$file)
}
writeLines(as.character(country_json(cm2)), mpath, useBytes = TRUE)
step(sprintf("wrote %d development-partnership files to %s and their manifest entries", length(files), out_dir))

if (nzchar(audit_iso)) {
  a <- pairs[pairs$donor_iso3 == audit_iso, , drop = FALSE]
  utils::write.csv(a, file.path(export_dir, sprintf("development_partnerships_%s.csv", audit_iso)), row.names = FALSE, na = "")
  # the eight checks of every development pair (country_devpart_audit())
  au <- country_devpart_audit(pairs, audit_iso, as.numeric(cfg$aid$technology_share_min %||% 0.5))
  utils::write.csv(au, file.path(export_dir, sprintf("development_partnerships_%s_audit.csv", audit_iso)), row.names = FALSE, na = "")
  chk <- grep("^c[0-9]_[a-z_]+$", names(au), value = TRUE)
  chk <- chk[!grepl("_evidence$", chk)]
  step(sprintf("%s audit: %d development pairs; checks passed: %s", audit_iso, nrow(au), paste(sprintf("%s %d", sub("^c[0-9]_", "", chk), vapply(chk, function(k) sum(au[[k]]), 0L)), collapse = ", ")))
  step(sprintf("%s: %d pairs (%s)", audit_iso, nrow(a), paste(names(table(a$route)), table(a$route), sep = " ", collapse = ", ")))
}
