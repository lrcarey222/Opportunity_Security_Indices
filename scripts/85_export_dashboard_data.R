# Export the data of the Allied Industrial Base Explorer (dashboard/, a static React app).
#
#   Rscript scripts/85_export_dashboard_data.R [--out-dir <dir>] [--r-out <dir>] [--paper]
#                                              [--allow-fail]
#
# The ONE place the dashboard reads the pipeline's outputs. It reads data/processed (and the few
# raw files the partner layer needs; the reserves and production themes for the endowment screen
# of the allied picks), builds the R bundle with the pure builders of R/dashboard/, runs the R
# reference engine at the NEIS baseline (every dial at 50) and checks it against the stored
# outputs, then writes
#
#   <out-dir>/manifest.json          schema, vintage, flags, notes, the validation report, sources,
#                                    the baseline inputs and parameters, the dial grid, the chunk
#                                    list and the budget (served with no-cache)
#   <out-dir>/<chunk>.<md5_8>.osb    core (initial), vars, partner: gzipped OSB1 containers
#                                    (R/dashboard/dash_osb.R, dash_web.R), lossless, immutable names
#   <r-out>/bundle.rds               the R bundle (input of scripts/86_export_dashboard_fixtures.R)
#   <r-out>/validation.rds           the stored pipeline tables the baseline checks compare with
#   <r-out>/validation_report.csv    engine at baseline vs the stored outputs, per check
#
# <out-dir> defaults to <repo>/dashboard/public/data, <r-out> to <processed_dir>/dashboard (both
# relative to the repository, never hard-coded). Every baseline parameter is read from the stored
# outputs or the files that produced them (config/weights.yml, config/rankability.yml, the Policy
# index's own record, the Strategic Index constants), so after a pipeline rerun a re-export
# refreshes the dashboard; nothing here fixes a vintage.
#
# The original allied network design (scripts/30) is retired (2026-10-17): the bundle no longer has
# a network chunk, its parameters or its greedy baseline (the Allied Industrial Base analysis,
# scripts/84, replaces it). --paper copies the paper's preview PDF to
# dashboard/public/paper/ (off by default: publishing the preview is the authors' decision). Each
# chunk is decoded again and must be identical() to what was encoded; the export stops above the
# budget (initial <= 3 MB gz, all chunks <= 25 MB gz).
#
# Every baseline check is PASS, INFO or FAIL. A FAIL stops the export before anything is published
# (validation_report.csv is still written to <r-out>); --allow-fail publishes anyway, for diagnosis
# only, with a warning.
#
# Friendshore's CM-FDI parts (commit 78be8b5; R/themes/partnership_strength/cm_fdi.R): the cells
# whose outbound rows name CM-FDI as their source (the Components and Assembly cells of Solar, Wind,
# Batteries and Electric Vehicles, which config/partnership.yml cm_fdi.stages gives the same
# projects) carry the CM-FDI investment tie in place of the IMF position, and the
# chinese_parent_exposure rows the ownership component; both reach the bundle as
# partner$fs$outbound_source / outbound_cmfdi / cpe (dash_bundle_fs_cmfdi()) and are checked against
# config/partnership.yml cm_fdi, the stored exposure table and, through the Friendshore index array,
# bit for bit against the pipeline.
#
# Six stages (2026-10-28): the cells are the 11 headline technologies x the pipeline's six
# supply-chain stages (DASH_STAGES = SUPPLY_CHAIN_STAGES, 66 cells, one pcell chunk each). The
# baseline stage weights are SUPPLY_CHAIN_STAGE_SI_WEIGHTS, the sectors left out of the Strategic
# Index are the pipeline's own list (index_outputs$strategic_excluded_sectors; its rule-based part is
# shipped as excluded_sectors and the engine finds the degenerate sectors itself) and the allied pick's
# stages are STRATEGIC_ALLIED_STAGES (every stage but Deployment), shipped as
# baseline_inputs$allied_stages. A dyad table labelled with other stages stops the export
# (dash_bundle_dyads()).

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

t_script <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
allow_fail <- arg_flag("--allow-fail")
copy_paper <- arg_flag("--paper") && !arg_flag("--no-paper")
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
engine_files <- dash_engine_files()

# ---- Pipeline code (read only) and the engine ----------------------------------------------------

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "cm_fdi.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))
source(file.path(repo_root, "R", "indices", "build_policy_index.R"))
source(file.path(repo_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))
for (f in engine_files[grepl("^R/dashboard/", engine_files)]) source(file.path(repo_root, f))

config <- getOption("opportunity_security.config")
weights_cfg <- getOption("opportunity_security.weights")
processed_dir <- file.path(repo_root, config$processed_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(processed_dir, "outputs")
outputs_rds <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds)) {
  outputs_rds <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) file.path(repo_root, config$outputs_rds) else file.path(outputs_dir, "index_outputs.rds")
}
raw_dir <- file.path(repo_root, config$raw_data_dir)
rankability_path <- Sys.getenv("OPSI_RANKABILITY", file.path(repo_root, "config", "rankability.yml"))
rankability <- if (file.exists(rankability_path)) yaml::read_yaml(rankability_path) else list()
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
out_dir <- abs_path(arg_value("--out-dir", file.path("dashboard", "public", "data")))
r_out_dir <- abs_path(arg_value("--r-out", file.path(config$processed_dir, "dashboard")))
paper_out_dir <- file.path(dirname(out_dir), "paper")

sources_used <- character()
read_src <- function(path, reader = readRDS, ...) {
  if (!file.exists(path)) stop("Missing input: ", path)
  sources_used <<- unique(c(sources_used, path))
  reader(path, ...)
}
maybe_src <- function(path, reader = readRDS, ...) if (file.exists(path)) read_src(path, reader, ...) else NULL
rel <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- paste0(normalizePath(repo_root, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, root)) substring(p, nchar(root) + 1) else basename(p)
}
val_rows <- list()
add_check <- function(check, layer, status, n = NA_integer_, max_abs_diff = NA_real_, note = NA_character_) {
  val_rows[[length(val_rows) + 1]] <<- tibble::tibble(check = check, layer = layer, status = status, n = as.integer(n),
                                                      max_abs_diff = as.numeric(max_abs_diff), note = as.character(note))
}
cmp_num <- function(check, layer, a, b, tol = 1e-10, note = NA_character_, fail = "FAIL") {
  na_mis <- sum(xor(is.na(a), is.na(b)))
  d <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
  if (!is.finite(d)) d <- 0
  status <- if (na_mis == 0 && d <= tol) "PASS" else fail
  add_check(check, layer, status, length(a), d,
            if (na_mis > 0) paste0(na_mis, " NA mismatches", if (!is.na(note)) paste0("; ", note) else "") else note)
  invisible(status)
}
notes <- character()
sources_params <- list()

techs <- partnership_strength_headline_techs()
stages <- DASH_STAGES
country_info <- standardize_country_info(read_src(file.path(raw_dir, "wdi_country_info.csv"), read.csv))

# IMF Direct Investment Positions for the ties chunk (section "Bilateral ties" below), read first while
# the session is small: imf_dip.csv is ~1.9 GB, so only the columns the outbound builder reads (as
# scripts/15 read_imf_dip()) plus SCALE, and only the reported rows of the four series shown.
source(file.path(repo_root, "R", "dashboard", "dash_ties.R"))
step("Reading IMF direct investment positions (column- and row-filtered)")
dip_path <- read_src(file.path(raw_dir, "imf_dip.csv"), function(p) p)
dip_hdr <- names(data.table::fread(dip_path, nrows = 0))
dip_raw <- data.table::fread(
  dip_path, select = c(intersect(c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR", "SCALE"), dip_hdr), grep("^X?[0-9]{4}$", dip_hdr, value = TRUE)),
  showProgress = FALSE
)
dip_raw <- as.data.frame(dip_raw[dip_raw$DV_TYPE == "Reported official data" & dip_raw$INDICATOR %in% DASH_DIP_MEASURES, ])
invisible(gc())

# ---- Index outputs (pillars, policy, Strategic Index) ----------------------------------------------

step("Reading ", rel(outputs_rds))
io <- read_src(outputs_rds)
es_cat_all <- io$energy_security_category_scores
eo_cat_all <- io$economic_opportunity_category_scores
if (is.null(es_cat_all)) es_cat_all <- io$energy_security_outputs$category_scores
if (is.null(eo_cat_all)) eo_cat_all <- io$economic_opportunity_outputs$category_scores
es_cc <- io$energy_security_category_contributions
eo_cc <- io$economic_opportunity_category_contributions
es_vc <- io$energy_security_variable_contributions
eo_vc <- io$economic_opportunity_variable_contributions
stored <- list(
  es_index = io$energy_security_index,
  eo_index = io$economic_opportunity_index,
  es_cc = es_cc[es_cc$tech %in% techs, , drop = FALSE],
  eo_cc = eo_cc[eo_cc$tech %in% techs, , drop = FALSE],
  policy_index = io$policy_index,
  policy_eligibility = io$policy_eligibility,
  strategic_index = io$strategic_index,
  # the sectors scripts/20 left out of the Strategic Index by rule (those no stage-specific source
  # measures, and those in which a pillar is constant across countries): the baseline's exclusions
  strategic_excluded_sectors = io$strategic_excluded_sectors
)
if (is.null(stored$strategic_excluded_sectors)) {
  stop(rel(outputs_rds), " has no strategic_excluded_sectors (the sectors the six-stage Strategic Index leaves out); rerun scripts/20.")
}
policy_component_tbl <- io$policy_component_tbl
policy_provenance <- io$policy_index_provenance
io_coalition <- io$coalition_members
rm(io)
invisible(gc())

es_in_data <- sort(unique(es_cat_all$category[!is.na(es_cat_all$category_score)]), method = "radix")
eo_in_data <- sort(unique(eo_cat_all$category[!is.na(eo_cat_all$category_score)]), method = "radix")
pillar_rule <- "rankable" %in% names(stored$es_index)

# Category weights as recorded in the stored contributions (weights.yml for categories without rows)
recorded_weights <- function(cc, cfg) {
  w <- unlist(cfg)
  storage.mode(w) <- "double"
  rec <- unique(cc[!is.na(cc$weight), c("category", "weight")])
  if (anyDuplicated(rec$category)) stop("The stored contributions carry two weights for one category.")
  diff <- rec$category[rec$category %in% names(w) & w[rec$category] != rec$weight]
  if (length(diff)) {
    notes <<- c(notes, paste0("config/weights.yml differs from the weights the stored pillars were built with (", paste(diff, collapse = ", "), "); the stored weights are the baseline."))
  }
  w[rec$category] <- rec$weight
  w[order(names(w), method = "radix")]
}
es_weights <- recorded_weights(es_cc, weights_cfg$energy_security)
eo_weights <- recorded_weights(eo_cc, weights_cfg$economic_opportunity)
sources_params$es_weights <- "stored category contributions (weight), else config/weights.yml"

pillar_countries <- unique(c(es_cc$Country, eo_cc$Country, policy_component_tbl$Country))
country_tbl <- tibble::tibble(
  Country = pillar_countries,
  iso3c = country_name_to_iso3(pillar_countries),
  sovereign = is_sovereign_economy(pillar_countries)
)

step("Pillars")
pillar_parts <- function(cc, vc, in_data) {
  cc <- cc[cc$tech %in% techs, , drop = FALSE]
  vc <- vc[vc$tech %in% techs, , drop = FALSE]
  dash_bundle_pillar(cc, vc, in_data, country_tbl, allow_partial_categories = isTRUE(config$allow_partial_categories))
}
pillars <- list(es = pillar_parts(es_cc, es_vc, es_in_data), eo = pillar_parts(eo_cc, eo_vc, eo_in_data))
trl <- eo_cat_all[eo_cat_all$category == "Technological Readiness" & eo_cat_all$tech %in% techs, c("Country", "tech", "supply_chain", "category_score")]
names(trl)[4] <- "trl_index"
rm(es_cat_all, eo_cat_all, es_vc, eo_vc)

# Policy weights: the Policy index's own record (policy_index_provenance), else the weights its
# explanation states (builds before the record existed)
policy_weights <- NULL
if (!is.null(policy_provenance$weights)) {
  policy_weights <- policy_provenance$weights
  sources_params$policy_weights <- "index_outputs$policy_index_provenance$weights"
} else {
  expl <- unique(stored$policy_index$explanation)
  m <- regmatches(expl[1], gregexpr("([A-Za-z][A-Za-z -]*?) \\(([0-9.]+)\\)", expl[1]))[[1]]
  short <- trimws(sub(" \\(([0-9.]+)\\)$", "", m))
  short <- sub("^(Weighted mean of|of) ", "", short)
  w <- as.numeric(sub("^.*\\(([0-9.]+)\\)$", "\\1", m))
  vars <- unique(policy_component_tbl$variable)
  vshort <- sub(" (Policy Index|policy score)$", "", vars)
  policy_weights <- stats::setNames(w, vars[match(short, vshort)])
  if (anyNA(names(policy_weights))) stop("Could not read the Policy index weights from its explanation: ", expl[1])
  sources_params$policy_weights <- paste0("parsed from policy_index$explanation: ", expl[1])
  notes <- c(notes, paste0("The stored Policy index predates its provenance record; its weights were read from its explanation (",
                           paste(names(policy_weights), policy_weights, sep = " = ", collapse = ", "), ")."))
}
# The pipeline's component order (its component_weights: IEA PAMS, then CAT), which orders the
# policy_basis labels ("IEA PAMS + CAT", "IEA PAMS only") and the missing-component lists
policy_components <- if (!is.null(policy_provenance$components)) as.character(policy_provenance$components) else names(policy_weights)
policy_weights <- policy_weights[order(names(policy_weights), method = "radix")]
policy_bundle <- dash_bundle_policy(policy_component_tbl, names(policy_weights), techs)
policy_bundle$components <- intersect(policy_components, names(policy_weights))
sources_params$policy_components <- if (!is.null(policy_provenance$components)) "index_outputs$policy_index_provenance$components" else "the order of the Policy weights"
rm(policy_component_tbl)
# Required components (eligibility: a country needs each of them somewhere): the Policy index's record
# (required_components, from the 9 Oct 2026 rule "IEA PAMS required, CAT where it rates"); absent,
# every weighted component is required (the earlier rule). Shipped only when a strict subset.
policy_required <- policy_provenance$required_components
if (!is.null(policy_required)) {
  policy_required <- intersect(names(policy_weights), as.character(policy_required))
  if (!length(policy_required)) stop("policy_index_provenance$required_components names none of the weighted Policy components.")
  if (length(policy_required) < length(policy_weights)) policy_bundle$required <- policy_required
  sources_params$policy_required <- "index_outputs$policy_index_provenance$required_components"
}

# ---- Coalition ------------------------------------------------------------------------------------

coalition_tbl <- maybe_src(file.path(processed_dir, "coalition_tbl.rds"))
geo_coalition_tbl <- maybe_src(file.path(processed_dir, "geo_coalition_tbl.rds"))
coalition <- if (!is.null(coalition_tbl)) coalition_members(coalition_tbl) else coalition_members(NULL, geo_coalition_tbl = geo_coalition_tbl)
if (is.null(coalition_tbl)) notes <- c(notes, "coalition_tbl.rds is missing: the single-tier alignment coalition of geo_coalition_tbl.rds is used.")
allies <- coalition %>%
  dplyr::select("iso3", "tier", "network_node") %>%
  dplyr::left_join(dplyr::distinct(country_info, iso3 = .data$iso3c, Country = .data$country), by = "iso3")
allies <- allies[!is.na(allies$Country) & !duplicated(allies$iso3), c("Country", "iso3", "tier", "network_node")]

# ---- Endowment screen of the allied picks ---------------------------------------------------------

# strategic_resource_base() (R/indices/build_indices.R) on the same reserves and production theme
# tables as scripts/20: one row per Country x screened Extraction stage (its default stage, as
# scripts/20 calls it), resource_base TRUE where the technology-level reserves or production are
# positive. The engine flags the SI cells with it
# (dash_si()) and the allied selection skips the FALSE ones unless the advanced setting turns the
# screen off; checked below against the stored Strategic Index's flags. OPSI_SI_RESOURCE_SCREEN=0
# (scripts/20's robustness switch) sets the baseline to screen off.
step("Endowment screen (reserves and production themes)")
resource_tbls <- list(
  reserves = maybe_src(file.path(processed_dir, "reserves_tbl.rds")),
  production = maybe_src(file.path(processed_dir, "critical_minerals_production_tbl.rds"))
)
resource_tbls <- resource_tbls[!vapply(resource_tbls, is.null, logical(1))]
resource_base_tbl <- NULL
if (length(resource_tbls)) {
  resource_base_tbl <- do.call(strategic_resource_base, unname(resource_tbls))
  resource_base_tbl <- tibble::as_tibble(resource_base_tbl[resource_base_tbl$tech %in% techs,
                                                           c("Country", "tech", "supply_chain", "resource_base", "resource_evidence")])
  sources_params$resource_base <- paste0("strategic_resource_base(", paste(names(resource_tbls), collapse = ", "), ") over the headline technologies")
} else {
  notes <- c(notes, "No reserves or production theme table: the allied picks are not screened for a resource base.")
}
rm(resource_tbls)
si_resource_screen <- !identical(Sys.getenv("OPSI_SI_RESOURCE_SCREEN"), "0")
if (!si_resource_screen) notes <- c(notes, "OPSI_SI_RESOURCE_SCREEN=0: the baseline allied picks are not screened for a resource base (robustness setting).")

# ---- Partner dyads --------------------------------------------------------------------------------

read_long <- function(name, extra = character()) {
  x <- read_src(file.path(processed_dir, paste0(name, ".rds")))
  x <- as.data.frame(x)[c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable", "value", intersect(extra, names(x)))]
  x
}
step("Friendshore dyads")
fs_long <- read_long("partner_friendshore_tbl", extra = "source")
po_keys_U <- NULL
U_fs <- unique(c(fs_long$reporter_iso, fs_long$partner_iso))
fs_dup <- dash_count_duplicate_keys(fs_long, c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable"))
if (fs_dup > 0) stop("partner_friendshore_tbl has ", fs_dup, " duplicate rows per (dyad, cell, variable).")
fs_vars <- unique(fs_long$variable)
geo_in_fs <- "geo_alignment" %in% fs_vars
# CM-FDI (commit 78be8b5): the cells whose outbound rows name CM-FDI as their source hold the
# investment tie instead of the IMF position; their rows become outbound_index_cmfdi, so the IMF
# rows stay one [U, U] matrix (dash_bundle_fs_cmfdi()). chinese_parent_exposure rows exist only where
# the component is defined.
step("Friendshore CM-FDI parts")
cm_split <- dash_bundle_fs_cmfdi(fs_long, techs, stages)
fs_long <- cm_split$long
cpe_in_fs <- "chinese_parent_exposure" %in% fs_vars
pcells_cm <- dash_partner_cells_tbl(techs, stages)
partnership_cfg_path <- file.path(repo_root, "config", "partnership.yml")
partnership_cfg <- if (file.exists(partnership_cfg_path)) read_src(partnership_cfg_path, yaml::read_yaml) else list()
cm_settings <- cm_fdi_resolve_settings(partnership_cfg$cm_fdi)
cfg_cells <- if (isTRUE(cm_settings$investment_ties)) {
  cc <- cm_fdi_cells(cm_settings)
  which(paste(pcells_cm$tech, pcells_cm$supply_chain) %in% paste(cc$tech, cc$supply_chain))
} else integer()
cell_names <- function(ids) if (length(ids)) paste(pcells_cm$tech[ids], pcells_cm$supply_chain[ids], collapse = ", ") else "none"
add_check("CM-FDI investment-tie cells == config/partnership.yml cm_fdi", "friendshore",
          if (identical(as.integer(cm_split$cells), as.integer(cfg_cells))) "PASS" else "FAIL", cm_split$n_rows, NA,
          paste0("stored table: ", cell_names(cm_split$cells), "; config: ", cell_names(cfg_cells),
                 " (investment_ties ", isTRUE(cm_settings$investment_ties), ")"))
add_check("CM-FDI cells: every outbound row names CM-FDI", "friendshore", if (cm_split$n_mixed == 0) "PASS" else "FAIL",
          cm_split$n_rows, NA, paste0(cm_split$n_mixed, " outbound rows in a CM-FDI cell with another source"))
add_check("chinese_parent_exposure in the Friendshore table", "friendshore",
          if (cpe_in_fs == isTRUE(cm_settings$chinese_parent_exposure)) "PASS" else "FAIL", NA, NA,
          paste0("rows present ", cpe_in_fs, "; config chinese_parent_exposure ", isTRUE(cm_settings$chinese_parent_exposure)))
sources_params$fs_cmfdi <- paste0("partner_friendshore_tbl source column (CM-FDI rows) and chinese_parent_exposure rows; config/partnership.yml cm_fdi: ",
                                  cell_names(cm_split$cells))
step("Opportunity dyads (keys)")
po_long <- read_long("partner_opportunity_tbl")
U <- sort(unique(c(U_fs, po_long$reporter_iso, po_long$partner_iso)), method = "radix")
U <- U[!is.na(U)]
fs_spec <- c(imp_trade_index = "rpc", econ_opp_raw = "rc", es_need = "rc", eo_partner = "pc", outbound_index = "rp")
if (geo_in_fs) fs_spec <- c(fs_spec, geo_alignment = "rp")
if (length(cm_split$cells)) fs_spec <- c(fs_spec, outbound_index_cmfdi = "rpc")
if (cpe_in_fs) fs_spec <- c(fs_spec, chinese_parent_exposure = "pc")
fsb <- dash_bundle_dyads(fs_long, U, techs, fs_spec, "Friendshore Index", stages)
rm(fs_long)
invisible(gc())
cm_parts <- dash_bundle_fs_cmfdi_parts(fsb, cm_split)
fsb$outbound_index_cmfdi <- NULL
# The ownership values against the pipeline's own exposure table (destination x cell)
cm_exposure_tbl <- maybe_src(file.path(processed_dir, "partner_cm_fdi_exposure_tbl.rds"))
if (!is.null(cm_parts$cpe)) {
  nz <- which(!is.na(cm_parts$cpe), arr.ind = TRUE)
  if (!is.null(cm_exposure_tbl)) {
    ke <- match(paste(U[nz[, 1]], pcells_cm$tech[nz[, 2]], pcells_cm$supply_chain[nz[, 2]], sep = "\r"),
                paste(cm_exposure_tbl$destination_iso, cm_exposure_tbl$tech, cm_exposure_tbl$supply_chain, sep = "\r"))
    n_unused <- sum(cm_exposure_tbl$destination_iso %in% U) - nrow(nz)
    cmp_num("chinese_parent_exposure == partner_cm_fdi_exposure_tbl", "friendshore", cm_parts$cpe[nz],
            as.numeric(cm_exposure_tbl$chinese_parent_exposure[ke]), 0,
            paste0(nrow(nz), " partner cells; ", length(unique(nz[, 1])), " partners; ", n_unused,
                   " stored destination cells of world economies without a Friendshore dyad there"))
  } else {
    add_check("chinese_parent_exposure == partner_cm_fdi_exposure_tbl", "friendshore", "INFO", nrow(nz), NA, "no stored exposure table")
  }
}
po_dup <- dash_count_duplicate_keys(po_long, c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable"))
if (po_dup > 0) stop("partner_opportunity_tbl has ", po_dup, " duplicate rows per (dyad, cell, variable).")
po_spec <- c(trade_index = "rpc", econ_opp_index = "rc", partner_es_need = "pc")
pob <- dash_bundle_dyads(po_long, U, techs, po_spec, "Opportunity Index", stages)
rm(po_long)
invisible(gc())
for (ch in list(list("friendshore", fsb$checks), list("opportunity", pob$checks))) {
  for (i in seq_len(nrow(ch[[2]]))) {
    r <- ch[[2]][i, ]
    if (r$shape != "rpc") {
      add_check(paste0("component constant over its key: ", r$variable), ch[[1]],
                if (isTRUE(r$consistent)) "PASS" else "FAIL", r$n_rows, NA, paste0("shape ", r$shape, "; conflicts ", r$n_conflicts))
    }
  }
}
pcells <- dash_partner_cells_tbl(techs, stages)

# Climate penalties: the CAT climate-policy index per economy as scripts/15 cleans it, and the
# technology GHG weight. Two technology tables are candidates (the dyad builders read whichever
# scripts/15 passed); the one that reproduces the stored indices is kept.
cat_raw <- read_src(file.path(raw_dir, "CAT_country ratings data.csv"), read.csv)
policy_tbl <- partnership_strength_clean_policy(cat_prepare_ratings(cat_raw, country_info = country_info), country_info = country_info)
policy_iso <- policy_tbl %>%
  dplyr::mutate(Country = partnership_strength_standardize_countries(.data$Country),
                iso3c = partnership_strength_country_to_iso(.data$Country, country_info)) %>%
  dplyr::filter(!is.na(.data$iso3c)) %>%
  dplyr::transmute(.data$iso3c, .data$climate_policy_index)
cpi <- dash_bundle_cpi(policy_iso, U)
gh_candidates <- list()
tg <- maybe_src(file.path(processed_dir, "tech_ghg_tbl.rds"))
if (!is.null(tg)) gh_candidates$tech_ghg_tbl <- tg
ipcc_path <- file.path(raw_dir, "ipcc_ghg_intensity.csv")
if (file.exists(ipcc_path)) gh_candidates$ipcc_clean_ghg <- partnership_strength_clean_ghg(read_src(ipcc_path, read.csv))

# FS penalty coefficient as written in the Friendshore builder
fs_src <- readLines(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"), warn = FALSE)
pen_line <- grep("penalty_r = \\(1 - gh_entry\\) \\* cpi_r \\* [0-9.]+", fs_src, value = TRUE)
fs_penalty <- if (length(pen_line)) as.numeric(sub("^.*cpi_r \\* ([0-9.]+).*$", "\\1", pen_line[1])) else 0.10
sources_params$fs_penalty <- if (length(pen_line)) "R/themes/partnership_strength/safer_friendshore.R (penalty_r)" else "default 0.10"
po_w_cfg <- weights_cfg$partnership_opportunity_components
po_penalty <- if (!is.null(po_w_cfg$climate_policy_weight)) as.numeric(po_w_cfg$climate_policy_weight) else 0.2
fs_weights <- partnership_strength_resolve_weights(weights_cfg$partnership_friendshore_components,
                                                   partnership_strength_friendshore_default_weights(), "Friendshore component")
fs_weights <- lapply(fs_weights[partnership_strength_friendshore_components()], as.numeric)
po_weights <- lapply(partnership_strength_resolve_opportunity_weights(po_w_cfg)[c("trade_index", "econ_opp_index", "partner_es_need")], as.numeric)

pick_gh <- function(adj_fun, comp, w, coef, stored_idx, mask, label) {
  best <- NULL
  for (nm in names(gh_candidates)) {
    gh <- dash_bundle_gh(gh_candidates[[nm]], pcells)
    comp$gh <- gh
    a <- adj_fun(comp, w, coef)
    idx <- dash_scurve_cells(a$adj, mask)
    d <- abs(idx - stored_idx)
    bad_na <- sum(xor(is.na(idx), is.na(stored_idx)))
    mx <- suppressWarnings(max(d, na.rm = TRUE))
    res <- list(name = nm, gh = gh, max = mx, bad_na = bad_na, identical = bad_na == 0 && isTRUE(all(idx == stored_idx | is.na(idx))))
    step("  ", label, " with ", nm, ": max |diff| ", format(mx, digits = 3), ", NA mismatches ", bad_na)
    if (is.null(best) || (res$bad_na + res$max) < (best$bad_na + best$max)) best <- res
  }
  best
}
fs_comp <- list(imp_trade = fsb$imp_trade_index, econ_opp_raw = fsb$econ_opp_raw, es_need = fsb$es_need, eo_partner = fsb$eo_partner,
                outbound = fsb$outbound_index, outbound_source = cm_parts$outbound_source, outbound_cmfdi = cm_parts$outbound_cmfdi,
                cpe = cm_parts$cpe, geo = if (geo_in_fs) fsb$geo_alignment else matrix(NA_real_, length(U), length(U)),
                mask = fsb$mask, cpi = cpi)
po_comp <- list(trade = pob$trade_index, econ_opp_index = pob$econ_opp_index, partner_es_need = pob$partner_es_need, mask = pob$mask, cpi = cpi)
step("Validating the dyad indices against the stored ones")
fs_pick <- pick_gh(dash_fs_adj, fs_comp, fs_weights, fs_penalty, fsb[["Friendshore Index"]], fsb$mask, "Friendshore")
po_pick <- pick_gh(dash_po_adj, po_comp, po_weights, po_penalty, pob[["Opportunity Index"]], pob$mask, "Opportunity")
fs_comp$gh <- fs_pick$gh
po_comp$gh <- po_pick$gh
sources_params$gh <- paste0("Friendshore: ", fs_pick$name, "; Opportunity: ", po_pick$name, " (median fill for technologies without a value)")
stored_fs_index <- fsb[["Friendshore Index"]]
stored_po_index <- pob[["Opportunity Index"]]
rm(fsb, pob)
invisible(gc())

# Best-five and PSI tables
fs_top5_tbl <- read_src(file.path(processed_dir, "partner_friendshore_top5_tbl.rds"))
po_top5_tbl <- read_src(file.path(processed_dir, "partner_opportunity_top5_tbl.rds"))
dv_top5_tbl <- read_src(file.path(processed_dir, "partner_development_top5_tbl.rds"))
fs_top5 <- dash_bundle_top5(fs_top5_tbl, U, "Friendshore Index")
po_top5 <- dash_bundle_top5(po_top5_tbl, U, "Opportunity Index")
dv <- dv_top5_tbl[dv_top5_tbl$variable == "Development Potential Index", , drop = FALSE]
dp <- tibble::tibble(
  partner_iso = as.character(dv$partner_iso),
  development = as.numeric(dv$value),
  imputed_weight_share = if ("imputed_weight_share" %in% names(dv)) as.numeric(dv$imputed_weight_share) else NA_real_
)
dp <- dp[!duplicated(dp$partner_iso), , drop = FALSE]
psi_tbl_stored <- maybe_src(file.path(processed_dir, "psi_tbl.rds"))
psi_dyad_stored <- maybe_src(file.path(processed_dir, "psi_dyad_tbl.rds"))
partner_names <- dplyr::bind_rows(
  tibble::tibble(iso = fs_top5_tbl$partner_iso, name = fs_top5_tbl$partner),
  tibble::tibble(iso = po_top5_tbl$partner_iso, name = po_top5_tbl$partner),
  tibble::tibble(iso = dv_top5_tbl$partner_iso, name = dv_top5_tbl$partner)
) %>% dplyr::filter(!is.na(.data$name)) %>% dplyr::distinct(.data$iso, .keep_all = TRUE)
pnames <- partner_names$name[match(U, partner_names$iso)]
pnames <- dplyr::coalesce(pnames, country_info$country[match(U, country_info$iso3c)], U)
sovereign_U <- psi_is_sovereign(pnames, U)
psi_rule <- !is.null(psi_tbl_stored) && "rankable" %in% names(psi_tbl_stored)
psi_tau <- if (psi_rule) {
  if (!is.null(rankability$psi$max_imputed_weight_share)) rankability$psi$max_imputed_weight_share else rankability$max_imputed_weight_share
} else NA_real_
rm(fs_top5_tbl, po_top5_tbl, dv_top5_tbl)

# Geopolitical alignment (for the Partners view; the Friendshore component when the build has it)
geo_tbl <- maybe_src(file.path(processed_dir, "geo_alignment_tbl.rds"))
geo_dyad <- NULL
if (!is.null(geo_tbl)) {
  gr <- match(geo_tbl$reporter_iso, U)
  gp <- match(geo_tbl$partner_iso, U)
  ok <- !is.na(gr) & !is.na(gp)
  al <- matrix(NA_real_, length(U), length(U), dimnames = list(U, U))
  al[cbind(gr[ok], gp[ok])] <- as.numeric(geo_tbl$geo_alignment[ok])
  imp <- matrix(NA, length(U), length(U), dimnames = list(U, U))
  if (all(c("basis_reporter", "basis_partner") %in% names(geo_tbl))) {
    imp[cbind(gr[ok], gp[ok])] <- (geo_tbl$basis_reporter[ok] != "own_votes" | geo_tbl$basis_partner[ok] != "own_votes")
  }
  geo_dyad <- list(alignment = al, imputed = imp)
}
ideal <- maybe_src(file.path(processed_dir, "geo_ideal_points_tbl.rds"))

# PSI reporter candidates: the reporters whose ratings the stored partner-level PSI averages
reporter_candidates <- list()
if (!is.null(geo_coalition_tbl)) reporter_candidates$geo_coalition <- tryCatch(allied_coalition_iso3(geo_coalition_tbl), error = function(e) NULL)
reporter_candidates$coalition_tbl <- sort(coalition$iso3)
reporter_candidates$legacy_23 <- sort(allied_network_default_iso3c())
if (!is.null(psi_dyad_stored) && "coalition_reporter" %in% names(psi_dyad_stored)) {
  reporter_candidates <- c(list(psi_dyad_tbl = sort(unique(psi_dyad_stored$reporter_iso[psi_dyad_stored$coalition_reporter %in% TRUE]))), reporter_candidates)
}
reporter_candidates <- reporter_candidates[!vapply(reporter_candidates, is.null, logical(1))]

partner_bundle <- list(
  U = U,
  names = pnames,
  sovereign = sovereign_U,
  cells = pcells,
  fs = list(imp_trade = fs_comp$imp_trade, econ_opp_raw = fs_comp$econ_opp_raw, es_need = fs_comp$es_need,
            eo_partner = fs_comp$eo_partner, outbound = fs_comp$outbound, outbound_source = fs_comp$outbound_source,
            outbound_cmfdi = fs_comp$outbound_cmfdi, cpe = fs_comp$cpe, geo = if (geo_in_fs) fs_comp$geo else NULL,
            mask = fs_comp$mask, cpi = cpi, gh = fs_comp$gh),
  po = list(trade = po_comp$trade, econ_opp_index = po_comp$econ_opp_index, partner_es_need = po_comp$partner_es_need,
            mask = po_comp$mask, cpi = cpi, gh = po_comp$gh),
  dp = dp,
  top5_share = list(fs = fs_top5$share, po = po_top5$share),
  psi = list(coalition_reporters = reporter_candidates[[1]], tau = psi_tau),
  geo_dyad = geo_dyad
)
rm(fs_comp, po_comp)
invisible(gc())

# ---- Coalition members and nodes -------------------------------------------------------------------
#
# The original allied network design (scripts/30, its greedy baseline and the network chunk) is
# retired (2026-10-17) and replaced by the Allied Industrial Base analysis (docs/aib_methodology.md).
# The coalition's members and nodes (the EU as one node) still label the economies in the dims
# (countries$network_node, nodes): the coalition helpers of R/indices/allied_network_design.R.

step("Coalition members and nodes")
network_members <- if (!is.null(coalition_tbl)) allied_network_add_node_members(coalition, coalition_tbl) else dplyr::mutate(coalition, node_only = FALSE)
members_net <- tibble::tibble(
  iso3c = network_members$iso3,
  country = dplyr::coalesce(network_members$country, country_info$country[match(network_members$iso3, country_info$iso3c)]),
  tier = network_members$tier,
  network_node = network_members$network_node,
  node_tier = if ("node_tier" %in% names(network_members)) network_members$node_tier else network_members$tier,
  node_only = network_members$node_only %in% TRUE
)
# GDP weights of a node's map position (the EU node sits at its members' GDP-weighted centre)
gdp_raw <- read_src(file.path(raw_dir, "wdi_gdp.csv"), read.csv, stringsAsFactors = FALSE)
country_size <- allied_network_country_size_tbl(gdp_raw, country_info, members_net$iso3c)
node_labels <- c(EUU = "European Union")

# ---- Dims -----------------------------------------------------------------------------------------

step("Dimensions")
all_iso <- sort(unique(c(U, country_tbl$iso3c[!is.na(country_tbl$iso3c)], members_net$iso3c)), method = "radix")
cown <- suppressWarnings(countrycode::countrycode(all_iso, "iso3c", "cown", custom_match = SOVEREIGN_COW_CUSTOM_MATCH, warn = FALSE))
ci_cols <- intersect(c("region", "income", "income_group", "incomeLevel"), names(country_info))
inc_col <- intersect(c("income_group", "income", "incomeLevel"), names(country_info))[1]
lonlat <- tibble::tibble(iso3c = all_iso, lon = NA_real_, lat = NA_real_)
if (requireNamespace("sf", quietly = TRUE) && requireNamespace("rnaturalearth", quietly = TRUE)) {
  ll <- tryCatch({
    suppressMessages(sf::sf_use_s2(FALSE))
    w <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    code <- ifelse(!is.na(w$iso_a3) & w$iso_a3 != "-99", w$iso_a3, ifelse(!is.na(w$iso_a3_eh) & w$iso_a3_eh != "-99", w$iso_a3_eh, w$adm0_a3))
    pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(w)))
    xy <- sf::st_coordinates(pts)
    tibble::tibble(iso3c = code, lon = xy[, 1], lat = xy[, 2]) %>% dplyr::distinct(.data$iso3c, .keep_all = TRUE)
  }, error = function(e) NULL)
  if (!is.null(ll)) {
    k <- match(all_iso, ll$iso3c)
    lonlat$lon <- ll$lon[k]
    lonlat$lat <- ll$lat[k]
  }
}
coal_k <- if (!is.null(coalition_tbl)) match(all_iso, toupper(coalition_tbl$iso3)) else rep(NA_integer_, length(all_iso))
ideal_k <- if (!is.null(ideal)) match(all_iso, ideal$iso3) else rep(NA_integer_, length(all_iso))
iso_n3 <- suppressWarnings(countrycode::countrycode(all_iso, "iso3c", "iso3n", warn = FALSE))
countries_dim <- tibble::tibble(
  iso3c = all_iso,
  iso_n3 = ifelse(is.na(iso_n3), NA_character_, sprintf("%03d", as.integer(iso_n3))),   # world-atlas feature ids
  country = dplyr::coalesce(country_info$country[match(all_iso, country_info$iso3c)], pnames[match(all_iso, U)], all_iso),
  region = if ("region" %in% names(country_info)) country_info$region[match(all_iso, country_info$iso3c)] else NA_character_,
  income_group = if (!is.na(inc_col)) as.character(country_info[[inc_col]][match(all_iso, country_info$iso3c)]) else NA_character_,
  sovereign = !is.na(cown),
  tier = dplyr::coalesce(if (!is.null(coalition_tbl)) tolower(coalition_tbl$tier[coal_k]) else NA_character_,
                         ifelse(all_iso %in% coalition$iso3, coalition$tier[match(all_iso, coalition$iso3)], "none")),
  network_node = members_net$network_node[match(all_iso, members_net$iso3c)],
  eu_member = if (!is.null(coalition_tbl) && "eu_member" %in% names(coalition_tbl)) coalition_tbl$eu_member[coal_k] %in% TRUE else FALSE,
  lon = lonlat$lon,
  lat = lonlat$lat,
  bloc = if (!is.null(ideal)) as.character(ideal$bloc[ideal_k]) else NA_character_,
  p_us = if (!is.null(ideal)) as.numeric(ideal$p_us[ideal_k]) else NA_real_
)
node_tbl <- allied_network_node_table(allied_network_check_members(members_net, members_net$iso3c))
gdp_m <- country_size$gdp_usd[match(members_net$iso3c, country_size$iso3c)]
nodes_dim <- tibble::tibble(
  network_node = node_tbl$network_node,
  label = ifelse(node_tbl$network_node %in% names(node_labels), node_labels[node_tbl$network_node],
                 countries_dim$country[match(node_tbl$network_node, countries_dim$iso3c)]),
  lon = vapply(node_tbl$network_node, function(n) {
    k <- members_net$network_node == n
    x <- countries_dim$lon[match(members_net$iso3c[k], countries_dim$iso3c)]
    w <- gdp_m[k]
    ok <- is.finite(x) & is.finite(w)
    if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_
  }, numeric(1)),
  lat = vapply(node_tbl$network_node, function(n) {
    k <- members_net$network_node == n
    x <- countries_dim$lat[match(members_net$iso3c[k], countries_dim$iso3c)]
    w <- gdp_m[k]
    ok <- is.finite(x) & is.finite(w)
    if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_
  }, numeric(1)),
  n_members = node_tbl$n_members,
  tier = node_tbl$tier
)
cells_dim <- pcells
cells_dim$sector <- paste(cells_dim$tech, cells_dim$supply_chain, sep = " - ")
dims <- list(
  countries = countries_dim,
  nodes = nodes_dim,
  techs = techs,
  stages = stages,
  cells = cells_dim,
  fossil_techs = c("Coal", "Oil", "Gas"),
  es_categories = names(es_weights),
  eo_categories = names(eo_weights),
  U = U
)

# ---- Baseline inputs and the bundle ------------------------------------------------------------------

baseline_inputs <- c(
  list(
    es_weights = es_weights,
    eo_weights = eo_weights,
    tau = if (pillar_rule) as.numeric(rankability$max_imputed_weight_share) else NA_real_,
    allow_partial_categories = isTRUE(config$allow_partial_categories),
    policy_weights = policy_weights,
    policy_iea_var = "IEA PAMS Policy Index",
    policy_cat_var = "CAT Policy Index",
    si_weights = STRATEGIC_INDEX_WEIGHTS,
    # the six stages' weights (each stage an equal share of its roll-up's 0.50 / 0.75 / 0.25), as
    # build_strategic_index() weights them in scripts/20; checked against the stored sc_weight below
    stage_weights = SUPPLY_CHAIN_STAGE_SI_WEIGHTS[stages],
    excluded_sectors = as.character(stored$strategic_excluded_sectors),
    allied_excluded_techs = eval(formals(select_allied_strategic_sectors)$excluded_techs),
    # the stages the allied pick may take (scripts/20 passes stages = STRATEGIC_ALLIED_STAGES; the
    # function's default also accepts the old Upstream / Midstream labels, which no cell carries)
    allied_stages = STRATEGIC_ALLIED_STAGES,
    require_resource_base = si_resource_screen && isTRUE(eval(formals(select_allied_strategic_sectors)$require_resource_base)),
    fossil_techs = c("Coal", "Oil", "Gas"),
    fossil_threshold = 90,
    fs_weights = fs_weights,
    fs_penalty = fs_penalty,
    po_weights = po_weights,
    po_penalty = po_penalty,
    psi_weights = unlist(weights_cfg$psi)[c("friendshore", "opportunity", "development")],
    psi_view = "coalition",
    psi_tau = psi_tau,
    top_k = 5L
  )
)
sources_params$tau <- if (pillar_rule) "config/rankability.yml" else "NA: the stored pillars predate the rankability rule"
sources_params$stage_weights <- "SUPPLY_CHAIN_STAGE_SI_WEIGHTS (R/utils/supply_chain_stages.R)"
sources_params$excluded_sectors <- "index_outputs$strategic_excluded_sectors less its degenerate sectors (scripts/20: unmeasured and unevidenced sectors; the engine finds the degenerate ones, dash_si_rule_excluded())"
sources_params$allied_stages <- "STRATEGIC_ALLIED_STAGES (R/indices/build_indices.R), as scripts/20 passes it"
if (!all(STRATEGIC_ALLIED_STAGES %in% stages)) stop("STRATEGIC_ALLIED_STAGES names a stage outside the bundle's stages.")
if (!pillar_rule) notes <- c(notes, "The stored ES / EO pillars predate the rankability rule: every scored cell counts as rankable at the baseline (tau = rule off).")

manifest <- list(
  bundle_version = 1L,
  created = Sys.time(),
  git_commit = tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_),
  osi_branch = tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_),
  sources = NULL,
  sources_params = sources_params,
  flags = list(
    pillar_rule = pillar_rule,
    geo_alignment_in_fs = geo_in_fs,
    # Friendshore's CM-FDI parts (investment ties in the CM-FDI cells, Chinese-parent ownership)
    fs_cmfdi = length(cm_split$cells) > 0 || cpe_in_fs,
    partner_pillar_consistent = FALSE
  ),
  baseline_inputs = baseline_inputs,
  validation = NULL,
  notes = notes
)
si_parts <- list(tech_ghg = if (!is.null(tg)) tibble::as_tibble(tg)[c("tech", "ghg_index")] else tibble::tibble(tech = character(), ghg_index = numeric()),
                 trl = tibble::as_tibble(trl), allies = tibble::as_tibble(allies))
if (!is.null(resource_base_tbl)) si_parts$resource_base <- resource_base_tbl
bundle <- dash_bundle_assemble(manifest, dims, pillars, policy_bundle, si_parts, partner_bundle)
# Ship the rule-based exclusions only: scripts/20 appends the sectors in which a pillar is constant
# across countries to its list; pre-excluded, the engine would keep them out at every setting as "not
# reported (pipeline rule)". It finds them itself (checked below: SI excluded sectors ==
# strategic_excluded_sectors).
p0 <- bundle$manifest$baseline
rule_excluded <- dash_si_rule_excluded(bundle, p0, dash_pillars(bundle, p0), dash_policy(bundle, p0), as.character(stored$strategic_excluded_sectors))
if (!identical(rule_excluded, manifest$baseline_inputs$excluded_sectors)) {
  step("  excluded_sectors: ", length(rule_excluded), " by rule; left to the engine (a pillar constant across countries): ",
       paste(setdiff(manifest$baseline_inputs$excluded_sectors, rule_excluded), collapse = ", "))
  manifest$baseline_inputs$excluded_sectors <- rule_excluded
  bundle <- dash_bundle_assemble(manifest, dims, pillars, policy_bundle, si_parts, partner_bundle)
}
rm(p0)
dash_validate_bundle(bundle)

# ---- Validation: the engine at the baseline against the stored outputs ---------------------------------

step("Engine at the baseline")
params <- bundle$manifest$baseline
t0 <- Sys.time()
res <- dash_run(bundle, params)
step("  dash_run: ", format(round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)), " s")

key3 <- function(x) paste(x$Country, x$tech, x$supply_chain, sep = "\r")

# Pillars
for (p in c("es", "eo")) {
  col <- DASH_PILLARS[[p]]
  st <- stored[[paste0(p, "_index")]]
  st <- st[st$tech %in% techs, , drop = FALSE]
  en <- res$pillars[[p]]
  k <- match(key3(st), key3(en))
  add_check(paste0(toupper(p), " cells present"), "pillars", if (!anyNA(k) && nrow(st) == nrow(en)) "PASS" else "FAIL", nrow(st), NA,
            paste0(sum(is.na(k)), " stored cells missing; engine ", nrow(en)))
  cmp_num(paste0(toupper(p), " value"), "pillars", en$value[k], st[[col]])
  if ("imputed_weight_share" %in% names(st)) cmp_num(paste0(toupper(p), " imputed_weight_share"), "pillars", en$imputed_weight_share[k], st$imputed_weight_share, 1e-12)
  if ("rankable" %in% names(st)) {
    add_check(paste0(toupper(p), " rankable / rank_exclusion"), "pillars",
              if (identical(en$rankable[k], st$rankable) && identical(en$rank_exclusion[k], st$rank_exclusion)) "PASS" else "FAIL", nrow(st))
  }
  cc <- stored[[paste0(p, "_cc")]]
  ec <- res$pillars[[paste0(p, "_contrib")]]
  kc <- match(paste(key3(cc), cc$category, sep = "\r"), paste(key3(ec), ec$category, sep = "\r"))
  cmp_num(paste0(toupper(p), " category_weight"), "pillars", ec$category_weight[kc], cc$category_weight, 1e-12)
  cmp_num(paste0(toupper(p), " weighted_category_contribution"), "pillars", ec$contribution[kc], cc$weighted_category_contribution, 1e-12)
}
# Policy
sp <- stored$policy_index
ep <- res$policy$index
kp <- match(key3(sp), key3(ep))
add_check("Policy cells present", "policy", if (!anyNA(kp) && nrow(sp) == nrow(ep)) "PASS" else "FAIL", nrow(sp), NA, paste0("engine ", nrow(ep)))
cmp_num("Policy value", "policy", ep$value[kp], sp$value)
add_check("Policy n_components", "policy", if (identical(as.integer(ep$n_components[kp]), as.integer(sp$n_components))) "PASS" else "FAIL", nrow(sp))
se <- stored$policy_eligibility
ee <- res$policy$eligibility
ke <- match(se$Country, ee$Country)
add_check("Policy eligibility", "policy", if (!anyNA(ke) && identical(ee$eligible[ke], se$eligible)) "PASS" else "FAIL", nrow(se))
# Character / logical columns: the same value or both NA, row by row; a column the stored table lacks
# (a build before the rule) is reported, not failed
same_vals <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
cmp_str <- function(check, layer, a, b, stored_has = TRUE, note = NA_character_) {
  if (!stored_has) return(add_check(check, layer, "INFO", length(a), NA, "the stored table has no such column (an earlier build)"))
  bad <- if (length(a) == length(b)) sum(!same_vals(a, b)) else NA_integer_
  add_check(check, layer, if (identical(bad, 0L)) "PASS" else "FAIL", length(b), NA,
            paste0(if (is.na(bad)) "lengths differ" else paste0(bad, " differ"), if (!is.na(note)) paste0("; ", note) else ""))
}
# policy_basis ("IEA PAMS + CAT" / "IEA PAMS only") and missing_required (the 9 Oct 2026 rule)
cmp_str("Policy eligibility policy_basis", "policy", ee$policy_basis[ke], se$policy_basis, "policy_basis" %in% names(se),
        paste(names(table(se$policy_basis)), table(se$policy_basis), sep = ": ", collapse = ", "))
cmp_str("Policy eligibility missing_required", "policy", ee$missing_required[ke], se$missing_required, "missing_required" %in% names(se),
        paste0(sum(!is.na(se$missing_required)), " countries lack a required component"))
cmp_str("Policy eligibility missing_components", "policy", ee$missing_components[ke], se$missing_components, "missing_components" %in% names(se))
cmp_str("Policy index policy_basis", "policy", ep$policy_basis[kp], sp$policy_basis, "policy_basis" %in% names(sp))
# Strategic Index
ss <- stored$strategic_index
es_ <- res$si$cells
ks <- match(key3(ss), key3(es_))
add_check("SI cells present", "si", if (!anyNA(ks) && nrow(ss) == nrow(es_)) "PASS" else "FAIL", nrow(ss), NA, paste0("engine ", nrow(es_)))
for (col in c("strategic_index", "pol", "eo", "es_need", "Economic_Opportunity_Index", "Energy_Security_Index", "value", "sc_weight", "tech_weight", "trl_index")) {
  if (col %in% names(ss)) cmp_num(paste0("SI ", col), "si", es_[[col]][ks], ss[[col]])
}
add_check("SI si_exclusion strings", "si", if (identical(es_$si_exclusion[ks], ss$si_exclusion)) "PASS" else "FAIL", nrow(ss), NA,
          paste0(sum(es_$si_exclusion[ks] != ss$si_exclusion | xor(is.na(es_$si_exclusion[ks]), is.na(ss$si_exclusion)), na.rm = TRUE), " differ"))
cmp_str("SI policy_basis", "si", es_$policy_basis[ks], ss$policy_basis, "policy_basis" %in% names(ss),
        paste0(sum(ss$policy_basis %in% "IEA PAMS only" & !is.na(ss$strategic_index)), " scored cells on IEA PAMS only"))
# The endowment screen: the engine's flags (bundle$si$resource_base, recomputed above) against the stored SI's
cmp_str("SI resource_base (endowment screen)", "si", es_$resource_base[ks], ss$resource_base, "resource_base" %in% names(ss),
        paste0(sum(ss$resource_base %in% FALSE & !is.na(ss$strategic_index)), " of ",
               sum(!is.na(ss$resource_base) & !is.na(ss$strategic_index)), " scored screened cells without a resource base"))
cmp_str("SI resource_evidence", "si", es_$resource_evidence[ks], ss$resource_evidence, "resource_evidence" %in% names(ss))
cmp_str("SI pick_exclusion", "si", es_$pick_exclusion[ks], ss$pick_exclusion, "pick_exclusion" %in% names(ss))
# The sectors left out: the engine's (the shipped list plus any it finds degenerate) against scripts/20's
ex_eng <- res$si$excluded$sector
ex_st <- unique(as.character(stored$strategic_excluded_sectors))
add_check("SI excluded sectors == strategic_excluded_sectors", "si",
          if (setequal(ex_eng, ex_st) && length(ex_eng) == length(ex_st)) "PASS" else "FAIL", length(ex_st), NA,
          paste0(length(setdiff(ex_eng, ex_st)), " extra in the engine (", paste(setdiff(ex_eng, ex_st), collapse = ", "), "), ",
                 length(setdiff(ex_st, ex_eng)), " missing"))
sc_ok <- "sc_weight" %in% names(ss) &&
  isTRUE(all(ss$sc_weight == unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[as.character(ss$supply_chain)])))
add_check("SI stored sc_weight == SUPPLY_CHAIN_STAGE_SI_WEIGHTS by stage", "si", if (sc_ok) "PASS" else "FAIL",
          nrow(ss), NA, paste0("stages ", paste(sort(unique(as.character(ss$supply_chain))), collapse = ", ")))
allied_csv <- maybe_src(file.path(processed_dir, "charts", "allied_strategic.csv"), read.csv, stringsAsFactors = FALSE)
if (!is.null(allied_csv)) {
  a_eng <- paste(res$allied$Country, res$allied$sector, sep = "\r")
  a_st <- paste(allied_csv$Country, allied_csv$sector, sep = "\r")
  same_set <- setequal(a_eng, a_st) && length(a_eng) == length(a_st)
  add_check("Allied selection == charts/allied_strategic.csv", "si", if (same_set) "PASS" else "FAIL", length(a_st), NA,
            paste0(length(unique(allied_csv$Country)), " members picked; ", length(setdiff(a_st, a_eng)), " stored picks missing, ",
                   length(setdiff(a_eng, a_st)), " extra; endowment screen ", if (isTRUE(params$si$require_resource_base)) "on" else "off"))
  ka <- match(a_st, a_eng)
  if (!anyNA(ka)) {
    cmp_num("Allied strategic_index == charts/allied_strategic.csv", "si", res$allied$strategic_index[ka], allied_csv$strategic_index)
    cmp_str("Allied policy_basis == charts/allied_strategic.csv", "si", res$allied$policy_basis[ka], allied_csv$policy_basis,
            "policy_basis" %in% names(allied_csv))
    cmp_str("Allied resource_base == charts/allied_strategic.csv", "si", res$allied$resource_base[ka], as.logical(allied_csv$resource_base),
            "resource_base" %in% names(allied_csv))
  }
  # Diagnostic: the picks with the screen flipped (the S17 parity scenario, the paper's robustness run)
  p_flip <- params
  p_flip$si$require_resource_base <- !isTRUE(params$si$require_resource_base)
  a_flip <- dash_allied_selection(res$si, bundle, p_flip)
  moved <- unique(a_flip$Country[!paste(a_flip$Country, a_flip$sector, sep = "\r") %in% a_eng])
  to <- sort(table(a_flip$sector[a_flip$Country %in% moved]), decreasing = TRUE)
  add_check(paste0("Allied selection with the endowment screen ", if (isTRUE(p_flip$si$require_resource_base)) "on" else "off"), "si", "INFO",
            length(unique(a_flip$Country)), NA,
            paste0(length(moved), " members change pick", if (length(moved)) paste0(" (to ", paste(names(to), to, collapse = ", "), ")") else ""))
}
# Dyads
fs_eng <- res$partner$cells$fs_index
po_eng <- res$partner$cells$po_index
cmp_num("Friendshore index array", "partner", as.vector(fs_eng), as.vector(stored_fs_index), 1e-12, paste0("gh: ", fs_pick$name))
cmp_num("Opportunity index array", "partner", as.vector(po_eng), as.vector(stored_po_index), 1e-12, paste0("gh: ", po_pick$name))
add_check("Friendshore index bit-identical", "partner", if (isTRUE(identical(unname(fs_eng), unname(stored_fs_index)))) "PASS" else "FAIL", length(fs_eng),
          NA, paste0("CM-FDI cells: ", cell_names(cm_split$cells), "; ownership component ", if (cpe_in_fs) "on" else "off"))
fsm <- res$partner$top5$fs
pom <- res$partner$top5$po
cmp_num("Friendshore best-five", "partner", fsm[fs_top5$exists], fs_top5$value[fs_top5$exists], 1e-12)
cmp_num("Opportunity best-five", "partner", pom[po_top5$exists], po_top5$value[po_top5$exists], 1e-12)
# PSI: which reporters does the stored partner-level PSI average?
psi_view_pick <- NULL
if (!is.null(psi_tbl_stored)) {
  best <- NULL
  for (nm in c(names(reporter_candidates), "world")) {
    p2 <- params
    if (nm == "world") {
      p2$partner$psi_view <- "world"
    } else {
      bundle$partner$psi$coalition_reporters <- reporter_candidates[[nm]]
    }
    pv <- dash_psi(bundle, res$partner$top5, p2)$partner
    k <- match(psi_tbl_stored$partner_iso, pv$partner_iso)
    d <- suppressWarnings(max(abs(pv$psi[k] - psi_tbl_stored$value), na.rm = TRUE))
    nam <- sum(xor(is.na(pv$psi[k]), is.na(psi_tbl_stored$value)))
    step("  PSI partner view with reporters '", nm, "': max |diff| ", format(d, digits = 3), ", NA mismatches ", nam)
    if (is.null(best) || (nam + d) < (best$nam + best$d)) best <- list(name = nm, d = d, nam = nam)
  }
  psi_view_pick <- best
  if (best$name == "world") {
    bundle$manifest$baseline_inputs$psi_view <- "world"
    bundle$partner$psi$coalition_reporters <- reporter_candidates[[1]]
    notes <- c(notes, "The stored partner-level PSI averages every reporter (world view); the baseline view is set to match it.")
  } else {
    bundle$partner$psi$coalition_reporters <- reporter_candidates[[best$name]]
  }
  add_check("PSI partner view == psi_tbl", "partner", if (best$nam == 0 && best$d <= 1e-10) "PASS" else "FAIL", nrow(psi_tbl_stored), best$d,
            paste0("reporters: ", best$name))
  sources_params$psi_reporters <- paste0("matched to the stored psi_tbl: ", best$name)
  bundle$manifest$sources_params <- sources_params
  bundle$manifest$notes <- notes
  bundle$manifest$baseline <- dash_params(bundle = bundle)
  params <- bundle$manifest$baseline
  res <- dash_run(bundle, params)
}
if (!is.null(psi_dyad_stored)) {
  dy <- res$partner$psi$dyad
  kd <- match(paste(psi_dyad_stored$reporter_iso, psi_dyad_stored$partner_iso), paste(dy$reporter_iso, dy$partner_iso))
  add_check("PSI dyads present", "partner", if (!anyNA(kd) && nrow(dy) == nrow(psi_dyad_stored)) "PASS" else "FAIL", nrow(psi_dyad_stored), NA, paste0("engine ", nrow(dy)))
  cmp_num("PSI dyad value", "partner", dy$psi[kd], psi_dyad_stored$Partnership_Strength_Index)
}
# ---- Bilateral ties (Partners view, Country partners section) ------------------------------------
#
# Auxiliary chunks read by the browser on demand, never by the engine (R/dashboard/dash_ties.R):
# `ties` (IMF direct investment positions by partner with the conduit centres, the CM-FDI factory
# projects behind the investment tie, Development Potential by sector, agreements, UN-vote alignment,
# top partners by the best-five summaries) and one `pcell-<slug>` per sector (each economy's top
# suppliers and markets at the baseline). The positions are checked against the raw IMF file: the
# pipeline's own builder on that file reproduces the bundle's investment tie, and so does ranking the
# shipped positions (dash_dip_index()).

step("Bilateral ties: IMF direct investment positions, CM-FDI flows, per-sector top partners")
dip_scale <- if ("SCALE" %in% names(dip_raw)) sort(unique(dip_raw$SCALE)) else NA_character_
# The window the pipeline averages the positions over (scripts/15 partner_trade_years) and its GDP
# year (safer_friendshore() default): checked below, since the builder with these settings must
# reproduce the bundle's investment tie.
dip_years <- 2020:2024
dip_gdp_year <- eval(formals(safer_friendshore)$gdp_year)
conduit_cfg <- partnership_cfg$outbound_fdi
conduit_rule <- partnership_strength_conduit_rule(rule = conduit_cfg$conduit_rule %||% "conduits", scope = conduit_cfg$conduit_scope %||% "both")
gdp_dip <- read_src(file.path(raw_dir, "wdi_gdp.csv"), read.csv, stringsAsFactors = FALSE)
U_p <- bundle$partner$U
ob_bundle <- bundle$partner$fs$outbound
if (is.null(ob_bundle)) ob_bundle <- matrix(NA_real_, length(U_p), length(U_p))
edges_pipe <- partnership_strength_build_outbound_edges(
  dip_raw[dip_raw$INDICATOR == DASH_DIP_MEASURES[["out_debt"]], , drop = FALSE], country_info, gdp_dip,
  gdp_year = dip_gdp_year, years = dip_years, conduit_iso = conduit_rule$iso3, conduit_scope = conduit_rule$scope
)
cmp_num("IMF positions (raw file) -> investment tie (pipeline builder) == bundle", "ties", c(dash_dyad_matrix(edges_pipe, U_p)), c(ob_bundle), tol = 0,
        note = paste0(basename(dip_path), "; mean of ", min(dip_years), "-", max(dip_years), "; GDP ", dip_gdp_year, "; conduit rule ", conduit_cfg$conduit_rule %||% "conduits"))
dip_pos <- dash_dip_positions(dip_raw, country_info, index_years = dip_years)
rm(dip_raw)
invisible(gc())
edges_ship <- dash_dip_index(dip_pos, gdp_dip, dip_gdp_year, conduit_rule$iso3, conduit_rule$scope)
cmp_num("Shipped IMF positions, ranked -> investment tie == bundle", "ties", c(dash_dyad_matrix(edges_ship, U_p)), c(ob_bundle), tol = 0,
        note = paste0(nrow(edges_ship), " dyads with a position in the index (", nrow(edges_pipe), " from the builder)"))
add_check("IMF position unit", "ties", if (length(dip_scale) == 1 && grepl("^Millions$", dip_scale)) "PASS" else "FAIL", nrow(dip_pos), NA,
          paste0("SCALE: ", paste(dip_scale, collapse = ", "), " (shipped as US$ millions)"))
cmfdi_ties_tbl <- read_src(file.path(processed_dir, "partner_cm_fdi_ties_tbl.rds"))
cmfdi_flows <- dash_cmfdi_flows(cmfdi_ties_tbl)
rm(cmfdi_ties_tbl)
cm_src <- bundle$partner$fs$outbound_source
if (!is.null(cm_src) && any(cm_src == "cm_fdi") && !is.null(bundle$partner$fs$outbound_cmfdi)) {
  cm_cells <- which(cm_src == "cm_fdi")
  ck <- match(paste(cmfdi_flows$tech, cmfdi_flows$supply_chain), paste(bundle$partner$cells$tech, bundle$partner$cells$supply_chain)[cm_cells])
  fr <- match(cmfdi_flows$investor_iso, U_p)
  fp <- match(cmfdi_flows$destination_iso, U_p)
  okf <- !is.na(ck) & !is.na(fr) & !is.na(fp)
  # partnership_strength_cm_fdi_tie_index(): a reporter with a project in some partner of a cell has
  # a tie with every partner of that cell (the partners without a project as one block below), and
  # none (NA) without; so the reporter-cells with a tie are those with a shipped flow, and within
  # them every partner with a flow scores above every partner without one
  oc <- bundle$partner$fs$outbound_cmfdi
  has_flow <- array(FALSE, dim(oc))
  has_flow[cbind(fr[okf], fp[okf], ck[okf])] <- TRUE
  rc_flow <- apply(has_flow, c(1, 3), any)
  rc_tie <- apply(!is.na(oc), c(1, 3), any)
  n_mis <- sum(rc_flow != rc_tie)
  n_order <- 0L
  for (ix in which(rc_tie & rc_flow)) {
    r <- (ix - 1L) %% nrow(rc_tie) + 1L
    k <- (ix - 1L) %/% nrow(rc_tie) + 1L
    v <- oc[r, , k]
    fl <- has_flow[r, , k]
    if (any(!fl & !is.na(v)) && min(v[fl], na.rm = TRUE) <= max(v[!fl], na.rm = TRUE)) n_order <- n_order + 1L
  }
  add_check("CM-FDI flows shipped == the investment ties (reporter-cells with a tie; partners with a project above the rest)", "ties",
            if (n_mis == 0 && n_order == 0) "PASS" else "FAIL", sum(rc_tie), NA,
            paste0(nrow(cmfdi_flows), " flows (", sum(!okf), " outside the economies or cells); ", n_mis, " reporter-cells differ; ", n_order, " out of order"))
  rm(oc, has_flow)
}
dev_tbl_p <- read_src(file.path(processed_dir, "partner_development_tbl.rds"))
dp_cells <- dash_dp_cells(dev_tbl_p, bundle$partner$cells)
rm(dev_tbl_p)
add_check("Development Potential by sector: one value per partner and sector", "ties",
          if (!anyDuplicated(paste(dp_cells$partner_iso, dp_cells$cell_id)) && all(unique(dp_cells$partner_iso) %in% bundle$partner$dp$partner_iso)) "PASS" else "FAIL",
          nrow(dp_cells), NA, paste0(length(unique(dp_cells$partner_iso)), " partners x ", length(unique(dp_cells$cell_id)), " sectors"))
agreements_ties <- read_src(file.path(repo_root, "data", "reference", "economic_security_agreements.csv"), read.csv, stringsAsFactors = FALSE)
dy_res <- res$partner$psi$dyad
top_all <- dplyr::bind_rows(
  tibble::add_column(dash_top_partners(dash_dyad_matrix(dy_res, U_p, "friendshore"), U_p, bundle$partner$sovereign), side = "fs", .after = "reporter_iso"),
  tibble::add_column(dash_top_partners(dash_dyad_matrix(dy_res, U_p, "opportunity"), U_p, bundle$partner$sovereign), side = "po", .after = "reporter_iso")
)
top_all$psi_rank <- dash_psi_rank_of(top_all, dy_res)
conduits_ship <- partnership_strength_conduit_centres()
conduits_ship$in_rule <- conduits_ship$iso3 %in% conduit_rule$iso3
ties_meta <- list(
  dip_years = I(dip_years), dip_gdp_year = dip_gdp_year, dip_unit = "US$ millions",
  dip_source = "IMF, Direct Investment Positions (DIP), reported official data",
  conduit_rule = conduit_cfg$conduit_rule %||% "conduits", conduit_scope = conduit_rule$scope, top_k = 10L,
  # the stages whose cells carry the CM-FDI investment tie (config/partnership.yml cm_fdi.stages,
  # Components and Assembly), each with the same projects: the cmfdi rows repeat every flow once
  # per stage, so counts and totals across stages must be taken once per project
  cmfdi_stages = I(if (length(cm_split$cells)) unique(as.character(bundle$partner$cells$supply_chain[cm_split$cells])) else character())
)
# the CC BY 4.0 attribution of CM-FDI, as the Sector module shows it (config/sector_dictionary.yml sources)
sector_dict_ties <- maybe_src(file.path(repo_root, "config", "sector_dictionary.yml"), yaml::read_yaml)
for (s_src in sector_dict_ties$sources %||% list()) if (identical(s_src$id, "cmfdi")) ties_meta$cmfdi_attribution <- s_src$attribution
add_check("CM-FDI attribution in the ties chunk", "ties", if (is.character(ties_meta$cmfdi_attribution) && grepl("CC BY", ties_meta$cmfdi_attribution)) "PASS" else "FAIL",
          NA, NA, "config/sector_dictionary.yml sources: cmfdi")
rm(sector_dict_ties)
ties_chunks <- c(
  dash_ties_chunk(dip_pos, conduits_ship, cmfdi_flows, dp_cells, agreements_ties,
                  if (!is.null(bundle$partner$geo_dyad)) bundle$partner$geo_dyad$alignment else NULL, top_all, U_p, ties_meta, bundle$partner$names),
  dash_cell_chunks(res$partner$cells, dy_res, U_p, bundle$partner$sovereign, bundle$partner$cells, k = 10L)
)
add_check("Per-sector top partners: one chunk per sector", "ties", if (length(ties_chunks) == nrow(bundle$partner$cells) + 1) "PASS" else "FAIL",
          length(ties_chunks) - 1, NA, "top 10 suppliers (Friendshore) and markets (Partnership Opportunity) per economy at the baseline")
rm(dip_pos, edges_pipe, edges_ship, gdp_dip)
invisible(gc())

# ---- Write -----------------------------------------------------------------------------------------

validation <- dplyr::bind_rows(val_rows)
# A FAIL stops the export before anything is published (--allow-fail: diagnosis only)
n_fail <- sum(validation$status == "FAIL")
if (n_fail > 0) {
  dir.create(r_out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(validation, file.path(r_out_dir, "validation_report.csv"), row.names = FALSE)
  message("\nFailing baseline checks:")
  print(as.data.frame(validation[validation$status == "FAIL", c("check", "layer", "n", "max_abs_diff", "note")]), row.names = FALSE)
  if (!allow_fail) {
    stop(n_fail, " baseline check(s) FAIL; nothing was published. See ", file.path(r_out_dir, "validation_report.csv"),
         " (rerun with --allow-fail to publish anyway, for diagnosis only).", call. = FALSE)
  }
}
bundle$manifest$validation <- validation
bundle$manifest$notes <- unique(notes)
bundle$manifest$sources_params <- sources_params
src <- unique(sources_used)
bundle$manifest$sources <- tibble::tibble(
  path = vapply(src, rel, character(1), USE.NAMES = FALSE),
  mtime = file.mtime(src),
  size = file.size(src),
  md5 = unname(tools::md5sum(src))
)
bundle$manifest$baseline <- dash_params(bundle = bundle)
if (!identical(unclass(bundle$manifest$baseline), unclass(params))) stop("The baseline parameters changed after the validation run.")
created <- Sys.time()
bundle$manifest$created <- created
dash_validate_bundle(bundle)

# Web chunks: encode, gzip, name by md5, decode again and compare
step("Web chunks -> ", out_dir)
chunks <- c(dash_web_chunks(bundle, res), ties_chunks)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
chunk_rows <- list()
for (nm in names(chunks)) {
  bytes <- dash_osb_encode(chunks[[nm]], nm, "chunk")
  tmp <- file.path(out_dir, paste0(nm, ".tmp.gz"))
  con <- gzfile(tmp, "wb", compression = 9)
  writeBin(bytes, con)
  close(con)
  md5 <- unname(tools::md5sum(tmp))
  file_name <- dash_web_chunk_file(nm, md5)
  dest <- file.path(out_dir, file_name)
  if (file.exists(dest)) file.remove(dest)
  if (!file.rename(tmp, dest)) stop("Could not write ", dest)
  con <- gzfile(dest, "rb")
  back <- readBin(con, "raw", n = length(bytes) + 1L)
  close(con)
  if (!identical(back, bytes)) stop("Chunk ", nm, ": the gzip file does not read back the encoded bytes.")
  if (!identical(dash_osb_decode(back)$objects, dash_osb_normalize(chunks[[nm]]))) stop("Chunk ", nm, " does not round-trip identical().")
  chunk_rows[[nm]] <- tibble::tibble(name = nm, file = file_name, bytes = length(bytes), bytes_gz = file.size(dest), md5 = md5, initial = nm == "core")
  step("  ", file_name, ": ", sprintf("%.2f MB, %.2f MB gz", length(bytes) / 2^20, file.size(dest) / 2^20), "; round trip identical")
  rm(bytes, back)
  invisible(gc())
}
chunk_info <- dplyr::bind_rows(chunk_rows)
id_file <- tempfile(fileext = ".txt")
# the bundle id names the engine's data (core, vars, partner); the auxiliary chunks (ties, pcell-*)
# are listed in the manifest but do not change it
writeBin(charToRaw(paste0(paste(sort(chunk_info$file[chunk_info$name %in% c("core", "vars", "partner")]), collapse = "\n"), "\n")), id_file)
bundle_id <- unname(tools::md5sum(id_file))
unlink(id_file)

# The paper: title from paper/main.tex; the preview PDF only with --paper
tex_title <- function(path) {
  x <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  i <- regexpr("\\\\title\\{", x)
  if (i < 0) return(NA_character_)
  chars <- strsplit(substring(x, i + attr(i, "match.length")), "")[[1]]
  depth <- 1
  j <- 0
  for (j in seq_along(chars)) {
    if (chars[j] == "{") depth <- depth + 1
    if (chars[j] == "}") depth <- depth - 1
    if (depth == 0) break
  }
  t <- paste(chars[seq_len(j - 1)], collapse = "")
  t <- gsub("\\\\thanks\\{[^}]*\\}", "", t)
  t <- gsub("\\\\\\\\", " ", t)
  t <- gsub("~", " ", t)
  t <- gsub("\\\\[A-Za-z]+\\*?\\{([^}]*)\\}", "\\1", t)
  t <- gsub("[{}]", "", t)
  trimws(gsub("\\s+", " ", t))
}
paper <- list(title = NA_character_, url = NA_character_, pdf = NA_character_)
tex <- file.path(repo_root, "paper", "main.tex")
if (file.exists(tex)) paper$title <- tex_title(tex)
paper_pdf <- list.files(file.path(repo_root, "paper"), pattern = "PREVIEW\\.pdf$", full.names = TRUE)
if (copy_paper && length(paper_pdf)) {
  dir.create(paper_out_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(paper_pdf[1], file.path(paper_out_dir, "paper.pdf"), overwrite = TRUE)
  paper$pdf <- "paper/paper.pdf"
} else if (file.exists(file.path(paper_out_dir, "paper.pdf"))) {
  warning(file.path(paper_out_dir, "paper.pdf"), " is left over from an earlier export and would be deployed; delete it or rerun with --paper.", call. = FALSE)
}

# manifest.json (budget figures include its own gzip size) and the budget
man0 <- dash_web_manifest(bundle, chunk_info, NULL, bundle_id, created, paper)
manifest_gz <- length(memCompress(charToRaw(enc2utf8(dash_json_text(man0, pretty = TRUE))), "gzip")) + 18
budget <- dash_web_budget(chunk_info, manifest_gz)
web_manifest <- dash_web_manifest(bundle, chunk_info, budget, bundle_id, created, paper)
over <- c(initial = budget$initial_gz > budget$limit_initial_gz, all = budget$all_gz > budget$limit_all_gz)
if (any(over)) {
  stop(sprintf("Over budget: initial %.2f MB gz (limit %.0f), all chunks %.2f MB gz (limit %.0f). manifest.json was not updated.",
               budget$initial_gz / 2^20, budget$limit_initial_gz / 2^20, budget$all_gz / 2^20, budget$limit_all_gz / 2^20))
}
writeBin(charToRaw(enc2utf8(dash_json_text(web_manifest, pretty = TRUE))), file.path(out_dir, "manifest.json"))
stale <- setdiff(list.files(out_dir, pattern = "^(core|vars|partner|network|ties|pcell-[a-z0-9-]+)\\.[0-9a-f]{8}\\.osb$"), chunk_info$file)
if (length(stale)) {
  file.remove(file.path(out_dir, stale))
  step("Removed ", length(stale), " stale chunk file(s)")
}

# R artefacts (not deployed)
dir.create(r_out_dir, recursive = TRUE, showWarnings = FALSE)
bundle$manifest$bundle_id <- bundle_id
bundle$manifest$web <- list(out_dir = rel(out_dir), chunks = chunk_info, budget = budget[c("initial_gz", "all_gz", "limit_initial_gz", "limit_all_gz")])
step("Writing ", rel(r_out_dir), "/bundle.rds")
saveRDS(bundle, file.path(r_out_dir, "bundle.rds"), compress = "gzip")
validation_store <- list(
  es_index = stored$es_index[stored$es_index$tech %in% techs, , drop = FALSE],
  eo_index = stored$eo_index[stored$eo_index$tech %in% techs, , drop = FALSE],
  es_cc = stored$es_cc,
  eo_cc = stored$eo_cc,
  policy_index = stored$policy_index,
  policy_eligibility = stored$policy_eligibility,
  strategic_index = stored$strategic_index,
  allied_strategic = allied_csv,
  fs_index = stored_fs_index,
  po_index = stored_po_index,
  fs_top5 = fs_top5,
  po_top5 = po_top5,
  psi_tbl = psi_tbl_stored,
  psi_dyad_tbl = psi_dyad_stored,
  psi_reporters = psi_view_pick
)
saveRDS(validation_store, file.path(r_out_dir, "validation.rds"), compress = "gzip")
utils::write.csv(validation, file.path(r_out_dir, "validation_report.csv"), row.names = FALSE)

message("\nValidation (engine at the baseline vs the stored outputs):")
print(as.data.frame(validation[c("check", "layer", "status", "n", "max_abs_diff")]), row.names = FALSE)
if (any(validation$status == "FAIL")) {
  warning("Some baseline checks FAIL (", sum(validation$status == "FAIL"), "); see ", file.path(r_out_dir, "validation_report.csv"), call. = FALSE)
}
message("\nWeb bundle ", substr(bundle_id, 1, 8), " in ", rel(out_dir), ":") # an 8-character prefix: never print a full 32-hex id
print(data.frame(file = chunk_info$file, MB = round(chunk_info$bytes / 2^20, 2), MB_gz = round(chunk_info$bytes_gz / 2^20, 3),
                 initial = chunk_info$initial), row.names = FALSE)
message(sprintf("Initial load (manifest + core) %.2f MB gz of %.0f; all chunks %.2f MB gz of %.0f. %.0f s.",
                budget$initial_gz / 2^20, budget$limit_initial_gz / 2^20, budget$all_gz / 2^20, budget$limit_all_gz / 2^20,
                as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
