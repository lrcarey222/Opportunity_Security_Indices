# Export the data of the Allied Industrial Base Explorer (dashboard/, a static React app).
#
#   Rscript scripts/85_export_dashboard_data.R [--out-dir <dir>] [--r-out <dir>] [--network-dir <dir>]
#                                              [--skip-network-baseline] [--skip-trade] [--paper]
#                                              [--allow-fail]
#
# The ONE place the dashboard reads the pipeline's outputs. It reads data/processed (and the few
# raw files the partner and network layers need; the reserves and production themes for the
# endowment screen of the allied picks), builds the R bundle with the pure builders of
# R/dashboard/, runs the R reference engine at the paper's baseline (every dial at 50) and checks it
# against the stored outputs, solves the greedy baseline network, then writes
#
#   <out-dir>/manifest.json          schema, vintage, flags, notes, the validation report, sources,
#                                    the baseline inputs and parameters, the dial grid, the chunk
#                                    list and the budget (served with no-cache)
#   <out-dir>/<chunk>.<md5_8>.osb    core (initial), vars, partner, network: gzipped OSB1 containers
#                                    (R/dashboard/dash_osb.R, dash_web.R), lossless, immutable names
#   <r-out>/bundle.rds               the R bundle (input of scripts/86_export_dashboard_fixtures.R)
#   <r-out>/validation.rds           the stored pipeline tables the baseline checks compare with
#   <r-out>/validation_report.csv    engine at baseline vs the stored outputs, per check
#
# <out-dir> defaults to <repo>/dashboard/public/data, <r-out> to <processed_dir>/dashboard (both
# relative to the repository, never hard-coded). Every baseline parameter is read from the stored
# outputs or the files that produced them (config/weights.yml, config/rankability.yml, the Policy
# index's own record, the Strategic Index constants, scripts/30's settings), so after a pipeline
# rerun a re-export refreshes the dashboard; nothing here fixes a vintage.
#
# The stored scripts/30 design (allied_network_*.csv in --network-dir, default the outputs dir) is
# the network baseline (exact, MILP) only when it was solved on the same nodes, stages, lambda and
# inputs as the bundle (flags$network_consistent); otherwise the greedy baseline is shown.
# --skip-network-baseline skips the greedy baseline (the network chunk then has no baseline design);
# --skip-trade skips the Comtrade dyad file (the EU node's edges fall back to stage weights and the
# observed concentration is left out); --paper copies the paper's preview PDF to
# dashboard/public/paper/ (off by default: publishing the preview is the authors' decision). Each
# chunk is decoded again and must be identical() to what was encoded; the export stops above the
# budget (initial <= 3 MB gz, all chunks <= 25 MB gz).
#
# Every baseline check is PASS, INFO or FAIL. A FAIL stops the export before anything is published
# (validation_report.csv is still written to <r-out>); --allow-fail publishes anyway, for diagnosis
# only, with a warning.
#
# Friendshore's CM-FDI parts (commit 78be8b5; R/themes/partnership_strength/cm_fdi.R): the cells
# whose outbound rows name CM-FDI as their source (Solar, Wind, Batteries and Electric Vehicles
# Midstream) carry the CM-FDI investment tie in place of the IMF position, and the
# chinese_parent_exposure rows the ownership component; both reach the bundle as
# partner$fs$outbound_source / outbound_cmfdi / cpe (dash_bundle_fs_cmfdi()) and are checked against
# config/partnership.yml cm_fdi, the stored exposure table and, through the Friendshore index array,
# bit for bit against the pipeline.

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
skip_network_baseline <- arg_flag("--skip-network-baseline")
skip_trade <- arg_flag("--skip-trade")
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
network_dir <- abs_path(arg_value("--network-dir", outputs_dir))
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
  strategic_index = io$strategic_index
)
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
# tables as scripts/20: one row per Country x screened Upstream stage, resource_base TRUE where the
# technology-level reserves or production are positive. The engine flags the SI cells with it
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
fs_dup <- sum(duplicated(fs_long[c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable")]))
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
po_dup <- sum(duplicated(po_long[c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable")]))
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

# ---- Network --------------------------------------------------------------------------------------

step("Network inputs")
network_members <- if (!is.null(coalition_tbl)) allied_network_add_node_members(coalition, coalition_tbl) else dplyr::mutate(coalition, node_only = FALSE)
s30_path <- file.path(repo_root, "scripts", "30_build_allied_network_design.R")
s30 <- readLines(s30_path, warn = FALSE)
s30_literal <- function(name, default) {
  hit <- grep(paste0("^\\s*", name, "\\s*(<-|=)\\s*"), s30, value = TRUE)
  if (!length(hit)) return(default)
  rhs <- trimws(sub("#.*$", "", sub(paste0("^\\s*", name, "\\s*(<-|=)\\s*"), "", hit[1])))
  rhs <- sub(",\\s*$", "", rhs)
  if (!grepl("^(-?[0-9.eE]+L?|TRUE|FALSE|\"[^\"]*\"|c\\((\\s*\"[^\"]*\"\\s*,?)+\\)|list\\(([a-z_]+ = [0-9.]+,? ?)+\\))$", rhs)) return(default)
  eval(parse(text = rhs), envir = baseenv())
}
if (isTRUE(s30_literal("network_node_all_members", TRUE)) == FALSE) network_members <- dplyr::mutate(coalition, node_only = FALSE)
iso3c_network <- network_members$iso3
members_net <- tibble::tibble(
  iso3c = network_members$iso3,
  country = dplyr::coalesce(network_members$country, country_info$country[match(network_members$iso3, country_info$iso3c)]),
  tier = network_members$tier,
  network_node = network_members$network_node,
  node_tier = if ("node_tier" %in% names(network_members)) network_members$node_tier else network_members$tier,
  node_only = network_members$node_only %in% TRUE
)
# scripts/30's current settings come first; the stored diagnostics only tell whether the stored
# design was solved with them (a stale design must not set the baseline)
diag_path <- file.path(network_dir, "allied_network_diagnostics.csv")
stored_diag <- maybe_src(diag_path, read.csv, stringsAsFactors = FALSE)
lambda_stored <- if (!is.null(stored_diag) && "diversity_lambda" %in% names(stored_diag)) unique(stored_diag$diversity_lambda) else NULL
node_policy_stored <- if (!is.null(stored_diag) && "node_policy" %in% names(stored_diag)) unique(as.logical(stored_diag$node_policy)) else NULL
dfl <- formals(allied_network_design)
lambda_s30 <- s30_literal("network_diversity_lambda", NULL)
net_lambda <- if (!is.null(lambda_s30)) lambda_s30 else if (length(lambda_stored) == 1 && is.finite(lambda_stored)) lambda_stored else 1.5
sources_params$lambda <- if (!is.null(lambda_s30)) "scripts/30 network_diversity_lambda" else if (length(lambda_stored) == 1) "allied_network_diagnostics.csv diversity_lambda" else "default 1.5"
node_policy_s30 <- s30_literal("network_node_policy", NULL)
net_node_policy <- if (!is.null(node_policy_s30)) node_policy_s30 else if (length(node_policy_stored) == 1 && !is.na(node_policy_stored)) node_policy_stored else FALSE
supply_chains <- s30_literal("network_supply_chains", c("Upstream", "Midstream"))
baseline_network <- list(
  node_weights = eval(dfl$node_weights),
  node_policy = isTRUE(net_node_policy),
  node_policy_weight_max = eval(dfl$node_weights)$policy,
  edge_weights = eval(dfl$edge_weights),
  demand_weights = s30_literal("demand_weights", list(need = 0.5, size = 0.5)),
  lambda = as.numeric(net_lambda),
  max_share = s30_literal("max_share", 0.40),
  min_share = s30_literal("min_share", 0.05),
  min_producers = as.integer(s30_literal("min_producers", 3L)),
  min_suppliers_per_consumer = as.integer(s30_literal("min_suppliers_per_consumer", 2L)),
  epsilon_supplier_share = s30_literal("epsilon_supplier_share", 0.10),
  w_node = s30_literal("w_node", 1),
  w_edge = s30_literal("w_edge", 0.5),
  w_dev = s30_literal("w_dev", 0),
  allow_self = isTRUE(s30_literal("allow_self", TRUE)),
  portfolio = list(enable = isTRUE(s30_literal("portfolio_enable", TRUE)), top_k = as.integer(s30_literal("portfolio_top_k", 5L)),
                   min_cap = s30_literal("portfolio_min_cap", 2), max_cap = s30_literal("portfolio_max_cap", 10),
                   transform = s30_literal("portfolio_transform", "log"),
                   max_iters = as.integer(s30_literal("network_portfolio_max_iters", 30L))),
  diversity_breakpoint_step = s30_literal("network_diversity_breakpoint_step", 0.01),
  supply_chains = supply_chains,
  milp_time_limit = s30_literal("network_milp_time_limit_sec", 300),
  policy_missing = s30_literal("policy_missing", "coalition_median"),
  missing_components = s30_literal("missing_components", "renormalize"),
  demand_mode = s30_literal("demand_mode", "mixed"),
  size_transform = "log_scurve"
)
sources_params$network <- "scripts/30_build_allied_network_design.R literals and allied_network_design() defaults; lambda / node_policy from the stored diagnostics when they record them"

gdp_raw <- read_src(file.path(raw_dir, "wdi_gdp.csv"), read.csv, stringsAsFactors = FALSE)
country_size <- allied_network_country_size_tbl(gdp_raw, country_info, iso3c_network)
dev_tbl <- maybe_src(file.path(processed_dir, "partner_development_country_tbl.rds"))
node_raw_member <- allied_network_node_inputs(
  economic_opportunity_index = stored$eo_index,
  energy_security_index = stored$es_index,
  policy_index = stored$policy_index,
  country_info = country_info,
  iso3c_network = iso3c_network,
  partner_development_country_tbl = dev_tbl,
  country_size_tbl = country_size,
  node_policy = TRUE
)
attr(node_raw_member, "input_checks") <- NULL
rm(dev_tbl)
node_member_weights <- if (any(members_net$network_node != members_net$iso3c)) read_src(file.path(processed_dir, "eu_stage_weights_tbl.rds")) else NULL
# Country spellings -> network ISO3 (allied_network_collapse_country_index()'s mapping), for the
# producer score's policy term
spellings <- unique(c(stored$policy_index$Country, stored$es_index$Country, stored$eo_index$Country))
canon <- dplyr::distinct(dplyr::transmute(country_info, iso3c = as.character(.data$iso3c), canonical = as.character(.data$country)), .data$iso3c, .keep_all = TRUE)
cm_iso <- partnership_strength_country_to_iso(partnership_strength_standardize_countries(spellings), country_info)
country_map <- tibble::tibble(Country = spellings, iso3c = cm_iso, is_canonical = dplyr::coalesce(spellings == canon$canonical[match(cm_iso, canon$iso3c)], FALSE))

trade_cells <- NULL
node_trade <- NULL
observed_hhi <- NULL
if (!skip_trade) {
  trade_path <- file.path(raw_dir, "comtrade_energy_dyads_all.csv")
  if (!file.exists(trade_path)) trade_path <- file.path(raw_dir, "allied_comtrade_energy_data.csv")
  hs6_path <- file.path(raw_dir, "energy_hs6_master.csv")
  if (file.exists(trade_path) && file.exists(hs6_path) && requireNamespace("data.table", quietly = TRUE)) {
    step("Basket trade (", basename(trade_path), ")")
    trade_cols <- c("reporter_iso", "partner_iso", "period", "ref_year", "flow_direction", "flow_code", "cmd_code", "primary_value")
    hdr <- names(data.table::fread(trade_path, nrows = 0))
    td <- as.data.frame(data.table::fread(
      trade_path, select = intersect(trade_cols, hdr),
      colClasses = list(character = intersect(c("reporter_iso", "partner_iso", "flow_direction", "cmd_code"), hdr)),
      na.strings = c("", "NA"), showProgress = FALSE
    ))
    sources_used <- unique(c(sources_used, trade_path, hs6_path))
    ycol <- intersect(c("period", "ref_year"), names(td))[1]
    yrs <- suppressWarnings(as.integer(td[[ycol]]))
    trade_year <- max(yrs, na.rm = TRUE)
    td <- td[!is.na(yrs) & yrs == trade_year, , drop = FALSE]
    pcfg_path <- file.path(repo_root, "config", "partnership.yml")
    pcfg <- if (file.exists(pcfg_path)) yaml::read_yaml(pcfg_path) else list()
    trade_cells <- partnership_strength_build_trade_cells(
      td, read.csv(hs6_path), years = trade_year,
      cif_fob_factor = if (is.null(pcfg$trade$cif_fob_factor)) PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR else as.numeric(pcfg$trade$cif_fob_factor)
    )
    rm(td)
    invisible(gc())
    node_trade <- allied_network_member_trade(trade_cells, iso3c_network)
    net_stages <- unique(node_raw_member[tolower(node_raw_member$supply_chain) %in% tolower(supply_chains), c("tech", "supply_chain")])
    mem_chk <- allied_network_check_members(members_net, members_net$iso3c)
    observed_hhi <- allied_network_observed_export_hhi(trade_cells, mem_chk, stages = net_stages)
    observed_hhi$year <- trade_year
    rm(trade_cells)
  } else {
    notes <- c(notes, "No Comtrade dyad file: the EU node's edges use stage weights and the observed concentration is left out.")
  }
}

read_csv_opt <- function(name) maybe_src(file.path(network_dir, name), read.csv, stringsAsFactors = FALSE)
network_stored <- list(
  specialization = read_csv_opt("allied_network_specialization.csv"),
  flows = read_csv_opt("allied_network_flows.csv"),
  diagnostics = stored_diag,
  caps = read_csv_opt("allied_network_portfolio_caps.csv"),
  counts = read_csv_opt("allied_network_portfolio_counts.csv"),
  topk = read_csv_opt("allied_network_topk_by_stage.csv"),
  member_shares = read_csv_opt("allied_network_member_shares.csv"),
  members = read_csv_opt("allied_network_members.csv")
)
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
    stage_weights = STRATEGIC_SUPPLY_CHAIN_WEIGHTS,
    excluded_sectors = STRATEGIC_EXCLUDED_SECTORS,
    allied_excluded_techs = eval(formals(select_allied_strategic_sectors)$excluded_techs),
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
  ),
  baseline_network
)
sources_params$tau <- if (pillar_rule) "config/rankability.yml" else "NA: the stored pillars predate the rankability rule"
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
    partner_pillar_consistent = FALSE,
    network_aggregation = any(members_net$network_node != members_net$iso3c),
    network_consistent = FALSE,
    milp_baseline = FALSE
  ),
  baseline_inputs = baseline_inputs,
  validation = NULL,
  notes = notes
)
si_parts <- list(tech_ghg = if (!is.null(tg)) tibble::as_tibble(tg)[c("tech", "ghg_index")] else tibble::tibble(tech = character(), ghg_index = numeric()),
                 trl = tibble::as_tibble(trl), allies = tibble::as_tibble(allies))
if (!is.null(resource_base_tbl)) si_parts$resource_base <- resource_base_tbl
network_bundle <- list(
  members = members_net,
  node_raw_member = tibble::as_tibble(node_raw_member),
  node_member_weights = node_member_weights,
  node_trade = node_trade,
  country_size = country_size,
  node_labels = node_labels,
  country_info = tibble::tibble(iso3c = countries_dim$iso3c, country = countries_dim$country),
  country_map = country_map,
  stored = network_stored,
  greedy_baseline = NULL,
  observed_hhi = observed_hhi
)
bundle <- dash_bundle_assemble(manifest, dims, pillars, policy_bundle, si_parts, partner_bundle, network_bundle)
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
# ---- Network ---------------------------------------------------------------------------------------

# Tables that no dial changes (member weights, node sizes, portfolio caps) are precomputed for every
# solvable stage, and the basket trade is trimmed to the rows the edge aggregation reads; both are
# checked against the engine's own computation at the baseline.
stage_tbl <- dash_network_stages(bundle, params)
fixed_net <- dash_network_fixed_tables(bundle)
trade_full <- bundle$network$node_trade
trade_trim <- dash_network_trim_trade(trade_full, dash_network_members(bundle), fixed_net$stages)
step("Network inputs at the baseline (", nrow(stage_tbl), " stages)")
inputs_net <- dash_network_inputs(bundle, params, res$partner$cells, policy = res$policy)
bundle_trim <- bundle
bundle_trim$network$node_trade <- trade_trim
inputs_trim <- dash_network_inputs(bundle_trim, params, res$partner$cells, policy = res$policy)
rm(bundle_trim)
add_check("Network inputs with the trimmed basket trade == full trade", "network",
          if (identical(inputs_trim$edges, inputs_net$edges) && identical(inputs_trim$raw_nodes, inputs_net$raw_nodes)) "PASS" else "FAIL",
          nrow(inputs_net$edges), NA, paste0("basket trade rows ", NROW(trade_full), " -> ", NROW(trade_trim)))
if (!is.null(inputs_net$member_weights)) {
  mw_k <- paste(fixed_net$member_weights$tech, fixed_net$member_weights$supply_chain, sep = "\r") %in%
    paste(stage_tbl$tech, stage_tbl$supply_chain, sep = "\r")
  add_check("Precomputed member weights == engine (baseline stages)", "network",
            if (identical(fixed_net$member_weights[mw_k, ], tibble::as_tibble(inputs_net$member_weights))) "PASS" else "FAIL", sum(mw_k))
  add_check("Precomputed node sizes == engine", "network",
            if (identical(fixed_net$node_size, tibble::as_tibble(inputs_net$node_size))) "PASS" else "FAIL", nrow(fixed_net$node_size))
}
bundle$network$member_weights <- fixed_net$member_weights
bundle$network$node_size <- fixed_net$node_size
bundle$network$caps <- fixed_net$caps
bundle$network$node_trade <- trade_trim
inputs_net <- inputs_trim
scored_net <- dash_network_score(inputs_net, params)

# The stored scripts/30 design is the network's baseline only when it was solved over the same
# nodes, stages and lambda and on the same inputs (producer scores, demand, edge weights).
net_consistent <- FALSE
if (!is.null(network_stored$diagnostics) && !is.null(network_stored$specialization)) {
  same_stages <- setequal(paste(network_stored$diagnostics$tech, network_stored$diagnostics$supply_chain), paste(stage_tbl$tech, stage_tbl$supply_chain))
  st_nodes <- unique(network_stored$specialization$iso3c)
  exp_nodes <- unique(members_net$network_node)
  same_nodes <- setequal(st_nodes, exp_nodes)
  same_lambda <- length(lambda_stored) == 1 && isTRUE(all.equal(lambda_stored, net_lambda))
  structural <- same_stages && same_nodes && same_lambda
  add_check("Stored network matches the current nodes, stages and lambda", "network", if (structural) "PASS" else "INFO",
            nrow(network_stored$diagnostics), NA,
            paste0(rel(network_dir), ": stages ", same_stages, ", nodes ", same_nodes, " (stored ", length(st_nodes), ", now ", length(exp_nodes),
                   "), lambda ", same_lambda, " (stored ", if (length(lambda_stored)) paste(lambda_stored, collapse = "/") else "not recorded", ", now ", net_lambda, ")"))
  inputs_ok <- FALSE
  if (structural) {
    sp_ <- network_stored$specialization
    sk <- paste(scored_net$nodes$tech, scored_net$nodes$supply_chain, scored_net$nodes$iso3c)
    k <- match(paste(sp_$tech, sp_$supply_chain, sp_$iso3c), sk)
    st <- vapply(c("producer_score", "demand_weight", "need_idx", "size_idx"), function(col) {
      if (!col %in% names(sp_)) return("PASS")
      cmp_num(paste0("Network node ", col, " == stored"), "network", scored_net$nodes[[col]][k], sp_[[col]], 1e-9, fail = "INFO")
    }, character(1))
    fl <- network_stored$flows
    ek <- paste(scored_net$edges$tech, scored_net$edges$supply_chain, scored_net$edges$reporter_iso, scored_net$edges$partner_iso)
    k2 <- match(paste(fl$tech, fl$supply_chain, fl$reporter_iso, fl$partner_iso), ek)
    ok2 <- !is.na(k2)
    st_e <- cmp_num("Network edge_weight == stored flows", "network", scored_net$edges$edge_weight[k2[ok2]], fl$edge_weight[ok2], 1e-9, fail = "INFO")
    unmatched <- sum(!ok2 & fl$reporter_iso != fl$partner_iso)
    add_check("Stored flows all on engine edges", "network", if (unmatched == 0) "PASS" else "INFO", nrow(fl), NA,
              paste0(unmatched, " stored flows between two nodes without an engine edge"))
    inputs_ok <- all(c(st, st_e) == "PASS") && unmatched == 0
  }
  net_consistent <- structural && inputs_ok
  if (net_consistent) {
    bundle$manifest$flags$milp_baseline <- all(network_stored$diagnostics$method == "milp")
  } else {
    notes <- c(notes, paste0("The stored allied-network design (", rel(network_dir), ") was not solved on the bundle's nodes, stages, lambda and inputs: ",
                             "the network's baseline is the engine's greedy design until scripts/30 is rerun on the current inputs."))
  }
} else {
  notes <- c(notes, "No stored allied-network design: the network's baseline is the engine's greedy design.")
}
bundle$manifest$flags$network_consistent <- net_consistent
sources_params$network_dir <- rel(network_dir)

if (!skip_network_baseline) {
  step("Greedy baseline network (", nrow(stage_tbl), " stages, share_rank_digits = ", DASH_WEB_SHARE_RANK_DIGITS, ")")
  t0 <- Sys.time()
  gb <- dash_network_solve(scored_net, params, bundle = bundle, solver = "greedy", share_rank_digits = DASH_WEB_SHARE_RANK_DIGITS)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  step("  solved in ", round(el, 1), " s; portfolio converged ", gb$portfolio$converged, " after ", gb$portfolio$iters, " iterations")
  gb$elapsed_sec <- el
  gb$share_rank_digits <- DASH_WEB_SHARE_RANK_DIGITS
  bundle$network$greedy_baseline <- gb
  add_check("Greedy baseline network solved", "network", if (isTRUE(gb$portfolio$converged) || is.null(gb$portfolio)) "PASS" else "INFO",
            nrow(gb$diagnostics), NA, paste0(round(el, 1), " s; ", sum(gb$specialization$production_share > 0, na.rm = TRUE), " producer slots"))
  if (!is.null(bundle$network$caps) && !is.null(gb$portfolio$caps)) {
    add_check("Precomputed portfolio caps == engine", "network",
              if (identical(bundle$network$caps$portfolio_cap, gb$portfolio$caps$portfolio_cap) &&
                  identical(bundle$network$caps$iso3c, gb$portfolio$caps$iso3c)) "PASS" else "FAIL", nrow(gb$portfolio$caps))
  }
  # The pipeline's own greedy rule (allied_network_design(method = "greedy")) ranks producers for the
  # caps on unrounded shares; report whether the dashboard's tie rule changes anything today.
  gr <- dash_network_solve(scored_net, params, bundle = bundle, solver = "greedy")
  cmp_gr <- dash_network_compare(gb, gr)
  add_check("Greedy baseline (share_rank_digits 9) == the pipeline's greedy rule", "network", if (!any(cmp_gr$stage$changed)) "PASS" else "INFO",
            nrow(cmp_gr$stage), suppressWarnings(max(cmp_gr$stage$max_abs_share_delta, na.rm = TRUE)),
            paste0(sum(cmp_gr$stage$changed), " stages differ (the pipeline rule breaks exact share ties by RAS noise)"))
  rm(gr)
  if (net_consistent) {
    cmp_st <- dash_network_compare(gb, network_stored)
    add_check("Greedy baseline vs the stored design (solver gap)", "network", "INFO", nrow(cmp_st$stage),
              suppressWarnings(max(cmp_st$stage$max_abs_share_delta, na.rm = TRUE)),
              paste0(sum(!cmp_st$stage$changed), " of ", nrow(cmp_st$stage), " stages with the same producers and shares; median HHI greedy ",
                     signif(stats::median(cmp_st$stage$hhi, na.rm = TRUE), 4), ", stored ", signif(stats::median(cmp_st$stage$hhi_ref, na.rm = TRUE), 4)))
  }
}

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
chunks <- dash_web_chunks(bundle, res)
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
writeBin(charToRaw(paste0(paste(sort(chunk_info$file), collapse = "\n"), "\n")), id_file)
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
stale <- setdiff(list.files(out_dir, pattern = "^(core|vars|partner|network)\\.[0-9a-f]{8}\\.osb$"), chunk_info$file)
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
message(sprintf("Initial load (manifest + core) %.2f MB gz of %.0f; all chunks %.2f MB gz of %.0f. Network baseline: %s. %.0f s.",
                budget$initial_gz / 2^20, budget$limit_initial_gz / 2^20, budget$all_gz / 2^20, budget$limit_all_gz / 2^20,
                if (net_consistent) "stored design" else if (!skip_network_baseline) "greedy (stored design not consistent)" else "none",
                as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
