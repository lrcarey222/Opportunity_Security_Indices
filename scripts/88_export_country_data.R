# Export the data of Country Insights, the third module of the Allied Industrial Base Explorer
# (dashboard/; spec: scratch_dash/country/spec_country.md, parts A and B).
#
#   Rscript scripts/88_export_country_data.R [--out-dir <dir>] [--iso3 JPN,DEU] [--check-only]
#                                            [--synthetic] [--no-validate-bundles] [--facts-dir <dir>]
#   Rscript scripts/88_export_country_data.R --render-memos [--iso3 USA,JPN] [--lint-only]
#                                            [--memo-dir <dir>] [--edge <msedge.exe>] [--python <python.exe>]
#                                            [--force-stale]
#
# IO only: every rule lives in the pure helpers of R/dashboard/country/ (country_helper_files()). The
# export reads data/processed (index_outputs.rds, psi_dyad_tbl.rds, psi_tbl.rds, coalition_tbl.rds,
# geo_*.rds, cm_fdi_projects_tbl.rds, policy_outputs.rds, the partner Friendshore / Opportunity cell
# and top-five tables, the theme tables named in config/country_insights.yml raw_tables,
# charts/allied_strategic.csv, nipo_policy_all.rds),
# data/reference/economic_security_agreements.csv, data/raw/wdi_country_info.csv, and the published
# Strategic and Sector bundles (dashboard/public/data/manifest.json and chunks;
# dashboard/public/data/sector/...): the Strategic core chunk names every economy (Country -> ISO3)
# and is the cross-check of every index value; the Sector chunks give the reconciled trade matrix, the
# Climate Action Tracker table and the CM-FDI and facts cross-checks. It writes
#
#   <out-dir>/manifest.json                 schema "osi-country" (no-cache; written last)
#   <out-dir>/facts-<ISO3>.<h8>.json        one fact sheet per economy with any score or partner row
#   <processed_dir>/dashboard/country/country_validation.csv
#   <facts-dir>/<ISO3>.json                 (optional) pretty copies of the core members' sheets for
#                                           the memo writers
#
# Six stages (2026-10-28): the sheets are keyed on the 66 sectors, 11 technologies x the six supply-chain
# stages (country_cells_tbl()). data/processed is six-stage since 2026-10-27, and the published Strategic
# and Sector bundles must be the six-stage exports of the same data/processed (run scripts/85 and scripts/87
# first): the cross-checks below compare every index value, the allied picks, the trade matrix and the
# Sector facts with them, and any FAIL writes nothing. The monthly basket takes the HS6 master's six-stage
# `stage`; a CM-FDI project counts in each stage of its `stages` list (Components and Assembly), totals once
# per project; the NIPO run is still three-stage, so the sheets list no measures by sector.
#
# Every fact sheet is read back and must pass country_check_sheet() and equal data/processed on every
# shared value (ES, EO, SI, PSI) after the JSON round trip. --check-only builds and validates without
# writing. --synthetic writes the deterministic fixture (country_synthetic_ctx(), no data read) to
# dashboard/src/country/__fixtures__/synthetic/data/country/, with a placeholder memo for the United
# States. Memos: see country_memo*.R and the --render-memos section at the end.

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

t_script <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

source(file.path(repo_root, "R", "dashboard", "country", "country_files.R"))
for (f in country_helper_files()) source(file.path(repo_root, f))

abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
processed_dir <- file.path(repo_root, config$processed_dir)
raw_dir <- file.path(repo_root, config$raw_data_dir)
cfg_path <- file.path(repo_root, "config", "country_insights.yml")
cfg <- yaml::read_yaml(cfg_path)
dict <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))

val_rows <- list()
add_check <- function(check, status, n = NA_integer_, max_abs_diff = NA_real_, note = NA_character_) {
  val_rows[[length(val_rows) + 1]] <<- data.frame(check = check, status = status, n = as.integer(n), max_abs_diff = as.numeric(max_abs_diff),
                                                  note = as.character(note), stringsAsFactors = FALSE)
  message(sprintf("  %-4s %s%s", status, check, if (!is.na(note)) paste0(" (", note, ")") else ""))
}

render_memos <- arg_flag("--render-memos")
if (render_memos) {
  source(file.path(repo_root, "scripts", "utils", "country_memos.R"))
  quit(status = country_render_memos_main(args, repo_root, cfg, dict))
}

synthetic <- arg_flag("--synthetic")
check_only <- arg_flag("--check-only")
validate_bundles <- !arg_flag("--no-validate-bundles")
only_iso <- arg_value("--iso3")
only_iso <- if (is.null(only_iso)) NULL else toupper(trimws(strsplit(only_iso, ",", fixed = TRUE)[[1]]))
facts_dir <- arg_value("--facts-dir")
out_dir <- abs_path(arg_value("--out-dir", if (synthetic) "dashboard/src/country/__fixtures__/synthetic/data/country" else "dashboard/public/data/country"))
created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
git_commit <- tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_)

read_osb <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  b <- readBin(con, "raw", n = file.size(path) * 40 + 1e6)
  dash_osb_decode(b)$objects
}
rds <- function(name) {
  p <- file.path(processed_dir, name)
  if (!file.exists(p)) stop("Missing input: ", p)
  readRDS(p)
}

# =====================================================================================================
# Inputs -> ctx
# =====================================================================================================

load_ctx <- arg_value("--load-ctx")
if (!is.null(load_ctx)) {
  # debugging: reuse the inputs of an earlier run (--save-ctx); bundle and input checks are not repeated
  step("Loading inputs from ", load_ctx)
  saved <- readRDS(load_ctx)
  ctx <- saved$ctx
  # the current config and dictionary, baseline date and build time (the saved inputs stay as they were)
  ctx$cfg <- cfg
  ctx$dict <- dict
  s_in_l <- max(file.mtime(file.path(processed_dir, c("outputs/index_outputs.rds", "psi_dyad_tbl.rds", "coalition_tbl.rds"))))
  ctx$meta$as_of <- format(as.Date(format(s_in_l, "%Y-%m-%d")))
  ctx$meta$as_of_label <- country_date_label(as.Date(ctx$meta$as_of))
  ctx$meta$created <- created
  ctx$meta$label <- country_fill(cfg$label, list(as_of_label = ctx$meta$as_of_label))
  # the Sector bundle's filled source texts (small chunk)
  xmf_l <- jsonlite::fromJSON(file.path(repo_root, "dashboard", "public", "data", "sector", "manifest.json"), simplifyVector = FALSE)
  for (ch in xmf_l$chunks) if (identical(ch$name, "sector-meta")) ctx$sector_sources <- as.data.frame(read_osb(file.path(repo_root, "dashboard", "public", "data", "sector", ch$file))$dict$sources)
  meta <- ctx$meta
  for (nm in setdiff(names(saved), "ctx")) assign(nm, saved[[nm]])
  dup_rows <- list(); picks_seen <- list()
  ctx$dup_fn <- function(e, n) dup_rows[[length(dup_rows) + 1]] <<- paste(e$pillar, e$category, e$variable, n)
  ctx$pick_fn <- function(iso3, pick) picks_seen[[iso3]] <<- pick
} else if (synthetic) {
  step("Synthetic inputs (country_synthetic_ctx)")
  ctx <- country_synthetic_ctx(cfg, dict)
  meta <- ctx$meta
  meta$created <- "2026-10-12T00:00:00Z"
  meta$git_commit <- NULL
  add_check("synthetic: inputs built without reading data", "PASS", nrow(ctx$roster))
} else {
  # ---- published bundles --------------------------------------------------------------------------
  data_dir <- file.path(repo_root, "dashboard", "public", "data")
  sm_path <- file.path(data_dir, "manifest.json")
  xm_path <- file.path(data_dir, "sector", "manifest.json")
  if (!file.exists(sm_path) || !file.exists(xm_path)) {
    if (validate_bundles) stop("The Strategic or Sector manifest is missing: run scripts/85 and scripts/87 first (or --no-validate-bundles).")
  }
  smf <- jsonlite::fromJSON(sm_path, simplifyVector = FALSE)
  xmf <- jsonlite::fromJSON(xm_path, simplifyVector = FALSE)
  chunk <- function(m, dir, name) {
    for (c in m$chunks) if (identical(c$name, name)) return(read_osb(file.path(dir, c$file)))
    stop("No chunk ", name)
  }
  step("Strategic core chunk ", smf$bundle_id)
  core <- chunk(smf, data_dir, "core")
  dims <- core$dims
  bcore <- core$baseline$core
  psi_partner <- core$baseline$psi_partner
  sdir <- file.path(data_dir, "sector")
  step("Sector meta, policy, fdi, facts chunks ", xmf$bundle_id)
  smeta <- chunk(xmf, sdir, "sector-meta")
  spol <- chunk(xmf, sdir, "sector-policy")$policy
  sfdi <- tryCatch(chunk(xmf, sdir, "sector-fdi")$fdi, error = function(e) NULL)
  sfacts <- chunk(xmf, sdir, "sector-facts")$facts
  geo <- smeta$dims$geo
  items <- smeta$dims$items
  vars_dict <- smeta$dict$vars
  geo_code <- function(i) geo$code[i + 1L]

  # freshness (A7.3 #8)
  s_created <- as.POSIXct(smf$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  x_created <- as.POSIXct(xmf$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  newest <- function(fs) max(file.mtime(fs[file.exists(fs)]))
  s_in <- newest(file.path(processed_dir, c("outputs/index_outputs.rds", "psi_dyad_tbl.rds", "coalition_tbl.rds")))
  x_in <- newest(file.path(processed_dir, c("import_dependence_tbl.rds", "trade_concentration_tbl.rds", "cm_fdi_projects_tbl.rds")))
  fresh_s <- s_in <= s_created
  fresh_x <- x_in <= x_created
  add_check("bundles: Strategic export newer than its inputs", if (fresh_s) "PASS" else if (validate_bundles) "FAIL" else "WARN", 1, NA,
            sprintf("inputs %s, export %s", format(s_in, tz = "UTC"), smf$created))
  add_check("bundles: Sector export newer than its inputs", if (fresh_x) "PASS" else if (validate_bundles) "FAIL" else "WARN", 1, NA,
            sprintf("inputs %s, export %s", format(x_in, tz = "UTC"), xmf$created))

  # ---- names -> ISO3 (the Strategic export's mapping) ---------------------------------------------
  nm <- unique(rbind(bcore$es[, c("Country", "iso3c")], bcore$eo[, c("Country", "iso3c")], bcore$si$cells[, c("Country", "iso3c")]))
  if (anyDuplicated(nm$Country)) stop("Strategic core chunk maps one name to two ISO3 codes")
  name_iso <- stats::setNames(nm$iso3c, nm$Country)
  coal <- as.data.frame(rds("coalition_tbl.rds"))
  extra_names <- stats::setNames(coal$iso3, coal$country)
  iso_of <- function(x) { out <- unname(name_iso[x]); miss <- is.na(out); out[miss] <- unname(extra_names[x[miss]]); out }

  # ---- roster -----------------------------------------------------------------------------------------
  step("Roster")
  ip <- as.data.frame(rds("geo_ideal_points_tbl.rds"))
  wdi <- read.csv(file.path(raw_dir, "wdi_country_info.csv"), stringsAsFactors = FALSE)
  dc <- as.data.frame(dims$countries)
  isos <- unique(c(coal$iso3, dc$iso3c))
  pick_col <- function(iso, d, key, col) d[[col]][match(iso, d[[key]])]
  roster <- data.frame(
    iso3 = isos,
    name = ifelse(is.na(pick_col(isos, dc, "iso3c", "country")), pick_col(isos, coal, "iso3", "country"), pick_col(isos, dc, "iso3c", "country")),
    tier = ifelse(is.na(pick_col(isos, coal, "iso3", "tier")), "none", pick_col(isos, coal, "iso3", "tier")),
    routes = pick_col(isos, coal, "iso3", "routes"),
    region = ifelse(is.na(pick_col(isos, dc, "iso3c", "region")), pick_col(isos, wdi, "iso3c", "region"), pick_col(isos, dc, "iso3c", "region")),
    income_group = ifelse(is.na(pick_col(isos, dc, "iso3c", "income_group")), pick_col(isos, wdi, "iso3c", "income"), pick_col(isos, dc, "iso3c", "income_group")),
    sovereign = pick_col(isos, dc, "iso3c", "sovereign") %in% TRUE | (is.na(pick_col(isos, dc, "iso3c", "sovereign")) & pick_col(isos, ip, "iso3", "alignment_basis") %in% "own_votes"),
    eu_member = pick_col(isos, coal, "iso3", "eu_member") %in% TRUE,
    network_node = pick_col(isos, coal, "iso3", "network_node"),
    bloc = pick_col(isos, ip, "iso3", "bloc"),
    s_axis = pick_col(isos, ip, "iso3", "s_axis"),
    alignment_basis = pick_col(isos, ip, "iso3", "alignment_basis"),
    alignment_from = pick_col(isos, ip, "iso3", "imputed_from"),
    iso_n3 = { n3 <- suppressWarnings(as.integer(pick_col(isos, as.data.frame(geo[c("code", "iso_n3")]), "code", "iso_n3"))); ifelse(is.na(n3), NA_character_, sprintf("%03d", n3)) },
    stringsAsFactors = FALSE
  )
  roster$routes[roster$routes %in% "none"] <- NA
  roster$network_node[!roster$tier %in% c("core", "partner")] <- NA
  roster <- roster[!is.na(roster$iso3) & roster$iso3 != "EUU", , drop = FALSE]
  roster <- roster[order(roster$name), , drop = FALSE]
  party_fn <- country_party_fn(roster, stats::setNames(geo$name, geo$code))

  # ---- indices (index_outputs.rds) ------------------------------------------------------------------
  step("index_outputs.rds")
  io <- rds("outputs/index_outputs.rds")
  cells_tbl <- country_cells_tbl()
  pillar_tbl <- function(x, col) {
    x <- as.data.frame(x)
    data.frame(iso3 = iso_of(x$Country), key = country_cell_key(x$tech, x$supply_chain), value = x[[col]], imputed_share = x$imputed_weight_share,
               rankable = x$rankable, reason = x$rank_exclusion, stringsAsFactors = FALSE)
  }
  es <- pillar_tbl(io$energy_security_index, "Energy_Security_Index")
  eo <- pillar_tbl(io$economic_opportunity_index, "Economic_Opportunity_Index")
  if (anyNA(es$iso3) || anyNA(eo$iso3)) stop("Economies without an ISO3 code in index_outputs: ", paste(unique(c(io$energy_security_index$Country[is.na(es$iso3)], io$economic_opportunity_index$Country[is.na(eo$iso3)])), collapse = ", "))
  # the inputs are at the six stages (a three-stage data/processed would match no sector)
  stage_vals <- unique(c(as.character(io$energy_security_index$supply_chain), as.character(io$economic_opportunity_index$supply_chain),
                         as.character(io$strategic_index$supply_chain)))
  add_check("inputs: Energy Security, Economic Opportunity and Strategic Index rows are at the six supply-chain stages",
            if (all(stage_vals %in% COUNTRY_STAGES)) "PASS" else "FAIL", length(stage_vals), NA,
            paste("stages:", paste(sort(stage_vals), collapse = ", ")))
  si_raw <- as.data.frame(io$strategic_index)
  si_all <- data.frame(iso3 = iso_of(si_raw$Country), key = country_cell_key(si_raw$tech, si_raw$supply_chain), tech = si_raw$tech, stage = si_raw$supply_chain,
                       eo_level = si_raw$Economic_Opportunity_Index, need = si_raw$energy_security_need, pol = si_raw$pol, sc_weight = si_raw$sc_weight,
                       tech_weight = si_raw$tech_weight, si = si_raw$strategic_index, si_exclusion = si_raw$si_exclusion, resource_base = si_raw$resource_base,
                       pick_exclusion = si_raw$pick_exclusion, policy_basis = si_raw$policy_basis, stringsAsFactors = FALSE)
  w_m <- smf$baseline_inputs$si_weights
  si_w <- c(pol = w_m$policy, eo = w_m$economic_opportunity, need = w_m$energy_security_need, sc = w_m$supply_chain, tech = w_m$technology)
  ok_w <- isTRUE(all.equal(unname(si_w), c(0.2, 0.3, 0.3, 0.1, 0.1)))
  add_check("SI weights: Strategic manifest = the pipeline's (0.2 policy, 0.3 opportunity, 0.3 need, 0.1 stage, 0.1 technology)", if (ok_w) "PASS" else "FAIL", 5)
  # the stage term: each stage's equal share of its old roll-up's weight (SUPPLY_CHAIN_STAGE_SI_WEIGHTS)
  d_sc <- suppressWarnings(max(abs(si_all$sc_weight - unname(SUPPLY_CHAIN_STAGE_SI_WEIGHTS[si_all$stage])), na.rm = TRUE))
  add_check("SI: stage weights = the six-stage weights (Components and Assembly 0.375, Deployment 0.25, Extraction, Refining and Recovery 0.5 / 3)",
            if (is.finite(d_sc) && d_sc <= 1e-9) "PASS" else "WARN", nrow(si_all), d_sc)
  ins <- is.finite(si_all$si)
  rec <- si_w[["eo"]] * si_all$eo_level + si_w[["need"]] * si_all$need + si_w[["pol"]] * si_all$pol + si_w[["sc"]] * si_all$sc_weight + si_w[["tech"]] * si_all$tech_weight
  d_si <- max(abs(rec[ins] - si_all$si[ins]))
  add_check("SI: sum of the five level terms = strategic_index (1e-12)", if (d_si <= 1e-12) "PASS" else "FAIL", sum(ins), d_si)
  pi_raw <- as.data.frame(io$policy_index)
  pi_raw$iso3 <- iso_of(pi_raw$Country)
  pi_raw$key <- country_cell_key(pi_raw$tech, pi_raw$supply_chain)
  pol_index_by <- lapply(split(pi_raw, pi_raw$iso3), function(d) as.list(stats::setNames(d$value, d$key)))
  pe <- as.data.frame(io$policy_eligibility)
  pe$iso3 <- iso_of(pe$Country)
  policy_basis <- as.list(stats::setNames(pe$policy_basis, pe$iso3))
  contrib <- function(cc, vc) {
    cc <- as.data.frame(cc)
    vc <- as.data.frame(vc)
    list(cat = data.frame(iso3 = iso_of(cc$Country), key = country_cell_key(cc$tech, cc$supply_chain), category = cc$category, category_score = cc$category_score,
                          category_weight = cc$category_weight, imputed = cc$imputed, missing_rule_applied = cc$missing_rule_applied, stringsAsFactors = FALSE),
         var = data.frame(iso3 = iso_of(vc$Country), key = country_cell_key(vc$tech, vc$supply_chain), category = vc$category, variable = vc$variable,
                          component_value = vc$component_value, component_weight_within_overall = vc$component_weight_within_overall,
                          imputed = vc$imputed, missing_rule_applied = vc$missing_rule_applied, stringsAsFactors = FALSE))
  }
  ces <- contrib(io$energy_security_category_contributions, io$energy_security_variable_contributions)
  ceo <- contrib(io$economic_opportunity_category_contributions, io$economic_opportunity_variable_contributions)
  # decomposition identities (A6): sum of category weight x score = pillar; sum of within-category weight x value = category score
  chk_dec <- function(cc, vc, pil, label) {
    s <- tapply(cc$category_weight * cc$category_score, paste(cc$iso3, cc$key), sum)
    p <- pil$value[match(names(s), paste(pil$iso3, pil$key))]
    d1 <- max(abs(s - p), na.rm = TRUE)
    s2 <- tapply(vc$component_weight_within_overall * vc$component_value, paste(vc$iso3, vc$key, vc$category), sum)
    c2 <- cc$category_score[match(names(s2), paste(cc$iso3, cc$key, cc$category))]
    d2 <- max(abs(s2 - c2), na.rm = TRUE)
    add_check(paste0(label, ": sum of category weight x score = pillar value (1e-12)"), if (d1 <= 1e-12) "PASS" else "FAIL", length(s), d1)
    add_check(paste0(label, ": sum of within-category weight x value = category score (1e-12)"), if (d2 <= 1e-12) "PASS" else "WARN", length(s2), d2)
  }
  chk_dec(ces$cat, ces$var, es, "Energy Security")
  chk_dec(ceo$cat, ceo$var, eo, "Economic Opportunity")
  rm(io); invisible(gc())

  # cross-check against the Strategic core chunk (A7.3 #1-2)
  cmp_pillar <- function(mine, theirs, label) {
    t <- as.data.frame(theirs)
    k1 <- paste(mine$iso3, mine$key)
    k2 <- paste(t$iso3c, country_cell_key(t$tech, t$supply_chain))
    i <- match(k1, k2)
    v2 <- t$value[i]
    same <- identical(mine$value, v2) && identical(mine$rankable, t$rankable[i])
    # imputed shares: scripts/85 recomputes them (dash_pillars.R) with another summation order, so they can differ in
    # the last bit; the sheet prints them rounded to 4 digits. Values and rankable flags must be identical.
    s2 <- t$imputed_weight_share[i]
    d_imp <- suppressWarnings(max(abs(mine$imputed_share - s2), na.rm = TRUE))
    same_na <- identical(is.na(mine$imputed_share), is.na(s2))
    n_bit <- sum(mine$imputed_share != s2, na.rm = TRUE)
    ok <- same && same_na && is.finite(d_imp) && d_imp <= 1e-12 && nrow(mine) == nrow(t) && !anyNA(i)
    add_check(paste0(label, ": values and rankable flags identical to the Strategic core chunk; imputed shares within 1e-12"),
              if (ok) "PASS" else if (validate_bundles) "FAIL" else "WARN", nrow(mine), max(abs(mine$value - v2), na.rm = TRUE),
              sprintf("imputed shares: %d differ in the last bit (max %.1e; summation order)", n_bit, d_imp))
  }
  cmp_pillar(es, bcore$es, "Energy Security")
  cmp_pillar(eo, bcore$eo, "Economic Opportunity")
  bsi <- as.data.frame(bcore$si$cells)
  k1 <- paste(si_all$iso3[ins], si_all$key[ins])
  k2 <- paste(bsi$iso3c, country_cell_key(bsi$tech, bsi$supply_chain))
  same_si <- identical(si_all$si[ins], bsi$strategic_index[match(k1, k2)])
  add_check("SI: in-sample values identical to the Strategic core chunk", if (same_si && sum(ins) == sum(bsi$in_sample %in% TRUE)) "PASS" else "FAIL", sum(ins))
  si_rank_core <- stats::setNames(bsi$rank_in_country, k2)

  stats_c <- country_cell_stats(es, eo)
  # the sectors without an Energy Security score for any economy (2026-10-28): derived here, shipped in every
  # sheet and the manifest; the constant (and the dashboard's NO_ES_CELLS) must be updated with the data
  no_es_cells <- country_no_es_cells(es, cells_tbl$key)
  add_check("sectors without Energy Security (no_es_cells) = COUNTRY_NO_ES_CELLS (and the dashboard's NO_ES_CELLS)",
            if (setequal(no_es_cells, COUNTRY_NO_ES_CELLS)) "PASS" else "FAIL", length(no_es_cells), NA,
            sprintf("%d of %d sectors can be placed; data: %s", nrow(cells_tbl) - length(no_es_cells), nrow(cells_tbl), paste(no_es_cells, collapse = " ")))

  # ---- IEA PAMS counts (policy_outputs.rds; large) -----------------------------------------------------
  step("policy_outputs.rds (IEA PAMS records)")
  po <- rds("policy_outputs.rds")
  pc <- as.data.frame(po$policy_clean)[, c("policy_id", "iso3", "tech", "supply_chain", "tech_crosscut", "sc_crosscut", "status_norm", "year")]
  rm(po); invisible(gc())
  pams_counts <- country_pams_counts_all(pc)
  eu_records <- length(unique(pc$policy_id[pc$iso3 == "EUR"]))
  # records_tech = Sector's sector_policy_counts() by country x tag (cross-cutting excluded)
  sp_counts <- {
    p <- spol$pams
    tags <- spol$tags
    out <- lapply(seq_along(tags), function(i) {
      hit <- bitwAnd(p$tech_mask, bitwShiftL(1L, i - 1L)) != 0L
      if (!any(hit)) return(NULL)
      t <- table(geo_code(p$geo[hit]))
      data.frame(iso3 = names(t), tech = tags[i], n = as.integer(t), stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  }
  mine_t <- pams_counts$tech
  kk <- paste(mine_t$iso3, mine_t$tech)
  ks <- paste(sp_counts$iso3, sp_counts$tech)
  common <- intersect(kk, ks)
  d_rt <- if (length(common)) max(abs(mine_t$n[match(common, kk)] - sp_counts$n[match(common, ks)])) else NA
  add_check("IEA PAMS: records by country x technology = Sector policy chunk counts", if (isTRUE(d_rt == 0) && length(common) >= 0.95 * length(union(kk, ks))) "PASS" else "WARN",
            length(common), d_rt, sprintf("%d keys only in policy_clean, %d only in the Sector chunk", length(setdiff(kk, ks)), length(setdiff(ks, kk))))
  rm(pc); invisible(gc())

  # ---- Climate Action Tracker (Sector policy chunk, built from the raw CAT file by sector_cat()) ------
  cat_tbl <- as.data.frame(spol$cat)
  cat_tbl$geo <- geo_code(cat_tbl$geo)
  add_check("CAT: ratings read from the Sector policy chunk (scripts/87 sector_cat() of the raw CAT file)", "PASS", nrow(cat_tbl))

  # ---- trade (Sector trade chunks, reconciled matrix, trade year) ------------------------------------
  step("Sector trade chunks")
  trade_year <- as.integer(cfg$trade_year %||% 2024)
  vcol <- paste0("v", trade_year)
  scol <- paste0("s", trade_year)
  flows <- list()
  cell_items <- items[items$axis == "cell", , drop = FALSE]
  basket_keys <- intersect(cell_items$slug[cell_items$has_trade %in% TRUE], cells_tbl$key)
  for (c in xmf$chunks) {
    if (!grepl("^sector-trade-", c$name)) next
    tr <- read_osb(file.path(sdir, c$file))$trade
    f <- tr$flows
    slug <- items$slug[f$item + 1L]
    keep <- slug %in% basket_keys & is.finite(f[[vcol]]) & f[[vcol]] > 0
    if (any(keep)) flows[[length(flows) + 1]] <- data.frame(key = slug[keep], exporter = geo_code(f$exporter[keep]), importer = geo_code(f$importer[keep]),
                                                           value = f[[vcol]][keep], status = f[[scol]][keep], stringsAsFactors = FALSE)
    rm(tr)
  }
  flows <- if (length(flows)) do.call(rbind, flows) else data.frame(key = character(), exporter = character(), importer = character(), value = numeric(),
                                                                    status = character(), stringsAsFactors = FALSE)
  # no basket at the six-stage keys: the Sector bundle is not the six-stage export of scripts/87
  add_check("trade: reconciled flows of the trade year read from the Sector trade chunks", if (nrow(flows) > 0) "PASS" else "FAIL", nrow(flows), NA,
            sprintf("%d six-stage cells with a basket; statuses %s", length(basket_keys), paste(names(table(flows$status)), collapse = "/")))

  # ---- CM-FDI ------------------------------------------------------------------------------------------
  # one row per project; `stages` lists the stages it counts in (config/partnership.yml cm_fdi.stages: Components
  # and Assembly carry the same projects, 2026-10-27); supply_chain is their shared roll-up (Midstream), not used
  cmf <- as.data.frame(rds("cm_fdi_projects_tbl.rds"))
  if (!"stages" %in% names(cmf)) stop("cm_fdi_projects_tbl.rds has no `stages` column: rerun scripts/15 on the six-stage pipeline.")
  fdi <- country_fdi_expand(cmf$project_id, cmf$tech, cmf$stages, cmf$home_iso3, cmf$destination_iso3, cmf$index_use)
  cmfdi_stages <- country_fdi_stages(fdi, cells_tbl)
  add_check("CM-FDI: every project counts in its six-stage manufacturing cells (counted once per project in the totals)",
            if (all(fdi$key %in% cells_tbl$key) && !anyDuplicated(paste(fdi$pid, fdi$key))) "PASS" else "FAIL", length(unique(fdi$pid)), NA,
            sprintf("%d projects, %d project x stage rows; stages %s", length(unique(fdi$pid)), nrow(fdi), paste(cmfdi_stages, collapse = ", ")))
  cmfdi_edition <- { e <- NA_character_; for (s in dict$sources) if (identical(s$id, "cmfdi")) e <- s$edition; e }
  if (!is.null(sfdi)) {
    # the Sector chunk keys each project on the cell of the first of its stages and lists the stages that carry
    # the same projects (fdi.stages, Components and Assembly; sector_fdi.R): each of its rows counts in every
    # listed stage's cell, as here (country_fdi_sector_counts()), and the two stage lists must be the same
    s_stages <- as.character(unlist(sfdi$stages))
    add_check("CM-FDI: the Sector fdi chunk's stages (fdi.stages) = the stages the projects count in here (meta.cmfdi_stages)",
              if (length(s_stages) == length(cmfdi_stages) && setequal(s_stages, cmfdi_stages)) "PASS" else "FAIL", length(s_stages), NA,
              sprintf("Sector: %s; Country: %s", paste(s_stages, collapse = ", "), paste(cmfdi_stages, collapse = ", ")))
    fl <- sfdi$flows[sfdi$flows$basis == "index_use", , drop = FALSE]
    a <- tryCatch(country_fdi_sector_counts(items$slug[fl$item + 1L], geo_code(fl$home), geo_code(fl$dest), fl$n, s_stages),
                  error = function(e) structure(integer(), err = conditionMessage(e)))
    p2 <- fdi[fdi$index_use %in% TRUE, , drop = FALSE]
    b <- table(paste(p2$home, p2$dest, p2$key))
    common <- intersect(names(a), names(b))
    same <- is.null(attr(a, "err")) && length(common) == length(a) && length(common) == length(b) && all(a[common] == b[common])
    add_check("CM-FDI: project counts by home x destination x cell = Sector fdi chunk (index_use; its rows count in every stage of fdi.stages)",
              if (same) "PASS" else "FAIL", length(b), NA,
              attr(a, "err") %||% sprintf("%d Sector rows on the %s cells; %d home x destination x cell counts, %d project x stage rows",
                                          nrow(fl), s_stages[1] %||% "?", length(a), sum(b)))
  }

  # ---- partners ---------------------------------------------------------------------------------------
  step("psi_dyad_tbl.rds, psi_tbl.rds")
  pd <- as.data.frame(rds("psi_dyad_tbl.rds"))
  psi_dyad <- data.frame(reporter_iso = pd$reporter_iso, partner_iso = pd$partner_iso, friendshore = pd$friendshore, opportunity = pd$opportunity,
                         development = pd$development, psi = pd$Partnership_Strength_Index, rankable = pd$rankable, rank_exclusion = pd$rank_exclusion,
                         rank = pd$rank, stringsAsFactors = FALSE)
  rm(pd)
  pt <- as.data.frame(rds("psi_tbl.rds"))
  pt <- pt[pt$viewpoint == "coalition", , drop = FALSE]
  psi_partner_tbl <- data.frame(partner_iso = pt$partner_iso, psi = pt$value, rank = pt$rank, rankable = pt$rankable, stringsAsFactors = FALSE)
  # the Strategic engine recomputes the coalition view (dash_partner.R): values may differ in the last bit
  # (summation order); ranks and rankable flags must be identical. The sheet prints psi_tbl's values.
  ipp <- match(psi_partner_tbl$partner_iso, psi_partner$partner_iso)
  d_pp <- suppressWarnings(max(abs(psi_partner_tbl$psi - psi_partner$psi[ipp]), na.rm = TRUE))
  same_pp <- !anyNA(ipp) && is.finite(d_pp) && d_pp <= 1e-12 && identical(is.na(psi_partner_tbl$psi), is.na(psi_partner$psi[ipp])) &&
    identical(as.numeric(psi_partner_tbl$rank), as.numeric(psi_partner$rank[ipp])) && identical(psi_partner_tbl$rankable %in% TRUE, psi_partner$rankable[ipp] %in% TRUE)
  add_check("PSI (coalition view): psi_tbl = Strategic core chunk psi_partner (ranks identical, values within 1e-12)", if (same_pp) "PASS" else "WARN",
            nrow(psi_partner_tbl), d_pp, sprintf("%d values differ in the last bit", sum(psi_partner_tbl$psi != psi_partner$psi[ipp], na.rm = TRUE)))
  coalition_view <- sprintf("coalition core (%d members)", sum(roster$tier == "core"))

  # best five cells (one cell table at a time; A7.3 #3)
  best5 <- list()
  for (side in c("fs", "po")) {
    tb <- if (side == "fs") "partner_friendshore_tbl.rds" else "partner_opportunity_tbl.rds"
    top <- if (side == "fs") "partner_friendshore_top5_tbl.rds" else "partner_opportunity_top5_tbl.rds"
    vname <- if (side == "fs") "Friendshore Index" else "Opportunity Index"
    step(tb)
    x <- readRDS(file.path(processed_dir, tb))
    x <- x[x$variable == vname, , drop = FALSE]
    x <- data.frame(reporter_iso = x$reporter_iso, partner_iso = x$partner_iso, key = country_cell_key(x$tech, x$supply_chain), value = x$value, stringsAsFactors = FALSE)
    x <- x[is.finite(x$value), , drop = FALSE]
    invisible(gc())
    b <- country_best5(x, side)
    rm(x); invisible(gc())
    t5 <- readRDS(file.path(processed_dir, top))
    t5 <- t5[t5$variable == vname, , drop = FALSE]
    ref <- t5$value[match(paste(b$reporter_iso, b$partner_iso), paste(t5$reporter_iso, t5$partner_iso))]
    d5 <- max(abs(b$mean - ref), na.rm = TRUE)
    add_check(paste0(vname, ": mean of the best five cells = the top-five table (1e-12)"), if (d5 <= 1e-12 && !anyNA(ref)) "PASS" else "FAIL", nrow(b), d5,
              sprintf("%d dyads without a top-five row", sum(is.na(ref))))
    rm(t5); invisible(gc())
    best5[[side]] <- b
  }
  best5 <- rbind(best5$fs, best5$po)

  # ---- ties ---------------------------------------------------------------------------------------------
  agreements <- read.csv(file.path(repo_root, "data", "reference", "economic_security_agreements.csv"), stringsAsFactors = FALSE)
  ga <- as.data.frame(rds("geo_alignment_tbl.rds"))[, c("reporter_iso", "partner_iso", "geo_alignment", "bloc_partner", "basis_partner")]
  un_window <- { w <- as.integer(strsplit(ip$window_years[1], ",", fixed = TRUE)[[1]]); paste0(min(w), "-", max(w)) }

  # ---- allied picks (the network design's member shares are retired, 2026-10-17) --------------------------
  allied_csv <- read.csv(file.path(processed_dir, "charts", "allied_strategic.csv"), stringsAsFactors = FALSE)
  allied <- data.frame(iso3 = allied_csv$iso3, tech = allied_csv$tech, supply_chain = allied_csv$supply_chain, strategic_index = allied_csv$strategic_index,
                       stringsAsFactors = FALSE)
  ba <- as.data.frame(bcore$allied)
  same_al <- identical(paste(sort(paste(allied$iso3, allied$tech, allied$supply_chain))), paste(sort(paste(ba$iso3c, ba$tech, ba$supply_chain))))
  add_check("allied picks: charts/allied_strategic.csv = Strategic core chunk allied", if (same_al) "PASS" else "FAIL", nrow(allied))
  # share of picks within near_tie of the runner-up (the pick_stability caveat)
  near <- vapply(seq_len(nrow(allied)), function(i) {
    d <- si_all[si_all$iso3 == allied$iso3[i], , drop = FALSE]
    ru <- country_pick_runner_up(d, country_cell_key(allied$tech[i], allied$supply_chain[i]))
    !is.null(ru) && is.finite(ru$margin) && ru$margin < (cfg$near_tie %||% 0.01)
  }, TRUE)
  pick_share <- mean(near)

  # ---- NIPO (latest completed run) --------------------------------------------------------------------------
  nipo <- NULL
  nipo_meta <- NULL
  np <- file.path(processed_dir, "nipo_policy_all.rds")
  if (file.exists(np)) {
    step("nipo_policy_all.rds")
    source(file.path(repo_root, "R", "utils", "country.R"))
    nx <- as.data.frame(readRDS(np))
    juris <- as.character(nx$`Implementing Jurisdiction`)
    nipo_titles_all <- as.character(nx$Title)
    nx_impl_raw <- nx$`Implementation Date`
    nx_rem_raw <- nx$`Removal Date`
    u <- unique(juris)
    map <- stats::setNames(country_name_to_iso3(u), u)
    nipo <- data.frame(
      iso3 = unname(map[juris]), title = as.character(nx$Title),
      impl_date = country_nipo_date(nx$`Implementation Date`), removal_date = country_nipo_date(nx$`Removal Date`),
      strength = nx$policy_strength_pkg, strength_base = nx$policy_strength, confidence = nx$mapping_confidence_mean,
      techs = as.character(nx$tech_csv), stages = as.character(nx$supply_chain_csv), url = as.character(nx$URL), source = NA_character_,
      stringsAsFactors = FALSE)
    rm(nx); invisible(gc())
    nt <- readRDS(file.path(processed_dir, "nipo_policy_index_tbl.rds"))
    nipo_asof <- max(nt$as_of_date, na.rm = TRUE)
    nipo_meta <- list(vintage = cfg$nipo$vintage, vintage_label = cfg$nipo$vintage_label, index_as_of = format(nipo_asof),
                      jurisdictions = length(unique(nipo$iso3[!is.na(nipo$iso3)])), rule = cfg$nipo$active_rule %||% "in_force",
                      strength_words = cfg$nipo$strength_words, confidence_words = cfg$nipo$confidence_words,
                      eu_prefixes = cfg$nipo$eu_prefixes, eu_min_jurisdictions = cfg$nipo$eu_min_jurisdictions %||% 5L,
                      staged = country_nipo_staged(nipo), staged_words = cfg$nipo$not_staged_words)
    add_check("NIPO: measures tagged with the six supply-chain stages (else no per-sector lists in the sheets)", if (nipo_meta$staged) "PASS" else "WARN",
              nrow(nipo), NA, sprintf("stages in the run: %s", paste(sort(unique(unlist(country_nipo_split(nipo$stages)))), collapse = ", ")))
    pos_s <- nipo$strength[is.finite(nipo$strength) & nipo$strength > 0]
    if (length(pos_s)) {
      q <- stats::quantile(pos_s, c(0.25, 0.75), names = FALSE)
      nipo_meta$strength_words <- paste0(nipo_meta$strength_words, " ", sprintf("In this run half of the measures with a positive strength score between %s and %s; the strongest scores %s.",
                                         country_fmt(q[1], "index"), country_fmt(q[2], "index"), country_fmt(max(pos_s), "index")))
    }
    bad_dates <- sum(!is.na(nx_impl_raw) & is.na(nipo$impl_date)) + sum(!is.na(nx_rem_raw) & is.na(nipo$removal_date))
    add_check("NIPO: implementation and removal dates read as dates (none lost)", if (bad_dates == 0) "PASS" else "FAIL", nrow(nipo), NA,
              sprintf("%d implementation dates missing in the run itself (announced or scheduled measures); stored as %s", sum(is.na(nipo$impl_date)),
                      paste(class(nx_impl_raw), collapse = "/")))
    rm(nx_impl_raw, nx_rem_raw)
    add_check("NIPO: policy table of the latest completed run read", "PASS", nrow(nipo), NA,
              sprintf("%s; %d implementing jurisdictions mapped, %d unmapped names; index as-of %s", cfg$nipo$vintage_label, nipo_meta$jurisdictions,
                      sum(is.na(map)), format(nipo_asof)))
    nipo <- nipo[!is.na(nipo$iso3), , drop = FALSE]
    # the run's extract: every title of the run is in cfg$nipo$extract_file
    xl <- list.files(raw_dir, pattern = "^GTA NIPO - .*[.]xlsx$", full.names = TRUE)
    if (length(xl) && requireNamespace("readxl", quietly = TRUE)) {
      run_titles <- unique(nipo_titles_all)
      found <- vapply(xl, function(p) {
        h <- names(readxl::read_excel(p, sheet = 1, n_max = 0))
        x <- readxl::read_excel(p, sheet = 1, col_types = ifelse(h == "Title", "text", "skip"))
        sum(run_titles %in% x$Title)
      }, 1)
      names(found) <- basename(xl)
      ok_v <- identical(unname(found[cfg$nipo$extract_file]), as.numeric(length(run_titles)))
      add_check(sprintf("NIPO: every title of the run is in %s (the labelled vintage)", cfg$nipo$extract_file), if (ok_v) "PASS" else "FAIL",
                length(run_titles), NA, paste(sprintf("%s %d", names(found), found), collapse = "; "))
    } else add_check("NIPO: vintage not re-checked (no GTA extract in data/raw)", "WARN", NA)
  }

  # ---- raw rows behind the drivers ------------------------------------------------------------------------
  step("Theme tables of raw_tables")
  rt <- unique(vapply(cfg$raw_tables, function(e) e$table, ""))
  raw_rows <- do.call(rbind, lapply(rt, function(tb) {
    x <- as.data.frame(rds(paste0(tb, ".rds")))
    want <- unique(vapply(Filter(function(e) e$table == tb, cfg$raw_tables), function(e) paste(e$category, e$raw_variable), ""))
    x <- x[x$data_type == "raw" & paste(x$category, x$variable) %in% want, , drop = FALSE]
    if ("sub_sector" %in% names(x)) x <- x[is.na(x$sub_sector) | x$sub_sector == "All", , drop = FALSE]
    data.frame(table = tb, iso3 = x$iso3c, tech = x$tech, stage = x$supply_chain, category = x$category, variable = x$variable, value = x$value,
               Year = x$Year, source = x$source, explanation = x$explanation, stringsAsFactors = FALSE)
  }))
  raw_idx <- country_raw_index(raw_rows)
  # Sector facts: notes and values by (var, item, geo, year)
  fv_var <- vars_dict$id[sfacts$obs$var + 1L]
  fv_item <- items$slug[sfacts$obs$item + 1L]
  fv_geo <- geo_code(sfacts$obs$geo)
  facts_key <- paste(fv_var, fv_item, fv_geo, sfacts$obs$year, sep = "\r")
  facts_val <- stats::setNames(sfacts$obs$value, facts_key)
  nt_k <- paste(vars_dict$id[sfacts$notes$var + 1L], items$slug[sfacts$notes$item + 1L], geo_code(sfacts$notes$geo), sfacts$notes$year, sep = "\r")
  notes_by <- stats::setNames(sfacts$notes$note, nt_k)
  note_fn <- country_note_fn(notes_by)
  # trade totals (A7.3 #4)
  t_imp <- stats::setNames(sfacts$obs$value[fv_var == "trade.imports" & sfacts$obs$year == trade_year], paste(fv_item, fv_geo)[fv_var == "trade.imports" & sfacts$obs$year == trade_year])
  t_exp <- stats::setNames(sfacts$obs$value[fv_var == "trade.exports" & sfacts$obs$year == trade_year], paste(fv_item, fv_geo)[fv_var == "trade.exports" & sfacts$obs$year == trade_year])

  # The baseline date: the day (local time) the pipeline wrote the newest input of the Strategic export,
  # i.e. the final rerun, not the UTC day of the export itself.
  as_of_date <- as.Date(format(s_in, "%Y-%m-%d"))
  meta <- list(
    as_of = format(as_of_date), as_of_label = country_date_label(as_of_date), created = created, git_commit = git_commit,
    strategic_bundle_id = smf$bundle_id, sector_bundle_id = xmf$bundle_id, trade_year = trade_year, un_window = un_window,
    cmfdi_edition = cmfdi_edition, rank_threshold = smf$baseline_params$advanced$tau %||% 0.5,
    pick_share_d = country_fmt(pick_share, "pct") , coalition_view = coalition_view, nipo = nipo_meta,
    no_es_cells = no_es_cells, cmfdi_stages = cmfdi_stages,
    validated = validate_bundles, synthetic = FALSE
  )
  meta$pick_share_d <- country_fmt(100 * pick_share, "pct")
  meta$label <- country_fill(cfg$label, list(as_of_label = meta$as_of_label))

  dup_rows <- list()
  raw_cited <- list()
  picks_seen <- list()
  ctx <- list(
    cfg = cfg, dict = dict, meta = meta, roster = roster,
    es_by = split(es, es$iso3), eo_by = split(eo, eo$iso3), stats = stats_c, no_es_cells = no_es_cells,
    si_by = split(si_all, si_all$iso3), si_w = si_w, pol_index_by = pol_index_by, policy_basis = policy_basis,
    pams_counts = pams_counts, eu_records = eu_records, cat_tbl = cat_tbl,
    cat_by = list(es = split(ces$cat, ces$cat$iso3), eo = split(ceo$cat, ceo$cat$iso3)),
    var_by = list(es = split(ces$var, ces$var$iso3), eo = split(ceo$var, ceo$var$iso3)),
    .cat_tbl = list(es = ces$cat, eo = ceo$cat), .var_tbl = list(es = ces$var, eo = ceo$var),
    raw_rows = raw_rows, raw_idx = raw_idx, flows = flows, basket_keys = basket_keys, fdi = fdi,
    psi_by = split(psi_dyad, psi_dyad$reporter_iso), best5 = best5, psi_partner = psi_partner_tbl,
    agreements = agreements, geo_alignment = ga, allied = allied, nipo = nipo,
    party_fn = party_fn, note_fn = note_fn,
    dup_fn = function(e, n) dup_rows[[length(dup_rows) + 1]] <<- paste(e$pillar, e$category, e$variable, n),
    pick_fn = function(iso3, pick) picks_seen[[iso3]] <<- pick
  )
  ctx$sector_sources <- as.data.frame(smeta$dict$sources)
  step("Driver statistics")
  ctx <- country_ctx_finish(ctx)
  # EU regional figures (raw_tables eu_region): every flagged entry and cell should give the EU members one shared
  # value (the IEA ETP 2035 shares and the IEA EV-region figures are copied to each member by the pipeline)
  eu_iso <- roster$iso3[roster$eu_member %in% TRUE]
  eu_ent <- Filter(function(e) !is.null(e$eu_region), cfg$raw_tables)
  eu_groups <- unique(unlist(lapply(eu_ent, function(e) {
    r <- raw_rows[raw_rows$table == e$table & raw_rows$category == e$category & raw_rows$variable == e$raw_variable &
                    raw_rows$iso3 %in% eu_iso & is.finite(raw_rows$value), , drop = FALSE]
    unique(paste(r$table, r$tech, r$stage, r$category, r$variable, sep = "\r"))
  })))
  add_check("EU regional figures: each eu_region figure gives the EU members one shared value (raw note eu_region_share)",
            if (length(eu_groups) && all(eu_groups %in% names(ctx$eu_vals))) "PASS" else "WARN", length(ctx$eu_vals), NA,
            sprintf("%d of %d entry x cell groups shared; %s", sum(eu_groups %in% names(ctx$eu_vals)), length(eu_groups),
                    paste(utils::head(gsub("\r", " / ", setdiff(eu_groups, names(ctx$eu_vals))), 3), collapse = "; ")))
  if (!is.null(ctx$nipo)) {
    sc_tab <- table(factor(ctx$nipo$scope, levels = c("eu", "multi", "national")))
    eu_pre_rows <- grepl("^(EU|EC|European Union|EU Countries):", ctx$nipo$title)
    add_check("NIPO: measures marked EU-wide, shared or national (titles recorded under several jurisdictions)",
              if (sc_tab[["eu"]] > 0 && all(ctx$nipo$scope[eu_pre_rows & ctx$nipo$n_juris >= (cfg$nipo$eu_min_jurisdictions %||% 5L)] == "eu")) "PASS" else "WARN",
              nrow(ctx$nipo), NA, sprintf("rows: %d EU-wide, %d shared, %d national; EU-wide titles recorded under %s jurisdictions (median)",
                                          sc_tab[["eu"]], sc_tab[["multi"]], sc_tab[["national"]],
                                          format(stats::median(ctx$nipo$n_juris[ctx$nipo$scope == "eu"]))))
  }
  save_ctx <- arg_value("--save-ctx")
  if (!is.null(save_ctx)) {
    saveRDS(list(ctx = ctx, es = es, eo = eo, si_all = si_all, psi_dyad = psi_dyad, si_rank_core = si_rank_core, t_imp = t_imp, t_exp = t_exp,
                 facts_val = facts_val, trade_year = trade_year), save_ctx)
    step("Saved inputs to ", save_ctx)
  }
}

# =====================================================================================================
# Sheets
# =====================================================================================================

roster <- ctx$roster
todo <- if (is.null(only_iso)) roster$iso3 else intersect(only_iso, roster$iso3)
step("Fact sheets: ", length(todo), " economies")
sheets <- list()
problems <- list()
t_sheets <- Sys.time()
for (iso in todo) {
  s <- tryCatch(withCallingHandlers(country_fact_sheet(iso, ctx), error = function(e) {
    calls <- vapply(sys.calls(), function(cl) paste(deparse(cl, nlines = 1), collapse = ""), "")
    attr(problems, "trace") <<- c(attr(problems, "trace"), paste(iso, ":", paste(utils::tail(calls, 6), collapse = " <- ")))
  }), error = function(e) { problems[[iso]] <<- conditionMessage(e); NULL })
  if (!is.null(s)) sheets[[iso]] <- s
}
if (length(problems)) message(paste(utils::head(attr(problems, "trace"), 3), collapse = "\n"))
add_check("sheets: built without error", if (!length(problems)) "PASS" else "FAIL", length(sheets), NA,
          if (length(problems)) paste(utils::head(paste(names(problems), unlist(problems)), 5), collapse = " | ") else
            sprintf("%.0f s", as.numeric(difftime(Sys.time(), t_sheets, units = "secs"))))

# JSON, round trip and the R mirror of the contract checks (A7.3 #7)
texts <- list()
drv_texts <- list()
rt_bad <- character()
chk_bad <- list()
for (iso in names(sheets)) {
  extra <- attr(sheets[[iso]], "drivers_extra")
  if (length(extra)) drv_texts[[iso]] <- as.character(country_json(country_drivers_doc(sheets[[iso]], extra)))
  j <- as.character(country_json(sheets[[iso]]))
  back <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  if (!identical(as.character(country_json(back)), j)) rt_bad <- c(rt_bad, iso)
  pr <- country_check_sheet(back)
  if (length(pr)) chk_bad[[iso]] <- pr
  texts[[iso]] <- j
  sheets[[iso]] <- back
}
add_check("sheets: JSON round trip exact (re-serialised text identical)", if (!length(rt_bad)) "PASS" else "FAIL", length(texts), NA,
          if (length(rt_bad)) paste(utils::head(rt_bad, 10), collapse = ", ") else NA)
add_check("sheets: country_check_sheet() (R mirror of checkFactSheet)", if (!length(chk_bad)) "PASS" else "FAIL", length(texts), NA,
          if (length(chk_bad)) paste(utils::head(paste(names(chk_bad), vapply(chk_bad, `[`, "", 1)), 5), collapse = " | ") else NA)
d_missing <- 0L
for (iso in names(sheets)) {
  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$v) && is.numeric(x$v) && (is.null(x$d) || !nzchar(x$d))) d_missing <<- d_missing + 1L
      for (y in x) walk(y)
    }
  }
  walk(sheets[[iso]])
}
add_check("sheets: every Fv with a value has a display string", if (d_missing == 0) "PASS" else "FAIL", length(sheets), NA, if (d_missing) paste(d_missing, "without d") else NA)
# A sector with a pillar constant across the economies compared ranks none of them (country_constant_pillars(),
# the Strategic Index's degenerate rule): it is never placed, so never in a corner or an order list.
const_cells <- country_constant_cells(ctx$stats)
placed_const <- character()
for (iso in names(sheets)) for (cl in sheets[[iso]]$cells) if (isTRUE(cl$placed) && cl$key %in% const_cells$key) placed_const <- c(placed_const, paste(iso, cl$key))
add_check("sheets: no sector with a pillar constant across the economies compared is placed", if (!length(placed_const)) "PASS" else "FAIL",
          length(sheets), NA,
          paste0(if (nrow(const_cells)) paste("constant:", paste(const_cells$key, toupper(const_cells$pillar), collapse = ", ")) else "none constant",
                 if (length(placed_const)) paste0("; placed: ", paste(utils::head(placed_const, 5), collapse = ", ")) else ""))

if (!synthetic) {
  # every shared value equals data/processed after the round trip (A7.3 #1-3); JSON integers read back
  # as R integers, so values compare as doubles, bit for bit; NULL (JSON null) equals NA
  same_num <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(length(b) == 1 && is.na(b))
    length(b) == 1 && !is.na(b) && identical(as.numeric(a), as.numeric(b))
  }
  bad_v <- 0L; n_v <- 0L; bad_si <- 0L; bad_rank <- 0L; bad_psi <- 0L; n_psi <- 0L
  es_k <- paste(es$iso3, es$key); eo_k <- paste(eo$iso3, eo$key); si_k <- paste(si_all$iso3, si_all$key)
  pd_k <- paste(psi_dyad$reporter_iso, psi_dyad$partner_iso)
  for (iso in names(sheets)) {
    s <- sheets[[iso]]
    for (c in s$cells) {
      k <- paste(iso, c$key)
      if (!is.null(c$es)) { n_v <- n_v + 1L; if (!same_num(c$es$v, es$value[match(k, es_k)])) bad_v <- bad_v + 1L }
      if (!is.null(c$eo)) { n_v <- n_v + 1L; if (!same_num(c$eo$v, eo$value[match(k, eo_k)])) bad_v <- bad_v + 1L }
      if (isTRUE(c$si$in_sample)) {
        if (!same_num(c$si$v, si_all$si[match(k, si_k)])) bad_si <- bad_si + 1L
        if (!same_num(c$si$rank$v, si_rank_core[[k]])) bad_rank <- bad_rank + 1L
      }
    }
    for (r in s$partners$rows) {
      i <- match(paste(iso, r$iso3), pd_k)
      if (is.na(i)) next
      n_psi <- n_psi + 1L
      if (!same_num(r$psi$v, psi_dyad$psi[i]) || !same_num(r$fs$v, psi_dyad$friendshore[i]) || !same_num(r$po$v, psi_dyad$opportunity[i]) ||
          !same_num(r$dp$v, psi_dyad$development[i]) || !same_num(r$rank$v, if (isTRUE(psi_dyad$rankable[i])) psi_dyad$rank[i] else NA)) bad_psi <- bad_psi + 1L
    }
  }
  add_check("read-back sheets: ES and EO values identical to index_outputs.rds", if (bad_v == 0) "PASS" else "FAIL", n_v, NA, if (bad_v) paste(bad_v, "differ") else NA)
  add_check("read-back sheets: SI values identical to index_outputs.rds, ranks = the core chunk's rank_in_country", if (bad_si + bad_rank == 0) "PASS" else "FAIL",
            NA, NA, if (bad_si + bad_rank) sprintf("%d values, %d ranks differ", bad_si, bad_rank) else NA)
  add_check("read-back sheets: partner PSI, Friendshore, Opportunity, Development and rank identical to psi_dyad_tbl.rds", if (bad_psi == 0) "PASS" else "FAIL", n_psi, NA,
            if (bad_psi) paste(bad_psi, "differ") else NA)
  # allied pick = first eligible sector
  bad_pick <- sum(vapply(names(picks_seen), function(iso) !identical(picks_seen[[iso]]$first, picks_seen[[iso]]$cell), TRUE))
  add_check("allied picks: each pick is the economy's top eligible sector (any stage but Deployment, the endowment screen on Extraction)", if (bad_pick == 0) "PASS" else "WARN", length(picks_seen), NA,
            sprintf("%s of the picks within %s of the runner-up", meta$pick_share_d, cfg$near_tie))
  # trade totals vs Sector facts (A7.3 #4)
  n_t <- 0L; d_t <- 0; bad_t <- 0L; over <- 0L
  for (iso in names(sheets)) for (c in sheets[[iso]]$cells) {
    if (is.null(c$trade) || !isTRUE(c$trade$has_basket)) next
    for (side in c("imports", "exports")) {
      v <- c$trade[[side]]$v %||% 0
      ref <- if (side == "imports") t_imp[paste(c$key, iso)] else t_exp[paste(c$key, iso)]
      if (is.na(ref)) { if (v > 0) bad_t <- bad_t + 1L; next }
      n_t <- n_t + 1L
      d <- abs(v - ref) / max(1, abs(ref))
      d_t <- max(d_t, d)
      if (d > 1e-9) bad_t <- bad_t + 1L
    }
  }
  add_check(sprintf("trade: cell imports and exports (%d) = Sector facts trade.imports / trade.exports (relative 1e-9)", trade_year),
            if (bad_t == 0) "PASS" else "FAIL", n_t, d_t, if (bad_t) paste(bad_t, "differ or are missing") else NA)
  # raw driver values vs Sector facts (A7.3 #6)
  n_r <- 0L; bad_r <- 0L; d_r <- 0
  for (iso in names(sheets)) for (c in sheets[[iso]]$cells) for (pl in c("es", "eo")) for (d in c$drivers[[pl]]) for (v in d$vars) {
    if (is.null(v$raw) || is.null(v$raw$v) || is.null(v$sector_link)) next
    q <- regmatches(v$sector_link, regexec("m=([^&]+)&item=([^&]+)", v$sector_link))[[1]]
    if (length(q) != 3) next
    ref <- facts_val[paste(q[2], q[3], iso, v$raw$year, sep = "\r")]
    if (is.na(ref)) next
    n_r <- n_r + 1L
    dd <- abs(v$raw$v - ref) / max(1e-12, abs(ref))
    d_r <- max(d_r, dd)
    if (dd > 1e-9) bad_r <- bad_r + 1L
  }
  add_check("drivers: raw values = Sector facts value of the same measure, item, economy and year (relative 1e-9)", if (bad_r == 0) "PASS" else "WARN", n_r, d_r,
            if (bad_r) paste(bad_r, "differ (units or vintages differ between the theme table and the Sector measure)") else NA)
  add_check("drivers: every cited (pillar, category, variable) resolves to at most one raw row", if (!length(dup_rows)) "PASS" else "FAIL",
            NA, NA, if (length(dup_rows)) paste(utils::head(unique(unlist(dup_rows)), 5), collapse = " | ") else NA)
}

# sizes (A7.3 #7)
gz_size <- function(txt) length(memCompress(charToRaw(enc2utf8(txt)), "gzip"))
sizes <- vapply(texts, gz_size, 1)
budget <- COUNTRY_BUDGET_GZ[["sheet"]]
target <- COUNTRY_BUDGET_GZ[["sheet_target"]]
add_check(sprintf("sheets: gzip size <= %s bytes (WARN above %s)", format(budget, big.mark = ","), format(target, big.mark = ",")),
          if (all(sizes <= budget)) (if (all(sizes <= target)) "PASS" else "WARN") else "FAIL",
          length(sizes), NA, if (length(sizes)) sprintf("max %s (%s), median %s", format(max(sizes), big.mark = ","), names(sizes)[which.max(sizes)], format(stats::median(sizes), big.mark = ",")) else NA)

# Monthly trade by sub-component (2026-10-19): one companion file per core member with monthly data,
# trade-<ISO3>.<h8>.json, from scripts/99's pull (data/raw/comtrade_monthly_core.csv and its windows file)
# and the in-basket HS6 master. An optional input: without it the Country pages show no monthly card.
trade_texts <- list()
mon <- NULL
if (synthetic) {
  mon <- country_synthetic_monthly()
} else {
  m_paths <- file.path(raw_dir, c("comtrade_monthly_core.csv", "comtrade_monthly_core_windows.csv", "energy_hs6_master.csv"))
  if (all(file.exists(m_paths))) {
    mon <- list(
      raw = utils::read.csv(m_paths[1], colClasses = c(period = "character", reporter_iso = "character", flow_code = "character", cmd_code = "character")),
      windows = utils::read.csv(m_paths[2], stringsAsFactors = FALSE),
      # the six-stage cells of the HS6 master's `stage` column (country_monthly_basket(); 2026-10-28)
      basket = country_monthly_basket(utils::read.csv(m_paths[3], colClasses = "character"))
    )
  } else {
    add_check("trade monthly: inputs present (scripts/99 data/raw/comtrade_monthly_core.csv and windows file)", "WARN", NA, NA,
              paste("missing:", paste(basename(m_paths[!file.exists(m_paths)]), collapse = ", ")))
  }
}
if (!is.null(mon)) {
  src_m <- Filter(function(s) identical(s$id, "comtrade_monthly"), cfg$sources)
  if (!length(src_m)) stop("config/country_insights.yml: sources has no comtrade_monthly entry.")
  src_m <- src_m[[1]]
  src_ref <- list(id = src_m$id, label = src_m$label, edition = src_m$edition %||% NA_character_,
                  licence = src_m$licence %||% NA_character_, url = src_m$url %||% NA_character_)
  mtbl <- country_monthly_table(mon$raw, mon$basket, mon$windows)
  core_m <- intersect(roster$iso3[roster$tier == "core"], names(sheets))
  probs_m <- character()
  for (iso in intersect(core_m, unique(mtbl$iso3))) {
    doc <- country_monthly_doc(mtbl, iso, mon$windows[mon$windows$reporter_iso == iso, , drop = FALSE], mon$basket, src_ref)
    txt <- as.character(country_json(doc))
    pr <- country_check_monthly_doc(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    if (length(pr)) probs_m <- c(probs_m, paste0(iso, ": ", pr))
    trade_texts[[iso]] <- txt
  }
  add_check("trade monthly: every doc passes country_check_monthly_doc() after a JSON round trip", if (!length(probs_m)) "PASS" else "FAIL",
            length(trade_texts), NA, if (length(probs_m)) paste(utils::head(probs_m, 3), collapse = " | ") else NA)
  tsz <- vapply(trade_texts, gz_size, 1)
  tb <- COUNTRY_BUDGET_GZ[["trade_monthly"]]
  add_check(sprintf("trade monthly: gzip size <= %d bytes", tb), if (all(tsz <= tb)) "PASS" else "FAIL", length(tsz), NA,
            if (length(tsz)) sprintf("max %s (%s), total %s", format(max(tsz), big.mark = ","), names(tsz)[which.max(tsz)], format(sum(tsz), big.mark = ",")) else NA)
  base_gap <- names(trade_texts)[vapply(trade_texts, function(txt) {
    d <- jsonlite::fromJSON(txt, simplifyVector = TRUE)$window$declared
    !isTRUE(d$imports[1]) || !isTRUE(d$exports[1])
  }, logical(1))]
  add_check("trade monthly: every economy reported the first month of its window (the index base)", if (!length(base_gap)) "PASS" else "WARN",
            length(trade_texts), NA, if (length(base_gap)) paste("no base month:", paste(base_gap, collapse = " ")) else NA)
  no_m <- setdiff(core_m, names(trade_texts))
  add_check("trade monthly: core members with a monthly file", if (!length(no_m)) "PASS" else "WARN", length(trade_texts), NA,
            if (length(no_m)) paste("no monthly data:", paste(no_m, collapse = " ")) else NA)
}

# =====================================================================================================
# Manifest and files
# =====================================================================================================

validation <- do.call(rbind, val_rows)
n_fail <- sum(validation$status == "FAIL")
md5_8 <- function(txt) substr(digest_md5(txt), 1, 8)
digest_md5 <- function(txt) {
  tf <- tempfile()
  writeBin(charToRaw(enc2utf8(txt)), tf)
  on.exit(unlink(tf))
  unname(tools::md5sum(tf))
}

old_manifest <- if (file.exists(file.path(out_dir, "manifest.json"))) tryCatch(jsonlite::fromJSON(file.path(out_dir, "manifest.json"), simplifyVector = FALSE), error = function(e) NULL) else NULL
old_memo <- list()
if (!is.null(old_manifest)) for (c in old_manifest$countries) if (!is.null(c$memo)) old_memo[[c$iso3]] <- c$memo

# Memos kept under an earlier quadrant rule (the user's decision of 2026-10-15; country_memo_rule_status()
# in R/dashboard/country/country_memo.R). A published memo whose fact sheet changed is re-checked: its
# claims (<memo-dir>/<ISO3>/claims.json) are re-resolved with the memo tooling on the new sheet and on a
# copy of it built under the earlier rule (each sector split at its median economy). When every claim
# matches the earlier-rule copy, only quadrant-derived claims can differ: the memo stays published with
# memo.rule_note (and rule_note_d, the words of the UI's notice) if its claims or the country's corners
# changed. Any other changed claim marks the memo stale, as before. Never writes in the memo directory.
memo_dir <- abs_path(arg_value("--memo-dir", "dashboard/country-memos"))
read_utf8 <- function(path) {
  txt <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(txt) <- "UTF-8"
  txt
}
sheet_with_drivers <- function(sheet_txt, drv_txt) {
  sh <- jsonlite::fromJSON(sheet_txt, simplifyVector = FALSE)
  if (!is.null(drv_txt)) sh <- country_merge_drivers(sh, jsonlite::fromJSON(drv_txt, simplifyVector = FALSE))
  sh
}
memo_rule_check <- function(iso) {
  dir <- file.path(memo_dir, iso)
  if (!file.exists(file.path(dir, "memo.src.md")) || !file.exists(file.path(dir, "claims.json"))) {
    return(list(status = "stale", changed = character(), other = "(no memo source or claims file)", moved = character()))
  }
  src <- read_utf8(file.path(dir, "memo.src.md"))
  stored <- jsonlite::fromJSON(file.path(dir, "claims.json"), simplifyVector = FALSE)$claims
  new <- sheet_with_drivers(texts[[iso]], drv_texts[[iso]])
  ctx_old <- ctx
  ctx_old$quadrant_rule <- "sector_median"
  ctx_old$pick_fn <- NULL
  ctx_old$dup_fn <- function(...) NULL
  so <- country_fact_sheet(iso, ctx_old)
  ex <- attr(so, "drivers_extra")
  old <- sheet_with_drivers(as.character(country_json(so)), if (length(ex)) as.character(country_json(country_drivers_doc(so, ex))) else NULL)
  moved <- country_quadrants_moved(new, old)
  st <- country_memo_rule_status(stored, country_memo_reclaim(src, new, cfg), country_memo_reclaim(src, old, cfg), moved)
  st$moved <- moved
  st
}
memo_rule_log <- list()
memo_revised <- character()

files <- list()
entries <- list()
for (i in seq_len(nrow(roster))) {
  ro <- roster[i, ]
  iso <- ro$iso3
  s <- sheets[[iso]]
  file <- NULL
  bytes <- NULL
  memo <- NULL
  if (!is.null(s)) {
    file <- sprintf("facts-%s.%s.json", iso, md5_8(texts[[iso]]))
    bytes <- unname(sizes[[iso]])
    files[[file]] <- texts[[iso]]
    if (!is.null(drv_texts[[iso]])) {
      dfile <- sprintf("drivers-%s.%s.json", iso, md5_8(drv_texts[[iso]]))
      files[[dfile]] <- drv_texts[[iso]]
    } else dfile <- NULL
    if (!is.null(trade_texts[[iso]])) {
      tfile <- sprintf("trade-%s.%s.json", iso, md5_8(trade_texts[[iso]]))
      files[[tfile]] <- trade_texts[[iso]]
    } else tfile <- NULL
    om <- old_memo[[iso]]
    if (!is.null(om) && identical(ro$tier, "core") && file.exists(file.path(out_dir, om$json))) {
      if (country_memo_under_revision(om, cfg$memo_revision)) {
        # written before the revision cut-off (config memo_revision): hidden as "being revised"
        om$stale <- TRUE
        om["rule_note"] <- list(NULL)
        om["rule_note_d"] <- list(NULL)
        memo_revised <- c(memo_revised, iso)
      } else if (identical(om$facts_sha256, s$checks$facts_sha256)) {
        om$stale <- isTRUE(om$stale)
      } else if (synthetic || (isTRUE(om$stale) && is.null(om$rule_note))) {
        om$stale <- TRUE
      } else {
        st <- tryCatch(memo_rule_check(iso), error = function(e) list(status = "stale", changed = character(), other = paste("(error:", conditionMessage(e), ")"), moved = character()))
        memo_rule_log[[iso]] <- st
        keep <- identical(st$status, "rule_note")
        om$stale <- identical(st$status, "stale")
        om["rule_note"] <- list(if (keep) COUNTRY_MEMO_RULE_NOTE else NULL)
        om["rule_note_d"] <- list(if (keep) country_memo_rule_note_words(cfg, COUNTRY_MEMO_RULE_NOTE, ro$name, om$generated_at) else NULL)
      }
      memo <- om
    }
  }
  e <- country_manifest_entry(as.list(ro), s, file, bytes, memo)
  e$drivers <- if (!is.null(s)) dfile else NULL
  e$trade_monthly <- if (!is.null(s)) tfile else NULL
  entries[[length(entries) + 1]] <- e
}

if (length(memo_rule_log)) {
  for (iso in names(memo_rule_log)) {
    st <- memo_rule_log[[iso]]
    message(sprintf("  memo %s: %s (claims changed: %d%s; sectors that changed corner: %d%s)", iso, st$status, length(st$changed),
                    if (length(st$changed)) paste0(" - ", paste(utils::head(st$changed, 4), collapse = ", ")) else "", length(st$moved),
                    if (length(st$other)) paste0("; other changes: ", paste(utils::head(st$other, 4), collapse = ", ")) else ""))
  }
  n_st <- table(factor(vapply(memo_rule_log, function(x) x$status, ""), levels = c("current", "rule_note", "stale")))
  add_check("memos: claims re-resolved on the new sheets (kept with the 2026-10-15 quadrant-rule note when only quadrant-derived claims changed)",
            if (n_st[["stale"]] == 0) "PASS" else "WARN", length(memo_rule_log), NA,
            sprintf("%d current, %d kept with the rule note, %d stale", n_st[["current"]], n_st[["rule_note"]], n_st[["stale"]]))
}

if (!is.null(cfg$memo_revision$before)) {
  add_check(paste0("memos: written before ", cfg$memo_revision$before, " are hidden as being revised (config memo_revision)"),
            "PASS", length(memo_revised), NA, if (length(memo_revised)) paste(memo_revised, collapse = " ") else "none")
}

# the synthetic fixture: a placeholder memo for the United States (built against until the memo phase)
memo_files <- list()
if (synthetic && !is.null(sheets[["USA"]])) {
  pm <- country_placeholder_memo(sheets[["USA"]], cfg)
  pj <- as.character(country_json(pm))
  pf <- sprintf("memo-USA.%s.json", md5_8(pj))
  memo_files[[pf]] <- pj
  for (k in seq_along(entries)) if (entries[[k]]$iso3 == "USA") {
    entries[[k]]$memo <- list(json = pf, pdf = NULL, docx = NULL, md = NULL, words = pm$words, generated_at = pm$generated_at, verified_at = pm$verified_at,
                              facts_sha256 = pm$facts_sha256, stale = FALSE)
  }
}

counts <- list(economies = length(entries), sheets = length(sheets), core = sum(roster$tier == "core"), partners = sum(roster$tier == "partner"),
               memos = sum(vapply(entries, function(e) !is.null(e$memo), TRUE)))
all_sources <- list()
for (s in sheets) for (src in s$sources) all_sources[[src$id]] <- src
meta$validated <- isTRUE(meta$validated) && n_fail == 0
manifest <- country_manifest(entries, meta, unname(all_sources), lapply(seq_len(nrow(validation)), function(i) as.list(validation[i, ])), counts)
mtxt <- as.character(country_json(manifest))
mprob <- country_check_manifest(jsonlite::fromJSON(mtxt, simplifyVector = FALSE))
add_check("manifest: country_check_manifest()", if (!length(mprob)) "PASS" else "FAIL", length(entries), NA, if (length(mprob)) paste(utils::head(mprob, 3), collapse = " | ") else NA)
add_check(sprintf("manifest: gzip size %s bytes <= %s", format(gz_size(mtxt), big.mark = ","), format(COUNTRY_BUDGET_GZ[["manifest"]], big.mark = ",")),
          if (gz_size(mtxt) <= COUNTRY_BUDGET_GZ[["manifest"]]) "PASS" else "WARN", 1)
validation <- do.call(rbind, val_rows)
n_fail <- sum(validation$status == "FAIL")

cat("\nValidation (", nrow(validation), " checks): ", sum(validation$status == "PASS"), " PASS, ", sum(validation$status == "WARN"), " WARN, ",
    n_fail, " FAIL\n", sep = "")
if (!synthetic) {
  vdir <- file.path(processed_dir, "dashboard", "country")
  if (!check_only) {
    dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(validation, file.path(vdir, "country_validation.csv"), row.names = FALSE)
  }
}
if (n_fail > 0) {
  print(validation[validation$status == "FAIL", c("check", "n", "max_abs_diff", "note")], row.names = FALSE)
  stop("Validation failed: nothing written.")
}
if (check_only) {
  step("--check-only: nothing written")
  quit(status = 0)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_text <- function(txt, path) {
  con <- file(path, "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
}
partial <- !is.null(only_iso)
for (f in names(files)) write_text(files[[f]], file.path(out_dir, f))
for (f in names(memo_files)) write_text(memo_files[[f]], file.path(out_dir, f))
if (partial && !is.null(old_manifest)) {
  # --iso3: replace those economies' entries in the existing manifest
  keep <- Filter(function(c) !c$iso3 %in% todo, old_manifest$countries)
  new <- Filter(function(c) c$iso3 %in% todo, entries)
  allc <- c(keep, new)
  allc <- allc[order(vapply(allc, function(c) c$name, ""))]
  manifest$countries <- allc
  manifest$counts$sheets <- sum(vapply(allc, function(c) !is.null(c$file), TRUE))
  mtxt <- as.character(country_json(manifest))
}
listed <- unlist(lapply(manifest$countries, function(c) c(c$file, c$drivers, c$trade_monthly, c$memo$json, c$memo$pdf, c$memo$docx, c$memo$md)))
old <- list.files(out_dir, pattern = "^(facts|drivers|trade|memo)-[A-Z0-9]{3}\\.[0-9a-f]{8}\\.(json|pdf|docx|md)$")
stale_files <- setdiff(old, listed)
if (length(stale_files)) unlink(file.path(out_dir, stale_files))
write_text(mtxt, file.path(out_dir, "manifest.json"))
step("Wrote ", length(files), " fact sheets, manifest (", format(nchar(mtxt), big.mark = ","), " bytes) to ", out_dir,
     if (length(stale_files)) paste0("; removed ", length(stale_files), " old files") else "")

if (synthetic && length(memo_files)) {
  # the placeholder memo's PDF, Word and Markdown files (the UI's download links are built against them);
  # optional: needs Edge (or Chrome) and Python with python-docx and pypdf
  source(file.path(repo_root, "scripts", "utils", "country_memos.R"))
  st <- tryCatch(country_render_memos_main(c("--placeholder", "--out-dir", out_dir, args), repo_root, cfg, dict),
                 error = function(e) { message("  WARN placeholder memo files not rendered: ", conditionMessage(e)); 1L })
  if (!identical(st, 0L)) message("  WARN placeholder memo files not rendered (the memo JSON is written)")
}

if (!is.null(facts_dir)) {
  fd <- abs_path(facts_dir)
  dir.create(fd, recursive = TRUE, showWarnings = FALSE)
  core_iso <- intersect(roster$iso3[roster$tier == "core"], names(sheets))
  for (iso in core_iso) {
    full <- if (!is.null(drv_texts[[iso]])) country_merge_drivers(sheets[[iso]], jsonlite::fromJSON(drv_texts[[iso]], simplifyVector = FALSE)) else sheets[[iso]]
    write_text(as.character(country_json(full, pretty = TRUE)), file.path(fd, paste0(iso, ".json")))
  }
  step("Wrote ", length(core_iso), " core fact sheets (pretty) to ", fd)
}
step("Done in ", format(round(difftime(Sys.time(), t_script, units = "mins"), 1)))
