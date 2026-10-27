# The web bundle of the Allied Industrial Base Explorer (dashboard/, a static React app): the R
# bundle split into the chunks the browser loads, and manifest.json (pure; no IO).
# scripts/85_export_dashboard_data.R encodes each chunk with dash_osb_encode(), gzips it into
# dashboard/public/data/<name>.<md5_8>.osb and writes the manifest; scripts/86 writes the test
# fixtures from the same chunks. The browser never sees R objects: only these chunks, so their
# paths and column names are the contract with the TypeScript engine (spec_react.md 4.3).
#
#   core     (initial)  dims; L1-L3 inputs (pillar category matrices, policy values and component
#                       order, SI inputs incl. the endowment screen's resource_base table);
#                       the exact baseline L1-L3 results; the baseline partner-level PSI
#   vars     (lazy)     pillar variable rows and the baseline category contributions
#   partner  (lazy)     Friendshore / Opportunity components (lossless; Friendshore's CM-FDI parts:
#                       outbound_source by cell, the CM-FDI investment ties outbound_cmfdi, the
#                       Chinese-parent ownership cpe), masks, CPI / GHG, Development, PSI reporters,
#                       alignment; baseline best-five matrices and the baseline PSI dyads
#
# The network chunk (the original allied network design and its greedy baseline) is retired
# (2026-10-17): the Allied Industrial Base analysis (docs/aib_methodology.md) replaces it.
#
# JSON conventions (json nodes and manifest.json; dash_json_value()): named vectors are objects,
# character vectors are always arrays, numbers / logicals of length 1 are scalars, NA is null,
# doubles have 17 significant digits (exact).

DASH_WEB_SCHEMA <- "osi-dashboard"
DASH_WEB_SCHEMA_VERSION <- 1L
DASH_WEB_LIMIT_INITIAL_GZ <- 3 * 2^20
DASH_WEB_LIMIT_ALL_GZ <- 25 * 2^20
DASH_WEB_CHUNKS <- c("core", "vars", "partner")

# params (a dash_params object) as the JSON structure manifest.json and the fixtures carry; the
# TypeScript makeParams() must return a deep-equal object.
dash_params_json <- function(params) dash_json_value(unclass(params))

# The dial grid: m(d) = 4^((d - 50) / 50) and the populism stage weights for every integer dial
# position, computed by R (so the browser never calls Math.pow for a dial).
dash_dial_grid <- function(bundle) {
  bi <- dash_baseline_inputs(bundle)
  d <- 0:100
  m <- dash_dial_multiplier(d)
  sw0 <- bi$stage_weights
  sw <- lapply(names(sw0), function(s) vapply(m, function(mp) dash_stage_weights(sw0, mp)[[s]], numeric(1)))
  names(sw) <- names(sw0)
  list(d = d, m = m, stage_weights = sw)
}

# The three chunks as nested lists (dash_osb_encode() input). baseline_result = dash_run() at the
# baseline (with the partner layer).
dash_web_chunks <- function(bundle, baseline_result) {
  d <- bundle$dims
  pa <- bundle$partner
  res <- baseline_result
  if (is.null(res$partner)) stop("dash_web_chunks(): the baseline result needs the partner layer (dash_run(partner = TRUE)).")
  pillar_core <- function(pb) pb[c("cells", "categories", "score", "present", "imputed", "missing_rule", "cw_imp", "cw_tot",
                                   "n_var", "categories_in_data", "allow_partial_categories")]
  core <- list(
    dims = list(
      countries = d$countries, nodes = d$nodes, techs = d$techs, stages = d$stages, fossil_techs = d$fossil_techs,
      es_categories = d$es_categories, eo_categories = d$eo_categories, cells = d$cells,
      U = pa$U, U_names = pa$names, U_sovereign = pa$sovereign
    ),
    pillars = list(es = pillar_core(bundle$pillars$es), eo = pillar_core(bundle$pillars$eo)),
    # required: the pipeline's required Policy components, only when a strict subset; components: the
    # pipeline's component order, for the policy_basis labels (dash_policy())
    policy = bundle$policy[intersect(c("cells", "variables", "values", "required", "components"), names(bundle$policy))],
    # resource_base: the endowment screen of the allied picks (dash_si()), when the build has it
    si = bundle$si[intersect(c("tech_ghg", "trl", "allies", "resource_base"), names(bundle$si))],
    baseline = list(
      core = list(
        es = res$pillars$es,
        eo = res$pillars$eo,
        policy = list(index = res$policy$index, eligibility = res$policy$eligibility, weights = dash_osb_json(res$policy$weights)),
        si = list(cells = res$si$cells, excluded = res$si$excluded, degenerate = res$si$degenerate,
                  weights = dash_osb_json(res$si$weights), stage_weights = dash_osb_json(res$si$stage_weights)),
        allied = res$allied
      ),
      psi_partner = res$partner$psi$partner,
      psi_view = res$partner$psi$view,
      psi_reporters = res$partner$psi$reporters
    )
  )
  vars <- list(
    pillars = list(es = list(var = bundle$pillars$es$var), eo = list(var = bundle$pillars$eo$var)),
    baseline = list(contrib = list(es = res$pillars$es_contrib, eo = res$pillars$eo_contrib))
  )
  partner <- list(
    partner = list(
      fs = pa$fs[intersect(c("imp_trade", "econ_opp_raw", "es_need", "eo_partner", "outbound", "outbound_source", "outbound_cmfdi",
                             "cpe", "geo", "mask", "cpi", "gh"), names(pa$fs))],
      po = pa$po[c("trade", "econ_opp_index", "partner_es_need", "mask", "cpi", "gh")],
      dp = pa$dp,
      top5_share = pa$top5_share,
      psi = list(coalition_reporters = as.character(pa$psi$coalition_reporters), tau = as.numeric(pa$psi$tau)),
      geo_dyad = pa$geo_dyad
    ),
    baseline = list(partner = list(
      top5_fs = res$partner$top5$fs, top5_po = res$partner$top5$po,
      fs_exists = res$partner$top5$fs_exists, po_exists = res$partner$top5$po_exists,
      psi_dyad = res$partner$psi$dyad
    ))
  )
  list(core = core, vars = vars, partner = partner)
}

# Chunk file name: <name>.<first 8 hex digits of the gzip file's md5>.osb
dash_web_chunk_file <- function(name, md5) paste0(name, ".", substr(md5, 1, 8), ".osb")

# Budget figures from the chunk table (name, bytes_gz, initial) and the manifest's gzip size.
dash_web_budget <- function(chunk_info, manifest_gz) {
  initial <- manifest_gz + sum(chunk_info$bytes_gz[chunk_info$initial])
  list(
    initial_gz = initial,
    all_gz = sum(chunk_info$bytes_gz),
    limit_initial_gz = DASH_WEB_LIMIT_INITIAL_GZ,
    limit_all_gz = DASH_WEB_LIMIT_ALL_GZ,
    initial_files = I(c("manifest.json", chunk_info$file[chunk_info$initial])),
    note = "initial = manifest.json + the initial chunks; npm run budget adds the app's JS, CSS and fonts"
  )
}

# manifest.json as a list (serialise with dash_json_text()). chunk_info: name, file, bytes,
# bytes_gz, md5, initial. created: POSIXct. paper: list(title, url, pdf).
dash_web_manifest <- function(bundle, chunk_info, budget, bundle_id, created, paper = NULL) {
  m <- bundle$manifest
  rows <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(list())
    tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
    for (nm in names(tbl)) if (inherits(tbl[[nm]], c("POSIXct", "POSIXlt", "Date"))) tbl[[nm]] <- dash_osb_format_time(tbl[[nm]])
    lapply(seq_len(nrow(tbl)), function(i) lapply(as.list(tbl[i, , drop = FALSE]), function(v) v[[1]]))
  }
  list(
    schema = DASH_WEB_SCHEMA,
    schema_version = DASH_WEB_SCHEMA_VERSION,
    bundle_id = bundle_id,
    created = dash_osb_format_time(created),
    git_commit = if (is.null(m$git_commit)) NA_character_ else m$git_commit,
    osi_branch = if (is.null(m$osi_branch)) NA_character_ else m$osi_branch,
    generator = "scripts/85_export_dashboard_data.R",
    r = list(version = R.version.string, long_double = isTRUE(capabilities("long.double"))),
    flags = m$flags,
    notes = I(as.character(m$notes)),
    validation = rows(m$validation),
    sources = rows(m$sources),
    sources_params = m$sources_params,
    baseline_inputs = dash_json_value(m$baseline_inputs),
    baseline_params = dash_params_json(m$baseline),
    dial_grid = dash_dial_grid(bundle),
    chunks = rows(chunk_info),
    budget = budget,
    encoding = list(lossy = FALSE, objects = I(character()),
                    note = "every double is stored losslessly (f64, dict16 or const); FS / PO input tolerance 0"),
    paper = if (is.null(paper)) list(title = NA_character_, url = NA_character_, pdf = NA_character_) else paper
  )
}
