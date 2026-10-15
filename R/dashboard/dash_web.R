# The web bundle of the Allied Industrial Base Explorer (dashboard/, a static React app): the R
# bundle split into the chunks the browser loads, and manifest.json (pure; no IO).
# scripts/85_export_dashboard_data.R encodes each chunk with dash_osb_encode(), gzips it into
# dashboard/public/data/<name>.<md5_8>.osb and writes the manifest; scripts/86 writes the test
# fixtures from the same chunks. The browser never sees R objects: only these chunks, so their
# paths and column names are the contract with the TypeScript engine (spec_react.md 4.3).
#
#   core     (initial)  dims; L1-L3 inputs (pillar category matrices, policy values and component
#                       order, SI inputs incl. the endowment screen's resource_base table);
#                       the exact baseline L1-L3 results; the baseline partner-level PSI and a
#                       summary of the baseline network
#   vars     (lazy)     pillar variable rows and the baseline category contributions
#   partner  (lazy)     Friendshore / Opportunity components (lossless; Friendshore's CM-FDI parts:
#                       outbound_source by cell, the CM-FDI investment ties outbound_cmfdi, the
#                       Chinese-parent ownership cpe), masks, CPI / GHG, Development, PSI reporters,
#                       alignment; baseline best-five matrices and the baseline PSI dyads
#   network  (lazy)     members, member-level node inputs, member weights / node sizes / portfolio
#                       caps (precomputed: not dials), basket trade of the bundled node(s), the
#                       stored scripts/30 design (when it matches the bundle) and the greedy baseline
#
# JSON conventions (json nodes and manifest.json; dash_json_value()): named vectors are objects,
# character vectors are always arrays, numbers / logicals of length 1 are scalars, NA is null,
# doubles have 17 significant digits (exact).

DASH_WEB_SCHEMA <- "osi-dashboard"
DASH_WEB_SCHEMA_VERSION <- 1L
DASH_WEB_LIMIT_INITIAL_GZ <- 3 * 2^20
DASH_WEB_LIMIT_ALL_GZ <- 25 * 2^20
DASH_WEB_SHARE_RANK_DIGITS <- 9L
DASH_WEB_CHUNKS <- c("core", "vars", "partner", "network")

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

# The network tables that do not depend on any dial, precomputed for every solvable stage (the
# stages of dash_network_stages() without fossil exclusion): member weights, node sizes and the
# portfolio caps (scripts/30's settings; not a dial). The engine computes the same values; the
# browser reads them.
dash_network_fixed_tables <- function(bundle) {
  nb <- bundle$network
  params <- bundle$manifest$baseline
  if (is.null(params)) params <- dash_params(bundle = bundle)
  params$network$exclude_techs <- character()
  members <- dash_network_members(bundle)
  stages <- dash_network_stages(bundle, params)
  aggregate <- allied_network_aggregates(members)
  member_weights <- allied_network_member_weights(members, if (aggregate) nb$node_member_weights else NULL, stages)
  node_size <- allied_network_node_size_tbl(nb$country_size, members)
  nodes_vec <- if (aggregate) allied_network_node_table(members)$network_node else members$iso3c
  size_tbl <- if (aggregate) node_size else nb$country_size
  pf <- params$network$portfolio
  cap_by_iso <- allied_network_compute_portfolio_caps(
    country_size_tbl = size_tbl, iso3c_network = nodes_vec,
    min_cap = pf$min_cap, max_cap = pf$max_cap, transform = pf$transform, clamp = TRUE
  )
  st <- tibble::as_tibble(size_tbl)
  caps <- tibble::tibble(
    iso3c = nodes_vec,
    portfolio_cap = as.integer(cap_by_iso[nodes_vec]),
    gdp_usd = as.numeric(st$gdp_usd[match(nodes_vec, st$iso3c)]),
    gdp_imputed = if ("gdp_imputed" %in% names(st)) st$gdp_imputed[match(nodes_vec, st$iso3c)] %in% TRUE else FALSE
  )
  list(member_weights = tibble::as_tibble(member_weights), node_size = tibble::as_tibble(node_size), caps = caps, stages = stages)
}

# Member basket trade restricted to the rows the edge aggregation can read: a member of a bundled
# (multi-member) node on at least one side, the two sides in different nodes, and a solvable stage.
# allied_network_aggregate_edge_component() reads trade only for such pairs, so the edges are
# unchanged (scripts/85 checks it).
dash_network_trim_trade <- function(node_trade, members, stages) {
  if (is.null(node_trade) || !nrow(node_trade)) return(node_trade)
  node_of <- stats::setNames(members$network_node, members$iso3c)
  sizes <- table(members$network_node)
  multi <- names(sizes)[sizes > 1]
  ne <- unname(node_of[node_trade$exporter_iso])
  ni <- unname(node_of[node_trade$importer_iso])
  keep <- !is.na(ne) & !is.na(ni) & ne != ni & (ne %in% multi | ni %in% multi) &
    paste(node_trade$tech, node_trade$supply_chain, sep = "\r") %in% paste(stages$tech, stages$supply_chain, sep = "\r")
  tibble::as_tibble(node_trade[keep, , drop = FALSE])
}

# The network shown at the baseline: the stored scripts/30 design when it was solved on the bundle's
# inputs (flags$network_consistent), else the greedy baseline.
dash_network_baseline_design <- function(bundle) {
  nb <- bundle$network
  fl <- bundle$manifest$flags
  if (isTRUE(fl$network_consistent) && !is.null(nb$stored$specialization)) {
    method <- unique(nb$stored$diagnostics$method)
    return(list(source = if (length(method) == 1 && identical(method, "milp")) "milp" else "stored",
                specialization = nb$stored$specialization, diagnostics = nb$stored$diagnostics))
  }
  if (!is.null(nb$greedy_baseline)) {
    return(list(source = "greedy", specialization = nb$greedy_baseline$specialization, diagnostics = nb$greedy_baseline$diagnostics))
  }
  NULL
}

dash_web_network_summary <- function(design) {
  if (is.null(design)) return(NULL)
  s <- design$specialization
  d <- design$diagnostics
  col <- function(x, nm, fill) if (nm %in% names(x)) x[[nm]] else rep(fill, nrow(x))
  list(
    source = design$source,
    specialization = tibble::tibble(
      tech = s$tech, supply_chain = s$supply_chain, network_node = s$iso3c,
      selected = as.logical(col(s, "selected", NA)), production_share = as.numeric(s$production_share),
      producer_score = as.numeric(col(s, "producer_score", NA_real_))
    ),
    diagnostics = tibble::tibble(
      tech = d$tech, supply_chain = d$supply_chain, method = as.character(col(d, "method", NA_character_)),
      n_producers = as.integer(col(d, "n_producers", NA_integer_)), hhi = as.numeric(col(d, "hhi", NA_real_)),
      effective_producers = 1 / as.numeric(col(d, "hhi", NA_real_))
    )
  )
}

# The four chunks as nested lists (dash_osb_encode() input). baseline_result = dash_run() at the
# baseline (with the partner layer); network_baseline = dash_network_baseline_design(bundle) unless
# given.
dash_web_chunks <- function(bundle, baseline_result, network_baseline = NULL) {
  d <- bundle$dims
  pa <- bundle$partner
  nb <- bundle$network
  res <- baseline_result
  if (is.null(res$partner)) stop("dash_web_chunks(): the baseline result needs the partner layer (dash_run(partner = TRUE)).")
  if (is.null(network_baseline)) network_baseline <- dash_network_baseline_design(bundle)
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
      psi_reporters = res$partner$psi$reporters,
      network_summary = dash_web_network_summary(network_baseline)
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
  fixed <- if (!is.null(nb$member_weights) && !is.null(nb$node_size) && !is.null(nb$caps)) nb else dash_network_fixed_tables(bundle)
  gb <- nb$greedy_baseline
  greedy <- NULL
  if (!is.null(gb)) {
    greedy <- gb[intersect(c("specialization", "flows", "diagnostics", "member_shares", "solver_by_stage", "stages"), names(gb))]
    if (!is.null(gb$portfolio)) greedy$portfolio <- gb$portfolio[c("caps", "counts", "topk", "converged", "iters", "violations_remaining")]
    greedy$forbidden <- dash_osb_json(gb$forbidden)
    greedy$share_rank_digits <- if (is.null(gb$share_rank_digits)) NA_integer_ else as.integer(gb$share_rank_digits)
  }
  stored <- NULL
  if (isTRUE(bundle$manifest$flags$network_consistent) && !is.null(nb$stored)) {
    stored <- nb$stored[intersect(c("specialization", "flows", "diagnostics", "caps", "counts", "topk", "member_shares"), names(nb$stored))]
  }
  network <- list(
    network = list(
      members = dash_network_members(bundle),
      node_raw_member = nb$node_raw_member,
      member_weights = fixed$member_weights,
      node_size = fixed$node_size,
      caps = fixed$caps,
      country_size = nb$country_size,
      node_trade = nb$node_trade,
      node_labels = dash_osb_json(nb$node_labels),
      country_info = nb$country_info,
      country_map = nb$country_map,
      stored = stored,
      greedy_baseline = greedy,
      observed_hhi = nb$observed_hhi
    )
  )
  list(core = core, vars = vars, partner = partner, network = network)
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
    share_rank_digits = DASH_WEB_SHARE_RANK_DIGITS,
    dial_grid = dash_dial_grid(bundle),
    chunks = rows(chunk_info),
    budget = budget,
    encoding = list(lossy = FALSE, objects = I(character()),
                    note = "every double is stored losslessly (f64, dict16 or const); FS / PO input tolerance 0"),
    paper = if (is.null(paper)) list(title = NA_character_, url = NA_character_, pdf = NA_character_) else paper
  )
}
