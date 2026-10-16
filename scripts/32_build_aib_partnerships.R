# Allied Industrial Base (AIB), step 2: stress tests, the Trade Partnership Network, the
# diversification-specialization frontier, Investment Partnership Prioritization, the emergent AIB
# network and Sectoral Compact formation (docs/aib_methodology.md; config/aib.yml; R/aib/).
#
# Usage:
#   Rscript scripts/32_build_aib_partnerships.R [--out-dir DIR] [--nodes id1,id2] [--skip-investment] [--universe U]
#
#   --out-dir          where scripts/31 wrote aib_inputs.rds and where the outputs go
#                      (default: <outputs_dir>/aib)
#   --nodes            run only these node ids (validation runs); the compacts then cover them only
#   --skip-investment  stop after the trade model (no investment, network or compacts)
#   --universe U       override config/aib.yml universe: global, coalition or ISO3,ISO3,...
#   --from-checkpoint  reuse the last run's trade model and investment simulation (same out dir)
#
# Checkpoints aib_checkpoint_trade.rds and aib_checkpoint_investment.rds are written for
# --from-checkpoint. Writes (CSV, one row per key; every table carries scenario and shock columns):
#   allied_trade_stress_test.csv       node / stage / technology x shock x scenario: exposure,
#                                      static, existing-network and optimized coverage, dividend,
#                                      residual gap, HHI and largest supplier (baseline, optimized)
#   allied_trade_partnerships.csv      optimized new / expanded relationships (headline limit)
#   allied_trade_frontier.csv          coverage and concentration at every supplier-share limit,
#                                      with each node's minimum sufficient diversification
#   allied_investment_partnerships.csv marginal capacity simulation, one row per host x increment
#   allied_investment_pairs.csv        investor (importer served) -> host rows with Friendshore
#   emergent_aib_edges.csv             directed trade and investment edges aggregated over nodes
#   emergent_aib_country_summary.csv   each economy's roles in the network (no single ranking)
#   aib_node_classification.csv        every node's policy class and its reasons
#   aib_compact_candidates.csv         candidate Sectoral Compacts
#   aib_compact_nodes.csv              node table per compact (the Gates & Fences handoff)
#   aib_compact_members.csv            member economies and their model-derived roles
#   aib_compact_coalition_path.csv     coverage as members are added (greedy minimum coalition)
#   allied_subsector_data_quality.csv  data-quality flags per sub-sector
#   aib_validation_cases.csv / .md     the hand-checked validation chains
#   aib_results.rds                    everything above in one list (for scripts/84)

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
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
if (!aib_has_lp()) stop("The AIB optimizer needs the Rglpk and slam packages (GLPK).")

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(repo_root, config$processed_dir, "outputs")
out_dir <- abs_path(arg_value("--out-dir", file.path(outputs_dir, "aib")))
inputs_path <- file.path(out_dir, "aib_inputs.rds")
if (!file.exists(inputs_path)) stop("Run scripts/31_build_aib_trade_network.R first: ", inputs_path, " not found.")
inputs <- readRDS(inputs_path)
cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))
opt <- cfg$optimization
tolerances <- c(opt$tolerance_coverage, opt$tolerance_specialization, opt$tolerance_opportunity)
neutral <- as.numeric(opt$neutral_score)
caps <- as.numeric(unlist(opt$max_supplier_share))
headline_cap <- as.numeric(opt$headline_max_supplier_share)
scenarios <- unlist(cfg$scenarios)
headline_shock <- cfg$headline_shock
headline_scenario <- cfg$headline_scenario
components <- unlist(cfg$compatibility$components)
write_out <- function(tbl, name) {
  utils::write.csv(tbl, file.path(out_dir, name), row.names = FALSE, na = "")
  step("  wrote ", name, " (", format(NROW(tbl), big.mark = ","), " rows)")
}

# ---- Universe -----------------------------------------------------------------------------------
universe_setting <- if (!is.null(arg_value("--universe"))) arg_value("--universe") else cfg$universe
if (is.character(universe_setting) && length(universe_setting) == 1 && grepl(",", universe_setting)) {
  universe_setting <- strsplit(universe_setting, ",", fixed = TRUE)[[1]]
}
universe <- if (identical(universe_setting, "global")) {
  NULL
} else if (identical(universe_setting, "coalition")) {
  if (is.null(inputs$coalition)) stop("universe: coalition needs data/processed/coalition_tbl.rds.")
  inputs$coalition$iso3[inputs$coalition$tier %in% c("core", "partner")]
} else {
  toupper(unlist(universe_setting))
}
universe_id <- if (is.character(universe_setting) && length(universe_setting) == 1) universe_setting else "custom"
# Open ship registries record vessel transfers as trade; they are never part of the universe
# (neither demand to restore nor a partner), and their shipments stay as they are.
registry_exclusions <- toupper(unlist(cfg$trade$exclude_from_universe))
if (length(registry_exclusions)) {
  all_econ <- unique(c(inputs$flows$exporter_iso, inputs$flows$importer_iso))
  universe <- setdiff(if (is.null(universe)) all_econ else universe, registry_exclusions)
}
step("AIB partnerships: universe ", universe_id, if (!is.null(universe)) paste0(" (", length(universe), " economies)") else " (every economy)")

nodes <- inputs$nodes
quality <- inputs$quality
opt_nodes <- quality$node_id[quality$optimize]
only <- arg_value("--nodes")
if (!is.null(only)) opt_nodes <- intersect(opt_nodes, strsplit(only, ",", fixed = TRUE)[[1]])
flows_by_node <- split(inputs$flows[inputs$flows$value > 0, c("node_id", "exporter_iso", "importer_iso", "value")],
                       inputs$flows$node_id[inputs$flows$value > 0])

targets <- aib_shock_targets(cfg$shocks, inputs$node_cells, inputs$exporter_metrics, cfg$trade$reexport_hubs)
targets <- targets[targets$node_id %in% opt_nodes, , drop = FALSE]
runs <- targets %>% dplyr::distinct(node_id, shock_key) %>% dplyr::arrange(node_id, shock_key)
headline_keys <- targets %>% dplyr::filter(shock_id == headline_shock) %>% dplyr::distinct(node_id, shock_key)
step("Trade model: ", length(opt_nodes), " nodes, ", length(cfg$shocks), " shocks, ", nrow(runs), " distinct node-shock runs x ",
     length(scenarios), " scenarios x (existing + ", length(unique(c(caps, headline_cap))), " optimized limits)")

node_scores <- function(nid) {
  n <- nodes[nodes$node_id == nid, ]
  aib_node_scores(nid, strsplit(n$techs, "; ")[[1]], n$supply_chain, inputs$exporter_metrics, inputs$op_index,
                  inputs$pams, inputs$cat_scores, rca_cap = opt$rca_cap, neutral = neutral, components = components)
}
node_problem <- function(nid, key) {
  aib_node_problem(flows_by_node[[nid]], aib_parse_shock_key(key), universe, cfg$shocked_demand,
                   as.numeric(cfg$trade$candidate_min_exports_usd), as.numeric(cfg$trade$candidate_min_world_share),
                   allow_domestic = isTRUE(opt$allow_domestic_retention))
}

# ---- Trade model: stress test, partnerships, frontier -----------------------------------------------
# --from-checkpoint reuses the trade model (and, when present, the investment simulation) of the last
# run in this out dir; use it only when nothing upstream of those stages changed.
trade_ckpt <- file.path(out_dir, "aib_checkpoint_trade.rds")
inv_ckpt <- file.path(out_dir, "aib_checkpoint_investment.rds")
use_ckpt <- arg_flag("--from-checkpoint") && file.exists(trade_ckpt)
if (use_ckpt) {
  ck <- readRDS(trade_ckpt)
  for (nm in names(ck)) assign(nm, ck[[nm]])
  step("Trade model from checkpoint ", trade_ckpt)
} else {
res_summary <- list(); res_frontier <- list(); res_partners <- list(); res_flows <- list(); res_flows_ex <- list(); res_supply <- list()
headline_problems <- list()
importer_loss <- list()
cur_node <- ""
sc_obj <- NULL
t_trade <- Sys.time()
for (k in seq_len(nrow(runs))) {
  nid <- runs$node_id[k]
  key <- runs$shock_key[k]
  if (!identical(nid, cur_node)) {
    sc_obj <- node_scores(nid)
    cur_node <- nid
  }
  prob <- node_problem(nid, key)
  if (is.null(prob)) next
  r <- aib_run_node_shock(prob, scenarios, caps, headline_cap, sc_obj, tolerances, neutral,
                          existing_growth = as.numeric(aib_or(opt$existing_relationship_growth, Inf)),
                          tolerance_stability = as.numeric(aib_or(opt$tolerance_stability, 1e-6)))
  tag <- function(t) if (is.null(t) || !nrow(t)) NULL else dplyr::mutate(t, node_id = nid, shock_key = key, .before = 1)
  res_summary[[k]] <- tag(r$summary)
  res_frontier[[k]] <- tag(r$frontier)
  res_supply[[k]] <- tag(r$supply)
  if (!is.null(r$partnerships) && nrow(r$partnerships)) {
    p <- r$partnerships
    p$export_opportunity <- sc_obj$op_raw(p$exporter_iso, p$importer_iso)
    p$compatibility <- sc_obj$compat(p$exporter_iso, p$importer_iso)
    res_partners[[k]] <- tag(p)
  }
  if (any(headline_keys$node_id == nid & headline_keys$shock_key == key)) {
    headline_problems[[nid]] <- prob
    res_flows[[nid]] <- tag(r$flows)
    res_flows_ex[[nid]] <- tag(r$flows_existing)
    importer_loss[[nid]] <- tibble::tibble(node_id = nid, importer_iso = names(prob$lost), lost_share = as.numeric(prob$lost),
                                           demand_share = as.numeric(prob$demand), lost_usd = as.numeric(prob$lost) * prob$D)
  }
  if (k %% 50 == 0) step("  ", k, "/", nrow(runs), " runs (", round(as.numeric(difftime(Sys.time(), t_trade, units = "mins")), 1), " min)")
}
summary_runs <- dplyr::bind_rows(res_summary)
frontier_runs <- dplyr::bind_rows(res_frontier)
partners_runs <- dplyr::bind_rows(res_partners)
flows_headline <- dplyr::bind_rows(res_flows)
flows_existing_headline <- dplyr::bind_rows(res_flows_ex)
supply_runs <- dplyr::bind_rows(res_supply)
importer_loss <- dplyr::bind_rows(importer_loss)
step("Trade model done in ", round(as.numeric(difftime(Sys.time(), t_trade, units = "mins")), 1), " min; ",
     sum(summary_runs$status_optimized != 0 | summary_runs$status_existing != 0), " runs with a solver problem")
saveRDS(list(summary_runs = summary_runs, frontier_runs = frontier_runs, partners_runs = partners_runs, flows_headline = flows_headline,
             flows_existing_headline = flows_existing_headline, supply_runs = supply_runs, importer_loss = importer_loss,
             headline_problems = headline_problems), trade_ckpt)
}

# Node rows of the hierarchy (a node shared by several technologies appears under each).
node_label <- nodes %>% dplyr::select(node_id, node_label = sub_sector, techs, node_stage = supply_chain)
if (!"aggregated_into" %in% names(quality)) quality$aggregated_into <- NA_character_
qual_cols <- quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, data_quality)
ns <- inputs$node_summary %>% dplyr::select(node_id, world_trade, top_exporter, top_exporter_share, china_export_share, export_hhi)
node_rows <- targets %>%
  dplyr::inner_join(summary_runs, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(qual_cols, by = "node_id") %>%
  dplyr::left_join(ns, by = "node_id")
supply_lv <- supply_runs %>%
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, tech, supply_chain), by = c("node_id", "shock_key"), relationship = "many-to-many")
conc <- dplyr::bind_rows(
  aib_aggregate_concentration(supply_lv, c("tech", "supply_chain")) %>% dplyr::mutate(level = "stage"),
  aib_aggregate_concentration(supply_lv, "tech") %>% dplyr::mutate(level = "technology")
)
stress <- dplyr::bind_rows(
  node_rows %>% dplyr::mutate(level = "subsector", n_nodes = 1L),
  aib_aggregate_levels(node_rows) %>%
    dplyr::left_join(conc, by = c("level", "tech", "supply_chain", "shock_id", "scenario")) %>%
    dplyr::left_join(dplyr::distinct(targets, shock_id, shock_label, shock_type, shock_level, shock_fraction), by = "shock_id")
) %>%
  dplyr::mutate(universe = universe_id) %>%
  dplyr::relocate(level, universe, tech, supply_chain, sub_sector, node_id, shock_id, shock_label, shock_level, target_iso, scenario)

# Frontier with each node's minimum sufficient diversification, plus stage/technology aggregates.
# A node shared by technologies whose largest supplier differs has one run per target (shock_key);
# every join below keeps them apart.
frontier_nodes <- frontier_runs %>%
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(summary_runs %>% dplyr::select(node_id, shock_key, scenario, demand_usd, shock_exposure), by = c("node_id", "shock_key", "scenario"))
fkeys <- c("node_id", "shock_key", "shock_id", "scenario")
fmin <- aib_frontier_minimum(frontier_nodes, opt$frontier_coverage_tolerance, aib_or(opt$frontier_acceptable_coverage, 0.8), keys = fkeys)
frontier_out <- dplyr::bind_rows(
  frontier_nodes %>% dplyr::left_join(fmin, by = fkeys) %>%
    dplyr::left_join(node_label, by = "node_id") %>% dplyr::mutate(level = "subsector"),
  frontier_nodes %>%
    dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, tech, supply_chain), by = c("node_id", "shock_key", "shock_id"), relationship = "many-to-many") %>%
    dplyr::group_by(tech, supply_chain, shock_id, scenario, max_supplier_share) %>%
    dplyr::summarise(optimized_coverage = sum(optimized_coverage * demand_usd) / sum(demand_usd),
                     existing_coverage = sum(existing_coverage * demand_usd) / sum(demand_usd),
                     static_coverage = sum(static_coverage * demand_usd) / sum(demand_usd),
                     second_shock_coverage = sum(second_shock_coverage * demand_usd, na.rm = TRUE) / sum(demand_usd),
                     demand_usd = sum(demand_usd), n_nodes = dplyr::n_distinct(node_id), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain, shock_id, scenario) %>%
    dplyr::mutate(coverage_loss_vs_unconstrained = pmax(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage, 0)) %>%
    dplyr::ungroup() %>% dplyr::mutate(level = "stage")
) %>% dplyr::mutate(universe = universe_id) %>% dplyr::select(-dplyr::any_of("shock_key")) %>%
  dplyr::relocate(level, universe, node_id, node_label, tech, supply_chain, shock_id, scenario, max_supplier_share)

ex_cols <- inputs$exporter_metrics %>% dplyr::select(node_id, exporter_iso, exporter_rca = export_rca, exporter_world_share = world_share,
                                                     exporter_production_share = production_share)
partners_out <- partners_runs %>%
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, shock_label, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(ex_cols, by = c("node_id", "exporter_iso")) %>%
  dplyr::left_join(node_label, by = "node_id") %>%
  dplyr::mutate(universe = universe_id, max_supplier_share = headline_cap) %>%
  dplyr::select(-shock_key) %>%
  dplyr::relocate(universe, node_id, node_label, techs, node_stage, shock_id, shock_label, scenario, exporter_iso, importer_iso, relationship)

step("Writing trade outputs")
write_out(stress, "allied_trade_stress_test.csv")
write_out(partners_out, "allied_trade_partnerships.csv")
write_out(frontier_out, "allied_trade_frontier.csv")
dq_out <- quality %>% dplyr::left_join(nodes %>% dplyr::select(node_id, primary_tech), by = "node_id") %>%
  dplyr::relocate(node_id, sub_sector, supply_chain, techs, data_quality, optimize, flags)
write_out(dq_out, "allied_subsector_data_quality.csv")

if (arg_flag("--skip-investment")) {
  step("Stopped after the trade model (--skip-investment).")
  quit(save = "no")
}

# ---- Investment partnership prioritization ------------------------------------------------------
inv <- cfg$investment
inv_shocks <- intersect(unlist(inv$shocks), vapply(cfg$shocks, `[[`, "", "id"))
edges_sc <- inputs$supply_chain_edges
# Host capability through a linked node counts only along material links (feedstock -> product),
# not merely for sitting in the same stage of a technology.
material_edges <- edges_sc[grepl("material link", edges_sc$basis), , drop = FALSE]
inv_hosts <- list(); inv_pairs <- list()
inv_scenarios <- intersect(unlist(aib_or(inv$scenarios, headline_scenario)), scenarios)
t_inv <- Sys.time()
if (use_ckpt && file.exists(inv_ckpt)) {
  ck <- readRDS(inv_ckpt)
  inv_hosts <- list(ck$inv_hosts); inv_pairs <- list(ck$inv_pairs)
  inv_shocks_run <- character()
  step("Investment from checkpoint ", inv_ckpt)
} else {
  inv_shocks_run <- inv_shocks
}
for (inv_sc in inv_scenarios) for (shk in inv_shocks_run) {
  keys <- targets %>% dplyr::filter(shock_id == shk) %>% dplyr::distinct(node_id, shock_key)
  gap <- summary_runs %>% dplyr::filter(scenario == inv_sc) %>%
    dplyr::inner_join(keys, by = c("node_id", "shock_key")) %>%
    dplyr::filter(residual_gap >= inv$min_residual_gap)
  step("Investment (", shk, ", ", inv_sc, "): ", nrow(gap), " node runs with a residual gap of at least ", 100 * inv$min_residual_gap, "%")
  for (i in seq_len(nrow(gap))) {
    nid <- gap$node_id[i]
    key <- gap$shock_key[i]
    prob <- if (shk == headline_shock && !is.null(headline_problems[[nid]]) &&
                identical(key, headline_keys$shock_key[headline_keys$node_id == nid][1])) headline_problems[[nid]] else node_problem(nid, key)
    if (is.null(prob)) next
    hosts <- aib_eligible_hosts(nid, inputs$exporter_metrics, material_edges, exclude = c(names(prob$shocked), unlist(inv$exclude_hosts)), universe = universe,
                                min_rca = inv$host_min_rca, min_share = inv$host_min_world_share,
                                min_production_share = inv$host_min_production_share, rca_cap = opt$rca_cap,
                                min_exports_usd = inv$host_min_exports_usd, min_material_share = inv$host_min_material_share)
    if (!nrow(hosts)) next
    hosts <- hosts %>% dplyr::arrange(dplyr::desc(capability_tier), dplyr::desc(capability_value)) %>% utils::head(inv$max_hosts_per_node)
    sc_obj <- node_scores(nid)
    fs_env <- aib_node_dyad_lookup(inputs$fs_index, strsplit(nodes$techs[nodes$node_id == nid], "; ")[[1]], nodes$supply_chain[nodes$node_id == nid])
    fs_lookup <- function(a, b) aib_lookup_pairs_raw(fs_env, a, b)
    r <- aib_investment_node(prob, inv_sc, hosts, as.numeric(unlist(inv$increments)), as.numeric(inv$headline_increment),
                             headline_cap, as.numeric(inv$diversified_max_supplier_share), sc_obj, fs_lookup, tolerances, neutral,
                             pair_hosts = inv$top_hosts, tie_resilience = inv$tie_resilience, tie_hhi = inv$tie_hhi,
                             base_flows = if (shk == headline_shock && !is.null(res_flows[[nid]])) {
                               bf <- res_flows[[nid]]
                               bf[bf$scenario == inv_sc, c("e", "m", "f", "base"), drop = FALSE]
                             } else NULL)
    if (!is.null(r$hosts)) inv_hosts[[length(inv_hosts) + 1]] <- dplyr::mutate(r$hosts, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, .before = 1)
    if (!is.null(r$pairs) && nrow(r$pairs)) inv_pairs[[length(inv_pairs) + 1]] <- dplyr::mutate(r$pairs, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, .before = 1)
    if (i %% 10 == 0) step("  ", i, "/", nrow(gap), " nodes (", round(as.numeric(difftime(Sys.time(), t_inv, units = "mins")), 1), " min)")
  }
}
inv_hosts <- dplyr::bind_rows(inv_hosts)
inv_pairs <- dplyr::bind_rows(inv_pairs)
if (length(inv_shocks_run)) saveRDS(list(inv_hosts = inv_hosts, inv_pairs = inv_pairs), inv_ckpt)
step("Investment done in ", round(as.numeric(difftime(Sys.time(), t_inv, units = "mins")), 1), " min: ",
     dplyr::n_distinct(paste(inv_hosts$node_id, inv_hosts$shock_key, inv_hosts$scenario)), " node-shock-scenario runs, ", nrow(inv_hosts), " host x increment rows")
if (nrow(inv_hosts)) {
  top_inv <- inv_pairs %>% dplyr::group_by(node_id, shock_key, shock_id, scenario, host_iso) %>%
    dplyr::arrange(dplyr::desc(supply_from_new_capacity), .by_group = TRUE) %>%
    dplyr::summarise(top_investor_partners = paste(utils::head(sprintf("%s (FS %s)", investor_iso, ifelse(is.na(friendshore), "n/a", sprintf("%.2f", friendshore))), 3), collapse = "; "), .groups = "drop")
  inv_out <- inv_hosts %>% dplyr::left_join(top_inv, by = c("node_id", "shock_key", "shock_id", "scenario", "host_iso")) %>%
    dplyr::left_join(dplyr::distinct(targets, node_id, shock_key, target_iso), by = c("node_id", "shock_key")) %>%
    dplyr::left_join(node_label, by = "node_id") %>%
    dplyr::mutate(universe = universe_id, analysis = "marginal capacity simulation (synthetic increment, not a project)") %>%
    dplyr::select(-shock_key) %>%
    dplyr::relocate(universe, node_id, node_label, techs, node_stage, shock_id, target_iso, scenario, increment, host_rank, host_iso)
} else {
  inv_out <- inv_hosts
}
write_out(inv_out, "allied_investment_partnerships.csv")
inv_pairs_out <- if (nrow(inv_pairs)) inv_pairs %>%
  dplyr::left_join(inv_hosts %>% dplyr::filter(abs(increment - inv$headline_increment) < 1e-9) %>%
                     dplyr::select(node_id, shock_id, shock_key, scenario, host_iso, host_rank),
                   by = c("node_id", "shock_id", "shock_key", "scenario", "host_iso"), relationship = "many-to-one") else inv_pairs
write_out(inv_pairs_out %>% dplyr::left_join(node_label, by = "node_id") %>% dplyr::select(-dplyr::any_of("shock_key")), "allied_investment_pairs.csv")

# ---- Emergent AIB network -------------------------------------------------------------------------
node_shock <- summary_runs %>% dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::select(node_id, shock_id, scenario, shock_exposure) %>% dplyr::distinct()
# One run per (node, shock): where a shared node has two targets under a shock, the run with the
# larger exposure stands for it in the network counts.
primary_key <- summary_runs %>% dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::filter(scenario == headline_scenario) %>% dplyr::group_by(node_id, shock_id) %>%
  dplyr::slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>% dplyr::select(node_id, shock_id, shock_key)
# One run can stand for several shocks (a China-led node's largest supplier is China), so a run's
# relationships count once per shock it stands for.
trade_for_net <- partners_runs %>% dplyr::inner_join(primary_key, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::select(node_id, shock_id, scenario, exporter_iso, importer_iso, delta_usd, delta_share_of_demand, relationship)
invest_top <- if (nrow(inv_pairs_out)) inv_pairs_out %>% dplyr::filter(dplyr::coalesce(host_rank, 99L) == 1L) %>%
  dplyr::semi_join(primary_key, by = c("node_id", "shock_id", "shock_key")) else NULL
edges <- aib_network_pairs(trade_for_net, invest_top, nodes, node_shock, headline_shock, cfg$classification$high_severity,
                           node_cells = inputs$node_cells, op_index = inputs$op_index, fs_index = inputs$fs_index)
imp_exposure <- importer_loss %>% dplyr::group_by(iso3 = importer_iso) %>%
  dplyr::summarise(headline_lost_usd = sum(lost_usd), n_exposed_subsectors = sum(lost_share > 0), .groups = "drop") %>%
  tidyr::expand_grid(scenario = scenarios)
countries <- aib_network_countries(edges, imp_exposure)
if (!is.null(countries)) {
  countries <- countries %>% dplyr::left_join(inputs$country_names, by = "iso3") %>% dplyr::relocate(iso3, country, scenario)
}
write_out(edges %>% dplyr::mutate(universe = universe_id, .before = 1), "emergent_aib_edges.csv")
write_out(countries, "emergent_aib_country_summary.csv")

# ---- Sectoral compacts (headline shock and scenario) ------------------------------------------------
cmp <- cfg$compacts
cls <- cfg$classification
head_sum <- summary_runs %>% dplyr::inner_join(headline_keys, by = c("node_id", "shock_key")) %>% dplyr::filter(scenario == headline_scenario)
manage_cov <- frontier_nodes %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario) %>%
  dplyr::group_by(node_id) %>%
  dplyr::summarise(coverage_loss_at_manage_cap = {
    k <- which.min(abs(max_supplier_share - cls$manage_cap))
    max(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage[k], 0)
  }, .groups = "drop")
best_host <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario,
                                                             abs(increment - inv$headline_increment) < 1e-9) %>%
  dplyr::group_by(node_id) %>% dplyr::arrange(host_rank, .by_group = TRUE) %>%
  dplyr::summarise(priority_investment_hosts = paste(utils::head(host_iso, 3), collapse = "; "),
                   best_marginal_gain = max(marginal_resilience_gain), .groups = "drop") else
  tibble::tibble(node_id = character(), priority_investment_hosts = character(), best_marginal_gain = numeric())
node_res <- nodes %>% dplyr::select(node_id, sub_sector, techs, supply_chain) %>%
  dplyr::left_join(quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, data_quality, optimize), by = "node_id") %>%
  dplyr::left_join(head_sum, by = "node_id") %>%
  dplyr::left_join(manage_cov, by = "node_id") %>%
  dplyr::left_join(best_host, by = "node_id") %>%
  dplyr::mutate(optimize = dplyr::coalesce(optimize, FALSE) & !is.na(shock_exposure))
if (!is.null(only)) node_res <- node_res[node_res$node_id %in% opt_nodes, , drop = FALSE]
classified <- aib_classify_nodes(node_res, cls)
# The class each node would get under every other shock (headline scenario), for transparency.
for (shk in setdiff(vapply(cfg$shocks, `[[`, "", "id"), headline_shock)) {
  keys_s <- targets %>% dplyr::filter(shock_id == shk) %>% dplyr::distinct(node_id, shock_key)
  sum_s <- summary_runs %>% dplyr::inner_join(keys_s, by = c("node_id", "shock_key")) %>% dplyr::filter(scenario == headline_scenario) %>%
    dplyr::group_by(node_id) %>% dplyr::slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
  mc_s <- frontier_nodes %>% dplyr::filter(shock_id == shk, scenario == headline_scenario) %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(coverage_loss_at_manage_cap = {
      k <- which.min(abs(max_supplier_share - cls$manage_cap))
      max(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage[k], 0)
    }, .groups = "drop")
  nr_s <- nodes %>% dplyr::select(node_id) %>%
    dplyr::left_join(quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, optimize), by = "node_id") %>%
    dplyr::left_join(sum_s, by = "node_id") %>% dplyr::left_join(mc_s, by = "node_id")
  cs <- aib_classify_nodes(nr_s, cls)
  classified[[paste0("class_", shk)]] <- cs$node_classification[match(classified$node_id, cs$node_id)]
}
# ... and under the other scenario for the headline shock.
for (osc in setdiff(scenarios, headline_scenario)) {
  sum_o <- summary_runs %>% dplyr::inner_join(headline_keys, by = c("node_id", "shock_key")) %>% dplyr::filter(scenario == osc)
  mc_o <- frontier_nodes %>% dplyr::filter(shock_id == headline_shock, scenario == osc) %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(coverage_loss_at_manage_cap = {
      k <- which.min(abs(max_supplier_share - cls$manage_cap))
      max(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage[k], 0)
    }, .groups = "drop")
  nr_o <- nodes %>% dplyr::select(node_id) %>%
    dplyr::left_join(quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, optimize), by = "node_id") %>%
    dplyr::left_join(sum_o, by = "node_id") %>% dplyr::left_join(mc_o, by = "node_id")
  co <- aib_classify_nodes(nr_o, cls)
  classified[[paste0("class_", osc)]] <- co$node_classification[match(classified$node_id, co$node_id)]
}
step("Node classes (", headline_shock, ", ", headline_scenario, "): ",
     paste(names(table(classified$node_classification)), table(classified$node_classification), sep = " ", collapse = "; "))

lost_vec <- split(importer_loss, importer_loss$node_id)
lost_vec <- lapply(lost_vec, function(t) stats::setNames(t$lost_share, t$importer_iso))
hosts_head <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario,
                                                             abs(increment - inv$headline_increment) < 1e-9) else NULL
# Partner vectors from the relationships that recur across every shock: each node's optimized
# supply by exporter summed over the shocks (headline scenario), plus the leading hosts of every
# simulated shock.
supply_rec <- supply_runs %>% dplyr::filter(scenario == headline_scenario, state == "optimized") %>%
  dplyr::group_by(node_id, shock_key) %>% dplyr::mutate(w = supply_usd / sum(supply_usd)) %>% dplyr::ungroup() %>%
  dplyr::group_by(node_id, exporter_iso) %>% dplyr::summarise(w = sum(w), .groups = "drop")
hosts_rec <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(scenario == headline_scenario, abs(increment - inv$headline_increment) < 1e-9,
                                                            host_rank <= inv$top_hosts) else NULL
vectors <- aib_node_partner_vectors(supply_rec, lost_vec, hosts_rec, inv$top_hosts)
coh <- aib_coherence_matrix(edges_sc, classified$node_id, max_hops = 2)
groups <- aib_group_nodes(classified, vectors, coh, cmp$min_partner_similarity, cmp$min_policy_similarity)
step("Compact grouping: ", nrow(groups), " intervention nodes in ", dplyr::n_distinct(groups$group), " groups")

targets_cov <- sort(as.numeric(unlist(cmp$coverage_targets)))
mid_target <- targets_cov[ceiling(length(targets_cov) / 2)]
compact_rows <- list(); compact_nodes <- list(); compact_members <- list(); compact_paths <- list()
ex_metrics <- inputs$exporter_metrics
for (g in sort(unique(groups$group))) {
  ids <- groups$node_id[groups$group == g]
  cn <- classified[classified$node_id %in% ids, , drop = FALSE]
  il <- importer_loss[importer_loss$node_id %in% ids, , drop = FALSE] %>%
    dplyr::group_by(importer_iso) %>% dplyr::summarise(lost = sum(lost_usd), .groups = "drop") %>% dplyr::arrange(dplyr::desc(lost))
  il$cum <- cumsum(il$lost) / sum(il$lost)
  n_anchor <- min(max(1, which(il$cum >= cmp$anchor_share)[1]), cmp$max_anchors, nrow(il), na.rm = TRUE)
  anchors <- il$importer_iso[seq_len(n_anchor)]
  # Candidate members: every candidate exporter of the compact's nodes and their eligible hosts.
  build_ids <- cn$node_id[cn$node_classification == "BUILD"]
  host_extra <- list()
  cand <- unique(unlist(lapply(ids, function(n) headline_problems[[n]]$cand)))
  for (n in build_ids) {
    h <- if (!is.null(hosts_head)) hosts_head %>% dplyr::filter(node_id == n, host_rank <= inv$top_hosts) else NULL
    if (!is.null(h) && nrow(h)) {
      host_extra[[n]] <- stats::setNames(rep(cmp$coalition_increment, nrow(h)), h$host_iso)
      cand <- union(cand, h$host_iso)
    }
  }
  path <- aib_coalition_path(headline_problems, ids, anchors, setdiff(cand, anchors), headline_scenario, host_extra, target = max(targets_cov))
  coal <- lapply(targets_cov, function(t) aib_coalition_at(path, t))
  names(coal) <- sprintf("%.0f", 100 * targets_cov)
  mid <- coal[[sprintf("%.0f", 100 * mid_target)]]
  members <- if (isTRUE(mid$reached)) strsplit(mid$members, " ")[[1]] else strsplit(utils::tail(path$members, 1), " ")[[1]]
  widest <- strsplit(utils::tail(path$members[!is.na(path$members)], 1), " ")[[1]]
  top_t <- coal[[length(coal)]]
  associates <- if (isTRUE(top_t$reached)) setdiff(strsplit(top_t$members, " ")[[1]], members) else setdiff(widest, members)

  # Roles from the model results.
  fl <- flows_headline %>% dplyr::filter(scenario == headline_scenario, node_id %in% ids)
  sup_share <- fl %>% dplyr::filter(m %in% anchors) %>% dplyr::group_by(node_id, e) %>% dplyr::summarise(f = sum(f), .groups = "drop") %>%
    dplyr::group_by(node_id) %>% dplyr::mutate(share = f / sum(f)) %>% dplyr::ungroup() %>%
    dplyr::left_join(ex_metrics %>% dplyr::select(node_id, e = exporter_iso, export_rca, world_share), by = c("node_id", "e")) %>%
    dplyr::left_join(cn %>% dplyr::select(node_id, supply_chain, node_classification), by = "node_id")
  role_rows <- list()
  add_role <- function(iso, role, basis) if (length(iso)) role_rows[[length(role_rows) + 1]] <<- tibble::tibble(iso3 = iso, role = role, basis = basis)
  add_role(intersect(anchors, members), "Demand anchor", "imports most of the compact's lost supply")
  sp <- sup_share %>% dplyr::filter(e %in% members, share >= cmp$role_min_share, dplyr::coalesce(export_rca, 0) >= 1)
  add_role(unique(sp$e[sp$supply_chain != "Upstream"]), "Specialized supplier", "RCA >= 1 and a material share of the anchors' optimized supply")
  add_role(unique(sp$e[sp$supply_chain == "Upstream"]), "Resource supplier", "RCA >= 1 and a material share of the anchors' optimized upstream supply")
  hh <- if (!is.null(hosts_head)) hosts_head %>% dplyr::filter(node_id %in% build_ids, host_rank <= inv$top_hosts, host_iso %in% members) else NULL
  if (!is.null(hh) && nrow(hh)) add_role(unique(hh$host_iso), "Capacity-building / investment host", "top-ranked host in a BUILD node")
  tp <- ex_metrics %>% dplyr::filter(node_id %in% cn$node_id[cn$node_classification %in% c("BUILD", "MANAGE SPECIALIZATION")],
                                     exporter_iso %in% members, dplyr::coalesce(export_rca, 0) >= 1, world_share >= 0.05)
  add_role(unique(tp$exporter_iso), "Technology partner", "revealed capability (RCA >= 1, >= 5% of world exports) in a BUILD or MANAGE node")
  # Bridge: supplies one node of the compact and is an anchor (a large loser) of another, or
  # supplies nodes in more than one stage.
  supplies <- dplyr::bind_rows(
    sp %>% dplyr::transmute(node_id, iso3 = e),
    if (!is.null(hh) && nrow(hh)) hh %>% dplyr::transmute(node_id, iso3 = host_iso)
  ) %>% dplyr::distinct()
  node_anchor <- importer_loss %>% dplyr::filter(node_id %in% ids) %>% dplyr::group_by(node_id) %>%
    dplyr::mutate(share = lost_share / sum(lost_share)) %>% dplyr::ungroup() %>%
    dplyr::filter(share >= cmp$node_anchor_share) %>% dplyr::transmute(node_id, iso3 = importer_iso)
  vertical <- supplies %>% dplyr::inner_join(node_anchor, by = "iso3", suffix = c("_sup", "_dep"), relationship = "many-to-many") %>%
    dplyr::filter(node_id_sup != node_id_dep)
  stage_span <- sup_share %>% dplyr::filter(e %in% members) %>% dplyr::group_by(e) %>% dplyr::summarise(n = dplyr::n_distinct(supply_chain), .groups = "drop")
  bridge <- intersect(members, union(unique(vertical$iso3), stage_span$e[stage_span$n > 1]))
  add_role(bridge, "Bridge partner", "supplies one node of the compact and depends on another, or spans its stages")
  add_role(associates, "Associate partner", sprintf("joins only to reach the %.0f%% target", 100 * max(targets_cov)))
  roles <- dplyr::bind_rows(role_rows)
  unassigned <- setdiff(members, roles$iso3)
  if (length(unassigned)) roles <- dplyr::bind_rows(roles, tibble::tibble(iso3 = unassigned, role = "Supplier (coalition member)", basis = "adds supply in the coalition test"))

  # Coverage the compact's own coalition achieves (non-members' shipments stay as they are), before
  # and after the investment response (the headline increment at up to `investment_hosts_per_node`
  # member hosts of each BUILD node), against universe demand; the global optimum is kept apart.
  member_hosts <- function(n) {
    if (!(n %in% build_ids) || is.null(hosts_head)) return(character())
    h <- hosts_head %>% dplyr::filter(node_id == n) %>% dplyr::arrange(host_rank)
    utils::head(intersect(h$host_iso, members), cmp$investment_hosts_per_node)
  }
  # Trade compact: every economy keeps its existing-relationship response (state B) and the coalition's
  # members may also open or expand relationships (coverage objective only).
  growth <- as.numeric(aib_or(opt$existing_relationship_growth, Inf))
  cov_mixed <- function(n, extra = NULL) {
    aib_solve_state(headline_problems[[n]], headline_scenario, "B", cap = 1, scores = node_scores(n), tolerances = tolerances,
                    neutral = neutral, steps = 1, existing_growth = growth, coordinating = c(members, names(extra)), extra = extra)$coverage
  }
  cov_trade <- vapply(ids, function(n) cov_mixed(n), numeric(1))
  cov_inv <- vapply(ids, function(n) {
    hs <- member_hosts(n)
    if (!length(hs)) return(cov_trade[[n]])
    cov_mixed(n, stats::setNames(rep(as.numeric(inv$headline_increment), length(hs)), hs))
  }, numeric(1))
  # Fragmented baseline: the anchors' coverage through existing relationships (state B), against
  # what the coalition gives them; a compact needs to beat it by `min_compact_gain`.
  fx <- flows_existing_headline %>% dplyr::filter(scenario == headline_scenario)
  anchor_dem <- vapply(ids, function(n) { A <- intersect(anchors, headline_problems[[n]]$scope); sum(headline_problems[[n]]$demand[A]) * headline_problems[[n]]$D }, numeric(1))
  frag <- vapply(ids, function(n) aib_anchor_existing_coverage(headline_problems[[n]], anchors, fx[fx$node_id == n, , drop = FALSE]), numeric(1))
  coal_cov <- vapply(ids, function(n) {
    v <- aib_anchor_coverage(headline_problems[[n]], anchors, members, headline_scenario, host_extra[[n]])
    if (is.na(v)) NA_real_ else v
  }, numeric(1))
  okd <- !is.na(frag) & !is.na(coal_cov) & anchor_dem > 0
  anchor_fragmented <- if (any(okd)) sum(frag[okd] * anchor_dem[okd]) / sum(anchor_dem[okd]) else NA_real_
  anchor_compact <- if (any(okd)) sum(coal_cov[okd] * anchor_dem[okd]) / sum(anchor_dem[okd]) else NA_real_
  compact_gain <- anchor_compact - anchor_fragmented
  dem <- cn$demand_usd
  wmean <- function(x) sum(x * dem) / sum(dem)
  mix <- aib_class_mix(cn$node_classification, cn$lost_usd)
  archetype <- aib_archetype(cn$node_classification, cn$lost_usd, cmp$archetype_dominance)
  secondary <- if (length(mix) >= 2 && mix[2] >= aib_or(cmp$policy_need_floor, 0.2)) names(mix)[2] else NA_character_
  n_countries <- length(members)
  compact_type <- if (!is.na(compact_gain) && compact_gain < aib_or(cmp$min_compact_gain, 0.05)) {
    "compact unnecessary: existing relationships do nearly as well"
  } else if (n_countries < cmp$min_countries) {
    "bilateral arrangement sufficient"
  } else {
    "candidate compact"
  }
  cid <- sprintf("AIB-C%02d", g)
  tools <- aib_archetype_tools()
  compact_rows[[length(compact_rows) + 1]] <- tibble::tibble(
    compact_id = cid,
    compact_name = aib_compact_name(cn$sub_sector, cn$techs),
    technology = paste(sort(unique(unlist(strsplit(cn$techs, "; ")))), collapse = "; "),
    stages = paste(sort(unique(cn$supply_chain)), collapse = "; "),
    included_subsectors = paste(cn$sub_sector, collapse = "; "),
    n_subsectors = nrow(cn),
    compact_type = compact_type,
    compact_archetype = archetype,
    node_classes = paste(sprintf("%s: %d", names(table(cn$node_classification)), table(cn$node_classification)), collapse = "; "),
    minimum_coalition = paste(sort(members), collapse = " "),
    minimum_coalition_size = n_countries,
    minimum_coalition_90 = coal[[1]]$members,
    minimum_coalition_95 = if (length(coal) >= 2) coal[[2]]$members else NA_character_,
    minimum_coalition_99 = coal[[length(coal)]]$members,
    target_reached_99 = isTRUE(top_t$reached),
    max_anchor_coverage = max(path$min_coverage),
    demand_anchors = paste(roles$iso3[roles$role == "Demand anchor"], collapse = " "),
    specialized_suppliers = paste(sort(unique(roles$iso3[roles$role %in% c("Specialized supplier", "Resource supplier")])), collapse = " "),
    investment_hosts = paste(sort(unique(roles$iso3[roles$role == "Capacity-building / investment host"])), collapse = " "),
    technology_partners = paste(sort(unique(roles$iso3[roles$role == "Technology partner"])), collapse = " "),
    bridge_partners = paste(sort(unique(roles$iso3[roles$role == "Bridge partner"])), collapse = " "),
    associate_partners = paste(sort(associates), collapse = " "),
    demand_usd = sum(dem),
    lost_usd = sum(cn$lost_usd),
    baseline_shock_exposure = sum(cn$lost_usd) / sum(dem),
    coverage_static = wmean(cn$static_coverage),
    coverage_without_coordination = wmean(cn$existing_coverage),
    coverage_with_trade_coordination = wmean(cov_trade),
    coverage_after_investment_simulation = wmean(cov_inv),
    coverage_global_optimum = wmean(cn$optimized_coverage),
    coordination_dividend = wmean(cov_trade) - wmean(cn$existing_coverage),
    residual_gap = 1 - wmean(cov_trade),
    residual_gap_after_investment = 1 - wmean(cov_inv),
    anchor_coverage_fragmented = anchor_fragmented,
    anchor_coverage_compact = anchor_compact,
    compact_gain_over_fragmented = compact_gain,
    class_mix = paste(sprintf("%s %.0f%%", names(mix), 100 * mix), collapse = "; "),
    secondary_class = secondary,
    key_policy_needs = aib_policy_needs(mix, aib_or(cmp$policy_need_floor, 0.2)),
    shock_id = headline_shock,
    scenario = headline_scenario,
    universe = universe_id
  )
  compact_members[[length(compact_members) + 1]] <- roles %>%
    dplyr::mutate(compact_id = cid, in_minimum_coalition = iso3 %in% members, .before = 1)
  compact_paths[[length(compact_paths) + 1]] <- path %>%
    dplyr::mutate(compact_id = cid, node_coverage = vapply(node_coverage, function(v) paste(sprintf("%s=%.3f", names(v), v), collapse = "; "), ""), .before = 1)
  compact_nodes[[length(compact_nodes) + 1]] <- tibble::tibble(node_id = ids, compact_id = cid)
}
compacts <- dplyr::bind_rows(compact_rows)
members_tbl <- dplyr::bind_rows(compact_members)
if (nrow(members_tbl)) {
  multi <- members_tbl %>% dplyr::filter(in_minimum_coalition) %>% dplyr::distinct(compact_id, iso3) %>% dplyr::count(iso3) %>% dplyr::filter(n > 1)
  extra_bridge <- members_tbl %>% dplyr::filter(in_minimum_coalition, iso3 %in% multi$iso3) %>% dplyr::distinct(compact_id, iso3) %>%
    dplyr::mutate(role = "Bridge partner", basis = "member of several compacts' minimum coalitions", in_minimum_coalition = TRUE)
  members_tbl <- dplyr::bind_rows(members_tbl, extra_bridge) %>% dplyr::distinct(compact_id, iso3, role, .keep_all = TRUE) %>%
    dplyr::left_join(inputs$country_names, by = "iso3")
  compacts <- compacts %>% dplyr::left_join(
    members_tbl %>% dplyr::filter(role == "Bridge partner") %>% dplyr::group_by(compact_id) %>%
      dplyr::summarise(bp = paste(sort(unique(iso3)), collapse = " "), .groups = "drop"), by = "compact_id") %>%
    dplyr::mutate(bridge_partners = dplyr::coalesce(bp, "")) %>% dplyr::select(-bp)
}
compacts <- compacts %>% dplyr::arrange(dplyr::desc(lost_usd))
if (nrow(compacts)) {
  # Renumber by lost supply, largest first.
  remap <- stats::setNames(sprintf("AIB-C%02d", seq_len(nrow(compacts))), compacts$compact_id)
  compacts$compact_id <- unname(remap[compacts$compact_id])
  members_tbl$compact_id <- unname(remap[members_tbl$compact_id])
  paths_tbl <- dplyr::bind_rows(compact_paths); paths_tbl$compact_id <- unname(remap[paths_tbl$compact_id])
  cnodes <- dplyr::bind_rows(compact_nodes); cnodes$compact_id <- unname(remap[cnodes$compact_id])
} else {
  paths_tbl <- tibble::tibble(); cnodes <- tibble::tibble(node_id = character(), compact_id = character())
}

# Node table (all classified nodes; compact_id empty where no compact is needed) with the
# Gates & Fences handoff fields.
top_partners <- partners_out %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario, relationship != "domestic retention") %>%
  dplyr::group_by(node_id) %>% dplyr::arrange(dplyr::desc(delta_usd), .by_group = TRUE) %>%
  dplyr::summarise(key_trade_partners = paste(utils::head(sprintf("%s->%s (%s)", exporter_iso, importer_iso, relationship), 4), collapse = "; "), .groups = "drop")
pct <- function(x) ifelse(is.na(x), "n/a", sprintf("%.0f%%", 100 * x))
detail <- inputs$node_cells_detail %>% dplyr::group_by(node_id) %>%
  dplyr::summarise(yf_subcomponent_id = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain), "::", sub_sector)), collapse = "; "), .groups = "drop")
cells_long <- inputs$node_cells %>% dplyr::select(node_id, technology = tech, stage = supply_chain)
node_table <- classified %>%
  dplyr::left_join(cnodes, by = "node_id") %>%
  dplyr::left_join(top_partners, by = "node_id") %>%
  dplyr::left_join(detail, by = "node_id") %>%
  dplyr::left_join(cells_long, by = "node_id", relationship = "one-to-many") %>%
  dplyr::mutate(
    severity = shock_exposure,
    concentration = optimized_largest_share,
    strategic_problem = paste0(classification_reason, "; shock exposure ", pct(shock_exposure), "; baseline largest supplier ",
                               baseline_largest_supplier, " (", pct(baseline_largest_share), ")"),
    trade_solution = ifelse(is.na(key_trade_partners), "no new relationships restore supply", key_trade_partners),
    investment_need = ifelse(dplyr::coalesce(residual_gap, 0) >= inv$min_residual_gap,
                             paste0(pct(residual_gap), " of demand unserved after trade; hosts: ", dplyr::coalesce(priority_investment_hosts, "none eligible")),
                             "none material"),
    concentration_risk = paste0("optimized largest supplier ", optimized_largest_supplier, " ", pct(optimized_largest_share), "; HHI ",
                                ifelse(is.na(optimized_hhi), "n/a", sprintf("%.2f", optimized_hhi))),
    recommended_policy_treatment = aib_class_treatment(node_classification),
    yf_cell = paste0(aib_slug(technology), "-", aib_slug(stage))
  ) %>%
  dplyr::select(compact_id, technology, stage, sub_sector, node_id, node_classification, classification_reason, severity,
                coordination_dividend, residual_gap, concentration, key_trade_partners, priority_investment_hosts,
                strategic_problem, trade_solution, investment_need, concentration_risk, recommended_policy_treatment,
                shock_exposure, static_coverage, existing_coverage, optimized_coverage, best_marginal_gain,
                optimized_largest_supplier, optimized_hhi, coverage_loss_at_manage_cap, data_quality, yf_cell, yf_subcomponent_id,
                need_coordinate, need_build, need_manage) %>%
  dplyr::mutate(shock_id = headline_shock, scenario = headline_scenario) %>%
  dplyr::arrange(compact_id, technology, stage, sub_sector)

write_out(classified %>% dplyr::select(node_id, sub_sector, techs, supply_chain, data_quality, node_classification, classification_reason,
                                       shock_exposure, static_coverage, existing_coverage, optimized_coverage, coordination_dividend,
                                       residual_gap, optimized_largest_supplier, optimized_largest_share, coverage_loss_at_manage_cap,
                                       optimized_top3_share, priority_investment_hosts, best_marginal_gain, need_coordinate, need_build, need_manage,
                                       dplyr::starts_with("class_")),
          "aib_node_classification.csv")
write_out(compacts, "aib_compact_candidates.csv")
write_out(node_table, "aib_compact_nodes.csv")
write_out(members_tbl, "aib_compact_members.csv")
write_out(paths_tbl, "aib_compact_coalition_path.csv")

# ---- Validation cases ------------------------------------------------------------------------------
val <- aib_validation_chains(unlist(cfg$validation_cases), classified %>% dplyr::left_join(dq_out %>% dplyr::filter(is.na(aggregated_into)) %>%
                               dplyr::select(node_id, flags), by = "node_id"), node_table, partners_out, inv_out, compacts, members_tbl,
                             headline_shock, headline_scenario, nodes, as.numeric(inv$headline_increment))
write_out(val$table, "aib_validation_cases.csv")
writeLines(val$markdown, file.path(out_dir, "aib_validation_cases.md"), useBytes = TRUE)

saveRDS(list(
  meta = list(built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), universe = universe_id, headline_shock = headline_shock,
              headline_scenario = headline_scenario, trade_year = inputs$meta$trade_year, config = cfg),
  nodes = nodes, node_cells = inputs$node_cells, quality = dq_out, stress = stress, partnerships = partners_out,
  frontier = frontier_out, investment = inv_out, investment_pairs = inv_pairs, edges = edges, countries = countries,
  classification = classified, compacts = compacts, compact_nodes = node_table, compact_members = members_tbl,
  coalition_paths = paths_tbl, flows_headline = flows_headline, importer_loss = importer_loss, validation = val$table,
  country_names = inputs$country_names
), file.path(out_dir, "aib_results.rds"))
step("AIB done in ", round(as.numeric(difftime(Sys.time(), t_script, units = "mins")), 1), " min; ", nrow(compacts), " candidate compacts")
