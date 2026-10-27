# Allied Industrial Base (AIB), step 2: stress tests, the Trade Partnership Network, the
# diversification-specialization frontier, Investment Partnership Prioritization, the emergent AIB
# network and Sectoral Compact formation (docs/aib_methodology.md; config/aib.yml; R/aib/).
#
# Usage:
#   Rscript scripts/32_build_aib_partnerships.R [--out-dir DIR] [--nodes id1,id2] [--skip-investment]
#     [--demand-scope S] [--partner-universe P] [--scope all|electro_industrial] [--from-checkpoint]
#     [--raw-dir DIR] [--processed-dir DIR]
#
#   --out-dir           where scripts/31 wrote aib_inputs.rds and where the outputs go
#                       (default: <outputs_dir>/aib)
#   --nodes             run only these node ids (validation runs); the compacts then cover them only
#   --skip-investment   stop after the trade model (no investment, network or compacts)
#   --demand-scope S    override config demand_scope: global, coalition or ISO3,ISO3,...
#   --partner-universe P override config candidate_partner_universe (same forms)
#   --scope M           override config strategic_scope mode (all or electro_industrial)
#   --from-checkpoint   reuse the last run's trade model and investment simulation (same out dir); the
#                       investment is simulated again when only its settings (config investment) changed
#   --raw-dir, --processed-dir  the OSI pipeline's inputs the host types read (income groups, Development
#                       Potential Index; scripts/utils/aib_config.R aib_read_host_type_data()), when
#                       aib_inputs.rds does not carry them (defaults: config raw_data_dir, processed_dir)
#
# Every trade run is one variant: shock semantics (hold: an export / supply shock; release: a two-way
# cut, the shocked economy's purchases also fall by the same share) x supply constraint (trade_only;
# production_constrained: physical capacity bounds what
# a supplier can add, where credible production data exist). The headline variant is the first of
# each list in config/aib.yml; every output keeps the variant columns, the demand scope and the
# candidate-partner universe. Checkpoints aib_checkpoint_trade.rds / aib_checkpoint_investment.rds
# serve --from-checkpoint. Writes (CSV):
#   allied_trade_stress_test.csv          node / stage / technology x shock x variant x scenario
#   allied_trade_partnerships.csv         optimized new / expanded relationships (headline limit)
#   allied_trade_frontier.csv             coverage, concentration, specialization and N-1 by limit
#   allied_trade_growth_sensitivity.csv   existing-network coverage, dividend and class by growth bound
#   allied_investment_partnerships.csv    marginal capacity simulation, host x increment
#   allied_investment_pairs.csv           investor (importer served) -> host rows with Friendshore
#   allied_investment_screened_hosts.csv  the hosts the alignment screen keeps out, simulated unranked, with
#                                         the alignment cost (best screened gain minus best eligible gain)
#   aib_host_screen.csv                   every candidate partner: UNGA bloc, demand scope, screened or not
#   emergent_aib_edges.csv                supplier -> buyer pairs (strategic scope)
#   emergent_aib_country_summary.csv      each economy's roles in the network (no single ranking)
#   aib_node_classification.csv           every node's class, multi-label needs and warnings
#   aib_compact_candidates.csv            Sectoral Compacts, per supply-constraint mode
#   aib_compact_nodes.csv                 node table per compact (the Gates & Fences handoff)
#   aib_compact_members.csv               members, roles, coalition tiers, critical dependencies
#   aib_compact_coalition_path.csv        coverage as members are added (greedy path)
#   aib_compact_leave_one_out.csv         each supplier's loss, coalition fixed and every candidate open
#   allied_subsector_data_quality.csv     data-quality and physical-validation flags per sub-sector
#   aib_validation_cases.csv / .md        the validation chains, trade-only vs production-constrained
#   aib_diversification.csv               the diversification scenario (Managed Interdependence) per node
# The dashboard's three scenarios (Baseline: observed trade; Managed Interdependence: the diversification
# shock; Decoupling: the headline shock, whose outputs are the files above) add:
#   aib_observed_supply.csv               Baseline: each node's observed supply to the demand scope by exporter
#   emergent_aib_edges_baseline.csv       Baseline: observed supplier -> buyer pairs (strategic scope, >= min share)
#   emergent_aib_country_summary_baseline.csv  Baseline: each economy's roles in that network
#   emergent_aib_edges_managed.csv        Managed Interdependence: the network of the diversification shock alone
#   emergent_aib_country_summary_managed.csv   Managed Interdependence: the economies' roles in it
#   aib_compact_candidates_managed.csv    Managed Interdependence: the headline compacts (same ids and nodes)
#   aib_compact_members_managed.csv       solved under the diversification shock (headline supply constraint):
#   aib_compact_coalition_path_managed.csv  coalition, tiers, roles, path and leave-one-out
#   aib_compact_leave_one_out_managed.csv
#   aib_results.rds                       everything above in one list (for scripts/84)

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

for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
if (!aib_has_lp()) stop("The AIB optimizer needs the Rglpk and slam packages (GLPK).")

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(repo_root, config$processed_dir, "outputs")
out_dir <- abs_path(arg_value("--out-dir", file.path(outputs_dir, "aib")))
inputs_path <- file.path(out_dir, "aib_inputs.rds")
if (!file.exists(inputs_path)) stop("Run scripts/31_build_aib_trade_network.R first: ", inputs_path, " not found.")
inputs <- readRDS(inputs_path)
# Six-stage inputs only (scripts/31 since 2026-10-27): an older aib_inputs.rds is Upstream / Midstream / Downstream.
assert_supply_chain_stages(c(inputs$nodes$supply_chain, inputs$node_cells$supply_chain), paste0(inputs_path, " (rerun scripts/31)"))
# The taxonomy is the one scripts/31 built the inputs with (config/aib_taxonomy_<taxonomy>.yml).
source(file.path(repo_root, "scripts", "utils", "aib_config.R"))
input_taxonomy <- aib_or(inputs$meta$taxonomy, "master")
if (!is.null(arg_value("--taxonomy")) && !identical(arg_value("--taxonomy"), input_taxonomy)) {
  stop("--taxonomy ", arg_value("--taxonomy"), " differs from the inputs' taxonomy (", input_taxonomy, "); rerun scripts/31 with it.")
}
cfg <- aib_read_config(repo_root, input_taxonomy)
taxonomy_tbl <- if (is.null(inputs$taxonomy)) tibble::tibble(tech = character(), index_code = character(), index_name = character()) else inputs$taxonomy
opt <- cfg$optimization
tolerances <- c(opt$tolerance_coverage, opt$tolerance_specialization, opt$tolerance_opportunity)
neutral <- as.numeric(opt$neutral_score)
caps <- as.numeric(unlist(opt$max_supplier_share))
headline_cap <- as.numeric(opt$headline_max_supplier_share)
scenarios <- unlist(cfg$scenarios)
headline_shock <- cfg$headline_shock
headline_scenario <- cfg$headline_scenario
components <- unlist(cfg$compatibility$components)
growth <- as.numeric(aib_or(opt$existing_relationship_growth, Inf))
growths <- as.numeric(unlist(aib_or(opt$existing_growth_sensitivity, list(0.25, 1, 3, Inf))))
semantics <- unlist(cfg$shocked_demand)
headline_semantics <- semantics[1]
sc_cfg <- cfg$supply_constraint
constraints <- unlist(aib_or(sc_cfg$modes, "trade_only"))
headline_constraint <- constraints[1]
cls <- cfg$classification
write_out <- function(tbl, name) {
  utils::write.csv(tbl, file.path(out_dir, name), row.names = FALSE, na = "")
  step("  wrote ", name, " (", format(NROW(tbl), big.mark = ","), " rows)")
}

# ---- Demand scope, candidate-partner universe, strategic scope ------------------------------------
all_econ <- unique(c(inputs$flows$exporter_iso, inputs$flows$importer_iso))
registry_exclusions <- toupper(unlist(cfg$trade$exclude_from_universe))
resolve_set <- function(set_cfg, override) {
  mode <- if (!is.null(override)) override else aib_or(set_cfg$mode, "global")
  if (grepl(",", mode)) return(list(id = "configured", iso = setdiff(toupper(strsplit(mode, ",", fixed = TRUE)[[1]]), registry_exclusions)))
  iso <- switch(mode,
    global = all_econ,
    coalition = {
      if (is.null(inputs$coalition)) stop("A coalition set needs data/processed/coalition_tbl.rds.")
      inputs$coalition$iso3[inputs$coalition$tier %in% unlist(aib_or(set_cfg$tiers, list("core", "partner")))]
    },
    configured = toupper(unlist(set_cfg$countries)),
    stop("Unknown set mode: ", mode)
  )
  list(id = mode, iso = setdiff(iso, registry_exclusions))
}
demand_set <- resolve_set(cfg$demand_scope, arg_value("--demand-scope"))
partner_set <- resolve_set(cfg$candidate_partner_universe, arg_value("--partner-universe"))
universe_id <- paste0(demand_set$id, " demand / ", partner_set$id, " partners")
ss <- cfg$strategic_scope
scope_mode <- arg_value("--scope", aib_or(ss$mode, "all"))
nodes <- inputs$nodes
if (!identical(scope_mode, "all") && is.null(ss$technologies[[scope_mode]])) {
  stop("Unknown strategic scope '", scope_mode, "': use all or one of ", paste(names(ss$technologies), collapse = ", "), ".")
}
scope_techs <- if (identical(scope_mode, "all")) unique(inputs$node_cells$tech) else unlist(ss$technologies[[scope_mode]])
in_scope <- vapply(strsplit(nodes$techs, "; "), function(t) any(t %in% scope_techs), logical(1))
scope_nodes <- nodes$node_id[in_scope]
if (!length(scope_nodes)) stop("No node is in the strategic scope '", scope_mode, "' (technologies: ", paste(scope_techs, collapse = ", "), ").")
step("AIB partnerships: demand scope ", demand_set$id, " (", length(demand_set$iso), " economies); partners ", partner_set$id,
     " (", length(partner_set$iso), "); strategic scope ", scope_mode, " (", length(scope_nodes), " of ", nrow(nodes), " nodes); ",
     "headline variant ", headline_semantics, " / ", headline_constraint)

quality <- inputs$quality
if (!"aggregated_into" %in% names(quality)) quality$aggregated_into <- NA_character_
opt_nodes <- quality$node_id[quality$optimize]
only <- arg_value("--nodes")
if (!is.null(only)) opt_nodes <- intersect(opt_nodes, strsplit(only, ",", fixed = TRUE)[[1]])
flows_by_node <- split(inputs$flows[inputs$flows$value > 0, c("node_id", "exporter_iso", "importer_iso", "value")],
                       inputs$flows$node_id[inputs$flows$value > 0])

# Physical production / capacity: a node is constrained only where the proxy covers at least
# `min_physical_completeness` of world production; otherwise it keeps its trade-only result, flagged.
prod <- inputs$production
phys_cov <- if (is.null(prod) || !nrow(prod)) tibble::tibble(node_id = character(), physical_completeness = numeric()) else
  prod %>% dplyr::group_by(node_id) %>% dplyr::summarise(physical_completeness = sum(production_share), .groups = "drop")
phys_nodes <- phys_cov$node_id[phys_cov$physical_completeness >= aib_or(sc_cfg$min_physical_completeness, 0.8)]
producer_min_share <- as.numeric(aib_or(sc_cfg$producer_min_share, 0))
phys_share_of <- function(nid) {
  p <- prod[prod$node_id == nid, , drop = FALSE]
  stats::setNames(p$production_share, p$iso3)
}

# The diversification scenario (a shock of type "diversify"): each node more concentrated than its
# higher-level sector gets the supplier-share cap that brings its exporter HHI down to that benchmark;
# the suppliers above the cap lose the excess, and the optimized state keeps the cap.
div_shock <- Filter(function(s) identical(s$type, "diversify"), cfg$shocks)
div_id <- if (length(div_shock)) div_shock[[1]]$id else NA_character_
div_targets <- NULL
if (length(div_shock)) {
  if (is.null(inputs$benchmarks)) {
    warning("A diversify shock is configured but the inputs have no benchmarks; rerun scripts/31.", call. = FALSE)
  } else {
    scope_supply <- inputs$flows[inputs$flows$node_id %in% opt_nodes & inputs$flows$importer_iso %in% demand_set$iso & inputs$flows$value > 0, ] %>%
      dplyr::group_by(node_id, exporter_iso) %>% dplyr::summarise(value = sum(value), .groups = "drop")
    div_targets <- aib_diversify_targets(inputs$exporter_metrics[inputs$exporter_metrics$node_id %in% opt_nodes, , drop = FALSE],
                                         inputs$benchmarks, level = aib_or(div_shock[[1]]$benchmark, "hs4"),
                                         reexport_hubs = cfg$trade$reexport_hubs, min_excess = as.numeric(aib_or(div_shock[[1]]$min_excess, 0)),
                                         scope_supply = scope_supply)
    step("Diversification scenario: ", sum(!is.na(div_targets$cap)), " of ", nrow(div_targets), " nodes are more concentrated than their benchmark")
  }
}
targets <- aib_shock_targets(cfg$shocks, inputs$node_cells, inputs$exporter_metrics, cfg$trade$reexport_hubs, diversify = div_targets)
targets <- targets[targets$node_id %in% opt_nodes, , drop = FALSE]
runs <- targets %>% dplyr::distinct(node_id, shock_key, .keep_all = TRUE) %>% dplyr::select(node_id, shock_key, cap) %>%
  dplyr::arrange(node_id, shock_key)
# A diversification run: its cap is a share of the scope's demand and the capped allies' own
# purchases stay protected (aib_node_problem(protect_shocked, cap_basis)).
is_div_key <- function(nid, key) !is.na(div_id) && any(targets$node_id == nid & targets$shock_key == key & targets$shock_id == div_id)
# The supplier-share limit of a run's optimized state: the diversification cap, else the headline one.
run_cap_of <- function(nid, key) {
  c0 <- targets$cap[targets$node_id == nid & targets$shock_key == key]
  c0 <- c0[!is.na(c0)]
  if (length(c0)) c0[1] else headline_cap
}
headline_keys <- targets %>% dplyr::filter(shock_id == headline_shock) %>% dplyr::distinct(node_id, shock_key)
step("Trade model: ", length(opt_nodes), " nodes, ", length(cfg$shocks), " shocks, ", nrow(runs), " node-shock runs x ",
     length(semantics), " shock semantics x ", length(constraints), " supply constraints (", length(phys_nodes),
     " nodes with physical data) x ", length(scenarios), " scenarios")

# The OSI technology x six-stage cells the partner indices, the Development Potential Index and the
# policy compatibility are read for: score_cells (scripts/31: each technology at its own stage, from the
# crosswalk with the dictionary, aib_osi_crosswalk(), or the node's own cells with the master taxonomy);
# an inputs file without it falls back to score_techs at score_stage; none (neutral scores) for a node
# with no code in the OSI basket. `stage` is one per technology.
score_cell <- function(nid) {
  n <- nodes[nodes$node_id == nid, ]
  if ("score_cells" %in% names(n) && !is.na(n$score_cells) && nzchar(n$score_cells)) {
    cells <- strsplit(strsplit(n$score_cells, "; ", fixed = TRUE)[[1]], "|", fixed = TRUE)
    return(list(techs = vapply(cells, `[`, "", 1), stage = vapply(cells, `[`, "", 2)))
  }
  has <- "score_techs" %in% names(n) && !is.na(n$score_techs) && nzchar(n$score_techs)
  if (has) return(list(techs = strsplit(n$score_techs, "; ")[[1]], stage = aib_or(n$score_stage, n$supply_chain)))
  if (identical(aib_or(cfg$nodes$source, "master"), "dictionary")) return(list(techs = character(), stage = n$supply_chain))
  list(techs = strsplit(n$techs, "; ")[[1]], stage = n$supply_chain)
}
node_scores <- function(nid) {
  sc <- score_cell(nid)
  aib_node_scores(nid, sc$techs, sc$stage, inputs$exporter_metrics, inputs$op_index,
                  inputs$pams, inputs$cat_scores, rca_cap = opt$rca_cap, neutral = neutral, components = components)
}
node_problem <- function(nid, key, sem = headline_semantics, con = headline_constraint) {
  div <- is_div_key(nid, key)
  aib_node_problem(flows_by_node[[nid]], aib_parse_shock_key(key), shocked_demand = sem,
                   protect_shocked = div, cap_basis = if (div) "demand" else "served",
                   candidate_min_exports = as.numeric(cfg$trade$candidate_min_exports_usd),
                   candidate_min_share = as.numeric(cfg$trade$candidate_min_world_share),
                   allow_domestic = isTRUE(opt$allow_domestic_retention),
                   demand_scope = demand_set$iso, partner_universe = partner_set$iso,
                   physical_share = if (identical(con, "production_constrained") && nid %in% phys_nodes) phys_share_of(nid) else NULL,
                   physical_min_share = producer_min_share)
}
vtag <- function(t, nid, key, sem, con) {
  if (is.null(t) || !nrow(t)) return(NULL)
  dplyr::mutate(t, node_id = nid, shock_key = key, shock_semantics = sem, supply_constraint = con, .before = 1)
}

# ---- Trade model: every variant ----------------------------------------------------------------
trade_ckpt <- file.path(out_dir, "aib_checkpoint_trade.rds")
inv_ckpt <- file.path(out_dir, "aib_checkpoint_investment.rds")
use_ckpt <- arg_flag("--from-checkpoint") && file.exists(trade_ckpt)
# A checkpoint is reused only when it was built with the same sets, variants, nodes and model settings,
# and at the six supply-chain stages (a three-stage checkpoint has other stage shocks and partner scores).
fingerprint <- list(demand = sort(demand_set$iso), partners = sort(partner_set$iso), semantics = semantics, constraints = constraints,
                    phys_nodes = sort(phys_nodes), nodes = sort(opt_nodes), shocks = cfg$shocks, scenarios = scenarios,
                    optimization = opt, trade = cfg$trade, production_proxies = cfg$production_proxies, supply_constraint = sc_cfg,
                    stages = aib_stage_levels())
check_fingerprint <- function(ck_fp, fp, path) {
  if (is.null(ck_fp)) stop(path, " has no settings fingerprint; rerun without --from-checkpoint.")
  differ <- names(fp)[!vapply(names(fp), function(k) identical(ck_fp[[k]], fp[[k]]), logical(1))]
  if (length(differ)) stop(path, " was built with other settings (", paste(differ, collapse = ", "), "); rerun without --from-checkpoint.")
}
if (use_ckpt) {
  ck <- readRDS(trade_ckpt)
  check_fingerprint(ck$fingerprint, fingerprint, trade_ckpt)
  ck$fingerprint <- NULL
  for (nm in names(ck)) assign(nm, ck[[nm]])
  step("Trade model from checkpoint ", trade_ckpt)
} else {
  res_summary <- list(); res_frontier <- list(); res_partners <- list(); res_supply <- list()
  res_flows <- list(); res_flows_ex <- list(); res_growth <- list(); importer_loss <- list()
  head_problems <- stats::setNames(lapply(constraints, function(x) list()), constraints)
  con_order <- c(intersect("trade_only", constraints), setdiff(constraints, "trade_only"))
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
    is_head_key <- any(headline_keys$node_id == nid & headline_keys$shock_key == key)
    for (sem in semantics) {
      shared <- NULL  # the unconstrained run, reused where no physical data constrain the node
      for (con in con_order) {
        physical <- identical(con, "production_constrained") && nid %in% phys_nodes
        if (!physical && !is.null(shared)) {
          prob <- shared$prob
          r <- shared$r
        } else {
          prob <- node_problem(nid, key, sem, con)
          if (is.null(prob)) next
          rcap <- run_cap_of(nid, key)
          r <- aib_run_node_shock(prob, scenarios, caps, rcap, sc_obj, tolerances, neutral,
                                  existing_growth = growth, tolerance_stability = as.numeric(aib_or(opt$tolerance_stability, 1e-6)),
                                  existing_cap = if (is_div_key(nid, key)) rcap else 1)
          if (!physical) shared <- list(prob = prob, r = r)
        }
        id <- paste(k, sem, con)
        res_summary[[id]] <- vtag(r$summary, nid, key, sem, con)
        res_frontier[[id]] <- vtag(r$frontier, nid, key, sem, con)
        res_supply[[id]] <- vtag(r$supply, nid, key, sem, con)
        if (!is.null(r$partnerships) && nrow(r$partnerships)) {
          p <- r$partnerships
          p$export_opportunity <- sc_obj$op_raw(p$exporter_iso, p$importer_iso)
          p$compatibility <- sc_obj$compat(p$exporter_iso, p$importer_iso)
          res_partners[[id]] <- vtag(p, nid, key, sem, con)
        }
        if (is_head_key && identical(sem, headline_semantics)) {
          head_problems[[con]][[nid]] <- prob
          res_flows[[id]] <- vtag(r$flows, nid, key, sem, con)
          res_flows_ex[[id]] <- vtag(r$flows_existing, nid, key, sem, con)
          if (identical(con, headline_constraint)) {
            importer_loss[[nid]] <- tibble::tibble(node_id = nid, importer_iso = names(prob$lost), lost_share = as.numeric(prob$lost),
                                                   demand_share = as.numeric(prob$demand), lost_usd = as.numeric(prob$lost) * prob$D)
            g <- aib_existing_growth_sensitivity(prob, scenarios, growths, sc_obj, tolerances, neutral)
            res_growth[[nid]] <- vtag(g, nid, key, sem, con)
          }
        }
      }
    }
    if (k %% 50 == 0) step("  ", k, "/", nrow(runs), " node-shock runs (", round(as.numeric(difftime(Sys.time(), t_trade, units = "mins")), 1), " min)")
  }
  summary_all <- dplyr::bind_rows(res_summary)
  frontier_all <- dplyr::bind_rows(res_frontier)
  partners_all <- dplyr::bind_rows(res_partners)
  supply_all <- dplyr::bind_rows(res_supply)
  flows_head_all <- dplyr::bind_rows(res_flows)
  flows_ex_head_all <- dplyr::bind_rows(res_flows_ex)
  growth_runs <- dplyr::bind_rows(res_growth)
  importer_loss <- dplyr::bind_rows(importer_loss)
  step("Trade model done in ", round(as.numeric(difftime(Sys.time(), t_trade, units = "mins")), 1), " min; ",
       sum(summary_all$status_optimized != 0 | summary_all$status_existing != 0), " variant runs with a solver problem")
  saveRDS(list(summary_all = summary_all, frontier_all = frontier_all, partners_all = partners_all, supply_all = supply_all,
               flows_head_all = flows_head_all, flows_ex_head_all = flows_ex_head_all, growth_runs = growth_runs,
               importer_loss = importer_loss, head_problems = head_problems, fingerprint = fingerprint), trade_ckpt)
}
pick_variant <- function(tbl, con = headline_constraint, sem = headline_semantics) {
  if (is.null(tbl) || !nrow(tbl)) return(tbl)
  tbl[tbl$supply_constraint == con & tbl$shock_semantics == sem, , drop = FALSE]
}
summary_runs <- pick_variant(summary_all)
frontier_runs <- pick_variant(frontier_all)
partners_runs <- pick_variant(partners_all)
supply_runs <- pick_variant(supply_all)
headline_problems <- head_problems[[headline_constraint]]

# ---- Trade outputs ---------------------------------------------------------------------------------
node_label <- nodes %>% dplyr::select(node_id, node_label = sub_sector, techs, node_stage = supply_chain)
ids_cols <- function(t) dplyr::mutate(t, demand_scope = demand_set$id, candidate_universe = partner_set$id, universe = universe_id)
qual_cols <- quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, data_quality)
ns <- inputs$node_summary %>% dplyr::select(node_id, world_trade, top_exporter, top_exporter_share, china_export_share, export_hhi)
vkeys <- c("shock_semantics", "supply_constraint")
node_rows <- targets %>%
  dplyr::inner_join(summary_all, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(qual_cols, by = "node_id") %>%
  dplyr::left_join(ns, by = "node_id") %>%
  dplyr::mutate(in_strategic_scope = node_id %in% scope_nodes)
agg_by_variant <- function(rows, sup) {
  dplyr::bind_rows(lapply(split(rows, paste(rows$shock_semantics, rows$supply_constraint)), function(r) {
    sv <- sup[sup$shock_semantics == r$shock_semantics[1] & sup$supply_constraint == r$supply_constraint[1], , drop = FALSE] %>%
      dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, tech, supply_chain), by = c("node_id", "shock_key"), relationship = "many-to-many")
    conc <- dplyr::bind_rows(
      aib_aggregate_concentration(sv, c("tech", "supply_chain")) %>% dplyr::mutate(level = "stage"),
      aib_aggregate_concentration(sv, "tech") %>% dplyr::mutate(level = "technology")
    )
    aib_aggregate_levels(r) %>%
      dplyr::left_join(conc, by = c("level", "tech", "supply_chain", "shock_id", "scenario")) %>%
      dplyr::mutate(shock_semantics = r$shock_semantics[1], supply_constraint = r$supply_constraint[1])
  }))
}
stress <- dplyr::bind_rows(
  node_rows %>% dplyr::mutate(level = "subsector", n_nodes = 1L),
  agg_by_variant(node_rows, supply_all) %>%
    dplyr::left_join(dplyr::distinct(targets, shock_id, shock_label, shock_type, shock_level, shock_fraction), by = "shock_id")
) %>%
  dplyr::mutate(in_strategic_scope = ifelse(level == "subsector", node_id %in% scope_nodes, tech %in% scope_techs)) %>%
  ids_cols() %>%
  dplyr::relocate(level, demand_scope, candidate_universe, universe, tech, supply_chain, sub_sector, node_id, shock_id, shock_label,
                  shock_level, target_iso, shock_semantics, supply_constraint, scenario)

# Frontier (a node shared by technologies with different largest suppliers keeps one run per target).
frontier_nodes_all <- frontier_all %>%
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(summary_all %>% dplyr::select(node_id, shock_key, shock_semantics, supply_constraint, scenario, demand_usd, shock_exposure),
                   by = c("node_id", "shock_key", vkeys, "scenario"))
fkeys <- c("node_id", "shock_key", "shock_id", vkeys, "scenario")
fmin <- aib_frontier_minimum(frontier_nodes_all, opt$frontier_coverage_tolerance, aib_or(opt$frontier_acceptable_coverage, 0.8), keys = fkeys)
frontier_out <- dplyr::bind_rows(
  frontier_nodes_all %>% dplyr::left_join(fmin, by = fkeys) %>% dplyr::left_join(node_label, by = "node_id") %>% dplyr::mutate(level = "subsector"),
  frontier_nodes_all %>%
    dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, tech, supply_chain), by = c("node_id", "shock_key", "shock_id"), relationship = "many-to-many") %>%
    dplyr::group_by(tech, supply_chain, shock_id, shock_semantics, supply_constraint, scenario, max_supplier_share) %>%
    dplyr::summarise(optimized_coverage = sum(optimized_coverage * demand_usd) / sum(demand_usd),
                     existing_coverage = sum(existing_coverage * demand_usd) / sum(demand_usd),
                     static_coverage = sum(static_coverage * demand_usd) / sum(demand_usd),
                     instantaneous_n1_retained_coverage = sum(instantaneous_n1_retained_coverage * demand_usd, na.rm = TRUE) / sum(demand_usd),
                     n1_reoptimized_coverage = sum(n1_reoptimized_coverage * demand_usd, na.rm = TRUE) / sum(demand_usd),
                     demand_usd = sum(demand_usd), n_nodes = dplyr::n_distinct(node_id), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain, shock_id, shock_semantics, supply_constraint, scenario) %>%
    dplyr::mutate(coverage_loss_vs_unconstrained = pmax(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage, 0)) %>%
    dplyr::ungroup() %>% dplyr::mutate(level = "stage")
) %>% ids_cols() %>% dplyr::select(-dplyr::any_of("shock_key")) %>%
  dplyr::mutate(in_strategic_scope = ifelse(is.na(node_id), tech %in% scope_techs, node_id %in% scope_nodes)) %>%
  dplyr::relocate(level, demand_scope, candidate_universe, node_id, node_label, tech, supply_chain, shock_id, shock_semantics,
                  supply_constraint, scenario, max_supplier_share)
frontier_nodes <- pick_variant(frontier_nodes_all)

ex_cols <- inputs$exporter_metrics %>% dplyr::select(node_id, exporter_iso, exporter_rca = export_rca, exporter_world_share = world_share,
                                                     exporter_production_share = production_share)
partners_out <- partners_all %>%
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, shock_label, target_iso, cap), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(ex_cols, by = c("node_id", "exporter_iso")) %>%
  dplyr::left_join(node_label, by = "node_id") %>%
  dplyr::mutate(max_supplier_share = dplyr::coalesce(cap, headline_cap), in_strategic_scope = node_id %in% scope_nodes) %>%
  dplyr::select(-cap) %>%
  ids_cols() %>%
  dplyr::select(-shock_key) %>%
  dplyr::relocate(demand_scope, candidate_universe, universe, node_id, node_label, techs, node_stage, shock_id, shock_label,
                  shock_semantics, supply_constraint, scenario, exporter_iso, importer_iso, relationship)
partners_head <- pick_variant(partners_out)

# Physical validation per node: data status and the divergence of physical from trade concentration.
div_thr <- aib_or(sc_cfg$divergence_threshold, 0.25)
phys_status <- quality %>% dplyr::filter(is.na(aggregated_into)) %>%
  dplyr::select(node_id, dplyr::any_of(c("production_top", "production_top_share", "production_china_share", "production_source")),
                top_exporter_share, china_export_share) %>%
  dplyr::left_join(phys_cov, by = "node_id") %>%
  dplyr::mutate(
    physical_data = dplyr::case_when(node_id %in% phys_nodes ~ "complete", !is.na(physical_completeness) ~ "incomplete", TRUE ~ "none"),
    physical_divergence = pmax(abs(dplyr::coalesce(production_top_share, NA_real_) - top_exporter_share),
                               abs(dplyr::coalesce(production_china_share, NA_real_) - dplyr::coalesce(china_export_share, 0)), na.rm = TRUE),
    physical_divergence = ifelse(physical_data == "none", NA_real_, physical_divergence),
    physically_divergent = dplyr::coalesce(physical_divergence >= div_thr, FALSE)
  )
unlisted <- inputs$exporter_metrics %>% dplyr::filter(node_id %in% phys_cov$node_id) %>%
  dplyr::group_by(node_id) %>%
  dplyr::summarise(trade_share_without_physical_data = sum(world_share[is.na(production_share) | production_share <= producer_min_share], na.rm = TRUE),
                   .groups = "drop")
phys_status <- phys_status %>% dplyr::left_join(unlisted, by = "node_id")
dq_out <- quality %>% dplyr::left_join(nodes %>% dplyr::select(node_id, primary_tech), by = "node_id") %>%
  dplyr::left_join(phys_status %>% dplyr::select(node_id, physical_completeness, physical_data, physical_divergence, physically_divergent,
                                                 trade_share_without_physical_data), by = "node_id") %>%
  dplyr::mutate(in_strategic_scope = node_id %in% scope_nodes) %>%
  dplyr::relocate(node_id, sub_sector, supply_chain, techs, data_quality, optimize, physical_data, physically_divergent, flags)

step("Writing trade outputs")
write_out(stress, "allied_trade_stress_test.csv")
write_out(partners_out, "allied_trade_partnerships.csv")
write_out(frontier_out, "allied_trade_frontier.csv")
write_out(dq_out %>% ids_cols(), "allied_subsector_data_quality.csv")

if (arg_flag("--skip-investment")) {
  step("Stopped after the trade model (--skip-investment).")
  quit(save = "no")
}

# ---- Investment partnership prioritization (headline semantics; both constraint modes) -------------
inv <- cfg$investment
inv_shocks <- intersect(unlist(inv$shocks), vapply(cfg$shocks, `[[`, "", "id"))
inv_scenarios <- intersect(unlist(aib_or(inv$scenarios, headline_scenario)), scenarios)
edges_sc <- inputs$supply_chain_edges
material_edges <- edges_sc[grepl("material link", edges_sc$basis), , drop = FALSE]
# Each host's synthetic increment is capped at host_scale_multiple x its current scale in the node
# (aib_host_increment()); the compact tests below cap their increments the same way.
host_scale_multiple <- as.numeric(aib_or(inv$host_scale_multiple, Inf))
host_inc <- function(inc, rows) {
  aib_host_increment(inc, if ("host_scale_share" %in% names(rows)) rows$host_scale_share else rep(Inf, nrow(rows)), host_scale_multiple)
}
# Host types (scaling, specialty, development): the income groups and the Development Potential Index
# come with the inputs when scripts/31 put them there, else from the OSI pipeline's own inputs.
ht <- aib_or(inv$host_types, list())
ht_data <- if (!is.null(inputs$country_income) || !is.null(inputs$dev_index)) {
  list(income = inputs$country_income, dev = inputs$dev_index, sources = c(income = "aib_inputs.rds", dev = "aib_inputs.rds"))
} else {
  aib_read_host_type_data(abs_path(arg_value("--raw-dir", config$raw_data_dir)), abs_path(arg_value("--processed-dir", config$processed_dir)))
}
if (!is.null(ht_data$income) && length(ht$income_overrides)) {
  ov <- unlist(ht$income_overrides)
  ht_data$income[names(ov)] <- as.character(ov)
}
scaling_min_share <- if (is.null(ht$scaling_min_share)) NULL else as.numeric(ht$scaling_min_share)
dev_hosts_n <- as.integer(aib_or(ht$development_hosts, 3))
host_types_of <- function(rows, keys) {
  if (is.null(rows) || !nrow(rows)) return(rows)
  dv <- rows %>% dplyr::distinct(node_id, host_iso)
  dv$development_index <- NA_real_
  for (n in unique(dv$node_id)) {
    k <- which(dv$node_id == n)
    sc <- score_cell(n)
    dv$development_index[k] <- aib_host_dev_index(dv$host_iso[k], sc$techs, sc$stage, ht_data$dev)
  }
  di <- dv$development_index[match(paste(rows$node_id, rows$host_iso), paste(dv$node_id, dv$host_iso))]
  aib_host_types(rows, ht_data$income, di, keys = keys,
                 specialty_min_rca = as.numeric(aib_or(ht$specialty_min_rca, 2)),
                 specialty_min_exports = as.numeric(aib_or(ht$specialty_min_exports_usd, 1e8)),
                 specialty_income = unlist(aib_or(ht$specialty_income, "High income")),
                 development_income = unlist(aib_or(ht$development_income, c("Lower middle income", "Upper middle income"))))
}
# Alignment screen (R/aib/aib_host_screen.R): the candidate partners that may host. The UNGA blocs come
# with the inputs when scripts/31 put them there, else from the OSI pipeline's processed tables. It
# applies before the shortlist and the host types; the largest screened hosts are simulated apart.
hs_cfg <- aib_or(inv$host_screen, list())
hs_mode <- aib_or(hs_cfg$mode, "none")
host_bloc <- if (!is.null(inputs$country_bloc)) list(bloc = inputs$country_bloc, source = "aib_inputs.rds") else
  aib_read_host_bloc(abs_path(arg_value("--processed-dir", config$processed_dir)))
host_screen <- aib_host_screen(partner_set$iso, demand_set$iso, host_bloc$bloc, mode = hs_mode,
                               exclude_blocs = unlist(aib_or(hs_cfg$exclude_blocs, "China-leaning")))
screened_iso <- host_screen$iso3[host_screen$screened]
n_screened_sim <- as.integer(aib_or(hs_cfg$screened_hosts_simulated, 5))
step("Host screen (", hs_mode, "): ", length(screened_iso), " of ", nrow(host_screen), " candidate partners are not investment hosts")
write_out(host_screen %>% dplyr::left_join(inputs$country_names, by = "iso3") %>% dplyr::relocate(iso3, country) %>%
            dplyr::mutate(screen_mode = hs_mode) %>% ids_cols(), "aib_host_screen.csv")
inv_hosts <- list(); inv_pairs <- list(); inv_screened <- list()
t_inv <- Sys.time()
inv_fingerprint <- c(fingerprint, list(investment = inv))
# The trade model's settings must match (checked above); a checkpoint built with other investment
# settings only is simulated again from the reused trade model.
inv_ck <- if (use_ckpt && file.exists(inv_ckpt)) readRDS(inv_ckpt) else NULL
if (!is.null(inv_ck)) {
  check_fingerprint(inv_ck$fingerprint[names(fingerprint)], fingerprint, inv_ckpt)
  if (!identical(inv_ck$fingerprint$investment, inv)) {
    step("Investment checkpoint ", inv_ckpt, " was built with other investment settings: simulating again")
    inv_ck <- NULL
  }
}
if (!is.null(inv_ck)) {
  inv_hosts <- list(inv_ck$inv_hosts); inv_pairs <- list(inv_ck$inv_pairs); inv_screened <- list(inv_ck$inv_screened)
  inv_plan <- character()
  step("Investment from checkpoint ", inv_ckpt)
} else {
  inv_plan <- inv_shocks
}
rm(inv_ck)
for (con in constraints) for (inv_sc in inv_scenarios) for (shk in inv_plan) {
  keys <- targets %>% dplyr::filter(shock_id == shk) %>% dplyr::distinct(node_id, shock_key)
  gap <- summary_all %>% dplyr::filter(scenario == inv_sc, supply_constraint == con, shock_semantics == headline_semantics) %>%
    dplyr::inner_join(keys, by = c("node_id", "shock_key")) %>%
    dplyr::filter(residual_gap >= inv$min_residual_gap)
  # Nodes without physical data give the same answer under both modes: run them once (trade_only).
  if (identical(con, "production_constrained") && "trade_only" %in% constraints) gap <- gap[gap$node_id %in% phys_nodes, , drop = FALSE]
  step("Investment (", shk, ", ", inv_sc, ", ", con, "): ", nrow(gap), " node runs with a residual gap of at least ", 100 * inv$min_residual_gap, "%")
  for (i in seq_len(nrow(gap))) {
    nid <- gap$node_id[i]
    key <- gap$shock_key[i]
    prob <- if (shk == headline_shock && !is.null(head_problems[[con]][[nid]]) &&
                identical(key, headline_keys$shock_key[headline_keys$node_id == nid][1])) head_problems[[con]][[nid]] else node_problem(nid, key, headline_semantics, con)
    if (is.null(prob)) next
    hosts <- aib_eligible_hosts(nid, inputs$exporter_metrics, material_edges, exclude = c(names(prob$shocked), unlist(inv$exclude_hosts)),
                                universe = partner_set$iso, min_rca = inv$host_min_rca, min_share = inv$host_min_world_share,
                                min_production_share = inv$host_min_production_share, rca_cap = opt$rca_cap,
                                min_exports_usd = inv$host_min_exports_usd, min_material_share = inv$host_min_material_share)
    if (!nrow(hosts)) next
    # The alignment screen first, so a screened economy is in no ranking or host type; its largest hosts
    # are simulated apart, unranked, for the alignment cost.
    screened_h <- hosts[hosts$host_iso %in% screened_iso, , drop = FALSE]
    hosts <- aib_host_shortlist(hosts[!hosts$host_iso %in% screened_iso, , drop = FALSE], nrow(hosts))
    # The largest hosts by current scale, and the leading specialty and development candidates wherever they rank.
    keep <- seq_len(min(nrow(hosts), inv$max_hosts_per_node))
    if (!is.null(ht_data$income) && nrow(hosts)) {
      ty <- host_types_of(dplyr::mutate(hosts, node_id = nid, delta_hhi = 0), "node_id")
      keep <- sort(union(keep, which(ty$specialty_rank <= inv$top_hosts | ty$development_rank <= dev_hosts_n)))
    }
    hosts <- hosts[keep, , drop = FALSE]
    sc_obj <- node_scores(nid)
    sc_cell <- score_cell(nid)
    fs_env <- aib_node_dyad_lookup(inputs$fs_index, sc_cell$techs, sc_cell$stage)
    fs_lookup <- function(a, b) aib_lookup_pairs_raw(fs_env, a, b)
    bf <- flows_head_all[flows_head_all$node_id == nid & flows_head_all$supply_constraint == con & flows_head_all$scenario == inv_sc, , drop = FALSE]
    rc <- run_cap_of(nid, key)
    r <- aib_investment_node(prob, inv_sc, hosts, as.numeric(unlist(inv$increments)), as.numeric(inv$headline_increment),
                             rc, min(as.numeric(inv$diversified_max_supplier_share), rc), sc_obj, fs_lookup, tolerances, neutral,
                             pair_hosts = inv$top_hosts, tie_resilience = inv$tie_resilience, tie_hhi = inv$tie_hhi,
                             base_flows = if (shk == headline_shock && nrow(bf)) bf[, c("e", "m", "f", "base")] else NULL,
                             scale_multiple = host_scale_multiple, scaling_min_share = scaling_min_share)
    if (!is.null(r$hosts)) inv_hosts[[length(inv_hosts) + 1]] <- dplyr::mutate(r$hosts, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, supply_constraint = con, .before = 1)
    if (!is.null(r$pairs) && nrow(r$pairs)) inv_pairs[[length(inv_pairs) + 1]] <- dplyr::mutate(r$pairs, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, supply_constraint = con, .before = 1)
    # The screened hosts: the same simulation, no investment pairs, no rank.
    if (nrow(screened_h) && n_screened_sim > 0) {
      rs <- aib_investment_node(prob, inv_sc, aib_host_shortlist(screened_h, n_screened_sim), as.numeric(unlist(inv$increments)),
                                as.numeric(inv$headline_increment), rc, min(as.numeric(inv$diversified_max_supplier_share), rc), sc_obj, fs_lookup,
                                tolerances, neutral, pair_hosts = 0, tie_resilience = inv$tie_resilience, tie_hhi = inv$tie_hhi,
                                scale_multiple = host_scale_multiple, scaling_min_share = scaling_min_share)
      if (!is.null(rs$hosts)) {
        inv_screened[[length(inv_screened) + 1]] <- rs$hosts %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c("host_rank", "gain_shortlist", "scaling_rank")), ~ NA_integer_),
                        dplyr::across(dplyr::any_of("rank_basis"), ~ NA_character_)) %>%
          dplyr::mutate(node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, supply_constraint = con, .before = 1)
      }
    }
  }
}
inv_hosts <- dplyr::bind_rows(inv_hosts)
inv_pairs <- dplyr::bind_rows(inv_pairs)
inv_screened <- dplyr::bind_rows(inv_screened)
# Copy the shared (no physical data) results into the production-constrained mode.
if (length(inv_plan) && all(c("trade_only", "production_constrained") %in% constraints) && nrow(inv_hosts)) {
  inv_hosts <- dplyr::bind_rows(inv_hosts, inv_hosts %>% dplyr::filter(supply_constraint == "trade_only", !node_id %in% phys_nodes) %>%
                                  dplyr::mutate(supply_constraint = "production_constrained"))
  if (nrow(inv_pairs)) inv_pairs <- dplyr::bind_rows(inv_pairs, inv_pairs %>% dplyr::filter(supply_constraint == "trade_only", !node_id %in% phys_nodes) %>%
                                                       dplyr::mutate(supply_constraint = "production_constrained"))
  if (nrow(inv_screened)) inv_screened <- dplyr::bind_rows(inv_screened, inv_screened %>% dplyr::filter(supply_constraint == "trade_only", !node_id %in% phys_nodes) %>%
                                                             dplyr::mutate(supply_constraint = "production_constrained"))
}
if (length(inv_plan)) saveRDS(list(inv_hosts = inv_hosts, inv_pairs = inv_pairs, inv_screened = inv_screened, fingerprint = inv_fingerprint), inv_ckpt)
step("Investment done in ", round(as.numeric(difftime(Sys.time(), t_inv, units = "mins")), 1), " min: ", nrow(inv_hosts), " host x increment rows")
ikeys <- c("node_id", "shock_key", "shock_id", "scenario", "supply_constraint")
# Specialty and development hosts beside the scaling ranking (classified after the simulation, so they
# follow the income and index data even from a checkpoint).
if (nrow(inv_hosts)) {
  inv_hosts <- host_types_of(inv_hosts %>% dplyr::select(-dplyr::any_of(c("income_group", "development_index", "is_specialty", "is_development",
                                                                           "specialty_rank", "development_rank", "host_type"))),
                             c(ikeys, "increment"))
  if (!"is_scaling" %in% names(inv_hosts)) inv_hosts$is_scaling <- NA
  h10 <- inv_hosts[abs(inv_hosts$increment - inv$headline_increment) < 1e-9 & inv_hosts$shock_id == headline_shock &
                     inv_hosts$scenario == headline_scenario & inv_hosts$supply_constraint == headline_constraint, , drop = FALSE]
  step("Host types (", headline_shock, ", headline): ", dplyr::n_distinct(h10$node_id), " nodes; with a scaling host ",
       dplyr::n_distinct(h10$node_id[h10$is_scaling %in% TRUE]), ", a specialty host ", dplyr::n_distinct(h10$node_id[h10$is_specialty]),
       ", a development host ", dplyr::n_distinct(h10$node_id[h10$is_development]))
}
# What the alignment screen gives up, per node x shock x variant x increment: the best screened host's
# gain (with the diversified supplier limit, the gain the ranking reads) minus the best eligible host's.
alignment <- aib_alignment_cost(inv_hosts, inv_screened, c(ikeys, "increment"))
if (nrow(alignment)) {
  a10 <- alignment[abs(alignment$increment - inv$headline_increment) < 1e-9 & alignment$shock_id == headline_shock &
                     alignment$scenario == headline_scenario & alignment$supply_constraint == headline_constraint, , drop = FALSE]
  step("Host screen (", headline_shock, ", headline): screened hosts simulated in ", nrow(a10), " nodes; a cost in ",
       sum(a10$alignment_cost > 0.005), " (largest ", sprintf("%.1f", 100 * max(c(0, a10$alignment_cost))), " pts)")
}
screened_out <- if (nrow(inv_screened)) inv_screened %>%
  dplyr::left_join(host_screen %>% dplyr::select(host_iso = iso3, bloc, screen_reason), by = "host_iso") %>%
  dplyr::left_join(alignment, by = c(ikeys, "increment")) %>%
  dplyr::left_join(dplyr::distinct(targets, node_id, shock_key, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(node_label, by = "node_id") %>%
  dplyr::mutate(shock_semantics = headline_semantics, in_strategic_scope = node_id %in% scope_nodes, host_screened = TRUE,
                analysis = "marginal capacity simulation of a screened host (not ranked)") %>%
  ids_cols() %>% dplyr::select(-shock_key) %>%
  dplyr::relocate(demand_scope, candidate_universe, universe, node_id, node_label, techs, node_stage, shock_id, target_iso,
                  shock_semantics, supply_constraint, scenario, increment, host_iso, bloc, screen_reason, alignment_cost) else inv_screened
write_out(screened_out, "allied_investment_screened_hosts.csv")
if (nrow(inv_hosts)) {
  top_inv <- inv_pairs %>% dplyr::group_by(dplyr::across(dplyr::all_of(c(ikeys, "host_iso")))) %>%
    dplyr::arrange(dplyr::desc(supply_from_new_capacity), .by_group = TRUE) %>%
    dplyr::summarise(top_investor_partners = paste(utils::head(sprintf("%s (FS %s)", investor_iso, ifelse(is.na(friendshore), "n/a", sprintf("%.2f", friendshore))), 3), collapse = "; "), .groups = "drop")
  inv_out <- inv_hosts %>% dplyr::left_join(top_inv, by = c(ikeys, "host_iso")) %>%
    dplyr::left_join(alignment, by = c(ikeys, "increment")) %>%
    dplyr::left_join(dplyr::distinct(targets, node_id, shock_key, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
    dplyr::left_join(node_label, by = "node_id") %>%
    dplyr::mutate(shock_semantics = headline_semantics, in_strategic_scope = node_id %in% scope_nodes,
                  analysis = "marginal capacity simulation (synthetic increment, not a project)") %>%
    ids_cols() %>% dplyr::select(-shock_key) %>%
    dplyr::relocate(demand_scope, candidate_universe, universe, node_id, node_label, techs, node_stage, shock_id, target_iso,
                    shock_semantics, supply_constraint, scenario, increment, host_rank, host_iso)
} else {
  inv_out <- inv_hosts
}
write_out(inv_out, "allied_investment_partnerships.csv")
inv_pairs_out <- if (nrow(inv_pairs)) inv_pairs %>%
  dplyr::left_join(inv_hosts %>% dplyr::filter(abs(increment - inv$headline_increment) < 1e-9) %>%
                     dplyr::select(dplyr::all_of(c(ikeys, "host_iso", "host_rank", "is_scaling"))), by = c(ikeys, "host_iso"), relationship = "many-to-one") else inv_pairs
write_out(inv_pairs_out %>% dplyr::left_join(node_label, by = "node_id") %>% ids_cols() %>% dplyr::select(-dplyr::any_of("shock_key")),
          "allied_investment_pairs.csv")
inv_hosts_head <- if (nrow(inv_hosts)) inv_hosts[inv_hosts$supply_constraint == headline_constraint, , drop = FALSE] else inv_hosts
inv_pairs_head <- if (nrow(inv_pairs_out)) inv_pairs_out[inv_pairs_out$supply_constraint == headline_constraint, , drop = FALSE] else inv_pairs_out

# ---- Emergent AIB network (headline variant, strategic scope) ---------------------------------------
# One network per scenario of the dashboard. Decoupling: over the supply shocks (the headline* fields are
# the headline shock, the cumulative ones sum every supply shock), with the headline shock's importer
# exposure. Managed Interdependence: the diversification shock alone (it removes no supplier, so it stays
# out of the supply shocks' sums), with its own importer exposure. Baseline: the observed network below.
build_network <- function(shock_ids, head_shock, loss) {
  net_targets <- targets %>% dplyr::filter(shock_id %in% shock_ids) %>% dplyr::distinct(node_id, shock_key, shock_id)
  node_shock <- summary_runs %>% dplyr::inner_join(net_targets, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
    dplyr::select(node_id, shock_id, scenario, shock_exposure) %>% dplyr::distinct()
  primary_key <- summary_runs %>% dplyr::inner_join(net_targets, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
    dplyr::filter(scenario == headline_scenario) %>% dplyr::group_by(node_id, shock_id) %>%
    dplyr::slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>% dplyr::select(node_id, shock_id, shock_key)
  trade_for_net <- partners_runs %>% dplyr::filter(node_id %in% scope_nodes) %>%
    dplyr::inner_join(primary_key, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
    dplyr::select(node_id, shock_id, scenario, exporter_iso, importer_iso, delta_usd, delta_share_of_demand, relationship)
  # The selected investment relationship of a node: its best scaling host (none where no host is one).
  invest_top <- if (nrow(inv_pairs_head)) inv_pairs_head %>% dplyr::filter(dplyr::coalesce(host_rank, 99L) == 1L, !is_scaling %in% FALSE,
                                                                           node_id %in% scope_nodes) %>%
    dplyr::semi_join(primary_key, by = c("node_id", "shock_id", "shock_key")) else NULL
  edges <- aib_network_pairs(trade_for_net, invest_top, nodes, node_shock, head_shock, cls$high_severity,
                             node_cells = inputs$node_cells, op_index = inputs$op_index, fs_index = inputs$fs_index)
  countries <- aib_network_countries(edges, aib_importer_exposure(loss, scope_nodes, scenarios))
  if (!is.null(countries)) countries <- countries %>% dplyr::left_join(inputs$country_names, by = "iso3") %>% dplyr::relocate(iso3, country, scenario)
  list(edges = edges, countries = countries)
}
net_cols <- function(t) if (is.null(t)) t else t %>% ids_cols() %>%
  dplyr::mutate(shock_semantics = headline_semantics, supply_constraint = headline_constraint, strategic_scope = scope_mode)
supply_shock_ids <- setdiff(vapply(cfg$shocks, `[[`, "", "id"), div_id)
net <- build_network(supply_shock_ids, headline_shock, importer_loss)
edges <- net$edges
countries <- net$countries
write_out(net_cols(edges), "emergent_aib_edges.csv")
write_out(net_cols(countries), "emergent_aib_country_summary.csv")

# Managed Interdependence: each importer's lost supply under the diversification shock, from the node
# problem alone (no LP), and the network of that shock.
div_keys <- if (is.na(div_id)) tibble::tibble(node_id = character(), shock_key = character()) else
  summary_runs %>% dplyr::filter(scenario == headline_scenario) %>%
  dplyr::semi_join(targets %>% dplyr::filter(shock_id == div_id), by = c("node_id", "shock_key")) %>%
  dplyr::distinct(node_id, shock_key) %>% dplyr::arrange(node_id)
# Each problem carries its run's supplier-share cap (share_cap, a share of the scope's demand): the compact
# tests (aib_compact_trade_coverage(), aib_anchor_terms()) keep it, as the scenario's own states B and C do.
problems_managed <- list()
for (i in seq_len(nrow(div_keys))) {
  p <- node_problem(div_keys$node_id[i], div_keys$shock_key[i])
  if (!is.null(p)) {
    p$share_cap <- run_cap_of(div_keys$node_id[i], div_keys$shock_key[i])
    problems_managed[[div_keys$node_id[i]]] <- p
  }
}
importer_loss_managed <- dplyr::bind_rows(lapply(names(problems_managed), function(nid) {
  p <- problems_managed[[nid]]
  tibble::tibble(node_id = nid, importer_iso = names(p$lost), lost_share = as.numeric(p$lost), demand_share = as.numeric(p$demand),
                 lost_usd = as.numeric(p$lost) * p$D)
}))
net_managed <- if (length(problems_managed)) build_network(div_id, div_id, importer_loss_managed) else list(edges = NULL, countries = NULL)
if (!is.null(net_managed$edges)) {
  write_out(net_cols(net_managed$edges), "emergent_aib_edges_managed.csv")
  write_out(net_cols(net_managed$countries), "emergent_aib_country_summary_managed.csv")
}

# Baseline: trade as it is in the reference year, before any shock. Each optimized node's observed supply
# to the demand scope by exporter (every exporter; the HHI and largest supplier of res$stress
# baseline_* are read on this basis), and the observed supplier -> buyer network of the strategic-scope
# nodes as a pseudo-shock "baseline" (scenario "observed"), keeping the pairs whose summed share of node
# demand is at least `baseline_min_share`; the country roles are read from the pairs kept.
baseline_min_share <- as.numeric(aib_or(cfg$network$baseline_min_share, 0.005))
observed_supply <- aib_observed_supply(inputs$flows, demand_set$iso, opt_nodes)
obs_net <- aib_observed_network_inputs(inputs$flows, demand_set$iso, intersect(scope_nodes, opt_nodes))
edges_baseline <- aib_network_min_share(
  aib_network_pairs(obs_net$trade, NULL, nodes, obs_net$node_shock, "baseline", cls$high_severity,
                    node_cells = inputs$node_cells, op_index = inputs$op_index, fs_index = inputs$fs_index),
  baseline_min_share)
countries_baseline <- aib_network_countries(edges_baseline)
if (!is.null(countries_baseline)) countries_baseline <- countries_baseline %>% dplyr::left_join(inputs$country_names, by = "iso3") %>%
  dplyr::relocate(iso3, country, scenario)
step("Baseline (observed): ", dplyr::n_distinct(observed_supply$node_id), " nodes, USD ", round(sum(observed_supply$supply_usd) / 1e9, 1),
     " bn supplied to the demand scope; network ", NROW(edges_baseline), " pairs with at least ", 100 * baseline_min_share,
     "% of node demand, ", NROW(countries_baseline), " economies")
write_out(observed_supply %>% dplyr::left_join(node_label, by = "node_id") %>% ids_cols() %>% dplyr::relocate(node_id, node_label),
          "aib_observed_supply.csv")
obs_cols <- function(t) if (is.null(t)) t else t %>% ids_cols() %>% dplyr::mutate(strategic_scope = scope_mode, min_pair_share = baseline_min_share)
write_out(obs_cols(edges_baseline), "emergent_aib_edges_baseline.csv")
write_out(obs_cols(countries_baseline), "emergent_aib_country_summary_baseline.csv")

# ---- Node classification and Sectoral Compacts (per supply-constraint mode) -------------------------
cmp <- cfg$compacts
solver_tl <- as.numeric(aib_or(cmp$solver_time_limit, 5))
chain_edges <- aib_chain_edges(inputs$node_cells_detail, cfg$supply_chain_links)
targets_cov <- sort(as.numeric(unlist(cmp$coverage_targets)))
mid_target <- targets_cov[ceiling(length(targets_cov) / 2)]
ex_metrics <- inputs$exporter_metrics
manage_loss <- function(fr) fr %>% dplyr::group_by(node_id) %>%
  dplyr::summarise(coverage_loss_at_manage_cap = {
    k <- which.min(abs(max_supplier_share - cls$manage_cap))
    max(optimized_coverage[which.max(max_supplier_share)] - optimized_coverage[k], 0)
  }, .groups = "drop")
classify_variant <- function(con, sc = headline_scenario, shk = headline_shock, existing_override = NULL, sem = headline_semantics) {
  keys_s <- targets %>% dplyr::filter(shock_id == shk) %>% dplyr::distinct(node_id, shock_key)
  sm <- summary_all %>% dplyr::filter(supply_constraint == con, shock_semantics == sem, scenario == sc) %>%
    dplyr::inner_join(keys_s, by = c("node_id", "shock_key")) %>%
    dplyr::group_by(node_id) %>% dplyr::slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
  if (!is.null(existing_override)) {
    sm <- sm %>% dplyr::left_join(existing_override, by = "node_id") %>%
      dplyr::mutate(existing_coverage = dplyr::coalesce(existing_g, existing_coverage),
                    coordination_dividend = pmax(optimized_coverage - existing_coverage, 0))
  }
  ml <- manage_loss(frontier_nodes_all %>% dplyr::filter(supply_constraint == con, shock_semantics == sem, shock_id == shk, scenario == sc) %>%
                      dplyr::semi_join(sm, by = c("node_id", "shock_key")))
  nr <- nodes %>% dplyr::select(node_id, sub_sector, techs, supply_chain) %>%
    dplyr::left_join(quality %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, data_quality, optimize), by = "node_id") %>%
    dplyr::left_join(sm, by = "node_id") %>% dplyr::left_join(ml, by = "node_id") %>%
    dplyr::mutate(optimize = dplyr::coalesce(optimize, FALSE) & !is.na(shock_exposure))
  if (!is.null(only)) nr <- nr[nr$node_id %in% opt_nodes, , drop = FALSE]
  aib_classify_nodes(nr, cls)
}
gl <- function(g) if (is.infinite(g)) "unlimited" else sprintf("%gx", 1 + g)

# The compacts of one shock: each group of nodes (group_ids: compact id -> node ids, in the order of
# cls_rows) gets its demand anchors, exact minimum coalitions, greedy path, coalition tiers, functional
# roles, leave-one-out and coverage metrics from that shock's node problems (`problems`), importer loss
# (`il_all`: node_id, importer_iso, lost_share, lost_usd), optimized and existing-network flows
# (`flows_m`, `fx_m`: node_id, e, m, f, base) and ranked investment hosts (`hosts_head`), with the node
# classes of that shock (`cls_rows`, classify_variant()). Only nodes with a problem are solved;
# `unchanged` (compact id -> node_id, demand_usd) adds the compact's other nodes to the demand-weighted
# coverage metrics as unchanged (coverage 1, nothing lost), as the diversification scenario leaves them.
# The hosts whose simulated capacity the compact tests count in a node: the leading scaling hosts and
# the leading specialty hosts (top_hosts of each; a run without host types counts the top_hosts best),
# never development hosts. Each adds at most its own scale (host_inc()).
coalition_hosts <- function(hosts_head, n) {
  if (is.null(hosts_head)) return(NULL)
  h <- hosts_head[hosts_head$node_id == n, , drop = FALSE]
  if (!nrow(h)) return(h)
  sc <- if ("is_scaling" %in% names(h)) !(h$is_scaling %in% FALSE) else rep(TRUE, nrow(h))
  sp <- if ("specialty_rank" %in% names(h)) dplyr::coalesce(h$specialty_rank <= inv$top_hosts, FALSE) else rep(FALSE, nrow(h))
  h <- h[(sc & h$host_rank <= inv$top_hosts) | sp, , drop = FALSE]
  h$coalition_scaling <- (if ("is_scaling" %in% names(h)) !(h$is_scaling %in% FALSE) else TRUE) & h$host_rank <= inv$top_hosts
  h$coalition_specialty <- if ("specialty_rank" %in% names(h)) dplyr::coalesce(h$specialty_rank <= inv$top_hosts, FALSE) else FALSE
  h[order(h$host_rank), , drop = FALSE]
}
# The leading development hosts of a node (development_hosts of them).
development_hosts_of <- function(hosts_head, ids) {
  if (is.null(hosts_head) || !"development_rank" %in% names(hosts_head)) return(hosts_head[0, , drop = FALSE])
  hosts_head[hosts_head$node_id %in% ids & dplyr::coalesce(hosts_head$development_rank <= dev_hosts_n, FALSE), , drop = FALSE]
}
solve_compacts <- function(con, shk, cls_rows, group_ids, problems, il_all, flows_m, fx_m, hosts_head, unchanged = NULL) {
  compact_rows <- list(); compact_nodes <- list(); compact_members <- list(); compact_paths <- list(); compact_loo <- list()
  for (cid in names(group_ids)) {
    ids <- group_ids[[cid]]
    ids <- ids[ids %in% names(problems)]
    if (!length(ids)) next
    cn <- cls_rows[cls_rows$node_id %in% ids, , drop = FALSE]
    il <- il_all[il_all$node_id %in% ids, , drop = FALSE] %>%
      dplyr::group_by(importer_iso) %>% dplyr::summarise(lost = sum(lost_usd), .groups = "drop") %>% dplyr::arrange(dplyr::desc(lost))
    il$cum <- cumsum(il$lost) / sum(il$lost)
    n_anchor <- min(max(1, which(il$cum >= cmp$anchor_share)[1]), cmp$max_anchors, nrow(il), na.rm = TRUE)
    anchors <- il$importer_iso[seq_len(n_anchor)]
    build_ids <- cn$node_id[cn$needs_build]
    host_extra <- list()
    cand <- unique(unlist(lapply(ids, function(n) problems[[n]]$cand)))
    for (n in build_ids) {
      h <- coalition_hosts(hosts_head, n)
      if (!is.null(h) && nrow(h)) {
        hx <- stats::setNames(host_inc(as.numeric(cmp$coalition_increment), h), h$host_iso)
        hx <- hx[hx > 1e-12]
        if (length(hx)) {
          host_extra[[n]] <- hx
          cand <- union(cand, names(hx))
        }
      }
    }
    pool <- setdiff(cand, anchors)
    # Exact minimum coalitions at every target; the greedy path is kept for the "as countries join" view.
    exact <- lapply(targets_cov, function(t) aib_coalition_exact(problems, ids, anchors, pool, headline_scenario, host_extra, target = t,
                                                                 time_limit = solver_tl))
    names(exact) <- sprintf("%.0f", 100 * targets_cov)
    path <- aib_coalition_path(problems, ids, anchors, pool, headline_scenario, host_extra, target = max(targets_cov))
    greedy_mid <- aib_coalition_at(path, mid_target)
    mid <- exact[[sprintf("%.0f", 100 * mid_target)]]
    widest <- strsplit(utils::tail(path$members[!is.na(path$members)], 1), " ")[[1]]
    members <- if (isTRUE(mid$reached)) mid$members else widest
    coalition_method <- if (isTRUE(mid$reached)) "exact (binary program: fewest countries meeting the target at every node)" else
      sprintf("greedy_sufficient_coalition (the %.0f%% target is out of reach; the widest greedy set)", 100 * mid_target)
    if (isTRUE(mid$reached) && identical(mid$optimal, FALSE)) {
      coalition_method <- paste0(coalition_method, "; the solver hit its time limit: best coalition found, minimality not proven")
    }
    top_t <- exact[[length(exact)]]
    # Associate partners: the fewest additions to the core coalition that reach the highest target
    # (the core held in, so the tiers nest).
    nest <- if (isTRUE(mid$reached)) aib_coalition_exact(problems, ids, anchors, pool, headline_scenario, host_extra,
                                                         target = max(targets_cov), fixed = members, time_limit = solver_tl) else NULL
    associates <- if (!is.null(nest) && isTRUE(nest$reached)) setdiff(nest$members, members) else character()
    crit <- aib_coalition_critical(problems, ids, anchors, members, headline_scenario, host_extra, target = mid_target, candidates = pool)
    # Prospective hosts: members in the coalition only for the simulated capacity they would host (their
    # current supply is critical nowhere); kept apart from the core and never critical supply partners.
    basis <- aib_membership_basis(members, anchors, crit)
    prospective <- sort(basis$iso3[basis$membership_basis == "simulated capacity"])
    # Critical supply partners: members needed for the target that no single other candidate can
    # replace (each member's current supply removed in turn).
    subs <- if (isTRUE(mid$reached)) aib_coalition_substitution(problems, ids, anchors, members, pool, headline_scenario, host_extra, target = mid_target) else tibble::tibble()
    crit_members <- if (nrow(subs)) sort(setdiff(subs$iso3[subs$critical], prospective)) else character()
    # Every non-member whose loss pushes a node below the target is reported (the leave-one-out
    # guarantee); those supplying under `critical_min_supply_share` of the anchors' demand matter only
    # because the coalition sits near the target, and are listed apart as marginal.
    crit_floor <- as.numeric(aib_or(cmp$critical_min_supply_share, 0))
    if (nrow(crit)) crit$material <- crit$supply_share >= crit_floor
    crit_ext_all <- if (nrow(crit)) sort(unique(crit$iso3[crit$critical & !crit$member])) else character()
    crit_external <- if (nrow(crit)) sort(unique(crit$iso3[crit$critical & !crit$member & crit$material])) else character()
    crit_external_marginal <- setdiff(crit_ext_all, crit_external)
    ext_dep <- if (nrow(crit)) sort(setdiff(unique(crit$iso3[!crit$member & crit$supply_share >= cmp$role_min_share]), crit_ext_all)) else character()

    # Functional roles from the model results, and the coalition tiers.
    fl <- flows_m %>% dplyr::filter(node_id %in% ids)
    sup_share <- fl %>% dplyr::filter(m %in% anchors) %>% dplyr::group_by(node_id, e) %>% dplyr::summarise(f = sum(f), .groups = "drop") %>%
      dplyr::group_by(node_id) %>% dplyr::mutate(share = f / sum(f)) %>% dplyr::ungroup() %>%
      dplyr::left_join(ex_metrics %>% dplyr::select(node_id, e = exporter_iso, export_rca, world_share), by = c("node_id", "e")) %>%
      dplyr::left_join(cn %>% dplyr::select(node_id, supply_chain, node_classification), by = "node_id")
    role_rows <- list()
    add_role <- function(iso, role, basis) if (length(iso)) role_rows[[length(role_rows) + 1]] <<- tibble::tibble(iso3 = iso, role = role, basis = basis)
    add_role(intersect(anchors, members), "Demand anchor", "imports most of the compact's lost supply")
    sp <- sup_share %>% dplyr::filter(e %in% members, share >= cmp$role_min_share, dplyr::coalesce(export_rca, 0) >= 1)
    # Resource nodes: Extraction and Recovery (raw materials, mined or recycled: the dictionary's old
    # Upstream, its Raw Material rows); every other stage makes a specialized supplier.
    resource <- sp$supply_chain %in% aib_resource_stages()
    add_role(unique(sp$e[!resource]), "Specialized supplier", "RCA >= 1 and a material share of the anchors' optimized supply")
    add_role(unique(sp$e[resource]), "Resource supplier", "RCA >= 1 and a material share of the anchors' optimized supply of raw materials (extraction, recovery)")
    # Host roles in the nodes that need capacity: scaling and specialty hosts the coalition takes in (their
    # simulated capacity counted), and the leading development hosts whether members or not (never counted).
    hh <- dplyr::bind_rows(lapply(build_ids, function(n) coalition_hosts(hosts_head, n)))
    hh <- if (nrow(hh)) hh[hh$host_iso %in% members, , drop = FALSE] else NULL
    if (!is.null(hh) && nrow(hh)) {
      add_role(unique(hh$host_iso[hh$coalition_scaling]), "Scaling host",
               "established producer (carries the full increment or holds >= 5% of world exports or production) among the leading hosts of a node that needs capacity")
      add_role(unique(hh$host_iso[hh$coalition_specialty]), "Specialty host",
               "high-income specialist below scale (RCA >= 2 on >= USD 100m of exports) among the leading specialty hosts of a node that needs capacity; counted at its own scale")
    }
    dh <- development_hosts_of(hosts_head, build_ids)
    add_role(unique(dh$host_iso), "Development host",
             "middle-income economy below scale with revealed capability, among the leading hosts by Development Potential Index in a node that needs capacity; future capacity, not counted toward the target")
    tp <- ex_metrics %>% dplyr::filter(node_id %in% cn$node_id[cn$needs_build | cn$needs_manage_specialization],
                                       exporter_iso %in% members, dplyr::coalesce(export_rca, 0) >= 1, world_share >= 0.05)
    add_role(unique(tp$exporter_iso), "Technology partner", "revealed capability (RCA >= 1, >= 5% of world exports) where capacity or specialization is the need")
    supplies <- dplyr::bind_rows(sp %>% dplyr::transmute(node_id, iso3 = e), if (!is.null(hh) && nrow(hh)) hh %>% dplyr::transmute(node_id, iso3 = host_iso)) %>% dplyr::distinct()
    node_anchor <- il_all %>% dplyr::filter(node_id %in% ids) %>% dplyr::group_by(node_id) %>%
      dplyr::mutate(share = lost_share / sum(lost_share)) %>% dplyr::ungroup() %>%
      dplyr::filter(share >= cmp$node_anchor_share) %>% dplyr::transmute(node_id, iso3 = importer_iso)
    vertical <- supplies %>% dplyr::inner_join(node_anchor, by = "iso3", suffix = c("_sup", "_dep"), relationship = "many-to-many") %>%
      dplyr::filter(node_id_sup != node_id_dep)
    # Spanning stages: supplying nodes of two or more of the six stages (refined material and the
    # components made from it count as two, as they are two steps of the chain).
    stage_span <- sup_share %>% dplyr::filter(e %in% members) %>% dplyr::group_by(e) %>% dplyr::summarise(n = dplyr::n_distinct(supply_chain), .groups = "drop")
    add_role(intersect(members, union(unique(vertical$iso3), stage_span$e[stage_span$n > 1])), "Bridge partner",
             "supplies one node of the compact and depends on another, or spans its stages")
    add_role(crit_members, "Critical supply partner", sprintf("no single other candidate can replace its supply at the %.0f%% target", 100 * mid_target))
    add_role(associates, "Associate partner", sprintf("joins only to reach the %.0f%% target", 100 * max(targets_cov)))
    add_role(crit_external, "Critical external dependency", sprintf("not a member, but losing it pushes a node below the %.0f%% target", 100 * mid_target))
    add_role(crit_external_marginal, "Critical external dependency (marginal)",
             sprintf("not a member; supplies under %.0f%% of the anchors' demand but tips a node below the %.0f%% target because the coalition sits near it",
                     100 * crit_floor, 100 * mid_target))
    add_role(ext_dep, "External dependency", sprintf("not a member; supplies at least %.0f%% of the anchors' demand in a node", 100 * cmp$role_min_share))
    roles <- dplyr::bind_rows(role_rows)
    unassigned <- setdiff(members, roles$iso3)
    if (length(unassigned)) roles <- dplyr::bind_rows(roles, tibble::tibble(iso3 = unassigned, role = "Supplier (coalition member)", basis = "adds supply in the coalition test"))
    roles$coalition_tier <- ifelse(roles$iso3 %in% members, "core coordinating member",
                                   ifelse(roles$iso3 %in% associates, "associate partner",
                                          ifelse(roles$iso3 %in% crit_external, "critical external dependency",
                                                 ifelse(roles$iso3 %in% crit_external_marginal, "critical external dependency (marginal)",
                                                        ifelse(roles$iso3 %in% ext_dep, "external dependency", "development host")))))
    roles$critical_supplier <- roles$iso3 %in% c(crit_members, crit_ext_all)
    roles$coalition_tier[roles$iso3 %in% prospective] <- "prospective host"
    roles$coalition_tier[roles$iso3 %in% crit_members] <- "critical supply partner"
    roles$membership_basis <- basis$membership_basis[match(roles$iso3, basis$iso3)]
    roles$loo_critical <- roles$iso3 %in% (if (nrow(crit)) crit$iso3[crit$critical] else character())
    roles$loo_critical_simulated <- roles$iso3 %in% (if (nrow(crit)) crit$iso3[crit$critical_simulated] else character())
    roles$substitutable_by <- if (nrow(subs)) dplyr::na_if(subs$single_substitutes[match(roles$iso3, subs$iso3)], "") else NA_character_
    roles$substitutes_needed <- if (nrow(subs)) subs$substitutes_needed[match(roles$iso3, subs$iso3)] else NA_integer_

    # Coverage: existing relationships for everyone plus the members' new and expanded relationships;
    # then the investment response at member hosts; the global optimum apart.
    member_hosts <- function(n) {
      if (!(n %in% build_ids) || is.null(hosts_head)) return(character())
      h <- coalition_hosts(hosts_head, n)
      utils::head(intersect(h$host_iso, members), cmp$investment_hosts_per_node)
    }
    # The problem's supplier-share cap binds (the diversification scenario's; none under the headline shock).
    cov_mixed <- function(n, extra = NULL) {
      aib_compact_trade_coverage(problems[[n]], headline_scenario, members, scores = node_scores(n), extra = extra,
                                 tolerances = tolerances, neutral = neutral, existing_growth = growth)
    }
    cov_trade <- vapply(ids, function(n) cov_mixed(n), numeric(1))
    cov_inv <- vapply(ids, function(n) {
      hs <- member_hosts(n)
      if (!length(hs)) return(cov_trade[[n]])
      hr <- hosts_head[hosts_head$node_id == n, , drop = FALSE]
      hx <- stats::setNames(host_inc(as.numeric(inv$headline_increment), hr[match(hs, hr$host_iso), , drop = FALSE]), hs)
      hx <- hx[hx > 1e-12]
      if (!length(hx)) return(cov_trade[[n]])
      cov_mixed(n, hx)
    }, numeric(1))
    anchor_dem <- vapply(ids, function(n) { A <- intersect(anchors, problems[[n]]$scope); sum(problems[[n]]$demand[A]) * problems[[n]]$D }, numeric(1))
    frag <- vapply(ids, function(n) aib_anchor_existing_coverage(problems[[n]], anchors, fx_m[fx_m$node_id == n, , drop = FALSE]), numeric(1))
    coal_cov <- vapply(ids, function(n) { v <- aib_anchor_coverage(problems[[n]], anchors, members, headline_scenario, host_extra[[n]]); if (is.na(v)) NA_real_ else v }, numeric(1))
    okd <- !is.na(frag) & !is.na(coal_cov) & anchor_dem > 0
    anchor_fragmented <- if (any(okd)) sum(frag[okd] * anchor_dem[okd]) / sum(anchor_dem[okd]) else NA_real_
    anchor_compact <- if (any(okd)) sum(coal_cov[okd] * anchor_dem[okd]) / sum(anchor_dem[okd]) else NA_real_
    compact_gain <- anchor_compact - anchor_fragmented
    # The compact's nodes the shock did not run on count as unchanged: their demand, coverage 1.
    ux <- if (is.null(unchanged)) NULL else unchanged[[cid]]
    one <- rep(1, NROW(ux))
    dem <- c(cn$demand_usd, if (NROW(ux)) ux$demand_usd)
    wmean <- function(x) sum(c(x, one) * dem) / sum(dem)
    mix <- aib_class_mix(cn$node_classification, cn$lost_usd)
    archetype <- aib_archetype(cn$node_classification, cn$lost_usd, cmp$archetype_dominance)
    secondary <- if (length(mix) >= 2 && mix[2] >= aib_or(cmp$policy_need_floor, 0.2)) names(mix)[2] else NA_character_
    n_countries <- length(members)
    compact_type <- if (!is.na(compact_gain) && compact_gain < aib_or(cmp$min_compact_gain, 0.05)) {
      "compact unnecessary: existing relationships do nearly as well"
    } else if (n_countries < cmp$min_countries) "bilateral arrangement sufficient" else "candidate compact"
    fmt_set <- function(x) if (is.null(x) || all(is.na(x))) NA_character_ else paste(sort(x), collapse = " ")
    compact_rows[[length(compact_rows) + 1]] <- tibble::tibble(
      compact_id = cid,
      compact_name = aib_compact_name(cn$sub_sector, cn$techs),
      technology = paste(sort(unique(unlist(strsplit(cn$techs, "; ")))), collapse = "; "),
      stages = paste(aib_stage_sort(cn$supply_chain), collapse = "; "),
      included_subsectors = paste(cn$sub_sector, collapse = "; "),
      n_subsectors = nrow(cn),
      compact_type = compact_type,
      compact_archetype = archetype,
      node_classes = paste(sprintf("%s: %d", names(table(cn$node_classification)), table(cn$node_classification)), collapse = "; "),
      node_policy_needs = paste(sprintf("%s: %s", cn$sub_sector, cn$policy_needs), collapse = "; "),
      coalition_method = coalition_method,
      minimum_coalition = if (isTRUE(mid$reached)) fmt_set(mid$members) else NA_character_,
      minimum_coalition_size = if (isTRUE(mid$reached)) length(mid$members) else NA_integer_,
      minimum_coalition_90 = fmt_set(exact[[1]]$members),
      minimum_coalition_95 = if (length(exact) >= 2) fmt_set(exact[[2]]$members) else NA_character_,
      minimum_coalition_99 = fmt_set(exact[[length(exact)]]$members),
      target_reached_95 = isTRUE(mid$reached),
      target_reached_99 = isTRUE(top_t$reached),
      associates_reach_99 = !is.null(nest) && isTRUE(nest$reached),
      robust_tie_break_solved = isTRUE(mid$robust_optimal),
      best_min_anchor_coverage = if (length(top_t$coverage)) min(top_t$coverage) else NA_real_,
      greedy_sufficient_coalition = if (isTRUE(greedy_mid$reached)) greedy_mid$members else NA_character_,
      max_anchor_coverage = max(path$min_coverage),
      core_coordinating_members = fmt_set(setdiff(members, prospective)),
      prospective_hosts = fmt_set(prospective),
      critical_supply_partners = fmt_set(crit_members),
      critical_external_dependencies = fmt_set(crit_external),
      critical_external_dependencies_marginal = fmt_set(crit_external_marginal),
      external_dependencies = fmt_set(ext_dep),
      demand_anchors = paste(roles$iso3[roles$role == "Demand anchor"], collapse = " "),
      specialized_suppliers = paste(sort(unique(roles$iso3[roles$role %in% c("Specialized supplier", "Resource supplier")])), collapse = " "),
      investment_hosts = paste(sort(unique(roles$iso3[roles$role == "Scaling host"])), collapse = " "),
      specialty_hosts = paste(sort(unique(roles$iso3[roles$role == "Specialty host"])), collapse = " "),
      development_hosts = paste(sort(unique(roles$iso3[roles$role == "Development host"])), collapse = " "),
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
      physical_warnings = paste(cn$sub_sector[!is.na(cn$classification_warning)], collapse = "; "),
      shock_id = shk, shock_semantics = headline_semantics, supply_constraint = con, scenario = headline_scenario
    )
    compact_members[[length(compact_members) + 1]] <- roles %>% dplyr::mutate(compact_id = cid, in_minimum_coalition = iso3 %in% members, .before = 1)
    compact_paths[[length(compact_paths) + 1]] <- path %>%
      dplyr::mutate(compact_id = cid, node_coverage = vapply(node_coverage, function(v) paste(sprintf("%s=%.3f", names(v), v), collapse = "; "), ""), .before = 1)
    compact_nodes[[length(compact_nodes) + 1]] <- tibble::tibble(node_id = ids, compact_id = cid)
    if (nrow(crit)) compact_loo[[length(compact_loo) + 1]] <- dplyr::mutate(crit, compact_id = cid, target = mid_target, .before = 1)
  }
  compacts <- dplyr::bind_rows(compact_rows)
  members_tbl <- dplyr::bind_rows(compact_members)
  paths_tbl <- dplyr::bind_rows(compact_paths)
  cnodes <- dplyr::bind_rows(compact_nodes)
  loo_tbl <- dplyr::bind_rows(compact_loo)
  if (nrow(members_tbl)) {
    multi <- members_tbl %>% dplyr::filter(in_minimum_coalition) %>% dplyr::distinct(compact_id, iso3) %>% dplyr::count(iso3) %>% dplyr::filter(n > 1)
    extra_bridge <- members_tbl %>% dplyr::filter(in_minimum_coalition, iso3 %in% multi$iso3) %>% dplyr::distinct(compact_id, iso3) %>%
      dplyr::mutate(role = "Bridge partner", basis = "member of several compacts' minimum coalitions", in_minimum_coalition = TRUE,
                    coalition_tier = "core coordinating member", critical_supplier = FALSE)
    members_tbl <- dplyr::bind_rows(members_tbl, extra_bridge) %>% dplyr::distinct(compact_id, iso3, role, .keep_all = TRUE) %>%
      dplyr::group_by(compact_id, iso3) %>%
      dplyr::mutate(coalition_tier = dplyr::first(coalition_tier), critical_supplier = any(critical_supplier %in% TRUE),
                    loo_critical = any(loo_critical %in% TRUE), loo_critical_simulated = any(loo_critical_simulated %in% TRUE),
                    membership_basis = dplyr::first(membership_basis), substitutable_by = dplyr::first(substitutable_by),
                    substitutes_needed = dplyr::first(substitutes_needed)) %>% dplyr::ungroup() %>%
      dplyr::left_join(inputs$country_names, by = "iso3")
    compacts <- compacts %>% dplyr::left_join(
      members_tbl %>% dplyr::filter(role == "Bridge partner") %>% dplyr::group_by(compact_id) %>%
        dplyr::summarise(bp = paste(sort(unique(iso3)), collapse = " "), .groups = "drop"), by = "compact_id") %>%
      dplyr::mutate(bridge_partners = dplyr::coalesce(bp, "")) %>% dplyr::select(-bp)
  }
  list(compacts = compacts, members = members_tbl, paths = paths_tbl, cnodes = cnodes, loo = loo_tbl)
}

# Each node's leading hosts by type ("; "-separated): the three best scaling hosts
# (priority_investment_hosts), the three best specialty hosts and the development_hosts best development
# hosts; and the best gain of any host.
node_host_lists <- function(h, keys) {
  scaling <- if ("is_scaling" %in% names(h)) !(h$is_scaling %in% FALSE) else rep(TRUE, nrow(h))
  h$sc_rank <- ifelse(scaling, h$host_rank, NA_integer_)
  if (!"specialty_rank" %in% names(h)) h$specialty_rank <- NA_integer_
  if (!"development_rank" %in% names(h)) h$development_rank <- NA_integer_
  hosts_by_rank <- function(iso, r, n) { k <- which(!is.na(r) & r <= n); paste(iso[k][order(r[k])], collapse = "; ") }
  h %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(priority_investment_hosts = hosts_by_rank(host_iso, sc_rank, 3), specialty_hosts = hosts_by_rank(host_iso, specialty_rank, 3),
                     development_hosts = hosts_by_rank(host_iso, development_rank, dev_hosts_n),
                     best_marginal_gain = max(marginal_resilience_gain), .groups = "drop")
}
build_compact_layer <- function(con) {
  classified <- classify_variant(con)
  hosts_m <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(supply_constraint == con) else inv_hosts
  best_host <- if (nrow(hosts_m)) node_host_lists(hosts_m %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario,
                                                                            abs(increment - inv$headline_increment) < 1e-9), "node_id") else
    tibble::tibble(node_id = character(), priority_investment_hosts = character(), specialty_hosts = character(),
                   development_hosts = character(), best_marginal_gain = numeric())
  classified <- classified %>% dplyr::left_join(best_host, by = "node_id")
  # The best host the alignment screen keeps out, its gain and what the screen costs (headline variant).
  screened_best <- if (nrow(alignment)) alignment %>%
    dplyr::filter(supply_constraint == con, shock_id == headline_shock, scenario == headline_scenario, abs(increment - inv$headline_increment) < 1e-9) %>%
    dplyr::group_by(node_id) %>% dplyr::slice_max(alignment_cost, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
    dplyr::transmute(node_id, screened_host = best_screened_host, screened_host_gain = best_screened_gain, alignment_cost) else
    tibble::tibble(node_id = character(), screened_host = character(), screened_host_gain = numeric(), alignment_cost = numeric())
  classified <- classified %>% dplyr::left_join(screened_best, by = "node_id")
  # The class under every other variant, each with its physical-validation warning.
  ps <- phys_status %>% dplyr::select(node_id, physical_data, physical_divergence, physically_divergent, trade_share_without_physical_data)
  put_variant <- function(key, cv) {
    k <- match(classified$node_id, cv$node_id)
    w <- cv %>% dplyr::left_join(ps, by = "node_id") %>%
      aib_physical_validation(constrained = identical(con, "production_constrained"), low_exposure = cls$low_exposure)
    classified[[paste0("class_", key)]] <<- cv$node_classification[k]
    classified[[paste0("classification_warning_", key)]] <<- w$classification_warning[k]
  }
  for (shk in setdiff(vapply(cfg$shocks, `[[`, "", "id"), headline_shock)) {
    put_variant(shk, classify_variant(con, shk = shk))
    # The diversification scenario runs only where a supplier is above the cap: elsewhere it has no class.
    if (identical(shk, div_id)) {
      off <- !classified$node_id %in% targets$node_id[targets$shock_id == shk]
      classified[[paste0("class_", shk)]][off] <- NA_character_
      classified[[paste0("classification_warning_", shk)]][off] <- NA
    }
  }
  for (osc in setdiff(scenarios, headline_scenario)) put_variant(osc, classify_variant(con, sc = osc))
  for (osem in setdiff(semantics, headline_semantics)) {
    co <- classify_variant(con, sem = osem)
    put_variant(osem, co)
    classified[[paste0("coordination_dividend_", osem)]] <- co$coordination_dividend[match(classified$node_id, co$node_id)]
    classified[[paste0("residual_gap_", osem)]] <- co$residual_gap[match(classified$node_id, co$node_id)]
  }
  # Sensitivity to the existing-relationship growth bound (headline constraint only: the growth runs).
  if (identical(con, headline_constraint) && nrow(growth_runs)) {
    for (g in growths) {
      ov <- growth_runs %>% dplyr::filter(scenario == headline_scenario, abs(existing_growth - g) < 1e-12 | (is.infinite(existing_growth) & is.infinite(g))) %>%
        dplyr::group_by(node_id) %>% dplyr::summarise(existing_g = dplyr::first(existing_coverage), .groups = "drop")
      put_variant(paste0("growth_", gl(g)), classify_variant(con, existing_override = ov))
    }
    gcols <- grep("^class_growth_", names(classified), value = TRUE)
    gm <- as.matrix(classified[, gcols, drop = FALSE])
    classified$coordinate_sensitive_to_growth <- apply(gm, 1, function(v) any(v == "COORDINATE", na.rm = TRUE) && length(unique(stats::na.omit(v))) > 1)
  }
  classified <- classified %>% dplyr::left_join(ps, by = "node_id") %>%
    aib_physical_validation(constrained = identical(con, "production_constrained"), low_exposure = cls$low_exposure) %>%
    dplyr::mutate(in_strategic_scope = node_id %in% scope_nodes, supply_constraint = con)

  problems <- head_problems[[con]]
  il_all <- importer_loss
  flows_m <- flows_head_all %>% dplyr::filter(supply_constraint == con, scenario == headline_scenario)
  fx_m <- flows_ex_head_all %>% dplyr::filter(supply_constraint == con, scenario == headline_scenario)
  supply_m <- supply_all %>% dplyr::filter(supply_constraint == con, shock_semantics == headline_semantics)
  hosts_head <- if (nrow(hosts_m)) hosts_m %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario,
                                                            abs(increment - inv$headline_increment) < 1e-9) else NULL
  lost_vec <- split(il_all, il_all$node_id)
  lost_vec <- lapply(lost_vec, function(t) stats::setNames(t$lost_share, t$importer_iso))
  supply_rec <- supply_m %>% dplyr::filter(scenario == headline_scenario, state == "optimized") %>%
    dplyr::group_by(node_id, shock_key) %>% dplyr::mutate(w = supply_usd / sum(supply_usd)) %>% dplyr::ungroup() %>%
    dplyr::group_by(node_id, exporter_iso) %>% dplyr::summarise(w = sum(w), .groups = "drop")
  hosts_rec <- if (nrow(hosts_m)) hosts_m %>% dplyr::filter(scenario == headline_scenario, abs(increment - inv$headline_increment) < 1e-9,
                                                            host_rank <= inv$top_hosts) else NULL
  vectors <- aib_node_partner_vectors(supply_rec, lost_vec, hosts_rec, inv$top_hosts)
  coh <- aib_chain_coherence(chain_edges, classified$node_id, max_hops = aib_or(cmp$chain_max_hops, 3),
                             allow_co_inputs = isTRUE(aib_or(cmp$allow_co_inputs, TRUE)))
  groups <- aib_group_nodes(classified[classified$in_strategic_scope, , drop = FALSE], vectors, coh,
                            cmp$min_partner_similarity, cmp$min_policy_similarity)
  # A compact is one connected piece of the chain, not linked only through another compact's node.
  groups <- aib_split_disconnected(groups, chain_edges, classified$node_id, max_hops = aib_or(cmp$chain_max_hops, 3),
                                   allow_co_inputs = isTRUE(aib_or(cmp$allow_co_inputs, TRUE)))
  step("Compacts (", con, "): ", nrow(groups), " intervention nodes in strategic scope, ", dplyr::n_distinct(groups$group), " groups")

  group_ids <- lapply(split(groups$node_id, groups$group), identity)
  names(group_ids) <- sprintf("AIB-C%02d", as.integer(names(group_ids)))
  sol <- solve_compacts(con, headline_shock, classified, group_ids, problems, il_all, flows_m, fx_m, hosts_head)
  compacts <- sol$compacts; members_tbl <- sol$members; paths_tbl <- sol$paths; cnodes <- sol$cnodes; loo_tbl <- sol$loo
  if (nrow(compacts)) {
    compacts <- compacts %>% dplyr::arrange(dplyr::desc(lost_usd))
    remap <- stats::setNames(sprintf("AIB-C%02d", seq_len(nrow(compacts))), compacts$compact_id)
    compacts$compact_id <- unname(remap[compacts$compact_id])
    members_tbl$compact_id <- unname(remap[members_tbl$compact_id])
    paths_tbl$compact_id <- unname(remap[paths_tbl$compact_id])
    cnodes$compact_id <- unname(remap[cnodes$compact_id])
    if (nrow(loo_tbl)) loo_tbl$compact_id <- unname(remap[loo_tbl$compact_id])
  } else {
    cnodes <- tibble::tibble(node_id = character(), compact_id = character())
  }
  # Node table with the Gates & Fences handoff fields.
  pm <- partners_out %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario, supply_constraint == con,
                                       shock_semantics == headline_semantics, relationship != "domestic retention")
  top_partners <- pm %>% dplyr::group_by(node_id) %>% dplyr::arrange(dplyr::desc(delta_usd), .by_group = TRUE) %>%
    dplyr::summarise(key_trade_partners = paste(utils::head(sprintf("%s->%s (%s)", exporter_iso, importer_iso, relationship), 4), collapse = "; "), .groups = "drop")
  pct <- function(x) ifelse(is.na(x), "n/a", sprintf("%.0f%%", 100 * x))
  none_if_empty <- function(x) ifelse(is.na(x) | !nzchar(x), "none", x)
  # Yards & Fences ids are OSI technology x six-stage cells ("batteries-components"): the node's own with
  # the master taxonomy, its crosswalk cell with the dictionary (no sub-component id: the dictionary's
  # nodes are not OSI sub-sectors).
  dictionary_nodes <- identical(aib_or(cfg$nodes$source, "master"), "dictionary")
  detail <- if (dictionary_nodes) {
    nodes %>% dplyr::transmute(node_id, yf_subcomponent_id = osi_subcomponents, yf_cells,
                               osi_cell = ifelse(is.na(yf_cells), NA_character_, sub(";.*$", "", yf_cells)))
  } else {
    inputs$node_cells_detail %>% dplyr::group_by(node_id) %>%
      dplyr::summarise(yf_subcomponent_id = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain), "::", sub_sector)), collapse = "; "),
                       yf_cells = NA_character_, osi_cell = NA_character_, .groups = "drop")
  }
  cells_long <- inputs$node_cells %>% dplyr::select(node_id, technology = tech, stage = supply_chain) %>%
    dplyr::left_join(taxonomy_tbl %>% dplyr::select(technology = tech, index_code, index_name), by = "technology")
  node_table <- classified %>%
    dplyr::left_join(cnodes, by = "node_id") %>%
    dplyr::left_join(top_partners, by = "node_id") %>%
    dplyr::left_join(detail, by = "node_id") %>%
    dplyr::left_join(cells_long, by = "node_id", relationship = "one-to-many") %>%
    dplyr::mutate(
      severity = shock_exposure, concentration = optimized_largest_share,
      strategic_problem = paste0(classification_reason, "; shock exposure ", pct(shock_exposure), "; baseline largest supplier ",
                                 baseline_largest_supplier, " (", pct(baseline_largest_share), ")"),
      trade_solution = ifelse(is.na(key_trade_partners), "no new relationships restore supply", key_trade_partners),
      investment_need = ifelse(dplyr::coalesce(residual_gap, 0) >= inv$min_residual_gap,
                               paste0(pct(residual_gap), " of demand unserved after trade; scaling hosts: ", none_if_empty(priority_investment_hosts),
                                      "; specialty hosts: ", none_if_empty(specialty_hosts), "; development hosts: ", none_if_empty(development_hosts)),
                               "none material"),
      concentration_risk = paste0("optimized largest supplier ", optimized_largest_supplier, " ", pct(optimized_largest_share), "; HHI ",
                                  ifelse(is.na(optimized_hhi), "n/a", sprintf("%.2f", optimized_hhi))),
      recommended_policy_treatment = aib_class_treatment(node_classification),
      yf_cell = if (dictionary_nodes) osi_cell else paste0(aib_slug(technology), "-", aib_slug(stage))
    ) %>%
    dplyr::select(compact_id, index_code, index_name, technology, stage, sub_sector, node_id, node_classification, policy_needs, classification_reason,
                  classification_warning, physical_validation, severity, coordination_dividend, residual_gap, concentration,
                  key_trade_partners, priority_investment_hosts, specialty_hosts, development_hosts, strategic_problem, trade_solution, investment_need, concentration_risk,
                  recommended_policy_treatment, shock_exposure, static_coverage, existing_coverage, optimized_coverage, best_marginal_gain,
                  screened_host, screened_host_gain, alignment_cost,
                  optimized_largest_supplier, optimized_hhi, coverage_loss_at_manage_cap, data_quality, yf_cell, yf_cells, yf_subcomponent_id,
                  need_coordinate, need_build, need_manage, need_manage_specialization, needs_coordinate, needs_build,
                  needs_manage_specialization, in_strategic_scope, supply_constraint) %>%
    dplyr::mutate(shock_id = headline_shock, shock_semantics = headline_semantics, scenario = headline_scenario) %>%
    dplyr::arrange(compact_id, technology, match(stage, aib_stage_levels()), sub_sector)
  list(classified = classified, compacts = compacts, members = members_tbl, paths = paths_tbl, node_table = node_table, loo = loo_tbl,
       cnodes = cnodes)
}
layers <- stats::setNames(lapply(constraints, build_compact_layer), constraints)
head_layer <- layers[[headline_constraint]]
classified <- head_layer$classified
step("Node classes (", headline_shock, ", ", headline_scenario, ", ", headline_constraint, "): ",
     paste(names(table(classified$node_classification)), table(classified$node_classification), sep = " ", collapse = "; "))

# ---- Managed Interdependence: the headline compacts solved under the diversification shock ----------
# The compacts keep their ids and nodes (formed under the headline shock, so the scenarios stay
# comparable); each is solved again, headline supply constraint only, with the diversification shock's
# node problems, importer loss, optimized and existing-network flows, investment hosts and node classes:
# coalition (90/95/99), greedy path, tiers, roles, leave-one-out and coverage metrics. The compact's nodes
# the scenario did not run on count as unchanged in the coverage metrics (coverage 1, nothing lost); a
# compact none of whose nodes it ran on has no coalition and says why (coalition_reason).
managed_layer <- NULL
if (length(problems_managed) && nrow(head_layer$compacts)) {
  t_m <- Sys.time()
  con_m <- headline_constraint
  ps_m <- phys_status %>% dplyr::select(node_id, physical_data, physical_divergence, physically_divergent, trade_share_without_physical_data)
  cls_m <- classify_variant(con_m, shk = div_id) %>% dplyr::left_join(ps_m, by = "node_id") %>%
    aib_physical_validation(constrained = identical(con_m, "production_constrained"), low_exposure = cls$low_exposure)
  # The trade checkpoint keeps the optimized and existing-network flows of the headline shock only: each
  # diversification run's states B and C are solved again exactly as the trade model solved them (same
  # problem, scores, cap and tolerances), and checked against its stored coverage.
  sm_div <- summary_runs %>% dplyr::filter(scenario == headline_scenario) %>% dplyr::semi_join(div_keys, by = c("node_id", "shock_key"))
  tol_stab <- as.numeric(aib_or(opt$tolerance_stability, 1e-6))
  fl_opt <- list(); fl_ex <- list(); n_off <- 0L
  for (nid in names(problems_managed)) {
    key <- div_keys$shock_key[div_keys$node_id == nid][1]
    prob <- problems_managed[[nid]]
    sc_obj <- node_scores(nid)
    rcap <- run_cap_of(nid, key)
    B <- aib_solve_state(prob, headline_scenario, "B", cap = rcap, scores = sc_obj, tolerances = tolerances, neutral = neutral,
                         existing_growth = growth, tolerance_stability = tol_stab)
    C <- aib_solve_state(prob, headline_scenario, "C", cap = rcap, scores = sc_obj, tolerances = tolerances, neutral = neutral,
                         tolerance_stability = tol_stab)
    s0 <- sm_div[sm_div$node_id == nid, , drop = FALSE]
    if (!nrow(s0) || !isTRUE(abs(C$coverage - s0$optimized_coverage[1]) < 1e-6 && abs(B$coverage - s0$existing_coverage[1]) < 1e-6)) n_off <- n_off + 1L
    if (!is.null(C$flows) && nrow(C$flows)) fl_opt[[nid]] <- dplyr::mutate(C$flows, node_id = nid, .before = 1)
    if (!is.null(B$flows) && nrow(B$flows)) fl_ex[[nid]] <- dplyr::mutate(B$flows, node_id = nid, .before = 1)
  }
  if (n_off) warning(n_off, " diversification runs did not reproduce their stored coverage; their compact roles may differ.", call. = FALSE)
  hosts_head_m <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(supply_constraint == con_m, shock_id == div_id, scenario == headline_scenario,
                                                                   abs(increment - inv$headline_increment) < 1e-9) else NULL
  if (!is.null(hosts_head_m) && !nrow(hosts_head_m)) hosts_head_m <- NULL
  cn_head <- head_layer$cnodes
  group_ids_m <- split(cn_head$node_id, cn_head$compact_id)
  demand_of <- stats::setNames(classified$demand_usd, classified$node_id)
  unchanged_m <- lapply(group_ids_m, function(ids) {
    u <- setdiff(ids, names(problems_managed))
    tibble::tibble(node_id = u, demand_usd = as.numeric(demand_of[u]))
  })
  no_flows <- tibble::tibble(node_id = character(), e = character(), m = character(), f = numeric(), base = numeric())
  sol_m <- solve_compacts(con_m, div_id, cls_m, group_ids_m, problems_managed, importer_loss_managed,
                          if (length(fl_opt)) dplyr::bind_rows(fl_opt) else no_flows, if (length(fl_ex)) dplyr::bind_rows(fl_ex) else no_flows,
                          hosts_head_m, unchanged = unchanged_m)
  # Each node's status in the scenario (as in aib_diversification.csv), for the reason a compact has no coalition.
  div_status <- function(ids) {
    d <- if (is.null(div_targets)) NULL else div_targets[match(ids, div_targets$node_id), , drop = FALSE]
    if (is.null(d)) return(rep(NA_character_, length(ids)))
    dplyr::case_when(is.na(d$node_id) ~ NA_character_, is.na(d$benchmark_hhi) ~ "no benchmark",
                     is.na(d$cap) ~ "already as diversified as its benchmark",
                     !ids %in% names(problems_managed) ~ "within the cap in the allied market",
                     TRUE ~ "diversified to the benchmark")
  }
  hc <- head_layer$compacts
  run_ids <- lapply(group_ids_m, function(ids) ids[ids %in% names(problems_managed)])
  none <- hc$compact_id[!hc$compact_id %in% sol_m$compacts$compact_id]
  empty_rows <- dplyr::bind_rows(lapply(none, function(cid) {
    ids <- group_ids_m[[cid]]
    tibble::tibble(compact_id = cid, coalition_reason = aib_scenario_coalition_reason(div_status(ids)),
                   demand_usd = sum(demand_of[ids]), lost_usd = 0, baseline_shock_exposure = 0, coverage_static = 1,
                   coverage_without_coordination = 1, coverage_with_trade_coordination = 1, coverage_after_investment_simulation = 1,
                   coverage_global_optimum = 1, coordination_dividend = 0, residual_gap = 0, residual_gap_after_investment = 0,
                   shock_id = div_id, shock_semantics = headline_semantics, supply_constraint = con_m, scenario = headline_scenario)
  }))
  keep_cols <- c("compact_id", "compact_name", "technology", "stages", "included_subsectors", "n_subsectors")
  compacts_managed <- dplyr::bind_rows(sol_m$compacts, empty_rows) %>%
    dplyr::select(-dplyr::any_of(setdiff(keep_cols, "compact_id"))) %>%
    dplyr::left_join(hc %>% dplyr::select(dplyr::all_of(keep_cols)), by = "compact_id") %>%
    dplyr::mutate(n_nodes_run = vapply(compact_id, function(cid) length(run_ids[[cid]]), integer(1)),
                  nodes_run = vapply(compact_id, function(cid) paste(run_ids[[cid]], collapse = " "), character(1)),
                  node_statuses = vapply(compact_id, function(cid) {
                    ids <- group_ids_m[[cid]]
                    paste(sprintf("%s: %s", ids, dplyr::coalesce(div_status(ids), "not assessed")), collapse = "; ")
                  }, character(1))) %>%
    dplyr::relocate(dplyr::all_of(keep_cols), n_nodes_run, nodes_run, dplyr::any_of("coalition_reason")) %>%
    dplyr::arrange(match(compact_id, hc$compact_id))
  if (!"coalition_reason" %in% names(compacts_managed)) compacts_managed$coalition_reason <- NA_character_
  # The trade compact is a restriction of the scenario's optimized state at the same cap: its coverage cannot
  # pass the global optimum beyond the lexicographic coverage tolerance.
  over <- compacts_managed$compact_id[dplyr::coalesce(compacts_managed$coverage_with_trade_coordination - compacts_managed$coverage_global_optimum, 0) >
                                        2 * tolerances[1] + 1e-6]
  if (length(over)) warning("Managed Interdependence compacts whose trade coverage passes the global optimum: ", paste(over, collapse = ", "), call. = FALSE)
  managed_layer <- list(compacts = compacts_managed, members = sol_m$members, paths = sol_m$paths, loo = sol_m$loo,
                        cnodes = sol_m$cnodes, classified = cls_m)
  step("Compacts under ", div_id, " (", con_m, "): ", nrow(sol_m$compacts), " of ", nrow(hc), " compacts solved (",
       length(names(problems_managed)), " scenario nodes), ", sum(!is.na(compacts_managed$minimum_coalition)), " reach the ",
       100 * mid_target, "% target; ", round(as.numeric(difftime(Sys.time(), t_m, units = "mins")), 1), " min")
}

# Classification table: the headline mode, with the other mode's class beside it.
other <- setdiff(constraints, headline_constraint)
class_out <- classified
for (o in other) {
  k <- match(class_out$node_id, layers[[o]]$classified$node_id)
  class_out[[paste0("class_", o)]] <- layers[[o]]$classified$node_classification[k]
  class_out[[paste0("classification_warning_", o)]] <- layers[[o]]$classified$classification_warning[k]
  class_out[[paste0("physical_validation_", o)]] <- layers[[o]]$classified$physical_validation[k]
}
class_out[[paste0("class_", headline_constraint)]] <- class_out$node_classification
write_out(class_out %>% ids_cols() %>%
            dplyr::select(node_id, sub_sector, techs, supply_chain, in_strategic_scope, data_quality, node_classification, policy_needs,
                          classification_reason, classification_warning, physical_validation, physical_data, physical_divergence,
                          trade_share_without_physical_data,
                          shock_exposure, static_coverage, existing_coverage, optimized_coverage, coordination_dividend, residual_gap,
                          optimized_largest_supplier, optimized_largest_share, optimized_top3_share, coverage_loss_at_manage_cap,
                          optimized_n1_reoptimized_coverage, optimized_instantaneous_n1_retained_coverage, optimized_diversion,
                          priority_investment_hosts, specialty_hosts, development_hosts, best_marginal_gain,
                          dplyr::any_of(c("screened_host", "screened_host_gain", "alignment_cost")), need_coordinate, need_build, need_manage_specialization,
                          needs_coordinate, needs_build, needs_manage_specialization, dplyr::starts_with("class_"),
                          dplyr::any_of(paste0(c("coordination_dividend_", "residual_gap_"), rep(setdiff(semantics, headline_semantics), each = 2))),
                          dplyr::any_of("coordinate_sensitive_to_growth"), dplyr::starts_with("classification_warning_"),
                          dplyr::starts_with("physical_validation_"), supply_constraint, demand_scope, candidate_universe),
          "aib_node_classification.csv")

# Growth sensitivity table (headline variant).
if (nrow(growth_runs)) {
  growth_out <- growth_runs %>%
    dplyr::left_join(summary_runs %>% dplyr::inner_join(headline_keys, by = c("node_id", "shock_key")) %>%
                       dplyr::select(node_id, scenario, optimized_coverage, static_coverage, shock_exposure), by = c("node_id", "scenario")) %>%
    dplyr::mutate(existing_growth_label = vapply(existing_growth, gl, ""),
                  coordination_dividend = pmax(optimized_coverage - existing_coverage, 0))
  cls_long <- dplyr::bind_rows(lapply(growths, function(g) tibble::tibble(
    node_id = classified$node_id, existing_growth_label = gl(g),
    node_classification = classified[[paste0("class_growth_", gl(g))]])))
  growth_out <- growth_out %>% dplyr::left_join(cls_long, by = c("node_id", "existing_growth_label")) %>%
    dplyr::mutate(node_classification = ifelse(scenario == headline_scenario, node_classification, NA_character_)) %>%
    dplyr::left_join(node_label, by = "node_id") %>% ids_cols() %>%
    dplyr::mutate(in_strategic_scope = node_id %in% scope_nodes, shock_id = headline_shock) %>%
    dplyr::relocate(node_id, node_label, techs, scenario, existing_growth_label, existing_growth, existing_coverage, coordination_dividend, node_classification)
  write_out(growth_out, "allied_trade_growth_sensitivity.csv")
}

stack <- function(name) dplyr::bind_rows(lapply(layers, `[[`, name))
compacts <- stack("compacts") %>% ids_cols()
node_table <- stack("node_table") %>% ids_cols()
members_tbl <- dplyr::bind_rows(lapply(names(layers), function(m) if (nrow(layers[[m]]$members)) dplyr::mutate(layers[[m]]$members, supply_constraint = m)))
paths_tbl <- dplyr::bind_rows(lapply(names(layers), function(m) if (nrow(layers[[m]]$paths)) dplyr::mutate(layers[[m]]$paths, supply_constraint = m)))
write_out(compacts, "aib_compact_candidates.csv")
write_out(node_table, "aib_compact_nodes.csv")
write_out(members_tbl %>% ids_cols(), "aib_compact_members.csv")
write_out(paths_tbl %>% ids_cols(), "aib_compact_coalition_path.csv")
loo_out <- dplyr::bind_rows(lapply(names(layers), function(m) if (nrow(layers[[m]]$loo)) dplyr::mutate(layers[[m]]$loo, supply_constraint = m)))
write_out(loo_out %>% ids_cols(), "aib_compact_leave_one_out.csv")
if (!is.null(managed_layer)) {
  m_cols <- function(t) if (is.null(t) || !nrow(t)) t else t %>% dplyr::mutate(shock_id = div_id, supply_constraint = headline_constraint) %>% ids_cols()
  write_out(managed_layer$compacts %>% ids_cols(), "aib_compact_candidates_managed.csv")
  write_out(m_cols(managed_layer$members), "aib_compact_members_managed.csv")
  write_out(m_cols(managed_layer$paths), "aib_compact_coalition_path_managed.csv")
  write_out(m_cols(managed_layer$loo), "aib_compact_leave_one_out_managed.csv")
}

# ---- The diversification scenario, one row per node and scenario ----------------------------------------
# Each node's exporter concentration against its higher-level sector's, the supplier cap that closes the
# difference, and what it takes (headline semantics and supply constraint): the supply the capped
# suppliers lose, how much existing and new trade relationships replace under the cap, the capacity gap
# left and the hosts that would close it.
div_out <- NULL
if (!is.null(div_targets)) {
  dk <- targets %>% dplyr::filter(shock_id == div_id) %>% dplyr::distinct(node_id, shock_key)
  ds <- summary_runs %>% dplyr::inner_join(dk, by = c("node_id", "shock_key")) %>%
    dplyr::select(node_id, scenario, demand_usd, lost_usd, shock_exposure, static_coverage, existing_coverage, optimized_coverage,
                  coordination_dividend, residual_gap, residual_gap_usd, optimized_largest_supplier, optimized_largest_share, optimized_hhi,
                  n_new_links, n_expanded_links, optimized_top3_share)
  hosts_d <- if (nrow(inv_hosts_head)) node_host_lists(inv_hosts_head %>% dplyr::filter(shock_id == div_id, abs(increment - inv$headline_increment) < 1e-9),
                                                       c("node_id", "scenario")) %>% dplyr::select(-best_marginal_gain) else
    tibble::tibble(node_id = character(), scenario = character(), priority_investment_hosts = character(), specialty_hosts = character(),
                   development_hosts = character())
  rel_d <- partners_head %>% dplyr::filter(shock_id == div_id, relationship != "domestic retention") %>%
    dplyr::group_by(node_id, scenario) %>% dplyr::arrange(dplyr::desc(delta_usd), .by_group = TRUE) %>%
    dplyr::summarise(key_trade_partners = paste(utils::head(sprintf("%s->%s", exporter_iso, importer_iso), 4), collapse = "; "),
                     new_and_expanded_usd = sum(delta_usd), .groups = "drop")
  cls_col <- paste0("class_", div_id)
  div_cls <- if (cls_col %in% names(classified)) classified %>% dplyr::select(node_id, diversify_class = dplyr::all_of(cls_col)) else
    tibble::tibble(node_id = character(), diversify_class = character())
  div_out <- div_targets %>%
    dplyr::left_join(inputs$benchmarks %>% dplyr::select(node_id, benchmark_hs4, benchmark_sector, n_benchmark_lines, benchmark_hs4_headings,
                                                         benchmark_year), by = "node_id") %>%
    dplyr::left_join(node_label, by = "node_id") %>%
    dplyr::left_join(taxonomy_tbl %>% dplyr::select(techs = tech, index_code, index_name), by = "techs") %>%
    dplyr::left_join(ds, by = "node_id", relationship = "one-to-many") %>%
    dplyr::left_join(hosts_d, by = c("node_id", "scenario")) %>%
    dplyr::left_join(rel_d, by = c("node_id", "scenario")) %>%
    dplyr::left_join(div_cls, by = "node_id") %>%
    # Above the benchmark worldwide but no supplier above the cap in the demand scope: nothing to replace there.
    dplyr::mutate(status = dplyr::case_when(is.na(benchmark_hhi) ~ "no benchmark",
                                            is.na(cap) ~ "already as diversified as its benchmark",
                                            is.na(scenario) ~ "within the cap in the allied market",
                                            TRUE ~ "diversified to the benchmark"),
                  diversify_class = dplyr::if_else(status == "diversified to the benchmark", diversify_class, NA_character_),
                  shock_id = div_id, shock_semantics = headline_semantics, supply_constraint = headline_constraint,
                  in_strategic_scope = node_id %in% scope_nodes) %>%
    dplyr::relocate(node_id, node_label, techs, index_code, node_stage, status, current_hhi, benchmark_hhi, benchmark_basis, cap,
                    target_iso, scenario) %>%
    ids_cols()
  write_out(div_out, "aib_diversification.csv")
}

# ---- Validation cases: trade-only vs production-constrained ------------------------------------------
val_tables <- list(); val_md <- c()
for (m in constraints) {
  L <- layers[[m]]
  v <- aib_validation_chains(unlist(cfg$validation_cases),
                             L$classified %>% dplyr::left_join(dq_out %>% dplyr::filter(is.na(aggregated_into)) %>% dplyr::select(node_id, flags), by = "node_id"),
                             L$node_table, partners_out %>% dplyr::filter(supply_constraint == m, shock_semantics == headline_semantics),
                             if (nrow(inv_out)) inv_out[inv_out$supply_constraint == m, , drop = FALSE] else NULL, L$compacts, L$members,
                             headline_shock, headline_scenario, nodes, as.numeric(inv$headline_increment),
                             mode_label = paste0(m, "; ", headline_semantics, "; ", universe_id))
  val_tables[[m]] <- dplyr::mutate(v$table, supply_constraint = m)
  val_md <- c(val_md, v$markdown)
}
write_out(dplyr::bind_rows(val_tables) %>% ids_cols(), "aib_validation_cases.csv")
writeLines(val_md, file.path(out_dir, "aib_validation_cases.md"), useBytes = TRUE)

saveRDS(list(
  meta = list(built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), universe = universe_id, demand_scope = demand_set$id,
              candidate_universe = partner_set$id, n_demand_scope = length(demand_set$iso), headline_shock = headline_shock,
              headline_scenario = headline_scenario, headline_semantics = headline_semantics, headline_constraint = headline_constraint,
              strategic_scope = scope_mode, scope_techs = scope_techs, trade_year = inputs$meta$trade_year,
              taxonomy = input_taxonomy, dictionary_scope = aib_or(inputs$meta$dictionary_scope, NA_character_), config = cfg,
              host_type_sources = ht_data$sources, host_screen_mode = hs_mode, host_bloc_source = host_bloc$source),
  taxonomy = taxonomy_tbl,
  nodes = nodes, node_cells = inputs$node_cells, quality = dq_out, stress = stress, partnerships = partners_head,
  frontier = pick_variant(frontier_out), frontier_all = frontier_out,
  investment = if (nrow(inv_out)) inv_out[inv_out$supply_constraint == headline_constraint, , drop = FALSE] else inv_out,
  investment_pairs = inv_pairs_head, edges = edges, countries = countries,
  investment_screened = if (nrow(screened_out)) screened_out[screened_out$supply_constraint == headline_constraint, , drop = FALSE] else screened_out,
  host_screen = host_screen,
  classification = classified, compacts = head_layer$compacts, compact_nodes = head_layer$node_table,
  compact_members = head_layer$members, coalition_paths = head_layer$paths, leave_one_out = head_layer$loo, layers = layers,
  growth = if (exists("growth_out")) growth_out else NULL, importer_loss = importer_loss,
  diversification = div_out, benchmarks = inputs$benchmarks,
  validation = dplyr::bind_rows(val_tables), country_names = inputs$country_names, scope_nodes = scope_nodes,
  # The dashboard's other two scenarios (scripts/84 needs nothing from aib_inputs.rds). Managed
  # Interdependence (the diversification shock, headline variant): its network, importer loss and the
  # headline compacts solved under it. Baseline (observed reference-year trade): each optimized node's
  # supply to the demand scope by exporter and the observed network.
  scenario_shock = div_id,
  edges_managed = net_managed$edges, countries_managed = net_managed$countries, importer_loss_managed = importer_loss_managed,
  compacts_managed = managed_layer$compacts, compact_members_managed = managed_layer$members,
  coalition_paths_managed = managed_layer$paths, leave_one_out_managed = managed_layer$loo,
  baseline_supply = observed_supply, edges_baseline = edges_baseline, countries_baseline = countries_baseline,
  baseline_min_share = baseline_min_share
), file.path(out_dir, "aib_results.rds"))
step("AIB done in ", round(as.numeric(difftime(Sys.time(), t_script, units = "mins")), 1), " min; ",
     nrow(head_layer$compacts), " compacts (", headline_constraint, ")")
