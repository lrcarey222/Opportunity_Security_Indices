# Allied Industrial Base (AIB), step 2: stress tests, the Trade Partnership Network, the
# diversification-specialization frontier, Investment Partnership Prioritization, the emergent AIB
# network and Sectoral Compact formation (docs/aib_methodology.md; config/aib.yml; R/aib/).
#
# Usage:
#   Rscript scripts/32_build_aib_partnerships.R [--out-dir DIR] [--nodes id1,id2] [--skip-investment]
#     [--demand-scope S] [--partner-universe P] [--scope all|electro_industrial] [--from-checkpoint]
#
#   --out-dir           where scripts/31 wrote aib_inputs.rds and where the outputs go
#                       (default: <outputs_dir>/aib)
#   --nodes             run only these node ids (validation runs); the compacts then cover them only
#   --skip-investment   stop after the trade model (no investment, network or compacts)
#   --demand-scope S    override config demand_scope: global, coalition or ISO3,ISO3,...
#   --partner-universe P override config candidate_partner_universe (same forms)
#   --scope M           override config strategic_scope mode (all or electro_industrial)
#   --from-checkpoint   reuse the last run's trade model and investment simulation (same out dir)
#
# Every trade run is one variant: shock semantics (hold: an export / supply shock; release: full
# decoupling) x supply constraint (trade_only; production_constrained: physical capacity bounds what
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

targets <- aib_shock_targets(cfg$shocks, inputs$node_cells, inputs$exporter_metrics, cfg$trade$reexport_hubs)
targets <- targets[targets$node_id %in% opt_nodes, , drop = FALSE]
runs <- targets %>% dplyr::distinct(node_id, shock_key) %>% dplyr::arrange(node_id, shock_key)
headline_keys <- targets %>% dplyr::filter(shock_id == headline_shock) %>% dplyr::distinct(node_id, shock_key)
step("Trade model: ", length(opt_nodes), " nodes, ", length(cfg$shocks), " shocks, ", nrow(runs), " node-shock runs x ",
     length(semantics), " shock semantics x ", length(constraints), " supply constraints (", length(phys_nodes),
     " nodes with physical data) x ", length(scenarios), " scenarios")

node_scores <- function(nid) {
  n <- nodes[nodes$node_id == nid, ]
  aib_node_scores(nid, strsplit(n$techs, "; ")[[1]], n$supply_chain, inputs$exporter_metrics, inputs$op_index,
                  inputs$pams, inputs$cat_scores, rca_cap = opt$rca_cap, neutral = neutral, components = components)
}
node_problem <- function(nid, key, sem = headline_semantics, con = headline_constraint) {
  aib_node_problem(flows_by_node[[nid]], aib_parse_shock_key(key), shocked_demand = sem,
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
# A checkpoint is reused only when it was built with the same sets, variants, nodes and model settings.
fingerprint <- list(demand = sort(demand_set$iso), partners = sort(partner_set$iso), semantics = semantics, constraints = constraints,
                    phys_nodes = sort(phys_nodes), nodes = sort(opt_nodes), shocks = cfg$shocks, scenarios = scenarios,
                    optimization = opt, trade = cfg$trade, production_proxies = cfg$production_proxies, supply_constraint = sc_cfg)
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
          r <- aib_run_node_shock(prob, scenarios, caps, headline_cap, sc_obj, tolerances, neutral,
                                  existing_growth = growth, tolerance_stability = as.numeric(aib_or(opt$tolerance_stability, 1e-6)))
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
  dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id, shock_label, target_iso), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::left_join(ex_cols, by = c("node_id", "exporter_iso")) %>%
  dplyr::left_join(node_label, by = "node_id") %>%
  dplyr::mutate(max_supplier_share = headline_cap, in_strategic_scope = node_id %in% scope_nodes) %>%
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
inv_hosts <- list(); inv_pairs <- list()
t_inv <- Sys.time()
inv_fingerprint <- c(fingerprint, list(investment = inv))
if (use_ckpt && file.exists(inv_ckpt)) {
  ck <- readRDS(inv_ckpt)
  check_fingerprint(ck$fingerprint, inv_fingerprint, inv_ckpt)
  inv_hosts <- list(ck$inv_hosts); inv_pairs <- list(ck$inv_pairs)
  inv_plan <- character()
  step("Investment from checkpoint ", inv_ckpt)
} else {
  inv_plan <- inv_shocks
}
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
    hosts <- hosts %>% dplyr::arrange(dplyr::desc(capability_tier), dplyr::desc(capability_value)) %>% utils::head(inv$max_hosts_per_node)
    sc_obj <- node_scores(nid)
    fs_env <- aib_node_dyad_lookup(inputs$fs_index, strsplit(nodes$techs[nodes$node_id == nid], "; ")[[1]], nodes$supply_chain[nodes$node_id == nid])
    fs_lookup <- function(a, b) aib_lookup_pairs_raw(fs_env, a, b)
    bf <- flows_head_all[flows_head_all$node_id == nid & flows_head_all$supply_constraint == con & flows_head_all$scenario == inv_sc, , drop = FALSE]
    r <- aib_investment_node(prob, inv_sc, hosts, as.numeric(unlist(inv$increments)), as.numeric(inv$headline_increment),
                             headline_cap, as.numeric(inv$diversified_max_supplier_share), sc_obj, fs_lookup, tolerances, neutral,
                             pair_hosts = inv$top_hosts, tie_resilience = inv$tie_resilience, tie_hhi = inv$tie_hhi,
                             base_flows = if (shk == headline_shock && nrow(bf)) bf[, c("e", "m", "f", "base")] else NULL)
    if (!is.null(r$hosts)) inv_hosts[[length(inv_hosts) + 1]] <- dplyr::mutate(r$hosts, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, supply_constraint = con, .before = 1)
    if (!is.null(r$pairs) && nrow(r$pairs)) inv_pairs[[length(inv_pairs) + 1]] <- dplyr::mutate(r$pairs, node_id = nid, shock_id = shk, shock_key = key, scenario = inv_sc, supply_constraint = con, .before = 1)
  }
}
inv_hosts <- dplyr::bind_rows(inv_hosts)
inv_pairs <- dplyr::bind_rows(inv_pairs)
# Copy the shared (no physical data) results into the production-constrained mode.
if (length(inv_plan) && all(c("trade_only", "production_constrained") %in% constraints) && nrow(inv_hosts)) {
  inv_hosts <- dplyr::bind_rows(inv_hosts, inv_hosts %>% dplyr::filter(supply_constraint == "trade_only", !node_id %in% phys_nodes) %>%
                                  dplyr::mutate(supply_constraint = "production_constrained"))
  if (nrow(inv_pairs)) inv_pairs <- dplyr::bind_rows(inv_pairs, inv_pairs %>% dplyr::filter(supply_constraint == "trade_only", !node_id %in% phys_nodes) %>%
                                                       dplyr::mutate(supply_constraint = "production_constrained"))
}
if (length(inv_plan)) saveRDS(list(inv_hosts = inv_hosts, inv_pairs = inv_pairs, fingerprint = inv_fingerprint), inv_ckpt)
step("Investment done in ", round(as.numeric(difftime(Sys.time(), t_inv, units = "mins")), 1), " min: ", nrow(inv_hosts), " host x increment rows")
ikeys <- c("node_id", "shock_key", "shock_id", "scenario", "supply_constraint")
if (nrow(inv_hosts)) {
  top_inv <- inv_pairs %>% dplyr::group_by(dplyr::across(dplyr::all_of(c(ikeys, "host_iso")))) %>%
    dplyr::arrange(dplyr::desc(supply_from_new_capacity), .by_group = TRUE) %>%
    dplyr::summarise(top_investor_partners = paste(utils::head(sprintf("%s (FS %s)", investor_iso, ifelse(is.na(friendshore), "n/a", sprintf("%.2f", friendshore))), 3), collapse = "; "), .groups = "drop")
  inv_out <- inv_hosts %>% dplyr::left_join(top_inv, by = c(ikeys, "host_iso")) %>%
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
                     dplyr::select(dplyr::all_of(c(ikeys, "host_iso", "host_rank"))), by = c(ikeys, "host_iso"), relationship = "many-to-one") else inv_pairs
write_out(inv_pairs_out %>% dplyr::left_join(node_label, by = "node_id") %>% ids_cols() %>% dplyr::select(-dplyr::any_of("shock_key")),
          "allied_investment_pairs.csv")
inv_hosts_head <- if (nrow(inv_hosts)) inv_hosts[inv_hosts$supply_constraint == headline_constraint, , drop = FALSE] else inv_hosts
inv_pairs_head <- if (nrow(inv_pairs_out)) inv_pairs_out[inv_pairs_out$supply_constraint == headline_constraint, , drop = FALSE] else inv_pairs_out

# ---- Emergent AIB network (headline variant, strategic scope) ---------------------------------------
node_shock <- summary_runs %>% dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::select(node_id, shock_id, scenario, shock_exposure) %>% dplyr::distinct()
primary_key <- summary_runs %>% dplyr::inner_join(dplyr::distinct(targets, node_id, shock_key, shock_id), by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::filter(scenario == headline_scenario) %>% dplyr::group_by(node_id, shock_id) %>%
  dplyr::slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>% dplyr::select(node_id, shock_id, shock_key)
trade_for_net <- partners_runs %>% dplyr::filter(node_id %in% scope_nodes) %>%
  dplyr::inner_join(primary_key, by = c("node_id", "shock_key"), relationship = "many-to-many") %>%
  dplyr::select(node_id, shock_id, scenario, exporter_iso, importer_iso, delta_usd, delta_share_of_demand, relationship)
invest_top <- if (nrow(inv_pairs_head)) inv_pairs_head %>% dplyr::filter(dplyr::coalesce(host_rank, 99L) == 1L, node_id %in% scope_nodes) %>%
  dplyr::semi_join(primary_key, by = c("node_id", "shock_id", "shock_key")) else NULL
edges <- aib_network_pairs(trade_for_net, invest_top, nodes, node_shock, headline_shock, cls$high_severity,
                           node_cells = inputs$node_cells, op_index = inputs$op_index, fs_index = inputs$fs_index)
imp_exposure <- importer_loss %>% dplyr::filter(node_id %in% scope_nodes) %>% dplyr::group_by(iso3 = importer_iso) %>%
  dplyr::summarise(headline_lost_usd = sum(lost_usd), n_exposed_subsectors = sum(lost_share > 0), .groups = "drop") %>%
  tidyr::expand_grid(scenario = scenarios)
countries <- aib_network_countries(edges, imp_exposure)
if (!is.null(countries)) countries <- countries %>% dplyr::left_join(inputs$country_names, by = "iso3") %>% dplyr::relocate(iso3, country, scenario)
write_out(edges %>% ids_cols() %>% dplyr::mutate(shock_semantics = headline_semantics, supply_constraint = headline_constraint, strategic_scope = scope_mode),
          "emergent_aib_edges.csv")
write_out(countries %>% ids_cols() %>% dplyr::mutate(shock_semantics = headline_semantics, supply_constraint = headline_constraint,
                                                     strategic_scope = scope_mode), "emergent_aib_country_summary.csv")

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

build_compact_layer <- function(con) {
  classified <- classify_variant(con)
  hosts_m <- if (nrow(inv_hosts)) inv_hosts %>% dplyr::filter(supply_constraint == con) else inv_hosts
  best_host <- if (nrow(hosts_m)) hosts_m %>% dplyr::filter(shock_id == headline_shock, scenario == headline_scenario,
                                                            abs(increment - inv$headline_increment) < 1e-9) %>%
    dplyr::group_by(node_id) %>% dplyr::arrange(host_rank, .by_group = TRUE) %>%
    dplyr::summarise(priority_investment_hosts = paste(utils::head(host_iso, 3), collapse = "; "),
                     best_marginal_gain = max(marginal_resilience_gain), .groups = "drop") else
    tibble::tibble(node_id = character(), priority_investment_hosts = character(), best_marginal_gain = numeric())
  classified <- classified %>% dplyr::left_join(best_host, by = "node_id")
  # The class under every other variant, each with its physical-validation warning.
  ps <- phys_status %>% dplyr::select(node_id, physical_data, physical_divergence, physically_divergent, trade_share_without_physical_data)
  put_variant <- function(key, cv) {
    k <- match(classified$node_id, cv$node_id)
    w <- cv %>% dplyr::left_join(ps, by = "node_id") %>%
      aib_physical_validation(constrained = identical(con, "production_constrained"), low_exposure = cls$low_exposure)
    classified[[paste0("class_", key)]] <<- cv$node_classification[k]
    classified[[paste0("classification_warning_", key)]] <<- w$classification_warning[k]
  }
  for (shk in setdiff(vapply(cfg$shocks, `[[`, "", "id"), headline_shock)) put_variant(shk, classify_variant(con, shk = shk))
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

  compact_rows <- list(); compact_nodes <- list(); compact_members <- list(); compact_paths <- list(); compact_loo <- list()
  for (g in sort(unique(groups$group))) {
    ids <- groups$node_id[groups$group == g]
    ids <- ids[ids %in% names(problems)]
    if (!length(ids)) next
    cn <- classified[classified$node_id %in% ids, , drop = FALSE]
    il <- il_all[il_all$node_id %in% ids, , drop = FALSE] %>%
      dplyr::group_by(importer_iso) %>% dplyr::summarise(lost = sum(lost_usd), .groups = "drop") %>% dplyr::arrange(dplyr::desc(lost))
    il$cum <- cumsum(il$lost) / sum(il$lost)
    n_anchor <- min(max(1, which(il$cum >= cmp$anchor_share)[1]), cmp$max_anchors, nrow(il), na.rm = TRUE)
    anchors <- il$importer_iso[seq_len(n_anchor)]
    build_ids <- cn$node_id[cn$needs_build]
    host_extra <- list()
    cand <- unique(unlist(lapply(ids, function(n) problems[[n]]$cand)))
    for (n in build_ids) {
      h <- if (!is.null(hosts_head)) hosts_head %>% dplyr::filter(node_id == n, host_rank <= inv$top_hosts) else NULL
      if (!is.null(h) && nrow(h)) {
        host_extra[[n]] <- stats::setNames(rep(cmp$coalition_increment, nrow(h)), h$host_iso)
        cand <- union(cand, h$host_iso)
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
    # Critical supply partners: members needed for the target that no single other candidate can
    # replace (each member's supply removed in turn).
    subs <- if (isTRUE(mid$reached)) aib_coalition_substitution(problems, ids, anchors, members, pool, headline_scenario, host_extra, target = mid_target) else tibble::tibble()
    crit_members <- if (nrow(subs)) sort(subs$iso3[subs$critical]) else character()
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
    add_role(unique(sp$e[sp$supply_chain != "Upstream"]), "Specialized supplier", "RCA >= 1 and a material share of the anchors' optimized supply")
    add_role(unique(sp$e[sp$supply_chain == "Upstream"]), "Resource supplier", "RCA >= 1 and a material share of the anchors' optimized upstream supply")
    hh <- if (!is.null(hosts_head)) hosts_head %>% dplyr::filter(node_id %in% build_ids, host_rank <= inv$top_hosts, host_iso %in% members) else NULL
    if (!is.null(hh) && nrow(hh)) add_role(unique(hh$host_iso), "Capacity-building / investment host", "top-ranked host in a node that needs capacity")
    tp <- ex_metrics %>% dplyr::filter(node_id %in% cn$node_id[cn$needs_build | cn$needs_manage_specialization],
                                       exporter_iso %in% members, dplyr::coalesce(export_rca, 0) >= 1, world_share >= 0.05)
    add_role(unique(tp$exporter_iso), "Technology partner", "revealed capability (RCA >= 1, >= 5% of world exports) where capacity or specialization is the need")
    supplies <- dplyr::bind_rows(sp %>% dplyr::transmute(node_id, iso3 = e), if (!is.null(hh) && nrow(hh)) hh %>% dplyr::transmute(node_id, iso3 = host_iso)) %>% dplyr::distinct()
    node_anchor <- il_all %>% dplyr::filter(node_id %in% ids) %>% dplyr::group_by(node_id) %>%
      dplyr::mutate(share = lost_share / sum(lost_share)) %>% dplyr::ungroup() %>%
      dplyr::filter(share >= cmp$node_anchor_share) %>% dplyr::transmute(node_id, iso3 = importer_iso)
    vertical <- supplies %>% dplyr::inner_join(node_anchor, by = "iso3", suffix = c("_sup", "_dep"), relationship = "many-to-many") %>%
      dplyr::filter(node_id_sup != node_id_dep)
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
                                                        "external dependency"))))
    roles$critical_supplier <- roles$iso3 %in% c(crit_members, crit_ext_all)
    roles$coalition_tier[roles$iso3 %in% crit_members] <- "critical supply partner"
    roles$loo_critical <- roles$iso3 %in% (if (nrow(crit)) crit$iso3[crit$critical] else character())
    roles$substitutable_by <- if (nrow(subs)) dplyr::na_if(subs$single_substitutes[match(roles$iso3, subs$iso3)], "") else NA_character_
    roles$substitutes_needed <- if (nrow(subs)) subs$substitutes_needed[match(roles$iso3, subs$iso3)] else NA_integer_

    # Coverage: existing relationships for everyone plus the members' new and expanded relationships;
    # then the investment response at member hosts; the global optimum apart.
    member_hosts <- function(n) {
      if (!(n %in% build_ids) || is.null(hosts_head)) return(character())
      h <- hosts_head %>% dplyr::filter(node_id == n) %>% dplyr::arrange(host_rank)
      utils::head(intersect(h$host_iso, members), cmp$investment_hosts_per_node)
    }
    cov_mixed <- function(n, extra = NULL) {
      aib_solve_state(problems[[n]], headline_scenario, "B", cap = 1, scores = node_scores(n), tolerances = tolerances,
                      neutral = neutral, steps = 1, existing_growth = growth, coordinating = c(members, names(extra)), extra = extra)$coverage
    }
    cov_trade <- vapply(ids, function(n) cov_mixed(n), numeric(1))
    cov_inv <- vapply(ids, function(n) {
      hs <- member_hosts(n)
      if (!length(hs)) return(cov_trade[[n]])
      cov_mixed(n, stats::setNames(rep(as.numeric(inv$headline_increment), length(hs)), hs))
    }, numeric(1))
    anchor_dem <- vapply(ids, function(n) { A <- intersect(anchors, problems[[n]]$scope); sum(problems[[n]]$demand[A]) * problems[[n]]$D }, numeric(1))
    frag <- vapply(ids, function(n) aib_anchor_existing_coverage(problems[[n]], anchors, fx_m[fx_m$node_id == n, , drop = FALSE]), numeric(1))
    coal_cov <- vapply(ids, function(n) { v <- aib_anchor_coverage(problems[[n]], anchors, members, headline_scenario, host_extra[[n]]); if (is.na(v)) NA_real_ else v }, numeric(1))
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
    } else if (n_countries < cmp$min_countries) "bilateral arrangement sufficient" else "candidate compact"
    fmt_set <- function(x) if (is.null(x) || all(is.na(x))) NA_character_ else paste(sort(x), collapse = " ")
    cid <- sprintf("AIB-C%02d", g)
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
      core_coordinating_members = fmt_set(members),
      critical_supply_partners = fmt_set(crit_members),
      critical_external_dependencies = fmt_set(crit_external),
      critical_external_dependencies_marginal = fmt_set(crit_external_marginal),
      external_dependencies = fmt_set(ext_dep),
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
      physical_warnings = paste(cn$sub_sector[!is.na(cn$classification_warning)], collapse = "; "),
      shock_id = headline_shock, shock_semantics = headline_semantics, supply_constraint = con, scenario = headline_scenario
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
                    loo_critical = any(loo_critical %in% TRUE), substitutable_by = dplyr::first(substitutable_by),
                    substitutes_needed = dplyr::first(substitutes_needed)) %>% dplyr::ungroup() %>%
      dplyr::left_join(inputs$country_names, by = "iso3")
    compacts <- compacts %>% dplyr::left_join(
      members_tbl %>% dplyr::filter(role == "Bridge partner") %>% dplyr::group_by(compact_id) %>%
        dplyr::summarise(bp = paste(sort(unique(iso3)), collapse = " "), .groups = "drop"), by = "compact_id") %>%
      dplyr::mutate(bridge_partners = dplyr::coalesce(bp, "")) %>% dplyr::select(-bp)
  }
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
  detail <- inputs$node_cells_detail %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(yf_subcomponent_id = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain), "::", sub_sector)), collapse = "; "), .groups = "drop")
  cells_long <- inputs$node_cells %>% dplyr::select(node_id, technology = tech, stage = supply_chain)
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
                               paste0(pct(residual_gap), " of demand unserved after trade; hosts: ", dplyr::coalesce(priority_investment_hosts, "none eligible")),
                               "none material"),
      concentration_risk = paste0("optimized largest supplier ", optimized_largest_supplier, " ", pct(optimized_largest_share), "; HHI ",
                                  ifelse(is.na(optimized_hhi), "n/a", sprintf("%.2f", optimized_hhi))),
      recommended_policy_treatment = aib_class_treatment(node_classification),
      yf_cell = paste0(aib_slug(technology), "-", aib_slug(stage))
    ) %>%
    dplyr::select(compact_id, technology, stage, sub_sector, node_id, node_classification, policy_needs, classification_reason,
                  classification_warning, physical_validation, severity, coordination_dividend, residual_gap, concentration,
                  key_trade_partners, priority_investment_hosts, strategic_problem, trade_solution, investment_need, concentration_risk,
                  recommended_policy_treatment, shock_exposure, static_coverage, existing_coverage, optimized_coverage, best_marginal_gain,
                  optimized_largest_supplier, optimized_hhi, coverage_loss_at_manage_cap, data_quality, yf_cell, yf_subcomponent_id,
                  need_coordinate, need_build, need_manage, need_manage_specialization, needs_coordinate, needs_build,
                  needs_manage_specialization, in_strategic_scope, supply_constraint) %>%
    dplyr::mutate(shock_id = headline_shock, shock_semantics = headline_semantics, scenario = headline_scenario) %>%
    dplyr::arrange(compact_id, technology, stage, sub_sector)
  list(classified = classified, compacts = compacts, members = members_tbl, paths = paths_tbl, node_table = node_table, loo = loo_tbl)
}
layers <- stats::setNames(lapply(constraints, build_compact_layer), constraints)
head_layer <- layers[[headline_constraint]]
classified <- head_layer$classified
step("Node classes (", headline_shock, ", ", headline_scenario, ", ", headline_constraint, "): ",
     paste(names(table(classified$node_classification)), table(classified$node_classification), sep = " ", collapse = "; "))

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
                          priority_investment_hosts, best_marginal_gain, need_coordinate, need_build, need_manage_specialization,
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
              strategic_scope = scope_mode, scope_techs = scope_techs, trade_year = inputs$meta$trade_year, config = cfg),
  nodes = nodes, node_cells = inputs$node_cells, quality = dq_out, stress = stress, partnerships = partners_head,
  frontier = pick_variant(frontier_out), frontier_all = frontier_out,
  investment = if (nrow(inv_out)) inv_out[inv_out$supply_constraint == headline_constraint, , drop = FALSE] else inv_out,
  investment_pairs = inv_pairs_head, edges = edges, countries = countries,
  classification = classified, compacts = head_layer$compacts, compact_nodes = head_layer$node_table,
  compact_members = head_layer$members, coalition_paths = head_layer$paths, leave_one_out = head_layer$loo, layers = layers,
  growth = if (exists("growth_out")) growth_out else NULL, importer_loss = importer_loss,
  validation = dplyr::bind_rows(val_tables), country_names = inputs$country_names, scope_nodes = scope_nodes
), file.path(out_dir, "aib_results.rds"))
step("AIB done in ", round(as.numeric(difftime(Sys.time(), t_script, units = "mins")), 1), " min; ",
     nrow(head_layer$compacts), " compacts (", headline_constraint, ")")
