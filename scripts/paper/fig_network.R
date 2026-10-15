# Figures, table and numbers: the designed allied network (paper Section 10, Results).
#
#   network_map             designed producer -> consumer flows in three Midstream stages as small
#                           multiples; two-layer layout computed on igraph graph objects (producers
#                           left, consumers right in one fixed order), drawn with ggplot2
#   network_specialization  37 network nodes x 22 stages (Upstream, Midstream): designed production
#                           share, with the stages from which the portfolio-cap loop barred a node
#   network_concentration   designed Herfindahl index and largest share per stage against the
#                           observed export concentration of the network's own nodes
#                           (allied_network_observed_hhi.csv: 2024 reconciled basket trade, the EU
#                           counting extra-EU exports only) and of world exports (trade theme)
#   network_portfolio       GDP-log portfolio caps against the realised number of top-5
#                           appearances, with caps (production design) and without (study run)
#   tables/network_producers.tex  designed producers and shares, technology x stage
#   tables/network_eu_split.tex   the EU node's designed shares split back to its members
#   results/network.json    every number quoted in the Section 10 results text
#
# Reads only stored outputs: data/processed/outputs/allied_network_*.csv (scripts/30 at
# lambda = 0.78, final run of 2026-10-12), data/processed/trade_concentration_tbl.rds (world exports
# of the basket, UN Comtrade 2024), data/processed/partner_development_country_tbl.rds (which nodes
# have an observed development potential), data/raw/wdi_country_info.csv (regions, income groups)
# and, for the counts without caps, the lambda study's no-caps run at the production lambda
# (<study dir>/runs/nc37_l0.78.rds; study dir = OPSI_NETWORK_STUDY_DIR, default
# ../scratch_final/network next to the repository root). That run is used only after checking that
# its settings and its node inputs match the stored production design. The node-level 2024 exports
# (designed producers vs the largest exporting nodes) come from the study's reconciled basket trade
# (<study dir>/trade_cells_2024.rds), checked to reproduce allied_network_observed_hhi.csv.
# Also runs the allied-network unit tests (tests/testthat/test-allied-network-*.R).
#
# Run from the repository root:
#   Rscript scripts/paper/fig_network.R

local({
  find_theme <- function() {
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    while (dirname(d) != d) {
      f <- file.path(d, "scripts", "paper", "_paper_theme.R")
      if (file.exists(f)) return(f)
      d <- dirname(d)
    }
    stop("Run from inside the repository.")
  }
  source(find_theme(), local = FALSE)
})

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(igraph)
})

root <- paper_repo_root()
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "country.R"))
# Pure helpers of the design (top-K counting for the no-caps comparison)
source(file.path(root, "R", "indices", "allied_network_design.R"))
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
raw_dir <- file.path(root, config$raw_data_dir)
outputs_dir <- file.path(processed_dir, "outputs")
study_dir <- Sys.getenv("OPSI_NETWORK_STUDY_DIR", file.path(dirname(root), "scratch_final", "network"))

read_out <- function(name) {
  utils::read.csv(file.path(outputs_dir, paste0("allied_network_", name, ".csv")), stringsAsFactors = FALSE)
}
spec    <- read_out("specialization")
flows   <- read_out("flows")
diag    <- read_out("diagnostics")
checks  <- read_out("input_checks")
members <- read_out("members")
mshares <- read_out("member_shares")
nodes   <- read_out("nodes")
nedges  <- read_out("node_edges")
obs_hhi <- read_out("observed_hhi")
caps    <- read_out("portfolio_caps")
counts  <- read_out("portfolio_counts")
topk    <- read_out("topk_by_stage")
build   <- read_out("build_candidates")

stage_levels <- c("Upstream", "Midstream")
lambda <- unique(diag$diversity_lambda)
stopifnot(length(lambda) == 1, nrow(diag) == 22, all(diag$supply_chain %in% stage_levels))
node_codes <- sort(unique(spec$iso3c))
n_nodes <- length(node_codes)
# The final design: 57 coalition members + 6 EU members outside the tiers = 63 economies, 37 nodes
stopifnot(n_nodes == 37, setequal(node_codes, nodes$network_node), nrow(spec) == n_nodes * 22)
eu_code <- "EUU"
spec <- spec %>% dplyr::mutate(selected = as.logical(selected), producer = production_share > 1e-9)
prod <- spec %>% dplyr::filter(producer)

# Display labels: "EU" for the EU-27 node, ISO3 otherwise; short country names for the rows
node_lab <- function(x) ifelse(x == eu_code, "EU", x)
country_info <- standardize_country_info(read.csv(file.path(raw_dir, "wdi_country_info.csv")))
short_name <- c("Democratic Republic of Congo" = "DR Congo", "South Korea" = "Korea",
                "United States" = "United States", "European Union" = "European Union (EU-27)")
node_info <- nodes %>%
  dplyr::select(iso3c = network_node, tier, n_members, gdp_usd) %>%
  dplyr::left_join(spec %>% dplyr::distinct(iso3c, country), by = "iso3c") %>%
  dplyr::left_join(country_info %>% dplyr::distinct(iso3c, .keep_all = TRUE) %>%
                     dplyr::select(iso3c, region, income), by = "iso3c") %>%
  dplyr::mutate(
    region = dplyr::if_else(iso3c == eu_code, "Europe & Central Asia", region),
    income = dplyr::if_else(iso3c == eu_code, "High income", income),
    name = dplyr::coalesce(unname(short_name[country]), country)
  )
stopifnot(!anyNA(node_info$region), nrow(node_info) == n_nodes)

# ---------------------------------------------------------------------------
# Coverage, inputs, method
member_tbl <- members %>% dplyr::mutate(node_only = as.logical(node_only))
nodes_no_es <- spec %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(n_eo_es = sum(node_components_n == 2), n_eo_only = sum(node_components_n == 1),
                   nodes_eo_only = paste(sort(node_lab(iso3c[node_components_n == 1])), collapse = " "),
                   n_zero_demand = sum(demand_weight <= 0),
                   zero_demand = paste(sort(node_lab(iso3c[demand_weight <= 0])), collapse = " "),
                   .groups = "drop")
coverage <- list(
  lambda = lambda,
  n_members = sum(!member_tbl$node_only),
  n_core = sum(member_tbl$tier == "core"), n_partner = sum(member_tbl$tier == "partner"),
  n_node_only_eu = sum(member_tbl$node_only), node_only_eu = I(sort(member_tbl$iso3[member_tbl$node_only])),
  n_economies = nrow(member_tbl), n_nodes = length(node_codes),
  eu_members = nodes$n_members[nodes$network_node == eu_code],
  n_stages = nrow(diag), stages_by_supply_chain = as.list(table(diag$supply_chain)),
  methods = as.list(table(diag$method)),
  n_fallback = sum(!is.na(diag$fallback_reason) & nzchar(diag$fallback_reason)),
  n_nodes_per_stage = I(sort(unique(diag$n_nodes))),
  n_nodes_no_score = sum(diag$n_nodes_no_score),
  node_policy_term = "off (producer score = EO and ES)",
  stages_nodes_scored_on_eo_only = nodes_no_es %>% dplyr::filter(n_eo_only > 0) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), n_eo_only, nodes_eo_only),
  stages_with_zero_demand_node = nodes_no_es %>% dplyr::filter(n_zero_demand > 0) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), zero_demand),
  hhi_tangent_max_error = max(diag$hhi_tangent_max_error), hhi_tangent_error_bound = unique(diag$hhi_tangent_error_bound),
  input_checks = checks,
  input_checks_all_unique = all(checks$dup_keys == 0 & checks$multivalued_keys == 0 & checks$rows_in == checks$keys_out)
)

# ---------------------------------------------------------------------------
# Producers, shares, concentration of the design
share_vals <- prod$production_share
diag_out <- diag %>%
  dplyr::left_join(prod %>% dplyr::group_by(tech, supply_chain) %>%
                     dplyr::slice_max(production_share, n = 1, with_ties = FALSE) %>%
                     dplyr::ungroup() %>% dplyr::select(tech, supply_chain, top_producer = iso3c),
                   by = c("tech", "supply_chain")) %>%
  dplyr::transmute(tech, supply_chain, method, n_producers, hhi, effective_producers,
                   max_production_share, top_producer = node_lab(top_producer),
                   barred = producers_forbidden, objective, objective_node, objective_edge,
                   diversity_penalty, min_suppliers_realized, min_supplier_share_realized)
producers <- list(
  n_producer_slots = nrow(prod),
  n_producers_dist = as.list(table(diag$n_producers)),
  n_producers_median = stats::median(diag$n_producers),
  n_producers_min = min(diag$n_producers), n_producers_max = max(diag$n_producers),
  n_producers_median_by_supply_chain = as.list(tapply(diag$n_producers, diag$supply_chain, stats::median)),
  hhi_min = min(diag$hhi), hhi_median = stats::median(diag$hhi), hhi_max = max(diag$hhi),
  hhi_median_by_supply_chain = as.list(tapply(diag$hhi, diag$supply_chain, stats::median)),
  effective_producers_median = stats::median(diag$effective_producers),
  effective_producers_min = min(diag$effective_producers), effective_producers_max = max(diag$effective_producers),
  stage_hhi_min = diag_out %>% dplyr::slice_min(hhi, n = 1) %>% dplyr::transmute(stage = paste(tech, supply_chain), hhi),
  stage_hhi_max = diag_out %>% dplyr::slice_max(hhi, n = 2) %>% dplyr::transmute(stage = paste(tech, supply_chain), hhi),
  max_share_all = max(share_vals),
  max_share_stage = diag_out %>% dplyr::slice_max(max_production_share, n = 1) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), top_producer, max_production_share),
  max_share_median = stats::median(diag$max_production_share),
  n_stages_max_share_le_030 = sum(diag$max_production_share <= 0.30 + 1e-9),
  n_shares_at_cap_040 = sum(abs(share_vals - 0.40) < 1e-6),
  n_shares_at_floor_005 = sum(abs(share_vals - 0.05) < 1e-6),
  pct_shares_at_floor_005 = 100 * mean(abs(share_vals - 0.05) < 1e-6),
  n_shares_on_breakpoint_grid = sum(abs(share_vals * 200 - round(share_vals * 200)) < 1e-6),
  by_stage = diag_out
)

# Roles per node (Upstream / Midstream), with cap and GDP
roles <- prod %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(n_roles = dplyr::n(), n_up = sum(supply_chain == "Upstream"),
                   n_mid = sum(supply_chain == "Midstream"), total_share = sum(production_share),
                   max_share = max(production_share),
                   stages = paste(sprintf("%s %s (%.3f)", tech, supply_chain, production_share), collapse = "; "),
                   .groups = "drop") %>%
  dplyr::right_join(node_info %>% dplyr::select(iso3c, name, tier, region, income, gdp_usd), by = "iso3c") %>%
  dplyr::mutate(dplyr::across(c(n_roles, n_up, n_mid, total_share, max_share), ~ dplyr::coalesce(.x, 0))) %>%
  dplyr::left_join(caps %>% dplyr::select(iso3c, portfolio_cap), by = "iso3c") %>%
  dplyr::left_join(counts %>% dplyr::select(iso3c, topk_count), by = "iso3c") %>%
  dplyr::arrange(dplyr::desc(n_roles), dplyr::desc(total_share), dplyr::desc(gdp_usd))
tech_spread <- prod %>%
  dplyr::group_by(tech) %>%
  dplyr::summarise(n_distinct_producers = dplyr::n_distinct(iso3c), n_roles = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(n_distinct_producers)
income_share <- prod %>%
  dplyr::left_join(node_info %>% dplyr::select(iso3c, income), by = "iso3c") %>%
  dplyr::group_by(supply_chain) %>%
  dplyr::summarise(share_production_non_high_income = sum(production_share[income != "High income"]) / sum(production_share),
                   share_production_partner_tier = sum(production_share[tier == "partner"]) / sum(production_share),
                   n_stages = dplyr::n_distinct(tech), .groups = "drop")
node_roles <- list(
  roles = roles %>% dplyr::transmute(iso3c = node_lab(iso3c), name, tier, n_roles, n_up, n_mid, total_share,
                                     max_share, portfolio_cap, topk_count, stages),
  n_nodes_selected = sum(roles$n_roles > 0),
  nodes_never_selected = I(node_lab(roles$iso3c[roles$n_roles == 0])),
  nodes_most_roles = roles %>% dplyr::slice_max(n_roles, n = 4) %>% dplyr::transmute(iso3c = node_lab(iso3c), n_roles),
  n_nodes_upstream_only = sum(roles$n_up > 0 & roles$n_mid == 0),
  nodes_upstream_only = I(node_lab(roles$iso3c[roles$n_up > 0 & roles$n_mid == 0])),
  n_nodes_midstream_only = sum(roles$n_mid > 0 & roles$n_up == 0),
  nodes_midstream_only = I(node_lab(roles$iso3c[roles$n_mid > 0 & roles$n_up == 0])),
  spearman_roles_vs_cap = stats::cor(roles$n_roles, roles$portfolio_cap, method = "spearman"),
  spearman_roles_vs_gdp = stats::cor(roles$n_roles, roles$gdp_usd, method = "spearman"),
  tech_spread = tech_spread,
  income_share_by_supply_chain = income_share,
  n_non_high_income_nodes = sum(node_info$income != "High income"),
  tier_counts = as.list(table(node_info$tier))
)

# ---------------------------------------------------------------------------
# The EU node: designed shares and the derived member split
eu_prod <- prod %>% dplyr::filter(iso3c == eu_code) %>% dplyr::arrange(dplyr::desc(production_share))
eu_rank <- topk %>% dplyr::filter(iso3c == eu_code, production_share > 1e-9) %>%
  dplyr::select(tech, supply_chain, rank_in_stage)
eu_split <- mshares %>%
  dplyr::filter(network_node == eu_code, as.logical(selected)) %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::arrange(dplyr::desc(production_share_derived), .by_group = TRUE) %>%
  dplyr::summarise(node_share = dplyr::first(node_production_share),
                   weight_basis = paste(unique(weight_basis), collapse = "/"),
                   top_members = paste(sprintf("%s %.3f", utils::head(iso3c, 3), utils::head(production_share_derived, 3)),
                                       collapse = ", "),
                   n_members_positive = sum(production_share_derived > 1e-12),
                   .groups = "drop")
eu_member_totals <- mshares %>%
  dplyr::filter(network_node == eu_code) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(sum_derived_share = sum(production_share_derived), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(sum_derived_share))
stopifnot(all(abs(mshares %>% dplyr::group_by(tech, supply_chain) %>%
                    dplyr::summarise(s = sum(production_share_derived), .groups = "drop") %>% dplyr::pull(s) - 1) < 1e-9))
eu <- list(
  n_members = nodes$n_members[nodes$network_node == eu_code],
  gdp_usd = nodes$gdp_usd[nodes$network_node == eu_code],
  portfolio_cap = caps$portfolio_cap[caps$iso3c == eu_code],
  topk_count = counts$topk_count[counts$iso3c == eu_code],
  n_stages_selected = nrow(eu_prod),
  n_up = sum(eu_prod$supply_chain == "Upstream"), n_mid = sum(eu_prod$supply_chain == "Midstream"),
  share_median = stats::median(eu_prod$production_share),
  share_min = min(eu_prod$production_share), share_max = max(eu_prod$production_share),
  stages = eu_prod %>% dplyr::left_join(eu_rank, by = c("tech", "supply_chain")) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), production_share, rank_in_stage),
  n_stages_top5 = sum(eu_rank$rank_in_stage <= 5),
  n_stages_top_producer = sum(eu_rank$rank_in_stage == 1),
  barred_stages = I(paste(diag$tech, diag$supply_chain)[grepl(eu_code, diag$producers_forbidden)]),
  midstream_stages_not_selected = I(setdiff(paste(diag$tech[diag$supply_chain == "Midstream"], "Midstream"),
                                            paste(eu_prod$tech, eu_prod$supply_chain))),
  member_split_by_stage = eu_split,
  member_split_weight_basis_counts = as.list(table(mshares$weight_basis[mshares$network_node == eu_code])),
  member_summed_derived_share_top = utils::head(eu_member_totals, 8),
  observed_top_exporter_stages = I(paste(obs_hhi$tech, obs_hhi$supply_chain)[obs_hhi$top_node == eu_code]),
  n_observed_top_exporter = sum(obs_hhi$top_node == eu_code),
  observed_share_median = stats::median(obs_hhi$top_share[obs_hhi$top_node == eu_code]),
  # Rank compression of demand: the demand weight is built from within-coalition S-curved ranks of
  # need and size, so the EU node (one point in each ranking) gets about one node's weight
  gdp_share_of_nodes = nodes$gdp_usd[nodes$network_node == eu_code] / sum(nodes$gdp_usd),
  demand_weight = list(
    median = stats::median(spec$demand_weight[spec$iso3c == eu_code]),
    min = min(spec$demand_weight[spec$iso3c == eu_code]), max = max(spec$demand_weight[spec$iso3c == eu_code])
  ),
  demand_weight_rank_median = stats::median(spec %>% dplyr::group_by(tech, supply_chain) %>%
                                              dplyr::mutate(r = rank(-demand_weight, ties.method = "min")) %>%
                                              dplyr::ungroup() %>% dplyr::filter(iso3c == eu_code) %>% dplyr::pull(r)),
  usa_demand_weight_median = stats::median(spec$demand_weight[spec$iso3c == "USA"]),
  usa_gdp_share_of_nodes = nodes$gdp_usd[nodes$network_node == "USA"] / sum(nodes$gdp_usd),
  demand_weight_max_any_node = max(spec$demand_weight),
  size_idx = unique(spec$size_idx[spec$iso3c == eu_code])
)

# ---------------------------------------------------------------------------
# Flows: redundancy, self supply, routed edge quality
flow_stage <- flows %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(
    n_flows = dplyr::n(),
    self_share = sum(flow_share[reporter_iso == partner_iso]),
    flow_weighted_edge_weight_offdiag = sum((flow_share * edge_weight)[reporter_iso != partner_iso]) /
      sum(flow_share[reporter_iso != partner_iso]),
    share_flow_both_components = sum(flow_share[friendshore_index > 0 & opportunity_index > 0]) / sum(flow_share),
    .groups = "drop"
  )
suppliers_per_consumer <- flows %>%
  dplyr::filter(flow_share > 1e-12) %>%
  dplyr::count(tech, supply_chain, partner_iso, name = "n_suppliers")
flow_stats <- list(
  n_flows_total = nrow(flows),
  n_flows_per_stage_min = min(flow_stage$n_flows), n_flows_per_stage_max = max(flow_stage$n_flows),
  suppliers_per_consumer = as.list(table(suppliers_per_consumer$n_suppliers)),
  n_consumer_stage_pairs = nrow(suppliers_per_consumer),
  min_suppliers_realized_all = min(diag$min_suppliers_realized),
  min_supplier_share_realized_range = I(range(diag$min_supplier_share_realized)),
  n_stages_epsilon_binding = sum(abs(diag$min_supplier_share_realized - 0.10) < 1e-6),
  n_stages_with_self_flow = sum(flow_stage$self_share > 0),
  self_flow = flow_stage %>% dplyr::filter(self_share > 0) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), self_share),
  flow_weighted_edge_weight_offdiag_median = stats::median(flow_stage$flow_weighted_edge_weight_offdiag),
  flow_weighted_edge_weight_offdiag_range = I(range(flow_stage$flow_weighted_edge_weight_offdiag)),
  by_stage = flow_stage
)

# Edge coverage of the node pairs, from the edge-aggregation counts (component rows per stage and
# weight basis: "member" = a pair of single-member nodes, "trade"/"stage" = an EU edge)
n_pairs <- length(node_codes) * (length(node_codes) - 1)
edge_cov <- nedges %>%
  dplyr::filter(supply_chain %in% stage_levels) %>%
  dplyr::group_by(component, tech, supply_chain) %>%
  # (the EU counts first: summarise() would otherwise see the summed n_edges)
  dplyr::summarise(n_eu_trade = sum(n_edges[weight_basis == "trade"]),
                   n_eu_stage = sum(n_edges[weight_basis == "stage"]),
                   n_eu_equal = sum(n_edges[weight_basis == "equal"]),
                   n_edges = sum(n_edges), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = component, values_from = c(n_edges, n_eu_trade, n_eu_stage, n_eu_equal), values_fill = 0)
stopifnot(!anyNA(edge_cov))
edges <- list(
  n_offdiag_pairs_per_stage = n_pairs,
  n_stages_friendshore_complete = sum(edge_cov$n_edges_friendshore == n_pairs),
  opportunity_share_of_pairs = edge_cov %>%
    dplyr::transmute(stage = paste(tech, supply_chain), friendshore = n_edges_friendshore / n_pairs,
                     opportunity = n_edges_opportunity / n_pairs),
  stages_without_opportunity = I(paste(edge_cov$tech, edge_cov$supply_chain)[edge_cov$n_edges_opportunity == 0]),
  n_eu_edges = sum(edge_cov$n_eu_trade_friendshore + edge_cov$n_eu_stage_friendshore + edge_cov$n_eu_equal_friendshore +
                     edge_cov$n_eu_trade_opportunity + edge_cov$n_eu_stage_opportunity + edge_cov$n_eu_equal_opportunity),
  eu_edges_trade_weighted_share = (sum(edge_cov$n_eu_trade_friendshore) + sum(edge_cov$n_eu_trade_opportunity)) /
    sum(edge_cov$n_eu_trade_friendshore + edge_cov$n_eu_stage_friendshore + edge_cov$n_eu_equal_friendshore +
          edge_cov$n_eu_trade_opportunity + edge_cov$n_eu_stage_opportunity + edge_cov$n_eu_equal_opportunity),
  n_stages_eu_edges_all_trade_weighted = sum(edge_cov$n_eu_stage_friendshore + edge_cov$n_eu_equal_friendshore == 0 &
                                               edge_cov$n_eu_trade_friendshore > 0),
  intra_eu_member_edges_dropped = nedges %>% dplyr::distinct(component, intra_node_member_edges_dropped),
  # Over all 22 x n_pairs ordered (pair, stage) combinations (each pair has the friendshore
  # component where it has the opportunity one; checked)
  share_pair_stages_both_components = sum(edge_cov$n_edges_opportunity) / (n_pairs * nrow(edge_cov)),
  share_pair_stages_friendshore_only = sum(edge_cov$n_edges_friendshore - edge_cov$n_edges_opportunity) /
    (n_pairs * nrow(edge_cov)),
  n_stages_opportunity_complete = sum(edge_cov$n_edges_opportunity == n_pairs)
)
stopifnot(all(edge_cov$n_edges_friendshore >= edge_cov$n_edges_opportunity))

# ---------------------------------------------------------------------------
# Observed concentration: (1) the network's own nodes (allied_network_observed_hhi.csv, the
# calibration reference) and (2) world exports across all reporters (trade theme, raw exports)
trade <- readRDS(file.path(processed_dir, "trade_concentration_tbl.rds"))
exports <- trade %>%
  dplyr::filter(variable == "exports", data_type == "raw", is.finite(value), value > 0) %>%
  dplyr::select(iso3c, tech, supply_chain, exports = value, Year)
hhi_stored <- trade %>%
  dplyr::filter(variable == "HHI", data_type == "raw") %>%
  dplyr::distinct(tech, supply_chain, HHI_W_stored = value)
comtrade_year <- unique(exports$Year)
rm(trade); invisible(gc())
network_economies <- member_tbl$iso3
world <- exports %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::mutate(share = exports / sum(exports), rank_world = rank(-exports, ties.method = "first")) %>%
  dplyr::ungroup()
world_stage <- world %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(
    n_reporters = dplyr::n(),
    HHI_W = sum(share^2),
    world_leader = iso3c[which.max(share)], world_leader_share = max(share),
    world_leader_in_network = world_leader %in% network_economies,
    top3_world = paste(iso3c[order(-share)][1:3], collapse = " "),
    china_rank = if (any(iso3c == "CHN")) rank_world[iso3c == "CHN"] else NA_integer_,
    china_share = if (any(iso3c == "CHN")) share[iso3c == "CHN"] else NA_real_,
    network_share_of_world = sum(share[iso3c %in% network_economies]),
    .groups = "drop"
  ) %>%
  dplyr::left_join(hhi_stored, by = c("tech", "supply_chain"))
stopifnot(max(abs(world_stage$HHI_W - world_stage$HHI_W_stored), na.rm = TRUE) < 1e-9)
conc <- diag %>%
  dplyr::select(tech, supply_chain, HHI_D = hhi, max_share_D = max_production_share, n_producers) %>%
  dplyr::left_join(obs_hhi %>% dplyr::select(tech, supply_chain, HHI_N = hhi, top_node, top_share_N = top_share,
                                             effective_exporters, n_nodes_exporting),
                   by = c("tech", "supply_chain")) %>%
  dplyr::left_join(world_stage %>% dplyr::select(-HHI_W_stored), by = c("tech", "supply_chain"))
conc13 <- conc %>% dplyr::filter(!is.na(HHI_N))
stopifnot(nrow(conc13) == nrow(obs_hhi), all(!is.na(conc13$HHI_W)))
# Designed top producer vs observed top exporting node
top_design <- prod %>% dplyr::group_by(tech, supply_chain) %>%
  dplyr::slice_max(production_share, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
  dplyr::select(tech, supply_chain, top_design = iso3c)
conc13 <- conc13 %>% dplyr::left_join(top_design, by = c("tech", "supply_chain"))
concentration <- list(
  description = paste(
    "HHI_D = Herfindahl index of designed production shares (allied_network_diagnostics.csv, 22 stages).",
    "HHI_N = Herfindahl index of the network nodes' 2024 basket exports (allied_network_observed_hhi.csv:",
    "reconciled Comtrade matrix, importer report else exporter mirror x 1.06; shares within the", n_nodes, "nodes;",
    "the EU-27 node counts extra-EU exports only),", nrow(obs_hhi), "stages with an HS6 basket. HHI_W = Herfindahl",
    "index of", comtrade_year, "exports of the basket across all reporters of the trade theme",
    "(trade_concentration_tbl, variable exports, raw; equal to its stored raw HHI; Hong Kong is not among the",
    "exporters, as a re-export hub). network_share_of_world = share of world exports of the",
    nrow(member_tbl), "network economies (", sum(!member_tbl$node_only), "members +", sum(member_tbl$node_only),
    "EU members outside the tiers; gross, incl. intra-EU)."),
  comtrade_year = comtrade_year,
  n_stages_with_observed = nrow(conc13),
  stages_without_basket = I(paste(conc$tech, conc$supply_chain)[is.na(conc$HHI_N)]),
  calibration_target_median_HHI_N = stats::median(conc13$HHI_N),
  calibration_target_equivalent_exporters = 1 / stats::median(conc13$HHI_N),
  HHI_D_median_22 = stats::median(conc$HHI_D),
  HHI_D_equivalent_22 = 1 / stats::median(conc$HHI_D),
  margin_target_minus_design = stats::median(conc13$HHI_N) - stats::median(conc$HHI_D),
  HHI_D_basket = list(min = min(conc13$HHI_D), median = stats::median(conc13$HHI_D), max = max(conc13$HHI_D)),
  HHI_N = list(min = min(conc13$HHI_N), median = stats::median(conc13$HHI_N), max = max(conc13$HHI_N)),
  HHI_W = list(min = min(conc13$HHI_W), median = stats::median(conc13$HHI_W), max = max(conc13$HHI_W)),
  equivalent = list(design_basket = 1 / stats::median(conc13$HHI_D), nodes = 1 / stats::median(conc13$HHI_N),
                    world = 1 / stats::median(conc13$HHI_W)),
  n_stages_HHI_D_below_HHI_N = sum(conc13$HHI_D < conc13$HHI_N),
  stages_HHI_D_above_HHI_N = conc13 %>% dplyr::filter(HHI_D >= HHI_N) %>%
    dplyr::transmute(stage = paste(tech, supply_chain), HHI_D, HHI_N),
  n_stages_HHI_D_below_HHI_W = sum(conc13$HHI_D < conc13$HHI_W),
  n_stages_HHI_N_above_HHI_W = sum(conc13$HHI_N > conc13$HHI_W),
  spearman_HHI_D_vs_HHI_N = stats::cor(conc13$HHI_D, conc13$HHI_N, method = "spearman"),
  max_share_D_basket = list(min = min(conc13$max_share_D), median = stats::median(conc13$max_share_D), max = max(conc13$max_share_D)),
  top_share_N = list(min = min(conc13$top_share_N), median = stats::median(conc13$top_share_N), max = max(conc13$top_share_N)),
  world_leader_share = list(min = min(conc13$world_leader_share), median = stats::median(conc13$world_leader_share),
                            max = max(conc13$world_leader_share)),
  n_stages_design_max_below_node_top = sum(conc13$max_share_D < conc13$top_share_N),
  n_stages_top_design_equals_top_node = sum(conc13$top_design == conc13$top_node),
  observed_top_node_counts = as.list(table(node_lab(conc13$top_node))),
  n_stages_china_leads_world = sum(conc13$world_leader == "CHN"),
  stages_china_leads_world = conc13 %>% dplyr::filter(world_leader == "CHN") %>%
    dplyr::transmute(stage = paste(tech, supply_chain), china_share),
  china_share_when_leading = list(min = min(conc13$china_share[conc13$world_leader == "CHN"]),
                                  median = stats::median(conc13$china_share[conc13$world_leader == "CHN"]),
                                  max = max(conc13$china_share[conc13$world_leader == "CHN"])),
  n_stages_world_leader_outside_network = sum(!conc13$world_leader_in_network),
  world_leaders_outside_network = as.list(table(conc13$world_leader[!conc13$world_leader_in_network])),
  network_share_of_world = list(min = min(conc13$network_share_of_world),
                                median = stats::median(conc13$network_share_of_world),
                                max = max(conc13$network_share_of_world)),
  n_reporters_range = I(range(conc13$n_reporters)),
  semiconductors_note = "trade theme also has Semiconductors Midstream, not a network technology; excluded",
  by_stage = conc
)

# ---------------------------------------------------------------------------
# Designed producers vs the nodes' observed exporters. The node-level 2024 exports come from the
# lambda study's reconciled basket trade (<study dir>/trade_cells_2024.rds, built on the final basket),
# aggregated as allied_network_observed_export_hhi() aggregates them (a bundled node counts exports
# outside the node only); the per-stage HHI must reproduce allied_network_observed_hhi.csv.
tc <- readRDS(file.path(study_dir, "trade_cells_2024.rds"))
tc_value <- intersect(c("value_last", "value"), names(tc))[1]
node_of <- stats::setNames(as.character(member_tbl$network_node), as.character(member_tbl$iso3))
node_exp <- tibble::tibble(
  exporter_iso = as.character(tc$exporter_iso), importer_iso = as.character(tc$importer_iso),
  tech = as.character(tc$tech), supply_chain = as.character(tc$supply_chain),
  value = dplyr::coalesce(suppressWarnings(as.numeric(tc[[tc_value]])), 0)
) %>%
  dplyr::filter(exporter_iso %in% names(node_of), exporter_iso != importer_iso) %>%
  dplyr::mutate(node = unname(node_of[exporter_iso]), importer_node = unname(node_of[importer_iso])) %>%
  dplyr::filter(is.na(importer_node) | importer_node != node) %>%
  dplyr::semi_join(diag, by = c("tech", "supply_chain")) %>%
  dplyr::group_by(tech, supply_chain, node) %>%
  dplyr::summarise(exports = sum(value), .groups = "drop") %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::filter(sum(exports) > 0) %>%
  dplyr::mutate(export_share = exports / sum(exports), export_rank = rank(-exports, ties.method = "first")) %>%
  dplyr::ungroup()
rm(tc); invisible(gc())
exp_chk <- node_exp %>% dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(hhi_re = sum(export_share^2), .groups = "drop") %>%
  dplyr::inner_join(obs_hhi %>% dplyr::select(tech, supply_chain, hhi), by = c("tech", "supply_chain"))
stopifnot(nrow(exp_chk) == nrow(obs_hhi), max(abs(exp_chk$hhi_re - exp_chk$hhi)) < 1e-9)
vs_exp <- spec %>%
  dplyr::select(tech, supply_chain, iso3c, production_share) %>%
  dplyr::inner_join(dplyr::distinct(node_exp, tech, supply_chain), by = c("tech", "supply_chain")) %>%
  dplyr::left_join(node_exp %>% dplyr::select(tech, supply_chain, iso3c = node, export_share, export_rank),
                   by = c("tech", "supply_chain", "iso3c")) %>%
  dplyr::mutate(export_share = dplyr::coalesce(export_share, 0), top3 = dplyr::coalesce(export_rank <= 3, FALSE)) %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(
    share_design_outside_top3 = sum(production_share[!top3]),
    n_design_producers = sum(production_share > 1e-9),
    n_top3_designed = sum(top3 & production_share > 1e-9),
    top3_exporters = paste(node_lab(iso3c[top3][order(export_share[top3], decreasing = TRUE)]), collapse = " "),
    spearman_share_vs_exports = stats::cor(production_share, export_share, method = "spearman"),
    .groups = "drop"
  )
design_vs_exporters <- list(
  description = paste(
    "Per basket stage: share of designed production assigned to nodes outside the three largest exporting nodes",
    "(2024 reconciled basket trade, EU counting extra-EU exports, the calibration reference) and the Spearman rank",
    "correlation across the", n_nodes, "nodes between designed production share and observed export share (zeros",
    "included)."),
  n_stages = nrow(vs_exp),
  share_outside_top3 = list(median = stats::median(vs_exp$share_design_outside_top3),
                            min = min(vs_exp$share_design_outside_top3), max = max(vs_exp$share_design_outside_top3)),
  n_stages_all_top3_designed = sum(vs_exp$n_top3_designed == 3),
  n_stages_no_top3_designed = sum(vs_exp$n_top3_designed == 0),
  n_top3_designed_dist = as.list(table(vs_exp$n_top3_designed)),
  spearman_share_vs_exports = list(median = stats::median(vs_exp$spearman_share_vs_exports),
                                   min = min(vs_exp$spearman_share_vs_exports),
                                   max = max(vs_exp$spearman_share_vs_exports)),
  by_stage = vs_exp
)

# ---------------------------------------------------------------------------
# Portfolio caps; counts without caps from the study's no-caps run at the production lambda
nc_path <-file.path(study_dir, "runs", sprintf("nc%d_l%s.rds", n_nodes, format(lambda)))
if (!file.exists(nc_path)) stop("No-caps study run not found: ", nc_path, " (set OPSI_NETWORK_STUDY_DIR).")
nc <- readRDS(nc_path)
nc_ok_params <- isFALSE(nc$params$portfolio_enable) && isTRUE(all.equal(nc$params$diversity_lambda, lambda)) &&
  identical(nc$params$node_policy, FALSE) && setequal(nc$params$supply_chains, stage_levels) &&
  nc$params$w_edge == 0.5 && nc$params$demand_mode == "mixed" && nc$params$max_share == 0.40 &&
  nc$params$min_share == 0.05 && nc$params$min_suppliers_per_consumer == 2 &&
  nc$params$epsilon_supplier_share == 0.10 && nc$params$auto_milp_max_nodes >= n_nodes
nc_inputs <- spec %>%
  dplyr::select(tech, supply_chain, iso3c, a = producer_score, d = demand_weight, w = connectivity_out) %>%
  dplyr::inner_join(nc$specialization %>% dplyr::select(tech, supply_chain, iso3c, a2 = producer_score,
                                                          d2 = demand_weight, w2 = connectivity_out),
                    by = c("tech", "supply_chain", "iso3c"))
nc_input_diff <- max(abs(c(nc_inputs$a - nc_inputs$a2, nc_inputs$d - nc_inputs$d2, nc_inputs$w - nc_inputs$w2)), na.rm = TRUE)
if (!nc_ok_params || nrow(nc_inputs) != nrow(spec) || nc_input_diff > 1e-9) {
  stop("The no-caps study run does not match the production settings or inputs: ", nc_path)
}
cap_by_iso <- stats::setNames(caps$portfolio_cap, caps$iso3c)
topk_nc <- allied_network_build_topk_tbl(nc$specialization, portfolio_top_k = 5)
counts_nc <- allied_network_count_topk(topk_nc, node_codes, cap_by_iso)
# Check that the stored top-K table gives the stored capped counts. (Recomputing the strict top-K rank
# from the CSV is not exact: shares tied at fifth place in the CSV can differ in the last bits inside
# the design, which then decides the tie before the producer-score tie-break does.)
counts_chk <- allied_network_count_topk(topk %>% dplyr::mutate(in_top_k = as.logical(in_top_k)), node_codes, cap_by_iso)
stopifnot(all(counts_chk$topk_count[match(counts$iso3c, counts_chk$iso3c)] == counts$topk_count))
topk_ties_at_k <- topk %>%
  dplyr::filter(production_share > 1e-9) %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(tied_at_k = sum(abs(production_share - production_share[topk_rank == 5]) < 1e-9) > 1,
                   .groups = "drop")
port <- caps %>%
  dplyr::select(iso3c, portfolio_cap, gdp_usd) %>%
  dplyr::left_join(counts %>% dplyr::select(iso3c, count_capped = topk_count), by = "iso3c") %>%
  dplyr::left_join(counts_nc %>% dplyr::select(iso3c, count_uncapped = topk_count), by = "iso3c") %>%
  dplyr::left_join(roles %>% dplyr::select(iso3c, n_roles), by = "iso3c") %>%
  dplyr::left_join(nc$specialization %>% dplyr::filter(production_share > 1e-9) %>% dplyr::count(iso3c, name = "n_roles_uncapped"),
                   by = "iso3c") %>%
  dplyr::mutate(n_roles_uncapped = dplyr::coalesce(n_roles_uncapped, 0L)) %>%
  dplyr::arrange(dplyr::desc(gdp_usd))
barred <- diag %>%
  dplyr::select(tech, supply_chain, producers_forbidden) %>%
  dplyr::mutate(iso3c = strsplit(dplyr::coalesce(producers_forbidden, ""), " ")) %>%
  tidyr::unnest(iso3c) %>%
  dplyr::filter(nzchar(iso3c)) %>%
  dplyr::select(tech, supply_chain, iso3c)
J_capped <- sum(diag$objective); J_uncapped <- sum(nc$diagnostics$objective)
J_stage <- diag %>% dplyr::select(tech, supply_chain, J = objective) %>%
  dplyr::inner_join(nc$diagnostics %>% dplyr::select(tech, supply_chain, J_nc = objective), by = c("tech", "supply_chain")) %>%
  dplyr::mutate(loss_pct = 100 * (J_nc - J) / J_nc)
portfolio <- list(
  caps = stats::setNames(as.list(port$portfolio_cap), node_lab(port$iso3c)),
  caps_sum = sum(port$portfolio_cap), caps_range = I(range(port$portfolio_cap)),
  required_min = 3 * nrow(diag),
  count_capped_sum = sum(port$count_capped),
  n_nodes_at_cap = sum(port$count_capped == port$portfolio_cap),
  nodes_at_cap = I(node_lab(port$iso3c[port$count_capped == port$portfolio_cap])),
  n_nodes_zero_top5 = sum(port$count_capped == 0),
  converged = all(as.logical(diag$portfolio_converged)), passes = unique(diag$portfolio_iters),
  violations_remaining = unique(diag$portfolio_violations_remaining),
  n_exclusions_final = nrow(barred),
  n_stages_with_exclusions = dplyr::n_distinct(paste(barred$tech, barred$supply_chain)),
  exclusions_by_node = barred %>% dplyr::count(iso3c, name = "n_barred") %>% dplyr::arrange(dplyr::desc(n_barred)) %>%
    dplyr::mutate(iso3c = node_lab(iso3c)),
  uncapped_source = basename(nc_path),
  uncapped_inputs_max_abs_diff = nc_input_diff,
  count_uncapped = stats::setNames(as.list(port$count_uncapped), node_lab(port$iso3c)),
  n_nodes_over_cap_uncapped = sum(port$count_uncapped > port$portfolio_cap),
  nodes_over_cap_uncapped = port %>% dplyr::filter(count_uncapped > portfolio_cap) %>%
    dplyr::transmute(iso3c = node_lab(iso3c), count_uncapped, portfolio_cap),
  n_nodes_zero_top5_uncapped = sum(port$count_uncapped == 0),
  n_nodes_no_role_uncapped = sum(port$n_roles_uncapped == 0),
  n_nodes_no_role_capped = sum(port$n_roles == 0),
  hhi_median_uncapped = stats::median(nc$diagnostics$hhi),
  objective_sum_capped = J_capped, objective_sum_uncapped = J_uncapped,
  objective_loss_pct = 100 * (J_uncapped - J_capped) / J_uncapped,
  objective_loss_pct_stage_max = max(J_stage$loss_pct),
  n_stages_objective_lower = sum(J_stage$loss_pct > 1e-7),
  n_stages_tied_at_fifth_place = sum(topk_ties_at_k$tied_at_k),
  stages_tied_at_fifth_place = I(paste(topk_ties_at_k$tech, topk_ties_at_k$supply_chain)[topk_ties_at_k$tied_at_k]),
  gdp_note = "GDP: WDI NY.GDP.MKTP.CD, latest finite year per member; the EU node sums its 27 members"
)

# ---------------------------------------------------------------------------
# Build candidates: nodes with an observed development potential
dev <- readRDS(file.path(processed_dir, "partner_development_country_tbl.rds"))
dev_obs <- dev %>%
  dplyr::filter(category == "development", variable == "Development Potential Index", data_type == "index",
                partner_iso %in% node_codes, is.finite(value)) %>%
  dplyr::distinct(iso3c = partner_iso, tech, supply_chain)
rm(dev); invisible(gc())
build_top <- build %>%
  dplyr::semi_join(dev_obs, by = c("iso3c", "tech", "supply_chain")) %>%
  dplyr::arrange(dplyr::desc(build_priority)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::mutate(selected = as.logical(selected))
build_stats <- list(
  nodes_with_observed_dp = I(sort(unique(dev_obs$iso3c))),
  top10 = build_top %>% dplyr::select(iso3c, tech, supply_chain, dev_potential, producer_score,
                                      connectivity_out, build_priority, selected, production_share),
  n_top10_selected = sum(build_top$selected)
)

# ---------------------------------------------------------------------------
# Endowment of the designed Upstream producers. The SI's allied picks screen Upstream stages on a
# recorded resource base (strategic_resource_base(): technology-level Reserves or Production > 0 in
# the reserves and critical-minerals production themes, as scripts/20 builds it); the network
# design does not. Here the same rule is applied, after the fact, to the designed Upstream
# producers; the EU node counts as endowed in a stage when any of its 27 members is.
source(file.path(root, "R", "indices", "build_indices.R"))  # strategic_resource_base()
rb_tbl <- strategic_resource_base(readRDS(file.path(processed_dir, "reserves_tbl.rds")),
                                  readRDS(file.path(processed_dir, "critical_minerals_production_tbl.rds")))
rb_iso <- rb_tbl %>%
  dplyr::mutate(iso3 = country_name_to_iso3(Country)) %>%
  dplyr::filter(!is.na(iso3)) %>%
  dplyr::inner_join(member_tbl %>% dplyr::select(iso3, network_node), by = "iso3") %>%
  dplyr::group_by(iso3c = network_node, tech) %>%
  dplyr::summarise(resource_base = any(resource_base), .groups = "drop")
screened_techs <- sort(unique(rb_tbl$tech))
up_prod <- prod %>%
  dplyr::filter(supply_chain == "Upstream") %>%
  dplyr::left_join(rb_iso, by = c("iso3c", "tech")) %>%
  dplyr::mutate(screened = tech %in% screened_techs,
                resource_base = dplyr::if_else(screened, dplyr::coalesce(resource_base, FALSE), NA))
up_no_base <- up_prod %>% dplyr::filter(resource_base %in% FALSE)
endowment <- list(
  description = paste("Designed Upstream producers checked against the SI endowment screen",
                      "(technology-level Reserves or Production > 0; EU node endowed if any member is).",
                      "Technologies without reserves or production records are not screened."),
  screened_techs = I(screened_techs),
  n_upstream_roles = nrow(up_prod),
  n_upstream_roles_screened = sum(up_prod$screened),
  n_upstream_roles_no_base = nrow(up_no_base),
  share_upstream_production_no_base_by_stage = up_prod %>%
    dplyr::filter(screened) %>%
    dplyr::group_by(stage = paste(tech, supply_chain)) %>%
    dplyr::summarise(share_no_base = sum(production_share[resource_base %in% FALSE]),
                     n_no_base = sum(resource_base %in% FALSE), n_producers = dplyr::n(), .groups = "drop"),
  roles_no_base = up_no_base %>%
    dplyr::transmute(stage = paste(tech, supply_chain), node = node_lab(iso3c), production_share) %>%
    dplyr::arrange(stage, dplyr::desc(production_share))
)
endowment$median_stage_share_no_base <- stats::median(endowment$share_upstream_production_no_base_by_stage$share_no_base)

results <- list(
  description = paste0("Allied network design, production configuration of scripts/30 (", n_nodes,
                       " nodes with the EU-27 as one ",
                       "node, Upstream and Midstream, diversification lambda = ", lambda, ", GDP-log portfolio caps, ",
                       "MILP), read from data/processed/outputs/allied_network_*.csv."),
  coverage = coverage,
  producers = producers,
  node_roles = node_roles,
  eu_node = eu,
  flows = flow_stats,
  edges = edges,
  concentration = concentration,
  design_vs_exporters = design_vs_exporters,
  portfolio = portfolio,
  build_candidates = build_stats,
  endowment = endowment
)
write_paper_results(results, "network")

# ---------------------------------------------------------------------------
# Table: designed producers by technology and stage
fmt_share <- function(x) sub("^0", "", sprintf("%.2f", x))
prod_cells <- prod %>%
  dplyr::arrange(tech, supply_chain, dplyr::desc(production_share), iso3c) %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(cell = paste(sprintf("%s~%s", node_lab(iso3c), fmt_share(production_share)), collapse = ", "),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = supply_chain, values_from = cell) %>%
  dplyr::select(tech, dplyr::all_of(stage_levels)) %>%
  dplyr::arrange(tech)
tab_lines <- c(
  "% Generated by scripts/paper/fig_network.R from data/processed/outputs/allied_network_specialization.csv",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Designed producers and production shares by technology and stage}",
  "\\label{tab:network_producers}",
  "\\footnotesize",
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}p{2.1cm} >{\\raggedright\\arraybackslash}X >{\\raggedright\\arraybackslash}X@{}}",
  "\\toprule",
  "Technology & Upstream & Midstream \\\\",
  "\\midrule",
  sprintf("%s & %s & %s \\\\", prod_cells$tech, prod_cells$Upstream, prod_cells$Midstream),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\emph{Notes:} Producers of each stage in the production design (", n_nodes, " network nodes, ",
         "diversification weight $\\lambda=", lambda, "$, portfolio caps on, every stage solved by the MILP), ",
         "ordered by the designed production share $x_{p,ts}$, the share of the coalition's unit demand in the ",
         "stage that the node supplies (ISO3 codes; EU = the EU-27 node; shares sum to one within a cell and are ",
         "printed to two decimals). Source: \\texttt{allied\\_network\\_specialization.csv}."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tab_lines, file.path(tab_dir, "network_producers.tex"))

# Table: the EU node's designed shares split back to its members (derived, not designed)
basis_lab <- c(production = "production", exports = "exports", gdp = "GDP")
fmt3 <- function(x) sub("^0", "", sprintf("%.3f", x))
eu_tab <- mshares %>%
  dplyr::filter(network_node == eu_code, node_production_share > 1e-9) %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::arrange(dplyr::desc(production_share_derived), .by_group = TRUE) %>%
  dplyr::summarise(node_share = dplyr::first(node_production_share),
                   basis = paste(unique(weight_basis), collapse = "/"),
                   top3 = paste(sprintf("%s~%s", utils::head(iso3c, 3), fmt3(utils::head(production_share_derived, 3))),
                                collapse = ", "),
                   top3_weight = sum(utils::head(member_weight, 3)),
                   n_members_positive = sum(member_weight > 1e-12),
                   .groups = "drop") %>%
  dplyr::arrange(factor(supply_chain, levels = stage_levels), dplyr::desc(node_share))
stopifnot(nrow(eu_tab) == nrow(eu_prod), all(eu_tab$basis %in% names(basis_lab)))
eu_tab_lines <- c(
  "% Generated by scripts/paper/fig_network.R from data/processed/outputs/allied_network_member_shares.csv",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{The EU node's designed production shares and their split to member states}",
  "\\label{tab:network_eu_split}",
  "\\footnotesize",
  "\\begin{tabular}{@{}l l r l l r@{}}",
  "\\toprule",
  "Technology & Stage & EU share & Weights & Three largest members (derived share) & Top-3 weight \\\\",
  "\\midrule",
  sprintf("%s & %s & %s & %s & %s & %s \\\\", eu_tab$tech, eu_tab$supply_chain, fmt3(eu_tab$node_share),
          unname(basis_lab[eu_tab$basis]), eu_tab$top3, sub("^0", "", sprintf("%.2f", eu_tab$top3_weight))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\emph{Notes:} Stages in which the EU-27 node produces in the production design (",
         nrow(eu_tab), " of ", nrow(diag), "). EU share: the node's designed production share. The design solves the EU ",
         "as one node; its share is split back to the 27 member states in proportion to the stage weights that ",
         "aggregate their inputs (Weights: the members' production or export shares in the stage, else their GDP ",
         "shares), so the member figures are derived, not designed. Top-3 weight: the combined weight of the three ",
         "largest members within the node. ISO3 codes. Source: \\texttt{allied\\_network\\_member\\_shares.csv}."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
writeLines(eu_tab_lines, file.path(tab_dir, "network_eu_split.tex"))
results$eu_node$split_table <- eu_tab

# ---------------------------------------------------------------------------
# Figure: node x stage production-share matrix
tech_levels <- sort(unique(spec$tech))
row_tbl <- roles %>%
  dplyr::mutate(lab = sprintf("%s (%d)", name, as.integer(n_roles)))
heat <- tidyr::expand_grid(iso3c = node_codes, tech = tech_levels, supply_chain = stage_levels) %>%
  dplyr::left_join(spec %>% dplyr::select(iso3c, tech, supply_chain, production_share),
                   by = c("iso3c", "tech", "supply_chain")) %>%
  dplyr::left_join(barred %>% dplyr::mutate(barred = TRUE), by = c("iso3c", "tech", "supply_chain")) %>%
  dplyr::mutate(
    production_share = dplyr::coalesce(production_share, 0),
    barred = dplyr::coalesce(barred, FALSE),
    member = factor(row_tbl$lab[match(iso3c, row_tbl$iso3c)], levels = rev(row_tbl$lab)),
    supply_chain = factor(supply_chain, levels = stage_levels),
    tech = factor(tech, levels = tech_levels)
  )
stopifnot(!anyNA(heat$member))
share_top <- ceiling(max(heat$production_share) * 100) / 100
stopifnot(share_top <= 0.40)
p_spec <- ggplot(heat, aes(x = tech, y = member)) +
  geom_tile(fill = paper_ink$neutral, colour = "white", linewidth = 0.35) +
  geom_tile(data = heat %>% dplyr::filter(production_share > 0), aes(fill = production_share),
            colour = "white", linewidth = 0.35) +
  geom_point(data = heat %>% dplyr::filter(barred), aes(shape = "Barred by the portfolio cap"),
             size = 1.5, stroke = 0.45, colour = paper_ink$secondary) +
  facet_grid(. ~ supply_chain, scales = "free_x", space = "free_x") +
  scale_fill_gradientn(colours = paper_seq_blue, limits = c(0.05, share_top), breaks = seq(0.05, 0.30, 0.05),
                       labels = sprintf("%.2f", seq(0.05, 0.30, 0.05)), name = "Designed production share") +
  scale_shape_manual(values = c("Barred by the portfolio cap" = 4), name = NULL) +
  guides(fill = guide_colourbar(order = 1, barwidth = unit(2.0, "in"), barheight = unit(0.12, "in"),
                                title.position = "left", title.vjust = 0.9),
         shape = guide_legend(order = 2)) +
  labs(x = NULL, y = NULL) +
  theme_paper(base_size = 8.5, grid = "none") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 7.5),
    axis.line.x = element_blank(),
    strip.text = element_text(size = 8.5, hjust = 0.5),
    panel.spacing.x = unit(0.5, "lines"),
    legend.margin = margin(0, 0, 0, 0)
  )
n_rows <- nlevels(heat$member)
bands <- tibble::tibble(supply_chain = factor(stage_levels, levels = stage_levels))
for (sc in stage_levels) {
  p_spec <- p_spec +
    geom_rect(data = bands[bands$supply_chain == sc, ], inherit.aes = FALSE,
              aes(xmin = -Inf, xmax = Inf), ymin = n_rows + 0.62, ymax = n_rows + 0.98,
              fill = paper_stage_cols[[sc]])
}
p_spec <- p_spec + scale_y_discrete(expand = expansion(add = c(0.6, 1.05)))
save_paper_fig(p_spec, "network_specialization", width = paper_width_full, height = 6.6)

# ---------------------------------------------------------------------------
# Figure: flows of three Midstream stages. Consumers sit in one fixed order in every panel (World
# Bank region, then GDP); producers sit on the left at the flow-weighted mean position of their
# consumers (the barycentre step of a two-layer Sugiyama layout), spread evenly, so few flows cross.
map_stages <- tibble::tibble(tech = c("Batteries", "Solar", "Wind"), supply_chain = "Midstream")
region_order <- c("North America", "Latin America & Caribbean", "Europe & Central Asia",
                  "Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "East Asia & Pacific")
region_short <- c("North America" = "N. Am.", "Latin America & Caribbean" = "Lat. Am.",
                  "Europe & Central Asia" = "Europe", "Middle East & North Africa" = "MENA",
                  "Sub-Saharan Africa" = "SSA", "South Asia" = "S. Asia", "East Asia & Pacific" = "E. Asia")
stopifnot(all(node_info$region %in% region_order))
cons_order <- node_info %>%
  dplyr::arrange(match(region, region_order), dplyr::desc(gdp_usd)) %>%
  dplyr::mutate(y = rev(seq_len(dplyr::n())))
n_cons <- nrow(cons_order)
x_prod <- 0; x_cons <- 1

stage_layout <- function(tech_, sc_) {
  st_flows <- flows %>% dplyr::filter(tech == tech_, supply_chain == sc_, flow_share > 1e-12)
  st_spec <- spec %>% dplyr::filter(tech == tech_, supply_chain == sc_)
  # Bipartite graph: producer layer "P:" and consumer layer "C:" (a node can be both)
  g <- igraph::graph_from_data_frame(
    st_flows %>% dplyr::transmute(from = paste0("P:", reporter_iso), to = paste0("C:", partner_iso), weight = flow_share),
    directed = TRUE,
    vertices = tibble::tibble(name = c(paste0("P:", st_spec$iso3c[st_spec$producer]), paste0("C:", st_spec$iso3c)))
  )
  igraph::V(g)$type <- grepl("^C:", igraph::V(g)$name)
  stopifnot(igraph::is_bipartite(g))
  indeg <- igraph::degree(g, v = paste0("C:", st_spec$iso3c[st_spec$demand_weight > 0]), mode = "in")
  stopifnot(all(indeg >= 2))
  # Barycentre of each producer over the fixed consumer positions, weighted by its flows
  cons_y <- stats::setNames(cons_order$y, paste0("C:", cons_order$iso3c))
  el <- igraph::as_data_frame(g, what = "edges")
  bary <- el %>% dplyr::group_by(from) %>%
    dplyr::summarise(b = sum(weight * cons_y[to]) / sum(weight), .groups = "drop") %>%
    dplyr::arrange(b)
  n_p <- nrow(bary)
  # spread producers evenly over the consumer range, keeping the barycentre order
  bary$y <- if (n_p == 1) mean(range(cons_y)) else seq(min(cons_y) + 1.5, max(cons_y) - 1.5, length.out = n_p)
  prod_nodes <- bary %>%
    dplyr::transmute(iso3c = sub("^P:", "", from), y) %>%
    dplyr::left_join(st_spec %>% dplyr::select(iso3c, production_share), by = "iso3c") %>%
    dplyr::mutate(x = x_prod)
  edges_xy <- el %>%
    dplyr::transmute(p = sub("^P:", "", from), c = sub("^C:", "", to), flow = weight) %>%
    dplyr::left_join(prod_nodes %>% dplyr::select(p = iso3c, y0 = y), by = "p") %>%
    dplyr::mutate(y1 = cons_y[paste0("C:", c)])
  hhi <- diag$hhi[diag$tech == tech_ & diag$supply_chain == sc_]
  lab <- sprintf("%s %s\n%d producers, HHI %.3f", tech_, tolower(sc_), n_p, hhi)
  list(prod_nodes = prod_nodes %>% dplyr::mutate(panel = lab),
       edges = edges_xy %>% dplyr::mutate(panel = lab),
       cons = cons_order %>% dplyr::mutate(panel = lab, producer = iso3c %in% prod_nodes$iso3c),
       panel = lab, self_share = sum(st_flows$flow_share[st_flows$reporter_iso == st_flows$partner_iso]),
       min_in_degree = min(indeg), n_flows = nrow(st_flows))
}
maps <- purrr::pmap(map_stages, function(tech, supply_chain) stage_layout(tech, supply_chain))
panel_levels <- purrr::map_chr(maps, "panel")
# Smooth (cubic) curves from producer to consumer
curve_pts <- function(e, n = 30) {
  t <- seq(0, 1, length.out = n)
  s <- t * t * (3 - 2 * t)
  purrr::map_dfr(seq_len(nrow(e)), function(i) {
    tibble::tibble(id = i, x = x_prod + 0.06 + (x_cons - 0.03 - x_prod - 0.06) * t,
                   y = e$y0[i] + (e$y1[i] - e$y0[i]) * s, flow = e$flow[i], panel = e$panel[i])
  })
}
map_edges <- purrr::map_dfr(maps, function(m) curve_pts(m$edges) %>% dplyr::mutate(id = paste(m$panel, id))) %>%
  dplyr::mutate(panel = factor(panel, levels = panel_levels))
map_prod <- purrr::map_dfr(maps, "prod_nodes") %>%
  dplyr::mutate(panel = factor(panel, levels = panel_levels),
                label = sprintf("%s %s", node_lab(iso3c), fmt_share(production_share)))
map_cons <- purrr::map_dfr(maps, "cons") %>%
  dplyr::mutate(panel = factor(panel, levels = panel_levels), label = node_lab(iso3c))
# Region separators and labels (consumer side)
reg_tbl <- cons_order %>% dplyr::group_by(region) %>%
  dplyr::summarise(y_top = max(y), y_bot = min(y), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(y_top)) %>%
  dplyr::mutate(band = seq_len(dplyr::n()) %% 2 == 0, lab = region_short[region])
bands_tbl <- tidyr::expand_grid(reg_tbl %>% dplyr::filter(band), panel = factor(panel_levels, levels = panel_levels))
reg_lab_tbl <- reg_tbl %>% dplyr::mutate(panel = factor(panel_levels[3], levels = panel_levels))
p_map <- ggplot() +
  geom_rect(data = bands_tbl, aes(xmin = x_cons - 0.02, xmax = x_cons + 0.36, ymin = y_bot - 0.5, ymax = y_top + 0.5),
            fill = paper_ink$neutral, colour = NA) +
  geom_path(data = map_edges, aes(x = x, y = y, group = id, linewidth = flow),
            colour = paper_stage_cols[["Midstream"]], alpha = 0.55, lineend = "round") +
  scale_linewidth_continuous(range = c(0.15, 1.6), limits = c(0, NA), breaks = c(0.005, 0.015, 0.03),
                             labels = c("0.005", "0.015", "0.030"), name = "Flow (share of stage demand)") +
  geom_point(data = map_cons, aes(x = x_cons, y = y), size = 0.9, colour = paper_ink$muted) +
  geom_text(data = map_cons, aes(x = x_cons + 0.04, y = y, label = label,
                                 fontface = ifelse(producer, "bold", "plain"),
                                 colour = ifelse(producer, "p", "c")),
            hjust = 0, size = 2.5) +
  scale_colour_manual(values = c(p = paper_ink$primary, c = paper_ink$secondary), guide = "none") +
  geom_point(data = map_prod, aes(x = x_prod, y = y, size = production_share), shape = 21,
             fill = paper_stage_cols[["Midstream"]], colour = "white", stroke = 0.5) +
  scale_size_area(max_size = 5.5, limits = c(0, max(0.3, map_prod$production_share)), breaks = c(0.05, 0.15, 0.25),
                  labels = c("0.05", "0.15", "0.25"), name = "Production share") +
  geom_text(data = map_prod, aes(x = x_prod - 0.07, y = y, label = label), hjust = 1, size = 2.65,
            colour = paper_ink$primary) +
  geom_text(data = reg_lab_tbl, aes(x = x_cons + 0.40, y = (y_top + y_bot) / 2, label = lab),
            hjust = 0, size = 2.4, colour = paper_ink$muted, lineheight = 0.9) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(limits = c(-0.60, 1.86), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, n_cons + 0.6), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  guides(size = guide_legend(order = 1, override.aes = list(fill = paper_stage_cols[["Midstream"]])),
         linewidth = guide_legend(order = 2)) +
  theme_paper(base_size = 8.5, grid = "none") +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.line.x = element_blank(),
        strip.text = element_text(size = 8, hjust = 0, lineheight = 1.05),
        panel.spacing.x = unit(0.2, "lines"),
        legend.box = "horizontal", legend.spacing.x = unit(0.3, "lines"),
        plot.margin = margin(4, 4, 4, 4))
save_paper_fig(p_map, "network_map", width = paper_width_full, height = 6.2)

results$network_map <- list(
  stages = panel_levels,
  consumer_order = I(node_lab(cons_order$iso3c)),
  self_flow_share = stats::setNames(as.list(purrr::map_dbl(maps, "self_share")), map_stages$tech),
  min_suppliers_per_consumer = stats::setNames(as.list(purrr::map_dbl(maps, "min_in_degree")), map_stages$tech),
  n_flows = stats::setNames(as.list(purrr::map_dbl(maps, "n_flows")), map_stages$tech),
  producers = map_prod %>% dplyr::transmute(stage = map_stages$tech[match(as.character(panel), panel_levels)],
                                            iso3c = node_lab(iso3c), production_share) %>%
    dplyr::arrange(stage, dplyr::desc(production_share)),
  # share of the stage's routed flow that stays within the consumer's World Bank region
  flows_same_region = purrr::map_dfr(maps, function(m) m$edges %>%
      dplyr::left_join(node_info %>% dplyr::select(c = iso3c, c_region = region), by = "c") %>%
      dplyr::left_join(node_info %>% dplyr::select(p = iso3c, p_region = region), by = "p") %>%
      dplyr::summarise(stage = sub("\n.*", "", m$panel[1]),
                       share_flow_same_region = sum(flow[c_region == p_region]) / sum(flow)))
)

# ---------------------------------------------------------------------------
# Figure: designed vs observed concentration
nodes_series <- sprintf("Exports of the %d network nodes", n_nodes)
conc_long <- conc %>%
  dplyr::select(tech, supply_chain, HHI_D, HHI_N, HHI_W, max_share_D, top_share_N, world_leader_share) %>%
  tidyr::pivot_longer(-c(tech, supply_chain), names_to = "what", values_to = "value") %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    measure = dplyr::if_else(what %in% c("HHI_D", "HHI_N", "HHI_W"), "(a) Herfindahl index", "(b) Largest single share"),
    series = dplyr::case_when(
      what %in% c("HHI_W", "world_leader_share") ~ "World exports (all reporters)",
      what %in% c("HHI_N", "top_share_N") ~ nodes_series,
      TRUE ~ paste("Design,", tolower(supply_chain))
    ),
    supply_chain = factor(supply_chain, levels = stage_levels),
    tech = factor(tech, levels = rev(tech_levels))
  )
dumbbell <- conc_long %>%
  dplyr::filter(series == nodes_series | grepl("^Design", series)) %>%
  dplyr::mutate(end = dplyr::if_else(grepl("^Design", series), "to", "from")) %>%
  dplyr::select(tech, supply_chain, measure, end, value) %>%
  tidyr::pivot_wider(names_from = end, values_from = value) %>%
  dplyr::filter(!is.na(from), !is.na(to))
series_levels <- c("World exports (all reporters)", nodes_series, "Design, upstream", "Design, midstream")
series_fill <- stats::setNames(c("white", paper_ink$muted, paper_stage_cols[["Upstream"]], paper_stage_cols[["Midstream"]]),
                               series_levels)
series_col <- stats::setNames(c(paper_ink$primary, paper_ink$muted, paper_stage_cols[["Upstream"]], paper_stage_cols[["Midstream"]]),
                              series_levels)
target <- stats::median(conc13$HHI_N)
ref_lines <- tibble::tibble(measure = "(a) Herfindahl index", x = target)
target_lab <- sprintf("Calibration target, median of the nodes' export HHI (%.3f)", target)
# world-leader labels sit right of the largest value in panel (b)
leader_x <- max(conc_long$value[conc_long$measure == "(b) Largest single share"]) + 0.04
leader_lab <- conc13 %>%
  dplyr::transmute(tech = factor(tech, levels = rev(tech_levels)), supply_chain = factor(supply_chain, levels = stage_levels),
                   measure = "(b) Largest single share", lab = world_leader, outside = !world_leader_in_network)
no_basket <- conc %>% dplyr::filter(is.na(HHI_N)) %>%
  dplyr::transmute(tech = factor(tech, levels = rev(tech_levels)), supply_chain = factor(supply_chain, levels = stage_levels),
                   measure = "(b) Largest single share", lab = "no basket", outside = FALSE)
leader_head <- tibble::tibble(supply_chain = factor("Upstream", levels = stage_levels),
                              measure = "(b) Largest single share", lab = "World leader")
x_range <- tibble::tibble(measure = c("(a) Herfindahl index", "(b) Largest single share"), x = c(0.3, leader_x + 0.07),
                          tech = factor("Batteries", levels = rev(tech_levels)),
                          supply_chain = factor("Upstream", levels = stage_levels))
p_conc <- ggplot(conc_long, aes(y = tech)) +
  # first layer trains the y scale on every technology of the panel, so the panels keep the
  # alphabetical order (later layers would append the stages without a basket at the top)
  geom_blank(aes(x = value)) +
  geom_vline(data = ref_lines, aes(xintercept = x, linetype = target_lab), colour = paper_ink$axis, linewidth = 0.45) +
  scale_linetype_manual(values = stats::setNames("solid", target_lab), name = NULL) +
  geom_segment(data = dumbbell, aes(x = from, xend = to, y = tech, yend = tech),
               colour = paper_ink$muted, linewidth = 0.35) +
  geom_point(aes(x = value, fill = series, colour = series), shape = 21, size = 2.0, stroke = 0.55) +
  geom_text(data = dplyr::bind_rows(leader_lab, no_basket),
            aes(x = leader_x, y = tech, label = lab, fontface = ifelse(outside, "bold", "plain"),
                alpha = ifelse(lab == "no basket", "nb", "lead")),
            hjust = 0, size = 2.5, colour = paper_ink$secondary, inherit.aes = FALSE) +
  scale_alpha_manual(values = c(nb = 0.6, lead = 1), guide = "none") +
  geom_text(data = leader_head, aes(x = leader_x, y = Inf, label = lab), hjust = 0, vjust = -0.6,
            size = 2.5, colour = paper_ink$secondary, inherit.aes = FALSE) +
  geom_blank(data = x_range, aes(x = x)) +
  scale_fill_manual(values = series_fill, breaks = series_levels, name = NULL) +
  scale_colour_manual(values = series_col, breaks = series_levels, name = NULL) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1), labels = function(x) as.character(round(x, 1)),
                     expand = expansion(add = c(0.01, 0.015))) +
  facet_grid(supply_chain ~ measure, scales = "free", space = "free") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1),
         linetype = guide_legend(order = 2, override.aes = list(linewidth = 0.6))) +
  labs(x = "Concentration of the stage (share-based, 0 to 1)", y = NULL) +
  theme_paper(base_size = 8.5, grid = "x") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 8),
        strip.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(0.8, "lines"),
        legend.box = "vertical", legend.spacing.y = unit(0.1, "lines"),
        legend.margin = margin(0, 0, 0, 0))
save_paper_fig(p_conc, "network_concentration", width = paper_width_full, height = 5.0)

# ---------------------------------------------------------------------------
# Figure: portfolio caps vs top-5 appearances (nodes ordered by GDP, largest on top)
port_plot <- port %>% dplyr::mutate(member = factor(node_lab(iso3c), levels = rev(node_lab(port$iso3c))))
x_max <- max(c(port$count_uncapped, port$portfolio_cap))
p_port <- ggplot(port_plot, aes(y = member)) +
  geom_segment(data = port_plot %>% dplyr::filter(count_uncapped != count_capped),
               aes(x = count_uncapped, xend = count_capped, yend = member),
               colour = paper_ink$muted, linewidth = 0.4) +
  geom_point(aes(x = portfolio_cap, shape = "Cap (GDP-log rule)"), size = 2.6, colour = paper_ink$primary) +
  geom_point(aes(x = count_uncapped, shape = "Without caps"), size = 1.9, colour = paper_ink$secondary, stroke = 0.6) +
  geom_point(aes(x = count_capped, shape = "With caps (production design)"), size = 1.9, colour = paper_cat[["blue"]]) +
  scale_shape_manual(name = NULL, values = c("Cap (GDP-log rule)" = 124, "Without caps" = 1,
                                             "With caps (production design)" = 16),
                     breaks = c("Cap (GDP-log rule)", "Without caps", "With caps (production design)")) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(
    colour = c(paper_ink$primary, paper_ink$secondary, paper_cat[["blue"]]), size = c(2.6, 1.9, 1.9)))) +
  # explicit limits: the first layer holds only the nodes whose count changes
  scale_y_discrete(limits = rev(node_lab(port$iso3c))) +
  scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max + 1, 2), expand = expansion(mult = c(0.02, 0.03))) +
  labs(x = "Stages in which the node is a top-5 producer", y = "Network node (ordered by GDP)") +
  theme_paper(base_size = 8.5, grid = "x") +
  theme(axis.text.y = element_text(size = 7.5), legend.box.margin = margin(t = -4))
save_paper_fig(p_port, "network_portfolio", width = paper_width_half * 1.5, height = 5.4)

# ---------------------------------------------------------------------------
# Unit tests of the allied-network design (Appendix C, tab:alg_tests): synthetic inputs only
unit_tests <- local({
  files <- sort(list.files(file.path(root, "tests", "testthat"), pattern = "^test-allied-network-.*\\.R$", full.names = TRUE))
  stopifnot(length(files) > 0)
  warn_msgs <- character()
  rows <- lapply(files, function(f) {
    res <- testthat::test_file(f, reporter = "silent", stop_on_failure = FALSE)
    for (t in res) for (x in t$results) {
      if (inherits(x, "expectation_warning")) warn_msgs <<- c(warn_msgs, conditionMessage(x))
    }
    df <- as.data.frame(res)
    tibble::tibble(file = basename(f), tests = nrow(df), expectations_passed = sum(df$passed),
                   failed = sum(df$failed), errors = sum(df$error), skipped = sum(df$skipped),
                   warnings = sum(df$warning))
  })
  by_file <- dplyr::bind_rows(rows)
  list(
    description = "testthat::test_file(stop_on_failure = FALSE) on tests/testthat/test-allied-network-*.R",
    design_code_md5 = unname(tools::md5sum(file.path(root, "R", "indices", "allied_network_design.R"))),
    n_files = nrow(by_file), tests = sum(by_file$tests),
    expectations_passed = sum(by_file$expectations_passed), failed = sum(by_file$failed),
    errors = sum(by_file$errors), skipped = sum(by_file$skipped), warnings = sum(by_file$warnings),
    warning_messages = I(unique(warn_msgs)), by_file = by_file
  )
})
results$unit_tests <- unit_tests
write_paper_results(results, "network")
message("Done: network.json, network_map, network_specialization, network_concentration, network_portfolio, ",
        "tables/network_producers.tex")
