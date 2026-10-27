# A synthetic AIB run for the strategic layer (R/aib/aib_strategic.R; test-aib-strategic.R and
# test-aib-dash-network.R): seven allied economies and one non-allied supplier (CHN), seven nodes in two
# strategic compacts plus one unmapped node, five workstreams (one straddling two compacts and placed by an
# override, one outside every compact), the four scenarios (Baseline, Managed Interdependence = "div",
# partial disruption = china_50, Decoupling = china_100) and both capital inputs. Data only: the test files
# source the R code.

strat_fx_allied <- function() c("AUS", "DEU", "IND", "JPN", "KOR", "USA", "VNM")

strat_fx_inputs <- function() {
  nodes <- tibble::tibble(
    node_id = c("ore", "oxide", "magnet", "motor", "cellmat", "cell", "pellets"),
    sub_sector = c("Rare ore", "Separated oxides", "Permanent magnets", "Electric motors", "Cell materials", "Battery cells", "Wood pellets"),
    supply_chain = c("Upstream", "Midstream", "Midstream", "Downstream", "Midstream", "Downstream", "Upstream"))
  world <- c(ore = 100, oxide = 200, magnet = 300, motor = 1000, cellmat = 400, cell = 800, pellets = 50)
  sh <- list(
    ore = c(AUS = 0.5, CHN = 0.3, USA = 0.1, IND = 0.1),
    oxide = c(CHN = 0.7, AUS = 0.15, VNM = 0.1, JPN = 0.05),
    magnet = c(CHN = 0.6, JPN = 0.25, KOR = 0.1, DEU = 0.05),
    motor = c(CHN = 0.3, DEU = 0.25, USA = 0.15, JPN = 0.15, KOR = 0.1, VNM = 0.05),
    cellmat = c(CHN = 0.5, KOR = 0.3, JPN = 0.15, DEU = 0.05),
    cell = c(CHN = 0.6, KOR = 0.2, DEU = 0.1, USA = 0.05, VNM = 0.05),
    pellets = c(USA = 0.5, DEU = 0.5))
  rca <- c(AUS = 5, CHN = 2.5, JPN = 2, KOR = 1.8, DEU = 1.2, USA = 0.8, IND = 1.1, VNM = 1.3)
  em <- dplyr::bind_rows(lapply(names(sh), function(n) tibble::tibble(
    node_id = n, exporter_iso = names(sh[[n]]), world_share = unname(sh[[n]]), exports = unname(sh[[n]]) * world[[n]],
    export_rca = unname(rca[names(sh[[n]])]))))
  # flows: each exporter's exports split over importers by fixed weights (no self-trade); 2020 values are
  # the same except IND and VNM, which doubled since (momentum)
  imp_w <- c(USA = 0.35, DEU = 0.2, JPN = 0.1, KOR = 0.1, IND = 0.1, VNM = 0.05, AUS = 0.05, CHN = 0.05)
  flows <- dplyr::bind_rows(lapply(seq_len(nrow(em)), function(i) {
    w <- imp_w[names(imp_w) != em$exporter_iso[i]]
    tibble::tibble(node_id = em$node_id[i], exporter_iso = em$exporter_iso[i], importer_iso = names(w),
                   value = em$exports[i] * unname(w) / sum(w),
                   value_hist = em$exports[i] * unname(w) / sum(w) * ifelse(em$exporter_iso[i] %in% c("IND", "VNM"), 0.5, 1))
  }))
  list(
    nodes = nodes,
    quality = tibble::tibble(node_id = nodes$node_id, aggregated_into = NA_character_, optimize = TRUE, world_trade = unname(world[nodes$node_id])),
    node_summary = tibble::tibble(node_id = nodes$node_id, world_trade = unname(world[nodes$node_id])),
    exporter_metrics = em,
    production = tibble::tibble(node_id = "ore", iso3 = "AUS", production_share = 0.45),
    flows = flows,
    supply_chain_edges = tibble::tibble(node_a = c("ore", "oxide", "magnet", "cellmat"), node_b = c("oxide", "magnet", "motor", "cell"),
                                        basis = "material link (dictionary)"),
    coalition = tibble::tibble(iso3 = c(strat_fx_allied(), "CHN"), tier = c("core", "core", "partner", "core", "core", "core", "partner", "none"))
  )
}

strat_fx_res <- function() {
  ids <- c("ore", "oxide", "magnet", "motor", "cellmat", "cell", "pellets")
  demand <- c(100, 200, 300, 1000, 400, 800, 50)
  lost <- c(30, 140, 180, 300, 200, 100, 0)
  cls <- c("BUILD", "BUILD", "MANAGE SPECIALIZATION", "COORDINATE", "BUILD", "LOW INTERVENTION", "LOW INTERVENTION")
  classification <- tibble::tibble(
    node_id = ids, sub_sector = c("Rare ore", "Separated oxides", "Permanent magnets", "Electric motors", "Cell materials", "Battery cells", "Wood pellets"),
    demand_usd = demand, optimize = TRUE, node_classification = cls,
    classification_reason = paste("reason for", ids),
    needs_build = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), needs_coordinate = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    needs_manage_specialization = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    baseline_largest_supplier = c("AUS", "CHN", "CHN", "CHN", "CHN", "CHN", "USA"), baseline_largest_share = c(0.5, 0.7, 0.6, 0.3, 0.5, 0.6, 0.5),
    lost_usd = lost, shock_exposure = lost / demand, static_coverage = 1 - lost / demand,
    existing_coverage = c(0.75, 0.35, 0.45, 0.72, 0.55, 0.9, 1), optimized_coverage = c(0.8, 0.5, 0.95, 0.85, 0.6, 0.92, 1),
    residual_gap = c(0.2, 0.5, 0.05, 0.15, 0.4, 0.08, 0),
    optimized_largest_supplier = c("AUS", "AUS", "JPN", "DEU", "KOR", "KOR", "USA"), optimized_largest_share = c(0.6, 0.45, 0.6, 0.35, 0.5, 0.4, 0.5),
    class_china_50 = c("LOW INTERVENTION", "BUILD", "LOW INTERVENTION", "LOW INTERVENTION", "BUILD", "LOW INTERVENTION", "LOW INTERVENTION"))
  ran <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  diversification <- tibble::tibble(
    node_id = ids, scenario = "reallocation",
    status = ifelse(ran, "diversified to the benchmark", "within the cap in the allied market"),
    diversify_class = ifelse(ran, c(NA, "BUILD", "MANAGE SPECIALIZATION", NA, "LOW INTERVENTION", NA, NA), NA_character_),
    lost_usd = ifelse(ran, c(0, 50, 60, 0, 20, 0, 0), NA_real_), shock_exposure = ifelse(ran, c(0, 0.25, 0.2, 0, 0.05, 0, 0), NA_real_),
    static_coverage = ifelse(ran, c(1, 0.75, 0.8, 1, 0.95, 1, 1), NA_real_), existing_coverage = ifelse(ran, c(1, 0.8, 0.85, 1, 0.97, 1, 1), NA_real_),
    optimized_coverage = ifelse(ran, c(1, 0.85, 0.98, 1, 0.99, 1, 1), NA_real_), residual_gap = ifelse(ran, c(0, 0.15, 0.02, 0, 0.01, 0, 0), NA_real_),
    coordination_dividend = ifelse(ran, c(0, 0.05, 0.13, 0, 0.02, 0, 0), NA_real_),
    optimized_largest_share = ifelse(ran, c(0, 0.3, 0.5, 0, 0.3, 0, 0), NA_real_),
    optimized_largest_supplier = ifelse(ran, c(NA, "AUS", "JPN", NA, "KOR", NA, NA), NA_character_))
  stress <- tibble::tibble(
    level = "subsector", shock_id = "china_50", scenario = "reallocation", node_id = ids,
    lost_usd = lost / 2, shock_exposure = lost / demand / 2, static_coverage = 1 - lost / demand / 2,
    existing_coverage = c(0.9, 0.6, 0.8, 0.9, 0.7, 0.95, 1), optimized_coverage = c(0.95, 0.7, 0.97, 0.95, 0.75, 0.97, 1),
    residual_gap = c(0.05, 0.3, 0.03, 0.05, 0.25, 0.03, 0), coordination_dividend = c(0.05, 0.1, 0.17, 0.05, 0.05, 0.02, 0),
    optimized_largest_supplier = c("AUS", "AUS", "JPN", "DEU", "KOR", "KOR", "USA"), optimized_largest_share = c(0.55, 0.4, 0.55, 0.3, 0.45, 0.35, 0.5))
  inv <- function(shock, node, hosts, gap) tibble::tibble(
    node_id = node, shock_id = shock, scenario = "reallocation", supply_constraint = "production_constrained", shock_semantics = "hold",
    increment = 0.1, host_iso = hosts, host_rank = seq_along(hosts), marginal_resilience_gain = 0.1 / seq_along(hosts),
    residual_gap_before = gap, node_world_share = NA_real_, production_share = NA_real_)
  investment <- dplyr::bind_rows(
    inv("china_100", "ore", c("AUS", "IND"), 0.2), inv("china_100", "oxide", c("AUS", "VNM", "JPN"), 0.5),
    inv("china_100", "magnet", c("JPN", "KOR"), 0.05), inv("china_100", "cellmat", c("KOR", "IND"), 0.4),
    inv("china_100", "motor", "DEU", 0.15), inv("div", "oxide", c("VNM", "AUS"), 0.15), inv("div", "magnet", "KOR", 0.02))
  em <- strat_fx_inputs()$exporter_metrics
  investment$node_world_share <- dplyr::coalesce(em$world_share[match(paste(investment$node_id, investment$host_iso), paste(em$node_id, em$exporter_iso))], 0)
  compact_nodes <- tibble::tibble(compact_id = c("AIB-C01", "AIB-C01", "AIB-C02", "AIB-C02", "AIB-C03", "AIB-C04", "AIB-C05"),
                                  node_id = c("ore", "oxide", "magnet", "cellmat", "motor", "cell", "pellets"))
  mem <- function(cid, iso, role, basis, tier, inmin, crit) tibble::tibble(compact_id = cid, iso3 = iso, role = role, basis = basis,
                                                                           coalition_tier = tier, in_minimum_coalition = inmin, critical_supplier = crit)
  vertical <- "supplies one node of the compact and depends on another, or spans its stages"
  several <- "member of several compacts' minimum coalitions"
  members <- dplyr::bind_rows(
    mem("AIB-C01", c("AUS", "USA", "VNM", "CHN"), c("Resource supplier", "Demand anchor", "Bridge partner", NA), c("supplies", "buys", vertical, NA),
        c("core coordinating member", "core coordinating member", "core coordinating member", "critical external dependency"), c(TRUE, TRUE, TRUE, FALSE), c(TRUE, FALSE, FALSE, FALSE)),
    mem("AIB-C02", c("JPN", "KOR", "USA", "DEU"), c("Specialized supplier", "Specialized supplier", "Demand anchor", "Bridge partner"),
        c("supplies", "supplies", "buys", several), c("critical supply partner", "core coordinating member", "core coordinating member", "core coordinating member"),
        TRUE, c(TRUE, FALSE, FALSE, FALSE)),
    mem("AIB-C03", c("DEU", "USA", "JPN", "VNM"), c("Supplier", "Demand anchor", "Supplier", "Bridge partner"), c("supplies", "buys", "supplies", vertical),
        "core coordinating member", TRUE, FALSE),
    mem("AIB-C04", c("KOR", "USA", "DEU"), c("Supplier", "Demand anchor", "Supplier"), c("supplies", "buys", "supplies"),
        c("core coordinating member", "core coordinating member", "associate partner"), c(TRUE, TRUE, FALSE), FALSE),
    mem("AIB-C05", "USA", "Demand anchor", "buys", "core coordinating member", TRUE, FALSE))
  members_managed <- members[members$compact_id %in% c("AIB-C01", "AIB-C02") & members$iso3 %in% c("AUS", "USA", "JPN", "KOR"), , drop = FALSE]
  partnerships <- tibble::tibble(
    node_id = c("ore", "oxide", "magnet", "cellmat", "magnet", "oxide"), exporter_iso = c("AUS", "AUS", "JPN", "KOR", "KOR", "VNM"),
    relationship = c("new", "expanded", "expanded", "new", "domestic retention", "new"), shock_id = c("china_100", "china_100", "china_100", "china_100", "div", "div"),
    scenario = "reallocation", delta_share_of_demand = c(0.1, 0.2, 0.15, 0.1, 0.05, 0.08),
    importer_iso = c("USA", "JPN", "USA", "DEU", "KOR", "JPN"), delta_usd = c(10, 40, 45, 40, 15, 16))
  edge <- function(s, b, t, i) tibble::tibble(supplier_iso = s, buyer_iso = b, scenario = "reallocation", cumulative_trade_resilience_contribution = t,
                                              cumulative_investment_resilience_contribution = i)
  list(
    classification = classification, diversification = diversification, stress = stress, investment = investment,
    compact_nodes = compact_nodes, compact_members = members, compact_members_managed = members_managed, partnerships = partnerships,
    edges = edge(c("AUS", "JPN", "KOR", "VNM", "AUS"), c("USA", "USA", "DEU", "JPN", "JPN"), c(0.5, 0.4, 0.3, 0.1, 0.2), c(0.1, 0, 0.05, 0, 0)),
    edges_managed = edge(c("AUS", "KOR"), c("JPN", "USA"), c(0.3, 0.2), c(0, 0.1)),
    country_names = tibble::tibble(iso3 = c(strat_fx_allied(), "CHN"),
                                   country = c("Australia", "Germany", "India", "Japan", "Korea, Rep.", "United States", "Viet Nam", "China")),
    compacts = tibble::tibble(compact_id = sprintf("AIB-C%02d", 1:5), compact_name = c("Ore / Oxides", "Magnets / Cell materials", "Motors", "Cells", "Pellets"),
                              compact_type = "candidate compact", compact_archetype = "Integrated Compact", class_mix = "BUILD 100%", minimum_coalition = "AUS USA"),
    compacts_managed = tibble::tibble(compact_id = sprintf("AIB-C%02d", 1:5), n_nodes_run = c(1L, 2L, 0L, 0L, 0L), minimum_coalition = "AUS"),
    scenario_shock = "div",
    meta = list(headline_shock = "china_100", headline_scenario = "reallocation", headline_constraint = "production_constrained",
                headline_semantics = "hold", trade_year = 2024, built = "2026-10-20T09:57:00",
                config = list(shocks = list(list(id = "china_100", label = "China supply removed"), list(id = "china_50", label = "China supply halved"),
                                            list(id = "div", label = "Diversify", type = "diversify")),
                              investment = list(headline_increment = 0.1),
                              classification = list(low_exposure = 0.15, adequate_coverage = 0.90, build_gap = 0.10, coordinate_dividend = 0.05,
                                                    manage_largest_share = 0.40, manage_top3_share = 0.75, manage_cap = 0.40, manage_coverage_loss = 0.05)))
  )
}

# config/aib_strategic_compacts.yml (parsed): SC-A (ore -> oxides -> magnets -> motors), SC-B (cell materials ->
# cells) and SC-C (no node in the run); pellets unmapped; AIB-C02 (magnet + cellmat) placed in SC-A by override.
strat_fx_compacts_yml <- function() {
  list(
    taxonomy = "dictionary",
    compacts = list(
      list(strategic_compact_id = "SC-A", strategic_compact_name = "Alpha Chain", family = "Electrotechnology", purpose = "Alpha purpose.",
           value_chain = list(list(value_chain_order = 1, step = "Ore", nodes = list("ore"), extended_nodes = list("ore-extended")),
                              list(value_chain_order = 2, step = "Oxides", nodes = list("oxide")),
                              list(value_chain_order = 3, step = "Magnets", nodes = list("magnet")),
                              list(value_chain_order = 4, step = "Motors", nodes = list("motor")))),
      list(strategic_compact_id = "SC-B", strategic_compact_name = "Beta Chain", family = "Electrotechnology", purpose = "Beta purpose.",
           value_chain = list(list(value_chain_order = 1, step = "Cell materials", nodes = list("cellmat")),
                              list(value_chain_order = 2, step = "Cells", nodes = list("cell")))),
      list(strategic_compact_id = "SC-C", strategic_compact_name = "Gamma Chain", family = "Other", purpose = "Gamma purpose.",
           value_chain = list(list(value_chain_order = 1, step = "Gamma", extended_nodes = list("gamma-extended"))))),
    unmapped = list(list(node_id = "pellets", scope = "core", reason = "no process link")),
    workstream_overrides = list(list(workstream_id = "AIB-C02", nodes = list("cellmat", "magnet"), primary_compact = "SC-A",
                                     also_spans = list("SC-B"), reason = "test override")))
}

# config/aib_node_functions.yml (parsed): every node but `cell`, which falls back on its stage (Downstream).
strat_fx_node_functions <- function() {
  list(fallback_by_stage = list(Upstream = "resource", Midstream = "components", Downstream = "systems"),
       nodes = list(ore = "resource", oxide = "processing", magnet = "components", motor = "systems", cellmat = "components"))
}

# The capital inputs: IMF DIP rows (USD millions; NLD a conduit: dropped as a reporter and as a counterpart)
# and CM-FDI projects.
strat_fx_capital <- function() {
  ind <- "Outward Direct investment, Net (assets less liabilities), All financial instruments, All entities"
  dip <- data.frame(
    COUNTRY = c("United States", "United States", "Japan", "Korea, Rep.", "Germany", "Australia", "India", "Netherlands", "Japan"),
    COUNTERPART_COUNTRY = c("Japan", "Netherlands", "United States", "United States", "United States", "Japan", "United States", "United States", "Germany"),
    DV_TYPE = "Reported official data", INDICATOR = ind,
    `2023` = c(3000000, 900000, 1200000, 500000, 1400000, 400000, NA, 2000000, 300000),
    `2022` = c(NA, NA, NA, NA, NA, NA, 60000, NA, NA), check.names = FALSE, stringsAsFactors = FALSE)
  iso <- c("United States" = "USA", "Japan" = "JPN", "Korea, Rep." = "KOR", "Germany" = "DEU", "Australia" = "AUS", "India" = "IND", "Netherlands" = "NLD")
  cmfdi <- data.frame(
    sector = c("Battery", "Battery", "EV", "Solar", "Battery", "Wind"), home_iso3 = c("KOR", "JPN", "JPN", "USA", "DEU", "DEU"),
    destination_iso3 = c("USA", "USA", "IND", "IND", "USA", "DEU"), capital_usd_m = c(60000, 40000, 5000, 18000, 30000, 999),
    attribution_weight = 1, index_use = TRUE, stringsAsFactors = FALSE)
  list(dip = dip, cmfdi = cmfdi, iso_of = function(x) unname(iso[x]), conduits = c("NLD", "LUX", "IRL"))
}
