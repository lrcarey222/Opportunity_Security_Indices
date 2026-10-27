repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "aib", "aib_compacts.R"))
source(file.path(repo_root, "R", "aib", "aib_strategic.R"))
suppressPackageStartupMessages(library(dplyr))

# The strategic layer (R/aib/aib_strategic.R, scripts/33; docs/aib_methodology.md section 16) on the synthetic
# run of helper-aib-strategic.R, the tracked configs, and (when present) the real outputs of scripts/33.

strat_layer_cfg <- function() {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_layer.yml"))
  cfg$capital$cmfdi_sectors <- list(Battery = "SC-B", EV = "SC-B")
  cfg
}
strat_run <- function(inputs = strat_fx_inputs(), res = strat_fx_res(), yml = strat_fx_compacts_yml(), nf = strat_fx_node_functions(),
                      cfg = strat_layer_cfg(), capital = TRUE) {
  cap <- strat_fx_capital()
  aib_strategic_layer(inputs, res, yml, nf, cfg, capital = if (capital) list(dip = cap$dip, cmfdi = cap$cmfdi) else list(),
                      conduits = cap$conduits, iso_of = cap$iso_of)
}
strat_st <- strat_run()
strat_real_dir <- file.path(repo_root, "data", "processed", "outputs", "aib")

# ---- Compact hierarchy -------------------------------------------------------------------------------------

test_that("the tracked configs keep their contract: one compact and step per node, rising steps, a function for every node", {
  y <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_compacts.yml"))
  nf <- yaml::read_yaml(file.path(repo_root, "config", "aib_node_functions.yml"))
  m <- aib_strategic_map(y)
  core <- m$node_id[m$scope == "core"]
  unm <- vapply(y$unmapped, function(u) u$node_id, "")
  v <- aib_strategic_validate_map(y, c(core, intersect(unm, c("biofuels-and-wood-pellets"))))
  expect_equal(v$problems, character())
  expect_false(anyDuplicated(m$node_id) > 0)
  expect_gte(length(unique(m$strategic_compact_id)), 6)
  expect_lte(length(unique(m$strategic_compact_id)), 10)
  for (k in y$compacts) {
    expect_true(nzchar(k$strategic_compact_id) && nzchar(k$strategic_compact_name) && nzchar(k$purpose))
    expect_true(all(diff(vapply(k$value_chain, function(s) as.numeric(s$value_chain_order), 0)) > 0))
  }
  fn <- aib_strategic_node_functions(nf, v$map)
  expect_equal(fn$problems, character())
  expect_equal(fn$notes, character())      # no mapped node falls back on its stage
  expect_true(all(fn$functions$node_function %in% aib_strategic_functions()))
  # every override names known compacts and a node set of mapped nodes
  for (o in y$workstream_overrides) {
    expect_true(o$primary_compact %in% m$strategic_compact_id)
    expect_true(all(unlist(o$nodes) %in% m$node_id))
  }
})

test_that("a node without a function falls back on its six-stage stage", {
  nf <- yaml::read_yaml(file.path(repo_root, "config", "aib_node_functions.yml"))
  expect_setequal(names(nf$fallback_by_stage), SUPPLY_CHAIN_STAGES)
  ids <- paste0("n", seq_along(SUPPLY_CHAIN_STAGES))
  fn <- aib_strategic_node_functions(list(fallback_by_stage = nf$fallback_by_stage), tibble::tibble(node_id = ids),
                                     stats::setNames(SUPPLY_CHAIN_STAGES, ids))
  expect_equal(fn$problems, character())
  expect_equal(stats::setNames(fn$functions$node_function, SUPPLY_CHAIN_STAGES),
               c(Extraction = "resource", Refining = "processing", Components = "components", Assembly = "systems",
                 Deployment = "systems", Recovery = "resource"))
  expect_equal(fn$functions$function_basis[1], "fallback: stage Extraction")
  # The built-in default is the same; an old three-stage label has nothing to fall back on.
  expect_equal(aib_strategic_node_functions(list(), tibble::tibble(node_id = ids), stats::setNames(SUPPLY_CHAIN_STAGES, ids))$functions,
               fn$functions)
  old <- aib_strategic_node_functions(list(), tibble::tibble(node_id = "x"), c(x = "Upstream"))
  expect_match(old$problems, "no function and no stage to fall back on")
  expect_equal(strat_st$map$node_function[strat_st$map$node_id == "cell"], "systems")   # the fixture's Assembly node
})

test_that("the map's validation names a node mapped twice, falling steps, an unlisted run node and unknown override compacts", {
  y <- strat_fx_compacts_yml()
  y$compacts[[2]]$value_chain[[2]]$nodes <- list("cell", "magnet")
  y$compacts[[1]]$value_chain[[3]]$value_chain_order <- 1
  y$workstream_overrides[[1]]$primary_compact <- "SC-Z"
  v <- aib_strategic_validate_map(y, c(strat_fx_inputs()$nodes$node_id, "stray"))
  expect_true(any(grepl("node mapped more than once: magnet", v$problems)))
  expect_true(any(grepl("SC-A: value_chain_order is not strictly increasing", v$problems)))
  expect_true(any(grepl("run node stray is neither mapped", v$problems)))
  expect_true(any(grepl("override AIB-C02 names unknown compacts: SC-Z", v$problems)))
  # the layer stops on them (scripts/33 stops the run)
  expect_true(length(strat_run(yml = y)$problems) > 0)
})

test_that("an aggregated node the map does not list takes the compact and step of its heaviest member", {
  y <- strat_fx_compacts_yml()
  agg <- tibble::tibble(node_id = c("oxide", "magnet"), aggregated_into = "re--aggregated")
  v <- aib_strategic_validate_map(y, c("ore", "re--aggregated", "motor", "cellmat", "cell", "pellets"), agg, c(oxide = 200, magnet = 300))
  expect_equal(v$problems, character())
  r <- v$map[v$map$node_id == "re--aggregated", ]
  expect_equal(c(r$strategic_compact_id, r$value_chain_step), c("SC-A", "Magnets"))
  expect_match(r$resolution, "member magnet")
})

test_that("every workstream maps to at most one strategic compact: by demand, or by an override matched on its node set", {
  ws <- strat_st$workstreams
  expect_equal(sort(ws$workstream_id), sprintf("AIB-C%02d", 1:5))
  expect_false(anyDuplicated(ws$workstream_id) > 0)
  expect_true(all(is.na(ws$strategic_compact_id) | ws$strategic_compact_id %in% strat_st$compact_defs$strategic_compact_id))
  # AIB-C02 (magnet 300 + cellmat 400) is SC-B by demand (57%), SC-A by the override, which records the split
  c02 <- ws[ws$workstream_id == "AIB-C02", ]
  expect_equal(c02$strategic_compact_id, "SC-A")
  expect_match(c02$assignment_basis, "^override")
  expect_equal(c02$demand_split, "SC-B 57%; SC-A 43%")
  expect_equal(c02$also_spans, "SC-B")
  expect_true(c02$straddles)
  # a workstream with no mapped node belongs to no compact
  expect_true(is.na(ws$strategic_compact_id[ws$workstream_id == "AIB-C05"]))
  # an override whose node set no workstream has is stale: the demand rule applies and the layer says so
  y <- strat_fx_compacts_yml()
  y$workstream_overrides[[1]]$nodes <- list("magnet", "motor")
  st2 <- strat_run(yml = y)
  expect_equal(st2$workstreams$strategic_compact_id[st2$workstreams$workstream_id == "AIB-C02"], "SC-B")
  expect_equal(nrow(st2$stale_overrides), 1)
  expect_true(any(grepl("stale workstream override AIB-C02", st2$notes)))
})

test_that("every compact holds valid mapped nodes in value-chain order, and the order does not depend on input row order", {
  sp <- strat_st$spine[strat_st$spine$scenario == "decoupling", ]
  expect_true(all(sp$node_id %in% strat_fx_inputs()$nodes$node_id))
  expect_false("pellets" %in% sp$node_id)                       # unmapped
  expect_false("SC-C" %in% strat_st$compacts$strategic_compact_id)  # no node in this run
  expect_equal(sp$node_id, c("ore", "oxide", "magnet", "motor", "cellmat", "cell"))
  for (k in unique(sp$strategic_compact_id)) expect_false(is.unsorted(sp$value_chain_order[sp$strategic_compact_id == k]))
  expect_equal(sp$node_function, c("resource", "processing", "components", "systems", "components", "systems"))
  # shuffled inputs give the same layer
  inp <- strat_fx_inputs()
  res <- strat_fx_res()
  set.seed(7)
  inp$flows <- inp$flows[sample(nrow(inp$flows)), ]
  inp$exporter_metrics <- inp$exporter_metrics[sample(nrow(inp$exporter_metrics)), ]
  res$compact_nodes <- res$compact_nodes[sample(nrow(res$compact_nodes)), ]
  res$investment <- res$investment[sample(nrow(res$investment)), ]
  res$compact_members <- res$compact_members[sample(nrow(res$compact_members)), ]
  st2 <- strat_run(inputs = inp, res = res)
  expect_identical(st2$spine, strat_st$spine)
  expect_identical(st2$workstreams, strat_st$workstreams)
  expect_identical(st2$compacts, strat_st$compacts)
})

# ---- Scenario persistence and the policy portfolio -----------------------------------------------------------

test_that("strategic compact ids, nodes and steps are the same in every scenario", {
  cp <- strat_st$compacts
  expect_equal(sort(unique(cp$scenario)), sort(c("baseline", "managed", "partial", "decoupling")))
  by <- split(cp, cp$scenario)
  ref <- by$decoupling
  for (s in names(by)) {
    expect_equal(by[[s]]$strategic_compact_id, ref$strategic_compact_id)
    expect_equal(by[[s]]$included_nodes, ref$included_nodes)
    expect_equal(by[[s]]$value_chain, ref$value_chain)
    expect_equal(by[[s]]$workstreams, ref$workstreams)
  }
  # Baseline: no shock, nothing invented
  b <- by$baseline
  expect_true(all(b$no_shock_node_count == b$n_nodes))
  expect_true(all(b$coverage_existing == 1 & b$coverage_post_investment == 1))
  expect_true(all(is.na(b$primary_mission)))
  # partial disruption: classes of class_china_50, no investment simulation
  expect_true(all(is.na(by$partial$coverage_post_investment)))
  expect_equal(strat_st$scenarios$shock_id, c(NA, "div", "china_50", "china_100"))
})

test_that("node classes aggregate to the compact's counts in every scenario, and coverage is demand-weighted", {
  sp <- strat_st$spine
  cp <- strat_st$compacts
  col <- c(BUILD = "build_node_count", COORDINATE = "coordinate_node_count", `MANAGE SPECIALIZATION` = "manage_node_count",
           `LOW INTERVENTION` = "low_node_count", `INSUFFICIENT DATA` = "insufficient_node_count", `NOT STRESSED` = "not_stressed_node_count",
           `NO SHOCK` = "no_shock_node_count")
  for (i in seq_len(nrow(cp))) {
    x <- sp[sp$strategic_compact_id == cp$strategic_compact_id[i] & sp$scenario == cp$scenario[i], ]
    for (k in names(col)) expect_equal(cp[[col[[k]]]][i], sum(x$node_status == k), info = paste(cp$strategic_compact_id[i], cp$scenario[i], k))
    expect_equal(sum(unlist(cp[i, unname(col)])), cp$n_nodes[i])
    expect_equal(cp$needs_build_nodes[i], sum(x$needs_build))
    ok <- !is.na(x$coverage_existing) & x$demand_usd > 0
    if (any(ok)) expect_equal(cp$coverage_existing[i], sum(x$coverage_existing[ok] * x$demand_usd[ok]) / sum(x$demand_usd[ok]))
  }
  # Decoupling, SC-A: classes as classified; the post-investment coverage adds the rank-1 host's gain (capped at 1)
  d <- sp[sp$scenario == "decoupling" & sp$strategic_compact_id == "SC-A", ]
  expect_equal(d$node_status, c("BUILD", "BUILD", "MANAGE SPECIALIZATION", "COORDINATE"))
  expect_equal(d$coverage_post_investment, pmin(1, d$coverage_coordinated + c(0.1, 0.1, 0.1, 0.1)))
  # Managed: the scenario's own classes where it ran, NOT STRESSED (coverage 1) elsewhere
  m <- sp[sp$scenario == "managed", ]
  expect_equal(m$node_status, c("NOT STRESSED", "BUILD", "MANAGE SPECIALIZATION", "NOT STRESSED", "LOW INTERVENTION", "NOT STRESSED"))
  expect_true(all(m$coverage_existing[m$node_status == "NOT STRESSED"] == 1))
})

test_that("missions follow the classes' shares of lost supply, multi-label needs fill free slots, and the menu follows the classes", {
  cp <- strat_st$compacts
  a <- cp[cp$strategic_compact_id == "SC-A" & cp$scenario == "decoupling", ]
  # lost supply: COORDINATE 300 (motor), MANAGE SPECIALIZATION 180 (magnet), BUILD 170 (ore 30 + oxide 140)
  expect_equal(c(a$primary_mission_class, a$secondary_mission_class, a$supporting_mission_class), c("COORDINATE", "MANAGE SPECIALIZATION", "BUILD"))
  expect_equal(a$primary_mission, "coordinate trade to redirect existing supply in motors")
  expect_equal(a$supporting_mission, "build missing capacity in oxides and ore")
  expect_equal(a$primary_mission_basis, "COORDINATE: 46% of lost supply, 1 node")
  expect_equal(a$policy_mix, "COORDINATE + MANAGE SPECIALIZATION + BUILD")
  cfg <- strat_layer_cfg()
  for (k in c("COORDINATE", "MANAGE SPECIALIZATION", "BUILD")) expect_true(grepl(paste(unlist(cfg$policy_menu[[k]]), collapse = ", "), a$policy_menu, fixed = TRUE))
  # Partial, SC-B: one BUILD node; its MANAGE SPECIALIZATION need beside the class takes the second slot
  b <- cp[cp$strategic_compact_id == "SC-B" & cp$scenario == "partial", ]
  expect_equal(c(b$primary_mission_class, b$secondary_mission_class), c("BUILD", "MANAGE SPECIALIZATION"))
  expect_match(b$secondary_mission_basis, "need beside the headline class")
  # no composite score anywhere
  expect_false(any(grepl("score", names(cp))))
})

# ---- Strategic country roles -------------------------------------------------------------------------------------

test_that("role assignments are deterministic and respect the caps and both gates", {
  r1 <- strat_st$roles
  expect_identical(strat_run()$roles, r1)
  P <- strat_st$params
  asg <- r1[r1$primary_or_secondary %in% c("primary", "secondary"), ]
  expect_true(all(table(asg$iso3[asg$primary_or_secondary == "primary"]) <= P$max_primary))
  expect_true(all(table(asg$iso3[asg$primary_or_secondary == "secondary"]) <= P$max_secondary))
  expect_true(all(asg$gate1_value > 0 & asg$gate2_value > 0))
  expect_true(all(asg$gate1_pct >= P$p_secondary & asg$gate2_pct >= P$p_secondary))
  pr <- asg[asg$primary_or_secondary == "primary", ]
  expect_true(all(pr$gate1_pct >= P$p_primary & pr$gate2_pct >= P$p_primary))
  # non-allied economies: the three supply roles only
  expect_true(all(r1$role_code[!r1$allied] %in% aib_strategic_supply_roles()))
  # CIA: NA (not assessed) for an economy without a DIP position, never 0
  cia <- r1[r1$role_code == "CIA", ]
  expect_true(is.na(cia$gate2_value[cia$iso3 == "VNM"]))
  expect_true(is.na(cia$primary_or_secondary[cia$iso3 == "VNM"]) || cia$primary_or_secondary[cia$iso3 == "VNM"] == "not assigned")
  # the results on the synthetic run
  expect_equal(asg$iso3[asg$primary_or_secondary == "primary"], "USA")
  expect_equal(asg$role_code[asg$iso3 == "USA"], "DMA")
})

test_that("capital evidence drops conduit reporters and counterparts, and reads the latest configured year", {
  cap <- strat_fx_capital()
  d <- aib_capital_dip_outward(cap$dip, cap$conduits, iso_of = cap$iso_of)
  expect_false("NLD" %in% d$iso3)
  expect_equal(d$fdi_out_usd_bn[d$iso3 == "USA"], 3000)          # the NLD counterpart is dropped
  expect_equal(d$fdi_out_usd_bn[d$iso3 == "JPN"], 1500)
  expect_equal(d$fdi_year[d$iso3 == "IND"], 2022L)
  p <- aib_capital_cmfdi_projects(cap$cmfdi)
  expect_false(any(p$home_iso3 == p$destination_iso3))
  o <- aib_capital_cmfdi_outward(p)
  expect_equal(o$cmfdi_out_usd_bn[o$iso3 == "KOR"], 60)
  # without the capital inputs the capital role is not assessed, and the run says so
  st0 <- strat_run(capital = FALSE)
  expect_true(all(is.na(st0$roles$gate1_value[st0$roles$role_code == "CIA"])))
  expect_true(any(grepl("not assessed", st0$notes)))
})

test_that("no economy receives every strategic role: a dominant economy that is not scaling up fails Development & Scale-Up", {
  P <- aib_strategic_params()
  n <- 20
  cty <- tibble::tibble(iso3 = sprintf("C%02d", seq_len(n)), allied = TRUE)
  for (g in unique(unlist(aib_strategic_gates()))) cty[[g]] <- seq_len(n)
  # C20 tops every gate except the scale-up rate, where incumbents present everywhere rank low
  cty$scaleup_rate <- rev(seq_len(n)) / n
  for (g in unlist(aib_strategic_support())) cty[[g]] <- 0
  r <- aib_assign_strategic_roles(cty, P)
  top <- r[r$iso3 == "C20", ]
  expect_false(top$primary_ok[top$role_code == "DSP"])
  expect_equal(sum(top$primary_ok | top$secondary_ok), 6)
  expect_equal(sum(top$primary_or_secondary == "primary", na.rm = TRUE), 3)
  expect_equal(sum(top$primary_or_secondary == "secondary", na.rm = TRUE), 2)
  expect_equal(sum(top$primary_or_secondary == "qualifies beyond the cap", na.rm = TRUE), 1)
  # order: strength in whole points, then the other gate, then the fixed role order
  expect_equal(top$role_code[top$primary_or_secondary %in% "primary"], c("RPA", "STM", "SMI"))
  # a zero on either gate fails the role, whatever the percentile
  cty$rp_positions[cty$iso3 == "C20"] <- 0
  r2 <- aib_assign_strategic_roles(cty, P)
  expect_false(r2$secondary_ok[r2$iso3 == "C20" & r2$role_code == "RPA"])
  # deterministic: shuffled rows, same table
  set.seed(3)
  r3 <- aib_assign_strategic_roles(cty[sample(n), ], P)
  expect_identical(r3, r2)
})

# ---- Compact-specific country roles --------------------------------------------------------------------------------

test_that("compact roles are supported by the node evidence of their own compact", {
  cr <- strat_st$compact_roles
  ev <- strat_st$evidence
  nodes <- strat_st$map
  P <- strat_st$params
  expect_false(anyDuplicated(paste(cr$strategic_compact_id, cr$iso3)) > 0)
  for (i in seq_len(nrow(cr))) {
    x <- cr[i, ]
    lead <- x$lead_function
    expect_true(isTRUE(x[[lead]]), info = paste(x$iso3, x$strategic_compact_id, lead))
    if (!is.na(x$lead_node)) expect_equal(nodes$strategic_compact_id[nodes$node_id == x$lead_node], x$strategic_compact_id)
    e <- ev[ev$iso3 == x$iso3 & ev$node_id == x$lead_node, ]
    if (lead == "f_resource") expect_true(e$anchor_position && e$step_function == "resource")
    if (lead == "f_processing") expect_true(e$anchor_position && e$step_function == "processing")
    if (lead == "f_specialized") expect_true(e$specialist_position)
    if (lead %in% c("f_resource", "f_processing", "f_specialized")) expect_gte(x$lead_share, P$position_min)
    if (lead == "f_demand") {
      expect_true(x$allied)
      expect_equal(x$lead_share, e$world_import_share)
    }
  }
  # the variable geometry: the same country, different roles in different compacts
  jpn <- cr[cr$iso3 == "JPN", ]
  expect_equal(jpn$compact_role[jpn$strategic_compact_id == "SC-A"], "Specialized technology supplier")
  expect_equal(cr$compact_role[cr$iso3 == "AUS" & cr$strategic_compact_id == "SC-A"], "Resource supplier")
  expect_equal(cr$compact_role[cr$iso3 == "CHN" & cr$strategic_compact_id == "SC-A"], "Processing anchor (non-allied supplier)")
  # the investment function needs CM-FDI capital in the compact's sectors (SC-B here)
  kor <- cr[cr$iso3 == "KOR" & cr$strategic_compact_id == "SC-B", ]
  expect_true(kor$f_investment)
  expect_true(all(is.na(cr$investment_contribution[cr$strategic_compact_id == "SC-A"])))
})

test_that("input demand does not lead a compact role; demand at or after the country's supply step does", {
  # Two steps: n1 (processing) -> n2 (components). AAA makes n2 (12% of world supply) and buys 40% of the
  # world's n1, its own input; BBB refines n1 (15%) and buys 30% of the world's n2.
  nodes <- tibble::tibble(node_id = c("n1", "n2"), sub_sector = c("Refined input", "Component"), sc_id = "SC-X",
                          vc_order = c(1L, 2L), vc_step = c("Refining", "Components"), step_function = c("processing", "components"),
                          world_trade = c(100, 100), optimize = TRUE)
  cn <- tibble::tibble(
    iso3 = c("AAA", "AAA", "BBB", "BBB"), node_id = c("n1", "n2", "n1", "n2"), sc_id = "SC-X",
    vc_order = c(1L, 2L, 1L, 2L), vc_step = c("Refining", "Components", "Refining", "Components"),
    step_function = c("processing", "components", "processing", "components"),
    world_share = c(0, 0.12, 0.15, 0), position_share = c(0, 0.12, 0.15, 0), exports = c(0, 12, 15, 0),
    imports = c(40, 0, 10, 30), world_import_share = c(0.40, 0, 0.10, 0.30),
    anchor_position = c(FALSE, FALSE, TRUE, FALSE), specialist_position = FALSE,
    best_host_rank = NA_real_, host_scaled = FALSE, capable_host = FALSE, momentum = FALSE,
    world_trade = 100, optimize = TRUE) |>
    dplyr::mutate(supply_usd_eq = position_share * world_trade)
  pt <- tibble::tibble(relationship = character(), shock_id = character(), node_id = character(), exporter_iso = character(),
                       delta_share_of_demand = numeric())
  cr <- aib_compact_country_roles(cn, nodes, tibble::tibble(), NULL, pt, c("AAA", "BBB"), strat_st$params)$compact_roles
  aaa <- cr[cr$iso3 == "AAA", ]
  bbb <- cr[cr$iso3 == "BBB", ]
  # AAA's larger demand position (40% of n1) sits before its supply step: a function, not its lead role
  expect_true(aaa$f_demand)
  expect_equal(aaa$compact_role, "Scale manufacturer")
  expect_equal(aaa$lead_node, "n2")
  # BBB's demand is downstream of what it supplies: it competes, and 30% beats its 15% processing position
  expect_true(bbb$f_processing)
  expect_equal(bbb$compact_role, "Demand anchor")
  expect_equal(bbb$lead_node, "n2")
  expect_equal(bbb$lead_share, 0.30)
})

test_that("scale is a node-level presence: >= 5% of a node of the only manufacturing step makes a scale manufacturer, < 5% does not", {
  # One manufacturing step (two vehicle nodes) after an ore step. AAA holds 7% of p1 (3.5% of the step: the
  # old step rule failed it); BBB 3%. DDD makes 12% of p1 and buys 14% of the world's p2 (a supply function
  # wins a tie within lead_margin). EEE buys 4% of the world's p2 (below the 5% demand-lead floor) and is a
  # capable host with momentum, so the host function leads.
  nodes <- tibble::tibble(node_id = c("r1", "p1", "p2"), sub_sector = c("Ore", "Cars", "Buses"), sc_id = "SC-Y",
                          vc_order = c(1L, 2L, 2L), vc_step = c("Ore", "Vehicles", "Vehicles"), step_function = c("resource", "systems", "systems"),
                          world_trade = c(100, 1000, 1000), optimize = TRUE)
  mk <- function(iso, share, imp2 = 0, host = FALSE) tibble::tibble(
    iso3 = iso, node_id = nodes$node_id, sc_id = "SC-Y", vc_order = nodes$vc_order, vc_step = nodes$vc_step, step_function = nodes$step_function,
    world_share = c(0, share, 0), position_share = c(0, share, 0), exports = c(0, share * 1000, 0), imports = c(0, 0, imp2),
    world_import_share = c(0, 0, imp2 / 1000), anchor_position = FALSE, specialist_position = FALSE, best_host_rank = NA_real_,
    host_scaled = FALSE, capable_host = c(FALSE, host, FALSE), momentum = c(FALSE, host, FALSE), world_trade = nodes$world_trade, optimize = TRUE)
  cn <- dplyr::bind_rows(mk("AAA", 0.07), mk("BBB", 0.03), mk("DDD", 0.12, imp2 = 140), mk("EEE", 0, imp2 = 40, host = TRUE)) |>
    dplyr::mutate(supply_usd_eq = position_share * world_trade)
  pt <- tibble::tibble(relationship = character(), shock_id = character(), node_id = character(), exporter_iso = character(),
                       delta_share_of_demand = numeric())
  cr <- aib_compact_country_roles(cn, nodes, tibble::tibble(), NULL, pt, c("AAA", "BBB", "DDD", "EEE"), strat_st$params)$compact_roles
  aaa <- cr[cr$iso3 == "AAA", ]
  expect_true(aaa$f_scale)
  expect_false(aaa$f_scale_step)
  expect_equal(aaa$compact_role, "Scale manufacturer")
  expect_equal(aaa$lead_node, "p1")
  expect_false("BBB" %in% cr$iso3)                                  # 3%: no function at all
  ddd <- cr[cr$iso3 == "DDD", ]
  expect_true(ddd$f_demand)
  expect_equal(ddd$compact_role, "Scale manufacturer")             # 12% supply beats 14% demand within the margin
  eee <- cr[cr$iso3 == "EEE", ]
  expect_true(eee$f_demand && eee$f_host)
  expect_equal(eee$compact_role, "Capacity-building host")         # 4% of world imports cannot lead
})

test_that("a new-capacity host is named only when no other host ties with it, and hosts are credited only on nodes that need capacity", {
  res <- strat_fx_res()
  iv <- res$investment
  # oxide under Decoupling: the three ranked hosts tie at the full increment (gain, and no HHI difference)
  iv$marginal_resilience_gain[iv$shock_id == "china_100" & iv$node_id == "oxide"] <- 0.1
  iv$delta_hhi <- -0.01
  res$investment <- iv
  st <- strat_run(res = res)
  sp <- st$spine[st$spine$scenario == "decoupling", ]
  ox <- sp[sp$node_id == "oxide", ]
  expect_true(is.na(ox$top_host))
  expect_equal(ox$host_tie_n, 3L)
  expect_equal(ox$host_tied, "AUS VNM JPN")
  expect_equal(ox$model_rank1, "AUS")
  ore <- sp[sp$node_id == "ore", ]                                   # AUS 0.10 against IND 0.05: a finding, named
  expect_equal(ore$top_host, "AUS")
  expect_equal(ore$host_tie_n, 1L)
  a <- st$compacts[st$compacts$strategic_compact_id == "SC-A" & st$compacts$scenario == "decoupling", ]
  expect_equal(a$build_hosts, "AUS")
  expect_equal(a$build_host_ties, 1L)
  # post-investment coverage: coordinated + min(increment, gap) whoever hosts (by construction), not in the headline
  expect_equal(ox$coverage_post_investment, min(1, ox$coverage_coordinated + 0.1))
  expect_false(grepl("after simulated new capacity", a$headline_result))
  expect_match(a$coverage_basis, "by construction")
  # credit: the best gain x the node's demand, split among the tied capable hosts; the larger scenario counts
  ev <- st$evidence
  g <- function(iso, node) ev$host_gap_usd[ev$iso3 == iso & ev$node_id == node]
  expect_equal(g("AUS", "oxide"), 0.1 * 200 / 3)
  expect_equal(g("VNM", "oxide"), 0.1 * 200)                         # the single best host under Managed Interdependence
  # magnet needs no new capacity in either scenario (MANAGE SPECIALIZATION, no BUILD need): no host is credited
  expect_true(all(is.na(ev$best_host_rank[ev$node_id == "magnet"])))
})

test_that("under Managed Interdependence a node left alone only because its benchmark is concentrated too is NOT TESTED, with no coverage credit", {
  res <- strat_fx_res()
  dv <- res$diversification
  dv$current_hhi <- c(0.30, NA, NA, 0.05, NA, 0.25, NA)
  dv$benchmark_hhi <- c(0.40, NA, NA, 0.40, NA, 0.10, NA)
  dv$status[dv$node_id %in% c("ore", "motor", "cell")] <- "already as diversified as its benchmark"
  res$diversification <- dv
  st <- strat_run(res = res)
  m <- st$spine[st$spine$scenario == "managed", ]
  expect_equal(m$node_status[m$node_id == "ore"], "NOT TESTED")       # concentrated node, concentrated benchmark
  expect_equal(m$node_status[m$node_id == "motor"], "NOT STRESSED")   # the node is not concentrated
  expect_equal(m$node_status[m$node_id == "cell"], "NOT STRESSED")    # the benchmark is not concentrated
  ore <- m[m$node_id == "ore", ]
  expect_true(is.na(ore$coverage_existing) && is.na(ore$coverage_post_investment) && is.na(ore$lost_usd))
  expect_match(ore$status_detail, "see its Decoupling class \\(BUILD\\)")
  a <- st$compacts[st$compacts$strategic_compact_id == "SC-A" & st$compacts$scenario == "managed", ]
  expect_equal(a$not_tested_node_count, 1L)
  x <- m[m$node_id %in% c("oxide", "magnet", "motor"), ]
  expect_equal(a$coverage_existing, sum(x$coverage_existing * x$demand_usd) / sum(x$demand_usd))
  expect_match(a$headline_result, "1 concentrated node is not tested under this scenario")
  # the Managed Interdependence verb: supply has to shift (no supplier is removed)
  expect_match(a$headline_result, "has to shift away from suppliers above the cap")
  expect_match(a$primary_mission_basis, "of supply to shift")
  d <- st$compacts[st$compacts$strategic_compact_id == "SC-A" & st$compacts$scenario == "decoupling", ]
  expect_match(d$headline_result, "is lost")
})

test_that("a mission leads only when material, and a step whose other nodes have another class is named through the mission's nodes", {
  cfg <- strat_layer_cfg()
  nd <- tibble::tibble(node_id = c("a", "b", "c"), node_label = c("Heat pumps", "Solar water heaters", "Compressors"),
                       value_chain_order = c(2L, 2L, 1L), value_chain_step = c("Heating units", "Heating units", "Compressors"),
                       node_status = c("LOW INTERVENTION", "BUILD", "LOW INTERVENTION"), lost_usd = c(4e9, 7e7, 1e8),
                       needs_build = c(FALSE, TRUE, FALSE), needs_coordinate = FALSE, needs_manage = FALSE)
  ms <- aib_strategic_missions(nd, cfg$missions)
  expect_equal(ms$primary$cls, "BUILD")
  expect_false(ms$primary$material)                                   # 2% of lost supply, US$0.07bn
  expect_equal(ms$primary$text, "build missing capacity in solar water heaters (heating units)")
  expect_match(ms$primary$basis, "a targeted action")
  nd2 <- nd
  nd2$lost_usd[2] <- 1.2e9                                            # >= US$1bn: material
  expect_true(aib_strategic_missions(nd2, cfg$missions)$primary$material)
  nd3 <- nd
  nd3$node_status[1] <- "BUILD"                                       # the whole step is BUILD: named as the step
  expect_equal(aib_strategic_missions(nd3, cfg$missions)$primary$text, "build missing capacity in heating units")
  # the compact statement: the monitor template leads, the small node is a targeted action
  status <- tibble::tibble(node_id = nd$node_id, scenario = "decoupling", demand_usd = c(1e10, 1e9, 2e9), node_status = nd$node_status,
                           lost_usd = nd$lost_usd, coverage_static = 0.9, coverage_existing = 0.9, coverage_coordinated = 0.95,
                           coverage_post_investment = 0.97, needs_build = nd$needs_build, needs_coordinate = FALSE, needs_manage = FALSE,
                           top_host = NA_character_, host_tie_n = c(NA, 4L, NA))
  map <- tibble::tibble(node_id = nd$node_id, strategic_compact_id = "SC-H", value_chain_order = nd$value_chain_order, value_chain_step = nd$value_chain_step)
  defs <- tibble::tibble(strategic_compact_id = "SC-H", strategic_compact_name = "Heat", family = NA_character_, purpose = NA_character_)
  scen <- tibble::tibble(scenario = "decoupling", label = "Decoupling", shock_id = "x")
  ws <- tibble::tibble(workstream_id = character(), strategic_compact_id = character(), also_spans = character(), nodes = character())
  pf <- aib_strategic_portfolio(status, map, defs, scen, ws, list(), cfg$missions, cfg$policy_menu, node_labels = stats::setNames(nd$node_label, nd$node_id))
  expect_equal(pf$compacts$mission_lead, "monitor (targeted actions only)")
  expect_match(pf$compacts$mission_statement, "^Monitor: no intervention class reaches the materiality floor .*; targeted: build missing capacity in solar water heaters \\(heating units\\)\\.$")
  expect_equal(pf$compacts$build_host_ties, 1L)
  # an untested node changes the no-intervention sentence
  status$node_status <- c("LOW INTERVENTION", "INSUFFICIENT DATA", "LOW INTERVENTION")
  status$needs_build <- FALSE
  pf2 <- aib_strategic_portfolio(status, map, defs, scen, ws, list(), cfg$missions, cfg$policy_menu)
  expect_match(pf2$compacts$mission_statement, "every node the model could test \\(1 node the model could not test\\)")
})

test_that("the institutional slots keep to the allied set and to economies that supply the compact", {
  res <- strat_fx_res()
  add <- tibble::tibble(compact_id = c("AIB-C02", "AIB-C02", "AIB-C03", "AIB-C04"), iso3 = c("IND", "TWN", "RUS", "AUS"),
                        role = c(NA, NA, "Supplier", "Supplier"), basis = c(NA, NA, "supplies", "supplies"),
                        coalition_tier = c("critical external dependency", "critical external dependency", "core coordinating member", "critical supply partner"),
                        in_minimum_coalition = c(FALSE, FALSE, TRUE, TRUE), critical_supplier = c(FALSE, FALSE, FALSE, TRUE))
  res$compact_members <- dplyr::bind_rows(res$compact_members, add)
  st <- strat_run(res = res)
  d <- st$compacts[st$compacts$scenario == "decoupling", ]
  a <- d[d$strategic_compact_id == "SC-A", ]
  codes <- function(x) strsplit(x, " ", fixed = TRUE)[[1]]
  expect_equal(a$critical_external_dependencies, "CHN")             # non-allied only
  # TWN (treat_as_allied) and IND are listed as critical external dependencies outside every SC-A coalition:
  # each is "outside the core" unless it is one of SC-A's core countries, and a core country never is
  core_a <- codes(a$core_countries)
  expect_true("TWN" %in% codes(a$allied_outside_core))
  expect_setequal(codes(a$allied_outside_core), setdiff(c("IND", "TWN"), core_a))
  expect_length(intersect(core_a, codes(a$allied_outside_core)), 0)
  expect_false("RUS" %in% codes(a$coalition_members))
  expect_equal(a$non_allied_relied_on, "RUS")
  expect_equal(a$critical_supply_partners, "JPN")
  b <- d[d$strategic_compact_id == "SC-B", ]
  expect_true("AUS" %in% codes(b$coalition_members))
  expect_false("AUS" %in% codes(b$critical_supply_partners))        # no supply position in SC-B
  cr <- st$compact_roles
  expect_equal(cr$criticality[cr$iso3 == "IND" & cr$strategic_compact_id == "SC-A"],
               if ("IND" %in% core_a) "" else "allied supplier outside the core coalition")
  expect_equal(cr$criticality[cr$iso3 == "CHN" & cr$strategic_compact_id == "SC-A"], "critical external dependency")
  # an allied economy with a compact role, outside the core countries and every SC-A coalition, that a workstream
  # lists as a critical external dependency: an allied supplier outside the core coalition
  # (the economy is any allied one with an SC-A compact role that is not a core country: the core is structural,
  # so taking it out of SC-A's coalitions does not change the core)
  ws_a <- st$workstreams$workstream_id[st$workstreams$strategic_compact_id == "SC-A"]
  out_iso <- setdiff(cr$iso3[cr$strategic_compact_id == "SC-A" & cr$allied & cr$criticality %in% ""], c(core_a, "IND", "TWN"))
  skip_if(length(out_iso) == 0, "the fixture has no allied non-core economy in SC-A")
  x4 <- out_iso[1]
  m4 <- res$compact_members
  m4 <- m4[!(m4$iso3 == x4 & m4$compact_id %in% ws_a), ]
  res4 <- res
  # out of SC-A's coalitions in both scenarios (compact roles are structural: they read both)
  if (!is.null(res4$compact_members_managed)) {
    mm <- res4$compact_members_managed
    res4$compact_members_managed <- mm[!(mm$iso3 == x4 & mm$compact_id %in% ws_a), , drop = FALSE]
  }
  res4$compact_members <- dplyr::bind_rows(m4, tibble::tibble(compact_id = ws_a[1], iso3 = x4, role = NA_character_, basis = NA_character_,
                                                              coalition_tier = "critical external dependency", in_minimum_coalition = FALSE,
                                                              critical_supplier = FALSE))
  st4 <- strat_run(res = res4)
  a4 <- st4$compacts[st4$compacts$scenario == "decoupling" & st4$compacts$strategic_compact_id == "SC-A", ]
  expect_false(x4 %in% codes(a4$core_countries))
  expect_true(x4 %in% codes(a4$allied_outside_core))
  expect_equal(st4$compact_roles$criticality[st4$compact_roles$iso3 == x4 & st4$compact_roles$strategic_compact_id == "SC-A"],
               "allied supplier outside the core coalition")
  # treat_as_allied: an economy outside the coalition tiers joins the allied set, not the demand scope
  cfg <- strat_layer_cfg()
  cfg$allied$treat_as_allied <- list("CHN")
  st2 <- strat_run(cfg = cfg)
  expect_true("CHN" %in% st2$allied)
  expect_false("CHN" %in% st2$demand_scope)
  expect_equal(st2$compact_roles$compact_role[st2$compact_roles$iso3 == "CHN" & st2$compact_roles$strategic_compact_id == "SC-A"], "Processing anchor")
})

test_that("compact relevance, resilience and the narrative slots weigh value, not shares of small nodes", {
  cr <- strat_st$compact_roles
  # relevance: the larger of the supplier and demand contributions
  expect_equal(cr$compact_relevance, pmax(cr$supplier_contribution, cr$demand_contribution))
  # resilience: US$ restored over the compact's allied demand (SC-A: 100 + 200 + 300 + 1000)
  aus <- cr[cr$iso3 == "AUS" & cr$strategic_compact_id == "SC-A", ]
  expect_equal(aus$resilience_usd_decoupling, 10 + 40)
  expect_equal(aus$resilience_contribution_decoupling, 50 / 1600)
  # partners: US$ that new and expanded trade restores between the two (both scenarios, both directions)
  n <- strat_st$narratives
  expect_equal(n$items_partners[n$iso3 == "JPN"], "USA AUS VNM")
  expect_equal(n$items_partners[n$iso3 == "USA"], "JPN AUS")
})

test_that("a thesis reads its role-based logic on the priority compacts only, complementary compacts first", {
  # A and B share five compacts. S5: both are demand anchors (no complementarity: left out). S1-S3: their
  # roles differ but neither buys what the other makes. S4, the least relevant, would give technology.
  roles_a <- c("Scale manufacturer", "Scale manufacturer", "Scale manufacturer", "Specialized technology supplier", "Demand anchor")
  roles_b <- c("Processing anchor", "Processing anchor", "Processing anchor", "Scale manufacturer", "Demand anchor")
  cr <- tibble::tibble(
    iso3 = rep(c("AAA", "BBB"), each = 5), sc_id = rep(c("S1", "S2", "S3", "S4", "S5"), 2),
    compact_relevance = rep(c(0.5, 0.4, 0.3, 0.1, 0.9), 2),
    compact_role = c(roles_a, roles_b), compact_functions = c(roles_a, roles_b))
  pair <- tibble::tibble(a = "AAA", b = "BBB")
  th <- aib_relationship_theses(pair, cr, NULL)
  expect_equal(th$priority_compacts, "S1 S2 S3")
  expect_equal(th$shared_compacts, 5L)
  expect_equal(th$logic, "")
  # with S4 among the priority compacts, its evidence counts
  cr$compact_relevance[cr$sc_id == "S4"] <- 0.45
  th2 <- aib_relationship_theses(pair, cr, NULL)
  expect_equal(th2$priority_compacts, "S1 S4 S2")
  expect_equal(th2$logic, "technology")
  # a complementary compact (one side buys what the other makes) leads whatever its relevance
  cr$compact_role[cr$iso3 == "BBB" & cr$sc_id == "S3"] <- "Demand anchor"
  cr$compact_functions[cr$iso3 == "BBB" & cr$sc_id == "S3"] <- "Demand anchor"
  th3 <- aib_relationship_theses(pair, cr, NULL)
  expect_equal(th3$priority_compacts, "S3 S1 S4")
  expect_equal(th3$logic, "technology; market access")
  # the article follows the first label ("an investment relationship")
  e <- tibble::tibble(supplier_iso = "AAA", buyer_iso = "BBB", cumulative_trade_resilience_contribution = 0,
                      cumulative_investment_resilience_contribution = 0.1)
  th4 <- aib_relationship_theses(pair, cr[cr$sc_id == "S5", ], e)
  expect_equal(th4$priority_compacts, "")
  expect_match(th4$thesis, "^AAA - BBB: an investment relationship\\.$")
})

test_that("narrative items are codes the bundle can join, and aggregated nodes read as their members in prose", {
  n <- strat_st$narratives
  codes <- function(x) unlist(strsplit(stats::na.omit(x[nzchar(x)]), " ", fixed = TRUE))
  expect_true(all(codes(c(n$items_contributes, n$items_needs, n$items_capacity)) %in% strat_st$map$node_id))
  expect_true(all(codes(n$items_compacts) %in% strat_st$compact_defs$strategic_compact_id))
  expect_true(all(codes(n$items_partners) %in% strat_fx_allied()))
  expect_true(all(codes(n$items_focus) %in% aib_strategic_functions()))
  for (i in seq_len(nrow(n))) {
    expect_equal(length(codes(n$items_contributes[i])), length(codes(n$items_contributes_share[i])))
  }
  expect_equal(aib_strategic_prose_label(c("Uranium/fuel Upstream: aggregated low-coverage sub-sectors (Spent fuel & nuclear scrap, Uranium ores)", "Wafers")),
               c("Spent fuel & nuclear scrap and uranium ores (aggregated)", "Wafers"))
})

test_that("moving a node to another compact changes only the compacts involved: node functions are keyed by node id", {
  y <- strat_fx_compacts_yml()
  y$compacts[[2]]$value_chain[[2]]$nodes <- NULL
  y$compacts[[3]]$value_chain[[1]]$nodes <- list("cell")
  st2 <- strat_run(yml = y)
  expect_equal(st2$problems, character())
  expect_equal(st2$map$node_function[st2$map$node_id == "cell"], "systems")
  keep <- function(x) as.data.frame(x[x$strategic_compact_id == "SC-A", setdiff(names(x), "strategic_compact_name")])
  expect_equal(keep(st2$compact_roles), keep(strat_st$compact_roles))
  expect_equal(st2$workstreams$strategic_compact_id[st2$workstreams$workstream_id == "AIB-C04"], "SC-C")
  expect_true(all(c("SC-A", "SC-B", "SC-C") %in% st2$compacts$strategic_compact_id))
})

test_that("narratives and theses are filled from templates, and the core countries are the same in every scenario", {
  n <- strat_st$narratives
  expect_true(all(strat_fx_allied() %in% n$iso3))
  expect_match(n$narrative[n$iso3 == "USA"], "^United States: Demand & Market Anchor\\.")
  expect_match(n$narrative[n$iso3 == "DEU"], "No strategic AIB role at the current thresholds")
  core <- unique(strat_st$compacts[, c("strategic_compact_id", "core_countries")])
  expect_equal(nrow(core), length(unique(strat_st$compacts$strategic_compact_id)))
  th <- strat_st$theses
  expect_true(all(th$a < th$b))
  expect_true(all(c("AUS", "USA") %in% c(th$a, th$b)))
  expect_match(th$thesis[th$a == "AUS" & th$b == "USA"], "^Australia - United States: a trade")
})

# ---- The real outputs of scripts/33 (when present) ----------------------------------------------------------------

test_that("the real strategic layer keeps the contract (skipped without scripts/33's outputs)", {
  f <- file.path(strat_real_dir, "aib_strategic.rds")
  skip_if_not(file.exists(f), "no aib_strategic.rds (run scripts/33)")
  st <- readRDS(f)
  ws <- st$workstreams
  expect_false(anyDuplicated(ws$workstream_id) > 0)
  expect_true(all(!is.na(ws$strategic_compact_id)))
  cp <- st$compacts
  by <- split(cp$included_nodes, cp$scenario)
  for (s in names(by)) expect_equal(by[[s]], by[["decoupling"]])
  expect_gte(length(unique(cp$strategic_compact_id)), 6)
  expect_lte(length(unique(cp$strategic_compact_id)), 10)
  q <- st$roles[st$roles$primary_ok | st$roles$secondary_ok, ]
  expect_lt(max(table(q$iso3)), 7)                                   # nobody qualifies for all seven
  prim <- function(iso) sort(st$roles$role_code[st$roles$iso3 == iso & st$roles$primary_or_secondary == "primary"])
  expect_false(identical(prim("JPN"), prim("KOR")))
  expect_false(identical(prim("JPN"), prim("USA")))
  expect_false(identical(prim("KOR"), prim("USA")))
  cr <- st$compact_roles
  for (i in seq_len(nrow(cr))) expect_true(isTRUE(cr[[cr$lead_function[i]]][i]))
})

test_that("the core keeps the major players and adds the chain's upstream anchors", {
  st <- strat_run()
  cr <- st$compact_roles
  skip_if_not(all(c("lead_share", "allied", "compact_role", "compact_relevance") %in% names(cr)), "compact roles without lead shares")
  core <- unique(st$compacts[, c("strategic_compact_id", "core_countries")])
  al <- cr[cr$allied, , drop = FALSE]
  expect_gt(nrow(al), 0)
  for (sc in unique(al$strategic_compact_id)) {
    d <- al[al$strategic_compact_id == sc, , drop = FALSE]
    d <- d[order(-d$compact_relevance, d$iso3), , drop = FALSE]
    cc <- strsplit(core$core_countries[core$strategic_compact_id == sc], " ", fixed = TRUE)[[1]]
    expect_true(all(utils::head(d$iso3, 3) %in% cc), info = sc)                       # the three most relevant
    up <- d[d$compact_role %in% AIB_CORE_FUNCTION_ORDER[1:2] & dplyr::coalesce(d$lead_share, 0) >= 0.10, , drop = FALSE]
    up <- up[order(match(up$compact_role, AIB_CORE_FUNCTION_ORDER), -up$lead_share), , drop = FALSE]
    expect_true(all(up$iso3[!duplicated(up$compact_role)] %in% cc), info = sc)        # the strongest upstream anchors
    expect_lte(length(cc), 6)
  }
})
