# Partnership Memos (docs/partnership_memos.md): validation of the generic pipeline on a synthetic pair.
# The pair is AAA / BBB with a coercive third supplier CCC, so nothing here depends on US-Japan data.

pm_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/utils/supply_chain_stages.R", sort(list.files(file.path(pm_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE))))
  source(if (file.exists(f)) f else file.path(pm_root, f), local = FALSE)

pm_test_cfg <- function() {
  cfg <- yaml::read_yaml(file.path(pm_root, "config", "partnership_memos.yml"))
  cfg$inputs$coercive_suppliers <- c("CCC")
  cfg$pairs$TEST <- list(a = "AAA", b = "BBB", a_name = "Alpha", a_short = "Alpha", b_name = "Beta", b_short = "Beta",
                         title = "Alpha-Beta Strategic Energy & Industrial Partnership")
  cfg
}

stages6 <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")

# One strategic-position row per country x T1 x stage, defaults neutral; `over` sets columns per (iso, stage).
mk_sp <- function(over) {
  g <- expand.grid(iso3 = c("AAA", "BBB", "CCC"), supply_chain = stages6, stringsAsFactors = FALSE)
  g$tech <- "T1"; g$Country <- g$iso3
  n <- nrow(g)
  def <- list(cp_contender_pct = 0.3, competitive_position_percentile = 0.4, cp_high = FALSE, cp_is_contender = FALSE,
              deployment_need = NA_real_, supply_chain_need = 0.3, home_share_exposure = 0.01, sn_import_material = TRUE,
              stage_evidence_quality_cp = "high", stage_evidence_quality_sn = "high", trade_benchmark_share = 0.01, trade_benchmark_rca = 0.5,
              cp_production_position = NA_real_, cp_manufacturing_capacity = NA_real_, cp_investment_position = NA_real_, trade_momentum_score = 0.5,
              imports_usd = 1e9, imports_to_gdp = 0.001, china_share = 0.2, coercive_import_share = 0.2, top_supplier_iso3 = "CCC",
              top_supplier_share = 0.3, import_hhi = 0.2, strategic_industrial_value = 0.5, large_yard_case = FALSE,
              strategic_choice = "watch", security_rationale = NA_character_, delivery_mode = "diversified_global", posture_confidence = "high",
              policy_priority_percentile = 0.5, policy_support_priority = 0.5, policy_protection_priority = 0.4, policy_momentum = 0.5,
              policy_alignment = "aligned", evidence_specificity_cp = 0.8, trade_benchmark_period = "2022-2024",
              recommended_partner_1 = NA_character_, recommended_partner_2 = NA_character_, recommended_partner_3 = NA_character_,
              partner_aib_status = NA_character_, world_top_iso3 = "CCC", world_top_share = 0.3, coercive_top_iso3 = "CCC",
              coercive_world_share = 0.3, trusted_world_share = 0.6)
  for (k in names(def)) g[[k]] <- rep(def[[k]], n)
  g$deployment_need[g$supply_chain == "Deployment"] <- 0.3
  g$supply_chain_need[g$supply_chain == "Deployment"] <- NA
  for (o in over) { i <- g$iso3 == o$iso & g$supply_chain == o$stage; for (k in setdiff(names(o), c("iso", "stage"))) g[[k]][i] <- o[[k]] }
  g
}

mk_inputs <- function(sp_over = list(), dol_over = NULL) {
  sp <- mk_sp(c(list(
    # Extraction: both need it, CCC holds 80% -> shared vulnerability
    list(iso = "AAA", stage = "Extraction", supply_chain_need = 0.9, coercive_world_share = 0.8, coercive_import_share = 0.6),
    list(iso = "BBB", stage = "Extraction", supply_chain_need = 0.9, coercive_world_share = 0.8, coercive_import_share = 0.6),
    # Refining: AAA strong, BBB high need -> AAA supplies / BBB needs
    list(iso = "AAA", stage = "Refining", cp_high = TRUE, cp_is_contender = TRUE, cp_contender_pct = 0.9, supply_chain_need = 0.2),
    list(iso = "BBB", stage = "Refining", supply_chain_need = 0.85),
    # Components: BBB strong, AAA high need, trusted capacity short -> capacity constrained (BBB supplier)
    list(iso = "BBB", stage = "Components", cp_high = TRUE, cp_is_contender = TRUE, cp_contender_pct = 0.85, trusted_world_share = 0.2),
    list(iso = "AAA", stage = "Components", supply_chain_need = 0.95, trusted_world_share = 0.2),
    # Assembly: both strong -> direct competition
    list(iso = "AAA", stage = "Assembly", cp_high = TRUE, cp_is_contender = TRUE, cp_contender_pct = 0.9),
    list(iso = "BBB", stage = "Assembly", cp_high = TRUE, cp_is_contender = TRUE, cp_contender_pct = 0.8),
    # Recovery: weak evidence on BBB
    list(iso = "BBB", stage = "Recovery", stage_evidence_quality_cp = "low", supply_chain_need = 0.9),
    list(iso = "AAA", stage = "Recovery", cp_high = TRUE, cp_is_contender = TRUE, cp_contender_pct = 0.9)), sp_over))
  nodes <- data.frame(node_id = c("n-ext", "n-ref", "n-comp", "n-asm", "n-low"),
                      supply_chain = c("Extraction", "Refining", "Components", "Assembly", "Assembly"),
                      score_cells = c("T1|Extraction", "T1|Refining", "T1|Components", "T1|Assembly", "T1|Assembly"), stringsAsFactors = FALSE)
  spine <- data.frame(node_id = nodes$node_id, node_label = c("Ore", "Refined metal", "Part", "Machine", "Commodity box"),
                      strategic_compact_id = c("SC-X", "SC-X", "SC-X", "SC-X", "SC-Y"), value_chain_step = c("ore", "metal", "part", "machine", "box"),
                      value_chain_order = c(1, 2, 3, 4, 1), demand_usd = 1e9, largest_supplier = "CCC", largest_share = c(0.8, 0.3, 0.3, 0.3, 0.2),
                      scenario = "baseline", stringsAsFactors = FALSE)
  nclass <- data.frame(node_id = nodes$node_id, node_classification = c("BUILD", "LOW INTERVENTION", "BUILD", "LOW INTERVENTION", "LOW INTERVENTION"),
                       policy_needs = "none", residual_gap = c(0.4, 0, 0.3, 0, 0), static_coverage = 0.8,
                       data_quality = "High confidence", optimized_largest_supplier = c("DDD", "AAA", "BBB", "AAA", "DDD"),
                       priority_investment_hosts = c("DDD", "", "AAA; DDD", "", ""), specialty_hosts = "", development_hosts = "", stringsAsFactors = FALSE)
  dol <- rbind(
    data.frame(node_id = "n-ext", iso3 = c("AAA", "BBB", "CCC"), position_share = c(0.01, 0.01, 0.8), net_import_share = c(0.2, 0.2, 0), scope_demand_share = c(0.3, 0.3, 0)),
    data.frame(node_id = "n-ref", iso3 = c("AAA", "BBB", "CCC"), position_share = c(0.3, 0.01, 0.3), net_import_share = c(0, 0.2, 0), scope_demand_share = c(0.05, 0.3, 0)),
    data.frame(node_id = "n-comp", iso3 = c("AAA", "BBB", "CCC"), position_share = c(0.01, 0.15, 0.3), net_import_share = c(0.25, 0, 0), scope_demand_share = c(0.3, 0.05, 0)),
    data.frame(node_id = "n-asm", iso3 = c("AAA", "BBB", "CCC"), position_share = c(0.2, 0.25, 0.3), net_import_share = c(0, 0, 0), scope_demand_share = c(0.1, 0.1, 0)),
    data.frame(node_id = "n-low", iso3 = c("AAA", "BBB", "CCC"), position_share = c(0.01, 0.01, 0.2), net_import_share = c(0.005, 0.005, 0), scope_demand_share = c(0.01, 0.01, 0)))
  dol$strategic_compact_id <- ifelse(dol$node_id == "n-low", "SC-Y", "SC-X")
  dol$world_share <- dol$position_share; dol$production_share <- NA; dol$export_rca <- 1.2; dol$world_import_share <- 0.1
  dol$best_host_rank <- NA; dol$capable_host <- FALSE; dol$momentum <- FALSE; dol$anchor_position <- FALSE; dol$specialist_position <- FALSE
  if (!is.null(dol_over)) dol <- dol_over(dol)
  trade <- data.frame(reporter_iso = c("AAA", "AAA", "BBB", "BBB"), partner_iso = c("BBB", "CCC", "AAA", "CCC"), tech = "T1",
                      supply_chain = "Refining", imports_usd = c(1e8, 9e8, 2e8, 8e8), stringsAsFactors = FALSE)
  ev <- data.frame(evidence_id = c("X1", "X2"), item_type = c("project", "framework"), name = c("Part plant", "Minerals framework"),
                   institution = c("PartCo", "Govts"), countries = c("AAA|BBB", "AAA|BBB"), status = c("announced", "signed"),
                   publication_date = c("2026-01-01", "2025-10-01"), access_date = "2026-11-06", technology = "t1", compact = c("SC-X", "SC-X"),
                   stage = c("Components", "cross_stage"), aib_node = c("n-comp", ""), instrument = c("fdi", "dialogue"), amount_usd = "",
                   source = "test", url = "https://example.org", claim = c("A plant is announced.", "A framework is signed."),
                   verification = c("partial", "verified"), research_pass = "test", basis = "external", stringsAsFactors = FALSE)
  mech <- data.frame(mechanism_id = "M1", pair = "AAA|BBB", name = "Investment vehicle", mechanism_type = "investment_vehicle", status = "in_force",
                     evidence_ids = "X2", scope_compacts = "SC-X", cross_sector = "TRUE", has_pipeline = "TRUE", has_finance = "TRUE",
                     has_kpis = "FALSE", has_review = "partial", reciprocal = "FALSE", sunset = "2029-01-19", assessed_on = "2026-11-06", notes = "", stringsAsFactors = FALSE)
  inst <- utils::read.csv(file.path(pm_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)
  croles <- data.frame(strategic_compact_id = "SC-X", iso3 = c("AAA", "BBB"), compact_role = c("Demand anchor", "Specialized technology supplier"),
                       lead_node = c("n-ref", "n-comp"), compact_relevance = c(0.2, 0.2), stringsAsFactors = FALSE)
  list(sp = sp, nodes = nodes, spine = spine, nclass = nclass, dol = dol, trade = trade, ev = ev, mech = mech, inst = inst, croles = croles,
       invest_pairs = data.frame(node_id = character(), investor_iso = character(), host_iso = character(), host_rank = integer()))
}

run_pair <- function(inp = mk_inputs(), cfg = pm_test_cfg()) {
  frame <- pm_node_frame(inp$nodes, inp$spine, inp$nclass, inp$dol, cfg$inputs$coercive_suppliers)
  pc <- cfg$pairs$TEST
  res <- pm_assess_pair(list(sp = inp$sp, frame = frame, dol = inp$dol, trade = inp$trade, ev = inp$ev, mech = inp$mech, inst = inp$inst,
                             devpart = NULL, invest_pairs = inp$invest_pairs, croles = inp$croles), pc, cfg)
  d <- res$dyad
  compacts <- data.frame(strategic_compact_id = c("SC-X", "SC-Y"), strategic_compact_name = c("Compact X", "Compact Y"), primary_mission = "build",
                         class_mix = "", mission_statement = "Build X.", stringsAsFactors = FALSE)
  cc <- pm_compact_candidates(d, compacts, inp$croles, inp$mech, cfg, "AAA", "BBB", inp$ev)
  sel <- cc$strategic_compact_id[cc$recommended]
  arch <- do.call(rbind, lapply(cc$strategic_compact_id, function(cm) pm_compact_architecture(d, cm, res$links)))
  nms <- list(a_short = "Alpha", b_short = "Beta")
  trade_tbl <- pm_trade_agenda(d, cfg, "AAA", "BBB", nms)
  inv <- pm_investment_agenda(d, cfg, inp$ev, inp$inst, "AAA", "BBB")
  deals <- pm_deal_pipeline(d, inp$inst, data.frame(iso3 = "AAA", institution = "X", abbreviation = "XA", instrument_families = "demand|trade|finance|investment|standards|technology|security|development"),
                            inp$ev, cfg, "AAA", "BBB", nms)
  mea <- pm_mea(sel, inp$mech, cfg)
  nonp <- pm_non_priorities(d, mea, cfg, nms, "AAA", "BBB")
  sroles <- data.frame(iso3 = c("AAA", "BBB"), role_code = c("DMA", "STM"), strategic_role = c("Demand & Market Anchor", "Specialized Technology"),
                       primary_or_secondary = "primary", role_strength = 0.9, stringsAsFactors = FALSE)
  memo <- pm_memo_sections(d = d, prof = res$profiles, cc = cc, arch = arch, trade_tbl = trade_tbl, inv_tbl = inv, third = data.frame(), deals = deals,
                           mea = mea, nonp = nonp, ev = inp$ev, mech = inp$mech, sroles = sroles,
                           agreements = data.frame(iso3 = "BBB", agreement = "G7"), compacts = compacts, pair_cfg = cfg$pairs$TEST, cfg = cfg)
  reg <- pm_evidence_register(memo$recommendations, d, inp$ev, inp$mech, "AAA", "BBB")
  list(d = d, prof = res$profiles, cc = cc, trade = trade_tbl, inv = inv, deals = deals, mea = mea, nonp = nonp, memo = memo, reg = reg)
}

test_that("every recommendation has an evidence trail", {
  r <- run_pair()
  recs <- r$memo$recommendations
  expect_gt(nrow(recs), 0)
  expect_true(all(recs$rec_id %in% r$reg$claim_id))
  expect_true(all(r$reg$basis %in% c("osi", "external")))
  ext <- r$reg[r$reg$basis == "external", ]
  expect_true(all(nzchar(ext$external_ref)))
})

test_that("six-stage units roll up to the three-stage view", {
  expect_identical(pm_rollup(stages6), c("Upstream", "Upstream", "Midstream", "Midstream", "Downstream", "Upstream"))
  r <- run_pair()
  p <- r$prof
  expect_true(all(p$stage %in% stages6))
  expect_identical(p$rollup, pm_rollup(p$stage))
})

test_that("proxy evidence is never treated as exact", {
  r <- run_pair(); cfg <- pm_test_cfg()
  nodes <- r$d[r$d$unit_type == "node", ]
  expect_true(all(nodes$need_basis_a == "inherited_from_cell"))
  expect_true(all(nodes$evidence_quality <= cfg$evidence$proxy_cap + 1e-9, na.rm = TRUE))
  need_rows <- r$reg[r$reg$osi_variable == "strategic_need" & r$reg$unit %in% nodes$unit_id, ]
  expect_gt(nrow(need_rows), 0)
  expect_true(all(need_rows$evidence_specificity == "proxy"))
})

test_that("low-evidence units are flagged and never joint bets", {
  r <- run_pair()
  rec <- r$d[r$d$unit_id == "T1|Recovery", ]
  expect_true(rec$low_evidence)
  expect_false(any(r$d$priority_tier == 1 & r$d$low_evidence))
})

test_that("the system can output LOW INTERVENTION / no priority", {
  r <- run_pair()
  low <- r$d[r$d$unit_id == "n-low", ]
  expect_equal(low$relationship_state, "low_strategic_case")
  expect_equal(low$treatment, "LOW INTERVENTION")
  expect_equal(low$priority_label, "leave open")
  expect_true(any(r$cc$compact_fit == "not a bilateral priority"))
  expect_gte(nrow(r$nonp), 1)
})

test_that("two high competitive positions are competition, not complementarity", {
  r <- run_pair()
  both <- r$d[r$d$position_band_a == "strong" & r$d$position_band_b == "strong" & r$d$stage != "Deployment", ]
  expect_gt(nrow(both), 0)
  expect_true(all(both$relationship_state == "direct_competition"))
  expect_true(all(both$bilateral_complementarity == 0))
  expect_false(any(r$d$relationship_state %in% PM_ACTIVE_STATES & r$d$position_band_a == "strong" & r$d$position_band_b == "strong"))
})

test_that("competition, supply, capacity and shared-vulnerability states are detected", {
  d <- run_pair()$d
  st <- setNames(d$relationship_state, d$unit_id)
  expect_equal(unname(st["n-asm"]), "direct_competition")
  expect_equal(unname(st["n-ref"]), "a_supplies_b_needs")
  expect_equal(unname(st["n-comp"]), "capacity_constrained_trusted_supply")
  expect_equal(d$supplier[d$unit_id == "n-comp"], "BBB")
  expect_equal(unname(st["n-ext"]), "shared_vulnerability")
  expect_equal(unname(st["T1|Assembly"]), "direct_competition")
})

test_that("the main instrument is the lightest that can resolve the state", {
  d <- run_pair()$d
  cfg <- pm_test_cfg()
  inst <- utils::read.csv(file.path(pm_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)
  fam <- setNames(inst$family, inst$instrument_id)
  act <- d[!is.na(d$main_instrument), ]
  ok <- mapply(function(st, m) fam[[m]] %in% unlist(cfg$effective_families[[st]]), act$relationship_state, act$main_instrument)
  expect_true(all(ok))
  heavy <- d[grepl("price_floor|strategic_equity", d$instruments), ]
  expect_true(all(heavy$severe_chokepoint & heavy$allied_short))
})

test_that("the same data produce identical memo tables", {
  r1 <- run_pair(); r2 <- run_pair()
  flat <- function(x) { for (k in names(x)) if (is.list(x[[k]])) x[[k]] <- vapply(x[[k]], paste, character(1), collapse = ";"); x }
  expect_identical(flat(r1$d), flat(r2$d))
  expect_identical(r1$deals, r2$deals)
  expect_identical(r1$reg, r2$reg)
  expect_identical(r1$memo$markdown[-3], r2$memo$markdown[-3])
})

test_that("the pipeline is generic: no country codes in R/partnerships", {
  files <- list.files(file.path(pm_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE)
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  txt <- sub("#.*$", "", txt)
  expect_false(any(grepl("\"(USA|JPN|CHN|KOR|AUS)\"", txt)))
  r <- run_pair()
  expect_true(all(c("AAA", "BBB") %in% r$prof$iso3))
})

test_that("an MEA is not recommended where an existing mechanism carries pipeline and finance", {
  r <- run_pair()
  expect_true(r$mea$option %in% c("existing", "upgrade"))
  expect_true(any(grepl("Do not create a new bilateral", r$nonp$text)))
})

test_that("the print document follows the brief format and cites external evidence as sources", {
  r <- run_pair(); cfg <- pm_test_cfg()
  md <- c(r$memo$markdown, "", "## 12. Wide table", "", "| A | B | C | D | E | F | G | H | I | J | Key obstacle |", "|---|---|---|---|---|---|---|---|---|---|---|",
          "| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | x_y;z | none |")
  doc <- pm_document(md, mk_inputs()$ev, r$memo$boxes, cfg$pairs$TEST, cfg$document,
                     list(pair_id = "AAA_BBB", as_of = "2026-10-30", as_of_label = "30 October 2026", evidence_access = "2026-11-06",
                          generated_at = "2026-11-06T00:00:00Z"), "OSI tables.")
  expect_equal(doc$format, "brief")
  expect_equal(doc$sections[[1]]$n, 0L)
  expect_equal(doc$sections[[1]]$blocks[[1]]$type, "choices")
  expect_true(all(vapply(doc$sections[-1], function(s) s$n > 0, TRUE)))
  expect_equal(doc$sources[[1]]$id, "osi")
  ids <- vapply(doc$sources[-1], function(s) s$id, "")
  expect_true(all(ids %in% mk_inputs()$ev$evidence_id))
  wide <- Filter(function(b) identical(b$type, "table"), doc$sections[[length(doc$sections)]]$blocks)[[1]]
  expect_equal(length(wide$head), 10L)   # only listed columns are dropped: one of eleven here
  expect_false("Key obstacle" %in% wide$head)
  cells <- vapply(wide$rows[[1]], function(c) c$inlines[[1]]$v, "")
  expect_true("x y; z" %in% cells)
  txt <- unlist(lapply(doc$sections, function(s) lapply(s$blocks, function(b) if (!is.null(b$items)) lapply(b$items, function(i) vapply(i$inlines, function(z) z$v, "")) else if (!is.null(b$inlines)) vapply(b$inlines, function(z) z$v, ""))))
  expect_false(any(grepl("[External", txt, fixed = TRUE)))
})
