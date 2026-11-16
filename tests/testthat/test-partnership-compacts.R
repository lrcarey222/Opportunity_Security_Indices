# Partnership Memos: blocs (pm_bloc.R) and Compact memos (pm_compact.R, pm_compact_memo.R) on synthetic data.
# Members AAA, BBB, DDD; CCC is the coercive supplier; EE1 and EE2 form the bloc EEE. Nothing here uses real data.

pm_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/utils/supply_chain_stages.R", sort(list.files(file.path(pm_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE))))
  source(if (file.exists(f)) f else file.path(pm_root, f), local = FALSE)

cp_cfg <- function() {
  cfg <- yaml::read_yaml(file.path(pm_root, "config", "partnership_memos.yml"))
  cfg$inputs$coercive_suppliers <- "CCC"
  cfg$blocs <- list(EEE = list(name = "Bloc", members = c("EE1", "EE2")))
  cfg
}
stg6 <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")

# strategic-position rows: one technology T1, every stage, neutral defaults; `over` sets columns per (iso, stage)
cp_sp <- function(isos, over = list()) {
  g <- expand.grid(iso3 = isos, supply_chain = stg6, stringsAsFactors = FALSE)
  g$tech <- "T1"; g$Country <- g$iso3
  def <- list(cp_contender_pct = 0.3, competitive_position_percentile = 0.4, cp_high = FALSE, cp_is_contender = FALSE, deployment_need = NA_real_,
              supply_chain_need = 0.3, home_share_exposure = 0.01, sn_import_material = TRUE, stage_evidence_quality_cp = "high",
              stage_evidence_quality_sn = "high", trade_benchmark_share = 0.01, trade_benchmark_rca = 0.5, trade_benchmark_exports = 1e8,
              cp_production_position = NA_real_, cp_manufacturing_capacity = NA_real_, cp_investment_position = NA_real_, trade_momentum_score = 0.5,
              imports_usd = 1e9, imports_to_gdp = 0.001, china_share = 0.2, coercive_import_share = 0.2, top_supplier_iso3 = "CCC", top_supplier_share = 0.3,
              import_hhi = 0.2, strategic_industrial_value = 0.5, large_yard_case = FALSE, strategic_choice = "watch", security_rationale = NA_character_,
              delivery_mode = "diversified_global", posture_confidence = "high", policy_priority_percentile = 0.5, policy_support_priority = 0.5,
              policy_protection_priority = 0.3, policy_momentum = 0.5, policy_alignment = "aligned", evidence_specificity_cp = 0.8,
              trade_benchmark_period = "2022-2024", recommended_partner_1 = NA_character_, recommended_partner_2 = NA_character_,
              recommended_partner_3 = NA_character_, partner_aib_status = NA_character_, world_top_iso3 = "CCC", world_top_share = 0.3,
              coercive_top_iso3 = "CCC", coercive_world_share = 0.3, trusted_world_share = 0.6)
  for (k in names(def)) g[[k]] <- rep(def[[k]], nrow(g))
  g$deployment_need[g$supply_chain == "Deployment"] <- 0.3
  g$supply_chain_need[g$supply_chain == "Deployment"] <- NA
  for (o in over) { i <- g$iso3 == o$iso & g$supply_chain == o$stage; for (k in setdiff(names(o), c("iso", "stage"))) g[[k]][i] <- o[[k]] }
  g
}

cp_inputs <- function() {
  sp <- cp_sp(c("AAA", "BBB", "DDD", "CCC"), list(
    list(iso = "AAA", stage = "Extraction", supply_chain_need = 0.9), list(iso = "BBB", stage = "Extraction", supply_chain_need = 0.9),
    list(iso = "BBB", stage = "Refining", supply_chain_need = 0.85), list(iso = "AAA", stage = "Components", supply_chain_need = 0.95),
    list(iso = "DDD", stage = "Components", supply_chain_need = 0.9)))
  nodes <- data.frame(node_id = c("n-ext", "n-ref", "n-comp", "n-asm", "n-low"), supply_chain = c("Extraction", "Refining", "Components", "Assembly", "Assembly"),
                      score_cells = c("T1|Extraction", "T1|Refining", "T1|Components", "T1|Assembly", "T1|Assembly"), stringsAsFactors = FALSE)
  spine <- data.frame(node_id = nodes$node_id, node_label = c("Ore", "Refined metal", "Part", "Machine", "Commodity box"), strategic_compact_id = "SC-X",
                      value_chain_step = c("ore", "metal", "part", "machine", "machine"), value_chain_order = c(1, 2, 3, 4, 4), demand_usd = 1e9,
                      largest_supplier = "CCC", largest_share = c(0.8, 0.3, 0.3, 0.3, 0.2), scenario = "baseline", stringsAsFactors = FALSE)
  nclass <- data.frame(node_id = nodes$node_id, node_classification = c("BUILD", "LOW INTERVENTION", "BUILD", "LOW INTERVENTION", "LOW INTERVENTION"),
                       policy_needs = "none", residual_gap = c(0.4, 0, 0.3, 0, 0), static_coverage = 0.8, data_quality = "High confidence",
                       optimized_largest_supplier = c("FFF", "AAA", "BBB", "AAA", "FFF"), priority_investment_hosts = c("FFF", "", "AAA; FFF", "", ""),
                       specialty_hosts = "", development_hosts = "", stringsAsFactors = FALSE)
  dl <- function(node, ps, ni, dm) data.frame(node_id = node, iso3 = c("AAA", "BBB", "DDD", "CCC"), position_share = ps, net_import_share = ni, scope_demand_share = dm)
  dol <- rbind(dl("n-ext", c(0.01, 0.01, 0.01, 0.8), c(0.2, 0.2, 0, 0), c(0.3, 0.3, 0.01, 0)),
               dl("n-ref", c(0.3, 0.01, 0.01, 0.3), c(0, 0.2, 0, 0), c(0.05, 0.3, 0.01, 0)),
               dl("n-comp", c(0.01, 0.15, 0.01, 0.3), c(0.25, 0, 0.1, 0), c(0.3, 0.05, 0.1, 0)),
               dl("n-asm", c(0.2, 0.25, 0.01, 0.3), c(0, 0, 0, 0), c(0.1, 0.1, 0.01, 0)),
               dl("n-low", c(0.01, 0.01, 0.01, 0.2), c(0.005, 0.005, 0, 0), c(0.01, 0.01, 0.01, 0)))
  dol$strategic_compact_id <- "SC-X"; dol$allied <- dol$iso3 != "CCC"
  dol$world_share <- dol$position_share; dol$production_share <- NA; dol$export_rca <- 1.2; dol$world_import_share <- 0.1
  dol$best_host_rank <- NA; dol$capable_host <- FALSE; dol$momentum <- FALSE; dol$anchor_position <- FALSE; dol$specialist_position <- FALSE
  dol$node_function <- "components"
  flows <- data.frame(node_id = c("n-ref", "n-ref", "n-comp", "n-comp", "n-ext", "n-ext"), exporter_iso = c("AAA", "CCC", "BBB", "CCC", "CCC", "CCC"),
                      importer_iso = c("BBB", "BBB", "AAA", "AAA", "AAA", "BBB"), value = c(4e8, 6e8, 2e8, 8e8, 5e8, 5e8), stringsAsFactors = FALSE)
  ev <- data.frame(evidence_id = c("Y1", "Y2", "Y3"), item_type = c("project", "plurilateral", "policy"), name = c("Part plant", "Compact forum", "Ore export ban"),
                   institution = c("PartCo", "Govts", "CCC government"), countries = c("AAA|BBB", "AAA|BBB|DDD", "AAA|CCC"),
                   status = c("operating", "in_force", "in_force"), publication_date = c("2026-01-01", "2025-10-01", "2025-04-01"), access_date = "2026-11-13",
                   technology = "t1", compact = "SC-X", stage = c("Components", "cross_stage", "Extraction"), aib_node = c("n-comp", "", "n-ext"),
                   instrument = c("fdi", "dialogue", "export_control"), amount_usd = "", source = "test", url = "https://example.org",
                   claim = c("A part plant operates.", "A forum is in force.", "An export control is in force."), verification = c("partial", "verified", "verified"),
                   research_pass = "test", basis = "external", stringsAsFactors = FALSE)
  mech <- data.frame(mechanism_id = c("P1", "B1", "B2", "U1"), pair = c("AAA|BBB|DDD", "AAA|BBB", "AAA|BBB|YYY|XXX", "AAA|BBB|DDD"),
                     name = c("Compact forum", "Bilateral fund", "Bundle of bilateral MOUs", "Minerals forum"),
                     mechanism_type = c("plurilateral", "finance_cooperation", "bilateral_bundle", "plurilateral"), status = "in_force",
                     evidence_ids = "Y2", scope_compacts = c("SC-X", "SC-X", "SC-X", "SC-UP"), cross_sector = "FALSE", has_pipeline = "partial",
                     has_finance = "TRUE", has_kpis = "FALSE", has_review = "FALSE", reciprocal = "FALSE", sunset = "", assessed_on = "2026-11-13", notes = "",
                     stringsAsFactors = FALSE)
  inst <- utils::read.csv(file.path(pm_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)
  croles <- data.frame(strategic_compact_id = "SC-X", iso3 = c("AAA", "BBB"), compact_role = c("Demand anchor", "Specialized technology supplier"),
                       lead_node = c("n-ref", "n-comp"), compact_relevance = c(0.2, 0.2), stringsAsFactors = FALSE)
  list(sp = sp, nodes = nodes, spine = spine, nclass = nclass, dol = dol, flows = flows, ev = ev, mech = mech, inst = inst, croles = croles)
}

run_compact <- function(inp = cp_inputs(), cfg = cp_cfg()) {
  frame <- pm_node_frame(inp$nodes, inp$spine, inp$nclass, inp$dol, cfg$inputs$coercive_suppliers)
  ccfg <- list(id = "SC-X", title = "Compact X", source_compacts = "SC-X")
  fc <- pm_compact_frame(frame, "SC-X", ccfg)
  nm <- c(AAA = "Alpha", BBB = "Beta", DDD = "Delta", CCC = "Gamma")
  members <- pm_compact_members(fc, inp$dol, inp$croles, c("AAA", "BBB"), nm, cfg)
  prof <- pm_compact_profiles(inp$sp, fc, inp$dol, members$iso3, cfg)
  cm <- pm_compact_mechanisms(inp$mech, members$iso3, "SC-X", "SC-X", upstream_scope = "SC-UP")
  na <- pm_compact_assess(prof$nodes, prof$cells, fc, inp$flows, members$iso3, cfg, inp$ev, cm, inp$inst, NULL, members$iso3)
  lanes <- pm_compact_lanes(na, prof$nodes, inp$flows)
  gov <- pm_compact_governance(cm, members$iso3[members$core], cfg)
  trade <- pm_compact_trade(na, cfg, nm)
  inv <- pm_compact_investment(na, cfg, inp$ev, inp$inst, nm)
  deals <- pm_compact_deals(na, inp$inst, data.frame(iso3 = "AAA", institution = "X", abbreviation = "XA", instrument_families = "demand|trade|finance|investment|standards|technology|security|development"),
                            inp$ev, cfg, nm)
  nonp <- pm_compact_non_priorities(na, gov, cfg, nm, inp$ev)
  ids <- pm_compact_evidence_ids(fc, inp$ev, members$iso3, "SC-X")
  memo <- pm_compact_memo_sections(na = na, prof = prof$nodes, cells = prof$cells, members = members, lanes = lanes, trade_tbl = trade, inv_tbl = inv,
                                   third = data.frame(), deals = deals, gov = gov, nonp = nonp, ev = inp$ev, ev_ids = ids,
                                   agreements = data.frame(iso3 = "AAA", agreement = "G7"), aib_meta = list(mission_statement = "Build X."),
                                   ccfg = ccfg, cfg = cfg, names = nm, parties = members$iso3)
  reg <- pm_compact_evidence_register(memo$recommendations, na, inp$ev, "SC-X")
  list(na = na, members = members, lanes = lanes, gov = gov, cm = cm, trade = trade, nonp = nonp, memo = memo, reg = reg)
}

test_that("a bloc is one economy: intra-bloc trade leaves the flows and the world shares are re-based", {
  fl <- data.frame(node_id = "n1", exporter_iso = c("EE1", "EE1", "EE2", "AAA", "CCC"), importer_iso = c("EE2", "AAA", "BBB", "EE1", "EE2"),
                   value = c(50, 20, 10, 15, 5), value_hist = 1, stringsAsFactors = FALSE)
  m <- pm_bloc_flows(fl, c("EE1", "EE2"), "EEE")
  expect_false(any(m$exporter_iso == "EEE" & m$importer_iso == "EEE"))
  expect_equal(sum(m$value), 50)
  expect_equal(sum(m$value[m$exporter_iso == "EEE"]), 30)
  dol <- data.frame(node_id = "n1", iso3 = c("EE1", "EE2", "AAA", "BBB", "CCC"), allied = c(TRUE, TRUE, TRUE, TRUE, FALSE),
                    position_share = c(0.7, 0.1, 0.15, 0, 0.05), world_share = c(0.7, 0.1, 0.15, 0, 0.05), production_share = c(0.2, 0.1, NA, NA, NA),
                    export_rca = c(2, 1, 1, NA, 1), scope_demand_share = c(0.1, 0.6, 0.2, 0.1, 0), net_import_share = 0, world_import_share = 0,
                    best_host_rank = c(2, NA, 1, NA, NA), capable_host = c(TRUE, FALSE, TRUE, FALSE, FALSE), momentum = FALSE,
                    anchor_position = FALSE, specialist_position = FALSE, node_function = "processing", stringsAsFactors = FALSE)
  rca <- data.frame(node_id = "n1", exporter_iso = c("EE1", "EE2"), exports = c(70, 10), export_rca = c(2, 1))
  prod <- data.frame(node_id = "n1", iso3 = c("EE1", "EE2"), production_share = c(0.2, 0.1))
  out <- pm_bloc_dol(dol, fl, prod, rca, c("EE1", "EE2"), "EEE", list(position_min = 0.1, spec_rca = 1.5))
  expect_false(any(out$iso3 %in% c("EE1", "EE2")))
  b <- out[out$iso3 == "EEE", ]
  expect_equal(b$world_share, 30 / 50)
  expect_equal(b$production_share, 0.3)
  expect_equal(b$bloc_lead_member, "EE1")
  expect_equal(b$best_host_rank, 2)
  expect_equal(sum(out$world_share), 1)
  expect_true(b$anchor_position)
  ev <- pm_bloc_evidence(data.frame(countries = c("AAA|EE2", "AAA|BBB"), stringsAsFactors = FALSE), c("EE1", "EE2"), "EEE")
  expect_identical(ev$countries, c("AAA|EE2|EEE", "AAA|BBB"))
})

test_that("a bloc's cell row takes its strongest member's position and its extra-bloc import exposure", {
  sp <- cp_sp(c("EE1", "EE2", "AAA"), list(list(iso = "EE1", stage = "Refining", cp_contender_pct = 0.9, cp_high = TRUE, cp_is_contender = TRUE, supply_chain_need = 0.2),
                                             list(iso = "EE2", stage = "Refining", supply_chain_need = 0.8)))
  imp <- data.frame(reporter_iso = c("EE1", "EE2", "EE2", "EE1"), partner_iso = c("CCC", "AAA", "EE1", "EE2"), tech = "T1", supply_chain = "Refining",
                    imports_usd = c(1e8, 3e8, 5e8, 1e9), stringsAsFactors = FALSE)
  out <- pm_bloc_sp(sp, imp, c("EE1", "EE2"), "EEE", "Bloc", "CCC", "CCC")
  r <- out[out$iso3 == "EEE" & out$supply_chain == "Refining", ]
  expect_equal(nrow(r), 1L)
  expect_true(r$cp_high)
  expect_equal(r$cp_contender_pct, 0.9)
  expect_equal(r$bloc_lead_member, "EE1")
  expect_equal(r$imports_usd, 4e8)                      # intra-bloc imports are not imports
  expect_equal(r$coercive_import_share, 0.25)
  expect_equal(r$supply_chain_need, (0.2 * 1e8 + 0.8 * 3e8) / 4e8)
  bi <- pm_bloc_imports(imp, c("EE1", "EE2"), "EEE")
  expect_false(any(bi$reporter_iso == "EEE" & bi$partner_iso == "EEE"))
})

test_that("compact states follow the ordered rules and strength alone is never complementarity", {
  r <- run_compact()
  st <- setNames(r$na$relationship_state, r$na$node_id)
  expect_equal(unname(st["n-ext"]), "shared_vulnerability")
  expect_equal(unname(st["n-ref"]), "member_supply")
  expect_equal(unname(st["n-comp"]), "capacity_constrained_trusted_supply")
  expect_equal(unname(st["n-asm"]), "member_competition")
  expect_equal(unname(st["n-low"]), "low_strategic_case")
  expect_equal(r$na$treatment[r$na$node_id == "n-comp"], "BUILD")
  comp <- r$na[r$na$relationship_state == "member_competition", ]
  expect_true(all(comp$n_lanes == 0 & comp$n_strong >= 2))
})

test_that("a lane needs a supplier that outranks its buyer, and lanes carry the 2024 flow", {
  r <- run_compact()
  expect_gt(nrow(r$lanes), 0)
  expect_true(all(r$lanes$supplier != r$lanes$buyer))
  l <- r$lanes[r$lanes$node_id == "n-ref", ]
  expect_equal(l$supplier, "AAA"); expect_equal(l$buyer, "BBB")
  expect_equal(l$share_of_buyer_imports, 0.4)
})

test_that("compact bets need importance, evidence and additionality; the export control raises urgency", {
  r <- run_compact(); cfg <- cp_cfg()
  bets <- r$na[r$na$priority_tier == 1, ]
  expect_true(all(bets$relationship_state %in% PM_COMPACT_ACTIVE))
  expect_true(all(bets$strategic_importance >= cfg$thresholds$need_high & !bets$low_evidence & bets$additionality_label != "low"))
  expect_true(r$na$active_restriction[r$na$node_id == "n-ext"])
  expect_gte(r$na$urgency[r$na$node_id == "n-ext"], 0.8)
  fam <- setNames(r$na$main_instrument_family, r$na$node_id)
  for (i in which(!is.na(r$na$main_instrument)))
    expect_true(fam[[i]] %in% unlist(cfg$effective_families[[r$na$rule_state[i]]]))
})

test_that("governance picks a plurilateral host, never a bundle of bilateral MOUs, and flags partial scope", {
  r <- run_compact()
  expect_false(r$cm$plurilateral[r$cm$mechanism_id == "B2"])
  expect_true(r$cm$partial_scope[r$cm$mechanism_id == "U1"])
  expect_equal(r$gov$option, "upgrade")
  expect_equal(r$gov$host_mechanism, "P1")
  expect_true(any(r$nonp$kind == "institution"))
})

test_that("every compact recommendation has an evidence trail and the memo prints in the brief format", {
  r <- run_compact(); cfg <- cp_cfg()
  recs <- r$memo$recommendations
  expect_true(all(recs$rec_id[!is.na(recs$unit_id)] %in% r$reg$claim_id))
  expect_true(all(r$reg$basis %in% c("osi", "external")))
  heads <- grep("^## ", r$memo$markdown, value = TRUE)
  expect_equal(length(heads), 12L)
  doc <- pm_document(r$memo$markdown, cp_inputs()$ev, r$memo$boxes, list(title = "Compact X"), utils::modifyList(cfg$document, cfg$compact_document),
                     list(pair_id = "SC-X", as_of = "2026-10-30", as_of_label = "30 October 2026", evidence_access = "2026-11-13", generated_at = "2026-11-13T00:00:00Z"), "OSI tables.")
  expect_equal(doc$format, "brief")
  expect_equal(doc$doc_kind, "Compact memo")
  expect_equal(doc$sections[[1]]$blocks[[1]]$type, "choices")
  r2 <- run_compact()
  expect_identical(r$memo$markdown[-3], r2$memo$markdown[-3])
})
