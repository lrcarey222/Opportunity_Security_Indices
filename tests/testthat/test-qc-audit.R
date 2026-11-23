# Guards the end-to-end case audit builder (R/qc/qc_audit.R; docs/QC_SYSTEM_DESIGN.md 6.4): all ten numbered
# sections, every link with an allowed status, links the data cannot support marked "not reproducible from
# shipped/pipeline data" (never dropped), recomputed links that disagree marked differs, the shipped-bundle
# comparison, the need-family percentile rule (immaterial importers ranked as 0), the pathway re-threshold, and
# labels read from the canonical TypeScript constants rather than re-typed.

qa_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/industrial_opportunity.R",
            "R/strategic_position/strategic_value.R", "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R",
            "R/qc/qc_audit.R")) {
  source(file.path(qa_root(), f))
}
qa_cfg <- function() yaml::read_yaml(file.path(qa_root(), "config", "strategic_position.yml"))
qa_labels <- function() qc_audit_labels(readLines(file.path(qa_root(), "dashboard", "src", "content", "vocabulary.ts"), warn = FALSE, encoding = "UTF-8"),
                                        readLines(file.path(qa_root(), "dashboard", "src", "posture", "grammar.ts"), warn = FALSE, encoding = "UTF-8"))

# Six importers in one producing cell. AAA is the audited cell; FFF is an immaterial importer.
qa_peers <- function() {
  data.frame(iso3 = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"),
             import_hhi = c(0.6, 0.2, 0.3, 0.4, 0.5, 0.9),
             china_share = c(0.7, 0.1, 0.005, 0.3, 0.2, 0.9),
             opposite_bloc_share = c(0.8, 0.1, 0.0, 0.3, 0.2, 0.9),
             sn_import_material = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
             supply_chain_need_score = c(0.7, 0.2, 0.3, 0.4, 0.5, 0.1),
             competitive_position_score = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
             home_share = c(0.06, 0.05, 0.04, 0.03, 0.02, 0.01), home_share_exposure = NA_real_, stringsAsFactors = FALSE)
}
qa_row <- function(...) {
  p <- qa_peers()
  imm <- p$sn_import_material %in% FALSE
  pct <- function(v) { v <- ifelse(imm, 0, v); x <- sp_midrank_pct(v); x[imm] <- 0; x }
  fl <- function(v) ifelse(v < 0.01, 0, v)
  conc <- pct(p$import_hhi)[1]
  risky <- mean(c(pct(fl(p$china_share))[1], pct(fl(p$opposite_bloc_share))[1]))
  base <- list(Country = "Testland", iso3 = "AAA", tech = "Batteries", supply_chain = "Components", strategic_choice = "preserve",
               strategic_posture = "compete", compete_mode = "preserve", engine_posture = "compete", posture_variant = "compete_preserve",
               sn_import_dependence = 0.5, sn_supplier_concentration = conc, sn_risky_supplier_exposure = risky, sn_trusted_alternatives = 0.4,
               sn_trusted_in_score = TRUE, sn_self_supplied = FALSE, sn_import_material = TRUE, import_hhi = 0.6, china_share = 0.7,
               opposite_bloc_share = 0.8, imports_usd = 5e6, home_share = 0.06,
               supply_chain_need_score = 0.7, supply_chain_need_uncapped = sp_midrank_pct(p$supply_chain_need_score)[1],
               supply_chain_need = sp_midrank_pct(p$supply_chain_need_score)[1], supply_chain_need_cap = NA_character_,
               siv_market = 0.9, siv_commons = 0.5, siv_option = 0, siv_score = (0.9 + 0.5 + 0) / 3, strategic_industrial_value = 0.8,
               siv_cap = NA_character_, industrial_opportunity = 0.9, io_node_prize = 0.9, io_home_market = 0.3, io_home_market_scored = TRUE,
               large_yard_case = FALSE, large_yard_n = 2, large_yard_signals = "passive;substitutable;diversified",
               competitive_position_score = 0.9, cp_export_position = 0.8, cp_manufacturing_capacity = 0.6, # mean 0.7: the stored 0.9 differs
               cp_contender_pct = 0.9, cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "strong",
               pathway_credible = "yes", pw_direct_share = 0.06, pw_export_share = 0.1, pw_feasibility_pct = 0.8, pw_learning = "strong",
               decision_trace = '[{"step":"need","test":"supply_chain_need","value":0.917,"threshold":0.67,"result":"high"},{"step":"prize","test":"strategic_industrial_value","value":0.8,"threshold":0.6,"result":"high"},{"step":"position","test":"cp_contender_pct","value":0.9,"threshold":0.75,"result":"high"}]',
               yard = "medium", yard_rollup = "medium", decision_step = "triage", policy_alignment = "aligned", choice_reason = "a reason")
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}
qa_manifest <- function() list(inputs = list(
  list(id = "src_a", source_name = "Source A", vintage = "2024", licence = "CC BY", url = "https://example.org/a",
       required_by = list("scripts/15_build_partner_themes.R")),
  list(id = "src_b", source_name = "Source B", required_by = list("scripts/15_build_partner_themes.R"))))

test_that("an audit has the ten numbered sections and every link carries an allowed status", {
  a <- qc_audit_cell(qa_row(), peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels(),
                     meta = list(generated_at = "2026-11-20T00:00:00Z", pipeline_release = "pipeline-test"))
  expect_equal(vapply(a$sections, function(s) s$n, numeric(1)), 1:10)
  expect_equal(vapply(a$sections, function(s) s$id, character(1)), names(QC_AUDIT_SECTIONS))
  links <- do.call(rbind, lapply(a$sections, function(s) s$links))
  expect_true(all(links$status %in% QC_AUDIT_STATUSES))
  md <- qc_audit_markdown(a)
  for (i in 1:10) expect_true(any(startsWith(md, paste0("## ", i, ". "))), info = paste("section", i))
  expect_true(any(md == "## Shipped bundle comparison"))
  expect_true(any(md == "## Gaps: links not reproduced"))
})

test_that("links the data cannot support are marked not reproducible, never dropped", {
  a <- qc_audit_cell(qa_row(), peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels())
  links <- do.call(rbind, lapply(a$sections, function(s) cbind(n = s$n, s$links)))
  nr <- links[links$status == QC_AUDIT_NOT_REPRO, ]
  expect_gt(nrow(nr), 5)
  # no import-exposure source rows were given: said so explicitly
  expect_true(any(nr$n == 2 & grepl("import exposure row", nr$link)))
  # the pathway signals themselves need side tables scripts/26 does not save
  expect_true(any(grepl("pw_\\* signals", nr$link)))
  # the raw input without vintage or licence is a provenance gap
  pg <- links[links$status == "provenance_gap", ]
  expect_equal(nrow(pg), 1L)
  expect_match(pg$detail, "src_b")
  expect_equal(a$summary$not_reproducible, nrow(nr))
  expect_true(any(grepl(QC_AUDIT_NOT_REPRO, a$gaps, fixed = TRUE)))
  # no shipped bundle given: the comparison says so
  expect_false(a$bundle_comparison$available)
  expect_true(any(grepl("no shipped bundle", a$gaps)))
})

test_that("recomputed links reproduce or differ: need families, need percentile, the CP score", {
  a <- qc_audit_cell(qa_row(), peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels())
  links <- do.call(rbind, lapply(a$sections, function(s) s$links))
  st <- function(pat) links$status[grepl(pat, links$link)]
  # the immaterial importer FFF is ranked as 0 before the percentile (strategic_need.R); a naive rank of its
  # 0.9 HHI would give AAA 0.75 instead of 0.917
  expect_equal(qa_row()$sn_supplier_concentration, 5.5 / 6)
  expect_equal(sp_midrank_pct(qa_peers()$import_hhi)[1], 4.5 / 6)
  expect_equal(st("^sn_supplier_concentration"), "reproduced")
  expect_equal(st("^sn_risky_supplier_exposure"), "reproduced")
  expect_equal(st("^need percentile"), "reproduced")
  expect_equal(st("^CP score = mean"), "differs")
  expect_equal(st("^pathway_credible"), "reproduced")
  expect_equal(st("^choice = canonical rule rerun"), "reproduced")
  # the stored trace is checked against the rule's steps
  expect_equal(st("^trace need: result"), "reproduced")
})

test_that("the pathway re-threshold follows the anchor rule", {
  cfg <- qa_cfg()$decision_framework$pathway
  r <- qa_row()
  expect_equal(qc_audit_pathway_anchor(r, cfg)$credible, "yes")
  # an anchor alone, with another family observed but not fired: not corroborated
  r2 <- qa_row(pw_export_share = NA_real_, pw_feasibility_pct = 0.1, pw_learning = "weak")
  expect_equal(qc_audit_pathway_anchor(r2, cfg)$credible, "no")
  # Extraction without a resource base: no
  r3 <- qa_row(supply_chain = "Extraction", resource_base = FALSE)
  expect_equal(qc_audit_pathway_anchor(r3, cfg)$credible, "no")
  expect_true(is.na(qc_audit_pathway_anchor(qa_row(supply_chain = "Deployment"), cfg)$credible))
})

test_that("the shipped bundle comparison lists every difference", {
  bundle <- list(detail = list(tech = "Batteries", stage = "Components", posture = "compete", variant = "compete_preserve", mode = "preserve",
                               need = 0.5, cp = 0.9, cp_families = list(export_position = 0.8, manufacturing_capacity = 0.6),
                               import_hhi = round(qa_row()$sn_supplier_concentration, 3)),
                 core = list(p = "C", m = "preserve", yd = "M"), created = "2026-11-17T20:33:31Z", git_commit = "abc1234",
                 detail_file = "posture/posture-detail-AAA.json", core_file = "posture/posture-core.json")
  a <- qc_audit_cell(qa_row(), peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels(), bundle = bundle)
  cmp <- a$bundle_comparison
  expect_true(cmp$available)
  f <- setNames(cmp$fields$status, cmp$fields$field)
  expect_equal(f[["choice (core)"]], "same")
  expect_equal(f[["need"]], "differs")
  expect_equal(f[["cp"]], "same")
  expect_equal(f[["cp_families.export_position"]], "same")
  # in the pipeline but not shipped
  expect_equal(f[["china_share"]], "missing_in_bundle")
  expect_equal(cmp$n_differences, sum(cmp$fields$status != "same"))
  expect_equal(a$inputs$bundle_git_commit, "abc1234")
})

test_that("labels come from the canonical TypeScript constants", {
  lb <- qa_labels()
  expect_equal(unname(lb$choice_name[c("secure", "preserve", "catch_up", "partner", "accelerate", "watch")]),
               c("Secure", "Preserve", "Catch Up", "Partner", "Accelerate", "Watch"))
  expect_equal(qc_audit_choice_label("not_assessed", lb), "Not assessed")
  a <- qc_audit_cell(qa_row(), peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = lb)
  rec <- a$sections[[10]]$items
  expect_equal(rec$value[rec$item == "choice_label"], "Preserve")
  expect_equal(rec$value[rec$item == "yard_and_fence"], unname(lb$yard_and_fence["medium"]))
  expect_equal(qc_audit_stem("JPN", "Electric Vehicles", "Refining"), "JPN_Electric-Vehicles_Refining")
})

test_that("self-trade partner rows are reported and left out of the HHI and total reconciliation", {
  ie <- data.frame(importer_iso = "AAA", tech = "Batteries", supply_chain = "Components", imports_usd = 100, import_hhi = 0.52,
                   china_share = 0.7, opposite_bloc_share = 0.8, top_supplier_iso3 = "BBB", top_supplier_share = 0.6,
                   imports_to_gdp = 0.01, stringsAsFactors = FALSE)
  # partners without self: BBB 60, CCC 40 -> HHI 0.36 + 0.16 = 0.52; the self row (AAA 50) would make AAA second
  ip <- data.frame(partner_iso = c("BBB", "AAA", "CCC"), imports_usd = c(60, 50, 40), stringsAsFactors = FALSE)
  r <- qa_row(imports_usd = 100, import_hhi = 0.52, top_supplier_iso3 = "BBB", top_supplier_share = 0.6, imports_to_gdp = 0.01)
  a <- qc_audit_cell(r, peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels(),
                     sources = list(import_exposure = ie, import_partners = ip))
  l <- a$sections[[2]]$links
  st <- function(pat) l$status[grepl(pat, l$link)]
  expect_equal(st("^partner rows: self-trade"), "differs")
  expect_match(l$detail[grepl("^partner rows: self-trade", l$link)], "sp_partner_recs")
  expect_equal(st("^base import HHI <- partner rows without self-trade"), "reproduced")
  expect_equal(st("^base imports_usd <- partner rows without self-trade"), "reproduced")
  expect_equal(st("^base top supplier <- partner rows"), "reproduced")
  expect_equal(a$sections[[2]]$tables$import_partners_self$partner_iso, "AAA")
  # without a self row the link reproduces
  a2 <- qc_audit_cell(r, peers = qa_peers(), cfg = qa_cfg(), manifest = qa_manifest(), labels = qa_labels(),
                      sources = list(import_exposure = ie, import_partners = ip[ip$partner_iso != "AAA", ]))
  l2 <- a2$sections[[2]]$links
  expect_equal(l2$status[grepl("^partner rows: self-trade", l2$link)], "reproduced")
})
