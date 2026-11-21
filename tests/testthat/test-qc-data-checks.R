# Guards the pipeline-level data QC checks (R/qc/qc_core.R, R/qc/qc_data_checks.R; docs/QC_SYSTEM_DESIGN.md
# 6.2) on small synthetic tables: schema, missingness, numeric validity (negatives, NaN / Inf, malformed strings,
# shares > 1, stacked share bases), trade consistency (duplicate dyads, mirror vs reported, units, the HS6
# crosswalk), classification logic (an irreproducible choice is a fail with examples; invariants; knife-edges; the
# need-floor cap) and provenance (missing fields, licence conflicts, stale vintages, required_by).

qd_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/schema.R", "R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R",
            "R/strategic_position/industrial_opportunity.R", "R/strategic_position/strategic_value.R",
            "R/strategic_position/decision_engine.R", "R/qc/qc_core.R", "R/qc/qc_data_checks.R")) {
  source(file.path(qd_root(), f))
}
qd_cfg <- function() yaml::read_yaml(file.path(qd_root(), "config", "strategic_position.yml"))$decision_framework

# One cell of the strategic position table (a producing stage unless set).
qd_cell <- function(iso3, ...) {
  base <- list(Country = paste("Country", iso3), iso3 = iso3, tech = "Solar", supply_chain = "Components", engine_posture = "deploy",
               posture_exclusion = NA_character_, supply_chain_need = 0.9, supply_chain_need_uncapped = 0.9, supply_chain_need_score = 0.5,
               supply_chain_need_cap = NA_character_, strategic_industrial_value = 0.3, industrial_opportunity = 0.3, siv_cap = NA_character_,
               deployment_need = NA_real_, cp_contender_pct = 0.2, cp_contender_basis = "contenders", cp_is_contender = TRUE,
               t3_capability_lo = "moderate", pathway_credible = "no", large_yard_case = FALSE, large_yard_near = FALSE,
               competitive_position_score = 0.4, competitive_position_percentile = 0.4, home_share = 0.05, home_share_exposure = 0.05,
               top_supplier_iso3 = "CHN", top_supplier_share = 0.5, china_share = 0.5, import_hhi = 0.3, imports_usd = 1e8,
               trade_benchmark_share = 0.05, trade_benchmark_source = "comtrade", trade_benchmark_rca = 1.2, trade_benchmark_exports = 1e8,
               exposure_basis = "base")
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}
# A table whose stored choice, posture and modes come from the canonical rule (then tampered by a test).
qd_tbl <- function(...) {
  x <- do.call(rbind, list(...))
  r <- qc_choice_recompute(x, qd_cfg())
  x$strategic_choice <- r$qc_choice
  x$cp_high <- ifelse(x$supply_chain == "Deployment" | is.na(r$qc_choice), NA, r$qc_cp_high)
  x$strategic_posture <- unname(SP_CHOICE_VERB_POSTURE[x$strategic_choice])
  x$compete_mode <- ifelse(x$strategic_choice %in% SP_CHOICE_COMPETE_MODES, x$strategic_choice, NA_character_)
  x$deploy_mode <- ifelse(x$strategic_choice %in% SP_DEPLOY_MODES, x$strategic_choice, NA_character_)
  x
}
qd_row <- function(checks, id) checks[checks$check_id == id, , drop = FALSE]

test_that("qc_check builds a contract row and rejects unknown statuses", {
  r <- qc_check("x", "schema", "t", "warn", 3, "detail", c("a", "", NA, "b"))
  expect_named(r, QC_CHECK_COLS)
  expect_equal(r$examples, "a | b")
  expect_error(qc_check("x", "schema", "t", "bad"), "status")
  expect_equal(qc_status_worst(c("info", "fail", "warn")), "fail")
  expect_equal(qc_lab("x=", character()), character())
  b <- qc_bind(r, list(qc_check("y", "a", "t")), NULL)
  expect_equal(nrow(b), 2)
  expect_true(qc_has_fail(qc_bind(qc_check("z", "a", "t", "fail", 1))))
})

test_that("the schema check fails on a bad stage, a non-canonical choice, a duplicate key and a malformed ISO3", {
  x <- qd_tbl(qd_cell("AAA"), qd_cell("BBB"), qd_cell("CCC"))
  ok <- qc_schema_sp(x)
  expect_false(any(ok$status == "fail"))
  bad <- rbind(x, x[1, ])
  bad$supply_chain[2] <- "Midstream"
  bad$strategic_choice[3] <- "keep_open"
  bad$iso3[3] <- "cc1"
  s <- qc_schema_sp(bad)
  expect_equal(qd_row(s, "schema_stage_vocabulary")$status, "fail")
  expect_match(qd_row(s, "schema_stage_vocabulary")$examples, "Midstream")
  expect_equal(qd_row(s, "schema_choice_vocabulary")$status, "fail")
  expect_equal(qd_row(s, "schema_key_unique")$status, "fail")
  expect_equal(qd_row(s, "schema_iso3_format")$status, "fail")
  expect_match(qd_row(s, "schema_iso3_format")$examples, "cc1")
})

test_that("a non-tidy table is not run through validate_schema and legacy stages are flagged", {
  proj <- data.frame(project_id = 1:3, destination_iso3 = c("USA", "DEU", "usa"), tech = "Solar",
                     supply_chain = "Midstream", stages = c("Components|Assembly", "Assembly", "Midstream"))
  s <- qc_schema_theme(proj, "cm_fdi_projects_tbl", key = "project_id")
  expect_match(qd_row(s, "schema_theme_validate_schema")$detail, "does not apply")
  expect_equal(qd_row(s, "schema_theme_stage_vocabulary")$status, "warn")
  expect_equal(qd_row(s, "schema_theme_stage_vocabulary")$n, 3L)
  expect_equal(qd_row(s, "schema_theme_stage_vocabulary_stages")$n, 1L)
  expect_equal(qd_row(s, "schema_theme_iso3_format")$n, 1L)
})

test_that("missingness separates structural gaps from partial ones and flags a sudden increase", {
  cells <- c(lapply(c("AAA", "BBB", "CCC", "DDD"), function(i) qd_cell(i, supply_chain = "Recovery", cp_contender_pct = NA_real_)),
             lapply(c("AAA", "BBB", "CCC", "DDD"), function(i) qd_cell(i, supply_chain = "Refining",
                                                                         cp_contender_pct = if (i == "AAA") 0.5 else NA_real_)))
  x <- qd_tbl(cells[[1]], cells[[2]], cells[[3]], cells[[4]], cells[[5]], cells[[6]], cells[[7]], cells[[8]])
  m <- qc_missingness_sp(x)
  expect_equal(qd_row(m, "missing_cp_contender_pct_structural")$n, 1L)
  expect_match(qd_row(m, "missing_cp_contender_pct_structural")$examples, "Solar/Recovery")
  expect_equal(qd_row(m, "missing_cp_contender_pct")$status, "warn")
  prev <- x
  prev$cp_contender_pct[prev$supply_chain == "Refining"] <- 0.5
  j <- qc_missingness_sp(x, previous = prev)
  expect_equal(qd_row(j, "missing_increase_vs_previous")$status, "warn")
  expect_true(qd_row(j, "missing_increase_vs_previous")$needs_human_explanation)
})

test_that("numeric validity catches negatives, NaN / Inf, malformed strings, out-of-unit shares and stacked share bases", {
  x <- qd_tbl(qd_cell("AAA"), qd_cell("BBB"), qd_cell("CCC"))
  ok <- qc_numeric_validity(x, "sp")
  expect_false(any(ok$status %in% c("warn", "fail")))
  bad <- x
  bad$imports_usd[1] <- -5
  bad$trade_benchmark_rca[2] <- Inf
  bad$china_share[3] <- 1.4
  bad$supply_chain_need[2] <- NaN
  bad$import_hhi[1] <- 0
  bad$trade_benchmark_share <- c(0.6, 0.5, 0.3)
  bad$trade_benchmark_source <- c("comtrade", "comtrade", "atlas_fallback")
  n <- qc_numeric_validity(bad, "sp")
  expect_equal(qd_row(n, "numeric_negative_imports_usd")$status, "warn")
  expect_equal(qd_row(n, "numeric_inf_nan")$status, "fail")  # NaN in a decision input
  expect_equal(qd_row(n, "numeric_division_by_zero_trade_benchmark_rca")$n, 1L)
  expect_equal(qd_row(n, "numeric_out_of_unit_china_share")$n, 1L)
  expect_equal(qd_row(n, "numeric_hhi_range_import_hhi")$n, 1L)
  s <- qd_row(n, "numeric_share_sum_trade_benchmark_share")
  expect_equal(s$status, "warn")
  expect_match(s$examples, "atlas_fallback 0.300")
  txt <- x
  txt$imports_usd <- c("1e8", "12,5", "n/a")
  t2 <- qc_numeric_validity(txt, "sp")
  expect_equal(qd_row(t2, "numeric_malformed_strings")$n, 2L)
  expect_equal(qc_malformed_numeric(c("1", " 2.5", "NA", "abc", NA)), "abc")
})

test_that("theme numeric validity flags NaN index rows, negatives and world shares over 1", {
  th <- data.frame(iso3c = c("AAA", "BBB", "CCC", "AAA"), tech = "Copper", supply_chain = "Extraction",
                   variable = c("Copper Production", "Copper Production", "Overall Reserves Index", "Copper Production"),
                   data_type = c("raw", "raw", "index", "raw"), value = c(5, -1, NaN, 2), Year = 2024L)
  n <- qc_numeric_theme(th, "theme")
  expect_equal(qd_row(n, "numeric_theme_negative")$n, 1L)
  expect_equal(qd_row(n, "numeric_theme_inf_nan")$n, 1L)
  expect_match(qd_row(n, "numeric_theme_inf_nan")$detail, "Overall Reserves Index: 1")
  ms <- data.frame(iso3 = c("AAA", "BBB", "WLD"), tech = "Copper", supply_chain = "Extraction", measure = "usgs_reserves",
                   mineral = "Copper", year = 2024L, value = 1, world_share = c(0.7, 0.6, 1), is_aggregate = c(FALSE, FALSE, TRUE),
                   unit = c("Thousand tonnes", "Kilograms", "Thousand tonnes"))
  expect_equal(qd_row(qc_numeric_theme(ms, "mss"), "numeric_theme_share_sum")$n, 1L)
  u <- qc_units(ms, "mss")
  expect_equal(u$n, 1L)
  ms2 <- ms
  ms2$mineral <- c("Copper", "Oil", "Copper")
  ms2$unit <- c("Thousand tonnes", "kb/d", "Thousand tonnes")
  expect_equal(qc_units(ms2, "mss")$n, 0L)  # one unit per measure x mineral is fine
})

test_that("importer shares: China above the top single supplier is flagged with its basis", {
  x <- qd_tbl(qd_cell("AAA", top_supplier_share = 0.4, china_share = 0.55, exposure_basis = "blend"), qd_cell("BBB"))
  s <- qc_importer_shares(x)
  expect_equal(qd_row(s, "trade_shares_china_share_le_top")$n, 1L)
  expect_match(qd_row(s, "trade_shares_china_share_le_top")$detail, "blend=1")
  expect_equal(qd_row(s, "trade_shares_china_top_agree")$n, 1L)
})

test_that("dyad checks catch duplicates, negatives and mirror discrepancies on the same window only", {
  d <- data.frame(reporter_iso = c("AAA", "AAA", "BBB", "AAA", "CCC", "DDD"),
                  partner_iso = c("BBB", "BBB", "AAA", "CCC", "AAA", "AAA"),
                  flow_direction = c("export", "export", "import", "export", "import", "import"),
                  cmd_code = "850440", primary_value = c(1e7, 1e7, 1e5 * 1.1, 2e6, 2.1e6, -1),
                  period = 9999L, window_end = c(202604L, 202604L, 202604L, 202604L, 202604L, 202604L), months_used = 12L)
  r <- qc_trade_dyads(d)
  expect_equal(qd_row(r, "trade_duplicate_dyads")$status, "fail")
  expect_equal(qd_row(r, "trade_negative_values")$n, 1L)
  m <- qd_row(r, "trade_mirror_vs_reported")
  expect_match(m$detail, "1 of 2")
  expect_match(m$examples, "reporter_iso=AAA; partner_iso=BBB")
  d2 <- d[-2, ]
  d2$window_end[3] <- 202512L  # CCC's import window differs from AAA's export window: not compared
  expect_match(qd_row(qc_trade_dyads(d2), "trade_mirror_vs_reported")$detail, "of 1 ")
})

test_that("the HS6 crosswalk counts multi-cell codes by design and flags duplicates and HS 281290 as cabling", {
  m <- data.frame(tech = c("Wind", "Solar", "Solar", "Grid", "Grid"),
                  sub_sector = c("Array Cables & Collection Systems", "Solar Cabling & Connectors", "Solar Cabling & Connectors", "Transformers", "Transformers"),
                  hs6 = c("281290", "281290", "281290", "850440", "850440"), stage = c("Refining", "Refining", "Refining", "Components", "Components"),
                  excluded = FALSE)
  r <- qc_hs6_crosswalk(m)
  expect_equal(qd_row(r, "hs6_duplicate_rows")$n, 2L)
  expect_equal(qd_row(r, "hs6_codes_in_several_cells")$status, "info")
  expect_equal(qd_row(r, "hs6_codes_in_several_cells")$n, 1L)
  expect_equal(qd_row(r, "hs6_known_misfiling_281290")$status, "warn")
  expect_equal(qd_row(r, "hs6_known_misfiling_281290")$n, 3L)
})

test_that("an irreproducible stored choice is a fail with examples; a reproducible table passes", {
  x <- qd_tbl(qd_cell("AAA"), qd_cell("BBB", strategic_industrial_value = 0.8, cp_contender_pct = 0.9),
              qd_cell("CCC", supply_chain = "Deployment", deployment_need = 0.7, supply_chain_need = NA_real_),
              qd_cell("DDD", supply_chain_need = NA_real_), qd_cell("EEE", posture_exclusion = "entrepot"))
  expect_equal(x$strategic_choice, c("partner", "preserve", "accelerate", "not_assessed", NA))
  ok <- qc_choice_reproduction(x, qd_cfg())
  expect_equal(qd_row(ok, "class_choice_reproduced")$status, "info")
  expect_equal(qd_row(ok, "class_choice_reproduced")$n, 0L)
  bad <- x
  bad$strategic_choice[1] <- "watch"
  r <- qc_choice_reproduction(bad, qd_cfg())
  expect_equal(qd_row(r, "class_choice_reproduced")$status, "fail")
  expect_match(qd_row(r, "class_choice_reproduced")$examples, "iso3=AAA.*strategic_choice=watch; qc_choice=partner")
  expect_match(qd_row(r, "class_choice_irreproducible_transitions")$examples, "watch -> partner: 1")
  expect_true(qc_has_fail(r))
})

test_that("Competitive Position among contenders is recomputed and a material difference fails", {
  cells <- lapply(sprintf("C%02d", 1:6), function(i) qd_cell(i, competitive_position_score = match(i, sprintf("C%02d", 1:6)) / 10,
                                                             home_share_exposure = 0.05, strategic_industrial_value = 0.8))
  x <- do.call(rbind, cells)
  x <- sp_cp_contender_pct(x[, setdiff(names(x), c("cp_contender_pct", "cp_contender_basis", "cp_is_contender"))], qd_cfg())
  x <- qd_tbl(x)
  ok <- qc_cp_contender_reproduction(x, qd_cfg())
  expect_equal(ok$status, "info")
  bad <- x
  bad$cp_contender_pct[1] <- 0.95  # the bottom contender stored as top quarter
  r <- qc_cp_contender_reproduction(bad, qd_cfg())
  expect_equal(r$status, "fail")
  expect_match(r$examples, "iso3=C01")
})

test_that("excluded rows counted as contenders are flagged", {
  x <- qd_tbl(qd_cell("HKG", posture_exclusion = "entrepot"), qd_cell("AAA"))
  r <- qc_excluded_contenders(x)
  expect_equal(r$status, "warn")
  expect_match(r$examples, "HKG")
})

test_that("posture invariants fail on a mode on the wrong posture and Compete at Deployment", {
  x <- qd_tbl(qd_cell("AAA"), qd_cell("BBB", supply_chain = "Deployment", deployment_need = 0.3, supply_chain_need = NA_real_))
  expect_false(any(qc_posture_invariants_sp(x)$status == "fail"))
  bad <- x
  bad$deploy_mode[1] <- "watch"
  bad$strategic_choice[2] <- "partner"
  bad$strategic_posture[2] <- "compete"
  bad$compete_mode[2] <- "partner"
  bad$deploy_mode[2] <- NA
  r <- qc_posture_invariants_sp(bad)
  expect_equal(qd_row(r, "class_invariant_compete_mode_alone")$n, 1L)
  expect_equal(qd_row(r, "class_invariant_no_compete_at_deployment")$n, 1L)
})

test_that("knife-edges: values exactly on a threshold, a node-constant prize and the need-floor cap are reported", {
  x <- qd_tbl(qd_cell("AAA", cp_contender_pct = 0.75, strategic_industrial_value = 0.62),
              qd_cell("BBB", supply_chain_need = 0.61, supply_chain_need_uncapped = 0.8, supply_chain_need_score = 0.02,
                      supply_chain_need_cap = "score_below_floor", strategic_industrial_value = 0.62),
              qd_cell("CCC", strategic_industrial_value = 0.62), qd_cell("DDD", strategic_industrial_value = 0.62),
              qd_cell("EEE", strategic_industrial_value = 0.62))
  expect_equal(x$strategic_choice[1], "preserve")
  b <- qc_threshold_boundaries(x, qd_cfg())
  expect_equal(qd_row(b, "class_exactly_on_threshold_cp")$n, 1L)
  expect_true(qd_row(b, "class_exactly_on_threshold_cp")$needs_human_explanation)
  expect_equal(qd_row(b, "class_node_constant_prize_near_threshold")$n, 1L)
  cap <- qc_need_cap(x, qd_cfg())
  expect_equal(cap$n, 1L)
  expect_match(cap$detail, "uncapped percentile >= 0.67")
})

test_that("provenance: missing fields, stale vintages, required_by gaps and licence conflicts", {
  man <- list(
    list(id = "weo", source_type = "manual", path = "weo.xlsx", url = "https://www.iea.org/data/weo/", vintage = "WEO 2025",
         licence = "CC BY-NC-SA 4.0", required_by = list("scripts/10_build.R")),
    list(id = "old", source_type = "api", path = "old.csv", url = "https://example.org/old", vintage = "AR5 (2014)",
         licence = "CC BY 4.0", required_by = list("scripts/gone.R")),
    list(id = "bare", source_type = "manual", path = "bare.csv"),
    list(id = "derived_x", source_type = "derived", path = "x.csv")
  )
  dict <- list(list(id = "iea_weo", url = "https://iea.org/data/weo", licence = "attribution", attribution = "IEA (2025)", issues = ""),
               list(id = "iea_nze", url = "https://iea.org/nze", licence = "attribution", attribution = "CC BY-NC-SA 3.0 IGO", issues = ""))
  p <- qc_provenance(man, dict, repo_files = c("scripts/10_build.R"), as_of = as.Date("2026-11-20"))
  expect_equal(qd_row(p, "prov_missing_url")$n, 1L)
  expect_equal(qd_row(p, "prov_missing_licence")$examples, "bare")
  expect_match(qd_row(p, "prov_missing_licence")$detail, "1 derived/generated")
  expect_equal(qd_row(p, "prov_stale_vintage")$n, 1L)
  expect_match(qd_row(p, "prov_stale_vintage")$examples, "old")
  expect_equal(qd_row(p, "prov_required_by_missing")$examples, "old -> scripts/gone.R")
  lc <- qd_row(p, "prov_licence_conflict")
  expect_equal(lc$n, 1L)
  expect_match(lc$examples, "iea_weo 'attribution' vs weo")
  expect_equal(qd_row(p, "prov_licence_self_conflict")$n, 1L)
  expect_equal(qc_licence_class(c("CC BY 4.0", "CC BY-NC-SA 4.0", "RESTRICTED", "CC0-1.0", NA)),
               c("attribution", "non_commercial", "restricted", "open", NA))
})

test_that("the memos stamp hashes the partnerships manifest LF-normalised (as the browser and Node do)", {
  lf <- "{\n  \"schema\": \"osi-partnership-memos\"\n}\n"
  expect_equal(qc_memos_manifest_text(gsub("\n", "\r\n", lf, fixed = TRUE)), lf)
  expect_equal(qc_memos_manifest_text(paste0("\ufeff", lf)), lf)
  expect_equal(qc_memos_manifest_text(lf), lf)
})
