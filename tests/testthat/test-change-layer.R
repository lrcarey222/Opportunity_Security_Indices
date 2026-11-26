# Change layer (docs/change-layer-architecture.md): posture vintages, deterministic change classification,
# methodology attribution, explanation, provenance and the Intelligence bridge. Seeded with constructed vintages.

cl_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/qc/qc_core.R", "R/qc/qc_drift.R", "R/change/posture_vintage.R", "R/change/posture_compare.R",
            "R/change/posture_explain.R", "R/change/posture_impacts.R", "R/change/change_export.R",
            "R/change/posture_bridge.R")) source(file.path(cl_root, f))
cl_cfg <- yaml::read_yaml(file.path(cl_root, "config/change_layer.yml"))

cl_cell <- function(iso3 = "USA", tech = "Electric Grid", stage = "Components", choice = "partner", need = 0.80,
                    prize = 0.65, cp = 0.60, market = 0.40, share = 0.030, confidence = "moderate", evidence = "medium") {
  tibble::tibble(iso3 = iso3, tech = tech, stage = stage, choice = choice, posture = posture_family_of(choice),
                 variant = NA_character_, need_type = ifelse(stage == "Deployment", "deployment", "supply_chain"),
                 need = need, prize = prize, cp = cp, market = market, share = share, confidence = confidence,
                 evidence = evidence, borderline = NA_character_, pathway = "yes", partner1 = "JPN", deploy_case = NA_character_)
}
# a background cell that never changes, so every pair has an unchanged cell
cl_bg <- function() cl_cell(iso3 = "JPN", tech = "Solar", stage = "Assembly", choice = "watch")
cl_v <- function(...) dplyr::bind_rows(cl_cell(...), cl_bg())

cl_header <- function(id, created, method = "m-aaaa", data = "d-1111", unverified = FALSE, data_verified = TRUE,
                      stages = c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")) {
  list(vintage_id = id, created = created, method = list(id = method, unverified = unverified, label = "test"),
       data = list(id = data, verified = data_verified), bars = cl_cfg$thresholds$score$default_bars,
       techs = list("Electric Grid", "Solar"), stages = as.list(stages))
}
H1 <- cl_header("v1", "2026-11-01T00:00:00Z")
H2 <- cl_header("v2", "2026-11-21T00:00:00Z", data = "d-2222")
H2m <- cl_header("v2m", "2026-11-21T00:00:00Z", method = "m-bbbb")   # method changed, same verified data
cl_cmp <- function(prev, cur, hp = H1, hc = H2, ...) posture_compare(prev, cur, hp, hc, cl_cfg, ...)
cl_usa <- function(res) res$changes[res$changes$iso3 == "USA", ]

test_that("identical vintages give no meaningful change", {
  r <- cl_cmp(cl_v(), cl_v())
  expect_equal(nrow(r$changes), 0)
  expect_equal(r$pair$n_unchanged, 2)
  expect_false(r$pair$method_changed)
})

test_that("sub-threshold noise is not a change", {
  r <- cl_cmp(cl_v(), cl_v(need = 0.82, cp = 0.63, share = 0.031, market = 0.42))
  expect_equal(nrow(r$changes), 0)
})

test_that("a data change without a conclusion change is data only", {
  r <- cl_cmp(cl_v(), cl_v(share = 0.045))
  x <- cl_usa(r)
  expect_equal(x$types, "data")
  expect_equal(x$severity, "minor")
  expect_equal(x$cur_choice, x$prev_choice)
  expect_equal(x$attribution, "real_world")
  expect_match(x$explanation, "Export share moved 3.0% → 4.5%", fixed = TRUE)
  expect_match(x$sources, "comtrade_energy_trade")
})

test_that("an evidence and confidence improvement is classified with direction", {
  r <- cl_cmp(cl_v(), cl_v(evidence = "high", confidence = "high"))
  x <- cl_usa(r)
  expect_setequal(strsplit(x$types, ";")[[1]], c("evidence", "confidence"))
  expect_equal(x$evidence_direction, "improved")
  expect_equal(x$confidence_direction, "improved")
  expect_equal(x$severity, "moderate")
  expect_match(x$explanation, "Evidence improved from medium to high")
  expect_match(x$explanation, "Confidence rose from moderate to high")
})

test_that("a material score change records the delta and the bar crossed", {
  r <- cl_cmp(cl_v(), cl_v(cp = 0.71))
  x <- cl_usa(r)
  expect_equal(x$types, "score")
  expect_equal(x$severity, "moderate")      # |delta| 0.11 >= moderate_delta
  expect_equal(x$d_cp, 0.11, tolerance = 1e-9)
  expect_true(is.na(x$crossed))
  r2 <- cl_cmp(cl_v(), cl_v(cp = 0.76))       # 0.60 -> 0.76 crosses the 0.75 position bar
  expect_match(cl_usa(r2)$crossed, "cp 0.600->0.760 (bar 0.75)", fixed = TRUE)
  expect_match(cl_usa(r2)$explanation, "Position rose 0.60 → 0.76, crossing the 0.75 bar", fixed = TRUE)
})

test_that("a recommendation change is major, with posture vs mode subtypes", {
  r <- cl_cmp(cl_v(), cl_v(choice = "catch_up", cp = 0.78, evidence = "high"))
  x <- cl_usa(r)
  expect_true(grepl("recommendation", x$types))
  expect_equal(x$subtype, "mode")
  expect_equal(x$severity, "major")
  expect_match(x$explanation, "^Recommendation changed from Partner to Catch Up\\.")
  r2 <- cl_cmp(cl_v(), cl_v(choice = "accelerate"))
  expect_equal(cl_usa(r2)$subtype, "posture")
  r3 <- cl_cmp(cl_v(), cl_v(choice = "not_assessed"))
  expect_equal(cl_usa(r3)$subtype, "coverage")
  expect_match(cl_usa(r3)$explanation, "No longer assessed (was Partner)", fixed = TRUE)
})

test_that("a methodology-only change is attributed to the method", {
  r <- cl_cmp(cl_v(), cl_v(choice = "catch_up", cp = 0.78), hc = H2m)
  x <- cl_usa(r)
  expect_true(r$pair$method_changed)
  expect_equal(x$attribution, "methodology")
  expect_match(x$explanation, "comes from the method, not the world")
})

test_that("a method change is never credited alone when the data is not verified or also changed", {
  cur <- cl_v(choice = "catch_up", cp = 0.78)
  h_unv <- cl_header("v2u", "2026-11-21T00:00:00Z", method = "m-bbbb", data_verified = FALSE)
  x <- cl_usa(cl_cmp(cl_v(), cur, hc = h_unv))
  expect_equal(x$attribution, "unattributed")
  expect_match(x$explanation, "cannot be ruled out")
  h_dat <- cl_header("v2d", "2026-11-21T00:00:00Z", method = "m-bbbb", data = "d-9999")
  expect_equal(cl_usa(cl_cmp(cl_v(), cur, hc = h_dat))$attribution, "mixed")
})

test_that("a method change with moved inputs and unstamped data is mixed, and a bridge decomposes it", {
  cur <- cl_v(choice = "catch_up", cp = 0.78, share = 0.06)
  h_unv <- cl_header("v2u", "2026-11-21T00:00:00Z", method = "m-bbbb", data_verified = FALSE)
  r <- cl_cmp(cl_v(), cur, hc = h_unv)
  expect_equal(cl_usa(r)$attribution, "mixed")
  # verified-same data: even a moved share is the method's (upstream code re-derived it from the same inputs)
  expect_equal(cl_usa(cl_cmp(cl_v(), cur, hc = H2m))$attribution, "methodology")
  # bridge = new method on old inputs: the method alone moved the choice; data then moved share only
  bridge <- cl_v(choice = "catch_up", cp = 0.78)
  rb <- cl_cmp(cl_v(), cur, hc = H2m, bridge = bridge)
  x <- cl_usa(rb)
  expect_equal(x$attribution, "both")
  expect_match(x$method_types, "recommendation")
  expect_equal(x$world_types, "data")
  # bridge identical to current: the whole change is methodological
  rb2 <- cl_cmp(cl_v(), cur, hc = H2m, bridge = cur)
  expect_equal(cl_usa(rb2)$attribution, "methodology")
})

test_that("an unverified (dirty) fingerprint is flagged", {
  r <- cl_cmp(cl_v(), cl_v(cp = 0.76), hc = cl_header("v2", "2026-11-21T00:00:00Z", unverified = TRUE))
  expect_true(cl_usa(r)$method_unverified)
  expect_match(cl_usa(r)$explanation, "unverified")
})

test_that("identical engine input ledgers make the data verifiably the same; differing ones make it changed", {
  led <- function(h, id) { h$data$inputs_id <- id; h$data$verified <- FALSE; h }
  expect_equal(change_data_relation(led(H1, "i-1")$data, led(H2m, "i-1")$data), "same")
  expect_equal(change_data_relation(led(H1, "i-1")$data, led(H2m, "i-2")$data), "changed")
  expect_equal(change_data_relation(list(id = "d-1"), list(id = "d-1")), "unknown")
  expect_equal(change_data_relation(list(id = "d-1"), list(id = "d-2")), "changed")
  # a stamp added on one side only is not a data change; moved windows are
  w <- list(benchmark_period = "2022-2024", momentum_period = "2025", momentum_through = "Dec 2025")
  expect_equal(change_data_relation(c(list(id = "d-1"), w), c(list(id = "d-9", verified = TRUE), w)), "unknown")
  expect_equal(change_data_relation(c(list(id = "d-1"), w), c(list(id = "d-9", verified = TRUE), utils::modifyList(w, list(momentum_through = "Jun 2026")))), "changed")
  cur <- cl_v(choice = "catch_up", cp = 0.78)
  expect_equal(cl_usa(cl_cmp(cl_v(), cur, hp = led(H1, "i-1"), hc = led(H2m, "i-1")))$attribution, "methodology")
  expect_equal(cl_usa(cl_cmp(cl_v(), cur, hp = led(H1, "i-1"), hc = led(H2m, "i-2")))$attribution, "mixed")
  # same engine, different inputs: new data
  expect_equal(cl_usa(cl_cmp(cl_v(), cur, hp = led(H1, "i-1"), hc = led(H2, "i-2")))$attribution, "real_world")
})

test_that("the input ledger records the data files a run reads, with md5", {
  source(file.path(cl_root, "scripts/utils/change_stamps.R"))
  td <- tempfile("ledger"); dir.create(td)
  f_rds <- file.path(td, "a.rds"); saveRDS(1:3, f_rds)
  f_csv <- file.path(td, "b.csv"); utils::write.csv(data.frame(x = 1), f_csv, row.names = FALSE)
  change_ledger_start()
  on.exit(change_ledger_stop(), add = TRUE)
  invisible(readRDS(f_rds)); invisible(utils::read.csv(f_csv))
  if (requireNamespace("data.table", quietly = TRUE)) invisible(data.table::fread(f_csv))
  doc <- change_ledger_write(file.path(td, "ledger.json"), td)
  expect_setequal(names(doc$files), c("a.rds", "b.csv"))
  expect_equal(doc$files[["a.rds"]], unname(tools::md5sum(f_rds)))
  change_ledger_stop()
  invisible(readRDS(f_rds))   # no longer traced: nothing new is recorded after a restart
  change_ledger_start(); change_ledger_stop()
  expect_length(ls(change_ledger_env), 0)
})

test_that("the method stamp fingerprints engine + upstream and ignores presentation", {
  source(file.path(cl_root, "scripts/utils/change_stamps.R"))
  skip_if(Sys.which("git") == "", "git not available")
  paths <- list(engine = list("R/change/posture_compare.R"), upstream = list("R/change/posture_vintage.R"),
                presentation = list("R/change/change_export.R"))
  a <- change_method_stamp(cl_root, paths, commit = "HEAD")
  paths_b <- paths
  paths_b$presentation <- list("R/change/posture_explain.R")
  b <- change_method_stamp(cl_root, paths_b, commit = "HEAD")
  expect_equal(a$id, b$id)
  expect_false(identical(a$presentation, b$presentation))
  expect_equal(a$n_files, 2)
  expect_match(a$id, "^m-")
})

test_that("vintages with different stage taxonomies are not compared", {
  h4 <- cl_header("v0", "2026-10-01T00:00:00Z", stages = c("Upstream", "Midstream", "Downstream", "Deployment"))
  r <- cl_cmp(cl_v(), cl_v(choice = "accelerate"), hp = h4)
  expect_false(r$pair$comparable)
  expect_equal(nrow(r$changes), 0)
  expect_match(r$pair$reason, "stage taxonomy")
  expect_equal(posture_previous_comparable(list(h4, H1, H2)), c(NA, NA, 2L))
})

test_that("assessment ids, method ids and data ids are stable", {
  expect_equal(assessment_id("usa", "Electric Grid", "Components"), "aib:posture:USA:electric-grid:components")
  a <- change_method_id(list(`R/x` = "1", `config/y.yml` = "2"))
  expect_equal(a, change_method_id(list(`config/y.yml` = "2", `R/x` = "1")))
  expect_false(identical(a, change_method_id(list(`R/x` = "1", `config/y.yml` = "3"))))
  expect_equal(change_data_id(list(benchmark_period = "2022-2024")), change_data_id(list(benchmark_period = "2022-2024")))
})

test_that("a vintage decodes from a columnar posture core", {
  core <- list(countries = list(iso3 = list("JPN", "USA")), meta = list(techs = list("Solar"), stages = list("Components", "Deployment")),
               cells = list(c = list(0, 1), t = list(0, 0), s = list(0, 1), p = list("C", "D"), m = list("partner", NULL),
                            dm = list(NULL, "accelerate"), need = list(0.8, 0.7), prize = list(0.6, NULL), io = list(0.4, NULL),
                            cp = list(0.5, NULL), share = list(0.02, 0.1), conf = list("high", "low"), ev = list("medium", NULL),
                            nt = list("supply_chain", "deployment"), bd = list(NULL, NULL), pw = list("yes", NULL),
                            v = list("compete_partner", "deploy_accelerate"), p1 = list("KOR", NULL), dc = list(NULL, "security")))
  v <- posture_vintage_from_core(core)
  expect_equal(names(v), POSTURE_VINTAGE_COLS)
  expect_equal(v$choice, c("partner", "accelerate"))
  expect_equal(v$posture, c("compete", "deploy"))
  expect_equal(v$need, c(0.8, 0.7))
  expect_equal(v$evidence, c("medium", NA))
})

test_that("crosswalk candidates need geography and sector, and skip unassessed cells", {
  cw <- yaml::read_yaml(file.path(cl_root, "config/neis_intelligence_crosswalk.yml"))
  cells <- dplyr::bind_rows(cl_cell(), cl_cell(stage = "Assembly"), cl_cell(stage = "Refining", choice = "not_assessed"))
  c1 <- posture_impact_candidates(list(geography = "united-states", sector = "grid"), cw, cells)
  expect_setequal(c1$assessment_id, c("aib:posture:USA:electric-grid:components", "aib:posture:USA:electric-grid:assembly"))
  c2 <- posture_impact_candidates(list(geography = "united-states", sector = "grid", value_chain_node = "components"), cw, cells)
  expect_equal(c2$assessment_id, "aib:posture:USA:electric-grid:components")
  expect_equal(nrow(posture_impact_candidates(list(geography = "united-states"), cw, cells)), 0)
})

test_that("impact records are validated and only accepted ones in the window attach", {
  ok <- list(impact_id = "imp-1", event_id = "e1", assessment_id = "aib:posture:USA:electric-grid:components",
             dimension = "position", reason = "Plant cancelled", reason_method = "editorial", confidence = 0.7,
             status = "accepted", proposed_by = "rule", proposed_at = "2026-11-05T00:00:00Z",
             reviewer = "analyst", review_timestamp = "2026-11-10T00:00:00Z")
  bad <- list(impact_id = "imp-2", event_id = "e2", assessment_id = "aib:posture:usa:grid", dimension = "vibes",
              reason = "x", reason_method = "model", status = "accepted", proposed_by = "ai", proposed_at = "2026-11-05")
  expect_length(validate_analytical_impacts(list(ok)), 0)
  probs <- validate_analytical_impacts(list(ok, bad))
  expect_true(any(grepl("bad assessment_id", probs)) && any(grepl("bad dimension", probs)) && any(grepl("needs reviewer", probs)))
  # a rule proposal may leave the dimension unspecified, an accepted impact may not
  expect_length(validate_analytical_impacts(list(utils::modifyList(ok, list(status = "proposed", dimension = "unspecified")))), 0)
  expect_match(validate_analytical_impacts(list(utils::modifyList(ok, list(dimension = "unspecified")))), "needs a dimension")
  late <- utils::modifyList(ok, list(impact_id = "imp-3", review_timestamp = "2026-11-25T00:00:00Z"))
  prop <- utils::modifyList(ok, list(impact_id = "imp-4", status = "proposed"))
  r <- cl_cmp(cl_v(), cl_v(cp = 0.76))
  ch <- attach_impacts(r$changes, list(ok, late, prop), list(v1 = H1, v2 = H2))
  expect_equal(ch$impacts[ch$iso3 == "USA"], "imp-1")
})

test_that("every provenance source id exists in the raw inputs manifest", {
  man <- yaml::read_yaml(file.path(cl_root, "config/raw_inputs_manifest.yml"))
  known <- vapply(man$inputs, function(e) e$id, character(1))
  ids <- unique(unlist(lapply(cl_cfg$sources, function(s) unlist(s$source_ids))))
  expect_true(all(ids %in% known), info = paste(setdiff(ids, known), collapse = ", "))
})

test_that("the bundle nests sides and keeps arrays as arrays", {
  r <- cl_cmp(cl_v(), cl_v(choice = "catch_up", cp = 0.78))
  b <- change_bundle(list(H1, H2), list(r), created = "2026-11-25T00:00:00Z", git_commit = "abc")
  js <- jsonlite::fromJSON(jsonlite::toJSON(b, auto_unbox = TRUE, null = "null", na = "null"), simplifyVector = FALSE)
  expect_equal(js$schema, "osi-changes")
  x <- js$changes[[1]]
  expect_equal(x$before$choice, "partner")
  expect_equal(x$after$choice, "catch_up")
  expect_true(is.list(x$types))
  expect_true(is.list(x$impacts))
})

test_that("the evidence basis is the dominant variable-stage status, and a change of basis is an evidence change", {
  tbl <- data.frame(exact_evidence_share_cp = c(0.7, 0.1, NA), split_evidence_share_cp = c(0.1, 0.1, NA),
                    shared_evidence_share_cp = c(0.1, 0.2, NA), proxy_evidence_share_cp = c(0.1, 0.6, NA),
                    tech_wide_evidence_share_cp = c(0, 0, NA), no_data_share_cp = c(0, 0, NA))
  expect_equal(evidence_status_from_shares(tbl), c("exact", "proxy", NA))
  expect_equal(evidence_status_from_shares(data.frame(x = 1)), NA_character_)
  prev <- dplyr::mutate(cl_v(), evidence_status = "proxy")
  cur <- dplyr::mutate(cl_v(), evidence_status = c("exact", "proxy"))
  x <- cl_usa(cl_cmp(prev, cur))
  expect_equal(x$types, "evidence")
  expect_equal(x$evidence_direction, "improved")
  expect_match(x$explanation, "Evidence basis changed from proxy to exact (stage-specific).", fixed = TRUE)
  expect_false(grepl("Evidence improved from", x$explanation))   # the band held
  expect_equal(x$cur_evidence_status, "exact")
})

test_that("bridge sandbox dirs keep nesting and the plan materializes data-dir inputs and redirects the rest", {
  sd <- bridge_sandbox_dirs(list(outputs = "/r/data/processed/outputs", processed = "/r/data/processed", raw = "/r/data/raw",
                                 diagnostics = "/r/diagnostics/sp"), "/sb")
  expect_equal(sd$processed, "/sb/processed")
  expect_equal(sd$outputs, "/sb/processed/outputs")
  expect_equal(sd$raw, "/sb/raw")
  plan <- bridge_plan(list(`data/processed/a.rds` = "aa11", `data/processed/outputs/b.rds` = "bb22", `data/reference/c.csv` = "cc33"),
                      "/r", list(outputs = "/r/data/processed/outputs", processed = "/r/data/processed"), sd, "/store")
  expect_equal(plan$target, c("/sb/processed/a.rds", "/sb/processed/outputs/b.rds", NA))
  expect_equal(plan$store[3], "/store/cc/cc33.csv")
  expect_true(bridge_usable(list(pure = TRUE, inputs_id = "i-1", method = list(id = "m-2")),
                            list(data = list(inputs_id = "i-1")), list(method = list(id = "m-2"))))
  expect_false(bridge_usable(list(pure = FALSE, inputs_id = "i-1", method = list(id = "m-2")),
                             list(data = list(inputs_id = "i-1")), list(method = list(id = "m-2"))))
})

test_that("a bridge run re-reads the previous inputs from the store, writes only in its sandbox and reports leaks", {
  source(file.path(cl_root, "scripts/utils/change_stamps.R"))
  root <- normalizePath(tempfile("repo"), winslash = "/", mustWork = FALSE); dir.create(root)
  proc <- file.path(root, "data", "processed"); out <- file.path(proc, "outputs"); ref <- file.path(root, "data", "reference")
  for (d in c(out, ref)) dir.create(d, recursive = TRUE)
  store <- file.path(root, "data", "vintages", "inputs")
  saveRDS(c(a = 1), file.path(proc, "need.rds")); utils::write.csv(data.frame(k = 1), file.path(ref, "map.csv"), row.names = FALSE)
  engine <- function(proc, out) {     # a stand-in for scripts/26: reads two inputs, writes one output
    n <- readRDS(file.path(proc, "need.rds")); m <- utils::read.csv(file.path(root, "data", "reference", "map.csv"))
    saveRDS(list(need = n, k = m$k), file.path(out, "result.rds"))
  }
  # 1. a normal run: ledger + store
  change_ledger_start(); engine(proc, out); change_ledger_stop()
  led <- change_ledger_write(file.path(out, "ledger.json"), root)
  expect_setequal(names(led$files), c("data/processed/need.rds", "data/reference/map.csv"))
  st <- change_store_inputs(led, root, store)
  expect_equal(st$stored, 2L)
  expect_equal(change_store_inputs(led, root, store)$kept, 2L)       # content-addressed: nothing copied twice
  # 2. the live inputs move on
  saveRDS(c(a = 99), file.path(proc, "need.rds")); utils::write.csv(data.frame(k = 7), file.path(ref, "map.csv"), row.names = FALSE)
  live_out <- readRDS(file.path(out, "result.rds"))
  # 3. the bridge: old inputs, sandboxed writes
  b <- change_bridge_setup(file.path(out, "ledger.json"), root, dirs = list(outputs = out, processed = proc), sandbox = file.path(root, "sb"), store_dir = store)
  change_ledger_start(b); engine(b$dirs$processed, b$dirs$outputs); change_ledger_stop()
  res <- readRDS(file.path(b$dirs$outputs, "result.rds"))
  expect_equal(unname(res$need), 1)             # materialized in the sandbox from the store
  expect_equal(res$k, 1)                        # a repository reference table, redirected to the store
  expect_identical(readRDS(file.path(out, "result.rds")), live_out)   # the live output was not touched
  rep <- change_bridge_finish(b, list(id = "m-x"))
  expect_true(rep$pure)
  expect_equal(rep$n_redirected, 1L)
  # 4. a read the previous run never made is a leak: the bridge is impure
  utils::write.csv(data.frame(z = 1), file.path(ref, "new.csv"), row.names = FALSE)
  b2 <- change_bridge_setup(file.path(out, "ledger.json"), root, dirs = list(outputs = out, processed = proc), sandbox = file.path(root, "sb2"), store_dir = store)
  change_ledger_start(b2); engine(b2$dirs$processed, b2$dirs$outputs); invisible(utils::read.csv(file.path(ref, "new.csv"))); change_ledger_stop()
  rep2 <- change_bridge_finish(b2, list(id = "m-x"))
  expect_false(rep2$pure)
  expect_match(rep2$leaks, "new.csv$")
  expect_error(change_bridge_setup(file.path(root, "nope"), root, list(), "", store), "neither a file nor a vintage id")
})

test_that("ledger paths are logical: folded but never resolved through links", {
  source(file.path(cl_root, "scripts/utils/change_stamps.R"))
  expect_equal(change_abs(r"(C:\x\data\raw\..\raw\.\f.csv)"), "C:/x/data/raw/f.csv")
  expect_equal(change_abs("/srv/repo/a/../b"), "/srv/repo/b")
  expect_true(startsWith(change_abs("rel/f.rds"), change_abs(getwd())))
})

test_that("pruning the input store keeps only what a vintage could still be bridged from", {
  source(file.path(cl_root, "scripts/utils/change_stamps.R"))
  st <- tempfile("store"); dir.create(file.path(st, "aa"), recursive = TRUE); dir.create(file.path(st, "bb"))
  for (f in c("aa/aa11.rds", "aa/aa22.csv", "bb/bb33.rds", "bb/bb44.rds.part")) writeLines("x", file.path(st, f))
  dry <- change_store_prune(st, c("aa11", "bb33"), dry_run = TRUE)
  expect_equal(dry$removed, 2L)
  expect_true(file.exists(file.path(st, "aa/aa22.csv")))
  pr <- change_store_prune(st, c("aa11", "bb33"))
  expect_equal(pr$kept, 2L)
  expect_setequal(basename(list.files(st, recursive = TRUE)), c("aa11.rds", "bb33.rds"))
})
