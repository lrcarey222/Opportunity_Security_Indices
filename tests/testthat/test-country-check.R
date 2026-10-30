# Country Insights fact sheets on the synthetic inputs (spec_country.md A7.3, A8.5, A12): every sheet
# builds, survives the JSON round trip unchanged and passes country_check_sheet() (the R mirror of the
# TS checkFactSheet()); each broken case of tests/testthat/fixtures/country_broken_cases.json (shared
# with dashboard/src/country/__tests__/contract.test.ts) is reported with its keyword; the manifest
# passes country_check_manifest(); the paths the fixture must exercise are present (the Upgrade rule,
# the top_si_fallback rule of an all-Grow core member, no Strategic Index, a territory, the EU's
# Climate Action Tracker rating, the industrial-policy measures). Since 2026-10-28 the sheets are keyed on
# the 66 six-stage sectors: the stage constants equal the pipeline's, the eight sectors without an Energy
# Security score ship with every sheet, CM-FDI projects count in both manufacturing stages but once in the
# totals, and the three-stage industrial-policy run lists no measures by sector.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "country", "country_files.R"))
for (f in country_helper_files()) source(file.path(repo_root, f))
cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_insights.yml"))
dict <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))

ctx <- country_synthetic_ctx(cfg, dict)
ctx$meta$created <- "2026-10-12T00:00:00Z"
sheets <- lapply(stats::setNames(ctx$roster$iso3, ctx$roster$iso3), function(iso) country_fact_sheet(iso, ctx))
round_trip <- function(s) jsonlite::fromJSON(as.character(country_json(s)), simplifyVector = FALSE)

test_that("the sectors are the 11 technologies x the six supply-chain stages, in the Sector cell order", {
  # COUNTRY_STAGES is written out in country_util.R (scripts/84 sources that file alone): it must equal the pipeline's
  expect_identical(COUNTRY_STAGES, SUPPLY_CHAIN_STAGES)
  ct <- country_cells_tbl()
  expect_equal(nrow(ct), 66L)
  expect_false(anyDuplicated(ct$key) > 0)
  expect_equal(ct$key[1:6], c("batteries-extraction", "batteries-refining", "batteries-components", "batteries-assembly",
                              "batteries-deployment", "batteries-recovery"))
  expect_equal(ct$tech, rep(COUNTRY_TECHS, each = 6))
  expect_equal(ct$stage, rep(SUPPLY_CHAIN_STAGES, 11))
  expect_equal(ct$label[ct$key == "batteries-refining"], "Batteries \u2014 Refining")
  expect_equal(ct$label_prose[ct$key == "batteries-refining"], "battery refining")
  expect_equal(ct$label_prose[ct$key == "electric-vehicles-deployment"], "electric-vehicle deployment")
  expect_equal(ct$label_prose[ct$key == "solar-components"], "solar components")
  expect_false(any(grepl("stream", c(ct$key, ct$label, ct$label_prose), ignore.case = TRUE)))
  expect_setequal(names(COUNTRY_TECH_PROSE), COUNTRY_TECHS)
  # no technology slug ends in a stage slug (the dashboard's parseCellKey() reads the stage off the end)
  for (t in country_slug(COUNTRY_TECHS)) expect_false(any(endsWith(t, paste0("-", tolower(SUPPLY_CHAIN_STAGES)))), info = t)
  # the eight sectors without an Energy Security score (index_outputs.rds, 2026-10-27): at most 58 placed
  expect_length(COUNTRY_NO_ES_CELLS, 8)
  expect_true(all(COUNTRY_NO_ES_CELLS %in% ct$key))
  es <- data.frame(iso3 = "A", key = setdiff(ct$key, COUNTRY_NO_ES_CELLS), value = 0.5)
  expect_identical(country_no_es_cells(rbind(es, data.frame(iso3 = "B", key = "coal-recovery", value = NA))), COUNTRY_NO_ES_CELLS[order(match(COUNTRY_NO_ES_CELLS, ct$key))])
  expect_identical(COUNTRY_SCHEMA_VERSION, 3L)                          # schema 3: legacy_corner (design D6)
})

test_that("every synthetic sheet builds, round-trips exactly and passes the contract check", {
  expect_length(sheets, 10)
  for (iso in names(sheets)) {
    j <- as.character(country_json(sheets[[iso]]))
    back <- jsonlite::fromJSON(j, simplifyVector = FALSE)
    expect_identical(as.character(country_json(back)), j, info = iso)
    expect_equal(country_check_sheet(back), character(), info = iso)
    expect_false(grepl("NaN|Infinity", j), info = iso)
  }
})

test_that("the fixture exercises every path of the profile", {
  b <- lapply(sheets, round_trip)
  expect_equal(b$DEU$priority_rule, "upgrade")
  expect_equal(b$USA$priority_rule, "top_si_fallback")                 # the all-Grow core member (as the United States and Japan)
  expect_equal(b$USA$summary$legacy_corners$legacy_upgrade$v, 0)
  expect_false(b$NGA$meta$has_si)
  expect_equal(b$NGA$priority_rule, "top_eo")
  expect_equal(b$PRI$meta$status, "non_rankable")
  expect_equal(b$PRI$meta$alignment_basis, "inherited")
  expect_equal(b$DEU$policy$cat$scope, "eu_bloc")
  expect_true(b$DEU$meta$eu_member)
  expect_equal(b$BRA$meta$tier, "partner")
  expect_true(isTRUE(b$USA$policy$nipo$covered))
  # no Energy Security score for any economy in the eight no_es_cells sectors; every sheet ships the list
  for (s in b) {
    expect_length(s$cells, 66)
    expect_setequal(unlist(s$no_es_cells), COUNTRY_NO_ES_CELLS)
    expect_lte(s$summary$n_placed$v, 58)
    expect_equal(unlist(s$policy$coverage$cols), SUPPLY_CHAIN_STAGES)
    expect_equal(unlist(s$meta$cmfdi_stages), c("Components", "Assembly"))
    for (c in s$cells) if (c$key %in% COUNTRY_NO_ES_CELLS) {
      expect_null(c$es)
      expect_false(isTRUE(c$placed))
      expect_match(c$legacy_corner_reason, "no Energy Security score for any economy in this sector", fixed = TRUE)
    }
  }
  # the allied pick: any stage but Deployment, never a screened Extraction sector or an excluded technology
  for (s in b) if (!is.null(s$pick)) {
    pc <- Filter(function(c) c$key == s$pick$cell, s$cells)[[1]]
    expect_true(pc$stage %in% COUNTRY_PICK_STAGES, info = s$iso3)
    expect_false(pc$tech %in% COUNTRY_PICK_EXCLUDED_TECHS, info = s$iso3)
    expect_null(pc$si$pick_exclusion, info = s$iso3)
  }
  # a top Energy Security driver with a raw figure in the United States' sheet (the memo tests' raw token)
  raw_top <- unlist(lapply(b$USA$cells, function(c) lapply(Filter(function(d) isTRUE(d$top), c$drivers$es), function(d) {
    vapply(d$vars, function(v) !is.null(v$raw$v), TRUE)
  })))
  expect_true(any(raw_top))
  # the industrial-policy run is still tagged with the old three stages: counts, no per-sector lists
  n <- b$USA$policy$nipo
  expect_false(n$staged)
  expect_equal(n$staged_d, cfg$nipo$not_staged_words)
  expect_length(n$cells, 0)
  expect_length(n$n_by_cell, 0)
  expect_length(n$policies, 0)
  expect_gt(n$n_active$v, 0)
})

test_that("CM-FDI projects count in both manufacturing stages, and once in the partner and economy totals", {
  f <- ctx$fdi
  expect_equal(sort(unique(f$stage)), c("Assembly", "Components"))
  expect_equal(nrow(f), 2 * length(unique(f$pid)))
  for (iso in c("DEU", "USA", "CHN")) {
    x <- country_fdi(f, iso)
    u <- f[f$index_use %in% TRUE & f$home != f$dest & !duplicated(f$pid), ]
    expect_equal(x$n_in, sum(u$dest == iso), info = iso)
    expect_equal(x$n_out, sum(u$home == iso), info = iso)
    for (t in c("batteries", "electric-vehicles", "solar", "wind")) {
      cm <- x$cells[[paste0(t, "-components")]]
      as <- x$cells[[paste0(t, "-assembly")]]
      expect_equal(cm$in_n$v, as$in_n$v, info = paste(iso, t))     # the same projects in both stages
      expect_equal(cm$out_n$v, as$out_n$v, info = paste(iso, t))
    }
    for (j in setdiff(c(unique(f$home), unique(f$dest)), iso)) expect_equal(x$partner_in(j), sum(u$dest == iso & u$home == j), info = paste(iso, j))
  }
  expect_error(country_fdi_expand("P1", "Solar", "Midstream", "CHN", "USA", TRUE), "not supply-chain stages")
})

test_that("the Sector fdi chunk's counts (keyed on the first stage's cell) equal the Country's counts in every listed stage", {
  # the scripts/88 cross-check: Sector rows on the Components cell, fdi.stages Components and Assembly
  f <- ctx$fdi
  p <- f[f$index_use %in% TRUE & f$stage == "Components", , drop = FALSE]
  agg <- aggregate(list(n = rep(1L, nrow(p))), p[, c("key", "home", "dest")], sum)
  a <- country_fdi_sector_counts(agg$key, agg$home, agg$dest, agg$n, c("Components", "Assembly"))
  p2 <- f[f$index_use %in% TRUE, , drop = FALSE]
  b <- table(paste(p2$home, p2$dest, p2$key))
  expect_setequal(names(a), names(b))
  expect_equal(unname(a[names(b)]), as.vector(b))
  expect_true(any(grepl("-assembly$", names(a))))
  # a row counts once per listed stage; one stage only gives that stage's cells
  x <- country_fdi_sector_counts(c("solar-components", "solar-components"), c("CHN", "CHN"), c("USA", "USA"), c(2L, 3L), c("Components", "Assembly"))
  expect_equal(x, c("CHN USA solar-assembly" = 5L, "CHN USA solar-components" = 5L))
  expect_equal(country_fdi_sector_counts("solar-components", "CHN", "USA", 1L, "Components"), c("CHN USA solar-components" = 1L))
  expect_length(country_fdi_sector_counts(character(), character(), character(), integer(), c("Components", "Assembly")), 0)
  # rows not on the first stage's cell, or stages that are not supply-chain stages, stop the check
  expect_error(country_fdi_sector_counts("solar-assembly", "CHN", "USA", 1L, c("Components", "Assembly")), "not keyed on the Components cell")
  expect_error(country_fdi_sector_counts("solar-midstream", "CHN", "USA", 1L, "Midstream"), "supply-chain stages")
})

test_that("summary counts, orders and Strategic Index parts agree with the cells", {
  for (s in lapply(sheets, round_trip)) {
    placed <- Filter(function(c) isTRUE(c$placed), s$cells)
    expect_equal(s$summary$n_placed$v, length(placed))
    q <- vapply(placed, function(c) c$legacy_corner, "")
    # schema 3 (design D6): the legacy corners are namespaced; no key reads as the canonical Secure or Watch
    expect_true(all(q %in% COUNTRY_LEGACY_CORNER_IDS))
    expect_null(s$summary$quadrants)
    for (c in s$cells) expect_null(c$quadrant)
    for (k in COUNTRY_LEGACY_CORNER_IDS) expect_equal(s$summary$legacy_corners[[k]]$v, sum(q == k))
    expect_equal(s$summary$n_insecure$v, sum(q %in% c("legacy_upgrade", "legacy_exposed")))
    # the corners split at the country's median sector: the median of the plotted positions (2026-10-15)
    for (pl in c("es", "eo")) {
      pos <- vapply(placed, function(c) c[[pl]]$pos, 1)
      if (!length(pos)) { expect_null(s$summary$country_median[[pl]]$v); next }
      expect_equal(s$summary$country_median[[pl]]$v, stats::median(pos))
      expect_equal(sum(pos >= stats::median(pos)), sum(q %in% (if (pl == "es") c("legacy_grow", "legacy_stable") else c("legacy_grow", "legacy_upgrade"))))
    }
    pct <- vapply(s$summary$es_order, function(k) Filter(function(c) c$key == k, s$cells)[[1]]$es$pct$v, 1)
    expect_false(is.unsorted(pct))
    for (c in s$cells) if (isTRUE(c$si$in_sample)) expect_equal(sum(unlist(lapply(c$si$parts, function(p) p$v))), c$si$v, tolerance = 1e-9)
  }
})

cases <- jsonlite::fromJSON(file.path(repo_root, "tests", "testthat", "fixtures", "country_broken_cases.json"), simplifyVector = FALSE)

# The same mutations as breakSheet() in dashboard/src/country/__tests__/contract.test.ts.
break_sheet <- function(s, op) {
  placed <- which(vapply(s$cells, function(c) isTRUE(c$placed), TRUE))[1]
  ins <- which(vapply(s$cells, function(c) isTRUE(c$si$in_sample), TRUE))[1]
  ranked <- which(vapply(s$partners$rows, function(r) !is.null(r$rank$v), TRUE))
  switch(op,
    legacy_corner_flip = { q <- s$cells[[placed]]$legacy_corner; s$cells[[placed]]$legacy_corner <- if (q == "legacy_grow") "legacy_stable" else "legacy_grow" },
    pos_wrong_side = { p <- s$cells[[placed]]$es; s$cells[[placed]]$es$pos <- if (p$v >= p$med$v) 10 else 90 },
    country_median_off = { s$summary$country_median$es$v <- s$summary$country_median$es$v + 5 },
    si_parts_off = { s$cells[[ins]]$si$parts$eo$v <- s$cells[[ins]]$si$parts$eo$v + 0.1 },
    partner_rank_dup = { s$partners$rows[[ranked[2]]]$rank <- s$partners$rows[[ranked[1]]]$rank },
    reason_unknown = { s$partners$rows[[1]]$reason <- "xx" },
    cell_key_unknown = { s$cells[[1]]$key <- "fusion-extraction" },
    summary_count = { s$summary$n_placed$v <- s$summary$n_placed$v + 1 },
    order_swap = {
      o <- unlist(s$summary$es_order)
      pct <- function(k) Filter(function(c) c$key == k, s$cells)[[1]]$es$pct$v
      i <- which(vapply(seq_len(length(o) - 1), function(j) pct(o[j + 1]) != pct(o[j]), TRUE))[1]
      o[c(i, i + 1)] <- o[c(i + 1, i)]
      s$summary$es_order <- as.list(o)
    },
    fv_empty_d = { s$cells[[placed]]$es$pct$d <- "" },
    no_es_cell_with_es = {
      g <- which(vapply(s$cells, function(c) c$key == "geothermal-extraction", TRUE))
      s$cells[[g]]$es <- s$cells[[placed]]$es
    },
    placed_not_rankable = { s$cells[[placed]]$es$rankable <- FALSE },
    caveat_unknown = { s$caveats[[1]]$id <- "nope" },
    # the 2026-10 bug: out-of-sample sectors took the top ranks, so in-sample ranks ran past n
    priority_rank_offset = { s$cells[[ins]]$priority$rank$v <- s$cells[[ins]]$si$n$v + 2 },
    stop("unknown op ", op)
  )
  s
}

test_that("each shared broken case is reported with its keyword (R mirror of checkFactSheet)", {
  base <- round_trip(sheets[[cases$base]])
  expect_equal(country_check_sheet(base), character())
  for (cs in cases$cases) {
    pr <- country_check_sheet(break_sheet(base, cs$op))
    expect_true(length(pr) > 0, info = cs$op)
    expect_true(grepl(cs$keyword, paste(pr, collapse = " | "), fixed = TRUE), info = paste(cs$op, ":", paste(pr, collapse = " | ")))
  }
})

test_that("the R check reports unknown sector keys outside the cells, a wrong stage and per-sector lists of an unstaged run", {
  base <- round_trip(sheets$USA)
  has <- function(s, kw) any(grepl(kw, country_check_sheet(s), fixed = TRUE))
  b <- base
  i <- which(vapply(b$partners$rows, function(r) length(r$fs_best5) > 0, TRUE))[1]
  b$partners$rows[[i]]$fs_best5[[1]] <- "batteries-midstream"
  expect_true(has(b, "fs_best5: 'batteries-midstream' is not a sector key"))
  b <- base
  b$cells[[2]]$stage <- "Upstream"
  expect_true(has(b, "is not a supply-chain stage"))
  b <- base
  b$cells[[2]]$stage <- "Assembly"
  expect_true(has(b, "is not the stage of the key"))
  b <- base
  b$summary$si_order[[1]] <- "oil-upstream"
  expect_true(has(b, "summary.si_order: 'oil-upstream' is not a sector key"))
  b <- base
  b$policy$coverage$cols <- list("Upstream", "Midstream", "Downstream")
  expect_true(has(b, "policy.coverage.cols"))
  b <- base
  b$policy$nipo$cells <- list(`solar-assembly` = list(0L))
  expect_true(has(b, "staged false"))
  b <- base
  b$no_es_cells <- list("geothermal-midstream")
  expect_true(has(b, "no_es_cells: unknown keys"))
})

test_that("the manifest of the synthetic export passes its check; a broken one does not", {
  entries <- lapply(seq_len(nrow(ctx$roster)), function(i) {
    ro <- as.list(ctx$roster[i, ])
    country_manifest_entry(ro, round_trip(sheets[[ro$iso3]]), sprintf("facts-%s.0123abcd.json", ro$iso3), 20000L, NULL)
  })
  counts <- list(economies = length(entries), sheets = length(entries), core = sum(ctx$roster$tier == "core"),
                 partners = sum(ctx$roster$tier == "partner"), memos = 0L)
  m <- country_manifest(entries, ctx$meta, list(), list(), counts)
  back <- jsonlite::fromJSON(as.character(country_json(m)), simplifyVector = FALSE)
  expect_equal(country_check_manifest(back), character())
  expect_identical(back$schema_version, 3L)
  expect_true(all(vapply(back$countries, function(e) is.null(e$quadrants), TRUE)))
  expect_true(all(vapply(back$countries, function(e) all(names(e$legacy_corners) %in% COUNTRY_LEGACY_CORNER_IDS), TRUE)))
  expect_setequal(unlist(back$no_es_cells), COUNTRY_NO_ES_CELLS)
  expect_equal(unlist(back$cmfdi_stages), c("Components", "Assembly"))
  back$countries[[1]]$file <- "facts-USA.json"
  expect_true(length(country_check_manifest(back)) > 0)
  # a memo kept under the earlier quadrant rule (2026-10-15) carries its note and words, and is not stale
  usa <- which(vapply(m$countries, function(c) c$iso3 == "USA", TRUE))
  memo <- list(json = "memo-USA.0123abcd.json", pdf = NULL, docx = NULL, md = NULL, words = 900L, generated_at = "2026-10-14T18:00:00Z",
               verified_at = "2026-10-14T18:00:00Z", facts_sha256 = "x", stale = FALSE, rule_note = COUNTRY_MEMO_RULE_NOTE,
               rule_note_d = country_memo_rule_note_words(cfg, COUNTRY_MEMO_RULE_NOTE, "United States", "2026-10-14T18:00:00Z"))
  m$countries[[usa]]$memo <- memo
  ok <- jsonlite::fromJSON(as.character(country_json(m)), simplifyVector = FALSE)
  expect_equal(country_check_manifest(ok), character())
  bad <- ok
  bad$countries[[usa]]$memo$rule_note_d <- NULL
  expect_match(paste(country_check_manifest(bad), collapse = " | "), "rule_note_d")
  bad <- ok
  bad$countries[[usa]]$memo$stale <- TRUE
  expect_match(paste(country_check_manifest(bad), collapse = " | "), "stale")
})

test_that("the gzip budgets are the six-stage export's measured sizes + ~15% (the same numbers as contract.ts and check-budget.mjs)", {
  # 2026-10-28, first six-stage export (the largest of the check-only, scratch and published runs): largest sheet
  # 52,001 B (JPN; 51,998 published), median 37,621 B; manifest 18,560 B (published, 49 validation rows); largest
  # monthly trade file 17,043 B (USA)
  expect_identical(COUNTRY_BUDGET_GZ, c(manifest = 21504, sheet = 60416, sheet_target = 37888, trade_monthly = 25600))
  expect_true(all(COUNTRY_BUDGET_GZ %% 1024 == 0))
  expect_lt(COUNTRY_BUDGET_GZ[["sheet_target"]], COUNTRY_BUDGET_GZ[["sheet"]])
  measured <- c(sheet = 52001, manifest = 18560, trade_monthly = 17043)
  for (k in names(measured)) {
    expect_gte(COUNTRY_BUDGET_GZ[[k]], 1.15 * measured[[k]])     # at least ~15% headroom
  }
  expect_lte(COUNTRY_BUDGET_GZ[["sheet"]], 1.15 * measured[["sheet"]] + 1024)  # and not more than a KiB beyond it
  # every synthetic sheet is within the budget
  sz <- vapply(sheets, function(s) length(memCompress(charToRaw(enc2utf8(as.character(country_json(s)))), "gzip")), 1)
  expect_true(all(sz <= COUNTRY_BUDGET_GZ[["sheet"]]))
})

test_that("the placeholder memo carries the verbatim label and resolves its tokens", {
  pm <- country_placeholder_memo(round_trip(sheets$USA), cfg)
  expect_equal(pm$label, "AI-generated from the data and checked against it; NEIS baseline as of 12 October 2026.")
  vals <- unlist(lapply(pm$summary, function(it) vapply(Filter(function(n) n$t == "value", it$inlines), function(n) n$v, "")))
  expect_false(any(vals == COUNTRY_DASH))
  expect_true(pm$words > 0)
})
