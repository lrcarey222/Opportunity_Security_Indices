# Country Insights fact sheets on the synthetic inputs (spec_country.md A7.3, A8.5, A12): every sheet
# builds, survives the JSON round trip unchanged and passes country_check_sheet() (the R mirror of the
# TS checkFactSheet()); each broken case of tests/testthat/fixtures/country_broken_cases.json (shared
# with dashboard/src/country/__tests__/contract.test.ts) is reported with its keyword; the manifest
# passes country_check_manifest(); the paths the fixture must exercise are present (the Upgrade rule,
# the top_si_fallback rule of an all-Grow core member, no Strategic Index, a territory, the EU's
# Climate Action Tracker rating, the industrial-policy measures).

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "country", "country_files.R"))
for (f in country_helper_files()) source(file.path(repo_root, f))
cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_insights.yml"))
dict <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))

ctx <- country_synthetic_ctx(cfg, dict)
ctx$meta$created <- "2026-10-12T00:00:00Z"
sheets <- lapply(stats::setNames(ctx$roster$iso3, ctx$roster$iso3), function(iso) country_fact_sheet(iso, ctx))
round_trip <- function(s) jsonlite::fromJSON(as.character(country_json(s)), simplifyVector = FALSE)

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
  expect_equal(b$USA$summary$quadrants$upgrade$v, 0)
  expect_false(b$NGA$meta$has_si)
  expect_equal(b$NGA$priority_rule, "top_eo")
  expect_equal(b$PRI$meta$status, "non_rankable")
  expect_equal(b$PRI$meta$alignment_basis, "inherited")
  expect_equal(b$DEU$policy$cat$scope, "eu_bloc")
  expect_true(b$DEU$meta$eu_member)
  expect_equal(b$BRA$meta$tier, "partner")
  expect_true(isTRUE(b$USA$policy$nipo$covered))
  # no Energy Security score in the paper for these three sectors, for every economy
  for (s in b) for (c in s$cells) if (c$key %in% c("geothermal-upstream", "geothermal-midstream", "electric-grid-downstream")) {
    expect_null(c$es)
    expect_false(isTRUE(c$placed))
  }
})

test_that("summary counts, orders and Strategic Index parts agree with the cells", {
  for (s in lapply(sheets, round_trip)) {
    placed <- Filter(function(c) isTRUE(c$placed), s$cells)
    expect_equal(s$summary$n_placed$v, length(placed))
    q <- vapply(placed, function(c) c$quadrant, "")
    for (k in c("grow", "upgrade", "secure", "watch")) expect_equal(s$summary$quadrants[[k]]$v, sum(q == k))
    expect_equal(s$summary$n_insecure$v, sum(q %in% c("upgrade", "secure")))
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
    quadrant_flip = { q <- s$cells[[placed]]$quadrant; s$cells[[placed]]$quadrant <- if (q == "grow") "watch" else "grow" },
    pos_wrong_side = { p <- s$cells[[placed]]$es; s$cells[[placed]]$es$pos <- if (p$v >= p$med$v) 10 else 90 },
    si_parts_off = { s$cells[[ins]]$si$parts$eo$v <- s$cells[[ins]]$si$parts$eo$v + 0.1 },
    partner_rank_dup = { s$partners$rows[[ranked[2]]]$rank <- s$partners$rows[[ranked[1]]]$rank },
    reason_unknown = { s$partners$rows[[1]]$reason <- "xx" },
    cell_key_unknown = { s$cells[[1]]$key <- "fusion-upstream" },
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
      g <- which(vapply(s$cells, function(c) c$key == "geothermal-upstream", TRUE))
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
  back$countries[[1]]$file <- "facts-USA.json"
  expect_true(length(country_check_manifest(back)) > 0)
})

test_that("the placeholder memo carries the verbatim label and resolves its tokens", {
  pm <- country_placeholder_memo(round_trip(sheets$USA), cfg)
  expect_equal(pm$label, "AI-generated from the data and checked against it; paper baseline as of 12 October 2026.")
  vals <- unlist(lapply(pm$summary, function(it) vapply(Filter(function(n) n$t == "value", it$inlines), function(n) n$v, "")))
  expect_false(any(vals == COUNTRY_DASH))
  expect_true(pm$words > 0)
})
