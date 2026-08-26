# Finger-Kreinin export similarity index.
#
# The computation is classification-agnostic, so the strongest available check is to
# feed it the historical SITC3 pull and confirm it reproduces the previously computed
# export_similarity_index_2000_present.csv exactly. That fixture is optional (the raw
# pull is a large untracked artefact), so the deterministic maths is also covered by
# synthetic cases that always run.

opsi_esi_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure computation, not the script's bootstrap or its runner.
opsi_load_esi <- function() {
  path <- file.path(opsi_esi_root(), "R", "charts", "export_similarity.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c(
    "EXPORT_SIMILARITY_COUNTRIES", "EXPORT_SIMILARITY_DEFAULT_START",
    "EXPORT_SIMILARITY_MIN_PRODUCTS", "EXPORT_SIMILARITY_LEVELS",
    "EXPORT_SIMILARITY_DEFAULT_LEVEL", "export_similarity_aggregate",
    "export_similarity_basket_size",
    "export_similarity_pairs", "export_similarity_one",
    "export_similarity_compute", "export_similarity_empty",
    "export_similarity_by_tech", "export_similarity_tidy",
    "export_similarity_build_requests", "export_similarity_cache_path",
    "export_similarity_partner_series", "export_similarity_plot_partner"
  )
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("identical export structures score 100 and disjoint ones score 0", {
  opsi_load_esi()

  same <- data.frame(
    year = 2020, iso = rep(c("CHN", "USA"), each = 2),
    code = rep(c("850760", "854142"), 2), value = c(60, 40, 30, 20),
    stringsAsFactors = FALSE
  )
  expect_equal(export_similarity_one(same, 2020, "CHN", "USA", min_products = 1)$esi, 100)

  disjoint <- data.frame(
    year = 2020, iso = c("CHN", "USA"),
    code = c("850760", "854142"), value = c(100, 100),
    stringsAsFactors = FALSE
  )
  expect_equal(export_similarity_one(disjoint, 2020, "CHN", "USA", min_products = 1)$esi, 0)
})

test_that("the index is symmetric and matches a hand calculation", {
  opsi_load_esi()

  # CHN shares 0.75/0.25; USA shares 0.25/0.75.
  # sum(min) = min(.75,.25) + min(.25,.75) = .25 + .25 = 0.5 -> 50.
  d <- data.frame(
    year = 2020, iso = c("CHN", "CHN", "USA", "USA"),
    code = c("A", "B", "A", "B"), value = c(75, 25, 25, 75),
    stringsAsFactors = FALSE
  )
  expect_equal(export_similarity_one(d, 2020, "CHN", "USA", min_products = 1)$esi, 50)
  expect_equal(
    export_similarity_one(d, 2020, "USA", "CHN", min_products = 1)$esi,
    export_similarity_one(d, 2020, "CHN", "USA", min_products = 1)$esi
  )
})

test_that("a product missing for one country counts as zero, not as an omission", {
  opsi_load_esi()

  d <- data.frame(
    year = 2020, iso = c("CHN", "CHN", "USA"),
    code = c("A", "B", "A"), value = c(50, 50, 100),
    stringsAsFactors = FALSE
  )
  res <- export_similarity_one(d, 2020, "CHN", "USA", min_products = 1)
  # CHN .5/.5 vs USA 1/0 -> min(.5,1) + min(.5,0) = .5 -> 50.
  expect_equal(res$esi, 50)
  expect_equal(res$products_compared, 2L)
})

test_that("a basket too thin to have structure yields NA, not a spurious 100", {
  opsi_load_esi()

  # One shared product makes both shares 1, so the raw formula returns 100 - which
  # reads as "identical export structures" when it means "nothing to compare".
  thin <- data.frame(
    year = 2024, iso = c("CHN", "DEU"), code = c("854142", "854142"),
    value = c(10, 20), stringsAsFactors = FALSE
  )
  expect_equal(export_similarity_one(thin, 2024, "CHN", "DEU", min_products = 1)$esi, 100)

  guarded <- export_similarity_one(thin, 2024, "CHN", "DEU")
  expect_true(is.na(guarded$esi))
  expect_equal(guarded$products_compared, 1L)

  # The product count is still reported so the thinness is visible in the output.
  panel <- export_similarity_compute(thin, basket_size = 20, group = "Semiconductors")
  expect_true(all(is.na(panel$esi)))
  expect_true(all(panel$products_compared == 1))
  expect_equal(EXPORT_SIMILARITY_MIN_PRODUCTS, 5L)
})

test_that("six countries produce fifteen unordered pairs", {
  opsi_load_esi()

  pairs <- export_similarity_pairs(names(EXPORT_SIMILARITY_COUNTRIES))
  expect_equal(nrow(pairs), 15)
  expect_equal(anyDuplicated(paste(pairs$country_a, pairs$country_b)), 0)
  expect_true(all(pairs$country_a < pairs$country_b))
})

test_that("the panel covers every pair-year and reports basket coverage", {
  opsi_load_esi()

  d <- expand.grid(
    year = 2019:2020, iso = c("CHN", "USA", "DEU"), code = c("A", "B"),
    stringsAsFactors = FALSE
  )
  d$value <- seq_len(nrow(d))

  out <- export_similarity_compute(d, basket_size = 4, group = "All technologies",
                                   min_products = 1)
  expect_equal(nrow(out), 2 * 3)          # 2 years x 3 pairs
  expect_true(all(out$products_compared == 2))
  expect_true(all(out$coverage_pct == 50))
  expect_true(all(out$group == "All technologies"))
  expect_false(any(is.na(out$esi)))
})

test_that("aggregation re-keys HS6 to coarser levels", {
  opsi_load_esi()

  d <- data.frame(
    year = 2024, iso = "CHN",
    code = c("854142", "854143", "850760"), value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_equal(export_similarity_aggregate(d, "hs6")$code, d$code)
  expect_equal(export_similarity_aggregate(d, "hs4")$code, c("8541", "8541", "8507"))
  expect_equal(export_similarity_aggregate(d, "hs2")$code, c("85", "85", "85"))

  # Values are left intact; compute() sums them per (iso, code).
  expect_equal(sum(export_similarity_aggregate(d, "hs4")$value), 6)
  expect_error(export_similarity_aggregate(d, "nonsense"))
})

test_that("tech and sub_sector levels map through the master, many-to-many", {
  opsi_load_esi()

  master_long <- data.frame(
    hs6 = c("854142", "854142", "850760"),
    tech = c("Solar", "Semiconductors", "Batteries"),
    sub_sector = c("Solar Cells", "Discrete Semiconductors", "Battery Cells"),
    stringsAsFactors = FALSE
  )
  d <- data.frame(year = 2024, iso = "CHN", code = c("854142", "850760"),
                  value = c(10, 20), stringsAsFactors = FALSE)

  by_tech <- export_similarity_aggregate(d, "tech", master_long)
  # 854142 belongs to two technologies, so it contributes to both.
  expect_equal(nrow(by_tech), 3)
  expect_setequal(by_tech$code, c("Solar", "Semiconductors", "Batteries"))

  by_sub <- export_similarity_aggregate(d, "sub_sector", master_long)
  expect_setequal(by_sub$code, c("Solar Cells", "Discrete Semiconductors", "Battery Cells"))

  # Those levels are undefined without the crosswalk.
  expect_error(export_similarity_aggregate(d, "tech"), "needs master_long")
})

test_that("basket size is counted at the requested level", {
  opsi_load_esi()

  master_long <- data.frame(
    hs6 = c("854142", "854143", "850760"),
    tech = c("Solar", "Solar", "Batteries"),
    sub_sector = c("Solar Cells", "Solar Modules", "Battery Cells"),
    stringsAsFactors = FALSE
  )

  expect_equal(export_similarity_basket_size(master_long, "hs6"), 3)
  expect_equal(export_similarity_basket_size(master_long, "hs4"), 2)   # 8541, 8507
  expect_equal(export_similarity_basket_size(master_long, "hs2"), 1)   # 85
  expect_equal(export_similarity_basket_size(master_long, "tech"), 2)
  expect_equal(export_similarity_basket_size(master_long, "sub_sector"), 3)

  # Restricting to one technology's codes narrows the basket.
  expect_equal(
    export_similarity_basket_size(master_long, "hs6", codes = c("854142", "854143")), 2
  )
})

test_that("the index rises with aggregation and records its level", {
  opsi_load_esi()

  # Two countries differing inside 8541 but identical at the 8541 heading.
  d <- data.frame(
    year = 2024,
    iso = c("CHN", "CHN", "USA", "USA"),
    code = c("854142", "854143", "854142", "854143"),
    value = c(90, 10, 10, 90),
    stringsAsFactors = FALSE
  )

  fine <- export_similarity_one(d, 2024, "CHN", "USA", min_products = 1)$esi
  coarse <- export_similarity_one(
    export_similarity_aggregate(d, "hs4"), 2024, "CHN", "USA", min_products = 1
  )$esi

  expect_equal(fine, 20)     # min(.9,.1) + min(.1,.9)
  expect_equal(coarse, 100)  # the difference disappears inside 8541
  expect_gt(coarse, fine)

  out <- export_similarity_compute(d, group = "All technologies", level = "hs6",
                                   min_products = 1)
  expect_true(all(out$level == "hs6"))
})

test_that("hs4 is the default level", {
  opsi_load_esi()
  expect_equal(EXPORT_SIMILARITY_DEFAULT_LEVEL, "hs4")
  expect_true(EXPORT_SIMILARITY_DEFAULT_LEVEL %in% EXPORT_SIMILARITY_LEVELS)
})

test_that("the request grid satisfies the Comtrade client contract", {
  opsi_load_esi()

  client <- readLines(
    file.path(opsi_esi_root(), "scripts", "utils", "comtrade_client.R"), warn = FALSE
  )
  line <- grep("required_cols\\s*<-", client, value = TRUE)[1]
  required <- eval(parse(text = sub(".*required_cols\\s*<-\\s*", "", line)))

  req <- export_similarity_build_requests(
    reporters = c("CHN", "USA"), commodity_codes = list("850760"), years = 2019:2020
  )

  expect_true(all(required %in% names(req)))
  expect_equal(nrow(req), 4)
  expect_true(all(req$start_date == req$end_date))
  expect_equal(anyDuplicated(req$request_id), 0)
  expect_equal(unique(req$partner), "World")
  expect_equal(unique(req$flow_direction), "export")
})

test_that("the partner series picks up China on either side of a pair", {
  opsi_load_esi()

  r <- data.frame(
    group = "All technologies", level = "hs4", year = 2024,
    country_a = c("USA", "CHN", "DEU"), country_b = c("CHN", "DEU", "FRA"),
    country_a_name = c("United States", "China", "Germany"),
    country_b_name = c("China", "Germany", "France"),
    pair = c("United States - China", "China - Germany", "Germany - France"),
    esi = c(33.3, 54.3, 49.6),
    products_compared = 98L, coverage_pct = 86,
    stringsAsFactors = FALSE
  )

  s <- export_similarity_partner_series(r, "CHN")

  # Pairs are stored alphabetically, so the partner can be on either side.
  expect_equal(nrow(s), 2)
  expect_setequal(s$counterpart, c("USA", "DEU"))
  expect_true(all(s$partner_name == "China"))
  # The pair not involving China is dropped.
  expect_false("Germany - France" %in% s$pair)
})

test_that("the partner series drops suppressed cells and other groups", {
  opsi_load_esi()

  r <- data.frame(
    group = c("All technologies", "All technologies", "Solar"),
    level = "hs4", year = 2024,
    country_a = "CHN", country_b = c("DEU", "USA", "DEU"),
    country_a_name = "China", country_b_name = c("Germany", "United States", "Germany"),
    pair = c("China - Germany", "China - United States", "China - Germany"),
    esi = c(54.3, NA_real_, 70.1),
    products_compared = c(98L, 1L, 12L), coverage_pct = 86,
    stringsAsFactors = FALSE
  )

  s <- export_similarity_partner_series(r, "CHN")
  expect_equal(nrow(s), 1)              # NA suppressed, Solar is a different group
  expect_equal(s$counterpart, "DEU")

  solar <- export_similarity_partner_series(r, "CHN", group = "Solar")
  expect_equal(nrow(solar), 1)
  expect_equal(solar$esi, 70.1)
})

test_that("the chart is built and labelled with the level it was computed at", {
  opsi_load_esi()
  skip_if_not_installed("ggplot2")

  series <- data.frame(
    year = rep(2020:2024, each = 2),
    level = "hs4",
    counterpart = rep(c("DEU", "USA"), 5),
    counterpart_name = rep(c("Germany", "United States"), 5),
    partner = "CHN", partner_name = "China",
    esi = c(54, 33, 55, 34, 53, 32, 56, 35, 54, 33),
    products_compared = 98L, coverage_pct = 86,
    stringsAsFactors = FALSE
  )

  p <- export_similarity_plot_partner(series, "China", level = "hs4")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "China")
  expect_match(p$labels$subtitle, "HS4 heading")

  # A different level must be reflected in the subtitle, since values are only
  # comparable within a level.
  p6 <- export_similarity_plot_partner(series, "China", level = "hs6")
  expect_match(p6$labels$subtitle, "HS6 subheading")

  expect_error(
    export_similarity_plot_partner(series[0, ], "China"),
    "No similarity values"
  )
})

test_that("it reproduces the historical SITC3 index exactly", {
  opsi_load_esi()
  root <- opsi_esi_root()

  raw_path <- file.path(root, "comtrade_sitc3_exports_raw_2000_present.rds")
  csv_path <- file.path(root, "export_similarity_index_2000_present.csv")
  skip_if_not(file.exists(raw_path) && file.exists(csv_path),
              "historical SITC3 fixture not present")

  raw <- readRDS(raw_path)
  stored <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

  # The original index used SITC Rev.3 three-digit exports to World.
  d <- raw[!is.na(raw$aggr_level) & raw$aggr_level == 3 &
             raw$flow_desc == "Export" & raw$partner_desc == "World", ]
  trade_long <- data.frame(
    year = as.integer(d$ref_year), iso = as.character(d$reporter_iso),
    code = as.character(d$cmd_code), value = as.numeric(d$primary_value),
    stringsAsFactors = FALSE
  )
  trade_long <- trade_long[!is.na(trade_long$value), ]

  got <- export_similarity_compute(trade_long)

  key <- function(df) paste(df$year, df$country_a, df$country_b)
  merged <- merge(
    data.frame(k = key(stored), stored = stored$esi, stringsAsFactors = FALSE),
    data.frame(k = key(got), got = got$esi, stringsAsFactors = FALSE),
    by = "k"
  )

  expect_equal(nrow(merged), nrow(stored))
  expect_equal(merged$got, merged$stored, tolerance = 1e-9)
})
