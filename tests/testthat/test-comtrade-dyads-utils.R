# Helpers of the all-reporter Comtrade energy-dyad pull (scripts/utils/comtrade_dyads_utils.R):
# planning within the free tier's record cap, re-planning without refetching, and turning raw
# chunks into rows shaped like allied_comtrade_energy_data.csv.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "comtrade_dyads_utils.R"))

raw_row <- function(reporter = "AUS", partner = "JPN", code = "854140", value = 100,
                    customs = "C00", mot = 0, partner2 = 0, flow = "M") {
  data.frame(
    type_code = "C", freq_code = "A", ref_period_id = 20200101L, ref_year = 2020L, ref_month = 52L,
    period = "2020", reporter_code = 36L, reporter_iso = reporter, reporter_desc = reporter,
    flow_code = flow, flow_desc = if (flow == "M") "Import" else "Export",
    partner_code = 392L, partner_iso = partner, partner_desc = partner,
    partner2code = partner2, partner2iso = "W00", partner2desc = "World",
    classification_code = "H5", classification_search_code = "HS", is_original_classification = TRUE,
    cmd_code = code, cmd_desc = "x", aggr_level = 6L, is_leaf = TRUE,
    customs_code = customs, customs_desc = "TOTAL CPC", mos_code = "0", mot_code = mot, mot_desc = "TOTAL MOT",
    qty_unit_code = 8L, qty_unit_abbr = "kg", qty = 1, is_qty_estimated = FALSE,
    alt_qty_unit_code = 8L, alt_qty_unit_abbr = "kg", alt_qty = 1, is_alt_qty_estimated = FALSE,
    net_wgt = 1, is_net_wgt_estimated = FALSE, gross_wgt = 0, is_gross_wgt_estimated = FALSE,
    cifvalue = value, fobvalue = 0, primary_value = value, legacy_estimation_flag = 0L,
    is_reported = TRUE, is_aggregate = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("the basket pads codes with zeros and keeps only legacy codes that are in it", {
  b <- comtrade_dyads_basket(list(
    data.frame(hs6 = c("85414", "854140", "280530"), legacy_hs = c("TRUE", "TRUE", "FALSE")),
    NULL,
    data.frame(HS6 = c("280530", "70310"))
  ))
  expect_equal(b$codes, c("070310", "085414", "280530", "854140"))
  expect_equal(b$legacy, c("085414", "854140"))
})

test_that("estimates use the lower rate for reporters with extended breakdowns, within caps", {
  reporters <- data.frame(year = 2020L, reporter_iso = c("AAA", "BBB", "CCC", "DDD"),
                          total_records = c(1e5, 1e5, 1e8, 10), extended = c(FALSE, TRUE, TRUE, FALSE))
  e <- comtrade_dyads_estimate(reporters, c("import", "export"))
  imp <- e[e$flow == "import", ]
  expect_equal(imp$est[1], 1.1 * 0.031 * 1e5)
  expect_equal(imp$est[2], 1.1 * 0.011 * 1e5)
  expect_equal(imp$est[3], 10500)
  expect_equal(imp$est[4], 200)
  expect_equal(e$est[e$flow == "export" & e$reporter_iso == "CCC"], 25000)
  # Without the flag every reporter gets the standard rate.
  e2 <- comtrade_dyads_estimate(reporters[, c("year", "reporter_iso", "total_records")], "import")
  expect_equal(e2$est[2], 1.1 * 0.031 * 1e5)
})

test_that("packing respects the record target and the reporter limit", {
  est <- c(50, 40, 30, 20, 10, 120)
  bins <- comtrade_dyads_pack(est, target = 60, max_items = 2)
  loads <- tapply(est, bins, sum)
  sizes <- tabulate(bins)
  expect_true(all(loads[loads != 120] <= 60))
  expect_true(all(sizes <= 2))
  expect_equal(sum(bins == bins[6]), 1L)
})

test_that("the plan orders imports first and tags chunk ids", {
  est <- data.frame(year = rep(c(2020L, 2024L), each = 2), reporter_iso = c("A", "B", "A", "B"),
                    flow = rep(c("export", "import"), 2), est = 1000)
  p <- comtrade_dyads_plan(est, target_records = 5000, n_groups = 2, id_tag = "c")
  expect_equal(p$flow[1:4], rep("import", 4))
  expect_true(all(grepl("^[0-9]{4}_(import|export)_g[12]_c[0-9]{2}$", p$chunk_id)))
  # one call per year x flow x code group
  expect_equal(nrow(p), 8L)
})

test_that("a re-plan keeps started chunks and never plans a covered reporter again", {
  reporters <- data.frame(year = 2020L, reporter_iso = c("A", "B", "C", "D"), total_records = 1e5)
  est <- comtrade_dyads_estimate(reporters, "import")
  old <- comtrade_dyads_plan(est, target_records = 3500, max_reporters = 50, n_groups = 2)
  started <- seq_len(nrow(old)) == 1
  new <- comtrade_dyads_replan(old, started, est, id_tag = "c", target_records = 50000,
                               max_reporters = 50, n_groups = 2)
  expect_equal(new[1, ], old[1, ])
  expanded <- do.call(rbind, lapply(seq_len(nrow(new)), function(i) {
    data.frame(g = new$code_group[i], r = strsplit(new$reporters[i], " ")[[1]])
  }))
  expect_false(anyDuplicated(expanded) > 0)
  expect_setequal(expanded$r[expanded$g == 1], c("A", "B", "C", "D"))
  expect_setequal(expanded$r[expanded$g == 2], c("A", "B", "C", "D"))
  expect_error(comtrade_dyads_replan(old, started, est, id_tag = "b", target_records = 3500, n_groups = 2), "collide")
})

test_that("a capped request splits by reporter first, then by commodity", {
  s <- comtrade_dyads_split(c("A", "B", "C"), c("1", "2"))
  expect_equal(s$a$reporters, c("A", "B"))
  expect_equal(s$b$codes, c("1", "2"))
  s1 <- comtrade_dyads_split("A", c("1", "2", "3"))
  expect_equal(s1$a$codes, c("1", "2"))
  expect_error(comtrade_dyads_split("A", "1"), "cannot split")
  expect_true(comtrade_dyads_is_truncated(100000))
  expect_false(comtrade_dyads_is_truncated(99999))
  expect_true(comtrade_dyads_is_empty(data.frame(count = 0)))
})

test_that("standardizing drops World, areas, non-totals and placeholders and adds the flags", {
  raw <- rbind(
    raw_row(),
    raw_row(partner = "W00"),
    raw_row(partner = "S19"),
    raw_row(customs = "C01"),
    raw_row(code = "85414")
  )
  raw[nrow(raw) + 1, ] <- raw[1, ]
  raw$reporter_iso[nrow(raw)] <- NA
  out <- comtrade_dyads_standardize(raw, economies = c("AUS", "JPN"), legacy_codes = "854140", taiwan = "drop")
  expect_equal(nrow(out), 2L)
  expect_equal(unname(attr(out, "dropped")), c(1, 1, 2))
  expect_equal(out$cmd_code, c("854140", "085414"))
  expect_equal(out$cmd_code_legacy_hs, c(TRUE, FALSE))
  expect_equal(out$classification_requested, c("HS", "HS"))
  expect_equal(out$flow_direction, c("import", "import"))
  expect_identical(names(out), c(COMTRADE_DYADS_ALLIED_COLUMNS, COMTRADE_DYADS_FLAG_COLUMNS))
})

test_that("Taiwan (Comtrade S19, 'Other Asia, nes') is recoded to TWN by default, as reporter and partner", {
  expect_identical(comtrade_dyads_recode_taiwan(c("S19", "JPN", NA, "W00")), c("TWN", "JPN", NA, "W00"))

  raw <- rbind(
    raw_row(partner = "S19", value = 250),
    raw_row(reporter = "S19", partner = "JPN", flow = "X", value = 75),
    raw_row(partner = "W00")
  )
  # TWN is in scope although the economy list (WDI, which has no Taiwan) does not list it.
  out <- comtrade_dyads_standardize(raw, economies = c("AUS", "JPN"))
  expect_equal(nrow(out), 2L)
  expect_equal(unname(attr(out, "dropped")), c(0, 0, 1))
  expect_equal(out$partner_iso, c("TWN", "JPN"))
  expect_equal(out$reporter_iso, c("AUS", "TWN"))
  expect_equal(out$reporter_req, c("AUS", "TWN"))
  expect_equal(out$primary_value, c(250, 75))
  expect_false(any(c(out$reporter_iso, out$partner_iso) == COMTRADE_DYADS_TAIWAN_ISO))
  expect_length(comtrade_dyads_validate(out, c("AUS", "JPN", COMTRADE_DYADS_TAIWAN_ISO3)), 0)
  expect_match(comtrade_dyads_validate(out, c("AUS", "JPN")), "outside the economy list: TWN", all = FALSE)

  # The old behaviour stays available: drop leaves no Taiwan rows.
  dropped <- comtrade_dyads_standardize(raw, economies = c("AUS", "JPN"), taiwan = "drop")
  expect_equal(nrow(dropped), 0L)
  expect_equal(unname(attr(dropped, "dropped")), c(0, 0, 3))
})

test_that("identical duplicates are kept once and distinct ones are summed", {
  base <- comtrade_dyads_standardize(rbind(raw_row(), raw_row(code = "280530")), economies = c("AUS", "JPN"))
  refetched <- rbind(base, base[1, ])
  out <- comtrade_dyads_collapse_duplicates(refetched)
  expect_equal(nrow(out), 2L)
  expect_equal(sum(out$primary_value), 200)
  expect_equal(attr(out, "n_identical"), 1L)

  other <- base[1, ]
  other$partner_code <- 999L
  other$primary_value <- 50
  out2 <- comtrade_dyads_collapse_duplicates(rbind(base, other))
  expect_equal(nrow(out2), 2L)
  expect_equal(out2$primary_value[out2$cmd_code == "854140"], 150)
})

test_that("validation flags duplicate keys, aggregates and self-dyads", {
  ok <- comtrade_dyads_standardize(rbind(raw_row(), raw_row(code = "280530")), economies = c("AUS", "JPN"))
  expect_length(comtrade_dyads_validate(ok, c("AUS", "JPN")), 0)
  expect_match(comtrade_dyads_validate(rbind(ok, ok[1, ]), c("AUS", "JPN")), "duplicate keys", all = FALSE)
  # Re-imports (reporter == partner) are kept as reported, like the allied file, unless refused.
  self <- ok
  self$partner_iso[1] <- "AUS"
  expect_length(comtrade_dyads_validate(self, c("AUS", "JPN")), 0)
  expect_match(comtrade_dyads_validate(self, c("AUS", "JPN"), allow_self = FALSE), "self-dyads", all = FALSE)
  expect_match(comtrade_dyads_validate(ok, "AUS"), "partners outside", all = FALSE)
})

test_that("query length counts percent-encoded commas", {
  expect_equal(comtrade_dyads_query_chars(c(36, 392), c("854140", "280530")), 110L + 15L + 8L)
  groups <- comtrade_dyads_code_groups(sprintf("%06d", 1:298), 2)
  expect_equal(lengths(groups), c(149L, 149L))
  expect_lt(comtrade_dyads_query_chars(1:50, groups[[1]]), COMTRADE_DYADS_MAX_QUERY_CHARS)
})
