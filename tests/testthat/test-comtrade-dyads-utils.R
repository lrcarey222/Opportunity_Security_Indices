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

test_that("a supplementary pull takes only basket codes, and combined pulls must not share a code", {
  basket <- c("260300", "740311", "760110", "854140")
  expect_identical(comtrade_dyads_select_codes(basket), basket)
  expect_identical(comtrade_dyads_select_codes(basket, character()), basket)
  expect_identical(comtrade_dyads_select_codes(basket, c(" 760110", "740311", "740311")), c("740311", "760110"))
  expect_error(comtrade_dyads_select_codes(basket, c("740311", "740319")), "outside the HS6 basket: 740319")
  # Blank entries (a trailing comma) are ignored; an entry that is not a code is an error rather
  # than dropped, since dropping every entry would turn the request into the whole basket.
  expect_identical(comtrade_dyads_select_codes(basket, c("", "7403.11")), "740311")
  expect_error(comtrade_dyads_select_codes(basket, c("cathodes")), "not HS6 codes: cathodes")
  expect_error(comtrade_dyads_select_codes(basket, c("74O311")), "outside the HS6 basket: 074311")

  expect_length(comtrade_dyads_shared_codes(list(main = basket[c(1, 4)], extra = basket[2:3])), 0)
  expect_identical(comtrade_dyads_shared_codes(list(main = basket, extra = c("740311", "999999"))), "740311")

  # Every basket code must come from some directory: the main pull alone (planned before the
  # codes joined the basket) or the supplementary pull alone leaves codes out.
  expect_length(comtrade_dyads_uncovered_codes(basket, list(main = basket[c(1, 4)], extra = basket[2:3])), 0)
  expect_identical(comtrade_dyads_uncovered_codes(basket, list(main = basket[c(1, 4)])), c("740311", "760110"))
  expect_identical(comtrade_dyads_uncovered_codes(basket, list(extra = basket[2:3])), c("260300", "854140"))
  expect_length(comtrade_dyads_uncovered_codes(basket[1:2], list(main = basket)), 0)
})

world_row <- function(reporter = "AUS", code = "740311", flow = "X", value = 100, year = 2024L) {
  r <- raw_row(reporter = reporter, partner = "W00", code = code, value = value, flow = flow)
  r$partner_code <- 0L
  r$partner_desc <- "World"
  r$ref_year <- year
  r$period <- as.character(year)
  r
}

test_that("World totals are read from partner = everything chunks, totals only", {
  raw <- rbind(world_row(), world_row(flow = "M", code = "76011"), raw_row(), world_row(reporter = "JPN"))
  raw$customs_code[4] <- "C01"
  w <- comtrade_dyads_world_totals(raw)
  expect_equal(nrow(w), 2L)
  expect_equal(w$cmd_code, c("740311", "076011"))
  expect_equal(w$flow_direction, c("export", "import"))
  expect_true(all(w$partner_code == 0))
  expect_null(comtrade_dyads_world_totals(data.frame(count = 0)))
})

test_that("the energy-trade file is rebuilt from the pull's World rows, reporter-flow by reporter-flow", {
  # The staged file as scripts/98 reads it: codes and periods as text, flow_direction added.
  existing <- rbind(
    world_row(code = "260300", value = 5),                     # pulled, revised in the newer release
    world_row(reporter = "JPN", code = "260300", flow = "M"),  # pulled, unchanged
    world_row(code = "854140", value = 9),                     # pulled, gone from the newer release
    world_row(code = "440321", value = 3),                     # not pulled: kept
    world_row(reporter = "NZL", code = "260300", value = 4)    # reporter-flow the pull lacks: kept
  )
  existing$flow_direction <- ifelse(existing$flow_code == "X", "export", "import")
  rownames(existing) <- NULL
  pulled <- c("260300", "740311", "854140", "854142", "760110")

  world <- comtrade_dyads_world_totals(rbind(
    world_row(code = "260300", value = 7),
    world_row(reporter = "JPN", code = "260300", flow = "M"),
    world_row(code = "740311", value = 30),                    # a code the file lacked
    world_row(reporter = "CHL", code = "740311", value = 50),  # a reporter the file lacked
    world_row(reporter = "S19", code = "854142", value = 20),  # Taiwan, recoded to TWN
    world_row(code = "760110", year = 2020L),                  # not the file's year
    world_row(code = "271600")                                 # a code the pull did not request
  ))
  out <- comtrade_energy_trade_from_world(existing, world, pulled)
  ch <- attr(out, "changes")
  attr(out, "changes") <- NULL

  expect_identical(names(out), names(existing))
  expect_identical(vapply(out, class, character(1)), vapply(existing, class, character(1)))
  expect_setequal(paste(out$reporter_iso, out$flow_code, out$cmd_code, out$primary_value),
                  c("AUS X 260300 7", "JPN M 260300 100", "AUS X 740311 30", "CHL X 740311 50", "TWN X 854142 20",
                    "AUS X 440321 3", "NZL X 260300 4"))
  expect_equal(unname(ch$keys), c(1, 1, 3, 1, 1, 1))
  expect_equal(names(ch$keys), c("unchanged", "revised", "new", "dropped", "kept_outside_pull", "kept_reporter_flows"))
  expect_equal(ch$by_flow$reporters_before[ch$by_flow$flow_code == "X"], 2L)
  expect_equal(ch$by_flow$reporters_after[ch$by_flow$flow_code == "X"], 4L)
  # The kept rows are the file's rows as they were.
  kept <- out[out$cmd_code == "440321" | out$reporter_iso == "NZL", ]
  rownames(kept) <- NULL
  expected <- existing[c(4, 5), ]
  rownames(expected) <- NULL
  expect_identical(kept, expected)
  expect_true(all(out$flow_direction == ifelse(out$flow_code == "X", "export", "import")))

  # Rerunning on its own output changes nothing; Taiwan can be left out instead.
  again <- comtrade_energy_trade_from_world(out, world, pulled)
  expect_equal(attr(again, "changes")$keys[["unchanged"]], 5)
  attr(again, "changes") <- NULL
  expect_identical(again, out)
  no_twn <- comtrade_energy_trade_from_world(existing, world, pulled, taiwan = "drop")
  expect_false(any(no_twn$reporter_iso %in% c("TWN", "S19")))

  # A reporter fetched twice is an error rather than a doubled value, and a pull without the
  # file's year is an error rather than an empty file.
  expect_error(comtrade_energy_trade_from_world(existing, rbind(world, world[3, ]), pulled), "duplicate")
  expect_error(comtrade_energy_trade_from_world(existing, world[world$ref_year == 2020, ], pulled), "no requested code")
  expect_error(comtrade_energy_trade_from_world(existing, NULL, pulled), "No World rows")
})

test_that("trade values compare equal up to the digits write.csv keeps", {
  expect_equal(comtrade_energy_trade_same_value(c(340854851740.669, 1, NA, NA, 0), c(340854851740.66900634765625, 1.01, NA, 2, 0)),
               c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

test_that("total exports are refreshed for the fetched reporters, totals only, values as doubles", {
  total_row <- function(reporter, value, year = 2024L, flow = "X", customs = "C00") {
    r <- world_row(reporter = reporter, code = "TOTAL", value = value, flow = flow, year = year)
    r$customs_code <- customs
    r
  }
  existing <- rbind(total_row("AUS", 100), total_row("CPV", 5))
  existing$cifvalue <- NA  # all NA in the staged file, so read.csv makes it logical
  fetched <- rbind(
    total_row("AUS", 110),                  # revised
    total_row("S19", 300),                  # Taiwan, new
    total_row("JPN", 1, customs = "C01"),   # not a total
    total_row("JPN", 2, year = 2020L),      # not the year
    total_row("JPN", 3, flow = "M")         # not exports
  )
  out <- comtrade_total_export_refresh(existing, fetched, 2024)

  expect_equal(out$reporter_iso, c("AUS", "CPV", "TWN"))
  expect_equal(out$primary_value, c(110, 5, 300))
  expect_type(out$cifvalue, "double")
  expect_equal(out$cifvalue, c(110, NA, 300))
  expect_identical(names(out), names(existing))
  expect_equal(attr(out, "changes"), c(refreshed = 1, revised = 1, added = 1, kept = 1))

  none <- comtrade_total_export_refresh(existing, NULL, 2024)
  expect_equal(none$primary_value, existing$primary_value)
  expect_error(comtrade_total_export_refresh(existing, rbind(fetched, fetched[1, ]), 2024), "duplicate")
})
