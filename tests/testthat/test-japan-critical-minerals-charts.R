# Japan critical minerals chart helpers.
#
# The figures themselves are not asserted on - they are ggplot objects over staged raw
# data - but the transformations underneath them carry real judgement calls: which HS
# chapter counts as which processing stage, what happens when the IEA workbook omits a
# Total row, and which Comtrade vintage is usable. Those are covered here on synthetic
# fixtures, so they always run.

opsi_jcm_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap or its runner.
opsi_load_jcm <- function() {
  path <- file.path(opsi_jcm_root(), "R", "charts", "japan_critical_minerals_security.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c(
    "JCM_PALETTE", "JCM_FOCUS_COLOUR", "JCM_CHINA_COLOUR", "JCM_OTHER_COLOUR",
    "JCM_ROW_COLOUR", "JCM_IEA_SUMMARY_ROWS", "JCM_POLICY_PEERS",
    "JCM_POLICY_COLOURS", "JCM_STAGE_LEVELS",
    "jcm_processing_stage", "jcm_hs6", "jcm_critmin_paths", "jcm_iea_shares",
    "jcm_fig1_data", "jcm_fig2_concentration", "jcm_fig2_focus_output",
    "jcm_supplier_exposure", "jcm_stage_flows"
  )
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("processing stage separates ore from chemistry from metal", {
  opsi_load_jcm()

  expect_equal(jcm_processing_stage("260300"), JCM_STAGE_LEVELS[1])  # copper ore
  expect_equal(jcm_processing_stage("250410"), JCM_STAGE_LEVELS[1])  # natural graphite
  expect_equal(jcm_processing_stage("282520"), JCM_STAGE_LEVELS[2])  # lithium hydroxide
  expect_equal(jcm_processing_stage("391000"), JCM_STAGE_LEVELS[2])  # silicones
  expect_equal(jcm_processing_stage("750210"), JCM_STAGE_LEVELS[3])  # unwrought nickel
  expect_equal(jcm_processing_stage("811100"), JCM_STAGE_LEVELS[3])  # manganese articles

  # A chapter the rule does not cover must fall out as NA rather than land in a bucket
  # by accident; the runner reports how much value that is.
  expect_true(is.na(jcm_processing_stage("870380")))
  expect_equal(jcm_processing_stage(c("260300", "870380")), c(JCM_STAGE_LEVELS[1], NA))
})

test_that("HS codes are zero-padded to six characters", {
  opsi_load_jcm()

  expect_equal(jcm_hs6(260300), "260300")
  expect_equal(jcm_hs6("260300"), "260300")
  # Comtrade codes that lost a leading zero to numeric coercion must come back intact.
  expect_equal(jcm_hs6(28461), "028461")
})

jcm_supply_fixture <- function() {
  # Two mineral-stages: one with a published Total row, one without.
  d <- data.frame(
    panel = c(rep("Lithium", 4), rep("Cobalt", 3)),
    stage_raw = c(rep("Refining", 4), rep("Refining", 3)),
    stage = "Refine / process",
    entity = c("China", "Japan", "Rest of world", "Total", "China", "Japan", "Rest of world"),
    base = c(60, 10, 30, 100, 80, 20, 100),
    horizon = c(50, 20, 30, 100, 70, 30, 100),
    stringsAsFactors = FALSE
  )
  d$stage <- factor(d$stage, levels = c("Mine", "Refine / process"))
  d
}

test_that("shares use the published Total, and fall back to the producer sum without one", {
  opsi_load_jcm()

  shares <- expect_message(
    jcm_iea_shares(jcm_supply_fixture()),
    "no Total row for Cobalt"
  )

  # Summary rows are never producers.
  expect_false(any(shares$entity %in% JCM_IEA_SUMMARY_ROWS))

  lithium <- shares[shares$panel == "Lithium", ]
  expect_equal(lithium$share_base[lithium$entity == "China"], 0.6)
  expect_equal(sum(lithium$share_base), 1)

  # Cobalt has no Total row; the denominator is the producer sum, 200.
  cobalt <- shares[shares$panel == "Cobalt", ]
  expect_equal(cobalt$share_base[cobalt$entity == "Japan"], 0.1)
  expect_equal(sum(cobalt$share_base), 1)
})

test_that("figure 1 groups to four buckets that sum to the whole stage", {
  opsi_load_jcm()

  shares <- suppressMessages(jcm_iea_shares(jcm_supply_fixture()))
  fig1 <- jcm_fig1_data(shares, focus_name = "Japan")

  expect_setequal(
    as.character(unique(fig1$group)),
    c("China", "Japan", "Other named producers", "Rest of world")
  )

  totals <- stats::aggregate(share ~ panel + stage, data = fig1, FUN = sum)
  expect_true(all(abs(totals$share - 1) < 1e-12))

  # Only published panel/stage pairs survive - nothing is invented for the mine stage,
  # which this fixture has no rows for.
  expect_setequal(as.character(unique(fig1$stage)), "Refine / process")
})

test_that("figure 1 reports a zero share for a focal country that is absent", {
  opsi_load_jcm()

  shares <- suppressMessages(jcm_iea_shares(jcm_supply_fixture()))
  fig1 <- jcm_fig1_data(shares, focus_name = "Korea")

  korea <- fig1[fig1$group == "Korea", ]
  expect_equal(nrow(korea), 2)
  expect_true(all(korea$share == 0))
  # Japan's supply has to go somewhere - it becomes another named producer, not a gap.
  other <- fig1[fig1$group == "Other named producers" & fig1$panel == "Lithium", ]
  expect_equal(other$share, 0.1)
})

test_that("supplier exposure weights each commodity's export HHI by the importer's spend", {
  opsi_load_jcm()

  exports <- data.frame(
    reporter_iso = c("X", "Y", "X", "Z"),
    hs6 = c("A", "A", "B", "B"),
    cmd_desc = "d",
    primary_value = c(80, 20, 50, 50),
    stringsAsFactors = FALSE
  )
  imports <- data.frame(
    reporter_iso = "JPN",
    hs6 = c("A", "B"),
    cmd_desc = "d",
    primary_value = c(100, 300),
    stringsAsFactors = FALSE
  )

  out <- jcm_supplier_exposure(imports, exports)

  # Commodity A: shares .8/.2 -> HHI .68, top supplier X. Commodity B: .5/.5 -> HHI .5.
  a <- out$by_commodity[out$by_commodity$hs6 == "A", ]
  expect_equal(a$supplier_hhi, 0.68)
  expect_equal(a$top_supplier_share, 0.8)
  expect_equal(a$top_supplier_iso, "X")
  expect_equal(a$n_suppliers, 2)

  # Weighted by the $100 / $300 basket: (.68*100 + .5*300) / 400.
  jpn <- out$by_importer[out$by_importer$reporter_iso == "JPN", ]
  expect_equal(jpn$weighted_supplier_hhi, 0.545)
  expect_equal(jpn$weighted_top_supplier_share, 0.575)
  expect_equal(jpn$import_bill_usd, 400)
  expect_equal(jpn$n_commodities, 2)
})

test_that("stage flows net exports against imports and keep every stage", {
  opsi_load_jcm()

  flows <- rbind(
    data.frame(reporter_iso = "JPN", hs6 = "260300", stage = JCM_STAGE_LEVELS[1],
               flow = "Imports", primary_value = 15, stringsAsFactors = FALSE),
    data.frame(reporter_iso = "JPN", hs6 = "750210", stage = JCM_STAGE_LEVELS[3],
               flow = "Imports", primary_value = 4, stringsAsFactors = FALSE),
    data.frame(reporter_iso = "JPN", hs6 = "750210", stage = JCM_STAGE_LEVELS[3],
               flow = "Exports", primary_value = 10, stringsAsFactors = FALSE),
    data.frame(reporter_iso = "KOR", hs6 = "260300", stage = JCM_STAGE_LEVELS[1],
               flow = "Imports", primary_value = 99, stringsAsFactors = FALSE)
  )

  out <- jcm_stage_flows(flows[flows$flow == "Imports", ], flows[flows$flow == "Exports", ],
                         focus_iso = "JPN")

  ore <- out[out$stage == JCM_STAGE_LEVELS[1], ]
  expect_equal(ore$Imports, 15)
  # A stage with no exports must read as zero, not drop out or come back NA.
  expect_equal(ore$Exports, 0)
  expect_equal(ore$net, -15)

  metal <- out[out$stage == JCM_STAGE_LEVELS[3], ]
  expect_equal(metal$net, 6)

  # Korea's ore purchase must not leak into Japan's row.
  expect_equal(sum(out$Imports), 19)
})

test_that("the newest Comtrade vintage is skipped when it is still a stub", {
  opsi_load_jcm()

  dir <- withr::local_tempdir()
  write_pull <- function(year, flow, reporters) {
    utils::write.csv(
      data.frame(
        ref_year = year, reporter_iso = reporters, cmd_code = "260300",
        cmd_desc = "Copper ores and concentrates", primary_value = 1,
        stringsAsFactors = FALSE
      ),
      file.path(dir, sprintf("critmin_%s_%d.csv", flow, year)), row.names = FALSE
    )
  }

  full <- c("JPN", sprintf("R%02d", 1:9))
  write_pull(2024, "import", full)
  write_pull(2024, "export", full)
  # Comtrade publishes a year reporter by reporter, so the freshest file is a stub.
  write_pull(2025, "import", c("BRA", "PRY"))
  write_pull(2025, "export", c("BRA", "PRY"))

  paths <- expect_message(
    jcm_critmin_paths(dir, "JPN", min_reporters = 10L),
    "using 2024; skipped 2025"
  )
  expect_equal(paths$year, 2024)
  expect_equal(basename(paths$import), "critmin_import_2024.csv")

  # A year present in only one flow cannot be used, however fresh it is.
  write_pull(2026, "import", full)
  paths <- suppressMessages(jcm_critmin_paths(dir, "JPN", min_reporters = 10L))
  expect_equal(paths$year, 2024)

  # And a focal reporter missing from every vintage is an error, not a silent fallback.
  expect_error(
    suppressMessages(jcm_critmin_paths(dir, "ZWE", min_reporters = 10L)),
    "no ZWE"
  )
})
