# The HS6 crosswalk consolidation.
#
# Three differently-shaped crosswalk files used to be staged independently and had
# drifted: on shared codes only 60-81% agreed on the technology assignment, so ES/EO
# and PSI were scored on different definitions of the same technologies. They are now
# views generated from data/reference/energy_hs6_master.csv. These tests pin the views
# to the column contracts their consumers actually parse.

opsi_hs6_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

opsi_load_hs6 <- function() {
  source(file.path(opsi_hs6_root(), "R", "utils", "hs6_crosswalk.R"), local = FALSE)
}

opsi_hs6_master <- function() {
  path <- file.path(opsi_hs6_root(), "data", "reference", "energy_hs6_master.csv")
  skip_if_not(file.exists(path), "HS6 master not present")
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("the master is present and well formed", {
  opsi_load_hs6()
  master <- opsi_hs6_master()

  expect_true(all(HS6_MASTER_COLUMNS %in% names(master)))
  expect_gt(nrow(master), 100)
  expect_silent(hs6_validate_master(master))

  long <- hs6_normalize_master(master)
  expect_true(all(nchar(long$hs6) == 6))
  expect_false(any(is.na(long$hs6)))
  expect_type(long$essential, "logical")
  # Sub-sector text in the source export contains embedded newlines.
  expect_false(any(grepl("[\r\n]", long$sub_sector)))
})

test_that("codes are normalised to six characters", {
  opsi_load_hs6()

  expect_equal(hs6_clean_code(c("850760", 850760, " 85076 0 ")), rep("850760", 3))
  expect_equal(hs6_clean_code("8507"), "008507")
  expect_true(is.na(hs6_clean_code("")))
  expect_true(is.na(hs6_clean_code("abc")))
})

test_that("every view carries the columns its consumer parses", {
  opsi_load_hs6()
  views <- hs6_build_views(opsi_hs6_master())

  # 05_ingest_sources.R uses subcat$HS6; 10_build_themes.R renames
  # Technology -> tech and "Value Chain" -> supply_chain.
  bolstered <- views[["hts_codes_categories_bolstered_final.csv"]]
  expect_true(all(c("Technology", "Value Chain", "Sub.Sector", "HS6") %in% names(bolstered)))

  # R/charts/35_trade_bloc_counterfactual.R reads the lower-case shape.
  consolidated <- views[["consolidated_hs6_energy_tech_long.csv"]]
  expect_true(all(c("tech", "supply_chain", "sub_sector", "HS6") %in% names(consolidated)))

  # 10_build_themes.R renames "Value Chain" -> Value.Chain, then
  # prepare_subcat_mapping() needs an HS6 column AND an essential column before it
  # will apply the essential-goods override at all.
  essential <- views[["hs6_categories_with_essential.csv"]]
  expect_true(all(c("Technology", "Value Chain", "HS6", "essential") %in% names(essential)))
  expect_type(essential$essential, "logical")

  for (v in views) expect_gt(nrow(v), 100)
})

test_that("views agree with each other and with the master on every code", {
  opsi_load_hs6()
  master_long <- hs6_normalize_master(opsi_hs6_master())
  views <- hs6_build_views(opsi_hs6_master())

  master_codes <- sort(unique(master_long$hs6))
  for (name in names(views)) {
    expect_equal(sort(unique(views[[name]]$HS6)), master_codes, info = name)
  }

  tech_of <- function(df, code_col, tech_col) {
    s <- split(trimws(df[[tech_col]]), df[[code_col]])
    lapply(s, function(x) sort(unique(x)))
  }

  from_master <- tech_of(master_long, "hs6", "tech")
  a <- tech_of(views[["hts_codes_categories_bolstered_final.csv"]], "HS6", "Technology")
  b <- tech_of(views[["consolidated_hs6_energy_tech_long.csv"]], "HS6", "tech")

  # The drift that motivated this consolidation must now be impossible.
  expect_equal(a, from_master)
  expect_equal(b, from_master)
  expect_equal(a, b)
})

test_that("read.csv round-trips the views into the names consumers expect", {
  opsi_load_hs6()
  views <- hs6_build_views(opsi_hs6_master())

  dir <- file.path(tempdir(), paste0("hs6-", basename(tempfile(""))))
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  path <- file.path(dir, "bolstered.csv")
  write.csv(views[["hts_codes_categories_bolstered_final.csv"]], path, row.names = FALSE, na = "")

  # 10_build_themes.R uses readr::read_csv, which preserves the space in "Value Chain".
  skip_if_not_installed("readr")
  rt <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  expect_true(all(c("Technology", "Value Chain", "HS6") %in% names(rt)))

  renamed <- dplyr::rename(rt, tech = "Technology", supply_chain = "Value Chain")
  expect_true(all(c("tech", "supply_chain") %in% names(renamed)))

  # HS6 must survive as a usable 6-character code, not lose a leading zero.
  codes <- hs6_clean_code(rt$HS6)
  expect_false(any(is.na(codes)))
  expect_true(all(nchar(codes) == 6))
})

test_that("the essential view unlocks the NIPO essential-goods override", {
  opsi_load_hs6()
  views <- hs6_build_views(opsi_hs6_master())
  essential <- views[["hs6_categories_with_essential.csv"]]

  # prepare_subcat_mapping() resolves these column names and only applies the
  # override when both an HS6 column and an essential column are found.
  ess_names <- names(essential)
  hs6_col <- c("HS6", "hs6", "code")[c("HS6", "hs6", "code") %in% ess_names][1]
  essential_col <- c("essential", "essential_for_tech_sc")[
    c("essential", "essential_for_tech_sc") %in% ess_names][1]

  expect_false(is.na(hs6_col))
  expect_false(is.na(essential_col))

  # Both states must be represented, otherwise the flag carries no information.
  expect_true(any(essential[[essential_col]]))
  expect_true(any(!essential[[essential_col]]))
})

test_that("the generated views are not version controlled", {
  opsi_load_hs6()
  reference <- file.path(opsi_hs6_root(), "data", "reference")
  skip_if_not(dir.exists(reference), "no reference dir")

  # Only the master belongs in version control; the views are pipeline output.
  for (name in names(hs6_view_builders())) {
    expect_false(
      file.exists(file.path(reference, name)),
      info = paste0(name, " is a generated view and must not be committed to data/reference")
    )
  }

  expect_true(file.exists(file.path(reference, "energy_hs6_master.csv")))
})

test_that("a key flagged essential in any row is essential, and appears once", {
  opsi_load_hs6()
  master <- data.frame(
    tech = c("Batteries", "Batteries", "Solar"),
    supply_chain = "Midstream",
    sub_sector = c("Cells", "Cells", "Modules"),
    hs6 = c("850760", "850760", "854143"),
    essential = c(FALSE, TRUE, NA),
    stringsAsFactors = FALSE
  )

  long <- hs6_normalize_master(master)
  expect_equal(nrow(long), 2)
  expect_true(long$essential[long$hs6 == "850760"])
  expect_false(long$essential[long$hs6 == "854143"])
  # A master without the provenance columns reads as all-current codes.
  expect_false(any(long$legacy_hs))
})

test_that("the master carries one row per key and one sub-sector per code and stage", {
  opsi_load_hs6()
  long <- hs6_normalize_master(opsi_hs6_master())

  expect_false(anyDuplicated(long[, c("tech", "supply_chain", "sub_sector", "hs6")]) > 0)
  # include_sub_sector would count a code twice if it sat in two sub-sectors of a cell.
  expect_false(anyDuplicated(long[, c("tech", "supply_chain", "hs6")]) > 0)

  # The essential view is what NIPO reads; it must not reintroduce the conflicting pairs.
  essential <- hs6_build_views(opsi_hs6_master())[["hs6_categories_with_essential.csv"]]
  expect_false(anyDuplicated(essential[, c("Technology", "Value Chain", "Sub.Sector", "HS6")]) > 0)
})

test_that("raw materials are filed Upstream, not in a manufacturing stage", {
  opsi_load_hs6()
  long <- hs6_normalize_master(opsi_hs6_master())

  # Ores and concentrates (chapter 26, natural graphite 2504, mineral ores 2530), and
  # waste and scrap (precious metals 7112, nickel 7503, cobalt 8105.30, e-waste 8549).
  raw_material <- grepl("^(26|2504|2530|7112|7503|810530|8549)", long$hs6)
  expect_true(any(raw_material))
  expect_true(all(long$supply_chain[raw_material] == "Upstream"))

  battery_mid <- long$hs6[long$tech == "Batteries" & long$supply_chain == "Midstream"]
  battery_up <- long$hs6[long$tech == "Batteries" & long$supply_chain == "Upstream"]
  expect_false(any(c("260400", "260500", "250410") %in% battery_mid))
  expect_true(all(c("260400", "260500", "250410") %in% battery_up))
  # Refined battery chemicals and cells stay Midstream.
  expect_true(all(c("282520", "283691", "850760") %in% battery_mid))

  # Untreated softwood logs are not solar, wind or grid components.
  expect_false(any(long$hs6 %in% c("440321", "440323", "440324", "440325")))
})

test_that("legacy codes bridge the HS92 Atlas panel and are flagged", {
  opsi_load_hs6()
  long <- hs6_normalize_master(opsi_hs6_master())

  expect_type(long$legacy_hs, "logical")
  legacy <- long[long$legacy_hs, , drop = FALSE]
  expect_gt(nrow(legacy), 0)

  # PV cells and modules only exist as 8541.42/43 from HS2022; 8541.40 carries them before.
  solar_mid <- long[long$tech == "Solar" & long$supply_chain == "Midstream", ]
  expect_true(all(c("854140", "854142", "854143") %in% solar_mid$hs6))
  expect_true(solar_mid$legacy_hs[solar_mid$hs6 == "854140"])
  expect_false(any(solar_mid$legacy_hs[solar_mid$hs6 %in% c("854142", "854143")]))

  # Every legacy code sits beside at least one current code of the same cell, so it only
  # ever extends a basket rather than defining one.
  cells_with_current <- unique(paste(long$tech[!long$legacy_hs], long$supply_chain[!long$legacy_hs]))
  expect_true(all(paste(legacy$tech, legacy$supply_chain) %in% cells_with_current))

  # Residual "n.e.s." headings are not bridged: HS92 3823.90 (chemical products n.e.s.) is
  # wider than any battery input and would be a fifth of the Atlas battery basket.
  expect_false("382390" %in% legacy$hs6)
})

test_that("hs6_match_report counts the codes a source cannot see", {
  opsi_load_hs6()
  long <- hs6_normalize_master(data.frame(
    tech = c("Solar", "Solar", "Wind"),
    supply_chain = "Midstream",
    sub_sector = "x",
    hs6 = c("854142", "854140", "850231"),
    essential = TRUE,
    legacy_hs = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ))

  report <- hs6_match_report(long, c("854140", 850230))
  expect_equal(report$n_codes, 3)
  expect_equal(report$n_unmatched, 2)
  expect_equal(report$n_legacy, 1)
  expect_setequal(report$unmatched$hs6, c("854142", "850231"))
})
