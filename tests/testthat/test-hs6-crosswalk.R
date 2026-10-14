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
