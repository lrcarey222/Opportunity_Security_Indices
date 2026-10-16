# Shared machinery and per-technology logic behind the nuclear and grid chart scripts.
#
# The figures are ggplot objects over staged raw data and are not asserted on. What is
# covered here is the reasoning underneath them: how the commodity universe is built from
# a crosswalk that is duplicated and many-to-many, the concentration and exposure maths,
# and the two scripts' own decisions about which codes belong in a technology.

opsi_ts_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# tech_security_common.R has no bootstrap block, so it can be sourced whole. It reads
# `config` from the global environment inside its function bodies only, which the fixture
# helper below stands in for.
opsi_load_ts <- function() {
  source(file.path(opsi_ts_root(), "R", "charts", "tech_security_common.R"),
         local = globalenv())
  invisible(TRUE)
}

# The chart scripts bootstrap at the top level, so only their definitions are evaluated.
opsi_load_chart_defs <- function(file, wanted) {
  exprs <- parse(file.path(opsi_ts_root(), "R", "charts", file))
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = globalenv())
    }
  }
  invisible(TRUE)
}

# A throwaway repo root holding just a crosswalk, with `config` pointed at it.
opsi_with_crosswalk <- function(rows, code) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "raw"))
  utils::write.csv(rows, file.path(dir, "raw", "hs6_categories_with_essential.csv"),
                   row.names = FALSE)

  old <- if (exists("config", envir = globalenv())) get("config", envir = globalenv())
  assign("config", list(raw_data_dir = "raw", processed_dir = "processed"),
         envir = globalenv())
  withr::defer(
    if (is.null(old)) rm("config", envir = globalenv()) else
      assign("config", old, envir = globalenv()),
    envir = parent.frame()
  )
  code(dir)
}

CROSSWALK_FIXTURE <- data.frame(
  Technology = c("Nuclear", "Nuclear", "Nuclear", "Nuclear",
                 "Electric Grid", "Electric Grid", "Electric Grid", "Coal"),
  Value.Chain = c("Midstream", "Midstream", "Midstream", "Midstream",
                  "Midstream", "Upstream", "Midstream", "Midstream"),
  Sub.Sector = c("Nuclear Fuel Cycle", "Nuclear Fuel Cycle", "Steam Cycle & Balance of Plant",
                 "Steam Cycle & Balance of Plant", "Transformers", "Electricity",
                 "Grid Cables & Accessories", "Steam Cycle & Balance of Plant"),
  # 284410 is duplicated exactly, as the real file duplicates 16 rows; 840690 is shared
  # with Coal, so it must not come back as nuclear-exclusive.
  HS6 = c(284410, 284410, 840690, 840211, 850423, 271600, 440321, 840690),
  essential = TRUE,
  stringsAsFactors = FALSE
)

test_that("ts_hs6 pads with zeros, not spaces, and leaves NA alone", {
  opsi_load_ts()

  expect_equal(ts_hs6(260300), "260300")
  expect_equal(ts_hs6(28461), "028461")
  expect_equal(ts_hs6(c("850440", NA)), c("850440", NA))
})

test_that("the commodity universe deduplicates and flags exclusivity", {
  opsi_load_ts()

  opsi_with_crosswalk(CROSSWALK_FIXTURE, function(dir) {
    nuc <- ts_tech_codes(dir, "Nuclear")

    # Four rows collapse to three codes: 284410 appears twice in the fixture.
    expect_equal(nrow(nuc), 3)
    expect_setequal(nuc$hs6, c("284410", "840690", "840211"))

    # 840690 is also filed under Coal, so it is not nuclear-exclusive; the other two are.
    expect_false(nuc$exclusive[nuc$hs6 == "840690"])
    expect_true(all(nuc$exclusive[nuc$hs6 %in% c("284410", "840211")]))

    expect_equal(nuc$sub_sector[nuc$hs6 == "284410"], "Nuclear Fuel Cycle")
  })
})

test_that("a code mapped to two sub-sectors within one technology stops the run", {
  opsi_load_ts()

  bad <- rbind(CROSSWALK_FIXTURE, data.frame(
    Technology = "Nuclear", Value.Chain = "Midstream", Sub.Sector = "Reactor Components",
    HS6 = 284410, essential = TRUE, stringsAsFactors = FALSE
  ))

  opsi_with_crosswalk(bad, function(dir) {
    expect_error(ts_tech_codes(dir, "Nuclear"), "more than one sub-sector")
  })
})

test_that("exclude_codes drops commodities from the universe", {
  opsi_load_ts()

  opsi_with_crosswalk(CROSSWALK_FIXTURE, function(dir) {
    nuc <- ts_tech_codes(dir, "Nuclear", exclude_codes = "284410")
    expect_false("284410" %in% nuc$hs6)
    expect_equal(nrow(nuc), 2)
  })
})

test_that("commodity concentration reports HHI, top supplier and breadth", {
  opsi_load_ts()

  exports <- data.frame(
    hs6 = c("A", "A", "B", "B", "B", "B"),
    reporter_iso = c("X", "Y", "P", "Q", "R", "S"),
    value = c(80, 20, 25, 25, 25, 25),
    stringsAsFactors = FALSE
  )
  out <- ts_commodity_concentration(exports)

  a <- out[out$hs6 == "A", ]
  expect_equal(a$supplier_hhi, 0.68)          # .8^2 + .2^2
  expect_equal(a$top_supplier_share, 0.8)
  expect_equal(a$top_supplier_iso, "X")
  expect_equal(a$top3_share, 1)               # only two suppliers exist
  expect_equal(a$n_suppliers, 2)
  expect_equal(a$world_exports, 100)

  b <- out[out$hs6 == "B", ]
  expect_equal(b$supplier_hhi, 0.25)          # four equal suppliers
  expect_equal(b$top3_share, 0.75)
  expect_equal(b$n_suppliers, 4)

  # Rows come back largest market first.
  expect_equal(out$hs6[1], "A")
})

test_that("supplier exposure weights commodity concentration by the importer's spend", {
  opsi_load_ts()

  conc <- data.frame(
    hs6 = c("A", "B"), supplier_hhi = c(0.68, 0.50),
    top_supplier_share = c(0.8, 0.5), stringsAsFactors = FALSE
  )
  imports <- data.frame(
    reporter_iso = c("JPN", "JPN", "KOR"),
    hs6 = c("A", "B", "A"),
    value = c(100, 300, 50),
    stringsAsFactors = FALSE
  )

  out <- ts_supplier_exposure(imports, conc)

  jpn <- out[out$reporter_iso == "JPN", ]
  expect_equal(jpn$weighted_supplier_hhi, 0.545)   # (.68*100 + .5*300) / 400
  expect_equal(jpn$weighted_top_supplier_share, 0.575)
  expect_equal(jpn$import_bill_usd, 400)
  expect_equal(jpn$n_commodities, 2)

  # Korea buys only the concentrated commodity, so it scores that commodity's own HHI.
  expect_equal(out$weighted_supplier_hhi[out$reporter_iso == "KOR"], 0.68)
  # Ordered by bill, largest first.
  expect_equal(out$reporter_iso[1], "JPN")
})

test_that("grid sub-sectors all map to a component family, and unknowns fail loudly", {
  opsi_load_ts()
  opsi_load_chart_defs(
    "japan_grid_components_security.R",
    c("JGS_TECH", "JGS_FAMILIES", "JGS_FAMILY_LEVELS", "JGS_FAMILY_COLOURS",
      "JGS_SHORT_NAMES", "JGS_ELECTRICITY_CODE", "JGS_TIMBER_PREFIX",
      "jgs_family", "jgs_short_label", "jgs_codes")
  )

  # Every sub-sector the real crosswalk uses for grid must be covered by a family, or
  # figure 1 silently loses a slice of the market.
  cw <- utils::read.csv(
    file.path(opsi_ts_root(), "data", "raw", "hs6_categories_with_essential.csv"),
    stringsAsFactors = FALSE
  )
  grid_subsectors <- unique(cw$Sub.Sector[cw$Technology == "Electric Grid"])
  mapped <- unlist(JGS_FAMILIES, use.names = FALSE)
  expect_setequal(setdiff(grid_subsectors, "Electricity"), mapped)

  expect_equal(as.character(jgs_family("Transformers")), "Transformers")
  expect_equal(as.character(jgs_family("Surge Arresters")), "Switchgear & protection")
  expect_error(jgs_family("Something New"), "not mapped to a component family")

  # Every colour in the palette names a real family, and every family has a colour.
  expect_setequal(names(JGS_FAMILY_COLOURS), JGS_FAMILY_LEVELS)
})

test_that("the grid universe drops electricity and softwood unless asked not to", {
  opsi_load_ts()
  opsi_load_chart_defs(
    "japan_grid_components_security.R",
    c("JGS_TECH", "JGS_ELECTRICITY_CODE", "JGS_TIMBER_PREFIX", "jgs_codes")
  )

  opsi_with_crosswalk(CROSSWALK_FIXTURE, function(dir) {
    codes <- jgs_codes(dir)

    # 850423 is equipment and stays; 271600 is the Upstream electricity row and 440321 is
    # a chapter-44 softwood heading, so both are marked out of universe with a reason.
    expect_true(codes$in_universe[codes$hs6 == "850423"])
    expect_false(codes$in_universe[codes$hs6 == "271600"])
    expect_false(codes$in_universe[codes$hs6 == "440321"])
    expect_match(codes$dropped_reason[codes$hs6 == "271600"], "electricity")
    expect_match(codes$dropped_reason[codes$hs6 == "440321"], "softwood")

    # Nothing is deleted - the exclusions stay in the CSV so the decision is auditable.
    expect_equal(nrow(codes), 3)

    kept <- jgs_codes(dir, keep_all = TRUE)
    expect_true(all(kept$in_universe))
    expect_true(all(is.na(kept$dropped_reason)))
  })
})

test_that("component labels distinguish headings that share a description", {
  opsi_load_ts()
  opsi_load_chart_defs(
    "japan_grid_components_security.R", c("JGS_SHORT_NAMES", "jgs_short_label")
  )

  # The seven transformer headings all begin "Electrical transformers" in Comtrade, so a
  # label taken from the description alone cannot tell them apart.
  desc <- rep("Electrical transformers; having a power handling capacity...", 3)
  labels <- jgs_short_label(c("850421", "850423", "850434"), desc)
  expect_equal(length(unique(labels)), 3)

  # A code with no curated name falls back to the description rather than to NA.
  fallback <- jgs_short_label("999999", "Widgets; assorted, of base metal")
  expect_equal(fallback, "Widgets")
})

test_that("nuclear labels cover the fuel cycle and fall back elsewhere", {
  opsi_load_ts()
  opsi_load_chart_defs(
    "japan_nuclear_security.R",
    c("JNS_TECH", "JNS_SUBSECTOR_LEVELS", "JNS_SUBSECTOR_LABELS",
      "JNS_SUBSECTOR_COLOURS", "JNS_SHORT_NAMES", "jns_short_label",
      "jns_label_subsector")
  )

  expect_equal(jns_short_label("284420", "Uranium; enriched in U235, ..."),
               "Enriched uranium & plutonium")
  expect_equal(jns_short_label("840690", "Turbines; parts of steam and other vapour"),
               "Turbines")

  # The three sub-sectors the crosswalk uses for nuclear all have a display label and a
  # colour, in the same order.
  cw <- utils::read.csv(
    file.path(opsi_ts_root(), "data", "raw", "hs6_categories_with_essential.csv"),
    stringsAsFactors = FALSE
  )
  expect_setequal(unique(cw$Sub.Sector[cw$Technology == "Nuclear"]), JNS_SUBSECTOR_LEVELS)
  expect_setequal(names(JNS_SUBSECTOR_COLOURS), unname(JNS_SUBSECTOR_LABELS))
  expect_equal(as.character(jns_label_subsector("Nuclear Fuel Cycle")), "Fuel cycle")
})

test_that("policy activity fills gap years with zero and stops at the last complete year", {
  opsi_load_ts()

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "processed"))
  this_year <- as.integer(format(Sys.Date(), "%Y"))

  panel <- data.frame(
    iso3 = c("JPN", "JPN", "JPN", "KOR"),
    country = c("Japan", "Japan", "Japan", "Korea"),
    tech = "Nuclear",
    supply_chain = "Midstream",
    # 2021 is missing for Japan, and the current (partial) year must be excluded.
    announce_year = c(2020L, 2022L, this_year, 2020L),
    n_policies_window = c(5L, 9L, 99L, 3L),
    stringsAsFactors = FALSE
  )
  saveRDS(panel, file.path(dir, "processed", "nipo_tech_year.rds"))

  old <- if (exists("config", envir = globalenv())) get("config", envir = globalenv())
  assign("config", list(raw_data_dir = "raw", processed_dir = "processed"),
         envir = globalenv())
  withr::defer(
    if (is.null(old)) rm("config", envir = globalenv()) else
      assign("config", old, envir = globalenv())
  )

  out <- ts_policy_activity(dir, "Nuclear", peers = c(JPN = "Japan", KOR = "Korea"),
                            from_year = 2020L)

  expect_equal(attr(out, "last_complete_year"), this_year - 1L)
  expect_false(any(out$year >= this_year))

  jpn <- out[out$iso3 == "JPN" & out$supply_chain == "Midstream", ]
  expect_equal(jpn$n_active_policies[jpn$year == 2021], 0L)
  expect_equal(jpn$n_active_policies[jpn$year == 2022], 9L)

  # Both peers and both stages are present for every year, so no line jumps a gap.
  expect_equal(
    nrow(out),
    2L * 2L * length(seq(2020L, this_year - 1L))
  )
})
