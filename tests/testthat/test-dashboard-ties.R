# The ties chunks of the Strategic bundle (R/dashboard/dash_ties.R; scripts/85 "Bilateral ties"): the
# IMF positions are read with the outbound builder's mapping and filters, and ranking the shipped
# positions reproduces the builder's investment tie exactly; per-sector top partners; CM-FDI flows;
# Development Potential by sector; the chunk's shape. Synthetic inputs; no data needed.

ti_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(ti_root, "R", "utils", "scurve.R"))
source(file.path(ti_root, "R", "utils", "country.R"))
source(file.path(ti_root, "R", "utils", "schema.R"))
source(file.path(ti_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(ti_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(ti_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(ti_root, "R", "dashboard", "dash_osb.R"))
source(file.path(ti_root, "R", "dashboard", "dash_ties.R"))

ti_country_info <- function() {
  tibble::tibble(
    country = c("United States", "Japan", "India", "Netherlands", "Germany", "Brazil"),
    iso3c = c("USA", "JPN", "IND", "NLD", "DEU", "BRA"),
    income = "High income"
  )
}

ti_fdi <- function() {
  od <- DASH_DIP_MEASURES[["out_debt"]]
  ia <- DASH_DIP_MEASURES[["in_all"]]
  tibble::tibble(
    COUNTRY = c("United States", "United States", "United States", "United States", "United States", "Japan", "Japan", "United States", "United States", "India"),
    COUNTERPART_COUNTRY = c("Japan", "India", "Netherlands", "Germany", "World", "India", "United States", "Japan", "Brazil", "Japan"),
    DV_TYPE = c(rep("Reported official data", 9), "Estimated"),
    INDICATOR = c(od, od, od, od, od, od, od, ia, od, od),
    `2019` = c(1, 2, 3, 4, 5, 6, 7, 8, NA, 1),
    `2020` = c(50, 10, 900, NA, 99, 30, 70, 400, -5, 1),
    `2021` = c(70, NA, 950, 40, 99, NA, 90, 450, NA, 1),
    check.names = FALSE
  )
}

ti_gdp <- function() {
  tibble::tibble(iso3c = c("USA", "JPN", "IND", "NLD", "DEU", "BRA"), year = 2023, NY.GDP.MKTP.CD = c(27, 4, 3.5, 1.1, 4.4, 2.2) * 1e12)
}

test_that("positions: reported rows of the four series, ISO3 pairs, latest year and the index-window mean", {
  pos <- dash_dip_positions(ti_fdi(), ti_country_info(), index_years = 2020:2021)
  expect_false(any(pos$partner_iso %in% c(NA, "World")))
  expect_false("IND" %in% pos$reporter_iso) # only an estimated row
  us_jp <- pos[pos$reporter_iso == "USA" & pos$partner_iso == "JPN" & pos$measure == "out_debt", ]
  expect_equal(nrow(us_jp), 1)
  expect_equal(us_jp$year, 2021L)
  expect_equal(us_jp$value, 70)
  expect_equal(us_jp$mean_idx, 60)
  # a pair without the reporter's latest year keeps the year and an NA value, but its window mean
  us_in <- pos[pos$reporter_iso == "USA" & pos$partner_iso == "IND", ]
  expect_true(is.na(us_in$value))
  expect_equal(us_in$mean_idx, 10)
  # the inward series carries no index mean
  expect_true(all(is.na(pos$mean_idx[pos$measure == "in_all"])))
  expect_equal(pos$value[pos$measure == "in_all"], 450)
})

test_that("ranking the shipped positions reproduces the outbound builder's investment tie", {
  ci <- ti_country_info()
  for (scope in c("both", "counterpart")) {
    conduits <- c("NLD")
    built <- partnership_strength_build_outbound_edges(ti_fdi(), ci, ti_gdp(), gdp_year = 2023, years = 2020:2021, conduit_iso = conduits, conduit_scope = scope)
    pos <- dash_dip_positions(ti_fdi(), ci, index_years = 2020:2021)
    mine <- dash_dip_index(pos, ti_gdp(), 2023, conduits, scope)
    U <- ci$iso3c
    expect_identical(dash_dyad_matrix(mine, U), dash_dyad_matrix(built, U))
  }
})

test_that("top partners: sovereign, not the reporter, highest first, rank with ties at the lowest", {
  U <- c("A", "B", "C", "D", "E")
  M <- matrix(NA_real_, 5, 5, dimnames = list(U, U))
  M[1, ] <- c(0.9, 0.5, 0.7, 0.7, 0.99)
  sov <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
  top <- dash_top_partners(M, U, sov, k = 2)
  expect_equal(top$partner_iso, c("C", "D"))
  expect_equal(top$rank, c(1L, 1L))
  expect_equal(top$n, c(3L, 3L))
  expect_equal(unique(top$reporter_iso), "A")
  psi <- tibble::tibble(reporter_iso = "A", partner_iso = c("C", "D"), rank = c(4L, 2L))
  expect_equal(dash_psi_rank_of(top, psi), c(4L, 2L))
})

test_that("per-sector chunks: one per cell, named by the shared slug rule", {
  U <- c("A", "B", "C")
  cells <- tibble::tibble(cell_id = 1:2, tech = c("Electric Vehicles", "Solar"), supply_chain = c("Midstream", "Upstream"))
  fs <- array(runif(18), c(3, 3, 2))
  po <- array(runif(18), c(3, 3, 2))
  ch <- dash_cell_chunks(list(fs_index = fs, po_index = po), tibble::tibble(reporter_iso = character(), partner_iso = character(), rank = integer()), U, rep(TRUE, 3), cells, k = 1)
  expect_equal(names(ch), c("pcell-electric-vehicles-midstream", "pcell-solar-upstream"))
  t1 <- ch[[1]]$top
  expect_setequal(unique(t1$side), c("fs", "po"))
  expect_equal(nrow(t1), 6) # 3 reporters x 2 sides x top 1
  r <- t1[t1$reporter_iso == "A" & t1$side == "fs", ]
  expect_equal(r$partner_iso, U[-1][which.max(fs[1, 2:3, 1])])
  expect_equal(r$value, max(fs[1, 2:3, 1]))
})

test_that("CM-FDI flows, Development Potential by sector and the ties chunk", {
  ties_tbl <- tibble::tibble(investor_iso = c("JPN", "USA"), destination_iso = c("USA", "IND"), tech = "Solar", supply_chain = "Midstream",
                             n_projects = c(3, 1), n_disclosed = c(2, 0), value_usd_m = c(500, NA))
  fl <- dash_cmfdi_flows(ties_tbl)
  expect_equal(fl$investor_iso, c("JPN", "USA"))
  cells <- tibble::tibble(cell_id = 1:2, tech = c("Solar", "Wind"), supply_chain = "Midstream")
  dev <- tibble::tibble(partner_iso = c("IND", "IND", "IND", "BRA"), tech = c("Solar", "Solar", "Wind", "Coal"), supply_chain = "Midstream",
                        variable = c("Development Potential Index", "Development Potential Index", "aid_index_supplychain", "Development Potential Index"), value = c(0.4, 0.4, 0.9, 0.2))
  dp <- dash_dp_cells(dev, cells)
  expect_equal(nrow(dp), 1)
  expect_equal(dp$cell_id, 1L)
  U <- c("USA", "JPN", "IND")
  pos <- dash_dip_positions(ti_fdi(), ti_country_info(), index_years = 2020:2021)
  conduits <- partnership_strength_conduit_centres()
  conduits$in_rule <- TRUE
  top_all <- tibble::add_column(dash_top_partners(matrix(0.5, 3, 3), U, rep(TRUE, 3)), side = "fs", .after = "reporter_iso")
  ch <- dash_ties_chunk(pos, conduits, fl, dp, data.frame(iso3 = c("USA", "JPN"), agreement = "MSP"), NULL, top_all, U, list(top_k = 10L), c("United States", "Japan", "India"))
  expect_named(ch, "ties")
  expect_true(all(ch$ties$dip$reporter_iso %in% U) && all(ch$ties$dip$partner_iso %in% U))
  expect_false("geo" %in% names(ch$ties))
  expect_equal(ch$ties$U_names, c("United States", "Japan", "India"))
  expect_s3_class(ch$ties$meta, "dash_osb_json")
  # the chunk encodes and decodes (the browser's OSB format)
  back <- dash_osb_decode(dash_osb_encode(ch, "ties", "chunk"))$objects
  expect_identical(back, dash_osb_normalize(ch))
})

test_that("the slug rule matches the TypeScript stageSlug()", {
  expect_equal(dash_cell_slug("Electric Vehicles", "Midstream"), "electric-vehicles-midstream")
  expect_equal(dash_cell_slug("Oil & Gas", "Upstream"), "oil-gas-upstream")
})
