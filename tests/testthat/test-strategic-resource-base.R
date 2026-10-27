# Tests for the Strategic Index endowment screen (strategic_resource_base(),
# flag_strategic_resource_base() and the allied pick in R/indices/build_indices.R) and for
# add_unlisted_economies() (R/utils/country.R).
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))

resource_row <- function(Country, tech, category, value, data_type = "index",
                         variable = paste(tech, category), supply_chain = "Extraction") {
  tibble::tibble(
    Country = Country, tech = tech, supply_chain = supply_chain, category = category,
    variable = variable, data_type = data_type, value = value
  )
}

test_that("strategic_resource_base reads the technology-level reserves and production rows", {
  reserves <- dplyr::bind_rows(
    resource_row("A", "Nuclear", "Reserves", 0.4),
    resource_row("B", "Nuclear", "Reserves", NA_real_),
    resource_row("C", "Nuclear", "Reserves", 0),
    # A mineral row is not the technology rollup and does not count for Nuclear.
    resource_row("B", "Copper", "Reserves", 0.9, variable = "Copper Reserves"),
    # The category's overall index is not a resource measurement.
    resource_row("C", "Nuclear", "Reserves", 0.7, variable = "Overall Reserves Index")
  )
  production <- dplyr::bind_rows(
    resource_row("B", "Nuclear", "Production", 120, data_type = "raw"),
    resource_row("C", "Nuclear", "Production", 0, data_type = "raw"),
    # Refined output (uranium conversion, enrichment) is not a resource endowment.
    resource_row("D", "Nuclear", "Production", 5, supply_chain = "Refining")
  )
  rb <- strategic_resource_base(reserves, production)
  nuc <- rb[rb$tech == "Nuclear", ]
  expect_setequal(nuc$Country, c("A", "B", "C"))
  expect_true(nuc$resource_base[nuc$Country == "A"])
  expect_true(nuc$resource_base[nuc$Country == "B"])
  expect_false(nuc$resource_base[nuc$Country == "C"])
  expect_identical(nuc$resource_evidence[nuc$Country == "B"], "Production")
  expect_true(is.na(nuc$resource_evidence[nuc$Country == "C"]))
  expect_true(rb$resource_base[rb$tech == "Copper" & rb$Country == "B"])
  expect_false(any(rb$supply_chain != "Extraction"))
  expect_identical(eval(formals(strategic_resource_base)$stage), "Extraction")
  expect_error(strategic_resource_base(), "at least one")
})

test_that("strategic_resource_base stops on tables that carry the old three stages", {
  legacy <- dplyr::bind_rows(
    resource_row("A", "Nuclear", "Reserves", 0.4, supply_chain = "Upstream"),
    resource_row("B", "Nuclear", "Production", 3, supply_chain = "Upstream")
  )
  # At Extraction the rows would all drop and nothing would be screened.
  expect_error(
    strategic_resource_base(legacy),
    "carry stage Upstream, none of Extraction"
  )
  # One table at six stages does not hide another still at three: its production would be lost.
  expect_error(
    strategic_resource_base(resource_row("A", "Nuclear", "Reserves", 0.4), legacy[2, ]),
    "resource table 2 carry stage Upstream, none of Extraction"
  )
  # A three-stage caller names the tables' own stage and gets the screen as before.
  rb <- strategic_resource_base(legacy, stage = "Upstream")
  expect_equal(rb$Country, c("A", "B"))
  expect_true(all(rb$resource_base))
  expect_true(all(rb$supply_chain == "Upstream"))
  # Tables without any technology-level row return an empty screen rather than stop.
  empty <- strategic_resource_base(resource_row("A", "Copper", "Reserves", 1, variable = "Copper Mine Reserves"))
  expect_equal(nrow(empty), 0L)
})

test_that("flag_strategic_resource_base flags screened Extraction cells and leaves the rest NA", {
  si <- tidyr::crossing(Country = c("A", "B", "Z"), tech = c("Nuclear", "Solar"),
                        supply_chain = c("Extraction", "Refining", "Assembly")) %>%
    dplyr::mutate(strategic_index = seq(0.1, 0.9, length.out = dplyr::n()))
  rb <- tibble::tibble(
    Country = c("A", "B"), tech = "Nuclear", supply_chain = "Extraction",
    resource_base = c(TRUE, FALSE), resource_evidence = c("Reserves", NA)
  )
  out <- flag_strategic_resource_base(si, rb)
  expect_equal(nrow(out), nrow(si))
  expect_equal(out$strategic_index, si$strategic_index)
  nu <- out[out$tech == "Nuclear" & out$supply_chain == "Extraction", ]
  expect_identical(nu$resource_base[nu$Country == "A"], TRUE)
  expect_identical(nu$resource_base[nu$Country == "B"], FALSE)
  # Z is not in the resource tables: no recorded resources.
  expect_identical(nu$resource_base[nu$Country == "Z"], FALSE)
  expect_identical(nu$pick_exclusion[nu$Country == "Z"], "no resource base")
  # Nuclear - Refining (conversion, enrichment) is not screened, nor is any other stage.
  other <- out[!(out$tech == "Nuclear" & out$supply_chain == "Extraction"), ]
  expect_true(all(is.na(other$resource_base)))
  expect_true(all(is.na(other$pick_exclusion)))
  # Re-flagging replaces the columns rather than duplicating them.
  again <- flag_strategic_resource_base(out, rb)
  expect_identical(names(again), names(out))
  expect_error(
    flag_strategic_resource_base(si, dplyr::bind_rows(rb, rb)),
    "duplicated"
  )
})

test_that("the allied pick skips Extraction stages without a resource base", {
  si <- tibble::tibble(
    Country = c("A", "A", "B", "B", "B"),
    tech = c("Nuclear", "Solar", "Nuclear", "Solar", "Nuclear"),
    supply_chain = c("Extraction", "Assembly", "Extraction", "Assembly", "Refining"),
    strategic_index = c(0.9, 0.5, 0.8, 0.6, 0.7)
  ) %>%
    dplyr::mutate(sector = paste(.data$tech, .data$supply_chain, sep = " - "))
  rb <- tibble::tibble(Country = "A", tech = "Nuclear", supply_chain = "Extraction", resource_base = TRUE)
  flagged <- flag_strategic_resource_base(si, rb)
  picks <- select_allied_strategic_sectors(flagged, ally_countries = c("A", "B"))
  expect_identical(picks$sector[picks$Country == "A"], "Nuclear - Extraction")
  # B has no uranium: its Nuclear - Extraction is barred, its Nuclear - Refining is not screened.
  expect_identical(picks$sector[picks$Country == "B"], "Nuclear - Refining")
  # The screen can be turned off, and a table without the flag is not screened.
  unscreened <- select_allied_strategic_sectors(flagged, ally_countries = c("A", "B"), require_resource_base = FALSE)
  expect_identical(unscreened$sector[unscreened$Country == "B"], "Nuclear - Extraction")
  plain <- select_allied_strategic_sectors(si, ally_countries = c("A", "B"))
  expect_identical(plain$sector[plain$Country == "B"], "Nuclear - Extraction")
})

test_that("add_unlisted_economies adds Taiwan under its canonical name once", {
  ci <- tibble::tibble(country = c("Japan", "Korea, Rep."), iso3c = c("JPN", "KOR"), region = "East Asia & Pacific")
  out <- add_unlisted_economies(ci)
  expect_equal(nrow(out), 3)
  expect_identical(out$country[out$iso3c == "TWN"], "Taiwan")
  expect_true(is.na(out$region[out$iso3c == "TWN"]))
  expect_identical(add_unlisted_economies(out), out)
  expect_identical(add_unlisted_economies(ci, iso3 = character()), ci)
  expect_null(add_unlisted_economies(NULL))
})
