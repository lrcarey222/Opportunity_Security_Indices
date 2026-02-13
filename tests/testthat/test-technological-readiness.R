repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "assertions.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

iea_fixture <- function() {
  read_fixture_csv("technological_readiness_iea_fixture.csv")
}

test_that("taxonomy token detection handles merged taxonomy strings", {
  expect_true(taxonomy_has_token(
    sector_raw = "Supply,Manufacturing,Road transport,Vehicle,Battery",
    token = "Battery",
    sector4 = "Vehicle,Battery"
  ))

  expect_true(taxonomy_has_token(
    sector_raw = "Supply,Power,Grid,Storage",
    token = "Storage",
    sector4 = "Storage"
  ))

  expect_false(taxonomy_has_token(
    sector_raw = "Demand,Transport,Road transport,Vehicle",
    token = "Battery",
    sector4 = "Vehicle"
  ))
})

test_that("battery taxonomy is prioritized over road-transport EV mapping", {
  assigned <- iea_fixture() |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech() |>
    dplyr::select(name, tech)

  lfp_sector <- assigned$tech[assigned$name == "LFP cell manufacturing"]
  sodium_sector <- assigned$tech[assigned$name == "Sodium-ion pack"]
  ev_sector <- assigned$tech[assigned$name == "EV drivetrain"]

  expect_identical(lfp_sector, "Batteries")
  expect_identical(sodium_sector, "Batteries")
  expect_identical(ev_sector, "Electric Vehicles")
})

test_that("tech aggregation uses mean of item-level bell scores and does not impute missing", {
  fixture <- iea_fixture() |>
    dplyr::mutate(
      trl2023 = dplyr::case_when(
        name == "LFP cell manufacturing" ~ 3,
        name == "Sodium-ion pack" ~ 9,
        TRUE ~ trl2023
      )
) |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()

  tech_tbl <- technological_readiness_build_tech(
    fixture,
    techs = c("Batteries", "Coal"),
    min_trl = 2,
    mu = 6,
    max_trl = 11
  )

  battery_row <- tech_tbl[tech_tbl$tech == "Batteries", ]
  coal_row <- tech_tbl[tech_tbl$tech == "Coal", ]

  expected_item_mean <- mean(c(
    trl_bell_hard(3, min_trl = 2, mu = 6, max_trl = 11),
    trl_bell_hard(9, min_trl = 2, mu = 6, max_trl = 11)
  ))

  expect_equal(battery_row$trl2023, 6)
  expect_true(!is.na(battery_row$trl_index))
  expect_true(battery_row$trl_level_index < trl_bell_hard(6, min_trl = 2, mu = 6, max_trl = 11))
  expect_equal(battery_row$trl_level_index, expected_item_mean)

  expect_true(is.na(coal_row$trl2023))
  expect_true(is.na(coal_row$trl_index))
})

test_that("technological_readiness output schema stays stable", {
  out <- technological_readiness(iea_fixture(), techs = c("Batteries", "Electric Vehicles", "Coal"))

  expect_named(
    out,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value", "Year", "source", "explanation")
  )

  expect_true(all(out$Country == "Global"))
  expect_setequal(unique(out$supply_chain), c("Upstream", "Midstream", "Downstream"))
  expect_true(all(c("TRL 2023", "TRL Δ 2020–2023", "TRL Level Index", "TRL Momentum Index", "TRL Index") %in% unique(out$variable)))
})
