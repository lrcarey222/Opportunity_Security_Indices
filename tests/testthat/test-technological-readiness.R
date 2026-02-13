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
    sector4 = "Vehicle,Battery",
    token = "Battery"
  ))

  expect_true(taxonomy_has_token(
    sector_raw = "Supply,Power,Grid,Storage",
    sector4 = "Storage",
    token = "Storage"
  ))

  expect_false(taxonomy_has_token(
    sector_raw = "Demand,Transport,Road transport,Vehicle",
    sector4 = "Vehicle",
    token = "Battery"
  ))
})

test_that("battery taxonomy is prioritized over road-transport EV mapping", {
  assigned <- iea_fixture() |>
    technological_readiness_clean() |>
    technological_readiness_assign_sector() |>
    dplyr::select(name, rmi_sector)

  lfp_sector <- assigned$rmi_sector[assigned$name == "LFP cell manufacturing"]
  sodium_sector <- assigned$rmi_sector[assigned$name == "Sodium-ion pack"]
  ev_sector <- assigned$rmi_sector[assigned$name == "EV drivetrain"]

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
    technological_readiness_clean()

  tech_tbl <- technological_readiness_build_tech(
    fixture,
    techs = c("Batteries", "Coal"),
    min_trl = 2,
    mu = 6,
    max_trl = 11,
    gamma = 1
  )

  battery_row <- tech_tbl[tech_tbl$tech == "Batteries", ]
  coal_row <- tech_tbl[tech_tbl$tech == "Coal", ]

  expected_item_mean <- mean(c(
    trl_bell_soft(3, min_trl = 2, mu = 6, max_trl = 11, gamma = 1),
    trl_bell_soft(9, min_trl = 2, mu = 6, max_trl = 11, gamma = 1)
  ))

  expect_equal(battery_row$trl2023, 6)
  expect_equal(battery_row$trl_index, expected_item_mean)
  expect_true(battery_row$trl_index < trl_bell_soft(6, min_trl = 2, mu = 6, max_trl = 11, gamma = 1))

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
  expect_setequal(unique(out$variable), c("TRL 2023", "TRL Index"))
})
