repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

# read_iea_tech_map_rules() finds config/ from the repo root; without this it would
# resolve against tests/testthat.
withr::local_options(opportunity_security.repo_root = repo_root, .local_envir = teardown_env())

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



test_that("battery mapping can match by name when taxonomy tokens are sparse", {
  sparse_battery <- tibble::tibble(
    name = "Advanced battery separator",
    sector = "Demand,Transport,Road transport,Vehicle",
    supplyChain = "Manufacturing",
    trl2023 = 6
  )

  assigned <- sparse_battery |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()

  expect_identical(unique(assigned$tech), "Batteries")
})

test_that("mapping any-match rules do not require all any_* fields simultaneously", {
  hydrogen_name_only <- tibble::tibble(
    name = "Electrolyser balance-of-plant",
    sector = "Supply,Power,Infrastructure,Equipment",
    supplyChain = "Other",
    trl2023 = 5
  )

  assigned <- hydrogen_name_only |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()

  expect_identical(unique(assigned$tech), "Green Hydrogen")
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

  # Three fixture items map to Batteries: the two cells set above plus "Grid storage
  # system" at TRL 6, which the Storage taxonomy token claims. Derive the expectation
  # from the assignment so the check stays about the aggregation, not the rule set.
  battery_trls <- fixture$trl2023[which(fixture$tech == "Batteries")]
  expect_setequal(battery_trls, c(3, 9, 6))
  expected_item_mean <- mean(trl_bell_hard(battery_trls, min_trl = 2, mu = 6, max_trl = 11))

  expect_equal(battery_row$trl2023, mean(battery_trls))
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
  # The fixture carries a single TRL year, so the momentum window collapses onto it.
  expect_true(all(c("TRL 2023", "TRL Δ 2023–2023", "TRL Level Index", "TRL Momentum Index", "TRL Index") %in% unique(out$variable)))
  expect_true(all(out$Year == 2023L))
})

test_that("TRL years follow the release rather than a pinned year", {
  fixture <- iea_fixture()
  fixture$trl2024 <- fixture$trl2023 + 1
  fixture$trl2025 <- fixture$trl2023 + 1

  out <- technological_readiness(fixture, techs = c("Batteries", "Electric Vehicles", "Coal"))

  expect_true("TRL 2025" %in% out$variable)
  expect_false("TRL 2023" %in% out$variable)
  expect_true("TRL Δ 2023–2025" %in% out$variable)
  expect_true(all(out$Year == 2025L))
})

test_that("the 2026 IEA public dataset layout normalizes onto the legacy columns", {
  public_dataset <- tibble::tibble(
    `tech.final.name` = c("Sodium-ion battery", "Small modular reactor"),
    `tech.description` = c("desc a", "desc b"),
    `category.1` = c("Energy networks and storage", "Nuclear"),
    `category.2` = c("Electrochemical storage", "Fission"),
    `category.3` = c("Batteries", "Small modular reactors"),
    `category.4` = c("", ""),
    `Energy Storage.cc` = c("Batteries", ""),
    `Power.cc` = c("Storage", "Generation"),
    `TRL.2020` = c(4, 5),
    `TRL.2025` = c(7, 6)
  )

  normalized <- technological_readiness_normalize_iea(public_dataset)

  expect_true(all(c("name", "sector", "supplyChain", "trl2020", "trl2025") %in% names(normalized)))
  expect_identical(normalized$name[[1]], "Sodium-ion battery")
  # Empty category cells are dropped rather than left as blank taxonomy tokens.
  expect_identical(
    normalized$sector[[1]],
    "Energy networks and storage,Electrochemical storage,Batteries"
  )
  expect_identical(normalized$supplyChain[[1]], "Batteries,Storage")
  expect_identical(normalized$supplyChain[[2]], "Generation")

  assigned <- normalized |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()

  expect_identical(assigned$tech[assigned$name == "Sodium-ion battery"], "Batteries")
  expect_identical(assigned$tech[assigned$name == "Small modular reactor"], "Nuclear")

  out <- technological_readiness(public_dataset, techs = c("Batteries", "Nuclear"))
  expect_true(all(c("TRL 2025", "TRL Δ 2020–2025", "TRL Index") %in% out$variable))
  expect_true(all(out$Year == 2025L))
})
