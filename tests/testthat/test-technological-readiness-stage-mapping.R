repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

# read_iea_tech_map_rules() finds config/ from the repo root; without this it would
# resolve against tests/testthat.
withr::local_options(opportunity_security.repo_root = repo_root, .local_envir = teardown_env())

# Rows in the layout of the 2026 IEA public dataset, as read.csv() returns it (check.names:
# "Energy Storage.cc" becomes "Energy.Storage.cc"). `cc` holds "<theme>=<value>" tags joined
# by ";", e.g. "Hydrogen=Direct use;Power=Generation".
guide_rows <- function(items) {
  themes <- c("Hydrogen", "CCUS", "Mobility", "Energy Storage", "Power", "Heat", "Bioenergy", "Digitalisation")
  cc_value <- function(cc, theme) {
    vapply(cc, function(tags) {
      tags <- strsplit(dplyr::coalesce(tags, ""), ";", fixed = TRUE)[[1]]
      hit <- tags[startsWith(tags, paste0(theme, "="))]
      if (length(hit) == 0) "" else sub("^[^=]*=", "", hit[[1]])
    }, character(1), USE.NAMES = FALSE)
  }

  out <- tibble::tibble(
    `tech.final.name` = items$name,
    `tech.description` = "",
    `category.1` = items$c1,
    `category.2` = dplyr::coalesce(items$c2, ""),
    `category.3` = dplyr::coalesce(items$c3, ""),
    `category.4` = dplyr::coalesce(items$c4, "")
  )
  for (theme in themes) {
    out[[paste0(gsub(" ", ".", theme), ".cc")]] <- cc_value(items$cc, theme)
  }
  out$TRL.2020 <- items$trl2020
  for (year in 2021:2024) {
    out[[paste0("TRL.", year)]] <- NA_real_
  }
  out$TRL.2025 <- items$trl2025
  # Optional post-TRL-9 adoption stage, blank where the Guide gives none.
  if ("post2025" %in% names(items)) {
    out$Post.index.2025 <- dplyr::coalesce(items$post2025, "")
  }
  out
}

assign_guide <- function(items) {
  guide_rows(items) |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()
}

mapping_of <- function(assigned, item) {
  rows <- assigned[assigned$name == item, ]
  list(
    tech = sort(unique(stats::na.omit(rows$tech))),
    stage = sort(unique(stats::na.omit(rows$stage))),
    status = unique(rows$mapping_status)
  )
}

test_that("loose name patterns no longer pull unrelated Guide items into a technology", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Highly insulating window", "Buildings", "Design and envelope", "Building envelope technologies", "Fenestration", "", 9, 9,
    "Solid oxide electrolyser cell", "Hydrogen", "Production", "Electrolysis", "Electrolyser designs", "Hydrogen=Production", 7, 8,
    "Multi-junction cell PV", "Renewables", "Solar", "Photovoltaic", "", "Power=Generation", 9, 9,
    "Primary smelting with multipolar cell", "Industry", "Aluminium", "Other production techniques", "", "Power=End-use", 4, 5,
    "Primary smelting with chloride electrolysis", "Industry", "Aluminium", "Other production techniques", "", "Power=End-use", 4, 5,
    "Thermal decontamination in a vacuum reactor with integrated nitrogen flushing for PET", "Industry", "Chemicals", "Recycling", "", "", 9, 9,
    "X-ray transmission for recycling", "Industry", "Metallic products", "Recycling", "", "", 9, 9,
    "Sand thermal battery", "Energy networks and storage", "Thermal storage for buildings and district heating", "District heating thermal storage", "", "Energy Storage=Thermal energy storage;Heat=Storage", NA, 8,
    "Absorption heat pump", "Buildings", "Operations and equipment", "Heating, cooling and ventilation technologies", "Thermally-driven heat pump", "Hydrogen=Direct use;Power=End-use;Heat=Generation", 9, 9
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "Highly insulating window")$status, "out_of_scope")
  expect_identical(mapping_of(assigned, "Solid oxide electrolyser cell")$tech, "Green Hydrogen")
  expect_identical(mapping_of(assigned, "Solid oxide electrolyser cell")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Multi-junction cell PV")$tech, "Solar")
  for (item in c(
    "Primary smelting with multipolar cell", "Primary smelting with chloride electrolysis",
    "Thermal decontamination in a vacuum reactor with integrated nitrogen flushing for PET",
    "X-ray transmission for recycling", "Sand thermal battery", "Absorption heat pump"
  )) {
    expect_identical(mapping_of(assigned, item)$status, "out_of_scope", info = item)
    expect_length(mapping_of(assigned, item)$tech, 0)
  }
})

test_that("road transport splits into EVs, hydrogen vehicles and out-of-scope engines", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Fast and ultra-fast charging", "Transport", "Road transport", "EV charging equipment", "", "Mobility=Electric (batteries);Energy Storage=Batteries;Power=End-use", 8, 9,
    # Filed under road transport, but the Guide describes chargers for electric ships.
    "Fast charging", "Transport", "Road transport", "EV charging equipment", "", "Mobility=Electric (batteries);Energy Storage=Batteries;Power=End-use", 7, 7,
    "Battery electric truck", "Transport", "Road transport", "Electric vehicle (EV)", "", "Mobility=Electric (vehicles)", 8, 9,
    "Rare-earth free electric motor", "Transport", "Road transport", "Electric vehicle (EV)", "", "Mobility=Electric (vehicles)", 9, 9,
    "Sodium-ion battery", "Transport", "Road transport", "Electric vehicle battery (electrical storage)", "Next-generation batteries", "Mobility=Electric (batteries);Energy Storage=Batteries;Power=Storage", 7, 9,
    "Hydrogen fuel cell truck", "Transport", "Road transport", "Hydrogen use in road transport", "Hydrogen fuel cell electric vehicles", "Hydrogen=Direct use;Mobility=Hydrogen", 8, 9,
    "Flex-fuel and hybrid flex-fuel vehicle", "Transport", "Road transport", "Advanced combustion engines and powetrains", "", "Mobility=Methanol and ethanol;Bioenergy=Biofuels use", 9, 9,
    "Battery-electric aircraft: regional (20+ seats)", "Transport", "Aviation", "Aircrafts", "", "Mobility=Electric (vehicles);Power=End-use", 3, 3,
    "Hybrid fuel-cell - gas-turbine aircraft propulsion system", "Transport", "Aviation", "Components", "Hydrogen-powered aircraft", "Mobility=Hydrogen", 3, 3
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "Fast and ultra-fast charging")$tech, "Electric Vehicles")
  expect_identical(mapping_of(assigned, "Fast and ultra-fast charging")$stage, "Downstream")
  expect_identical(mapping_of(assigned, "Fast charging")$status, "out_of_scope")
  expect_length(mapping_of(assigned, "Fast charging")$tech, 0)
  # Vehicle designs are the end product: Midstream (final assembly) since 2026-10-26.
  expect_identical(mapping_of(assigned, "Battery electric truck")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Rare-earth free electric motor")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Sodium-ion battery")$tech, "Batteries")
  expect_identical(mapping_of(assigned, "Sodium-ion battery")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Hydrogen fuel cell truck")$tech, "Green Hydrogen")
  expect_identical(mapping_of(assigned, "Hydrogen fuel cell truck")$stage, "Downstream")
  expect_identical(mapping_of(assigned, "Hybrid fuel-cell - gas-turbine aircraft propulsion system")$tech, "Green Hydrogen")
  expect_identical(mapping_of(assigned, "Flex-fuel and hybrid flex-fuel vehicle")$status, "out_of_scope")
  expect_identical(mapping_of(assigned, "Battery-electric aircraft: regional (20+ seats)")$status, "out_of_scope")
})

test_that("green hydrogen production is water splitting; fossil and other routes are excluded", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Alkaline electrolyser", "Hydrogen", "Production", "Electrolysis", "Electrolyser designs", "Hydrogen=Production;Power=End-use", 9, 9,
    "Photon-based direct solar water splitting", "Hydrogen", "Production", "Other", "", "Hydrogen=Production", 4, 5,
    "Steam reforming - high capture rates", "Hydrogen", "Production", "Thermal processes", "CCUS-enabled", "Hydrogen=Production;CCUS=Capture", 6, 6,
    "Catalytic decomposition (methane)", "Hydrogen", "Production", "Thermal processes", "Pyrolysis", "Hydrogen=Production", 6, 7,
    "Natural hydrogen extraction", "Hydrogen", "Production", "Geologic hydrogen", "", "Hydrogen=Production", 4, 5,
    # The Guide writes the subscript "H₂"; the matcher reads it as "H2".
    "Salt cavern storage", "Hydrogen", "H₂ infrastructure", "H₂ storage", "Underground storage", "Hydrogen=Storage", 9, 9,
    "Offshore renewable hydrogen production", "Renewables", "Wind", "", "", "Power=Generation", 4, 5,
    "DRI production based on 100% electrolytic hydrogen", "Industry", "Iron and steel", "Direct reduced iron (DRI)", "", "Hydrogen=Direct use", 5, 6,
    "Direct electrochemical nitrogen reduction", "Industry", "Chemicals", "Ammonia", "Alternative routes", "Hydrogen=Hydrogen-based fuels production;Mobility=Ammonia", 4, 5
  )
  assigned <- assign_guide(items)

  for (item in c("Alkaline electrolyser", "Photon-based direct solar water splitting", "Offshore renewable hydrogen production")) {
    expect_identical(mapping_of(assigned, item)$tech, "Green Hydrogen", info = item)
    expect_identical(mapping_of(assigned, item)$stage, "Midstream", info = item)
  }
  for (item in c("Salt cavern storage", "DRI production based on 100% electrolytic hydrogen")) {
    expect_identical(mapping_of(assigned, item)$tech, "Green Hydrogen", info = item)
    expect_identical(mapping_of(assigned, item)$stage, "Downstream", info = item)
  }
  for (item in c(
    "Steam reforming - high capture rates", "Catalytic decomposition (methane)",
    "Natural hydrogen extraction", "Direct electrochemical nitrogen reduction"
  )) {
    expect_identical(mapping_of(assigned, item)$status, "out_of_scope", info = item)
  }
})

test_that("oil and gas are separated where the Guide separates them", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Post-combustion carbon capture (cracking)", "Fossil fuels", "Oil and gas", "Carbon management", "Refining", "CCUS=Capture", 5, 5,
    "Post-combustion: chemical absorption (natural gas with CCUS)", "Fossil fuels", "Oil and gas", "Carbon management", "Power generation", "CCUS=Capture;Power=Generation;Heat=Generation", 6, 8,
    "Mini-LNG facilities", "Fossil fuels", "Oil and gas", "Methane emissions monitoring and abatement", "", "Power=Generation;Heat=Generation", 9, 9,
    "Flare gas recovery", "Fossil fuels", "Oil and gas", "Methane emissions monitoring and abatement", "", "Power=Generation;Heat=Generation", 8, 9,
    "Coal mine methane use for energy supply", "Fossil fuels", "Coal", "Methane emissions monitoring and abatement", "", "Power=Generation", 9, 9,
    "Oxy-fuelling (coal)", "Fossil fuels", "Coal", "Carbon management", "", "CCUS=Capture;Power=Generation;Heat=Generation", 6, 7
  )
  assigned <- assign_guide(items)

  refining <- mapping_of(assigned, "Post-combustion carbon capture (cracking)")
  expect_identical(refining$tech, "Oil")
  expect_identical(refining$stage, "Midstream")
  gas_power <- mapping_of(assigned, "Post-combustion: chemical absorption (natural gas with CCUS)")
  expect_identical(gas_power$tech, "Gas")
  expect_identical(gas_power$stage, "Downstream")
  lng <- mapping_of(assigned, "Mini-LNG facilities")
  expect_identical(lng$tech, "Gas")
  expect_identical(lng$stage, "Midstream")
  # Methane abatement at "oil and gas facilities" is genuinely shared.
  flare <- mapping_of(assigned, "Flare gas recovery")
  expect_identical(flare$tech, c("Gas", "Oil"))
  expect_identical(flare$stage, "Upstream")
  expect_identical(mapping_of(assigned, "Coal mine methane use for energy supply")$stage, "Upstream")
  expect_identical(mapping_of(assigned, "Oxy-fuelling (coal)")$stage, "Downstream")

  out <- technological_readiness(guide_rows(items), techs = c("Oil", "Gas", "Coal"))
  trl <- out[out$variable == "TRL Index", ]
  value_of <- function(tech, stage) trl$value[trl$tech == tech & trl$supply_chain == stage]
  expect_equal(value_of("Oil", "Upstream"), value_of("Gas", "Upstream"))
  expect_false(isTRUE(all.equal(value_of("Oil", "Midstream"), value_of("Gas", "Midstream"))))
  expect_false(isTRUE(all.equal(value_of("Oil", "Downstream"), value_of("Gas", "Downstream"))))
})

test_that("acid gas removal is shared by oil and gas although the Guide files it under refining", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Advanced acid gas removal", "Fossil fuels", "Oil and gas", "Carbon management", "Refining", "CCUS=Capture", 9, 9,
    "Oxy-fuelling carbon capture (cracking)", "Fossil fuels", "Oil and gas", "Carbon management", "Refining", "CCUS=Capture", 5, 5,
    "Mini-CNG facilities", "Fossil fuels", "Oil and gas", "Methane emissions monitoring and abatement", "", "", 9, 9
  )
  assigned <- assign_guide(items)

  agr <- mapping_of(assigned, "Advanced acid gas removal")
  expect_identical(agr$tech, c("Gas", "Oil"))
  expect_identical(agr$stage, "Midstream")
  expect_identical(agr$status, "mapped")
  expect_identical(mapping_of(assigned, "Oxy-fuelling carbon capture (cracking)")$tech, "Oil")

  tech_tbl <- technological_readiness_build_tech(assigned, techs = c("Oil", "Gas"))
  mid <- tech_tbl[tech_tbl$supply_chain == "Midstream", ]
  # The shared item enters both cells once: Oil averages it with the refinery item, Gas with
  # the CNG plant.
  expect_equal(mid$n_items[mid$tech == "Oil"], 2L)
  expect_equal(mid$n_items[mid$tech == "Gas"], 2L)
  expect_equal(mid$trl2025[mid$tech == "Oil"], 7)
  expect_equal(mid$trl2025[mid$tech == "Gas"], 9)
})

test_that("ammonia used as a hydrogen-based fuel counts as green hydrogen use even when untagged", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Ammonia (high temperature heating)", "Industry", "Industrial heating", "High temperature", "Fuel-based heating", "Heat=Generation", NA, 5,
    "Ammonia (low temperature heating)", "Industry", "Industrial heating", "Low to medium temperature", "Fuel-based heating", "Heat=Generation", NA, 4,
    "DRI production using ammonia as a reductant", "Industry", "Iron and steel", "Direct reduced iron (DRI)", "", "", NA, 3,
    "Hydrogen (high temperature heating)", "Industry", "Industrial heating", "High temperature", "Fuel-based heating", "Hydrogen=Direct use;Heat=Generation", NA, 8,
    # Ammonia made without hydrogen, or from hydrogen made with CCUS, stays out.
    "Chemical looping (ammonia production)", "Industry", "Chemicals", "Ammonia", "Alternative routes", "Mobility=Ammonia", NA, 4,
    "Haber-bosch process using hydrogen produced with full CCUS", "Industry", "Chemicals", "Ammonia", "Thermocatalytic", "CCUS=Capture;Mobility=Ammonia", 6, 6,
    "Electrification (with thermal storage)", "Industry", "Cement and concrete", "Shift in energy sources and electrification", "", "Power=End-use", NA, 8
  )
  assigned <- assign_guide(items)

  for (item in c(
    "Ammonia (high temperature heating)", "Ammonia (low temperature heating)",
    "DRI production using ammonia as a reductant", "Hydrogen (high temperature heating)"
  )) {
    expect_identical(mapping_of(assigned, item)$tech, "Green Hydrogen", info = item)
    expect_identical(mapping_of(assigned, item)$stage, "Downstream", info = item)
  }
  expect_identical(mapping_of(assigned, "Ammonia (high temperature heating)")$status, "mapped")
  for (item in c(
    "Chemical looping (ammonia production)",
    "Haber-bosch process using hydrogen produced with full CCUS",
    "Electrification (with thermal storage)"
  )) {
    expect_identical(mapping_of(assigned, item)$status, "out_of_scope", info = item)
    expect_length(mapping_of(assigned, item)$tech, 0)
  }
})

test_that("nuclear keeps fission and assigns fuel cycle and reactors to their stages", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Uranium enrichment: laser-based techniques", "Nuclear", "Fission", "Nuclear fuel cycles", "Front-end of the fuel cycle", "", 5, 6,
    "Radioactive waste disposal", "Nuclear", "Fission", "Nuclear fuel cycles", "Back-end of the fuel cycle", "", 5, 5,
    "Fuel reprocessing (PUREX process and new advanced techniques)", "Nuclear", "Fission", "Nuclear fuel cycles", "Back-end of the fuel cycle", "", 7, 7,
    "Water-cooled SMR", "Nuclear", "Fission", "Small modular reactors", "", "Power=Generation;Heat=Generation", 6, 7,
    "Magnetic confinement fusion: tokamaks", "Nuclear", "Fusion", "Magnetic confinement fusion", "", "Power=Generation;Heat=Generation", 3, 3
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "Uranium enrichment: laser-based techniques")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Radioactive waste disposal")$stage, "Downstream")
  # Reactor designs are the end product (Midstream); reprocessing is recovery (Upstream).
  expect_identical(mapping_of(assigned, "Water-cooled SMR")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Fuel reprocessing (PUREX process and new advanced techniques)")$stage, "Upstream")
  expect_identical(mapping_of(assigned, "Magnetic confinement fusion: tokamaks")$status, "out_of_scope")
})

test_that("battery minerals and recycling map to Batteries; generic and rare-earth items do not", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Direct lithium extraction from brine", "Critical minerals", "Mineral processing and refining", "Hydrometallurgy", "", "Energy Storage=Minerals mining and extraction", 5, 6,
    "High pressure acid leaching", "Critical minerals", "Mineral processing and refining", "Hydrometallurgy", "", "Energy Storage=Minerals mining and extraction", 9, 9,
    "Hydrometallurgical battery recycling", "Critical minerals", "E-waste recycling", "Battery recycling", "Processing", "Mobility=Electric (batteries);Energy Storage=Batteries (recycling)", 9, 9,
    "EV battery reuse in BESS", "Critical minerals", "E-waste recycling", "Battery recycling", "Pre-processing and disassembly", "Mobility=Electric (batteries);Energy Storage=Batteries (recycling)", 4, 5,
    "Bioleaching", "Critical minerals", "Mineral processing and refining", "Hydrometallurgy", "", "Energy Storage=Minerals mining and extraction", NA, 9,
    "Novel REE separation", "Critical minerals", "Mineral processing and refining", "Hydrometallurgy", "", "Energy Storage=Minerals mining and extraction", NA, 3
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "Direct lithium extraction from brine")$stage, "Upstream")
  expect_identical(mapping_of(assigned, "High pressure acid leaching")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Hydrometallurgical battery recycling")$stage, "Upstream")
  # Second-life reuse is recovery, which rolls up to Upstream.
  expect_identical(mapping_of(assigned, "EV battery reuse in BESS")$stage, "Upstream")
  for (item in c("Direct lithium extraction from brine", "High pressure acid leaching", "Hydrometallurgical battery recycling")) {
    expect_identical(mapping_of(assigned, item)$tech, "Batteries", info = item)
  }
  expect_identical(mapping_of(assigned, "Bioleaching")$status, "out_of_scope")
  expect_identical(mapping_of(assigned, "Novel REE separation")$status, "out_of_scope")
})

test_that("cable recycling is grid recovery and geothermal drilling is deployment", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Chemical recycling of cables", "Critical minerals", "E-waste recycling", "Cable recycling", "", "", 4, 5,
    "Millimeter wave drilling", "Renewables", "Geothermal", "Subsurface operations for geothermal energy", "Drilling", "", 3, 4,
    "Enhanced geothermal systems", "Renewables", "Geothermal", "Subsurface operations for geothermal energy", "Next-generation geothermal energy systems", "Power=Generation", 6, 7
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "Chemical recycling of cables")$tech, "Electric Grid")
  expect_identical(mapping_of(assigned, "Chemical recycling of cables")$stage, "Upstream")
  for (item in c("Millimeter wave drilling", "Enhanced geothermal systems")) {
    expect_identical(mapping_of(assigned, item)$tech, "Geothermal", info = item)
    expect_identical(mapping_of(assigned, item)$stage, "Downstream", info = item)
  }
})

test_that("grid items split into equipment and operation, with stray whitespace in the taxonomy", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "HTLS conductors", "Energy networks and storage", "Power grids", "Cables and conductors", "", "Power=Transmission and distribution", 9, 9,
    "HVDC breaker", "Energy networks and storage", "Power grids", "Control systems and monitoring", "", "Energy Storage=Flexibility (grids);Power=Transmission and distribution", 7, 8,
    # The 2026 file writes this level with a trailing space.
    "Virtual power plants", "Energy networks and storage", "Power grids", "Capacity management ", "", "Energy Storage=Flexibility (grids);Power=Transmission and distribution", 8, 9,
    "Smart meter", "Buildings", "Operations and equipment", "Building energy management systems", "", "Power=Transmission and distribution;Digitalisation=Yes", 9, 9,
    "Lighting control system", "Buildings", "Operations and equipment", "Lighting technologies and control systems", "", "Power=Transmission and distribution;Digitalisation=Yes", 9, 9,
    "Battery storage system with grid-forming inverter", "Energy networks and storage", "Power grids", "Capacity management ", "", "Energy Storage=Flexibility (grids);Power=Transmission and distribution", NA, 9
  )
  assigned <- assign_guide(items)

  expect_identical(mapping_of(assigned, "HTLS conductors")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "HVDC breaker")$stage, "Midstream")
  expect_identical(mapping_of(assigned, "Virtual power plants")$tech, "Electric Grid")
  expect_identical(mapping_of(assigned, "Virtual power plants")$stage, "Downstream")
  expect_identical(mapping_of(assigned, "Smart meter")$tech, "Electric Grid")
  expect_identical(mapping_of(assigned, "Lighting control system")$status, "out_of_scope")
  expect_identical(mapping_of(assigned, "Battery storage system with grid-forming inverter")$tech, "Batteries")
  expect_identical(mapping_of(assigned, "Battery storage system with grid-forming inverter")$stage, "Downstream")
})

test_that("cells average their stage's items and fall back to the technology when a stage is empty", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Coal mine methane use for energy supply", "Fossil fuels", "Coal", "Methane emissions monitoring and abatement", "", "Power=Generation", 9, 9,
    "Ventilation air methane oxidisers", "Fossil fuels", "Coal", "Methane emissions monitoring and abatement", "", "", 7, 9,
    "Oxy-fuelling (coal)", "Fossil fuels", "Coal", "Carbon management", "", "CCUS=Capture;Power=Generation;Heat=Generation", 6, 7,
    # No TRL before 2025: no momentum, but its level still counts.
    "Post-combustion: solid adsorption (coal with CCUS)", "Fossil fuels", "Coal", "Carbon management", "Post-combustion capture", "CCUS=Capture;Power=Generation;Heat=Generation", NA, 5
  )
  bell <- function(x) trl_bell_hard(x, min_trl = 2, mu = 6, max_trl = 11)
  momentum <- function(delta) (pmin(pmax(delta, 0), 3) / 3)^0.5

  tech_tbl <- guide_rows(items) |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech() |>
    technological_readiness_build_tech(techs = "Coal")
  row <- function(stage) tech_tbl[tech_tbl$supply_chain == stage, ]

  up <- row("Upstream")
  expect_identical(up$basis, "stage")
  expect_equal(up$n_items, 2L)
  expect_equal(up$trl2025, 9)
  expect_equal(up$trl_level_index, bell(9))
  expect_equal(up$trl_momentum_index, mean(momentum(c(0, 2))))

  down <- row("Downstream")
  expect_equal(down$n_items, 2L)
  expect_equal(down$trl_level_index, mean(bell(c(7, 5))))
  # Only the item with a 2020 reading has momentum.
  expect_equal(down$trl_momentum_index, momentum(1))
  expect_equal(down$trl_delta_2020_2025, 1)

  # Coal has no Midstream item: the cell carries the technology-wide means over all four items.
  mid <- row("Midstream")
  expect_identical(mid$basis, "technology")
  expect_equal(mid$n_items, 0L)
  expect_equal(mid$n_items_tech, 4L)
  expect_equal(mid$trl_level_index, mean(bell(c(9, 9, 7, 5))))
  expect_equal(mid$trl_momentum_index, mean(momentum(c(0, 2, 1))))

  # Every cell's index is exactly the published blend of its components.
  expect_equal(tech_tbl$trl_index, 0.7 * tech_tbl$trl_level_index + 0.3 * tech_tbl$trl_momentum_index)
  expect_true(length(unique(round(tech_tbl$trl_index, 10))) == 3)

  out <- technological_readiness(guide_rows(items), techs = "Coal")
  expect_true(all(grepl("technology-wide", out$explanation[out$supply_chain == "Midstream"], fixed = TRUE)))
  expect_true(all(grepl("Stage-specific: 2", out$explanation[out$supply_chain == "Upstream"], fixed = TRUE)))
})

test_that("a stage whose items lack a start-year TRL takes the technology-wide momentum", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Onshore wind", "Renewables", "Wind", "", "", "Power=Generation", 9, 9,
    "Floating offshore wind turbine", "Renewables", "Wind", "", "", "Power=Generation", 6, 8,
    "Coal mine methane use for energy supply", "Fossil fuels", "Coal", "Methane emissions monitoring and abatement", "", "Power=Generation", 7, 9,
    "Oxy-fuelling (coal)", "Fossil fuels", "Coal", "Carbon management", "", "CCUS=Capture;Power=Generation;Heat=Generation", NA, 7
  )
  tech_tbl <- guide_rows(items) |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech() |>
    technological_readiness_build_tech(techs = c("Coal", "Wind"))

  coal_down <- tech_tbl[tech_tbl$tech == "Coal" & tech_tbl$supply_chain == "Downstream", ]
  coal_up <- tech_tbl[tech_tbl$tech == "Coal" & tech_tbl$supply_chain == "Upstream", ]
  expect_identical(coal_down$basis, "stage")
  expect_identical(coal_down$momentum_basis, "technology")
  expect_equal(coal_down$trl_level_index, trl_bell_hard(7))
  # The technology-wide momentum comes from the one coal item with a 2020 reading.
  expect_equal(coal_down$trl_momentum_index, coal_up$trl_momentum_index)
  expect_false(is.na(coal_down$trl_index))

  # Wind only has Midstream items (turbine designs), so all three stages read the same values.
  wind <- tech_tbl[tech_tbl$tech == "Wind", ]
  expect_identical(wind$basis[match(c("Upstream", "Midstream", "Downstream"), wind$supply_chain)], c("technology", "stage", "technology"))
  expect_equal(dplyr::n_distinct(wind$trl_index), 1L)
})

test_that("a TRL 9 item's post-TRL-9 adoption stage puts its level on the 1-11 scale", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025, ~post2025,
    "Onshore wind", "Renewables", "Wind", "", "", "Power=Generation", 9, 9, "Commercial in many markets",
    "Seabed fixed offshore wind turbine", "Renewables", "Wind", "", "", "Power=Generation", 8, 9, "Building momentum",
    "Floating offshore wind turbine", "Renewables", "Wind", "", "", "Power=Generation", 7, 9, "Awaiting adoption",
    # A stage on an item below TRL 9 (the 2026 Guide has three) leaves its TRL alone.
    "Airborne wind energy system", "Renewables", "Wind", "", "", "Power=Generation", 4, 6, "Awaiting adoption",
    # A blank end-year TRL with a stage: the Guide grades adoption only from TRL 9 on.
    "Lithium-ion battery", "Transport", "Road transport", "Electric vehicle battery (electrical storage)", "Lithium based batteries", "Mobility=Electric (batteries)", 9, NA, "Commercial in many markets"
  )
  bell <- function(x) trl_bell_hard(x, min_trl = 2, mu = 6, max_trl = 11)
  momentum <- function(delta) (pmin(pmax(delta, 0), 3) / 3)^0.5
  build <- function(rows) {
    guide_rows(rows) |>
      technological_readiness_clean() |>
      technological_readiness_assign_tech() |>
      technological_readiness_build_tech(techs = c("Wind", "Batteries"))
  }
  cell <- function(tbl, tech, stage) tbl[tbl$tech == tech & tbl$supply_chain == stage, ]

  tech_tbl <- build(items)
  wind <- cell(tech_tbl, "Wind", "Midstream")
  expect_equal(wind$trl2025, mean(c(11, 10, 9, 6)))
  expect_equal(wind$trl_level_index, mean(bell(c(11, 10, 9, 6))))
  # The change since 2020 stays on the 1-9 scale the release reports for every year.
  expect_equal(wind$trl_delta_2020_2025, mean(c(0, 1, 2, 2)))
  expect_equal(wind$trl_momentum_index, mean(momentum(c(0, 1, 2, 2))))

  # The blank-TRL item counts instead of dropping out: level TRL 11, no change since 2020.
  battery <- cell(tech_tbl, "Batteries", "Midstream")
  expect_equal(battery$n_items, 1L)
  expect_equal(battery$trl2025, 11)
  expect_equal(battery$trl_level_index, 0)
  expect_equal(battery$trl_delta_2020_2025, 0)

  expect_equal(
    technological_readiness_extended_trl(
      c(9, 9, 9, 8, NA, NA, 9),
      c("Commercial in many markets", "building  momentum", "Awaiting adoption", "Building momentum",
        "Commercial in many markets", NA, NA)
    ),
    c(11, 10, 9, 8, 11, NA, 9)
  )

  # Without the column (the legacy extract, earlier releases) the reported TRL stands.
  plain <- build(dplyr::select(items, -post2025))
  expect_equal(cell(plain, "Wind", "Downstream")$trl2025, mean(c(9, 9, 9, 6)))
  expect_equal(cell(plain, "Batteries", "Midstream")$n_items, 0L)

  odd <- items[1, ]
  odd$post2025 <- "Mainstream"
  expect_warning(
    technological_readiness(guide_rows(odd), techs = "Wind"),
    "unknown post-TRL-9 adoption stage"
  )
})

test_that("an item no rule knows is reported and warned about", {
  items <- tibble::tribble(
    ~name, ~c1, ~c2, ~c3, ~c4, ~cc, ~trl2020, ~trl2025,
    "Onshore wind", "Renewables", "Wind", "", "", "Power=Generation", 9, 9,
    "Space elevator", "Space", "Launch", "", "", "", 2, 3
  )

  expect_warning(
    out <- technological_readiness(guide_rows(items), techs = "Wind"),
    "match no rule"
  )
  diagnostics <- attr(out, "mapping_diagnostics")
  expect_identical(diagnostics$unclassified_items$name, "Space elevator")
  expect_equal(diagnostics$pct_unclassified, 0.5)
  expect_true(all(c("item_table", "counts_by_tech_stage", "out_of_scope_by_rule") %in% names(diagnostics)))
})

test_that("mapping rules are validated when the config is read", {
  cfg <- read_iea_tech_map_rules()
  expect_true(all(c("rules", "legacy_rules") %in% names(cfg)))
  ids <- vapply(cfg$rules, function(rule) null_coalesce(rule$id, NA_character_), character(1))
  expect_false(anyNA(ids))
  expect_false(anyDuplicated(ids) > 0)
  # Every rule names an index technology or "none", and an out-of-scope rule says why.
  index_techs <- c(
    "Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind", "Oil",
    "Solar", "Gas", "Geothermal", "Electric Grid"
  )
  for (rule in cfg$rules) {
    techs <- unlist(rule$tech)
    if (identical(techs, "none")) {
      expect_true(!is.null(rule$reason) && nzchar(rule$reason), info = rule$id)
    } else {
      expect_true(all(techs %in% index_techs), info = rule$id)
      expect_true(!is.null(rule$stage), info = rule$id)
    }
  }

  expect_error(
    technological_readiness_validate_rules(list(list(id = "bare", tech = "Wind"))),
    "no matching condition"
  )
  expect_error(
    technological_readiness_validate_rules(list(list(id = "bad_stage", tech = "Wind", stage = "Sideways", path = "Renewables"))),
    "unknown stage"
  )
})

test_that("the legacy extract still maps through the legacy rules to every stage", {
  legacy <- tibble::tibble(
    name = c("Onshore wind turbine", "Vapour recovery units"),
    sector = c("Supply,Energy transformation,Power,Wind", "Supply,Fossil fuels,Oil and gas,Methane abatement"),
    supplyChain = c("Power generation", "CH4 abatement"),
    trl2022 = c(9, 9),
    trl2023 = c(9, 9)
  )
  assigned <- legacy |>
    technological_readiness_clean() |>
    technological_readiness_assign_tech()

  wind <- assigned[assigned$name == "Onshore wind turbine", ]
  expect_identical(unique(wind$tech), "Wind")
  expect_setequal(wind$stage, c("Upstream", "Midstream", "Downstream"))
  vru <- assigned[assigned$name == "Vapour recovery units", ]
  expect_setequal(unique(vru$tech), c("Oil", "Gas"))
})

test_that("the staged 2026 Guide is fully classified and separates oil from gas", {
  guide_path <- file.path(repo_root, "data", "raw", "IEACleanTechGuidepublicdataset.csv")
  skip_if_not(file.exists(guide_path), "IEA Clean Tech Guide public dataset not staged in data/raw")

  guide <- read.csv(guide_path, fileEncoding = "UTF-8-BOM")
  expect_no_warning(out <- technological_readiness(guide))
  diagnostics <- attr(out, "mapping_diagnostics")

  expect_equal(nrow(diagnostics$unclassified_items), 0L)
  expect_equal(nrow(out), 11L * 3L * 5L)
  trl <- out[out$variable == "TRL Index", ]
  expect_false(anyNA(trl$value))
  expect_true(all(trl$value >= 0 & trl$value <= 1))

  # Every mapped item has an end-year level, the blank-TRL "Lithium-ion battery" included, and
  # the adoption stage lifts some cells above the release's TRL 9 ceiling.
  cells <- merge(
    diagnostics$tech_stage_table, diagnostics$counts_by_tech_stage,
    by.x = c("tech", "supply_chain"), by.y = c("tech", "stage")
  )
  expect_equal(cells$n_items, cells$n)
  expect_true(any(diagnostics$tech_stage_table$trl2025 > 9))

  oil <- trl$value[trl$tech == "Oil"]
  gas <- trl$value[trl$tech == "Gas"]
  expect_false(isTRUE(all.equal(oil, gas)))
  # Stages now differ within a technology wherever the Guide has stage-specific items.
  spread <- tapply(trl$value, trl$tech, function(v) length(unique(round(v, 10))))
  expect_true(sum(spread > 1) >= 8)
})
