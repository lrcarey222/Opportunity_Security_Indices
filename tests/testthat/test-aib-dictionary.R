repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R")) source(file.path(repo_root, "R", f))
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "scripts", "utils", "aib_config.R"))
suppressPackageStartupMessages(library(dplyr))

dict <- tibble::tribble(
  ~hs6,     ~hs6_desc,                          ~index_code, ~index_name,           ~sector,       ~segment,           ~item_type,           ~stage,      ~specificity, ~scope,     ~primary, ~notes,
  "284690", "Rare-earth compounds",             "A",         "Materials & Inputs",  "Rare earths", "RE oxides",        "Processed Material", "Midstream", 1,            "core",     "TRUE",   "",
  "280530", "Rare-earth metals",                "A",         "Materials & Inputs",  "Rare earths", "RE metals",        "Processed Material", "Midstream", 1,            "core",     "TRUE",   "",
  "261220", "Thorium ores and concentrates",    "A",         "Materials & Inputs",  "Rare earths", "Monazite",         "Raw Material",       "Upstream",  2,            "core",     "TRUE",   "",
  "284610", "Cerium compounds",                 "A",         "Materials & Inputs",  "Rare earths", "Cerium",           "Processed Material", "Midstream", 3,            "extended", "TRUE",   "x",
  "850511", "Permanent magnets of metal",       "B",         "Electrotechnologies", "Magnets",     "NdFeB",            "Component",          "Midstream", 1,            "core",     "TRUE",   "",
  "280530", "Rare-earth metals",                "B",         "Electrotechnologies", "Magnets",     "RE metals (sec.)", "Processed Material", "Midstream", 2,            "core",     "FALSE",  ""
)

test_that("the dictionary becomes master rows: core primary rows by default, the index carried along", {
  m <- aib_dictionary_master(dict)
  expect_equal(sort(m$hs6), sort(c("284690", "280530", "261220", "850511")))   # no extended, no secondary row
  expect_equal(m$tech[m$hs6 == "850511"], "Magnets")
  expect_equal(m$supply_chain[m$hs6 == "261220"], "Upstream")                  # the dictionary's own stage: the grouping
  expect_equal(m$stage[m$hs6 == "261220"], "Extraction")                       # the six-stage label
  expect_equal(m$stage[m$hs6 == "850511"], "Components")
  expect_equal(m$index_code[m$hs6 == "850511"], "B")
  expect_true(all(m$essential))
  ext <- aib_dictionary_master(dict, "extended")
  expect_true("284610" %in% ext$hs6)
  expect_false(ext$essential[ext$hs6 == "284610"])                             # specificity 3
  expect_equal(sum(aib_dictionary_master(dict, primary_only = FALSE)$hs6 == "280530"), 2)
  expect_error(aib_dictionary_master(dplyr::select(dict, -item_type)), "item_type")
})

test_that("the six-stage label follows the item type; recycled raw material is Recovery", {
  it <- c("Raw Material", "Raw Material", "Raw Material", "Raw Material", "Processed Material", "Component", "Final Product", "Process Equipment",
          "Processed Material")
  txt <- c("Copper ores and concentrates", "Copper; waste and scrap", "Slag, ash and residues Metal residues (incl. black mass)",
           "Animal fats, boiled or oxidised Used cooking oil & inedible fat blends", "Copper; refined", "Wind blades", "Electric motors",
           "Furnaces for cells", "Waste-heat boilers parts")
  expect_equal(aib_dictionary_stage(it, txt),
               c("Extraction", "Recovery", "Recovery", "Recovery", "Refining", "Components", "Assembly", "Assembly", "Refining"))
  expect_equal(aib_dictionary_stage("  Final Product "), "Assembly")
  expect_error(aib_dictionary_stage("Service"), "unknown item_type")
})

test_that("in the electro-industrial dictionary exactly the recycled-feed primary rows are Recovery", {
  d <- utils::read.csv(file.path(repo_root, "data", "reference", "electro_industrial_hs6_dictionary.csv"),
                       colClasses = c(hs6 = "character", hs1992_join = "character"), stringsAsFactors = FALSE)
  m <- aib_dictionary_master(d, "extended", primary_only = TRUE)
  expect_setequal(m$hs6[m$stage == "Recovery"],
                  c("854921", "854929", "854919", "810530", "262099", "750300", "854913", "854914", "760200", "740400", "810931", "284450", "151800"))
  p <- d[toupper(d$primary) == "TRUE", ]
  st <- m$stage[match(p$hs6, m$hs6)]
  expect_true(all(st[p$item_type == "Raw Material"] %in% c("Extraction", "Recovery")))
  expect_true(all(st[p$item_type == "Processed Material"] == "Refining"))
  expect_true(all(st[p$item_type == "Component"] == "Components"))
  expect_true(all(st[p$item_type %in% c("Final Product", "Process Equipment")] == "Assembly"))
  expect_false(any(st == "Deployment"))
  # The Raw Material rows are exactly the dictionary's Upstream: the Resource-supplier role keeps its nodes.
  expect_true(all(st[p$stage == "Upstream"] %in% aib_resource_stages()))
  expect_false(any(st[p$stage != "Upstream"] %in% aib_resource_stages()))
})

test_that("splits match a sector and source stage, send unlisted codes to rest, and skip optional absent cells", {
  m <- aib_dictionary_master(dict, "extended")
  splits <- list(
    list(tech = "Rare earths", supply_chain = "Midstream", optional = TRUE,
         into = list(list(sub_sector = "Rare-earth metals", hs6 = list("280530"))), rest = "Rare-earth oxides & compounds"),
    list(tech = "Rare earths", supply_chain = "Upstream", optional = TRUE, rest = "Rare-earth ores (monazite)"),
    list(tech = "Solar", supply_chain = "Downstream", optional = TRUE, rest = "PV modules")
  )
  expect_silent(cells <- aib_master_cells(m, splits))
  expect_equal(cells$sub_sector[cells$hs6 == "280530"], "Rare-earth metals")
  expect_equal(sort(cells$sub_sector[cells$hs6 %in% c("284690", "284610")]), rep("Rare-earth oxides & compounds", 2))
  expect_equal(cells$sub_sector[cells$hs6 == "261220"], "Rare-earth ores (monazite)")
  expect_equal(cells$sub_sector[cells$hs6 == "850511"], "Magnets")             # not split: the sector name
  expect_equal(cells$source_stage[cells$hs6 == "261220"], "Upstream")
  expect_equal(cells$supply_chain[cells$hs6 == "261220"], "Extraction")
  # Without rest, an unassigned code is still an error.
  bad <- list(list(tech = "Rare earths", supply_chain = "Midstream", into = list(list(sub_sector = "Metals", hs6 = list("280530")))))
  expect_error(aib_master_cells(m, bad), "unassigned")
})

test_that("the OSI crosswalk weights six-stage cells by trade and leaves non-basket nodes neutral", {
  osi <- tibble::tribble(
    ~tech,               ~supply_chain, ~stage,       ~sub_sector,                         ~hs6,     ~excluded,
    "Electric Vehicles", "Upstream",    "Refining",   "Rare-Earth Compounds",              "284690", FALSE,
    "Wind",              "Upstream",    "Refining",   "Rare-Earth Compounds",              "284690", FALSE,
    "Electric Vehicles", "Upstream",    "Refining",   "Rare-Earth Metals",                 "280530", FALSE,
    "Electric Vehicles", "Midstream",   "Components", "Permanent Magnets & Magnet Parts",  "850511", FALSE,
    "Wind",              "Midstream",   "Components", "Permanent Magnets & Magnet Parts",  "850511", TRUE,     # excluded row
    "Motors",            "Midstream",   "Assembly",   "Motors",                            "850152", FALSE,
    "Electric Vehicles", "Midstream",   "Components", "Traction Motors",                   "850152", FALSE
  )
  node_codes <- tibble::tibble(node_id = c("ree", "ree", "mag", "hp", "mot"), hs6 = c("284690", "280530", "850511", "841861", "850152"))
  trade <- tibble::tibble(hs6 = c("284690", "280530", "850511", "841861", "850152"), value = c(90, 10, 50, 40, 30))
  xw <- aib_osi_crosswalk(node_codes, osi, trade, min_share = 0.2)
  ree <- xw[xw$node_id == "ree", ]
  expect_equal(ree$score_techs, "Electric Vehicles; Wind")                    # Wind holds 90% of the matched trade
  expect_equal(ree$score_stage, "Refining")                                   # the master's six-stage `stage`
  expect_equal(ree$osi_coverage, 1)
  expect_equal(strsplit(ree$osi_subcomponents, "; ")[[1]][1], "electric-vehicles-refining::Rare-Earth Compounds")
  expect_equal(xw$score_techs[xw$node_id == "mag"], "Electric Vehicles")      # the excluded Wind row does not count
  expect_equal(xw$yf_cells[xw$node_id == "mag"], "electric-vehicles-components")
  # A code that is a component of one technology and a final product of another sits in both cells.
  expect_setequal(strsplit(xw$yf_cells[xw$node_id == "mot"], "; ")[[1]], c("electric-vehicles-components", "motors-assembly"))
  # ...and each technology is scored at its own stage, not both at one shared score_stage (whose tie
  # goes to the earlier stage in chain order).
  expect_equal(ree$score_cells, "Electric Vehicles|Refining; Wind|Refining")
  expect_setequal(strsplit(xw$score_cells[xw$node_id == "mot"], "; ")[[1]], c("Electric Vehicles|Components", "Motors|Assembly"))
  expect_equal(xw$score_stage[xw$node_id == "mot"], "Components")
  expect_true(is.na(xw$score_cells[xw$node_id == "hp"]))
  hp <- xw[xw$node_id == "hp", ]
  expect_true(is.na(hp$score_techs))
  expect_equal(hp$osi_coverage, 0)
  bad <- osi
  bad$stage[1] <- "Midstream"
  expect_error(aib_osi_crosswalk(node_codes, bad, trade), "not supply-chain stages")
})

test_that("node score lookups read each technology at its own stage", {
  # HS 850760 (li-ion cells and packs) is Batteries Assembly and Electric Vehicles Components; Electric
  # Vehicles Assembly holds the vehicles, which a cells node must not read.
  op <- tibble::tribble(
    ~reporter_iso, ~partner_iso, ~tech,               ~supply_chain, ~value,
    "KOR",         "USA",        "Batteries",         "Assembly",    0.8,
    "KOR",         "USA",        "Electric Vehicles", "Components",  0.6,
    "KOR",         "USA",        "Electric Vehicles", "Assembly",    0.1
  )
  env <- aib_node_dyad_lookup(op, c("Batteries", "Electric Vehicles"), c("Assembly", "Components"))
  expect_equal(aib_lookup_pairs(env, "KOR", "USA"), 0.7)
  # One stage for every technology still works (a node whose cells share a stage).
  expect_equal(aib_lookup_pairs(aib_node_dyad_lookup(op, "Electric Vehicles", "Assembly"), "KOR", "USA"), 0.1)
  pams <- tibble::tibble(iso3 = "KOR", tech = op$tech, supply_chain = op$supply_chain, intensity = op$value)
  expect_equal(aib_pams_node(pams, c("Batteries", "Electric Vehicles"), c("Assembly", "Components"))$intensity, 0.7)
  expect_error(aib_score_cell_keys(c("Batteries", "Electric Vehicles", "Solar"), c("Assembly", "Components")), "one per technology")
  expect_equal(aib_score_cell_keys(character(), character()), character())
})

test_that("the taxonomy overlay replaces the taxonomy keys of config/aib.yml", {
  d <- aib_read_config(repo_root, "dictionary")
  expect_equal(d$taxonomy, "dictionary")
  expect_equal(d$nodes$source, "dictionary")
  expect_equal(d$strategic_scope$mode, "all")
  m <- aib_read_config(repo_root, "master")
  expect_null(m$nodes$source)
  expect_equal(m$strategic_scope$mode, "electro_industrial")
  expect_identical(d$optimization, m$optimization)                            # shared settings untouched
  expect_error(aib_read_config(repo_root, "nope"), "Unknown AIB taxonomy")
  # Every link, proxy and validation name of the dictionary overlay is a node the splits create.
  names <- unique(unlist(lapply(d$nodes$splits, function(s) c(s$rest, vapply(s$into, function(p) p$sub_sector, "")))))
  used <- c(unlist(d$supply_chain_links), unlist(d$material_links), vapply(d$production_proxies, function(p) p$sub_sector, ""),
            unlist(d$validation_cases))
  expect_equal(setdiff(used, names), character())
  # Split keys stay on the taxonomy's own Upstream / Midstream / Downstream (the node grouping).
  for (cf in list(d, m)) expect_true(all(vapply(cf$nodes$splits, function(s) s$supply_chain, "") %in% SUPPLY_CHAIN_LEGACY_STAGES))
})
