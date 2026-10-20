repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R")) source(file.path(repo_root, "R", f))
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "scripts", "utils", "aib_config.R"))
suppressPackageStartupMessages(library(dplyr))

dict <- tibble::tribble(
  ~hs6,     ~index_code, ~index_name,             ~sector,       ~segment,          ~stage,       ~specificity, ~scope,      ~primary, ~notes,
  "284690", "A",         "Materials & Inputs",    "Rare earths", "RE oxides",       "Midstream",  1,            "core",      "TRUE",   "",
  "280530", "A",         "Materials & Inputs",    "Rare earths", "RE metals",       "Midstream",  1,            "core",      "TRUE",   "",
  "261220", "A",         "Materials & Inputs",    "Rare earths", "Monazite",        "Upstream",   2,            "core",      "TRUE",   "",
  "284610", "A",         "Materials & Inputs",    "Rare earths", "Cerium",          "Midstream",  3,            "extended",  "TRUE",   "x",
  "850511", "B",         "Electrotechnologies",   "Magnets",     "NdFeB",           "Midstream",  1,            "core",      "TRUE",   "",
  "280530", "B",         "Electrotechnologies",   "Magnets",     "RE metals (sec.)", "Midstream", 2,            "core",      "FALSE",  ""
)

test_that("the dictionary becomes master rows: core primary rows by default, the index carried along", {
  m <- aib_dictionary_master(dict)
  expect_equal(sort(m$hs6), sort(c("284690", "280530", "261220", "850511")))   # no extended, no secondary row
  expect_equal(m$tech[m$hs6 == "850511"], "Magnets")
  expect_equal(m$supply_chain[m$hs6 == "261220"], "Upstream")
  expect_equal(m$index_code[m$hs6 == "850511"], "B")
  expect_true(all(m$essential))
  ext <- aib_dictionary_master(dict, "extended")
  expect_true("284610" %in% ext$hs6)
  expect_false(ext$essential[ext$hs6 == "284610"])                             # specificity 3
  expect_equal(sum(aib_dictionary_master(dict, primary_only = FALSE)$hs6 == "280530"), 2)
})

test_that("splits match a sector and stage, send unlisted codes to rest, and skip optional absent cells", {
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
  # Without rest, an unassigned code is still an error.
  bad <- list(list(tech = "Rare earths", supply_chain = "Midstream", into = list(list(sub_sector = "Metals", hs6 = list("280530")))))
  expect_error(aib_master_cells(m, bad), "unassigned")
})

test_that("the OSI crosswalk weights cells by trade and leaves non-basket nodes neutral", {
  osi <- tibble::tribble(
    ~tech,               ~supply_chain, ~sub_sector,                         ~hs6,     ~excluded,
    "Electric Vehicles", "Upstream",    "Rare-Earth Compounds",              "284690", FALSE,
    "Wind",              "Upstream",    "Rare-Earth Compounds",              "284690", FALSE,
    "Electric Vehicles", "Upstream",    "Rare-Earth Metals",                 "280530", FALSE,
    "Electric Vehicles", "Midstream",   "Permanent Magnets & Magnet Parts",  "850511", FALSE,
    "Wind",              "Midstream",   "Permanent Magnets & Magnet Parts",  "850511", TRUE    # excluded row
  )
  node_codes <- tibble::tibble(node_id = c("ree", "ree", "mag", "hp"), hs6 = c("284690", "280530", "850511", "841861"))
  trade <- tibble::tibble(hs6 = c("284690", "280530", "850511", "841861"), value = c(90, 10, 50, 40))
  xw <- aib_osi_crosswalk(node_codes, osi, trade, min_share = 0.2)
  ree <- xw[xw$node_id == "ree", ]
  expect_equal(ree$score_techs, "Electric Vehicles; Wind")                    # Wind holds 90% of the matched trade
  expect_equal(ree$score_stage, "Upstream")
  expect_equal(ree$osi_coverage, 1)
  expect_equal(strsplit(ree$osi_subcomponents, "; ")[[1]][1], "electric-vehicles-upstream::Rare-Earth Compounds")
  expect_equal(xw$score_techs[xw$node_id == "mag"], "Electric Vehicles")      # the excluded Wind row does not count
  expect_equal(xw$yf_cells[xw$node_id == "mag"], "electric-vehicles-midstream")
  hp <- xw[xw$node_id == "hp", ]
  expect_true(is.na(hp$score_techs))
  expect_equal(hp$osi_coverage, 0)
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
})
