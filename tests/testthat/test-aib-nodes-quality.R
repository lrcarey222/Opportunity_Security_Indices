repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages(library(dplyr))

master <- tibble::tribble(
  ~tech,               ~supply_chain, ~stage,       ~sub_sector,                         ~hs6,     ~essential, ~legacy_hs, ~excluded, ~note,
  "Electric Vehicles", "Midstream",   "Components", "Permanent Magnets & Magnet Parts",  "850511", TRUE,       FALSE,      FALSE,     "",
  "Wind",              "Midstream",   "Components", "Permanent Magnets & Magnet Parts",  "850511", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Refining",   "Polysilicon & Wafers",              "280461", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Components", "Polysilicon & Wafers",              "381800", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Components", "Solar Cells",                       "854142", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Components", "Solar Cells",                       "854140", TRUE,       TRUE,       FALSE,     "legacy",
  "Solar",             "Midstream",   "",           "Dropped",                           "999999", TRUE,       FALSE,      TRUE,      "excluded",
  "Batteries",         "Upstream",    "Extraction", "Ores & Concentrates",               "250410", TRUE,       FALSE,      FALSE,     "",
  "Electric Grid",     "Upstream",    "Extraction", "Ores & Concentrates",               "260300", TRUE,       FALSE,      FALSE,     ""
)
splits <- list(list(tech = "Solar", supply_chain = "Midstream", sub_sector = "Polysilicon & Wafers",
                    into = list(list(sub_sector = "Polysilicon", hs6 = list("280461")), list(sub_sector = "Wafers", hs6 = list("381800")))))

test_that("master cells drop excluded rows and apply splits", {
  cells <- aib_master_cells(master, splits)
  expect_false("Dropped" %in% cells$sub_sector)
  expect_setequal(cells$sub_sector[cells$tech == "Solar"], c("Polysilicon", "Wafers", "Solar Cells", "Solar Cells"))
  expect_equal(cells$source_stage[cells$hs6 == "280461"], "Midstream")      # the master's supply_chain: the grouping
  expect_equal(cells$supply_chain[cells$hs6 == "280461"], "Refining")       # the master's six-stage stage
  bad <- list(list(tech = "Solar", supply_chain = "Midstream", sub_sector = "Polysilicon & Wafers",
                   into = list(list(sub_sector = "Polysilicon", hs6 = list("280461")))))
  expect_error(aib_master_cells(master, bad), "unassigned")
  old <- master
  old$stage[1] <- "Midstream"
  expect_error(aib_master_cells(old), "not supply-chain stages")
  expect_error(aib_master_cells(dplyr::select(master, -stage)), "stage")
})

test_that("identical HS6 sets across technologies are one physical node; same names with different sets are not", {
  nd <- aib_build_nodes(aib_master_cells(master, splits))
  mag <- nd$nodes[nd$nodes$node_id == "permanent-magnets-and-magnet-parts", ]
  expect_equal(nrow(mag), 1)
  expect_equal(mag$techs, "Electric Vehicles; Wind")
  expect_equal(mag$supply_chain, "Components")
  expect_equal(mag$source_stage, "Midstream")
  expect_equal(sum(nd$node_cells$node_id == "permanent-magnets-and-magnet-parts"), 2)
  ores <- nd$nodes$node_id[grepl("^ores-and-concentrates", nd$nodes$node_id)]
  expect_length(ores, 2)
  expect_true(all(grepl("--upstream--", ores)))                              # disambiguated by the source stage
  expect_true(all(nd$nodes$supply_chain[nd$nodes$node_id %in% ores] == "Extraction"))
  expect_false(any(nd$nodes$stage_mixed))
  expect_equal(nd$nodes$node_id, nd$nodes$node_id[order(nd$nodes$primary_tech, match(nd$nodes$source_stage, SUPPLY_CHAIN_LEGACY_STAGES),
                                                         nd$nodes$sub_sector)])
})

test_that("a node's six-stage label is the stage with the most world trade; mixed nodes are flagged", {
  m <- tibble::tribble(
    ~tech,      ~supply_chain, ~stage,       ~sub_sector,   ~hs6,
    "Uranium",  "Upstream",    "Extraction", "Ores",        "261210",
    "Uranium",  "Upstream",    "Recovery",   "Ores",        "284450",
    "Uranium",  "Midstream",   "Components", "Fuel",        "840130",
    "Uranium",  "Midstream",   "Refining",   "Fuel",        "810910",
    "Motors",   "Midstream",   "Assembly",   "Motors",      "850152",
    "EV",       "Midstream",   "Components", "Motors",      "850152"
  )
  cells <- aib_master_cells(m)
  trade <- tibble::tibble(hs6 = c("261210", "284450", "840130", "810910", "850152"), value = c(10, 90, 900, 100, 50))
  nd <- aib_build_nodes(cells, trade)
  n <- function(id, col) nd$nodes[[col]][nd$nodes$node_id == id]
  expect_equal(n("ores", "supply_chain"), "Recovery")                        # spent fuel has 90% of the trade
  expect_true(n("ores", "stage_mixed"))
  expect_equal(n("ores", "stage_mix"), "Recovery 90%, Extraction 10%")
  expect_equal(n("fuel", "supply_chain"), "Components")
  # The shared motors node: one code, a final product of Motors and a component of EV. Each cell keeps
  # its technology's stage; the node splits the code's trade between them and a tie goes up the chain.
  expect_equal(n("motors", "supply_chain"), "Components")
  expect_true(n("motors", "stage_mixed"))
  mc <- nd$node_cells[nd$node_cells$node_id == "motors", ]
  expect_equal(mc$supply_chain[mc$tech == "Motors"], "Assembly")
  expect_equal(mc$supply_chain[mc$tech == "EV"], "Components")
  # Without trade the most codes win.
  nd0 <- aib_build_nodes(cells)
  expect_equal(nd0$nodes$supply_chain[nd0$nodes$node_id == "ores"], "Extraction")
  # The node set does not depend on the labels: the same ids, cells and codes whatever the trade.
  expect_identical(nd0$nodes$node_id, nd$nodes$node_id)
  expect_identical(nd0$node_codes, nd$node_codes)
  # The data-quality flag reads like stage_conflict and puts the node under caution.
  summ <- tibble::tibble(node_id = nd$nodes$node_id, world_trade = 1e9, world_trade_hist = 1e9, mirror_share = 0, n_material_exporters = 20L,
                         n_material_importers = 40L, missing_rca_share = 0)
  dq <- list(min_world_trade_usd = 5e7, min_material_exporters = 3, min_material_importers = 5,
             caution_mirror_share = 0.3, caution_missing_rca_share = 0.2, caution_nonessential_share = 0.5)
  q <- aib_data_quality(nd$nodes, dplyr::mutate(nd$node_codes, essential = TRUE), summ, dq)
  expect_match(q$flags[q$node_id == "ores"], "codes span several supply-chain stages \\(Recovery 90%, Extraction 10% of trade\\), filed under Recovery")
  expect_equal(q$data_quality[q$node_id == "ores"], "Usable with caution")
  expect_false(grepl("; ", q$flags[q$node_id == "ores"], fixed = TRUE))       # one flag: the export splits flags on "; "
  expect_equal(q$flags[q$node_id == "fuel"], "codes span several supply-chain stages (Components 90%, Refining 10% of trade), filed under Components")
})

test_that("stage_by_trade splits a code's trade among its stages and orders ties along the chain", {
  cs <- tibble::tibble(node_id = c("a", "a", "b", "b", "c"), hs6 = c("100001", "100002", "100003", "100003", "100004"),
                       supply_chain = c("Assembly", "Refining", "Assembly", "Components", "Deployment"))
  out <- aib_stage_by_trade(cs, "node_id", tibble::tibble(hs6 = c("100001", "100002", "100003"), value = c(30, 70, 10)))
  expect_equal(out$supply_chain, c("Refining", "Components", "Deployment"))
  expect_equal(out$stage_mix[1], "Refining 70%, Assembly 30%")
  expect_equal(out$stage_mix[2], "Components 50%, Assembly 50%")
  expect_true(is.na(out$stage_mix[3]))
  expect_equal(out$stage_mixed, c(TRUE, TRUE, FALSE))
  tiny <- aib_stage_by_trade(cs[1:2, ], "node_id", tibble::tibble(hs6 = c("100001", "100002"), value = c(1, 1e4)))
  expect_equal(tiny$stage_mix, "Refining 100%, Assembly <1%")
})

test_that("data quality flags thin trade, solar revisions and multi-use codes", {
  nd <- aib_build_nodes(aib_master_cells(master, splits))
  summ <- tibble::tibble(
    node_id = nd$nodes$node_id,
    world_trade = c(1e9, 1e9, 1e9, 1e7, 1e9, 1e9)[seq_len(nrow(nd$nodes))],
    world_trade_hist = 1e9, mirror_share = 0.05, n_relationships = 100L,
    n_exporters = 50L, n_material_exporters = 20L, top_exporter = "CHN", top_exporter_share = 0.5,
    export_hhi = 0.3, china_export_share = 0.5, missing_rca_share = 0, n_rca_above_1 = 5L,
    n_importers = 80L, n_material_importers = 40L, growth = 0
  )
  dq <- list(min_world_trade_usd = 5e7, min_material_exporters = 3, min_material_importers = 5, material_flow_usd = 1e6,
             caution_mirror_share = 0.3, caution_missing_rca_share = 0.2, caution_nonessential_share = 0.5)
  q <- aib_data_quality(nd$nodes, nd$node_codes, summ, dq)
  cells <- q[q$node_id == "solar-cells", ]
  expect_equal(cells$data_quality, "Usable with caution")
  expect_match(cells$flags, "solar HS revision")
  expect_match(cells$flags, "HS2022 revision")
  thin <- q[summ$world_trade[match(q$node_id, summ$node_id)] < 5e7, ]
  expect_true(all(thin$data_quality == "Insufficient for optimization"))
})

test_that("insufficient nodes are aggregated upward through a material link first", {
  # The grouping is the source stage: the six-stage labels differ (ore Extraction, compounds Refining;
  # x1 Extraction, x2 Recovery) and change neither the siblings, the merged id nor the label.
  nodes <- tibble::tibble(node_id = c("ore", "comp", "metal", "x1", "x2"),
                          sub_sector = c("RE Ore", "RE Compounds", "RE Metals", "Odd 1", "Odd 2"),
                          supply_chain = c("Extraction", "Refining", "Refining", "Extraction", "Recovery"), source_stage = "Upstream",
                          primary_tech = c("EV", "EV", "EV", "Wind", "Wind"))
  node_cells <- tibble::tibble(node_id = nodes$node_id, tech = nodes$primary_tech, supply_chain = nodes$supply_chain, source_stage = "Upstream",
                               sub_sector = nodes$sub_sector)
  quality <- tibble::tibble(node_id = nodes$node_id, world_trade = c(1, 100, 50, 1, 1),
                            data_quality = c("Insufficient for optimization", "Usable with caution", "High confidence",
                                             "Insufficient for optimization", "Insufficient for optimization"))
  edges <- aib_supply_chain_edges(node_cells, list(list("RE Ore", "RE Compounds")))
  expect_equal(sum(grepl("same technology stage", edges$basis)), 4)          # ore-comp, ore-metal, comp-metal, x1-x2
  mm <- aib_quality_merge_map(quality, nodes, node_cells, edges)
  expect_equal(mm$merged_id[mm$node_id == "ore"], "comp")
  expect_equal(mm$merged_label[mm$node_id == "comp"], "RE Compounds + RE Ore")
  expect_equal(unique(mm$merged_id[mm$node_id %in% c("x1", "x2")]), "wind-upstream--aggregated")
  expect_equal(unique(mm$merged_label[mm$node_id %in% c("x1", "x2")]), "Wind Upstream: aggregated low-coverage sub-sectors (Odd 1, Odd 2)")
  expect_error(aib_supply_chain_edges(dplyr::select(node_cells, -source_stage)), "source_stage")
})

test_that("coherence reaches two hops on the supply-chain graph", {
  edges <- tibble::tibble(node_a = c("a", "b"), node_b = c("b", "c"), basis = "material link")
  m <- aib_coherence_matrix(edges, c("a", "b", "c", "d"), max_hops = 2)
  expect_true(m["a", "c"])
  expect_false(m["a", "d"])
  expect_false(aib_coherence_matrix(edges, c("a", "b", "c"), max_hops = 1)["a", "c"])
})

test_that("LOW and COORDINATE classes without physical validation carry the warning", {
  nd <- tibble::tibble(
    node_id = c("wafers", "polysilicon", "magnets", "ree", "lithium"),
    node_classification = c("LOW INTERVENTION", "LOW INTERVENTION", "LOW INTERVENTION", "COORDINATE", "BUILD"),
    shock_exposure = c(0.30, 0.10, 0.30, 0.40, 0.50),
    physical_data = c("complete", "complete", "none", "incomplete", "complete"),
    physically_divergent = c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  tr <- aib_physical_validation(nd, constrained = FALSE, low_exposure = 0.15)
  # Trade only: divergent physical data leave a LOW class unvalidated; no data is flagged in the
  # validation column but not warned (there is nothing to diverge from).
  expect_equal(tr$classification_warning, c("PHYSICAL CAPABILITY NOT VALIDATED", "PHYSICAL CAPABILITY NOT VALIDATED", NA, "PHYSICAL CAPABILITY NOT VALIDATED", NA))
  expect_equal(tr$physical_validation[3], "trade only; no physical data")
  pc <- aib_physical_validation(nd, constrained = TRUE, low_exposure = 0.15)
  # Production-constrained: the wafer class rests on physically bounded suppliers; polysilicon's
  # LOW rests on low trade exposure, which the physical bound does not validate.
  expect_true(is.na(pc$classification_warning[1]))
  expect_equal(pc$classification_warning[2], "PHYSICAL CAPABILITY NOT VALIDATED")
  expect_match(pc$physical_validation[2], "understates")
  expect_equal(pc$classification_warning[4], "PHYSICAL CAPABILITY NOT VALIDATED")  # incomplete data
  expect_true(is.na(pc$classification_warning[5]))                                   # BUILD is never softened
})
