repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages(library(dplyr))

master <- tibble::tribble(
  ~tech,               ~supply_chain, ~sub_sector,                         ~hs6,     ~essential, ~legacy_hs, ~excluded, ~note,
  "Electric Vehicles", "Midstream",   "Permanent Magnets & Magnet Parts",  "850511", TRUE,       FALSE,      FALSE,     "",
  "Wind",              "Midstream",   "Permanent Magnets & Magnet Parts",  "850511", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Polysilicon & Wafers",              "280461", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Polysilicon & Wafers",              "381800", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Solar Cells",                       "854142", TRUE,       FALSE,      FALSE,     "",
  "Solar",             "Midstream",   "Solar Cells",                       "854140", TRUE,       TRUE,       FALSE,     "legacy",
  "Solar",             "Midstream",   "Dropped",                           "999999", TRUE,       FALSE,      TRUE,      "excluded",
  "Batteries",         "Upstream",    "Ores & Concentrates",               "250410", TRUE,       FALSE,      FALSE,     "",
  "Electric Grid",     "Upstream",    "Ores & Concentrates",               "260300", TRUE,       FALSE,      FALSE,     ""
)
splits <- list(list(tech = "Solar", supply_chain = "Midstream", sub_sector = "Polysilicon & Wafers",
                    into = list(list(sub_sector = "Polysilicon", hs6 = list("280461")), list(sub_sector = "Wafers", hs6 = list("381800")))))

test_that("master cells drop excluded rows and apply splits", {
  cells <- aib_master_cells(master, splits)
  expect_false("Dropped" %in% cells$sub_sector)
  expect_setequal(cells$sub_sector[cells$tech == "Solar"], c("Polysilicon", "Wafers", "Solar Cells", "Solar Cells"))
  bad <- list(list(tech = "Solar", supply_chain = "Midstream", sub_sector = "Polysilicon & Wafers",
                   into = list(list(sub_sector = "Polysilicon", hs6 = list("280461")))))
  expect_error(aib_master_cells(master, bad), "unassigned")
})

test_that("identical HS6 sets across technologies are one physical node; same names with different sets are not", {
  nd <- aib_build_nodes(aib_master_cells(master, splits))
  mag <- nd$nodes[nd$nodes$node_id == "permanent-magnets-and-magnet-parts", ]
  expect_equal(nrow(mag), 1)
  expect_equal(mag$techs, "Electric Vehicles; Wind")
  expect_equal(sum(nd$node_cells$node_id == "permanent-magnets-and-magnet-parts"), 2)
  ores <- nd$nodes$node_id[grepl("^ores-and-concentrates", nd$nodes$node_id)]
  expect_length(ores, 2)
  expect_true(all(grepl("--upstream--", ores)))
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
  nodes <- tibble::tibble(node_id = c("ore", "comp", "metal", "x1", "x2"),
                          sub_sector = c("RE Ore", "RE Compounds", "RE Metals", "Odd 1", "Odd 2"),
                          supply_chain = "Upstream", primary_tech = c("EV", "EV", "EV", "Wind", "Wind"))
  node_cells <- tibble::tibble(node_id = nodes$node_id, tech = nodes$primary_tech, supply_chain = "Upstream", sub_sector = nodes$sub_sector)
  quality <- tibble::tibble(node_id = nodes$node_id, world_trade = c(1, 100, 50, 1, 1),
                            data_quality = c("Insufficient for optimization", "Usable with caution", "High confidence",
                                             "Insufficient for optimization", "Insufficient for optimization"))
  edges <- aib_supply_chain_edges(node_cells, list(list("RE Ore", "RE Compounds")))
  mm <- aib_quality_merge_map(quality, nodes, node_cells, edges)
  expect_equal(mm$merged_id[mm$node_id == "ore"], "comp")
  expect_equal(mm$merged_label[mm$node_id == "comp"], "RE Compounds + RE Ore")
  expect_equal(unique(mm$merged_id[mm$node_id %in% c("x1", "x2")]), "wind-upstream--aggregated")
})

test_that("coherence reaches two hops on the supply-chain graph", {
  edges <- tibble::tibble(node_a = c("a", "b"), node_b = c("b", "c"), basis = "material link")
  m <- aib_coherence_matrix(edges, c("a", "b", "c", "d"), max_hops = 2)
  expect_true(m["a", "c"])
  expect_false(m["a", "d"])
  expect_false(aib_coherence_matrix(edges, c("a", "b", "c"), max_hops = 1)["a", "c"])
})
