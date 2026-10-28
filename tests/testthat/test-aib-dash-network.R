repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(repo_root, "R", "aib", "aib_nodes.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_export.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_check.R"))
source(file.path(repo_root, "R", "dashboard", "country", "country_util.R"))
suppressPackageStartupMessages(library(dplyr))

# A two-scenario run with the columns scripts/32 writes into aib_results.rds.
aib_net_res <- function() {
  countries <- tibble::tibble(
    iso3 = c("DEU", "USA", "DEU", "USA"), scenario = c("reallocation", "reallocation", "conservative", "conservative"),
    supplier_n_partners = c(3, 0, 2, 0), buyer_n_partners = c(0, 2, 0, 1), investment_host_n_partners = c(1, 0, 1, 0), investor_n_partners = c(0, 1, 0, 1),
    supplier_n_selected = c(9, 0, 4, 0), buyer_n_selected = c(0, 9, 0, 4), investment_host_n_selected = c(1, 0, 1, 0), investor_n_selected = c(0, 1, 0, 1),
    supplier_headline_contribution = c(0.25, 0, 0.1, 0), buyer_headline_contribution = c(0, 0.25, 0, 0.1),
    investment_host_headline_contribution = c(0.1, 0, 0.1, 0), investor_headline_contribution = c(0, 0.1, 0, 0.1),
    supplier_cumulative_contribution = c(1.2, 0, 0.5, 0), buyer_cumulative_contribution = c(0, 1.2, 0, 0.5),
    investment_host_cumulative_contribution = c(0.2, 0, 0.2, 0), investor_cumulative_contribution = c(0, 0.2, 0, 0.2),
    supplier_cumulative_coordination_dividend = c(0.05, 0, 0, 0), buyer_cumulative_coordination_dividend = c(0, 0.05, 0, 0),
    supplier_headline_volume_usd = c(5e8, 0, 2e8, 0), buyer_headline_volume_usd = c(0, 5e8, 0, 2e8),
    supplier_n_extraction_partners = c(1, 0, 1, 0), buyer_n_extraction_partners = 0, investment_host_n_extraction_partners = 0, investor_n_extraction_partners = 0,
    supplier_n_refining_partners = 0, buyer_n_refining_partners = 0, investment_host_n_refining_partners = 0, investor_n_refining_partners = 0,
    supplier_n_components_partners = c(2, 0, 1, 0), buyer_n_components_partners = c(0, 1, 0, 1), investment_host_n_components_partners = c(1, 0, 1, 0), investor_n_components_partners = c(0, 1, 0, 1),
    supplier_n_assembly_partners = 0, buyer_n_assembly_partners = 0, investment_host_n_assembly_partners = 0, investor_n_assembly_partners = 0,
    supplier_n_deployment_partners = 0, buyer_n_deployment_partners = 0, investment_host_n_deployment_partners = 0, investor_n_deployment_partners = 0,
    supplier_n_recovery_partners = 0, buyer_n_recovery_partners = 0, investment_host_n_recovery_partners = 0, investor_n_recovery_partners = 0,
    technologies = "Solar", n_technologies = 1, roles = c("investment_host; supplier", "buyer; investor", "investment_host; supplier", "buyer; investor"),
    headline_lost_usd = c(NA, 3e9, NA, 3e9), n_exposed_subsectors = c(NA, 2, NA, 2)
  )
  edges <- tibble::tibble(
    supplier_iso = c("DEU", "DEU"), buyer_iso = c("USA", "USA"), scenario = c("reallocation", "conservative"),
    n_trade_selected = c(9, 4), n_investment_selected = c(1, 1), n_shocks = c(2, 2), n_subsectors = 1, n_technologies = 1,
    technologies = "Solar", subsectors = "solar-cells", n_high_severity = 1,
    extraction = FALSE, refining = FALSE, components = TRUE, assembly = FALSE, deployment = FALSE, recovery = FALSE,
    headline_trade_volume_usd = c(5e8, 2e8), headline_trade_contribution = c(0.25, 0.1), headline_investment_contribution = 0.1,
    cumulative_trade_resilience_contribution = c(1.2, 0.5), cumulative_coordination_dividend = c(0.05, 0),
    cumulative_investment_resilience_contribution = 0.2, export_opportunity = 0.9, friendshore = NA_real_
  )
  pairs <- tibble::tibble(
    node_id = "solar-cells", shock_id = c("china_100", "china_100", "china_100", "china_50"), scenario = "reallocation",
    supply_constraint = "production_constrained", investor_iso = c("USA", "USA", "DEU", "USA"), host_iso = c("DEU", "KOR", "DEU", "DEU"),
    increment = 0.1, supply_from_new_capacity = c(0.08, 0.07, 0.01, 0.05), supply_from_new_capacity_usd = c(8e8, 7e8, 1e8, 5e8),
    investor_lost_share = 0.4, export_opportunity = 0.9, friendshore = 0.7,
    relationship = c("investment partnership", "investment partnership", "domestic", "investment partnership"), host_rank = c(1L, 2L, 1L, 1L)
  )
  hosts <- tibble::tibble(
    node_id = "solar-cells", shock_id = "china_100", scenario = "reallocation", supply_constraint = "production_constrained",
    increment = c(0.1, 0.1, 0.05, 0.1), host_iso = c("DEU", "KOR", "DEU", "JPN"), host_rank = c(1L, 2L, 1L, NA),
    marginal_resilience_gain = c(0.09, 0.08, 0.05, 0.01), coverage_before = 0.5, coverage_after = c(0.59, 0.58, 0.55, 0.51),
    new_capacity_used = 0.09, additional_demand_served_usd = 9e8, capability_tier = 3L, capability_basis = "production share in the node"
  )
  list(
    meta = list(headline_shock = "china_100", headline_scenario = "reallocation", headline_semantics = "hold", headline_constraint = "production_constrained",
                strategic_scope = "electro_industrial", demand_scope = "coalition", candidate_universe = "global", universe = "coalition demand / global partners",
                config = list(shocks = list(list(id = "china_100"), list(id = "china_50")), investment = list(headline_increment = 0.1))),
    countries = countries, edges = edges, investment_pairs = pairs, investment = hosts
  )
}

test_that("the network section keeps the headline scenario, four sides and the variant", {
  n <- aib_dash_network(aib_net_res())
  expect_length(n$countries, 2)
  deu <- n$countries[[1]]
  expect_equal(deu$iso3, "DEU")
  expect_equal(deu$supplier$headline, 0.25)
  expect_equal(deu$supplier$cumulative, 1.2)
  expect_equal(deu$host$partners, 1L)
  expect_null(deu$host$volumeUsd)
  expect_true(is.na(deu$lostUsd))
  expect_equal(n$countries[[2]]$lostUsd, 3e9)
  expect_length(n$edges, 1)
  expect_equal(n$edges[[1]]$trade, 0.25)
  expect_equal(as.character(n$edges[[1]]$stages), "Components")
  # Partner counts by stage: the six stages in lower case, no Upstream / Midstream / Downstream.
  expect_equal(deu$supplier$components, 2L)
  expect_equal(deu$supplier$extraction, 1L)
  expect_true(all(tolower(SUPPLY_CHAIN_STAGES) %in% names(deu$host)))
  expect_false(any(c("upstream", "midstream", "downstream") %in% names(deu$supplier)))
  old <- aib_net_res()
  old$edges$upstream <- TRUE                                                 # a three-stage flag is not a stage
  expect_equal(as.character(aib_dash_network(old)$edges[[1]]$stages), "Components")
  expect_equal(n$variant$scenario, "reallocation")
  expect_equal(as.character(n$variant$shocks), c("china_100", "china_50"))
  j <- as.character(country_json(n$edges[[1]]))
  expect_match(j, '"nodes":\\["solar-cells"\\]')
  expect_match(j, '"friendshore":null')
})

test_that("investment flows keep the headline shock and increment, ranked hosts, no domestic rows", {
  f <- aib_dash_investment_flows(aib_net_res())
  expect_length(f$pairs, 2)
  expect_equal(vapply(f$pairs, `[[`, "", "host"), c("DEU", "KOR"))
  expect_equal(vapply(f$pairs, `[[`, 1L, "rank"), c(1L, 2L))
  expect_length(f$hosts, 2)
  expect_equal(f$variant$increment, 0.1)
  expect_equal(f$variant$maxRank, 5L)
})

test_that("investment flows keep only nodes of the strategic scope", {
  res <- aib_net_res()
  res$classification <- tibble::tibble(node_id = "solar-cells", in_strategic_scope = TRUE)
  expect_length(aib_dash_investment_flows(res)$pairs, 2)
  res$classification$in_strategic_scope <- FALSE
  f <- aib_dash_investment_flows(res)
  expect_length(f$pairs, 0)
  expect_length(f$hosts, 0)
})

test_that("stages are exported in chain order, as arrays", {
  s <- aib_dash_stage_order(c("Recovery", "Assembly", "Refining", "Extraction"))
  expect_equal(as.character(s), c("Extraction", "Refining", "Assembly", "Recovery"))
  expect_s3_class(aib_dash_stage_order("Assembly"), "AsIs")
  expect_equal(aib_dash_stage_keys(), c("extraction", "refining", "components", "assembly", "deployment", "recovery"))
})

test_that("the bundle version is 3, the version the dashboard's contract reads (an app reading 2 asks for a reload)", {
  expect_identical(AIB_DASH_VERSION, 3L)
  ts <- readLines(file.path(repo_root, "dashboard", "src", "aib", "contract.ts"), warn = FALSE)
  v <- sub("^export const AIB_VERSION = ([0-9]+);.*$", "\\1", grep("^export const AIB_VERSION = [0-9]+;", ts, value = TRUE))
  expect_equal(as.integer(v), AIB_DASH_VERSION)
})

test_that("the R mirror checks the six stages as contract.ts does: node.stage, compact and edge stage lists, the three-stage cells", {
  six <- paste(SUPPLY_CHAIN_STAGES, collapse = ", ")
  doc <- list(
    nodes = list(list(id = "a", stage = "Refining", yfCellsLegacy = aib_dash_arr("batteries-midstream"), yfSubcomponentsLegacy = aib_dash_arr("batteries-midstream::Anode Materials")),
                 list(id = "b", stage = "Deployment", yfCellsLegacy = aib_dash_arr(character()), yfSubcomponentsLegacy = aib_dash_arr(character())),
                 list(id = "c")),
    compacts = list(list(id = "AIB-C01", stages = aib_dash_stage_order(c("Refining", "Extraction"))), list(id = "AIB-C02", stages = aib_dash_arr(character())))
  )
  expect_equal(aib_check_stages(doc), character())
  expect_equal(aib_check_bundle_additions(doc), character())
  bad <- doc
  bad$nodes[[1]]$stage <- "Upstream"
  bad$nodes[[2]]$stage <- NA_character_
  bad$nodes[[1]]$yfCellsLegacy <- aib_dash_arr(c("batteries-midstream", "batteries-refining"))
  bad$nodes[[1]]$yfSubcomponentsLegacy <- aib_dash_arr(c("batteries-midstream", "batteries-midstream::"))
  bad$nodes[[2]]$yfCellsLegacy <- "batteries-midstream"                         # not I(): collapses to a string
  bad$compacts[[1]]$stages <- aib_dash_arr(c("Components", "Refining", "Refining"))
  bad$compacts[[2]]$stages <- aib_dash_arr("Midstream")
  p <- aib_check_stages(bad)
  expect_equal(p, c(
    sprintf("nodes[0].stage: 'Upstream' is not one of %s", six),
    "nodes[0].yfCellsLegacy[1]: 'batteries-refining' is not a three-stage cell (<tech>-<upstream|midstream|downstream>)",
    "nodes[0].yfSubcomponentsLegacy[0]: 'batteries-midstream' is not a three-stage sub-component (<tech>-<upstream|midstream|downstream>::<name>)",
    "nodes[0].yfSubcomponentsLegacy[1]: 'batteries-midstream::' is not a three-stage sub-component (<tech>-<upstream|midstream|downstream>::<name>)",
    "nodes[1].stage: expected a string",
    "nodes[1].yfCellsLegacy: expected an array of strings",
    "compacts[0].stages: duplicate 'Refining'",
    "compacts[0].stages: not in chain order (Components, Refining, Refining)",
    sprintf("compacts[1].stages: 'Midstream' is not one of %s", six)
  ))
  expect_true(all(p %in% aib_check_bundle_additions(bad)))
  # the network edge stages through the same list check
  res <- aib_net_res()
  net <- list(nodes = list(list(id = "solar-cells")), network = aib_dash_network(res))
  net$network$edges[[1]]$stages <- aib_dash_arr(c("Assembly", "Components"))
  expect_equal(aib_check_network(net), "network.edges[0].stages: not in chain order (Assembly, Components)")
  net$network$edges[[1]]$stages <- aib_dash_arr(c("Components", "Components"))
  expect_equal(aib_check_network(net), "network.edges[0].stages: duplicate 'Components'")
})

test_that("the nodes' three-stage cells: the legacy crosswalk with the dictionary, each node's own cells with the master", {
  osi <- tibble::tribble(
    ~tech,       ~supply_chain, ~stage,       ~sub_sector,          ~hs6,     ~excluded,
    "Batteries", "Midstream",   "Refining",   "Anode Materials",    "380190", FALSE,
    "Batteries", "Upstream",    "Extraction", "Natural Graphite",   "250410", FALSE,
    "Solar",     "Midstream",   "Assembly",   "Solar Modules",      "854143", FALSE,
    "Wind",      "Midstream",   "Components", "Generator Parts",    "850300", TRUE
  )
  codes <- tibble::tibble(node_id = c("anode", "graphite", "graphite", "modules", "gen", "h2"), hs6 = c("380190", "250410", "380190", "854143", "850300", "842839"))
  lc <- aib_dash_legacy_cells(codes, osi)
  expect_setequal(lc$node_id, c("anode", "graphite", "modules"))        # no basket code (h2) or an excluded one (gen): absent
  expect_equal(lc$yf_cells[lc$node_id == "anode"], "batteries-midstream")   # not batteries-upstream, the roll-up of Refining
  expect_equal(lc$osi_subcomponents[lc$node_id == "anode"], "batteries-midstream::Anode Materials")
  expect_setequal(strsplit(lc$yf_cells[lc$node_id == "graphite"], "; ")[[1]], c("batteries-upstream", "batteries-midstream"))
  # weighed by trade, the larger code's cell comes first; counted, the tie goes to the earlier stage
  lt <- aib_dash_legacy_cells(codes, osi, tibble::tibble(hs6 = c("250410", "380190"), value = c(10, 90)))
  expect_equal(lt$yf_cells[lt$node_id == "graphite"], "batteries-midstream; batteries-upstream")
  expect_equal(lc$yf_cells[lc$node_id == "graphite"], "batteries-upstream; batteries-midstream")
  # the crosswalk asserts the stage column it reads
  bad <- osi
  bad$supply_chain[1] <- "Refining"
  expect_error(aib_dash_legacy_cells(codes, bad), "not Upstream / Midstream / Downstream")
  expect_error(aib_osi_crosswalk(codes, dplyr::select(osi, -supply_chain), stage_col = "supply_chain"), "supply_chain")
  # the master taxonomy: the node's own technologies at its source stage
  cells <- tibble::tibble(node_id = c("magnets", "magnets"), tech = c("Electric Vehicles", "Wind"), source_stage = "Midstream")
  detail <- dplyr::mutate(cells, sub_sector = "Permanent Magnets & Magnet Parts")
  lm <- aib_dash_legacy_cells(NULL, NULL, taxonomy = "master", node_cells = cells, node_cells_detail = detail)
  expect_equal(lm$yf_cells, "electric-vehicles-midstream; wind-midstream")
  expect_equal(lm$osi_subcomponents, "electric-vehicles-midstream::Permanent Magnets & Magnet Parts; wind-midstream::Permanent Magnets & Magnet Parts")
  # in JSON a one-item list stays an array (the exporter splits with aib_dash_split(), I())
  expect_match(as.character(country_json(list(yfCellsLegacy = aib_dash_split(lc$yf_cells[lc$node_id == "anode"], "; ")))), '"yfCellsLegacy":["batteries-midstream"]', fixed = TRUE)
  # every node the inputs hold is known, with or without a basket code: the exporter writes [] for h2 and gen,
  # and nothing for a node of another run (the dashboard then uses the roll-up)
  expect_setequal(attr(lc, "known_nodes"), c("anode", "graphite", "modules", "gen", "h2"))
  expect_equal(attr(lm, "known_nodes"), "magnets")
  a <- aib_dash_node_legacy(lc, "anode")
  expect_equal(as.character(a$cells), "batteries-midstream")
  expect_s3_class(a$cells, "AsIs")
  expect_equal(as.character(a$subcomponents), "batteries-midstream::Anode Materials")
  h2 <- aib_dash_node_legacy(lc, "h2")                                    # known, no basket code: []
  expect_length(h2$cells, 0)
  expect_s3_class(h2$cells, "AsIs")
  expect_match(as.character(country_json(list(yfCellsLegacy = h2$cells))), '"yfCellsLegacy":[]', fixed = TRUE)
  expect_null(aib_dash_node_legacy(lc, "lithium-chemicals"))             # not a node of these inputs: left out
  expect_null(aib_dash_node_legacy(NULL, "anode"))                       # no legacy cells at all: left out
  unattributed <- lc
  attr(unattributed, "known_nodes") <- NULL                              # a table without the attribute: every node known
  expect_length(aib_dash_node_legacy(unattributed, "lithium-chemicals")$cells, 0)
})

test_that("an aggregated node's label names its six-stage stage; its id, other labels and unknown stages are kept", {
  old <- "Uranium/fuel Upstream: aggregated low-coverage sub-sectors (Spent fuel & nuclear scrap, Uranium ores)"
  expect_equal(aib_dash_node_label(old, "Extraction"), "Uranium/fuel Extraction: aggregated low-coverage sub-sectors (Spent fuel & nuclear scrap, Uranium ores)")
  expect_equal(aib_dash_node_label("Wind Midstream: aggregated low-coverage sub-sectors (A, B)", "Components"), "Wind Components: aggregated low-coverage sub-sectors (A, B)")
  # a label built with the six-stage stage already, an ordinary label with a stage word, an unknown stage, NA
  expect_equal(aib_dash_node_label("Wind Recovery: aggregated low-coverage sub-sectors (A, B)", "Recovery"), "Wind Recovery: aggregated low-coverage sub-sectors (A, B)")
  expect_equal(aib_dash_node_label("Upstream oil & gas equipment", "Assembly"), "Upstream oil & gas equipment")
  expect_equal(aib_dash_node_label(old, "Upstream"), old)
  expect_true(is.na(aib_dash_node_label(NA_character_, "Extraction")))
})

test_that("the R mirror accepts the exported sections and names each broken field", {
  res <- aib_net_res()
  doc <- list(nodes = list(list(id = "solar-cells")), network = aib_dash_network(res), investmentFlows = aib_dash_investment_flows(res))
  expect_equal(aib_check_bundle_additions(doc), character())
  expect_equal(aib_check_bundle_additions(list(nodes = list())), character())
  bad <- doc
  bad$network$edges[[1]]$nodes <- "solar-cells"
  bad$network$countries[[2]]$iso3 <- "DEU"
  bad$investmentFlows$pairs[[1]]$host <- "USA"
  bad$investmentFlows$pairs[[2]]$rank <- 7L
  bad$investmentFlows$hosts[[1]]$node <- "wafers"
  bad$network$edges[[1]]$stages <- I("Midstream")                            # a three-stage label
  bad$network$countries[[1]]$supplier$refining <- NULL
  p <- aib_check_bundle_additions(bad)
  expect_true(any(startsWith(p, "network.edges[0].stages: 'Midstream' is not one of Extraction, Refining, Components, Assembly, Deployment, Recovery")))
  expect_true(any(grepl("^network.countries\\[0\\].supplier.refining: expected a number", p)))
  expect_true(any(grepl("^network.edges\\[0\\].nodes: expected an array", p)))
  expect_true(any(grepl("^network.countries\\[1\\].iso3: duplicate 'DEU'", p)))
  expect_true(any(grepl("^investmentFlows.pairs\\[0\\]: investor and host are the same economy", p)))
  expect_true(any(grepl("^investmentFlows.pairs\\[1\\].rank: 7 above 5", p)))
  expect_true(any(grepl("^investmentFlows.hosts\\[0\\].node: 'wafers' is not a node", p)))
})

# The taxonomy fields as aib_dash_bundle() writes them (meta.indices from res$taxonomy, node.indexCode,
# compact.indices); an older bundle without them (OSI technology names) passes too.
aib_tax_doc <- function() {
  tx <- tibble::tibble(tech = c("Rare earths", "Magnets", "Heat pumps/cooling"), index_code = c("A", "B", "E"),
                       index_name = c("Materials & Inputs", "Electrotechnologies", "Electrified End Uses"))
  idx <- aib_dash_index_of(list(taxonomy = tx))
  list(
    meta = list(taxonomy = "dictionary", dictionaryScope = "core",
                indices = unname(lapply(split(idx, idx$index_code), function(t) list(code = t$index_code[1], name = t$index_name[1], sectors = aib_dash_arr(sort(unique(t$tech))))))),
    nodes = list(list(id = "rare-earth-metals", indexCode = "A"), list(id = "heat-pumps", indexCode = "E"), list(id = "old", indexCode = NA_character_)),
    compacts = list(list(id = "AIB-C01", indices = aib_dash_arr(c("A", "B"))), list(id = "AIB-C02", indices = aib_dash_arr("E")))
  )
}

test_that("the R mirror accepts the taxonomy fields, an older bundle without them, and names each broken one", {
  expect_null(aib_dash_index_of(list(taxonomy = NULL)))
  expect_null(aib_dash_index_of(list(taxonomy = tibble::tibble(tech = "Solar", index_code = NA_character_, index_name = NA_character_))))
  doc <- aib_tax_doc()
  expect_length(doc$meta$indices, 3)
  expect_equal(doc$meta$indices[[1]]$code, "A")
  expect_equal(aib_check_taxonomy(doc), character())
  expect_equal(aib_check_bundle_additions(doc), character())
  # a one-item array stays an array in JSON
  j <- as.character(country_json(doc$compacts[[2]]))
  expect_match(j, '"indices":["E"]', fixed = TRUE)
  # master: indices [], no index on nodes or compacts; an older bundle: no field at all
  master <- list(meta = list(taxonomy = "master", dictionaryScope = NA_character_, indices = list()),
                 nodes = list(list(id = "battery-cells", indexCode = NA_character_)), compacts = list(list(id = "AIB-C01", indices = aib_dash_arr(character()))))
  expect_equal(aib_check_taxonomy(master), character())
  expect_equal(aib_check_taxonomy(list(meta = list(universe = "u"), nodes = list(list(id = "x")), compacts = list(list(id = "AIB-C01")))), character())
  bad <- doc
  bad$meta$taxonomy <- 3
  bad$meta$indices[[2]]$code <- "A"
  bad$meta$indices[[3]]$code <- "e"
  bad$meta$indices[[1]]$sectors <- "Rare earths"
  bad$nodes[[2]]$indexCode <- "Z"
  bad$compacts[[1]]$indices <- "A"
  bad$compacts[[2]]$indices <- aib_dash_arr(c("A", "Q"))
  p <- aib_check_taxonomy(bad)
  has <- function(msg) any(startsWith(p, msg))
  expect_true(has("meta.taxonomy: expected a string or null"))
  expect_true(has("meta.indices[1].code: duplicate 'A'"))
  expect_true(has("meta.indices[2].code: 'e' is not one capital letter"))
  expect_true(has("meta.indices[0].sectors: expected an array"))
  expect_true(has("nodes[1].indexCode: 'Z' is not an index of meta.indices"))
  expect_true(has("compacts[0].indices: expected an array"))
  expect_true(has("compacts[1].indices: 'Q' is not an index of meta.indices"))
})

# The diversification scenario as aib_dash_bundle() writes it (NA where JSON has null; capped and hosts
# through aib_dash_arr / aib_dash_split): one node per status, and a node the scenario did not assess.
aib_div_node <- function(status, cap = NA_real_, target_iso = NA_character_, ran = FALSE, hosts = NA_character_, partners = NA_character_, cls = NA_character_) {
  num <- function(x) if (ran) aib_dash_round(x) else NA_real_
  list(id = gsub(" ", "-", status), diversify = list(
    status = aib_dash_chr(status), currentHhi = aib_dash_round(0.3839), benchmarkHhi = if (status == "no benchmark") NA_real_ else aib_dash_round(0.0858),
    basis = if (status == "no benchmark") NA_character_ else aib_dash_chr("other HS6 lines of heading 8507"), cap = aib_dash_round(cap),
    capped = if (is.na(target_iso)) aib_dash_arr(character()) else aib_dash_split(target_iso, "+"),
    excess = if (is.na(cap)) NA_real_ else aib_dash_round(if (ran) 0.4887 else 0), static = num(0.5113), existing = num(0.5189), optimized = num(0.5189),
    residual = num(0.4811), residualUsd = if (ran) aib_dash_round(51677505816, 0) else NA_real_,
    hosts = aib_dash_split(hosts, "; "), partners = aib_dash_chr(partners), cls = aib_dash_chr(cls)
  ))
}

aib_div_doc <- function() {
  list(
    meta = list(classes = aib_dash_arr(c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA")),
                diversify = list(shockId = "diversify_benchmark", label = "Diversified to its sector's concentration", benchmark = "hs4")),
    nodes = list(
      aib_div_node("diversified to the benchmark", cap = 0.1222, target_iso = "CHN", ran = TRUE, hosts = "FIN; NLD; CAN", partners = "TWN->SAU", cls = "BUILD"),
      aib_div_node("within the cap in the allied market", cap = 0.3582),
      aib_div_node("already as diversified as its benchmark"),
      aib_div_node("no benchmark"),
      list(id = "not-assessed")
    )
  )
}

test_that("the R mirror accepts the diversification fields, an older bundle without them, and names each broken one", {
  doc <- aib_div_doc()
  expect_equal(aib_check_diversify(doc), character())
  expect_equal(aib_check_bundle_additions(doc), character())
  # one capped supplier and one host stay arrays in JSON; NA is written as null
  j <- as.character(country_json(doc$nodes[[1]]$diversify))
  expect_match(j, '"capped":["CHN"]', fixed = TRUE)
  expect_match(j, '"hosts":["FIN","NLD","CAN"]', fixed = TRUE)
  one <- aib_div_node("diversified to the benchmark", cap = 0.2, target_iso = "DEU", ran = TRUE, hosts = "FIN", cls = "LOW INTERVENTION")
  expect_equal(aib_check_diversify(list(meta = doc$meta, nodes = list(one))), character())
  j1 <- as.character(country_json(one$diversify))
  expect_match(j1, '"hosts":["FIN"]', fixed = TRUE)
  expect_match(j1, '"capped":["DEU"]', fixed = TRUE)
  j2 <- as.character(country_json(doc$nodes[[3]]$diversify))
  expect_match(j2, '"cap":null', fixed = TRUE)
  expect_match(j2, '"capped":[]', fixed = TRUE)
  expect_match(j2, '"cls":null', fixed = TRUE)
  # an older bundle: no meta.diversify, no node.diversify
  expect_equal(aib_check_diversify(list(meta = list(universe = "u"), nodes = list(list(id = "x")))), character())
  bad <- doc
  bad$meta$diversify$shockId <- NA_character_
  bad$nodes[[1]]$diversify$status <- "diversified"
  bad$nodes[[1]]$diversify$cap <- 1.2
  bad$nodes[[1]]$diversify$capped <- "CHN"
  bad$nodes[[1]]$diversify$cls <- "REBUILD"
  bad$nodes[[2]]$diversify$residualUsd <- -1
  bad$nodes[[2]]$diversify$excess <- "none"
  bad$nodes[[3]]$diversify$hosts <- NULL
  bad$nodes[[4]] <- list(id = "x", diversify = "no benchmark")
  p <- aib_check_diversify(bad)
  has <- function(msg) any(startsWith(p, msg))
  expect_true(has("meta.diversify.shockId: expected a string"))
  expect_true(has("nodes[0].diversify.status: 'diversified' is not one of"))
  expect_true(has("nodes[0].diversify.cap: 1.2 above 1"))
  expect_true(has("nodes[0].diversify.capped: expected an array"))
  expect_true(has("nodes[0].diversify.cls: 'REBUILD' is not one of meta.classes"))
  expect_true(has("nodes[1].diversify.residualUsd: -1 below 0"))
  expect_true(has("nodes[1].diversify.excess: expected a number or null"))
  expect_true(has("nodes[2].diversify.hosts: expected an array"))
  expect_true(has("nodes[3].diversify: not an object"))
  expect_length(p, 9)
  # scripts/84 runs the check through aib_check_bundle_additions(): the same problems, and the taxonomy ones
  pa <- aib_check_bundle_additions(bad)
  expect_true(all(p %in% pa))
  expect_true(any(startsWith(pa, "nodes[0].diversify.status")))
  tax <- doc
  tax$nodes[[1]]$indexCode <- 3
  expect_true(any(startsWith(aib_check_bundle_additions(tax), "nodes[0].indexCode: expected a string or null")))
})

test_that("the R mirror rejects what TypeScript rejects: a missing diversify key, a NULL object, a class outside the five", {
  doc <- aib_div_doc()
  # a missing key is undefined in TypeScript, never null
  miss <- doc
  for (k in c("cap", "cls", "basis", "residualUsd", "status")) miss$nodes[[1]]$diversify[[k]] <- NULL
  p <- aib_check_diversify(miss)
  expect_true("nodes[0].diversify.cap: expected a number or null" %in% p)
  expect_true("nodes[0].diversify.residualUsd: expected a number or null" %in% p)
  expect_true("nodes[0].diversify.basis: expected a string or null" %in% p)
  expect_true("nodes[0].diversify.cls: expected a string or null" %in% p)
  expect_true(any(startsWith(p, "nodes[0].diversify.status: 'undefined' is not one of")))
  expect_length(p, 5)
  expect_equal(p, aib_check_bundle_additions(miss))
  # a present NULL is JSON null: not an object, for the node and for meta
  nul <- doc
  nul$nodes[2] <- list(list(id = "y", diversify = NULL))
  nul$meta["diversify"] <- list(NULL)
  expect_true("diversify" %in% names(nul$nodes[[2]]))
  p2 <- aib_check_diversify(nul)
  expect_true("nodes[1].diversify: not an object" %in% p2)
  expect_true("meta.diversify: not an object" %in% p2)
  expect_length(p2, 2)
  # without meta.classes the five classes apply, as AIB_CLASSES does in TypeScript
  nocls <- doc
  nocls$meta$classes <- NULL
  expect_equal(aib_check_diversify(nocls), character())
  nocls$nodes[[1]]$diversify$cls <- "REBUILD"
  expect_equal(aib_check_diversify(nocls), "nodes[0].diversify.cls: 'REBUILD' is not one of meta.classes")
})

# ---- The scenario toggle (2026-10-19) ------------------------------------------------------------------
# The dashboard's synthetic fixture (dashboard/src/aib/__fixtures__/synthetic/data/aib: the main document
# with meta.scenarios, node.baseline, the node.diversify run fields, compact.managed / baseline, and the two
# scenario files), read back as the lists aib_dash_bundle() builds (string arrays as I() vectors), so the
# R mirror and contract.ts check the same files.
aib_fixture_dir <- file.path(repo_root, "dashboard", "src", "aib", "__fixtures__", "synthetic", "data", "aib")

aib_as_r <- function(x) {
  if (!is.list(x)) return(x)
  if (!length(x) && is.null(names(x))) return(I(character()))
  if (is.null(names(x)) && all(vapply(x, function(v) is.character(v) && length(v) == 1, logical(1)))) return(I(unlist(x)))
  nm <- names(x)
  out <- lapply(x, aib_as_r)
  names(out) <- nm
  out
}

# The fixture is six-stage since 2026-10-28 (version 3, as scripts/84 writes a bundle): node and compact
# stages, network stages and per-side counts, and the nodes' three-stage cells (yfCellsLegacy).
aib_fixture <- function() {
  read <- function(f) aib_as_r(jsonlite::fromJSON(file.path(aib_fixture_dir, f), simplifyVector = FALSE))
  manifest <- jsonlite::fromJSON(file.path(aib_fixture_dir, "manifest.json"), simplifyVector = FALSE)
  list(manifest = manifest, main = read(manifest$files$compacts), managed = read(manifest$files$scenarios$managed),
       baseline = read(manifest$files$scenarios$baseline))
}

test_that("the R mirror accepts the dashboard's synthetic fixture: the scenario fields and both scenario files", {
  skip_if_not(file.exists(file.path(aib_fixture_dir, "manifest.json")))
  fx <- aib_fixture()
  expect_equal(aib_check_scenario_files(fx$manifest$files$scenarios), character())
  expect_equal(aib_check_scenarios(fx$main), character())
  expect_equal(aib_check_bundle_additions(fx$main), character())
  expect_equal(aib_check_scenario_doc(fx$managed, fx$main, expect = "managed"), character())
  expect_equal(aib_check_scenario_doc(fx$baseline, fx$main, expect = "baseline"), character())
  # without the main document: no cross-checks, the same verdict
  expect_equal(aib_check_scenario_doc(fx$managed), character())
  expect_equal(aib_check_scenario_doc(fx$baseline), character())
  # the scenario list, then the supply scenarios beside it
  expect_equal(vapply(fx$main$meta$scenarios, `[[`, "", "id"), c("baseline", "managed", "decoupling"))
  expect_equal(as.character(fx$main$meta$supplyScenarios), c("conservative", "reallocation"))
  # six-stage throughout (version 3): the stages, the six counts per side, the nodes' three-stage cells
  expect_equal(fx$main$version, AIB_DASH_VERSION)
  expect_equal(fx$manifest$version, AIB_DASH_VERSION)
  expect_equal(aib_check_stages(fx$main), character())
  expect_true(all(vapply(fx$main$nodes, `[[`, "", "stage") %in% SUPPLY_CHAIN_STAGES))
  anode <- fx$main$nodes[[which(vapply(fx$main$nodes, `[[`, "", "id") == "bat-mid-anode")]]
  expect_equal(as.character(anode$yfCells), "batteries-refining")
  expect_equal(as.character(anode$yfCellsLegacy), "batteries-midstream")
  side <- fx$main$network$countries[[1]]$supplier
  expect_true(all(aib_dash_stage_keys() %in% names(side)))
  expect_false(any(c("upstream", "midstream", "downstream") %in% names(side)))
})

test_that("the scenario fields are optional: an older bundle passes, and meta.scenarios may list the supply scenarios", {
  older <- list(meta = list(scenarios = aib_dash_arr(c("conservative", "reallocation"))), nodes = list(list(id = "x", diversify = list(status = "no benchmark"))),
                compacts = list(list(id = "AIB-C01", nodes = aib_dash_arr("x"))))
  expect_equal(aib_check_scenarios(older), character())
  expect_equal(aib_check_scenarios(list(meta = list(scenarios = list("conservative", "reallocation")))), character())
  expect_equal(aib_check_scenario_files(NULL), character())
  expect_equal(aib_check_scenario_files(list(managed = NULL, other = "x.json")), character())
})

test_that("the R mirror names each broken scenario field of the main document", {
  skip_if_not(file.exists(file.path(aib_fixture_dir, "manifest.json")))
  bad <- aib_fixture()$main
  bad$meta$scenarios[[2]]$id <- "diversify"
  bad$meta$scenarios[[3]]$id <- "baseline"
  bad$meta$scenarios[[1]]$words <- NULL
  bad$meta$supplyScenarios <- "reallocation"
  i <- which(vapply(bad$nodes, `[[`, "", "id") == "ev-wind-mid-magnets")
  bad$nodes[[i]]$baseline$suppliers[[1]] <- list("ZZZ")
  bad$nodes[[i]]$baseline$largestShare <- 1.4
  bad$nodes[[i]]$diversify$lostUsd <- -1
  bad$nodes[[i]]$diversify$newLinks <- 2.5
  bad$compacts[[1]]$managed$nodesRun <- 3L
  bad$compacts[[1]]$managed$optimized <- NULL
  bad$compacts[[2]]$baseline <- "none"
  bad$meta$scenarios <- c(bad$meta$scenarios, list("conservative"))
  p <- aib_check_scenarios(bad)
  has <- function(msg) any(startsWith(p, msg))
  at <- sprintf("nodes[%d]", i - 1)
  expect_true(has("meta.scenarios[1].id: 'diversify' is not one of baseline, managed, decoupling"))
  expect_true(has("meta.scenarios[2].id: duplicate 'baseline'"))
  expect_true(has("meta.scenarios[0].words: expected a string"))
  expect_true(has("meta.scenarios[3]: expected a scenario object"))
  expect_true(has("meta.supplyScenarios: expected an array"))
  expect_true(has(paste0(at, ".baseline.suppliers[0]: expected [ISO3, share]")))
  expect_true(has(paste0(at, ".baseline.largestShare: 1.4 above 1")))
  expect_true(has(paste0(at, ".diversify.lostUsd: -1 below 0")))
  expect_true(has(paste0(at, ".diversify.newLinks: 2.5 is not an integer")))
  expect_true(has("compacts[0].managed.nodesRun: 3 above 2"))
  expect_true(has("compacts[0].managed.optimized: expected a number or null"))
  expect_true(has("compacts[1].baseline: not an object"))
  expect_length(p, 12)
  # scripts/84 runs them through aib_check_bundle_additions()
  expect_true(all(p %in% aib_check_bundle_additions(bad)))
})

test_that("the R mirror names each broken field of a scenario file, and the manifest's scenario names", {
  skip_if_not(file.exists(file.path(aib_fixture_dir, "manifest.json")))
  fx <- aib_fixture()
  m <- fx$managed
  # a compact none of whose nodes the scenario ran on: coalition null with the reason (JSON null)
  m$compacts[[3]]["coalition"] <- list(NULL)
  m$compacts[[3]]$coalitionReason <- "no node of this compact is more concentrated than its benchmark"
  m$compacts[[3]]$nodesRun <- 0L
  expect_equal(aib_check_scenario_doc(m, fx$main, expect = "managed"), character())
  expect_match(as.character(country_json(m$compacts[[3]])), '"coalition":null', fixed = TRUE)
  m$compacts[[3]]$coalitionReason <- NULL
  m$compacts[[2]]$id <- "AIB-C09"
  m$compacts[[1]]$coalition$reached99 <- "yes"
  m$compacts[[1]]$trade[[1]]$relationship <- "domestic"
  m$network$edges[[1]]$nodes <- aib_dash_arr("wafers")
  m$investmentFlows$pairs[[1]]$rank <- 9L
  m$shockId <- NA_character_
  p <- aib_check_scenario_doc(m, fx$main, expect = "baseline")
  has <- function(msg) any(startsWith(p, msg))
  expect_true(has("document.scenario: 'managed' where the manifest names a baseline file"))
  expect_true(has("document.shockId: expected a string"))
  expect_true(has("compacts[2].coalitionReason: expected a string when coalition is null"))
  expect_true(has("compacts[1].id: 'AIB-C09' is not a compact of the main document"))
  expect_true(has("compacts[0].coalition.reached99: expected true or false"))
  expect_true(has("compacts[0].trade[0].relationship: 'domestic' is not one of new, expanded"))
  expect_true(has("network.edges[0].nodes: 'wafers' is not a node of the document"))
  expect_true(has("investmentFlows.pairs[0].rank: 9 above 5"))
  expect_length(p, 8)
  b <- fx$baseline
  b$nodes[[1]]$suppliers[[2]] <- list("AAA", -5, 0.2)
  b$nodes[[2]]$id <- b$nodes[[1]]$id
  b$compacts[[1]]$id <- "AIB-C77"
  b$schema <- "osi-aib"
  pb <- aib_check_scenario_doc(b, fx$main)
  hb <- function(msg) any(startsWith(pb, msg))
  expect_true(hb("document.schema: expected 'osi-aib-scenario'"))
  expect_true(hb("nodes[0].suppliers[1][1]: -5 below 0"))
  expect_true(hb(sprintf("nodes[1].id: duplicate '%s'", b$nodes[[1]]$id)))
  expect_true(hb("compacts[0].id: 'AIB-C77' is not a compact of the main document"))
  expect_equal(aib_check_scenario_doc("x"), "document: not an object")
  expect_equal(aib_check_scenario_files(list(managed = "scenario-baseline.0123abcd.json", baseline = "scenario-baseline.json")),
               c("manifest.files.scenarios.managed: not a scenario-managed.<8 hex>.json file name",
                 "manifest.files.scenarios.baseline: not a scenario-baseline.<8 hex>.json file name"))
  # tuples stay arrays in JSON: [ISO3, share] and [ISO3, USD, share]
  j <- as.character(country_json(list(suppliers = list(list("CHN", 0.4)), flows = list(list("CHN", 1e9, NA_real_)))))
  expect_match(j, '"suppliers":[["CHN",0.4]]', fixed = TRUE)
  expect_match(j, '"flows":[["CHN",1000000000,null]]', fixed = TRUE)
})

# ---- The strategic layer (2026-10-20) ---------------------------------------------------------------------
# The `strategic` section (aib_dash_strategic() of scripts/33's layer; aib_check_strategic()), built from the
# synthetic run of helper-aib-strategic.R.
source(file.path(repo_root, "R", "aib", "aib_compacts.R"))
source(file.path(repo_root, "R", "aib", "aib_strategic.R"))

strat_dash_doc <- function() {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_layer.yml"))
  cfg$capital$cmfdi_sectors <- list(Battery = "SC-B", EV = "SC-B")
  cap <- strat_fx_capital()
  st <- aib_strategic_layer(strat_fx_inputs(), strat_fx_res(), strat_fx_compacts_yml(), strat_fx_node_functions(), cfg,
                            capital = list(dip = cap$dip, cmfdi = cap$cmfdi), conduits = cap$conduits, iso_of = cap$iso_of)
  st$meta <- list(default_scenario = "managed")
  list(nodes = lapply(strat_fx_inputs()$nodes$node_id, function(id) list(id = id)),
       compacts = lapply(sprintf("AIB-C%02d", 1:5), function(id) list(id = id)),
       strategic = aib_dash_strategic(st, aib_strategic_gate_labels()))
}

test_that("the strategic section is optional, persistent across scenarios, and accepted by the R mirror before and after JSON", {
  expect_equal(aib_check_strategic(list(nodes = list(), compacts = list())), character())
  doc <- strat_dash_doc()
  s <- doc$strategic
  expect_equal(aib_check_strategic(doc), character())
  expect_equal(aib_check_bundle_additions(doc), character())
  rt <- aib_as_r(jsonlite::fromJSON(as.character(country_json(doc)), simplifyVector = FALSE))
  expect_equal(aib_check_strategic(rt), character())
  # SC-C (no node in the run) is left out; the others have every scenario, the same spine in each
  expect_equal(vapply(s$compacts, `[[`, "", "id"), c("SC-A", "SC-B"))
  expect_equal(vapply(s$scenarios, `[[`, "", "id"), c("baseline", "managed", "partial", "decoupling"))
  a <- s$compacts[[1]]
  expect_equal(names(a$scenarios), c("baseline", "managed", "partial", "decoupling"))
  spine <- unlist(lapply(a$steps, function(x) as.character(x$nodes)))
  for (sid in names(a$scenarios)) expect_equal(vapply(a$scenarios[[sid]]$nodes, `[[`, "", 1), spine)
  expect_equal(spine, c("ore", "oxide", "magnet", "motor"))
  expect_equal(as.character(a$workstreams), c("AIB-C01", "AIB-C02", "AIB-C03"))
  expect_equal(as.character(s$compacts[[2]]$alsoTouching), "AIB-C02")
  # no coalitions under Baseline and the partial disruption: null partners; arrays where there are coalitions
  expect_null(a$scenarios$baseline$criticalPartners)
  expect_equal(as.character(a$scenarios$decoupling$criticalPartners), "JPN")
  j <- as.character(country_json(a$scenarios$baseline))
  expect_match(j, '"criticalPartners":null', fixed = TRUE)
  expect_match(as.character(country_json(a$scenarios$decoupling)), '"policyMix":["COORDINATE","MANAGE SPECIALIZATION","BUILD"]', fixed = TRUE)
  # one-item arrays stay arrays; tuples stay tuples
  expect_match(as.character(country_json(s$compacts[[2]])), '"alsoTouching":["AIB-C02"]', fixed = TRUE)
  usa <- s$countries[[which(vapply(s$countries, `[[`, "", "iso3") == "USA")]]
  expect_match(as.character(country_json(usa)), '"primary":["DMA"]', fixed = TRUE)
  expect_equal(usa$roles[[1]][[1]], "DMA")
  expect_length(usa$roles, 7)
  expect_length(s$countries[[which(vapply(s$countries, `[[`, "", "iso3") == "CHN")]]$roles, 3)
  ws <- s$workstreams[[which(vapply(s$workstreams, `[[`, "", "id") == "AIB-C02")]]
  expect_equal(ws$basis, "override")
  expect_equal(ws$split[[1]], list("SC-B", 0.57))
  # 2026-10-21 review: the spine tuple counts the hosts tied with rank 1, NOT TESTED has a count, a mission
  # says whether it is a targeted action, and the allied-only slots and the stress note are written
  d <- a$scenarios$decoupling
  expect_true(all(vapply(d$nodes, length, 1L) == 5L))
  expect_equal(d$nodes[[1]][[4]], "AUS")
  expect_equal(d$nodes[[1]][[5]], 1L)
  expect_equal(a$scenarios$managed$counts$notTested, 0L)
  expect_true(all(vapply(d$missions, function(m) is.logical(m$targeted), logical(1))))
  for (k in c("alliedOutsideCore", "nonAlliedReliedOn", "buildHostsNonAllied", "buildHostTies", "stressNote")) expect_true(k %in% names(d), info = k)
  expect_equal(as.character(d$buildHosts), "AUS")
})

test_that("the R mirror names each broken strategic field", {
  doc <- strat_dash_doc()
  s <- doc$strategic
  s$defaultScenario <- "stress"
  s$compacts[[1]]$steps[[2]]$order <- 1L
  s$compacts[[2]]$steps[[1]]$nodes <- aib_dash_arr(c("cellmat", "zinc"))
  s$compacts[[1]]$scenarios$partial <- NULL
  s$compacts[[1]]$scenarios$decoupling$counts$build <- 9L
  s$compacts[[1]]$scenarios$decoupling$criticalPartners <- NULL
  s$compacts[[1]]$division[[1]][[6]] <- "x"
  s$workstreams[[1]]$compact <- "SC-Z"
  i_usa <- which(vapply(s$countries, `[[`, "", "iso3") == "USA")
  s$countries[[i_usa]]$primary <- aib_dash_arr(c("DMA", "CIA", "CBH", "SMI"))
  i_chn <- which(vapply(s$countries, `[[`, "", "iso3") == "CHN")
  s$countries[[i_chn]]$secondary <- aib_dash_arr("DMA")
  s$compactRoles[[1]]$leadNode <- "cell"
  s$theses[[1]][[4]] <- aib_dash_arr("friendship")
  s$framing <- 3
  s$compacts[[1]]$scenarios$managed$nodes[[1]][[5]] <- 0L
  doc$strategic <- s
  p <- aib_check_strategic(doc)
  has <- function(msg) any(startsWith(p, msg))
  expect_true(has("strategic.defaultScenario: 'stress' is not a listed scenario"))
  expect_true(has("strategic.compacts[0].steps[1].order: 1 does not follow 1"))
  expect_true(has("strategic.compacts[1].steps[0].nodes: 'zinc' is not a node of the document"))
  expect_true(has("strategic.compacts[1].steps[0].functions: expected one per node"))
  expect_true(has("strategic.compacts[0].scenarios: no 'partial'"))
  expect_true(has("strategic.compacts[0].scenarios.decoupling.counts.build: 9 where the spine has 2"))
  expect_true(has("strategic.compacts[0].scenarios.decoupling.criticalPartners: expected an array"))
  expect_true(has("strategic.compacts[0].division[0][5]: expected flags"))
  expect_true(has("strategic.workstreams[0].compact: 'SC-Z' is not a strategic compact"))
  expect_true(has(sprintf("strategic.countries[%d].primary: more than 3 roles", i_usa - 1)))
  expect_true(has(sprintf("strategic.countries[%d].secondary: 'DMA' is not a supply role", i_chn - 1)))
  expect_true(has("strategic.compactRoles[0].leadNode: 'cell' is not a node of"))
  expect_true(has("strategic.theses[0][3]: 'friendship' is not a relationship logic"))
  expect_true(has("strategic.framing"))
  expect_true(has("strategic.compacts[0].scenarios.managed.nodes[0][4]"))
  expect_true(all(p %in% aib_check_bundle_additions(doc)))
  expect_equal(aib_check_strategic(list(strategic = "x")), "strategic: not an object")
  # a compact id twice, or a node in two compacts
  d2 <- strat_dash_doc()
  d2$strategic$compacts[[2]]$id <- "SC-A"
  d2$strategic$compacts[[2]]$steps[[1]]$nodes <- aib_dash_arr("ore")
  p2 <- aib_check_strategic(d2)
  expect_true(any(startsWith(p2, "strategic.compacts[1].id: duplicate 'SC-A'")))
  expect_true(any(startsWith(p2, "strategic.compacts[1].steps[0].nodes: 'ore' is in more than one step or compact")))
})
