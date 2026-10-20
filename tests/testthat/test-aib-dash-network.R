repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
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
    supplier_n_upstream_partners = c(1, 0, 1, 0), buyer_n_upstream_partners = 0, investment_host_n_upstream_partners = 0, investor_n_upstream_partners = 0,
    supplier_n_midstream_partners = c(2, 0, 1, 0), buyer_n_midstream_partners = c(0, 1, 0, 1), investment_host_n_midstream_partners = c(1, 0, 1, 0), investor_n_midstream_partners = c(0, 1, 0, 1),
    supplier_n_downstream_partners = 0, buyer_n_downstream_partners = 0, investment_host_n_downstream_partners = 0, investor_n_downstream_partners = 0,
    technologies = "Solar", n_technologies = 1, roles = c("investment_host; supplier", "buyer; investor", "investment_host; supplier", "buyer; investor"),
    headline_lost_usd = c(NA, 3e9, NA, 3e9), n_exposed_subsectors = c(NA, 2, NA, 2)
  )
  edges <- tibble::tibble(
    supplier_iso = c("DEU", "DEU"), buyer_iso = c("USA", "USA"), scenario = c("reallocation", "conservative"),
    n_trade_selected = c(9, 4), n_investment_selected = c(1, 1), n_shocks = c(2, 2), n_subsectors = 1, n_technologies = 1,
    technologies = "Solar", subsectors = "solar-cells", n_high_severity = 1, upstream = FALSE, midstream = TRUE, downstream = FALSE,
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
  expect_equal(as.character(n$edges[[1]]$stages), "Midstream")
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
  p <- aib_check_bundle_additions(bad)
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
