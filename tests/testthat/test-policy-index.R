# Tests for the composite Policy index (R/indices/build_policy_index.R) and the policy
# components it combines (R/categories/policy/).
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "categories", "policy", "iea_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "dual_use_scores.R"))
source(file.path(repo_root, "R", "indices", "build_policy_index.R"))

component_rows <- function(country, tech, supply_chain, variable, value) {
  tibble::tibble(
    Country = country,
    tech = tech,
    supply_chain = supply_chain,
    variable = variable,
    value = value
  )
}

all_three <- function(country, tech = "Solar", supply_chain = "Extraction",
                      values = c(0.8, 0.6, 0.4)) {
  component_rows(
    country, tech, supply_chain,
    c("NIPO Policy Index", "IEA PAMS Policy Index", "CAT Policy Index"),
    values
  )
}

dual_use_row <- function(country, tech = "Solar", supply_chain = "Extraction", value = 0.2) {
  component_rows(country, tech, supply_chain, "Dual-use policy score", value)
}

# The three-component weights used until Oct 2026 (NIPO 4, IEA PAMS 3, CAT 2). NIPO is out of
# the composite now, but these exercise the renormalisation over a component that is missing
# from some cells (NIPO has no Downstream, now Deployment, cells) and the eligibility rule with
# three components. The composite tests label cells with the six stages; NIPO's own
# Upstream / Midstream / Downstream rows are covered by their own test below.
nipo_weights <- c("NIPO Policy Index" = 4, "IEA PAMS Policy Index" = 3, "CAT Policy Index" = 2)

test_that("the IEA and NIPO weight tables no longer overwrite each other", {
  # Both files are sourced into one environment (scripts/10 does the same). Under the old
  # shared name JURIS_WEIGHTS the NIPO table (International 0.20) replaced the IEA one.
  expect_false(exists("JURIS_WEIGHTS"))
  expect_equal(IEA_JURIS_WEIGHTS$w_juris[IEA_JURIS_WEIGHTS$jurisdiction == "International"], 0.66)
  expect_equal(NIPO_JURIS_WEIGHTS$w_juris[NIPO_JURIS_WEIGHTS$jurisdiction == "International"], 0.20)
  expect_true(exists("IEA_STATUS_WEIGHTS"))
})

test_that("IEA Extraction stage words match whole words only", {
  # As substrings "ore" matched "more" and "Korea", "mine" "determine", "lease" "release".
  expect_equal(assign_supply_chain("korea will do more before the release to determine"), "Cross-cutting")
  expect_equal(assign_supply_chain("support for iron ore mining"), "Extraction")
  expect_equal(assign_supply_chain("offshore oil and gas leases and drilling"), "Extraction")
  # "factory" is a generic manufacturing word: Components and Assembly
  expect_equal(
    assign_supply_chain("new coal mines and battery factory"),
    c("Extraction", "Components", "Assembly")
  )
  # the three-stage rules, kept for the Sector dashboard export, are unchanged
  expect_equal(
    assign_supply_chain("new coal mines and battery factory", stages = "legacy"),
    c("Upstream", "Midstream")
  )
  expect_equal(assign_supply_chain("support for iron ore mining", stages = "legacy"), "Upstream")
})

test_that("IEA stage keywords split the old three stages into six", {
  # Upstream splits into Extraction, Refining and Recovery
  expect_equal(assign_supply_chain("grants for lithium refining and nickel smelting"), "Refining")
  expect_equal(assign_supply_chain("battery recycling and end-of-life collection targets"), "Recovery")
  expect_equal(assign_supply_chain("tariffs on scrap metal"), "Recovery")
  expect_equal(assign_supply_chain("exploration licences for critical minerals"), "Extraction")
  expect_equal(
    assign_supply_chain("support for the extraction, processing and recycling of critical minerals"),
    c("Extraction", "Refining", "Recovery")
  )
  expect_equal(
    assign_supply_chain("funding for mining and processing of critical minerals"),
    c("Extraction", "Refining")
  )
  # Midstream splits into Components and Assembly; a named step wins over a generic word
  expect_equal(assign_supply_chain("tax credit for solar cell and wafer manufacturing"), "Components")
  expect_equal(assign_supply_chain("grant for an electric vehicle assembly plant"), "Assembly")
  expect_equal(assign_supply_chain("support for manufacturing of electric vehicles"), "Assembly")
  expect_equal(assign_supply_chain("domestic battery manufacturing programme"), c("Components", "Assembly"))
  expect_equal(
    assign_supply_chain("cell manufacturing and module assembly incentives"),
    c("Components", "Assembly")
  )
  # Downstream is Deployment
  expect_equal(assign_supply_chain("rebates for rooftop solar installation"), "Deployment")
  expect_equal(assign_supply_chain("energy consumption reporting"), "Deployment")
  # a policy may hit several stages
  expect_equal(
    assign_supply_chain("ev battery recycling, cell production and charging deployment"),
    c("Components", "Deployment", "Recovery")
  )
  # words that only look like stage words
  expect_equal(assign_supply_chain("assembly bill 3 adopted by the national assembly"), "Cross-cutting")
  expect_equal(assign_supply_chain("the programme has four key components"), "Cross-cutting")
  expect_equal(assign_supply_chain("faster permit processing"), "Cross-cutting")
  expect_equal(assign_supply_chain("cash for scrapping old cars"), "Cross-cutting")
  expect_equal(assign_supply_chain("grants for individuals to scrap inefficient cars"), "Cross-cutting")
  expect_equal(assign_supply_chain("funding for the scrap vehicle bonus"), "Cross-cutting")
  expect_equal(assign_supply_chain("the act was refined by the 2008 act"), "Cross-cutting")
  expect_equal(assign_supply_chain("labels for cathode ray tube televisions"), "Cross-cutting")
  expect_equal(assign_supply_chain("embargo on refined petroleum products"), "Refining")
  expect_equal(assign_supply_chain("tariffs on copper cathodes"), "Refining")
  # every rule names a stage, in chain order
  expect_equal(names(SUPPLY_CHAIN_RULES), SUPPLY_CHAIN_STAGES)
  # the vectorised and per-text forms agree
  texts <- c("lithium refining", "national assembly", "battery factory")
  expect_equal(assign_supply_chains(texts), lapply(texts, assign_supply_chain))
})

# A minimal clean_pams_raw() output: one row per policy x country.
pams_country_rows <- function(text_all, technologies, iso3 = "AAA") {
  tibble::tibble(
    policy_id = seq_along(text_all),
    iso3 = iso3,
    country = paste("Country", iso3),
    text_all = text_all,
    technologies_list = as.list(technologies),
    policyType_list = list(character(0)),
    status = "In force",
    jurisdiction = "National"
  )
}

test_that("IEA stages are relative to the technology", {
  pams <- pams_country_rows(
    c(
      "geothermal exploration and drilling permits",
      "hydrogen use in refineries",
      "oil refinery standards",
      "lithium mining royalties"
    ),
    c("Geothermal", "Hydrogen", "Oil", "Lithium")
  )
  tbl <- build_policy_table(pams, stages = "six")
  stage_of <- function(id) tbl$supply_chain[tbl$policy_id == id]

  # geothermal drilling is Deployment; refineries are where green hydrogen is used
  expect_equal(tbl$tech[tbl$policy_id == 1], "Geothermal")
  expect_equal(stage_of(1), "Deployment")
  expect_equal(tbl$tech[tbl$policy_id == 2], "Green Hydrogen")
  expect_equal(stage_of(2), "Deployment")
  # the same words keep their stage for other technologies
  expect_equal(stage_of(3), "Refining")
  expect_equal(stage_of(4), "Extraction")
  expect_true(all(tbl$w_sc == 1))
  # a move onto a stage the policy already hits is not counted twice
  expect_equal(apply_tech_stage_overrides(c("Extraction", "Deployment"), "Geothermal"), "Deployment")
  expect_equal(apply_tech_stage_overrides("Cross-cutting", "Geothermal"), "Cross-cutting")

  # the default (the Sector dashboard export) keeps the three stages and no moves
  legacy <- build_policy_table(pams)
  expect_equal(legacy$supply_chain[legacy$policy_id == 1], "Upstream")
  expect_equal(legacy$supply_chain[legacy$policy_id == 4], "Upstream")
  expect_true(all(legacy$supply_chain %in% c("Upstream", "Midstream", "Downstream", "Cross-cutting")))
})

test_that("IEA PAMS spreads a policy naming no stage over all six stages at the cross-cutting weight", {
  pams <- pams_country_rows(
    c("national solar strategy", "solar cell manufacturing grants"),
    c("Solar", "Solar")
  )
  tbl <- build_policy_table(pams, stages = "six")
  out <- build_policy_index(tbl, tech_universe = c("Solar", "Wind"))
  clean <- out$policy_clean

  strategy <- clean %>% dplyr::filter(.data$policy_id == 1)
  expect_setequal(strategy$supply_chain, SUPPLY_CHAIN_STAGES)
  expect_equal(unique(strategy$w_sc), 0.75)
  expect_equal(clean$supply_chain[clean$policy_id == 2], "Components")
  idx <- out$policy_index
  expect_setequal(idx$supply_chain, SUPPLY_CHAIN_STAGES)
  expect_equal(idx$n_policies[idx$supply_chain == "Components"], 2L)
  expect_true(all(idx$n_policies[idx$supply_chain != "Components"] == 1L))

  # a three-stage policy table cannot be expanded onto the six stages
  expect_error(
    build_policy_index(build_policy_table(pams), tech_universe = "Solar"),
    "outside supply_chain_universe"
  )
  # ...but reproduces a three-stage build when asked for one
  legacy <- build_policy_index(
    build_policy_table(pams), tech_universe = "Solar",
    supply_chain_universe = c("Upstream", "Midstream", "Downstream")
  )
  expect_setequal(legacy$policy_index$supply_chain, c("Upstream", "Midstream", "Downstream"))
})

test_that("the composite is the weighted mean renormalised over present components (4/3/2 weights)", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    # Deployment has no NIPO cell: weights renormalise to IEA 3, CAT 2.
    component_rows(
      "A", "Solar", "Deployment",
      c("IEA PAMS Policy Index", "CAT Policy Index"),
      c(0.6, 0.3)
    )
  )

  out <- build_composite_policy_index(comp, component_weights = nipo_weights)
  up <- out$index %>% dplyr::filter(.data$supply_chain == "Extraction")
  down <- out$index %>% dplyr::filter(.data$supply_chain == "Deployment")

  expect_equal(up$value, (4 * 0.8 + 3 * 0.6 + 2 * 0.4) / 9)
  expect_equal(down$value, (3 * 0.6 + 2 * 0.3) / 5)
  expect_equal(up$n_components, 3L)
  expect_equal(down$n_components, 2L)
  expect_true(all(out$index$variable == "Overall Policy Index"))
  expect_match(up$explanation, "NIPO (4), IEA PAMS (3), CAT (2)", fixed = TRUE)
})

test_that("the composite stage names match R/utils/supply_chain_stages.R", {
  expect_identical(POLICY_SUPPLY_CHAIN_STAGES, SUPPLY_CHAIN_STAGES)
  expect_identical(POLICY_LEGACY_STAGES, SUPPLY_CHAIN_LEGACY_STAGES)
})

test_that("NIPO rows still labelled Upstream / Midstream / Downstream never become composite cells", {
  # IEA PAMS and CAT on all six stages, NIPO (still built three-stage) on Upstream and Midstream,
  # the dual-use score on six stages: the policy_component_tbl scripts/10 writes since 2026-10-26.
  six <- dplyr::bind_rows(lapply(seq_along(SUPPLY_CHAIN_STAGES), function(i) {
    component_rows(
      "A", "Solar", SUPPLY_CHAIN_STAGES[[i]],
      c("IEA PAMS Policy Index", "CAT Policy Index"),
      c(i / 10, 0.5)
    )
  }))
  nipo <- component_rows("A", "Solar", c("Upstream", "Midstream"), "NIPO Policy Index", c(0.9, 0.2))
  comp <- dplyr::bind_rows(six, nipo, dual_use_row("A", supply_chain = "Assembly", value = 1))

  # NIPO carries no weight: its rows are not read, the cells are the six stages
  out <- build_composite_policy_index(comp)
  expect_setequal(out$index$supply_chain, SUPPLY_CHAIN_STAGES)
  expect_equal(nrow(out$index), length(SUPPLY_CHAIN_STAGES))
  expect_equal(out$index, build_composite_policy_index(six)$index)
  expect_false(any(grepl("copied to each stage", out$index$explanation)))

  # NIPO weighted again: each legacy value is copied onto the stages its label covers
  w <- build_composite_policy_index(comp, component_weights = nipo_weights)$index
  value_at <- function(stage) w$value[w$supply_chain == stage]
  iea_at <- function(stage) match(stage, SUPPLY_CHAIN_STAGES) / 10
  expect_setequal(w$supply_chain, SUPPLY_CHAIN_STAGES)
  for (stage in supply_chain_stages_of("Upstream")) {
    expect_equal(value_at(stage), (4 * 0.9 + 3 * iea_at(stage) + 2 * 0.5) / 9)
  }
  for (stage in supply_chain_stages_of("Midstream")) {
    expect_equal(value_at(stage), (4 * 0.2 + 3 * iea_at(stage) + 2 * 0.5) / 9)
  }
  # no NIPO Downstream row, so Deployment renormalises to IEA PAMS and CAT
  expect_equal(value_at("Deployment"), (3 * iea_at("Deployment") + 2 * 0.5) / 5)
  expect_equal(w$n_components[w$supply_chain == "Deployment"], 2L)
  expect_match(w$explanation[[1]], "NIPO scored Upstream / Midstream / Downstream, copied to each stage covered", fixed = TRUE)

  # a table built before the migration (every weighted row three-stage, as the dashboard's
  # fixture) keeps its three-stage cells
  legacy <- build_composite_policy_index(all_three("A", supply_chain = "Upstream"), component_weights = nipo_weights)
  expect_equal(legacy$index$supply_chain, "Upstream")

  # a stage the composite cannot place stops rather than becoming a cell the SI never joins
  expect_error(
    build_composite_policy_index(dplyr::bind_rows(six, component_rows("A", "Wind", "Cross-cutting", "CAT Policy Index", 0.5))),
    "neither supply-chain stages"
  )
})

test_that("the dual-use score is carried for reference but is not a component", {
  # It is an LLM-elicited judgement, identical for every country, so it cannot rank them.
  expect_false("Dual-use policy score" %in% names(POLICY_COMPONENT_WEIGHTS))

  for (w in list(POLICY_COMPONENT_WEIGHTS, nipo_weights)) {
    without <- build_composite_policy_index(all_three("A"), component_weights = w)
    with <- build_composite_policy_index(
      dplyr::bind_rows(all_three("A"), dual_use_row("A", value = 1)),
      component_weights = w
    )
    expect_equal(with$index$value, without$index$value)
    expect_equal(with$index$n_components, length(w))
    expect_false("has_dual_use_policy_score" %in% names(with$eligibility))
  }

  # A dual-use row does not stand in for a missing component: B has no CAT rating, so it is
  # ineligible under the both-required rule, and C has no IEA PAMS, so it is ineligible under
  # the default (IEA PAMS required).
  b_only <- dplyr::bind_rows(
    all_three("A"),
    component_rows("B", "Solar", "Extraction", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.5, 0.5)),
    dual_use_row("B"),
    component_rows("C", "Solar", "Extraction", "CAT Policy Index", 0.5),
    dual_use_row("C")
  )
  el <- build_composite_policy_index(b_only, required_components = names(POLICY_COMPONENT_WEIGHTS))$eligibility
  expect_false(el$eligible[el$Country == "B"])
  el <- build_composite_policy_index(
    b_only, component_weights = nipo_weights, required_components = names(nipo_weights)
  )$eligibility
  expect_false(el$eligible[el$Country == "B"])
  el <- build_composite_policy_index(b_only)$eligibility
  expect_true(el$eligible[el$Country == "B"])
  expect_false(el$eligible[el$Country == "C"])
})

test_that("with every component required, a country needs all three somewhere to be scored", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    # B has no CAT rating anywhere.
    component_rows(
      "B", "Solar", "Extraction",
      c("NIPO Policy Index", "IEA PAMS Policy Index"),
      c(0.5, 0.5)
    ),
    # C has all three, but in different cells.
    component_rows("C", "Solar", "Extraction", "NIPO Policy Index", 0.3),
    component_rows("C", "Wind", "Components", "IEA PAMS Policy Index", 0.4),
    component_rows("C", "Wind", "Deployment", "CAT Policy Index", 0.5),
    # D's only NIPO value is NA, which does not count as observed.
    all_three("D", values = c(NA, 0.5, 0.5))
  )

  out <- build_composite_policy_index(
    comp, component_weights = nipo_weights, required_components = names(nipo_weights)
  )
  el <- out$eligibility

  expect_setequal(el$Country[el$eligible], c("A", "C"))
  expect_setequal(unique(out$index$Country), c("A", "C"))
  expect_equal(el$missing_components[el$Country == "B"], "CAT Policy Index")
  expect_equal(el$missing_components[el$Country == "D"], "NIPO Policy Index")
  expect_equal(el$missing_required[el$Country == "B"], "CAT Policy Index")
  expect_true(is.na(el$missing_required[el$Country == "A"]))
  expect_equal(el$n_components[el$Country == "B"], 2L)
  expect_false(el$has_cat_policy_index[el$Country == "B"])
  expect_true(el$has_nipo_policy_index[el$Country == "A"])
  # the basis lists the components in weight order; not eligible -> no basis
  expect_equal(el$policy_basis[el$Country == "A"], "NIPO + IEA PAMS + CAT")
  expect_true(is.na(el$policy_basis[el$Country == "B"]))
  expect_match(out$index$explanation[[1]], "countries kept only if all 3 components exist somewhere", fixed = TRUE)
})

test_that("duplicate component rows are averaged and techs can be restricted", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    component_rows("A", "Solar", "Extraction", "NIPO Policy Index", 0.4),
    all_three("A", tech = "Semiconductors")
  )

  out <- build_composite_policy_index(comp, techs = "Solar", component_weights = nipo_weights)

  # NIPO = mean(0.8, 0.4) = 0.6.
  expect_equal(out$index$value, (4 * 0.6 + 3 * 0.6 + 2 * 0.4) / 9)
  expect_equal(unique(out$index$tech), "Solar")
})

test_that("component_weights must be named by component variable", {
  expect_error(
    build_composite_policy_index(all_three("A"), component_weights = c(4, 3, 2)),
    "named numeric vector"
  )
})

# The weights scripts/20 passes since Oct 2026, read from the script so the test follows it:
# IEA PAMS 3 + CAT 2 for every country, NIPO excluded from the composite for now.
production_policy_weights <- function() {
  exprs <- parse(file.path(repo_root, "scripts", "20_build_indices.R"), keep.source = FALSE)
  hit <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("policy_component_weights"))
  }, as.list(exprs))
  if (length(hit) != 1) stop("scripts/20 must assign policy_component_weights exactly once.")
  eval(hit[[1]][[3]], envir = baseenv())
}

test_that("scripts/20 builds the Policy index from IEA PAMS (3) and CAT (2) only", {
  w <- production_policy_weights()
  expect_equal(w, c("IEA PAMS Policy Index" = 3, "CAT Policy Index" = 2))
  expect_false("NIPO Policy Index" %in% names(w))
  expect_false("Dual-use policy score" %in% names(w))
  src <- readLines(file.path(repo_root, "scripts", "20_build_indices.R"), warn = FALSE)
  # the composite is built with these weights, passed explicitly; the function default is the
  # same (so the paper scripts that call build_composite_policy_index() without weights rebuild
  # the stored index)
  expect_true(any(grepl("component_weights = policy_component_weights", src, fixed = TRUE)))
  expect_equal(POLICY_COMPONENT_WEIGHTS, w)
  # and the exclusion is recorded in the provenance saved with the index outputs
  expect_true(any(grepl("excluded_components = policy_components_excluded", src, fixed = TRUE)))
  expect_true(any(grepl("policy_index_provenance = policy_index_provenance", src, fixed = TRUE)))
})

# The eligibility rule of scripts/20: its policy_require_all and policy_required_components
# assignments, evaluated with OPSI_POLICY_REQUIRE_ALL set to `env_value` (NA: unset).
production_required_components <- function(env_value = NA) {
  exprs <- as.list(parse(file.path(repo_root, "scripts", "20_build_indices.R"), keep.source = FALSE))
  pick <- function(name) {
    hit <- Filter(function(e) {
      is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name))
    }, exprs)
    if (length(hit) != 1) stop("scripts/20 must assign ", name, " exactly once.")
    hit[[1]]
  }
  old <- Sys.getenv("OPSI_POLICY_REQUIRE_ALL", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("OPSI_POLICY_REQUIRE_ALL") else Sys.setenv(OPSI_POLICY_REQUIRE_ALL = old))
  if (is.na(env_value)) Sys.unsetenv("OPSI_POLICY_REQUIRE_ALL") else Sys.setenv(OPSI_POLICY_REQUIRE_ALL = env_value)
  env <- new.env(parent = baseenv())
  env$policy_component_weights <- production_policy_weights()
  eval(pick("policy_require_all"), envir = env)
  eval(pick("policy_required_components"), envir = env)
  env$policy_required_components
}

test_that("scripts/20 requires IEA PAMS only; OPSI_POLICY_REQUIRE_ALL=1 requires every component", {
  # the default of the function and of scripts/20 agree (the paper scripts rely on the default)
  expect_equal(production_required_components(), "IEA PAMS Policy Index")
  expect_equal(POLICY_REQUIRED_COMPONENTS, production_required_components())
  expect_equal(production_required_components("0"), "IEA PAMS Policy Index")
  expect_equal(production_required_components("1"), names(production_policy_weights()))
  expect_equal(production_required_components("true"), names(production_policy_weights()))
  src <- readLines(file.path(repo_root, "scripts", "20_build_indices.R"), warn = FALSE)
  expect_true(any(grepl("required_components = policy_required_components", src, fixed = TRUE)))
  # the reason an ally has no pick names the missing *required* component, not CAT
  expect_true(any(grepl("lacks \", .data$missing_required", src, fixed = TRUE)))
})

test_that("by default a country with IEA PAMS but no CAT is scored from IEA PAMS alone, flagged", {
  w <- production_policy_weights()
  comp <- dplyr::bind_rows(
    # A has both, in two cells; its Wind cell has IEA PAMS only and is renormalised to it.
    component_rows("A", "Solar", "Extraction", c("IEA PAMS Policy Index", "CAT Policy Index"), c(0.6, 0.4)),
    component_rows("A", "Wind", "Components", "IEA PAMS Policy Index", 0.3),
    # B has IEA PAMS but no CAT rating anywhere (the PAMS-only fallback).
    component_rows("B", "Solar", "Extraction", "IEA PAMS Policy Index", 0.2),
    component_rows("B", "Wind", "Components", c("IEA PAMS Policy Index", "NIPO Policy Index"), c(0.8, 0.9)),
    # C has a CAT rating but no IEA PAMS: not scored (IEA PAMS is the backbone).
    component_rows("C", "Solar", "Extraction", "CAT Policy Index", 0.5),
    # D has only an NA IEA PAMS value, which does not count as observed.
    component_rows("D", "Solar", "Extraction", c("IEA PAMS Policy Index", "CAT Policy Index"), c(NA, 0.5)),
    dual_use_row("B", value = 1)
  )

  out <- build_composite_policy_index(comp, component_weights = w)
  el <- out$eligibility
  idx <- out$index
  get <- function(tbl, country, col) tbl[[col]][tbl$Country == country]

  expect_setequal(el$Country[el$eligible], c("A", "B"))
  expect_equal(get(el, "A", "policy_basis"), "IEA PAMS + CAT")
  expect_equal(get(el, "B", "policy_basis"), "IEA PAMS only")
  expect_true(all(is.na(c(get(el, "C", "policy_basis"), get(el, "D", "policy_basis")))))
  # missing_components still lists every weighted component not observed; missing_required
  # only the required ones, and only for countries that are not scored
  expect_equal(get(el, "B", "missing_components"), "CAT Policy Index")
  expect_true(is.na(get(el, "B", "missing_required")))
  expect_equal(get(el, "C", "missing_required"), "IEA PAMS Policy Index")
  expect_equal(get(el, "D", "missing_required"), "IEA PAMS Policy Index")
  expect_equal(get(el, "B", "n_components"), 1L)

  # B's index is its IEA PAMS value (NIPO and the dual-use score are not components)
  b <- idx %>% dplyr::filter(.data$Country == "B") %>% dplyr::arrange(.data$tech)
  expect_equal(b$value, c(0.2, 0.8))
  expect_equal(b$n_components, c(1L, 1L))
  expect_equal(unique(b$policy_basis), "IEA PAMS only")
  expect_match(b$explanation[[1]], "countries kept if IEA PAMS exists somewhere; basis IEA PAMS only", fixed = TRUE)
  # A is unchanged by the fallback
  a <- idx %>% dplyr::filter(.data$Country == "A") %>% dplyr::arrange(.data$tech)
  expect_equal(a$value, c((3 * 0.6 + 2 * 0.4) / 5, 0.3))
  expect_equal(unique(a$policy_basis), "IEA PAMS + CAT")
  strict <- build_composite_policy_index(comp, component_weights = w, required_components = names(w))
  expect_setequal(strict$eligibility$Country[strict$eligibility$eligible], "A")
  expect_equal(
    dplyr::select(strict$index, -"explanation"),
    dplyr::select(dplyr::filter(idx, .data$Country == "A"), -"explanation")
  )
})

test_that("required_components must be weighted components", {
  expect_error(
    build_composite_policy_index(all_three("A"), required_components = "NIPO Policy Index"),
    "required_components must name"
  )
  expect_error(
    build_composite_policy_index(all_three("A"), required_components = character()),
    "required_components must name"
  )
  expect_error(
    build_composite_policy_index(all_three("A"), required_components = NA_character_),
    "required_components must name"
  )
})

test_that("with both required, the composite is 3 IEA + 2 CAT and NIPO is ignored", {
  w <- production_policy_weights()
  comp <- dplyr::bind_rows(
    # A has all three: its NIPO value is ignored.
    all_three("A", values = c(0.9, 0.6, 0.4)),
    # B has IEA PAMS and CAT but no NIPO anywhere: eligible now.
    component_rows("B", "Solar", "Extraction", c("IEA PAMS Policy Index", "CAT Policy Index"), c(0.2, 0.7)),
    # C has NIPO and IEA PAMS but no CAT rating: ineligible.
    component_rows("C", "Solar", "Extraction", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.5, 0.5)),
    # D has CAT only: ineligible.
    component_rows("D", "Solar", "Extraction", "CAT Policy Index", 0.5),
    # A Wind cell with IEA PAMS only is renormalised to IEA alone.
    component_rows("A", "Wind", "Components", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.1, 0.3)),
    dual_use_row("A", value = 1)
  )

  out <- build_composite_policy_index(comp, component_weights = w, required_components = names(w))
  el <- out$eligibility
  idx <- out$index

  expect_setequal(el$Country[el$eligible], c("A", "B"))
  expect_equal(el$missing_components[el$Country == "C"], "CAT Policy Index")
  expect_equal(el$missing_components[el$Country == "D"], "IEA PAMS Policy Index")
  expect_false(any(grepl("nipo", names(el))))
  expect_equal(idx$value[idx$Country == "A" & idx$tech == "Solar"], (3 * 0.6 + 2 * 0.4) / 5)
  expect_equal(idx$value[idx$Country == "B"], (3 * 0.2 + 2 * 0.7) / 5)
  expect_equal(idx$value[idx$Country == "A" & idx$tech == "Wind"], 0.3)
  expect_equal(idx$n_components[idx$Country == "A" & idx$tech == "Solar"], 2L)
  expect_match(idx$explanation[[1]], "IEA PAMS (3), CAT (2)", fixed = TRUE)
  expect_false(any(grepl("NIPO", idx$explanation)))
})

test_that("CAT labels resolve to WDI names and EU members inherit the EU bloc rating", {
  country_info <- tibble::tibble(
    country = c("United Arab Emirates", "Germany", "France", "Italy", "Denmark", "Iran", "Gambia", "Japan"),
    iso3c = c("ARE", "DEU", "FRA", "ITA", "DNK", "IRN", "GMB", "JPN")
  )
  cat_raw <- tibble::tibble(
    Country = c("UAE", "Germany", "EU", "Iran (Islamic Republic of)", "The Gambia", "Japan"),
    Overall.rating = c(
      "Critically insufficient", "Insufficient", "Highly insufficient",
      "Critically insufficient", "Almost Sufficient", "Insufficient"
    )
  )

  out <- cat_prepare_ratings(cat_raw, country_info = country_info, eu_member_iso3 = c("DEU", "FRA", "ITA", "DNK"))

  expect_false("EU" %in% out$Country)
  expect_false("European Union" %in% out$Country)
  expect_true(all(c("United Arab Emirates", "Iran", "Gambia") %in% out$Country))
  # Germany keeps its own rating; the other members take the bloc's.
  expect_equal(out$Overall.rating[out$Country == "Germany"], "Insufficient")
  expect_equal(out$rating_scope[out$Country == "Germany"], "national")
  for (member in c("France", "Italy", "Denmark")) {
    expect_equal(out$Overall.rating[out$Country == member], "Highly insufficient")
    expect_equal(out$rating_scope[out$Country == member], "EU bloc rating")
  }
  expect_equal(anyDuplicated(out$Country), 0L)
})

test_that("cat_policy_index computes c*g + (1-c)*(1-g) and labels EU bloc rows", {
  tech_ghg <- tibble::tibble(tech = c("Coal", "Solar"), ghg_index = c(0, 0.9))
  cat_policy <- tibble::tibble(
    Country = c("A", "B"),
    climate_policy_index = c(0.25, 1),
    rating_scope = c("national", "EU bloc rating")
  )

  out <- cat_policy_index(tech_ghg, cat_policy)

  # one economy-wide value, copied onto each of the six stages
  expect_setequal(out$supply_chain, SUPPLY_CHAIN_STAGES)
  expect_equal(nrow(out), 2L * 2L * length(SUPPLY_CHAIN_STAGES))
  expect_equal(nrow(dplyr::distinct(out, .data$Country, .data$tech, .data$value)), 4L)
  expect_true(all(grepl("the same on every supply-chain stage", out$explanation, fixed = TRUE)))

  a_coal <- unique(out$value[out$Country == "A" & out$tech == "Coal"])
  b_solar <- unique(out$value[out$Country == "B" & out$tech == "Solar"])
  expect_equal(a_coal, 0.25 * 0 + 0.75 * 1)
  expect_equal(b_solar, 1 * 0.9)
  expect_true(all(grepl("EU bloc rating", out$explanation[out$Country == "B"])))
  expect_false(any(grepl("EU bloc rating", out$explanation[out$Country == "A"])))

  # a single stage, as scripts/paper/fig_si_fixed_inputs.R asks for
  one <- cat_policy_index(tech_ghg, cat_policy, supply_chain_universe = "Components")
  expect_equal(unique(one$supply_chain), "Components")
  expect_equal(nrow(one), 4L)

  # A scope lost in a join reads as national; the explanation is never blank.
  cat_policy$rating_scope[1] <- NA
  out_na <- cat_policy_index(tech_ghg, cat_policy)
  expect_false(any(is.na(out_na$explanation)))
})

test_that("dual-use scores are copied to every country and say they are LLM-generated", {
  raw <- tibble::tibble(
    technology = c("Solar", "Nuclear"),
    stage = c("Upstream", "Midstream"),
    dual_use_score_0_1 = c(0.2, 0.9)
  )

  out <- clean_dual_use_scores(raw, countries = c("A", "B"))

  # Upstream covers Extraction, Refining and Recovery; Midstream Components and Assembly
  expect_equal(nrow(out), 2L * (3L + 2L))
  expect_equal(
    out$value[out$Country == "A" & out$tech == "Nuclear"],
    out$value[out$Country == "B" & out$tech == "Nuclear"]
  )
  expect_true(all(grepl("LLM-generated", out$source)))
})

test_that("dual-use scores are copied from the source's three stages onto the six", {
  # the raw table's codes: U / M / D, described as primary / secondary / tertiary
  raw <- tibble::tibble(
    technology = c("Batteries", "Batteries", "Batteries", "Oil"),
    stage = c("U", "M", "D", "tertiary"),
    dual_use_score_0_1 = c(0.85, 0.9, 0.7, 0.9)
  )

  out <- clean_dual_use_scores(raw, countries = "A")
  bat <- out %>% dplyr::filter(.data$tech == "Batteries")
  value_at <- function(stage) bat$value[bat$supply_chain == stage]

  assert_supply_chain_stages(out$supply_chain, "dual_use_scores_tbl")
  expect_setequal(bat$supply_chain, SUPPLY_CHAIN_STAGES)
  for (stage in c("Extraction", "Refining", "Recovery")) expect_equal(value_at(stage), 0.85)
  for (stage in c("Components", "Assembly")) expect_equal(value_at(stage), 0.9)
  expect_equal(value_at("Deployment"), 0.7)
  expect_equal(out$supply_chain[out$tech == "Oil"], "Deployment")
  expect_match(bat$explanation[bat$supply_chain == "Refining"], "scored for Upstream and copied to each stage it covers")
  expect_false("source_stage" %in% names(out))

  # an unknown stage code stops instead of becoming a cell no index joins
  expect_error(
    clean_dual_use_scores(dplyr::mutate(raw, stage = c("U", "M", "D", "X")), countries = "A"),
    "unknown supply_chain"
  )
})
