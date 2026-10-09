# Tests for the composite Policy index (R/indices/build_policy_index.R) and the policy
# components it combines (R/categories/policy/).
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
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

all_three <- function(country, tech = "Solar", supply_chain = "Upstream",
                      values = c(0.8, 0.6, 0.4)) {
  component_rows(
    country, tech, supply_chain,
    c("NIPO Policy Index", "IEA PAMS Policy Index", "CAT Policy Index"),
    values
  )
}

dual_use_row <- function(country, tech = "Solar", supply_chain = "Upstream", value = 0.2) {
  component_rows(country, tech, supply_chain, "Dual-use policy score", value)
}

# The three-component weights used until Oct 2026 (NIPO 4, IEA PAMS 3, CAT 2). NIPO is out of
# the composite now, but these exercise the renormalisation over a component that is missing
# from some cells (NIPO has no Downstream cells) and the eligibility rule with three components.
nipo_weights <- c("NIPO Policy Index" = 4, "IEA PAMS Policy Index" = 3, "CAT Policy Index" = 2)

test_that("the IEA and NIPO weight tables no longer overwrite each other", {
  # Both files are sourced into one environment (scripts/10 does the same). Under the old
  # shared name JURIS_WEIGHTS the NIPO table (International 0.20) replaced the IEA one.
  expect_false(exists("JURIS_WEIGHTS"))
  expect_equal(IEA_JURIS_WEIGHTS$w_juris[IEA_JURIS_WEIGHTS$jurisdiction == "International"], 0.66)
  expect_equal(NIPO_JURIS_WEIGHTS$w_juris[NIPO_JURIS_WEIGHTS$jurisdiction == "International"], 0.20)
  expect_true(exists("IEA_STATUS_WEIGHTS"))
})

test_that("IEA Upstream stage words match whole words only", {
  # As substrings "ore" matched "more" and "Korea", "mine" "determine", "lease" "release".
  expect_equal(assign_supply_chain("korea will do more before the release to determine"), "Cross-cutting")
  expect_equal(assign_supply_chain("support for iron ore mining"), "Upstream")
  expect_equal(assign_supply_chain("offshore oil and gas leases and drilling"), "Upstream")
  expect_equal(assign_supply_chain("new coal mines and battery factory"), c("Upstream", "Midstream"))
})

test_that("the composite is the weighted mean renormalised over present components (4/3/2 weights)", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    # Downstream has no NIPO cell: weights renormalise to IEA 3, CAT 2.
    component_rows(
      "A", "Solar", "Downstream",
      c("IEA PAMS Policy Index", "CAT Policy Index"),
      c(0.6, 0.3)
    )
  )

  out <- build_composite_policy_index(comp, component_weights = nipo_weights)
  up <- out$index %>% dplyr::filter(.data$supply_chain == "Upstream")
  down <- out$index %>% dplyr::filter(.data$supply_chain == "Downstream")

  expect_equal(up$value, (4 * 0.8 + 3 * 0.6 + 2 * 0.4) / 9)
  expect_equal(down$value, (3 * 0.6 + 2 * 0.3) / 5)
  expect_equal(up$n_components, 3L)
  expect_equal(down$n_components, 2L)
  expect_true(all(out$index$variable == "Overall Policy Index"))
  expect_match(up$explanation, "NIPO (4), IEA PAMS (3), CAT (2)", fixed = TRUE)
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

  # A country with a dual-use row but no CAT rating is still ineligible.
  b_only <- dplyr::bind_rows(
    all_three("A"),
    component_rows("B", "Solar", "Upstream", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.5, 0.5)),
    dual_use_row("B")
  )
  el <- build_composite_policy_index(b_only)$eligibility
  expect_false(el$eligible[el$Country == "B"])
  el <- build_composite_policy_index(b_only, component_weights = nipo_weights)$eligibility
  expect_false(el$eligible[el$Country == "B"])
})

test_that("a country needs all three components somewhere to be scored", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    # B has no CAT rating anywhere.
    component_rows(
      "B", "Solar", "Upstream",
      c("NIPO Policy Index", "IEA PAMS Policy Index"),
      c(0.5, 0.5)
    ),
    # C has all three, but in different cells.
    component_rows("C", "Solar", "Upstream", "NIPO Policy Index", 0.3),
    component_rows("C", "Wind", "Midstream", "IEA PAMS Policy Index", 0.4),
    component_rows("C", "Wind", "Downstream", "CAT Policy Index", 0.5),
    # D's only NIPO value is NA, which does not count as observed.
    all_three("D", values = c(NA, 0.5, 0.5))
  )

  out <- build_composite_policy_index(comp, component_weights = nipo_weights)
  el <- out$eligibility

  expect_setequal(el$Country[el$eligible], c("A", "C"))
  expect_setequal(unique(out$index$Country), c("A", "C"))
  expect_equal(el$missing_components[el$Country == "B"], "CAT Policy Index")
  expect_equal(el$missing_components[el$Country == "D"], "NIPO Policy Index")
  expect_equal(el$n_components[el$Country == "B"], 2L)
  expect_false(el$has_cat_policy_index[el$Country == "B"])
  expect_true(el$has_nipo_policy_index[el$Country == "A"])
})

test_that("duplicate component rows are averaged and techs can be restricted", {
  comp <- dplyr::bind_rows(
    all_three("A"),
    component_rows("A", "Solar", "Upstream", "NIPO Policy Index", 0.4),
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

test_that("without NIPO the composite is 3 IEA + 2 CAT and needs both components", {
  w <- production_policy_weights()
  comp <- dplyr::bind_rows(
    # A has all three: its NIPO value is ignored.
    all_three("A", values = c(0.9, 0.6, 0.4)),
    # B has IEA PAMS and CAT but no NIPO anywhere: eligible now.
    component_rows("B", "Solar", "Upstream", c("IEA PAMS Policy Index", "CAT Policy Index"), c(0.2, 0.7)),
    # C has NIPO and IEA PAMS but no CAT rating: ineligible.
    component_rows("C", "Solar", "Upstream", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.5, 0.5)),
    # D has CAT only: ineligible.
    component_rows("D", "Solar", "Upstream", "CAT Policy Index", 0.5),
    # A Wind cell with IEA PAMS only is renormalised to IEA alone.
    component_rows("A", "Wind", "Midstream", c("NIPO Policy Index", "IEA PAMS Policy Index"), c(0.1, 0.3)),
    dual_use_row("A", value = 1)
  )

  out <- build_composite_policy_index(comp, component_weights = w)
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

  out <- cat_policy_index(tech_ghg, cat_policy, supply_chain_universe = "Upstream")

  a_coal <- out$value[out$Country == "A" & out$tech == "Coal"]
  b_solar <- out$value[out$Country == "B" & out$tech == "Solar"]
  expect_equal(a_coal, 0.25 * 0 + 0.75 * 1)
  expect_equal(b_solar, 1 * 0.9)
  expect_true(all(grepl("EU bloc rating", out$explanation[out$Country == "B"])))
  expect_false(any(grepl("EU bloc rating", out$explanation[out$Country == "A"])))

  # A scope lost in a join reads as national; the explanation is never blank.
  cat_policy$rating_scope[1] <- NA
  out_na <- cat_policy_index(tech_ghg, cat_policy, supply_chain_universe = "Upstream")
  expect_false(any(is.na(out_na$explanation)))
})

test_that("dual-use scores are copied to every country and say they are LLM-generated", {
  raw <- tibble::tibble(
    technology = c("Solar", "Nuclear"),
    stage = c("Upstream", "Midstream"),
    dual_use_score_0_1 = c(0.2, 0.9)
  )

  out <- clean_dual_use_scores(raw, countries = c("A", "B"))

  expect_equal(nrow(out), 4L)
  expect_equal(
    out$value[out$Country == "A" & out$tech == "Nuclear"],
    out$value[out$Country == "B" & out$tech == "Nuclear"]
  )
  expect_true(all(grepl("LLM-generated", out$source)))
})
