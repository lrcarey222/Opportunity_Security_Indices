# Guards the curated node attributes of the strategic posture layer (docs/strategic_position.md,
# section 7): data/reference/posture_node_attributes.csv and the helpers that check and resolve it
# (R/strategic_position/posture_nodes.R). Fixtures are synthetic; the tracked reference CSVs are read
# only to check the curated table itself.

pn_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(pn_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(pn_repo_root(), "R", "strategic_position", "posture_nodes.R"))

pn_ref_csv <- function(name) {
  utils::read.csv(file.path(pn_repo_root(), "data", "reference", name), colClasses = "character", check.names = FALSE)
}

# ---- Synthetic fixtures ------------------------------------------------------------------------------
# Two technologies x six stages, one Y&F-linked layer, one stand-alone control layer, one country row.
pn_fixture_stage <- function(tech, stage) {
  dep <- stage == "Deployment"
  data.frame(
    node_id = paste0(tolower(tech), ":", tolower(stage), ":stage"), tech = tech, stage = stage, layer_id = "stage",
    layer_label = paste(tech, stage), layer_type = "stage", country_iso3 = "*", yf_component_id = "",
    critical_function = "Something", consequence = "moderate", time_horizon = "years",
    digitally_active = "no", remote_control = "no", separability = "Unknown",
    substitutability = "moderate", qualification_time = "m3to12", reconstitution_time = "y1to3", stockpile = "Unknown",
    process_knowledge = "partly", workforce_suppliers = "partly", adjacent_innovation = "no", maturity = "mature",
    standardization = "moderate", differentiation = "moderate", commoditization = "partly",
    leap_candidate = "no", successor = "", beyond_novelty = "", cost_perf_advantage = "", incumbent_vulnerable = "",
    scale_pathway = "", deployment_critical = if (dep) "no" else "yes",
    primary_measure = if (dep) "none" else "atlas_export_share", mineral = "", market_scope = "global",
    source = "fixture", confidence = "moderate", notes = "fixture row", stringsAsFactors = FALSE
  )
}

pn_fixture <- function() {
  techs <- c("Solar", "Oil")
  st <- do.call(rbind, lapply(techs, function(t) do.call(rbind, lapply(SUPPLY_CHAIN_STAGES, function(s) pn_fixture_stage(t, s)))))
  yf_layer <- pn_fixture_stage("Solar", "Assembly")
  yf_layer[1, c("node_id", "layer_id", "layer_label", "layer_type", "yf_component_id")] <-
    c("solar:assembly:pv-x", "pv-x", "PV thing", "hardware", "pv-x")
  # Blank cells inherit from the Y&F row; consequence is overridden here.
  blank <- c("critical_function", "time_horizon", "digitally_active", "remote_control", "separability", "substitutability",
             "qualification_time", "reconstitution_time", "stockpile", "process_knowledge", "workforce_suppliers",
             "adjacent_innovation", "maturity")
  yf_layer[1, blank] <- ""
  yf_layer$consequence <- "low"
  ctl <- pn_fixture_stage("Oil", "Assembly")
  ctl[1, c("node_id", "layer_id", "layer_label", "layer_type")] <- c("oil:assembly:ot", "ot", "OT control", "control")
  ctl[1, c("digitally_active", "remote_control", "separability", "consequence", "time_horizon", "primary_measure", "deployment_critical")] <-
    c("yes", "yes", "separable", "high", "days", "none", "no")
  cty <- pn_fixture_stage("Oil", "Extraction")
  cty[1, setdiff(PN_COLUMNS, c("node_id", "tech", "stage", "layer_id", "country_iso3", "source", "confidence", "notes"))] <- ""
  cty$node_id <- "oil:extraction:stage@JPN"
  cty$country_iso3 <- "JPN"
  cty$stockpile <- "m3to12"
  cty$confidence <- "high"
  rbind(st, yf_layer, ctl, cty)
}

pn_fixture_yf <- function() {
  data.frame(
    component_id = c("pv-x", "unused"), layer = c("hardware", "control"),
    critical_function = c("Power-sector deployment", "x"), consequence = c("moderate", "high"),
    time_horizon = c("months", "days"), digitally_active = c("no", "yes"), remote_control = c("no", "yes"),
    separability = c("Unknown", "separable"), substitutability = c("hard", "Unknown"),
    qualification_time = c("y1to3", "Unknown"), reconstitution_time = c("gt3y", "Unknown"), stockpile = c("Unknown", "Unknown"),
    surge_capacity = c("none", "Unknown"), process_knowledge = c("yes", "Unknown"), workforce_suppliers = c("partly", "Unknown"),
    adjacent_innovation = c("Unknown", "Unknown"), complementary_capabilities = c("yes", "Unknown"), maturity = c("scaling", "mature"),
    paper_placement = c("medium", "small"), stringsAsFactors = FALSE
  )
}

pn_fx_techs <- c("Solar", "Oil")

# ---- The curated reference table ---------------------------------------------------------------------
test_that("the reference table passes its checks against the Y&F table and the headline technologies", {
  cfg <- yaml::read_yaml(file.path(pn_repo_root(), "config", "strategic_position.yml"))
  tbl <- pn_read_check(pn_ref_csv("posture_node_attributes.csv"), pn_ref_csv("yards_fences_assessments.csv"),
                       techs = unlist(cfg$headline_techs))
  expect_identical(names(tbl), PN_COLUMNS)
  expect_setequal(unlist(cfg$headline_techs), PN_HEADLINE_TECHS)
})

test_that("the reference table has the 66 whole-cell rows and links all 21 Y&F components", {
  tbl <- pn_ref_csv("posture_node_attributes.csv")
  yf <- pn_ref_csv("yards_fences_assessments.csv")
  cells <- tbl[tbl$layer_id == "stage" & tbl$country_iso3 == "*", ]
  expect_equal(nrow(cells), length(PN_HEADLINE_TECHS) * length(SUPPLY_CHAIN_STAGES))
  expect_equal(anyDuplicated(paste(cells$tech, cells$stage)), 0)
  expect_setequal(intersect(yf$component_id, tbl$yf_component_id), yf$component_id)
  expect_true(all(cells$digitally_active == "no" & cells$remote_control == "no" & cells$separability == "Unknown"))
  expect_true(all(cells$primary_measure[cells$stage == "Deployment"] == "none"))
  star <- tbl[tbl$country_iso3 == "*", ]
  expect_true(all(star$primary_measure[star$layer_type == "control"] == "none"))
  expect_true(all(nzchar(tbl$source)) && all(tbl$confidence %in% PN_CONFIDENCE))
})

test_that("no yard, posture or paper-placement column exists in the node table", {
  tbl <- pn_ref_csv("posture_node_attributes.csv")
  expect_false(any(grepl("yard|placement|posture", names(tbl), ignore.case = TRUE)))
  expect_false(any(grepl("yard|placement|posture", PN_COLUMNS, ignore.case = TRUE)))
  bad <- cbind(pn_fixture(), paper_placement = "small")
  expect_error(pn_read_check(bad, pn_fixture_yf(), techs = pn_fx_techs), "outside the contract")
  # Resolved rows carry no yard either, even though the Y&F table has paper_placement.
  res <- pn_resolve(pn_fixture(), pn_fixture_yf(), iso3 = "DEU")
  expect_false(any(grepl("yard|placement|posture", names(res), ignore.case = TRUE)))
})

test_that("each Y&F-linked layer sits at a stage its evidence scope maps to in the HS6 master", {
  tbl <- pn_ref_csv("posture_node_attributes.csv")
  yf <- pn_ref_csv("yards_fences_assessments.csv")
  m <- pn_ref_csv("energy_hs6_master.csv")
  m <- m[m$excluded == "FALSE", ]
  m$scope <- paste0(gsub(" ", "-", tolower(m$tech)), "-", tolower(m$supply_chain), "::", m$sub_sector)
  linked <- tbl[tbl$yf_component_id != "" & tbl$country_iso3 == "*", ]
  for (i in seq_len(nrow(linked))) {
    y <- yf[yf$component_id == linked$yf_component_id[i], ]
    scope <- if (y$evidence_scope == "subcomponent") y$evidence_scope_id else
      sub("^subcomponent:", "", grep("^subcomponent:", strsplit(y$secondary_scopes, ";")[[1]], value = TRUE)[1])
    if (is.na(scope) || !nzchar(scope)) next
    hit <- m[m$scope == scope & m$tech == linked$tech[i], ]
    # A proxy scope from another technology (battery enclosures on solar racking) is explained in notes.
    if (nrow(hit) == 0) next
    expect_true(linked$stage[i] %in% hit$stage, info = paste(linked$node_id[i], scope))
  }
})

# ---- pn_read_check -----------------------------------------------------------------------------------
test_that("the synthetic fixture passes the checks", {
  expect_silent(tbl <- pn_read_check(pn_fixture(), pn_fixture_yf(), techs = pn_fx_techs))
  expect_equal(nrow(tbl), nrow(pn_fixture()))
})

test_that("vocabulary: values outside the domains are rejected, Unknown is accepted", {
  fx <- pn_fixture()
  fx$consequence[1] <- "critical"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "consequence outside")
  fx <- pn_fixture()
  fx$consequence[1] <- "Unknown"
  expect_silent(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs))
  fx <- pn_fixture()
  fx$market_scope[1] <- "Unknown"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "market_scope outside")
  fx <- pn_fixture()
  fx$primary_measure[1] <- "bnef_capacity"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "unknown primary_measure")
  fx <- pn_fixture()
  fx$stage[1] <- "Upstream"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "six stages")
  fx <- pn_fixture()
  fx$confidence[1] <- "medium"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "confidence")
})

test_that("completeness: every technology x stage needs a whole-cell row, and '*' rows state every attribute", {
  fx <- pn_fixture()
  fx <- fx[!(fx$tech == "Oil" & fx$stage == "Recovery" & fx$layer_id == "stage"), ]
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "missing whole-cell")
  fx <- pn_fixture()
  fx$substitutability[1] <- ""
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "empty substitutability")
  fx <- pn_fixture()
  fx$source[2] <- ""
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "empty source")
  fx <- pn_fixture()
  fx <- rbind(fx, fx[1, ])
  fx$node_id[nrow(fx)] <- "dup"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "duplicate")
})

test_that("whole cells keep the physical-path convention; control layers are digital", {
  fx <- pn_fixture()
  fx$digitally_active[1] <- "yes"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "physical|digitally_active = no")
  fx <- pn_fixture()
  i <- which(fx$layer_id == "ot")
  fx$digitally_active[i] <- "no"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "digitally active")
  fx <- pn_fixture()
  fx$primary_measure[i] <- "atlas_export_share"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "primary_measure 'none'")
  fx <- pn_fixture()
  fx$primary_measure[fx$stage == "Deployment" & fx$layer_id == "stage"][1] <- "atlas_export_share"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "primary_measure 'none'")
})

test_that("leap tests exist only on leap candidates, and a candidate states them all", {
  fx <- pn_fixture()
  fx$beyond_novelty[1] <- "partly"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "not a leap candidate")
  fx <- pn_fixture()
  fx$leap_candidate[1] <- "yes"
  fx$successor[1] <- "Something new"
  fx[1, PN_LEAP_TESTS] <- c("partly", "Unknown", "partly", "partly")
  expect_silent(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs))
  fx$scale_pathway[1] <- ""
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "leap candidate without scale_pathway")
})

test_that("Y&F links must exist and match the component's layer", {
  fx <- pn_fixture()
  fx$yf_component_id[fx$layer_id == "pv-x"] <- "nope"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "not in yards_fences_assessments")
  fx <- pn_fixture()
  fx$layer_type[fx$layer_id == "pv-x"] <- "material"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "layer_type differs")
})

test_that("country rows only refine an existing node", {
  fx <- pn_fixture()
  i <- which(fx$country_iso3 == "JPN")
  fx$layer_id[i] <- "not-a-layer"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "without a '\\*' row")
  fx <- pn_fixture()
  fx$stockpile[i] <- ""
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "override nothing")
  fx <- pn_fixture()
  fx$country_iso3[i] <- "Japan"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "ISO3")
})

# ---- pn_resolve --------------------------------------------------------------------------------------
test_that("resolve returns one row per (iso3, tech, stage, layer_id) with no literal Unknown", {
  fx <- pn_fixture()
  res <- pn_resolve(fx, pn_fixture_yf(), iso3 = c("JPN", "DEU"))
  n_star <- sum(fx$country_iso3 == "*")
  expect_equal(nrow(res), 2 * n_star)
  expect_equal(anyDuplicated(paste(res$iso3, res$tech, res$stage, res$layer_id)), 0)
  expect_true(all(c("iso3", "tech", "stage", "layer_id", "layer_type", names(PN_DOMAINS), "attr_provenance",
                    "attr_origin", "attr_confidence", "attr_source", "country_override") %in% names(res)))
  expect_false(any(unlist(res[names(PN_DOMAINS)]) %in% PN_UNKNOWN))
  expect_false(any(c("surge_capacity", "complementary_capabilities", "country_iso3") %in% names(res)))
  expect_error(pn_resolve(fx, pn_fixture_yf(), iso3 = "Japan"), "ISO3")
  expect_error(pn_resolve(fx, NULL, iso3 = "JPN"), "yf_components")
})

test_that("a linked layer inherits blank cells from the Y&F row and keeps its own values", {
  res <- pn_resolve(pn_fixture(), pn_fixture_yf(), iso3 = "DEU")
  r <- res[res$layer_id == "pv-x", ]
  expect_equal(r$consequence, "low") # own value beats the Y&F 'moderate'
  expect_equal(r$time_horizon, "months") # inherited
  expect_equal(r$substitutability, "hard")
  expect_equal(r$reconstitution_time, "gt3y")
  expect_equal(r$maturity, "scaling")
  expect_equal(r$critical_function, "Power-sector deployment")
  expect_true(is.na(r$adjacent_innovation)) # Y&F Unknown stays unknown
  expect_true(is.na(r$separability))
  expect_match(r$attr_provenance, "adjacent_innovation:unknown")
  expect_match(r$attr_provenance, "time_horizon:assessment")
  expect_match(r$attr_origin, "time_horizon:yf")
  expect_match(r$attr_origin, "consequence:node")
  expect_false(grepl("adjacent_innovation:", r$attr_origin))
  expect_match(r$attr_provenance, "beyond_novelty:n_a")
})

test_that("a country row overrides the '*' row field by field, for that country only", {
  res <- pn_resolve(pn_fixture(), pn_fixture_yf(), iso3 = c("JPN", "DEU"))
  jp <- res[res$iso3 == "JPN" & res$tech == "Oil" & res$stage == "Extraction" & res$layer_id == "stage", ]
  de <- res[res$iso3 == "DEU" & res$tech == "Oil" & res$stage == "Extraction" & res$layer_id == "stage", ]
  expect_equal(jp$stockpile, "m3to12")
  expect_true(is.na(de$stockpile))
  expect_equal(jp$consequence, de$consequence) # untouched fields inherit the '*' row
  expect_true(jp$country_override)
  expect_false(de$country_override)
  expect_match(jp$attr_origin, "stockpile:country")
  # The country row is high confidence, the '*' row moderate: the resolved row takes the lower.
  expect_equal(jp$attr_confidence, "moderate")
  expect_match(jp$attr_source, "fixture \\| fixture")
})

test_that("coverage counts unknowns per field, leap tests only on candidates", {
  res <- pn_resolve(pn_fixture(), pn_fixture_yf(), iso3 = "DEU")
  cov <- pn_coverage(res)
  expect_equal(cov$n_unknown[cov$field == "separability"], nrow(res) - 1) # only the control layer states it
  expect_false("beyond_novelty" %in% cov$field)
  expect_equal(cov$n_unknown[cov$field == "consequence"], 0)
})

test_that("the large-yard attributes are optional columns after maturity, with their own vocabularies", {
  expect_equal(PN_COLUMNS[match("maturity", PN_COLUMNS) + 1:3], PN_OPTIONAL_COLUMNS)
  expect_equal(PN_DOMAINS$standardization, c("low", "moderate", "high"))
  expect_equal(PN_DOMAINS$differentiation, c("low", "moderate", "high"))
  expect_equal(PN_DOMAINS$commoditization, c("no", "partly", "yes"))
  # a table that predates them passes and reads them as not observed
  fx <- pn_fixture()
  old <- fx[, setdiff(names(fx), PN_OPTIONAL_COLUMNS)]
  expect_silent(tbl <- pn_read_check(old, pn_fixture_yf(), techs = pn_fx_techs))
  expect_identical(names(tbl), PN_COLUMNS)
  res <- pn_resolve(old, pn_fixture_yf(), iso3 = "DEU")
  expect_true(all(is.na(res$standardization) & is.na(res$differentiation) & is.na(res$commoditization)))
  expect_match(res$attr_provenance[1], "commoditization:unknown")
  # values outside the vocabulary are refused; Unknown and blank are not observed
  fx$standardization[1] <- "very"
  expect_error(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs), "standardization outside")
  fx <- pn_fixture()
  fx$commoditization[2] <- "Unknown"
  fx$differentiation[3] <- ""
  expect_silent(pn_read_check(fx, pn_fixture_yf(), techs = pn_fx_techs))
  res <- pn_resolve(fx, pn_fixture_yf(), iso3 = c("JPN", "DEU"))
  expect_true(is.na(res$commoditization[res$node_id == fx$node_id[2] & res$iso3 == "DEU"]))
  expect_true(is.na(res$differentiation[res$node_id == fx$node_id[3] & res$iso3 == "DEU"]))
  expect_equal(res$commoditization[res$node_id == fx$node_id[1] & res$iso3 == "DEU"], "partly")
  # a country row may state one
  fx <- pn_fixture()
  fx$commoditization[fx$country_iso3 == "JPN"] <- "yes"
  res <- pn_resolve(fx, pn_fixture_yf(), iso3 = c("JPN", "DEU"))
  key <- res$tech == "Oil" & res$stage == "Extraction" & res$layer_id == "stage"
  expect_equal(res$commoditization[key & res$iso3 == "JPN"], "yes")
  expect_equal(res$commoditization[key & res$iso3 == "DEU"], "partly")
})
