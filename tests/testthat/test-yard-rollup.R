# Guards the layer-first yards (R/strategic_position/yard_rollup.R; docs/strategic_position.md, "Layer-first
# yards and the roll-up"; user spec 2026-10-30, Yards & Fences item 1): the choice -> yard mapping, the roll-up
# of a cell's units (all agree, each mixed form), the children as a JSON array (one item stays an array), layers
# taking their own choice, and the paper-reproduction diagnostic. Fixtures are synthetic.

yr_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/industrial_opportunity.R", "R/strategic_position/strategic_value.R",
            "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R")) {
  source(file.path(yr_repo_root(), f))
}
yr_full_cfg <- function() yaml::read_yaml(file.path(yr_repo_root(), "config", "strategic_position.yml"))
yr_cfg <- function() yr_full_cfg()$decision_framework
yr_post <- function() yr_full_cfg()$strategic_posture

test_that("each choice maps to the paper's yard: Secure small, Preserve / Catch Up / Partner and Accelerate medium, Watch large", {
  v <- c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "watch", "not_assessed", NA)
  st <- c("Assembly", "Assembly", "Refining", "Extraction", "Deployment", "Components", "Deployment", "Assembly", "Assembly")
  expect_equal(sp_yard_of_choice(v, st), c("small", "medium", "medium", "medium", "medium", "large", "large", NA, NA))
  # Accelerate is a Deployment choice: elsewhere it has no yard
  expect_true(is.na(sp_yard_of_choice("accelerate", "Assembly")))
  expect_equal(unname(SP_YARD_FENCE[c("small", "medium", "large")]), c("high", "medium", "low"))
})

test_that("the roll-up: one yard when the units agree, else the mixed form they span", {
  expect_equal(sp_yard_combine(c("medium", "medium", NA)), "medium")
  expect_equal(sp_yard_combine(c("small", "medium")), "mixed_high_medium")
  expect_equal(sp_yard_combine(c("medium", "large", "large")), "mixed_medium_low")
  expect_equal(sp_yard_combine(c("large", "small")), "mixed_high_low")
  expect_equal(sp_yard_combine(c("large", "medium", "small", NA)), "mixed_high_medium_low")
  expect_true(is.na(sp_yard_combine(c(NA, NA))))
  expect_true(all(c("small", "medium", "large", "mixed_high_medium", "mixed_medium_low", "mixed_high_low", "mixed_high_medium_low") %in%
                    SP_YARD_ROLLUPS))
})

yr_cells <- function() {
  data.frame(iso3 = "AAA", tech = c("Solar", "Solar", "Electric Grid", "Batteries", "Wind", "Nuclear"),
             supply_chain = c("Assembly", "Components", "Assembly", "Assembly", "Assembly", "Refining"),
             strategic_choice = c("partner", "watch", "watch", "preserve", "watch", "not_assessed"), stringsAsFactors = FALSE)
}
yr_layers <- function() {
  data.frame(iso3 = "AAA",
             tech = c("Solar", "Solar", "Solar", "Electric Grid", "Electric Grid", "Batteries", "Batteries", "Nuclear"),
             supply_chain = c("Assembly", "Assembly", "Components", "Assembly", "Assembly", "Assembly", "Assembly", "Refining"),
             layer_id = c("pv-modules", "pv-racking", "pv-nextgen", "grid-scada", "grid-transformers", "bess-control", "bess-cells", "nuc-haleu"),
             layer_label = c("Modules", "Racking", "Next-gen PV", "Grid control", "Transformers", "BMS", "Cells", "HALEU"),
             strategic_choice = c("watch", "watch", "catch_up", "secure", "watch", "secure", "preserve", "secure"),
             choice_reason = paste("Reason", 1:8), stringsAsFactors = FALSE)
}

test_that("cells roll their stage row and layers up, with the children as a JSON array", {
  r <- sp_yard_rollup(yr_cells(), yr_layers())
  c <- r$cells
  expect_equal(c$yard, c("medium", "large", "large", "medium", "large", NA))
  expect_equal(c$yard_rollup, c("mixed_medium_low", "mixed_medium_low", "mixed_high_low", "mixed_high_medium", "large", "small"))
  expect_equal(c$yard_units, c(3L, 2L, 3L, 3L, 1L, 2L))
  expect_equal(c$yard_mixed, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_true(is.na(c$yard_children[5]))                  # no layers
  # one child stays a one-item array
  expect_match(c$yard_children[2], "^\\[\\{")
  one <- jsonlite::fromJSON(c$yard_children[2], simplifyVector = FALSE)
  expect_length(one, 1)
  expect_equal(one[[1]][c("layer_id", "label", "yard", "choice", "reason")],
               list(layer_id = "pv-nextgen", label = "Next-gen PV", yard = "medium", choice = "catch_up", reason = "Reason 3"))
  two <- jsonlite::fromJSON(c$yard_children[1], simplifyVector = FALSE)
  expect_equal(vapply(two, `[[`, "", "yard"), c("large", "large"))
  # the layers carry their own yard and their cell's roll-up
  l <- r$layers
  expect_equal(l$yard, c("large", "large", "medium", "small", "large", "small", "medium", "small"))
  # all three yards in one cell
  l3 <- yr_layers()
  l3$strategic_choice[l3$layer_id == "grid-transformers"] <- "partner"
  expect_equal(sp_yard_rollup(yr_cells(), l3)$cells$yard_rollup[3], "mixed_high_medium_low")
  expect_equal(l$yard_rollup[l$layer_id == "grid-scada"], "mixed_high_low")
  # the counts by technology x stage
  k <- sp_yard_rollup_counts(c)
  expect_equal(sum(k$n_cells), 6)
  expect_equal(k$mixed_medium_low[k$tech == "Solar" & k$supply_chain == "Assembly"], 1)
  expect_equal(k$n_layers_per_cell[k$tech == "Solar" & k$supply_chain == "Assembly"], 2L)
  lk <- sp_yard_layer_counts(l)
  expect_equal(nrow(lk), 8)
  expect_equal(lk$yard_small[lk$layer_id == "grid-scada"], 1)
  # a table without layers rolls up to its own yards
  r0 <- sp_yard_rollup(yr_cells(), NULL)
  expect_equal(r0$cells$yard_rollup, c("medium", "large", "large", "medium", "large", NA))
  expect_true(all(r0$cells$yard_units == 1L))
})

# ---- Layers take their own choice ---------------------------------------------------------------------------------
yr_attrs <- function() {
  a <- function(tech, stage, layer_id, ...) {
    base <- list(tech = tech, stage = stage, layer_id = layer_id, maturity = "mature", digitally_active = "no", standardization = "moderate",
                 differentiation = "moderate", commoditization = "partly", substitutability = "moderate", qualification_time = "m3to12",
                 process_knowledge = "partly", workforce_suppliers = "partly", adjacent_innovation = "partly", leap_candidate = "no",
                 successor = "", beyond_novelty = "", cost_perf_advantage = "", incumbent_vulnerable = "", scale_pathway = "")
    v <- list(...)
    base[names(v)] <- v
    as.data.frame(base, stringsAsFactors = FALSE)
  }
  rbind(
    a("Solar", "Assembly", "stage"),
    a("Solar", "Assembly", "pv-racking", standardization = "high", differentiation = "low", commoditization = "yes", substitutability = "easy",
      qualification_time = "lt3m", process_knowledge = "no", workforce_suppliers = "no", adjacent_innovation = "no"),
    a("Solar", "Assembly", "pv-nextgen", maturity = "emerging", process_knowledge = "yes", adjacent_innovation = "yes", leap_candidate = "yes",
      successor = "perovskite tandem", beyond_novelty = "partly", cost_perf_advantage = "yes", incumbent_vulnerable = "partly",
      scale_pathway = "partly"),
    a("Solar", "Assembly", "pv-firmware", digitally_active = "yes")
  )
}
yr_engine_rows <- function() {
  # the posture engine's rows for one country: a cell and three layers (the firmware layer Secure by the engine)
  data.frame(Country = "Aland", iso3 = "AAA", tech = "Solar", supply_chain = "Assembly",
             layer_id = c("stage", "pv-racking", "pv-nextgen", "pv-firmware"), layer_label = c("Solar Assembly", "Racking", "Next-gen PV", "Firmware"),
             layer_type = c("stage", "structure", "hardware", "control"),
             strategic_posture = c("deploy", "deploy", "deploy", "secure"), posture_variant = c("deploy", "deploy", "deploy", "secure_control"),
             posture_possible = c("deploy", "deploy", "deploy", "secure"), pivotal_inputs = NA_character_, posture_confidence = "high",
             posture_exclusion = NA_character_, security_rationale = c(NA, NA, NA, "operational_control"),
             delivery_mode = c("diversified_global", "diversified_global", "diversified_global", "allied"),
             maturity = c("mature", "mature", "emerging", "mature"), home_share = 0.01, trusted_world_share = 0.4,
             coercive_world_share = 0.6, coercive_top_share = 0.55, import_hhi = 0.4, imports_usd = 5e9, stringsAsFactors = FALSE)
}

test_that("layers take their own choice: own value and large-yard case, the cell's need, position and pathway; Secure kept", {
  eng <- yr_engine_rows()
  cell <- eng[eng$layer_id == "stage", setdiff(names(eng), c("layer_id", "layer_type")), drop = FALSE]
  cell <- cbind(cell, supply_chain_need = 0.9, supply_chain_need_n_families = 4L, industrial_opportunity = 0.9, cp_contender_pct = 0.3,
                cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "moderate", pathway_credible = "no",
                pathway_basis = "none_observed_fires", stage_evidence_quality_cp = "high", world_top_share = 0.8, io_maturity_class = "mature")
  # a reference of cell scores so the percentile has a scale
  others <- cell[rep(1, 6), ]
  others$iso3 <- paste0("Z", 1:6)
  others$industrial_opportunity <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  others_attrs <- do.call(rbind, lapply(others$iso3, function(i) cbind(iso3 = i, yr_attrs()[1, ])))
  attrs <- rbind(do.call(rbind, lapply("AAA", function(i) cbind(iso3 = i, yr_attrs()))), others_attrs)
  cells <- rbind(cell, others)
  cells <- sp_strategic_value(cells, attrs, yr_cfg())
  cells <- sp_large_yard_case(cells, attrs, yr_cfg())
  cells <- sp_decision_choice(cells, yr_cfg(), yr_post())
  cells$choice_reason <- sp_choice_reason(cells, yr_cfg())
  expect_equal(cells$strategic_choice[1], "partner")      # high need, no high position, no pathway
  layers <- sp_layer_choice(eng[eng$layer_id != "stage", ], cells, attrs, yr_cfg(), yr_post())
  ch <- stats::setNames(layers$strategic_choice, layers$layer_id)
  expect_equal(unname(ch["pv-firmware"]), "secure")       # the engine's own Secure, exactly as before
  expect_equal(layers$posture_variant[layers$layer_id == "pv-firmware"], "secure_control")
  expect_equal(layers$delivery_mode[layers$layer_id == "pv-firmware"], "allied")
  expect_equal(unname(ch["pv-racking"]), "watch")         # its own large-yard case
  expect_true(layers$large_yard_case[layers$layer_id == "pv-racking"])
  expect_equal(layers$siv_cap[layers$layer_id == "pv-racking"], "market_only")
  expect_match(layers$watch_reasons[layers$layer_id == "pv-racking"], "large_yard_case")
  expect_false(layers$large_yard_case[layers$layer_id == "pv-nextgen"])
  expect_equal(unname(ch["pv-nextgen"]), "partner")       # its own high value, the cell's high need and no position or pathway
  # the parent's need, position and pathway; the layer's own value
  nx <- layers[layers$layer_id == "pv-nextgen", ]
  expect_equal(nx$supply_chain_need, 0.9)
  expect_equal(nx$cp_contender_pct, 0.3)
  expect_equal(nx$siv_market, 0.9)
  expect_gt(nx$siv_option, 0.5)
  expect_true(all(!is.na(layers$choice_reason)))
  # the roll-up: Partner (stage row), Watch (racking), Partner (next-gen PV), Secure (firmware)
  r <- sp_yard_rollup(cells, layers)
  expect_equal(r$cells$yard_rollup[1], "mixed_high_medium_low")
  kids <- jsonlite::fromJSON(r$cells$yard_children[1], simplifyVector = FALSE)
  expect_setequal(vapply(kids, `[[`, "", "layer_id"), c("pv-racking", "pv-nextgen", "pv-firmware"))
  expect_true(all(vapply(kids, function(k) nzchar(k$reason), logical(1))))
})

# ---- The paper-reproduction diagnostic ----------------------------------------------------------------------------
test_that("the paper reproduction reads paper_placement beside the derived yards, with a stage-row fallback", {
  yf <- data.frame(component_id = c("pv-racking", "pv-nextgen", "grid-x", "orphan"), technology = c("Solar", "Solar", "Electric Grid", "Wind"),
                   stage = c("Midstream", "Midstream", "Midstream", "Upstream"), component = c("Racking", "Next-gen", "Grid X", "Orphan"),
                   paper_placement = c("large", "medium", "small", ""), stringsAsFactors = FALSE)
  units <- data.frame(iso3 = rep(c("USA", "JPN", "DEU"), each = 3),
                      tech = rep(c("Solar", "Solar", "Electric Grid"), 3), supply_chain = rep(c("Assembly", "Assembly", "Assembly"), 3),
                      layer_id = rep(c("pv-racking", "pv-nextgen", "stage"), 3),
                      yf_component_id = rep(c("pv-racking", "pv-nextgen", NA), 3),
                      yard = c("large", "medium", "medium", "large", "large", "medium", "large", "medium", "small"),
                      strategic_choice = "watch", choice_reason = "why", large_yard_case = c(TRUE, FALSE, FALSE),
                      large_yard_reason = "ly", strategic_industrial_value = 0.5, stringsAsFactors = FALSE)
  out <- sp_yf_reproduction(yf, units, countries = c("USA", "JPN", "DEU"))
  expect_equal(out$agree[out$component_id == "pv-racking"], "agree")
  expect_equal(out$modal_yard[out$component_id == "pv-nextgen"], "medium")
  expect_equal(out$yard_JPN[out$component_id == "pv-nextgen"], "large")
  expect_equal(out$agree[out$component_id == "pv-nextgen"], "agree")
  g <- out[out$component_id == "grid-x", ]
  expect_equal(g$unit_kind, "stage_row")                  # Midstream -> the Assembly stage row
  expect_equal(g$agree, "differ")
  expect_equal(out$agree[out$component_id == "orphan"], "no_unit")
  expect_true(all(c("paper_placement", "modal_yard", "yard_USA", "reason", "large_yard_reason") %in% names(out)))
})
