# The dashboard's reference engine (R/dashboard) against the pipeline for the October 2026 method
# rules it ports: the policy basis of the Policy index (IEA PAMS required, CAT where it rates:
# build_composite_policy_index()) and the endowment screen of the allied picks
# (flag_strategic_resource_base(), select_allied_strategic_sectors(require_resource_base)). Runs
# on the synthetic bundle (dash_fixture_bundle()); no data needed.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
for (f in dash_engine_files()) source(file.path(repo_root, f))
source(file.path(repo_root, "R", "dashboard", "dash_scenarios.R"))
source(file.path(repo_root, "R", "indices", "build_policy_index.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))

screen_bundles <- list(
  all_required = function() dash_fixture_bundle(seed = 1),
  pams_required = function() dash_fixture_bundle(seed = 2, policy_required = "IEA PAMS Policy Index")
)

key3 <- function(x) paste(x$Country, x$tech, x$supply_chain, sep = "\r")

for (nm in names(screen_bundles)) {
  test_that(paste0("policy_basis and missing_required match build_composite_policy_index() (", nm, ")"), {
    b <- screen_bundles[[nm]]()
    raw <- attr(b, "fixture_raw")
    res <- dash_run(b, dash_params(bundle = b), partner = FALSE)
    w <- b$manifest$baseline_inputs$policy_weights[b$policy$components]
    req <- if (is.null(b$policy$required)) names(w) else b$policy$required
    pe <- build_composite_policy_index(raw$policy_component_tbl, techs = b$dims$techs, component_weights = w,
                                       required_components = req)
    el <- res$policy$eligibility
    k <- match(pe$eligibility$Country, el$Country)
    expect_false(anyNA(k))
    expect_identical(el$eligible[k], pe$eligibility$eligible)
    expect_identical(el$policy_basis[k], pe$eligibility$policy_basis)
    expect_identical(el$missing_required[k], pe$eligibility$missing_required)
    expect_identical(el$missing_components[k], pe$eligibility$missing_components)
    ki <- match(key3(pe$index), key3(res$policy$index))
    expect_false(anyNA(ki))
    expect_identical(res$policy$index$policy_basis[ki], pe$index$policy_basis)
    expect_lt(max(abs(res$policy$index$value[ki] - pe$index$value)), 1e-12)
    # the basis reaches the Strategic Index cells (NA where the cell has no Policy value) and the picks
    cells <- res$si$cells
    kc <- match(key3(cells), key3(res$policy$index))
    expect_identical(cells$policy_basis, res$policy$index$policy_basis[kc])
    ka <- match(paste(res$allied$Country, res$allied$sector), paste(cells$Country, cells$sector))
    expect_identical(res$allied$policy_basis, cells$policy_basis[ka])
    # the labels follow the pipeline's component order, not the alphabetical one
    expect_true(all(is.na(el$policy_basis) | startsWith(el$policy_basis, "IEA PAMS")))
  })

  test_that(paste0("the endowment screen matches flag_strategic_resource_base() and the pipeline's pick (", nm, ")"), {
    b <- screen_bundles[[nm]]()
    on <- dash_run(b, dash_params(bundle = b), partner = FALSE)
    off <- dash_run(b, dash_params(advanced = list(require_resource_base = FALSE), bundle = b), partner = FALSE)
    cells <- on$si$cells
    flagged <- flag_strategic_resource_base(cells[setdiff(names(cells), c("resource_base", "resource_evidence", "pick_exclusion"))],
                                            b$si$resource_base)
    expect_identical(cells$resource_base, flagged$resource_base)
    expect_identical(cells$resource_evidence, flagged$resource_evidence)
    expect_identical(cells$pick_exclusion, flagged$pick_exclusion)
    # the index is the same with the screen off; only the picks move
    expect_identical(off$si$cells, on$si$cells)
    # the pick's stages are the pipeline's (scripts/20 passes STRATEGIC_ALLIED_STAGES): every stage but
    # Deployment
    expect_identical(b$manifest$baseline_inputs$allied_stages, STRATEGIC_ALLIED_STAGES)
    for (screen in c(TRUE, FALSE)) {
      sel <- select_allied_strategic_sectors(flagged, b$si$allies, stages = STRATEGIC_ALLIED_STAGES, require_resource_base = screen)
      eng <- if (screen) on$allied else off$allied
      expect_setequal(paste(eng$Country, eng$sector), paste(sel$Country, sel$sector))
      expect_true(all(eng$supply_chain %in% STRATEGIC_ALLIED_STAGES))
    }
    # the screen bars Extraction cells only
    expect_true(all(cells$supply_chain[!is.na(cells$resource_base)] == "Extraction"))
    expect_false(any(on$allied$resource_base %in% FALSE))
    expect_true(any(off$allied$resource_base %in% FALSE))
    expect_true(all(off$allied$pick_exclusion[off$allied$resource_base %in% FALSE] == "no resource base"))
    expect_identical(attr(off, "notes"), "endowment screen off")
    expect_length(attr(on, "notes"), 0L)
  })
}

test_that("require_resource_base is an advanced setting with the build's default and the S17 scenario flips it", {
  b <- dash_fixture_bundle(seed = 1)
  p <- dash_params(bundle = b)
  expect_true(p$advanced$require_resource_base)
  expect_true(p$si$require_resource_base)
  expect_false(dash_params(advanced = list(require_resource_base = FALSE), bundle = b)$si$require_resource_base)
  expect_error(dash_params(advanced = list(require_resource_base = NA), bundle = b), "TRUE or FALSE")
  b_off <- b
  b_off$manifest$baseline_inputs$require_resource_base <- FALSE
  expect_false(dash_params(bundle = b_off)$si$require_resource_base)
  s17 <- Filter(function(s) s$id == "S17", dash_fixture_scenarios(b))[[1]]
  expect_identical(s17$advanced, list(require_resource_base = FALSE))
  expect_identical(Filter(function(s) s$id == "S17", dash_fixture_scenarios(b_off))[[1]]$advanced, list(require_resource_base = TRUE))
  # a bundle without the screen's table screens nothing (an unflagged table in the pipeline)
  b_none <- b
  b_none$si$resource_base <- NULL
  r <- dash_run(b_none, dash_params(bundle = b_none), partner = FALSE)
  expect_true(all(is.na(r$si$cells$resource_base)))
  expect_true(all(is.na(r$si$cells$pick_exclusion)))
})

test_that("the allied pick reads its stages from the bundle and refuses a bundle without them", {
  b <- dash_fixture_bundle(seed = 1)
  res <- dash_run(b, dash_params(bundle = b), partner = FALSE)
  # another stage list moves the picks to it (Deployment, which the pipeline never picks)
  expect_false(any(res$allied$supply_chain == "Deployment"))
  b_dep <- b
  b_dep$manifest$baseline_inputs$allied_stages <- "Deployment"
  a_dep <- dash_allied_selection(res$si, b_dep, dash_params(bundle = b))
  expect_gt(nrow(a_dep), 0L)
  expect_true(all(a_dep$supply_chain == "Deployment"))
  b_none <- b
  b_none$manifest$baseline_inputs$allied_stages <- NULL
  expect_error(dash_allied_selection(res$si, b_none), "allied_stages is missing")
  expect_error(dash_validate_bundle(b_none), "allied_stages missing")
  b_bad <- b
  b_bad$manifest$baseline_inputs$allied_stages <- c("Upstream", "Midstream")
  expect_error(dash_validate_bundle(b_bad), "allied_stages missing or naming a stage outside")
  b_sw <- b
  b_sw$manifest$baseline_inputs$stage_weights <- c(Upstream = 0.5, Midstream = 0.75, Downstream = 0.25)
  expect_error(dash_validate_bundle(b_sw), "stage_weights must name every stage")
})

test_that("the core chunk ships the screen's table and the policy component order", {
  b <- dash_fixture_bundle(seed = 1)
  base <- dash_fixture_baseline(b)
  core <- dash_web_chunks(b, base$result)$core
  expect_identical(core$si$resource_base, b$si$resource_base)
  expect_identical(core$policy$components, b$policy$components)
  back <- dash_osb_decode(dash_osb_encode(core, "core", "chunk"))$objects
  expect_identical(back$si$resource_base, dash_osb_normalize(core)$si$resource_base)
})

test_that("the shipped exclusions are the rule-based ones: the engine finds and names a degenerate sector itself", {
  b <- dash_fixture_bundle(seed = 1)
  p <- dash_params(bundle = b)
  pl <- dash_pillars(b, p)
  po <- dash_policy(b, p)
  # the fixture's Wind - Extraction has a constant pillar; scripts/20 appends such a sector to its list
  stored <- c("Wind - Components", "Wind - Extraction")
  expect_identical(dash_si_rule_excluded(b, p, pl, po, stored), "Wind - Components")
  # pre-excluded, it reads as the pipeline rule; left to the engine, it is named and the index is the same
  p_all <- p
  p_all$si$excluded_sectors <- stored
  r_all <- dash_si(b, p_all, pl, po)
  r <- dash_si(b, p, pl, po)
  why <- function(x) stats::setNames(x$excluded$reason, x$excluded$sector)[["Wind - Extraction"]]
  expect_identical(why(r_all), "excluded: not reported (pipeline rule)")
  expect_identical(why(r), "excluded: a pillar is constant across countries")
  expect_setequal(r$excluded$sector, stored)
  expect_identical(r$cells$strategic_index, r_all$cells$strategic_index)
})
