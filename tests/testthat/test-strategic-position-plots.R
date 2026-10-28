# Review charts for the strategic position framework (R/charts/strategic_position_plots.R).
#
# Runs every chart on the synthetic strategic_position_tbl (sp_plot_example_tbl(): the section 6-7
# schema with the recommended Secure / Compete / Deploy posture, six of the eight review countries
# (Australia and the United Kingdom are absent), 11 techs x 6 stages, missing values where the
# sources have none) and checks that each builds without warnings, keeps the stages in chain order,
# shows missing cells as n/a rather than zero, and stops clearly on an absent country.

sp_plots_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

sp_plots_module <- file.path(sp_plots_repo_root(), "R", "charts", "strategic_position_plots.R")
source(file.path(sp_plots_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(sp_plots_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(sp_plots_module)

sp_fixture <- function() sp_plot_example_tbl()

# Build a plot without drawing it, failing the test on any error or warning.
sp_build <- function(p) {
  b <- NULL
  expect_no_warning(b <- ggplot2::ggplot_build(p))
  b
}

sp_x_labels <- function(b, panel = 1) {
  b$layout$panel_params[[panel]]$x$get_labels()
}

# Built data of the layers drawn with `geom` (a ggproto class name), bound into one data frame.
sp_layer_data <- function(b, geom) {
  keep <- c("x", "y", "label", "shape", "size", "fill", "colour", "PANEL")
  hit <- vapply(b$plot$layers, function(l) inherits(l$geom, geom), logical(1))
  out <- do.call(rbind, lapply(b$data[hit], function(d) {
    d <- as.data.frame(d)
    for (k in setdiff(keep, names(d))) d[[k]] <- NA
    d[keep]
  }))
  if (is.null(out)) data.frame() else out
}

sp_country <- function(tbl, name) tbl[tbl$Country == name, , drop = FALSE]

# ---- module hygiene --------------------------------------------------------------------------------

test_that("the module no longer carries the old eight-posture overlay or alignment_class", {
  src <- paste(readLines(sp_plots_module, warn = FALSE), collapse = "\n")
  old <- c(
    "aligned_strategic_strength", "latent_advantage_high_need", "policy_led_strategic_build",
    "low_position_high_need", "low_priority_low_need", "\\balignment_class\\b", "SP_PLOT_POSTURES\\b"
  )
  for (pattern in old) expect_false(grepl(pattern, src), info = pattern)
})

test_that("every top-level constant in the module carries the SP_PLOT_ prefix", {
  exprs <- parse(sp_plots_module, keep.source = FALSE)
  assigned <- unlist(lapply(exprs, function(e) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) as.character(e[[2]]) else NULL
  }))
  constants <- assigned[grepl("^[A-Z][A-Z0-9_]*$", assigned)]
  expect_gt(length(constants), 5)
  expect_true(all(startsWith(constants, "SP_PLOT_")), info = paste(constants, collapse = ", "))
  # functions are sp_* and lower case
  expect_true(all(grepl("^sp_", setdiff(assigned, constants))))
})

# ---- fixture and country set ---------------------------------------------------------------------

test_that("the example table has the section 6-7 schema", {
  tbl <- sp_fixture()
  expect_equal(nrow(tbl), 6 * 11 * 6)
  expect_equal(anyDuplicated(tbl[SP_CELL_KEYS]), 0)
  expect_setequal(unique(tbl$supply_chain), SUPPLY_CHAIN_STAGES)
  expect_true(all(c(
    "Country", "iso3", "tech", "supply_chain", "legacy_stage", "competitive_position_score",
    "competitive_position_percentile", "cp_within_country_pct", "strategic_need_percentile",
    "policy_priority_percentile", "policy_support_priority", "policy_protection_priority", "policy_momentum",
    "policy_priority_sources", "evidence_specificity_cp", "evidence_specificity_sn", "evidence_specificity_pp",
    "stage_evidence_quality", "competitive_policy_alignment", "competitive_policy_basis", "priority_gap",
    "alignment_boundary", "strategic_posture", "posture_variant", "posture_confidence", "policy_alignment",
    "compete_strategy", "delivery_mode", "sensitive_layers_secure"
  ) %in% names(tbl)))
  expect_false("alignment_class" %in% names(tbl))
  expect_true(any(is.na(tbl$competitive_position_percentile)))
  expect_true(any(is.na(tbl$policy_priority_percentile)))
  expect_true(any(is.na(tbl$strategic_need_percentile)))
  expect_true(any(is.na(tbl$strategic_posture)))
})

test_that("the example table's competitive-policy alignment uses the within-country position", {
  tbl <- sp_fixture()
  expect_true(all(tbl$competitive_policy_basis == "within_country"))
  expect_true(all(tbl$competitive_policy_alignment %in% c(SP_PLOT_ALIGNMENT_CLASSES, "insufficient_data")))
  # cp_within_country_pct re-ranks the cross-country percentile within the country
  re <- stats::ave(tbl$competitive_position_percentile, tbl$Country, FUN = sp_midrank_pct)
  expect_equal(tbl$cp_within_country_pct, re)
  expect_equal(
    tbl$competitive_policy_alignment == "insufficient_data",
    is.na(tbl$cp_within_country_pct) | is.na(tbl$policy_priority_percentile)
  )
  ok <- tbl$competitive_policy_alignment != "insufficient_data"
  hx <- tbl$cp_within_country_pct[ok] >= 0.5
  hy <- tbl$policy_priority_percentile[ok] >= 0.5
  expect_equal(tbl$competitive_policy_alignment[ok] == "aligned_strength", hx & hy)
  expect_equal(tbl$competitive_policy_alignment[ok] == "low_priority", !hx & !hy)
})

test_that("the example table's posture fields are internally consistent", {
  tbl <- sp_fixture()
  post <- tbl$strategic_posture
  cls <- !is.na(post)
  expect_true(all(post[cls] %in% SP_PLOT_POSTURE_LEVELS))
  shares <- prop.table(table(factor(post[cls], levels = SP_PLOT_POSTURE_LEVELS)))
  expect_true(all(shares > 0))
  expect_equal(names(which.max(shares)), "deploy")
  expect_gt(shares[["deploy"]], 0.5)
  # Deployment cells are never Secure or Compete
  expect_true(all(post[cls & tbl$supply_chain == "Deployment"] == "deploy"))

  variants <- c(
    "secure_control", "secure_integrated", "secure_whole", "secure_unclear", "secure_input", "secure_both",
    "compete_preserve", "compete_adjacent_capability", "compete_catch_up", "compete_leap_ahead", "compete_costly",
    "deploy", "deploy_floor", "deploy_diversify"
  )
  expect_true(all(tbl$posture_variant[cls] %in% variants))
  expect_equal(sub("_.*$", "", tbl$posture_variant[cls]), post[cls])
  expect_equal(is.na(tbl$posture_variant), !cls)
  expect_equal(!is.na(tbl$compete_strategy), cls & post == "compete")
  expect_true(all(tbl$compete_strategy[!is.na(tbl$compete_strategy)] %in% c("preserve", "adjacent_capability", "catch_up", "leap_ahead")))
  expect_true(all(tbl$posture_confidence[cls] %in% c("low", "moderate", "high")))
  expect_true(all(tbl$delivery_mode[cls] %in% c("domestic", "allied", "diversified_global", "mixed")))
  expect_true(all(is.na(tbl$posture_confidence[!cls]) & is.na(tbl$delivery_mode[!cls])))
  expect_true(all(is.na(tbl$sensitive_layers_secure[!cls])))
  expect_true(any(!is.na(tbl$sensitive_layers_secure)))
})

test_that("the example table's policy alignment compares the posture with priority", {
  tbl <- sp_fixture()
  pa <- tbl$policy_alignment
  post <- tbl$strategic_posture
  pp <- tbl$policy_priority_percentile
  expect_equal(is.na(pa), is.na(post))
  cls <- !is.na(post)
  expect_true(all(pa[cls] %in% SP_PLOT_POLICY_ALIGNMENT$policy_alignment))
  expect_true(all(pa[cls & is.na(pp)] == "insufficient_data"))
  sc <- cls & !is.na(pp) & post %in% c("secure", "compete") & pa != "mixed"
  expect_equal(pa[sc] == "aligned", pp[sc] >= 0.5)
  expect_true(all(pa[sc] %in% c("aligned", "under_prioritized")))
  dep <- cls & !is.na(pp) & post == "deploy"
  expect_true(all(pa[dep] %in% c("aligned", "over_prioritized")))
  expect_equal(pa[dep] == "over_prioritized", pp[dep] >= 0.67 & tbl$supply_chain[dep] != "Deployment")
  # mixed only for Compete (a fence without a build); every kind of disagreement appears
  expect_true(all(post[cls & pa == "mixed"] == "compete"))
  expect_true(all(c("under_prioritized", "over_prioritized", "mixed") %in% pa))
})

test_that("the example table leaves the global random seed as it found it", {
  set.seed(7)
  before <- .Random.seed
  sp_plot_example_tbl()
  expect_identical(.Random.seed, before)
})

test_that("sp_review_country_set keeps the requested countries present, by name or iso3", {
  tbl <- sp_fixture()
  got <- sp_review_country_set(tbl, c("United States", "Australia", "CHN", "United Kingdom", "Japan"))
  expect_equal(as.vector(got), c("United States", "China", "Japan"))
  expect_equal(attr(got, "absent"), c("Australia", "United Kingdom"))
  none <- sp_review_country_set(tbl, "Narnia")
  expect_length(none, 0)
  expect_equal(attr(none, "absent"), "Narnia")
})

# ---- A. country matrix -------------------------------------------------------------------------

test_that("the country matrix plots the cells with both axes, within-country position by default", {
  tbl <- sp_fixture()
  p <- sp_plot_country_matrix(tbl, "Japan")
  expect_s3_class(p, "ggplot")
  b <- sp_build(p)

  jp <- sp_country(tbl, "Japan")
  both <- !is.na(jp$cp_within_country_pct) & !is.na(jp$policy_priority_percentile)
  points <- sp_layer_data(b, "GeomPoint")
  expect_equal(nrow(points), sum(both))
  expect_equal(sort(points$x), sort(jp$cp_within_country_pct[both]))
  # hollow (white-filled) points for the cells without a need score
  expect_equal(sum(points$fill == "white"), sum(both & is.na(jp$strategic_need_percentile)))
  expect_match(p$labels$x, "within Japan", fixed = TRUE)

  expect_match(p$labels$subtitle, SP_PLOT_PRIORITY_CAVEAT, fixed = TRUE)
  expect_match(p$labels$subtitle, "not proof of no priority", fixed = TRUE)
  expect_match(p$labels$caption, sprintf("Not plotted: %d of 66 cells", sum(!both)), fixed = TRUE)
})

test_that("the country matrix colours by recommended posture and shapes by policy alignment", {
  tbl <- sp_fixture()
  b <- sp_build(sp_plot_country_matrix(tbl, "United States"))
  us <- sp_country(tbl, "United States")
  plotted <- us[!is.na(us$cp_within_country_pct) & !is.na(us$policy_priority_percentile), ]
  scored <- plotted[!is.na(plotted$strategic_need_percentile), ]
  points <- sp_layer_data(b, "GeomPoint")
  for (p in SP_PLOT_POSTURE_LEVELS) {
    expect_equal(sum(points$fill == SP_PLOT_POSTURE_COLOURS[[p]]), sum(scored$strategic_posture %in% p), info = p)
  }
  shape_of <- stats::setNames(SP_PLOT_POLICY_ALIGNMENT$shape, SP_PLOT_POLICY_ALIGNMENT$policy_alignment)
  for (a in c("under_prioritized", "over_prioritized", "mixed")) {
    expect_equal(sum(points$shape == shape_of[[a]]), sum(plotted$policy_alignment %in% a), info = a)
  }
  # disagreeing cells with a need score carry the dark outline
  expect_equal(
    sum(points$colour == "grey10"),
    sum(scored$policy_alignment %in% c("under_prioritized", "over_prioritized", "mixed"))
  )
  # the fill legend lists the three postures (and Not classified only when present)
  breaks <- b$plot$scales$get_scales("fill")$get_limits()
  expect_equal(breaks[1:3], SP_PLOT_POSTURE_LEVELS)
  expect_equal("unclassified" %in% breaks, any(is.na(plotted$strategic_posture)))
  # grey corner names are the competitive-policy alignment quadrants
  corners <- sp_layer_data(b, "GeomText")$label
  expect_true(all(SP_PLOT_ALIGNMENT_LABELS %in% corners))
})

test_that("the within-country position falls back to re-ranking when cp_within_country_pct is absent", {
  tbl <- sp_fixture()
  with_col <- sp_layer_data(sp_build(sp_plot_country_matrix(tbl, "Germany")), "GeomPoint")
  without <- sp_layer_data(sp_build(sp_plot_country_matrix(tbl[names(tbl) != "cp_within_country_pct"], "Germany")), "GeomPoint")
  expect_equal(sort(without$x), sort(with_col$x))
})

test_that("the cross-country matrix plots the cross-country percentile and notes the table's basis", {
  tbl <- sp_fixture()
  p <- sp_plot_country_matrix(tbl, "Germany", position = "cross_country")
  b <- sp_build(p)
  de <- sp_country(tbl, "Germany")
  de <- de[!is.na(de$competitive_position_percentile) & !is.na(de$policy_priority_percentile), ]
  expect_equal(sort(sp_layer_data(b, "GeomPoint")$x), sort(de$competitive_position_percentile))
  expect_match(p$labels$x, "across countries", fixed = TRUE)
  expect_match(gsub("\n", " ", p$labels$caption), "uses the within-country position percentile", fixed = TRUE)
  expect_error(sp_plot_country_matrix(tbl, "Germany", position = "peer"), "should be one of")
})

test_that("the country matrix labels at most n_labels distinct cells as 'Tech - Stage'", {
  tbl <- sp_fixture()
  p <- sp_plot_country_matrix(tbl, "IND", n_labels = 7)
  b <- sp_build(p)
  labels <- sp_layer_data(b, "GeomTextRepel")$label
  expect_length(labels, 7)
  expect_equal(anyDuplicated(labels), 0)
  expect_true(all(grepl(paste0(" - (", paste(SUPPLY_CHAIN_STAGES, collapse = "|"), ")$"), labels)))
  expect_length(sp_layer_data(sp_build(sp_plot_country_matrix(tbl, "India", n_labels = 0)), "GeomTextRepel")$label, 0)
})

test_that("label picks rotate through priority gap, corners and need without posture columns", {
  pts <- data.frame(
    tech = paste0("T", 1:6), supply_chain = "Assembly",
    competitive_position_percentile = c(0.95, 0.05, 0.9, 0.1, 0.55, 0.45),
    policy_priority_percentile = c(0.95, 0.95, 0.1, 0.05, 0.5, 0.45),
    strategic_need_percentile = c(0.1, 0.2, 0.3, 0.4, 0.99, NA),
    priority_gap = c(0, -0.9, 0.8, 0.1, NA, 0),
    stringsAsFactors = FALSE
  )
  got <- sp_matrix_label_cells(pts, n_labels = 3)
  expect_equal(got$label_reason, c("priority_gap", "corner", "strategic_need"))
  expect_equal(got$tech, c("T2", "T1", "T5")) # |gap| 0.9; T1 nearest (1,1) first among corners; need 0.99
  all_rows <- sp_matrix_label_cells(pts, n_labels = 20)
  expect_equal(nrow(all_rows), 6)
  expect_equal(anyDuplicated(all_rows$tech), 0)
  expect_equal(nrow(sp_matrix_label_cells(pts, n_labels = 0)), 0)
})

test_that("label picks prefer under-prioritized Secure/Compete and over-prioritized Deploy cells", {
  pts <- data.frame(
    tech = paste0("T", 1:8), supply_chain = "Refining",
    competitive_position_percentile = c(0.95, 0.05, 0.9, 0.1, 0.55, 0.45, 0.3, 0.7),
    policy_priority_percentile = c(0.95, 0.95, 0.1, 0.05, 0.5, 0.2, 0.9, 0.3),
    strategic_need_percentile = c(0.1, 0.2, 0.3, 0.4, 0.99, 0.5, 0.6, 0.7),
    priority_gap = c(0, -0.9, 0.8, 0.1, 0.05, 0.25, -0.6, 0.4),
    strategic_posture = c("deploy", "deploy", "compete", "secure", "deploy", "compete", "deploy", "deploy"),
    policy_alignment = c(
      "over_prioritized", "over_prioritized", "under_prioritized", "under_prioritized",
      "aligned", "under_prioritized", "over_prioritized", "aligned"
    ),
    stringsAsFactors = FALSE
  )
  got <- sp_matrix_label_cells(pts, n_labels = 4)
  # half the labels go to mismatches: Secure under first (T4), then the highest-priority over (T1)
  expect_equal(got$label_reason[1:2], rep("policy_mismatch", 2))
  expect_equal(got$tech[1:2], c("T4", "T1"))
  expect_equal(got$label_reason[3:4], c("priority_gap", "corner"))
  # with mismatch_share = 1 every label is a mismatch while any are left
  all_mm <- sp_matrix_label_cells(pts, n_labels = 6, mismatch_share = 1)
  expect_equal(all_mm$label_reason, rep("policy_mismatch", 6))
  expect_setequal(all_mm$tech, c("T1", "T2", "T3", "T4", "T6", "T7"))
  # an aligned Deploy cell is never a mismatch
  expect_false(any(all_mm$tech %in% c("T5", "T8")))
})

test_that("the country matrix stops on an absent country, bad thresholds, stages or columns", {
  tbl <- sp_fixture()
  expect_error(sp_plot_country_matrix(tbl, "Australia"), "country 'Australia' is not in the table")
  expect_error(sp_plot_country_matrix(tbl, c("Japan", "India")), "single Country")
  expect_error(sp_plot_country_matrix(tbl, "Japan", thresholds = c(cp = 1.5, pp = 0.5)), "thresholds")
  bad <- tbl
  bad$supply_chain[bad$supply_chain == "Assembly"] <- "Midstream"
  expect_error(sp_plot_country_matrix(bad, "Japan"), "not supply-chain stages")
  expect_error(sp_plot_country_matrix(tbl[, names(tbl) != "priority_gap"], "Japan"), "priority_gap")
  expect_error(sp_plot_country_matrix(tbl[, names(tbl) != "policy_alignment"], "Japan"), "policy_alignment")
})

test_that("the country matrix still draws a country with no cell on both axes", {
  tbl <- sp_fixture()
  tbl$policy_priority_percentile[tbl$Country == "Germany"] <- NA
  p <- sp_plot_country_matrix(tbl, "Germany")
  b <- sp_build(p)
  expect_equal(nrow(sp_layer_data(b, "GeomPoint")), 0)
  expect_match(p$labels$caption, "Not plotted: 66 of 66 cells", fixed = TRUE)
})

test_that("the country matrix draws unclassified cells in grey with their own legend entry", {
  tbl <- sp_fixture()
  jp <- tbl$Country == "Japan" & !is.na(tbl$cp_within_country_pct) & !is.na(tbl$policy_priority_percentile) &
    !is.na(tbl$strategic_need_percentile)
  hit <- which(jp & !is.na(tbl$strategic_posture))[1:3]
  tbl$strategic_posture[hit] <- NA
  tbl$policy_alignment[hit] <- NA
  b <- sp_build(sp_plot_country_matrix(tbl, "Japan"))
  points <- sp_layer_data(b, "GeomPoint")
  expect_equal(sum(points$fill == SP_PLOT_UNCLASSIFIED_COLOUR), sum(jp & is.na(tbl$strategic_posture)))
  expect_gte(sum(points$fill == SP_PLOT_UNCLASSIFIED_COLOUR), 3)
  expect_true("unclassified" %in% b$plot$scales$get_scales("fill")$get_limits())
  expect_true("not_compared" %in% b$plot$scales$get_scales("shape")$get_limits())
})

# ---- B. specialization heatmap -----------------------------------------------------------------

test_that("the specialization heatmap facets countries in order with stages in chain order", {
  tbl <- sp_fixture()
  # shuffled rows must not reorder the stage axis
  shuffled <- tbl[rev(seq_len(nrow(tbl))), ]
  p <- sp_plot_specialization_heatmap(shuffled, c("India", "USA", "Japan"))
  expect_s3_class(p, "ggplot")
  b <- sp_build(p)
  expect_equal(levels(b$layout$layout$Country), c("India", "United States", "Japan"))
  expect_equal(sp_x_labels(b), SUPPLY_CHAIN_STAGES)
  expect_equal(nrow(b$data[[1]]), 3 * 11 * 6)
})

test_that("the specialization heatmap shows missing cells as n/a in grey, not as zero", {
  tbl <- sp_fixture()
  tbl <- tbl[!(tbl$Country == "China" & tbl$tech == "Wind" & tbl$supply_chain == "Recovery"), ]
  p <- sp_plot_specialization_heatmap(tbl, c("China", "Germany"), techs = c("Wind", "Nuclear", "Fusion"))
  b <- sp_build(p)
  tiles <- b$data[[1]]
  text <- b$data[[2]]
  expect_equal(nrow(tiles), 2 * 3 * 6) # Fusion is drawn as an n/a row
  na_rows <- text$label == "n/a"
  expect_true(all(tiles$fill[na_rows] == SP_PLOT_NA_FILL))
  expect_true(all(tiles$fill[!na_rows] != SP_PLOT_NA_FILL))
  expect_gte(sum(na_rows), 1 + 2 * 6) # the dropped row plus the Fusion rows
  expect_false(any(text$label[!na_rows] == "n/a"))
})

test_that("the specialization heatmap stops when a requested country is absent", {
  tbl <- sp_fixture()
  expect_error(sp_plot_specialization_heatmap(tbl, c("Japan", "Australia")), "not in the table: Australia")
  expect_error(sp_plot_specialization_heatmap(tbl, character(0)), "no countries")
  dup <- rbind(tbl, tbl[tbl$Country == "Germany", ][1, ])
  expect_error(sp_plot_specialization_heatmap(dup, "Germany"), "more than one row")
})

# ---- C. alignment heatmap ----------------------------------------------------------------------

test_that("the alignment heatmap combines four panels into one ggplot", {
  tbl <- sp_fixture()
  p <- sp_plot_alignment_heatmap(tbl, "South Korea")
  expect_s3_class(p, "ggplot")
  sp_build(p)
})

test_that("the alignment heatmap panels keep chain order and show n/a", {
  tbl <- sp_fixture()
  panels <- sp_plot_alignment_heatmap(tbl, "KOR", combine = FALSE)
  expect_named(panels, c("position", "priority", "need", "posture"))
  for (nm in names(panels)) {
    expect_s3_class(panels[[nm]], "ggplot")
    b <- sp_build(panels[[nm]])
    expect_equal(sp_x_labels(b), SUPPLY_CHAIN_STAGES, info = nm)
    expect_equal(nrow(b$data[[1]]), 11 * 6, info = nm)
  }
  kr <- sp_country(tbl, "South Korea")
  pri <- sp_build(panels$priority)$data[[2]]
  expect_equal(sum(pri$label == "n/a"), sum(is.na(kr$policy_priority_percentile)))
})

test_that("the posture panel fills by recommended posture with policy codes and layer markers", {
  tbl <- sp_fixture()
  panels <- sp_plot_alignment_heatmap(tbl, "South Korea", combine = FALSE)
  expect_match(panels$posture$labels$title, "Recommended posture", fixed = TRUE)
  post <- sp_build(panels$posture)
  tiles <- post$data[[1]]
  codes <- post$data[[2]]$label
  kr <- sp_country(tbl, "South Korea")
  expect_equal(sum(codes == "n/a"), sum(is.na(kr$strategic_posture)))
  expect_true(all(codes %in% c(SP_PLOT_POLICY_ALIGNMENT$code, "n/a")))
  expect_true(all(tiles$fill[codes == "n/a"] == SP_PLOT_NA_FILL))
  for (p in SP_PLOT_POSTURE_LEVELS) {
    expect_equal(sum(tiles$fill == SP_PLOT_POSTURE_COLOURS[[p]]), sum(kr$strategic_posture %in% p), info = p)
  }
  code_of <- stats::setNames(SP_PLOT_POLICY_ALIGNMENT$code, SP_PLOT_POLICY_ALIGNMENT$policy_alignment)
  for (a in names(code_of)) expect_equal(sum(codes == code_of[[a]]), sum(kr$policy_alignment %in% a), info = a)
  # the legend lists the three postures plus n/a, even those absent from the grid
  expect_equal(length(post$plot$scales$get_scales("fill")$get_breaks()), 4)
  # one diamond per cell with a Secure sensitive layer
  marks <- sp_layer_data(post, "GeomPoint")
  expect_equal(nrow(marks), sum(!is.na(kr$sensitive_layers_secure)))
  expect_gt(nrow(marks), 0)
  expect_true(all(marks$shape == 18))
})

test_that("the posture panel draws without sensitive_layers_secure and with no Secure layers", {
  tbl <- sp_fixture()
  p1 <- sp_plot_alignment_heatmap(tbl[names(tbl) != "sensitive_layers_secure"], "Japan", combine = FALSE)$posture
  expect_equal(nrow(sp_layer_data(sp_build(p1), "GeomPoint")), 0)
  p2 <- sp_plot_alignment_heatmap(tbl, "China", combine = FALSE)$posture # China has no Secure layers
  expect_equal(nrow(sp_layer_data(sp_build(p2), "GeomPoint")), 0)
  expect_s3_class(sp_plot_alignment_heatmap(tbl, "China"), "ggplot")
})

test_that("the alignment heatmap stops on an absent country or a missing posture column", {
  tbl <- sp_fixture()
  expect_error(sp_plot_alignment_heatmap(tbl, "Australia"), "country 'Australia' is not in the table")
  expect_error(sp_plot_alignment_heatmap(tbl[names(tbl) != "policy_alignment"], "Japan"), "policy_alignment")
})

# ---- D. evidence heatmap -----------------------------------------------------------------------

test_that("evidence bands follow the contract's quality bands", {
  b <- sp_evidence_band(c(0, 0.01, 0.33, 0.5, 0.67, 1, NA))
  expect_equal(as.character(b), c("none", "low", "medium", "medium", "high", "high", NA))
  expect_equal(levels(b), SP_PLOT_EVIDENCE_BANDS)
  expect_equal(as.character(sp_evidence_band(0.5, c(high = 0.5, medium = 0.2))), "high")
})

test_that("the evidence heatmap for one country prints the share and n/a for missing cells", {
  tbl <- sp_fixture()
  p <- sp_plot_evidence_heatmap(tbl, "India", "pp")
  expect_s3_class(p, "ggplot")
  b <- sp_build(p)
  expect_equal(sp_x_labels(b), SUPPLY_CHAIN_STAGES)
  ind <- sp_country(tbl, "India")
  text <- b$data[[2]]
  expect_equal(sum(text$label == "n/a"), sum(is.na(ind$evidence_specificity_pp)))
  expect_true(all(grepl("^[0-9]+%$", text$label[text$label != "n/a"])))
  expect_match(p$labels$title, "Revealed Policy Priority: India", fixed = TRUE)
})

test_that("the cross-country evidence heatmap shows the mean over countries with a value", {
  tbl <- sp_fixture()
  p <- sp_plot_evidence_heatmap(tbl, dimension = "sn")
  b <- sp_build(p)
  text <- b$data[[2]]
  expect_equal(nrow(text), 11 * 6)
  cell <- tbl[tbl$tech == "Solar" & tbl$supply_chain == "Refining", "evidence_specificity_sn"]
  # the grid puts the first tech at the top, so look the tile up by its labels
  tiles <- cbind(b$data[[1]], label = text$label)
  x <- match("Refining", SUPPLY_CHAIN_STAGES)
  y <- match("Solar", rev(sort(unique(tbl$tech))))
  expect_equal(tiles$label[tiles$x == x & tiles$y == y], sprintf("%.0f%%", 100 * mean(cell, na.rm = TRUE)))
  # Deployment need evidence is 0 everywhere in the fixture: "none", not n/a
  dep <- tiles[tiles$x == match("Deployment", SUPPLY_CHAIN_STAGES), ]
  expect_true(all(dep$label == "0%"))
  expect_error(sp_plot_evidence_heatmap(tbl, dimension = "xx"), "should be one of")
  expect_error(sp_plot_evidence_heatmap(tbl, "Australia"), "country 'Australia' is not in the table")
})

# ---- E. posture map and posture mix ------------------------------------------------------------

test_that("the posture map facets countries in order with stages in chain order", {
  tbl <- sp_fixture()
  shuffled <- tbl[rev(seq_len(nrow(tbl))), ]
  p <- sp_plot_posture_map(shuffled, c("JPN", "United States", "China", "Germany"))
  expect_s3_class(p, "ggplot")
  b <- sp_build(p)
  expect_equal(levels(b$layout$layout$Country), c("Japan", "United States", "China", "Germany"))
  expect_equal(sp_x_labels(b), SUPPLY_CHAIN_STAGES)
  tiles <- b$data[[1]]
  codes <- b$data[[2]]
  expect_equal(nrow(tiles), 4 * 11 * 6)
  sub <- tbl[tbl$Country %in% c("Japan", "United States", "China", "Germany"), ]
  expect_equal(sum(codes$label == "n/a"), sum(is.na(sub$strategic_posture)))
  expect_equal(sum(codes$label == "U"), sum(sub$policy_alignment %in% "under_prioritized"))
  expect_equal(sum(codes$label == "O"), sum(sub$policy_alignment %in% "over_prioritized"))
  expect_equal(sum(tiles$fill == SP_PLOT_POSTURE_COLOURS[["secure"]]), sum(sub$strategic_posture %in% "secure"))
  expect_equal(nrow(sp_layer_data(b, "GeomPoint")), sum(!is.na(sub$sensitive_layers_secure)))
  expect_match(p$labels$caption, "Postures differ across the countries shown in", fixed = TRUE)
})

test_that("the posture map shows missing cells and techs as n/a in grey", {
  tbl <- sp_fixture()
  tbl <- tbl[!(tbl$Country == "India" & tbl$tech == "Solar" & tbl$supply_chain == "Components"), ]
  p <- sp_plot_posture_map(tbl, c("India", "South Korea"), techs = c("Solar", "Wind", "Fusion"))
  b <- sp_build(p)
  tiles <- b$data[[1]]
  codes <- b$data[[2]]$label
  expect_equal(nrow(tiles), 2 * 3 * 6)
  sub <- tbl[tbl$Country %in% c("India", "South Korea") & tbl$tech %in% c("Solar", "Wind"), ]
  expect_equal(sum(codes == "n/a"), sum(is.na(sub$strategic_posture)) + 1 + 2 * 6)
  expect_true(all(tiles$fill[codes == "n/a"] == SP_PLOT_NA_FILL))
  expect_true(all(tiles$fill[codes != "n/a"] %in% SP_PLOT_POSTURE_COLOURS))
})

test_that("the posture map stops on an absent country or missing columns", {
  tbl <- sp_fixture()
  expect_error(sp_plot_posture_map(tbl, c("Japan", "Australia")), "not in the table: Australia")
  expect_error(sp_plot_posture_map(tbl, character(0)), "no countries")
  expect_error(sp_plot_posture_map(tbl[names(tbl) != "strategic_posture"], "Japan"), "strategic_posture")
  dup <- rbind(tbl, tbl[tbl$Country == "Japan", ][1, ])
  expect_error(sp_plot_posture_map(dup, "Japan"), "more than one row")
})

test_that("the posture mix stacks each country's classified-cell shares", {
  tbl <- sp_fixture()
  countries <- c("India", "China", "United States")
  p <- sp_plot_posture_mix(tbl, countries)
  expect_s3_class(p, "ggplot")
  b <- sp_build(p)
  bars <- b$data[[1]]
  # each bar spans 0 to 100% of the country's classified cells
  ends <- tapply(bars$xmax, bars$y, max)
  expect_equal(as.vector(ends), rep(1, 3), tolerance = 1e-9)
  expect_true(all(as.vector(tapply(bars$xmin, bars$y, min)) == 0))
  # first country on top
  ylabs <- b$layout$panel_params[[1]]$y$get_labels()
  expect_match(ylabs[length(ylabs)], "^India\n", perl = TRUE)
  ind <- sp_country(tbl, "India")
  cls <- ind$strategic_posture[!is.na(ind$strategic_posture)]
  expect_match(ylabs[length(ylabs)], sprintf("%d classified cells", length(cls)), fixed = TRUE)
  # secure is anchored at zero and its width is India's secure share
  top <- bars[bars$y == length(ylabs), ]
  sec <- top[top$fill == SP_PLOT_POSTURE_COLOURS[["secure"]], ]
  expect_equal(sec$xmin, 0)
  expect_equal(sec$xmax - sec$xmin, mean(cls == "secure"))
  text <- b$data[[2]]
  expect_true(all(grepl("^[0-9]+%$", text$label)))
})

test_that("the posture mix handles a country with no classified cells and absent countries", {
  tbl <- sp_fixture()
  tbl$strategic_posture[tbl$Country == "Germany"] <- NA
  p <- sp_plot_posture_mix(tbl, c("Germany", "Japan"))
  b <- sp_build(p)
  expect_equal(length(unique(b$data[[1]]$y)), 1)
  expect_match(p$labels$caption, "Germany 66", fixed = TRUE)
  expect_true(any(grepl("^Germany\n0 classified cells$", b$layout$panel_params[[1]]$y$get_labels())))
  expect_error(sp_plot_posture_mix(tbl, c("Japan", "Australia")), "not in the table: Australia")
})
