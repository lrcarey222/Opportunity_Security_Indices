source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

# Review outputs for the strategic position framework (docs/strategic_position.md):
#   1. charts: for each review country in config/strategic_position.yml, the position x priority matrix,
#      the alignment heatmap and the evidence heatmap; plus the cross-country specialization heatmap. PNGs
#      go to <processed>/charts/strategic_review/ (not tracked);
#   2. the spec Part XXI validation of the canonical decision (Need -> Prize -> Position -> Choice ->
#      Policy alignment) in diagnostics/strategic_position/: decision_review.md (legacy vs new with the
#      attribution of disagreements, the Industrial Opportunity validation, the position-choice consistency
#      counts before vs after, the deployment review for nine countries, Secure by rationale and its
#      selectivity, high-policy-intensity Watch nodes), legacy_osi_corners_vs_choice_*.csv (the Country
#      fact sheets' Grow / Upgrade / Exposed / Stable corners against the choice) and choice_cards.md;
#   3. the engine's test-by-test decision cards (decision_cards.md; the Secure override and the engine's
#      own Compete reading, kept as diagnostics).

library(dplyr)

source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "strategic_position", "sp_helpers.R"))
source(file.path(repo_root, "R", "charts", "strategic_position_plots.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "strategic_position", "decision_cards.R"))
source(file.path(repo_root, "R", "strategic_position", "posture_engine.R")) # sp_policy_alignment()
source(file.path(repo_root, "R", "strategic_position", "decision_engine.R"))
source(file.path(repo_root, "R", "strategic_position", "industrial_opportunity.R"))
source(file.path(repo_root, "R", "strategic_position", "sp_diag_choice.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
sp_cfg <- yaml::read_yaml(Sys.getenv("OPSI_STRATEGIC_POSITION", file.path(repo_root, "config", "strategic_position.yml")))

processed_dir <- file.path(repo_root, config$processed_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(processed_dir, "outputs")
}
tbl_path <- file.path(outputs_dir, "strategic_position_tbl.rds")
if (!file.exists(tbl_path)) stop(tbl_path, " not found; run scripts/26_build_strategic_position.R.")
strategic_position_tbl <- readRDS(tbl_path)
if (!"strategic_choice" %in% names(strategic_position_tbl)) stop("strategic_position_tbl.rds predates the canonical choice: rerun scripts/26.")

charts_dir <- file.path(processed_dir, "charts", "strategic_review")
dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)

save_png <- function(plot, name, width = 1600, height = 1000) {
  path <- file.path(charts_dir, paste0(name, ".png"))
  ragg::agg_png(path, width = width, height = height, res = 144)
  print(plot)
  grDevices::dev.off()
  path
}
slug <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))

countries <- sp_review_country_set(strategic_position_tbl, unlist(sp_cfg$review_countries))
missing <- setdiff(unlist(sp_cfg$review_countries), countries)
if (length(missing) > 0) message("Review countries without data: ", paste(missing, collapse = ", "))

written <- character()
for (country in countries) {
  written <- c(written,
    save_png(sp_plot_country_matrix(strategic_position_tbl, country), paste0(slug(country), "_matrix")),
    save_png(sp_plot_alignment_heatmap(strategic_position_tbl, country), paste0(slug(country), "_alignment"), 2000, 1400),
    save_png(sp_plot_evidence_heatmap(strategic_position_tbl, country, "cp"), paste0(slug(country), "_evidence_cp"))
  )
}
written <- c(written,
  save_png(sp_plot_specialization_heatmap(strategic_position_tbl, countries), "specialization_heatmap", 2400, 1600),
  save_png(sp_plot_posture_map(strategic_position_tbl, countries), "posture_map", 2400, 1600),
  save_png(sp_plot_posture_mix(strategic_position_tbl, countries), "posture_mix"),
  save_png(sp_plot_evidence_heatmap(strategic_position_tbl, NULL, "cp"), "evidence_cp_all"),
  save_png(sp_plot_evidence_heatmap(strategic_position_tbl, NULL, "sn"), "evidence_sn_all"),
  save_png(sp_plot_evidence_heatmap(strategic_position_tbl, NULL, "pp"), "evidence_pp_all")
)
message("Strategic position review: ", length(written), " charts in ", charts_dir)

# Decision cards for the seven example cases (Secure, Compete, Deploy, under-prioritized Secure,
# under-supported Compete, over-prioritized Deploy, high need with low position delivered with allies).
layers_path <- file.path(outputs_dir, "strategic_posture_layers_tbl.rds")
layers_tbl <- if (file.exists(layers_path)) readRDS(layers_path) else NULL
card_cfg <- sp_cfg$decision_cards %||% list()
# The allied case may be a sensitive layer (most Secure nodes delivered with allies are control
# layers sourced from trusted vendors): layers carry their cell's Strategic Position columns, and
# their observed-vs-recommended alignment is judged on the cell's observed policy.
candidates <- strategic_position_tbl
if (!is.null(layers_tbl)) {
  cell_cols <- c("strategic_need_percentile", "policy_priority_percentile", "competitive_position_percentile",
                 "cp_within_country_pct", "stage_evidence_quality", "stage_evidence_quality_cp",
                 "competitive_policy_alignment", "nipo_priority", "pams_priority", "policy_support_priority",
                 "policy_protection_priority")
  layer_rows <- layers_tbl %>%
    dplyr::select(-dplyr::any_of(cell_cols)) %>%
    dplyr::left_join(strategic_position_tbl %>% dplyr::select("iso3", "tech", "supply_chain", dplyr::any_of(cell_cols)),
                     by = c("iso3", "tech", "supply_chain"))
  layer_rows$policy_alignment <- sp_policy_alignment(layer_rows, sp_cfg$policy_alignment)
  candidates <- dplyr::bind_rows(strategic_position_tbl %>% dplyr::mutate(layer_id = "stage"), layer_rows)
}
# The cards read the engine's full test trace (engine_trace); decision_trace is now the compact choice trace.
if ("engine_trace" %in% names(candidates)) candidates$decision_trace <- dplyr::coalesce(candidates$engine_trace, candidates$decision_trace)
cases <- sp_card_cases(candidates, unlist(sp_cfg$review_countries),
                       need_min = card_cfg$need_min %||% 0.5, position_max = card_cfg$position_max %||% 0.5,
                       layer_cases = "allied_high_need_low_position")
isos <- unique(c(strategic_position_tbl$iso3, strategic_position_tbl$coercive_top_iso3))
isos <- isos[!is.na(isos)]
card_tbl <- strategic_position_tbl
if ("engine_trace" %in% names(card_tbl)) card_tbl$decision_trace <- dplyr::coalesce(card_tbl$engine_trace, card_tbl$decision_trace)
cards_md <- sp_decision_cards_md(card_tbl, cases, layers = layers_tbl,
                                 iso3_names = stats::setNames(country_name_from_iso3(isos), isos))
cards_path <- file.path(repo_root, "diagnostics", "strategic_position", "decision_cards.md")
dir.create(dirname(cards_path), recursive = TRUE, showWarnings = FALSE)
writeLines(cards_md, cards_path, useBytes = TRUE)
message("Decision cards: ", sum(cases$case_found), " of ", nrow(cases), " cases in ", cards_path)

# ---- Spec Part XXI: validation of the canonical decision ------------------------------------------------
diag_dir <- file.path(repo_root, "diagnostics", "strategic_position")
wcsv <- function(x, f) utils::write.csv(x, file.path(diag_dir, f), row.names = FALSE, na = "")
tbl <- strategic_position_tbl
dec_cfg <- sp_cfg$decision_framework
th <- sp_choice_thresholds(dec_cfg)

# The legacy OSI corners, as the Country fact sheets ship them (dashboard/public/data/country/facts-*.json:
# cells[].legacy_corner, or cells[].quadrant before the 2026-10-29 rename).
facts_dir <- file.path(repo_root, "dashboard", "public", "data", "country")
facts <- list.files(facts_dir, pattern = "^facts-[A-Z]{3}\\.[0-9a-f]+\\.json$", full.names = TRUE)
facts <- facts[order(file.mtime(facts), decreasing = TRUE)]
facts <- facts[!duplicated(substr(basename(facts), 7, 9))]
corners <- dplyr::bind_rows(lapply(facts, function(f) {
  d <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(d) || !length(d$cells)) return(NULL)
  dplyr::bind_rows(lapply(d$cells, function(c) {
    k <- c$legacy_corner %||% c$quadrant
    if (is.null(k) || is.null(c$tech) || is.null(c$stage)) return(NULL)
    data.frame(iso3 = d$iso3, tech = c$tech, supply_chain = c$stage, legacy_corner = as.character(k), stringsAsFactors = FALSE)
  }))
}))
corner_cmp <- if (nrow(corners)) sp_diag_corner_vs_choice(tbl, corners) else NULL
if (!is.null(corner_cmp)) {
  wcsv(corner_cmp$pairs, "legacy_osi_corners_vs_choice_pairs.csv")
  wcsv(corner_cmp$transitions, "legacy_osi_corners_vs_choice_transitions.csv")
  wcsv(corner_cmp$drivers, "legacy_osi_corners_vs_choice_drivers.csv")
}
legacy_q_dist <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE] %>%
  dplyr::mutate(legacy = dplyr::case_when(.data$legacy_quadrant_posture %in% "secure" ~ "secure", !is.na(.data$legacy_quadrant_mode) ~ .data$legacy_quadrant_mode,
                                          .data$supply_chain == "Deployment" ~ "deploy (Deployment, not decided)",
                                          .data$legacy_quadrant %in% "watch" ~ "watch", TRUE ~ "insufficient_data (shown as Deploy)")) %>%
  dplyr::count(.data$legacy, name = "n_cells")
choice_dist <- sp_diag_choice_distribution(tbl, character())
choice_country <- sp_diag_choice_distribution(tbl, "Country")
qvc <- sp_diag_quadrant_vs_choice(tbl, sp_cfg$strategic_posture$decision_quadrant$need_threshold %||% 0.67,
                                  sp_cfg$strategic_posture$decision_quadrant$opportunity_threshold %||% 0.67)
qvc_drivers <- qvc %>% dplyr::filter(.data$driver != "agree") %>% dplyr::group_by(.data$driver) %>%
  dplyr::summarise(n_cells = sum(.data$n_cells), .groups = "drop") %>% dplyr::arrange(dplyr::desc(.data$n_cells))
qvc_big <- qvc %>% dplyr::filter(.data$driver != "agree", .data$legacy != .data$choice) %>% dplyr::arrange(dplyr::desc(.data$n_cells)) %>% utils::head(12)
io_val <- sp_io_validation(tbl, momentum_weight = dec_cfg$industrial_opportunity$investment_momentum$weight %||% 0.25)
cons <- sp_diag_consistency_before_after(tbl)
wcsv(cons, "consistency_before_after.csv")
sel <- sp_diag_secure_selectivity(tbl, th$need)
dep_rev <- sp_diag_deployment_review(tbl, unlist(sp_cfg$deployment_review_countries), unlist(sp_cfg$deployment_review_techs))
watch_pol <- sp_diag_watch_policy(tbl)
mvd <- sp_diag_manufacturing_vs_deployment(tbl)
wcsv(sel$by_rationale, "secure_by_rationale_stage.csv")
sens <- sp_choice_threshold_sensitivity(tbl, dec_cfg, dec_cfg$thresholds_sensitivity)
io_dist <- tbl[tbl$supply_chain != "Deployment" & !is.na(tbl$industrial_opportunity), , drop = FALSE]
dn_dist <- tbl[tbl$supply_chain == "Deployment" & !is.na(tbl$deployment_need), , drop = FALSE]
q5 <- function(v) paste(format(round(stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 2), nsmall = 2), collapse = " / ")
review_set <- unique(c(unlist(sp_cfg$review_countries), unlist(sp_cfg$deployment_review_countries), unlist(sp_cfg$choice_card_countries)))
review_rows <- choice_country[choice_country$Country %in% review_set, , drop = FALSE]
dep_show <- dep_rev[, intersect(c("Country", "tech", "deployment_need", "dn_demand_pct", "dn_policy_gap_pct", "dn_system_pct", "dn_basis",
                                  "strategic_choice", "choice_Components", "choice_Assembly", "policy_alignment", "manufacturing_vs_deployment"),
                                names(dep_rev)), drop = FALSE]
corner_md <- if (!is.null(corner_cmp)) {
  c(paste0("Legacy OSI corners (Energy Security x Economic Opportunity, Country fact sheets; ", corner_cmp$n_cells,
           " country x six-stage cells) against the choice, with the conceptual mapping Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner, Stable ~ Watch:"), "",
    sp_md_table(corner_cmp$pairs[, c("legacy_corner", "maps_to", "n_cells", "n_agree", "agreement", paste0("to_", SP_CHOICES))]), "",
    "Why the corners and the choice disagree:", "", sp_md_table(corner_cmp$drivers), "")
} else {
  c("(No Country fact sheets found: the OSI corner comparison was skipped.)", "")
}
dep_line <- if (nrow(dn_dist)) {
  paste0("deployment_need over Deployment cells (min / q1 / median / q3 / max): ", q5(dn_dist$deployment_need), "; Accelerate ",
         sum(tbl$strategic_choice %in% "accelerate"), ", Watch ", sum(tbl$strategic_choice %in% "watch" & tbl$supply_chain == "Deployment"),
         ", not assessed ", sum(tbl$strategic_choice %in% "not_assessed" & tbl$supply_chain == "Deployment"), ".")
} else {
  "deployment_need is not available (data/processed/deployment_need_tbl.rds, scripts/95): every Deployment & Use cell is not assessed."
}
varying <- !is.na(io_val$disguise$prize_sd_within_node) & io_val$disguise$prize_sd_within_node > 0
md <- c(
  "# Decision framework review (spec Part XXI)", "",
  paste0("Generated by scripts/27_review_strategic_position.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), " from strategic_position_tbl.rds. ",
         "Thresholds: need ", th$need, ", prize ", th$prize, ", Competitive Position top quarter of contenders (", th$cp, ") with the capability floor, ",
         "deployment need ", th$deployment, ", borderline band ", th$band, "."), "",
  "## Distribution of the choices", "", sp_md_table(choice_dist), "",
  "By review country:", "", sp_md_table(review_rows[, c("Country", "n_cells", SP_CHOICES)]), "",
  "Threshold sensitivity (cells per choice when one threshold moves; the base row is the configured thresholds):", "",
  sp_md_table(sens), "",
  "## 1. Legacy vs new", "",
  "Legacy need x EO quadrant (the posture until 2026-10-28), all classified cells:", "", sp_md_table(legacy_q_dist), "",
  paste0("What explains the cells whose choice changed (first that applies: Secure override, Deployment need, not assessed, ",
         "the cleaned prize, the cleaned need, else Competitive Position and the pathway):"), "",
  sp_md_table(qvc_drivers), "", "Largest transitions:", "", sp_md_table(qvc_big), "",
  corner_md,
  "## 2. Industrial Opportunity validation", "",
  paste0("industrial_opportunity over producing cells (min / q1 / median / q3 / max): ", q5(io_dist$industrial_opportunity),
         "; cells at or above the prize threshold: ", sum(io_dist$industrial_opportunity >= th$prize), " of ", nrow(io_dist), "."), "",
  "Correlations (Spearman; within-node values are NA where the prize is constant within the node, i.e. no national home market):", "",
  sp_md_table(io_val$correlations, 3), "",
  "Component weights:", "", sp_md_table(io_val$weights, 3), "",
  "Largest movers from the legacy EO opportunity percentile:", "",
  sp_md_table(utils::head(io_val$movers[, intersect(c("Country", "tech", "supply_chain", "legacy_eo_opportunity_pct", "industrial_opportunity", "change",
                                                      "competitive_position_percentile"), names(io_val$movers))], 15), 3), "",
  paste0("Disguised-capability check: the prize varies within a node only through the national home market. Nodes where it varies: ",
         sum(varying), " of ", nrow(io_val$disguise), "; median within-node Spearman with Competitive Position there: ",
         if (any(varying)) format(round(stats::median(io_val$disguise$spearman_with_cp[varying], na.rm = TRUE), 3)) else "n/a", "."), "",
  "## 3. Position-choice consistency, before vs after", "", sp_md_table(cons), "",
  "## 4. Deployment review", "", dep_line, "",
  sp_md_table(dep_show), "",
  "Deployment priority vs domestic manufacturing priority, all countries (Components / Assembly choice of the same technology):", "",
  sp_md_table(mvd %>% dplyr::filter(.data$deployment_choice == "accelerate") %>% dplyr::arrange(.data$tech, dplyr::desc(.data$n_countries))), "",
  "## 5. Secure selectivity", "", sp_md_table(sel$summary, 3), "", sp_md_table(sel$by_rationale), "",
  "## 6. Watch nodes with high observed policy intensity", "",
  paste0(nrow(watch_pol), " Watch cells carry policy priority, protection or deployment localisation in the top third (watch_high_policy_intensity.csv). Review countries:"), "",
  sp_md_table(utils::head(watch_pol[watch_pol$Country %in% review_set, , drop = FALSE], 40), 2)
)
writeLines(md, file.path(diag_dir, "decision_review.md"), useBytes = TRUE)
wcsv(watch_pol, "watch_high_policy_intensity.csv")
writeLines(sp_choice_cards_md(tbl, unlist(sp_cfg$choice_card_countries), sp_cfg$node_phrases %||% list()),
           file.path(diag_dir, "choice_cards.md"), useBytes = TRUE)
message("Decision framework review: decision_review.md, choice_cards.md", if (!is.null(corner_cmp)) ", legacy_osi_corners_vs_choice_*.csv", " in ", diag_dir)
