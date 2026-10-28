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

# Review charts for the strategic position framework (docs/strategic_position.md): for each review
# country in config/strategic_position.yml, the position x priority matrix, the alignment heatmap and
# the evidence heatmap; plus the cross-country specialization heatmap. PNGs go to
# <processed>/charts/strategic_review/ (not tracked).

library(dplyr)

source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "strategic_position", "sp_helpers.R"))
source(file.path(repo_root, "R", "charts", "strategic_position_plots.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "strategic_position", "decision_cards.R"))
source(file.path(repo_root, "R", "strategic_position", "posture_engine.R")) # sp_policy_alignment()

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
cases <- sp_card_cases(candidates, unlist(sp_cfg$review_countries),
                       need_min = card_cfg$need_min %||% 0.5, position_max = card_cfg$position_max %||% 0.5,
                       layer_cases = "allied_high_need_low_position")
isos <- unique(c(strategic_position_tbl$iso3, strategic_position_tbl$coercive_top_iso3))
isos <- isos[!is.na(isos)]
cards_md <- sp_decision_cards_md(strategic_position_tbl, cases, layers = layers_tbl,
                                 iso3_names = stats::setNames(country_name_from_iso3(isos), isos))
cards_path <- file.path(repo_root, "diagnostics", "strategic_position", "decision_cards.md")
dir.create(dirname(cards_path), recursive = TRUE, showWarnings = FALSE)
writeLines(cards_md, cards_path, useBytes = TRUE)
message("Decision cards: ", sum(cases$case_found), " of ", nrow(cases), " cases in ", cards_path)
