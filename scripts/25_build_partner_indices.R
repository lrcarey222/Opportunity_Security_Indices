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

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "country.R"))  # is_sovereign_economy() needs it
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "aggregate_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "psi_composite.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))  # coalition_members()

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
if (is.null(config) || is.null(weights)) {
  stop("Config or weights not loaded; run scripts/00_setup.R first.")
}

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found: ", processed_dir)
}

read_processed_tbl <- function(name, processed_dir) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Processed table not found: ", path)
  }
  readRDS(path)
}

write_processed_tbl <- function(tbl, name, processed_dir) {
  if (is.null(tbl)) {
    return(invisible(NULL))
  }
  saveRDS(tbl, file.path(processed_dir, paste0(name, ".rds")))
  invisible(tbl)
}

# PSI inputs are the top-5 summaries written by scripts/15: dyad-level for Friendshore and
# Opportunity (one row per reporter, partner and variable), partner-level for Development. The
# PSI is collapsed over technology and stage (psi_prepare_partner_top5()); see
# R/indices/build_partnership_strength_index.R for the formula and the two viewpoints.
partner_friendshore_top5_tbl <- read_processed_tbl("partner_friendshore_top5_tbl", processed_dir)
partner_opportunity_top5_tbl <- read_processed_tbl("partner_opportunity_top5_tbl", processed_dir)
partner_development_top5_tbl <- read_processed_tbl("partner_development_top5_tbl", processed_dir)

# The PSI rankability rule reads each dyad's (partner's) imputed weight share off the index rows
# of the top-5 tables; tables written before the rule have none, and every partner would then be
# unranked, so stop rather than publish no ranking.
top5_without_share <- c(
  partner_friendshore_top5_tbl = !"imputed_weight_share" %in% names(partner_friendshore_top5_tbl),
  partner_opportunity_top5_tbl = !"imputed_weight_share" %in% names(partner_opportunity_top5_tbl),
  partner_development_top5_tbl = !"imputed_weight_share" %in% names(partner_development_top5_tbl)
)
if (any(top5_without_share)) {
  stop(
    "No imputed_weight_share in ", paste(names(top5_without_share)[top5_without_share], collapse = ", "),
    "; rerun scripts/15_build_partner_themes.R before scripts/25."
  )
}

# Rankability rule (config/rankability.yml, the file scripts/20 reads; OPSI_RANKABILITY points to
# another): a partner is ranked when it is a sovereign state and at most tau of its PSI weight rests
# on imputed inputs, tau = psi$max_imputed_weight_share (the pillars' value when that is absent).
rankability_path <- Sys.getenv("OPSI_RANKABILITY", file.path(repo_root, "config", "rankability.yml"))
if (!file.exists(rankability_path)) {
  stop("Rankability config not found: ", rankability_path)
}
rankability <- yaml::read_yaml(rankability_path)
psi_tau <- if (!is.null(rankability$psi$max_imputed_weight_share)) {
  rankability$psi$max_imputed_weight_share
} else {
  rankability$max_imputed_weight_share
}
psi_tau <- resolve_max_imputed_weight_share(psi_tau)

# The PSI keys on Country; it must name one partner ISO3 and vice versa, or partners would pool.
partner_names <- dplyr::bind_rows(
  psi_prepare_partner_top5(partner_friendshore_top5_tbl),
  psi_prepare_partner_top5(partner_opportunity_top5_tbl),
  psi_prepare_partner_top5(partner_development_top5_tbl)
) %>%
  dplyr::distinct(Country, partner_iso)
if (anyDuplicated(partner_names$Country) || anyDuplicated(partner_names$partner_iso)) {
  stop("Partner names and ISO3 codes are not one-to-one in the partner top-5 tables.")
}

# The allied coalition, read as scripts/20 and scripts/30 read it (coalition_members() in
# R/indices/build_indices.R): the two-tier coalition of coalition_tbl.rds
# (scripts/13_build_coalition.R) or, until that table exists, the single-tier alignment coalition
# of geo_coalition_tbl.rds (scripts/12, member == TRUE, all tier "core"; coalition_members()
# warns). Decision of 2026-10-08: the headline PSI averages the dyad pillars over the CORE members
# (tier core; EU members individually); the full coalition (core + partner tiers) is a secondary
# view.
coalition_path <- file.path(processed_dir, "coalition_tbl.rds")
geo_coalition_path <- file.path(processed_dir, "geo_coalition_tbl.rds")
if (!file.exists(coalition_path) && !file.exists(geo_coalition_path)) {
  stop(
    "No coalition table in ", processed_dir, ": neither coalition_tbl.rds ",
    "(scripts/13_build_coalition.R) nor geo_coalition_tbl.rds ",
    "(scripts/12_build_geopolitical_alignment.R); run scripts/12 and scripts/13 before scripts/25."
  )
}
coalition <- if (file.exists(coalition_path)) {
  coalition_members(readRDS(coalition_path))
} else {
  coalition_members(NULL, geo_coalition_tbl = readRDS(geo_coalition_path))
}
coalition_iso3 <- sort(coalition$iso3[coalition$tier == "core"])
coalition_full_iso3 <- sort(coalition$iso3)
if (length(coalition_iso3) == 0) {
  stop("The coalition has no core members; the headline PSI averages over the core tier.")
}

psi_views <- build_partnership_strength_viewpoints(
  friendshore_top5 = partner_friendshore_top5_tbl,
  opportunity_top5 = partner_opportunity_top5_tbl,
  development_top5 = partner_development_top5_tbl,
  weights = weights$psi,
  coalition_iso3 = coalition_iso3,
  full_coalition_iso3 = coalition_full_iso3
)

psi_year <- max(suppressWarnings(as.integer(partner_friendshore_top5_tbl$Year)), na.rm = TRUE)

# Partner-level PSI tables, one row per partner (tech = supply_chain = "All"), in the standard
# Country schema plus partner_iso, viewpoint, the number of reporters averaged per pillar, the
# imputed weight share (overall and per category, in this view) and rankable / rank_exclusion /
# rank (psi_flag_rankable(): every partner is scored; sovereign states with an imputed weight share
# <= tau are ranked; rankings and top-N lists should use rank, not value order over all rows).
psi_view_tbl <- function(view, viewpoint, explanation) {
  n_rep <- view$category_contributions %>%
    dplyr::filter(category %in% c("friendshore", "opportunity")) %>%
    dplyr::select(Country, category, n_obs) %>%
    tidyr::pivot_wider(names_from = category, values_from = n_obs, names_prefix = "n_reporters_")
  psi_composite(view$partnership_strength_index, year = psi_year, explanation = explanation) %>%
    dplyr::left_join(partner_names, by = "Country") %>%
    dplyr::left_join(n_rep, by = "Country") %>%
    dplyr::left_join(view$imputation, by = "Country") %>%
    dplyr::mutate(viewpoint = viewpoint) %>%
    psi_flag_rankable(max_imputed_weight_share = psi_tau)
}

# Headline: the core coalition's view.
psi_tbl <- psi_view_tbl(
  psi_views$coalition,
  "coalition",
  paste0(
    "Partner-level composite of the Friendshore, Opportunity and Development indices from the ",
    "allied coalition's viewpoint: Friendshore and Opportunity are the mean over the ",
    length(coalition_iso3), " core coalition reporters (", coalition$source[1], ") of each dyad's top-5 ",
    "(tech, stage) score; weights renormalized over the categories the partner has."
  )
)
# Secondary: the full coalition's view (core and partner tiers).
psi_full_coalition_tbl <- psi_view_tbl(
  psi_views$full_coalition,
  "full_coalition",
  paste0(
    "Partner-level composite of the Friendshore, Opportunity and Development indices from the ",
    "full coalition's viewpoint: Friendshore and Opportunity are the mean over the ",
    length(coalition_full_iso3), " core and partner coalition reporters (", coalition$source[1], ") of ",
    "each dyad's top-5 (tech, stage) score; weights renormalized over the categories the partner has."
  )
)
# Secondary: every reporter's view (the PSI before October 2026).
psi_world_tbl <- psi_view_tbl(
  psi_views$world,
  "world",
  paste(
    "Partner-level composite of the Friendshore, Opportunity and Development indices from every",
    "reporter's viewpoint: Friendshore and Opportunity are the mean over all reporters of each",
    "dyad's top-5 (tech, stage) score; weights renormalized over the categories the partner has."
  )
)
psi_category_contributions_tbl <- dplyr::mutate(psi_views$coalition$category_contributions, viewpoint = "coalition")
psi_full_coalition_category_contributions_tbl <- dplyr::mutate(
  psi_views$full_coalition$category_contributions,
  viewpoint = "full_coalition"
)
psi_world_category_contributions_tbl <- dplyr::mutate(psi_views$world$category_contributions, viewpoint = "world")

psi_rank_message <- function(tbl, label) {
  sovereign_over <- grepl("^imputed weight share above", tbl$rank_exclusion)
  paste0(
    label, ": ", sum(tbl$rankable), " of ", nrow(tbl), " partners ranked; not sovereign: ",
    sum(grepl("not a sovereign economy", tbl$rank_exclusion)), "; sovereign with an imputed weight share above ",
    format(psi_tau), ": ", sum(sovereign_over), " (", paste(sort(tbl$partner_iso[sovereign_over]), collapse = ", "), ")",
    if (any(tbl$rank_exclusion %in% "imputed weight share unknown")) {
      paste0("; share unknown: ", paste(sort(tbl$partner_iso[tbl$rank_exclusion %in% "imputed weight share unknown"]), collapse = ", "))
    }
  )
}
message(
  "PSI: headline from the core coalition's viewpoint (", length(coalition_iso3), " reporters: ",
  paste(coalition_iso3, collapse = ", "), "); full-coalition view (", length(coalition_full_iso3),
  " core + partner reporters) in psi_full_coalition_tbl.rds; world view (all reporters) in psi_world_tbl.rds.\n",
  psi_rank_message(psi_tbl, "Coalition view (core)"), "\n",
  psi_rank_message(psi_full_coalition_tbl, "Full-coalition view"), "\n",
  psi_rank_message(psi_world_tbl, "World view")
)

# (Reporter, partner) PSI from the same top-5 inputs; it has no viewpoint.
psi_dyad_tbl <- build_partnership_strength_dyads(
  friendshore_top5 = partner_friendshore_top5_tbl,
  opportunity_top5 = partner_opportunity_top5_tbl,
  development_top5 = partner_development_top5_tbl,
  weights = weights$psi
) %>%
  dplyr::left_join(
    dplyr::distinct(partner_friendshore_top5_tbl, reporter_iso, reporter),
    by = "reporter_iso"
  ) %>%
  dplyr::left_join(dplyr::rename(partner_names, partner = Country), by = "partner_iso") %>%
  dplyr::mutate(
    Year = as.integer(psi_year),
    # coalition_reporter: a core member (the rows the headline averages); reporter_tier: core,
    # partner (with core, the full-coalition view) or NA (not in the coalition).
    coalition_reporter = reporter_iso %in% coalition_iso3,
    reporter_tier = coalition$tier[match(reporter_iso, coalition$iso3)],
    partner = dplyr::coalesce(partner, partner_iso)
  ) %>%
  # The same rule per dyad: rankable when the partner is sovereign and the dyad's imputed weight
  # share is at most tau; rank is the partner's place among the reporter's rankable partners.
  psi_flag_rankable(
    country_col = "partner",
    value_col = "Partnership_Strength_Index",
    max_imputed_weight_share = psi_tau
  ) %>%
  dplyr::group_by(reporter_iso) %>%
  dplyr::mutate(
    rank = dplyr::min_rank(dplyr::desc(dplyr::if_else(rankable, Partnership_Strength_Index, NA_real_)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    reporter,
    partner,
    reporter_iso,
    partner_iso,
    friendshore,
    opportunity,
    development,
    n_categories,
    Partnership_Strength_Index,
    imputed_weight_share,
    rankable,
    rank_exclusion,
    rank,
    coalition_reporter,
    reporter_tier,
    Year
  )

# Outputs:
# - psi_tbl: HEADLINE partner-level PSI, core-coalition viewpoint (read by
#   R/charts/Country_Sector_Megatable.R and scripts/paper/); psi_category_contributions_tbl: its
#   category scores (n_obs = reporters).
#   imputed_weight_share (and <category>_imputed_share): the share of the partner's PSI weight
#   resting on imputed inputs in this view; rank / rankable / rank_exclusion: the ranking over the
#   sovereign partners with imputed_weight_share <= tau (config/rankability.yml); the others are
#   scored only.
# - psi_full_coalition_tbl / psi_full_coalition_category_contributions_tbl: the same from the full
#   coalition's viewpoint (core and partner tiers).
# - psi_world_tbl / psi_world_category_contributions_tbl: the same from every reporter's viewpoint.
# - psi_dyad_tbl: one row per (reporter, partner), self-dyads excluded; coalition_reporter flags the
#   rows the headline averages (core members), reporter_tier the reporter's coalition tier;
#   imputed_weight_share / rankable / rank_exclusion as above per dyad, rank among the reporter's
#   rankable partners. None of them is technology-specific.
write_processed_tbl(psi_tbl, "psi_tbl", processed_dir)
write_processed_tbl(psi_category_contributions_tbl, "psi_category_contributions_tbl", processed_dir)
write_processed_tbl(psi_full_coalition_tbl, "psi_full_coalition_tbl", processed_dir)
write_processed_tbl(
  psi_full_coalition_category_contributions_tbl,
  "psi_full_coalition_category_contributions_tbl",
  processed_dir
)
write_processed_tbl(psi_world_tbl, "psi_world_tbl", processed_dir)
write_processed_tbl(psi_world_category_contributions_tbl, "psi_world_category_contributions_tbl", processed_dir)
write_processed_tbl(psi_dyad_tbl, "psi_dyad_tbl", processed_dir)
