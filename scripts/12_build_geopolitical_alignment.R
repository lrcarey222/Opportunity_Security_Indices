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

# Geopolitical alignment between countries from UN General Assembly voting, for the Friendshore
# theme (scripts/15, component geo_alignment), and the allied coalition derived from it.
#
# Inputs:
# - data/raw/un_ga_ideal_points_all.csv (config alignment$series all, the default) or
#   data/raw/un_ga_ideal_points.csv (series fp, a sensitivity): the published
#   Bailey-Strezhnev-Voeten (2017) ideal points, all-votes or final-passage series, one row per
#   country x UNGA session (year), fetched from Harvard Dataverse by
#   scripts/fetchers/un_ideal_points.R during scripts/05_ingest_sources.R.
# - data/raw/wdi_gdp.csv (WDI GDP, current USD, scripts/05) for the coalition's size floor.
# - data/reference/territory_sovereign_map.csv (project-authored): economies without a UN vote;
#   territories and special administrative regions inherit their sovereign's position, Taiwan,
#   Palestine and Kosovo are neutral (p = alignment$neutral_p).
# Outputs (data/processed):
# - geo_ideal_points_tbl.rds: one row per economy: country, theta (mean ideal point over the
#   window), n_sessions, sessions_used, n_votes, d_us, d_china, s_axis (position on the US-China
#   axis), beyond_anchor, u_us / u_china (memberships of the U.S.- and China-leaning blocs), p_us
#   (US-side membership: 1, 0.5 or 0 under hard blocs), bloc (U.S.-leaning / China-leaning /
#   Nonaligned), the rule's cuts on the axis, alignment_basis (own_votes, inherited, neutral,
#   none) and imputed_from (the sovereign an inheriting economy copies), series.
# - geo_alignment_tbl.rds: one row per ordered pair (reporter_iso, partner_iso) of those
#   economies, with theta_reporter, theta_partner, ipd, s_*, p_*, bloc_*, basis_* and
#   geo_alignment = p_i p_j + (1 - p_i)(1 - p_j) (NA when either economy has no alignment), plus
#   bloc_rule, tau, window_sessions, window_years, series, source_file and dataset_version.
# - geo_coalition_tbl.rds: one row per country of the ideal-point file: iso3, country, theta,
#   s_axis, p_us, u_us, bloc, gdp_usd, gdp_year, member (U.S.-leaning bloc and GDP >= the floor)
#   and reason, plus the settings (series, bloc_rule, tau, p_threshold, gdp_floor_usd,
#   window_sessions, window_years). Only countries with their own UN votes can be members.
# Settings: config/geopolitical_distance.yml anchors, alignment (series, sessions, bloc_rule,
# bloc_quantile, tau, neutral_p) and coalition (p_threshold, gdp_floor_usd). Functions:
# R/geopolitical_distance/alignment.R.

source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "geopolitical_distance", "alignment.R"))
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}

geo_config <- yaml::read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
alignment_cfg <- geo_config$alignment
required_settings <- c("series", "sessions", "bloc_rule", "bloc_quantile", "tau", "neutral_p")
missing_settings <- setdiff(required_settings, names(alignment_cfg))
if (length(missing_settings) > 0) {
  stop("config/geopolitical_distance.yml alignment lacks: ", paste(missing_settings, collapse = ", "))
}
coalition_cfg <- geo_config$coalition
missing_settings <- setdiff(c("p_threshold", "gdp_floor_usd"), names(coalition_cfg))
if (length(missing_settings) > 0) {
  stop("config/geopolitical_distance.yml coalition lacks: ", paste(missing_settings, collapse = ", "))
}
us_iso3 <- geo_config$anchors$us_iso3
china_iso3 <- geo_config$anchors$china_iso3
if (is.null(us_iso3) || is.null(china_iso3)) {
  stop("config/geopolitical_distance.yml has no anchors$us_iso3 / anchors$china_iso3.")
}

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

skip_data_downloads <- tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
# alignment$series picks the ideal-point series: all votes (primary) or final passage.
ideal_point_files <- c(
  all = file.path(raw_data_path, "un_ga_ideal_points_all.csv"),
  fp = file.path(raw_data_path, "un_ga_ideal_points.csv")
)
series <- tolower(as.character(alignment_cfg$series))
if (length(series) != 1 || !series %in% names(ideal_point_files)) {
  stop("config/geopolitical_distance.yml alignment$series must be one of: ", paste(names(ideal_point_files), collapse = ", "))
}
ideal_points_path <- ideal_point_files[[series]]
gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
no_vote_map_path <- file.path(raw_inputs_reference_dir(repo_root), "territory_sovereign_map.csv")
if (!file.exists(no_vote_map_path)) {
  stop("Missing project reference file: ", no_vote_map_path)
}

missing_inputs <- c(ideal_points_path, gdp_path)[!file.exists(c(ideal_points_path, gdp_path))]
if (length(missing_inputs) > 0) {
  if (!skip_data_downloads) {
    stop(
      "Missing raw input(s): ", paste(missing_inputs, collapse = ", "), "\n",
      "scripts/05_ingest_sources.R fetches the UN ideal points from Harvard Dataverse (fetchers ",
      "un_ga_ideal_points_all and un_ga_ideal_points) and stages wdi_gdp.csv."
    )
  }
  message("Skipping geopolitical alignment; missing file(s): ", paste(missing_inputs, collapse = ", "))
} else {
  ideal_points_raw <- utils::read.csv(ideal_points_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  gdp_raw <- utils::read.csv(gdp_path, stringsAsFactors = FALSE, check.names = FALSE)
  no_vote_map <- geo_validate_no_vote_map(
    utils::read.csv(no_vote_map_path, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
  )
  estimate <- unique(stats::na.omit(ideal_points_raw$estimate))
  expected_estimate <- c(all = "All", fp = "FP")[[series]]
  if (!identical(estimate, expected_estimate)) {
    stop(basename(ideal_points_path), " carries estimate ", paste(estimate, collapse = "/"),
         ", not the ", expected_estimate, " series that alignment$series ", series, " asks for.")
  }

  resolved <- geo_resolve_ideal_point_iso3(ideal_points_raw)
  window <- geo_latest_sessions(resolved$data, alignment_cfg$sessions)

  geo_ideal_points_tbl <- geo_country_bloc_membership(
    resolved$data,
    session_ids = window,
    rule = alignment_cfg$bloc_rule,
    tau = as.numeric(alignment_cfg$tau),
    quantile = as.numeric(alignment_cfg$bloc_quantile),
    us_iso3 = us_iso3,
    china_iso3 = china_iso3
  )
  # Country names: the canonical name of the ISO3 (the file's spelling is not always clean).
  geo_ideal_points_tbl <- tibble::add_column(
    geo_ideal_points_tbl,
    country = country_name_from_iso3(geo_ideal_points_tbl$iso3),
    .after = "iso3"
  )

  # The coalition from the countries with their own vote, before the no-vote map is applied.
  geo_coalition_tbl <- derive_alignment_coalition(
    geo_ideal_points_tbl,
    gdp_raw,
    p_threshold = as.numeric(coalition_cfg$p_threshold),
    gdp_floor_usd = as.numeric(coalition_cfg$gdp_floor_usd)
  )

  # Economies without a UN vote: inherit the sovereign's position or stay neutral (after the
  # bloc cuts, so they do not move the quartiles). An economy with its own vote keeps it.
  map_with_own_vote <- intersect(no_vote_map$iso3, geo_ideal_points_tbl$iso3[!is.na(geo_ideal_points_tbl$theta)])
  geo_ideal_points_tbl <- geo_apply_no_vote_map(
    geo_ideal_points_tbl,
    no_vote_map,
    neutral_p = as.numeric(alignment_cfg$neutral_p)
  )
  geo_ideal_points_tbl$series <- expected_estimate
  geo_alignment_tbl <- geo_dyadic_alignment(geo_ideal_points_tbl)
  geo_alignment_tbl$series <- expected_estimate
  geo_coalition_tbl$series <- expected_estimate
  geo_coalition_tbl$bloc_rule <- alignment_cfg$bloc_rule
  geo_coalition_tbl$tau <- as.numeric(alignment_cfg$tau)
  geo_coalition_tbl$p_threshold <- as.numeric(coalition_cfg$p_threshold)
  geo_coalition_tbl$gdp_floor_usd <- as.numeric(coalition_cfg$gdp_floor_usd)
  geo_coalition_tbl$window_sessions <- geo_ideal_points_tbl$window_sessions[1]
  geo_coalition_tbl$window_years <- geo_ideal_points_tbl$window_years[1]

  provenance <- unique(ideal_points_raw[intersect(c("source_file", "dataset_version"), names(ideal_points_raw))])
  if (nrow(provenance) == 1) {
    for (tbl_name in c("geo_alignment_tbl", "geo_ideal_points_tbl", "geo_coalition_tbl")) {
      tbl <- get(tbl_name)
      tbl$source_file <- provenance$source_file
      tbl$dataset_version <- provenance$dataset_version
      assign(tbl_name, tbl)
    }
    rm(tbl, tbl_name)
  }

  saveRDS(geo_alignment_tbl, file.path(processed_dir, "geo_alignment_tbl.rds"))
  saveRDS(geo_ideal_points_tbl, file.path(processed_dir, "geo_ideal_points_tbl.rds"))
  saveRDS(geo_coalition_tbl, file.path(processed_dir, "geo_coalition_tbl.rds"))

  # Report what went into the measure (countries with their own vote; the map's economies apart).
  basis <- geo_ideal_points_tbl$alignment_basis
  pts <- geo_ideal_points_tbl[basis %in% c(GEO_ALIGNMENT_BASIS[["own"]], GEO_ALIGNMENT_BASIS[["none"]]) & is.na(geo_ideal_points_tbl$imputed_from), ]
  inherited <- geo_ideal_points_tbl[basis == GEO_ALIGNMENT_BASIS[["inherited"]], ]
  neutral <- geo_ideal_points_tbl[basis == GEO_ALIGNMENT_BASIS[["neutral"]], ]
  orphaned <- geo_ideal_points_tbl[basis == GEO_ALIGNMENT_BASIS[["none"]] & !is.na(geo_ideal_points_tbl$imputed_from), ]
  with_theta <- pts$iso3[!is.na(pts$theta)]
  without_theta <- pts$iso3[is.na(pts$theta)]
  partial <- pts[!is.na(pts$theta) & pts$n_sessions < length(window), ]
  beyond <- pts[!is.na(pts$beyond_anchor), ]
  window_years <- sort(unique(resolved$data$year[resolved$data$session %in% window]))
  bloc_counts <- table(factor(pts$bloc, levels = unname(GEO_BLOC_LABELS)))
  anchor <- function(iso) sprintf("%s %.3f", iso, pts$theta[match(iso, pts$iso3)])
  members <- geo_coalition_tbl[geo_coalition_tbl$member, ]
  message(
    "Geopolitical alignment: ", expected_estimate, " series, UNGA sessions ", paste(window, collapse = ", "),
    " (", paste(window_years, collapse = ", "), ")",
    if (nrow(provenance) == 1) paste0(", ", provenance$source_file, ", Dataverse ", provenance$dataset_version) else "",
    "\n  ", length(with_theta), " countries with an ideal point (", anchor(us_iso3), ", ", anchor(china_iso3), "); ",
    nrow(geo_alignment_tbl), " ordered pairs, ", sum(!is.na(geo_alignment_tbl$geo_alignment)), " with an alignment.",
    "\n  Bloc rule ", alignment_cfg$bloc_rule, ", tau ", alignment_cfg$tau,
    if (as.numeric(alignment_cfg$tau) == 0) " (hard blocs)" else "",
    if (identical(alignment_cfg$bloc_rule, "quartile")) {
      sprintf(" (quantile %s; cuts on the axis s >= %.3f U.S.-leaning, s <= %.3f China-leaning)",
              alignment_cfg$bloc_quantile, pts$s_us_cut[1], pts$s_china_cut[1])
    } else {
      ""
    },
    "\n  Blocs (own votes): ", paste0(names(bloc_counts), " ", as.integer(bloc_counts), collapse = ", "),
    "\n  Coalition (U.S.-leaning, GDP >= USD ", format(as.numeric(coalition_cfg$gdp_floor_usd) / 1e9), "bn): ",
    nrow(members), " members: ", paste(members$iso3, collapse = " ")
  )
  if (nrow(inherited) > 0) {
    message(
      "  No UN vote, inherits the sovereign's position (", nrow(inherited), "): ",
      paste0(inherited$iso3, "=", inherited$imputed_from, collapse = ", ")
    )
  }
  if (nrow(neutral) > 0) {
    message("  No UN vote, neutral (p = ", alignment_cfg$neutral_p, "): ", paste(neutral$iso3, collapse = ", "))
  }
  if (length(map_with_own_vote) > 0) {
    message("  In territory_sovereign_map.csv but with their own UN votes (kept): ", paste(map_with_own_vote, collapse = ", "))
  }
  if (nrow(orphaned) > 0) {
    warning(
      "No alignment for economies whose sovereign has no vote in the window: ",
      paste0(orphaned$iso3, " (", orphaned$imputed_from, ")", collapse = ", "),
      call. = FALSE
    )
  }
  small <- geo_coalition_tbl[!geo_coalition_tbl$member & geo_coalition_tbl$bloc %in% GEO_BLOC_LABELS[["us"]], ]
  if (nrow(small) > 0) {
    message("  U.S.-leaning but below the GDP floor or without GDP: ", paste(small$iso3, collapse = " "))
  }
  if (nrow(beyond) > 0) {
    message(
      "  Beyond an anchor on the axis (s clamped): ",
      paste0(beyond$iso3, " (", beyond$beyond_anchor, ")", collapse = ", ")
    )
  }
  if (length(without_theta) > 0) {
    message("  No vote in the window and not in the map (alignment NA): ", paste(sort(without_theta), collapse = ", "))
  }
  if (nrow(partial) > 0) {
    message(
      "  Averaged over fewer sessions: ",
      paste0(partial$iso3, " (", partial$sessions_used, ")", collapse = ", ")
    )
  }
  if (nrow(resolved$fallback) > 0) {
    fallback <- resolved$fallback
    message(
      "  ISO3 not from the country name: ",
      paste0(fallback$name, " [", fallback$file_iso3, ", sessions ", fallback$first_session, "-",
             fallback$last_session, "] -> ", fallback$iso3, " (", fallback$iso3_source, ")", collapse = "; ")
    )
  }
  if (nrow(resolved$unmatched) > 0) {
    unmatched <- resolved$unmatched
    message(
      "  Names with no ISO3 (dropped): ",
      paste0(unmatched$name, " [", unmatched$file_iso3, ", sessions ", unmatched$first_session, "-",
             unmatched$last_session, "]", collapse = "; ")
    )
    in_window <- unmatched$last_session >= min(window)
    if (any(in_window)) {
      warning(
        "Unmatched country names with votes in the alignment window: ",
        paste(unmatched$name[in_window], collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (nrow(resolved$disagreements) > 0) {
    warning(
      "ISO3 from the country name differs from the file's iso3c for: ",
      paste0(resolved$disagreements$name, " (", resolved$disagreements$file_iso3, " -> ",
             resolved$disagreements$iso3, ")", collapse = ", "),
      call. = FALSE
    )
  }
  rm(
    pts, with_theta, without_theta, partial, beyond, window_years, bloc_counts, anchor, members, small,
    gdp_raw, basis, inherited, neutral, orphaned, map_with_own_vote, estimate, expected_estimate
  )
}
