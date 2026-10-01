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
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "aggregate_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "psi_composite.R"))

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
# Opportunity (one row per reporter, partner and variable), partner-level for Development.
# Country is the partner (its ISO3 when it has no name), and the PSI is collapsed over
# technology and stage: see R/indices/build_partnership_strength_index.R for the formula.
prepare_partner_top5_for_psi <- function(tbl) {
  tbl %>%
    dplyr::transmute(
      Country = dplyr::coalesce(as.character(partner), as.character(partner_iso)),
      partner_iso = as.character(partner_iso),
      tech = "All",
      supply_chain = "All",
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      value = suppressWarnings(as.numeric(value)),
      Year = suppressWarnings(as.integer(Year)),
      source = as.character(source),
      explanation = as.character(explanation)
    )
}

partner_friendshore_top5_tbl <- read_processed_tbl("partner_friendshore_top5_tbl", processed_dir)
partner_opportunity_top5_tbl <- read_processed_tbl("partner_opportunity_top5_tbl", processed_dir)
partner_development_top5_tbl <- read_processed_tbl("partner_development_top5_tbl", processed_dir)

partner_friendshore_country_tbl <- prepare_partner_top5_for_psi(partner_friendshore_top5_tbl)
partner_opportunity_country_tbl <- prepare_partner_top5_for_psi(partner_opportunity_top5_tbl)
partner_development_country_tbl <- prepare_partner_top5_for_psi(partner_development_top5_tbl)

# The PSI keys on Country; it must name one partner ISO3 and vice versa, or partners would pool.
partner_names <- dplyr::bind_rows(
  partner_friendshore_country_tbl,
  partner_opportunity_country_tbl,
  partner_development_country_tbl
) %>%
  dplyr::distinct(Country, partner_iso)
if (anyDuplicated(partner_names$Country) || anyDuplicated(partner_names$partner_iso)) {
  stop("Partner names and ISO3 codes are not one-to-one in the partner top-5 tables.")
}

partnership_strength_outputs <- build_partnership_strength_index(
  theme_tables = list(
    friendshore = dplyr::select(partner_friendshore_country_tbl, -partner_iso),
    opportunity = dplyr::select(partner_opportunity_country_tbl, -partner_iso),
    development = dplyr::select(partner_development_country_tbl, -partner_iso)
  ),
  weights = weights$psi,
  allow_partial_categories = TRUE
)

psi_year <- max(partner_friendshore_country_tbl$Year, na.rm = TRUE)

# Partner-level PSI (one row per partner; tech = supply_chain = "All").
psi_tbl <- psi_composite(
  partnership_strength_outputs$partnership_strength_index,
  year = psi_year,
  explanation = paste(
    "Partner-level composite of the Friendshore, Opportunity and Development indices",
    "(mean over reporters of each dyad's top-5 (tech, stage) score), weights renormalized",
    "over the categories the partner has."
  )
) %>%
  dplyr::left_join(partner_names, by = "Country")

# (Reporter, partner) PSI from the same top-5 inputs.
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
  dplyr::mutate(Year = as.integer(psi_year)) %>%
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
    Year
  )

# psi_tbl: one row per partner (tech = supply_chain = "All"; read by R/charts/Country_Sector_Megatable.R).
# psi_dyad_tbl: one row per (reporter, partner), self-dyads excluded. Neither is technology-specific.
write_processed_tbl(psi_tbl, "psi_tbl", processed_dir)
write_processed_tbl(
  partnership_strength_outputs$category_contributions,
  "psi_category_contributions_tbl",
  processed_dir
)
write_processed_tbl(psi_dyad_tbl, "psi_dyad_tbl", processed_dir)

