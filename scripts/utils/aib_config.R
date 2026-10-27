# Allied Industrial Base (AIB) settings: config/aib.yml with the taxonomy overlay
# config/aib_taxonomy_<taxonomy>.yml on top (its keys replace those of aib.yml). The taxonomy is the
# `taxonomy` key of aib.yml unless `taxonomy` is given (the scripts' --taxonomy flag).
# Used by scripts/31_build_aib_trade_network.R and scripts/32_build_aib_partnerships.R.
aib_read_config <- function(repo_root, taxonomy = NULL) {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))
  tx <- if (is.null(taxonomy) || !nzchar(taxonomy)) cfg$taxonomy else taxonomy
  if (is.null(tx) || !nzchar(tx)) tx <- "master"
  path <- file.path(repo_root, "config", paste0("aib_taxonomy_", tx, ".yml"))
  if (!file.exists(path)) stop("Unknown AIB taxonomy '", tx, "': ", path, " not found.")
  overlay <- yaml::read_yaml(path)
  for (k in names(overlay)) cfg[[k]] <- overlay[[k]]
  cfg$taxonomy <- tx
  cfg
}

# The country data the investment host types need (docs/aib_methodology.md, 7), read from the OSI
# pipeline's inputs: World Bank income groups (raw wdi_country_info.csv: iso3c, income) and the
# Development Potential Index of each partner by technology and stage (processed
# partner_development_country_tbl.rds; not reporter-specific). Returns list(income = named character
# ISO3 -> income group, dev = tibble partner_iso, tech, supply_chain, value, sources); a missing file
# gives NULL for its part and a warning.
aib_read_host_type_data <- function(raw_dir, processed_dir) {
  inc_path <- file.path(raw_dir, "wdi_country_info.csv")
  dev_path <- file.path(processed_dir, "partner_development_country_tbl.rds")
  income <- NULL
  if (file.exists(inc_path)) {
    ci <- utils::read.csv(inc_path, stringsAsFactors = FALSE)
    ci <- ci[!is.na(ci$iso3c) & nzchar(ci$iso3c), , drop = FALSE]
    income <- stats::setNames(as.character(ci$income), ci$iso3c)
  } else {
    warning("No income groups (", inc_path, "): no specialty or development hosts.", call. = FALSE)
  }
  dev <- NULL
  if (file.exists(dev_path)) {
    d <- readRDS(dev_path)
    d <- d[d$variable == "Development Potential Index" & !is.na(d$value), c("partner_iso", "tech", "supply_chain", "value")]
    dev <- dplyr::distinct(tibble::as_tibble(d))
  } else {
    warning("No Development Potential Index (", dev_path, "): development hosts ranked by capability only.", call. = FALSE)
  }
  list(income = income, dev = dev, sources = c(income = inc_path, dev = dev_path))
}

# Each economy's UNGA voting bloc for the investment-host alignment screen (docs/aib_methodology.md, 7;
# R/aib/aib_host_screen.R): processed geo_alignment_tbl.rds (scripts/12; U.S.-leaning, Nonaligned or
# China-leaning on the US-China axis of the Bailey-Strezhnev-Voeten ideal points). Returns list(bloc =
# named character ISO3 -> bloc, source); a missing file gives bloc NULL (no economy screened) and a warning.
aib_read_host_bloc <- function(processed_dir) {
  path <- file.path(processed_dir, "geo_alignment_tbl.rds")
  if (!file.exists(path)) {
    warning("No UNGA blocs (", path, "): the host alignment screen keeps every partner.", call. = FALSE)
    return(list(bloc = NULL, source = path))
  }
  g <- readRDS(path)
  b <- dplyr::distinct(tibble::tibble(iso3 = c(g$reporter_iso, g$partner_iso), bloc = c(as.character(g$bloc_reporter), as.character(g$bloc_partner))))
  b <- b[!is.na(b$iso3) & !is.na(b$bloc) & !duplicated(b$iso3), , drop = FALSE]
  list(bloc = stats::setNames(b$bloc, b$iso3), source = path)
}
