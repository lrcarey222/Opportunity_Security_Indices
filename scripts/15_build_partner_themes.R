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

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "aggregate_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "stronger_development.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}
if (is.null(weights)) {
  stop("Weights not loaded; run scripts/00_setup.R first.")
}

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

write_processed_tbl <- function(tbl, name, processed_dir) {
  if (is.null(tbl)) {
    return(invisible(NULL))
  }
  saveRDS(tbl, file.path(processed_dir, paste0(name, ".rds")))
  invisible(tbl)
}

read_index_outputs <- function(config, processed_dir) {
  outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
  if (!nzchar(outputs_rds_path)) {
    outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
      file.path(repo_root, config$outputs_rds)
    } else {
      file.path(processed_dir, "outputs", "index_outputs.rds")
    }
  }
  if (!file.exists(outputs_rds_path)) {
    stop("Index outputs not found: ", outputs_rds_path)
  }
  readRDS(outputs_rds_path)
}

is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

raw_data_dir <- config$raw_data_dir
if (is.null(raw_data_dir)) {
  stop("Config missing raw_data_dir.")
}
sharepoint_raw_dir <- config$sharepoint_raw_dir

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))

copy_raw_file <- function(source_path, dest_path) {
  sync_raw_file(source_path, dest_path) != "missing"
}

resolve_raw_data_dir <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) {
      message("Skipping raw data lookup; directory not found: ", raw_base_dir)
      return(NULL)
    }
    stop("Raw data directory not found: ", raw_base_dir)
  }

  raw_base_dir
}

raw_data_path <- resolve_raw_data_dir(repo_root, raw_data_dir, skip_data_downloads)
if (is.null(raw_data_path)) {
  invisible(list())
  return()
}

# Settings that are not weights (trade source, CIF/FOB factor, conduit rule).
partnership_cfg_path <- file.path(repo_root, "config", "partnership.yml")
partnership_cfg <- if (file.exists(partnership_cfg_path)) yaml::read_yaml(partnership_cfg_path) else list()

# Bilateral trade: the all-reporter Comtrade pull (scripts/98_pull_comtrade_energy_dyads.R) when it
# is present, else the 22-reporter allied file (config/partnership.yml trade$source).
comtrade_all_path <- file.path(raw_data_path, "comtrade_energy_dyads_all.csv")
comtrade_allied_path <- file.path(raw_data_path, "allied_comtrade_energy_data.csv")
trade_source <- if (file.exists(comtrade_all_path) || file.exists(comtrade_allied_path)) {
  partnership_strength_resolve_trade_source(
    partnership_cfg$trade$source,
    all_reporters_exists = file.exists(comtrade_all_path),
    allied_exists = file.exists(comtrade_allied_path)
  )
} else {
  # Neither file: the missing-file check below reports it (or skips under SKIP_DATA_DOWNLOADS).
  list(source = "all_reporters", fallback = FALSE)
}
comtrade_dyads_path <- if (identical(trade_source$source, "all_reporters")) comtrade_all_path else comtrade_allied_path
if (isTRUE(trade_source$fallback)) {
  banner <- strrep("!", 78)
  warning(
    "\n", banner,
    "\nPARTNER THEMES FALL BACK TO THE 22-REPORTER COMTRADE FILE (allied_comtrade_energy_data.csv):",
    "\n", comtrade_all_path, " is missing. Dyads without a reporting importer or exporter among",
    "\nthe 22 get an imputed trade component. Run scripts/98_pull_comtrade_energy_dyads.R and rerun.",
    "\n", banner,
    call. = FALSE, immediate. = TRUE
  )
}
message("Partner themes: bilateral trade from ", basename(comtrade_dyads_path), " (", trade_source$source, ").")
trade_cif_fob_factor <- if (is.null(partnership_cfg$trade$cif_fob_factor)) {
  PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR
} else {
  as.numeric(partnership_cfg$trade$cif_fob_factor)
}
outbound_conduits <- partnership_strength_conduit_rule(
  rule = if (is.null(partnership_cfg$outbound_fdi$conduit_rule)) "conduits" else partnership_cfg$outbound_fdi$conduit_rule,
  scope = if (is.null(partnership_cfg$outbound_fdi$conduit_scope)) "both" else partnership_cfg$outbound_fdi$conduit_scope
)

subcat_path <- file.path(raw_data_path, "energy_hs6_master.csv")
fdi_path <- file.path(raw_data_path, "imf_dip.csv")
tech_ghg_path <- file.path(raw_data_path, "ipcc_ghg_intensity.csv")
cat_policy_path <- file.path(raw_data_path, "CAT_country ratings data.csv")
country_info_path <- file.path(raw_data_path, "wdi_country_info.csv")
country_gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
wb_doingbusiness_path <- file.path(raw_data_path, "wb_doingbusiness.csv")
wb_wdi_path <- file.path(raw_data_path, "wb_wdi.csv")
oecd_api_path <- file.path(raw_data_path, "oecd_crs_api.csv")

if (!is.null(sharepoint_raw_dir) && nzchar(sharepoint_raw_dir)) {
  # sync_raw_file() is a no-op when the staged copy has not moved ahead of the local one.
  copy_raw_file(file.path(sharepoint_raw_dir, "oecd_crs_api.csv"), oecd_api_path)
}

missing_files <- c(
  comtrade_dyads_path,
  subcat_path,
  fdi_path,
  tech_ghg_path,
  cat_policy_path,
  country_info_path,
  country_gdp_path,
  wb_doingbusiness_path,
  wb_wdi_path,
  oecd_api_path
)
missing_files <- missing_files[!file.exists(missing_files)]

if (length(missing_files) > 0) {
  if (skip_data_downloads) {
    message("Skipping partnership themes; missing file(s): ", paste(missing_files, collapse = ", "))
    invisible(list())
    return()
  }
  expected_list <- paste0("- ", missing_files)
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

if (!exists("economic_opportunity_outputs") || !exists("energy_security_outputs")) {
  index_outputs <- read_index_outputs(config, processed_dir)
  economic_opportunity_outputs <- index_outputs$economic_opportunity_outputs
  energy_security_outputs <- index_outputs$energy_security_outputs
}

# imf_dip.csv is ~1.9 GB, mostly repeated metadata text; read only the columns the outbound
# builder uses (read.csv of the whole file needs several GB).
read_imf_dip <- function(path) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    return(read.csv(path))
  }
  header <- names(data.table::fread(path, nrows = 0))
  keep <- c(
    intersect(c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"), header),
    grep("^X?[0-9]{4}$", header, value = TRUE)
  )
  as.data.frame(data.table::fread(path, select = keep, showProgress = FALSE))
}

# The all-reporter file runs to millions of rows and ~50 columns; read only those the trade
# builder uses (cmd_code as text, so no HS6 code loses a leading zero).
read_comtrade_dyads <- function(path) {
  wanted <- c("reporter_iso", "partner_iso", "period", "ref_year", "flow_direction", "flow_code", "cmd_code", "primary_value")
  if (!requireNamespace("data.table", quietly = TRUE)) {
    return(read.csv(path, colClasses = c(cmd_code = "character")))
  }
  header <- names(data.table::fread(path, nrows = 0))
  keep <- intersect(wanted, header)
  as.data.frame(data.table::fread(
    path,
    select = keep,
    colClasses = list(character = intersect(c("reporter_iso", "partner_iso", "flow_direction", "cmd_code"), keep)),
    na.strings = c("", "NA"),
    showProgress = FALSE
  ))
}

comtrade_dyads <- read_comtrade_dyads(comtrade_dyads_path)
subcat <- read.csv(subcat_path)
fdi_raw <- read_imf_dip(fdi_path)
tech_ghg_raw <- read.csv(tech_ghg_path)
cat_raw <- read.csv(cat_policy_path)
country_info <- read.csv(country_info_path)
country_gdp <- read.csv(country_gdp_path)
wb_doingbusiness <- read.csv(wb_doingbusiness_path)
wb_wdi <- read.csv(wb_wdi_path)
oecd_api_raw <- read.csv(oecd_api_path)

country_info <- standardize_country_info(country_info)

econ_opp_index <- economic_opportunity_outputs$index
energy_security_index <- energy_security_outputs$index

# UNGA voting alignment (scripts/12_build_geopolitical_alignment.R): the Friendshore geo_alignment
# component, one row per (reporter_iso, partner_iso). Economies without a UN vote are assigned there
# (territories and SARs take their sovereign's bloc; Taiwan, Palestine and Kosovo are neutral). A
# dyad the table does not cover, or with a missing value, gets NA and the component drops out of
# that dyad's mean.
geo_alignment_path <- file.path(processed_dir, "geo_alignment_tbl.rds")
if (!file.exists(geo_alignment_path)) {
  stop("Geopolitical alignment not found: ", geo_alignment_path, "; run scripts/12_build_geopolitical_alignment.R.")
}
geo_alignment_tbl <- readRDS(geo_alignment_path)

tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
# Same CAT treatment as the Policy index (scripts/10): WDI names, and EU members without
# their own rating take the EU bloc rating.
policy_tbl <- partnership_strength_clean_policy(
  cat_prepare_ratings(cat_raw, country_info = country_info),
  country_info = country_info
)

# The partner themes rank over the pillars' 11 headline technologies (scripts/20 `techs`); the
# all-tech ES/EO tables also carry minerals, Aluminium, Steel, Heat pumps and NA-tech rows.
partner_techs <- partnership_strength_headline_techs()

# Table shapes written below (see the builders' headers for the direction conventions):
# - partner_friendshore_* / partner_opportunity_*: one row per (reporter_iso, partner_iso, tech,
#   supply_chain, variable), self-dyads excluded. Friendshore: reporter = importer rating the
#   partner as a supplier. Opportunity: reporter = exporter rating the partner as a market.
#   *_country_tbl is the partner-level mean over reporters of the dyad index.
# - partner_development_*: partner-level, one row per (partner_iso, tech, supply_chain,
#   variable), non-high-income partners only; there is no reporter dimension.
# - partner_trade_coverage_tbl: by theme and (tech, stage), the dyad cells whose trade component is
#   observed trade, a recorded zero or imputed, with the trade file used.

# One reconciled exporter -> importer flow matrix for both dyad themes (importer's declaration,
# else the exporter's as a mirror at cif_fob_factor; partnership_strength_build_trade_cells()).
partner_trade_years <- 2020:2024
trade_cells <- partnership_strength_build_trade_cells(
  comtrade_dyads,
  subcat,
  years = partner_trade_years,
  cif_fob_factor = trade_cif_fob_factor
)
rm(comtrade_dyads)
invisible(gc())
message(
  "Partner themes: ", format(nrow(trade_cells), big.mark = ","), " observed (exporter, importer, tech, stage) cells; ",
  round(100 * mean(trade_cells$source_last[!is.na(trade_cells$source_last)] == "mirror"), 1),
  "% of those observed in the last year from mirror declarations."
)

friendshore_outputs <- safer_friendshore(
  comtrade_dyads = NULL,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  fdi_raw = fdi_raw,
  country_info = country_info,
  gdp_data = country_gdp,
  component_weights = weights$partnership_friendshore_components,
  years = partner_trade_years,
  techs = partner_techs,
  geo_alignment = geo_alignment_tbl,
  trade_cells = trade_cells,
  conduit_iso = outbound_conduits$iso3,
  conduit_scope = outbound_conduits$scope
)
rm(fdi_raw)
invisible(gc())

# Countries in the Friendshore universe with no geo_alignment value against any partner: their
# dyads are scored on the other components.
geo_covered <- unique(c(
  geo_alignment_tbl$reporter_iso[!is.na(geo_alignment_tbl$geo_alignment)],
  geo_alignment_tbl$partner_iso[!is.na(geo_alignment_tbl$geo_alignment)]
))
fs_countries <- unique(c(
  friendshore_outputs$country$partner_iso,
  friendshore_outputs$top5$reporter_iso
))
geo_missing <- sort(setdiff(fs_countries, geo_covered))
if (length(geo_missing) > 0) {
  message("Friendshore: no geo_alignment (component dropped) for ", paste(geo_missing, collapse = ", "))
}
rm(geo_alignment_tbl, geo_covered, fs_countries, geo_missing)

partner_friendshore_variable_tbl <- friendshore_outputs$variable
partner_friendshore_contributions_tbl <- friendshore_outputs$contributions
partner_friendshore_top5_tbl <- friendshore_outputs$top5
partner_friendshore_country_tbl <- friendshore_outputs$country
write_processed_tbl(partner_friendshore_variable_tbl, "partner_friendshore_tbl", processed_dir)
write_processed_tbl(
  partner_friendshore_country_tbl,
  "partner_friendshore_country_tbl",
  processed_dir
)
write_processed_tbl(
  partner_friendshore_contributions_tbl,
  "partner_friendshore_inputs_tbl",
  processed_dir
)
write_processed_tbl(
  partner_friendshore_variable_tbl,
  "partner_friendshore_variable_tbl",
  processed_dir
)
write_processed_tbl(
  partner_friendshore_contributions_tbl,
  "partner_friendshore_contributions_tbl",
  processed_dir
)
write_processed_tbl(
  partner_friendshore_top5_tbl,
  "partner_friendshore_top5_tbl",
  processed_dir
)

opportunity_outputs <- prosperous_opportunity(
  comtrade_dyads = NULL,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  country_info = country_info,
  component_weights = weights$partnership_opportunity_components,
  years = partner_trade_years,
  techs = partner_techs,
  trade_cells = trade_cells
)
rm(trade_cells)
invisible(gc())

# Trade coverage of both dyad themes by (tech, stage): cells scored on observed trade, on a
# recorded zero, or imputed at the cell mean (no reporting importer or exporter in the last year),
# and the cells of a (tech, stage) the HS6 crosswalk maps no code to (no_data).
partner_trade_coverage_tbl <- dplyr::bind_rows(
  friendshore_outputs$trade_coverage,
  opportunity_outputs$trade_coverage
) %>%
  dplyr::mutate(
    trade_source = trade_source$source,
    trade_file = basename(comtrade_dyads_path),
    cif_fob_factor = trade_cif_fob_factor
  )
for (theme_name in unique(partner_trade_coverage_tbl$theme)) {
  cov <- partner_trade_coverage_tbl[partner_trade_coverage_tbl$theme == theme_name, ]
  cov <- cov[cov$n_cells > cov$n_no_data, ]
  message(
    "Partner themes (", theme_name, "), ", nrow(cov), " (tech, stage) with trade data: trade component observed for ",
    round(100 * sum(cov$n_trade + cov$n_zero) / sum(cov$n_cells), 1), "% of dyad cells (",
    round(100 * sum(cov$n_trade) / sum(cov$n_cells), 1), "% with trade), imputed for ",
    round(100 * sum(cov$n_imputed) / sum(cov$n_cells), 1), "%."
  )
}
write_processed_tbl(partner_trade_coverage_tbl, "partner_trade_coverage_tbl", processed_dir)

partner_opportunity_variable_tbl <- opportunity_outputs$variable
partner_opportunity_contributions_tbl <- opportunity_outputs$contributions
partner_opportunity_top5_tbl <- opportunity_outputs$top5
partner_opportunity_country_tbl <- opportunity_outputs$country
write_processed_tbl(partner_opportunity_variable_tbl, "partner_opportunity_tbl", processed_dir)
write_processed_tbl(
  partner_opportunity_country_tbl,
  "partner_opportunity_country_tbl",
  processed_dir
)
write_processed_tbl(
  partner_opportunity_contributions_tbl,
  "partner_opportunity_inputs_tbl",
  processed_dir
)
write_processed_tbl(
  partner_opportunity_variable_tbl,
  "partner_opportunity_variable_tbl",
  processed_dir
)
write_processed_tbl(
  partner_opportunity_contributions_tbl,
  "partner_opportunity_contributions_tbl",
  processed_dir
)
write_processed_tbl(
  partner_opportunity_top5_tbl,
  "partner_opportunity_top5_tbl",
  processed_dir
)

development_outputs <- stronger_development(
  country_info = country_info,
  gdp_data = country_gdp,
  economic_opportunity_index = econ_opp_index,
  energy_security_index = energy_security_index,
  wb_wdi = wb_wdi,
  wb_doingbusiness = wb_doingbusiness,
  oecd_api_raw = oecd_api_raw,
  component_weights = weights$partnership_development_components,
  techs = partner_techs
)

# Development is partner-level, so the "country" table is the variable table itself (one row per
# partner, tech, stage and variable); the allied network joins it to nodes on partner_iso.
partner_development_variable_tbl <- development_outputs$variable
partner_development_contributions_tbl <- development_outputs$contributions
partner_development_top5_tbl <- development_outputs$top5
write_processed_tbl(partner_development_variable_tbl, "partner_development_tbl", processed_dir)
write_processed_tbl(
  partner_development_variable_tbl,
  "partner_development_country_tbl",
  processed_dir
)
write_processed_tbl(
  partner_development_variable_tbl,
  "partner_development_variable_tbl",
  processed_dir
)
write_processed_tbl(
  partner_development_contributions_tbl,
  "partner_development_inputs_tbl",
  processed_dir
)
write_processed_tbl(
  partner_development_contributions_tbl,
  "partner_development_contributions_tbl",
  processed_dir
)
write_processed_tbl(
  partner_development_top5_tbl,
  "partner_development_top5_tbl",
  processed_dir
)



# Its own path. This used OPSI_OUTPUTS_RDS / config$outputs_rds, which name the index outputs
# this script reads, so setting either overwrote index_outputs.rds; and without them it used
# `outputs_dir`, which only scripts/20 defines.
outputs_rds_path <- Sys.getenv("OPSI_PARTNERSHIP_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds_path)) {
  outputs_rds_path <- file.path(processed_dir, "outputs", "partnership_index_outputs.rds")
}

outputs_rds_dir <- dirname(outputs_rds_path)
if (!dir.exists(outputs_rds_dir)) {
  dir.create(outputs_rds_dir, recursive = TRUE)
}

saveRDS(
  list(
    partner_friendshore_variable_tbl = partner_friendshore_variable_tbl,
    partner_friendshore_contributions_tbl = partner_friendshore_contributions_tbl,
    partner_friendshore_top5_tbl = partner_friendshore_top5_tbl,
    partner_friendshore_country_tbl = partner_friendshore_country_tbl,
    partner_opportunity_contributions_tbl = partner_opportunity_contributions_tbl,
    partner_opportunity_variable_tbl = partner_opportunity_variable_tbl,
    partner_opportunity_top5_tbl = partner_opportunity_top5_tbl,
    partner_opportunity_country_tbl = partner_opportunity_country_tbl,
    partner_development_variable_tbl = partner_development_variable_tbl,
    partner_development_contributions_tbl = partner_development_contributions_tbl,
    partner_development_top5_tbl = partner_development_top5_tbl,
    partner_trade_coverage_tbl = partner_trade_coverage_tbl
  ),
  outputs_rds_path
)
