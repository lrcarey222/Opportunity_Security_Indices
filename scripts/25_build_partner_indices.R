source(local({
  # Prefer sys.frame(1)$ofile when sourced (e.g., from run_pipeline.R).
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    # Fallback for direct Rscript execution of this script.
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
  }
  if (!nzchar(this_file)) stop("Unable to resolve script path for bootstrap.")
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
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
prepare_partner_top5_for_psi <- function(tbl) {
  tbl %>%
    dplyr::transmute(
      Country = as.character(partner),
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

partner_opportunity_country_tbl <- read_processed_tbl(
  "partner_opportunity_top5_tbl",
  processed_dir
) %>%
  prepare_partner_top5_for_psi()

partner_development_country_tbl <- read_processed_tbl(
  "partner_development_top5_tbl",
  processed_dir
) %>%
  prepare_partner_top5_for_psi()

partner_friendshore_country_tbl <- read_processed_tbl(
  "partner_friendshore_top5_tbl",
  processed_dir
) %>%
  prepare_partner_top5_for_psi()

if (!exists("economic_opportunity_outputs")) {
  index_outputs <- read_index_outputs(config, processed_dir)
  economic_opportunity_outputs <- index_outputs$economic_opportunity_outputs
}

partnership_strength_outputs <- build_partnership_strength_index(
  theme_tables = list(
    friendshore = partner_friendshore_country_tbl,
    opportunity = partner_opportunity_country_tbl,
    development = partner_development_country_tbl
  ),
  weights = weights$psi,
  allow_partial_categories = TRUE
)

psi_tbl <- psi_composite(
  partnership_strength_outputs$partnership_strength_index,
  year = max(partner_friendshore_country_tbl$Year, na.rm = TRUE)
)
