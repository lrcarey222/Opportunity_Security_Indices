# Build partnership strength indices.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

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

partner_friendshore_country_tbl <- read_processed_tbl(
  "partner_friendshore_country_tbl",
  processed_dir
)
partner_opportunity_country_tbl <- read_processed_tbl(
  "partner_opportunity_country_tbl",
  processed_dir
)
partner_development_country_tbl <- read_processed_tbl(
  "partner_development_country_tbl",
  processed_dir
)
partner_friendshore_tbl <- read_processed_tbl("partner_friendshore_tbl", processed_dir)

if (!exists("economic_opportunity_outputs")) {
  index_outputs <- read_index_outputs(config, processed_dir)
  economic_opportunity_outputs <- index_outputs$economic_opportunity_outputs
}

partner_economic_opportunity_tbl <- aggregate_economic_opportunity_index(economic_opportunity_outputs)

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
  year = max(partner_friendshore_tbl$Year, na.rm = TRUE)
)
