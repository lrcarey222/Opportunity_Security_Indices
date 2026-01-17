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

if (!exists("partner_friendshore_tbl") || !exists("partner_opportunity_tbl") || !exists("partner_development_tbl")) {
  stop("Partner theme tables not found; run scripts/15_build_partner_themes.R first.")
}

if (!exists("economic_opportunity_outputs")) {
  stop("economic_opportunity_outputs not found; run scripts/20_build_indices.R first.")
}

partner_economic_opportunity_tbl <- aggregate_economic_opportunity_index(economic_opportunity_outputs)

partnership_strength_outputs <- build_partnership_strength_index(
  theme_tables = list(
    friendshore = partner_friendshore_tbl,
    opportunity = partner_opportunity_tbl,
    development = partner_development_tbl
  ),
  weights = weights$psi,
  allow_partial_categories = TRUE
)

psi_tbl <- psi_composite(
  partnership_strength_outputs$partnership_strength_index,
  year = max(partner_friendshore_tbl$Year, na.rm = TRUE)
)
