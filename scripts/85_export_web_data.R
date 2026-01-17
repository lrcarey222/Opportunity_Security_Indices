# Export web-facing JSON data for the site.
if (!exists("repo_root")) {
  repo_root <- getOption("opportunity_security.repo_root")
}

if (is.null(repo_root) || !nzchar(repo_root)) {
  repo_root <- resolve_repo_root()
}

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to load config files.")
  }
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  config <- yaml::read_yaml(config_path)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required to export web data.")
}

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "outputs", "web_data_helpers.R"))

site_public_dir <- file.path(repo_root, "site", "public")
site_data_dir <- file.path(site_public_dir, "data")
site_geo_dir <- file.path(site_public_dir, "geo")

if (!dir.exists(site_data_dir)) {
  dir.create(site_data_dir, recursive = TRUE)
}

load_latest_country_info <- function(repo_root, raw_data_dir) {
  raw_base_dir <- file.path(repo_root, raw_data_dir)
  snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
  if (length(snapshot_dirs) == 0) {
    stop("No raw data snapshots found in: ", raw_base_dir)
  }
  snapshot_info <- file.info(snapshot_dirs)
  latest_snapshot <- snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1]
  country_path <- file.path(latest_snapshot, "wdi_country_info.csv")
  if (!file.exists(country_path)) {
    stop("WDI country info missing from snapshot: ", country_path)
  }
  standardize_country_info(read.csv(country_path))
}

country_info <- if (exists("country_info")) {
  standardize_country_info(country_info)
} else {
  load_latest_country_info(repo_root, config$raw_data_dir)
}

if (!exists("energy_security_index") || !exists("economic_opportunity_index")) {
  stop("Index tables not found; run scripts/20_build_indices.R before exporting web data.")
}
if (!exists("energy_security_category_contributions") || !exists("economic_opportunity_category_contributions")) {
  stop("Category contribution tables not found; run scripts/20_build_indices.R before exporting web data.")
}

country_lookup <- build_country_lookup(country_info)

indices_tbl <- build_indices_export(
  energy_security_index,
  economic_opportunity_index,
  country_lookup
)

category_contributions_tbl <- build_category_contributions_export(
  energy_security_category_contributions,
  economic_opportunity_category_contributions,
  country_lookup
)

scatter_tbl <- build_scatter_points_export(indices_tbl)

candidate_tables <- list(
  energy_access_tbl = if (exists("energy_access_tbl")) energy_access_tbl else NULL,
  import_dependence_tbl = if (exists("import_dependence_tbl")) import_dependence_tbl else NULL,
  reserves_tbl = if (exists("reserves_tbl")) reserves_tbl else NULL,
  foreign_dependency_tbl = if (exists("foreign_dependency_tbl")) foreign_dependency_tbl else NULL,
  critical_minerals_processing_tbl = if (exists("critical_minerals_processing_tbl")) {
    critical_minerals_processing_tbl
  } else {
    NULL
  },
  critical_minerals_production_tbl = if (exists("critical_minerals_production_tbl")) {
    critical_minerals_production_tbl
  } else {
    NULL
  },
  critical_minerals_trade_tbl = if (exists("critical_minerals_trade_tbl")) {
    critical_minerals_trade_tbl
  } else {
    NULL
  },
  energy_consumption_tbl = if (exists("energy_consumption_tbl")) energy_consumption_tbl else NULL,
  trade_concentration_tbl = if (exists("trade_concentration_tbl")) trade_concentration_tbl else NULL,
  energy_prices_tbl = if (exists("energy_prices_tbl")) energy_prices_tbl else NULL,
  export_feasibility_tbl = if (exists("export_feasibility_tbl")) export_feasibility_tbl else NULL,
  future_demand_tbl = if (exists("future_demand_tbl")) future_demand_tbl else NULL,
  market_share_manufacturing_tbl = if (exists("market_share_manufacturing_tbl")) {
    market_share_manufacturing_tbl
  } else {
    NULL
  },
  cost_competitiveness_tbl = if (exists("cost_competitiveness_tbl")) cost_competitiveness_tbl else NULL,
  production_depth_momentum_tbl = if (exists("production_depth_momentum_tbl")) {
    production_depth_momentum_tbl
  } else {
    NULL
  },
  overcapacity_premium_tbl = if (exists("overcapacity_premium_tbl")) overcapacity_premium_tbl else NULL
)

latest_year <- extract_latest_year(candidate_tables)

jsonlite::write_json(
  compact_table(indices_tbl),
  path = file.path(site_data_dir, "indices.json"),
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  compact_table(category_contributions_tbl),
  path = file.path(site_data_dir, "category_contributions.json"),
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  compact_table(scatter_tbl),
  path = file.path(site_data_dir, "scatter_points.json"),
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  list(year = if (is.na(latest_year)) NA_integer_ else as.integer(latest_year)),
  path = file.path(site_data_dir, "latest_year.json"),
  auto_unbox = TRUE,
  na = "null"
)

source_geo_path <- file.path(site_geo_dir, "ne_110m_admin_0_countries.geojson")

if (!file.exists(source_geo_path)) {
  if (requireNamespace("rnaturalearth", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
    if (!dir.exists(site_geo_dir)) {
      dir.create(site_geo_dir, recursive = TRUE)
    }
    ne_countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
    sf::st_write(ne_countries, source_geo_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  } else {
    stop(
      "Natural Earth source GeoJSON not found and required packages are missing. ",
      "Install 'rnaturalearth' and 'sf' or place the file at: ",
      source_geo_path
    )
  }
}

geojson <- jsonlite::fromJSON(source_geo_path, simplifyVector = FALSE)
geojson <- trim_geojson(geojson, id_key = "ISO_A3", name_key = "ADMIN")
geojson <- filter_geojson_features(geojson, indices_tbl$iso3)

jsonlite::write_json(
  geojson,
  path = file.path(site_data_dir, "countries.geojson"),
  auto_unbox = TRUE,
  na = "null"
)
