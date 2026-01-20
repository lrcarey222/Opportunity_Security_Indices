# Natural Earth source data

Place Natural Earth country polygons here as `ne_110m_admin_0_countries.geojson`.
The export script (`scripts/85_export_web_data.R`) will use this source file to
produce `site/public/data/countries.geojson`. If the file is missing and the
`rnaturalearth` + `sf` packages are installed, the script will generate it
locally (no external API calls).
