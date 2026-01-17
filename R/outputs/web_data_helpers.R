# Helpers for exporting web-facing data products.

build_country_lookup <- function(country_info) {
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  country_info %>%
    dplyr::select(iso3c, country) %>%
    dplyr::mutate(
      iso3c = as.character(iso3c),
      country = as.character(country)
    ) %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c)) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)
}

attach_iso3 <- function(tbl, country_lookup, country_col = "Country") {
  require_columns(tbl, country_col, label = "index table")

  join_keys <- stats::setNames("country", country_col)

  tbl %>%
    dplyr::left_join(country_lookup, by = join_keys) %>%
    dplyr::rename(iso3 = iso3c)
}

build_indices_export <- function(energy_security_index,
                                 economic_opportunity_index,
                                 country_lookup) {
  require_columns(energy_security_index, c("Country", "tech", "supply_chain"), label = "energy_security_index")
  require_columns(economic_opportunity_index, c("Country", "tech", "supply_chain"), label = "economic_opportunity_index")

  energy_tbl <- attach_iso3(energy_security_index, country_lookup) %>%
    dplyr::rename(energy_security_index = Energy_Security_Index)

  economic_tbl <- attach_iso3(economic_opportunity_index, country_lookup) %>%
    dplyr::rename(economic_opportunity_index = Economic_Opportunity_Index)

  join_keys <- c("iso3", "tech", "supply_chain")
  if ("sub_sector" %in% names(energy_tbl) && "sub_sector" %in% names(economic_tbl)) {
    join_keys <- c(join_keys, "sub_sector")
  }

  joined <- energy_tbl %>%
    dplyr::select(dplyr::any_of(c("iso3", "Country", "tech", "supply_chain", "sub_sector", "energy_security_index"))) %>%
    dplyr::inner_join(
      economic_tbl %>%
        dplyr::select(dplyr::any_of(c("iso3", "tech", "supply_chain", "sub_sector", "economic_opportunity_index"))),
      by = join_keys
    )

  joined %>%
    dplyr::mutate(
      energy_security_index = as.numeric(energy_security_index),
      economic_opportunity_index = as.numeric(economic_opportunity_index)
    ) %>%
    dplyr::rename(country = Country)
}

build_category_contributions_export <- function(energy_security_category_contributions,
                                               economic_opportunity_category_contributions,
                                               country_lookup) {
  require_columns(
    energy_security_category_contributions,
    c(
      "Country",
      "tech",
      "supply_chain",
      "category",
      "category_score",
      "category_weight",
      "weighted_category_contribution"
    ),
    label = "energy_security_category_contributions"
  )
  require_columns(
    economic_opportunity_category_contributions,
    c(
      "Country",
      "tech",
      "supply_chain",
      "category",
      "category_score",
      "category_weight",
      "weighted_category_contribution"
    ),
    label = "economic_opportunity_category_contributions"
  )

  normalize_tbl <- function(tbl, pillar_label) {
    attach_iso3(tbl, country_lookup) %>%
      dplyr::mutate(
        pillar = pillar_label,
        category_score = as.numeric(category_score),
        category_weight = as.numeric(category_weight),
        weighted_category_contribution = as.numeric(weighted_category_contribution)
      ) %>%
      dplyr::select(
        iso3,
        Country,
        tech,
        supply_chain,
        dplyr::any_of("sub_sector"),
        pillar,
        category,
        category_score,
        category_weight,
        weighted_category_contribution
      )
  }

  dplyr::bind_rows(
    normalize_tbl(energy_security_category_contributions, "Energy security"),
    normalize_tbl(economic_opportunity_category_contributions, "Economic opportunity")
  ) %>%
    dplyr::rename(country = Country)
}

build_scatter_points_export <- function(indices_tbl) {
  require_columns(indices_tbl, c("iso3", "energy_security_index", "economic_opportunity_index"), label = "indices_tbl")

  indices_tbl %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarize(
      x = mean(economic_opportunity_index, na.rm = TRUE),
      y = mean(energy_security_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      x = as.numeric(x),
      y = as.numeric(y)
    )
}

extract_latest_year <- function(tables) {
  year_values <- purrr::map_dbl(
    tables,
    function(tbl) {
      if (!inherits(tbl, "data.frame")) {
        return(NA_real_)
      }
      year_col <- intersect(c("Year", "year"), names(tbl))
      if (length(year_col) == 0) {
        return(NA_real_)
      }
      suppressWarnings(max(as.numeric(tbl[[year_col[1]]]), na.rm = TRUE))
    }
  )

  year_values <- year_values[is.finite(year_values)]
  if (length(year_values) == 0) {
    return(NA_real_)
  }

  max(year_values, na.rm = TRUE)
}

compact_table <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  list(
    columns = names(tbl),
    rows = purrr::map(seq_len(nrow(tbl)), function(i) unname(as.list(tbl[i, , drop = TRUE])))
  )
}

trim_geojson <- function(geojson, id_key = "ISO_A3", name_key = "ADMIN") {
  if (is.null(geojson$features)) {
    stop("GeoJSON input is missing features.")
  }

  geojson$features <- purrr::map(
    geojson$features,
    function(feature) {
      props <- feature$properties
      iso3 <- props[[id_key]]
      name <- props[[name_key]]
      feature$properties <- list(iso3 = iso3, name = name)
      feature
    }
  )

  geojson
}

filter_geojson_features <- function(geojson, iso3_values) {
  if (is.null(geojson$features)) {
    stop("GeoJSON input is missing features.")
  }

  if (is.null(iso3_values) || length(iso3_values) == 0) {
    return(geojson)
  }

  iso3_values <- unique(stats::na.omit(as.character(iso3_values)))

  geojson$features <- purrr::keep(
    geojson$features,
    function(feature) {
      props <- feature$properties
      iso3 <- if (!is.null(props$iso3)) props$iso3 else props$ISO_A3
      iso3 %in% iso3_values
    }
  )

  geojson
}
