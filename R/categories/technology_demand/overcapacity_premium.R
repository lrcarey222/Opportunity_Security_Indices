# Overcapacity premium theme builder functions.

overcapacity_premium_build_overcapacity_clean <- function(overcapacity_raw) {
  require_columns(
    overcapacity_raw,
    c("Sector", "Component", "Overcapacity Ratio"),
    label = "overcapacity_raw"
  )

  # The sheet labels only the first component of each sector block, so filling down assigns
  # every component to the block it sits in. Component names are a fallback only where no
  # block label precedes them: "Cells" appears in both the battery and the solar block, and
  # matching on it used to move battery cells into Solar.
  overcapacity_raw %>%
    dplyr::mutate(Sector = stringr::str_squish(as.character(Sector))) %>%
    tidyr::fill(Sector, .direction = "down") %>%
    dplyr::mutate(
      Sector = dplyr::case_when(
        # BNEF labels the hydrogen block "H" + U+2082 (subscript two). The literal "H???"
        # this used to match was that label after an encoding round trip, so hydrogen never
        # reached the table. Escaped so the pattern survives whatever encoding the file is
        # sourced in.
        stringr::str_detect(Sector, "^H(\u2082|2|\\?+)$") |
          stringr::str_detect(tolower(Sector), "hydrogen|electroly") ~ "Green Hydrogen",
        Sector == "Battery" ~ "Batteries",
        is.na(Sector) & Component %in% c("Separators", "Electrolytes", "Cathodes", "Anodes") ~ "Batteries",
        is.na(Sector) & Component %in% c("Onshore nacelles", "Offshore nacelles") ~ "Wind",
        is.na(Sector) & Component %in% c("Polysilicon", "Wafers", "Modules") ~ "Solar",
        TRUE ~ Sector
      )
    )
}

overcapacity_premium_build_overcapacity_indices <- function(overcapacity_clean, year = 2025L) {
  require_columns(
    overcapacity_clean,
    c("Sector", "Overcapacity Ratio"),
    label = "overcapacity_clean"
  )

  overcapacity_clean %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarize(
      Overcapacity = mean(`Overcapacity Ratio`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      tech = Sector,
      supply_chain = "Midstream",
      Overcapacity
    ) %>%
    dplyr::mutate(`Overall Overcapacity_index` = 2^(-Overcapacity)) %>%
    tidyr::pivot_longer(
      cols = c(Overcapacity, `Overall Overcapacity_index`),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      category = "Technology Demand",
      Year = as.integer(year),
      source = "BNEF Energy Transition Supply Chains 2025",
      explanation = dplyr::if_else(
        data_type == "raw",
        "Ratio of Midstream capacity to forecast demand, mean over the sector's components",
        "2^(-ratio): more overcapacity scores lower; a global value copied to every country"
      )
    ) %>%
    dplyr::select(
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )
}

# The countries the trade theme scores. Overcapacity is a global, technology-level ratio, so
# it is copied to every one of them regardless of which cells they trade in: keying the copy
# on the trade table's (country, tech, stage) rows dropped any technology without HS6 codes,
# which is how hydrogen would have fallen out even once its label matched.
overcapacity_premium_build_country_reference <- function(trade_tidy) {
  require_columns(trade_tidy, c("Country", "tech"), label = "trade_tidy")

  trade_tidy %>%
    dplyr::filter(!is.na(Country), nzchar(Country), !is.na(tech)) %>%
    dplyr::distinct(Country)
}

overcapacity_premium <- function(overcapacity_raw, trade_tidy, year = 2025L) {
  overcapacity_clean <- overcapacity_premium_build_overcapacity_clean(overcapacity_raw)
  overcapacity_indices <- overcapacity_premium_build_overcapacity_indices(
    overcapacity_clean,
    year = year
  )
  country_reference <- overcapacity_premium_build_country_reference(trade_tidy)

  assert_unique_keys(country_reference, "Country", label = "overcapacity_country_reference")
  assert_unique_keys(
    overcapacity_indices %>% dplyr::distinct(tech, supply_chain),
    c("tech", "supply_chain"),
    label = "overcapacity_indices"
  )

  overcapacity_tidy <- country_reference %>%
    tidyr::crossing(overcapacity_indices) %>%
    dplyr::filter(!is.na(variable))

  output <- standardize_theme_table(overcapacity_tidy)
  validate_schema(output)
  output
}
