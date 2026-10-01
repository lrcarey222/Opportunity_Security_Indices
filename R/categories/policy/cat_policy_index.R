# CAT policy index (country x tech x supply_chain).

# Country labels the Climate Action Tracker ratings file uses that neither
# standardize_country_names() nor a WDI name match resolves to the WDI spelling. Without
# them the rows kept a CAT-only name ("UAE") and never met the NIPO, IEA and dual-use rows
# for the same country, so the country failed the four-source policy eligibility rule.
CAT_COUNTRY_ALIASES <- c(
  "UAE" = "United Arab Emirates",
  "USA" = "United States",
  "Iran (Islamic Republic of)" = "Iran",
  "The Gambia" = "Gambia",
  "EU" = "European Union"
)

# EU-27 member states (ISO3). CAT rates the EU as one bloc and only some members (Germany)
# individually.
EU27_ISO3 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN",
  "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP",
  "SWE"
)

# Prepare the raw CAT ratings table for partnership_strength_clean_policy().
#
# 1. Recodes CAT-specific labels (CAT_COUNTRY_ALIASES) and puts the country_info spelling on
#    every row that matches it by standardized name.
# 2. Gives each EU member state without its own CAT rating the EU bloc rating, and drops the
#    bloc row. Member states' climate policy is set largely at EU level (the ETS, the
#    effort-sharing and renewables targets), so the bloc rating is the closest available
#    measure of their stance; without it France, Italy and Denmark had no CAT component and
#    failed the four-source eligibility rule. Members CAT rates themselves keep their own
#    rating.
#
# Adds `rating_scope`: "national" or "EU bloc rating". Pure; country_info is the
# standardized WDI country table (country, iso3c).
cat_prepare_ratings <- function(cat_raw,
                                country_info,
                                eu_member_iso3 = EU27_ISO3,
                                eu_label = "European Union") {
  require_columns(cat_raw, c("Country", "Overall.rating"), label = "cat_raw")
  require_columns(country_info, c("country", "iso3c"), label = "country_info")

  ref <- country_info %>%
    dplyr::transmute(
      iso3c = toupper(as.character(.data$iso3c)),
      country = as.character(.data$country),
      country_std = standardize_country_names(.data$country)
    ) %>%
    dplyr::distinct(.data$iso3c, .keep_all = TRUE)

  ratings <- cat_raw %>%
    dplyr::mutate(
      Country = stringr::str_squish(as.character(.data$Country)),
      Country = dplyr::recode(.data$Country, !!!CAT_COUNTRY_ALIASES, .default = .data$Country),
      Country = standardize_country_names(.data$Country),
      Country = dplyr::coalesce(ref$country[match(.data$Country, ref$country_std)], .data$Country),
      rating_scope = "national"
    )

  bloc <- ratings %>% dplyr::filter(.data$Country == eu_label)
  ratings <- ratings %>% dplyr::filter(.data$Country != eu_label)

  if (nrow(bloc) == 0) {
    return(ratings)
  }

  members <- ref %>%
    dplyr::filter(.data$iso3c %in% toupper(eu_member_iso3)) %>%
    dplyr::filter(!.data$country %in% ratings$Country)

  if (nrow(members) == 0) {
    return(ratings)
  }

  member_rows <- tidyr::crossing(
    members %>% dplyr::select(Country = "country"),
    bloc %>% dplyr::slice(1) %>% dplyr::select(-"Country")
  ) %>%
    dplyr::mutate(rating_scope = "EU bloc rating")

  dplyr::bind_rows(ratings, member_rows)
}

# value = c*g + (1 - c)*(1 - g), with c the CAT rating score (0.25 critically insufficient ...
# 1 almost sufficient) and g the technology's ghg_index (1 = cleanest). It measures how well a
# technology fits the country's climate stance: clean technologies score high in countries
# CAT rates as ambitious, fossil technologies score high in laggards. No stage variation.
# If cat_policy carries rating_scope (from cat_prepare_ratings()), rows that use the EU bloc
# rating say so in `explanation`.
cat_policy_index <- function(tech_ghg,
                             cat_policy,
                             supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                             year = 0L) {
  require_columns(tech_ghg, c("tech", "ghg_index"), label = "tech_ghg")
  require_columns(cat_policy, c("Country", "climate_policy_index"), label = "cat_policy")

  if (!"rating_scope" %in% names(cat_policy)) {
    cat_policy$rating_scope <- "national"
  }

  base_tbl <- tidyr::crossing(
    cat_policy %>% dplyr::select(dplyr::any_of(c("Country", "iso3c", "climate_policy_index", "rating_scope"))),
    tech_ghg %>% dplyr::select("tech", "ghg_index")
  ) %>%
    tidyr::crossing(supply_chain = supply_chain_universe) %>%
    dplyr::mutate(
      g = .data$ghg_index,
      c = .data$climate_policy_index,
      value = c * g + (1 - c) * (1 - g)
    )

  explanation_base <- paste(
    "CAT alignment c*g + (1-c)*(1-g); c = CAT overall rating score,",
    "g = technology ghg_index (IPCC AR5 lifecycle intensity, 1 = cleanest)"
  )

  base_tbl %>%
    dplyr::transmute(
      Country = .data$Country,
      dplyr::across(dplyr::any_of("iso3c")),
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      category = "Policy",
      variable = "CAT Policy Index",
      data_type = "index",
      value = .data$value,
      Year = as.integer(year),
      source = "Climate Action Tracker & IPCC",
      # A rating_scope lost in a join (NA) reads as national rather than blanking the text.
      explanation = dplyr::if_else(
        dplyr::coalesce(.data$rating_scope == "EU bloc rating", FALSE),
        paste0(explanation_base, "; c is the EU bloc rating (no national CAT rating)"),
        explanation_base
      )
    )
}
