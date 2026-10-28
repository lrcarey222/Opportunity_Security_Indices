# Foreign Dependency theme builder functions.
foreign_dependency_build_country_reference <- function(ei, year = 2025) {
  # Use EI as the authoritative country reference and harmonize legacy naming tweaks.
  # "USSR" is EI's CIS aggregate, still published for the latest year.
  ei %>%
    dplyr::filter(
      Year == year,
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::distinct(Country)
}

# EI country rows for the regional expansions, one per country: standardized name, EU flag,
# EI sub-region.
#
# The expansions below copy regional values to countries and then rank every row they
# produce, so aggregates ("Total Asia Pacific", "Other Europe", "USSR") and spellings that
# only collapse later ("US", "Vietnam") have to be gone before the ranking, not after. EI
# flags Cyprus and Estonia EU = 1 on some variables and 0 on others; max() keeps one row.
foreign_dependency_ei_countries <- function(ei, year = NULL) {
  year <- year %||% max(suppressWarnings(as.integer(ei$Year)), na.rm = TRUE)

  ei %>%
    dplyr::filter(
      as.character(Year) == as.character(year),
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      EU = max(as.integer(EU), na.rm = TRUE),
      SubRegion = dplyr::first(SubRegion[!is.na(SubRegion) & nzchar(SubRegion)]),
      .groups = "drop"
    )
}

foreign_dependency_build_mineral_supply <- function(critical,
                                                    mineral_demand_clean,
                                                    country_reference,
                                                    year = iea_critical_minerals_base_year(critical),
                                                    gamma = 0.5,
                                                    horizon_year = 2035) {
  # The IEA restates its base year each release, so address the supply columns by year.
  base_col <- iea_critical_minerals_year_col(critical, year, label = "mineral supply")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral supply")

  critical <- critical %>%
    dplyr::mutate(`Sector.Country` = standardize_country_names(`Sector.Country`))

  # Every roster country gets a zero-supply row for each (mineral, stage) it is not listed
  # for, so a non-supplier is scored the same way whether or not the IEA lists it for some
  # other mineral: own share 0, plus the world-concentration (HHI) terms every country shares.
  # Completing only the listed countries scored those at the tied-zero rank and left the
  # unlisted ones at a flat 0 in the final completion below. Listed suppliers outside the
  # roster are completed too (as critical_minerals_processing() does): Lao PDR mines only
  # rare earths, and without zero rows for the other minerals its EV score averaged rare
  # earths alone and ranked second to China.
  countries <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      !`Sector.Country` %in% c("", "Rest of world")
    ) %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::bind_rows(dplyr::distinct(country_reference, `Sector.Country` = Country)) %>%
    dplyr::distinct(`Sector.Country`)

  # Compute market shares, HHI, and the composite supply security index.
  #
  # Mining rows score the Extraction stage and refining/processing rows the Refining stage
  # (iea_critical_minerals_supply_stage()): pooling them put ore and refined-metal tonnage into
  # one share and one HHI. Shares and HHIs are therefore taken within (mineral, tech, stage).
  mineral_supply <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    dplyr::rename(country = `Sector.Country`) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::mutate(
      mineral = harmonize_mineral_names(mineral),
      supply_chain = iea_critical_minerals_supply_stage(supply_chain_raw, legacy = FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      mineral, country, supply_chain,
      supply_base = dplyr::all_of(base_col),
      supply_horizon = dplyr::all_of(horizon_col)
    ) %>%
    # Countries absent from a (mineral, stage) block are not among its listed suppliers.
    tidyr::complete(
      tidyr::nesting(mineral, supply_chain),
      country = countries$`Sector.Country`,
      fill = list(supply_base = 0, supply_horizon = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Mineral = harmonize_mineral_names(Mineral), tech, share_24, share_35),
      by = c("mineral" = "Mineral"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * supply_base,
      supply_35 = share_35 * supply_horizon
    ) %>%
    dplyr::group_by(mineral, tech, supply_chain) %>%
    dplyr::mutate(
      share_frac_24 = supply_24 / sum(supply_24, na.rm = TRUE),
      HHI_24 = sum(share_frac_24^2, na.rm = TRUE),
      share_frac_35 = supply_35 / sum(supply_35, na.rm = TRUE),
      HHI_35 = sum(share_frac_35^2, na.rm = TRUE)
    ) %>%
    dplyr::filter(
      country != "Rest of world",
      !is.na(share_frac_24),
      !is.na(share_frac_35)
    ) %>%
    # Ranked within stage (pooled across minerals and techs, as before the stage split), so a
    # refining share is compared with other refining shares. A zero share scores 0 and the
    # suppliers are ranked above it (iea_critical_minerals_share_index()).
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(
      market_share24_index = iea_critical_minerals_share_index(share_frac_24, gamma = gamma),
      market_share35_index = iea_critical_minerals_share_index(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      security_index = mean(dplyr::c_across(c(market_share24_index:hhi35_index)), na.rm = TRUE)
    ) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))

  # Aggregate into the canonical schema with completed country coverage (every roster country
  # already has rows, so the 0 fill below is only a safeguard).
  mineral_supply %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::summarize(
      security_index = stats::weighted.mean(security_index, w = share_35, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    tidyr::pivot_longer(
      cols = c(security_index),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      category = "Foreign Dependency",
      variable = "Mineral Supply",
      data_type = "index",
      Year = year,
      source = "IEA Critical Minerals Database",
      explanation = dplyr::case_when(
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries, and HHI across critical minerals, ",
          "of {dplyr::if_else(supply_chain == 'Refining', 'refined', 'mined')} supply"
        )
      )
    ) %>%
    dplyr::select(
      country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    ) %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(
      country = country_reference$Country,
      fill = list(value = 0)
    ) %>%
    dplyr::rename(Country = country)
}

# Stage of each IEA ETP manufacturing-capacity technology (docs/supply_chain_stages.md). For
# solar (modules), wind (nacelles), electrolysers and heat pumps ETP measures the end-product
# step, which is final assembly. For batteries it measures cell capacity, and a cell is a
# component of the pack. ETP has one figure per technology, so the per-technology rows are then
# copied onto the other manufacturing stage (supply_chain_fill_manufacturing_pair()); the
# coalition's supplier test keeps this primary stage only (COALITION_CLEANTECH_STAGES).
foreign_dependency_cleantech_stage <- function(technology) {
  dplyr::if_else(technology == "Batteries", "Components", "Assembly")
}

foreign_dependency_build_cleantech_midstream <- function(ei,
                                                         cleantech_midstream,
                                                         year = 2035,
                                                         gamma = 0.5) {
  # Reshape IEA clean tech midstream data into a long form with year tags. Row labels go
  # through the same name standardization as the EI roster they are joined to ("US").
  cleantech_long <- cleantech_midstream %>%
    dplyr::rename(Country = X) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    tidyr::pivot_longer(
      cols = -Country,
      names_to = c("Technology", "Status"),
      names_pattern = "(.*)\\.\\.\\.(Current|Pipeline)",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Year = dplyr::if_else(Status == "Current", "2024", "2035"),
      # "Heat.Pumps" is read.csv()'s name for the IEA header "Heat Pumps - Current".
      Technology = csv_header_label(Technology),
      # IEA reports electrolyser manufacturing capacity; in this pipeline that is Green
      # Hydrogen Assembly, as market_share_manufacturing() already names it. Left as
      # "Electrolyzers" it matched no index technology, so Green Hydrogen Assembly had no
      # Energy Security reading and dropped out of the Strategic Index.
      Technology = dplyr::if_else(Technology == "Electrolyzers", "Green Hydrogen", Technology)
    )

  # Map EU member countries into a shared "EU" entity to match legacy logic.
  country_map <- ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(
      Country_clean = dplyr::if_else(Country == "Vietnam", "Viet Nam", Country),
      country2 = dplyr::if_else(EU == 1, "EU", Country_clean)
    ) %>%
    dplyr::select(country2) %>%
    dplyr::distinct()

  # Compute global market shares per technology for the target year. The denominator is the
  # IEA world total, including the table's "Other" row, which has no EI country and is dropped
  # by the join (as in market_share_manufacturing_build_cleantech_midstream()).
  world_totals <- cleantech_long %>%
    dplyr::filter(Year == as.character(year)) %>%
    dplyr::group_by(Technology) %>%
    dplyr::summarize(world_total = sum(Value, na.rm = TRUE), .groups = "drop")

  ct_ms <- country_map %>%
    dplyr::left_join(cleantech_long, by = c("country2" = "Country")) %>%
    dplyr::filter(Year == as.character(year)) %>%
    dplyr::left_join(world_totals, by = "Technology") %>%
    dplyr::mutate(market_share = Value / world_total) %>%
    dplyr::select(Country = country2, Technology, market_share)

  # Convert market shares into percentile-based indices.
  ct_idx <- ct_ms %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::ungroup()

  # Emit tech-level raw and indexed market shares in canonical form.
  cleantech_clean <- ct_idx %>%
    tidyr::pivot_longer(
      cols = c(market_share, market_share_index),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = Technology,
      supply_chain = foreign_dependency_cleantech_stage(Technology),
      category = "Foreign Dependency",
      variable = "Market Share",
      data_type = dplyr::if_else(name == "market_share", "raw", "index"),
      Year = as.character(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue(
          "2035 market share for {tech} = Value ÷ global total for {tech}"
        ),
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries"
        )
      )
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    ) %>%
    supply_chain_fill_manufacturing_pair(by = c("Country", "tech", "variable", "data_type", "Year"))

  # Build the composite clean-tech manufacturing index across technologies. It is one value per
  # country, so it takes one stage: Assembly, the step four of its five inputs measure (solar,
  # wind, electrolyser and heat-pump end-product capacity). The fifth, battery cell capacity, is
  # scored in both manufacturing stages by its own per-technology rows. "Clean Tech Midstream" is
  # not an index technology, so the composite scores no cell.
  comp_ct <- ct_idx %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      mean_idx = mean(market_share_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      composite_index = median_scurve(mean_idx, gamma = gamma),
      tech = "Clean Tech Midstream",
      supply_chain = "Assembly",
      category = "Foreign Dependency",
      variable = "Market Share",
      data_type = "index",
      Year = as.character(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = paste(
        "Percent-rank of the mean of the per-technology manufacturing-capacity indexes, filed under",
        "Assembly: solar, wind, electrolyser and heat-pump capacity is end-product capacity, battery",
        "capacity is cells (NB: All EU countries are given same market share)"
      )
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value = composite_index,
      Year,
      source,
      explanation
    )

  # Expand the EU rollup back to individual countries via EI mapping.
  cleantech_final <- dplyr::bind_rows(
    lapply(list(cleantech_clean, comp_ct), standardize_bind_rows_inputs)
  ) %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))

  ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(
      Country = dplyr::if_else(Country == "Vietnam", "Viet Nam", Country),
      country2 = dplyr::if_else(EU == 1, "EU", Country),
      country2 = dplyr::if_else(country2 == "US", "United States", country2)
    ) %>%
    dplyr::left_join(
      cleantech_final,
      by = c("country2" = "Country"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::select(-country2, -EU) %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))
}

# The five regions of the IEA Global EV Outlook extract (data/raw/ev_Midstream_capacity.csv).
# They partition the world: their 2024 sales add up to the world's 17.3 million EVs. Rest of
# World is the remainder, so it holds non-EU Europe (United Kingdom, Norway, Turkiye,
# Switzerland, ...) as well as Latin America, Africa, the Middle East and the CIS.
FOREIGN_DEPENDENCY_EV_REGIONS <- c(
  "China", "European Union", "North America", "Other Asia Pacific", "Rest of World"
)

# IEA EV region of each EI economy (vectorized over the EI country, EU flag and sub-region).
#
# The legacy expansion matched EI sub-regions to the IEA regions in three joins and, to keep
# EU members out of Rest of World, sent the whole EI "Europe" sub-region to a placeholder
# ("boop") that matched nothing. That also dropped every non-EU European economy (United
# Kingdom, Norway, Turkiye, Ukraine, Switzerland, Iceland, Serbia, North Macedonia) from the EV
# rows, although the IEA counts their sales and production in Rest of World.
foreign_dependency_ev_region <- function(country, eu, sub_region) {
  dplyr::case_when(
    country == "China" ~ "China",
    eu %in% 1 ~ "European Union",
    sub_region %in% "North America" ~ "North America",
    sub_region %in% "Asia Pacific" ~ "Other Asia Pacific",
    TRUE ~ "Rest of World"
  )
}

# One row per IEA region for `year`, with the extract's `import_share` renamed for what it
# holds. That column is (production - sales) / sales, a net export ratio: positive for a net
# exporter (2024: Other Asia Pacific +0.30, China +0.10), negative for a net importer (Rest of
# World -0.89). The index ranks it as it should, higher = less dependent on imported EVs; only
# the name was inverted. It is checked against that definition, so a restaged file that carries
# a true import share (higher = more dependent) stops here instead of entering the index with
# the wrong sign.
foreign_dependency_ev_regional_values <- function(ev_midstream, year) {
  require_columns(
    ev_midstream,
    c("Year", "Region", "Domestic.Production", "Domestic.sales", "import_share", "market_share"),
    label = "ev_midstream"
  )

  regional <- ev_midstream %>%
    dplyr::filter(as.character(Year) == as.character(year)) %>%
    dplyr::transmute(
      Region,
      Domestic.sales,
      Domestic.Production,
      net_export_ratio = import_share,
      market_share
    )

  if (!setequal(regional$Region, FOREIGN_DEPENDENCY_EV_REGIONS) || anyDuplicated(regional$Region) > 0) {
    stop(
      "ev_midstream must have one row per IEA region for ", year, " (",
      paste(FOREIGN_DEPENDENCY_EV_REGIONS, collapse = ", "), "); found: ",
      paste(regional$Region, collapse = ", ")
    )
  }

  implied <- (regional$Domestic.Production - regional$Domestic.sales) / regional$Domestic.sales
  off <- abs(regional$net_export_ratio - implied) > 1e-6
  if (any(off | is.na(off))) {
    stop(
      "ev_midstream import_share is not (production - sales) / sales for ",
      paste(regional$Region[off | is.na(off)], collapse = ", "),
      "; the EV Assembly net export term assumes that definition."
    )
  }

  regional
}

# Explanations of the EV rows, keyed by the column name before the "_index" suffix is stripped
# (the raw and index rows share the name "market_share"). The composite keeps its variable
# name, ev_Midstream, from the three-stage pipeline; it is scored under Assembly.
foreign_dependency_ev_explanations <- function() {
  c(
    Domestic.sales = "EV sales in the region (million vehicles)",
    Domestic.Production = "EV production in the region (million vehicles)",
    net_export_ratio = paste(
      "Net export ratio of the region = (production - sales) / sales; positive means net",
      "exporter, negative net importer (the IEA extract labels it import_share)"
    ),
    market_share = "The region's share of world EV production",
    production_index = "Percent-ranked EV production of the region",
    sales_index = "Percent-ranked EV sales of the region",
    net_export_index = "Percent-ranked net export ratio of the region (higher = less dependent on imported EVs)",
    market_share_index = "Percent-ranked share of world EV production",
    ev_Midstream_index = "Mean of the percent-ranked production, net export ratio and market share"
  )
}

foreign_dependency_build_ev_midstream <- function(ei,
                                                  ev_midstream,
                                                  year = 2024,
                                                  gamma = 0.5) {
  require_columns(ei, c("Country", "EU", "SubRegion"), label = "ei")
  regional <- foreign_dependency_ev_regional_values(ev_midstream, year = year)

  countries <- ei %>%
    dplyr::distinct(Country, EU, SubRegion) %>%
    dplyr::mutate(Region = foreign_dependency_ev_region(Country, EU, SubRegion))
  assert_unique_keys(countries, "Country", label = "ei")

  explanations <- foreign_dependency_ev_explanations()

  # Every economy takes its region's values, and the percent ranks are taken over economies,
  # so a region counts once per EI economy in it (as in the legacy expansion).
  countries %>%
    dplyr::inner_join(regional, by = "Region", relationship = "many-to-one") %>%
    dplyr::mutate(
      production_index = median_scurve(Domestic.Production, gamma = gamma),
      sales_index = median_scurve(Domestic.sales, gamma = gamma),
      net_export_index = median_scurve(net_export_ratio, gamma = gamma),
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    # Sales are demand, not manufacturing: they stay out of the composite and are emitted as
    # Deployment rows (user decision 2026-10-26, docs/supply_chain_stages.md).
    dplyr::mutate(ev_Midstream_index = mean(c(production_index, net_export_index, market_share_index))) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      Country, Region,
      Domestic.sales, Domestic.Production, net_export_ratio, market_share,
      production_index, sales_index, net_export_index, market_share_index, ev_Midstream_index
    ) %>%
    tidyr::pivot_longer(
      cols = -c(Country, Region),
      names_to = "variable",
      values_to = "value"
    ) %>%
    # Standardize variables into raw/index pairs in the canonical schema. The values are
    # regional, copied to each economy, and the explanation names the region. Production,
    # market share, the net export ratio and the composite are the vehicle-manufacturing step,
    # Assembly; the net export ratio compares production with sales, but it measures the
    # production surplus, so it stays with production. Sales are Deployment.
    dplyr::mutate(
      tech = "Electric Vehicles",
      supply_chain = dplyr::if_else(variable %in% c("Domestic.sales", "sales_index"), "Deployment", "Assembly"),
      category = "Foreign Dependency",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      explanation = paste0(
        unname(explanations[variable]), ". IEA region: ", Region,
        " (the regional value is copied to each economy in it)"
      ),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as.character(year),
      source = "IEA EV Outlook"
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    ) %>%
    dplyr::distinct()
}

foreign_dependency <- function(critical,
                               mineral_demand_clean,
                               ei,
                               cleantech_midstream,
                               ev_midstream,
                               year = list(
                                 minerals = iea_critical_minerals_base_year(critical),
                                 cleantech = "2035",
                                 ev = "2024"
                               ),
                               ei_year = "2025",
                               gamma = 0.5) {
  # Reference country list and run each foreign dependency sub-theme builder. The EI country
  # roster follows the EI release year, which moves independently of the IEA vintages in `year`.
  country_reference <- foreign_dependency_build_country_reference(ei, year = ei_year)
  ei_countries <- foreign_dependency_ei_countries(ei, year = ei_year)

  mineral_supply <- foreign_dependency_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    year = year$minerals,
    gamma = gamma
  )

  cleantech <- foreign_dependency_build_cleantech_midstream(
    ei = ei_countries,
    cleantech_midstream = cleantech_midstream,
    year = year$cleantech,
    gamma = gamma
  )

  ev_midstream_tbl <- foreign_dependency_build_ev_midstream(
    ei = ei_countries,
    ev_midstream = ev_midstream,
    year = year$ev,
    gamma = gamma
  )
  standardized <- lapply(
    list(mineral_supply, cleantech, ev_midstream_tbl),
    standardize_bind_rows_inputs
  )

  dplyr::bind_rows(standardized) %>%
    energy_security_add_overall_index()
}
