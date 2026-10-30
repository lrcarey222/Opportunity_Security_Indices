# Investment theme builder functions.
#
# Rows carry the six supply-chain stages (R/utils/supply_chain_stages.R): GCIM Electric Power and
# Transport investment is Deployment; manufacturing capacity is staged by its Product; the
# critical-minerals sector is Refining; and manufacturing capex, which has no product detail, is
# replicated onto Components and Assembly.

# GCIM manufacturing-capacity Product -> stage, relative to the technology of the row:
# polysilicon is a refined commodity (Refining), as are the critical-minerals products (lithium
# carbonate / hydroxide / chloride / sulfate, nickel and cobalt sulfate, and the bare Lithium /
# Nickel / Cobalt / Graphite the monitor also lists); wafers, cells, blades and electrode active
# materials (EAM) only work inside a finished good (Components); modules (solar modules, battery
# packs), nacelles and ZEVs are the finished goods (Assembly). Singular and plural forms both
# match. NA for a product not listed here.
investment_product_to_stage <- function(product) {
  product_clean <- stringr::str_squish(stringr::str_to_lower(as.character(product)))

  dplyr::case_when(
    stringr::str_detect(product_clean, "polysilicon") ~ "Refining",
    stringr::str_detect(product_clean, "\\b(wafers?|cells?|blades?|eam)\\b|electrode active") ~ "Components",
    stringr::str_detect(product_clean, "\\b(modules?|nacelles?|zevs?)\\b|zero emission vehicle") ~ "Assembly",
    # After the part and finished-good rules, so "Lithium-ion cells" stays a component.
    stringr::str_detect(product_clean, "^(lithium|nickel|cobalt|graphite)\\b") ~ "Refining",
    TRUE ~ NA_character_
  )
}

# GCIM Sector -> supply-chain stage.
#
# Electric Power and Transport (capex on generation, storage and vehicles put into service) are
# Deployment. Manufacturing and Industry are staged by `product` (investment_product_to_stage())
# when the source carries one, as the capacity sheet does. Annual capex has no Product column, so
# without `product` a manufacturing row comes back as the Midstream roll-up, which the caller
# replicates onto Components and Assembly with supply_chain_expand_legacy(); a product the map
# does not know comes back the same way, with a warning. Unexpected Sector values default to
# Deployment with a warning.
investment_segment_to_supply_chain <- function(segment, product = NULL) {
  segment_clean <- stringr::str_trim(as.character(segment))
  manufacturing <- !is.na(segment_clean) & segment_clean %in% c("Manufacturing", "Industry")

  mapped <- rep("Deployment", length(segment_clean))
  mapped[manufacturing] <- "Midstream"

  if (!is.null(product)) {
    if (length(product) != length(segment_clean)) {
      stop("investment_segment_to_supply_chain(): `product` must be as long as `segment`.")
    }
    product_stage <- investment_product_to_stage(product)
    staged <- manufacturing & !is.na(product_stage)
    mapped[staged] <- product_stage[staged]

    unknown_products <- sort(unique(as.character(product)[manufacturing & is.na(product_stage)]), na.last = TRUE)
    if (length(unknown_products) > 0) {
      warning(
        "Unexpected manufacturing Product value(s): ",
        paste(unknown_products, collapse = ", "),
        ". Replicating these rows onto Components and Assembly."
      )
    }
  }

  unexpected_segments <- sort(unique(segment_clean[!segment_clean %in% c(
    "Manufacturing", "Industry", "Electric Power", "Transport", "", NA_character_
  )]))
  if (length(unexpected_segments) > 0) {
    warning(
      "Unexpected Segment value(s): ",
      paste(unexpected_segments, collapse = ", "),
      ". Defaulting these rows to Deployment."
    )
  }

  mapped
}

# Explanation suffix for the annual-investment rows of a stage, saying how the capex reached it.
investment_annual_stage_note <- function(supply_chain) {
  dplyr::case_when(
    supply_chain %in% c("Components", "Assembly") ~
      "; manufacturing capex has no product detail, so the same amount is replicated onto Components and Assembly (do not sum across stages)",
    supply_chain == "Refining" ~
      "; critical-minerals (refining) capex, split between Batteries and Electric Vehicles rather than counted in both",
    TRUE ~ ""
  )
}

investment_map_tech <- function(technology) {
  tech_clean <- stringr::str_squish(stringr::str_to_lower(as.character(technology)))

  dplyr::case_when(
    stringr::str_detect(tech_clean, "solar") ~ "Solar",
    stringr::str_detect(tech_clean, "wind") ~ "Wind",
    stringr::str_detect(tech_clean, "batter(y|ies)|battery storage|bess") ~ "Batteries",
    stringr::str_detect(tech_clean, "zero emission vehicles|electric vehicles|\\bzev\\b|\\bev\\b") ~ "Electric Vehicles",
    stringr::str_detect(tech_clean, "nuclear") ~ "Nuclear",
    stringr::str_detect(tech_clean, "geothermal") ~ "Geothermal",
    TRUE ~ NA_character_
  )
}

investment_is_critical_minerals <- function(technology) {
  tech_clean <- stringr::str_squish(stringr::str_to_lower(as.character(technology)))
  stringr::str_detect(tech_clean, "critical minerals")
}

# Rank-based index for a non-negative activity measure (investment, capacity).
#
# Zero means no activity and scores 0, the same as the zero the missing-data rule gives a
# country the monitor does not cover. Positive values are ranked among themselves above a
# zero anchor, so the smallest investor still scores above 0. Ranking zeros together with
# everyone else gave a block of tied zeros a mid-table score (about 0.3-0.45 in a typical
# tech-year) while an uncovered country scored 0. An all-zero group is all 0.
investment_safe_index <- function(x, gamma = 0.5) {
  if (!is.numeric(x)) {
    stop("investment_safe_index() expects numeric input.")
  }

  out <- rep(NA_real_, length(x))
  observed <- !is.na(x)
  out[observed] <- 0

  positive <- observed & x > 0
  if (any(positive)) {
    out[positive] <- median_scurve(c(0, x[positive]), gamma = gamma)[-1]
  }
  out
}

# Split of GCIM "Critical Minerals" investment between the two technologies it feeds.
#
# The monitor's critical-minerals products are battery materials (lithium, cobalt, nickel,
# graphite and their chemicals), which serve both EV batteries and grid storage. Copying the
# full amount into both Batteries and Electric Vehicles Refining counted every dollar twice in
# any cross-technology total. The split is each technology's base-year IEA demand share for
# those minerals (the shares the mineral themes weight by), renormalized over the two techs
# and averaged across minerals with equal weight, so tonnages of different minerals are never
# added together. Rank-based indices within a (tech, stage) are unchanged by a constant split.
investment_critical_minerals_split <- function(mineral_demand_clean,
                                               minerals = c("Lithium", "Cobalt", "Nickel", "Graphite"),
                                               techs = c("Batteries", "Electric Vehicles")) {
  require_columns(mineral_demand_clean, c("Mineral", "tech", "share_24"), label = "mineral_demand_clean")

  shares <- mineral_demand_clean %>%
    dplyr::filter(Mineral %in% minerals, tech %in% techs) %>%
    dplyr::group_by(Mineral, tech) %>%
    dplyr::summarize(share = sum(as.numeric(share_24), na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(Mineral, tech = techs, fill = list(share = 0)) %>%
    dplyr::group_by(Mineral) %>%
    dplyr::filter(sum(share) > 0) %>%
    dplyr::mutate(share = share / sum(share)) %>%
    dplyr::group_by(tech) %>%
    dplyr::summarize(share = mean(share), .groups = "drop")

  if (nrow(shares) == 0) {
    stop("No base-year demand for ", paste(minerals, collapse = "/"), " in ", paste(techs, collapse = "/"), ".")
  }

  split <- stats::setNames(shares$share, shares$tech)
  split[techs] / sum(split[techs])
}

# Equal split when no demand-based split is supplied.
investment_default_critical_minerals_split <- function() {
  c("Batteries" = 0.5, "Electric Vehicles" = 0.5)
}

# Critical-minerals rows (capex and capacity) are Refining: the monitor tracks processing plants
# whose products are refined chemicals and metals (investment_product_to_stage()), and refining
# rolls up to Upstream.
investment_route_critical_minerals <- function(tbl, value_col, split) {
  split <- split %||% investment_default_critical_minerals_split()
  if (is.null(names(split)) || any(!is.finite(split)) || any(split < 0) || abs(sum(split) - 1) > 1e-6) {
    stop("critical_minerals_split must be a named, non-negative vector summing to 1.")
  }

  tbl %>%
    tidyr::crossing(tibble::tibble(tech = names(split), split_share = unname(split))) %>%
    dplyr::mutate(
      "{value_col}" := .data[[value_col]] * split_share,
      supply_chain = "Refining"
    ) %>%
    dplyr::select(-split_share)
}

investment_assert_columns <- function(tbl, expected, label) {
  require_columns(tbl, expected, label = label)
  invisible(tbl)
}

# GCIM relabels columns between releases (e.g. "Sector" replaced "Segment" in the
# 2026Q1 download). Inputs are renamed to the canonical names used below rather
# than pinned to whatever the current release happens to call them.
investment_input_column_map <- c(
  region = "Region",
  country = "Country",
  segment = "Segment",
  sector = "Segment",
  market_sector = "Segment",
  technology = "Technology",
  product = "Product",
  category = "Category",
  capacity_status = "Category",
  capacity_category = "Category",
  year = "Year",
  quarter = "Quarter",
  investment = "Investment",
  value = "Value",
  end_use_application = "End_use_application",
  facility_type = "Facility_Type",
  investment_status = "Investment_Status",
  announcement_quarter = "Announcement_Quarter"
)

investment_canonical_column <- function(x) {
  raw <- as.character(x)
  raw[is.na(raw)] <- ""
  raw <- stringr::str_squish(raw)

  key <- stringr::str_to_lower(raw)
  key <- stringr::str_replace_all(key, "[^a-z0-9]+", "_")
  key <- stringr::str_replace_all(key, "^_+|_+$", "")

  mapped <- unname(investment_input_column_map[key])
  dplyr::coalesce(mapped, raw)
}

investment_standardize_input_names <- function(tbl) {
  if (is.null(tbl)) {
    return(tbl)
  }
  names(tbl) <- investment_canonical_column(names(tbl))
  tbl
}

# The GCIM manufacturing/industry capacity sheet, cleaned and staged: Country, tech, supply_chain,
# Product, capacity_stage (the source's Category), Value. Capacity is staged by the source's own
# Product labels: Solar polysilicon => Refining, wafers/cells => Components, modules => Assembly; Wind
# blades => Components, nacelles => Assembly; Batteries EAM/cells => Components, modules => Assembly;
# ZEVs => Assembly. Rows are staged after the tech filter so the unmapped Industry products (cement,
# steel, SAF) do not trip the unknown-product warning. Critical minerals are routed to Refining of
# the battery technologies by critical_minerals_split (investment_route_critical_minerals()).
investment_capacity_clean <- function(capacity_tbl, critical_minerals_split = NULL) {
  capacity_tbl <- investment_standardize_input_names(capacity_tbl)
  investment_assert_columns(capacity_tbl, c("Country", "Segment", "Technology", "Product", "Category", "Value"), label = "capacity_tbl")
  capacity_clean <- capacity_tbl %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      Technology = as.character(Technology),
      tech = investment_map_tech(Technology),
      Segment = as.character(Segment),
      Product = as.character(Product),
      capacity_stage = as.character(Category),
      Value = suppressWarnings(as.numeric(Value))
    ) %>%
    dplyr::mutate(Value = dplyr::coalesce(Value, 0))

  capacity_regular <- capacity_clean %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain = investment_segment_to_supply_chain(Segment, Product),
      Product,
      capacity_stage,
      Value
    ) %>%
    supply_chain_expand_legacy()

  capacity_critical_minerals <- capacity_clean %>%
    dplyr::filter(investment_is_critical_minerals(Technology)) %>%
    dplyr::select(Country, Product, capacity_stage, Value) %>%
    investment_route_critical_minerals("Value", critical_minerals_split)

  dplyr::bind_rows(capacity_regular, capacity_critical_minerals)
}

# Each country's share of world operating capacity, per product, from the cleaned capacity
# (investment_capacity_clean()): operating = "Current operational capacity"; committed = "Operational
# yet to come online" + "Under construction - anticipated capacity"; announced = "Announced -
# anticipated capacity" + "Paused - anticipated capacity". Each is divided by the world's current
# operating capacity of the same product (units differ across products, so shares are per product);
# a stage takes its most material product (the maximum share). Returns Country, tech, supply_chain,
# operating_share, committed_share, announced_share, n_products; a node the monitor covers but where
# a country has no row is an observed zero (the caller's coverage rule).
investment_capacity_shares <- function(capacity_clean) {
  require_columns(capacity_clean, c("Country", "tech", "supply_chain", "Product", "capacity_stage", "Value"), label = "capacity_clean")
  status <- dplyr::case_when(
    capacity_clean$capacity_stage == "Current operational capacity" ~ "operating",
    capacity_clean$capacity_stage %in% c("Operational yet to come online", "Under construction - anticipated capacity") ~ "committed",
    capacity_clean$capacity_stage %in% c("Announced - anticipated capacity", "Paused - anticipated capacity") ~ "announced",
    TRUE ~ NA_character_
  )
  by_product <- capacity_clean %>%
    dplyr::mutate(status = status) %>%
    dplyr::filter(!is.na(.data$status)) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain, .data$Product, .data$status) %>%
    dplyr::summarise(value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "status", values_from = "value", values_fill = 0)
  for (s in c("operating", "committed", "announced")) if (!s %in% names(by_product)) by_product[[s]] <- 0
  by_product %>%
    dplyr::group_by(.data$tech, .data$supply_chain, .data$Product) %>%
    dplyr::mutate(world_operating = sum(.data$operating, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$world_operating > 0) %>%
    dplyr::mutate(dplyr::across(c("operating", "committed", "announced"), ~ .x / .data$world_operating, .names = "{.col}_share")) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(operating_share = max(.data$operating_share), committed_share = max(.data$committed_share),
                     announced_share = max(.data$announced_share), n_products = dplyr::n(), .groups = "drop")
}

investment_momentum <- function(annual_tbl,
                                capacity_tbl,
                                quarterly_tbl = NULL,
                                momentum_window_years = 3,
                                capacity_year = 2025L,
                                gamma = 0.5,
                                country_reference = NULL,
                                enforce_country_reference = FALSE,
                                critical_minerals_split = NULL) {
  country_reference_std <- NULL
  if (!is.null(country_reference)) {
    country_reference_std <- standardize_country_names(country_reference)
    country_reference_std <- unique(country_reference_std[!is.na(country_reference_std) & nzchar(country_reference_std)])
  }

  annual_tbl <- investment_standardize_input_names(annual_tbl)
  capacity_tbl <- investment_standardize_input_names(capacity_tbl)
  quarterly_tbl <- investment_standardize_input_names(quarterly_tbl)

  investment_assert_columns(
    annual_tbl,
    c("Country", "Segment", "Technology", "Year", "Investment"),
    label = "annual_tbl"
  )
  investment_assert_columns(
    capacity_tbl,
    c(
      "Country", "Segment", "Technology", "Product", "Category", "Value"
    ),
    label = "capacity_tbl"
  )

  # Mapping assumptions:
  # Segment -> supply_chain: Electric Power/Transport=>Deployment. Manufacturing/Industry capex
  # has no Product column, so it is replicated onto Components and Assembly (one number, both
  # stages); manufacturing capacity is staged by its Product (investment_product_to_stage()).
  # Critical Minerals => Refining. Unexpected Segment values default to Deployment with warning.
  # Technology values are mapped to the model tech universe; unsupported technologies are dropped.

  annual_clean <- annual_tbl %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      Technology = as.character(Technology),
      tech = investment_map_tech(Technology),
      supply_chain_segment = investment_segment_to_supply_chain(Segment),
      Year = suppressWarnings(as.integer(Year)),
      investment_value = suppressWarnings(as.numeric(Investment))
    ) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::mutate(investment_value = dplyr::coalesce(investment_value, 0))

  # Manufacturing rows come back as the Midstream roll-up and are replicated onto Components and
  # Assembly: the aggregated GCIM workbook reports manufacturing capex by Sector x Technology only
  # (no product, on the annual, quarterly and announced sheets alike), so one plant's capex
  # cannot be split between the parts it makes and the finished goods.
  annual_regular <- annual_clean %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(Country, tech, supply_chain = supply_chain_segment, Year, investment_value) %>%
    supply_chain_expand_legacy()

  # Explicit special-case mapping requested by user: raw Technology == "Critical Minerals"
  # feeds Batteries Refining and Electric Vehicles Refining. It is split between them
  # (investment_critical_minerals_split(); equal split by default) rather than copied in full
  # to both, which double counted it.
  annual_critical_minerals <- annual_clean %>%
    dplyr::filter(investment_is_critical_minerals(Technology)) %>%
    dplyr::select(Country, Year, investment_value) %>%
    investment_route_critical_minerals("investment_value", critical_minerals_split)

  annual_clean <- dplyr::bind_rows(annual_regular, annual_critical_minerals)

  # Critical Minerals has no direct tech mapping but is routed to Refining above,
  # so it is not a dropped technology.
  dropped_techs <- annual_tbl %>%
    dplyr::mutate(mapped_tech = investment_map_tech(Technology)) %>%
    dplyr::filter(
      is.na(mapped_tech),
      !investment_is_critical_minerals(Technology),
      !is.na(Technology),
      Technology != ""
    ) %>%
    dplyr::distinct(Technology) %>%
    dplyr::pull(Technology)
  if (length(dropped_techs) > 0) {
    message(
      "Dropping unmapped technologies from investment input: ",
      paste(sort(dropped_techs), collapse = ", ")
    )
  }

  if (nrow(annual_clean) == 0) {
    stop(
      "No mappable annual investment rows after applying Segment/Technology mappings. ",
      "Expected at least one technology mapping into: Solar, Wind, Batteries, Electric Vehicles, Nuclear, Geothermal."
    )
  }

  if (!is.null(country_reference_std) && isTRUE(enforce_country_reference)) {
    annual_filtered <- annual_clean %>%
      dplyr::filter(Country %in% country_reference_std)

    if (nrow(annual_filtered) == 0) {
      warning(
        "country_reference filtering removed all annual rows; retaining unfiltered annual rows instead."
      )
    } else {
      annual_clean <- annual_filtered
    }
  }

  annual_agg <- annual_clean %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::summarise(investment_value = sum(investment_value, na.rm = TRUE), .groups = "drop")

  annual_raw <- annual_agg %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Annual Investment (USD bn, 2024$)",
      data_type = "raw",
      value = investment_value,
      Year,
      source = "GCIM / Rhodium",
      explanation = paste0(
        "Annual investment amount in 2024 USD billions, summed within Country-tech-supply_chain-year",
        investment_annual_stage_note(supply_chain)
      )
    )

  # The monitor lists investment where it happened, so a country missing from a year, or from
  # a stage it reports elsewhere, invested nothing there. Complete each country's panel over
  # every year and over the stages the monitor covers for that technology - and only those:
  # completing every stage would manufacture all-zero cells (Solar/Wind Refining, Nuclear and
  # Geothermal everywhere but Deployment, every Extraction and Recovery cell) that the index
  # then scored as observed zeros. Completing the years puts every country's latest value in
  # the same year, so the latest-year index and the momentum window compare like with like
  # instead of a 2019 value against a 2025 one.
  observed_stages <- annual_agg %>%
    dplyr::distinct(tech, supply_chain)
  panel_years <- seq.int(min(annual_agg$Year), max(annual_agg$Year))

  annual_complete <- annual_agg %>%
    dplyr::distinct(Country, tech) %>%
    dplyr::inner_join(observed_stages, by = "tech", relationship = "many-to-many") %>%
    tidyr::crossing(Year = panel_years) %>%
    dplyr::left_join(annual_agg, by = c("Country", "tech", "supply_chain", "Year")) %>%
    dplyr::mutate(investment_value = dplyr::coalesce(investment_value, 0))

  annual_index <- annual_complete %>%
    dplyr::group_by(tech, supply_chain, Year) %>%
    dplyr::mutate(value = investment_safe_index(log1p(investment_value), gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Annual Investment Index",
      data_type = "index",
      value,
      Year,
      source = "GCIM / Rhodium + author normalization",
      explanation = paste0(
        "Within tech-supply-chain-year median_scurve of log1p(annual investment) among investors, above a zero anchor; zero investment = 0",
        investment_annual_stage_note(supply_chain)
      )
    )

  annual_latest <- annual_complete %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::mutate(latest_year = max(Year, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Year == latest_year) %>%
    dplyr::rename(invest_latest = investment_value) %>%
    dplyr::mutate(baseline_year = latest_year - as.integer(momentum_window_years)) %>%
    dplyr::select(Country, tech, supply_chain, latest_year, baseline_year, invest_latest)

  annual_baseline <- annual_complete %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      baseline_year = Year,
      invest_baseline = investment_value
    )

  momentum_tbl <- annual_latest %>%
    dplyr::left_join(
      annual_baseline,
      by = c("Country", "tech", "supply_chain", "baseline_year")
    ) %>%
    dplyr::mutate(
      invest_baseline = dplyr::coalesce(invest_baseline, 0),
      momentum_raw = log1p(invest_latest) - log1p(invest_baseline),
      Year = as.integer(latest_year)
    ) %>%
    # Momentum is ranked among countries that invested in either year of the window. With
    # nothing in both years there is no momentum to rank: the row is left out and the zero
    # missing-data rule scores it 0, like any country outside the monitor.
    dplyr::filter(invest_latest > 0 | invest_baseline > 0) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(momentum_raw, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Investment Momentum Index",
      data_type = "index",
      value,
      Year,
      source = "GCIM / Rhodium + author normalization",
      explanation = paste0(
        "Within tech-supply-chain median_scurve(log1p(latest)-log1p(baseline)) among countries investing in either year",
        investment_annual_stage_note(supply_chain)
      )
    )

  momentum_raw_tbl <- annual_latest %>%
    dplyr::left_join(
      annual_baseline,
      by = c("Country", "tech", "supply_chain", "baseline_year")
    ) %>%
    dplyr::mutate(
      invest_baseline = dplyr::coalesce(invest_baseline, 0),
      value = log1p(invest_latest) - log1p(invest_baseline),
      Year = as.integer(latest_year)
    ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Investment Momentum (log1p latest-baseline)",
      data_type = "raw",
      value,
      Year,
      source = "GCIM / Rhodium + author transform",
      explanation = paste0("Raw momentum signal before normalization", investment_annual_stage_note(supply_chain))
    )

  capacity_clean <- investment_capacity_clean(capacity_tbl, critical_minerals_split)

  if (!is.null(country_reference_std) && isTRUE(enforce_country_reference)) {
    capacity_filtered <- capacity_clean %>%
      dplyr::filter(Country %in% country_reference_std)

    if (nrow(capacity_filtered) == 0) {
      warning(
        "country_reference filtering removed all capacity rows; retaining unfiltered capacity rows instead."
      )
    } else {
      capacity_clean <- capacity_filtered
    }
  }

  if (nrow(capacity_clean) == 0) {
    stop("No mappable capacity rows remain after Technology/Segment mapping.")
  }

  capacity_stage_levels <- c(
    "Current operational capacity",
    "Operational yet to come online",
    "Under construction - anticipated capacity",
    "Announced - anticipated capacity",
    "Paused - anticipated capacity"
  )

  capacity_product_stage <- capacity_clean %>%
    dplyr::filter(capacity_stage %in% capacity_stage_levels) %>%
    dplyr::group_by(Country, tech, supply_chain, Product, capacity_stage) %>%
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain, Product, capacity_stage) %>%
    dplyr::mutate(product_stage_index = investment_safe_index(log1p(Value), gamma = gamma)) %>%
    dplyr::ungroup()

  capacity_stage_raw <- capacity_clean %>%
    dplyr::filter(capacity_stage %in% capacity_stage_levels) %>%
    dplyr::mutate(stage_group = dplyr::case_when(
      capacity_stage == "Current operational capacity" ~ "Operating",
      capacity_stage %in% c(
        "Operational yet to come online",
        "Under construction - anticipated capacity",
        "Announced - anticipated capacity",
        "Paused - anticipated capacity"
      ) ~ "Pipeline",
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(stage_group)) %>%
    dplyr::group_by(Country, tech, supply_chain, stage_group) %>%
    dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = dplyr::if_else(
        stage_group == "Operating",
        "Operating Capacity (raw stage sum)",
        "Pipeline Capacity (raw stage sum)"
      ),
      data_type = "raw",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium",
      explanation = "Raw summed capacity by status group over the products in this supply-chain stage (units may differ across products, e.g. battery EAM in kilotons and cells in GWh; the capacity indices use per-product indices instead)"
    )

  operating_capacity <- capacity_product_stage %>%
    dplyr::filter(capacity_stage == "Current operational capacity") %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(raw_stage_value = mean(product_stage_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = investment_safe_index(raw_stage_value, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Operating Capacity Index",
      data_type = "index",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium + author normalization",
      explanation = "Mean of product-level operating capacity indices over the products in this supply-chain stage (each product indexed on its own because units differ), renormalized by tech-supply-chain"
    )

  pipeline_capacity <- capacity_product_stage %>%
    dplyr::filter(capacity_stage %in% c(
      "Operational yet to come online",
      "Under construction - anticipated capacity",
      "Announced - anticipated capacity",
      "Paused - anticipated capacity"
    )) %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(raw_stage_value = mean(product_stage_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = investment_safe_index(raw_stage_value, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Pipeline Capacity Index",
      data_type = "index",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium + author normalization",
      explanation = "Mean of product-level pipeline capacity indices over the products in this supply-chain stage (each product indexed on its own because units differ), renormalized by tech-supply-chain"
    )

  out <- dplyr::bind_rows(
    annual_raw,
    momentum_raw_tbl,
    capacity_stage_raw,
    annual_index,
    momentum_tbl,
    operating_capacity,
    pipeline_capacity
  ) %>%
    standardize_theme_table()

  validate_schema(out)
  assert_supply_chain_stages(out$supply_chain, label = "investment_momentum() supply_chain")
  out
}

# Sheet names have also moved between releases (capacity used to live on the
# announced-investment tab), so sheets are matched by pattern, most specific first.
investment_gcim_sheet_patterns <- list(
  annual = c("^annual_actual_investment$", "annual.*actual.*invest"),
  quarterly = c("^mfg_ind_quarterly_actual_inv", "quarter.*actual.*inv"),
  capacity = c("^mfg_ind_capacity$", "^mfg.?ind.*capacity", "manufactur.*capacity")
)

investment_resolve_gcim_sheet <- function(sheets, patterns, label, required = TRUE) {
  for (pattern in patterns) {
    hits <- sheets[stringr::str_detect(sheets, stringr::regex(pattern, ignore_case = TRUE))]
    if (length(hits) > 0) {
      return(hits[[1]])
    }
  }

  if (!required) {
    return(NA_character_)
  }

  stop(
    "Could not locate the ", label, " sheet in the GCIM investment workbook. ",
    "Tried patterns: ", paste(patterns, collapse = ", "),
    ". Found sheets: ", paste(sheets, collapse = ", "), "."
  )
}

# Header rows sit below a variable number of title/notes rows, so the header is
# found by scanning for the first row that carries all required column labels.
investment_read_gcim_sheet <- function(path, sheet, required_columns, label, probe_rows = 30) {
  probe <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    n_max = probe_rows
  ))

  header_row <- NA_integer_
  for (i in seq_len(nrow(probe))) {
    candidate <- investment_canonical_column(
      vapply(probe[i, , drop = FALSE], function(col) as.character(col)[[1]], character(1))
    )
    if (all(required_columns %in% candidate)) {
      header_row <- i
      break
    }
  }

  if (is.na(header_row)) {
    stop(
      "Unexpected column layout in GCIM investment workbook sheet '", sheet, "' (", label, "). ",
      "Expected columns: ", paste(required_columns, collapse = ", "),
      ". No header row carrying them was found in the first ", probe_rows, " rows. ",
      "Row 1 reads: ",
      paste(
        investment_canonical_column(
          vapply(probe[1, , drop = FALSE], function(col) as.character(col)[[1]], character(1))
        ),
        collapse = ", "
      ),
      "."
    )
  }

  tbl <- suppressMessages(readxl::read_excel(path, sheet = sheet, skip = header_row - 1))
  tbl <- investment_standardize_input_names(tbl)

  named_cols <- nzchar(names(tbl)) & !stringr::str_detect(names(tbl), "^\\.{3}[0-9]+$")
  tbl <- tbl[, named_cols, drop = FALSE]

  investment_assert_columns(tbl, required_columns, label = paste0(label, " sheet '", sheet, "'"))
  tbl
}

investment_momentum_from_excel <- function(path,
                                           momentum_window_years = 3,
                                           capacity_year = 2025L,
                                           gamma = 0.5,
                                           country_reference = NULL,
                                           critical_minerals_split = NULL) {
  if (!file.exists(path)) {
    stop(
      "Missing GCIM investment monitor file at ",
      path,
      ". Expected file name: data/raw/GCIM_Investment_Capacity_aggregated.xlsx"
    )
  }

  sheets <- readxl::excel_sheets(path)
  annual_sheet <- investment_resolve_gcim_sheet(
    sheets, investment_gcim_sheet_patterns$annual, "annual investment"
  )
  capacity_sheet <- investment_resolve_gcim_sheet(
    sheets, investment_gcim_sheet_patterns$capacity, "manufacturing/industry capacity"
  )
  quarterly_sheet <- investment_resolve_gcim_sheet(
    sheets, investment_gcim_sheet_patterns$quarterly, "quarterly investment",
    required = FALSE
  )

  annual_tbl <- investment_read_gcim_sheet(
    path, annual_sheet,
    c("Country", "Segment", "Technology", "Year", "Investment"),
    label = "annual investment"
  )
  capacity_tbl <- investment_read_gcim_sheet(
    path, capacity_sheet,
    c("Country", "Segment", "Technology", "Product", "Category", "Value"),
    label = "manufacturing/industry capacity"
  )

  # Quarterly investment is carried through for callers but is not used by any
  # current index component, so a missing/renamed sheet is a message, not a stop.
  quarterly_tbl <- NULL
  if (is.na(quarterly_sheet)) {
    message(
      "No quarterly investment sheet found in ", path,
      "; continuing without it. Found sheets: ", paste(sheets, collapse = ", ")
    )
  } else {
    quarterly_tbl <- tryCatch(
      investment_read_gcim_sheet(
        path, quarterly_sheet,
        c("Country", "Segment", "Technology", "Quarter", "Investment"),
        label = "quarterly investment"
      ),
      error = function(e) {
        message(
          "Skipping quarterly investment sheet '", quarterly_sheet, "': ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl,
    quarterly_tbl = quarterly_tbl,
    momentum_window_years = momentum_window_years,
    capacity_year = capacity_year,
    gamma = gamma,
    country_reference = country_reference,
    critical_minerals_split = critical_minerals_split
  )
}
