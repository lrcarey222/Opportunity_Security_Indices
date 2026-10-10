# Trade category core computations (Comtrade + Atlas hybrid pipeline).
#
# One builder computes every trade variable; the two themes then keep the components their
# pillar scores (TRADE_THEME_VARIABLES), so the ES and EO trade tables are no longer copies
# of each other:
#
#   trade_concentration (ES "Overall Trade Risk Index"): exposure. S(-HHI) of the world
#     export market, own export market share, and S(-net imports / GDP) in the basket.
#   export_feasibility (EO "Overall Trade Index"): revealed and latent export capability.
#     Market share, Balassa RCA, export size, Atlas feasibility, S(-HHI).
#
# Every country-level index is ranked across the country_info economies within (tech,
# supply_chain, sub_sector) - one data year per build. HHI is a property of the world
# market for the basket, so it is one value per cell: it moves a cell's level relative to
# other cells but cannot rank countries inside it. Its S-curve is ranked across the cells of
# the scored (headline) technologies only (`hhi_rank_techs`); a cell of another technology
# (Semiconductors) is placed against them and cannot move them (median_scurve_ref()).
#
# Re-export hubs (`reexport_hubs`, config/index_definition.yml trade$re_export_hubs: Hong Kong)
# are dropped from the Comtrade measures - exports, imports, Balassa RCA, export size and the
# export HHI, and with them from the world aggregates of the RCA and the HHI - because their
# Comtrade exports are mostly goods made elsewhere, already counted in the producers' exports.
# Their Atlas variables (market share, feasibility, deficit/GDP) stay.

TRADE_THEME_VARIABLES <- list(
  trade_concentration = list(
    index = c("HHI", "market_share", "deficit_gdp"),
    raw = c("HHI", "market_share", "exports", "imports", "deficit", "gdp", "deficit_gdp")
  ),
  export_feasibility = list(
    index = c("market_share", "rca", "export_size", "feas", "HHI"),
    raw = c("HHI", "market_share", "exports", "imports", "export_rca", "feasibility")
  )
)

trade_core_build_energy_codes <- function(subcat, include_sub_sector = FALSE) {
  pick_col <- function(tbl, candidates, label) {
    match <- intersect(candidates, names(tbl))
    if (length(match) == 0) {
      stop("Missing ", label, " column in trade codes file.")
    }
    match[[1]]
  }

  tech_col <- pick_col(subcat, c("tech", "Technology", "technology"), "tech")
  supply_chain_col <- pick_col(
    subcat,
    # "Value Chain" with a space is the header the staged hts_codes_categories CSV
    # actually carries. readr::read_csv() preserves it verbatim, so callers that hand
    # the file over unrenamed reach here with the spaced spelling.
    c("supply_chain", "Value.Chain", "Value Chain", "value_chain", "Supply.Chain", "supply chain"),
    "supply_chain"
  )
  sub_sector_col <- pick_col(subcat, c("sub_sector", "Sub.Sector", "subsector"), "sub_sector")
  hs6_col <- pick_col(subcat, c("HS6", "hs6", "HS_6"), "HS6")

  subcat %>%
    dplyr::transmute(
      tech = as.character(.data[[tech_col]]),
      supply_chain = as.character(.data[[supply_chain_col]]),
      sub_sector = as.character(.data[[sub_sector_col]]),
      code6 = stringr::str_pad(as.character(.data[[hs6_col]]), width = 6, side = "left", pad = "0"),
      code4 = substr(code6, 1, 4)
    ) %>%
    dplyr::filter(!is.na(code6), nzchar(code6)) %>%
    dplyr::mutate(
      sub_sector = if (isTRUE(include_sub_sector)) dplyr::coalesce(sub_sector, "All") else "All"
    ) %>%
    dplyr::distinct()
}

trade_core_detect_year_column <- function(tbl, label = "trade data") {
  year_col <- intersect(c("period", "ref_year", "year", "Year"), names(tbl))
  if (length(year_col) == 0) {
    stop("Unable to locate year column in ", label, ".")
  }
  year_col[[1]]
}

trade_core_filter_year <- function(tbl, year, label = "trade data") {
  year_col <- trade_core_detect_year_column(tbl, label = label)
  tbl %>%
    dplyr::filter(as.character(.data[[year_col]]) == as.character(year))
}

# Atlas codes arrive as integers when the file is read without colClasses, which drops the
# leading zero of chapters 01-09 (e.g. 2710.00 -> 271000 survives, 0101.11 -> 10111 does not).
trade_core_pad_atlas_codes <- function(tbl, width) {
  tbl %>%
    dplyr::mutate(
      product_hs92_code = stringr::str_pad(
        as.character(product_hs92_code),
        width = width,
        side = "left",
        pad = "0"
      )
    )
}

# The country's share of world exports of the basket: its basket exports over the world's,
# with world exports of each product summed over every Atlas reporter (the Atlas
# global_market_share is exactly export_value over that sum). This used to be the mean of
# the country's product-level shares weighted by its own exports, which scores dominance
# of the few products a country ships rather than its weight in the basket: DRC's cobalt
# gave it 0.86 in Batteries Midstream against a 0.7% basket share, and a country with
# imports but no basket exports got NaN, later imputed to the average.
trade_core_build_aec_market_share <- function(aec_6_data, energy_codes, year = 2023) {
  keys <- c("tech", "supply_chain", "sub_sector")
  flows <- aec_6_data %>%
    trade_core_filter_year(year = year, label = "AEC HS6 market share data") %>%
    trade_core_pad_atlas_codes(width = 6) %>%
    dplyr::mutate(export_value = suppressWarnings(as.numeric(export_value)))

  basket_codes <- dplyr::distinct(energy_codes, dplyr::across(dplyr::all_of(keys)), code6)

  world_basket <- flows %>%
    dplyr::group_by(product_hs92_code) %>%
    dplyr::summarize(world_exports = sum(export_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::inner_join(basket_codes, by = c("product_hs92_code" = "code6"), relationship = "one-to-many") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarize(world_exports = sum(world_exports), .groups = "drop")

  flows %>%
    dplyr::inner_join(basket_codes, by = c("product_hs92_code" = "code6"), relationship = "many-to-many") %>%
    dplyr::group_by(country_iso3_code, dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarize(basket_exports = sum(export_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::inner_join(world_basket, by = keys, relationship = "many-to-one") %>%
    dplyr::transmute(
      country_iso3_code,
      tech,
      supply_chain,
      sub_sector,
      market_share = dplyr::if_else(world_exports > 0, basket_exports / world_exports, NA_real_)
    )
}

trade_core_build_aec_feasibility <- function(aec_4_data, energy_codes, year = 2022) {
  # One row per HS4 heading and cell. Joining the HS6-level code list directly repeated a
  # heading once per child HS6 in the basket, so a heading with many mapped HS6 codes
  # weighed more in the mean distance than one with a single code.
  headings <- energy_codes %>%
    dplyr::distinct(tech, supply_chain, sub_sector, code4)

  aec_4_data %>%
    trade_core_filter_year(year = year, label = "AEC HS4 feasibility data") %>%
    trade_core_pad_atlas_codes(width = 4) %>%
    dplyr::left_join(headings, by = c("product_hs92_code" = "code4"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(country_iso3_code, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      feasibility = 1 - mean(distance, na.rm = TRUE),
      .groups = "drop"
    )
}

# Nominal GDP (current USD) per country in the trade year, or the latest earlier year a
# country reports, so a late WDI release does not drop the country from the ratio.
trade_core_pick_gdp <- function(gdp_data, year) {
  require_columns(gdp_data, c("iso3c", "year"), label = "gdp_data")
  gdp_col <- intersect(c("NY.GDP.MKTP.CD", "gdp", "GDP", "value"), names(gdp_data))
  if (length(gdp_col) == 0) {
    stop("gdp_data carries no GDP column (expected NY.GDP.MKTP.CD).")
  }

  gdp_data %>%
    dplyr::transmute(
      iso3c = toupper(as.character(iso3c)),
      gdp_year = suppressWarnings(as.integer(year)),
      gdp = suppressWarnings(as.numeric(.data[[gdp_col[[1]]]]))
    ) %>%
    dplyr::filter(!is.na(gdp), gdp > 0, !is.na(gdp_year), gdp_year <= as.integer(year)) %>%
    dplyr::slice_max(gdp_year, n = 1, with_ties = FALSE, by = iso3c)
}

# Net imports of the basket relative to GDP, the ES trade exposure term. Atlas flows are
# used rather than Comtrade because the Atlas reconciles reporter and partner declarations,
# so it covers ~235 economies against Comtrade's ~143 reporters, and it is the source and
# year of the other country-level ES trade component (market share).
#
# deficit_gdp = (imports - exports) / GDP: positive is a deficit. The index is ranked on its
# negation, so a smaller deficit or a surplus scores as more secure.
trade_core_build_aec_trade_balance <- function(aec_6_data, energy_codes, gdp_data, year = 2023) {
  require_columns(aec_6_data, c("export_value", "import_value"), label = "AEC HS6 trade data")

  flows <- aec_6_data %>%
    trade_core_filter_year(year = year, label = "AEC HS6 trade data") %>%
    trade_core_pad_atlas_codes(width = 6) %>%
    dplyr::left_join(energy_codes, by = c("product_hs92_code" = "code6"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(country_iso3_code, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      basket_exports = sum(as.numeric(export_value), na.rm = TRUE),
      basket_imports = sum(as.numeric(import_value), na.rm = TRUE),
      .groups = "drop"
    )

  flows %>%
    dplyr::left_join(
      trade_core_pick_gdp(gdp_data, year = year),
      by = c("country_iso3_code" = "iso3c"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      deficit = basket_imports - basket_exports,
      deficit_gdp = deficit / gdp
    ) %>%
    dplyr::select(country_iso3_code, tech, supply_chain, sub_sector, deficit, gdp, deficit_gdp)
}

# `exclude_reporters` (ISO3 codes; the re-export hubs) drops those reporters' rows, both flows,
# before anything is summed, so every measure built from this table (exports, imports, RCA and
# its world aggregates, export size, the HHI and its world shares) is without them.
trade_core_build_comtrade_trade <- function(comtrade_data,
                                            energy_codes,
                                            year = 2024,
                                            exclude_reporters = NULL) {
  comtrade_data <- trade_core_filter_year(
    comtrade_data,
    year = year,
    label = "Comtrade energy trade data"
  )

  if (length(exclude_reporters) > 0) {
    comtrade_data <- comtrade_data %>%
      dplyr::filter(!toupper(as.character(reporter_iso)) %in% toupper(as.character(exclude_reporters)))
  }

  if ("partner_desc" %in% names(comtrade_data)) {
    comtrade_data <- comtrade_data %>%
      dplyr::filter(partner_desc == "World")
  } else if ("partner_iso" %in% names(comtrade_data)) {
    comtrade_data <- comtrade_data %>%
      dplyr::filter(partner_iso == "World")
  }

  comtrade_data %>%
    dplyr::mutate(
      cmd_code = stringr::str_pad(as.character(cmd_code), width = 6, side = "left", pad = "0"),
      flow_direction = stringr::str_to_lower(flow_direction)
    ) %>%
    dplyr::filter(flow_direction %in% c("export", "import")) %>%
    dplyr::left_join(energy_codes, by = c("cmd_code" = "code6"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(reporter_iso, tech, supply_chain, sub_sector, flow_direction) %>%
    dplyr::summarize(value = sum(primary_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = flow_direction,
      values_from = value,
      values_fill = 0
    ) %>%
    # "imports" is the name the tidy step reports; the singular never reached the table.
    dplyr::rename(dplyr::any_of(c("exports" = "export", "imports" = "import")))
}

trade_core_build_comtrade_rca <- function(comtrade_trade,
                                          comtrade_total_export,
                                          year = 2024) {
  total_export <- trade_core_filter_year(
    comtrade_total_export,
    year = year,
    label = "Comtrade total export data"
  )

  total_export <- total_export %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(total_exports = sum(primary_value, na.rm = TRUE), .groups = "drop")

  # Both world aggregates are summed over the reporters present in both extracts. The
  # Balassa ratio compares a country's basket share with the world's, and a world basket
  # total over one reporter set divided by a world total over another is neither.
  common_reporters <- intersect(unique(total_export$reporter_iso), unique(comtrade_trade$reporter_iso))

  total_world <- total_export %>%
    dplyr::filter(reporter_iso %in% common_reporters) %>%
    dplyr::summarize(total_world_exports = sum(total_exports, na.rm = TRUE)) %>%
    dplyr::pull(total_world_exports)

  world_exports <- comtrade_trade %>%
    dplyr::filter(reporter_iso %in% common_reporters) %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::summarize(world_exports = sum(exports, na.rm = TRUE), .groups = "drop")

  comtrade_trade %>%
    dplyr::left_join(total_export, by = "reporter_iso") %>%
    dplyr::left_join(world_exports, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::mutate(
      share = exports / total_exports,
      world_share = world_exports / total_world,
      rca = share / world_share
    ) %>%
    dplyr::rename(export_rca = rca)
}

# One HHI per cell over the exporters of the table, and HHI_index = S(-HHI) across cells.
# `rank_techs` (the headline technologies the indices score, config/coalition.yml
# headline_techs) is the ranking universe: their cells are ranked among themselves only, and a
# cell of any other technology (Semiconductors, which the master carries but no index scores)
# gets the value it would take if it alone were added to them (median_scurve_ref()). An
# unscored basket therefore cannot move a scored cell's index, which it did when every cell of
# the file was ranked together (completing the Semiconductors basket moved Batteries Midstream
# from 0.290 to 0.217). NULL ranks every cell together.
trade_core_build_comtrade_hhi <- function(comtrade_trade, rank_techs = NULL) {
  hhi <- comtrade_trade %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::mutate(share_frac = exports / sum(exports, na.rm = TRUE)) %>%
    dplyr::summarize(HHI = sum(share_frac^2, na.rm = TRUE), .groups = "drop")

  if (is.null(rank_techs)) {
    return(dplyr::mutate(hhi, HHI_index = median_scurve(-HHI)))
  }
  # trade_core_build_tidy() renames "Natural Gas" to "Gas"; match either spelling.
  tech_key <- dplyr::if_else(hhi$tech == "Natural Gas", "Gas", hhi$tech)
  in_ref <- tech_key %in% as.character(rank_techs)
  if (nrow(hhi) > 0 && !any(in_ref)) {
    warning(
      "trade_core_build_comtrade_hhi(): no trade cell belongs to `rank_techs`, so every HHI index is NA.",
      call. = FALSE
    )
  }
  dplyr::mutate(hhi, HHI_index = median_scurve_ref(-HHI, in_ref = in_ref))
}

# `trade_balance` (trade_core_build_aec_trade_balance()) is optional so callers that do not
# score the ES exposure term can omit it. `economies` (ISO3 codes, normally country_info's)
# restricts the rankings to the economies the index scores; NULL ranks every code present.
trade_core_build_country_trade <- function(comtrade_trade,
                                           comtrade_total_export,
                                           market_share,
                                           feasibility,
                                           year = 2024,
                                           trade_balance = NULL,
                                           economies = NULL) {
  keys <- c("reporter_iso", "tech", "supply_chain", "sub_sector")

  comtrade_rca <- trade_core_build_comtrade_rca(
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    year = year
  ) %>%
    dplyr::mutate(exports = dplyr::coalesce(exports, 0))

  atlas_tbls <- list(market_share, feasibility, trade_balance)
  atlas_tbls <- atlas_tbls[!vapply(atlas_tbls, is.null, logical(1))]

  # Full joins: the Atlas covers economies that do not report to Comtrade, and their
  # observed Atlas variables are kept rather than being dropped with the Comtrade row they
  # would have been attached to. Comtrade-only variables stay NA for them.
  country_trade <- Reduce(
    function(acc, tbl) {
      dplyr::full_join(
        acc,
        dplyr::rename(tbl, reporter_iso = country_iso3_code),
        by = keys
      )
    },
    atlas_tbls,
    comtrade_rca
  )

  if (!"deficit_gdp" %in% names(country_trade)) {
    country_trade$deficit_gdp <- NA_real_
  }

  # The Atlas also lists "areas not specified" (ANS), uninhabited territories (Antarctica,
  # Bouvet Island, ...) and small dependencies outside the WDI roster. They were ranked and
  # then dropped at the country_info join, so each still took a rank from every economy
  # below it. Raw shares keep their world denominators; only the rankings are restricted.
  if (!is.null(economies)) {
    country_trade <- country_trade %>%
      dplyr::filter(reporter_iso %in% toupper(as.character(economies)))
  }

  # Every index is ranked across countries within the technology-stage cell. Export size
  # and feasibility used to be ranked within each country across its own cells, which put
  # Moldova and Samoa next to China in Solar Midstream because solar was their "best"
  # basket, and mixed within-country and cross-country ranks in one category mean.
  country_trade %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share),
      rca_index = median_scurve(export_rca),
      export_size_index = median_scurve(exports),
      feas_index = median_scurve(feasibility),
      deficit_gdp_index = median_scurve(-deficit_gdp)
    ) %>%
    dplyr::ungroup()
}

trade_core_build_indices <- function(country_trade, hhi_tbl, country_info) {
  # Inner join: a country with no trade row used to come through with tech and stage NA,
  # which reached the ES index as a nameless cell scored 0.
  trade_indices <- country_info %>%
    dplyr::select(country, iso3c) %>%
    dplyr::inner_join(country_trade, by = c("iso3c" = "reporter_iso")) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::distinct()

  trade_indices <- trade_indices %>%
    dplyr::left_join(hhi_tbl, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::arrange(dplyr::desc(export_size_index))

  list(trade_indices = trade_indices, trade_indices_hhi = hhi_tbl)
}

# `data_years` (optional, named: atlas_6, atlas_4, comtrade) is appended to each row's
# explanation. The table carries one Year stamp - the Comtrade year - because the overall
# rows are grouped by Year, but the Atlas components are read at their own data years.
# `hhi_rank_techs` and `reexport_hubs` (as given to trade_category_build_trade()) only word the
# HHI and Comtrade explanations.
trade_core_build_tidy <- function(trade_indices,
                                  year = 2023,
                                  include_sub_sector = FALSE,
                                  data_years = NULL,
                                  hhi_rank_techs = NULL,
                                  reexport_hubs = NULL) {
  year_note <- function(key) {
    if (is.null(data_years) || is.null(data_years[[key]]) || is.na(data_years[[key]])) {
      return("")
    }
    paste0(" [", data_years[[key]], "]")
  }
  atlas_6_note <- year_note("atlas_6")
  atlas_4_note <- year_note("atlas_4")
  comtrade_note <- year_note("comtrade")
  # No square brackets: the "[YYYY]" year notes are parsed back out of explanations.
  hub_note <- if (length(reexport_hubs) > 0) {
    paste0(", re-export hubs (", paste(sort(unique(toupper(reexport_hubs))), collapse = ", "), ") excluded")
  } else {
    ""
  }
  hhi_scope <- if (is.null(hhi_rank_techs)) {
    "across technology-stage cells"
  } else {
    "across the cells of the scored (headline) technologies, other cells placed against them"
  }

  trade_indices %>%
    dplyr::rename(Country = country) %>%
    dplyr::mutate(
      tech = dplyr::if_else(tech == "Natural Gas", "Gas", tech),
      sub_sector = if (isTRUE(include_sub_sector)) {
        dplyr::coalesce(as.character(sub_sector), "All")
      } else {
        "All"
      }
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(c(
        "exports",
        "imports",
        "export_rca",
        "feasibility",
        "market_share",
        "deficit",
        "gdp",
        "deficit_gdp",
        "market_share_index",
        "rca_index",
        "export_size_index",
        "feas_index",
        "deficit_gdp_index",
        "HHI",
        "HHI_index"
      )),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Trade",
      Year = year,
      # Matched on the names after "_index" is stripped; "feas" used to fall through to
      # Comtrade here.
      source = dplyr::case_when(
        variable %in% c("market_share", "feasibility", "feas") ~
          "Harvard Atlas of Economic Complexity",
        variable %in% c("deficit", "gdp", "deficit_gdp") ~
          "Harvard Atlas of Economic Complexity; World Bank WDI",
        TRUE ~ "UN Comtrade"
      ),
      explanation = dplyr::case_when(
        variable == "HHI" & data_type == "raw" ~ paste0(
          "Herfindahl-Hirschman index of world exports of the basket across Comtrade reporters",
          hub_note, "; one value per technology-stage cell", comtrade_note
        ),
        variable == "HHI" ~ paste0(
          "S-curve of -HHI ", hhi_scope, " (less concentrated market scores higher); ",
          "constant across countries within a cell", comtrade_note
        ),
        variable == "market_share" & data_type == "raw" ~ paste0(
          "Share of world exports of the basket's HS6 codes (Atlas)", atlas_6_note
        ),
        variable == "market_share" ~ paste0(
          "S-curve of basket market share across countries within the cell", atlas_6_note
        ),
        variable == "exports" ~ paste0("Exports of the basket to the world, USD", comtrade_note),
        variable == "imports" ~ paste0("Imports of the basket from the world, USD", comtrade_note),
        variable == "export_rca" ~ paste0(
          "Balassa revealed comparative advantage in the basket", hub_note, comtrade_note
        ),
        variable == "rca" ~ paste0("S-curve of Balassa RCA across countries within the cell", comtrade_note),
        variable == "export_size" ~ paste0(
          "S-curve of basket exports across countries within the cell", comtrade_note
        ),
        variable == "feasibility" ~ paste0(
          "1 - mean Atlas product-space distance over the basket's HS4 headings", atlas_4_note
        ),
        variable == "feas" ~ paste0(
          "S-curve of Atlas feasibility across countries within the cell", atlas_4_note
        ),
        variable == "deficit" ~ paste0("Net imports of the basket (imports - exports), USD", atlas_6_note),
        variable == "gdp" ~ paste0("GDP, current USD (WDI; latest year at or before the trade year)", atlas_6_note),
        variable == "deficit_gdp" & data_type == "raw" ~ paste0(
          "Net imports of the basket / GDP; positive is a deficit", atlas_6_note
        ),
        variable == "deficit_gdp" ~ paste0(
          "S-curve of -(net imports / GDP) across countries within the cell; ",
          "a surplus or smaller deficit scores higher", atlas_6_note
        ),
        TRUE ~ variable
      )
    ) %>%
    dplyr::select(
      iso3c,
      Country,
      tech,
      supply_chain,
      sub_sector,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )
}

# Keep the variables one theme scores (TRADE_THEME_VARIABLES). NULL keeps everything.
trade_core_select_theme <- function(tidy, theme = NULL) {
  if (is.null(theme)) {
    return(tidy)
  }
  if (!theme %in% names(TRADE_THEME_VARIABLES)) {
    stop(
      "Unknown trade theme '", theme, "'; expected one of: ",
      paste(names(TRADE_THEME_VARIABLES), collapse = ", ")
    )
  }

  spec <- TRADE_THEME_VARIABLES[[theme]]
  tidy %>%
    dplyr::filter(
      (data_type == "index" & variable %in% spec$index) |
        (data_type == "raw" & variable %in% spec$raw)
    )
}

trade_category_build_trade <- function(subcat,
                                       aec_4_data,
                                       aec_6_data,
                                       comtrade_trade,
                                       comtrade_total_export,
                                       country_info,
                                       year_4 = 2022,
                                       year_6 = 2023,
                                       year_comtrade = 2025,
                                       include_sub_sector = FALSE,
                                       gdp_data = NULL,
                                       theme = NULL,
                                       hhi_rank_techs = NULL,
                                       reexport_hubs = NULL) {
  energy_codes <- trade_core_build_energy_codes(subcat, include_sub_sector = include_sub_sector)

  market_share <- trade_core_build_aec_market_share(
    aec_6_data,
    energy_codes,
    year = year_6
  )

  feasibility <- trade_core_build_aec_feasibility(
    aec_4_data,
    energy_codes,
    year = year_4
  )

  # Re-export hubs leave the Comtrade table here, so they are in neither the numerators nor the
  # world aggregates of RCA (its world totals are summed over reporters present in both
  # extracts, so the hubs' all-product exports drop out too) and the HHI.
  comtrade_trade <- trade_core_build_comtrade_trade(
    comtrade_trade,
    energy_codes,
    year = year_comtrade,
    exclude_reporters = reexport_hubs
  )

  # The ES exposure term needs GDP; without it the builder still produces every other
  # variable, and deficit_gdp is simply absent.
  trade_balance <- if (is.null(gdp_data)) {
    NULL
  } else {
    trade_core_build_aec_trade_balance(aec_6_data, energy_codes, gdp_data = gdp_data, year = year_6)
  }

  country_trade <- trade_core_build_country_trade(
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    market_share = market_share,
    feasibility = feasibility,
    year = year_comtrade,
    trade_balance = trade_balance,
    economies = country_info$iso3c
  )

  hhi_tbl <- trade_core_build_comtrade_hhi(comtrade_trade, rank_techs = hhi_rank_techs)

  indices <- trade_core_build_indices(country_trade, hhi_tbl, country_info)

  trade_core_build_tidy(
    indices$trade_indices,
    year = year_comtrade,
    include_sub_sector = include_sub_sector,
    data_years = list(atlas_6 = year_6, atlas_4 = year_4, comtrade = year_comtrade),
    hhi_rank_techs = hhi_rank_techs,
    reexport_hubs = reexport_hubs
  ) %>%
    dplyr::filter(!is.na(value)) %>%
    trade_core_select_theme(theme = theme) %>%
    energy_security_add_overall_index(include_sub_sector = include_sub_sector)
}
