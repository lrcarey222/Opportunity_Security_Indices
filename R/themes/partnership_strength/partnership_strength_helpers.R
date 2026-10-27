# Helper utilities for partnership strength themes.
#
# Stages. Since 2026-10-27 every partner table keyed by (tech, supply_chain) holds the six
# supply-chain stages (R/utils/supply_chain_stages.R; docs/supply_chain_stages.md): the pillar
# cells are six-stage since 2026-10-26, and the partner trade cells take each HS6 code's six-stage
# `stage` (partnership_strength_build_trade_mapping()). The theme builders are keyed generically
# on supply_chain; scripts/15 stops on a table that holds anything else
# (partnership_strength_assert_stages()).

# The six stages and the roll-up each belongs to, spelled out rather than taken from
# SUPPLY_CHAIN_STAGE_ROLLUP (R/utils/supply_chain_stages.R) so these files still source on their
# own: the dashboard exports (scripts/85, scripts/87) source the partnership builders without the
# stage helpers. tests/testthat/test-partnership-strength.R checks the two are identical.
PARTNERSHIP_STRENGTH_STAGE_ROLLUP <- c(
  Extraction = "Upstream",
  Refining = "Upstream",
  Components = "Midstream",
  Assembly = "Midstream",
  Deployment = "Downstream",
  Recovery = "Upstream"
)

# Stop unless every supply_chain value of a partner table is one of the six stages
# (assert_supply_chain_stages(), R/utils/supply_chain_stages.R, which the caller sources).
# allow_all = TRUE also accepts "All", the supply_chain of the partner-level PSI rows, which are
# collapsed over technology and stage (scripts/25). A table without a supply_chain column passes.
partnership_strength_assert_stages <- function(tbl, label, allow_all = FALSE) {
  if (is.null(tbl) || !"supply_chain" %in% names(tbl)) {
    return(invisible(tbl))
  }
  x <- as.character(tbl$supply_chain)
  if (isTRUE(allow_all)) x <- x[!x %in% "All"]
  assert_supply_chain_stages(x, label = paste0(label, " supply_chain"))
  invisible(tbl)
}

partnership_strength_pick_column <- function(tbl, candidates, label) {
  match <- intersect(candidates, names(tbl))
  if (length(match) == 0) {
    stop("Missing ", label, " column.")
  }
  match[[1]]
}

partnership_strength_safe_scurve <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  median_scurve(x)
}

partnership_strength_min_max_index <- function(x) {
  finite_x <- x[is.finite(x)]
  if (!length(finite_x)) {
    return(rep(NA_real_, length(x)))
  }
  min_x <- min(finite_x, na.rm = TRUE)
  max_x <- max(finite_x, na.rm = TRUE)
  if (isTRUE(all.equal(min_x, max_x))) {
    return(rep(0, length(x)))
  }
  x2 <- x
  x2[x == Inf] <- max_x
  x2[x == -Inf] <- min_x
  (x2 - min_x) / (max_x - min_x)
}

partnership_strength_standardize_bind_rows <- function(tbl) {
  standardize_bind_rows_inputs(tbl)
}

partnership_strength_weighted_mean <- function(values, weights) {
  if (length(values) != length(weights)) {
    stop("Weights must align with values for weighted mean.")
  }
  if (length(values) == 0) {
    return(NA_real_)
  }
  valid <- !is.na(values)
  if (!any(valid)) {
    return(NA_real_)
  }
  sum(values[valid] * weights[valid]) / sum(weights[valid])
}

# Row-wise version of partnership_strength_weighted_mean() over a list of equal-length
# component vectors: each row averages the components it has, with the remaining weights
# renormalized (a missing component is dropped, not scored 0). NA when no weighted
# component is present. Vectorized because the dyad tables run to ~1.5M rows, where
# rowwise() took minutes.
partnership_strength_row_weighted_mean <- function(values, weights) {
  if (length(values) != length(weights)) {
    stop("Weights must align with values for weighted mean.")
  }
  num <- 0
  den <- 0
  for (k in seq_along(values)) {
    x <- suppressWarnings(as.numeric(values[[k]]))
    ok <- !is.na(x)
    num <- num + ifelse(ok, x * weights[[k]], 0)
    den <- den + ok * weights[[k]]
  }
  ifelse(den > 0, num / den, NA_real_)
}

# Mean of the k largest non-missing values; NA when there are none.
partnership_strength_top_k_mean <- function(x, k = 5L) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  mean(sort(x, decreasing = TRUE)[seq_len(min(k, length(x)))])
}

# The 11 headline technologies the ES, EO and Policy pillars report (the `techs` vector in
# scripts/20_build_indices.R). The partner themes rank over the same set, so within-reporter
# ranks and the top-k summaries are not driven by the sparse extra rows the all-tech ES/EO
# tables carry (minerals, Aluminium, Steel, Heat pumps, NA-tech rows ...). A test checks the
# two lists agree.
partnership_strength_headline_techs <- function() {
  c(
    "Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind", "Oil",
    "Solar", "Gas", "Geothermal", "Electric Grid"
  )
}

partnership_strength_filter_techs <- function(tbl, techs = NULL) {
  if (is.null(techs)) {
    return(tbl)
  }
  tbl %>%
    dplyr::filter(tech %in% techs, !is.na(supply_chain))
}

# Fail before writing if a partner table has more than one row per key. The Friendshore table
# once shipped ~2x duplicated (a grouped-tibble leak) and Opportunity with conflicting copies
# (country-name variants); both passed silently into the allied network.
partnership_strength_assert_unique <- function(tbl, keys, label = "partnership table") {
  require_columns(tbl, keys, label = label)
  if (!vctrs::vec_duplicate_any(tbl[keys])) {
    return(invisible(tbl))
  }
  dup_keys <- tbl[keys][vctrs::vec_duplicate_detect(tbl[keys]), , drop = FALSE]
  examples <- utils::head(unique(do.call(paste, c(dup_keys, sep = "|"))), 3)
  stop(
    "Duplicate rows in ", label, " for key (", paste(keys, collapse = ", "), "): ",
    nrow(dup_keys), " rows, e.g. ", paste(examples, collapse = "; ")
  )
}

# Best-k summary ("top5" tables): for each unit (dyad, or partner for development) and
# variable, the mean of the k largest values across its (tech, supply_chain) cells. Each
# variable picks its own k cells, so a component's summary need not come from the cells that
# lead the index. This is an optimistic best-sector summary, not an average relationship.
# Since the six-stage migration (2026-10-27) a unit has up to six cells per technology instead of
# three (66 over the 11 headline technologies, from 33), and k stays 5: the five best cells can
# now come from two stages of one technology's chain where they used to span more technologies,
# and a technology with several strong stages weighs more in the summary than before.
partnership_strength_build_top_k <- function(variable_tbl, id_cols, k = 5L, year) {
  variable_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "category", "variable")))) %>%
    dplyr::summarise(
      value = partnership_strength_top_k_mean(value, k),
      source = dplyr::first(source),
      explanation = dplyr::first(explanation),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      data_type = "index",
      Year = as.integer(year)
    )
}

# Partner-level roll-up of a dyadic index, in the standard Country schema: for each
# (partner, tech, stage), the mean of the dyad index over reporters (self-dyads are excluded
# upstream), with the number of reporters behind it. Written as partner_<theme>_country_tbl,
# which used to be a byte copy of the dyad table.
partnership_strength_build_partner_rollup <- function(variable_tbl,
                                                      index_variable,
                                                      year,
                                                      explanation) {
  variable_tbl %>%
    dplyr::filter(variable == index_variable) %>%
    dplyr::group_by(partner, partner_iso, tech, supply_chain, category, variable) %>%
    dplyr::summarise(
      n_reporters = sum(!is.na(value)),
      value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      Country = partner,
      partner,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type = "index",
      value,
      n_reporters,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = explanation
    )
}

partnership_strength_resolve_weights <- function(weights, defaults, label) {
  if (is.null(weights) || length(weights) == 0) {
    return(defaults)
  }

  # Allow a scalar numeric shorthand (e.g., w = 1) by broadcasting it to all
  # expected components.
  if (is.atomic(weights) && is.null(names(weights)) && length(weights) == 1) {
    return(stats::setNames(as.list(rep(as.numeric(weights), length(defaults))), names(defaults)))
  }

  # Allow unnamed numeric vectors with the same arity as defaults by assigning
  # names positionally.
  if (is.atomic(weights) && is.null(names(weights)) && length(weights) == length(defaults)) {
    weights <- stats::setNames(as.list(as.numeric(weights)), names(defaults))
  }

  # Accept numeric vectors and coerce to list for `$`/`[[` access downstream.
  if (is.atomic(weights) && !is.list(weights)) {
    weights <- as.list(weights)
  }

  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop(label, " weights must be a named list or a scalar numeric value.")
  }

  expected <- names(defaults)
  missing <- setdiff(expected, names(weights))
  extra <- setdiff(names(weights), expected)
  if (length(missing) > 0) {
    stop(
      label,
      " weights missing: ",
      paste(missing, collapse = ", "),
      ". Expected names: ",
      paste(expected, collapse = ", ")
    )
  }
  if (length(extra) > 0) {
    stop(
      label,
      " weights include unexpected entries: ",
      paste(extra, collapse = ", "),
      ". Expected names: ",
      paste(expected, collapse = ", ")
    )
  }

  weights
}

partnership_strength_validate_schema <- function(tbl, label = "partnership theme") {
  validate_schema(tbl)

  invalid_types <- tbl %>%
    dplyr::filter(!data_type %in% c("raw", "index"))
  if (nrow(invalid_types) > 0) {
    stop("Invalid data_type values in ", label, "; expected raw or index only.")
  }

  if (!is.integer(tbl$Year) || any(is.na(tbl$Year))) {
    stop("Year must be integer and non-missing in ", label, ".")
  }

  invisible(tbl)
}

partnership_strength_standardize_countries <- function(country) {
  standardize_country_names(country)
}

partnership_strength_country_to_iso <- function(
    country,
    country_info,
    unresolved_action = c("ignore", "warn", "error")) {
  unresolved_action <- match.arg(unresolved_action)

  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map country names to ISO3 codes.")
  }

  cleaned <- partnership_strength_standardize_countries(country)
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(country, .keep_all = TRUE)

  # match() rather than a join: a name listed twice in the lookup must not add rows.
  mapped <- tibble::tibble(
    country = cleaned,
    iso3c = lookup$iso3c[match(cleaned, lookup$country)]
  )

  # Keep only ISO3 values present in country_info to ensure downstream joins
  # stay within the modeled country universe.
  valid_iso3c <- unique(country_info$iso3c)
  mapped$iso3c <- dplyr::if_else(mapped$iso3c %in% valid_iso3c, mapped$iso3c, NA_character_)

  aggregates <- c(
    "EU",
    "European Union",
    "World",
    "OECD",
    "OPEC",
    "Aggregates"
  )

  # Names outside country_info go through the shared resolver (R/utils/country.R), which returns
  # an ISO3 code for Taiwan and the territories WDI does not list, and NA for aggregates and
  # historical entities. The bare countrycode call used here before read "USSR" as Russia and
  # "Northern Cyprus" as Cyprus, so those rows were pooled with the present-day economy.
  update_idx <- which(is.na(mapped$iso3c) & !is.na(mapped$country) & !mapped$country %in% aggregates)
  if (length(update_idx) > 0) {
    mapped$iso3c[update_idx] <- country_name_to_iso3(mapped$country[update_idx])
  }

  unresolved <- mapped$country[is.na(mapped$iso3c) & !mapped$country %in% aggregates]
  if (length(unresolved) > 0) {
    sample_vals <- paste(utils::head(unique(unresolved), 5), collapse = ", ")
    message <- paste0(
      "Unmapped country names in partnership_strength_country_to_iso: ",
      sample_vals
    )

    if (identical(unresolved_action, "error")) {
      stop(message)
    }
    if (identical(unresolved_action, "warn")) {
      warning(message, call. = FALSE)
    }
  }

  mapped$iso3c
}

partnership_strength_iso_to_country <- function(iso, country_info) {
  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map ISO3 codes to country names.")
  }
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  iso <- as.character(iso)
  out <- lookup$country[match(iso, lookup$iso3c)]

  # country_to_iso() accepts ISO3 codes outside country_info (Taiwan and ~20 territories the
  # ES/EO tables carry). WDI has no name for them, so they came back NA and the PSI, which keys
  # on the partner name, pooled them all into one "NA" partner. Name them with the shared
  # canonical name ("Taiwan"), else countrycode's; the name maps back to the same ISO3.
  missing <- is.na(out) & !is.na(iso)
  if (any(missing)) {
    missing_iso <- unique(iso[missing])
    fallback <- partnership_strength_standardize_countries(country_name_from_iso3(missing_iso))
    out[missing] <- fallback[match(iso[missing], missing_iso)]
  }
  out
}

# Collapse a Country-keyed ES/EO index table to one value per (iso3c, tech, supply_chain).
#
# The upstream country indices carried some economies under two or three spellings ("Viet Nam" /
# "Vietnam", "Slovak Republic" / "Slovakia", "Taiwan" / "Taiwan, Province of China", ...), each
# with its own value. Joined on ISO3 they fanned every dyad cell into 2-36 conflicting rows.
# Rule: drop missing values; prefer the record filed under the canonical spelling (the
# country_info name, else the shared canonical / countrycode name), first as written and then
# after standardize_country_names(); average only among records at the best available level.
# Historical and unrecognised entities ("USSR", "Northern Cyprus") get no ISO3 and are dropped.
# Once the upstream tables carry one spelling per economy this is a no-op safety net.
partnership_strength_country_iso_table <- function(tbl,
                                                   value_col,
                                                   country_info,
                                                   techs = NULL) {
  require_columns(tbl, c("Country", "tech", "supply_chain", value_col), label = value_col)
  tbl <- partnership_strength_filter_techs(tbl, techs)

  raw_name <- stringr::str_squish(as.character(tbl$Country))
  std_name <- partnership_strength_standardize_countries(raw_name)
  keyed <- tibble::tibble(
    raw_name = raw_name,
    std_name = std_name,
    iso3c = partnership_strength_country_to_iso(std_name, country_info),
    tech = as.character(tbl$tech),
    supply_chain = as.character(tbl$supply_chain),
    value = suppressWarnings(as.numeric(tbl[[value_col]]))
  ) %>%
    dplyr::filter(!is.na(iso3c))

  canonical <- partnership_strength_iso_to_country(keyed$iso3c, country_info)
  keyed$priority <- dplyr::case_when(
    keyed$raw_name == canonical ~ 2L,
    keyed$std_name == canonical ~ 1L,
    TRUE ~ 0L
  )
  keyed$priority[is.na(keyed$value)] <- -1L

  keyed %>%
    dplyr::group_by(iso3c, tech, supply_chain) %>%
    dplyr::filter(priority == max(priority)) %>%
    dplyr::summarise(
      !!rlang::sym(value_col) := if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

partnership_strength_clean_outbound_countries <- function(country) {
  recode_map <- c(
    "Curacao" = "Curaçao",
    "Cura ao" = "Curaçao",
    "Turkey" = "Turkiye",
    "United kingdom" = "United Kingdom",
    "Korea, Republic of" = "South Korea",
    "Egypt, Arab Republic of" = "Egypt",
    "Lao People's Democratic Republic" = "Laos",
    "Russian Federation" = "Russia",
    "Czechia" = "Czech Republic",
    "Venezuela, República Bolivariana de" = "Venezuela",
    # IMF DIP spellings that neither WDI nor countrycode recognise.
    "Hong Kong Special Administrative Region, People's Republic of China" = "Hong Kong",
    "Macao Special Administrative Region, People's Republic of China" = "Macau",
    "Aruba, Kingdom of the Netherlands" = "Aruba",
    "Curaçao, Kingdom of the Netherlands" = "Curaçao",
    "Sint Maarten, Kingdom of the Netherlands" = "Sint Maarten (Dutch part)",
    "Kosovo, Republic of" = "Kosovo",
    "Anguilla, United Kingdom-British Overseas Territory" = "Anguilla",
    "Montserrat, United Kingdom-British Overseas Territory" = "Montserrat"
  )

  partnership_strength_standardize_countries(dplyr::recode(country, !!!recode_map, .default = country))
}

# ---- Bilateral trade flows ---------------------------------------------------------------------
#
# The Friendshore and Opportunity trade components read one reconciled flow matrix: for every
# exporter e, importer m, (tech, stage) cell and theme year, the value of e's exports to m.
#
# Source rule (per importer, cell and year):
# 1. Importer's report. When m reports imports in the cell that year (any partner, any of the
#    cell's HS6 codes), its declarations are used for every exporter: m's recorded imports from e,
#    0 where m records none. Importers' declarations are the usual first choice for bilateral flows
#    (customs record imports more carefully than exports, for duties and origin rules).
# 2. Mirror. Otherwise, when e reports exports in the cell that year, e's declared exports to m are
#    used (0 where e records none), multiplied by `cif_fob_factor`: importers value at CIF (cost,
#    insurance and freight to the border), exporters at FOB, so an export declaration understates
#    the same flow on the importer's basis. The default, 1.06, is the IMF Direction of Trade
#    Statistics convention for deriving partner imports from reported exports (Guide to DOTS;
#    DOTS Yearbook 2017). In the all-reporter pull the importers that file both values give a
#    median CIF/FOB ratio of 1.05 (2020 imports). The factor is a constant, so it moves mirror flows
#    against reported ones in the cross-dyad ranks and keeps a dyad's growth from jumping where
#    its source switches between years; it does not change any within-source ranking. A few
#    importers value their own imports FOB (cifvalue 0 in Comtrade: Canada, Mexico and South
#    Africa among the large ones, ~4% of import rows of the 2020/2024 pull); their declarations
#    are used as filed, so their own-report flows sit ~5% low against CIF-valued ones in the
#    cross-dyad (stage-1) ranks only.
# 3. Otherwise the flow is unobserved (NA), and the trade component is imputed downstream.
#
# The choice is made per (importer, cell, year), not per row: a cell never mixes the importer's
# and the exporter's declarations. "Reports in the cell" means at least one record in the cell
# that year, so a reporter that files none of the cell's codes (or a partial pull missing a code
# group) counts as not reporting it rather than as a reported zero. Self-dyads (re-imports) are
# dropped. The economies are those the trade file names as a reporter or partner; a pair with an
# economy outside it is unobserved.

# Constant CIF/FOB factor applied to mirror (exporter-reported, FOB) flows; see above.
PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR <- 1.06

# Which Comtrade dyad file the themes read (config/partnership.yml trade$source), given which
# files exist: "all_reporters" (comtrade_energy_dyads_all.csv, scripts/98) or "allied" (the
# 22-reporter allied_comtrade_energy_data.csv). "auto" prefers the all-reporter pull and falls back
# to the allied file with fallback = TRUE, which the caller must warn about; an explicit choice
# stops when its file is missing. Returns list(source, fallback).
partnership_strength_resolve_trade_source <- function(setting = "auto", all_reporters_exists, allied_exists) {
  setting <- if (is.null(setting) || !nzchar(setting)) "auto" else tolower(as.character(setting))
  if (!setting %in% c("auto", "all_reporters", "allied")) {
    stop("trade source must be auto, all_reporters or allied, not '", setting, "'.")
  }
  if (identical(setting, "all_reporters") && !isTRUE(all_reporters_exists)) {
    stop("trade source all_reporters: comtrade_energy_dyads_all.csv is missing (scripts/98_pull_comtrade_energy_dyads.R).")
  }
  if (identical(setting, "allied") && !isTRUE(allied_exists)) {
    stop("trade source allied: allied_comtrade_energy_data.csv is missing.")
  }
  if (identical(setting, "auto")) {
    if (isTRUE(all_reporters_exists)) return(list(source = "all_reporters", fallback = FALSE))
    if (isTRUE(allied_exists)) return(list(source = "allied", fallback = TRUE))
    stop("No Comtrade dyad file: neither comtrade_energy_dyads_all.csv nor allied_comtrade_energy_data.csv is present.")
  }
  list(source = setting, fallback = FALSE)
}

# A Comtrade dyad table (the all-reporter pull or the 22-reporter allied file, comtradr tidy
# names) reduced to reporter_iso, partner_iso, period, flow_direction ("import" / "export"),
# cmd_code (6 digits) and primary_value. Rows without both ISO3 codes or a value, self-dyads and
# periods outside `years` (all when NULL) are dropped.
partnership_strength_standardize_comtrade <- function(comtrade_dyads, years = NULL) {
  require_columns(comtrade_dyads, c("reporter_iso", "partner_iso"), label = "comtrade_dyads")
  flow_col <- partnership_strength_pick_column(comtrade_dyads, c("flow_direction", "trade_flow", "flow"), "flow")
  period_col <- partnership_strength_pick_column(comtrade_dyads, c("period", "year", "Year", "ref_year"), "period")
  value_col <- partnership_strength_pick_column(comtrade_dyads, c("primary_value", "trade_value", "value"), "trade value")
  code_col <- partnership_strength_pick_column(comtrade_dyads, c("cmd_code", "code", "hs6"), "HS6 code")

  out <- tibble::tibble(
    reporter_iso = as.character(comtrade_dyads$reporter_iso),
    partner_iso = as.character(comtrade_dyads$partner_iso),
    period = suppressWarnings(as.integer(comtrade_dyads[[period_col]])),
    flow_direction = stringr::str_to_lower(as.character(comtrade_dyads[[flow_col]])),
    cmd_code = stringr::str_pad(as.character(comtrade_dyads[[code_col]]), width = 6, side = "left", pad = "0"),
    primary_value = suppressWarnings(as.numeric(comtrade_dyads[[value_col]]))
  )
  keep <- !is.na(out$reporter_iso) & nzchar(out$reporter_iso) &
    !is.na(out$partner_iso) & nzchar(out$partner_iso) &
    out$reporter_iso != out$partner_iso &
    !is.na(out$period) & !is.na(out$primary_value) &
    out$flow_direction %in% c("import", "export")
  if (!is.null(years)) keep <- keep & out$period %in% years
  out[keep, , drop = FALSE]
}

# The reconciled flow matrix at (tech, stage) level for the first and last theme year, from a raw
# Comtrade dyad table and the HS6 crosswalk (rule above). One row per (exporter_iso, importer_iso,
# tech, supply_chain) observed in at least one of the two years:
#   value_first / value_last   flow in min(years) / max(years); NA = unobserved that year, 0 = a
#                              reporter's declarations record no such trade
#   source_first / source_last "importer", "mirror" or NA (unobserved)
# A code that the crosswalk files under several technologies counts in each of them (the cells
# are technology-attributed trade, not a partition); duplicate crosswalk rows count once.
# mirror = FALSE keeps only importers' declarations (no fallback).
partnership_strength_build_trade_cells <- function(comtrade_dyads,
                                                   subcat,
                                                   years = 2020:2024,
                                                   cif_fob_factor = PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR,
                                                   mirror = TRUE) {
  if (!is.numeric(cif_fob_factor) || length(cif_fob_factor) != 1 || is.na(cif_fob_factor) || cif_fob_factor <= 0) {
    stop("cif_fob_factor must be one positive number.")
  }
  y_first <- min(years)
  y_last <- max(years)
  keys <- c("exporter_iso", "importer_iso", "tech", "supply_chain")

  mapping <- partnership_strength_build_trade_mapping(subcat) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::distinct(tech, supply_chain, code)

  std <- partnership_strength_standardize_comtrade(comtrade_dyads, years = c(y_first, y_last))
  economies <- sort(unique(c(std$reporter_iso, std$partner_iso)))

  cells <- std %>%
    dplyr::inner_join(mapping, by = c("cmd_code" = "code"), relationship = "many-to-many") %>%
    dplyr::group_by(reporter_iso, partner_iso, period, flow_direction, tech, supply_chain) %>%
    dplyr::summarise(value = sum(primary_value), .groups = "drop")
  rm(std)

  # Who reports which cell in which year.
  importers <- cells %>%
    dplyr::filter(flow_direction == "import") %>%
    dplyr::distinct(importer_iso = reporter_iso, period, tech, supply_chain)
  exporters <- cells %>%
    dplyr::filter(flow_direction == "export") %>%
    dplyr::distinct(exporter_iso = reporter_iso, period, tech, supply_chain)

  # Observed pairs: every exporter of a reporting importer; for the other importers, every
  # importer of a reporting exporter.
  own <- importers %>%
    tidyr::expand_grid(exporter_iso = economies) %>%
    dplyr::filter(exporter_iso != importer_iso) %>%
    dplyr::left_join(
      cells %>%
        dplyr::filter(flow_direction == "import") %>%
        dplyr::transmute(exporter_iso = partner_iso, importer_iso = reporter_iso, period, tech, supply_chain, value),
      by = c(keys, "period")
    ) %>%
    dplyr::mutate(value = dplyr::coalesce(value, 0), source = "importer")

  observed <- own
  if (isTRUE(mirror)) {
    mirrored <- exporters %>%
      tidyr::expand_grid(importer_iso = economies) %>%
      dplyr::filter(exporter_iso != importer_iso) %>%
      dplyr::anti_join(importers, by = c("importer_iso", "period", "tech", "supply_chain")) %>%
      dplyr::left_join(
        cells %>%
          dplyr::filter(flow_direction == "export") %>%
          dplyr::transmute(exporter_iso = reporter_iso, importer_iso = partner_iso, period, tech, supply_chain, value),
        by = c(keys, "period")
      ) %>%
      dplyr::mutate(value = dplyr::coalesce(value, 0) * cif_fob_factor, source = "mirror")
    observed <- dplyr::bind_rows(own, mirrored)
    rm(mirrored)
  }
  rm(own, cells)

  first <- observed %>%
    dplyr::filter(period == y_first) %>%
    dplyr::select(dplyr::all_of(keys), value_first = value, source_first = source)
  last <- observed %>%
    dplyr::filter(period == y_last) %>%
    dplyr::select(dplyr::all_of(keys), value_last = value, source_last = source)
  out <- dplyr::full_join(first, last, by = keys)
  partnership_strength_assert_unique(out, keys, label = "trade cells")
}

# One direction of the reconciled matrix as the dyad series the trade indices rank:
# view "import" (Friendshore): reporter = importer, partner = exporter; view "export"
# (Opportunity): reporter = exporter, partner = importer. Cells whose last-year flow is unobserved
# are dropped (the builders impute them); growth is defined only where both years are observed
# and the first-year flow is positive.
partnership_strength_trade_view <- function(trade_cells, view = c("import", "export")) {
  view <- match.arg(view)
  require_columns(
    trade_cells,
    c("exporter_iso", "importer_iso", "tech", "supply_chain", "value_first", "value_last"),
    label = "trade_cells"
  )
  reporter <- if (identical(view, "import")) trade_cells$importer_iso else trade_cells$exporter_iso
  partner <- if (identical(view, "import")) trade_cells$exporter_iso else trade_cells$importer_iso
  tibble::tibble(
    reporter_iso = reporter,
    partner_iso = partner,
    tech = trade_cells$tech,
    supply_chain = trade_cells$supply_chain,
    level_last = trade_cells$value_last,
    level_first = trade_cells$value_first
  ) %>%
    dplyr::filter(!is.na(level_last)) %>%
    dplyr::mutate(
      growth = dplyr::if_else(!is.na(level_first) & level_first > 0, (level_last / level_first) - 1, NA_real_)
    )
}

# Growth term of the two-stage trade index (config/partnership.yml trade): growth is scored only
# from a first-year flow of at least this many USD. A ratio from a smaller base is mostly noise in
# the base: the two declarations of one flow can disagree by more than the base itself (2020 US ->
# Kazakhstan, Electric Grid Midstream of the three-stage build: USD 5k in Kazakhstan's import
# declaration, about USD 11m in the US export declaration), and the growth S-curve then ranks such
# a dyad among the fastest growing. Below the floor, and where the first year has no trade, a
# trading dyad takes the neutral growth score (growth_unscored "neutral": 0.5, the median growth
# score) or is scored on its level alone ("level_only", the rule before 2026-10-20). See
# partnership_strength_two_stage_trade_index().
PARTNERSHIP_STRENGTH_GROWTH_MIN_BASE_USD <- 1e6

# The growth settings of the trade index from config/partnership.yml `trade` (growth_min_base_usd,
# growth_unscored), with the defaults above for absent keys. Returns list(min_base_usd, unscored).
partnership_strength_trade_growth_settings <- function(cfg = NULL) {
  out <- list(min_base_usd = PARTNERSHIP_STRENGTH_GROWTH_MIN_BASE_USD, unscored = "neutral")
  if (!is.null(cfg$growth_min_base_usd)) out$min_base_usd <- suppressWarnings(as.numeric(cfg$growth_min_base_usd))
  if (!is.null(cfg$growth_unscored)) out$unscored <- tolower(as.character(cfg$growth_unscored))
  if (length(out$min_base_usd) != 1 || is.na(out$min_base_usd) || out$min_base_usd < 0) {
    stop("trade growth_min_base_usd must be one number >= 0.")
  }
  if (length(out$unscored) != 1 || !out$unscored %in% c("neutral", "level_only")) {
    stop("trade growth_unscored must be neutral or level_only, not '", paste(out$unscored, collapse = ", "), "'.")
  }
  out
}

# The two-stage trade index of a dyad series (Friendshore imports, Opportunity exports).
# Stage 1, within (tech, stage) across dyads that trade (a positive flow in either theme year):
# S-curve of the last-year level and of growth, combined 2:1. Growth is scored (S-curved) only for
# the dyads whose first-year flow is at least trade_growth$min_base_usd; the other trading dyads
# (no first-year trade, or a smaller base) take the neutral growth score 0.5. So a new or
# tiny-base flow is neither ranked among the fastest growing nor scored on its level alone, which
# put it above larger partners with ordinary growth (trade_growth$unscored "level_only" restores
# that rule; with min_base_usd 0 it is the index before 2026-10-20). Stage 2: S-curve within
# (reporter, tech, stage). Dyads observed with no trade in either year (observed zeros) enter
# stage 2 as one tie block below every trading partner, so they take the block's mid-rank, the
# convention the outbound index uses for zero positions. Returns reporter_iso, partner_iso, tech,
# supply_chain, `index_col` and trade_status ("trade" / "zero").
partnership_strength_two_stage_trade_index <- function(ds, index_col, trade_growth = partnership_strength_trade_growth_settings()) {
  require_columns(ds, c("reporter_iso", "partner_iso", "tech", "supply_chain", "level_last", "level_first", "growth"), label = "dyad series")
  trade_growth <- partnership_strength_trade_growth_settings(
    list(growth_min_base_usd = trade_growth$min_base_usd, growth_unscored = trade_growth$unscored)
  )
  neutral_growth <- if (identical(trade_growth$unscored, "neutral")) 0.5 else NA_real_
  ds %>%
    dplyr::mutate(
      trading = (!is.na(level_last) & level_last > 0) | (!is.na(level_first) & level_first > 0),
      growth_scored = trading & !is.na(growth) & !is.na(level_first) & level_first > 0 &
        level_first >= trade_growth$min_base_usd
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      level_idx = partnership_strength_safe_scurve(dplyr::if_else(trading, level_last, NA_real_)),
      growth_idx = partnership_strength_safe_scurve(dplyr::if_else(growth_scored, growth, NA_real_)),
      growth_idx = dplyr::if_else(trading & !growth_scored, neutral_growth, growth_idx),
      # Available-case on both sides: the numerator used to propagate a missing growth, so a dyad
      # with last-year but no first-year trade lost its trade score and was imputed at the mean.
      combined = partnership_strength_row_weighted_mean(list(level_idx, growth_idx), c(2, 1)),
      combined = dplyr::if_else(trading, combined, -1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::mutate(!!index_col := partnership_strength_safe_scurve(combined)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(trade_status = dplyr::if_else(trading, "trade", "zero")) %>%
    dplyr::select(reporter_iso, partner_iso, tech, supply_chain, dplyr::all_of(index_col), trade_status)
}

# Trade indices built elsewhere (or by hand in tests) may lack trade_status: a non-missing index
# then counts as observed trade.
partnership_strength_with_trade_status <- function(trade_indices, index_col) {
  if (!"trade_status" %in% names(trade_indices)) {
    trade_indices$trade_status <- dplyr::if_else(is.na(trade_indices[[index_col]]), NA_character_, "trade")
  }
  trade_indices
}

# ---- Cells without trade data -------------------------------------------------------------------
#
# A (tech, stage) the HS6 crosswalk maps no code to has no bilateral flow at all (trade_status
# "no_data"). In the six-stage basket that is every Deployment cell (it has no basket, as Downstream
# had none), every stage of Green Hydrogen and Geothermal, Solar Extraction, and Recovery outside
# Batteries, Nuclear and Oil: 28 of the 66 headline cells (2026-10-27 master). Its trade term
# used to be the constant 0.5 for every dyad. Within such a cell the other components then ranked
# the reporter's partners on their own. In Opportunity that was the market's energy-security need
# alone, because the reporter's EO is the same for all its markets. Small insecure economies that
# the reporter barely trades with collected five such cells near the top of its list (2026-10-20:
# Equatorial Guinea, Sao Tome and Burundi were the 9th, 11th and 12th of the US's 218 markets).
# The fill rule (config/partnership.yml trade: no_data_fill) sets the trade term of these cells:
#   - "dyad" (default): the dyad's own trade index, the mean over its cells with an observed flow
#     (trade or a recorded zero) in the same technology, else over all its observed cells; a dyad
#     without an observed cell keeps 0.5. Cells imputed at the cell mean are not used: they carry
#     no information on the dyad.
#   - "constant": 0.5 everywhere, the rule before 2026-10-20.
# trade_status stays "no_data" either way: the coverage table counts the cells as before, and the
# imputed weight share leaves their trade term out (below).
PARTNERSHIP_STRENGTH_NO_DATA_FILLS <- c("dyad", "constant")

# The no_data fill rule from config/partnership.yml `trade` (no_data_fill), "dyad" when absent.
partnership_strength_no_data_fill_setting <- function(cfg = NULL) {
  fill <- if (is.null(cfg$no_data_fill)) "dyad" else tolower(as.character(cfg$no_data_fill))
  if (length(fill) != 1 || !fill %in% PARTNERSHIP_STRENGTH_NO_DATA_FILLS) {
    stop(
      "trade no_data_fill must be one of ", paste(PARTNERSHIP_STRENGTH_NO_DATA_FILLS, collapse = ", "),
      ", not '", paste(fill, collapse = ", "), "'."
    )
  }
  fill
}

# Fill the trade index `index_col` of the no_data cells of a dyad table (one row per reporter_iso,
# partner_iso, tech, supply_chain, with trade_status) by the rule `fill`. Adds trade_fill, the
# basis of each no_data cell's trade term: "same_tech" or "all_tech" (the dyad's observed cells it
# averages) or "constant" (0.5 kept); NA in cells with trade data, which are not touched.
partnership_strength_fill_no_data_trade <- function(tbl, index_col, fill = "dyad") {
  require_columns(
    tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "trade_status", index_col),
    label = "dyad cells"
  )
  fill <- partnership_strength_no_data_fill_setting(list(no_data_fill = fill))
  tbl <- dplyr::ungroup(tbl)
  no_data <- tbl$trade_status %in% "no_data"
  tbl$trade_fill <- dplyr::if_else(no_data, "constant", NA_character_)
  if (identical(fill, "constant") || !any(no_data)) {
    return(tbl)
  }

  observed <- tbl$trade_status %in% c("trade", "zero") & !is.na(tbl[[index_col]])
  obs <- tibble::tibble(
    reporter_iso = tbl$reporter_iso[observed],
    partner_iso = tbl$partner_iso[observed],
    tech = tbl$tech[observed],
    x = tbl[[index_col]][observed]
  )
  same_tech <- obs %>%
    dplyr::group_by(reporter_iso, partner_iso, tech) %>%
    dplyr::summarise(fill_same = mean(x), .groups = "drop")
  all_tech <- obs %>%
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarise(fill_all = mean(x), .groups = "drop")
  # One row per no_data cell, in table order (the fill tables are unique on their keys).
  nd <- tibble::tibble(
    reporter_iso = tbl$reporter_iso[no_data],
    partner_iso = tbl$partner_iso[no_data],
    tech = tbl$tech[no_data]
  ) %>%
    dplyr::left_join(same_tech, by = c("reporter_iso", "partner_iso", "tech")) %>%
    dplyr::left_join(all_tech, by = c("reporter_iso", "partner_iso"))

  rows <- which(no_data)
  tbl[[index_col]][rows] <- dplyr::coalesce(nd$fill_same, nd$fill_all, tbl[[index_col]][rows])
  tbl$trade_fill[rows] <- dplyr::case_when(
    !is.na(nd$fill_same) ~ "same_tech",
    !is.na(nd$fill_all) ~ "all_tech",
    TRUE ~ "constant"
  )
  tbl
}

# ---- Imputed weight share (PSI rankability) -----------------------------------------------------
#
# The pillars rank a cell only when at most a share tau of its index weight rests on imputed
# components (config/rankability.yml). The PSI applies the same rule to partners, with this
# measure of how much of a partner's score rests on imputed inputs. Per scored cell (a dyad's
# (tech, stage) for Friendshore and Opportunity, a partner's (tech, stage) for Development):
#
#   imputed_weight_share = sum_c w_c u_c / sum_c w_c
#
# over the cell's partner- and pair-specific components c (weights w_c from config/weights.yml),
# with u_c the unobserved fraction of component c:
#   - 1 for a component that is missing, so the theme's weighted mean drops it and renormalizes
#     over the others (a dyad without an IMF DIP position, a dyad without a geo_alignment value,
#     a Development indicator the partner lacks): its weight rests on the other components;
#   - 1 for a filled component: a trade cell without an observed flow, imputed at the (tech,
#     stage) mean (trade_status "imputed"), and a geo_alignment assigned rather than read from
#     both countries' own UN votes (territories inherit their sovereign's bloc; Taiwan, Palestine
#     and Kosovo are neutral; basis_reporter / basis_partner of geo_alignment_tbl);
#   - the pillar cell's own imputed_weight_share for a component read off the partner's ES or EO
#     cell (Friendshore eo_partner, Opportunity partner_es_need, Development energy_security and
#     economic_opportunity); 0 when the pillar table does not carry the column;
#   - 0 for an observed component.
# Left out of the measure, in numerator and denominator:
#   - the reporter's own attributes (Friendshore es_need and econ_opp_raw, Opportunity
#     econ_opp_index). They are the same for every partner a reporter rates in a (tech, stage),
#     so they cannot make one partner's rank more or less reliable than another's, and counting
#     them would add the reporters' pillar imputation to every partner alike;
#   - the trade component of a (tech, stage) without trade data (trade_status "no_data": no HS6
#     code maps to it). Such a cell has no trade component of its own, as a pillar cell has no
#     category its sources never cover. Its trade term is the dyad's trade in its other cells
#     (partnership_strength_fill_no_data_trade()), whose observation is counted in those cells;
#     counting the fill as imputed would make 21% of the rankable dyads unrankable (2026-10-20);
#   - the climate-policy penalties, which are subtracted from the composite, not weighted in it.
# A dyad's (partner's) share is the mean over the five (tech, stage) cells its top-5 index value
# averages (partnership_strength_top_k_share()); the PSI averages those as it averages the scores
# (psi_imputed_weight_share(), R/indices/build_partnership_strength_index.R).

# Weighted share of unobserved weight. `unobserved` is a list of numeric vectors in [0, 1] (one
# per component, equal lengths): NA marks a component that is not part of the cell. NA where no
# component with a positive weight is part of the cell.
partnership_strength_imputed_weight_share <- function(unobserved, weights) {
  if (length(unobserved) != length(weights)) {
    stop("Weights must align with the unobserved fractions.")
  }
  num <- 0
  den <- 0
  for (k in seq_along(unobserved)) {
    u <- suppressWarnings(as.numeric(unobserved[[k]]))
    w <- as.numeric(weights[[k]])
    ok <- !is.na(u) & !is.na(w) & w > 0
    num <- num + ifelse(ok, w * u, 0)
    den <- den + ifelse(ok, w, 0)
  }
  ifelse(den > 0, num / den, NA_real_)
}

# Unobserved fraction of a trade component from its trade_status: 1 imputed, 0 observed (trade
# or a recorded zero), NA for "no_data" (the cell has no trade component).
partnership_strength_trade_unobserved <- function(trade_status) {
  dplyr::case_when(
    trade_status %in% "no_data" ~ NA_real_,
    trade_status %in% "imputed" ~ 1,
    TRUE ~ 0
  )
}

# Unobserved fraction of a component read off a pillar cell: 1 when the component is missing,
# else the pillar cell's imputed_weight_share (0 when unknown).
partnership_strength_pillar_unobserved <- function(value, pillar_share) {
  pillar_share <- suppressWarnings(as.numeric(pillar_share))
  if (length(pillar_share) == 0) pillar_share <- rep(NA_real_, length(value))
  dplyr::if_else(is.na(value), 1, dplyr::coalesce(pillar_share, 0))
}

# A pillar index table's imputed_weight_share at one value per (iso3c, tech, supply_chain), named
# `name`; NULL when the table does not carry the column (a pillar build before the rankability
# rule), in which case the components read off it count as observed.
partnership_strength_pillar_imputed_share <- function(index_tbl, country_info, name, techs = NULL) {
  if (is.null(index_tbl) || !"imputed_weight_share" %in% names(index_tbl)) {
    return(NULL)
  }
  partnership_strength_country_iso_table(index_tbl, "imputed_weight_share", country_info, techs = techs) %>%
    dplyr::rename(!!name := imputed_weight_share)
}

# Join a pillar share (from partnership_strength_pillar_imputed_share(), possibly NULL) onto `tbl`
# by `iso_col`, tech and supply_chain; adds the column `name` (NA throughout when NULL).
partnership_strength_join_pillar_share <- function(tbl, pillar_share, iso_col, name) {
  if (is.null(pillar_share)) {
    tbl[[name]] <- NA_real_
    return(tbl)
  }
  dplyr::left_join(
    tbl,
    dplyr::rename(pillar_share, !!iso_col := iso3c),
    by = c(iso_col, "tech", "supply_chain")
  )
}

# Mean imputed_weight_share of the k cells with the highest `value_col` for each unit `id_cols`:
# the cells partnership_strength_top_k_mean() averages for that unit's top-5 value (cells without
# a value are skipped; ties at the k-th place are broken by tech and stage order). One row per unit.
partnership_strength_top_k_share <- function(tbl, id_cols, value_col, share_col = "imputed_weight_share", k = 5L) {
  require_columns(tbl, c(id_cols, "tech", "supply_chain", value_col, share_col), label = "top-k share")
  tbl <- tbl[!is.na(tbl[[value_col]]), c(id_cols, "tech", "supply_chain", value_col, share_col), drop = FALSE]
  ord <- do.call(order, c(
    lapply(id_cols, function(col) tbl[[col]]),
    list(-tbl[[value_col]], tbl$tech, tbl$supply_chain)
  ))
  tbl <- tbl[ord, , drop = FALSE]
  unit <- do.call(paste, c(lapply(id_cols, function(col) as.character(tbl[[col]])), sep = "|"))
  pos <- stats::ave(seq_along(unit), unit, FUN = seq_along)
  tbl <- tbl[pos <= k, , drop = FALSE]
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::summarise(!!share_col := mean(.data[[share_col]]), .groups = "drop")
}

# Attach a unit's imputed_weight_share to the index rows of its top-5 table (variable ==
# `index_variable`); component rows get NA (each of them averages its own top cells).
partnership_strength_add_top5_share <- function(top5_tbl, unit_share, id_cols, index_variable) {
  out <- dplyr::left_join(top5_tbl, unit_share, by = id_cols)
  out$imputed_weight_share[out$variable != index_variable] <- NA_real_
  out
}

# Coverage of a dyad table's trade component by (tech, stage): dyad cells scored on an observed
# flow (trade or a recorded zero), cells imputed at the cell mean, and cells of a (tech, stage)
# without trade data (no HS6 code mapped; n_no_data = n_cells there). share_imputed is the
# imputed share of the cells of a (tech, stage) with trade data (NA for one without). The no_data
# cells split by the basis of their trade term (trade_fill, partnership_strength_fill_no_data_trade()):
# the dyad's observed cells in the same technology, in all technologies, or the constant 0.5 (all
# of them when the table has no trade_fill).
partnership_strength_trade_coverage <- function(dyads, theme) {
  require_columns(dyads, c("reporter_iso", "partner_iso", "tech", "supply_chain", "trade_status"), label = "dyads")
  if (!"trade_fill" %in% names(dyads)) {
    dyads$trade_fill <- dplyr::if_else(dyads$trade_status %in% "no_data", "constant", NA_character_)
  }
  dyads %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(
      n_cells = dplyr::n(),
      n_trade = sum(trade_status %in% "trade"),
      n_zero = sum(trade_status %in% "zero"),
      n_imputed = sum(trade_status %in% "imputed"),
      n_no_data = sum(trade_status %in% "no_data"),
      n_no_data_same_tech = sum(trade_status %in% "no_data" & trade_fill %in% "same_tech"),
      n_no_data_all_tech = sum(trade_status %in% "no_data" & trade_fill %in% "all_tech"),
      n_no_data_constant = sum(trade_status %in% "no_data" & trade_fill %in% "constant"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      theme = theme,
      share_imputed = dplyr::if_else(n_cells > n_no_data, n_imputed / (n_cells - n_no_data), NA_real_)
    ) %>%
    dplyr::relocate(theme)
}

# The HS6 crosswalk as (tech, supply_chain, sub_sector, code) rows, the cells the trade builders
# key on, read on the crosswalk's supply_chain (or Value.Chain) column. The HS6 master
# (data/reference/energy_hs6_master.csv, staged to data/raw by scripts/04) carries the three-stage
# supply_chain next to the six-stage `stage`; the six-stage readers (scripts/15, scripts/30) copy
# `stage` into supply_chain before they call this, so the partner trade cells are keyed (tech,
# stage) like the pillar cells they are joined to, while readers not yet moved (the Sector export,
# scripts/87; the paper figures) still get the three-stage cells. Blank values are NA (the trade
# builders drop them; scripts/15 stops on them).
partnership_strength_build_trade_mapping <- function(subcat, include_sub_sector = FALSE) {
  tech_col <- partnership_strength_pick_column(
    subcat,
    c("tech", "Technology", "technology"),
    "technology"
  )
  supply_chain_col <- partnership_strength_pick_column(
    subcat,
    c("supply_chain", "Value.Chain", "value_chain", "Supply.Chain", "supply chain"),
    "supply_chain"
  )
  sub_sector_col <- intersect(c("sub_sector", "Sub.Sector", "subsector"), names(subcat))
  code_col <- partnership_strength_pick_column(subcat, c("code", "HS6", "hs6", "HS_6", "cmd_code"), "HS6")
  # Resolved outside transmute(): the inline `if (...) .data[[sub_sector_col[[1]]]]` it replaces
  # errored (subscript out of bounds) on a crosswalk without a sub_sector column.
  sub_sector_values <- if (length(sub_sector_col) > 0) as.character(subcat[[sub_sector_col[[1]]]]) else rep(NA_character_, nrow(subcat))

  subcat %>%
    dplyr::transmute(
      tech = as.character(.data[[tech_col]]),
      supply_chain = dplyr::na_if(stringr::str_squish(as.character(.data[[supply_chain_col]])), ""),
      sub_sector = sub_sector_values,
      code = stringr::str_pad(as.character(.data[[code_col]]), width = 6, side = "left", pad = "0")
    ) %>%
    dplyr::filter(!is.na(code), nzchar(code)) %>%
    dplyr::mutate(
      sub_sector = if (isTRUE(include_sub_sector)) dplyr::coalesce(sub_sector, "All") else "All"
    ) %>%
    dplyr::distinct()
}

# Lifecycle GHG intensity of oil-fired electricity, gCO2eq/kWh: the median of the IPCC
# literature review in SRREN (Moomaw et al. 2011, Annex II, Table A.II.4, p. 982;
# https://www.ipcc.ch/site/assets/uploads/2018/03/Annex-II-Methodology-1.pdf, accessed
# 2026-10-07; min 510, quartiles 722 / 907, max 1170). Every other row of
# data/raw/ipcc_ghg_intensity.csv is a lifecycle median from AR5 WGIII Annex III,
# Table A.III.2 (p. 1335), which has no oil-fired technology. The staged table's oil value,
# 720, is in neither table (it is close to the SRREN 25th percentile), and the file is
# staged from SharePoint, so it is corrected here.
IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH <- 840

partnership_strength_clean_ghg <- function(tech_ghg_raw) {
  require_columns(tech_ghg_raw, c("Tech", "ghg_intensity"), label = "tech_ghg_raw")
  
  base <- tech_ghg_raw %>%
    dplyr::mutate(
      # normalize unicode dashes to plain hyphen
      Tech = stringr::str_replace_all(Tech, "[\u2013\u2014]", "-"),
      Tech = stringr::str_squish(Tech),
      Tech = dplyr::case_when(
        Tech == "Coal (pulverised)" ~ "Coal",
        Tech == "Natural-gas combined cycle" ~ "Gas",
        Tech == "Oil-fired steam" ~ "Oil",
        stringr::str_detect(Tech, "^Solar PV") ~ "Solar",
        stringr::str_detect(Tech, "Concentrated solar power") ~ "Solar",
        stringr::str_detect(Tech, "^Wind") ~ "Wind",
        Tech == "Nuclear (light-water)" ~ "Nuclear",
        TRUE ~ Tech
      ),
      ghg_intensity = dplyr::if_else(Tech == "Oil", IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH, as.numeric(ghg_intensity))
    ) %>%
    # Geothermal has its own IPCC row (38 gCO2e/kWh); it used to be filtered out here and
    # imputed with the Solar value below.
    dplyr::filter(Tech %in% c("Coal","Gas","Oil","Solar","Nuclear","Wind","Geothermal")) %>%
    dplyr::mutate(ghg_index = partnership_strength_min_max_index(-ghg_intensity)) %>%
    dplyr::transmute(tech = Tech, ghg_index) %>%
    dplyr::group_by(tech) %>%
    dplyr::summarise(ghg_index = mean(ghg_index, na.rm = TRUE), .groups = "drop")

  get_idx <- function(df, t) {
    x <- df$ghg_index[df$tech == t]
    if (length(x) == 0 || all(is.na(x))) NA_real_ else x[1]
  }

  safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

  extra <- tibble::tibble(
    tech = c("Batteries", "Electric Vehicles", "Green Hydrogen", "Geothermal", "Electric Grid"),
    ghg_index = c(
      get_idx(base, "Solar"),
      safe_mean(base$ghg_index[base$tech %in% c("Coal","Oil","Gas","Solar","Wind","Nuclear")]),
      safe_mean(base$ghg_index[base$tech %in% c("Gas","Solar")]),
      get_idx(base, "Solar"),
      safe_mean(base$ghg_index[base$tech %in% c("Solar","Wind")])
    )
  ) %>%
    # Proxies only for technologies without their own IPCC row.
    dplyr::filter(!tech %in% base$tech)

  dplyr::bind_rows(base, extra)
}


partnership_strength_clean_policy <- function(cat_raw, country_info = NULL) {
  
  cleaned <- cat_raw %>%
    dplyr::mutate(
      climate_policy_index = dplyr::case_when(
        Overall.rating == "Critically insufficient" ~ 0.25,
        Overall.rating == "Highly insufficient"     ~ 0.5,
        Overall.rating == "Insufficient"            ~ 0.75,
        Overall.rating == "Almost Sufficient"       ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::transmute(Country = Country, climate_policy_index = climate_policy_index)
  
  # standardize country names + (optionally) attach iso3c and filter to valid iso3c
  standardize_country_table(cleaned, country_info = country_info)
}
