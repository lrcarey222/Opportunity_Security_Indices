# EIA International Energy Statistics: parse the bulk file, convert its fossil fuel series to
# the EI Statistical Review's units, and supply them where EI reports no row for an economy.
#
# Why: EI's production tables list fewer economies than its consumption tables and fold the rest
# into "Other <region>" residuals, so an absent EI production row means "none, or some inside a
# residual", not 0. The themes read it as 0 until 2026-10-11, then as NA, which left 122 ES
# Upstream fossil cells of near-total importers (Japan, South Korea, German oil, ...) to the
# pillar's global-average fill. User decision of 2026-10-11: where EI has no row for an economy
# and fuel, take the EIA value of the same year in EI's units, flagged with its source, as
# observed data; an EIA zero, or no production series at all for an economy EIA covers, is a
# real zero. EI stays the source wherever it reports. Two departures, both documented in the
# README: gas and coal, which EIA has published only to 2024, take EIA's latest year within one
# year of EI's (eia_intl_lookup(max_lag)); and a value whose fuel is not EI's fuel fails the
# comparability check (EIA_INTL_COMPARABILITY; Estonia coal, where EI counts oil shale) and is
# not used.
#
# Pure (no IO). scripts/fetchers/eia_intl.R downloads https://www.eia.gov/opendata/bulk/INTL.zip
# and stages eia_intl_parse_lines() as data/raw/eia_intl_fossil_production.csv; scripts/10 and
# scripts/40 pass that table to the import dependence and production builders as `eia`.

EIA_INTL_SOURCE <- "EIA International Energy Statistics"
EIA_INTL_BULK_URL <- "https://www.eia.gov/opendata/bulk/INTL.zip"

# The staged series. Bulk series ids read INTL.<product>-<activity>-<geography>-<unit>.A (A =
# annual; activity 1 = production, 2 = consumption). Products: 57 crude oil including lease
# condensate, 58 natural gas plant liquids (NGPL), 26 dry natural gas, 7 coal (anthracite,
# bituminous, subbituminous and lignite; coke is product 21), 5 petroleum and other liquids.
eia_intl_series_spec <- function() {
  spec <- tibble::tribble(
    ~fuel,  ~flow,         ~component,         ~product, ~activity, ~unit,
    "oil",  "production",  "crude_condensate", 57L,      1L,        "TBPD",
    "oil",  "production",  "ngpl",             58L,      1L,        "TBPD",
    "gas",  "production",  "dry_gas",          26L,      1L,        "BCM",
    "coal", "production",  "coal",             7L,       1L,        "TJ",
    "oil",  "consumption", "petroleum",        5L,       2L,        "TBPD",
    "oil",  "consumption", "petroleum",        5L,       2L,        "TJ",
    "gas",  "consumption", "dry_gas",          26L,      2L,        "BCM",
    "coal", "consumption", "coal",             7L,       2L,        "TJ"
  )
  spec$series_prefix <- paste0("INTL.", spec$product, "-", spec$activity, "-")
  spec
}

# Columns of the staged extract, in order.
EIA_INTL_COLUMNS <- c(
  "iso3", "country", "fuel", "flow", "component", "year", "value", "unit", "series_id",
  "value_note", "last_updated"
)

# Tidy rows of the wanted series from the bulk file's lines (one JSON object per series).
#
# The lines are untrusted data: each candidate is parsed as JSON and kept only if its series_id
# matches INTL.<product>-<activity>-<ISO3>-<unit>.A for a (product, activity, unit) of `spec`.
# Four-letter geographies (WORL, OECD, regions) and monthly or quarterly series are not kept.
# A value EIA does not give as a number ("--", "NA", "ie", "w") is NA with the code in
# `value_note`, never 0. `min_year` drops earlier years.
eia_intl_parse_lines <- function(lines, spec = eia_intl_series_spec(), min_year = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to parse the EIA bulk file.")
  }
  lines <- as.character(lines)
  # Cheap fixed-string prefilter; the full id is checked after parsing.
  wanted <- rep(FALSE, length(lines))
  for (prefix in unique(spec$series_prefix)) {
    wanted <- wanted | grepl(paste0("\"series_id\":\"", prefix), lines, fixed = TRUE)
  }
  rows <- lapply(lines[wanted], eia_intl_parse_record, spec = spec)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  out <- if (length(rows) == 0) eia_intl_empty() else dplyr::bind_rows(rows)
  if (!is.null(min_year)) {
    out <- out[out$year >= as.integer(min_year), , drop = FALSE]
  }
  out <- out[order(out$series_id, out$year), EIA_INTL_COLUMNS, drop = FALSE]
  rownames(out) <- NULL
  out
}

eia_intl_empty <- function() {
  tibble::tibble(
    iso3 = character(), country = character(), fuel = character(), flow = character(),
    component = character(), year = integer(), value = numeric(), unit = character(),
    series_id = character(), value_note = character(), last_updated = character()
  )
}

# One series (one line of the bulk file) as tidy rows, or NULL when it is not a wanted series.
eia_intl_parse_record <- function(line, spec = eia_intl_series_spec()) {
  rec <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(rec) || !is.character(rec$series_id) || length(rec$series_id) != 1) {
    return(NULL)
  }
  id <- rec$series_id
  parts <- regmatches(id, regexec("^INTL\\.([0-9]+)-([0-9]+)-([A-Z]{3})-([A-Z]+)\\.A$", id))[[1]]
  if (length(parts) != 5) {
    return(NULL)
  }
  hit <- spec[spec$product == as.integer(parts[2]) & spec$activity == as.integer(parts[3]) &
                spec$unit == parts[5], , drop = FALSE]
  if (nrow(hit) != 1 || !is.list(rec$data) || length(rec$data) == 0) {
    return(NULL)
  }

  scalar <- function(x) if (length(x) == 1 && (is.character(x) || is.numeric(x))) x else NA
  year <- vapply(rec$data, function(p) as.character(scalar(p[[1]])), character(1))
  raw <- lapply(rec$data, function(p) if (length(p) >= 2) scalar(p[[2]]) else NA)
  value <- vapply(raw, function(v) if (is.numeric(v)) as.numeric(v) else suppressWarnings(as.numeric(v)), numeric(1))
  note <- vapply(raw, function(v) if (is.numeric(v) || is.na(v)) "" else as.character(v), character(1))
  note[is.na(value) & !nzchar(note)] <- "NA"
  note[!is.na(value)] <- ""
  keep <- grepl("^[0-9]{4}$", year)
  if (!any(keep)) {
    return(NULL)
  }

  tibble::tibble(
    iso3 = parts[4],
    country = eia_intl_country_name(if (is.character(rec$name)) rec$name else NA_character_),
    fuel = hit$fuel,
    flow = hit$flow,
    component = hit$component,
    year = as.integer(year[keep]),
    value = value[keep],
    unit = hit$unit,
    series_id = id,
    value_note = note[keep],
    last_updated = if (is.character(rec$last_updated)) rec$last_updated[1] else NA_character_
  )
}

# The geography of a series name, "<description>, <geography>, Annual". The staged series'
# descriptions have no comma ("Dry natural gas production"); a geography may ("Korea, South").
eia_intl_country_name <- function(name) {
  out <- sub(", Annual$", "", name)
  sub("^[^,]*, ", "", out)
}

# Conversion of the staged series to the EI variables the themes read.
#
#   oilprod_kbd  thousand b/d: crude oil incl. lease condensate (57) + NGPL (58). EI's oil
#                production is crude, shale oil, oil sands, condensates and NGLs, and excludes
#                biofuels, coal- and gas-to-liquids and refinery gains, which EIA puts in "other
#                liquids" (59) and "refinery processing gain" (56), not staged. NGPL is added
#                when EIA gives it; crude is required.
#   gasprod_ej   EJ: dry natural gas (26), bcm x 0.036 EJ/bcm, the factor EI applies to its own
#                bcm (every EI row has gasprod_ej / gasprod_bcm = 0.036). Dry gas excludes flared,
#                vented and reinjected gas, as EI's production does.
#   coalprod_ej  EJ: coal (7), TJ x 1e-6 x 0.95. EIA's heat contents are gross calorific values
#                and EI's EJ are net; 0.95 is the conventional net/gross ratio for coal (IEA, UN).
#   oilcons_kbd  thousand b/d: petroleum and other liquids consumption (5).
#   oil_tes_ej   EJ: the same in TJ x 1e-6 x 0.95 (net/gross for oil products).
#   gas_tes_ej   EJ: dry natural gas consumption (26), bcm x 0.036.
#   coal_tes_ej  EJ: coal consumption (7), TJ x 1e-6 x 0.95.
#
# Checked against economies both sources report (2020 and 2024; scratch notes of 2026-10-11):
# median EI/EIA ratio 1.006-1.011 for oil production, 0.97-0.99 for gas, 0.99-1.05 for coal
# (wide spread: EI uses its own calorific values), 0.98 for oil consumption in b/d.
eia_intl_conversions <- function() {
  tibble::tribble(
    ~ei_var,       ~fuel,  ~flow,         ~component,         ~unit,  ~factor,  ~required,
    "oilprod_kbd", "oil",  "production",  "crude_condensate", "TBPD", 1,        TRUE,
    "oilprod_kbd", "oil",  "production",  "ngpl",             "TBPD", 1,        FALSE,
    "gasprod_ej",  "gas",  "production",  "dry_gas",          "BCM",  0.036,    TRUE,
    "coalprod_ej", "coal", "production",  "coal",             "TJ",   0.95e-6,  TRUE,
    "oilcons_kbd", "oil",  "consumption", "petroleum",        "TBPD", 1,        TRUE,
    "oil_tes_ej",  "oil",  "consumption", "petroleum",        "TJ",   0.95e-6,  TRUE,
    "gas_tes_ej",  "gas",  "consumption", "dry_gas",          "BCM",  0.036,    TRUE,
    "coal_tes_ej", "coal", "consumption", "coal",             "TJ",   0.95e-6,  TRUE
  )
}

# The EI consumption variable (EJ) of each production variable, for the comparability check of
# eia_intl_lookup().
EIA_INTL_CONSUMPTION_OF <- c(oilprod_kbd = "oil_tes_ej", gasprod_ej = "gas_tes_ej", coalprod_ej = "coal_tes_ej")

# Comparability check of an EIA production value (eia_intl_lookup()). The value is not used when
# EIA's consumption of the fuel covers less than `min_coverage` of EI's consumption for the same
# economy and year, and the part EIA leaves out exceeds `min_gap_ej` EJ: EI then counts under
# the fuel something EIA's series exclude, so EIA's production is not production of EI's fuel.
# In 2015-2025 this is Estonia coal only. EI's Estonian coal consumption (0.09-0.15 EJ) is about
# 1,000 times EIA's, because EI counts oil shale, which Estonia mines itself, as coal and EIA's
# coal series do not, so EIA's coal production of 0 would score Estonia as a total importer. On
# the other gap cells EIA covers 83-120% of EI's oil and gas consumption (5th-95th percentile),
# and 2-150% of its coal consumption, the low end all tiny consumers (below 0.01 EJ apart);
# scratch notes of 2026-10-11 (fixes8).
EIA_INTL_COMPARABILITY <- list(min_coverage = 0.1, min_gap_ej = 0.01)

# How each EI variable was converted, for the explanation text.
eia_intl_conversion_text <- function(ei_var) {
  text <- c(
    oilprod_kbd = "crude oil incl. lease condensate plus NGPL, thousand b/d",
    gasprod_ej = "dry natural gas, bcm x 0.036 EJ/bcm",
    coalprod_ej = "coal, gross TJ x 0.95 to net EJ",
    oilcons_kbd = "petroleum and other liquids consumption, thousand b/d",
    oil_tes_ej = "petroleum and other liquids consumption, gross TJ x 0.95 to net EJ",
    gas_tes_ej = "dry natural gas consumption, bcm x 0.036 EJ/bcm",
    coal_tes_ej = "coal consumption, gross TJ x 0.95 to net EJ"
  )
  unname(text[ei_var])
}

# The staged extract with its column types checked and coerced (read.csv() leaves them loose).
eia_intl_check_extract <- function(eia) {
  require_columns(
    eia,
    c("iso3", "fuel", "flow", "component", "year", "value", "unit", "series_id"),
    label = "EIA International Energy Statistics extract"
  )
  eia <- tibble::as_tibble(eia)
  for (col in c("iso3", "fuel", "flow", "component", "unit", "series_id")) {
    eia[[col]] <- as.character(eia[[col]])
  }
  eia$year <- suppressWarnings(as.integer(eia$year))
  eia$value <- suppressWarnings(as.numeric(eia$value))
  dupes <- duplicated(eia[, c("series_id", "year")])
  if (any(dupes)) {
    stop("EIA International Energy Statistics extract has duplicate (series_id, year) rows, e.g. ",
         paste(utils::head(unique(eia$series_id[dupes]), 3), collapse = ", "))
  }
  eia[!is.na(eia$year), , drop = FALSE]
}

# One value per (iso3, ei_var, year) in EI's units. NA when EIA has the series but not a number
# for the year, or lacks a required component.
eia_intl_ei_units <- function(eia, ei_vars = NULL) {
  conv <- eia_intl_conversions()
  if (!is.null(ei_vars)) {
    conv <- conv[conv$ei_var %in% ei_vars, , drop = FALSE]
  }
  joined <- dplyr::inner_join(eia_intl_check_extract(eia), conv, by = c("fuel", "flow", "component", "unit"))
  out <- dplyr::summarize(
    dplyr::group_by(joined, iso3, ei_var, year),
    complete = any(required) && all(!is.na(value[required])),
    series_id = paste(series_id[!is.na(value)], collapse = " + "),
    value = sum(value * factor, na.rm = TRUE),
    .groups = "drop"
  )
  out$value[!out$complete] <- NA_real_
  out$series_id[!out$complete] <- NA_character_
  out[, c("iso3", "ei_var", "year", "value", "series_id")]
}

# The EIA value for each requested cell: `cells` has iso3, ei_var and year (the EI year the value
# stands in for). Returns the cells with
#   value      in EI units, NA when EIA has none;
#   eia_year   the EIA year used: the cell's year, or when EIA has not published it yet (gas and
#              coal end a year before EI) the latest earlier year within `max_lag` years;
#   series_id  the EIA series summed;
#   eia_basis  "series", "no_series" (an economy EIA covers - one with any staged value - but no
#              series for the variable: no production, a real 0), "not_comparable" (see below;
#              value NA) or NA;
#   eia_consumption_coverage  EIA's consumption of the fuel over `ei_consumption` (NA without it).
#
# `ei_consumption` (optional, EJ, one per row of `cells`): EI's consumption of the cell's fuel.
# With it, a production value fails the comparability check of EIA_INTL_COMPARABILITY (`comparability`)
# when EIA's consumption of the fuel in the EIA year used covers too little of EI's.
eia_intl_lookup <- function(cells, eia, max_lag = 1L, ei_consumption = NULL,
                            comparability = EIA_INTL_COMPARABILITY) {
  require_columns(cells, c("iso3", "ei_var", "year"), label = "cells")
  max_lag <- as.integer(max_lag)
  if (length(max_lag) != 1 || is.na(max_lag) || max_lag < 0) {
    stop("max_lag must be one non-negative number of years.")
  }
  if (!is.null(ei_consumption) && length(ei_consumption) != nrow(cells)) {
    stop("ei_consumption must have one value per row of cells.")
  }
  cells <- tibble::as_tibble(cells)
  cells$iso3 <- as.character(cells$iso3)
  cells$ei_var <- as.character(cells$ei_var)
  cells$year <- as.integer(cells$year)
  units <- eia_intl_ei_units(eia)
  if (nrow(cells) == 0) {
    cells$value <- numeric()
    cells$series_id <- character()
    cells$eia_year <- integer()
    cells$eia_basis <- character()
    cells$eia_consumption_coverage <- numeric()
    return(cells)
  }

  # The latest EIA year with a value in [year - max_lag, year], per cell.
  valued <- units[!is.na(units$value), , drop = FALSE]
  names(valued)[names(valued) == "year"] <- "eia_year"
  candidates <- dplyr::inner_join(
    dplyr::distinct(cells, iso3, ei_var, year),
    valued,
    by = c("iso3", "ei_var"),
    relationship = "many-to-many"
  )
  candidates <- candidates[candidates$eia_year <= candidates$year &
                             candidates$eia_year >= candidates$year - max_lag, , drop = FALSE]
  candidates <- candidates[order(candidates$iso3, candidates$ei_var, candidates$year, -candidates$eia_year), , drop = FALSE]
  candidates <- candidates[!duplicated(candidates[, c("iso3", "ei_var", "year")]), , drop = FALSE]
  candidates$eia_basis <- rep("series", nrow(candidates))

  out <- dplyr::left_join(cells, candidates, by = c("iso3", "ei_var", "year"))
  covered <- unique(valued$iso3)
  no_series <- is.na(out$eia_basis) & out$iso3 %in% covered &
    !paste(out$iso3, out$ei_var) %in% paste(units$iso3, units$ei_var)
  out$value[no_series] <- 0
  out$eia_year[no_series] <- out$year[no_series]
  out$eia_basis[no_series] <- "no_series"

  out$eia_consumption_coverage <- NA_real_
  if (!is.null(ei_consumption)) {
    cons_var <- unname(EIA_INTL_CONSUMPTION_OF[out$ei_var])
    eia_cons <- units$value[match(
      paste(out$iso3, cons_var, out$eia_year),
      paste(units$iso3, units$ei_var, units$year)
    )]
    ei_cons <- as.numeric(ei_consumption)
    out$eia_consumption_coverage <- dplyr::if_else(ei_cons > 0, eia_cons / ei_cons, NA_real_)
    not_comparable <- !is.na(out$value) & !is.na(out$eia_consumption_coverage) &
      out$eia_consumption_coverage < comparability$min_coverage &
      ei_cons - eia_cons > comparability$min_gap_ej
    out$value[not_comparable] <- NA_real_
    out$eia_basis[not_comparable] <- "not_comparable"
  }
  out
}

# ISO3 code of each EI economy (standardized name), from the panel's ISO3166_alpha3 column.
ei_iso3_lookup <- function(ei, iso3_col = "ISO3166_alpha3") {
  require_columns(ei, c("Country", iso3_col), label = "ei")
  pairs <- unique(data.frame(
    Country = standardize_country_names(as.character(ei$Country)),
    iso3 = toupper(trimws(as.character(ei[[iso3_col]]))),
    stringsAsFactors = FALSE
  ))
  pairs <- pairs[!is.na(pairs$iso3) & grepl("^[A-Z]{3}$", pairs$iso3), , drop = FALSE]
  clash <- unique(pairs$Country[duplicated(pairs$Country)])
  if (length(clash) > 0) {
    stop("EI economies with more than one ISO3 code: ", paste(clash, collapse = ", "))
  }
  tibble::as_tibble(pairs)
}

# Why an EIA production value failed the comparability check (eia_basis "not_comparable"):
# `coverage` is eia_consumption_coverage, one value per element of `fuel`, or several for one
# fuel (then given as a range).
eia_intl_not_comparable_text <- function(fuel, coverage) {
  pct <- function(x) format(signif(100 * x, 2), scientific = FALSE, trim = TRUE)
  share <- if (length(fuel) == 1 && length(coverage) > 1) {
    rng <- range(coverage, na.rm = TRUE)
    if (rng[1] == rng[2]) pct(rng[1]) else paste0(pct(rng[1]), "-", pct(rng[2]))
  } else {
    pct(coverage)
  }
  paste0(
    "the ", EIA_INTL_SOURCE, " value is not used: EIA's ", tolower(fuel), " consumption is ",
    share, "% of EI's, so EI counts as ", tolower(fuel), " a fuel EIA's series leave out (Estonia: oil shale)"
  )
}

# The explanation text of a value taken from EIA. `fuel` is the technology label ("Oil", "Gas",
# "Coal"), `flow` "production" or "consumption".
eia_intl_fill_note <- function(fuel, ei_var, eia_year, target_year, eia_basis = "series", flow = "production") {
  year_txt <- dplyr::if_else(
    eia_year == target_year,
    as.character(target_year),
    paste0(eia_year, ", the latest EIA year, for ", target_year)
  )
  what <- dplyr::if_else(
    eia_basis == "no_series",
    paste0("no ", tolower(fuel), " ", flow, " series for this economy, so 0"),
    eia_intl_conversion_text(ei_var)
  )
  paste0(
    "EI does not report ", tolower(fuel), " ", flow, " for this economy separately; ", EIA_INTL_SOURCE,
    " (", year_txt, "): ", what
  )
}
