# Energy import dependence theme builder functions.

# EI production variables of the theme, by fuel, and the consumption variables it reads.
IMPORT_DEPENDENCE_PRODUCTION_VARS <- c(oil = "oilprod_kbd", gas = "gasprod_ej", coal = "coalprod_ej")
IMPORT_DEPENDENCE_CONSUMPTION_VARS <- c("oil_tes_ej", "oilcons_kbd", "gas_tes_ej", "coal_tes_ej")

# `eia`: the staged EIA International Energy Statistics extract
# (data/raw/eia_intl_fossil_production.csv; R/utils/eia_intl.R), or NULL. With it, a production
# value EI does not report for an economy on the theme's roster is taken from EIA (same year;
# gas and coal, which EIA publishes a year after oil, from the latest EIA year within
# `eia_max_lag` years) and recorded in <var>_eia_year and <var>_eia_basis. A value whose fuel is
# not EI's (EIA_INTL_COMPARABILITY: 2025 Estonia coal, where EI counts oil shale) is not used.
# Where such a value is of an earlier year than the other side of the fuel's ratio, production
# and consumption are then set against each other in one year (import_dependence_pair_years()).
#
# `eia_fill_consumption` (default FALSE here; scripts/10 sets it TRUE since 2026-10-11): also take the oil,
# gas and coal consumption of economies EI lists with a production row but no oil consumption
# row (Angola, Nigeria, Libya, Guyana, ...) from EIA, which puts them on the theme's roster.
import_dependence_clean_raw <- function(ei,
                                        year = 2025,
                                        eia = NULL,
                                        eia_max_lag = 1L,
                                        eia_fill_consumption = FALSE) {
  wide <- ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    # Standardized before ranking, so each economy enters the percent ranks once; "USSR"
    # (EI's CIS aggregate) is dropped with the other aggregates.
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::filter(
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country),
      Year == as.character(year),
      Var %in% c(
        "oil_tes_ej",
        "oilcons_kbd",
        "oilprod_kbd",
        "tes_gj_pc",
        "gasprod_ej",
        "gas_tes_ej",
        "coal_tes_ej",
        "coalprod_ej",
        "ren_power_tes_ej",
        "ren_power_twh",
        "tes_ej",
        "electbyfuel_ren_power",
        "electbyfuel_total"
      )
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value)

  if (!is.null(eia)) {
    for (col in c(IMPORT_DEPENDENCE_PRODUCTION_VARS, IMPORT_DEPENDENCE_CONSUMPTION_VARS)) {
      if (!col %in% names(wide)) wide[[col]] <- NA_real_
    }
    iso3 <- ei_iso3_lookup(ei)
    wide$iso3 <- iso3$iso3[match(wide$Country, iso3$Country)]
    if (isTRUE(eia_fill_consumption)) {
      producer_only <- is.na(wide$oil_tes_ej) &
        (!is.na(wide$oilprod_kbd) | !is.na(wide$gasprod_ej) | !is.na(wide$coalprod_ej))
      wide <- import_dependence_fill_eia(
        wide, eia, IMPORT_DEPENDENCE_CONSUMPTION_VARS, producer_only, year, eia_max_lag
      )
    }
  }

  wide <- dplyr::filter(wide, oil_tes_ej != 0)

  # Production EI does not report. EI's production tables list fewer economies than its
  # consumption tables and fold the rest into "Other <region>" residuals (all positive in 2025),
  # so an absent row means "not reported separately": none, or some inside the residual (New
  # Zealand gas, German oil), not zero. A reported zero (2025: Spain coal) stays 0. Until
  # 2026-10-11 these were zero-filled (0% self-sufficient), then NA (the pillar's global-average
  # fill); since then the EIA value fills them, and only a cell EIA has no value for, or no
  # comparable one (Estonia coal), stays NA (import_dependence_build_imports()).
  if (!is.null(eia)) {
    wide <- import_dependence_fill_eia(
      wide, eia, IMPORT_DEPENDENCE_PRODUCTION_VARS, rep(TRUE, nrow(wide)), year, eia_max_lag
    )
  }

  # Consumption is still zero-filled: on the economies kept here (those with an oil
  # consumption row) EI omits a gas or coal consumption row only where it also omits the
  # production row (2025: Cyprus, Iceland, Sri Lanka gas; Iraq, Turkmenistan coal), so the
  # fill reaches no balance or ratio; it only keeps such a fuel out of the fossil total.
  #
  # Named explicitly rather than as a column range: the pivoted column order follows the
  # order Vars happen to appear in the extract, so a positional range is not stable across
  # EI releases.
  wide <- dplyr::mutate(
    wide,
    dplyr::across(
      dplyr::any_of(c("oil_tes_ej", "gas_tes_ej", "coal_tes_ej")),
      ~tidyr::replace_na(.x, 0)
    )
  )

  # An EIA value of an earlier year stands in for `year` on one side of a fuel's ratio: set
  # production and consumption against each other in one year (import_dependence_pair_years()).
  if (!is.null(eia)) {
    wide <- import_dependence_pair_years(wide, ei, eia, year)
  }
  wide
}

# Production variable of each fuel and the consumption variable its self-sufficiency divides by
# (oil in thousand b/d, the unit of its production, so EI's EJ per barrel cancels), with the unit
# the explanation names.
IMPORT_DEPENDENCE_PAIRS <- tibble::tribble(
  ~fuel,  ~prod_var,     ~cons_var,     ~unit,
  "oil",  "oilprod_kbd", "oilcons_kbd", "thousand b/d",
  "gas",  "gasprod_ej",  "gas_tes_ej",  "EJ",
  "coal", "coalprod_ej", "coal_tes_ej", "EJ"
)

# Sets a fuel's production of an earlier year against consumption of that year. A production value
# EIA has not published for `year` stands in from EIA's latest year (eia_intl_lookup(max_lag)):
# 2025 gas and coal production of economies EI does not list (EIA 2024). Until 2026-10-11
# (fixes9) it was divided by EI's 2025 consumption, so a consumer whose use moved in 2025 was
# misread: Slovenia coal 121% self-sufficient (EIA 2024 production over EI 2025 consumption,
# which fell 39%; 73.5% against EI 2024), New Zealand gas 122% (109.5%).
#
# Rule, where production is of an earlier year than consumption: the self-sufficiency is
# production over consumption of the production year (consumption from the cell's own source:
# EI, or EIA in EI's units), and the balance applies it to the consumption the cell uses:
# production becomes P(pair) x C / C(pair), EIA's 2024 production scaled by EI's consumption
# change 2024-2025, i.e. the 2024 self-sufficiency carried to 2025, so the balance stays on 2025
# consumption as for every other economy and the Fossil total adds 2025 quantities.
#
# Not paired: consumption of an earlier year than production (eia_fill_consumption: the 2025
# consumption of the producers EI lists without consumption is EIA's 2024). Their 2025
# production is observed and moves (2024-2025: Yemen oil -57%, South Sudan oil +53%, Bolivia gas
# -15%); pairing would replace it with 2024's, so these cells keep EI's 2025 production over
# consumption carried from 2024, and the explanation names the EIA year (fixes9 review; user
# decision open, README EIA open end (6)). The general rule (pair year = earlier of the two) is
# kept below: `py != cy` in `need` would pair this side too.
#
# Zero production is left alone (0 in any year). A fuel whose pair-year consumption is missing or
# 0, or whose consumption is 0, keeps its values and is flagged <prod_var>_unpaired (none in
# 2025). Records <prod_var>_production_year, _consumption_year, _pair_year, _pair_production and
# _pair_consumption (in the units of IMPORT_DEPENDENCE_PAIRS) for the explanation.
import_dependence_pair_years <- function(wide, ei, eia, year) {
  year <- as.integer(year)
  vars <- c(IMPORT_DEPENDENCE_PAIRS$prod_var, IMPORT_DEPENDENCE_PAIRS$cons_var)
  ei_long <- tibble::tibble(
    Country = standardize_country_names(as.character(ei$Country)),
    Year = suppressWarnings(as.integer(as.character(ei$Year))),
    Var = as.character(ei$Var),
    Value = suppressWarnings(as.numeric(ei$Value))
  )
  ei_long <- ei_long[ei_long$Var %in% vars & !is.na(ei_long$Year) & !is.na(ei_long$Value), , drop = FALSE]
  ei_long <- ei_long[!duplicated(ei_long[, c("Country", "Year", "Var")]), , drop = FALSE]
  eia_units <- eia_intl_ei_units(eia, vars)
  ei_at <- function(var, yr) {
    ei_long$Value[match(paste(wide$Country, var, yr), paste(ei_long$Country, ei_long$Var, ei_long$Year))]
  }
  eia_at <- function(var, yr) {
    eia_units$value[match(paste(wide$iso3, var, yr), paste(eia_units$iso3, eia_units$ei_var, eia_units$year))]
  }

  for (i in seq_len(nrow(IMPORT_DEPENDENCE_PAIRS))) {
    pv <- IMPORT_DEPENDENCE_PAIRS$prod_var[i]
    cv <- IMPORT_DEPENDENCE_PAIRS$cons_var[i]
    p_eia_year <- as.integer(import_dependence_col(wide, paste0(pv, "_eia_year"), NA_integer_))
    c_eia_year <- as.integer(import_dependence_col(wide, paste0(cv, "_eia_year"), NA_integer_))
    py <- dplyr::coalesce(p_eia_year, year)
    cy <- dplyr::coalesce(c_eia_year, year)
    p <- wide[[pv]]
    cons <- wide[[cv]]
    pair <- pmin(py, cy)
    # Production of the earlier year only (see above).
    need <- !is.na(p) & p > 0 & py < cy
    # The other side's value of the pair year, from the same source as the cell's own value.
    p_pair <- dplyr::case_when(pair == py ~ p, is.na(p_eia_year) ~ ei_at(pv, pair), TRUE ~ eia_at(pv, pair))
    c_pair <- dplyr::case_when(pair == cy ~ cons, is.na(c_eia_year) ~ ei_at(cv, pair), TRUE ~ eia_at(cv, pair))
    ok <- need & !is.na(p_pair) & !is.na(c_pair) & c_pair > 0 & !is.na(cons) & cons > 0

    wide[[paste0(pv, "_production_year")]] <- dplyr::if_else(is.na(p), NA_integer_, py)
    wide[[paste0(pv, "_consumption_year")]] <- cy
    wide[[paste0(pv, "_pair_year")]] <- dplyr::if_else(ok, pair, NA_integer_)
    wide[[paste0(pv, "_pair_production")]] <- dplyr::if_else(ok, p_pair, NA_real_)
    wide[[paste0(pv, "_pair_consumption")]] <- dplyr::if_else(ok, c_pair, NA_real_)
    wide[[paste0(pv, "_unpaired")]] <- need & !ok
    wide[[pv]][ok] <- p_pair[ok] * cons[ok] / c_pair[ok]
  }
  wide
}

# The explanation of a fuel whose production and consumption are of different years
# (import_dependence_pair_years()); "" where they are of the same year. One element per row.
import_dependence_pair_note <- function(fuel, unit, production_source, consumption_source,
                                        production_year, consumption_year, pair_year,
                                        pair_production, pair_consumption, consumption, unpaired) {
  num <- function(x) {
    vapply(x, function(v) {
      if (is.na(v)) "NA" else format(signif(v, 4), big.mark = ",", scientific = FALSE, trim = TRUE)
    }, character(1))
  }
  fuel <- tolower(fuel)
  out <- rep("", length(fuel))
  sides <- paste0(
    ". Production is ", production_source, "'s ", production_year, " value and consumption ",
    consumption_source, "'s ", consumption_year
  )
  ss <- sprintf("%.1f%%", 100 * pair_production / pair_consumption)
  paired <- !is.na(pair_year)

  # Production of the earlier year: the self-sufficiency is that year's, carried to the
  # consumption year by EI's (or EIA's) consumption change.
  p <- paired & pair_year == production_year
  out[p] <- paste0(
    sides[p], ", so self-sufficiency pairs them in ", pair_year[p], ": ", num(pair_production[p]), " ",
    unit[p], " produced against ", consumption_source[p], "'s ", pair_year[p], " consumption of ",
    num(pair_consumption[p]), " ", unit[p], " (", ss[p], "). The balance applies that share to the ",
    consumption_year[p], " consumption of ", num(consumption[p]), " ", unit[p], " (production taken as the ",
    pair_year[p], " value x ", num(consumption[p] / pair_consumption[p]), ", the change in ",
    consumption_source[p], "'s ", fuel[p], " consumption ", pair_year[p], "-", consumption_year[p], ")"
  )
  # Consumption of the earlier year: production of that year, so both are that year's. Not reached
  # while import_dependence_pair_years() pairs only production of the earlier year.
  cons <- paired & !p
  out[cons] <- paste0(
    sides[cons], ", so self-sufficiency and balance pair them in ", pair_year[cons], ": ",
    num(pair_production[cons]), " ", unit[cons], " produced (", production_source[cons], ", ",
    pair_year[cons], ") against ", num(pair_consumption[cons]), " ", unit[cons], " consumed (", ss[cons], ")"
  )
  unpaired <- unpaired %in% TRUE
  out[unpaired] <- paste0(
    sides[unpaired], ", and no value of the earlier year is available for the other side (or a ",
    "consumption is 0), so they are compared as they are"
  )
  out
}

# Fills the NA cells of `vars` in the rows `rows` (logical) of the wide table from EIA, and
# records the EIA year and basis of each filled cell in <var>_eia_year and <var>_eia_basis. A
# production value is checked against EI's consumption of the fuel (eia_intl_lookup(),
# EIA_INTL_COMPARABILITY); one that fails stays NA with basis "not_comparable", and its coverage
# is kept in <var>_eia_coverage.
import_dependence_fill_eia <- function(wide, eia, vars, rows, year, max_lag) {
  for (v in vars) {
    year_col <- paste0(v, "_eia_year")
    basis_col <- paste0(v, "_eia_basis")
    coverage_col <- paste0(v, "_eia_coverage")
    if (!year_col %in% names(wide)) wide[[year_col]] <- NA_integer_
    if (!basis_col %in% names(wide)) wide[[basis_col]] <- NA_character_
    if (!coverage_col %in% names(wide)) wide[[coverage_col]] <- NA_real_
    need <- rows & is.na(wide[[v]])
    if (!any(need)) next
    cons_var <- if (v %in% names(EIA_INTL_CONSUMPTION_OF)) EIA_INTL_CONSUMPTION_OF[[v]] else NA_character_
    got <- eia_intl_lookup(
      tibble::tibble(iso3 = wide$iso3[need], ei_var = v, year = as.integer(year)),
      eia,
      max_lag = max_lag,
      ei_consumption = if (!is.na(cons_var) && cons_var %in% names(wide)) wide[[cons_var]][need] else NULL
    )
    filled <- !is.na(got$value)
    wide[[v]][need] <- got$value
    wide[[year_col]][need] <- dplyr::if_else(filled, got$eia_year, NA_integer_)
    wide[[basis_col]][need] <- dplyr::if_else(filled | got$eia_basis %in% "not_comparable", got$eia_basis, NA_character_)
    wide[[coverage_col]][need] <- got$eia_consumption_coverage
  }
  wide
}

# A column of the cleaned table, or `default` where an older or EIA-less table lacks it.
import_dependence_col <- function(tbl, col, default) {
  if (col %in% names(tbl)) tbl[[col]] else rep(default, nrow(tbl))
}

# Where neither EI nor EIA gives production of a fuel for an economy (NA from
# import_dependence_clean_raw()), the fuel's balance, self-sufficiency and index are NA, so the
# pillar's missing-data rule (config/missing_data.yml, `import_dependence: global_average`) fills
# the index and flags it imputed; *_production_reported records which, and
# *_production_source whether a value is EI's or EIA's (an EIA value is observed data, not
# imputed). Fossil sums the fuels with a value, as the Sector Insights exporter does
# (sector_facts_balance()), and is NA only when none of the three has one; a fuel without a
# value adds its consumption to Fossil but not its production.
import_dependence_build_imports <- function(imports_raw) {
  require_columns(
    imports_raw,
    c("oil_tes_ej", "oilcons_kbd", "oilprod_kbd", "gas_tes_ej", "gasprod_ej", "coal_tes_ej", "coalprod_ej"),
    label = "imports_raw"
  )
  source_of <- function(value, eia_year) {
    dplyr::case_when(is.na(value) ~ NA_character_, !is.na(eia_year) ~ "EIA", TRUE ~ "EI")
  }
  imports_raw %>%
    dplyr::mutate(
      oil_production_reported = !is.na(oilprod_kbd),
      gas_production_reported = !is.na(gasprod_ej),
      coal_production_reported = !is.na(coalprod_ej),
      fossil_production_reported = oil_production_reported | gas_production_reported | coal_production_reported,
      oil_production_source = source_of(oilprod_kbd, import_dependence_col(imports_raw, "oilprod_kbd_eia_year", NA_integer_)),
      gas_production_source = source_of(gasprod_ej, import_dependence_col(imports_raw, "gasprod_ej_eia_year", NA_integer_)),
      coal_production_source = source_of(coalprod_ej, import_dependence_col(imports_raw, "coalprod_ej_eia_year", NA_integer_)),
      fossil_production_source = dplyr::case_when(
        !fossil_production_reported ~ NA_character_,
        oil_production_source %in% "EIA" | gas_production_source %in% "EIA" | coal_production_source %in% "EIA" ~ "EIA",
        TRUE ~ "EI"
      ),
      # Consumption from EIA (eia_fill_consumption): oil_tes_ej carries the record.
      consumption_source = dplyr::if_else(
        !is.na(import_dependence_col(imports_raw, "oil_tes_ej_eia_year", NA_integer_)),
        "EIA",
        "EI"
      ),
      oil_calc = oil_tes_ej / oilcons_kbd,
      oilprod_ej = oil_calc * oilprod_kbd,
      fossilprod_ej = dplyr::if_else(
        fossil_production_reported,
        rowSums(cbind(oilprod_ej, gasprod_ej, coalprod_ej), na.rm = TRUE),
        NA_real_
      ),
      fossil_tes_ej = oil_tes_ej + gas_tes_ej + coal_tes_ej,
      # Net balance, production minus consumption (EJ): positive for a net exporter. These
      # columns were named *_imports_ej although a net importer has a negative value.
      oil_surplus_ej = oilprod_ej - oil_tes_ej,
      oil_surplus_kbd = oilprod_kbd - oilcons_kbd,
      gas_surplus_ej = gasprod_ej - gas_tes_ej,
      coal_surplus_ej = coalprod_ej - coal_tes_ej,
      fossil_surplus_ej = fossilprod_ej - fossil_tes_ej,
      # Self-sufficiency, production as a percent of consumption: 100 is balanced, 0 fully
      # imported, above 100 a net exporter. It replaces the former "Import share",
      # (production - consumption) / consumption x 100, which is this minus 100 and was
      # positive for exporters despite its name. The index was always oriented higher = less
      # import dependent, and a shift by 100 leaves the percent ranks as they were, with one
      # exception that this form removes: 100 * (0 - c) / c is not exactly -100 for every c,
      # so economies that produce none of a fuel were split by rounding noise instead of
      # tying (2025 oil: Spain -100.00000000000001 ranked below 38 exact -100s, Israel,
      # Netherlands, Poland and Singapore -99.999999999999986 above them; all of them were
      # zero-filled, so they are NA now). 100 * 0 / c is 0, so reported zeros tie.
      # Undefined where a country consumes none of the fuel, so the denominators are guarded
      # rather than emitting 0/0. The absolute balance above is still 0 and still
      # meaningful, and the index below averages over whichever of the two is available.
      # NA, like the balance, where the production is NA.
      oil_self_sufficiency = dplyr::if_else(oil_tes_ej > 0, 100 * oilprod_ej / oil_tes_ej, NA_real_),
      gas_self_sufficiency = dplyr::if_else(gas_tes_ej > 0, 100 * gasprod_ej / gas_tes_ej, NA_real_),
      coal_self_sufficiency = dplyr::if_else(coal_tes_ej > 0, 100 * coalprod_ej / coal_tes_ej, NA_real_),
      fossil_self_sufficiency = dplyr::if_else(
        fossil_tes_ej > 0,
        100 * fossilprod_ej / fossil_tes_ej,
        NA_real_
      )
    ) %>%
    # Ranked over the economies with a value only: NA production leaves the percent ranks.
    dplyr::mutate(
      oil_import_index = import_dependence_mean_rank(oil_self_sufficiency, oil_surplus_ej),
      gas_import_index = import_dependence_mean_rank(gas_self_sufficiency, gas_surplus_ej),
      coal_import_index = import_dependence_mean_rank(coal_self_sufficiency, coal_surplus_ej),
      fossil_import_index = import_dependence_mean_rank(fossil_self_sufficiency, fossil_surplus_ej)
    ) %>%
    dplyr::ungroup()
}

# Mean of the percent-ranked self-sufficiency and balance, over whichever is present; NA (not
# rowMeans()'s NaN) where neither is.
import_dependence_mean_rank <- function(self_sufficiency, balance) {
  ranked <- cbind(median_scurve(self_sufficiency), median_scurve(balance))
  out <- rowMeans(ranked, na.rm = TRUE)
  out[rowSums(!is.na(ranked)) == 0] <- NA_real_
  out
}

# The index is the mean of two percent-ranked components: the absolute balance and
# self-sufficiency. Publish both as raw rows so the composite can be audited, and keep them
# on separate `variable` names so (Country, tech, variable, data_type) stays unique.
#
# "Production surplus/deficit" must stay the EJ balance: config/index_definition.yml lists
# it as the Overall Energy Imports Index component, and R/charts/import_exposure_chart.R
# reads its raw row as production_surplus_deficit_ej.
import_dependence_component_specs <- function() {
  list(
    list(
      suffix = "_surplus_ej",
      variable = "Production surplus/deficit",
      data_type = "raw",
      explanation = paste(
        "{tech} production surplus/deficit (EJ) = production minus consumption;",
        "negative means net importer. NA where neither EI nor EIA reports the production"
      )
    ),
    list(
      suffix = "_self_sufficiency",
      variable = "Self-sufficiency",
      data_type = "raw",
      explanation = paste(
        "{tech} self-sufficiency (%) = production as a percent of consumption; below 100 means",
        "net importer, 0 fully imported. NA where the country consumes none of the fuel or",
        "neither EI nor EIA reports the production"
      )
    ),
    list(
      suffix = "_import_index",
      variable = "Production surplus/deficit",
      data_type = "index",
      explanation = paste(
        "Mean of the percent-ranked self-sufficiency and percent-ranked absolute balance for",
        "{tech}; higher means less dependent on imports"
      )
    )
  )
}

# Appended to the explanation of every row of an economy and fuel without a production value
# (*_production_reported FALSE), so a reader can tell that NA from "consumes none of the fuel".
# `eia_checked`: the EIA extract was consulted and has no value either.
import_dependence_unreported_note <- function(tech, eia_checked = FALSE) {
  fuels <- dplyr::if_else(tech == "Fossil", "oil, gas or coal", tolower(tech))
  paste0(
    ". Not reported separately: EI has no ", fuels, " production row for this economy (none, ",
    "or counted in an 'Other' regional total)",
    if (isTRUE(eia_checked)) " and the EIA International Energy Statistics give no value" else "",
    ", so the value is NA and the index takes the pillar's missing-data rule"
  )
}

# Per economy and technology: whether production has a value, the note its rows' explanation
# ends with, and whether any of its inputs comes from EIA (then the source names both).
import_dependence_annotations <- function(imports_indexed, techs, year) {
  eia_checked <- "oilprod_kbd_eia_year" %in% names(imports_indexed)
  consumption_eia_year <- import_dependence_col(imports_indexed, "oil_tes_ej_eia_year", NA_integer_)

  ann <- dplyr::bind_rows(lapply(names(techs), function(f) {
    var <- if (f %in% names(IMPORT_DEPENDENCE_PRODUCTION_VARS)) IMPORT_DEPENDENCE_PRODUCTION_VARS[[f]] else NA_character_
    pair <- IMPORT_DEPENDENCE_PAIRS[IMPORT_DEPENDENCE_PAIRS$fuel == f, , drop = FALSE]
    col <- function(suffix, default) {
      if (is.na(var)) rep(default, nrow(imports_indexed)) else import_dependence_col(imports_indexed, paste0(var, suffix), default)
    }
    cons_var <- if (nrow(pair) == 1) pair$cons_var else NA_character_
    tibble::tibble(
      Country = imports_indexed$Country,
      tech = unname(techs[[f]]),
      ei_var = var,
      production_reported = imports_indexed[[paste0(f, "_production_reported")]],
      production_source = import_dependence_col(imports_indexed, paste0(f, "_production_source"), "EI"),
      eia_year = col("_eia_year", NA_integer_),
      eia_basis = col("_eia_basis", NA_character_),
      eia_coverage = col("_eia_coverage", NA_real_),
      consumption_eia_year = consumption_eia_year,
      # Production and consumption of different years (import_dependence_pair_years()).
      pair_unit = if (nrow(pair) == 1) pair$unit else NA_character_,
      pair_consumption_source = if (is.na(cons_var)) NA_character_ else dplyr::if_else(
        is.na(import_dependence_col(imports_indexed, paste0(cons_var, "_eia_year"), NA_integer_)), "EI", "EIA"
      ),
      pair_consumption_used = if (is.na(cons_var)) NA_real_ else imports_indexed[[cons_var]],
      production_year = col("_production_year", NA_integer_),
      consumption_year = col("_consumption_year", NA_integer_),
      pair_year = col("_pair_year", NA_integer_),
      pair_production = col("_pair_production", NA_real_),
      pair_consumption = col("_pair_consumption", NA_real_),
      unpaired = col("_unpaired", FALSE)
    )
  }))

  ann$note <- ""
  unreported <- !ann$production_reported
  ann$note[unreported] <- import_dependence_unreported_note(ann$tech[unreported], eia_checked = eia_checked)
  # EIA has a value but its fuel is not EI's (eia_intl_lookup(), EIA_INTL_COMPARABILITY).
  not_comparable <- unreported & ann$eia_basis %in% "not_comparable"
  if (any(not_comparable)) {
    ann$note[not_comparable] <- paste0(
      ". Not reported separately: EI has no ", tolower(ann$tech[not_comparable]), " production row for ",
      "this economy, and ", eia_intl_not_comparable_text(ann$tech[not_comparable], ann$eia_coverage[not_comparable]),
      "; the value is NA and the index takes the pillar's missing-data rule"
    )
  }
  fuel_eia <- ann$production_reported & ann$production_source %in% "EIA" & ann$tech != "Fossil"
  if (any(fuel_eia)) {
    ann$note[fuel_eia] <- paste0(". ", eia_intl_fill_note(
      ann$tech[fuel_eia], ann$ei_var[fuel_eia], ann$eia_year[fuel_eia], year, ann$eia_basis[fuel_eia]
    ))
  }
  fossil_eia <- ann$production_reported & ann$production_source %in% "EIA" & ann$tech == "Fossil"
  ann$note[fossil_eia] <- paste0(
    ". Production includes values from the EIA International Energy Statistics where EI does not ",
    "report a fuel separately (see the Oil, Gas and Coal rows)"
  )
  consumption_eia <- !is.na(ann$consumption_eia_year)
  if (any(consumption_eia)) {
    ann$note[consumption_eia] <- paste0(
      ann$note[consumption_eia], ". ",
      eia_intl_fill_note("oil, gas and coal", "oil_tes_ej", ann$consumption_eia_year[consumption_eia], year, flow = "consumption"),
      " (oil in b/d: petroleum and other liquids consumption)"
    )
  }
  fuel_pair <- ann$tech != "Fossil" & (!is.na(ann$pair_year) | ann$unpaired %in% TRUE)
  if (any(fuel_pair)) {
    ann$note[fuel_pair] <- paste0(ann$note[fuel_pair], import_dependence_pair_note(
      ann$tech[fuel_pair], ann$pair_unit[fuel_pair], ann$production_source[fuel_pair],
      ann$pair_consumption_source[fuel_pair], ann$production_year[fuel_pair], ann$consumption_year[fuel_pair],
      ann$pair_year[fuel_pair], ann$pair_production[fuel_pair], ann$pair_consumption[fuel_pair],
      ann$pair_consumption_used[fuel_pair], ann$unpaired[fuel_pair]
    ))
    fossil_pair <- ann$tech == "Fossil" & ann$Country %in% ann$Country[fuel_pair & !is.na(ann$pair_year)]
    ann$note[fossil_pair] <- paste0(
      ann$note[fossil_pair], ". Each fuel's production enters with its consumption of the same year ",
      "(see the Oil, Gas and Coal rows)"
    )
  }
  ann$from_eia <- ann$production_source %in% "EIA" | consumption_eia
  ann[, c("Country", "tech", "production_reported", "note", "from_eia")]
}

import_dependence_build_tidy <- function(imports_indexed, year = 2025) {
  techs <- c(oil = "Oil", gas = "Gas", coal = "Coal", fossil = "Fossil")

  reported_cols <- paste0(names(techs), "_production_reported")
  require_columns(imports_indexed, reported_cols, label = "imports_indexed")
  annotations <- import_dependence_annotations(imports_indexed, techs, year)

  components <- lapply(import_dependence_component_specs(), function(spec) {
    cols <- paste0(names(techs), spec$suffix)
    require_columns(imports_indexed, cols, label = "imports_indexed")

    imports_indexed %>%
      dplyr::select(Country, dplyr::all_of(cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols),
        names_to = "tech",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        tech = unname(techs[sub(paste0(spec$suffix, "$"), "", tech)]),
        variable = spec$variable,
        data_type = spec$data_type,
        explanation = as.character(stringr::str_glue(spec$explanation))
      )
  })

  ei_source <- ei_stat_review_source(year)
  dplyr::bind_rows(components) %>%
    dplyr::left_join(annotations, by = c("Country", "tech")) %>%
    dplyr::mutate(
      explanation = paste0(explanation, note),
      supply_chain = "Upstream",
      category = "Energy Imports",
      Year = year,
      # An EIA value is observed data: the row says where it came from instead of being
      # left to the missing-data rule.
      source = dplyr::if_else(from_eia, paste0(ei_source, "; EIA International Energy Statistics"), ei_source)
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
    )
}

# `eia`, `eia_max_lag`, `eia_fill_consumption`: see import_dependence_clean_raw().
import_dependence <- function(ei, year = 2025, eia = NULL, eia_max_lag = 1L, eia_fill_consumption = FALSE) {
  energy_security_add_overall_index(
    import_dependence_clean_raw(
      ei,
      year = year,
      eia = eia,
      eia_max_lag = eia_max_lag,
      eia_fill_consumption = eia_fill_consumption
    ) %>%
      import_dependence_build_imports() %>%
      import_dependence_build_tidy(year = year)
  ) %>%
    # The overall row of an economy whose component is NA is mean(NA, na.rm = TRUE), NaN.
    dplyr::mutate(value = dplyr::if_else(is.nan(value), NA_real_, value))
}
