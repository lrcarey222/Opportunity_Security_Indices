# Japan's exposure to global oil and gas markets, in global context.
#
# One question, one chart: how much of each economy's total energy supply is oil and
# gas that it has to buy on world markets? For Japan the answer is almost three-fifths:
# it uses oil and gas in about the same proportion as the world as a whole, but it
# produces almost none of either, so nearly every barrel and cubic metre is imported
# and priced off global markets. The US burns just as much oil and gas and imports
# none of it on net; China and India import a large share of their oil but sit on coal.
#
# Each country's total energy supply (TES) is split into four parts that sum to 100%:
#   Imported oil         oil supply x net import share of oil
#   Imported gas         gas supply x net import share of gas
#   Domestic oil & gas   the oil and gas it produces itself
#   All other energy     coal, nuclear, hydro, renewables
#
# Net import share is 1 - production / consumption, floored at zero, so a net
# exporter shows no imported oil or gas. It is a net measure: it ignores re-exports,
# refined-product trade in both directions and stock changes, which is the right
# reading of exposure to world prices but not of physical cargo volumes. Oil uses
# barrels per day for both sides (EI does not publish oil production in EJ); gas uses
# EJ. Aggregates such as the EU are netted at the aggregate, so intra-EU trade drops out.
#
# Source: Energy Institute Statistical Review of World Energy, long-format panel
# staged in the repo at <raw_data_dir>/ei_stat_review_world_energy.csv. Variables:
#   tes_ej, oil_tes_ej, gas_tes_ej     supply by fuel, EJ
#   oilcons_kbd, oilprod_kbd           oil consumption and production, kb/d
#   gasprod_ej                         gas production, EJ
#
# Output (data/processed/charts/):
#   japan_energy_import_exposure_<year>_wide.csv
# Datawrapper-ready for one stacked bar chart: first column `country`, then the four
# share columns (percent of TES, summing to 100), then tooltip-only columns
# (net import dependence for oil and gas, and TES in EJ) to leave out of the bars.
# Rows are sorted by imported oil and gas share, highest first.
#
# Run:
#   Rscript R/charts/japan_energy_import_exposure.R
#
# Environment:
#   JEI_YEAR   data year (default: latest year with complete data for Japan)

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

## Configuration -------------------------------------------------------------

JEI_EI_FILE <- "ei_stat_review_world_energy.csv"

JEI_VARS <- c("tes_ej", "oil_tes_ej", "gas_tes_ej", "oilcons_kbd", "oilprod_kbd", "gasprod_ej")

# EI country name -> chart label. Japan's Asian import-dependent peers, the large
# European importers, and the big economies that produce much of their own oil, gas
# or coal, so the chart shows both ends of the exposure range.
JEI_COUNTRIES <- tibble::tribble(
  ~ei_name,          ~label,
  "Japan",           "Japan",
  "South Korea",     "South Korea",
  "Taiwan",          "Taiwan",
  "Germany",         "Germany",
  "Italy",           "Italy",
  "France",          "France",
  "Total EU",        "European Union",
  "United Kingdom",  "United Kingdom",
  "India",           "India",
  "China",           "China",
  "US",              "United States"
)

# Production variables are absent, not zero, for countries that produce none.
JEI_ZERO_IF_MISSING <- c("oilprod_kbd", "gasprod_ej")

## Transformations -----------------------------------------------------------

# Latest year in which the focus country has every variable the split needs.
jei_latest_year <- function(ei, country = "Japan") {
  rows <- ei[ei$Country == country & ei$Var %in% setdiff(JEI_VARS, JEI_ZERO_IF_MISSING) &
               !is.na(ei$Value), , drop = FALSE]
  complete <- tapply(rows$Var, rows$Year, function(v) all(setdiff(JEI_VARS, JEI_ZERO_IF_MISSING) %in% v))
  years <- as.integer(names(complete)[complete])
  if (length(years) == 0) stop("No year has complete EI data for ", country, ".")
  max(years)
}

# Long EI rows -> one row per country with the four TES shares and the
# dependence ratios behind them.
jei_exposure <- function(ei, year, countries = JEI_COUNTRIES) {
  rows <- ei[ei$Year == year & ei$Country %in% countries$ei_name & ei$Var %in% JEI_VARS,
             c("Country", "Var", "Value"), drop = FALSE]
  wide <- tidyr::pivot_wider(rows, names_from = "Var", values_from = "Value")
  wide <- as.data.frame(wide)

  absent <- setdiff(countries$ei_name, wide$Country)
  if (length(absent) > 0) stop("EI has no ", year, " rows for: ", paste(absent, collapse = ", "), ".")
  for (v in setdiff(JEI_VARS, names(wide))) wide[[v]] <- NA_real_
  for (v in JEI_ZERO_IF_MISSING) wide[[v]][is.na(wide[[v]])] <- 0

  required <- setdiff(JEI_VARS, JEI_ZERO_IF_MISSING)
  incomplete <- wide$Country[!stats::complete.cases(wide[, required])]
  if (length(incomplete) > 0) {
    stop("EI ", year, " is missing supply or consumption data for: ",
         paste(incomplete, collapse = ", "), ".")
  }

  oil_dep <- pmax(0, 1 - wide$oilprod_kbd / wide$oilcons_kbd)
  gas_dep <- ifelse(wide$gas_tes_ej > 0, pmax(0, 1 - wide$gasprod_ej / wide$gas_tes_ej), 0)

  imported_oil <- wide$oil_tes_ej * oil_dep
  imported_gas <- wide$gas_tes_ej * gas_dep

  out <- data.frame(
    country = countries$label[match(wide$Country, countries$ei_name)],
    imported_oil = 100 * imported_oil / wide$tes_ej,
    imported_gas = 100 * imported_gas / wide$tes_ej,
    domestic_oil_gas = 100 * (wide$oil_tes_ej + wide$gas_tes_ej - imported_oil - imported_gas) /
      wide$tes_ej,
    other_energy = 100 * (wide$tes_ej - wide$oil_tes_ej - wide$gas_tes_ej) / wide$tes_ej,
    oil_import_dependence = 100 * oil_dep,
    gas_import_dependence = 100 * gas_dep,
    tes_ej = wide$tes_ej,
    stringsAsFactors = FALSE
  )
  out[order(-(out$imported_oil + out$imported_gas), out$country), , drop = FALSE]
}

# Datawrapper headers: the four bar columns first, tooltip columns after.
jei_wide <- function(exposure, digits = 1L) {
  out <- exposure
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], round, digits = digits)
  names(out) <- c(
    "country", "Imported oil", "Imported gas", "Domestic oil & gas", "All other energy",
    "Oil import dependence (%)", "Gas import dependence (%)", "Total energy supply (EJ)"
  )
  rownames(out) <- NULL
  out
}

## Runner --------------------------------------------------------------------

run_japan_energy_import_exposure <- function(repo_root, year = NA_integer_, write_output = TRUE) {
  path <- file.path(repo_root, config$raw_data_dir, JEI_EI_FILE)
  if (!file.exists(path)) stop("EI Statistical Review panel not found at ", path, ".")

  ei <- readr::read_csv(
    path,
    col_types = readr::cols_only(
      Country = readr::col_character(), Year = readr::col_integer(),
      Var = readr::col_character(), Value = readr::col_double()
    ),
    progress = FALSE
  )
  ei <- as.data.frame(ei[ei$Var %in% JEI_VARS, , drop = FALSE])

  if (is.na(year)) year <- jei_latest_year(ei)
  wide <- jei_wide(jei_exposure(ei, year))

  jp <- wide[wide$country == "Japan", ]
  message(sprintf("EI %d: imported oil and gas = %.0f%% of Japan's energy supply (oil %.0f%%, gas %.0f%%)",
                  year, jp[["Imported oil"]] + jp[["Imported gas"]], jp[["Imported oil"]],
                  jp[["Imported gas"]]))
  if (!write_output) return(invisible(wide))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, sprintf("japan_energy_import_exposure_%d_wide.csv", year))
  utils::write.csv(wide, out_path, row.names = FALSE, na = "")
  message("Wrote ", out_path)

  invisible(wide)
}

opsi_japan_energy_imports_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_energy_import_exposure.R")
}

if (opsi_japan_energy_imports_run_directly()) {
  invisible(run_japan_energy_import_exposure(
    repo_root = repo_root,
    year = as.integer(Sys.getenv("JEI_YEAR", NA_character_))
  ))
}
