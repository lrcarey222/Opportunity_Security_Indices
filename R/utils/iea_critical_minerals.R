# IEA Critical Minerals Dataset reader.
#
# The IEA publishes this dataset as a multi-sheet Excel workbook laid out for human
# reading: a mineral name on its own row, one column group per scenario, blank spacer
# rows and columns between blocks. Earlier vintages reached this repo as a hand-flattened
# CSV (iea_criticalminerals_25.csv); the 2026 release is staged as the published workbook
# ("IEA Critical Minerals Dataset 2026.xlsx"), so the flattening lives here instead of in
# a manual pre-step.
#
# read_iea_critical_minerals() accepts either form and returns the same long frame the CSV
# provided, so every theme builder keeps one input contract:
#
#   Pillar          stable sheet id ("2 Total supply for key minerals",
#                   "3.1 Energy demand by tech", ...)
#   Mineral         mineral name; supply rows carry "<mineral> - <stage>"
#   Sector.Country  sector on the demand sheets, country on the supply sheet
#   Scenario        scenario label; supply rows are the workbook's single base case
#   X<year>         one numeric column per projection year the release publishes
#
# The base year moves with each release (2024 in the 2025 vintage, 2025 in the 2026 one),
# so value columns are addressed through iea_critical_minerals_year_col() rather than by a
# pinned name like `X2024`.

# The demand sheets publish every scenario side by side. The index has always been built
# on Stated Policies, so that is what the reader hands back unless a caller asks wider.
IEA_CRITICAL_MINERALS_SCENARIO <- "Stated Policies"

# The supply sheet carries a single projection ("base case"), not a policy scenario.
IEA_CRITICAL_MINERALS_BASE_CASE <- "Base case"

# Sheets the index reads, with the pillar label each one is flattened to.
#
# `match` is applied to the workbook's sheet names, which are renamed between releases
# ("3.1 Cleantech demand by tech" became "3.1 Energy demand by tech" in 2026), so only the
# leading section number is matched. The 4.x technology-detail sheets are not read.
iea_critical_minerals_sheet_specs <- function() {
  list(
    list(pillar = "1 Total demand for key minerals", match = "^1[^0-9.]", layout = "demand_by_sector"),
    list(pillar = "2 Total supply for key minerals", match = "^2[^0-9.]", layout = "supply_by_country"),
    list(pillar = "3.1 Energy demand by tech", match = "^3\\.1", layout = "demand_by_sector"),
    list(pillar = "3.2 Energy demand by mineral", match = "^3\\.2", layout = "demand_by_mineral")
  )
}

# Value columns present in a flattened frame, as a named integer vector (X2035 -> 2035).
iea_critical_minerals_year_cols <- function(critical) {
  hits <- grep("^X(19|20)\\d{2}$", names(critical), value = TRUE)
  stats::setNames(as.integer(sub("^X", "", hits)), hits)
}

# Earliest published year, i.e. the release's base year.
iea_critical_minerals_base_year <- function(critical) {
  years <- iea_critical_minerals_year_cols(critical)
  if (length(years) == 0) {
    stop("No X<year> value columns found in the IEA critical minerals data.")
  }
  min(years)
}

# Resolve one value column by year.
#
# Mirrors reserves_resolve_val_col(): when the requested year is absent (a release drops a
# projection year, or the base year moves), fall back to the nearest published year and say
# so, rather than failing on a column name that only differs by vintage.
iea_critical_minerals_year_col <- function(critical, year, label = "IEA critical minerals") {
  years <- iea_critical_minerals_year_cols(critical)
  if (length(years) == 0) {
    stop("No X<year> value columns found in ", label, ".")
  }

  year <- as.integer(year)
  exact <- names(years)[years == year]
  if (length(exact) > 0) {
    return(exact[[1]])
  }

  # Nearest published year; ties resolve to the later one.
  resolved <- names(years)[order(abs(years - year), -years)][1]
  message(label, ": no X", year, " column; using '", resolved, "'.")
  resolved
}

## Workbook parsing -----------------------------------------------------

# Read one sheet as parallel text/number grids.
#
# col_types = "list" keeps each cell's native type, so a year header (numeric 2035) is
# never confused with a label and numbers never round-trip through their text form.
iea_critical_minerals_read_grid <- function(path, sheet) {
  raw <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "list",
    .name_repair = "minimal",
    progress = FALSE
  )

  n_row <- nrow(raw)
  n_col <- ncol(raw)
  txt <- matrix("", nrow = n_row, ncol = n_col)
  num <- matrix(NA_real_, nrow = n_row, ncol = n_col)

  for (j in seq_len(n_col)) {
    column <- raw[[j]]
    for (i in seq_len(n_row)) {
      value <- column[[i]]
      if (is.null(value) || length(value) == 0 || all(is.na(value))) next
      value <- value[[1]]
      if (is.numeric(value)) {
        num[i, j] <- as.numeric(value)
      } else {
        txt[i, j] <- trimws(as.character(value))
      }
    }
  }

  list(txt = txt, num = num, n_row = n_row, n_col = n_col)
}

# Footnote rows sit directly under the data with no blank separator, so parsing stops here.
iea_critical_minerals_is_footnote <- function(label) {
  grepl("^(note|source)s?\\b", label, ignore.case = TRUE)
}

# Bare projection years, as distinct from the data values below them.
iea_critical_minerals_is_year <- function(values) {
  !is.na(values) & values == round(values) & values >= 1990 & values <= 2100
}

# Row holding the projection years: the first row carrying two or more bare years.
iea_critical_minerals_find_year_row <- function(grid, search_rows = 20L) {
  for (i in seq_len(min(grid$n_row, search_rows))) {
    if (sum(iea_critical_minerals_is_year(grid$num[i, ])) >= 2) return(i)
  }
  NA_integer_
}

iea_critical_minerals_year_columns <- function(grid, year_row) {
  which(iea_critical_minerals_is_year(grid$num[year_row, ]))
}

# Rows below the year header. Returns nothing — rather than seq()'s descending sequence —
# when the header is the last row on the sheet.
iea_critical_minerals_data_rows <- function(grid, year_row) {
  if (year_row >= grid$n_row) return(integer())
  seq.int(year_row + 1L, grid$n_row)
}

# Map each year column to its scenario.
#
# The demand sheets put scenario names one row above the years, each above the first year
# of its block. Year columns to the left of the first scenario name are the release's
# historical base year, which every scenario shares.
iea_critical_minerals_scenario_map <- function(grid, year_row, year_cols) {
  scenario_row <- year_row - 1L
  scenario_cols <- integer()
  if (scenario_row >= 1) {
    labels <- grid$txt[scenario_row, ]
    scenario_cols <- which(grepl("scenario", labels, ignore.case = TRUE))
  }

  if (length(scenario_cols) == 0) {
    return(list(
      scenarios = IEA_CRITICAL_MINERALS_BASE_CASE,
      shared = year_cols,
      by_scenario = stats::setNames(list(integer()), IEA_CRITICAL_MINERALS_BASE_CASE)
    ))
  }

  names(scenario_cols) <- grid$txt[scenario_row, scenario_cols]
  shared <- year_cols[year_cols < min(scenario_cols)]

  by_scenario <- lapply(seq_along(scenario_cols), function(k) {
    start <- scenario_cols[[k]]
    end <- if (k < length(scenario_cols)) scenario_cols[[k + 1L]] - 1L else grid$n_col
    year_cols[year_cols >= start & year_cols <= end]
  })
  names(by_scenario) <- names(scenario_cols)

  list(scenarios = names(scenario_cols), shared = shared, by_scenario = by_scenario)
}

# Flatten a demand sheet.
#
# `layout` is "demand_by_sector" when a mineral header row is followed by one row per
# sector, and "demand_by_mineral" when each data row is itself a mineral total.
iea_critical_minerals_parse_demand <- function(grid, pillar, layout) {
  year_row <- iea_critical_minerals_find_year_row(grid)
  if (is.na(year_row)) {
    stop("Could not locate the projection-year header row on sheet '", pillar, "'.")
  }

  year_cols <- iea_critical_minerals_year_columns(grid, year_row)
  scenario_map <- iea_critical_minerals_scenario_map(grid, year_row, year_cols)

  rows <- list()
  mineral <- NA_character_

  for (i in iea_critical_minerals_data_rows(grid, year_row)) {
    label <- grid$txt[i, 1]
    if (!nzchar(label)) next
    if (iea_critical_minerals_is_footnote(label)) break

    has_values <- any(!is.na(grid$num[i, year_cols]))

    if (identical(layout, "demand_by_sector")) {
      if (!has_values) {
        mineral <- label
        next
      }
      row_mineral <- mineral
      row_sector <- label
    } else {
      if (!has_values) next
      row_mineral <- label
      # The by-mineral sheet reports one figure per mineral, with no sector split.
      row_sector <- "Total"
    }

    if (is.na(row_mineral)) next

    for (scenario in scenario_map$scenarios) {
      cols <- c(scenario_map$shared, scenario_map$by_scenario[[scenario]])
      rows[[length(rows) + 1L]] <- list(
        Pillar = pillar,
        Mineral = row_mineral,
        `Sector.Country` = row_sector,
        Scenario = scenario,
        years = grid$num[year_row, cols],
        values = grid$num[i, cols]
      )
    }
  }

  rows
}

# Flatten the supply sheet.
#
# Mining and refining sit side by side, each as its own label column plus year columns, so
# blocks are found by grouping contiguous year columns and taking the column to their left
# as that block's labels.
iea_critical_minerals_parse_supply <- function(grid, pillar) {
  year_row <- iea_critical_minerals_find_year_row(grid)
  if (is.na(year_row)) {
    stop("Could not locate the projection-year header row on sheet '", pillar, "'.")
  }

  year_cols <- iea_critical_minerals_year_columns(grid, year_row)
  breaks <- c(0, which(diff(year_cols) != 1), length(year_cols))
  blocks <- lapply(seq_len(length(breaks) - 1L), function(k) {
    cols <- year_cols[(breaks[k] + 1L):breaks[k + 1L]]
    list(label_col = min(cols) - 1L, year_cols = cols)
  })
  blocks <- Filter(function(b) b$label_col >= 1, blocks)

  rows <- list()
  mineral <- stats::setNames(rep(NA_character_, length(blocks)), seq_along(blocks))

  for (i in iea_critical_minerals_data_rows(grid, year_row)) {
    labels <- vapply(blocks, function(b) grid$txt[i, b$label_col], character(1))
    if (any(iea_critical_minerals_is_footnote(labels))) break

    for (k in seq_along(blocks)) {
      block <- blocks[[k]]
      label <- labels[[k]]
      if (!nzchar(label)) next

      if (!any(!is.na(grid$num[i, block$year_cols]))) {
        mineral[[k]] <- label
        next
      }
      if (is.na(mineral[[k]])) next

      rows[[length(rows) + 1L]] <- list(
        Pillar = pillar,
        Mineral = mineral[[k]],
        `Sector.Country` = label,
        Scenario = IEA_CRITICAL_MINERALS_BASE_CASE,
        years = grid$num[year_row, block$year_cols],
        values = grid$num[i, block$year_cols]
      )
    }
  }

  rows
}

# Turn parsed rows (each carrying its own year vector) into one wide frame.
iea_critical_minerals_bind_rows <- function(rows) {
  if (length(rows) == 0) {
    stop("No data rows parsed from the IEA critical minerals workbook.")
  }

  years <- sort(unique(unlist(lapply(rows, function(r) r$years))))
  year_names <- paste0("X", years)

  values <- matrix(NA_real_, nrow = length(rows), ncol = length(years),
                   dimnames = list(NULL, year_names))
  for (i in seq_along(rows)) {
    values[i, paste0("X", rows[[i]]$years)] <- rows[[i]]$values
  }

  out <- data.frame(
    Pillar = vapply(rows, function(r) r$Pillar, character(1)),
    Mineral = vapply(rows, function(r) r$Mineral, character(1)),
    Sector.Country = vapply(rows, function(r) r$`Sector.Country`, character(1)),
    Scenario = vapply(rows, function(r) r$Scenario, character(1)),
    stringsAsFactors = FALSE
  )

  cbind(out, as.data.frame(values, stringsAsFactors = FALSE))
}

iea_critical_minerals_read_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  specs <- iea_critical_minerals_sheet_specs()

  rows <- list()
  for (spec in specs) {
    matched <- sheets[grepl(spec$match, sheets)]
    if (length(matched) == 0) {
      warning(
        "IEA critical minerals workbook has no sheet matching '", spec$match,
        "' (expected the ", spec$pillar, " block); skipping it."
      )
      next
    }

    grid <- iea_critical_minerals_read_grid(path, matched[[1]])
    parsed <- if (identical(spec$layout, "supply_by_country")) {
      iea_critical_minerals_parse_supply(grid, spec$pillar)
    } else {
      iea_critical_minerals_parse_demand(grid, spec$pillar, spec$layout)
    }
    rows <- c(rows, parsed)
  }

  iea_critical_minerals_bind_rows(rows)
}

## Public entrypoint ----------------------------------------------------

# Read the IEA Critical Minerals Dataset from either the published workbook or a
# previously flattened CSV, and return the canonical long frame.
#
# `scenario` is matched case-insensitively against the demand sheets' scenario labels; the
# supply sheet's base case is always kept. Pass NULL to keep every scenario.
read_iea_critical_minerals <- function(path,
                                      scenario = IEA_CRITICAL_MINERALS_SCENARIO,
                                      quiet = FALSE) {
  if (!file.exists(path)) {
    stop("IEA critical minerals input not found: ", path)
  }

  critical <- if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    iea_critical_minerals_read_workbook(path)
  } else {
    read.csv(path, stringsAsFactors = FALSE)
  }

  required <- c("Pillar", "Mineral", "Sector.Country", "Scenario")
  missing <- setdiff(required, names(critical))
  if (length(missing) > 0) {
    stop(
      "IEA critical minerals input ", basename(path),
      " is missing required column(s): ", paste(missing, collapse = ", ")
    )
  }

  year_cols <- names(iea_critical_minerals_year_cols(critical))
  if (length(year_cols) == 0) {
    stop("IEA critical minerals input ", basename(path), " has no X<year> value columns.")
  }
  # Blank cells in the staged CSV read back as "", so coerce every value column.
  critical[year_cols] <- lapply(critical[year_cols], as.numeric)

  if (!is.null(scenario) && nzchar(scenario)) {
    matched <- grepl(scenario, critical$Scenario, ignore.case = TRUE)
    is_base_case <- critical$Scenario == IEA_CRITICAL_MINERALS_BASE_CASE

    # The base case is always kept, so it would mask a scenario name that matches nothing
    # and silently drop every demand row.
    if (any(!is_base_case) && !any(matched & !is_base_case)) {
      stop(
        "No demand rows in ", basename(path), " match scenario '", scenario, "'. Available: ",
        paste(sort(unique(critical$Scenario[!is_base_case])), collapse = ", ")
      )
    }
    critical <- critical[matched | is_base_case, , drop = FALSE]
  }

  if (!quiet) {
    message(
      "IEA Critical Minerals Dataset: read ", nrow(critical), " rows from ",
      basename(path), " (base year ", iea_critical_minerals_base_year(critical),
      "; years ", paste(sort(iea_critical_minerals_year_cols(critical)), collapse = ", "), ")"
    )
  }

  rownames(critical) <- NULL
  critical
}
