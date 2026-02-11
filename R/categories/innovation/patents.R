# ==============================================================================
# FILE: R/categories/innovation/patents.R
# ------------------------------------------------------------------------------
# Build patent activity by Country × tech × supply_chain × Year, aligned to this
# repo's tech/supply_chain taxonomy (e.g., from consolidated_hs6_energy_tech_long.csv).
#
# This is designed to be "drop-in":
# - Put this file at: R/categories/innovation/patents.R
# - Source it from your pipeline (e.g., scripts/10_build_themes.R) and call:
#     patents_tbl <- patents_innovation(patents_raw, subcat = subcat, country_info = country_info)
#
# INPUT (flexible): a patent export / extract (Lens, PATSTAT, OPS, etc.)
# Must include:
#   - A country column (inventor/applicant/assignee): e.g. inventor_country, applicant_country, Country, iso3c
#   - A year column: e.g. priority_year, filing_year, publication_year, Year
#   - At least one classification column: cpc, cpc_codes, ipc, ipc_codes (sep by ; , | or whitespace)
# Optional:
#   - title, abstract (improves Offshore Wind + Upstream detection)
#   - weight / count column (family_count, count, weight). Otherwise each record counts as 1.
#
# OUTPUT: standardized theme table (schema.R):
#   Country, tech, supply_chain, category, variable, data_type, value, Year, source, explanation
#
# IMPORTANT:
# - The default CPC/IPC mappings below are "good-enough starter heuristics".
#   You will want to tune TECH_RULES_PATENT / SUPPLY_CHAIN_RULES_PATENT once you see
#   false positives/negatives in your data.
# ==============================================================================

# ---- Internal deps (repo utilities) ------------------------------------------
if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "scurve.R"))
}
if (!exists("require_columns", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "schema.R"))
}
if (!exists("standardize_country_names", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "country.R"))
}

# ---- Helpers -----------------------------------------------------------------

patents_trim_chr <- function(x) {
  stringr::str_squish(as.character(dplyr::coalesce(x, "")))
}

patents_is_iso3 <- function(x) {
  x <- toupper(patents_trim_chr(x))
  stringr::str_detect(x, "^[A-Z]{3}$")
}

patents_guess_col <- function(tbl, candidates) {
  nms <- names(tbl)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

patents_split_codes <- function(x) {
  # Accept list-cols, vectors, or delimited strings
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, patents_split_codes))
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Split on common delimiters
  toks <- strsplit(x, "\\s*[;|,]\\s*|\\s+", perl = TRUE)
  toks <- lapply(toks, function(v) {
    v <- toupper(patents_trim_chr(v))
    v <- v[nzchar(v)]
    # normalize: remove obvious prefixes, spaces
    v <- stringr::str_replace_all(v, "^CPC\\s*:?\\s*|^IPC\\s*:?\\s*", "")
    v <- stringr::str_replace_all(v, "\\s+", "")
    # keep alnum plus "/" and "." (some datasets)
    v <- stringr::str_replace_all(v, "[^A-Z0-9/\\.\\-]", "")
    v[nzchar(v)]
  })
  toks
}

patents_text_blob <- function(title = NULL, abstract = NULL, extra = NULL) {
  parts <- c(title, abstract, extra)
  parts <- parts[!is.null(parts)]
  if (length(parts) == 0) return(rep("", 1))
  stringr::str_to_lower(stringr::str_squish(paste(parts, collapse = " | ")))
}

patents_match_any <- function(blob, patterns) {
  if (length(patterns) == 0) return(FALSE)
  any(purrr::map_lgl(patterns, ~ stringr::str_detect(blob, stringr::regex(.x, ignore_case = TRUE))))
}

patents_codes_blob <- function(codes_vec) {
  # codes_vec is a character vector
  codes_vec <- toupper(patents_trim_chr(codes_vec))
  codes_vec <- codes_vec[nzchar(codes_vec)]
  if (length(codes_vec) == 0) return("")
  paste(unique(codes_vec), collapse = " | ")
}

# ---- Default tech + supply-chain rules ---------------------------------------
# Each tech has:
#   - codes: regex patterns matched against CPC/IPC codes blob
#   - text:  regex/keywords matched against (title|abstract) blob
#
# ------------------------------------------------------------------------------
# TECH RULES (OECD ENV-TECH + IEA Y02 mapping as primary signals)
# Sources emphasize Y02 tags (CCMT) + Y04S (smart grids), with CPC/IPC backstops.
# - Wind: Y02E10/70-76
# - Offshore wind: Y02E10/727 (+ offshore-specific CPCs)
# - Solar: Y02E10/40-60
# - Batteries: Y02E60/10 (+ H01M)
# - Grid: Y02E40 + Y02E60/60 (HVDC) + Y04S (smart grids)
# - E-mobility: Y02T10/64-72 + Y02T90 (incl. EV charging)
# - Nuclear: Y02E30
# ------------------------------------------------------------------------------
TECH_RULES_PATENT <- list(
  "Wind" = list(
    codes = c(
      "^Y02E10/7[0-6]",       # Y02E10/70-76 (wind energy)
      "^F03D",                # wind motors (classic CPC)
      "^F05B"                 # wind engines (sometimes appears for components)
    ),
    text  = c("\\bwind\\b", "turbine", "nacelle", "blade", "rotor", "tower")
  ),
  
  "Offshore Wind" = list(
    codes = c(
      "^Y02E10/727",          # offshore wind (OECD ocean economy + wind subclasses)
      "^F03D13/25",           # offshore assembly/mounting/commissioning
      "^F05B2240/95",         # offshore mounting/support components
      "^B63B2035/446"         # floating structures converting wind -> electric
    ),
    text  = c("offshore", "floating", "monopile", "jacket", "subsea", "sea bed", "marine wind")
  ),
  
  "Solar" = list(
    codes = c(
      "^Y02E10/4",            # solar thermal (40-47)
      "^Y02E10/5",            # solar PV (50-56)
      "^Y02E10/6",            # solar thermal-PV hybrids (60)
      "^H02S", "^F24S"        # PV generation systems / solar heat (backstops)
    ),
    text  = c("\\bsolar\\b", "\\bpv\\b", "photovolta", "inverter", "module", "panel", "wafer", "polysilicon")
  ),
  
  "Batteries" = list(
    codes = c(
      "^Y02E60/10",           # batteries (energy storage -> batteries)
      "^H01M"                 # processes/means for batteries & cells
      # NOTE: we intentionally *don't* include Y02E60/13-16 (capacitors/thermal/mech storage)
      # because your taxonomy is "Batteries" not "Storage (all types)".
    ),
    text  = c("battery", "batteries", "li-ion", "lithium ion", "anode", "cathode", "electrolyte", "bms", "cell")
  ),
  
  "Electric Vehicles" = list(
    codes = c(
      "^Y02T10/6[4-9]",        # Y02T10/64-69 (EV subclasses)
      "^Y02T10/7[0-2]",        # Y02T10/70-72 (EV subclasses)
      "^Y02T90/1",             # EV charging Y02T90/10-167 (prefix-capture)
      "^B60L", "^B60W", "^B60K" # propulsion/control (backstops)
    ),
    text  = c("electric vehicle", "\\bev\\b", "\\bevs\\b", "charging", "charger", "powertrain", "traction motor")
  ),
  
  "Electric Grid" = list(
    codes = c(
      "^Y02E40",               # efficient power generation/transmission/distribution
      "^Y02E60/60",            # HVDC transmission
      "^Y04S",                 # smart grids (ancillary Y scheme)
      "^H02J", "^H02G", "^H02B", "^H01F", "^G01R"  # power systems, lines, transformers, measurement
    ),
    text  = c("\\bgrid\\b", "transmission", "distribution", "substation", "transformer", "switchgear", "hvdc", "inverter", "smart grid")
  ),
  
  "Nuclear" = list(
    codes = c(
      "^Y02E30",               # nuclear energy (fusion/fission subclasses under Y02E30)
      "^G21"                   # nuclear engineering (backstop)
    ),
    text  = c("\\bnuclear\\b", "reactor", "\\bsmr\\b", "uranium", "enrichment", "spent fuel")
  ),
  
  # Fossil techs: keep your existing "supply-side" oriented heuristics.
  # (IEA notes stress fossil searches should avoid end-use engines to prevent skew.)
  "Oil" = list(
    codes = c("^E21B", "^C10G", "^C10M", "^F16L", "^F17C"),
    text  = c("\\boil\\b", "petroleum", "crude", "refiner", "pipeline", "drilling", "wellbore", "enhanced recovery")
  ),
  
  "Gas" = list(
    codes = c("^E21B", "^C10L", "^F17C", "^F25J", "^F16L"),
    text  = c("\\bgas\\b", "natural gas", "\\blng\\b", "liquefaction", "regas", "pipeline", "methane")
  ),
  
  "Coal" = list(
    codes = c("^E21C", "^C10B", "^C10L"),
    text  = c("\\bcoal\\b", "coking", "coke oven", "lignite", "coal-fired")
  )
)


# Supply-chain rules:
# Upstream ~= extraction/mining/metallurgy/refining feedstocks (heuristic)
# Midstream ~= everything else (default)
SUPPLY_CHAIN_RULES_PATENT <- list(
  "Upstream" = list(
    codes = c("^E21B", "^E21C", "^C22B", "^B03B", "^C01G", "^C10G"), # drilling/mining/metallurgy/separation/refining
    text  = c("mining", "mine", "ore", "brine", "extraction", "beneficiat", "smelt", "smelting", "refin", "leach", "hydrometall")
  ),
  "Midstream" = list(
    codes = character(0),
    text  = c("manufactur", "assembly", "fabricat", "production line", "gigafactory", "module", "component")
  )
)

# ---- Core mapping functions ---------------------------------------------------

patents_detect_tech <- function(codes_vec, text_blob, tech_rules = TECH_RULES_PATENT) {
  codes_blob <- patents_codes_blob(codes_vec)
  
  # First pass: code/text hits
  hits <- names(purrr::keep(tech_rules, function(rule) {
    code_hit <- patents_match_any(codes_blob, rule$codes %||% character(0))
    text_hit <- patents_match_any(text_blob,  rule$text  %||% character(0))
    isTRUE(code_hit || text_hit)
  }))
  
  # Offshore Wind override: if "Wind" hit AND offshore-ish words exist, prefer Offshore Wind
  if ("Wind" %in% hits) {
    offshore_rule <- tech_rules[["Offshore Wind"]]
    if (!is.null(offshore_rule) && patents_match_any(text_blob, offshore_rule$text %||% character(0))) {
      hits <- setdiff(hits, "Wind")
      hits <- unique(c(hits, "Offshore Wind"))
    }
  }
  
  hits
}

patents_detect_supply_chain <- function(codes_vec, text_blob,
                                        sc_rules = SUPPLY_CHAIN_RULES_PATENT,
                                        default_sc = "Midstream") {
  codes_blob <- patents_codes_blob(codes_vec)
  
  hits <- names(purrr::keep(sc_rules, function(rule) {
    code_hit <- patents_match_any(codes_blob, rule$codes %||% character(0))
    text_hit <- patents_match_any(text_blob,  rule$text  %||% character(0))
    isTRUE(code_hit || text_hit)
  }))
  
  if (length(hits) == 0) return(default_sc)
  # If upstream is present, treat as upstream priority (common for mixed text)
  if ("Upstream" %in% hits) return("Upstream")
  hits
}

# ---- Public API ---------------------------------------------------------------

patents_innovation <- function(patents_raw,
                               subcat = NULL,
                               country_info = NULL,
                               tech_rules = TECH_RULES_PATENT,
                               supply_chain_rules = SUPPLY_CHAIN_RULES_PATENT,
                               keep_unmapped = FALSE,
                               split_counts = TRUE,
                               default_supply_chain = "Midstream",
                               gamma = 0.5,
                               source_label = "Patent data (user-provided export)",
                               variable_raw = "Patent families",
                               variable_index = "Patent index",
                               category = "Innovation") {
  stopifnot(inherits(patents_raw, "data.frame"))
  
  # Optional: allowed tech/sc universe from subcat (your HS6 -> tech/sc mapping)
  allowed_pairs <- NULL
  if (!is.null(subcat)) {
    require_columns(subcat, c("tech", "supply_chain"), label = "subcat")
    allowed_pairs <- subcat %>%
      dplyr::transmute(tech = as.character(tech), supply_chain = as.character(supply_chain)) %>%
      dplyr::distinct()
  }
  
  # Guess columns
  col_country <- patents_guess_col(patents_raw, c("inventor_country", "applicant_country", "assignee_country", "Country", "country", "iso3c", "iso3", "ISO3"))
  col_year    <- patents_guess_col(patents_raw, c("priority_year", "filing_year", "application_year", "publication_year", "pub_year", "Year", "year"))
  col_cpc     <- patents_guess_col(patents_raw, c("cpc", "cpc_codes", "cpc_classifications", "CPC"))
  col_ipc     <- patents_guess_col(patents_raw, c("ipc", "ipc_codes", "ipc_classifications", "IPC"))
  col_title   <- patents_guess_col(patents_raw, c("title", "invention_title", "Title"))
  col_abs     <- patents_guess_col(patents_raw, c("abstract", "Abstract", "invention_abstract"))
  col_weight  <- patents_guess_col(patents_raw, c("family_count", "count", "weight", "n", "patent_count"))
  
  if (is.na(col_country)) stop("patents_innovation(): could not find a country column in patents_raw.")
  if (is.na(col_year))    stop("patents_innovation(): could not find a year column in patents_raw.")
  if (is.na(col_cpc) && is.na(col_ipc)) {
    stop("patents_innovation(): need at least one classification column (cpc* or ipc*).")
  }
  
  # Normalize Country + Year
  base <- patents_raw %>%
    dplyr::transmute(
      .country_raw = patents_trim_chr(.data[[col_country]]),
      .year_raw    = suppressWarnings(as.integer(.data[[col_year]])),
      .cpc_raw     = if (!is.na(col_cpc)) .data[[col_cpc]] else NA,
      .ipc_raw     = if (!is.na(col_ipc)) .data[[col_ipc]] else NA,
      .title_raw   = if (!is.na(col_title)) patents_trim_chr(.data[[col_title]]) else "",
      .abs_raw     = if (!is.na(col_abs)) patents_trim_chr(.data[[col_abs]]) else "",
      .w_raw       = if (!is.na(col_weight)) suppressWarnings(as.numeric(.data[[col_weight]])) else NA_real_
    ) %>%
    dplyr::mutate(
      weight = dplyr::coalesce(.data$.w_raw, 1),
      Year = .data$.year_raw
    ) %>%
    dplyr::filter(!is.na(Year))
  
  # Standardize to Country names, using country_info when possible
  if (!is.null(country_info)) {
    require_columns(country_info, c("iso3c", "country"), label = "country_info")
    lookup <- country_info %>%
      dplyr::transmute(
        iso3c = toupper(as.character(iso3c)),
        country = standardize_country_names(country)
      ) %>%
      dplyr::distinct(iso3c, country)
    
    base <- base %>%
      dplyr::mutate(
        iso3c_guess = dplyr::case_when(
          patents_is_iso3(.data$.country_raw) ~ toupper(.data$.country_raw),
          TRUE ~ NA_character_
        ),
        Country = dplyr::case_when(
          !is.na(.data$iso3c_guess) ~ lookup$country[match(.data$iso3c_guess, lookup$iso3c)],
          TRUE ~ standardize_country_names(.data$.country_raw)
        )
      ) %>%
      dplyr::filter(!is.na(Country), nzchar(Country)) %>%
      # Keep only countries in country_info universe (matches what your other builders do)
      standardize_country_table(country_info = country_info)
  } else {
    base <- base %>%
      dplyr::mutate(Country = standardize_country_names(.data$.country_raw)) %>%
      dplyr::filter(!is.na(Country), nzchar(Country))
  }
  
  # Parse CPC/IPC codes into a single list-col
  cpc_list <- if (!is.na(col_cpc)) patents_split_codes(base$.cpc_raw) else replicate(nrow(base), character(0), simplify = FALSE)
  ipc_list <- if (!is.na(col_ipc)) patents_split_codes(base$.ipc_raw) else replicate(nrow(base), character(0), simplify = FALSE)
  
  # Build per-record assignment
  assigned <- base %>%
    dplyr::mutate(
      codes = purrr::map2(cpc_list, ipc_list, ~ unique(c(.x, .y))),
      text_blob = purrr::pmap_chr(
        list(.data$.title_raw, .data$.abs_raw),
        ~ stringr::str_to_lower(stringr::str_squish(paste(..1, ..2, sep = " | ")))
      ),
      tech_hits = purrr::map2(.data$codes, .data$text_blob, ~ patents_detect_tech(.x, .y, tech_rules = tech_rules)),
      sc_hit = purrr::map2(.data$codes, .data$text_blob, ~ patents_detect_supply_chain(.x, .y, sc_rules = supply_chain_rules, default_sc = default_supply_chain))
    )
  
  if (!keep_unmapped) {
    assigned <- assigned %>%
      dplyr::mutate(tech_hits = purrr::map(.data$tech_hits, ~ .x[!is.na(.x) & nzchar(.x)])) %>%
      dplyr::filter(purrr::map_int(.data$tech_hits, length) > 0)
  } else {
    assigned <- assigned %>%
      dplyr::mutate(tech_hits = purrr::map(.data$tech_hits, ~ if (length(.x) == 0) "Unmapped" else .x))
  }
  
  # Expand to tech × supply_chain rows (fractional allocation by default)
  long <- assigned %>%
    dplyr::mutate(
      supply_chain_hits = purrr::map(.data$sc_hit, ~ {
        v <- as.character(.x)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v) == 0) default_supply_chain else v
      }),
      expanded = purrr::map2(.data$tech_hits, .data$supply_chain_hits, ~ tidyr::expand_grid(
        tech = .x,
        supply_chain = .y
      )),
      n_combo = purrr::map_int(.data$expanded, nrow)
    ) %>%
    tidyr::unnest(.data$expanded) %>%
    dplyr::mutate(
      alloc = if (isTRUE(split_counts)) .data$weight / pmax(1, .data$n_combo) else .data$weight
    ) %>%
    dplyr::select(Country, Year, tech, supply_chain, alloc)
  
  # Restrict to repo's allowed tech/sc pairs if provided
  if (!is.null(allowed_pairs)) {
    long <- long %>%
      dplyr::inner_join(allowed_pairs, by = c("tech", "supply_chain"))
  }
  
  # Aggregate
  counts <- long %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::summarise(patents = sum(alloc, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(patents = dplyr::coalesce(patents, 0))
  
  # Optional: complete zeros for full universe (countries × pairs × years)
  # If you want this, pass country_info + subcat (so we know universes).
  if (!is.null(country_info) && !is.null(allowed_pairs)) {
    countries_univ <- country_info %>%
      dplyr::transmute(Country = standardize_country_names(country)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(Country), nzchar(Country))
    
    years_univ <- sort(unique(counts$Year))
    counts <- tidyr::complete(
      counts,
      Country = countries_univ$Country,
      tech = allowed_pairs$tech,
      supply_chain = allowed_pairs$supply_chain,
      Year = years_univ,
      fill = list(patents = 0)
    )
  }
  
  # Build an index per (tech, supply_chain, Year) across countries (log1p dampens skew)
  counts <- counts %>%
    dplyr::group_by(tech, supply_chain, Year) %>%
    dplyr::mutate(
      patents_log = log1p(patents),
      patent_index = median_scurve(patents_log, gamma = gamma)
    ) %>%
    dplyr::ungroup()
  
  # Assemble standardized theme table
  out_raw <- counts %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = category,
      variable = variable_raw,
      data_type = "raw",
      value = as.numeric(patents),
      Year = as.integer(Year),
      source = source_label,
      explanation = "Count of patent records allocated to tech/supply_chain (fractional if split_counts = TRUE)."
    )
  
  out_idx <- counts %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = category,
      variable = variable_index,
      data_type = "index",
      value = as.numeric(patent_index),
      Year = as.integer(Year),
      source = source_label,
      explanation = "Index = median_scurve(log1p(patent_count)) computed across countries within each tech/supply_chain/year."
    )
  
  dplyr::bind_rows(out_raw, out_idx) %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      value = suppressWarnings(as.numeric(value)),
      Year = as.integer(Year),
      source = as.character(source),
      explanation = as.character(explanation)
    )
}

# Convenience wrapper: read from a CSV/Parquet/RDS path
patents_innovation_from_path <- function(path,
                                         reader = c("auto", "csv", "rds"),
                                         ...) {
  reader <- match.arg(reader)
  if (reader == "auto") {
    if (stringr::str_detect(tolower(path), "\\.rds$")) reader <- "rds" else reader <- "csv"
  }
  if (reader == "rds") {
    patents_raw <- readRDS(path)
  } else {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("Package 'readr' is required to read CSV patent inputs.")
    }
    patents_raw <- readr::read_csv(path, show_col_types = FALSE)
  }
  patents_innovation(patents_raw, ...)
}

# Small infix helper (like rlang %||%)
`%||%` <- function(a, b) if (!is.null(a)) a else b
