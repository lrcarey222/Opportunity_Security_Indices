# ==============================================================================
# GTA / NIPO POLICY INDEX (ENHANCED + DOCUMENTED)
# ------------------------------------------------------------------------------
# What this does (high-level):
#   1) Cleans raw NIPO/GTA inventory rows and maps HS6 codes -> (Technology, Value Chain)
#   2) Builds per-policy "strength" measures with interpretable components:
#        - Bite strength (bindingness proxy): type × status × jurisdiction × scope × duration
#        - Scale strength: bite × (breadth × geographic reach × money/coverage scale)
#        - Package strength: multiplies by a capped "policy mix" multiplier at State Act level
#   3) Converts the underlying event stream into a STOCK index:
#        - Active as-of a chosen date (defaults to latest date in the dataset)
#        - Separately tracks flows over a lookback window (new implemented / removed)
#   4) Aggregates to Country × Tech × SupplyChain, and maps sums into a 0-1 index
#      using median_scurve(log1p(sum)) to reduce outlier dominance.
# ==============================================================================
# ---- Dependencies -------------------------------------------------------------
# These are internal project utilities used elsewhere in your repo.
# - median_scurve(): monotonic mapping of values to 0-1; robust-ish to outliers
# - standardize_country_names(): ensures consistent Implementing Jurisdiction names

if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "scurve.R"))
}

if (!exists("standardize_country_names", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "country.R"))
}

# ==============================================================================
# 2) Core weights (type, status, jurisdiction)
# ==============================================================================

# ---- POLICY_TYPE_WEIGHTS ------------------------------------------------------
# Normative "bite" weights for instrument types on 0-1 scale.
# (You can swap this table without touching the rest of the pipeline.)
POLICY_TYPE_WEIGHTS <- tibble::tribble(
  ~intervention_type, ~w_type,
  "Import tariff", 0.75,
  "Internal taxation of imports", 0.70,
  "Tax or social insurance relief", 0.75,
  "Export tax", 0.70,
  "Import tariff quota", 0.80,
  "Tax-based export incentive", 0.80,
  "FDI: Entry and ownership rule", 0.75,
  "State aid, unspecified", 0.75,
  "Financial assistance in foreign market", 0.70,
  "Anti-dumping", 0.75,
  "State loan", 0.75,
  "Loan guarantee", 0.70,
  "Localisation, nes", 0.80,
  "Controls on commercial transactions and investment instruments", 0.65,
  "Financial grant", 0.90,
  "Capital injection and equity stakes (including bailouts)", 0.90,
  "Export-related non-tariff measure, nes", 0.70,
  "Other import charges", 0.60,
  "Local content incentive", 0.80,
  "Export ban", 1.00,
  "Anti-subsidy", 0.75,
  "Local content requirement", 0.95,
  "Import-related non-tariff measure, nes", 0.70,
  "Export licensing requirement", 0.65,
  "Import licensing requirement", 0.65,
  "In-kind grant", 0.90,
  "FDI: Financial incentive", 0.75,
  "Trade finance", 0.70,
  "Import price benchmark", 0.55,
  "Production subsidy", 0.90,
  "Price stabilisation", 0.70,
  "Export price benchmark", 0.55,
  "Import ban", 1.00,
  "FDI: Treatment and operations, nes", 0.65,
  "Export tariff quota", 0.75,
  "Interest payment subsidy", 0.80,
  "Import quota", 0.95,
  "Public procurement localisation", 0.85,
  "Public procurement, nes", 0.70,
  "Instrument unclear", 0.20,
  "Safeguard", 0.75,
  "Export quota", 0.90,
  "Import incentive", 0.65,
  "Local operations incentive", 0.75,
  "Import monitoring", 0.35,
  "Local value added incentive", 0.75,
  "Other export incentive", 0.65,
  "Local labour incentive", 0.70,
  "Public procurement preference margin", 0.75,
  "State aid, nes", 0.75,
  "Public procurement access", 0.70,
  "Foreign customer limit", 0.85,
  "Controls on credit operations", 0.65,
  "Local supply requirement for exports", 0.85,
  "Intellectual property protection", 0.50,
  "Export subsidy", 0.90,
  "Trade payment measure", 0.65,
  "Local labour requirement", 0.85,
  "Local operations requirement", 0.95,
  "Anti-circumvention", 0.65,
  "Minimum import price", 0.80,
  "Labour market access", 0.60,
  "Control on personal transactions", 0.55,
  "Port restriction", 0.80,
  "Post-migration treatment", 0.45,
  "Repatriation & surrender requirements", 0.75,
  "Trade balancing measure", 0.80,
  "Technical barrier to trade", 0.70,
  "Competitive devaluation", 0.75
)

# ---- STATUS_WEIGHTS -----------------------------------------------------------
# Interprets GTA's initial assessment as a multiplier + we also derive sign.
STATUS_WEIGHTS <- tibble::tribble(
  ~status, ~w_status,
  "Discriminatory", 1.00,
  "Liberalising", 0.60,
  "Neutral", 0.50,
  "Unclear", 0.30,
  "Unknown", 0.30
)

# ---- JURIS_WEIGHTS ------------------------------------------------------------
# Where implemented: national tends to have larger reach than local/international tagging.
JURIS_WEIGHTS <- tibble::tribble(
  ~jurisdiction, ~w_juris,
  "National", 1.00,
  "Subnational", 0.5,
  "State", 0.5,
  "Province", 0.5,
  "Regional", 0.5,
  "Local", 0.25,
  "International", 0.20,
  "Unknown", 0.30
)

# ==============================================================================
# 0) Helper functions
# ==============================================================================

# ---- validation --------------------------------------------------------------
check_required_columns <- function(tbl, cols, tbl_name = "table") {
  missing <- setdiff(cols, names(tbl))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing required columns: %s", tbl_name, paste(missing, collapse = ", ")))
  }
}

# ---- dates -------------------------------------------------------------------
as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x)
}

# ---- parsing / normalization -------------------------------------------------
as_bool <- function(x) {
  if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::coalesce(x_chr %in% c("true", "t", "1", "yes", "y"), FALSE)
}

normalize_chr_vec <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

safe_list_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0) character(0) else normalize_chr_vec(x)
}

parse_code_list <- function(x, width = 3) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) return(character(0))
  toks <- unlist(strsplit(x, "\\s*,\\s*"))
  toks <- stringr::str_replace_all(toks, "\\D", "")
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0) return(character(0))
  toks <- substr(toks, 1, width)
  toks <- stringr::str_pad(toks, width = width, pad = "0")
  unique(toks)
}

# ---- multipliers -------------------------------------------------------------
count_csv_tokens <- function(x) {
  vapply(x, function(v) {
    v <- as.character(v)
    if (is.na(v) || !nzchar(v)) return(0L)
    toks <- unlist(strsplit(v, "\\s*,\\s*"))
    toks <- unique(toks[nzchar(toks)])
    length(toks)
  }, integer(1))
}

calc_p95 <- function(x, fallback = 1) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  x <- x[x >= 0]
  if (length(x) == 0) return(fallback)
  p <- as.numeric(stats::quantile(x, probs = 0.95, na.rm = TRUE, names = FALSE, type = 7))
  if (!is.finite(p) || p <= 0) fallback else p
}

log_mult <- function(x, p95) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  x <- pmax(0, x)
  if (!is.finite(p95) || p95 <= 0) return(rep(1, length(x)))
  denom <- log1p(p95)
  if (!is.finite(denom) || denom <= 0) return(rep(1, length(x)))
  1 + (log1p(x) / denom)
}

cap_mult <- function(x, cap = 2.5) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 1
  pmin(cap, pmax(0, x))
}

# ---- CPC helpers -------------------------------------------------------------
get_cpc_hs_map <- function() {
  if (!requireNamespace("gtalibrary", quietly = TRUE)) {
    return(tibble::tibble(cpc3 = character(0), hs6 = character(0)))
  }
  df <- gtalibrary::cpc.to.hs
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(cpc3 = character(0), hs6 = character(0)))
  }
  nms <- tolower(names(df))

  cpc_col <- names(df)[which(grepl("^cpc$", nms) | grepl("cpc", nms))[1]]
  hs_col  <- names(df)[which(grepl("^hs$", nms) | grepl("hs", nms))[1]]

  if (is.na(cpc_col) || is.na(hs_col)) {
    return(tibble::tibble(cpc3 = character(0), hs6 = character(0)))
  }

  df %>%
    dplyr::transmute(
      cpc3 = stringr::str_pad(stringr::str_replace_all(as.character(.data[[cpc_col]]), "\\D", ""), 3, pad = "0"),
      hs6  = stringr::str_pad(stringr::str_replace_all(as.character(.data[[hs_col]]),  "\\D", ""), 6, pad = "0")
    ) %>%
    dplyr::mutate(
      cpc3 = substr(.data$cpc3, 1, 3),
      hs6  = substr(.data$hs6, 1, 6)
    ) %>%
    dplyr::filter(nzchar(.data$cpc3), nzchar(.data$hs6)) %>%
    dplyr::distinct(.data$cpc3, .data$hs6)
}

get_cpc3_names <- function() {
  if (!requireNamespace("gtalibrary", quietly = TRUE)) {
    return(tibble::tibble(cpc3 = character(0), cpc.name = character(0)))
  }
  df <- gtalibrary::cpc.names
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(cpc3 = character(0), cpc.name = character(0)))
  }
  nms <- tolower(names(df))

  code_col <- names(df)[which(nms %in% c("cpc", "code", "cpc_code") | grepl("^cpc$", nms) | grepl("cpc", nms))[1]]
  name_col <- names(df)[which(nms %in% c("cpc.name", "cpc_name", "name", "title", "description") |
                                grepl("name|title|desc", nms))[1]]
  lvl_col  <- names(df)[which(nms %in% c("cpc.digit.level", "cpc_digit_level", "digit.level", "digit_level", "level") |
                                grepl("digit|level", nms))[1]]

  if (is.na(code_col) || is.na(name_col)) {
    return(tibble::tibble(cpc3 = character(0), cpc.name = character(0)))
  }

  df %>%
    dplyr::transmute(
      cpc3 = stringr::str_pad(stringr::str_replace_all(as.character(.data[[code_col]]), "\\D", ""), 3, pad = "0"),
      cpc.name = stringr::str_squish(as.character(.data[[name_col]])),
      lvl = if (!is.na(lvl_col)) suppressWarnings(as.integer(.data[[lvl_col]])) else NA_integer_
    ) %>%
    dplyr::mutate(cpc3 = substr(.data$cpc3, 1, 3)) %>%
    dplyr::filter(
      nzchar(.data$cpc3),
      if (!all(is.na(.data$lvl))) .data$lvl == 3 else TRUE
    ) %>%
    dplyr::select(.data$cpc3, .data$cpc.name) %>%
    dplyr::distinct(.data$cpc3, .keep_all = TRUE)
}

build_cpc3_to_tech_sc_pairs <- function(subcat_raw, cpc_hs) {
  if (nrow(cpc_hs) == 0) {
    return(tibble::tibble(cpc3 = character(0), allowed_pairs_cpc = list(character(0))))
  }
  hs_col <- dplyr::case_when(
    "HS6" %in% names(subcat_raw) ~ "HS6",
    "hs6" %in% names(subcat_raw) ~ "hs6",
    TRUE ~ NA_character_
  )
  tech_col <- dplyr::case_when(
    "Technology" %in% names(subcat_raw) ~ "Technology",
    "tech" %in% names(subcat_raw) ~ "tech",
    TRUE ~ NA_character_
  )
  sc_col <- dplyr::case_when(
    "Value Chain" %in% names(subcat_raw) ~ "Value Chain",
    "supply_chain" %in% names(subcat_raw) ~ "supply_chain",
    TRUE ~ NA_character_
  )

  if (is.na(hs_col) || is.na(tech_col) || is.na(sc_col)) {
    stop("subcat_raw must contain HS6 + Technology/Value Chain (or hs6 + tech/supply_chain).")
  }

  sub <- subcat_raw %>%
    dplyr::transmute(
      hs6 = stringr::str_pad(stringr::str_replace_all(as.character(.data[[hs_col]]), "\\D", ""), 6, pad = "0"),
      tech = as.character(.data[[tech_col]]),
      supply_chain = as.character(.data[[sc_col]])
    ) %>%
    dplyr::filter(nzchar(.data$hs6), nzchar(.data$tech), nzchar(.data$supply_chain)) %>%
    dplyr::distinct(.data$hs6, .data$tech, .data$supply_chain)

  sub %>%
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6")) %>%
    dplyr::filter(!is.na(.data$cpc3)) %>%
    dplyr::mutate(pair = paste(.data$tech, .data$supply_chain, sep = "||")) %>%
    dplyr::group_by(.data$cpc3) %>%
    dplyr::summarise(
      allowed_pairs_cpc = list(sort(unique(.data$pair))),
      .groups = "drop"
    )
}

attach_cpc_validation <- function(nipo_country, cpc_hs, cpc_pairs) {
  if (!("cpc3_codes" %in% names(nipo_country))) {
    nipo_country$cpc3_codes <- replicate(nrow(nipo_country), character(0), simplify = FALSE)
    nipo_country$cpc3_n <- 0L
  }
  if (!("hs6_codes" %in% names(nipo_country))) {
    nipo_country$hs6_codes <- replicate(nrow(nipo_country), character(0), simplify = FALSE)
    nipo_country$total_hs6 <- 0L
    nipo_country$matched_hs6 <- 0L
  }

  if (nrow(cpc_hs) == 0 || nrow(cpc_pairs) == 0) {
    return(nipo_country %>%
      dplyr::mutate(
        allowed_pairs_cpc = replicate(n(), character(0), simplify = FALSE),
        hs6_cpc_match_rate = NA_real_,
        hs6_cpc_unmapped_rate = NA_real_
      ))
  }

  cpc_long <- nipo_country %>%
    dplyr::select(.data$nipo_row_id, .data$cpc3_codes) %>%
    tidyr::unnest(.data$cpc3_codes) %>%
    dplyr::rename(cpc3 = .data$cpc3_codes) %>%
    dplyr::filter(nzchar(.data$cpc3)) %>%
    dplyr::distinct()

  allowed <- cpc_long %>%
    dplyr::left_join(cpc_pairs, by = "cpc3") %>%
    dplyr::group_by(.data$nipo_row_id) %>%
    dplyr::summarise(
      allowed_pairs_cpc = list(sort(unique(unlist(.data$allowed_pairs_cpc)))),
      .groups = "drop"
    )

  hs6_long <- nipo_country %>%
    dplyr::select(.data$nipo_row_id, .data$hs6_codes) %>%
    tidyr::unnest(.data$hs6_codes) %>%
    dplyr::rename(hs6 = .data$hs6_codes) %>%
    dplyr::mutate(hs6 = stringr::str_pad(stringr::str_replace_all(as.character(.data$hs6), "\\D", ""), 6, pad = "0")) %>%
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6"))

  hs6_has_map <- hs6_long %>%
    dplyr::group_by(.data$nipo_row_id, .data$hs6) %>%
    dplyr::summarise(has_map = any(!is.na(.data$cpc3)), .groups = "drop")

  hs6_hits <- hs6_long %>%
    dplyr::filter(!is.na(.data$cpc3)) %>%
    dplyr::semi_join(cpc_long, by = c("nipo_row_id", "cpc3")) %>%
    dplyr::distinct(.data$nipo_row_id, .data$hs6) %>%
    dplyr::mutate(hit = TRUE)

  hs6_status <- hs6_has_map %>%
    dplyr::left_join(hs6_hits, by = c("nipo_row_id", "hs6")) %>%
    dplyr::mutate(
      hit = dplyr::coalesce(.data$hit, FALSE),
      unmapped = !.data$has_map
    )

  hs6_row_rates <- hs6_status %>%
    dplyr::group_by(.data$nipo_row_id) %>%
    dplyr::summarise(
      hs6_cpc_match_rate = {
        row_id <- .data$nipo_row_id[1]
        has_cpc_basket <- nipo_country$cpc3_n[match(row_id, nipo_country$nipo_row_id)] > 0
        if (isTRUE(has_cpc_basket)) mean(.data$hit, na.rm = TRUE) else NA_real_
      },
      hs6_cpc_unmapped_rate = mean(.data$unmapped, na.rm = TRUE),
      .groups = "drop"
    )

  nipo_country %>%
    dplyr::left_join(allowed, by = "nipo_row_id") %>%
    dplyr::left_join(hs6_row_rates, by = "nipo_row_id") %>%
    dplyr::mutate(
      allowed_pairs_cpc = purrr::map(.data$allowed_pairs_cpc, ~ if (is.null(.x)) character(0) else .x),
      hs6_cpc_match_rate = dplyr::coalesce(.data$hs6_cpc_match_rate, NA_real_),
      hs6_cpc_unmapped_rate = dplyr::coalesce(.data$hs6_cpc_unmapped_rate, NA_real_)
    )
}

attach_policy_cpc_names <- function(policy_tbl, cpc_names) {
  if (!("cpc3_codes" %in% names(policy_tbl))) {
    policy_tbl$cpc3_codes <- replicate(nrow(policy_tbl), character(0), simplify = FALSE)
    policy_tbl$cpc3_n <- 0L
  }
  if (is.null(cpc_names) || nrow(cpc_names) == 0) {
    policy_tbl$cpc3_codes_csv <- NA_character_
    policy_tbl$cpc_name_csv <- NA_character_
    return(policy_tbl)
  }

  policy_tbl %>%
    dplyr::mutate(
      cpc3_codes_csv = purrr::map_chr(.data$cpc3_codes, ~ if (length(.x) == 0) NA_character_ else paste(.x, collapse = " | ")),
      cpc_name_csv = purrr::map_chr(.data$cpc3_codes, ~ {
        if (length(.x) == 0) return(NA_character_)
        nm <- cpc_names$cpc.name[match(.x, cpc_names$cpc3)]
        nm <- unique(nm[!is.na(nm) & nzchar(nm)])
        if (length(nm) == 0) NA_character_ else paste(nm, collapse = " | ")
      })
    )
}

build_hs6_cpc_lookup <- function(cpc_hs, cpc_names) {
  if (nrow(cpc_hs) == 0) {
    return(tibble::tibble(HS6 = character(0), cpc3_codes_csv = character(0), cpc_name_csv = character(0)))
  }
  lu <- cpc_hs %>%
    dplyr::group_by(.data$hs6) %>%
    dplyr::summarise(
      cpc3 = list(sort(unique(.data$cpc3))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cpc3_codes_csv = purrr::map_chr(.data$cpc3, ~ if (length(.x) == 0) NA_character_ else paste(.x, collapse = " | "))
    )

  if (!is.null(cpc_names) && nrow(cpc_names) > 0) {
    lu <- lu %>%
      tidyr::unnest(.data$cpc3) %>%
      dplyr::left_join(cpc_names, by = c("cpc3" = "cpc3")) %>%
      dplyr::group_by(.data$hs6) %>%
      dplyr::summarise(
        cpc3_codes_csv = dplyr::first(.data$cpc3_codes_csv),
        cpc_name_csv = {
          nm <- unique(.data$cpc.name[!is.na(.data$cpc.name) & nzchar(.data$cpc.name)])
          if (length(nm) == 0) NA_character_ else paste(nm, collapse = " | ")
        },
        .groups = "drop"
      )
  } else {
    lu <- lu %>% dplyr::mutate(cpc_name_csv = NA_character_)
  }

  lu %>% dplyr::rename(HS6 = .data$hs6)
}

build_tech_sc_cpc_lookup <- function(subcat_raw, cpc_hs, cpc_names) {
  if (nrow(cpc_hs) == 0) {
    return(tibble::tibble(tech = character(0), supply_chain = character(0),
                          cpc3_codes_csv = character(0), cpc_name_csv = character(0)))
  }
  check_required_columns(subcat_raw, c("HS6", "Technology", "Value Chain"), "subcat_raw")

  tech_sc <- subcat_raw %>%
    dplyr::transmute(
      hs6 = stringr::str_pad(stringr::str_replace_all(as.character(.data$HS6), "\\D", ""), 6, pad = "0"),
      tech = as.character(.data$Technology),
      supply_chain = as.character(.data$`Value Chain`)
    ) %>%
    dplyr::filter(nzchar(.data$hs6), nzchar(.data$tech), nzchar(.data$supply_chain)) %>%
    dplyr::distinct()

  lu <- tech_sc %>%
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6")) %>%
    dplyr::filter(!is.na(.data$cpc3)) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      cpc3 = list(sort(unique(.data$cpc3))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cpc3_codes_csv = purrr::map_chr(.data$cpc3, ~ if (length(.x) == 0) NA_character_ else paste(.x, collapse = " | "))
    )

  if (!is.null(cpc_names) && nrow(cpc_names) > 0) {
    lu <- lu %>%
      tidyr::unnest(.data$cpc3) %>%
      dplyr::left_join(cpc_names, by = c("cpc3" = "cpc3")) %>%
      dplyr::group_by(.data$tech, .data$supply_chain) %>%
      dplyr::summarise(
        cpc3_codes_csv = dplyr::first(.data$cpc3_codes_csv),
        cpc_name_csv = {
          nm <- unique(.data$cpc.name[!is.na(.data$cpc.name) & nzchar(.data$cpc.name)])
          if (length(nm) == 0) NA_character_ else paste(nm, collapse = " | ")
        },
        .groups = "drop"
      )
  } else {
    lu <- lu %>% dplyr::mutate(cpc_name_csv = NA_character_)
  }

  lu
}

build_tech_cpc_lookup <- function(tech_sc_cpc_lu) {
  if (nrow(tech_sc_cpc_lu) == 0) {
    return(tibble::tibble(tech = character(0), cpc3_codes_csv = character(0), cpc_name_csv = character(0)))
  }
  tech_sc_cpc_lu %>%
    dplyr::group_by(.data$tech) %>%
    dplyr::summarise(
      cpc3_codes_csv = {
        codes <- unique(unlist(strsplit(dplyr::coalesce(.data$cpc3_codes_csv, ""), "\\s*\\|\\s*")))
        codes <- codes[nzchar(codes)]
        if (length(codes) == 0) NA_character_ else paste(sort(unique(codes)), collapse = " | ")
      },
      cpc_name_csv = {
        name_vals <- unique(unlist(strsplit(dplyr::coalesce(.data$cpc_name_csv, ""), "\\s*\\|\\s*")))
        name_vals <- name_vals[nzchar(name_vals)]
        if (length(name_vals) == 0) NA_character_ else paste(sort(unique(name_vals)), collapse = " | ")
      },
      .groups = "drop"
    )
}

# ---- allocation helpers ------------------------------------------------------
make_validated_combos <- function(techs, scs, allowed_pairs) {
  techs <- safe_list_or_empty(techs)
  scs   <- safe_list_or_empty(scs)
  if (length(techs) == 0 || length(scs) == 0) {
    return(tibble::tibble(tech = character(0), supply_chain = character(0), cpc_validation_failed = logical(0)))
  }
  combos <- utils::expand.grid(tech = techs, supply_chain = scs, stringsAsFactors = FALSE)
  if (!is.null(allowed_pairs) && length(allowed_pairs) > 0) {
    pair <- paste(combos$tech, combos$supply_chain, sep = "||")
    keep <- pair %in% allowed_pairs
    if (any(keep)) {
      combos <- combos[keep, , drop = FALSE]
      combos$cpc_validation_failed <- FALSE
    } else {
      combos$cpc_validation_failed <- TRUE
    }
  } else {
    combos$cpc_validation_failed <- FALSE
  }
  tibble::as_tibble(combos)
}

expand_cross_cutting_rows <- function(tbl,
                                      tech_universe,
                                      supply_chain_universe,
                                      split_strength = TRUE) {
  tech_universe <- setdiff(unique(tech_universe), c("Cross-cutting", "Unmapped"))
  supply_chain_universe <- setdiff(unique(supply_chain_universe), c("Cross-cutting", "Unmapped"))

  tbl %>%
    dplyr::mutate(
      tech_targets = purrr::map(.data$tech, ~ if (.x == "Cross-cutting") tech_universe else .x),
      sc_targets   = purrr::map(.data$supply_chain, ~ if (.x == "Cross-cutting") supply_chain_universe else .x),
      expanded     = purrr::map2(.data$tech_targets, .data$sc_targets, ~ tidyr::expand_grid(
        tech_exp = .x,
        sc_exp = .y
      )),
      expansion_n  = purrr::map_int(.data$expanded, nrow)
    ) %>%
    dplyr::select(-.data$tech_targets, -.data$sc_targets) %>%
    tidyr::unnest(.data$expanded) %>%
    dplyr::mutate(
      tech = .data$tech_exp,
      supply_chain = .data$sc_exp,
      alloc = if (isTRUE(split_strength)) .data$alloc / pmax(1, .data$expansion_n) else .data$alloc
    ) %>%
    dplyr::select(-.data$tech_exp, -.data$sc_exp, -.data$expansion_n)
}

allocate_policy_to_tech_sc <- function(policy_tbl) {
  policy_shares <- policy_tbl %>%
    dplyr::mutate(
      mapped_share = dplyr::if_else(.data$hs6_n > 0,
                                    pmin(1, .data$matched_hs6_n / pmax(1, .data$hs6_n)),
                                    0),
      tech_mapped = purrr::map(.data$Technology, safe_list_or_empty),
      sc_mapped   = purrr::map(.data$`Value Chain`, safe_list_or_empty),
      mapped_share = dplyr::if_else(
        (purrr::map_int(.data$tech_mapped, length) == 0) |
          (purrr::map_int(.data$sc_mapped, length) == 0),
        0,
        .data$mapped_share
      ),
      unmapped_share = dplyr::if_else(.data$hs6_n > 0, pmax(0, 1 - .data$mapped_share), 0),
      allowed_pairs_cpc = purrr::map(dplyr::coalesce(.data$allowed_pairs_cpc, list(character(0))),
                                     ~ if (is.null(.x)) character(0) else .x)
    )

  mapped_long <- policy_shares %>%
    dplyr::filter(.data$mapped_share > 0) %>%
    dplyr::mutate(
      combos = purrr::pmap(
        list(.data$tech_mapped, .data$sc_mapped, .data$allowed_pairs_cpc),
        make_validated_combos
      ),
      n_combo = purrr::map_int(.data$combos, nrow)
    ) %>%
    dplyr::filter(.data$n_combo > 0) %>%
    tidyr::unnest(.data$combos) %>%
    dplyr::mutate(
      alloc = .data$mapped_share / pmax(1, .data$n_combo),
      is_crosscutting_policy = FALSE
    ) %>%
    dplyr::select(-.data$n_combo)

  unmapped_long <- policy_shares %>%
    dplyr::filter(.data$unmapped_share > 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Unmapped",
      supply_chain = "Unmapped",
      alloc = .data$unmapped_share,
      cpc_validation_failed = FALSE,
      is_crosscutting_policy = FALSE
    )

  cross_long <- policy_shares %>%
    dplyr::filter(.data$hs6_n == 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Cross-cutting",
      supply_chain = "Cross-cutting",
      alloc = 1,
      cpc_validation_failed = FALSE,
      is_crosscutting_policy = TRUE
    )

  dplyr::bind_rows(mapped_long, unmapped_long, cross_long)
}

# ==============================================================================
# 1) Clean NIPO raw
#    - Keeps HS6 list as a list-column (hs6_codes)
#    - Computes distinct total_hs6 and matched_hs6 (not inflated by many-to-many joins)
# ==============================================================================

clean_nipo_raw <- function(raw_nipo, subcat_raw, country_info = NULL) {
  check_required_columns(raw_nipo, c("Product: HS 6-digit (2022)", "Implementing Jurisdiction"), "raw_nipo")
  check_required_columns(subcat_raw, c("HS6", "Technology", "Value Chain", "Sub.Sector"), "subcat_raw")

  subcat_lu <- subcat_raw %>%
    dplyr::mutate(code = stringr::str_pad(as.character(.data$HS6), width = 6, pad = "0")) %>%
    dplyr::distinct(.data$code, .data$Technology, .data$`Value Chain`, .data$Sub.Sector)

  nipo_classified <- raw_nipo %>%
    dplyr::mutate(
      nipo_row_id = dplyr::row_number(),
      hs6_raw = .data$`Product: HS 6-digit (2022)`
    ) %>%
    tidyr::separate_rows(.data$hs6_raw, sep = "\\s*,\\s*") %>%
    dplyr::mutate(code = stringr::str_pad(.data$hs6_raw, width = 6, pad = "0")) %>%
    dplyr::left_join(subcat_lu, by = "code", relationship = "many-to-many") %>%
    dplyr::group_by(.data$nipo_row_id) %>%
    dplyr::summarise(
      dplyr::across(
        -c(.data$Technology, .data$`Value Chain`, .data$Sub.Sector, .data$code, .data$hs6_raw),
        dplyr::first,
        .names = "{.col}"
      ),
      Technology    = list(sort(unique(na.omit(.data$Technology)))),
      `Value Chain` = list(sort(unique(na.omit(.data$`Value Chain`)))),
      Sub.Sector    = list(sort(unique(na.omit(.data$Sub.Sector)))),
      hs6_codes = list(sort(unique(na.omit(.data$code)))),
      total_hs6 = dplyr::n_distinct(.data$code[!is.na(.data$code) & nzchar(.data$code)]),
      matched_hs6 = dplyr::n_distinct(.data$code[!is.na(.data$Technology) & !is.na(.data$code) & nzchar(.data$code)]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      country = standardize_country_names(.data$`Implementing Jurisdiction`)
    )

  if (!is.null(country_info) && all(c("country", "iso3c") %in% names(country_info))) {
    country_lu <- country_info %>%
      dplyr::mutate(country = standardize_country_names(.data$country)) %>%
      dplyr::select(.data$country, .data$iso3c)

    nipo_classified <- nipo_classified %>%
      dplyr::left_join(country_lu, by = "country") %>%
      dplyr::mutate(iso3 = toupper(.data$iso3c))
  } else {
    nipo_classified <- nipo_classified %>%
      dplyr::mutate(iso3 = NA_character_)
  }

  if ("Sector: CPC 3-digit (v2.1)" %in% names(nipo_classified)) {
    nipo_classified <- nipo_classified %>%
      dplyr::mutate(
        cpc3_codes = purrr::map(.data$`Sector: CPC 3-digit (v2.1)`, parse_code_list, width = 3),
        cpc3_n = purrr::map_int(.data$cpc3_codes, length)
      )
  } else {
    nipo_classified <- nipo_classified %>%
      dplyr::mutate(
        cpc3_codes = replicate(nrow(.), character(0), simplify = FALSE),
        cpc3_n = 0L
      )
  }

  nipo_classified
}

# ==============================================================================
# 2) Build per-policy strength table (one row per policy entry)
#    - Computes: bite_strength_base, scale_strength_base
#    - Applies: package multiplier (m_package) at State Act ID level
# ==============================================================================

build_policy_base <- function(nipo_country_tbl,
                              duration_norm_months = 24,
                              duration_cap_months = 60,
                              breadth_cap = 3.0,
                              geo_cap = 3.0,
                              scale_cap = 3.0,
                              package_cap = 1.6,
                              package_step = 0.15) {
  check_required_columns(
    nipo_country_tbl,
    c(
      "nipo_row_id", "State Act ID", "Entry ID",
      "GTA Intervention Type", "Initial Assessment (Change Relative to 1 Jan 2009)",
      "Level of Government Implementation", "Announcement Date", "Implementation Date",
      "Removal Date", "total_hs6", "matched_hs6", "Affected Jurisdiction",
      "Trade Covered (USD Million)", "Size of Subsidy (USD Million)",
      "Is Horizontal", "Levels of Policy Intervention", "Firm: Beneficiary",
      "Is Export Policy", "Is Import Policy", "Is Trade Defence", "Is Subsidy",
      "Is Export Incentive", "Is FDI Policy", "Is Procurement Policy",
      "Is Localisation Policy", "Is Other Policy"
    ),
    "nipo_country_tbl"
  )

  base <- nipo_country_tbl %>%
    dplyr::mutate(
      policy_id = .data$nipo_row_id,
      state_act_id = .data$`State Act ID`,
      entry_id = .data$`Entry ID`,
      intervention_type = stringr::str_squish(dplyr::coalesce(.data$`GTA Intervention Type`, "")),
      status_raw = stringr::str_squish(dplyr::coalesce(.data$`Initial Assessment (Change Relative to 1 Jan 2009)`, "")),
      juris_raw  = stringr::str_squish(dplyr::coalesce(.data$`Level of Government Implementation`, "")),
      announce_date = as_date_safe(.data$`Announcement Date`),
      impl_date     = as_date_safe(.data$`Implementation Date`),
      removal_date  = as_date_safe(.data$`Removal Date`),
      hs6_n = dplyr::coalesce(as.integer(.data$total_hs6), 0L),
      matched_hs6_n = dplyr::coalesce(as.integer(.data$matched_hs6), 0L),
      cpc_n = count_csv_tokens(.data$`Sector: CPC 3-digit (v2.1)`),
      partner_n = count_csv_tokens(.data$`Affected Jurisdiction`),
      trade_covered_usd_m = suppressWarnings(as.numeric(.data$`Trade Covered (USD Million)`)),
      subsidy_usd_m       = suppressWarnings(as.numeric(.data$`Size of Subsidy (USD Million)`)),
      is_horizontal = as_bool(.data$`Is Horizontal`),
      policy_level  = stringr::str_to_lower(dplyr::coalesce(.data$`Levels of Policy Intervention`, "")),
      has_beneficiary = !is.na(.data$`Firm: Beneficiary`) & nzchar(as.character(.data$`Firm: Beneficiary`)),
      fam_export_policy       = as_bool(.data$`Is Export Policy`),
      fam_import_policy       = as_bool(.data$`Is Import Policy`),
      fam_trade_defence       = as_bool(.data$`Is Trade Defence`),
      fam_subsidy             = as_bool(.data$`Is Subsidy`),
      fam_export_incentive    = as_bool(.data$`Is Export Incentive`),
      fam_fdi_policy          = as_bool(.data$`Is FDI Policy`),
      fam_procurement_policy  = as_bool(.data$`Is Procurement Policy`),
      fam_localisation_policy = as_bool(.data$`Is Localisation Policy`),
      fam_other_policy        = as_bool(.data$`Is Other Policy`),
      status_norm = dplyr::case_when(
        .data$status_raw %in% STATUS_WEIGHTS$status ~ .data$status_raw,
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "discrimin") ~ "Discriminatory",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "liberal") ~ "Liberalising",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "neutral") ~ "Neutral",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "unclear|unknown") ~ "Unclear",
        TRUE ~ "Unknown"
      ),
      status_sign = dplyr::case_when(
        .data$status_norm == "Discriminatory" ~  1L,
        .data$status_norm == "Liberalising"  ~ -1L,
        TRUE ~ 0L
      ),
      jurisdiction_norm = dplyr::case_when(
        .data$juris_raw %in% JURIS_WEIGHTS$jurisdiction ~ .data$juris_raw,
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "national") ~ "National",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "internat") ~ "International",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "sub|state|province|regional") ~ "Subnational",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "local|city|municip") ~ "Local",
        TRUE ~ "Unknown"
      ),
      m_scope = dplyr::case_when(
        .data$is_horizontal ~ 1.00,
        .data$has_beneficiary ~ 0.60,
        stringr::str_detect(.data$policy_level, "economy|cross|horizontal") ~ 1.00,
        stringr::str_detect(.data$policy_level, "sector|industry") ~ 0.80,
        stringr::str_detect(.data$policy_level, "firm") ~ 0.60,
        TRUE ~ 0.75
      )
    ) %>%
    dplyr::left_join(POLICY_TYPE_WEIGHTS, by = c("intervention_type" = "intervention_type")) %>%
    dplyr::left_join(STATUS_WEIGHTS, by = c("status_norm" = "status")) %>%
    dplyr::left_join(JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
    dplyr::mutate(
      w_type   = dplyr::coalesce(.data$w_type, 0.40),
      w_status = dplyr::coalesce(.data$w_status, 0.50),
      w_juris  = dplyr::coalesce(.data$w_juris, 0.80)
    )

  p95_hs6     <- calc_p95(base$hs6_n, fallback = 1)
  p95_cpc     <- calc_p95(base$cpc_n, fallback = 1)
  p95_geo     <- calc_p95(base$partner_n, fallback = 1)
  p95_trade   <- calc_p95(base$trade_covered_usd_m, fallback = 1)
  p95_subsidy <- calc_p95(base$subsidy_usd_m, fallback = 1)

  base <- base %>%
    dplyr::mutate(
      planned_end = dplyr::case_when(
        !is.na(.data$impl_date) & !is.na(.data$removal_date) & (.data$removal_date >= .data$impl_date) ~ .data$removal_date,
        !is.na(.data$impl_date) ~ (.data$impl_date + round(duration_cap_months * 30.44)),
        TRUE ~ as.Date(NA)
      ),
      planned_days = dplyr::case_when(
        !is.na(.data$impl_date) & !is.na(.data$planned_end) ~ as.numeric(.data$planned_end - .data$impl_date),
        TRUE ~ 0
      ),
      planned_months = pmax(0, planned_days / 30.44),
      m_duration = dplyr::case_when(
        !is.na(.data$impl_date) ~ pmin(1, sqrt(planned_months / duration_norm_months)),
        TRUE ~ 0
      ),
      m_hs6 = cap_mult(log_mult(.data$hs6_n, p95_hs6), cap = breadth_cap),
      m_cpc = cap_mult(log_mult(.data$cpc_n, p95_cpc), cap = breadth_cap),
      m_breadth = cap_mult(.data$m_hs6 * .data$m_cpc, cap = breadth_cap),
      m_geo = cap_mult(log_mult(.data$partner_n, p95_geo), cap = geo_cap),
      m_trade   = cap_mult(log_mult(.data$trade_covered_usd_m, p95_trade), cap = scale_cap),
      m_subsidy = cap_mult(log_mult(.data$subsidy_usd_m, p95_subsidy), cap = scale_cap),
      m_scale   = pmax(.data$m_trade, .data$m_subsidy),
      bite_strength_base  = .data$w_type * .data$w_status * .data$w_juris * .data$m_scope * .data$m_duration,
      scale_strength_base = .data$bite_strength_base * .data$m_breadth * .data$m_geo * .data$m_scale
    )

  act_pkg <- base %>%
    dplyr::group_by(.data$iso3, .data$country, .data$state_act_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("fam_"), ~ any(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_families = rowSums(dplyr::across(dplyr::starts_with("fam_"), ~ as.integer(.x)), na.rm = TRUE),
      m_package = pmin(package_cap, 1 + package_step * pmax(0, .data$n_families - 1))
    )

  base %>%
    dplyr::left_join(act_pkg %>% dplyr::select(.data$iso3, .data$state_act_id, .data$m_package),
                     by = c("iso3", "state_act_id")) %>%
    dplyr::mutate(
      m_package = dplyr::coalesce(.data$m_package, 1.0),
      bite_strength_pkg  = .data$bite_strength_base  * .data$m_package,
      scale_strength_pkg = .data$scale_strength_base * .data$m_package
    )
}

# ==============================================================================
# 3) Add "as-of" stock/flow flags (used for outputs 1-3)
# ==============================================================================

add_asof_flags <- function(policy_base_tbl,
                           as_of_date = NULL,
                           flow_window_days = 365) {
  if (is.null(as_of_date)) {
    cand <- c(as_date_safe(policy_base_tbl$announce_date), as_date_safe(policy_base_tbl$impl_date))
    as_of_date <- suppressWarnings(max(cand, na.rm = TRUE))
    if (!is.finite(as_of_date)) as_of_date <- Sys.Date()
  }
  as_of_date <- as_date_safe(as_of_date)
  flow_start <- as_of_date - as.difftime(flow_window_days, units = "days")

  policy_base_tbl %>%
    dplyr::mutate(
      as_of_date = as_of_date,
      flow_window_days = flow_window_days,
      is_implemented_asof = !is.na(.data$impl_date) & (.data$impl_date <= as_of_date),
      is_active_asof = .data$is_implemented_asof & (is.na(.data$removal_date) | (.data$removal_date > as_of_date)),
      is_new_impl_window = .data$is_implemented_asof & (.data$impl_date >= flow_start) & (.data$impl_date <= as_of_date),
      is_removed_window  = !is.na(.data$removal_date) & (.data$removal_date >= flow_start) & (.data$removal_date <= as_of_date)
    )
}

# ==============================================================================
# 4) Output 1: policy-level table (country level)
# ==============================================================================

build_by_policy <- function(policy_asof_tbl, cpc_names) {
  by_policy <- policy_asof_tbl %>%
    dplyr::group_by(.data$iso3, .data$country) %>%
    dplyr::mutate(
      country_bite_stock = sum(.data$bite_strength_pkg[.data$is_active_asof], na.rm = TRUE),
      country_scale_stock = sum(.data$scale_strength_pkg[.data$is_active_asof], na.rm = TRUE),
      bite_share_of_country_stock = dplyr::if_else(
        .data$is_active_asof & .data$country_bite_stock > 0,
        .data$bite_strength_pkg / .data$country_bite_stock,
        NA_real_
      ),
      scale_share_of_country_stock = dplyr::if_else(
        .data$is_active_asof & .data$country_scale_stock > 0,
        .data$scale_strength_pkg / .data$country_scale_stock,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(
      bite_policy_index_within_country = dplyr::if_else(
        .data$is_active_asof,
        median_scurve(log1p(dplyr::if_else(.data$is_active_asof, .data$bite_strength_pkg, NA_real_))),
        NA_real_
      ),
      scale_policy_index_within_country = dplyr::if_else(
        .data$is_active_asof,
        median_scurve(log1p(dplyr::if_else(.data$is_active_asof, .data$scale_strength_pkg, NA_real_))),
        NA_real_
      )
    ) %>%
    dplyr::ungroup()

  by_policy <- attach_policy_cpc_names(by_policy, cpc_names)

  by_policy %>%
    dplyr::mutate(
      bite_stock_sum = .data$bite_strength_pkg,
      scale_stock_sum = .data$scale_strength_pkg,
      policy_index_bite = .data$bite_policy_index_within_country,
      policy_index_scale = .data$scale_policy_index_within_country
    ) %>%
    dplyr::select(
      iso3, country,
      policy_id, state_act_id, entry_id,
      Title, URL,
      intervention_type, status_norm, jurisdiction_norm,
      announce_date, impl_date, removal_date,
      is_implemented_asof, is_active_asof,
      bite_strength_pkg, scale_strength_pkg,
      bite_stock_sum, scale_stock_sum,
      bite_share_of_country_stock, scale_share_of_country_stock,
      bite_policy_index_within_country, scale_policy_index_within_country,
      policy_index_bite, policy_index_scale,
      hs6_cpc_match_rate, hs6_cpc_unmapped_rate,
      cpc3_codes_csv, cpc_name_csv
    )
}

# ==============================================================================
# 5) Output 2: HS6-level stock table (country level)
# ==============================================================================

build_by_hs6 <- function(policy_asof_tbl, hs6_cpc_lu, split_across_hs6 = TRUE) {
  hs6_long <- policy_asof_tbl %>%
    dplyr::mutate(
      hs6_list = purrr::map(.data$hs6_codes, function(v) {
        v <- as.character(v)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v) == 0) "Cross-cutting" else unique(v)
      })
    ) %>%
    tidyr::unnest(.data$hs6_list) %>%
    dplyr::rename(HS6 = .data$hs6_list) %>%
    dplyr::mutate(
      alloc_hs6 = dplyr::case_when(
        .data$HS6 == "Cross-cutting" ~ 1,
        split_across_hs6 ~ 1 / pmax(1, .data$hs6_n),
        TRUE ~ 1
      ),
      bite_hs6 = .data$bite_strength_pkg * .data$alloc_hs6,
      scale_hs6 = .data$scale_strength_pkg * .data$alloc_hs6
    )

  agg <- hs6_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$HS6) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id[.data$is_active_asof]),
      bite_stock_sum = sum(.data$bite_hs6[.data$is_active_asof], na.rm = TRUE),
      scale_stock_sum = sum(.data$scale_hs6[.data$is_active_asof], na.rm = TRUE),
      .groups = "drop"
    )

  idx <- agg %>%
    dplyr::group_by(.data$HS6) %>%
    dplyr::mutate(
      hs6_bite_index = median_scurve(log1p(.data$bite_stock_sum)),
      hs6_scale_index = median_scurve(log1p(.data$scale_stock_sum)),
      policy_index_bite = .data$hs6_bite_index,
      policy_index_scale = .data$hs6_scale_index
    ) %>%
    dplyr::ungroup()

  if (nrow(hs6_cpc_lu) > 0) {
    idx <- idx %>% dplyr::left_join(hs6_cpc_lu, by = "HS6")
  } else {
    idx <- idx %>% dplyr::mutate(cpc3_codes_csv = NA_character_, cpc_name_csv = NA_character_)
  }

  idx
}

# ==============================================================================
# 6) Output 3: tech x supply_chain stock table
# ==============================================================================

build_by_tech_sc <- function(policy_asof_tbl,
                             tech_sc_cpc_lu,
                             tech_universe,
                             supply_chain_universe,
                             expand_cross_cutting = TRUE,
                             split_cross_cutting_strength = TRUE) {
  tech_sc_long <- allocate_policy_to_tech_sc(policy_asof_tbl)

  if (isTRUE(expand_cross_cutting)) {
    tech_sc_long <- expand_cross_cutting_rows(
      tech_sc_long,
      tech_universe = tech_universe,
      supply_chain_universe = supply_chain_universe,
      split_strength = split_cross_cutting_strength
    ) %>%
      dplyr::filter(.data$tech != "Cross-cutting", .data$supply_chain != "Cross-cutting")
  }

  tech_sc_long <- tech_sc_long %>%
    dplyr::mutate(
      bite_ts = .data$bite_strength_pkg * .data$alloc,
      scale_ts = .data$scale_strength_pkg * .data$alloc
    )

  agg <- tech_sc_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id[.data$is_active_asof]),
      bite_stock_sum = sum(.data$bite_ts[.data$is_active_asof], na.rm = TRUE),
      scale_stock_sum = sum(.data$scale_ts[.data$is_active_asof], na.rm = TRUE),
      .groups = "drop"
    )

  idx <- agg %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      tech_sc_bite_index = median_scurve(log1p(.data$bite_stock_sum)),
      tech_sc_scale_index = median_scurve(log1p(.data$scale_stock_sum)),
      policy_index_bite = .data$tech_sc_bite_index,
      policy_index_scale = .data$tech_sc_scale_index
    ) %>%
    dplyr::ungroup()

  if (nrow(tech_sc_cpc_lu) > 0) {
    idx <- idx %>% dplyr::left_join(tech_sc_cpc_lu, by = c("tech", "supply_chain"))
  } else {
    idx <- idx %>% dplyr::mutate(cpc3_codes_csv = NA_character_, cpc_name_csv = NA_character_)
  }

  list(
    data = idx,
    policy_alloc = tech_sc_long
  )
}

# ==============================================================================
# 7) Output 4: tech annualized (time-weighted average stock)
# ==============================================================================

build_by_tech_year <- function(policy_base_tbl,
                               tech_cpc_lu,
                               tech_universe,
                               supply_chain_universe,
                               year_min = NULL,
                               year_max = NULL,
                               expand_cross_cutting = TRUE,
                               split_cross_cutting_strength = TRUE) {
  tmp <- policy_base_tbl %>%
    dplyr::mutate(
      is_active_asof = TRUE,
      as_of_date = as.Date(NA)
    )

  tech_sc_long <- allocate_policy_to_tech_sc(tmp)

  if (isTRUE(expand_cross_cutting)) {
    tech_sc_long <- expand_cross_cutting_rows(
      tech_sc_long,
      tech_universe = tech_universe,
      supply_chain_universe = supply_chain_universe,
      split_strength = split_cross_cutting_strength
    ) %>%
      dplyr::filter(.data$tech != "Cross-cutting", .data$supply_chain != "Cross-cutting")
  }

  tech_sc_long <- tech_sc_long %>%
    dplyr::mutate(
      bite_ts  = .data$bite_strength_pkg  * .data$alloc,
      scale_ts = .data$scale_strength_pkg * .data$alloc
    )

  impl_years <- suppressWarnings(as.integer(format(tech_sc_long$impl_date, "%Y")))
  impl_years <- impl_years[is.finite(impl_years)]
  if (is.null(year_min)) year_min <- if (length(impl_years) > 0) min(impl_years) else as.integer(format(Sys.Date(), "%Y"))

  rem_years <- suppressWarnings(as.integer(format(tech_sc_long$removal_date, "%Y")))
  rem_years <- rem_years[is.finite(rem_years)]
  if (is.null(year_max)) {
    year_max <- max(c(impl_years, rem_years, as.integer(format(Sys.Date(), "%Y"))), na.rm = TRUE)
  }

  annual_long <- tech_sc_long %>%
    dplyr::filter(!is.na(.data$impl_date)) %>%
    dplyr::mutate(
      start_year = suppressWarnings(as.integer(format(.data$impl_date, "%Y"))),
      end_year = dplyr::if_else(
        !is.na(.data$removal_date),
        suppressWarnings(as.integer(format(.data$removal_date, "%Y"))),
        year_max
      ),
      start_year = pmax(.data$start_year, year_min),
      end_year   = pmin(.data$end_year, year_max),
      year_list = purrr::map2(.data$start_year, .data$end_year, ~ {
        if (!is.finite(.x) || !is.finite(.y) || .y < .x) integer(0) else seq(.x, .y)
      })
    ) %>%
    tidyr::unnest(.data$year_list) %>%
    dplyr::rename(Year = .data$year_list) %>%
    dplyr::mutate(
      year_start = as.Date(paste0(.data$Year, "-01-01")),
      year_end_excl = as.Date(paste0(.data$Year + 1L, "-01-01")),
      days_in_year = as.numeric(.data$year_end_excl - .data$year_start),
      interval_start = pmax(.data$impl_date, .data$year_start),
      interval_end = dplyr::if_else(
        is.na(.data$removal_date),
        .data$year_end_excl,
        pmin(.data$removal_date, .data$year_end_excl)
      ),
      overlap_days = pmax(0, as.numeric(.data$interval_end - .data$interval_start)),
      year_frac = dplyr::if_else(.data$days_in_year > 0, .data$overlap_days / .data$days_in_year, 0),
      bite_avg  = .data$bite_ts  * .data$year_frac,
      scale_avg = .data$scale_ts * .data$year_frac,
      bite_avg_targeted  = dplyr::if_else(!.data$is_crosscutting_policy, .data$bite_avg,  0),
      scale_avg_targeted = dplyr::if_else(!.data$is_crosscutting_policy, .data$scale_avg, 0)
    )

  agg <- annual_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$Year) %>%
    dplyr::summarise(
      bite_avg_stock_sum  = sum(.data$bite_avg,  na.rm = TRUE),
      scale_avg_stock_sum = sum(.data$scale_avg, na.rm = TRUE),
      bite_avg_targeted_sum  = sum(.data$bite_avg_targeted,  na.rm = TRUE),
      scale_avg_targeted_sum = sum(.data$scale_avg_targeted, na.rm = TRUE),
      n_policies_touching_year = dplyr::n_distinct(.data$policy_id),
      .groups = "drop"
    )

  agg <- agg %>%
    dplyr::group_by(.data$iso3, .data$country, .data$Year) %>%
    dplyr::mutate(
      scale_total_year = sum(.data$scale_avg_stock_sum, na.rm = TRUE),
      scale_targeted_total_year = sum(.data$scale_avg_targeted_sum, na.rm = TRUE),
      scale_share_all = dplyr::if_else(.data$scale_total_year > 0,
                                       .data$scale_avg_stock_sum / .data$scale_total_year,
                                       NA_real_),
      scale_share_targeted = dplyr::if_else(.data$scale_targeted_total_year > 0,
                                            .data$scale_avg_targeted_sum / .data$scale_targeted_total_year,
                                            NA_real_)
    ) %>%
    dplyr::ungroup()

  idx <- agg %>%
    dplyr::group_by(.data$Year, .data$tech) %>%
    dplyr::mutate(
      tech_year_bite_index_xs  = median_scurve(log1p(.data$bite_avg_stock_sum)),
      tech_year_scale_index_xs = median_scurve(log1p(.data$scale_avg_stock_sum)),
      policy_index_bite = .data$tech_year_bite_index_xs,
      policy_index_scale = .data$tech_year_scale_index_xs,
      policy_index_scale_share_targeted = .data$scale_share_targeted,
      policy_index_scale_xs = .data$tech_year_scale_index_xs
    ) %>%
    dplyr::ungroup()

  if (nrow(tech_cpc_lu) > 0) {
    idx <- idx %>% dplyr::left_join(tech_cpc_lu, by = "tech")
  } else {
    idx <- idx %>% dplyr::mutate(cpc3_codes_csv = NA_character_, cpc_name_csv = NA_character_)
  }

  idx
}

# ==============================================================================
# 8) Output 5: CPC-level stock table
# ==============================================================================

build_by_cpc <- function(policy_asof_tbl,
                         cpc_name_lu = NULL,
                         split_across_cpc = TRUE) {
  if (!("cpc3_codes" %in% names(policy_asof_tbl))) {
    policy_asof_tbl <- policy_asof_tbl %>%
      dplyr::mutate(
        cpc3_codes = replicate(nrow(.), character(0), simplify = FALSE),
        cpc3_n = 0L
      )
  }

  cpc_long <- policy_asof_tbl %>%
    dplyr::mutate(
      cpc_list = purrr::map(.data$cpc3_codes, ~{
        v <- as.character(.x)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v) == 0) "Cross-cutting" else unique(substr(v, 1, 3))
      })
    ) %>%
    tidyr::unnest(.data$cpc_list) %>%
    dplyr::rename(cpc3 = .data$cpc_list) %>%
    dplyr::mutate(
      alloc_cpc = dplyr::case_when(
        .data$cpc3 == "Cross-cutting" ~ 1,
        split_across_cpc ~ 1 / pmax(1, dplyr::coalesce(.data$cpc3_n, 1L)),
        TRUE ~ 1
      ),
      bite_cpc  = .data$bite_strength_pkg  * .data$alloc_cpc,
      scale_cpc = .data$scale_strength_pkg * .data$alloc_cpc
    )

  if (nrow(cpc_long) == 0) {
    return(tibble::tibble())
  }

  agg <- cpc_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$cpc3) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id[.data$is_active_asof]),
      bite_stock_sum  = sum(.data$bite_cpc[.data$is_active_asof],  na.rm = TRUE),
      scale_stock_sum = sum(.data$scale_cpc[.data$is_active_asof], na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(cpc_name_lu) && nrow(cpc_name_lu) > 0) {
    agg <- agg %>% dplyr::left_join(cpc_name_lu, by = "cpc3")
  } else {
    agg <- agg %>% dplyr::mutate(cpc.name = NA_character_)
  }

  agg %>%
    dplyr::group_by(.data$cpc3) %>%
    dplyr::mutate(
      cpc_bite_index  = median_scurve(log1p(.data$bite_stock_sum)),
      cpc_scale_index = median_scurve(log1p(.data$scale_stock_sum)),
      policy_index_bite = .data$cpc_bite_index,
      policy_index_scale = .data$cpc_scale_index,
      cpc3_codes_csv = .data$cpc3,
      cpc_name_csv = .data$cpc.name
    ) %>%
    dplyr::ungroup()
}

# ==============================================================================
# 9) Top-level wrapper: nipo_policy_outputs()
# ==============================================================================

nipo_policy_outputs <- function(raw_nipo,
                                subcat_raw,
                                country_info = NULL,
                                as_of_date = NULL,
                                flow_window_days = 365,
                                duration_norm_months = 24,
                                duration_cap_months = 60,
                                breadth_cap = 3.0,
                                geo_cap = 3.0,
                                scale_cap = 3.0,
                                package_cap = 1.6,
                                package_step = 0.15,
                                tech_universe = NULL,
                                supply_chain_universe = NULL,
                                expand_cross_cutting = TRUE,
                                split_cross_cutting_strength = TRUE,
                                split_across_hs6 = TRUE,
                                year_min = NULL,
                                year_max = NULL) {
  nipo_country <- clean_nipo_raw(
    raw_nipo = raw_nipo,
    subcat_raw = subcat_raw,
    country_info = country_info
  )

  cpc_hs <- get_cpc_hs_map()
  cpc_names <- get_cpc3_names()
  cpc_pairs <- build_cpc3_to_tech_sc_pairs(subcat_raw = subcat_raw, cpc_hs = cpc_hs)

  nipo_country <- attach_cpc_validation(nipo_country, cpc_hs = cpc_hs, cpc_pairs = cpc_pairs)

  if (is.null(tech_universe)) {
    tech_universe <- subcat_raw %>%
      dplyr::pull(.data$Technology) %>%
      normalize_chr_vec()
  }
  if (is.null(supply_chain_universe)) {
    supply_chain_universe <- subcat_raw %>%
      dplyr::pull(.data$`Value Chain`) %>%
      normalize_chr_vec()
  }

  hs6_cpc_lu <- build_hs6_cpc_lookup(cpc_hs = cpc_hs, cpc_names = cpc_names)
  tech_sc_cpc_lu <- build_tech_sc_cpc_lookup(subcat_raw = subcat_raw, cpc_hs = cpc_hs, cpc_names = cpc_names)
  tech_cpc_lu <- build_tech_cpc_lookup(tech_sc_cpc_lu)

  policy_base <- build_policy_base(
    nipo_country_tbl = nipo_country,
    duration_norm_months = duration_norm_months,
    duration_cap_months = duration_cap_months,
    breadth_cap = breadth_cap,
    geo_cap = geo_cap,
    scale_cap = scale_cap,
    package_cap = package_cap,
    package_step = package_step
  )

  policy_asof <- add_asof_flags(
    policy_base_tbl = policy_base,
    as_of_date = as_of_date,
    flow_window_days = flow_window_days
  )

  by_policy <- build_by_policy(policy_asof, cpc_names = cpc_names)

  by_hs6 <- build_by_hs6(policy_asof, hs6_cpc_lu = hs6_cpc_lu, split_across_hs6 = split_across_hs6)

  tech_sc_out <- build_by_tech_sc(
    policy_asof_tbl = policy_asof,
    tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength
  )

  by_tech_sc <- tech_sc_out$data

  by_tech_year <- build_by_tech_year(
    policy_base_tbl = policy_base,
    tech_cpc_lu = tech_cpc_lu,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    year_min = year_min,
    year_max = year_max,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength
  )

  by_cpc <- build_by_cpc(
    policy_asof_tbl = policy_asof,
    cpc_name_lu = cpc_names,
    split_across_cpc = TRUE
  )

  list(
    by_policy = by_policy,
    by_hs6 = by_hs6,
    by_tech_sc = by_tech_sc,
    by_tech_year = by_tech_year,
    by_cpc = by_cpc,
    internals = list(
      nipo_country = nipo_country,
      policy_base = policy_base,
      policy_asof = policy_asof,
      policy_alloc_tech_sc = tech_sc_out$policy_alloc,
      cpc_hs = cpc_hs,
      cpc_names = cpc_names,
      cpc_pairs = cpc_pairs,
      hs6_cpc_lu = hs6_cpc_lu,
      tech_sc_cpc_lu = tech_sc_cpc_lu,
      tech_cpc_lu = tech_cpc_lu
    )
  )
}

# ==============================================================================
# Self-test (disabled)
# ==============================================================================

if (FALSE) {
  nipo_out <- nipo_policy_outputs(
    raw_nipo = nipo_raw,
    subcat_raw = subcat_raw,
    country_info = country_info
  )

  expected_cols <- list(
    by_policy = c("cpc3_codes_csv", "cpc_name_csv", "policy_index_bite", "policy_index_scale"),
    by_hs6 = c("cpc3_codes_csv", "cpc_name_csv", "policy_index_bite", "policy_index_scale"),
    by_tech_sc = c("cpc3_codes_csv", "cpc_name_csv", "policy_index_bite", "policy_index_scale"),
    by_tech_year = c("cpc3_codes_csv", "cpc_name_csv", "policy_index_bite", "policy_index_scale",
                     "policy_index_scale_share_targeted", "policy_index_scale_xs"),
    by_cpc = c("cpc3", "cpc.name", "policy_index_bite", "policy_index_scale")
  )

  for (nm in names(expected_cols)) {
    missing <- setdiff(expected_cols[[nm]], names(nipo_out[[nm]]))
    if (length(missing) > 0) {
      stop(sprintf("Missing columns in %s: %s", nm, paste(missing, collapse = ", ")))
    }
    print(utils::head(nipo_out[[nm]], 5))
  }
}
