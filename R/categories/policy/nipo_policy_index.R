# ==============================================================================
# GTA / NIPO DOMESTIC INTERVENTION INDEX (ENHANCED + DOCUMENTED)
# ------------------------------------------------------------------------------
# What this does (high-level):
#   1) Cleans raw NIPO/GTA inventory rows and maps HS6 codes -> (Technology, Value.Chain)
#   2) Builds a per-policy Domestic Intervention Strength measure with interpretable components:
#        - Tool weight: instrument family (subsidy/procurement/localisation/FDI/import etc.)
#        - Status multiplier: counts Discriminatory; includes Liberalising (downweighted); downweights Neutral/Unclear
#        - Reach/scale: jurisdiction × scope × duration × breadth × geographic reach × money/coverage scale
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
# Domestic Intervention Score (DIS) interpretation:
#   DIS is intended to represent the *strength of support / intervention* directed at a sector.
#   In that interpretation, both "Discriminatory" and "Liberalising" measures can be supportive.
#   We typically down-weight Liberalising actions because, in GTA/NIPO, they often reflect
#   barrier-removal that may benefit the sector's ecosystem (inputs, competition, investment)
#   rather than protect producers directly.
STATUS_LEVELS <- c("Distortive","Liberalising","Neutral","Unclear","Unknown")

# Base status weights (flow-conditional adjustment applied later):
# - Distortive always positive (adds intervention/support)
# - Liberalising depends on Affected Trade Flow:
#     * inward  -> negative (reduces domestic intervention)
#     * outward -> positive (supports outward expansion / market access)
# - Neutral/Unclear/Unknown downweighted positive
STATUS_WEIGHTS_BASE <- tibble::tribble(
  ~status, ~w_status_base,
  "Distortive",    1.00,
  "Neutral",       0.50,
  "Unclear",       0.30,
  "Unknown",       0.30
)

# Liberalising weights by flow (can be tuned)
LIBERALISING_INWARD_WEIGHT  <- -0.20
LIBERALISING_OUTWARD_WEIGHT <-  0.20
LIBERALISING_UNKNOWN_WEIGHT <-  0.00

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

# ---- DOMESTIC_FAMILY_WEIGHTS --------------------------------------------------
# Primary instrument-family weights for Domestic Intervention Score (DIS).
# We use the maximum weight across the families flagged for a measure.
# (If no family flags are present, we fall back to the intervention-type override.)
DOMESTIC_FAMILY_WEIGHTS <- tibble::tribble(
  ~family_flag, ~w_family,
  "fam_subsidy", 1.00,
  "fam_procurement_policy", 0.90,
  "fam_localisation_policy", 0.90,
  "fam_fdi_policy", 0.85,
  "fam_export_incentive", 0.80,
  "fam_import_policy", 0.70,
  "fam_trade_defence", 0.65,
  "fam_export_policy", 0.60,
  "fam_other_policy", 0.30
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
  x <- stringr::str_squish(x)
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
    "Value.Chain" %in% names(subcat_raw) ~ "Value.Chain",
    "supply_chain" %in% names(subcat_raw) ~ "supply_chain",
    TRUE ~ NA_character_
  )
  
  if (is.na(hs_col) || is.na(tech_col) || is.na(sc_col)) {
    stop("subcat_raw must contain HS6 + Technology/Value.Chain (or hs6 + tech/supply_chain).")
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
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6"), relationship = "many-to-many") %>%
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
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6"), relationship = "many-to-many")
  
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

build_hs6_name_lookup <- function(subcat_raw) {
  if (is.null(subcat_raw) || nrow(subcat_raw) == 0) {
    return(tibble::tibble(HS6 = character(0), hs6_name = character(0)))
  }
  nms <- names(subcat_raw)
  nms_lower <- tolower(nms)
  
  hs6_col <- dplyr::case_when(
    "HS6" %in% nms ~ "HS6",
    "hs6" %in% nms ~ "hs6",
    "HS_6" %in% nms ~ "HS_6",
    TRUE ~ NA_character_
  )
  name_idx <- which(grepl("hs6", nms_lower) & grepl("name|desc|description|title", nms_lower))
  if (length(name_idx) == 0) {
    name_idx <- which(grepl("description|desc|title|name", nms_lower))
  }
  name_col <- if (length(name_idx) > 0) nms[[name_idx[[1]]]] else NA_character_
  
  if (is.na(hs6_col) || is.na(name_col)) {
    return(tibble::tibble(HS6 = character(0), hs6_name = character(0)))
  }
  
  subcat_raw %>%
    dplyr::transmute(
      HS6 = stringr::str_pad(stringr::str_replace_all(as.character(.data[[hs6_col]]), "\\D", ""), 6, pad = "0"),
      hs6_name = stringr::str_squish(as.character(.data[[name_col]]))
    ) %>%
    dplyr::filter(nzchar(.data$HS6)) %>%
    dplyr::group_by(.data$HS6) %>%
    dplyr::summarise(
      hs6_name = {
        vals <- unique(.data$hs6_name[!is.na(.data$hs6_name) & nzchar(.data$hs6_name)])
        if (length(vals) == 0) NA_character_ else vals[[1]]
      },
      .groups = "drop"
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
  check_required_columns(subcat_raw, c("HS6", "Technology", "Value.Chain"), "subcat_raw")
  
  tech_sc <- subcat_raw %>%
    dplyr::transmute(
      hs6 = stringr::str_pad(stringr::str_replace_all(as.character(.data$HS6), "\\D", ""), 6, pad = "0"),
      tech = as.character(.data$Technology),
      supply_chain = as.character(.data$`Value.Chain`)
    ) %>%
    dplyr::filter(nzchar(.data$hs6), nzchar(.data$tech), nzchar(.data$supply_chain)) %>%
    dplyr::distinct()
  
  lu <- tech_sc %>%
    dplyr::left_join(cpc_hs, by = c("hs6" = "hs6"), relationship = "many-to-many") %>%
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
  combos <- base::expand.grid(tech = techs, supply_chain = scs, stringsAsFactors = FALSE)
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


# ---- Tech x Supply-chain validation using sector flags ----
# These multipliers increase the share of a policy allocated to a tech/supply_chain pair
# when NIPO sector flags provide corroborating evidence.
# Heuristic matching is intentionally conservative and fully configurable below.
DEFAULT_VALIDATION_BONUS <- 0.20  # +20% weight when corroborated
DEFAULT_VALIDATION_KEYWORD_BONUS <- 0.35  # +15% weight when Title/Source corroborates tech

# ---- Tech taxonomy (user-defined) ----
TECH_TAXONOMY <- c(
  "Electric Vehicles",
  "Nuclear",
  "Coal",
  "Batteries",
  "Green Hydrogen",
  "Wind",
  "Oil",
  "Solar",
  "Gas",
  "Geothermal",
  "Electric Grid",
  "Semiconductors Midstream",
  "Semiconductors Downstream (datacenters & AI)",
  "Magnets Upstream (rare earths)",
  "Magnets Midstream"
)

LOW_CARBON_TECHS <- c("Electric Vehicles","Nuclear","Batteries","Green Hydrogen","Wind","Solar","Geothermal","Electric Grid")
CRITICAL_MINERALS_LINKED_TECHS <- c(
  "Batteries",
  "Electric Vehicles",
  "Electric Grid",
  "Wind",
  "Solar",
  "Green Hydrogen",
  "Magnets Upstream (rare earths)",
  "Magnets Midstream"
)
DUAL_USE_LINKED_TECHS <- c("Nuclear","Electric Grid","Advanced Technology Products")
ADV_TECH_LINKED_TECHS <- c(
  "Electric Grid",
  "Batteries",
  "Nuclear",
  "Electric Vehicles",
  "Advanced Technology Products",
  "Semiconductors Midstream",
  "Semiconductors Downstream (datacenters & AI)"
)

is_low_carbon_tech <- function(tech) {
  tech %in% LOW_CARBON_TECHS
}
is_critical_minerals_tech <- function(tech) {
  tech %in% CRITICAL_MINERALS_LINKED_TECHS
}
is_dual_use_tech <- function(tech) {
  tech %in% DUAL_USE_LINKED_TECHS
}
is_advanced_tech <- function(tech) {
  tech %in% ADV_TECH_LINKED_TECHS
}

# Keyword validation: look for tech-relevant terms in Title/Source.
TECH_KEYWORDS <- list(
  `Electric Vehicles` = c("electric vehicle","electric vehicles","\\bev\\b","\\bevs\\b","charging","charger","battery electric","plug-in","phev","bev"),
  `Batteries` = c("battery","batteries","\\bli-ion\\b","lithium-ion","cell manufacturing","gigafactory","anode","cathode","bms","energy storage"),
  `Green Hydrogen` = c("hydrogen","\\bh2\\b","electrolyser","electrolyzer","electrolysis","green hydrogen","ammonia","ptx","power-to-x"),
  `Wind` = c("wind","turbine","offshore wind","onshore wind","blade","nacelle","rare earth"),
  `Solar` = c("solar","photovoltaic","\\bpv\\b","inverter","module","panel","wafer","polysilicon","silicon"),
  `Geothermal` = c("geothermal","egs","enhanced geothermal","heat flow","geofluid"),
  `Electric Grid` = c("grid","transmission","distribution","substation","transformer","switchgear","interconnector","interconnection","hvdc","smart grid","copper"),
  `Nuclear` = c("nuclear","reactor","smr","spent fuel","uranium","enrichment","fission"),
  `Coal` = c("coal","coking coal","thermal coal","coal-fired","lignite"),
  `Oil` = c("oil","petroleum","crude","refinery","refining","pipeline"),
  `Gas` = c("gas","natural gas","lng","liquefaction","regasification","pipeline gas"),
  `Semiconductors Midstream` = c(
    "semiconductor","semiconductors","chip","chips","wafer","wafers","fab","fabs",
    "foundry","fabrication","packaging","assembly","atmp","front-end"
  ),
  `Semiconductors Downstream (datacenters & AI)` = c(
    "datacenter","data center","server","servers","gpu","gpus","ai","artificial intelligence",
    "accelerator","hpc","cloud", "model training", "inference"
  ),
  `Magnets Upstream (rare earths)` = c(
    "rare earth","rare-earth","ndpr","neodymium","praseodymium","dysprosium","terbium",
    "magnet ore","rare earth mine","rare earth mining"
  ),
  `Magnets Midstream` = c(
    "magnet","magnets","permanent magnet","ndfeb","sintered magnet","magnet manufacturing",
    "magnet production","bonded magnet"
  )
)

keyword_evidence <- function(tech, title, source) {
  hay <- stringr::str_to_lower(paste(dplyr::coalesce(title, ""), dplyr::coalesce(source, ""), sep = " | "))
  kws <- TECH_KEYWORDS[[tech]]
  if (is.null(kws) || length(kws) == 0) return(FALSE)
  any(purrr::map_lgl(kws, ~ stringr::str_detect(hay, .x)))
}

# Supply-chain keyword validation: look for stage-relevant terms in Title/Source.
# Your supply_chain options: Upstream (commodities/critical minerals etc), Midstream (manufacturing),
# Downstream (deployment and services).
DEFAULT_VALIDATION_SC_KEYWORD_BONUS <- 0.25  # +12% when Title/Source corroborates supply-chain stage

# ---- Mapping confidence (applied to policy strength contributions) ----
CONFIDENCE_FLOOR <- 0.25
CONFIDENCE_CAP   <- 2
# Baseline formula: confidence = 0.75 + 0.75 * mapped_share * evidence_mean
# evidence_mean is the within-policy mean of combo_weight (>= 1 when validation hits).
CONFIDENCE_UNMAPPED      <- 0.10
CONFIDENCE_CROSSCUTTING  <- 0.25

SUPPLY_CHAIN_KEYWORDS <- list(
  `Upstream` = c(
    "mining", "mine", "extraction", "extractive", "ore", "concentrate", "beneficiation",
    "exploration", "prospecting", "drilling", "upstream",
    "smelt", "smelting", "refin", "refining", "processing", "metallurg", "critical mineral", "raw material", "rare earth"
  ),
  `Midstream` = c(
    "manufactur", "factory", "plant", "gigafactory", "assembly", "fabricat", "production line",
    "component", "module", "cells?", "anode", "cathode", "electrolyser manufacturing", "electrolyzer manufacturing",
    "enrichment", "conversion", "processing", "midstream",
    "foundry", "fabs?", "wafer", "chip packaging", "atmp", "magnet manufacturing", "ndfeb"
  ),
  `Downstream` = c(
    "deploy", "deployment", "install", "installation", "commission", "construction",
    "service", "servicing", "maintenance", "operations", "o\\&m", "retail",
    "charging station", "charger", "grid connection", "interconnection", "hook[- ]?up",
    "rebate", "consumer", "end[- ]?use", "downstream",
    "datacenter", "data center", "server", "gpu", "ai", "inference", "model training", "cloud"
  )
)

supply_chain_keyword_evidence <- function(supply_chain, title, source) {
  hay <- stringr::str_to_lower(paste(dplyr::coalesce(title, ""), dplyr::coalesce(source, ""), sep = " | "))
  kws <- SUPPLY_CHAIN_KEYWORDS[[supply_chain]]
  if (is.null(kws) || length(kws) == 0) return(FALSE)
  any(purrr::map_lgl(kws, ~ stringr::str_detect(hay, .x)))
}

validation_weight <- function(tech, supply_chain, title, source,
                              sector_low_carbon, sector_dual_use, sector_critical_minerals, sector_advanced_tech,
                              bonus = DEFAULT_VALIDATION_BONUS,
                              keyword_bonus = DEFAULT_VALIDATION_KEYWORD_BONUS,
                              sc_keyword_bonus = DEFAULT_VALIDATION_SC_KEYWORD_BONUS) {
  w <- 1.0
  
  # Sector-flag corroboration (tech-level).
  if (isTRUE(sector_low_carbon) && is_low_carbon_tech(tech)) w <- w + bonus
  if (isTRUE(sector_critical_minerals) && is_critical_minerals_tech(tech)) w <- w + bonus
  if (isTRUE(sector_dual_use) && is_dual_use_tech(tech)) w <- w + bonus
  if (isTRUE(sector_advanced_tech) && is_advanced_tech(tech)) w <- w + bonus
  
  # Keyword corroboration (tech-level).
  if (isTRUE(keyword_evidence(tech, title, source))) w <- w + keyword_bonus
  
  # Keyword corroboration (supply-chain stage-level).
  if (isTRUE(supply_chain_keyword_evidence(supply_chain, title, source))) w <- w + sc_keyword_bonus
  
  # Extra sanity: if it's explicitly "Critical Minerals", bias toward Upstream when present.
  if (isTRUE(sector_critical_minerals) && identical(supply_chain, "Upstream")) w <- w + 0.10
  
  w
}


allocate_policy_to_tech_sc <- function(policy_tbl) {
  policy_shares <- policy_tbl %>%
    dplyr::mutate(
      mapped_share = dplyr::if_else(.data$hs6_n > 0,
                                    pmin(1, .data$matched_hs6_n / pmax(1, .data$hs6_n)),
                                    0),
      tech_mapped = purrr::map(.data$Technology, ~ setdiff(safe_list_or_empty(.x), "Unmapped")),
      sc_mapped   = purrr::map(.data$`Value.Chain`, safe_list_or_empty),
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
      combo_weight = purrr::pmap_dbl(
        list(.data$tech,
             .data$supply_chain,
             dplyr::coalesce(.data$Title, ""),
             dplyr::coalesce(.data$Source, ""),
             .data$sector_low_carbon,
             .data$sector_dual_use,
             .data$sector_critical_minerals,
             .data$sector_advanced_tech),
        ~ validation_weight(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8)
      ),
      is_crosscutting_policy = FALSE,
      mapping_confidence = CONFIDENCE_UNMAPPED
    ) %>%
    dplyr::group_by(.data$nipo_row_id) %>%
    dplyr::mutate(
      alloc = .data$mapped_share * .data$combo_weight / pmax(1e-9, sum(.data$combo_weight, na.rm = TRUE)),
      evidence_mean = mean(.data$combo_weight, na.rm = TRUE),
      mapping_confidence = pmin(CONFIDENCE_CAP, pmax(CONFIDENCE_FLOOR, 0.75 + 0.75 * .data$mapped_share * dplyr::coalesce(.data$evidence_mean, 1)))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$n_combo)
  
  unmapped_long <- policy_shares %>%
    dplyr::filter(.data$unmapped_share > 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Unmapped",
      supply_chain = "Unmapped",
      alloc = .data$unmapped_share,
      cpc_validation_failed = FALSE,
      is_crosscutting_policy = FALSE,
      mapping_confidence = CONFIDENCE_UNMAPPED
    )
  
  cross_long <- policy_shares %>%
    dplyr::filter(.data$hs6_n == 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Cross-cutting",
      supply_chain = "Cross-cutting",
      alloc = 1,
      cpc_validation_failed = FALSE,
      is_crosscutting_policy = TRUE,
      mapping_confidence = CONFIDENCE_CROSSCUTTING
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
  check_required_columns(subcat_raw, c("HS6", "Technology", "Value.Chain", "Sub.Sector"), "subcat_raw")
  
  subcat_lu <- subcat_raw %>%
    dplyr::mutate(
      code = stringr::str_pad(as.character(.data$HS6), width = 6, pad = "0"),
      Technology = stringr::str_squish(as.character(.data$Technology)),
      `Value.Chain` = stringr::str_squish(as.character(.data$`Value.Chain`))
    ) %>%
    dplyr::distinct(.data$code, .data$Technology, .data$`Value.Chain`, .data$Sub.Sector)
  
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
        -c(.data$Technology, .data$`Value.Chain`, .data$Sub.Sector, .data$code, .data$hs6_raw),
        dplyr::first,
        .names = "{.col}"
      ),
      Technology    = list(sort(unique(na.omit(.data$Technology)))),
      `Value.Chain` = list(sort(unique(na.omit(.data$`Value.Chain`)))),
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
  
  
  # ---- Sector validation flags (binary) ----
  # These are used to strengthen/validate tech x supply_chain mappings where relevant.
  # If columns are absent in a given extract, default to FALSE.
  nipo_classified <- nipo_classified %>%
    dplyr::mutate(
      sector_low_carbon = if ("Sector: Low Carbon Technology" %in% names(nipo_classified)) as_bool(.data$`Sector: Low Carbon Technology`) else FALSE,
      sector_dual_use = if ("Sector: Dual-Use Products" %in% names(nipo_classified)) as_bool(.data$`Sector: Dual-Use Products`) else FALSE,
      sector_critical_minerals = if ("Sector: Critical Minerals" %in% names(nipo_classified)) as_bool(.data$`Sector: Critical Minerals`) else FALSE,
      sector_advanced_tech = if ("Sector: Advanced Technology Products" %in% names(nipo_classified)) as_bool(.data$`Sector: Advanced Technology Products`) else FALSE
    )
  
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
      flow_raw   = stringr::str_squish(dplyr::coalesce(.data$`Affected Trade Flow`, "")),
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
      sector_low_carbon        = as_bool(.data$sector_low_carbon),
      sector_dual_use           = as_bool(.data$sector_dual_use),
      sector_critical_minerals  = as_bool(.data$sector_critical_minerals),
      sector_advanced_tech      = as_bool(.data$sector_advanced_tech),
      flow_norm = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(.data$flow_raw), "inward")  ~ "inward",
        stringr::str_detect(stringr::str_to_lower(.data$flow_raw), "outward") ~ "outward",
        TRUE ~ "unknown"
      ),
      status_norm = dplyr::case_when(
        # NIPO/GTA sometimes uses Red/Amber/Green or Harmful/Beneficial language.
        .data$status_raw %in% STATUS_LEVELS ~ .data$status_raw,
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "red|harmful|restrict|discrimin|protection") ~ "Distortive",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "green|beneficial|liberal|facilitat") ~ "Liberalising",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "amber|likely") ~ "Unclear",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "neutral") ~ "Neutral",
        stringr::str_detect(stringr::str_to_lower(.data$status_raw), "unclear|unknown") ~ "Unclear",
        TRUE ~ "Unknown"
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
        stringr::str_detect(.data$policy_level, "economy|cross|horizontal") ~ 0.75,
        stringr::str_detect(.data$policy_level, "sector|industry") ~ 1,
        stringr::str_detect(.data$policy_level, "firm") ~ 0.40,
        TRUE ~ 0.75
      )
    ) %>%
    dplyr::left_join(POLICY_TYPE_WEIGHTS, by = c("intervention_type" = "intervention_type")) %>%    dplyr::left_join(JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
    dplyr::mutate(
      w_type   = dplyr::coalesce(.data$w_type, 0.40),
      w_status = dplyr::case_when(
        .data$status_norm == "Distortive" ~ 1.00,
        .data$status_norm == "Liberalising" & .data$flow_norm == "inward"  ~ LIBERALISING_INWARD_WEIGHT,
        .data$status_norm == "Liberalising" & .data$flow_norm == "outward" ~ LIBERALISING_OUTWARD_WEIGHT,
        .data$status_norm == "Liberalising" ~ LIBERALISING_UNKNOWN_WEIGHT,
        .data$status_norm == "Neutral" ~ 0.50,
        .data$status_norm == "Unclear" ~ 0.30,
        .data$status_norm == "Unknown" ~ 0.30,
        TRUE ~ 0.30
      ),
      w_juris  = dplyr::coalesce(.data$w_juris, 0.80),
      w_family = pmax(
        dplyr::if_else(.data$fam_subsidy,             1.00, 0),
        dplyr::if_else(.data$fam_procurement_policy,  0.90, 0),
        dplyr::if_else(.data$fam_localisation_policy, 0.90, 0),
        dplyr::if_else(.data$fam_fdi_policy,          0.85, 0),
        dplyr::if_else(.data$fam_export_incentive,    0.80, 0),
        dplyr::if_else(.data$fam_import_policy,       0.70, 0),
        dplyr::if_else(.data$fam_trade_defence,       0.65, 0),
        dplyr::if_else(.data$fam_export_policy,       0.60, 0),
        dplyr::if_else(.data$fam_other_policy,        0.30, 0),
        0
      ),
      w_tool = dplyr::if_else(.data$w_family > 0, .data$w_family, .data$w_type)
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
      m_scale   = pmax(.data$m_subsidy),
      bite_strength_base  = .data$w_tool * .data$w_status * .data$w_juris * .data$m_scope * .data$m_duration,
      scale_strength_base = .data$bite_strength_base * .data$m_breadth * .data$m_geo * 2*.data$m_scale,
      policy_strength= .data$w_tool * .data$w_status* .data$m_scope * .data$m_scale
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
      scale_strength_pkg = .data$scale_strength_base * .data$m_package,
      policy_strength_pkg = .data$policy_strength * .data$m_package
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
  
  # Summarise tech × supply_chain mapping (and confidence) at the policy level
  alloc_long <- allocate_policy_to_tech_sc(policy_asof_tbl) %>%
    dplyr::mutate(
      mapping_confidence = dplyr::coalesce(.data$mapping_confidence, 1)
    )
  
  map_sum <- alloc_long %>%
    dplyr::group_by(.data$policy_id) %>%
    dplyr::summarise(
      tech_csv = paste(sort(unique(.data$tech[.data$tech != "Unmapped"])), collapse = "|"),
      supply_chain_csv = paste(sort(unique(.data$supply_chain[.data$supply_chain != "Unmapped"])), collapse = "|"),
      tech_sc_csv = paste(sort(unique(paste(.data$tech, .data$supply_chain, sep = "::"))), collapse = "|"),
      mapping_confidence_mean = mean(.data$mapping_confidence, na.rm = TRUE),
      mapping_confidence_max = max(.data$mapping_confidence, na.rm = TRUE),
      mapped_share_sum = sum(dplyr::coalesce(.data$mapped_share, 0), na.rm = TRUE),
      .groups = "drop"
    )
  
  # CPC names (policy-level)
  cpc_name_lu <- cpc_names %>%
    dplyr::rename(cpc3 = 1, cpc_name = 2) %>%
    dplyr::mutate(cpc3 = as.character(.data$cpc3))
  
  by_policy <- policy_asof_tbl %>%
    dplyr::left_join(map_sum, by = "policy_id") %>%
    dplyr::mutate(
      hs6_codes_csv = vapply(.data$hs6_codes, function(v) paste(v, collapse = ","), character(1)),
      cpc3_codes_csv = vapply(.data$cpc3_codes, function(v) paste(v, collapse = ","), character(1)),
      cpc_name_csv = vapply(.data$cpc3_codes, function(v) {
        v <- as.character(v)
        if (length(v) == 0) return("")
        nm <- cpc_name_lu$cpc_name[match(v, cpc_name_lu$cpc3)]
        nm <- nm[!is.na(nm)]
        paste(unique(nm), collapse = "|")
      }, character(1))
    ) %>%
    dplyr::group_by(.data$iso3, .data$country) %>%
    dplyr::mutate(
      country_domestic_stock = sum(.data$scale_strength_pkg[.data$is_active_asof], na.rm = TRUE),
      domestic_share_of_country_stock = dplyr::if_else(
        .data$is_active_asof & .data$country_domestic_stock > 0,
        .data$scale_strength_pkg / .data$country_domestic_stock,
        0
      ),
      domestic_intervention_index = median_scurve(log1p(.data$scale_strength_pkg)),
      policy_strength_index = median_scurve(log1p(.data$policy_strength_pkg))
    ) %>%
    dplyr::ungroup()
  
  by_policy
}

# ==============================================================================
# 5) Output 2: HS6 stock table
# ==============================================================================

build_by_hs6 <- function(policy_asof_tbl,
                         hs6_cpc_lu,
                         hs6_name_lu = NULL,
                         split_across_hs6 = TRUE,
                         balance_alpha = 0.5) {
  balance_alpha <- max(0, min(1, balance_alpha))

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
      domestic_hs6 = .data$scale_strength_pkg * .data$alloc_hs6
    )
  
  policy_level <- hs6_long %>%
    dplyr::filter(.data$is_active_asof) %>%
    dplyr::group_by(.data$iso3, .data$country, .data$HS6, .data$policy_id) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      policy_strength = sum(.data$domestic_hs6, na.rm = TRUE),
      .groups = "drop"
    )

  agg <- policy_level %>%
    dplyr::group_by(.data$iso3, .data$country, .data$HS6) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id),
      domestic_strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      domestic_strength_avg = dplyr::if_else(.data$n_active_policies > 0,
                                             mean(.data$policy_strength, na.rm = TRUE),
                                             0),
      domestic_strength_balanced = exp(
        balance_alpha * log1p(.data$domestic_strength_sum) +
          (1 - balance_alpha) * log1p(.data$domestic_strength_avg)
      ) - 1,
      .groups = "drop"
    ) %>%
    dplyr::rename(domestic_stock_sum = .data$domestic_strength_balanced)
  
  idx <- agg %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(
      domestic_intervention_index = median_scurve(log1p(.data$domestic_stock_sum))
    ) %>%
    dplyr::ungroup()
  
  if (nrow(hs6_cpc_lu) > 0) {
    idx <- idx %>% dplyr::left_join(hs6_cpc_lu, by = "HS6")
  } else {
    idx <- idx %>% dplyr::mutate(cpc3_codes_csv = NA_character_, cpc_name_csv = NA_character_)
  }
  
  if (!is.null(hs6_name_lu) && nrow(hs6_name_lu) > 0) {
    idx <- idx %>% dplyr::left_join(hs6_name_lu, by = "HS6")
  } else {
    idx <- idx %>% dplyr::mutate(hs6_name = NA_character_)
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
                             split_cross_cutting_strength = TRUE,
                             balance_alpha = 0.5) {
  
  # balance_alpha in [0,1]:
  #   1.0 -> pure SUM (extensive margin dominates)
  #   0.0 -> pure MEAN (intensive margin dominates)
  #   default 0.5 -> geometric blend between sum and mean
  balance_alpha <- max(0, min(1, balance_alpha))
  
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
    dplyr::filter(.data$tech != "Unmapped") %>%
    dplyr::mutate(
      mapping_confidence = dplyr::coalesce(.data$mapping_confidence, 1),
      domestic_ts = .data$scale_strength_pkg * .data$alloc * .data$mapping_confidence
    )
  
  # Collapse to POLICY-level within each Country × Tech × SupplyChain so we can
  # blend "sum" and "average" policy strength without double-counting a policy.
  policy_level <- tech_sc_long %>%
    dplyr::filter(.data$is_active_asof) %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain, .data$policy_id) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      policy_strength = sum(.data$domestic_ts, na.rm = TRUE),
      .groups = "drop"
    )
  
  agg <- policy_level %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id),
      domestic_strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      domestic_strength_avg = dplyr::if_else(.data$n_active_policies > 0,
                                             mean(.data$policy_strength, na.rm = TRUE),
                                             0),
      # blended measure:
      # geometric blend: exp( alpha*log(sum+1) + (1-alpha)*log(avg+1) ) - 1
      domestic_strength_balanced = exp(
        balance_alpha * log1p(.data$domestic_strength_sum) +
          (1 - balance_alpha) * log1p(.data$domestic_strength_avg)
      ) - 1,
      .groups = "drop"
    ) %>%
    dplyr::rename(domestic_stock_sum = .data$domestic_strength_balanced)
  
  idx <- agg %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(
      domestic_intervention_index = median_scurve(log1p(.data$domestic_stock_sum))
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
# 7) Output 4: tech x supply_chain by announcement year (rolling 3-year window)
# ==============================================================================

# ==============================================================================


# ==============================================================================
# 7) Output 4: tech yearly priorities (rolling active window)
# ==============================================================================

build_by_tech_sc_year <- function(policy_base_tbl,
                                  tech_sc_cpc_lu,
                                  tech_universe,
                                  supply_chain_universe,
                                  year_min = NULL,
                                  year_max = NULL,
                                  rolling_window_years = 3,
                                  weight_by_active_fraction = TRUE,
                                  expand_cross_cutting = TRUE,
                                  split_cross_cutting_strength = TRUE,
                                  balance_alpha = 0.5) {
  
  balance_alpha <- max(0, min(1, balance_alpha))
  if (!is.finite(rolling_window_years) || rolling_window_years < 1) rolling_window_years <- 1
  rolling_window_years <- as.integer(rolling_window_years)
  
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
    dplyr::filter(.data$tech != "Unmapped") %>%
    dplyr::mutate(
      mapping_confidence = dplyr::coalesce(.data$mapping_confidence, 1),
      domestic_ts = .data$scale_strength_pkg * .data$alloc * .data$mapping_confidence,
      announce_year_raw = suppressWarnings(as.integer(format(.data$announce_date, "%Y"))),
      impl_year = suppressWarnings(as.integer(format(.data$impl_date, "%Y"))),
      anchor_year = dplyr::if_else(is.finite(.data$announce_year_raw), .data$announce_year_raw, .data$impl_year)
    ) %>%
    dplyr::filter(!is.na(.data$impl_date), is.finite(.data$anchor_year))
  
  anchor_years <- tech_sc_long$anchor_year
  if (is.null(year_min)) year_min <- if (length(anchor_years) > 0) min(anchor_years) else as.integer(format(Sys.Date(), "%Y"))
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(year_max)) year_max <- max(c(anchor_years, current_year), na.rm = TRUE)
  year_max <- min(year_max, current_year)
  
  tech_sc_long <- tech_sc_long %>%
    dplyr::mutate(
      removal_year_last_active = dplyr::if_else(
        is.na(.data$removal_date),
        year_max,
        suppressWarnings(as.integer(format(.data$removal_date - 1, "%Y")))
      ),
      end_year_window = .data$anchor_year + rolling_window_years - 1L,
      start_year = pmax(.data$anchor_year, year_min),
      end_year = pmin(year_max, .data$removal_year_last_active, .data$end_year_window),
      year_seq = purrr::map2(.data$start_year, .data$end_year, ~{
        if (is.na(.x) || is.na(.y) || .y < .x) integer(0) else seq.int(.x, .y)
      })
    ) %>%
    tidyr::unnest(.data$year_seq) %>%
    dplyr::rename(announce_year = .data$year_seq)
  
  if (nrow(tech_sc_long) == 0) {
    out <- tibble::tibble(
      iso3 = character(0), country = character(0),
      tech = character(0), supply_chain = character(0), announce_year = integer(0),
      domestic_strength_sum = numeric(0), domestic_strength_avg = numeric(0),
      domestic_strength_balanced = numeric(0), n_policies_window = integer(0),
      domestic_intervention_index_xs = numeric(0),
      cpc3_codes_csv = character(0), cpc_name_csv = character(0)
    )
    return(out)
  }
  
  annual <- tech_sc_long %>%
    dplyr::mutate(
      year_start = as.Date(paste0(.data$announce_year, "-01-01")),
      year_end_excl = as.Date(paste0(.data$announce_year + 1L, "-01-01")),
      interval_start = pmax(.data$impl_date, .data$year_start),
      interval_end = dplyr::if_else(
        is.na(.data$removal_date),
        .data$year_end_excl,
        pmin(.data$removal_date, .data$year_end_excl)
      ),
      days_in_year = as.numeric(.data$year_end_excl - .data$year_start),
      overlap_days = pmax(0, as.numeric(.data$interval_end - .data$interval_start)),
      year_weight = {
        if (isTRUE(weight_by_active_fraction)) {
          dplyr::if_else(.data$days_in_year > 0, .data$overlap_days / .data$days_in_year, 0)
        } else {
          dplyr::if_else(.data$overlap_days > 0, 1, 0)
        }
      },
      domestic_flow = .data$domestic_ts * .data$year_weight
    ) %>%
    dplyr::filter(.data$year_weight > 0)
  
  # Collapse to POLICY-level within each Country × Tech × SupplyChain × Year
  policy_level <- annual %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain, .data$announce_year, .data$policy_id) %>%
    dplyr::summarise(
      policy_strength = sum(.data$domestic_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  agg <- policy_level %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain, .data$announce_year) %>%
    dplyr::summarise(
      n_policies_window = dplyr::n_distinct(.data$policy_id),
      domestic_strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      domestic_strength_avg = dplyr::if_else(.data$n_policies_window > 0,
                                             mean(.data$policy_strength, na.rm = TRUE),
                                             0),
      domestic_strength_balanced = exp(
        balance_alpha * log1p(.data$domestic_strength_sum) +
          (1 - balance_alpha) * log1p(.data$domestic_strength_avg)
      ) - 1,
      .groups = "drop"
    )
  
  out <- agg %>%
    dplyr::group_by(.data$iso3, .data$country, .data$announce_year) %>%
    dplyr::mutate(
      domestic_intervention_index_xs = median_scurve(log1p(.data$domestic_strength_balanced))
    ) %>%
    dplyr::ungroup()
  
  if (nrow(tech_sc_cpc_lu) > 0) {
    out <- out %>% dplyr::left_join(tech_sc_cpc_lu, by = c("tech", "supply_chain"))
  } else {
    out <- out %>% dplyr::mutate(cpc3_codes_csv = NA_character_, cpc_name_csv = NA_character_)
  }
  
  out
}

# ==============================================================================
# 8) Output 5: CPC-level stock table
# ==============================================================================

build_by_cpc <- function(policy_asof_tbl,
                         cpc_name_lu = NULL,
                         split_across_cpc = TRUE,
                         balance_alpha = 0.5) {
  balance_alpha <- max(0, min(1, balance_alpha))

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
      domestic_cpc = .data$scale_strength_pkg * .data$alloc_cpc
    )
  
  if (nrow(cpc_long) == 0) {
    return(tibble::tibble())
  }
  
  policy_level <- cpc_long %>%
    dplyr::filter(.data$is_active_asof) %>%
    dplyr::group_by(.data$iso3, .data$country, .data$cpc3, .data$policy_id) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      policy_strength = sum(.data$domestic_cpc, na.rm = TRUE),
      .groups = "drop"
    )

  agg <- policy_level %>%
    dplyr::group_by(.data$iso3, .data$country, .data$cpc3) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id),
      domestic_strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      domestic_strength_avg = dplyr::if_else(.data$n_active_policies > 0,
                                             mean(.data$policy_strength, na.rm = TRUE),
                                             0),
      domestic_strength_balanced = exp(
        balance_alpha * log1p(.data$domestic_strength_sum) +
          (1 - balance_alpha) * log1p(.data$domestic_strength_avg)
      ) - 1,
      .groups = "drop"
    ) %>%
    dplyr::rename(domestic_stock_sum = .data$domestic_strength_balanced)
  
  if (!is.null(cpc_name_lu) && nrow(cpc_name_lu) > 0) {
    agg <- agg %>% dplyr::left_join(cpc_name_lu, by = "cpc3")
  } else {
    agg <- agg %>% dplyr::mutate(cpc.name = NA_character_)
  }
  
  agg %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(
      domestic_intervention_index = median_scurve(log1p(.data$domestic_stock_sum)),
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
                                year_max = NULL,
                                rolling_window_years = 3,
                                balance_alpha = 0.5,
                                weight_by_active_fraction = TRUE) {
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
      dplyr::pull(.data$`Value.Chain`) %>%
      normalize_chr_vec()
  }
  
  hs6_cpc_lu <- build_hs6_cpc_lookup(cpc_hs = cpc_hs, cpc_names = cpc_names)
  hs6_name_lu <- build_hs6_name_lookup(subcat_raw = subcat_raw)
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
  
  by_hs6 <- build_by_hs6(
    policy_asof,
    hs6_cpc_lu = hs6_cpc_lu,
    hs6_name_lu = hs6_name_lu,
    split_across_hs6 = split_across_hs6,
    balance_alpha = balance_alpha
  )
  
  tech_sc_out <- build_by_tech_sc(
    policy_asof_tbl = policy_asof,
    tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength
    ,
    balance_alpha = balance_alpha
  )
  
  by_tech_sc <- tech_sc_out$data
  
  by_tech_sc_year <- build_by_tech_sc_year(
    policy_base_tbl = policy_base,
    tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    year_min = year_min,
    year_max = year_max,
    rolling_window_years = rolling_window_years,
    weight_by_active_fraction = weight_by_active_fraction,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength,
    balance_alpha = balance_alpha
  )
  
  by_cpc <- build_by_cpc(
    policy_asof_tbl = policy_asof,
    cpc_name_lu = cpc_names,
    split_across_cpc = TRUE,
    balance_alpha = balance_alpha
  )
  
  list(
    by_policy = by_policy,
    by_hs6 = by_hs6,
    by_tech_sc = by_tech_sc,
    by_tech_sc_year = by_tech_sc_year,
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
      hs6_name_lu = hs6_name_lu,
      tech_sc_cpc_lu = tech_sc_cpc_lu,
      tech_cpc_lu = tech_cpc_lu
    )
  )
}

# ==============================================================================

# ------------------------------------------------------------------------------
# Backwards-compatible alias (clearer name)
# ------------------------------------------------------------------------------
nipo_domestic_intervention_outputs <- nipo_policy_outputs

# Self-test (disabled)
# ==============================================================================

if (FALSE) {
  nipo_out <- nipo_policy_outputs(
    raw_nipo = nipo_raw,
    subcat_raw = subcat_raw,
    country_info = country_info
  )
  
  expected_cols <- list(
    by_policy = c("cpc3_codes_csv", "cpc_name_csv", "domestic_intervention_index"),
    by_hs6 = c("cpc3_codes_csv", "cpc_name_csv", "hs6_name", "domestic_intervention_index"),
    by_tech_sc = c("cpc3_codes_csv", "cpc_name_csv", "domestic_intervention_index"),
    by_tech_sc_year = c("cpc3_codes_csv", "cpc_name_csv", "domestic_intervention_index_xs"),
    by_cpc = c("cpc3", "cpc.name", "domestic_intervention_index")
  )
  
  for (nm in names(expected_cols)) {
    
    missing <- setdiff(expected_cols[[nm]], names(nipo_out[[nm]]))
    if (length(missing) > 0) {
      stop(sprintf("Missing columns in %s: %s", nm, paste(missing, collapse = ", ")))
    }
    print(utils::head(nipo_out[[nm]], 5))
  }
}
