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
#
# Outputs:
#   - outputs$policy_clean : expanded policy rows (tech × supply_chain) with strength fields
#   - outputs$policy_agg   : aggregated sums and diagnostic metrics
#   - outputs$policy_index : index-ready table (includes bite/scale/restrictive/liberalising)
#   - index_tbl            : a long-format indicator table for downstream pipelines
#
# Key conceptual changes vs a simple type-weight index:
#   - Use Implementation Date + Removal Date to construct a STOCK (active policies) as-of a date
#   - Use scope (horizontal vs firm-specific), planned duration, breadth (HS6/CPC), partner breadth,
#     and monetary coverage when available
#   - Compute a "package" multiplier per State Act (mix of policy families in that act)
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
# GTA / NIPO POLICY OUTPUTS (4 requested views)
# ------------------------------------------------------------------------------
# Requires that these exist in your environment (as in your script):
#   - POLICY_TYPE_WEIGHTS (intervention_type -> w_type)
#   - STATUS_WEIGHTS      (status -> w_status)
#   - JURIS_WEIGHTS       (jurisdiction -> w_juris)
#   - median_scurve()     (your utils/scurve.R)
#   - standardize_country_names() (your utils/country.R)
#
# Produces:
#   1) by_policy: policy-level strength (country level)
#   2) by_hs6:    HS6-level stock strength + indices (country level)
#   3) by_tech_sc: tech x supply_chain stock strength + indices
#   4) by_tech_year: tech annualized (time-weighted avg stock) + indices
# ==============================================================================


# ==============================================================================
# 0) Helper functions
# ==============================================================================

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x)
}

as_bool <- function(x) {
  if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::coalesce(x_chr %in% c("true", "t", "1", "yes", "y"), FALSE)
}

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

normalize_chr_vec <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

safe_list_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0) character(0) else normalize_chr_vec(x)
}

# ==============================================================================
# 1) Clean NIPO raw
#    - Keeps HS6 list as a list-column (hs6_codes)
#    - Computes distinct total_hs6 and matched_hs6 (not inflated by many-to-many joins)
# ==============================================================================

clean_nipo_raw <- function(raw_nipo, subcat_raw, country_info = NULL) {
  
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
      # ---- carry original columns --------------------------------------------
      dplyr::across(
        -c(.data$Technology, .data$`Value Chain`, .data$Sub.Sector, .data$code, .data$hs6_raw),
        dplyr::first,
        .names = "{.col}"
      ),
      
      # ---- mapped taxonomy lists (union across HS6) ---------------------------
      Technology    = list(sort(unique(na.omit(.data$Technology)))),
      `Value Chain` = list(sort(unique(na.omit(.data$`Value Chain`)))),
      Sub.Sector    = list(sort(unique(na.omit(.data$Sub.Sector)))),
      
      # ---- HS6 list (distinct codes) ------------------------------------------
      hs6_codes = list(sort(unique(na.omit(.data$code)))),
      
      # ---- distinct HS6 counts -----------------------------------------------
      total_hs6 = dplyr::n_distinct(.data$code[!is.na(.data$code) & nzchar(.data$code)]),
      matched_hs6 = dplyr::n_distinct(.data$code[!is.na(.data$Technology) & !is.na(.data$code) & nzchar(.data$code)]),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      country = standardize_country_names(.data$`Implementing Jurisdiction`)
    )
  
  # ---- iso3 mapping (optional) -----------------------------------------------
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
  
  base <- nipo_country_tbl %>%
    dplyr::mutate(
      # ---- IDs ---------------------------------------------------------------
      policy_id = .data$nipo_row_id,
      state_act_id = .data$`State Act ID`,
      entry_id = .data$`Entry ID`,
      
      # ---- Key fields --------------------------------------------------------
      intervention_type = stringr::str_squish(dplyr::coalesce(.data$`GTA Intervention Type`, "")),
      status_raw = stringr::str_squish(dplyr::coalesce(.data$`Initial Assessment (Change Relative to 1 Jan 2009)`, "")),
      juris_raw  = stringr::str_squish(dplyr::coalesce(.data$`Level of Government Implementation`, "")),
      
      announce_date = as_date_safe(.data$`Announcement Date`),
      impl_date     = as_date_safe(.data$`Implementation Date`),
      removal_date  = as_date_safe(.data$`Removal Date`),
      
      # ---- HS6 breadth -------------------------------------------------------
      hs6_n = dplyr::coalesce(as.integer(.data$total_hs6), 0L),
      matched_hs6_n = dplyr::coalesce(as.integer(.data$matched_hs6), 0L),
      
      # ---- CPC/partner breadth ----------------------------------------------
      cpc_n = count_csv_tokens(.data$`Sector: CPC 3-digit (v2.1)`),
      partner_n = count_csv_tokens(.data$`Affected Jurisdiction`),
      
      # ---- scale proxies -----------------------------------------------------
      trade_covered_usd_m = suppressWarnings(as.numeric(.data$`Trade Covered (USD Million)`)),
      subsidy_usd_m       = suppressWarnings(as.numeric(.data$`Size of Subsidy (USD Million)`)),
      
      # ---- scope proxies -----------------------------------------------------
      is_horizontal = as_bool(.data$`Is Horizontal`),
      policy_level  = stringr::str_to_lower(dplyr::coalesce(.data$`Levels of Policy Intervention`, "")),
      has_beneficiary = !is.na(.data$`Firm: Beneficiary`) & nzchar(as.character(.data$`Firm: Beneficiary`)),
      
      # ---- policy family flags (packages) -----------------------------------
      fam_export_policy       = as_bool(.data$`Is Export Policy`),
      fam_import_policy       = as_bool(.data$`Is Import Policy`),
      fam_trade_defence       = as_bool(.data$`Is Trade Defence`),
      fam_subsidy             = as_bool(.data$`Is Subsidy`),
      fam_export_incentive    = as_bool(.data$`Is Export Incentive`),
      fam_fdi_policy          = as_bool(.data$`Is FDI Policy`),
      fam_procurement_policy  = as_bool(.data$`Is Procurement Policy`),
      fam_localisation_policy = as_bool(.data$`Is Localisation Policy`),
      fam_other_policy        = as_bool(.data$`Is Other Policy`),
      
      # ---- normalize status --------------------------------------------------
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
      
      # ---- normalize jurisdiction -------------------------------------------
      jurisdiction_norm = dplyr::case_when(
        .data$juris_raw %in% JURIS_WEIGHTS$jurisdiction ~ .data$juris_raw,
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "national") ~ "National",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "internat") ~ "International",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "sub|state|province|regional") ~ "Subnational",
        stringr::str_detect(stringr::str_to_lower(.data$juris_raw), "local|city|municip") ~ "Local",
        TRUE ~ "Unknown"
      ),
      
      # ---- scope multiplier (reach / bite proxy) -----------------------------
      m_scope = dplyr::case_when(
        .data$is_horizontal ~ 1.00,
        .data$has_beneficiary ~ 0.60,
        stringr::str_detect(.data$policy_level, "economy|cross|horizontal") ~ 1.00,
        stringr::str_detect(.data$policy_level, "sector|industry") ~ 0.80,
        stringr::str_detect(.data$policy_level, "firm") ~ 0.60,
        TRUE ~ 0.75
      )
    ) %>%
    # ---- join weights --------------------------------------------------------
  dplyr::left_join(POLICY_TYPE_WEIGHTS, by = c("intervention_type" = "intervention_type")) %>%
    dplyr::left_join(STATUS_WEIGHTS, by = c("status_norm" = "status")) %>%
    dplyr::left_join(JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
    dplyr::mutate(
      w_type   = dplyr::coalesce(.data$w_type, 0.40),
      w_status = dplyr::coalesce(.data$w_status, 0.50),
      w_juris  = dplyr::coalesce(.data$w_juris, 0.80)
    )
  
  # ---- denominators for multipliers (distinct policies) ----------------------
  p95_hs6     <- calc_p95(base$hs6_n, fallback = 1)
  p95_cpc     <- calc_p95(base$cpc_n, fallback = 1)
  p95_geo     <- calc_p95(base$partner_n, fallback = 1)
  p95_trade   <- calc_p95(base$trade_covered_usd_m, fallback = 1)
  p95_subsidy <- calc_p95(base$subsidy_usd_m, fallback = 1)
  
  base <- base %>%
    dplyr::mutate(
      # ---- planned duration proxy ------------------------------------------
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
      
      # ---- breadth / geo / scale multipliers --------------------------------
      m_hs6 = cap_mult(log_mult(.data$hs6_n, p95_hs6), cap = breadth_cap),
      m_cpc = cap_mult(log_mult(.data$cpc_n, p95_cpc), cap = breadth_cap),
      m_breadth = cap_mult(.data$m_hs6 * .data$m_cpc, cap = breadth_cap),
      
      m_geo = cap_mult(log_mult(.data$partner_n, p95_geo), cap = geo_cap),
      
      m_trade   = cap_mult(log_mult(.data$trade_covered_usd_m, p95_trade), cap = scale_cap),
      m_subsidy = cap_mult(log_mult(.data$subsidy_usd_m, p95_subsidy), cap = scale_cap),
      m_scale   = pmax(.data$m_trade, .data$m_subsidy),
      
      # ---- base strengths (per policy entry) --------------------------------
      bite_strength_base  = .data$w_type * .data$w_status * .data$w_juris * .data$m_scope * .data$m_duration,
      scale_strength_base = .data$bite_strength_base * .data$m_breadth * .data$m_geo * .data$m_scale
    )
  
  # ---- package multiplier at State Act level ---------------------------------
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
  
  base <- base %>%
    dplyr::left_join(act_pkg %>% dplyr::select(.data$iso3, .data$state_act_id, .data$m_package),
                     by = c("iso3", "state_act_id")) %>%
    dplyr::mutate(
      m_package = dplyr::coalesce(.data$m_package, 1.0),
      bite_strength_pkg  = .data$bite_strength_base  * .data$m_package,
      scale_strength_pkg = .data$scale_strength_base * .data$m_package
    )
  
  base
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
#    - Adds within-country 0-1 indices for the set of ACTIVE policies (optional)
# ==============================================================================

build_by_policy <- function(policy_asof_tbl) {
  
  # ---- shares within country stock (active only) ------------------------------
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
    dplyr::ungroup()
  
  # ---- within-country normalization (0-1) across ACTIVE policies --------------
  by_policy <- by_policy %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(
      bite_policy_index_within_country = dplyr::if_else(
        .data$is_active_asof,
        median_scurve(log1p(.data$bite_strength_pkg)),
        NA_real_
      ),
      scale_policy_index_within_country = dplyr::if_else(
        .data$is_active_asof,
        median_scurve(log1p(.data$scale_strength_pkg)),
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  by_policy %>%
    dplyr::select(
      iso3, country,
      policy_id, state_act_id, entry_id,
      Title, URL,
      intervention_type, status_norm, jurisdiction_norm,
      announce_date, impl_date, removal_date,
      is_implemented_asof, is_active_asof,
      bite_strength_pkg, scale_strength_pkg,
      bite_share_of_country_stock, scale_share_of_country_stock,
      bite_policy_index_within_country, scale_policy_index_within_country
    )
}

# ==============================================================================
# 5) Output 2: HS6-level stock table (country level)
#    - Expands HS6 codes and allocates each policy across its HS6 list (1/n)
#    - Indexing: compares countries *within each HS6* (0-1 across countries)
# ==============================================================================

build_by_hs6 <- function(policy_asof_tbl, split_across_hs6 = TRUE) {
  
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
  
  # ---- stock aggregation (as-of) ---------------------------------------------
  agg <- hs6_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$HS6) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id[.data$is_active_asof]),
      bite_stock_sum = sum(.data$bite_hs6[.data$is_active_asof], na.rm = TRUE),
      scale_stock_sum = sum(.data$scale_hs6[.data$is_active_asof], na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- indices across countries within each HS6 ------------------------------
  idx <- agg %>%
    dplyr::group_by(.data$HS6) %>%
    dplyr::mutate(
      hs6_bite_index = median_scurve(log1p(.data$bite_stock_sum)),
      hs6_scale_index = median_scurve(log1p(.data$scale_stock_sum))
    ) %>%
    dplyr::ungroup()
  
  idx
}

# ==============================================================================
# 6) Output 3: tech x supply_chain stock table
#    - Allocates each policy into:
#        (a) mapped tech×sc combinations (share = matched_hs6/total_hs6)
#        (b) an Unmapped bucket for residual share (if any)
#        (c) Cross-cutting policies (no HS6) -> Cross-cutting tech/sc, optionally expanded
#    - Indexing: compares countries within each tech×sc (0-1 across countries)
# ==============================================================================

expand_cross_cutting_rows <- function(tbl,
                                      tech_universe,
                                      supply_chain_universe,
                                      split_strength = TRUE) {
  
  tech_universe <- setdiff(unique(tech_universe), c("Cross-cutting", "Unmapped"))
  supply_chain_universe <- setdiff(unique(supply_chain_universe), c("Cross-cutting", "Unmapped"))
  
  out <- tbl %>%
    dplyr::mutate(
      tech_targets = purrr::map(.data$tech, ~ if (.x == "Cross-cutting") tech_universe else .x),
      sc_targets   = purrr::map(.data$supply_chain, ~ if (.x == "Cross-cutting") supply_chain_universe else .x),
      expanded     = purrr::map2(tech_targets, sc_targets, ~ tidyr::expand_grid(
        tech_exp = .x,
        sc_exp = .y
      )),
      expansion_n  = purrr::map_int(expanded, nrow)
    ) %>%
    dplyr::select(-tech_targets, -sc_targets) %>%
    tidyr::unnest(expanded) %>%
    dplyr::mutate(
      tech = .data$tech_exp,
      supply_chain = .data$sc_exp,
      # IMPORTANT: split_strength is scalar, so use base if()
      alloc = if (isTRUE(split_strength)) .data$alloc / pmax(1, .data$expansion_n) else .data$alloc
    ) %>%
    dplyr::select(-tech_exp, -sc_exp, -expansion_n)
  
  out
}

build_by_tech_sc <- function(policy_asof_tbl,
                             tech_universe,
                             supply_chain_universe,
                             expand_cross_cutting = TRUE,
                             split_cross_cutting_strength = TRUE) {
  
  # ---- mapped shares ----------------------------------------------------------
  policy_shares <- policy_asof_tbl %>%
    dplyr::mutate(
      mapped_share = dplyr::if_else(.data$hs6_n > 0, pmin(1, .data$matched_hs6_n / pmax(1, .data$hs6_n)), 0),
      # if the mapping lists are empty, force mapped_share -> 0
      tech_mapped = purrr::map(.data$Technology, safe_list_or_empty),
      sc_mapped   = purrr::map(.data$`Value Chain`, safe_list_or_empty),
      mapped_share = dplyr::if_else((purrr::map_int(.data$tech_mapped, length) == 0) |
                                      (purrr::map_int(.data$sc_mapped, length) == 0),
                                    0, .data$mapped_share),
      unmapped_share = dplyr::if_else(.data$hs6_n > 0, pmax(0, 1 - .data$mapped_share), 0)
    )
  
  # ---- (a) mapped tech×sc combos ---------------------------------------------
  mapped_long <- policy_shares %>%
    dplyr::filter(.data$mapped_share > 0) %>%
    dplyr::mutate(
      combos = purrr::map2(.data$tech_mapped, .data$sc_mapped, ~ tidyr::expand_grid(
        tech = .x,
        supply_chain = .y
      )),
      n_combo = purrr::map_int(.data$combos, nrow)
    ) %>%
    tidyr::unnest(.data$combos) %>%
    dplyr::mutate(
      alloc = .data$mapped_share / pmax(1, .data$n_combo)
    ) %>%
    dplyr::select(-n_combo)
  
  # ---- (b) unmapped bucket ----------------------------------------------------
  unmapped_long <- policy_shares %>%
    dplyr::filter(.data$unmapped_share > 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Unmapped",
      supply_chain = "Unmapped",
      alloc = .data$unmapped_share
    )
  
  # ---- (c) cross-cutting policies (no HS6) -----------------------------------
  cross_long <- policy_shares %>%
    dplyr::filter(.data$hs6_n == 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Cross-cutting",
      supply_chain = "Cross-cutting",
      alloc = 1
    )
  
  tech_sc_long <- dplyr::bind_rows(mapped_long, unmapped_long, cross_long)
  
  # ---- optionally expand cross-cutting across universe ------------------------
  if (isTRUE(expand_cross_cutting)) {
    tech_sc_long <- expand_cross_cutting_rows(
      tech_sc_long,
      tech_universe = tech_universe,
      supply_chain_universe = supply_chain_universe,
      split_strength = split_cross_cutting_strength
    ) %>%
      dplyr::filter(.data$tech != "Cross-cutting", .data$supply_chain != "Cross-cutting")
  }
  
  # ---- compute allocated strengths -------------------------------------------
  tech_sc_long <- tech_sc_long %>%
    dplyr::mutate(
      bite_ts = .data$bite_strength_pkg * .data$alloc,
      scale_ts = .data$scale_strength_pkg * .data$alloc
    )
  
  # ---- stock aggregation (as-of) ---------------------------------------------
  agg <- tech_sc_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      as_of_date = dplyr::first(.data$as_of_date),
      n_active_policies = dplyr::n_distinct(.data$policy_id[.data$is_active_asof]),
      bite_stock_sum = sum(.data$bite_ts[.data$is_active_asof], na.rm = TRUE),
      scale_stock_sum = sum(.data$scale_ts[.data$is_active_asof], na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- indices across countries within each tech×sc --------------------------
  idx <- agg %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      tech_sc_bite_index = median_scurve(log1p(.data$bite_stock_sum)),
      tech_sc_scale_index = median_scurve(log1p(.data$scale_stock_sum))
    ) %>%
    dplyr::ungroup()
  
  idx
}

# ==============================================================================
# 7) Output 4: tech annualized (time-weighted average stock)
#    - Uses the same tech mapping allocation as Output 3 (including cross-cutting expansion)
#    - For each policy-tech row, compute overlap fraction of each year and allocate strength
#    - Aggregates by Country × tech × Year
#    - Indexing: compares countries within each Year×tech
# ==============================================================================

build_by_tech_year <- function(policy_base_tbl,
                               tech_universe,
                               supply_chain_universe,
                               year_min = NULL,
                               year_max = NULL,
                               expand_cross_cutting = TRUE,
                               split_cross_cutting_strength = TRUE) {
  
  # Reuse the tech-sc allocator but WITHOUT as-of flags.
  # We'll create a minimal table with alloc, then do year overlap math.
  tmp <- policy_base_tbl %>%
    dplyr::mutate(
      # placeholders used by build_by_tech_sc
      is_active_asof = TRUE,
      as_of_date = as.Date(NA)
    )
  
  # Build allocation rows (policy × tech × sc × alloc)
  # We call build_by_tech_sc internals by recreating the long form quickly:
  policy_shares <- tmp %>%
    dplyr::mutate(
      mapped_share = dplyr::if_else(.data$hs6_n > 0, pmin(1, .data$matched_hs6_n / pmax(1, .data$hs6_n)), 0),
      tech_mapped = purrr::map(.data$Technology, safe_list_or_empty),
      sc_mapped   = purrr::map(.data$`Value Chain`, safe_list_or_empty),
      mapped_share = dplyr::if_else((purrr::map_int(.data$tech_mapped, length) == 0) |
                                      (purrr::map_int(.data$sc_mapped, length) == 0),
                                    0, .data$mapped_share),
      unmapped_share = dplyr::if_else(.data$hs6_n > 0, pmax(0, 1 - .data$mapped_share), 0)
    )
  
  mapped_long <- policy_shares %>%
    dplyr::filter(.data$mapped_share > 0) %>%
    dplyr::mutate(
      combos = purrr::map2(.data$tech_mapped, .data$sc_mapped, ~ tidyr::expand_grid(
        tech = .x,
        supply_chain = .y
      )),
      n_combo = purrr::map_int(.data$combos, nrow)
    ) %>%
    tidyr::unnest(.data$combos) %>%
    dplyr::mutate(alloc = .data$mapped_share / pmax(1, .data$n_combo)) %>%
    dplyr::select(-n_combo)
  
  unmapped_long <- policy_shares %>%
    dplyr::filter(.data$unmapped_share > 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Unmapped",
      supply_chain = "Unmapped",
      alloc = .data$unmapped_share
    )
  
  cross_long <- policy_shares %>%
    dplyr::filter(.data$hs6_n == 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::everything()),
      tech = "Cross-cutting",
      supply_chain = "Cross-cutting",
      alloc = 1
    )
  
  tech_sc_long <- dplyr::bind_rows(mapped_long, unmapped_long, cross_long)
  
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
  
  # ---- infer year range if not provided --------------------------------------
  impl_years <- suppressWarnings(as.integer(format(tech_sc_long$impl_date, "%Y")))
  impl_years <- impl_years[is.finite(impl_years)]
  if (is.null(year_min)) year_min <- if (length(impl_years) > 0) min(impl_years) else as.integer(format(Sys.Date(), "%Y"))
  
  # if removal dates exist, include their year for overlap calcs; otherwise use current year
  rem_years <- suppressWarnings(as.integer(format(tech_sc_long$removal_date, "%Y")))
  rem_years <- rem_years[is.finite(rem_years)]
  if (is.null(year_max)) {
    year_max <- max(c(impl_years, rem_years, as.integer(format(Sys.Date(), "%Y"))), na.rm = TRUE)
  }
  
  # ---- time-weighted annualization -------------------------------------------
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
      
      bite_avg = .data$bite_ts * .data$year_frac,
      scale_avg = .data$scale_ts * .data$year_frac
    )
  
  # ---- aggregate to Country × tech × Year (overall across supply_chain) -------
  agg <- annual_long %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$Year) %>%
    dplyr::summarise(
      bite_avg_stock_sum = sum(.data$bite_avg, na.rm = TRUE),
      scale_avg_stock_sum = sum(.data$scale_avg, na.rm = TRUE),
      n_policies_touching_year = dplyr::n_distinct(.data$policy_id),
      .groups = "drop"
    )
  
  # ---- indices across countries within each Year×tech -------------------------
  idx <- agg %>%
    dplyr::group_by(.data$Year, .data$tech) %>%
    dplyr::mutate(
      tech_year_bite_index = median_scurve(log1p(.data$bite_avg_stock_sum)),
      tech_year_scale_index = median_scurve(log1p(.data$scale_avg_stock_sum))
    ) %>%
    dplyr::ungroup()
  
  idx
}

# ==============================================================================
# 8) Top-level wrapper: nipo_policy_outputs()
# ==============================================================================

nipo_policy_outputs <- function(raw_nipo,
                                subcat_raw,
                                country_info = NULL,
                                
                                # As-of settings for outputs 1-3
                                as_of_date = NULL,
                                flow_window_days = 365,
                                
                                # Strength parameters
                                duration_norm_months = 24,
                                duration_cap_months = 60,
                                breadth_cap = 3.0,
                                geo_cap = 3.0,
                                scale_cap = 3.0,
                                package_cap = 1.6,
                                package_step = 0.15,
                                
                                # Tech/sc expansion settings (outputs 3-4)
                                tech_universe = NULL,
                                supply_chain_universe = NULL,
                                expand_cross_cutting = TRUE,
                                split_cross_cutting_strength = TRUE,
                                
                                # HS6 allocation
                                split_across_hs6 = TRUE,
                                
                                # Annual range (output 4)
                                year_min = NULL,
                                year_max = NULL) {
  
  # ---- clean + classify -------------------------------------------------------
  nipo_country <- clean_nipo_raw(
    raw_nipo = raw_nipo,
    subcat_raw = subcat_raw,
    country_info = country_info
  )
  
  # ---- infer universes if not provided ---------------------------------------
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
  
  # ---- base policy strength (time-agnostic) ----------------------------------
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
  
  # ---- add as-of flags for stock views ---------------------------------------
  policy_asof <- add_asof_flags(
    policy_base_tbl = policy_base,
    as_of_date = as_of_date,
    flow_window_days = flow_window_days
  )
  
  # ---- 1) policy-level -------------------------------------------------------
  by_policy <- build_by_policy(policy_asof)
  
  # ---- 2) HS6-level ----------------------------------------------------------
  by_hs6 <- build_by_hs6(policy_asof, split_across_hs6 = split_across_hs6)
  
  # ---- 3) tech × supply_chain ------------------------------------------------
  by_tech_sc <- build_by_tech_sc(
    policy_asof_tbl = policy_asof,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength
  )
  
  # ---- 4) tech annualized ----------------------------------------------------
  by_tech_year <- build_by_tech_year(
    policy_base_tbl = policy_base,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    year_min = year_min,
    year_max = year_max,
    expand_cross_cutting = expand_cross_cutting,
    split_cross_cutting_strength = split_cross_cutting_strength
  )
  
  list(
    by_policy = by_policy,
    by_hs6 = by_hs6,
    by_tech_sc = by_tech_sc,
    by_tech_year = by_tech_year,
    internals = list(
      nipo_country = nipo_country,
      policy_base = policy_base,
      policy_asof = policy_asof
    )
  )
}
