# GTA NIPO policy index.

if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "scurve.R"))
}

if (!exists("standardize_country_names", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "country.R"))
}

collapse_chr_listcol <- function(x, sep = " ") {
  vapply(x, function(v) {
    v <- as.character(v)
    v <- v[!is.na(v) & nzchar(v)]
    paste(v, collapse = sep)
  }, character(1))
}

normalize_list <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("Cross-cutting")
  }
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    "Cross-cutting"
  } else {
    x
  }
}

clean_nipo_raw <- function(raw_nipo, subcat_raw, country_info = NULL) {
  subcat_lu <- subcat_raw %>%
    dplyr::mutate(code = stringr::str_pad(as.character(.data$HS6), width = 6, pad = "0")) %>%
    dplyr::distinct(.data$code, .data$Technology, .data$Value.Chain, .data$Sub.Sector)

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
        -c(.data$Technology, .data$Value.Chain, .data$Sub.Sector, .data$code, .data$hs6_raw),
        dplyr::first,
        .names = "{.col}"
      ),
      Technology = list(sort(unique(na.omit(.data$Technology)))),
      Value.Chain = list(sort(unique(na.omit(.data$Value.Chain)))),
      Sub.Sector = list(sort(unique(na.omit(.data$Sub.Sector)))),
      matched_hs6 = sum(!is.na(.data$Technology)),
      total_hs6 = dplyr::n(),
      .groups = "drop"
    )

  nipo_classified <- nipo_classified %>%
    dplyr::mutate(
      country = standardize_country_names(.data$`Implementing Jurisdiction`)
    )

  if (!is.null(country_info) &&
    all(c("country", "iso3c") %in% names(country_info))) {
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

POLICY_TYPE_WEIGHTS <- tibble::tribble(
  ~intervention_type, ~w_type,
  "Import tariff", 0.80,
  "Internal taxation of imports", 0.60,
  "Tax or social insurance relief", 0.85,
  "Export tax", 0.80,
  "Import tariff quota", 0.80,
  "Tax-based export incentive", 0.85,
  "FDI: Entry and ownership rule", 0.60,
  "State aid, unspecified", 0.85,
  "Financial assistance in foreign market", 0.80,
  "Anti-dumping", 0.80,
  "State loan", 0.85,
  "Loan guarantee", 0.85,
  "Localisation, nes", 0.75,
  "Controls on commercial transactions and investment instruments", 0.45,
  "Financial grant", 0.85,
  "Capital injection and equity stakes (including bailouts)", 0.85,
  "Export-related non-tariff measure, nes", 0.80,
  "Other import charges", 0.70,
  "Local content incentive", 0.80,
  "Export ban", 0.85,
  "Anti-subsidy", 0.80,
  "Local content requirement", 0.85,
  "Import-related non-tariff measure, nes", 0.75,
  "Export licensing requirement", 0.70,
  "Import licensing requirement", 0.70,
  "In-kind grant", 0.85,
  "FDI: Financial incentive", 0.80,
  "Trade finance", 0.80,
  "Import price benchmark", 0.70,
  "Production subsidy", 0.85,
  "Price stabilisation", 0.60,
  "Export price benchmark", 0.70,
  "Import ban", 0.85,
  "FDI: Treatment and operations, nes", 0.55,
  "Export tariff quota", 0.75,
  "Interest payment subsidy", 0.85,
  "Import quota", 0.80,
  "Public procurement localisation", 0.75,
  "Public procurement, nes", 0.70,
  "Instrument unclear", 0.20,
  "Safeguard", 0.80,
  "Export quota", 0.80,
  "Import incentive", 0.80,
  "Local operations incentive", 0.80,
  "Import monitoring", 0.60,
  "Local value added incentive", 0.80,
  "Other export incentive", 0.75,
  "Local labour incentive", 0.80,
  "Public procurement preference margin", 0.75,
  "State aid, nes", 0.85,
  "Public procurement access", 0.70,
  "Foreign customer limit", 0.75,
  "Controls on credit operations", 0.45,
  "Local supply requirement for exports", 0.80,
  "Intellectual property protection", 0.40,
  "Export subsidy", 0.85,
  "Trade payment measure", 0.70,
  "Local labour requirement", 0.85,
  "Local operations requirement", 0.85,
  "Anti-circumvention", 0.80,
  "Minimum import price", 0.75,
  "Labour market access", 0.45,
  "Control on personal transactions", 0.40,
  "Port restriction", 0.70,
  "Post-migration treatment", 0.40,
  "Repatriation & surrender requirements", 0.50,
  "Trade balancing measure", 0.80,
  "Technical barrier to trade", 0.70,
  "Competitive devaluation", 0.60
)

STATUS_WEIGHTS <- tibble::tribble(
  ~status, ~w_status,
  "Discriminatory", 1.00,
  "Liberalising", 0.60,
  "Neutral", 0.50,
  "Unclear", 0.30,
  "Unknown", 0.30
)

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

build_policy_table <- function(nipo_country_tbl) {
  nipo_country_tbl %>%
    dplyr::mutate(
      policy_id = .data$nipo_row_id,
      title_text = stringr::str_squish(dplyr::coalesce(.data$Title, "")),
      status = stringr::str_squish(
        dplyr::coalesce(.data$`Initial Assessment (Change Relative to 1 Jan 2009)`, "")
      ),
      jurisdiction = stringr::str_squish(
        dplyr::coalesce(.data$`Level of Government Implementation`, "")
      ),
      intervention_type = stringr::str_squish(
        dplyr::coalesce(.data$`GTA Intervention Type`, "")
      ),
      tech_list = purrr::map(.data$Technology, normalize_list),
      supply_chain_list = purrr::map(.data$Value.Chain, normalize_list),
      text_all = stringr::str_to_lower(stringr::str_squish(paste(
        title_text,
        collapse_chr_listcol(.data$tech_list),
        collapse_chr_listcol(.data$supply_chain_list),
        sep = " | "
      )))
    ) %>%
    tidyr::unnest(.data$tech_list) %>%
    tidyr::unnest(.data$supply_chain_list) %>%
    dplyr::rename(tech = .data$tech_list, supply_chain = .data$supply_chain_list) %>%
    dplyr::mutate(
      tech_crosscut = (.data$tech == "Cross-cutting"),
      sc_crosscut = (.data$supply_chain == "Cross-cutting"),
      status_norm = dplyr::case_when(
        .data$status %in% STATUS_WEIGHTS$status ~ .data$status,
        stringr::str_detect(stringr::str_to_lower(.data$status), "discrimin") ~ "Discriminatory",
        stringr::str_detect(stringr::str_to_lower(.data$status), "liberal") ~ "Liberalising",
        stringr::str_detect(stringr::str_to_lower(.data$status), "neutral") ~ "Neutral",
        stringr::str_detect(stringr::str_to_lower(.data$status), "unclear|unknown") ~ "Unclear",
        TRUE ~ "Unknown"
      ),
      jurisdiction_norm = dplyr::case_when(
        .data$jurisdiction %in% JURIS_WEIGHTS$jurisdiction ~ .data$jurisdiction,
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "national") ~ "National",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "internat") ~ "International",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "sub|state|province|regional") ~
          "Subnational",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "local|city|municip") ~ "Local",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::left_join(POLICY_TYPE_WEIGHTS, by = c("intervention_type" = "intervention_type")) %>%
    dplyr::left_join(STATUS_WEIGHTS, by = c("status_norm" = "status")) %>%
    dplyr::left_join(JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
    dplyr::mutate(
      w_type = dplyr::coalesce(.data$w_type, 0.40),
      w_status = dplyr::coalesce(.data$w_status, 0.50),
      w_juris = dplyr::coalesce(.data$w_juris, 0.80),
      w_tech_specific = dplyr::if_else(.data$tech_crosscut, 0.70, 1.00),
      w_tech = .data$w_tech_specific,
      w_sc = dplyr::if_else(.data$sc_crosscut, 0.75, 1.00),
      policy_strength = .data$w_type * .data$w_status * .data$w_juris * .data$w_tech * .data$w_sc
    )
}

expand_cross_cutting <- function(policy_tbl,
                                 tech_universe,
                                 supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                                 split_strength = FALSE) {
  stopifnot(is.character(tech_universe), length(tech_universe) > 0)
  tech_universe <- setdiff(unique(tech_universe), "Cross-cutting")
  supply_chain_universe <- unique(supply_chain_universe)

  policy_tbl %>%
    dplyr::mutate(
      tech_targets = purrr::map(tech, ~ if (.x == "Cross-cutting") tech_universe else .x),
      sc_targets = purrr::map(
        supply_chain,
        ~ if (.x == "Cross-cutting") supply_chain_universe else .x
      ),
      expanded = purrr::map2(tech_targets, sc_targets, ~ tidyr::expand_grid(
        tech_exp = .x,
        supply_chain_exp = .y
      )),
      expansion_n = purrr::map_int(expanded, nrow)
    ) %>%
    dplyr::select(-tech_targets, -sc_targets) %>%
    tidyr::unnest(expanded) %>%
    dplyr::mutate(
      tech = tech_exp,
      supply_chain = supply_chain_exp,
      policy_strength = if (split_strength) policy_strength / expansion_n else policy_strength
    ) %>%
    dplyr::select(-tech_exp, -supply_chain_exp, -expansion_n) %>%
    dplyr::filter(tech != "Cross-cutting", supply_chain != "Cross-cutting")
}

build_policy_index <- function(policy_tbl,
                               tech_universe,
                               supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                               split_strength = FALSE) {
  policy_tbl_expanded <- expand_cross_cutting(
    policy_tbl = policy_tbl,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )

  agg <- policy_tbl_expanded %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_policies = dplyr::n_distinct(.data$policy_id),
      strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      strength_mean = mean(.data$policy_strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(strength_sum_log = log1p(.data$strength_sum))

  idx <- agg %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(policy_index = median_scurve(.data$strength_sum_log)) %>%
    dplyr::ungroup()

  if (any(idx$tech == "Cross-cutting") || any(idx$supply_chain == "Cross-cutting")) {
    stop("Cross-cutting categories remain after expansion; check expand_cross_cutting().")
  }

  list(
    policy_clean = policy_tbl_expanded,
    policy_agg = agg,
    policy_index = idx
  )
}

build_policy_index_from_raw <- function(raw_nipo,
                                        subcat_raw,
                                        country_info = NULL,
                                        tech_universe = NULL,
                                        supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                                        split_strength = FALSE) {
  nipo_country <- clean_nipo_raw(
    raw_nipo = raw_nipo,
    subcat_raw = subcat_raw,
    country_info = country_info
  )

  if (is.null(tech_universe)) {
    tech_universe <- nipo_country %>%
      dplyr::summarise(techs = list(sort(unique(unlist(.data$Technology))))) %>%
      dplyr::pull(.data$techs) %>%
      unlist() %>%
      as.character()
    tech_universe <- tech_universe[!is.na(tech_universe) & nzchar(tech_universe)]
  }

  policy_tbl <- build_policy_table(nipo_country)

  build_policy_index(
    policy_tbl = policy_tbl,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )
}

nipo_policy_index <- function(raw_nipo,
                              subcat_raw,
                              country_info = NULL,
                              year = 0L,
                              tech_universe = NULL,
                              supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                              split_strength = FALSE) {
  outputs <- build_policy_index_from_raw(
    raw_nipo = raw_nipo,
    subcat_raw = subcat_raw,
    country_info = country_info,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )

  index_tbl <- outputs$policy_index %>%
    dplyr::transmute(
      Country = .data$country,
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      category = "Policy",
      variable = "GTA NIPO Policy Index",
      data_type = "index",
      value = .data$policy_index,
      Year = as.integer(year),
      source = "GTA NIPO",
      explanation = "Policy strength index derived from GTA NIPO intervention attributes"
    )

  list(
    index_tbl = index_tbl,
    outputs = outputs
  )
}
