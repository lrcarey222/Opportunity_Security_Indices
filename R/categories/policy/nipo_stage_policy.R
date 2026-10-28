# NIPO direct six-stage policy layer (docs/strategic_position.md, section 4 "NIPO").
#
# Puts each GTA New Industrial Policy Observatory intervention on the country x technology x
# six-stage cells its HS6 codes name, through the `stage` column of the HS6 master
# (energy_hs6_master.csv). The legacy Domestic Intervention Score (nipo_policy_index.R) maps
# through the three-stage `supply_chain` column and an S-curve of strengths; nothing here uses
# either. The outputs are counts: stock (in force on the as-of date) and flow (announced in a
# trailing window), each the sum of attribution shares, so a measure contributes at most 1 in
# total, whatever the number of cells it touches.
#
# Rules (all parameters come from config/strategic_position.yml, policy_priority section):
#   * One measure per GTA intervention (Entry ID). EU acts that GTA records once per member
#     state become one measure of the EU ("EUU"); members see them only in the
#     eu_supranational_* cell fields, never in their own counts. Other entries recorded for
#     several jurisdictions (IPCEI state aid, Mercosur and GCC acts, EIB loans to named
#     members) stay one measure per jurisdiction: each member adopts or funds its own part.
#   * Sanctions and liberalising measures are excluded from the counts and reported.
#   * Only narrow measures with basket codes are attributed to cells. Broad tariff schedules,
#     sanctions lists and export-control lists would otherwise put every country in every cell
#     (the audit found 82% of legacy strength on measures with under half their codes in the
#     basket). Broad measures are tech_wide or cross_cutting context per country. A short code
#     list is narrow only when its basket codes are more than incidental (at least
#     narrow_count_min_basket_share of them): 208 stock measures had 11-20 codes with a single
#     basket code among them.
#   * Stock is what is in force on the as-of date. GTA gives one-off measures (a grant, a loan,
#     an export credit, a state-aid decision) no removal date, so the literal rule kept every one
#     since 2009 in force (43% of the stock predated 2020). Measures of one_off_families without a
#     removal date count in stock for one_off_stock_years after implementation; other measures,
#     and one-off measures that do carry a removal date, stay until removed.
#   * NIPO has no Deployment evidence (no HS6 code is a deployment) and no Geothermal or Green
#     Hydrogen basket: those cells are absent from the output (no_data), not zero.
#
# Depends on helpers the caller sources: R/utils/supply_chain_stages.R (stage constants),
# R/utils/country.R (country_name_to_iso3) and R/categories/policy/nipo_policy_index.R, whose
# pure helpers clean_code(), as_date_safe(), as_bool() and check_required_columns() are reused
# so that HS6 and date parsing match the legacy DIS. Pure: no IO.

# GTA extract columns this layer reads, under the names used here.
NIPO_STAGE_RAW_COLUMNS <- c(
  state_act_id = "State Act ID",
  entry_id = "Entry ID",
  jurisdiction_name = "Implementing Jurisdiction",
  level_of_government = "Level of Government Implementation",
  assessment = "Initial Assessment (Change Relative to 1 Jan 2009)",
  announcement_date = "Announcement Date",
  implementation_date = "Implementation Date",
  removal_date = "Removal Date",
  hs6_list = "Product: HS 6-digit (2022)",
  is_sanction = "Is Sanction",
  gta_intervention_type = "GTA Intervention Type",
  trade_covered_musd = "Trade Covered (USD Million)",
  subsidy_musd = "Size of Subsidy (USD Million)"
)

# EU-27. Members carry the EU's cell stock in eu_supranational_*.
NIPO_EU_MEMBERS_ISO3 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
  "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

# Former members and the last day EU law applied to them. GTA records pre-Brexit EU acts for
# the United Kingdom as well (371 Supra-national rows, all announced by 2020-11), so an act
# recorded for the UK in that period is still an EU act.
NIPO_FORMER_EU_MEMBERS <- c(GBR = "2020-12-31")

# The October 2026 NIPO extract assesses measures as Distortive or Liberalising, not with the
# GTA Red / Amber / Green evaluation the config names. Distortive covers GTA's red and amber
# ("almost certainly" and "likely" discriminatory); Liberalising is green.
NIPO_ASSESSMENT_EQUIVALENTS <- list(Distortive = c("Red", "Amber"), Liberalising = "Green")

# Posture-table families whose measures GTA records as one-off acts with no removal date, and the
# years such a measure counts in stock (config policy_priority$nipo one_off_families,
# one_off_stock_years). Five years is about the life of a grant or loan disbursement programme;
# it is a stated assumption, not a GTA field.
NIPO_ONE_OFF_FAMILIES <- c("grant", "public_finance", "export_support", "state_aid")
NIPO_ONE_OFF_STOCK_YEARS <- 5

# Stock rule per measure (measures table `stock_rule`): "one_off_window" for a one-off family
# without a removal date, "until_removed" otherwise.
NIPO_STAGE_STOCK_RULES <- c("until_removed", "one_off_window")

# The GTA family flags and the posture each implies, for the posture-table cross-check.
NIPO_STAGE_GTA_FLAG_POSTURE <- c(
  "Is Subsidy" = "support",
  "Is Export Incentive" = "support",
  "Is Import Policy" = "protection",
  "Is Trade Defence" = "protection",
  "Is Export Policy" = "protection",
  "Is Procurement Policy" = "protection",
  "Is Localisation Policy" = "either",
  "Is FDI Policy" = "either",
  "Is Other Policy" = "neither"
)

NIPO_STAGE_MEASURE_COLUMNS <- c(
  "intervention_id", "state_act_id", "iso3", "jurisdiction_name", "is_eu_supranational",
  "gta_intervention_type", "family", "support", "protection", "strength_class", "assessment",
  "is_sanction", "counted", "exclusion_reason", "announcement_date", "implementation_date",
  "removal_date", "stock_rule", "active_at_as_of", "n_codes", "n_basket_codes", "basket_share", "mapping_status",
  "link_basis", "mapped_techs", "subsidy_usd", "trade_covered_usd"
)

NIPO_STAGE_LINK_COLUMNS <- c(
  "intervention_id", "iso3", "tech", "supply_chain", "attribution_share", "n_codes_in_cell",
  "n_essential_codes_in_cell", "link_basis"
)

NIPO_STAGE_MAPPING_STATUSES <- c("attributed", "tech_wide", "cross_cutting", "unmapped")

nipo_stage_require_helpers <- function() {
  needed <- c("clean_code", "as_date_safe", "as_bool", "check_required_columns", "assert_supply_chain_stages")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(missing) > 0) {
    stop(
      "nipo_stage_policy.R needs ", paste(missing, collapse = ", "), ": source ",
      "R/utils/supply_chain_stages.R and R/categories/policy/nipo_policy_index.R first."
    )
  }
  invisible(TRUE)
}

# GTA ids are read as doubles; as.character() would print 100000 as "1e+05".
nipo_stage_id_chr <- function(x) {
  if (is.numeric(x)) {
    out <- sprintf("%.0f", x)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  out <- trimws(as.character(x))
  out[!nzchar(out)] <- NA_character_
  out
}

# ---- Inputs -------------------------------------------------------------------------------

# The six-stage master as (tech, supply_chain, hs6, essential), one row per cell and code.
# `supply_chain` is the master's six-stage `stage`; the master's own `supply_chain` column is
# the legacy three-stage label and is deliberately not read. Rows marked excluded by the basket
# review are dropped. Only the stages HS6 can evidence are kept: Deployment is not a traded
# product, so a Deployment row (none today) would put a product measure on the wrong stage.
nipo_stage_prepare_master <- function(master, stages = SUPPLY_CHAIN_SUPPLY_STAGES) {
  nipo_stage_require_helpers()
  check_required_columns(master, c("tech", "hs6", "essential", "stage"), "energy_hs6_master")
  if ("excluded" %in% names(master)) {
    master <- master[!as_bool(master$excluded), , drop = FALSE]
  }
  out <- tibble::tibble(
    tech = trimws(as.character(master$tech)),
    supply_chain = trimws(as.character(master$stage)),
    hs6 = clean_code(master$hs6, width = 6),
    essential = as_bool(master$essential)
  )
  out <- dplyr::filter(out, !is.na(.data$hs6), !is.na(.data$tech), nzchar(.data$tech))
  assert_supply_chain_stages(out$supply_chain, "energy_hs6_master stage")
  out %>%
    dplyr::filter(.data$supply_chain %in% stages) %>%
    # A code listed twice in one cell is essential if either row says so.
    dplyr::group_by(.data$tech, .data$supply_chain, .data$hs6) %>%
    dplyr::summarise(essential = any(.data$essential), .groups = "drop")
}

# Validated posture table (config/nipo_instrument_posture.csv).
nipo_stage_prepare_posture <- function(posture) {
  nipo_stage_require_helpers()
  check_required_columns(
    posture, c("gta_intervention_type", "family", "support", "protection", "strength_class"),
    "nipo_instrument_posture"
  )
  out <- tibble::tibble(
    gta_intervention_type = trimws(as.character(posture$gta_intervention_type)),
    family = trimws(as.character(posture$family)),
    support = as.integer(posture$support),
    protection = as.integer(posture$protection),
    strength_class = as.integer(posture$strength_class)
  )
  dup <- unique(out$gta_intervention_type[duplicated(out$gta_intervention_type)])
  if (length(dup) > 0) stop("nipo_instrument_posture lists a type twice: ", paste(dup, collapse = "; "))
  bad <- out$gta_intervention_type[
    !out$support %in% c(0L, 1L) | !out$protection %in% c(0L, 1L) | !out$strength_class %in% c(1L, 2L) |
      is.na(out$family) | !nzchar(out$family)
  ]
  if (length(bad) > 0) {
    stop(
      "nipo_instrument_posture: support and protection must be 0/1, strength_class 1/2 and family set; check ",
      paste(bad, collapse = "; ")
    )
  }
  out
}

# TRUE where the GTA assessment, or its Red/Amber/Green equivalent, is one the config counts.
nipo_stage_assessment_counted <- function(assessment,
                                          count_assessments,
                                          equivalents = NIPO_ASSESSMENT_EQUIVALENTS) {
  a <- trimws(as.character(assessment))
  via_alias <- vapply(a, function(v) {
    !is.na(v) && v %in% names(equivalents) && any(equivalents[[v]] %in% count_assessments)
  }, logical(1), USE.NAMES = FALSE)
  (!is.na(a) & a %in% count_assessments) | via_alias
}

nipo_stage_is_liberalising <- function(assessment, equivalents = NIPO_ASSESSMENT_EQUIVALENTS) {
  a <- trimws(as.character(assessment))
  green <- names(equivalents)[vapply(equivalents, function(e) "Green" %in% e, logical(1))]
  !is.na(a) & a %in% c("Liberalising", "Green", green)
}

# ---- 1. Measures --------------------------------------------------------------------------

# One row per GTA intervention: implementing iso3 (EU acts as `eu_iso3`), parsed dates, the
# posture of its instrument, and whether it is counted. Keeps the raw comma-separated HS6 list
# in `hs6_list` for nipo_stage_map_measures(). USD values are converted from the extract's USD
# million and, with split_repeated_values, shared out where a State Act repeats one figure on
# several interventions.
#
# An entry is an EU act when every jurisdiction GTA records it for is an EU member (or a former
# member, up to its exit) and GTA marks it Supra-national or records it for at least
# `eu_wide_min_members` members: the second clause catches EU-wide acts filed at another level
# (EIB programmes recorded for all 28 members at IFI level, an EU safeguard termination filed as
# National for 27) while leaving multi-member state aid such as IPCEIs (5 to 14 members, each
# funding its own share) with the members.
nipo_stage_clean_measures <- function(raw,
                                      posture,
                                      count_assessments = c("Red", "Amber"),
                                      exclude_sanctions = TRUE,
                                      assessment_equivalents = NIPO_ASSESSMENT_EQUIVALENTS,
                                      eu_members_iso3 = NIPO_EU_MEMBERS_ISO3,
                                      former_eu_members = NIPO_FORMER_EU_MEMBERS,
                                      eu_wide_min_members = 20,
                                      eu_iso3 = "EUU",
                                      eu_name = "European Union",
                                      split_repeated_values = TRUE,
                                      iso3_of = country_name_to_iso3) {
  nipo_stage_require_helpers()
  check_required_columns(raw, unname(NIPO_STAGE_RAW_COLUMNS), "GTA NIPO extract")
  posture <- nipo_stage_prepare_posture(posture)

  rows <- dplyr::transmute(raw, !!!rlang::syms(NIPO_STAGE_RAW_COLUMNS)) %>%
    dplyr::mutate(
      state_act_id = nipo_stage_id_chr(.data$state_act_id),
      entry_id = nipo_stage_id_chr(.data$entry_id),
      jurisdiction_name = trimws(as.character(.data$jurisdiction_name)),
      level_of_government = trimws(as.character(.data$level_of_government)),
      assessment = trimws(as.character(.data$assessment)),
      gta_intervention_type = trimws(as.character(.data$gta_intervention_type)),
      announcement_date = as_date_safe(.data$announcement_date),
      implementation_date = as_date_safe(.data$implementation_date),
      removal_date = as_date_safe(.data$removal_date),
      is_sanction = as_bool(.data$is_sanction),
      hs6_list = as.character(.data$hs6_list),
      trade_covered_musd = suppressWarnings(as.numeric(.data$trade_covered_musd)),
      subsidy_musd = suppressWarnings(as.numeric(.data$subsidy_musd))
    ) %>%
    dplyr::filter(!is.na(.data$entry_id))
  # Resolve each distinct name once; the extract repeats 77 names over 62k rows.
  names_u <- unique(rows$jurisdiction_name)
  rows$iso3 <- iso3_of(names_u)[match(rows$jurisdiction_name, names_u)]

  former_end <- as.Date(unname(former_eu_members[rows$iso3]))
  rows$eu_member_row <- rows$iso3 %in% eu_members_iso3 |
    (rows$iso3 %in% names(former_eu_members) & !is.na(rows$announcement_date) &
      !is.na(former_end) & rows$announcement_date <= former_end)

  eu_entries <- rows %>%
    dplyr::group_by(.data$entry_id) %>%
    dplyr::summarise(
      is_eu_act = all(.data$eu_member_row) &&
        (any(.data$level_of_government == "Supra-national", na.rm = TRUE) ||
          dplyr::n_distinct(.data$iso3) >= eu_wide_min_members),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$is_eu_act)
  eu_ids <- eu_entries$entry_id

  # The EU act's dates, instrument and HS6 list are the same on every member row (they are in
  # the October 2026 extract). Size of Subsidy is the act's budget, repeated on each row, so it
  # is taken once; Trade Covered is each member's own affected imports, so it is summed.
  eu_measures <- rows %>%
    dplyr::filter(.data$entry_id %in% eu_ids) %>%
    dplyr::group_by(.data$entry_id) %>%
    dplyr::summarise(
      state_act_id = dplyr::first(.data$state_act_id),
      gta_intervention_type = dplyr::first(.data$gta_intervention_type),
      assessment = dplyr::first(.data$assessment),
      is_sanction = any(.data$is_sanction),
      announcement_date = dplyr::first(.data$announcement_date),
      implementation_date = dplyr::first(.data$implementation_date),
      removal_date = dplyr::first(.data$removal_date),
      hs6_list = {
        v <- unique(.data$hs6_list[!is.na(.data$hs6_list) & nzchar(.data$hs6_list)])
        if (length(v) == 0) NA_character_ else paste(v, collapse = ",")
      },
      subsidy_usd = {
        v <- .data$subsidy_musd[!is.na(.data$subsidy_musd)]
        if (length(v) == 0) NA_real_ else max(v) * 1e6
      },
      trade_covered_usd = {
        v <- .data$trade_covered_musd[!is.na(.data$trade_covered_musd)]
        if (length(v) == 0) NA_real_ else sum(v) * 1e6
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      intervention_id = .data$entry_id,
      iso3 = eu_iso3,
      jurisdiction_name = eu_name,
      is_eu_supranational = TRUE
    )

  own_measures <- rows %>%
    dplyr::filter(!.data$entry_id %in% eu_ids) %>%
    dplyr::distinct(.data$entry_id, .data$jurisdiction_name, .keep_all = TRUE) %>%
    dplyr::group_by(.data$entry_id) %>%
    dplyr::mutate(
      intervention_id = if (dplyr::n() > 1) {
        paste0(.data$entry_id, "-", dplyr::coalesce(.data$iso3, .data$jurisdiction_name))
      } else {
        .data$entry_id
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      is_eu_supranational = FALSE,
      subsidy_usd = .data$subsidy_musd * 1e6,
      trade_covered_usd = .data$trade_covered_musd * 1e6
    )

  keep <- c(
    "intervention_id", "state_act_id", "iso3", "jurisdiction_name", "is_eu_supranational",
    "gta_intervention_type", "assessment", "is_sanction", "announcement_date",
    "implementation_date", "removal_date", "hs6_list", "subsidy_usd", "trade_covered_usd"
  )
  measures <- dplyr::bind_rows(
    dplyr::select(own_measures, dplyr::all_of(keep)),
    dplyr::select(eu_measures, dplyr::all_of(keep))
  )
  if (isTRUE(split_repeated_values)) {
    # One State Act often delivers one programme through several interventions, each recorded
    # with the act's full figure (Korea's USD 30bn outward-investment fund is a state loan, a
    # loan guarantee and an equity stake at 30bn each; an anti-dumping and a countervailing duty
    # on the same goods carry the same trade covered). Summed per cell, the figure would count
    # once per instrument, so a value repeated within an act is split equally among the
    # interventions that repeat it.
    measures <- measures %>%
      dplyr::mutate(
        subsidy_usd = .data$subsidy_usd / dplyr::n(),
        .by = c("state_act_id", "subsidy_usd")
      ) %>%
      dplyr::mutate(
        trade_covered_usd = .data$trade_covered_usd / dplyr::n(),
        .by = c("state_act_id", "trade_covered_usd")
      )
  }

  unknown <- setdiff(unique(measures$gta_intervention_type), posture$gta_intervention_type)
  if (length(unknown) > 0) {
    stop(
      "config/nipo_instrument_posture.csv has no row for GTA Intervention Type(s): ",
      paste(ifelse(is.na(unknown), "<NA>", unknown), collapse = "; "),
      ". Add them (family, support, protection, strength_class) before rebuilding."
    )
  }
  measures <- dplyr::left_join(measures, posture, by = "gta_intervention_type")

  assess_ok <- nipo_stage_assessment_counted(measures$assessment, count_assessments, assessment_equivalents)
  measures %>%
    dplyr::mutate(
      exclusion_reason = dplyr::case_when(
        is.na(.data$iso3) ~ "no_iso3",
        isTRUE(exclude_sanctions) & .data$is_sanction ~ "sanction",
        is.na(.data$assessment) | !nzchar(.data$assessment) ~ "assessment_missing",
        !assess_ok & nipo_stage_is_liberalising(.data$assessment, assessment_equivalents) ~ "liberalising",
        !assess_ok ~ "assessment_not_counted",
        TRUE ~ NA_character_
      ),
      counted = is.na(.data$exclusion_reason)
    ) %>%
    dplyr::arrange(.data$iso3, .data$intervention_id)
}

# ---- 2. Links -----------------------------------------------------------------------------

# Long (intervention_id, hs6) table of each measure's distinct, valid HS6 codes.
nipo_stage_split_hs6 <- function(intervention_id, hs6_list) {
  toks <- strsplit(dplyr::coalesce(as.character(hs6_list), ""), ",", fixed = TRUE)
  long <- tibble::tibble(
    intervention_id = rep(intervention_id, lengths(toks)),
    hs6 = clean_code(unlist(toks, use.names = FALSE), width = 6)
  )
  dplyr::distinct(dplyr::filter(long, !is.na(.data$hs6)))
}

# Adds the mapping columns to the measures and returns the attribution links.
#
# For each measure: n_codes (distinct HS6), n_basket_codes (of those, codes in the master),
# basket_share, link_basis ("essential" when any basket code is essential for its cell, so the
# measure is placed by its essential codes only; "all" otherwise), mapped_techs (technologies on
# that basis) and mapping_status. Links are written for attributed measures only (counted or
# not; the measures table says which are counted). A measure's share of a cell is
#   (1 / n basis codes) * sum over its basis codes in the cell of 1 / (cells the code is in),
# so its shares sum to 1 and a code shared by k technologies splits its weight k ways instead of
# being counted in full in each.
#
# Narrow: at least narrow_min_basket_share of the codes in the basket, or at most narrow_max_codes
# codes of which at least narrow_count_min_basket_share are in the basket. The floor on the
# count clause stops one basket code in a 15-code list (a measure on, say, chemicals that names
# one battery precursor) from putting the whole measure on that cell; set it to 0 for the
# count-only rule.
nipo_stage_map_measures <- function(measures,
                                    master,
                                    narrow_max_codes = 20,
                                    narrow_min_basket_share = 0.5,
                                    narrow_count_min_basket_share = 0.1) {
  nipo_stage_require_helpers()
  check_required_columns(measures, c("intervention_id", "iso3", "hs6_list"), "measures")
  check_required_columns(master, c("tech", "supply_chain", "hs6", "essential"), "prepared master")

  codes <- nipo_stage_split_hs6(measures$intervention_id, measures$hs6_list)
  n_codes <- dplyr::count(codes, .data$intervention_id, name = "n_codes")
  hits <- dplyr::inner_join(codes, master, by = "hs6", relationship = "many-to-many")
  rm(codes)

  basket <- hits %>%
    dplyr::group_by(.data$intervention_id) %>%
    dplyr::summarise(
      n_basket_codes = dplyr::n_distinct(.data$hs6),
      has_essential = any(.data$essential),
      .groups = "drop"
    )
  used <- hits %>%
    dplyr::inner_join(dplyr::select(basket, "intervention_id", "has_essential"), by = "intervention_id") %>%
    dplyr::filter(!.data$has_essential | .data$essential)
  techs <- used %>%
    dplyr::group_by(.data$intervention_id) %>%
    dplyr::summarise(
      n_techs = dplyr::n_distinct(.data$tech),
      mapped_techs = paste(sort(unique(.data$tech)), collapse = ";"),
      .groups = "drop"
    )

  mapped <- measures %>%
    dplyr::select(-dplyr::any_of(c(
      "n_codes", "n_basket_codes", "basket_share", "mapping_status", "link_basis", "mapped_techs"
    ))) %>%
    dplyr::left_join(n_codes, by = "intervention_id") %>%
    dplyr::left_join(basket, by = "intervention_id") %>%
    dplyr::left_join(techs, by = "intervention_id") %>%
    dplyr::mutate(
      n_codes = dplyr::coalesce(.data$n_codes, 0L),
      n_basket_codes = dplyr::coalesce(.data$n_basket_codes, 0L),
      basket_share = dplyr::if_else(.data$n_codes > 0, .data$n_basket_codes / .data$n_codes, NA_real_),
      narrow = !is.na(.data$basket_share) & (
        .data$basket_share >= narrow_min_basket_share |
          (.data$n_codes <= narrow_max_codes & .data$basket_share >= narrow_count_min_basket_share)
      ),
      mapping_status = dplyr::case_when(
        .data$n_basket_codes == 0 ~ "unmapped",
        .data$narrow ~ "attributed",
        .data$n_techs == 1 ~ "tech_wide",
        TRUE ~ "cross_cutting"
      ),
      link_basis = dplyr::case_when(
        .data$n_basket_codes == 0 ~ NA_character_,
        .data$has_essential ~ "essential",
        TRUE ~ "all"
      )
    ) %>%
    dplyr::select(-"narrow", -"has_essential", -"n_techs", -"hs6_list")

  attributed_ids <- mapped$intervention_id[mapped$mapping_status == "attributed"]
  shares <- used %>%
    dplyr::filter(.data$intervention_id %in% attributed_ids) %>%
    dplyr::group_by(.data$intervention_id, .data$hs6) %>%
    dplyr::mutate(w = 1 / dplyr::n()) %>%
    dplyr::group_by(.data$intervention_id) %>%
    dplyr::mutate(w = .data$w / dplyr::n_distinct(.data$hs6)) %>%
    dplyr::group_by(.data$intervention_id, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(attribution_share = sum(.data$w), .groups = "drop")
  code_counts <- hits %>%
    dplyr::filter(.data$intervention_id %in% attributed_ids) %>%
    dplyr::group_by(.data$intervention_id, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_codes_in_cell = dplyr::n_distinct(.data$hs6),
      n_essential_codes_in_cell = dplyr::n_distinct(.data$hs6[.data$essential]),
      .groups = "drop"
    )
  links <- shares %>%
    dplyr::left_join(code_counts, by = c("intervention_id", "tech", "supply_chain")) %>%
    dplyr::left_join(dplyr::select(mapped, "intervention_id", "iso3", "link_basis"), by = "intervention_id") %>%
    dplyr::select(dplyr::all_of(NIPO_STAGE_LINK_COLUMNS)) %>%
    dplyr::arrange(.data$iso3, .data$intervention_id, .data$tech, .data$supply_chain)

  list(measures = mapped, links = links)
}

# ---- 3. Stock and flow --------------------------------------------------------------------

# The as-of date: the configured one, else the latest announcement date in the extract.
nipo_stage_resolve_as_of <- function(measures, as_of = NULL) {
  if (!is.null(as_of) && length(as_of) == 1 && !is.na(as_of) && nzchar(as.character(as_of))) {
    return(as.Date(as_of))
  }
  out <- suppressWarnings(max(measures$announcement_date, na.rm = TRUE))
  if (!is.finite(out)) stop("nipo_stage_resolve_as_of(): no announcement dates to infer the as-of date from.")
  as.Date(out)
}

# In force on as_of: implemented on or before it and not removed by it. A measure flagged
# `one_off` (a one-off family with no removal date, see nipo_stage_stock_rule()) also lapses
# one_off_years after implementation: it is in stock only while implementation_date lies in
# (as_of - one_off_years, as_of], the same half-open window as the flow. NULL one_off_years
# keeps the literal rule.
nipo_stage_active <- function(implementation_date, removal_date, as_of, one_off = FALSE, one_off_years = NULL) {
  active <- !is.na(implementation_date) & implementation_date <= as_of &
    (is.na(removal_date) | removal_date > as_of)
  if (is.null(one_off_years) || !any(one_off, na.rm = TRUE)) return(active)
  lapsed <- one_off & is.na(removal_date) & !is.na(implementation_date) &
    implementation_date <= nipo_stage_years_before(as_of, one_off_years)
  active & !lapsed
}

# "one_off_window" for a measure of a one-off family that GTA gives no removal date, else
# "until_removed". A one-off measure with a removal date keeps it: GTA then knows when it ended.
nipo_stage_stock_rule <- function(family, removal_date, one_off_families = NIPO_ONE_OFF_FAMILIES) {
  ifelse(!is.na(family) & family %in% one_off_families & is.na(removal_date), "one_off_window", "until_removed")
}

# Stock membership of each row of a measures table (family, implementation_date, removal_date).
nipo_stage_in_stock <- function(measures,
                                as_of,
                                one_off_families = NIPO_ONE_OFF_FAMILIES,
                                one_off_years = NIPO_ONE_OFF_STOCK_YEARS) {
  rule <- nipo_stage_stock_rule(measures$family, measures$removal_date, one_off_families)
  nipo_stage_active(
    measures$implementation_date, measures$removal_date, as.Date(as_of),
    one_off = rule == "one_off_window", one_off_years = one_off_years
  )
}

# The date `years` calendar years before as_of (29 Feb rolls to 1 Mar).
nipo_stage_years_before <- function(as_of, years) {
  lt <- as.POSIXlt(as.Date(as_of))
  lt$year <- lt$year - years
  as.Date(lt)
}

# Announced in the trailing window (as_of - years, as_of].
nipo_stage_announced_within <- function(announcement_date, as_of, years) {
  !is.na(announcement_date) & announcement_date > nipo_stage_years_before(as_of, years) &
    announcement_date <= as_of
}

# One row per country x NIPO-observable cell (the master's tech x stage cells) for every
# implementing country with a counted measure (the EU as `eu_iso3` among them), zeros included.
# Only counted, attributed measures enter. support_*, protection_* and all_* are sums of
# attribution shares (a measure that is both support and protection counts once in all_*);
# *_iw multiply each share by the instrument's strength_class (sensitivity only). n_* count
# distinct measures. subsidy_usd_stock and trade_covered_usd_stock are identified values x
# attribution share: 0 when the cell has no stock measure, NA when it has some but none reports
# the value. fiscal_data_coverage (trade_data_coverage) is the share of the cell's support
# (protection) stock whose measures report a subsidy size (trade covered); NA without such stock.
# EU members carry the EU's all_stock and all_flow in eu_supranational_*; other rows NA.
# Stock follows nipo_stage_in_stock() (one-off families lapse after one_off_years); flow is
# announcement-dated and unaffected.
nipo_stage_cells <- function(measures,
                             links,
                             master,
                             as_of,
                             flow_windows = c(1, 3, 5),
                             flow_window_years = 3,
                             eu_members_iso3 = NIPO_EU_MEMBERS_ISO3,
                             eu_iso3 = "EUU",
                             one_off_families = NIPO_ONE_OFF_FAMILIES,
                             one_off_years = NIPO_ONE_OFF_STOCK_YEARS) {
  as_of <- as.Date(as_of)
  flow_windows <- sort(unique(as.integer(flow_windows)))
  main <- as.integer(flow_window_years)
  if (!main %in% flow_windows) flow_windows <- sort(c(flow_windows, main))

  cells <- dplyr::distinct(master, .data$tech, .data$supply_chain)
  countries <- sort(unique(measures$iso3[measures$counted & !is.na(measures$iso3)]))
  grid <- tidyr::crossing(tibble::tibble(iso3 = countries), cells)

  m <- measures %>%
    dplyr::filter(.data$counted, .data$mapping_status == "attributed") %>%
    dplyr::select(
      "intervention_id", "gta_intervention_type", "family", "support", "protection", "strength_class",
      "announcement_date", "implementation_date", "removal_date", "subsidy_usd", "trade_covered_usd"
    )
  x <- links %>%
    dplyr::select("intervention_id", "iso3", "tech", "supply_chain", "attribution_share") %>%
    dplyr::inner_join(m, by = "intervention_id")
  x$s <- x$attribution_share
  x$in_stock <- nipo_stage_in_stock(x, as_of, one_off_families, one_off_years)

  stock <- x %>%
    dplyr::filter(.data$in_stock) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      support_stock = sum(.data$s * .data$support),
      protection_stock = sum(.data$s * .data$protection),
      all_stock = sum(.data$s),
      support_stock_iw = sum(.data$s * .data$support * .data$strength_class),
      protection_stock_iw = sum(.data$s * .data$protection * .data$strength_class),
      all_stock_iw = sum(.data$s * .data$strength_class),
      n_measures_stock = dplyr::n_distinct(.data$intervention_id),
      n_instruments_stock = dplyr::n_distinct(.data$gta_intervention_type),
      subsidy_usd_stock = if (any(!is.na(.data$subsidy_usd))) sum(.data$subsidy_usd * .data$s, na.rm = TRUE) else NA_real_,
      trade_covered_usd_stock = if (any(!is.na(.data$trade_covered_usd))) {
        sum(.data$trade_covered_usd * .data$s, na.rm = TRUE)
      } else {
        NA_real_
      },
      .support_fiscal = sum(.data$s * .data$support * !is.na(.data$subsidy_usd)),
      .protection_trade = sum(.data$s * .data$protection * !is.na(.data$trade_covered_usd)),
      .groups = "drop"
    )

  out <- dplyr::left_join(grid, stock, by = c("iso3", "tech", "supply_chain"))
  for (w in flow_windows) {
    sfx <- paste0("_", w, "y")
    fl <- x %>%
      dplyr::filter(nipo_stage_announced_within(.data$announcement_date, as_of, w)) %>%
      dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
      dplyr::summarise(
        !!paste0("support_flow", sfx) := sum(.data$s * .data$support),
        !!paste0("protection_flow", sfx) := sum(.data$s * .data$protection),
        !!paste0("all_flow", sfx) := sum(.data$s),
        !!paste0("all_flow", sfx, "_iw") := sum(.data$s * .data$strength_class),
        !!paste0("n_new", sfx) := dplyr::n_distinct(.data$intervention_id),
        .groups = "drop"
      )
    # The instrument-weighted flow is a sensitivity of the main window only.
    if (w != main) fl[[paste0("all_flow", sfx, "_iw")]] <- NULL
    out <- dplyr::left_join(out, fl, by = c("iso3", "tech", "supply_chain"))
  }

  count_cols <- c(
    "support_stock", "protection_stock", "all_stock", "support_stock_iw", "protection_stock_iw",
    "all_stock_iw", "n_measures_stock", "n_instruments_stock",
    grep("_flow_|^n_new_", names(out), value = TRUE)
  )
  for (col in count_cols) out[[col]] <- dplyr::coalesce(as.numeric(out[[col]]), 0)
  for (col in c("n_measures_stock", "n_instruments_stock", grep("^n_new_", names(out), value = TRUE))) {
    out[[col]] <- as.integer(out[[col]])
  }

  no_stock <- out$n_measures_stock == 0
  out$subsidy_usd_stock[no_stock] <- 0
  out$trade_covered_usd_stock[no_stock] <- 0
  out$fiscal_data_coverage <- ifelse(out$support_stock > 0, out$.support_fiscal / out$support_stock, NA_real_)
  out$trade_data_coverage <- ifelse(out$protection_stock > 0, out$.protection_trade / out$protection_stock, NA_real_)

  eu_flow_col <- paste0("all_flow_", main, "y")
  eu_cols <- c("eu_supranational_stock", paste0("eu_supranational_flow_", main, "y"))
  eu <- out[out$iso3 == eu_iso3, c("tech", "supply_chain", "all_stock", eu_flow_col)]
  names(eu) <- c("tech", "supply_chain", eu_cols)
  out <- dplyr::left_join(out, eu, by = c("tech", "supply_chain"))
  member <- out$iso3 %in% eu_members_iso3
  for (col in eu_cols) out[[col]] <- ifelse(member, dplyr::coalesce(out[[col]], 0), NA_real_)

  flow_cols <- c(
    paste0("support_flow_", flow_windows, "y"),
    paste0("protection_flow_", flow_windows, "y"),
    paste0("all_flow_", flow_windows, "y"),
    paste0("all_flow_", main, "y_iw"),
    "n_measures_stock", "n_instruments_stock",
    paste0("n_new_", flow_windows, "y")
  )
  out %>%
    dplyr::select(
      "iso3", "tech", "supply_chain",
      "support_stock", "protection_stock", "all_stock",
      "support_stock_iw", "protection_stock_iw", "all_stock_iw",
      dplyr::all_of(flow_cols),
      "subsidy_usd_stock", "trade_covered_usd_stock", "fiscal_data_coverage", "trade_data_coverage",
      dplyr::all_of(eu_cols)
    ) %>%
    dplyr::arrange(.data$iso3, .data$tech, factor(.data$supply_chain, levels = SUPPLY_CHAIN_STAGES))
}

nipo_stage_share <- function(hit, of) {
  n <- sum(of)
  if (n > 0) sum(hit) / n else NA_real_
}

# One row per implementing country (the EU as `eu_iso3`): measure counts, exclusions, how the
# counted measures map (shares of counted measures) and the counted stock and flow at any
# mapping status. Stock uses the same one-off rule as the cells.
nipo_stage_country <- function(measures,
                               as_of,
                               flow_window_years = 3,
                               one_off_families = NIPO_ONE_OFF_FAMILIES,
                               one_off_years = NIPO_ONE_OFF_STOCK_YEARS) {
  as_of <- as.Date(as_of)
  measures <- measures[!is.na(measures$iso3), , drop = FALSE]
  measures$.active <- nipo_stage_in_stock(measures, as_of, one_off_families, one_off_years)
  measures %>%
    dplyr::mutate(
      .new = nipo_stage_announced_within(.data$announcement_date, as_of, flow_window_years)
    ) %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::summarise(
      n_measures = dplyr::n(),
      n_counted = sum(.data$counted),
      n_excluded_sanction = sum(.data$exclusion_reason %in% "sanction"),
      n_excluded_liberalising = sum(.data$exclusion_reason %in% "liberalising"),
      share_attributed = nipo_stage_share(.data$counted & .data$mapping_status == "attributed", .data$counted),
      share_tech_wide = nipo_stage_share(.data$counted & .data$mapping_status == "tech_wide", .data$counted),
      share_cross_cutting = nipo_stage_share(.data$counted & .data$mapping_status == "cross_cutting", .data$counted),
      share_unmapped = nipo_stage_share(.data$counted & .data$mapping_status == "unmapped", .data$counted),
      n_stock = sum(.data$counted & .data$.active),
      n_flow = sum(.data$counted & .data$.new),
      .groups = "drop"
    ) %>%
    dplyr::mutate(as_of = as_of) %>%
    dplyr::rename(!!paste0("n_flow_", flow_window_years, "y") := "n_flow") %>%
    dplyr::relocate("as_of", .after = "iso3")
}

# ---- Posture cross-check ------------------------------------------------------------------

# Per GTA Intervention Type: the family flag GTA sets on its rows, the posture that flag implies
# (NIPO_STAGE_GTA_FLAG_POSTURE) and whether the posture table agrees. A type GTA files under
# "Other" but which restricts imports, say, shows as a disagreement to review, not an error.
nipo_stage_posture_flag_check <- function(raw, posture) {
  nipo_stage_require_helpers()
  posture <- nipo_stage_prepare_posture(posture)
  flags <- intersect(names(NIPO_STAGE_GTA_FLAG_POSTURE), names(raw))
  type <- trimws(as.character(raw[["GTA Intervention Type"]]))
  flag_mat <- vapply(flags, function(f) as_bool(raw[[f]]), logical(length(type)))
  if (is.null(dim(flag_mat))) flag_mat <- matrix(flag_mat, nrow = length(type), dimnames = list(NULL, flags))
  dominant <- vapply(split(seq_along(type), type), function(i) {
    counts <- colSums(flag_mat[i, , drop = FALSE])
    if (all(counts == 0)) NA_character_ else names(counts)[which.max(counts)]
  }, character(1))
  n_rows <- vapply(split(seq_along(type), type), length, integer(1))
  tibble::tibble(gta_intervention_type = names(dominant), n_rows = unname(n_rows), gta_flag = unname(dominant)) %>%
    dplyr::left_join(posture, by = "gta_intervention_type") %>%
    dplyr::mutate(
      flag_implies = unname(NIPO_STAGE_GTA_FLAG_POSTURE[.data$gta_flag]),
      agrees = dplyr::case_when(
        is.na(.data$support) ~ NA,
        is.na(.data$flag_implies) ~ NA,
        .data$flag_implies == "support" ~ .data$support == 1L,
        .data$flag_implies == "protection" ~ .data$protection == 1L,
        .data$flag_implies == "either" ~ .data$support == 1L | .data$protection == 1L,
        .data$flag_implies == "neither" ~ .data$support == 0L & .data$protection == 0L
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n_rows))
}

# ---- Wrapper ------------------------------------------------------------------------------

# Builds the four NIPO stage tables from the raw GTA extract, the HS6 master and the posture
# table. `policy_cfg` is the policy_priority section of config/strategic_position.yml; its `nipo`
# keys (narrow_max_codes, narrow_min_basket_share, narrow_count_min_basket_share,
# exclude_sanctions, count_assessments, assessment_equivalents, one_off_stock_years,
# one_off_families, eu_wide_min_members, split_repeated_values, eu_supranational) set the rules.
# assessment_equivalents, eu_wide_min_members and split_repeated_values may also be passed as
# arguments: a non-NULL argument wins, then the config key, then the module default. The
# posture_table key is a path, which the calling script reads (this function does no IO).
# `settings$one_off_families_unknown` lists configured one-off families the posture table does
# not have (a typo there would silently keep that family on the literal stock rule).
nipo_stage_policy_outputs <- function(raw,
                                      master,
                                      posture,
                                      policy_cfg = list(),
                                      assessment_equivalents = NULL,
                                      eu_members_iso3 = NIPO_EU_MEMBERS_ISO3,
                                      former_eu_members = NIPO_FORMER_EU_MEMBERS,
                                      eu_wide_min_members = NULL,
                                      split_repeated_values = NULL,
                                      iso3_of = country_name_to_iso3) {
  or_default <- function(x, default) if (is.null(x)) default else x
  nipo_cfg <- or_default(policy_cfg$nipo, list())
  eu_mode <- or_default(nipo_cfg$eu_supranational, "report_only")
  if (!identical(eu_mode, "report_only")) {
    stop("nipo_stage_policy_outputs(): only eu_supranational = report_only is implemented, not '", eu_mode, "'.")
  }
  # yaml reads {Distortive: [Red, Amber], Liberalising: [Green]} as a named list of vectors.
  equivalents <- or_default(assessment_equivalents, or_default(nipo_cfg$assessment_equivalents, NIPO_ASSESSMENT_EQUIVALENTS))
  equivalents <- lapply(equivalents, function(v) as.character(unlist(v)))
  if (length(equivalents) > 0 && (is.null(names(equivalents)) || any(!nzchar(names(equivalents))))) {
    stop("nipo_stage_policy_outputs(): assessment_equivalents must be a named list (assessment -> Red/Amber/Green).")
  }
  one_off_years <- as.numeric(or_default(nipo_cfg$one_off_stock_years, NIPO_ONE_OFF_STOCK_YEARS))
  if (length(one_off_years) != 1 || is.na(one_off_years) || one_off_years < 0 || one_off_years != round(one_off_years)) {
    stop("nipo_stage_policy_outputs(): one_off_stock_years must be a whole number of years >= 0.")
  }
  # An empty list in the config ([]) switches the one-off window off.
  one_off_families <- as.character(unlist(or_default(nipo_cfg$one_off_families, NIPO_ONE_OFF_FAMILIES)))
  settings <- list(
    narrow_max_codes = or_default(nipo_cfg$narrow_max_codes, 20),
    narrow_min_basket_share = or_default(nipo_cfg$narrow_min_basket_share, 0.5),
    narrow_count_min_basket_share = or_default(nipo_cfg$narrow_count_min_basket_share, 0.1),
    exclude_sanctions = isTRUE(or_default(nipo_cfg$exclude_sanctions, TRUE)),
    count_assessments = unlist(or_default(nipo_cfg$count_assessments, c("Red", "Amber"))),
    assessment_equivalents = equivalents,
    one_off_stock_years = as.integer(one_off_years),
    one_off_families = one_off_families,
    one_off_families_unknown = setdiff(one_off_families, trimws(as.character(posture$family))),
    flow_window_years = as.integer(or_default(policy_cfg$flow_window_years, 3)),
    flow_windows = as.integer(unlist(or_default(policy_cfg$flow_windows_sensitivity, c(1, 3, 5)))),
    eu_supranational = eu_mode,
    eu_wide_min_members = as.integer(or_default(eu_wide_min_members, or_default(nipo_cfg$eu_wide_min_members, 20))),
    split_repeated_values = isTRUE(or_default(split_repeated_values, or_default(nipo_cfg$split_repeated_values, TRUE)))
  )

  prepared_master <- nipo_stage_prepare_master(master)
  measures <- nipo_stage_clean_measures(
    raw, posture,
    count_assessments = settings$count_assessments,
    exclude_sanctions = settings$exclude_sanctions,
    assessment_equivalents = settings$assessment_equivalents,
    eu_members_iso3 = eu_members_iso3,
    former_eu_members = former_eu_members,
    eu_wide_min_members = settings$eu_wide_min_members,
    split_repeated_values = settings$split_repeated_values,
    iso3_of = iso3_of
  )
  mapped <- nipo_stage_map_measures(
    measures, prepared_master,
    narrow_max_codes = settings$narrow_max_codes,
    narrow_min_basket_share = settings$narrow_min_basket_share,
    narrow_count_min_basket_share = settings$narrow_count_min_basket_share
  )
  rm(measures)
  as_of <- nipo_stage_resolve_as_of(mapped$measures, policy_cfg$as_of)
  settings$as_of <- as_of

  measures <- mapped$measures
  measures$stock_rule <- nipo_stage_stock_rule(measures$family, measures$removal_date, one_off_families)
  measures$active_at_as_of <- nipo_stage_in_stock(measures, as_of, one_off_families, settings$one_off_stock_years)
  measures <- dplyr::select(measures, dplyr::all_of(NIPO_STAGE_MEASURE_COLUMNS))
  cells <- nipo_stage_cells(
    measures, mapped$links, prepared_master, as_of,
    flow_windows = settings$flow_windows,
    flow_window_years = settings$flow_window_years,
    eu_members_iso3 = eu_members_iso3,
    one_off_families = one_off_families,
    one_off_years = settings$one_off_stock_years
  )
  country <- nipo_stage_country(
    measures, as_of,
    flow_window_years = settings$flow_window_years,
    one_off_families = one_off_families,
    one_off_years = settings$one_off_stock_years
  )

  list(
    measures = measures,
    links = mapped$links,
    cells = cells,
    country = country,
    as_of = as_of,
    settings = settings
  )
}
