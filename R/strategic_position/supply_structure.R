# Supply structure by country x technology x six-stage cell (docs/strategic_position.md section 7,
# "Supply structure"): who holds world supply of a stage, how much of it sits with potentially coercive
# suppliers of the assessed country, how much with the country itself and its trusted partners, and,
# for the stages whose supply is a set of minerals, which mineral is the chokepoint. These are the data
# inputs of the posture tests (R/strategic_position/posture_engine.R): the coercion leg of Test 2
# (coercive_top_share), the surge leg (trusted_world_share), Test 3 (home_share) and Test 5 (trusted
# alternatives; the domestic premium of sp_cost_premium()).
#
# Inputs: the supplier-share table of the coalition build (major_supplier_shares(),
# R/indices/coalition.R; data/processed/major_supplier_shares_tbl.rds) and the UN-voting alignment of
# scripts/12 (data/processed/geo_alignment_tbl.rds) through sp_coercive_pairs(). The import side of the
# same split (coercive_import_share, trusted_import_share) is in sp_import_exposure()
# (R/strategic_position/import_exposure.R), which takes the sp_coercive_pairs() table.
#
# Pure: no IO. Needs R/utils/supply_chain_stages.R.

# Potentially coercive powers (config/strategic_position.yml strategic_posture$coercive_powers): coercive
# to every economy outside their own UN-voting bloc, on top of the opposite bloc. Hong Kong and Macao
# carry China's bloc in geo_alignment_tbl (inherited), so they are exempt exactly where China is.
SP_COERCIVE_POWERS <- c("CHN", "HKG", "MAC", "RUS")

# Coercive and trusted suppliers of each reporter, from the geo_alignment_tbl pairs (reporter_iso,
# partner_iso, geo_alignment in {0, 0.5, 1}, bloc_reporter, bloc_partner):
#   coercive  the partner is in the opposite bloc (geo_alignment == 0; only when `opposite_bloc`), or is
#             one of `coercive_powers` and the reporter is not in that power's bloc (China is not coercive
#             to itself, to Hong Kong or to Russia; it is to India, which is Nonaligned).
#   trusted   same bloc, geo_alignment == 1, and not coercive. Nonaligned economies have geo_alignment
#             0.5 with everyone, so no partner is trusted for them: their trusted supply is their own.
#   coercive_basis  "opposite_bloc", "power", "opposite_bloc+power" or "none".
# A pair without a geo_alignment value is NA on both unless the powers rule decides it (a power outside
# the reporter's known bloc). A power whose own bloc the table does not give is coercive to every other
# economy. `opposite_bloc = FALSE` keeps the powers rule only (a sensitivity: the China-leaning bloc of
# UN voting holds Indonesia, Vietnam, Kazakhstan, Egypt, the UAE and others, which the bloc rule treats
# as coercive suppliers of every U.S.-leaning economy).
sp_coercive_pairs <- function(alignment, coercive_powers = SP_COERCIVE_POWERS, opposite_bloc = TRUE) {
  need <- c("reporter_iso", "partner_iso", "geo_alignment", "bloc_reporter", "bloc_partner")
  missing <- setdiff(need, names(alignment))
  if (length(missing) > 0) {
    stop("sp_coercive_pairs(): alignment lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  coercive_powers <- toupper(as.character(coercive_powers))
  pairs <- tibble::tibble(
    reporter_iso = toupper(as.character(alignment$reporter_iso)),
    partner_iso = toupper(as.character(alignment$partner_iso)),
    geo_alignment = as.numeric(alignment$geo_alignment),
    bloc_reporter = as.character(alignment$bloc_reporter),
    bloc_partner = as.character(alignment$bloc_partner)
  ) %>%
    dplyr::filter(!is.na(reporter_iso), !is.na(partner_iso), reporter_iso != partner_iso)
  if (anyDuplicated(pairs[c("reporter_iso", "partner_iso")]) > 0) {
    stop("sp_coercive_pairs(): alignment has more than one row per (reporter_iso, partner_iso).")
  }
  bad <- setdiff(unique(stats::na.omit(pairs$geo_alignment)), c(0, 0.5, 1))
  if (length(bad) > 0) {
    stop("sp_coercive_pairs(): geo_alignment outside {0, 0.5, 1}: ", paste(utils::head(bad, 5), collapse = ", "), ".")
  }

  # Each power's own bloc, from any row that names it (as partner or reporter).
  bloc_of <- dplyr::bind_rows(
    dplyr::select(pairs, iso3 = partner_iso, bloc = bloc_partner),
    dplyr::select(pairs, iso3 = reporter_iso, bloc = bloc_reporter)
  ) %>%
    dplyr::filter(iso3 %in% coercive_powers, !is.na(bloc), nzchar(bloc)) %>%
    dplyr::distinct(iso3, bloc)
  if (anyDuplicated(bloc_of$iso3) > 0) {
    stop("sp_coercive_pairs(): a coercive power has more than one bloc: ",
         paste(unique(bloc_of$iso3[duplicated(bloc_of$iso3)]), collapse = ", "), ".")
  }
  power_bloc <- stats::setNames(bloc_of$bloc, bloc_of$iso3)

  pairs %>%
    dplyr::mutate(
      is_power = partner_iso %in% coercive_powers,
      partner_power_bloc = unname(power_bloc[partner_iso]),
      # Exempt only where both blocs are known and equal: the powers rule is an explicit assumption,
      # so an economy of unknown bloc is not presumed to share a power's.
      same_bloc_as_power = !is.na(partner_power_bloc) & !is.na(bloc_reporter) & bloc_reporter == partner_power_bloc,
      by_power = is_power & !same_bloc_as_power,
      by_bloc = if (isTRUE(opposite_bloc)) geo_alignment == 0 else dplyr::if_else(is.na(geo_alignment), NA, FALSE),
      coercive = dplyr::case_when(by_power ~ TRUE, is.na(by_bloc) ~ NA, TRUE ~ by_bloc),
      trusted = dplyr::case_when(
        coercive %in% TRUE ~ FALSE,
        is.na(coercive) | is.na(geo_alignment) ~ NA,
        TRUE ~ geo_alignment == 1 & (is.na(bloc_reporter) | is.na(bloc_partner) | bloc_reporter == bloc_partner)
      ),
      coercive_basis = dplyr::case_when(
        is.na(coercive) ~ NA_character_,
        by_power & by_bloc %in% TRUE ~ "opposite_bloc+power",
        by_power ~ "power",
        by_bloc %in% TRUE ~ "opposite_bloc",
        TRUE ~ "none"
      )
    ) %>%
    dplyr::select(reporter_iso, partner_iso, geo_alignment, bloc_reporter, bloc_partner, coercive, trusted, coercive_basis) %>%
    dplyr::arrange(reporter_iso, partner_iso)
}

# The world-supply measure of each stage, in order of preference (the first the cell has is its
# primary_measure). Mineral measures that follow the first one in a stage's list supplement it with the
# minerals it does not list: the EI mine-production sheets carry manganese, zinc and the PGMs, which the
# IEA supply sheet leaves out (major_supplier_shares() already keeps each mineral in one of the two).
# Reserves (usgs_reserves, ei_reserves, uranium_resources) are stocks, not supply, and are not used.
# The IEA manufacturing capacity is a fallback only: it reports the EU as one bloc, so no EU member has
# a share of it (it supplies Green Hydrogen Assembly, which has no HS6 basket). An entry named
# "<tech>/<stage>" overrides the stage's list for one technology (sp_supply_select_scopes()).
SP_SUPPLY_MEASURE_PRIORITY <- list(
  Extraction = c("iea_mineral_mining", "ei_mineral_mining", "uranium_mine_production", "ei_production", "atlas_export_share"),
  Refining = c("iea_mineral_refining", "atlas_export_share"),
  Components = c("atlas_export_share", "iea_manufacturing_capacity"),
  Assembly = c("atlas_export_share", "iea_manufacturing_capacity"),
  Deployment = character(),
  Recovery = c("atlas_export_share")
)

# Measures that list every holder, so a country absent from the listing holds a true zero
# (config `structural_zero`): the Atlas export shares are each exporter over the sum of all exporters,
# and the Red Book lists every uranium producer. Every other listing names the main producers plus a
# residual ("Rest of world", "Other <region>", "Other" capacity) that major_supplier_shares() drops:
# an economy absent from it is NA (not quantified), bounded above by the residual.
SP_SUPPLY_COMPLETE_LISTINGS <- c("atlas_export_share", "uranium_mine_production")

# Labels of residual or aggregate rows that are never suppliers. major_supplier_shares() drops them
# upstream; they are dropped here too in case a table keeps them, and their share then stays in the
# listing's unlisted remainder.
SP_SUPPLY_RESIDUAL_LABEL <- "^\\s*(rest of|other\\b|others\\b|total|world)"

# World supply structure of every (tech, stage, measure[, mineral]) scope and, per assessed country, its
# own share and its coercive and trusted suppliers' shares. Returns a list of three tables:
#
# cells: one row per assessed country x tech x six-stage cell, on the stage's primary measure
#   (measure_priority; Deployment and cells without a usable measure: primary_measure "none", all NA).
#   primary_measure, mineral_measures (mineral cells: the measures the minerals come from),
#   measure_year, home_share, home_share_lo, home_share_hi, home_status, world_top_iso3,
#   world_top_share, world_hhi, coercive_top_iso3, coercive_top_share, coercive_world_share,
#   trusted_world_share, world_unlisted_share, driving_mineral, mineral_demand_share,
#   driving_home_share, n_minerals, minerals.
# measures: the same per (country, tech, stage, measure) for every measure the cell has, each on its
#   own (no supplement), so a node whose curated primary_measure differs from the default can read it.
# minerals: one row per (country, tech, stage, measure, mineral) of the mineral measures, with
#   eligible (demand share at least mineral_min_demand_share), weight (its share of the cell's demand
#   weights), in_cell (used by the cell row) and driving (the cell's driving mineral).
#
# Definitions (shares of world supply on the measure, aggregates excluded):
#   home_share          the country's share. Absent from a complete listing: 0. Absent from a partial
#                       listing: NA ("unlisted"), and home_share_lo / home_share_hi bracket it with 0
#                       and the listing's unlisted remainder (world_unlisted_share). A listed zero is 0.
#                       Mineral cells: the mean of the eligible minerals' shares weighted by
#                       mineral_demand_share (NA if any is unlisted; status "partial" if some are).
#   world_top_*         the largest listed supplier (ties: first ISO3) and its share.
#   world_hhi           sum of squared listed shares (a lower bound when the listing is partial).
#   coercive_*          over the country's coercive suppliers (sp_coercive_pairs()): the largest one
#                       and the sum. 0 when none is listed.
#   trusted_world_share the country's own share plus its trusted partners'. Over a partial listing
#                       the coercive and trusted sums are lower bounds; world_unlisted_share is the
#                       most the unlisted rest could add.
#   driving_mineral     mineral cells: the eligible mineral with the largest coercive_top_share (ties:
#                       the larger demand share, then name): the chokepoint the coercion leg tests. The
#                       cell's world_*, coercive_* and trusted_world_share are that mineral's, so the
#                       coercion and surge legs read the same chokepoint; home_share is the weighted mean
#                       (Test 3 asks about the stage, not one mineral) and driving_home_share the
#                       country's share of the driving mineral.
#   mineral_demand_share the technology's share of the mineral's clean-energy demand (IEA demand sheet
#                       3.1, share_24, as major_supplier_shares() carries it): how much of the mineral
#                       the technology takes, the weight the mineral themes already use. It is not the
#                       mineral's share of the technology's material needs.
#
# major_supplier_shares: iso3, tech, supply_chain, measure, mineral, world_share, is_aggregate, year,
#   mineral_demand_share (country optional). coercive_pairs: sp_coercive_pairs(). countries: the
#   assessed economies (default: every reporter of coercive_pairs). techs: default the table's.
sp_supply_structure <- function(major_supplier_shares,
                                coercive_pairs,
                                mineral_min_demand_share = 0.1,
                                measure_priority = SP_SUPPLY_MEASURE_PRIORITY,
                                complete_listing = SP_SUPPLY_COMPLETE_LISTINGS,
                                countries = NULL,
                                techs = NULL,
                                complete_tol = 0.01) {
  need <- c("iso3", "tech", "supply_chain", "measure", "mineral", "world_share", "is_aggregate", "year", "mineral_demand_share")
  missing <- setdiff(need, names(major_supplier_shares))
  if (length(missing) > 0) {
    stop("sp_supply_structure(): major_supplier_shares lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  missing <- setdiff(c("reporter_iso", "partner_iso", "coercive", "trusted"), names(coercive_pairs))
  if (length(missing) > 0) {
    stop("sp_supply_structure(): coercive_pairs lacks column(s) ", paste(missing, collapse = ", "), "; use sp_coercive_pairs().")
  }
  unknown_stage <- setdiff(sub("^.*/", "", names(measure_priority)), SUPPLY_CHAIN_STAGES)
  if (length(unknown_stage) > 0) stop("sp_supply_structure(): measure_priority names unknown stage(s) ", paste(unknown_stage, collapse = ", "), ".")
  if (!is.numeric(mineral_min_demand_share) || length(mineral_min_demand_share) != 1 || is.na(mineral_min_demand_share)) {
    stop("sp_supply_structure(): mineral_min_demand_share must be one number.")
  }
  assert_supply_chain_stages(major_supplier_shares$supply_chain, label = "major_supplier_shares supply_chain")

  if (is.null(countries)) countries <- sort(unique(as.character(coercive_pairs$reporter_iso)))
  countries <- unique(toupper(as.character(countries)))
  used_measures <- unique(unlist(measure_priority))
  label <- if ("country" %in% names(major_supplier_shares)) as.character(major_supplier_shares$country) else NA_character_

  shares <- tibble::tibble(
    iso3 = toupper(trimws(as.character(major_supplier_shares$iso3))),
    label = label,
    tech = as.character(major_supplier_shares$tech),
    supply_chain = as.character(major_supplier_shares$supply_chain),
    measure = as.character(major_supplier_shares$measure),
    mineral = as.character(major_supplier_shares$mineral),
    year = as.integer(major_supplier_shares$year),
    mineral_demand_share = as.numeric(major_supplier_shares$mineral_demand_share),
    world_share = as.numeric(major_supplier_shares$world_share),
    is_aggregate = as.logical(major_supplier_shares$is_aggregate)
  ) %>%
    dplyr::filter(
      measure %in% used_measures,
      !is_aggregate %in% TRUE,
      !is.na(world_share),
      !is.na(iso3), grepl("^[A-Z]{3}$", iso3),
      is.na(label) | !grepl(SP_SUPPLY_RESIDUAL_LABEL, label, ignore.case = TRUE)
    )
  if (!is.null(techs)) shares <- dplyr::filter(shares, tech %in% techs)
  if (any(shares$world_share < 0 | shares$world_share > 1 + 1e-9)) {
    stop("sp_supply_structure(): world_share outside [0, 1].")
  }
  scope_keys <- c("tech", "supply_chain", "measure", "mineral")
  if (anyDuplicated(shares[c(scope_keys, "iso3")]) > 0) {
    stop("sp_supply_structure(): more than one row per (iso3, tech, supply_chain, measure, mineral).")
  }
  if (any(!is.na(shares$mineral) & is.na(shares$mineral_demand_share))) {
    stop("sp_supply_structure(): mineral rows without mineral_demand_share.")
  }

  # World structure of each scope. A listing is complete when its measure lists every holder and its
  # listed shares do add up to the world (within complete_tol).
  scopes <- shares %>%
    dplyr::arrange(dplyr::desc(world_share), iso3) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(scope_keys))) %>%
    dplyr::summarise(
      measure_year = max(year, na.rm = FALSE),
      mineral_demand_share = max(mineral_demand_share),
      n_listed = dplyr::n(),
      listed_share = sum(world_share),
      world_hhi = sum(world_share^2),
      world_top_iso3 = dplyr::if_else(world_share[1] > 0, iso3[1], NA_character_),
      world_top_share = world_share[1],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      world_unlisted_share = pmax(0, 1 - listed_share),
      complete = measure %in% complete_listing & listed_share >= 1 - complete_tol
    )

  # Per assessed country x scope: its own share and its coercive and trusted suppliers'.
  grid <- tidyr::expand_grid(iso3 = countries, dplyr::select(scopes, dplyr::all_of(scope_keys)))
  own <- dplyr::select(shares, iso3, dplyr::all_of(scope_keys), own_share = world_share)
  cp <- tibble::tibble(
    iso3 = toupper(as.character(coercive_pairs$reporter_iso)),
    partner_iso = toupper(as.character(coercive_pairs$partner_iso)),
    coercive = as.logical(coercive_pairs$coercive),
    trusted = as.logical(coercive_pairs$trusted)
  ) %>%
    dplyr::filter(iso3 %in% countries, iso3 != partner_iso, coercive %in% TRUE | trusted %in% TRUE)
  partner <- shares %>%
    dplyr::select(partner_iso = iso3, dplyr::all_of(scope_keys), world_share) %>%
    dplyr::inner_join(cp, by = "partner_iso", relationship = "many-to-many")
  coer <- partner %>%
    dplyr::filter(coercive) %>%
    dplyr::arrange(dplyr::desc(world_share), partner_iso) %>%
    dplyr::group_by(iso3, dplyr::across(dplyr::all_of(scope_keys))) %>%
    dplyr::summarise(
      coercive_top_iso3 = dplyr::if_else(world_share[1] > 0, partner_iso[1], NA_character_),
      coercive_top_share = world_share[1],
      coercive_world_share = sum(world_share),
      .groups = "drop"
    )
  trust <- partner %>%
    dplyr::filter(trusted) %>%
    dplyr::group_by(iso3, dplyr::across(dplyr::all_of(scope_keys))) %>%
    dplyr::summarise(trusted_partner_share = sum(world_share), .groups = "drop")
  rm(partner)

  rs <- grid %>%
    dplyr::left_join(scopes, by = scope_keys) %>%
    dplyr::left_join(own, by = c("iso3", scope_keys)) %>%
    dplyr::left_join(coer, by = c("iso3", scope_keys)) %>%
    dplyr::left_join(trust, by = c("iso3", scope_keys)) %>%
    dplyr::mutate(
      home_share = dplyr::case_when(!is.na(own_share) ~ own_share, complete ~ 0, TRUE ~ NA_real_),
      home_status = dplyr::case_when(
        !is.na(own_share) & own_share > 0 ~ "listed",
        !is.na(own_share) | complete ~ "zero",
        TRUE ~ "unlisted"
      ),
      home_share_lo = dplyr::coalesce(home_share, 0),
      home_share_hi = dplyr::coalesce(home_share, world_unlisted_share),
      coercive_top_share = dplyr::coalesce(coercive_top_share, 0),
      coercive_world_share = dplyr::coalesce(coercive_world_share, 0),
      trusted_world_share = home_share_lo + dplyr::coalesce(trusted_partner_share, 0)
    ) %>%
    dplyr::select(-own_share, -trusted_partner_share, -listed_share)

  # Default scopes of each cell (world level: the same for every country).
  sel <- sp_supply_select_scopes(scopes, measure_priority, mineral_min_demand_share)

  cell_keys <- c("iso3", "tech", "supply_chain")
  cells <- rs %>%
    dplyr::inner_join(sel, by = scope_keys) %>%
    sp_supply_aggregate(group_cols = cell_keys, min_share = mineral_min_demand_share) %>%
    dplyr::left_join(dplyr::distinct(sel, tech, supply_chain, primary_measure, mineral_measures), by = c("tech", "supply_chain"))
  cell_techs <- if (is.null(techs)) sort(unique(shares$tech)) else techs
  cell_grid <- tidyr::expand_grid(iso3 = countries, tech = cell_techs, supply_chain = SUPPLY_CHAIN_STAGES)
  cells <- cell_grid %>%
    dplyr::left_join(cells, by = cell_keys) %>%
    dplyr::mutate(
      primary_measure = dplyr::coalesce(primary_measure, "none"),
      home_status = dplyr::coalesce(home_status, "no_data")
    ) %>%
    dplyr::select(dplyr::all_of(cell_keys), primary_measure, mineral_measures, dplyr::everything()) %>%
    dplyr::arrange(tech, factor(supply_chain, levels = SUPPLY_CHAIN_STAGES), iso3)

  measures <- rs %>%
    sp_supply_aggregate(group_cols = c(cell_keys, "measure"), min_share = mineral_min_demand_share) %>%
    dplyr::select(-driving_measure) %>%
    dplyr::arrange(tech, factor(supply_chain, levels = SUPPLY_CHAIN_STAGES), measure, iso3)

  # Mineral rows, flagged with their role in the cell row.
  drivers <- dplyr::filter(cells, !is.na(driving_mineral)) %>% dplyr::select(dplyr::all_of(cell_keys), driving_mineral, driving_measure)
  minerals <- rs %>%
    dplyr::filter(!is.na(mineral)) %>%
    dplyr::left_join(dplyr::mutate(sel, in_cell = TRUE) %>% dplyr::select(dplyr::all_of(scope_keys), in_cell), by = scope_keys) %>%
    dplyr::left_join(drivers, by = cell_keys) %>%
    dplyr::mutate(
      in_cell = dplyr::coalesce(in_cell, FALSE),
      eligible = mineral_demand_share >= mineral_min_demand_share,
      driving = in_cell & !is.na(driving_mineral) & mineral == driving_mineral & measure == driving_measure
    ) %>%
    dplyr::group_by(iso3, tech, supply_chain, in_cell) %>%
    dplyr::mutate(weight = dplyr::if_else(eligible & in_cell, mineral_demand_share / sum(mineral_demand_share[eligible]), 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-driving_mineral, -driving_measure, -complete) %>%
    dplyr::select(dplyr::all_of(cell_keys), measure, mineral, mineral_demand_share, eligible, in_cell, weight, driving, dplyr::everything()) %>%
    dplyr::arrange(tech, factor(supply_chain, levels = SUPPLY_CHAIN_STAGES), measure, mineral, iso3)

  cells <- dplyr::select(cells, -driving_measure)
  list(cells = cells, measures = measures, minerals = minerals)
}

# The scopes each (tech, stage) cell uses by default: the first measure of the stage's priority list
# that the cell has. An entry named "<tech>/<stage>" (e.g. "Coal/Extraction") replaces the stage's
# list for that technology; an empty one leaves the cell without a measure. A mineral measure counts
# only if one of its minerals reaches the demand floor; the mineral measures after it in the list then
# add the minerals it does not list. One row per selected scope with primary_measure and
# mineral_measures (the measures of the eligible minerals).
sp_supply_select_scopes <- function(scopes, measure_priority, min_share) {
  cells <- dplyr::distinct(scopes, tech, supply_chain)
  out <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    key <- paste0(cells$tech[i], "/", cells$supply_chain[i])
    pri <- if (key %in% names(measure_priority)) measure_priority[[key]] else measure_priority[[cells$supply_chain[i]]]
    pri <- unlist(pri)
    if (length(pri) == 0) next
    s <- scopes[scopes$tech == cells$tech[i] & scopes$supply_chain == cells$supply_chain[i] & scopes$measure %in% pri, , drop = FALSE]
    for (k in seq_along(pri)) {
      sm <- s[s$measure == pri[k], , drop = FALSE]
      if (nrow(sm) == 0) next
      if (all(is.na(sm$mineral))) {
        out[[i]] <- dplyr::mutate(sm, primary_measure = pri[k], mineral_measures = NA_character_)
        break
      }
      if (!any(sm$mineral_demand_share >= min_share)) next
      later <- pri[k:length(pri)]
      pool <- s[s$measure %in% later & !is.na(s$mineral), , drop = FALSE]
      pool <- pool[order(match(pool$measure, later), pool$mineral), , drop = FALSE]
      pool <- pool[!duplicated(pool$mineral), , drop = FALSE]
      el <- pool$measure[pool$mineral_demand_share >= min_share]
      out[[i]] <- dplyr::mutate(pool, primary_measure = pri[k], mineral_measures = paste(unique(el), collapse = "+"))
      break
    }
  }
  dplyr::bind_rows(out) %>%
    dplyr::select(tech, supply_chain, measure, mineral, primary_measure, mineral_measures)
}

# Aggregate country x scope rows to `group_cols`: a non-mineral scope passes through; mineral scopes
# below the demand floor drop out, home shares are demand-weighted, and the driving mineral (largest
# coercive_top_share) supplies the world, coercive and trusted columns.
sp_supply_aggregate <- function(rs, group_cols, min_share) {
  x <- rs %>%
    dplyr::mutate(
      is_mineral = !is.na(mineral),
      w = dplyr::if_else(is_mineral, mineral_demand_share, 1)
    ) %>%
    dplyr::filter(!is_mineral | w >= min_share)
  home <- x %>%
    dplyr::arrange(dplyr::desc(w), mineral) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_minerals = sum(is_mineral),
      minerals = if (any(is_mineral)) paste(mineral[is_mineral], collapse = ";") else NA_character_,
      n_obs = sum(!is.na(home_share)),
      n_scopes = dplyr::n(),
      home_share = if (anyNA(home_share)) NA_real_ else sum(w * home_share) / sum(w),
      home_share_lo = sum(w * home_share_lo) / sum(w),
      home_share_hi = sum(w * home_share_hi) / sum(w),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      home_status = dplyr::case_when(
        n_obs == n_scopes & home_share > 0 ~ "listed",
        n_obs == n_scopes ~ "zero",
        n_obs == 0 ~ "unlisted",
        TRUE ~ "partial"
      )
    ) %>%
    dplyr::select(-n_obs, -n_scopes)
  drive <- x %>%
    dplyr::arrange(dplyr::desc(coercive_top_share), dplyr::desc(w), mineral) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)), .keep_all = TRUE) %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(group_cols)),
      measure_year, world_top_iso3, world_top_share, world_hhi, coercive_top_iso3, coercive_top_share,
      coercive_world_share, trusted_world_share, world_unlisted_share,
      driving_mineral = mineral,
      driving_measure = dplyr::if_else(is_mineral, measure, NA_character_),
      mineral_demand_share = dplyr::if_else(is_mineral, mineral_demand_share, NA_real_),
      driving_home_share = dplyr::if_else(is_mineral, home_share, NA_real_)
    )
  home %>%
    dplyr::left_join(drive, by = group_cols) %>%
    dplyr::select(
      dplyr::all_of(group_cols), measure_year, home_share, home_share_lo, home_share_hi, home_status,
      world_top_iso3, world_top_share, world_hhi, coercive_top_iso3, coercive_top_share, coercive_world_share,
      trusted_world_share, world_unlisted_share, driving_mineral, driving_measure, mineral_demand_share,
      driving_home_share, n_minerals, minerals
    )
}

# ---- Domestic premium (Test 5) ------------------------------------------------------------------------

# IEA ETP relative production cost products that measure a headline technology's own product, and the
# cell each scores. Mirrors the headline rows of cost_competitiveness_iea_products()
# (R/categories/economic opportunity/cost_competitiveness.R; a test keeps the two in step): modules,
# turbines and electrolysers are Assembly, the battery figure is cells (Components). Heat pumps, steel,
# aluminium and ammonia are not headline-technology products and get no premium.
SP_COST_PREMIUM_PRODUCTS <- tibble::tribble(
  ~Product, ~tech, ~supply_chain,
  "Solar PV", "Solar", "Assembly",
  "Wind turbines", "Wind", "Assembly",
  "Electrolysers", "Green Hydrogen", "Assembly",
  "Batteries", "Batteries", "Components"
)

# The IEA's "Other Southeast Asia" relative-cost region: ASEAN (mirrors
# COST_COMPETITIVENESS_SOUTHEAST_ASIA_ISO3 of the cost theme).
SP_COST_SOUTHEAST_ASIA_ISO3 <- c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM")

# Region -> economies of the IEA relative-cost extract (data/raw/Relative_Costs_IEA.csv): the four
# single-economy regions, the EU (eu_iso3, e.g. EU27_ISO3 of R/categories/policy/cat_policy_index.R)
# and ASEAN. One row per (region, iso3).
sp_cost_region_map <- function(eu_iso3, southeast_asia_iso3 = SP_COST_SOUTHEAST_ASIA_ISO3) {
  dplyr::bind_rows(
    tibble::tibble(region = c("China", "United States", "Korea", "India"), iso3 = c("CHN", "USA", "KOR", "IND")),
    tibble::tibble(region = "European Union", iso3 = toupper(as.character(eu_iso3))),
    tibble::tibble(region = "Other Southeast Asia", iso3 = toupper(as.character(southeast_asia_iso3)))
  )
}

# Domestic production premium from the IEA ETP relative production costs (cost_raw: Product, Region,
# Year, Value, an index with China = 100): per product, premium_pct = 100 * (the region's cost / the
# cheapest region's cost - 1), i.e. the cost of producing at home relative to the cheapest major
# producer. Broadcast to the economies of each region (region_map: region, iso3; sp_cost_region_map()).
# One row per (iso3, tech, supply_chain) of `products`: premium_pct, premium_basis ("iea_regional"),
# cost_region, region_n_economies, region_broadcast (TRUE when the IEA value is a multi-economy region's,
# the EU or ASEAN, not the country's own), cost_index (China = 100), cheapest_region, cost_year,
# product. Economies outside the six regions (Japan, the United Kingdom, Australia, ...) get no row: their
# premium is not measured. Six regional values for one year (2023 in the staged extract): a screening
# value for Test 5, not a delivered-price estimate.
sp_cost_premium <- function(cost_raw, region_map, products = SP_COST_PREMIUM_PRODUCTS) {
  missing <- setdiff(c("Product", "Region", "Year", "Value"), names(cost_raw))
  if (length(missing) > 0) stop("sp_cost_premium(): cost_raw lacks column(s) ", paste(missing, collapse = ", "), ".")
  missing <- setdiff(c("region", "iso3"), names(region_map))
  if (length(missing) > 0) stop("sp_cost_premium(): region_map lacks column(s) ", paste(missing, collapse = ", "), ".")
  assert_supply_chain_stages(products$supply_chain, label = "sp_cost_premium() products supply_chain")
  raw <- tibble::tibble(
    product = trimws(as.character(cost_raw$Product)),
    cost_region = trimws(as.character(cost_raw$Region)),
    cost_year = suppressWarnings(as.integer(cost_raw$Year)),
    cost_index = suppressWarnings(as.numeric(cost_raw$Value))
  ) %>%
    dplyr::filter(product %in% products$Product, !is.na(cost_index))
  if (any(raw$cost_index <= 0)) stop("sp_cost_premium(): non-positive relative cost.")
  if (anyDuplicated(raw[c("product", "cost_region")]) > 0) {
    stop("sp_cost_premium(): more than one value per (Product, Region); pass one year.")
  }
  # A region the map does not name would silently lose its economies; it stops instead.
  unmapped <- setdiff(unique(raw$cost_region), region_map$region)
  if (length(unmapped) > 0) stop("sp_cost_premium(): region(s) not in region_map: ", paste(unmapped, collapse = ", "), ".")
  rm_tbl <- dplyr::distinct(tibble::tibble(cost_region = as.character(region_map$region), iso3 = toupper(as.character(region_map$iso3))))
  if (anyDuplicated(rm_tbl$iso3) > 0) stop("sp_cost_premium(): an economy is in more than one region of region_map.")
  rm_tbl <- dplyr::add_count(rm_tbl, cost_region, name = "region_n_economies")

  raw %>%
    dplyr::group_by(product) %>%
    dplyr::mutate(
      cheapest_region = cost_region[which.min(cost_index)],
      premium_pct = 100 * (cost_index / min(cost_index) - 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(dplyr::rename(products, product = Product), by = "product") %>%
    dplyr::inner_join(rm_tbl, by = "cost_region", relationship = "many-to-many") %>%
    dplyr::mutate(premium_basis = "iea_regional", region_broadcast = region_n_economies > 1) %>%
    dplyr::select(
      iso3, tech, supply_chain, premium_pct, premium_basis, cost_region, region_n_economies, region_broadcast,
      cost_index, cheapest_region, cost_year, product
    ) %>%
    dplyr::arrange(tech, supply_chain, iso3)
}

# A nonaligned reporter has no same-bloc partners, so under the bloc rule it trusts no one and no
# trusted surge or alternative supply ever exists for it (config strategic_posture$nonaligned_trust).
# With "non_coercive", such a reporter trusts every partner that is not coercive to it.
sp_nonaligned_trust <- function(coercive_pairs, rule = c("non_coercive", "bloc")) {
  rule <- match.arg(rule)
  if (rule == "bloc") return(coercive_pairs)
  no_trust <- coercive_pairs %>%
    dplyr::group_by(.data$reporter_iso) %>%
    dplyr::summarise(.none = !any(.data$trusted %in% TRUE), .groups = "drop") %>%
    dplyr::filter(.data$.none) %>%
    dplyr::pull("reporter_iso")
  hit <- coercive_pairs$reporter_iso %in% no_trust
  coercive_pairs$trusted[hit] <- coercive_pairs$coercive[hit] %in% FALSE
  coercive_pairs$trust_basis <- ifelse(hit, "nonaligned_non_coercive", "bloc")
  coercive_pairs
}
