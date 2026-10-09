# Safer friendshore theme (dyadic friendshoring index).
#
# Direction convention. In every friendshore table the reporter (reporter_iso) is the country
# that sources a technology and the partner (partner_iso) is the candidate supplier:
# Friendshore(i, j, t, s) rates partner j as a supplier of (t, s) to reporter i, from i's
# reported imports from j, i's own energy-security need, j's economic opportunity (its
# capability as a producer), i's outward direct-investment position in j (positions with a
# conduit financial centre excluded) and whether i and j sit in the same UN General Assembly
# voting bloc (geo_alignment), less a climate penalty. The
# Opportunity theme uses the other trade flow (reporter = exporter, partner = market;
# prosperous_opportunity.R), so FS(i, j) and OP(i, j) both rate j from i's side. The tables are
# not transposed here: a consumer that reads an edge as producer -> consumer must use FS(j, i)
# for the producer j supplying consumer i.
#
# Universe: reporters are the countries with an ES or EO record, partners those with an EO
# record, both collapsed to one row per ISO3; self-dyads are excluded. The trade component reads
# the reconciled flow matrix of partnership_strength_build_trade_cells()
# (partnership_strength_helpers.R): the reporter's own import declaration where it reports the
# cell, else the partner's export declaration (mirror, put on a CIF basis). A dyad cell with no
# observed flow in the last theme year takes the (tech, stage) mean (trade_status "imputed"); one
# observed with no trade is a recorded zero and ranks below every supplier of the reporter.
#
# geo_alignment (R/geopolitical_distance/alignment.R, written by scripts/12 as
# data/processed/geo_alignment_tbl.rds) is the same-bloc probability of i and j on the US-China
# axis of UNGA voting (Bailey-Strezhnev-Voeten ideal points over the latest sessions; blocs after
# Gopinath et al. 2024): A_ij = p_i p_j + (1 - p_i)(1 - p_j), p the US-side membership. With the
# hard blocs of the October 2026 build (tau = 0) p is 1, 0.5 or 0 and A is 1 for the same bloc,
# 0 for opposite blocs and 0.5 when either country is nonaligned; config/geopolitical_distance.yml
# keeps tau for smoothed sensitivity runs. It is symmetric and used as it is, not ranked within the
# reporter like imp_trade_index and outbound_index: it has a probability reading and many ties.
# It is the same for every (tech, stage) of a dyad. Economies without a UN vote are assigned by
# scripts/12 (territories and SARs take their sovereign's bloc, Taiwan, Palestine and Kosovo a
# neutral 0.5; see the alignment_basis / imputed_from columns of the alignment tables). A dyad the
# table does not cover has NA, and the component drops out of the weighted mean; it is never
# scored 0.
#
# Each dyad cell also carries imputed_weight_share, the share of its partner- and pair-specific
# component weight (trade, eo_partner, outbound, geo_alignment) resting on imputed or missing
# inputs (partnership_strength_imputed_weight_share(), partnership_strength_helpers.R); the top-5
# table carries the dyad's mean over its top-5 cells on the Friendshore Index rows, which the PSI
# rankability rule reads (scripts/25).

# The Friendshore components, in table order; each has a weight in
# config/weights.yml partnership_friendshore_components.
partnership_strength_friendshore_components <- function() {
  c("imp_trade_index", "econ_opp_raw", "es_need", "eo_partner", "outbound_index", "geo_alignment")
}

# Code defaults; the pipeline passes config/weights.yml partnership_friendshore_components.
partnership_strength_friendshore_default_weights <- function() {
  list(
    imp_trade_index = 1,
    econ_opp_raw = 1,
    es_need = 2,
    eo_partner = 1.5,
    outbound_index = 1.5,
    geo_alignment = 2
  )
}

# The dyadic alignment table reduced to the join keys, the component and geo_imputed, checked:
# one row per (reporter_iso, partner_iso), values in [0, 1] or NA. geo_imputed is TRUE where
# either country's bloc is assigned rather than read from its own UN votes (basis_reporter /
# basis_partner other than "own_votes": territories inherit their sovereign's, Taiwan, Palestine
# and Kosovo are neutral), NA when the table has no basis columns. NULL gives an empty table, so
# every dyad's geo_alignment is NA and the component drops out (the Friendshore of a build
# without it).
partnership_strength_prepare_geo_alignment <- function(geo_alignment) {
  if (is.null(geo_alignment)) {
    return(tibble::tibble(
      reporter_iso = character(), partner_iso = character(), geo_alignment = numeric(), geo_imputed = logical()
    ))
  }
  require_columns(geo_alignment, c("reporter_iso", "partner_iso", "geo_alignment"), label = "geo_alignment")
  geo_imputed <- if (all(c("basis_reporter", "basis_partner") %in% names(geo_alignment))) {
    as.character(geo_alignment$basis_reporter) != "own_votes" | as.character(geo_alignment$basis_partner) != "own_votes"
  } else {
    rep(NA, nrow(geo_alignment))
  }
  out <- tibble::tibble(
    reporter_iso = as.character(geo_alignment$reporter_iso),
    partner_iso = as.character(geo_alignment$partner_iso),
    geo_alignment = suppressWarnings(as.numeric(geo_alignment$geo_alignment)),
    geo_imputed = as.logical(geo_imputed)
  )
  out <- out[!is.na(out$reporter_iso) & !is.na(out$partner_iso) & out$reporter_iso != out$partner_iso, ]
  partnership_strength_assert_unique(out, c("reporter_iso", "partner_iso"), label = "geo_alignment")
  if (any(out$geo_alignment < 0 | out$geo_alignment > 1, na.rm = TRUE)) {
    stop("geo_alignment values must lie in [0, 1].")
  }
  out
}

# Reporter i's imports from partner j: the two-stage trade index of
# partnership_strength_two_stage_trade_index() (level 2 : growth 1 within (tech, stage), then
# ranked within the reporter; recorded zeros below every supplier). Returns reporter_iso,
# partner_iso, tech, supply_chain, imp_trade_index and trade_status.
partnership_strength_build_import_indices <- function(ds_import) {
  partnership_strength_two_stage_trade_index(ds_import, "imp_trade_index")
}

# Conduit financial centres for the outbound-investment component. Their direct-investment
# positions are dominated by pass-through capital in special purpose entities and holding
# companies, not investment in the local economy, so a DIP position with one of them says little
# about the reporter's real economic tie: before this list was extended the Netherlands and
# Singapore were the USA's 2nd and 3rd Friendshore partners (build of 2026-10-01), and its top two
# outbound partners, on the strength of their outbound positions.
#
# Source: Damgaard, Elkjaer and Johannesen, "The Rise of Phantom Investments", IMF Finance &
# Development 56(3), September 2019, summarizing their paper "What Is Real and What Is Not in the
# Global FDI Network?" (IMF Working Paper WP/19/274, December 2019; Journal of International Money
# and Finance 140, 2024, 102971, doi:10.1016/j.jimonfin.2023.102971). They separate phantom FDI
# (positions in corporate shells with no real link to the local economy, about 40 percent of
# global FDI in 2017) from real FDI with OECD and IMF CDIS data. Luxembourg and the Netherlands
# host nearly half of the phantom FDI, and ten economies (those two, Hong Kong SAR, the British
# Virgin Islands, Bermuda, Singapore, the Cayman Islands, Switzerland, Ireland and Mauritius) host
# more than 85 percent of it (dej_top10 below). The six other rows are the offshore centres this
# component already excluded as counterparts (previous_exclusion), kept on the same footing.
partnership_strength_conduit_centres <- function() {
  tibble::tribble(
    ~iso3, ~name, ~dej_top10, ~previous_exclusion,
    "NLD", "Netherlands", TRUE, FALSE,
    "LUX", "Luxembourg", TRUE, TRUE,
    "HKG", "Hong Kong SAR, China", TRUE, FALSE,
    "VGB", "British Virgin Islands", TRUE, TRUE,
    "BMU", "Bermuda", TRUE, TRUE,
    "SGP", "Singapore", TRUE, FALSE,
    "CYM", "Cayman Islands", TRUE, TRUE,
    "CHE", "Switzerland", TRUE, FALSE,
    "IRL", "Ireland", TRUE, TRUE,
    "MUS", "Mauritius", TRUE, TRUE,
    "BHS", "Bahamas", FALSE, TRUE,
    "BRB", "Barbados", FALSE, TRUE,
    "TCA", "Turks and Caicos Islands", FALSE, TRUE,
    "IMN", "Isle of Man", FALSE, TRUE,
    "GGY", "Guernsey", FALSE, TRUE,
    "CYP", "Cyprus", FALSE, TRUE
  )
}

# The conduit rule as configured (config/partnership.yml outbound_fdi): "conduits" (default)
# treats every position with a conduit centre as unobserved, on the side(s) given by `scope`;
# "legacy" reproduces the rule before October 2026 (the 12 previous_exclusion centres, as
# counterparts only) for sensitivity runs. Returns list(iso3, scope).
partnership_strength_conduit_rule <- function(rule = c("conduits", "legacy"), scope = c("both", "counterpart")) {
  rule <- match.arg(rule)
  scope <- match.arg(scope)
  centres <- partnership_strength_conduit_centres()
  if (identical(rule, "legacy")) {
    return(list(iso3 = centres$iso3[centres$previous_exclusion], scope = "counterpart"))
  }
  list(iso3 = centres$iso3, scope = scope)
}

# Reporter i's outward direct-investment (debt instrument) position in partner j, from IMF
# Direct Investment Positions (DIP). One row per (reporter_iso, partner_iso) with a reported
# position; dyads without one are absent (the friendshore composite then drops the component
# rather than scoring it 0).
#
# - The position is a stock, so it is the mean of the reported annual positions over `years`
#   (all year columns when NULL). Summing the stock over 16 years double-counted it and grew with
#   the length of a reporter's history.
# - A row with no reported year is missing. DIP reporters file explicit zeros (about half of the
#   outward-debt rows), so a reported zero is a real zero; an absent row is not evidence of one.
# - outbound_index = within-reporter S-curve of the mean of three S-curved measures: the position,
#   the position relative to partner GDP, and the position relative to both GDPs. A measure that
#   needs a missing GDP (e.g. Taiwan) drops out of that mean. A zero (or negative) position
#   enters that ranking as one tie block below every positive position, so it takes the block's
#   mid-rank like any tied value in the index: 0 where it is the reporter's only zero, ~0.5 where
#   the reporter files zeros for nearly every counterpart.
# - Conduit financial centres (partnership_strength_conduit_centres()): a position with one of
#   them is treated as unobserved, not as an outbound link: the row is dropped before the
#   ranking, so the dyad's outbound_index is NA and the Friendshore mean renormalizes over its
#   other components. `conduit_scope` "both" (default) drops positions where the centre is the
#   counterpart or the reporter (a conduit's own outward positions are the other side of the
#   same pass-through); "counterpart" drops only positions held in a centre.
partnership_strength_build_outbound_edges <- function(fdi_raw,
                                                      country_info,
                                                      gdp_data,
                                                      gdp_year = 2023,
                                                      years = NULL,
                                                      conduit_iso = partnership_strength_conduit_centres()$iso3,
                                                      conduit_scope = c("both", "counterpart")) {
  conduit_scope <- match.arg(conduit_scope)
  conduit_iso <- as.character(conduit_iso)
  require_columns(
    fdi_raw,
    c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"),
    label = "fdi_raw"
  )
  require_columns(gdp_data, c("iso3c", "year", "NY.GDP.MKTP.CD"), label = "gdp_data")

  # Year columns are "X2009" from read.csv() and "2009" from fread().
  year_cols <- grep("^X?[0-9]{4}$", names(fdi_raw), value = TRUE)
  if (!is.null(years)) {
    year_cols <- year_cols[as.integer(sub("^X", "", year_cols)) %in% years]
  }
  if (length(year_cols) == 0) {
    stop("fdi_raw has no year columns for the requested years.")
  }

  # Regional aggregates and unallocated positions are not counterpart countries.
  aggregates <- c(
    "World",
    "Not Specified (including Confidential)",
    "Europe",
    "North and Central America",
    "Oceania and Polar Regions",
    "Other Near and Middle East Economies",
    "Economies of Persian Gulf",
    "Central and South Asia",
    "East Asia",
    "North Africa",
    "North Atlantic and Caribbean",
    "South America",
    "Sub-Saharan Africa",
    "US Pacific Islands"
  )
  fdi <- fdi_raw %>%
    dplyr::filter(
      DV_TYPE == "Reported official data",
      INDICATOR == "Outward Direct investment, Assets (gross), Debt instruments, All entities"
    )

  positions <- vapply(
    year_cols,
    function(col) suppressWarnings(as.numeric(fdi[[col]])),
    numeric(nrow(fdi))
  )
  positions <- matrix(positions, nrow = nrow(fdi))
  n_reported <- rowSums(!is.na(positions))
  total <- ifelse(n_reported > 0, rowSums(positions, na.rm = TRUE) / pmax(n_reported, 1), NA_real_)

  # IMF names are matched through the shared name -> ISO3 mapping (WDI names plus countrycode);
  # the former exact join on WDI names dropped ~100 IMF spellings, among them China, the
  # Netherlands, Poland and Tuerkiye, which then scored 0 for every reporter.
  imf_names <- unique(c(fdi$COUNTRY, fdi$COUNTERPART_COUNTRY))
  imf_clean <- partnership_strength_clean_outbound_countries(imf_names)
  imf_iso <- partnership_strength_country_to_iso(imf_clean, country_info)
  imf_iso[imf_clean %in% aggregates] <- NA_character_

  gdp_latest <- gdp_data %>%
    dplyr::filter(as.character(year) == as.character(gdp_year)) %>%
    dplyr::transmute(iso3c = as.character(iso3c), GDP = suppressWarnings(as.numeric(NY.GDP.MKTP.CD))) %>%
    dplyr::filter(!is.na(GDP), GDP > 0) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  tibble::tibble(
    reporter_iso = imf_iso[match(fdi$COUNTRY, imf_names)],
    partner_iso = imf_iso[match(fdi$COUNTERPART_COUNTRY, imf_names)],
    total = total
  ) %>%
    dplyr::filter(
      !is.na(reporter_iso),
      !is.na(partner_iso),
      !is.na(total),
      reporter_iso != partner_iso,
      # Matched on ISO3, so IMF spellings cannot slip through.
      !partner_iso %in% conduit_iso,
      conduit_scope == "counterpart" | !reporter_iso %in% conduit_iso
    ) %>%
    # Two IMF spellings of one economy would otherwise give a dyad two positions.
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarise(total = mean(total), .groups = "drop") %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_reporter = GDP), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_partner = GDP), by = c("partner_iso" = "iso3c")) %>%
    dplyr::mutate(
      GDP_cp = GDP_partner / 1e6,
      GDP = GDP_reporter / 1e6,
      total_counterpart_share = total / GDP_cp,
      outbound_relative = (total / GDP) / GDP_cp
    ) %>%
    dplyr::mutate(
      total_index = partnership_strength_safe_scurve(total),
      total_share_index = partnership_strength_safe_scurve(total_counterpart_share),
      outbound_relative_index = partnership_strength_safe_scurve(outbound_relative),
      # Mean of the three measures. This read mean(total_index:outbound_relative_index), R's
      # sequence operator on two scalars, which returned total_index alone.
      outbound_index = partnership_strength_row_weighted_mean(
        list(total_index, total_share_index, outbound_relative_index),
        c(1, 1, 1)
      ),
      # Zero and negative positions are one tie block below every positive one. Their three-measure
      # means differed slightly (a measure dropped for a missing GDP is ranked over fewer rows),
      # which split identical zeros into sub-blocks: Algeria's zeros scored 0.46 or 0.76.
      outbound_index = dplyr::if_else(total <= 0, 0, outbound_index)
    ) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(outbound_index = partnership_strength_safe_scurve(outbound_index)) %>%
    dplyr::ungroup() %>%
    dplyr::select(reporter_iso, partner_iso, outbound_index)
}

partnership_strength_build_friendshore_dyads <- function(import_indices,
                                                         econ_opp_index,
                                                         energy_security_index,
                                                         outbound_edges,
                                                         tech_ghg,
                                                         policy,
                                                         country_info,
                                                         component_weights = NULL,
                                                         techs = NULL,
                                                         geo_alignment = NULL) {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_friendshore_default_weights(),
    "Friendshore component"
  )
  geo_alignment <- partnership_strength_prepare_geo_alignment(geo_alignment)

  # One value per (ISO3, tech, stage) before any ranking; the ES/EO tables file some economies
  # under several spellings (partnership_strength_country_iso_table()).
  es_cells <- partnership_strength_country_iso_table(
    energy_security_index,
    "Energy_Security_Index",
    country_info,
    techs = techs
  )
  eo_cells <- partnership_strength_country_iso_table(
    econ_opp_index,
    "Economic_Opportunity_Index",
    country_info,
    techs = techs
  )

  # Reporter need: 1 - S-curve of the reporter's ES across its own (tech, stage) cells, i.e. which
  # of i's technologies are least secure relative to i's others (not relative to other countries).
  es_iso <- es_cells %>%
    dplyr::group_by(iso3c) %>%
    dplyr::mutate(value = median_scurve(Energy_Security_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(reporter_iso = iso3c, tech, supply_chain, es_need = 1 - value)

  eo_partner_iso <- eo_cells %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(Economic_Opportunity_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, eo_partner = value)
  # The partner's EO cell imputed share, for the imputed weight share below (NULL when the EO
  # table predates the rankability rule).
  eo_partner_share <- partnership_strength_pillar_imputed_share(
    econ_opp_index, country_info, "eo_partner_imputed", techs = techs
  )

  econ_opp_iso <- eo_cells %>%
    dplyr::group_by(iso3c) %>%
    dplyr::mutate(value = median_scurve(Economic_Opportunity_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(exporter_iso = iso3c, tech, supply_chain, econ_opp_raw = value)

  ghg_iso <- tech_ghg %>%
    dplyr::transmute(tech, ghg_index = as.numeric(ghg_index))

  policy_iso <- policy %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    dplyr::transmute(iso3c, climate_policy_index) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarize(climate_policy_index = mean(climate_policy_index, na.rm = TRUE), .groups = "drop")

  default_ghg <- if (nrow(ghg_iso)) median(ghg_iso$ghg_index, na.rm = TRUE) else 0.5
  default_policy <- if (nrow(policy_iso)) median(policy_iso$climate_policy_index, na.rm = TRUE) else 0.5

  reporter_keys <- dplyr::bind_rows(
    es_iso %>% dplyr::distinct(reporter_iso, tech, supply_chain),
    econ_opp_iso %>% dplyr::distinct(reporter_iso = exporter_iso, tech, supply_chain)
  ) %>%
    dplyr::distinct(reporter_iso, tech, supply_chain)
  partner_keys <- eo_partner_iso %>%
    dplyr::distinct(partner_iso, tech, supply_chain)

  dyad_backbone <- reporter_keys %>%
    dplyr::inner_join(partner_keys, by = c("tech", "supply_chain"), relationship = "many-to-many") %>%
    # A country is not its own friendshore partner; keep self-dyads out of every ranking.
    dplyr::filter(reporter_iso != partner_iso)

  import_indices <- partnership_strength_with_trade_status(import_indices, "imp_trade_index")

  out <- dyad_backbone %>%
    dplyr::left_join(import_indices, by = c("reporter_iso", "partner_iso", "tech", "supply_chain")) %>%
    # A dyad cell without an observed flow (neither the importer nor, as a mirror, the exporter
    # reports the cell in the last theme year) takes the (tech, stage) mean below. A (tech, stage)
    # with no observed flow at all (the HS6 crosswalk maps no code to it) is "no_data": every dyad
    # gets the constant 0.5.
    dplyr::mutate(trade_status = dplyr::if_else(is.na(imp_trade_index), "imputed", trade_status)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      trade_status = if (all(trade_status %in% "imputed")) rep("no_data", dplyr::n()) else trade_status,
      imp_trade_index = dplyr::coalesce(
        imp_trade_index,
        dplyr::if_else(
          is.nan(mean(imp_trade_index, na.rm = TRUE)),
          0.5,
          mean(imp_trade_index, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(es_iso, by = c("tech", "supply_chain", "reporter_iso")) %>%
    dplyr::left_join(econ_opp_iso, by = c("reporter_iso" = "exporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(eo_partner_iso, by = c("tech", "supply_chain", "partner_iso")) %>%
    partnership_strength_join_pillar_share(eo_partner_share, "partner_iso", "eo_partner_imputed") %>%
    dplyr::left_join(outbound_edges, by = c("reporter_iso", "partner_iso")) %>%
    # NA where either country has no UN ideal point (or no table was given): dropped below.
    dplyr::left_join(geo_alignment, by = c("reporter_iso", "partner_iso")) %>%
    dplyr::left_join(dplyr::rename(policy_iso, reporter_iso = iso3c, cpi_r = climate_policy_index), by = "reporter_iso") %>%
    dplyr::left_join(dplyr::rename(policy_iso, partner_iso = iso3c, cpi_p = climate_policy_index), by = "partner_iso") %>%
    dplyr::left_join(ghg_iso, by = "tech") %>%
    dplyr::mutate(
      cpi_r = dplyr::coalesce(cpi_r, default_policy),
      cpi_p = dplyr::coalesce(cpi_p, default_policy),
      gh_entry = dplyr::coalesce(ghg_index, default_ghg),
      # outbound_index stays NA where the dyad has no IMF DIP position (most dyads), so the
      # weighted mean drops it and renormalizes. It used to be coalesced to 0, which scored a
      # missing record below a reported zero position (~0.3-0.4 after ranking). geo_alignment is
      # dropped the same way where it is NA.
      fsi_raw = partnership_strength_row_weighted_mean(
        list(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index, geo_alignment),
        c(
          component_weights$imp_trade_index,
          component_weights$econ_opp_raw,
          component_weights$es_need,
          component_weights$eo_partner,
          component_weights$outbound_index,
          component_weights$geo_alignment
        )
      ),
      penalty_r = (1 - gh_entry) * cpi_r * 0.10,
      penalty_p = (1 - gh_entry) * cpi_p * 0.10,
      fsi_adj = pmax(0, fsi_raw - (penalty_r + penalty_p)),
      # Share of the cell's partner- and pair-specific component weight resting on imputed or
      # missing inputs (partnership_strength_imputed_weight_share(), partnership_strength_helpers.R):
      # trade imputed at the cell mean, the partner's EO cell's own imputed share, no outbound
      # position, an assigned or missing geo_alignment. The reporter's es_need and econ_opp_raw
      # are left out, as is the trade component of a no_data (tech, stage).
      imputed_weight_share = partnership_strength_imputed_weight_share(
        list(
          partnership_strength_trade_unobserved(trade_status),
          partnership_strength_pillar_unobserved(eo_partner, eo_partner_imputed),
          as.numeric(is.na(outbound_index)),
          dplyr::if_else(is.na(geo_alignment) | geo_imputed %in% TRUE, 1, 0)
        ),
        c(
          component_weights$imp_trade_index,
          component_weights$eo_partner,
          component_weights$outbound_index,
          component_weights$geo_alignment
        )
      )
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(friendshore_index = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(friendshore_index_country = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      geo_alignment,
      penalty_r,
      penalty_p,
      fsi_raw,
      fsi_adj,
      friendshore_index,
      friendshore_index_country,
      trade_status,
      imputed_weight_share
    )

  partnership_strength_assert_unique(
    out,
    c("reporter_iso", "partner_iso", "tech", "supply_chain"),
    label = "friendshore dyads"
  )
}

# Top-3 reporter roll-up by partner; not called by the pipeline (scripts/15 writes the
# mean-over-reporters roll-up from partnership_strength_build_partner_rollup() instead).
partnership_strength_build_friendshore_country <- function(friendshore_all,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2024L) {
  friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(friendshore_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "friendshore",
      variable = "Friendshore Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average of the top friendshore dyads by partner country."
    )
}

partnership_strength_build_friendshore_inputs_country <- function(friendshore_all,
                                                                  country_info,
                                                                  component_weights,
                                                                  top_n = 3,
                                                                  year = 2024L) {
  component_keys <- partnership_strength_friendshore_components()
  component_weights_tbl <- tibble::tibble(
    component_key = component_keys,
    component_weight = vapply(component_keys, function(k) as.numeric(component_weights[[k]]), numeric(1), USE.NAMES = FALSE)
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  top_dyads <- friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(partner_iso, reporter_iso, tech, supply_chain) %>%
    dplyr::distinct()

  if (nrow(top_dyads) == 0) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      category = character(),
      variable = character(),
      data_type = character(),
      value = numeric(),
      component_weight = numeric(),
      component_weight_share = numeric(),
      weighted_component = numeric(),
      Year = integer(),
      source = character(),
      explanation = character()
    ))
  }

  friendshore_all %>%
    dplyr::distinct(
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      geo_alignment,
      .keep_all = TRUE
    ) %>%
    dplyr::inner_join(top_dyads, by = c("partner_iso", "reporter_iso", "tech", "supply_chain")) %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::select(
      Country,
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      geo_alignment
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(component_keys),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(weighted_component = value * component_weight_share) %>%
    dplyr::group_by(
      Country,
      tech,
      supply_chain,
      component_key,
      component_weight,
      component_weight_share
    ) %>%
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      weighted_component = mean(weighted_component, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      category = "friendshore",
      variable = component_key,
      data_type = "index",
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average component contribution across top friendshore dyads."
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

partnership_strength_build_friendshore_dyads_table <- function(friendshore_all,
                                                               country_info,
                                                               component_weights,
                                                               year = 2024L) {
  component_keys <- partnership_strength_friendshore_components()
  component_weights_tbl <- tibble::tibble(
    component_key = component_keys,
    component_weight = vapply(component_keys, function(k) as.numeric(component_weights[[k]]), numeric(1), USE.NAMES = FALSE)
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  friendshore_all %>%
    dplyr::mutate(
      Reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      Partner = partnership_strength_iso_to_country(partner_iso, country_info),
      Country = Partner,
      # Weights of the components this dyad has; fsi_raw renormalizes over them, so the
      # contributions below do too and sum to fsi_raw. The negations are parenthesized: R's `!`
      # binds looser than `*` and `+`.
      weight_available = component_weights$imp_trade_index * (!is.na(imp_trade_index)) +
        component_weights$econ_opp_raw * (!is.na(econ_opp_raw)) +
        component_weights$es_need * (!is.na(es_need)) +
        component_weights$eo_partner * (!is.na(eo_partner)) +
        component_weights$outbound_index * (!is.na(outbound_index)) +
        component_weights$geo_alignment * (!is.na(geo_alignment))
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      weight_available,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      geo_alignment,
      friendshore_index
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(component_keys, "friendshore_index")),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(
      variable = dplyr::if_else(component_key == "friendshore_index", "Friendshore Index", component_key),
      category = "friendshore",
      data_type = "index",
      weighted_component = dplyr::if_else(
        !is.na(component_weight_share) & weight_available > 0,
        value * component_weight / weight_available,
        NA_real_
      ),
      Year = as.integer(year),
      source = "Author calculation",
      explanation = dplyr::if_else(
        component_key == "friendshore_index",
        "Friendshore index: partner rated as a supplier to the reporter (reporter imports from partner).",
        "Friendshore component: partner rated as a supplier to the reporter (reporter imports from partner)."
      )
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

safer_friendshore <- function(comtrade_dyads,
                              subcat,
                              econ_opp_index,
                              energy_security_index,
                              tech_ghg,
                              policy,
                              fdi_raw,
                              country_info,
                              gdp_data,
                              component_weights = NULL,
                              years = 2020:2024,
                              top_n = 3,
                              gdp_year = 2023,
                              techs = NULL,
                              geo_alignment = NULL,
                              trade_cells = NULL,
                              cif_fob_factor = PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR,
                              conduit_iso = partnership_strength_conduit_centres()$iso3,
                              conduit_scope = "both") {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_friendshore_default_weights(),
    "Friendshore component"
  )
  # geo_alignment: the dyadic table from compute_alignment_from_ideal_points() (scripts/12). Without
  # it the component is NA for every dyad and drops out of the weighted mean.
  if (is.null(geo_alignment) && isTRUE(component_weights$geo_alignment > 0)) {
    message("safer_friendshore(): no geo_alignment table; the component is left out of every dyad.")
  }

  # The reconciled flow matrix (importer's report, else the exporter's as a mirror); scripts/15
  # builds it once and passes it to both dyad themes as `trade_cells`.
  if (is.null(trade_cells)) {
    trade_cells <- partnership_strength_build_trade_cells(comtrade_dyads, subcat, years = years, cif_fob_factor = cif_fob_factor)
  }
  ds_import <- partnership_strength_trade_view(trade_cells, view = "import")
  rm(trade_cells)
  import_indices <- partnership_strength_build_import_indices(ds_import)
  rm(ds_import)

  # DIP positions averaged over the same window as the trade data.
  outbound_edges <- partnership_strength_build_outbound_edges(
    fdi_raw,
    country_info,
    gdp_data,
    gdp_year = gdp_year,
    years = years,
    conduit_iso = conduit_iso,
    conduit_scope = conduit_scope
  )

  friendshore_all <- partnership_strength_build_friendshore_dyads(
    import_indices,
    econ_opp_index,
    energy_security_index,
    outbound_edges,
    tech_ghg,
    policy,
    country_info,
    component_weights = component_weights,
    techs = techs,
    geo_alignment = geo_alignment
  )

  friendshore_dyads <- partnership_strength_build_friendshore_dyads_table(
    friendshore_all,
    country_info,
    component_weights = component_weights,
    year = max(years)
  )

  variable_tbl <- friendshore_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner
    ) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
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

  contributions_tbl <- friendshore_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner,
      data_type = "contribution",
      contribution = dplyr::coalesce(weighted_component, value)
    ) %>%
    dplyr::filter(!is.na(contribution)) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      contribution,
      Year,
      source,
      explanation
    )
  # Trade coverage of the dyad cells (observed trade, recorded zero or imputed), by (tech, stage).
  trade_coverage <- partnership_strength_trade_coverage(friendshore_all, theme = "friendshore")
  # Each dyad's imputed weight share over the cells its top-5 Friendshore Index averages; the
  # PSI's rankability rule reads it off the top-5 table.
  dyad_share <- partnership_strength_top_k_share(
    friendshore_all,
    c("reporter_iso", "partner_iso"),
    "friendshore_index",
    k = 5L
  )
  # The long tables run to ~10M rows on the full build; release the intermediates before the
  # summaries.
  rm(friendshore_all, friendshore_dyads)

  partnership_strength_assert_unique(
    variable_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable"),
    label = "partner_friendshore_tbl"
  )

  top5_tbl <- partnership_strength_build_top_k(
    variable_tbl,
    c("reporter_iso", "partner_iso"),
    k = 5L,
    year = max(years)
  ) %>%
    dplyr::mutate(
      reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      partner = partnership_strength_iso_to_country(partner_iso, country_info),
      explanation = "Mean of the dyad's five highest (tech, stage) values; partner rated as a supplier to the reporter."
    ) %>%
    partnership_strength_add_top5_share(dyad_share, c("reporter_iso", "partner_iso"), "Friendshore Index") %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      category,
      variable,
      data_type,
      value,
      imputed_weight_share,
      Year,
      source,
      explanation
    )

  country_tbl <- partnership_strength_build_partner_rollup(
    variable_tbl,
    index_variable = "Friendshore Index",
    year = max(years),
    explanation = "Mean over reporters of the dyad Friendshore Index: partner as a supplier."
  )

  list(
    variable = variable_tbl,
    contributions = contributions_tbl,
    top5 = top5_tbl,
    country = country_tbl,
    trade_coverage = trade_coverage
  )
}
