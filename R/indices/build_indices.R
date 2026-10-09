# Strategic Index and allied sector selection (pure; scripts/20_build_indices.R does the IO).
#
# SI_its = 0.2 * pol_its + 0.3 * EO_its + 0.3 * (1 - ES_its) + 0.1 * lambda_s + 0.1 * g_t
#
#   pol     within-country median S-curve of the composite Policy index (value), taken over
#           the country's in-sample sectors only
#   EO      Economic Opportunity pillar, as built (cross-country level; not re-ranked)
#   1 - ES  energy-security NEED: the Energy Security pillar is oriented higher = more
#           secure, so the SI rewards sectors where the country is least secure. The column
#           that enters the formula is `energy_security_need`; `Energy_Security_Index` keeps
#           its security meaning in the output.
#   lambda  supply-chain stage weight (Midstream 0.75 > Upstream 0.50 > Downstream 0.25)
#   g_t     technology climate weight, ghg_index (1 = lowest lifecycle GHG intensity)
#
# Sample rule: a cell enters the SI only if EO, ES and the Policy index are all observed, and
# both pillar cells are rankable. Nothing is imputed. The pipeline used to fill a missing
# pillar with the country's mean over the other technologies in the same stage (1,041 of
# 6,757 cells lacked ES in the Sep 2026 build, 51 of the 571 scored ones), which puts an
# invented pillar value into a composite that is then ranked. Rankable (the `rankable` column
# the v2 pillar builders add, flag_rankable_cells() in index_builder_core.R) means a sovereign
# economy whose pillar value rests at most max_imputed_weight_share
# (config/rankability.yml) on imputed components; a pillar table without the column (built
# before the rule) counts every scored cell as rankable. Excluded cells stay in the table
# with strategic_index = NA and the reason in `si_exclusion`. `excluded_sectors`
# (Geothermal - Midstream) is removed before the within-country S-curves, so the ranks are
# taken over the sectors that are reported: pol over the scored sectors; eo and es_need
# (chart inputs, not in the SI) over the sectors where that pillar is observed and rankable,
# so they stay defined for countries without a Policy index.

STRATEGIC_INDEX_WEIGHTS <- c(
  policy = 0.2,
  economic_opportunity = 0.3,
  energy_security_need = 0.3,
  supply_chain = 0.1,
  technology = 0.1
)

STRATEGIC_SUPPLY_CHAIN_WEIGHTS <- c(Upstream = 0.50, Midstream = 0.75, Downstream = 0.25)

STRATEGIC_EXCLUDED_SECTORS <- c("Geothermal - Midstream")

# The allied coalition is derived, not listed by hand. Since Oct 2026 it has two tiers (core and
# partner) and the EU members share one network node: scripts/13_build_coalition.R writes
# coalition_tbl.rds (one row per country: iso3, country, tier core/partner/none, routes, bloc,
# gdp_usd, eu_member, network_node) and eu_stage_weights_tbl.rds. Until that table exists the
# coalition is the single-tier alignment coalition of geo_coalition_tbl.rds (member == TRUE),
# which scripts/12_build_geopolitical_alignment.R writes with derive_alignment_coalition()
# (R/geopolitical_distance/alignment.R): the U.S.-leaning UN-vote bloc with GDP at or above the
# floor in config/geopolitical_distance.yml (31 members with the five-session window, Oct 2026).
# Every consumer reads the coalition through coalition_members(): scripts/20 (the allied
# strategic selection, national: every member, EU members individually, with its tier) and
# scripts/30 (the allied network: iso3c_network, with tier and network_node passed through).
# It replaces the hand-picked 23-member list (STRATEGIC_ALLY_ISO3, removed Oct 2026; the same
# list survives only as the legacy/test default allied_network_default_iso3c() in
# R/indices/allied_network_design.R).

COALITION_MEMBER_TIERS <- c("core", "partner")

# ISO3 codes of a coalition table, upper-cased and trimmed; stops on missing, empty or
# duplicated codes rather than returning a partial coalition.
coalition_clean_iso3 <- function(iso3, label) {
  iso3 <- toupper(trimws(as.character(iso3)))
  if (anyNA(iso3) || any(!nzchar(iso3))) {
    stop(label, "$iso3 has missing or empty codes.")
  }
  if (anyDuplicated(iso3)) {
    stop(
      label, " has duplicate ISO3 codes: ",
      paste(unique(iso3[duplicated(iso3)]), collapse = ", "), "."
    )
  }
  iso3
}

# The coalition members, one row per member country, sorted by iso3: iso3, country, tier
# ("core" or "partner"), network_node (the node of the allied network the member belongs to:
# its own iso3, or a shared node such as the EU bundle), source (the table it came from) and,
# when coalition_tbl has them, routes, bloc, gdp_usd, eu_member and node_tier (the tier of the
# member's network node: the EU node's tier for EU members).
# - coalition_tbl (scripts/13_build_coalition.R, preferred): the members are the rows with tier
#   core or partner (case-insensitive); tier must be core, partner or none for every row. A member
#   whose network_node is missing or empty is its own node (network_node = iso3).
# - geo_coalition_tbl (scripts/12, fallback while coalition_tbl.rds does not exist): the rows with
#   member == TRUE, all tier "core", each its own node; a warning says so.
# Pure: the caller reads the tables (NULL for a table that does not exist). Stops when neither is
# given or the table is malformed.
coalition_members <- function(coalition_tbl, geo_coalition_tbl = NULL) {
  if (!is.null(coalition_tbl)) {
    require_columns(coalition_tbl, c("iso3", "tier", "network_node"), label = "coalition_tbl")
    iso3 <- coalition_clean_iso3(coalition_tbl$iso3, "coalition_tbl")
    tier <- tolower(trimws(as.character(coalition_tbl$tier)))
    bad_tier <- is.na(tier) | !tier %in% c(COALITION_MEMBER_TIERS, "none")
    if (any(bad_tier)) {
      stop(
        "coalition_tbl$tier must be core, partner or none for every country; found: ",
        paste(unique(ifelse(is.na(tier[bad_tier]), "NA", tier[bad_tier])), collapse = ", "),
        " (", paste(utils::head(iso3[bad_tier], 10), collapse = ", "), ")."
      )
    }
    node <- trimws(as.character(coalition_tbl$network_node))
    node <- ifelse(is.na(node) | !nzchar(node), iso3, node)
    out <- tibble::tibble(
      iso3 = iso3,
      country = if ("country" %in% names(coalition_tbl)) as.character(coalition_tbl$country) else NA_character_,
      tier = tier,
      network_node = node,
      source = "coalition_tbl"
    ) %>%
      dplyr::bind_cols(
        tibble::as_tibble(coalition_tbl) %>%
          dplyr::select(dplyr::any_of(c("routes", "bloc", "gdp_usd", "eu_member", "node_tier"))) %>%
          # a list column (e.g. several routes per country) is flattened to "a; b" so the
          # member table stays one flat row per member
          dplyr::mutate(dplyr::across(
            dplyr::where(is.list),
            ~ vapply(.x, function(v) paste(as.character(unlist(v)), collapse = "; "), character(1))
          ))
      ) %>%
      dplyr::filter(.data$tier %in% COALITION_MEMBER_TIERS)
    if (nrow(out) == 0) {
      stop("coalition_tbl has no coalition members (tier core or partner).")
    }
  } else if (!is.null(geo_coalition_tbl)) {
    iso3 <- allied_coalition_iso3(geo_coalition_tbl)
    country <- if ("country" %in% names(geo_coalition_tbl)) {
      as.character(geo_coalition_tbl$country)[
        match(iso3, toupper(trimws(as.character(geo_coalition_tbl$iso3))))
      ]
    } else {
      NA_character_
    }
    warning(
      "coalition_tbl.rds (scripts/13_build_coalition.R) is not available: falling back to the ",
      "single-tier alignment coalition of geo_coalition_tbl.rds (scripts/12, member == TRUE), ",
      length(iso3), " members, all tier 'core', each its own network node. Run ",
      "scripts/13_build_coalition.R for the two-tier coalition.",
      call. = FALSE
    )
    out <- tibble::tibble(
      iso3 = iso3,
      country = country,
      tier = "core",
      network_node = iso3,
      source = "geo_coalition_tbl"
    )
  } else {
    stop(
      "No coalition table: coalition_members() needs coalition_tbl (coalition_tbl.rds from ",
      "scripts/13_build_coalition.R) or, as a fallback, geo_coalition_tbl (geo_coalition_tbl.rds ",
      "from scripts/12_build_geopolitical_alignment.R)."
    )
  }
  out %>% dplyr::arrange(.data$iso3)
}

# ISO3 codes of the members of a geo_coalition_tbl (columns iso3 and a logical member), sorted:
# the validation behind coalition_members()'s fallback (scripts/25_build_partner_indices.R reads
# the coalition through coalition_members() too since Oct 2026). Stops on a malformed table rather
# than returning a partial coalition.
allied_coalition_iso3 <- function(geo_coalition_tbl) {
  require_columns(geo_coalition_tbl, c("iso3", "member"), label = "geo_coalition_tbl")
  member <- geo_coalition_tbl$member
  if (!is.logical(member) || anyNA(member)) {
    stop("geo_coalition_tbl$member must be TRUE/FALSE for every country (no NA).")
  }
  iso3 <- coalition_clean_iso3(geo_coalition_tbl$iso3, "geo_coalition_tbl")
  out <- sort(iso3[member])
  if (length(out) == 0) {
    stop("geo_coalition_tbl has no coalition members (member == TRUE).")
  }
  out
}

strategic_index_key_check <- function(tbl, label) {
  dup <- tbl %>%
    dplyr::count(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(
      label, " has ", nrow(dup), " duplicated Country x tech x supply_chain keys (e.g. ",
      paste(utils::head(paste(dup$Country, dup$tech, dup$supply_chain, sep = " / "), 3), collapse = "; "),
      "); joining it would duplicate strategic index rows."
    )
  }
  invisible(tbl)
}

# The key, the pillar value and its rankability (flag_rankable_cells()) as <prefix>_rankable
# and <prefix>_rank_exclusion. A table built before the rankability rule has no `rankable`
# column; every scored cell of it then counts as rankable.
strategic_pillar_rankability <- function(tbl, value_col, prefix) {
  out <- tbl %>%
    dplyr::select(dplyr::all_of(c("Country", "tech", "supply_chain", value_col)),
                  dplyr::any_of(c("rankable", "rank_exclusion")))
  if (!"rankable" %in% names(out)) {
    out$rankable <- !is.na(out[[value_col]])
  }
  if (!"rank_exclusion" %in% names(out)) {
    out$rank_exclusion <- NA_character_
  }
  out %>%
    dplyr::mutate(
      rankable = .data$rankable %in% TRUE,
      rank_exclusion = dplyr::if_else(
        !.data$rankable & is.na(.data$rank_exclusion),
        "flagged not rankable",
        .data$rank_exclusion
      )
    ) %>%
    dplyr::rename(
      !!paste0(prefix, "_rankable") := "rankable",
      !!paste0(prefix, "_rank_exclusion") := "rank_exclusion"
    )
}

build_strategic_index <- function(economic_opportunity_index,
                                  energy_security_index,
                                  policy_index,
                                  tech_ghg,
                                  technological_readiness_index = NULL,
                                  techs = NULL,
                                  weights = STRATEGIC_INDEX_WEIGHTS,
                                  supply_chain_weights = STRATEGIC_SUPPLY_CHAIN_WEIGHTS,
                                  excluded_sectors = STRATEGIC_EXCLUDED_SECTORS) {
  require_columns(
    economic_opportunity_index,
    c("Country", "tech", "supply_chain", "Economic_Opportunity_Index"),
    label = "economic_opportunity_index"
  )
  require_columns(
    energy_security_index,
    c("Country", "tech", "supply_chain", "Energy_Security_Index"),
    label = "energy_security_index"
  )
  require_columns(policy_index, c("Country", "tech", "supply_chain", "value"), label = "policy_index")
  require_columns(tech_ghg, c("tech", "ghg_index"), label = "tech_ghg")

  missing_weights <- setdiff(names(STRATEGIC_INDEX_WEIGHTS), names(weights))
  if (length(missing_weights) > 0) {
    stop("weights must name: ", paste(missing_weights, collapse = ", "))
  }

  eo <- strategic_pillar_rankability(economic_opportunity_index, "Economic_Opportunity_Index", "eo")
  es <- strategic_pillar_rankability(energy_security_index, "Energy_Security_Index", "es")
  pol <- policy_index %>%
    dplyr::select("Country", "tech", "supply_chain", "value")
  if (!is.null(techs)) {
    eo <- eo %>% dplyr::filter(.data$tech %in% techs)
    es <- es %>% dplyr::filter(.data$tech %in% techs)
    pol <- pol %>% dplyr::filter(.data$tech %in% techs)
  }
  strategic_index_key_check(eo, "economic_opportunity_index")
  strategic_index_key_check(es, "energy_security_index")
  strategic_index_key_check(pol, "policy_index")

  ghg <- tech_ghg %>%
    dplyr::distinct(.data$tech, .keep_all = TRUE) %>%
    dplyr::select("tech", "ghg_index")

  base <- eo %>%
    dplyr::left_join(es, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(pol, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(ghg, by = "tech")

  if (!is.null(technological_readiness_index)) {
    trl <- technological_readiness_index %>%
      dplyr::select("Country", "tech", "supply_chain", "trl_index")
    strategic_index_key_check(trl, "technological_readiness_index")
    base <- base %>% dplyr::left_join(trl, by = c("Country", "tech", "supply_chain"))
  } else {
    base$trl_index <- NA_real_
  }

  policy_countries <- unique(pol$Country[!is.na(pol$value)])

  base %>%
    dplyr::mutate(sector = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
    dplyr::filter(!.data$sector %in% excluded_sectors) %>%
    dplyr::mutate(
      energy_security_need = 1 - .data$Energy_Security_Index,
      sc_weight = unname(supply_chain_weights[.data$supply_chain]),
      # A fixed technology attribute, not a country measurement; the global mean only fills a
      # technology absent from tech_ghg (none of the 11 headline technologies is).
      tech_weight = dplyr::coalesce(.data$ghg_index, mean(ghg$ghg_index, na.rm = TRUE)),
      si_exclusion = paste_reasons(
        dplyr::if_else(!.data$Country %in% policy_countries, "country not policy-eligible", NA_character_),
        dplyr::if_else(.data$Country %in% policy_countries & is.na(.data$value), "missing policy index", NA_character_),
        dplyr::if_else(is.na(.data$Economic_Opportunity_Index), "missing economic opportunity", NA_character_),
        dplyr::if_else(is.na(.data$Energy_Security_Index), "missing energy security", NA_character_),
        dplyr::if_else(
          !is.na(.data$Economic_Opportunity_Index) & !(.data$eo_rankable %in% TRUE),
          paste0("economic opportunity not rankable (", .data$eo_rank_exclusion, ")"),
          NA_character_
        ),
        dplyr::if_else(
          !is.na(.data$Energy_Security_Index) & !(.data$es_rankable %in% TRUE),
          paste0("energy security not rankable (", .data$es_rank_exclusion, ")"),
          NA_character_
        ),
        dplyr::if_else(is.na(.data$sc_weight), "unknown supply chain stage", NA_character_)
      ),
      in_sample = is.na(.data$si_exclusion)
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(
      # pol, the only S-curve in the SI, is ranked over the scored sectors. eo and es_need
      # feed the charts' EO vs ES-risk views (package_selection_viz.R, the megatable's
      # strategic path) and do not depend on the Policy index, so they are ranked over the
      # sectors where that pillar is observed and rankable.
      pol = scurve_in_sample(.data$value, .data$in_sample),
      eo = scurve_in_sample(.data$Economic_Opportunity_Index, .data$eo_rankable %in% TRUE),
      es_need = scurve_in_sample(.data$energy_security_need, .data$es_rankable %in% TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Deprecated alias of es_need, kept so R/charts consumers that read `es` as the
      # S-curved energy-security risk keep working; use es_need.
      es = .data$es_need,
      strategic_index = dplyr::if_else(
        .data$in_sample,
        weights[["policy"]] * .data$pol +
          weights[["economic_opportunity"]] * .data$Economic_Opportunity_Index +
          weights[["energy_security_need"]] * .data$energy_security_need +
          weights[["supply_chain"]] * .data$sc_weight +
          weights[["technology"]] * .data$tech_weight,
        NA_real_
      )
    ) %>%
    dplyr::select(
      "Country", "tech", "supply_chain", "sector",
      "Economic_Opportunity_Index", "Energy_Security_Index", "energy_security_need",
      "value", "ghg_index", "trl_index",
      "eo", "es_need", "es", "pol",
      "sc_weight", "tech_weight",
      "strategic_index", "si_exclusion"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index))
}

# Weighted contribution of each SI term (they sum to strategic_index), for decomposition
# charts and tables.
strategic_index_contributions <- function(strategic_index, weights = STRATEGIC_INDEX_WEIGHTS) {
  require_columns(
    strategic_index,
    c("pol", "Economic_Opportunity_Index", "energy_security_need", "sc_weight", "tech_weight"),
    label = "strategic_index"
  )
  strategic_index %>%
    dplyr::mutate(
      contrib_policy = weights[["policy"]] * .data$pol,
      contrib_eo = weights[["economic_opportunity"]] * .data$Economic_Opportunity_Index,
      contrib_es_need = weights[["energy_security_need"]] * .data$energy_security_need,
      contrib_sc = weights[["supply_chain"]] * .data$sc_weight,
      contrib_ghg = weights[["technology"]] * .data$tech_weight
    )
}

# Sectors in which a pillar has no cross-country variation among the scored cells (standard
# deviation 0 over two or more countries). There the pillar term is a sector constant, not a
# measurement: in the Sep 2026 build ES was identically 0 in Nuclear - Upstream and
# Geothermal - Upstream (zero-filled cells), so energy_security_need = 1 for every country
# and Nuclear - Upstream was the top pick of 8 of the 18 allies scored. scripts/20 reports
# any sector listed here and rebuilds the index with it excluded.
strategic_index_degenerate_sectors <- function(strategic_index, tol = 1e-12) {
  require_columns(
    strategic_index,
    c("Country", "sector", "Economic_Opportunity_Index", "Energy_Security_Index", "strategic_index"),
    label = "strategic_index"
  )
  strategic_index %>%
    dplyr::filter(!is.na(.data$strategic_index)) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(.data$Country),
      es_sd = stats::sd(.data$Energy_Security_Index),
      eo_sd = stats::sd(.data$Economic_Opportunity_Index),
      es_first = .data$Energy_Security_Index[[1]],
      eo_first = .data$Economic_Opportunity_Index[[1]],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      es_constant = .data$n_countries > 1 & .data$es_sd <= tol,
      eo_constant = .data$n_countries > 1 & .data$eo_sd <= tol,
      es_value = dplyr::if_else(.data$es_constant, .data$es_first, NA_real_),
      eo_value = dplyr::if_else(.data$eo_constant, .data$eo_first, NA_real_)
    ) %>%
    dplyr::filter(.data$es_constant | .data$eo_constant) %>%
    dplyr::select(
      "sector", "n_countries", "es_constant", "es_value", "eo_constant", "eo_value",
      "es_sd", "eo_sd"
    ) %>%
    dplyr::arrange(.data$sector)
}

# Sectors in which one energy-security value is shared by at least `min_share` of the scored
# countries: a floor that is a measurement for some countries but the same number for many.
# Diagnostic only (nothing is excluded): with the need reading, a country at a low ES floor
# gets a high, sector-wide energy_security_need. With uranium in the Oct 2026 build, ES for
# Nuclear - Upstream (Production 1/2, Reserves 1/3, a global energy-price constant 1/6) is
# 0.117 for every economy without uranium production or resources: 32 of the 65 SI countries,
# and Nuclear - Upstream was then the top allied pick of 20 of 45 members. scripts/20 reports
# these sectors with the allied picks that fall in them.
strategic_index_es_floor_sectors <- function(strategic_index, min_share = 0.25, digits = 6) {
  require_columns(
    strategic_index,
    c("Country", "sector", "Energy_Security_Index", "strategic_index"),
    label = "strategic_index"
  )
  strategic_index %>%
    dplyr::filter(!is.na(.data$strategic_index)) %>%
    dplyr::mutate(es_rounded = round(.data$Energy_Security_Index, digits)) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(.data$Country),
      es_mode = {
        u <- unique(.data$es_rounded)
        u[which.max(tabulate(match(.data$es_rounded, u)))]
      },
      n_at_mode = max(tabulate(match(.data$es_rounded, unique(.data$es_rounded)))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(share_at_mode = .data$n_at_mode / .data$n_countries) %>%
    dplyr::filter(.data$n_countries > 1, .data$share_at_mode >= min_share) %>%
    dplyr::arrange(dplyr::desc(.data$share_at_mode), .data$sector)
}

# Endowment screen for the allied picks. The need reading (energy_security_need = 1 - ES) is
# highest where a country is least secure, and in an extraction stage a country without the
# resource sits at the ES floor: with uranium in the Oct 2026 build, Nuclear - Upstream was the
# top allied pick of 20 of 45 members, most with no uranium at all. The screen does not change
# the index; it decides which cells may be picked.
#
# strategic_resource_base() reads the reserves and production theme tables (reserves_tbl,
# critical_minerals_production_tbl: long tables with Country, tech, supply_chain, category,
# variable, value) and returns one row per Country x screened technology with resource_base =
# TRUE when the technology-level "<tech> Reserves" or "<tech> Production" value is positive
# (raw or index; both themes anchor the technology rollup at zero, so a positive index means a
# positive reserve or output of at least one of the technology's minerals or fuels). A
# technology is screened when any table has such a row for it; the others (none of the 11
# headline technologies today) are left unscreened. Countries absent from every table have no
# recorded resources.
strategic_resource_base <- function(..., stage = "Upstream", categories = c("Reserves", "Production")) {
  tbls <- Filter(Negate(is.null), list(...))
  if (length(tbls) == 0) {
    stop("strategic_resource_base() needs at least one reserves or production table.")
  }
  for (i in seq_along(tbls)) {
    require_columns(
      tbls[[i]],
      c("Country", "tech", "supply_chain", "category", "variable", "value"),
      label = paste0("resource table ", i)
    )
  }
  rows <- dplyr::bind_rows(lapply(tbls, function(t) {
    tibble::as_tibble(t) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        Country = as.character(.data$Country),
        tech = as.character(.data$tech),
        supply_chain = as.character(.data$supply_chain),
        category = as.character(.data$category),
        variable = as.character(.data$variable),
        value = suppressWarnings(as.numeric(.data$value))
      )
  })) %>%
    dplyr::filter(
      .data$supply_chain %in% stage,
      .data$category %in% categories,
      .data$variable == paste(.data$tech, .data$category),
      !is.na(.data$Country)
    )
  rows %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      resource_base = any(.data$value > 0, na.rm = TRUE),
      resource_evidence = {
        pos <- unique(.data$category[!is.na(.data$value) & .data$value > 0])
        if (length(pos) == 0) NA_character_ else paste(sort(pos), collapse = "; ")
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$tech, .data$Country)
}

# Add resource_base to a strategic index table: TRUE/FALSE in a screened (tech, stage), NA in
# the sectors the screen does not cover (Midstream, Downstream, unscreened technologies). A
# country missing from resource_base in a screened sector has no recorded resources (FALSE).
# pick_exclusion records "no resource base" where the screen bars the cell from the allied pick;
# strategic_index itself is unchanged.
flag_strategic_resource_base <- function(strategic_index, resource_base) {
  require_columns(strategic_index, c("Country", "tech", "supply_chain"), label = "strategic_index")
  require_columns(resource_base, c("Country", "tech", "supply_chain", "resource_base"), label = "resource_base")
  screened <- resource_base %>%
    dplyr::distinct(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(screened = TRUE)
  rb <- resource_base %>%
    dplyr::select("Country", "tech", "supply_chain", "resource_base", dplyr::any_of("resource_evidence"))
  if (anyDuplicated(rb[c("Country", "tech", "supply_chain")])) {
    stop("resource_base has duplicated Country x tech x supply_chain rows.")
  }
  out <- strategic_index %>%
    dplyr::select(-dplyr::any_of(c("resource_base", "resource_evidence", "pick_exclusion"))) %>%
    dplyr::left_join(screened, by = c("tech", "supply_chain")) %>%
    dplyr::left_join(rb, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::mutate(
      resource_base = dplyr::if_else(
        .data$screened %in% TRUE,
        dplyr::coalesce(.data$resource_base, FALSE),
        NA
      ),
      pick_exclusion = dplyr::if_else(.data$resource_base %in% FALSE, "no resource base", NA_character_)
    ) %>%
    dplyr::select(-"screened")
  if (!"resource_evidence" %in% names(out)) {
    out$resource_evidence <- NA_character_
  }
  out
}

# Top Upstream/Midstream sector per ally (ties kept), excluding Geothermal and Green Hydrogen
# (excluded_techs; this used to say the allied network does not model them, but since Oct 2026
# it models the Upstream and Midstream stages of all 11 technologies, so the exclusion is a rule
# of the selection itself). The selection is national: every coalition member is its own ally,
# EU members included (the EU bundle is a node of the allied network only). `ally_countries` is
# either a character vector of country names as spelled in strategic_index, or a data frame with
# one row per ally and a `Country` column (so spelled) whose other columns (scripts/20: iso3,
# tier, network_node from coalition_members()) are carried onto that ally's picks. With
# require_resource_base (the default) a cell flagged resource_base == FALSE by
# flag_strategic_resource_base() is not eligible: an Upstream extraction stage is picked only
# where the country has recorded reserves or production of the stage's resources.
select_allied_strategic_sectors <- function(strategic_index,
                                            ally_countries,
                                            stages = c("Midstream", "Upstream"),
                                            excluded_techs = c("Geothermal", "Green Hydrogen"),
                                            n = 1,
                                            require_resource_base = TRUE) {
  require_columns(strategic_index, c("Country", "tech", "supply_chain", "strategic_index"), label = "strategic_index")
  ally_tbl <- NULL
  if (is.data.frame(ally_countries)) {
    require_columns(ally_countries, "Country", label = "ally_countries")
    ally_tbl <- tibble::as_tibble(ally_countries)
    if (anyNA(ally_tbl$Country) || anyDuplicated(ally_tbl$Country)) {
      stop("ally_countries must have one row per Country (no NA, no duplicates).")
    }
    clash <- setdiff(intersect(names(ally_tbl), names(strategic_index)), "Country")
    if (length(clash) > 0) {
      stop("ally_countries columns also in strategic_index: ", paste(clash, collapse = ", "), ".")
    }
    ally_countries <- ally_tbl$Country
  }
  # The endowment screen (flag_strategic_resource_base()): an extraction stage without a
  # recorded resource base is never the pick. A table built without the flag is not screened.
  no_resource_base <- if (isTRUE(require_resource_base) && "resource_base" %in% names(strategic_index)) {
    strategic_index$resource_base %in% FALSE
  } else {
    rep(FALSE, nrow(strategic_index))
  }
  out <- strategic_index[!no_resource_base, , drop = FALSE] %>%
    dplyr::filter(
      .data$Country %in% ally_countries,
      .data$supply_chain %in% stages,
      !is.na(.data$strategic_index),
      !.data$tech %in% excluded_techs
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::slice_max(order_by = .data$strategic_index, n = n) %>%
    dplyr::ungroup()
  if (!is.null(ally_tbl)) {
    out <- out %>% dplyr::left_join(ally_tbl, by = "Country")
  }
  out
}

# Median S-curve over the non-NA rows flagged in_sample (TRUE: every non-NA row); NA elsewhere.
scurve_in_sample <- function(x, in_sample) {
  out <- rep(NA_real_, length(x))
  keep <- in_sample & !is.na(x)
  if (any(keep)) {
    out[keep] <- median_scurve(x[keep])
  }
  out
}

# Row-wise "; "-join of the non-NA reason strings; NA when there is none.
paste_reasons <- function(...) {
  parts <- list(...)
  out <- apply(do.call(cbind, parts), 1, function(r) {
    r <- r[!is.na(r)]
    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
  })
  as.character(out)
}
