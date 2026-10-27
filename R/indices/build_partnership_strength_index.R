# Build Partnership Strength index outputs (friendshore + opportunity + development).
#
# Level. The PSI is a partner-country score, collapsed over technology and stage and over
# reporters (scripts/25 passes tech = supply_chain = "All"):
#   C^k_j = mean over the rows for partner j of pillar k's index: for Friendshore and Opportunity
#           one row per reporter i != j of the viewpoint (below; the dyad's top-5 (tech, stage)
#           mean), for Development
#           the partner's own top-5 mean (it has no reporter);
#   PSI_j = sum_k w_k C^k_j / sum over the categories j has of w_k   (w = config/weights.yml psi).
# The top-5 means are over (tech, stage) cells keyed by the six supply-chain stages since
# 2026-10-27, so each draws on up to six cells per technology (66 over the 11 headline
# technologies) instead of three (33), with k still 5 (partnership_strength_build_top_k()): a
# technology with several strong stages can now fill more of a dyad's five cells.
# A category a partner lacks is dropped and the weights renormalized, not scored 0: Development
# is defined only for non-high-income partners, so high-income partners get
# PSI = (0.4 FS + 0.4 OP) / 0.8 rather than a score capped at 0.8. Each category uses its pillar
# index only (psi_index_variables()); the components, which include reporter attributes and a
# zero-weight term, are no longer averaged in alongside the index.
# build_partnership_strength_dyads() gives the same composite per (reporter, partner).
#
# Viewpoint. Whose ratings C^k_j averages is a choice. The headline PSI takes the allied
# coalition's viewpoint: the mean over the CORE coalition reporters (tier core of
# coalition_tbl.rds, scripts/13, read through coalition_members(); decision of 2026-10-08; a member
# is not averaged over its own self-dyad), so PSI_j is partner j's value to the coalition. The
# full-coalition view averages over the core and partner tiers together, and the world view over
# every reporter (219); both are secondary outputs. The world view mixes the ratings of the
# coalition with those of China-leaning and nonaligned reporters, whose Friendshore ratings carry
# the geo_alignment term from the other side. Development has no reporter and is the same in every
# view. build_partnership_strength_viewpoints() builds them from the same top-5 tables.
#
# Rankability. Every partner is scored; a partner is ranked only under the pillars' rule
# (config/rankability.yml): it is a sovereign state and at most a share tau of its PSI weight rests
# on imputed inputs (psi_flag_rankable()). The PSI's imputed weight share is built like its score
# (psi_imputed_weight_share()):
#   s^k_j  = mean over the same rows as C^k_j of the row's imputed_weight_share, the share of the
#            partner- and pair-specific component weight resting on imputed or missing inputs in the
#            cells its top-5 value averages (partnership_strength_imputed_weight_share(),
#            R/themes/partnership_strength/partnership_strength_helpers.R: trade cells imputed at
#            the cell mean, no outbound position, an assigned geo_alignment, missing Development
#            indicators, and the partner's ES / EO cells at their own pillar imputed share);
#   IWS_j  = sum_k w_k s^k_j / sum over the categories j has of w_k.
# rankable_j = sovereign_j AND IWS_j <= tau, in each view (the coalition view's shares average the
# coalition reporters' dyads only).

psi_index_variables <- function() {
  c(
    friendshore = "Friendshore Index",
    opportunity = "Opportunity Index",
    development = "Development Potential Index"
  )
}

if (!exists("normalize_sub_sector_or_keys", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "index_builder_core.R"))
}

standardize_partnership_strength_inputs <- function(theme_tables) {
  theme_names <- names(theme_tables)
  if (is.null(theme_names)) {
    theme_names <- rep("unknown_theme", length(theme_tables))
  }

  standardized <- Map(function(theme_name, tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }

    standardized_tbl <- standardize_theme_table(tbl)
    standardized_tbl <- standardize_bind_rows_inputs(standardized_tbl)
    validate_schema(standardized_tbl)

    standardized_tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        category = stringr::str_to_lower(as.character(category)),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  dplyr::bind_rows(standardized)
}

build_partnership_strength_index <- function(theme_tables,
                                             weights,
                                             allow_partial_categories = TRUE,
                                             index_variables = psi_index_variables()) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Partnership strength weights are missing or empty.")
  }

  psi_data <- standardize_partnership_strength_inputs(theme_tables) %>%
    dplyr::filter(data_type == "index")

  # One variable per category: the pillar index. NULL keeps every index-typed variable.
  if (!is.null(index_variables)) {
    names(index_variables) <- stringr::str_to_lower(names(index_variables))
    psi_data <- psi_data %>%
      dplyr::filter(variable == unname(index_variables[category]))
  }

  if (nrow(psi_data) == 0) {
    stop("Partnership strength inputs are empty after filtering to index values.")
  }

  if (!is.integer(psi_data$Year) || any(is.na(psi_data$Year))) {
    stop("Year must be integer and non-missing for partnership strength inputs.")
  }

  group_cols <- c("Country", "tech", "supply_chain", "category")

  category_scores <- psi_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      category_score = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      n_obs = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    # A category with no value for the partner (development for high-income partners) is
    # absent, so it takes no weight in the composite instead of counting as 0.
    dplyr::filter(!is.na(category_score))

  weights_tbl <- tibble::tibble(
    category = stringr::str_to_lower(names(weights)),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  index_outputs <- compute_index_and_contributions(
    category_scores,
    variable_contributions = category_scores %>%
      dplyr::mutate(
        variable = category,
        component_value = category_score,
        component_weight_within_overall = 1,
        missing_rule_applied = NA_character_,
        imputed = FALSE
      ) %>%
      dplyr::select(
        Country,
        tech,
        supply_chain,
        category,
        variable,
        component_value,
        component_weight_within_overall,
        missing_rule_applied,
        imputed
      ),
    weights_tbl = weights_tbl,
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = FALSE,
    pillar_label = "Partnership strength",
    index_col = "Partnership_Strength_Index"
  )

  index_tbl <- index_outputs$index_tbl %>%
    dplyr::select(-dplyr::any_of("complete_categories"))

  list(
    partnership_strength_index = index_tbl,
    category_contributions = index_outputs$category_contributions,
    variable_contributions = index_outputs$variable_contributions,
    index = index_tbl
  )
}

# PSI inputs from the top-5 summaries of scripts/15: dyad-level for Friendshore and Opportunity
# (one row per reporter, partner and variable), partner-level for Development. Country is the
# partner (its ISO3 when it has no name) and tech = supply_chain = "All": the PSI is collapsed over
# technology and stage. `reporters` (ISO3) keeps only those reporters' rows of a dyad table; NULL
# keeps every reporter. A partner-level table (no reporter_iso) passes through.
# imputed_weight_share (on the index rows of the top-5 tables) is carried along; NA when the table
# has no such column (built before the PSI rankability rule).
psi_prepare_partner_top5 <- function(tbl, reporters = NULL) {
  require_columns(tbl, c("partner_iso", "category", "variable", "data_type", "value", "Year"), label = "partner top5")
  if (!is.null(reporters) && "reporter_iso" %in% names(tbl)) {
    tbl <- tbl[as.character(tbl$reporter_iso) %in% reporters, , drop = FALSE]
  }
  partner_name <- if ("partner" %in% names(tbl)) as.character(tbl$partner) else rep(NA_character_, nrow(tbl))
  tibble::tibble(
    Country = dplyr::coalesce(partner_name, as.character(tbl$partner_iso)),
    partner_iso = as.character(tbl$partner_iso),
    tech = "All",
    supply_chain = "All",
    category = as.character(tbl$category),
    variable = as.character(tbl$variable),
    data_type = as.character(tbl$data_type),
    value = suppressWarnings(as.numeric(tbl$value)),
    imputed_weight_share = if ("imputed_weight_share" %in% names(tbl)) {
      suppressWarnings(as.numeric(tbl$imputed_weight_share))
    } else {
      rep(NA_real_, nrow(tbl))
    },
    Year = suppressWarnings(as.integer(tbl$Year)),
    source = if ("source" %in% names(tbl)) as.character(tbl$source) else NA_character_,
    explanation = if ("explanation" %in% names(tbl)) as.character(tbl$explanation) else NA_character_
  )
}

# The PSI's imputed weight share per partner (see the header), from the prepared top-5 inputs of
# one view (psi_prepare_partner_top5(); a named list friendshore / opportunity / development). Each
# category averages the rows its score averages (the index rows with a value), so s^k_j and C^k_j
# cover the same reporters; the categories are weighted as in the composite and renormalized over
# the ones the partner has. A category whose rows carry no share (inputs built before the rule)
# leaves the partner's share NA (unknown). Returns Country, imputed_weight_share and one
# <category>_imputed_share column per category.
psi_imputed_weight_share <- function(inputs, weights, index_variables = psi_index_variables()) {
  w <- stats::setNames(as.numeric(unlist(weights, use.names = FALSE)), stringr::str_to_lower(names(weights)))
  names(index_variables) <- stringr::str_to_lower(names(index_variables))
  per_category <- lapply(names(inputs), function(cat_name) {
    tbl <- inputs[[cat_name]]
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(NULL)
    }
    key <- stringr::str_to_lower(cat_name)
    tbl <- tbl[tbl$variable == index_variables[[key]] & !is.na(tbl$value), , drop = FALSE]
    if (nrow(tbl) == 0) {
      return(NULL)
    }
    tbl %>%
      dplyr::group_by(Country) %>%
      dplyr::summarise(share = mean(imputed_weight_share), .groups = "drop") %>%
      dplyr::mutate(category = key, weight = if (key %in% names(w)) w[[key]] else NA_real_)
  })
  long <- dplyr::bind_rows(per_category)
  if (nrow(long) == 0) {
    return(tibble::tibble(Country = character(), imputed_weight_share = numeric()))
  }
  long <- long[!is.na(long$weight) & long$weight > 0, , drop = FALSE]
  overall <- long %>%
    dplyr::group_by(Country) %>%
    dplyr::summarise(imputed_weight_share = sum(weight * share) / sum(weight), .groups = "drop")
  by_category <- long %>%
    dplyr::mutate(category = paste0(category, "_imputed_share")) %>%
    dplyr::select(Country, category, share) %>%
    tidyr::pivot_wider(names_from = category, values_from = share)
  dplyr::left_join(overall, by_category, by = "Country")
}

# The partner-level PSI from several viewpoints (see the header): `coalition` averages the
# Friendshore and Opportunity dyad scores over the reporters in `coalition_iso3` (the core tier in
# scripts/25), `world` over every reporter, and, when `full_coalition_iso3` is given (core and
# partner tiers), `full_coalition` over those reporters. Returns list(coalition = , world = ) plus
# full_coalition = when asked, each the build_partnership_strength_index() output for that
# viewpoint plus `reporters` (the reporters it averages over; NULL = all). Stops when no member of
# a coalition view is a reporter of both dyad tables; members missing from one are named in a
# warning.
build_partnership_strength_viewpoints <- function(friendshore_top5,
                                                  opportunity_top5,
                                                  development_top5,
                                                  weights,
                                                  coalition_iso3,
                                                  index_variables = psi_index_variables(),
                                                  full_coalition_iso3 = NULL) {
  require_columns(friendshore_top5, "reporter_iso", label = "friendshore_top5")
  require_columns(opportunity_top5, "reporter_iso", label = "opportunity_top5")
  check_members <- function(iso3, arg, view_label) {
    iso3 <- unique(toupper(trimws(as.character(iso3))))
    if (length(iso3) == 0) {
      stop(arg, " is empty.")
    }
    in_fs <- iso3 %in% friendshore_top5$reporter_iso
    in_op <- iso3 %in% opportunity_top5$reporter_iso
    if (!any(in_fs & in_op)) {
      stop("No coalition member is a reporter of both the Friendshore and Opportunity tables.")
    }
    if (!all(in_fs & in_op)) {
      warning(
        "Coalition members without Friendshore or Opportunity ratings (left out of the ", view_label, "): ",
        paste(iso3[!(in_fs & in_op)], collapse = ", "),
        call. = FALSE
      )
    }
    iso3
  }
  coalition_iso3 <- check_members(coalition_iso3, "coalition_iso3", "coalition view")
  if (!is.null(full_coalition_iso3)) {
    full_coalition_iso3 <- check_members(full_coalition_iso3, "full_coalition_iso3", "full-coalition view")
  }

  build_view <- function(reporters) {
    inputs <- list(
      friendshore = dplyr::select(psi_prepare_partner_top5(friendshore_top5, reporters), -partner_iso),
      opportunity = dplyr::select(psi_prepare_partner_top5(opportunity_top5, reporters), -partner_iso),
      development = dplyr::select(psi_prepare_partner_top5(development_top5), -partner_iso)
    )
    out <- build_partnership_strength_index(
      theme_tables = inputs,
      weights = weights,
      allow_partial_categories = TRUE,
      index_variables = index_variables
    )
    # The partner's imputed weight share in this view, for psi_flag_rankable().
    out$imputation <- psi_imputed_weight_share(inputs, weights, index_variables = index_variables)
    out$reporters <- reporters
    out
  }

  out <- list(
    coalition = build_view(coalition_iso3),
    world = build_view(NULL)
  )
  if (!is.null(full_coalition_iso3)) {
    out$full_coalition <- build_view(full_coalition_iso3)
  }
  out
}

# The PSI's imputed-weight threshold tau: the argument when given, else the `psi` entry of the
# rankability option scripts set from config/rankability.yml (opportunity_security.rankability),
# else the pillars' max_imputed_weight_share there; with none of them no threshold applies (tau =
# 1, resolve_max_imputed_weight_share()).
psi_resolve_max_imputed_weight_share <- function(max_imputed_weight_share = NULL) {
  if (is.null(max_imputed_weight_share)) {
    rule <- getOption("opportunity_security.rankability")
    max_imputed_weight_share <- if (!is.null(rule$psi$max_imputed_weight_share)) {
      rule$psi$max_imputed_weight_share
    } else {
      rule$max_imputed_weight_share
    }
  }
  resolve_max_imputed_weight_share(max_imputed_weight_share)
}

# Sovereignty of PSI partners: from the ISO3 code where there is one (Correlates of War state
# code, as is_sovereign_economy() reads it), else from the name.
psi_is_sovereign <- function(country, iso3 = NULL) {
  if (!exists("is_sovereign_economy", mode = "function")) {
    stop("The PSI rankability rule needs is_sovereign_economy() (R/indices/index_builder_core.R).")
  }
  out <- is_sovereign_economy(as.character(country))
  if (!is.null(iso3)) {
    iso3 <- toupper(trimws(as.character(iso3)))
    has_iso <- !is.na(iso3) & grepl("^[A-Z]{3}$", iso3)
    if (any(has_iso)) {
      cown <- suppressWarnings(countrycode::countrycode(
        iso3[has_iso],
        origin = "iso3c",
        destination = "cown",
        custom_match = SOVEREIGN_COW_CUSTOM_MATCH,
        warn = FALSE
      ))
      out[has_iso] <- !is.na(cown)
    }
  }
  out
}

# Rankability of the partner-level PSI on the pillars' rule (config/rankability.yml): every
# partner keeps its score, but a partner is ranked only when it is a sovereign state (Correlates of
# War state system; is_sovereign_economy(), R/indices/index_builder_core.R) and its imputed weight
# share (psi_imputed_weight_share(), `share_col`) is at most tau. Territories and SARs inherit their
# sovereign's UNGA bloc in geo_alignment (scripts/12), so in the coalition view a dependency of a
# coalition member scored like a member on that component without any foreign policy of its own;
# microstates that vote with the coalition but have few observed inputs of their own rose the same
# way, which the imputed-share condition stops. A partner whose share is unknown is not ranked.
# Shares within `tolerance` of tau count as <= tau. Adds `rankable`, `rank_exclusion` (why not;
# NA when rankable, the pillars' wording) and `rank` (1 = highest value among the rankable
# partners, ties take the lowest rank; NA when not rankable). `country_col` names the partner,
# `iso_col` (when present) its ISO3 code.
psi_flag_rankable <- function(psi_tbl,
                              country_col = "Country",
                              value_col = "value",
                              share_col = "imputed_weight_share",
                              iso_col = "partner_iso",
                              max_imputed_weight_share = NULL,
                              tolerance = 1e-9) {
  require_columns(psi_tbl, c(country_col, value_col, share_col), label = "psi_tbl")
  tau <- psi_resolve_max_imputed_weight_share(max_imputed_weight_share)
  value <- suppressWarnings(as.numeric(psi_tbl[[value_col]]))
  share <- suppressWarnings(as.numeric(psi_tbl[[share_col]]))
  sovereign <- psi_is_sovereign(
    psi_tbl[[country_col]],
    if (!is.null(iso_col) && iso_col %in% names(psi_tbl)) psi_tbl[[iso_col]] else NULL
  )
  scored <- !is.na(value)
  share_known <- !is.na(share)
  over <- share_known & share > tau + tolerance
  reason <- dplyr::case_when(
    !scored ~ "not scored",
    !sovereign & over ~ paste0("not a sovereign economy and imputed weight share above ", format(tau)),
    !sovereign ~ "not a sovereign economy",
    !share_known ~ "imputed weight share unknown",
    over ~ paste0("imputed weight share above ", format(tau)),
    TRUE ~ NA_character_
  )
  rankable <- is.na(reason)
  rank <- rep(NA_integer_, nrow(psi_tbl))
  rank[rankable] <- as.integer(rank(-value[rankable], ties.method = "min"))
  psi_tbl$rankable <- rankable
  psi_tbl$rank_exclusion <- reason
  psi_tbl$rank <- rank
  psi_tbl
}

# Dyadic PSI: the same composite for each (reporter, partner) pair, from the dyad top-5
# Friendshore and Opportunity scores and the partner's top-5 Development score, averaging over
# the categories the pair has (weights renormalized). Both dyad pillars rate the partner from the
# reporter's side (Friendshore: partner as a supplier to the reporter; Opportunity: partner as a
# market for the reporter's exports), so PSI(i, j) is partner j's value to reporter i.
build_partnership_strength_dyads <- function(friendshore_top5,
                                             opportunity_top5,
                                             development_top5,
                                             weights,
                                             index_variables = psi_index_variables()) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Partnership strength weights are missing or empty.")
  }
  require_columns(friendshore_top5, c("reporter_iso", "partner_iso", "variable", "value"), label = "friendshore_top5")
  require_columns(opportunity_top5, c("reporter_iso", "partner_iso", "variable", "value"), label = "opportunity_top5")
  require_columns(development_top5, c("partner_iso", "variable", "value"), label = "development_top5")

  w <- stats::setNames(as.numeric(unlist(weights, use.names = FALSE)), stringr::str_to_lower(names(weights)))
  missing_w <- setdiff(c("friendshore", "opportunity", "development"), names(w))
  if (length(missing_w) > 0) {
    stop("Partnership strength weights missing: ", paste(missing_w, collapse = ", "))
  }

  # The target variable is resolved outside filter(): the top5 tables have their own `category`
  # column, which would mask the argument inside the data mask. The index row's
  # imputed_weight_share comes along as <pillar>_share (NA when the table has none).
  pick_index <- function(tbl, pillar, keys) {
    target <- index_variables[[pillar]]
    if (!"imputed_weight_share" %in% names(tbl)) tbl$imputed_weight_share <- NA_real_
    tbl %>%
      dplyr::filter(variable == !!target) %>%
      dplyr::select(dplyr::all_of(keys), value, imputed_weight_share) %>%
      dplyr::rename(!!pillar := value, !!paste0(pillar, "_share") := imputed_weight_share)
  }

  fs <- pick_index(friendshore_top5, "friendshore", c("reporter_iso", "partner_iso"))
  op <- pick_index(opportunity_top5, "opportunity", c("reporter_iso", "partner_iso"))
  dv <- pick_index(development_top5, "development", "partner_iso")

  wts <- c(w[["friendshore"]], w[["opportunity"]], w[["development"]])
  out <- dplyr::full_join(fs, op, by = c("reporter_iso", "partner_iso")) %>%
    dplyr::left_join(dv, by = "partner_iso") %>%
    dplyr::filter(reporter_iso != partner_iso) %>%
    dplyr::mutate(
      n_categories = (!is.na(friendshore)) + (!is.na(opportunity)) + (!is.na(development)),
      Partnership_Strength_Index = partnership_strength_row_weighted_mean(
        list(friendshore, opportunity, development),
        wts
      ),
      # The dyad's imputed weight share, over the same categories and weights as its score.
      imputed_weight_share = psi_weighted_share(
        list(friendshore, opportunity, development),
        list(friendshore_share, opportunity_share, development_share),
        wts
      )
    ) %>%
    dplyr::select(-friendshore_share, -opportunity_share, -development_share)

  partnership_strength_assert_unique(out, c("reporter_iso", "partner_iso"), label = "psi dyads")
}

# Weighted mean of category shares over the categories present (a non-missing value) with a
# positive weight, renormalized as the composite is; NA when a present category's share is
# unknown or no category is present.
psi_weighted_share <- function(values, shares, weights) {
  num <- 0
  den <- 0
  unknown <- FALSE
  for (k in seq_along(values)) {
    present <- !is.na(values[[k]]) & weights[[k]] > 0
    s <- suppressWarnings(as.numeric(shares[[k]]))
    unknown <- unknown | (present & is.na(s))
    num <- num + ifelse(present, weights[[k]] * dplyr::coalesce(s, 0), 0)
    den <- den + ifelse(present, weights[[k]], 0)
  }
  ifelse(den > 0 & !unknown, num / den, NA_real_)
}
