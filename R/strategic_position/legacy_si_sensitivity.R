# Legacy Strategic Index sensitivity diagnostics (docs/strategic_position.md, "Why"; the
# stage_scenarios of config/strategic_position.yml). Pure: no IO; scripts/26 reads and writes.
#
# The legacy SI (build_strategic_index(), R/indices/build_indices.R) is
#   0.2 pol + 0.3 EO + 0.3 (1 - ES) + 0.1 sc_weight + 0.1 tech_weight.
# Two of its five terms are normative priors that are the same for every country: the stage
# weight sc_weight (lambda_s, SUPPLY_CHAIN_STAGE_SI_WEIGHTS) and the technology climate weight
# tech_weight (ghg_index). The framework drops both; these diagnostics measure what they, and the
# CAT component of the policy term, did to the legacy rankings and to the allied picks, without
# changing the stored SI.
#
# Because both priors are constant within a technology x stage cell, swapping them moves only a
# country's ordering of its own cells: the cross-country order inside a cell cannot change (the
# per-cell Spearman of a stage scenario is 1 by construction, which the comparison reports as a
# check). CAT is different: pol is a within-country S-curve, so removing CAT moves both orders.
#
# The caller sources R/utils/schema.R (require_columns), R/utils/scurve.R,
# R/utils/supply_chain_stages.R, R/indices/build_indices.R (STRATEGIC_INDEX_WEIGHTS,
# STRATEGIC_ALLIED_STAGES, select_allied_strategic_sectors(), scurve_in_sample()),
# R/indices/build_policy_index.R (build_composite_policy_index()) and
# R/strategic_position/sp_helpers.R.

# ---- internal helpers -----------------------------------------------------------------------------

# Spearman correlation over the pairs where both values exist; NA when fewer than min_n pairs or
# when either side is constant (a correlation is undefined there, not 0 or 1).
sp_lsi_spearman <- function(x, y, min_n = 3) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < min_n) return(NA_real_)
  x <- x[ok]
  y <- y[ok]
  if (length(unique(x)) < 2 || length(unique(y)) < 2) return(NA_real_)
  stats::cor(x, y, method = "spearman")
}

# The label(s) of the rows flagged `top` ("; "-joined, sorted, so ties compare as a set); NA when
# none is flagged.
sp_lsi_top_label <- function(label, top) {
  top <- top %in% TRUE
  if (!any(top)) return(NA_character_)
  paste(sort(unique(as.character(label[top]))), collapse = "; ")
}

# Country Insights policy thirds (country_thirds(), R/dashboard/country/country_util.R, used by
# country_priority_policy()): ranks with ties averaged, k = round(n / 3), high = rank <= k,
# low = rank > n - k, middle otherwise. Replicated here so this file does not source dashboard
# code. NA everywhere when x has no variation (Country Insights shows no thirds then either).
sp_lsi_thirds <- function(x) {
  ok <- !is.na(x)
  n <- sum(ok)
  out <- rep(NA_character_, length(x))
  if (n == 0 || length(unique(x[ok])) < 2) return(out)
  r <- rank(-x[ok], ties.method = "average")
  k <- round(n / 3)
  out[ok] <- ifelse(r <= k, "high", ifelse(r > n - k, "low", "middle"))
  out
}

# Ungrouped copy of a strategic_index table with character keys, a sector label and the legacy
# roll-up; stops on non-stage values and on duplicated cells.
sp_lsi_prepare <- function(strategic_index, label = "strategic_index") {
  require_columns(strategic_index, c(SP_CELL_KEYS, "strategic_index"), label = label)
  out <- tibble::as_tibble(strategic_index) %>% dplyr::ungroup()
  out$Country <- as.character(out$Country)
  out$tech <- as.character(out$tech)
  out$supply_chain <- as.character(out$supply_chain)
  assert_supply_chain_stages(out$supply_chain, label = paste0(label, "$supply_chain"))
  if (anyDuplicated(out[SP_CELL_KEYS])) {
    stop(label, " has duplicated Country x tech x supply_chain rows.")
  }
  if (!"sector" %in% names(out)) out$sector <- paste(out$tech, out$supply_chain, sep = " - ")
  out$legacy_stage <- supply_chain_stage_rollup(out$supply_chain)
  out
}

# Within-country and cross-country (within tech x stage) ranks of `si` per variant: 1 = highest,
# ties share the minimum rank, NA where si is NA (sp_rank_desc()).
sp_lsi_add_ranks <- function(long) {
  long %>%
    dplyr::group_by(.data$variant, .data$Country) %>%
    dplyr::mutate(
      rank_within_country = sp_rank_desc(.data$si),
      n_within_country = sum(!is.na(.data$si))
    ) %>%
    dplyr::group_by(.data$variant, .data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      rank_cross_country = sp_rank_desc(.data$si),
      n_cross_country = sum(!is.na(.data$si))
    ) %>%
    dplyr::ungroup()
}

# A config stage_scenarios list (scenario -> stage -> lambda, as yaml::read_yaml() parses it) as a
# named list of named numeric vectors over the six stages. Stops on a missing, unknown or
# non-finite stage weight, so a typo cannot silently zero a stage.
sp_lsi_scenario_lambdas <- function(scenarios) {
  if (!is.list(scenarios) || length(scenarios) == 0 || is.null(names(scenarios)) ||
      any(!nzchar(names(scenarios))) || anyDuplicated(names(scenarios))) {
    stop("scenarios must be a non-empty named list (scenario -> stage -> lambda).")
  }
  lapply(stats::setNames(names(scenarios), names(scenarios)), function(nm) {
    lam <- unlist(scenarios[[nm]])
    if (is.null(names(lam))) stop("stage scenario '", nm, "' has no stage names.")
    unknown <- setdiff(names(lam), SUPPLY_CHAIN_STAGES)
    missing <- setdiff(SUPPLY_CHAIN_STAGES, names(lam))
    if (length(unknown) > 0 || length(missing) > 0) {
      stop(
        "stage scenario '", nm, "' must give a weight to each of the six stages",
        if (length(missing) > 0) paste0("; missing ", paste(missing, collapse = ", ")),
        if (length(unknown) > 0) paste0("; unknown ", paste(unknown, collapse = ", ")), "."
      )
    }
    lam <- suppressWarnings(as.numeric(lam[SUPPLY_CHAIN_STAGES]))
    if (any(!is.finite(lam))) stop("stage scenario '", nm, "' has a non-numeric or missing weight.")
    stats::setNames(lam, SUPPLY_CHAIN_STAGES)
  })
}

# ---- 1. stage (and technology) prior scenarios ---------------------------------------------------

# Recompute the legacy SI under each stage-prior scenario:
#   si = strategic_index - w_sc * sc_weight + w_sc * lambda[stage]  (- w_tech * tech_weight when the
#   technology prior is off),
# with w_sc = weights[["supply_chain"]] and w_tech = weights[["technology"]]. Cells without an SI
# (si_exclusion) stay NA in every scenario and keep si_exclusion, pick_exclusion and resource_base,
# so the allied pick rules apply unchanged. The other weights are not renormalised: dropping a
# constant term shifts levels, not orders, and the stored scale stays comparable.
#
# `scenarios`: config stage_scenarios (scenario -> stage -> lambda). `tech_prior`: TRUE keeps the
# technology term, FALSE drops it; c(TRUE, FALSE) gives both, so the two priors can be separated.
# A scenario whose every lambda equals the stored sc_weight of its stage within match_stored_tol is
# the stored prior (the config writes 0.5 / 3 as 0.1667): it takes the stored sc_weight, so
# equal_share reproduces strategic_index exactly instead of shifting three stages by 3e-6
# (lambda_matches_stored records it). match_stored_tol = 0 turns this off.
#
# Returns one row per variant x cell: variant (scenario, with "_no_tech" when the technology prior
# is off), scenario, tech_prior, Country, tech, supply_chain, legacy_stage, sector, lambda,
# lambda_matches_stored, stage_term, tech_term, strategic_index (stored), si, the carried columns
# (si_exclusion, pick_exclusion, resource_base, policy_basis when present), rank_within_country,
# n_within_country, rank_cross_country, n_cross_country.
sp_si_stage_scenarios <- function(strategic_index,
                                  scenarios,
                                  weights = STRATEGIC_INDEX_WEIGHTS,
                                  tech_prior = c(TRUE, FALSE),
                                  match_stored_tol = 1e-3) {
  require_columns(strategic_index, c("sc_weight", "tech_weight"), label = "strategic_index")
  si <- sp_lsi_prepare(strategic_index)
  if (!all(c("supply_chain", "technology") %in% names(weights))) {
    stop("weights must name supply_chain and technology (STRATEGIC_INDEX_WEIGHTS).")
  }
  tech_prior <- unique(as.logical(tech_prior))
  if (length(tech_prior) == 0 || anyNA(tech_prior)) stop("tech_prior must be TRUE, FALSE or both.")
  lambdas <- sp_lsi_scenario_lambdas(scenarios)
  w_sc <- weights[["supply_chain"]]
  w_tech <- weights[["technology"]]

  scored <- !is.na(si$strategic_index)
  if (any(scored & (is.na(si$sc_weight) | is.na(si$tech_weight)))) {
    stop("strategic_index has scored cells without sc_weight or tech_weight.")
  }
  # The stored prior per stage: one value per stage, or the snap to it is not attempted.
  stored_by_stage <- tapply(si$sc_weight[scored], si$supply_chain[scored], function(x) {
    if (diff(range(x)) <= 1e-12) x[[1]] else NA_real_
  })
  stages_present <- names(stored_by_stage)

  carried <- intersect(c("si_exclusion", "pick_exclusion", "resource_base", "resource_evidence", "policy_basis"), names(si))
  base_cols <- c(SP_CELL_KEYS, "legacy_stage", "sector", "strategic_index", "sc_weight", "tech_weight", carried)
  base <- si[base_cols]

  out <- list()
  for (nm in names(lambdas)) {
    lam <- lambdas[[nm]]
    matches <- match_stored_tol > 0 && length(stages_present) > 0 && !anyNA(stored_by_stage) &&
      all(abs(lam[stages_present] - stored_by_stage[stages_present]) <= match_stored_tol)
    if (matches) lam[stages_present] <- stored_by_stage[stages_present]
    lam_row <- unname(lam[base$supply_chain])
    for (tp in tech_prior) {
      v <- base
      v$variant <- if (tp) nm else paste0(nm, "_no_tech")
      v$scenario <- nm
      v$tech_prior <- tp
      v$lambda <- lam_row
      v$lambda_matches_stored <- matches
      v$stage_term <- w_sc * lam_row
      v$tech_term <- if (tp) w_tech * v$tech_weight else 0
      # The stored SI minus the stored stage term plus the scenario's: for the stored prior the
      # difference is exactly 0, so the stored values come back bit for bit.
      tech_drop <- if (tp) 0 else w_tech * v$tech_weight
      v$si <- v$strategic_index + w_sc * (lam_row - v$sc_weight) - tech_drop
      v$si[is.na(v$strategic_index)] <- NA_real_
      out[[length(out) + 1]] <- v
    }
  }
  variant_order <- unique(unlist(lapply(names(lambdas), function(nm) {
    c(if (TRUE %in% tech_prior) nm, if (FALSE %in% tech_prior) paste0(nm, "_no_tech"))
  })))
  dplyr::bind_rows(out) %>%
    sp_lsi_add_ranks() %>%
    dplyr::select(
      "variant", "scenario", "tech_prior", dplyr::all_of(SP_CELL_KEYS), "legacy_stage", "sector",
      "lambda", "lambda_matches_stored", "stage_term", "tech_term", "strategic_index", "si",
      dplyr::all_of(carried),
      "rank_within_country", "n_within_country", "rank_cross_country", "n_cross_country"
    ) %>%
    dplyr::arrange(match(.data$variant, variant_order), .data$Country, .data$tech,
                   match(.data$supply_chain, SUPPLY_CHAIN_STAGES))
}

# ---- 2. ranking comparison against a baseline variant --------------------------------------------

# Compares every variant of a long table (sp_si_stage_scenarios(), or the `long` of
# sp_cat_rank_effect()) with the baseline variant. Returns a list:
#   country   per variant x country (countries with a scored cell): n_cells, spearman_within (the
#             country's own cell ordering against the baseline's), top1 / top1_baseline (sector;
#             ties "; "-joined), top1_stage / top1_stage_baseline, top1_changed, top_n_overlap
#             (cells in both top-n sets; ties included at the cut), top_n
#   cell      per variant x tech x stage: n_countries, spearman_cross (the cross-country order in
#             the cell against the baseline's; 1 by construction for stage and technology priors),
#             top_country / top_country_baseline, top_country_changed
#   stage     per variant x stage: n_top (countries whose top cell is at the stage; a tie of k
#             cells counts 1/k each, so shares sum to 1), share_top, the baseline's, change
#   top_cell  per variant x tech x stage: n_top, share_top and the baseline's (the specialisation
#             pattern behind `stage`)
#   summary   per variant: n_countries, spearman quantiles, top-1 changes, mean top-n overlap,
#             and the cell-level spearman_cross median and minimum
sp_si_scenario_comparison <- function(long, baseline = "equal_share", top_n = 5, min_n = 3) {
  require_columns(long, c("variant", SP_CELL_KEYS, "si", "rank_within_country"), label = "long")
  if (!baseline %in% long$variant) stop("baseline variant '", baseline, "' is not in long$variant.")
  long <- tibble::as_tibble(long) %>% dplyr::ungroup()
  if (!"sector" %in% names(long)) long$sector <- paste(long$tech, long$supply_chain, sep = " - ")
  variant_order <- unique(long$variant)
  ids <- long %>% dplyr::distinct(.data$variant, dplyr::across(dplyr::any_of(c("scenario", "tech_prior"))))

  base <- long %>%
    dplyr::filter(.data$variant == baseline) %>%
    dplyr::select(dplyr::all_of(SP_CELL_KEYS), base_si = "si", base_rank = "rank_within_country")
  joined <- long %>% dplyr::left_join(base, by = SP_CELL_KEYS)

  country <- joined %>%
    dplyr::group_by(.data$variant, .data$Country) %>%
    dplyr::summarise(
      n_cells = sum(!is.na(.data$si)),
      n_cells_baseline = sum(!is.na(.data$base_si)),
      spearman_within = sp_lsi_spearman(.data$si, .data$base_si, min_n),
      top1 = sp_lsi_top_label(.data$sector, .data$rank_within_country == 1),
      top1_baseline = sp_lsi_top_label(.data$sector, .data$base_rank == 1),
      top1_stage = sp_lsi_top_label(.data$supply_chain, .data$rank_within_country == 1),
      top1_stage_baseline = sp_lsi_top_label(.data$supply_chain, .data$base_rank == 1),
      top_n_overlap = sum(.data$rank_within_country <= top_n & .data$base_rank <= top_n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_cells > 0 | .data$n_cells_baseline > 0) %>%
    dplyr::mutate(
      top1_changed = dplyr::coalesce(.data$top1, "") != dplyr::coalesce(.data$top1_baseline, ""),
      top_n = top_n
    )

  base_x <- long %>%
    dplyr::filter(.data$variant == baseline) %>%
    dplyr::select(dplyr::all_of(SP_CELL_KEYS), base_si = "si")
  cell <- long %>%
    dplyr::left_join(base_x, by = SP_CELL_KEYS) %>%
    dplyr::group_by(.data$variant, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_countries = sum(!is.na(.data$si)),
      spearman_cross = sp_lsi_spearman(.data$si, .data$base_si, min_n),
      top_country = sp_lsi_top_label(.data$Country, !is.na(.data$si) & .data$si == suppressWarnings(max(.data$si, na.rm = TRUE))),
      top_country_baseline = sp_lsi_top_label(
        .data$Country, !is.na(.data$base_si) & .data$base_si == suppressWarnings(max(.data$base_si, na.rm = TRUE))
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_countries > 0) %>%
    dplyr::mutate(
      top_country_changed = dplyr::coalesce(.data$top_country, "") != dplyr::coalesce(.data$top_country_baseline, "")
    )

  # Top cells, a tie of k cells weighted 1/k so each country counts once.
  tops <- long %>%
    dplyr::filter(.data$rank_within_country %in% 1L) %>%
    dplyr::group_by(.data$variant, .data$Country) %>%
    dplyr::mutate(w = 1 / dplyr::n()) %>%
    dplyr::ungroup()
  n_ctry <- tops %>% dplyr::distinct(.data$variant, .data$Country) %>% dplyr::count(.data$variant, name = "n_countries")
  top_cell <- tops %>%
    dplyr::group_by(.data$variant, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(n_top = sum(.data$w), .groups = "drop") %>%
    tidyr::complete(variant, tidyr::nesting(tech, supply_chain), fill = list(n_top = 0)) %>%
    dplyr::left_join(n_ctry, by = "variant") %>%
    dplyr::mutate(share_top = .data$n_top / .data$n_countries)
  top_cell <- top_cell %>%
    dplyr::left_join(
      top_cell %>%
        dplyr::filter(.data$variant == baseline) %>%
        dplyr::select("tech", "supply_chain", n_top_baseline = "n_top", share_top_baseline = "share_top"),
      by = c("tech", "supply_chain")
    ) %>%
    dplyr::mutate(
      n_top_baseline = dplyr::coalesce(.data$n_top_baseline, 0),
      share_top_baseline = dplyr::coalesce(.data$share_top_baseline, 0),
      change_share = .data$share_top - .data$share_top_baseline
    ) %>%
    dplyr::arrange(match(.data$variant, variant_order), dplyr::desc(.data$n_top), .data$tech)

  stage <- tops %>%
    dplyr::group_by(.data$variant, .data$supply_chain) %>%
    dplyr::summarise(n_top = sum(.data$w), .groups = "drop") %>%
    tidyr::complete(variant, supply_chain = SUPPLY_CHAIN_STAGES, fill = list(n_top = 0)) %>%
    dplyr::left_join(n_ctry, by = "variant") %>%
    dplyr::mutate(share_top = .data$n_top / .data$n_countries)
  stage <- stage %>%
    dplyr::left_join(
      stage %>%
        dplyr::filter(.data$variant == baseline) %>%
        dplyr::select("supply_chain", n_top_baseline = "n_top", share_top_baseline = "share_top"),
      by = "supply_chain"
    ) %>%
    dplyr::mutate(change_share = .data$share_top - .data$share_top_baseline) %>%
    dplyr::arrange(match(.data$variant, variant_order), match(.data$supply_chain, SUPPLY_CHAIN_STAGES))

  summary <- country %>%
    dplyr::group_by(.data$variant) %>%
    dplyr::summarise(
      n_countries = dplyr::n(),
      spearman_within_median = stats::median(.data$spearman_within, na.rm = TRUE),
      spearman_within_p10 = unname(stats::quantile(.data$spearman_within, 0.1, na.rm = TRUE)),
      spearman_within_min = suppressWarnings(min(.data$spearman_within, na.rm = TRUE)),
      n_top1_changed = sum(.data$top1_changed),
      share_top1_changed = mean(.data$top1_changed),
      top_n_overlap_mean = mean(.data$top_n_overlap),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      cell %>%
        dplyr::group_by(.data$variant) %>%
        dplyr::summarise(
          n_cells_cross = dplyr::n(),
          spearman_cross_median = stats::median(.data$spearman_cross, na.rm = TRUE),
          spearman_cross_min = suppressWarnings(min(.data$spearman_cross, na.rm = TRUE)),
          n_cells_top_country_changed = sum(.data$top_country_changed),
          .groups = "drop"
        ),
      by = "variant"
    )

  add_ids <- function(tbl) {
    tbl %>%
      dplyr::left_join(ids, by = "variant") %>%
      dplyr::relocate(dplyr::any_of(c("variant", "scenario", "tech_prior"))) %>%
      dplyr::arrange(match(.data$variant, variant_order))
  }
  list(
    country = add_ids(country %>% dplyr::arrange(match(.data$variant, variant_order), .data$Country)),
    cell = add_ids(cell %>% dplyr::arrange(match(.data$variant, variant_order), .data$tech,
                                           match(.data$supply_chain, SUPPLY_CHAIN_STAGES))),
    stage = add_ids(stage),
    top_cell = add_ids(top_cell),
    summary = add_ids(summary)
  )
}

# ---- 3. allied picks under each variant -------------------------------------------------------------

# Re-runs the allied pick (select_allied_strategic_sectors(), with the arguments scripts/20 passes:
# stages = STRATEGIC_ALLIED_STAGES, the endowment screen on, the function's excluded technologies)
# on every variant's si and compares each member's pick with the baseline variant's. `allies` is
# what scripts/20 passes: a data frame with one row per member and a Country column (iso3, tier and
# network_node are carried), or a character vector of names. The resource screen is read from the
# long table's resource_base, so the same Extraction cells are barred in every variant.
#
# Returns a list:
#   picks        per variant x member: pick (sector, tech, stage; ties "; "-joined as scripts/20 would
#                write two rows), pick_si, runner_up, runner_up_si, margin (pick minus runner-up;
#                0 for a tie), the baseline's pick, stage, margin and runner-up, changed,
#                baseline_near (baseline margin < near_margin), old_pick_si (the baseline pick's si in
#                this variant) and old_pick_deficit (pick_si - old_pick_si: how far the old pick fell)
#   summary      per variant: n_members, n_picked, n_changed, n_changed_near_baseline,
#                n_near_baseline, n_stage_only_changes (same technology, other stage), margin median
#   by_tech      per variant x tech: n_picks (rows, as allied_strategic.csv counts them), baseline, change
#   by_stage     per variant x stage: the same
#   focus        per variant x focus technology: n_picks, n_baseline, n_lost (baseline picks of the
#                technology now another technology), n_gained, n_stage_moves (same technology, other
#                stage), n_baseline_near (its baseline picks within near_margin of the runner-up)
#   transitions  per variant: changed picks counted by baseline sector -> new sector
sp_si_pick_comparison <- function(long,
                                  allies,
                                  baseline = "equal_share",
                                  stages = STRATEGIC_ALLIED_STAGES,
                                  excluded_techs = eval(formals(select_allied_strategic_sectors)$excluded_techs),
                                  require_resource_base = TRUE,
                                  near_margin = 0.01,
                                  focus_techs = c("Batteries", "Nuclear")) {
  require_columns(long, c("variant", SP_CELL_KEYS, "si"), label = "long")
  if (!baseline %in% long$variant) stop("baseline variant '", baseline, "' is not in long$variant.")
  long <- tibble::as_tibble(long) %>% dplyr::ungroup()
  if (!"sector" %in% names(long)) long$sector <- paste(long$tech, long$supply_chain, sep = " - ")
  ally_tbl <- if (is.data.frame(allies)) {
    require_columns(allies, "Country", label = "allies")
    tibble::as_tibble(allies)
  } else {
    tibble::tibble(Country = as.character(allies))
  }
  ally_tbl$Country <- as.character(ally_tbl$Country)
  if (anyNA(ally_tbl$Country) || anyDuplicated(ally_tbl$Country)) {
    stop("allies must have one row per Country (no NA, no duplicates).")
  }
  ally_names <- ally_tbl$Country
  variant_order <- unique(long$variant)
  ids <- long %>% dplyr::distinct(.data$variant, dplyr::across(dplyr::any_of(c("scenario", "tech_prior"))))

  one_variant <- function(v) {
    tbl <- long[long$variant == v, , drop = FALSE]
    # Only what the selection reads, so the ally columns never clash with the long table's.
    slim <- tibble::tibble(
      Country = tbl$Country, tech = tbl$tech, supply_chain = tbl$supply_chain,
      sector = tbl$sector, strategic_index = tbl$si
    )
    if ("resource_base" %in% names(tbl)) slim$resource_base <- tbl$resource_base
    pick_rows <- select_allied_strategic_sectors(
      slim, ally_names, stages = stages, excluded_techs = excluded_techs, n = 1,
      require_resource_base = require_resource_base
    )
    top2 <- select_allied_strategic_sectors(
      slim, ally_names, stages = stages, excluded_techs = excluded_techs, n = 2,
      require_resource_base = require_resource_base
    )
    picks <- pick_rows %>%
      dplyr::group_by(.data$Country) %>%
      dplyr::summarise(
        n_pick_rows = dplyr::n(),
        pick = paste(sort(.data$sector), collapse = "; "),
        pick_tech = paste(sort(unique(.data$tech)), collapse = "; "),
        pick_stage = paste(sort(unique(.data$supply_chain)), collapse = "; "),
        pick_si = max(.data$strategic_index),
        .groups = "drop"
      )
    runner <- top2 %>%
      dplyr::left_join(picks %>% dplyr::select("Country", "pick", "n_pick_rows"), by = "Country") %>%
      dplyr::filter(.data$n_pick_rows == 1, .data$sector != .data$pick) %>%
      dplyr::group_by(.data$Country) %>%
      dplyr::summarise(
        runner_up = paste(sort(.data$sector[.data$strategic_index == max(.data$strategic_index)]), collapse = "; "),
        runner_up_si = max(.data$strategic_index),
        .groups = "drop"
      )
    picks %>%
      dplyr::left_join(runner, by = "Country") %>%
      dplyr::mutate(
        # A tie at the top is a zero margin; a member with one eligible cell has none.
        margin = dplyr::if_else(.data$n_pick_rows > 1, 0, .data$pick_si - .data$runner_up_si),
        variant = v
      )
  }
  picks <- dplyr::bind_rows(lapply(variant_order, one_variant))

  base <- picks %>%
    dplyr::filter(.data$variant == baseline) %>%
    dplyr::select("Country",
      baseline_pick = "pick", baseline_pick_tech = "pick_tech", baseline_pick_stage = "pick_stage",
      baseline_pick_si = "pick_si", baseline_runner_up = "runner_up", baseline_margin = "margin",
      baseline_n_pick_rows = "n_pick_rows"
    )
  # The baseline pick's si in each variant (single picks only; a tied baseline has no one old pick).
  old_si <- long %>%
    dplyr::select("variant", "Country", "sector", "si") %>%
    dplyr::inner_join(
      base %>% dplyr::filter(.data$baseline_n_pick_rows == 1) %>% dplyr::select("Country", sector = "baseline_pick"),
      by = c("Country", "sector")
    ) %>%
    dplyr::select("variant", "Country", old_pick_si = "si")

  grid <- tidyr::crossing(variant = variant_order, Country = ally_names)
  picks <- grid %>%
    dplyr::left_join(picks, by = c("variant", "Country")) %>%
    dplyr::left_join(base, by = "Country") %>%
    dplyr::left_join(old_si, by = c("variant", "Country")) %>%
    dplyr::mutate(
      picked = !is.na(.data$pick),
      # gaining or losing a pick counts as a change too
      changed = dplyr::coalesce(.data$pick, "") != dplyr::coalesce(.data$baseline_pick, ""),
      stage_only_change = .data$changed & dplyr::coalesce(.data$pick_tech == .data$baseline_pick_tech, FALSE),
      baseline_near = .data$baseline_margin < near_margin,
      old_pick_deficit = .data$pick_si - .data$old_pick_si
    ) %>%
    dplyr::left_join(ally_tbl, by = "Country") %>%
    dplyr::left_join(ids, by = "variant") %>%
    dplyr::relocate(dplyr::any_of(c("variant", "scenario", "tech_prior", "Country", names(ally_tbl)))) %>%
    dplyr::arrange(match(.data$variant, variant_order), .data$Country)

  summary <- picks %>%
    dplyr::group_by(.data$variant) %>%
    dplyr::summarise(
      n_members = dplyr::n(),
      n_picked = sum(.data$picked),
      n_changed = sum(.data$changed),
      n_stage_only_changes = sum(.data$stage_only_change, na.rm = TRUE),
      n_changed_near_baseline = sum(.data$changed & .data$baseline_near %in% TRUE),
      n_near_baseline = sum(.data$baseline_near %in% TRUE),
      margin_median = stats::median(.data$margin, na.rm = TRUE),
      n_near = sum(.data$margin < near_margin, na.rm = TRUE),
      .groups = "drop"
    )

  # Pick rows as scripts/20 writes them (a tied member has one row per tied sector).
  pick_rows <- picks %>%
    dplyr::filter(.data$picked) %>%
    dplyr::select("variant", "Country", "pick") %>%
    tidyr::separate_rows("pick", sep = "; ") %>%
    dplyr::left_join(long %>% dplyr::distinct(pick = .data$sector, .data$tech, .data$supply_chain), by = "pick")
  count_by <- function(col) {
    counts <- pick_rows %>%
      dplyr::count(dplyr::across(dplyr::all_of(c("variant", col))), name = "n_picks") %>%
      tidyr::complete(variant, !!rlang::sym(col), fill = list(n_picks = 0L))
    counts %>%
      dplyr::left_join(
        counts %>% dplyr::filter(.data$variant == baseline) %>% dplyr::select(dplyr::all_of(col), n_baseline = "n_picks"),
        by = col
      ) %>%
      dplyr::mutate(n_baseline = dplyr::coalesce(.data$n_baseline, 0L), change = .data$n_picks - .data$n_baseline)
  }
  by_tech <- count_by("tech") %>% dplyr::arrange(match(.data$variant, variant_order), dplyr::desc(.data$n_baseline), .data$tech)
  by_stage <- count_by("supply_chain") %>%
    dplyr::arrange(match(.data$variant, variant_order), match(.data$supply_chain, SUPPLY_CHAIN_STAGES))

  focus <- tidyr::crossing(variant = variant_order, tech = focus_techs) %>%
    dplyr::left_join(by_tech %>% dplyr::select("variant", "tech", "n_picks", "n_baseline"), by = c("variant", "tech")) %>%
    dplyr::mutate(n_picks = dplyr::coalesce(.data$n_picks, 0L), n_baseline = dplyr::coalesce(.data$n_baseline, 0L))
  focus_counts <- lapply(seq_len(nrow(focus)), function(i) {
    p <- picks[picks$variant == focus$variant[i], , drop = FALSE]
    t <- focus$tech[i]
    was <- p$baseline_pick_tech %in% t
    now <- p$pick_tech %in% t
    c(
      n_lost = sum(was & !now),
      n_gained = sum(!was & now),
      n_stage_moves = sum(was & now & p$changed),
      n_baseline_near = sum(was & p$baseline_near %in% TRUE)
    )
  })
  focus <- dplyr::bind_cols(focus, dplyr::bind_rows(lapply(focus_counts, as.list)))

  transitions <- picks %>%
    dplyr::filter(.data$changed) %>%
    dplyr::count(.data$variant, from = .data$baseline_pick, to = .data$pick, name = "n_members") %>%
    dplyr::arrange(match(.data$variant, variant_order), dplyr::desc(.data$n_members), .data$from)

  add_ids <- function(tbl) {
    tbl %>%
      dplyr::left_join(ids, by = "variant") %>%
      dplyr::relocate(dplyr::any_of(c("variant", "scenario", "tech_prior"))) %>%
      dplyr::arrange(match(.data$variant, variant_order))
  }
  list(
    picks = picks,
    summary = add_ids(summary),
    by_tech = add_ids(by_tech),
    by_stage = add_ids(by_stage),
    focus = add_ids(focus),
    transitions = add_ids(transitions)
  )
}

# ---- 4. CAT in the policy term ---------------------------------------------------------------------

# How much the CAT component moved the legacy SI's policy term. The composite Policy index is
# rebuilt twice with the existing builder, build_composite_policy_index() (R/indices/
# build_policy_index.R), from the same policy_component_tbl: once with `component_weights` as
# given (scripts/20: IEA PAMS 3 + CAT 2; the rebuild is checked against the stored `value`) and
# once with the CAT component removed (IEA PAMS alone, the same eligibility rule). pol is then
# recomputed exactly as build_strategic_index() does it, scurve_in_sample() within the country over
# the scored cells (checked against the stored pol), and
#   si_without_cat = strategic_index - w_pol * pol + w_pol * pol_without_cat.
# The other terms, the sample and the exclusions are unchanged, except that a scored cell whose
# policy value came from CAT alone loses its value and leaves the sample (n_cells_lost; none is
# expected while IEA PAMS is the required component). A country scored on IEA PAMS alone is
# unchanged by construction.
#
# Returns a list:
#   cells    one row per strategic_index row: policy_basis, value (stored), value_rebuilt,
#            value_without_cat, pol, pol_rebuilt, pol_without_cat, strategic_index, si_without_cat,
#            third / third_without_cat (Country Insights policy thirds of pol), in_sample,
#            in_sample_without_cat
#   long     variants "with_cat" (the stored SI) and "without_cat", shaped like
#            sp_si_stage_scenarios() output, for sp_si_scenario_comparison() and
#            sp_si_pick_comparison() with baseline = "with_cat"
#   country  per country with a scored cell: policy_basis, n_policy_cells and rho_policy_all
#            (Spearman of the composite with and without CAT over every Policy index cell of the
#            country), n_in_sample, n_cells_lost, rho_pol and rho_si (over scored cells),
#            n_third_changed, share_third_changed, n_high_low_flips, top_pol_changed (the top
#            policy cell), top1 / top1_without_cat / top1_changed (the top SI cell), top_n_overlap
#   check    max_abs_value_diff and max_abs_pol_diff of the rebuild against the stored columns
#            (Inf when a scored cell gets no rebuilt value; a warning when either exceeds tol),
#            n_cells_lost, countries by policy_basis
sp_cat_rank_effect <- function(strategic_index,
                               policy_component_tbl,
                               techs = NULL,
                               component_weights = POLICY_COMPONENT_WEIGHTS,
                               required_components = POLICY_REQUIRED_COMPONENTS,
                               cat_component = "CAT Policy Index",
                               weights = STRATEGIC_INDEX_WEIGHTS,
                               top_n = 5,
                               tol = 1e-9) {
  require_columns(strategic_index, c("value", "pol"), label = "strategic_index")
  si <- sp_lsi_prepare(strategic_index)
  if (!cat_component %in% names(component_weights)) {
    stop("cat_component '", cat_component, "' is not one of component_weights.")
  }
  weights_without <- component_weights[names(component_weights) != cat_component]
  required_without <- setdiff(required_components, cat_component)
  if (length(weights_without) == 0 || length(required_without) == 0) {
    stop("Removing ", cat_component, " leaves no weighted or no required policy component.")
  }
  if (is.null(techs)) techs <- sort(unique(si$tech))
  w_pol <- weights[["policy"]]

  key <- c("Country", "tech", "supply_chain")
  with_cat <- build_composite_policy_index(
    policy_component_tbl, techs = techs,
    component_weights = component_weights, required_components = required_components
  )$index
  without_cat <- build_composite_policy_index(
    policy_component_tbl, techs = techs,
    component_weights = weights_without, required_components = required_without
  )$index

  cells <- si %>%
    dplyr::left_join(
      with_cat %>% dplyr::transmute(Country, tech, supply_chain, value_rebuilt = .data$value),
      by = key
    ) %>%
    dplyr::left_join(
      without_cat %>% dplyr::transmute(Country, tech, supply_chain, value_without_cat = .data$value),
      by = key
    ) %>%
    dplyr::mutate(
      in_sample = !is.na(.data$strategic_index),
      in_sample_without_cat = .data$in_sample & !is.na(.data$value_without_cat)
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(
      pol_rebuilt = scurve_in_sample(.data$value_rebuilt, .data$in_sample),
      pol_without_cat = scurve_in_sample(.data$value_without_cat, .data$in_sample_without_cat),
      third = sp_lsi_thirds(dplyr::if_else(.data$in_sample, .data$pol, NA_real_)),
      third_without_cat = sp_lsi_thirds(dplyr::if_else(.data$in_sample_without_cat, .data$pol_without_cat, NA_real_))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # written as a difference so that an unchanged pol returns the stored SI bit for bit
      si_without_cat = dplyr::if_else(
        .data$in_sample_without_cat,
        .data$strategic_index + w_pol * (.data$pol_without_cat - .data$pol),
        NA_real_
      )
    )

  scored <- cells$in_sample
  value_diff <- abs(cells$value_rebuilt[scored] - cells$value[scored])
  pol_diff <- abs(cells$pol_rebuilt[scored] - cells$pol[scored])
  check <- list(
    max_abs_value_diff = if (any(scored)) max(c(value_diff, if (anyNA(value_diff)) Inf), na.rm = TRUE) else NA_real_,
    max_abs_pol_diff = if (any(scored)) max(c(pol_diff, if (anyNA(pol_diff)) Inf), na.rm = TRUE) else NA_real_,
    n_cells_lost = sum(scored & !cells$in_sample_without_cat),
    countries_by_basis = NULL
  )
  if ("policy_basis" %in% names(cells)) {
    basis <- cells[scored, c("Country", "policy_basis")]
    basis <- basis[!duplicated(basis$Country), ]
    check$countries_by_basis <- table(basis$policy_basis, useNA = "ifany")
  }
  if (isTRUE(check$max_abs_value_diff > tol) || isTRUE(check$max_abs_pol_diff > tol)) {
    warning(
      "sp_cat_rank_effect(): the rebuilt composite does not reproduce the stored policy term (max |value| diff ",
      signif(check$max_abs_value_diff, 3), ", max |pol| diff ", signif(check$max_abs_pol_diff, 3),
      "); check component_weights, required_components and techs against policy_index_provenance."
    )
  }

  carried <- intersect(c("si_exclusion", "pick_exclusion", "resource_base", "resource_evidence", "policy_basis"), names(cells))
  long_base <- cells[c(SP_CELL_KEYS, "legacy_stage", "sector", "strategic_index", carried)]
  long <- dplyr::bind_rows(
    long_base %>% dplyr::mutate(variant = "with_cat", policy_value = cells$value, pol = cells$pol, si = cells$strategic_index),
    long_base %>% dplyr::mutate(
      variant = "without_cat", policy_value = cells$value_without_cat, pol = cells$pol_without_cat,
      si = cells$si_without_cat,
      si_exclusion = if ("si_exclusion" %in% carried) {
        dplyr::if_else(cells$in_sample & !cells$in_sample_without_cat, "missing policy index without CAT", .data$si_exclusion)
      } else NULL
    )
  ) %>%
    dplyr::mutate(scenario = .data$variant, tech_prior = TRUE) %>%
    sp_lsi_add_ranks() %>%
    dplyr::relocate("variant", "scenario", "tech_prior")

  cmp <- sp_si_scenario_comparison(long, baseline = "with_cat", top_n = top_n)
  top <- cmp$country %>%
    dplyr::filter(.data$variant == "without_cat") %>%
    dplyr::select("Country", top1 = "top1_baseline", top1_without_cat = "top1", "top1_changed", "top_n_overlap")

  policy_all <- with_cat %>%
    dplyr::select(dplyr::all_of(key), with = "value") %>%
    dplyr::left_join(without_cat %>% dplyr::select(dplyr::all_of(key), without = "value"), by = key) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      n_policy_cells = sum(!is.na(.data$with)),
      rho_policy_all = sp_lsi_spearman(.data$with, .data$without),
      .groups = "drop"
    )

  country <- cells %>%
    dplyr::filter(.data$in_sample) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      policy_basis = if ("policy_basis" %in% names(cells)) dplyr::first(stats::na.omit(.data$policy_basis)) else NA_character_,
      n_in_sample = dplyr::n(),
      n_cells_lost = sum(!.data$in_sample_without_cat),
      rho_pol = sp_lsi_spearman(.data$pol, .data$pol_without_cat),
      rho_si = sp_lsi_spearman(.data$strategic_index, .data$si_without_cat),
      n_third_changed = sum(.data$third != .data$third_without_cat, na.rm = TRUE),
      share_third_changed = if (all(is.na(.data$third) | is.na(.data$third_without_cat))) NA_real_ else
        mean(.data$third != .data$third_without_cat, na.rm = TRUE),
      n_high_low_flips = sum(
        (.data$third %in% "high" & .data$third_without_cat %in% "low") |
          (.data$third %in% "low" & .data$third_without_cat %in% "high")
      ),
      top_pol_changed = !identical(
        sp_lsi_top_label(.data$sector, .data$pol == max(.data$pol)),
        sp_lsi_top_label(.data$sector, !is.na(.data$pol_without_cat) &
          .data$pol_without_cat == suppressWarnings(max(.data$pol_without_cat, na.rm = TRUE)))
      ),
      .groups = "drop"
    ) %>%
    dplyr::left_join(policy_all, by = "Country") %>%
    dplyr::left_join(top, by = "Country") %>%
    dplyr::relocate("Country", "policy_basis", "n_policy_cells", "rho_policy_all")

  list(
    cells = cells %>%
      dplyr::select(
        dplyr::all_of(SP_CELL_KEYS), "legacy_stage", "sector", dplyr::any_of("policy_basis"),
        "value", "value_rebuilt", "value_without_cat", "pol", "pol_rebuilt", "pol_without_cat",
        "strategic_index", "si_without_cat", "third", "third_without_cat",
        "in_sample", "in_sample_without_cat"
      ),
    long = long,
    country = country,
    check = check
  )
}
