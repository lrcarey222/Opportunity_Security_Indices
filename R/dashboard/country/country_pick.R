# The economy's allied pick (the Strategic Index's allied selection; spec_country.md A4.2 S5) with its
# runner-up. Pure. Until 2026-10-17 this file (then country_network.R) also gave the economy's role in
# the paper's allied network design; that design is retired, replaced by the Allied Industrial Base
# analysis, and the fact sheets no longer carry it.
#
# allied: the economy's row of data/processed/charts/allied_strategic.csv (tech, supply_chain,
# strategic_index) or NULL.

COUNTRY_PICK_EXCLUDED_TECHS <- c("Geothermal", "Green Hydrogen")
# The stages an allied pick may take (2026-10-28): every stage but Deployment, the pipeline's
# STRATEGIC_ALLIED_STAGES (R/indices/build_indices.R; select_allied_strategic_sectors()), which equals
# SUPPLY_CHAIN_SUPPLY_STAGES of R/utils/supply_chain_stages.R (tests/testthat/test-country-rules.R checks
# both). The endowment screen (resource_base, pick_exclusion "no resource base") falls on Extraction only.
COUNTRY_PICK_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Recovery")

# The pick's runner-up among the economy's eligible sectors: a stage of COUNTRY_PICK_STAGES (any but
# Deployment), in the Strategic Index sample, not geothermal or green hydrogen, not screened out
# (pick_exclusion NA: an Extraction sector without a recorded resource base is never eligible).
country_pick_runner_up <- function(si_rows, pick_key, cells = country_cells_tbl()) {
  el <- si_rows[is.finite(si_rows$si) & si_rows$stage %in% COUNTRY_PICK_STAGES & !si_rows$tech %in% COUNTRY_PICK_EXCLUDED_TECHS &
                  is.na(si_rows$pick_exclusion), , drop = FALSE]
  el <- el[order(-el$si, match(el$key, cells$key)), , drop = FALSE]
  if (!nrow(el)) return(NULL)
  first <- el$key[1]
  rest <- el[el$key != pick_key, , drop = FALSE]
  ru <- if (nrow(rest)) rest$key[1] else NA_character_
  si_pick <- si_rows$si[si_rows$key == pick_key][1]
  list(first = first, runner_up = ru, margin = if (is.na(ru)) NA_real_ else si_pick - rest$si[1])
}

# The pick rule in words (2026-10-14; config pick_rule, "{excluded}" = the excluded technologies in lower
# case), so memos can say why a higher-ranked green-hydrogen or geothermal sector is not the pick.
country_pick_rule_words <- function(template = NULL, excluded = COUNTRY_PICK_EXCLUDED_TECHS) {
  if (is.null(template) || !nzchar(template)) return(NULL)
  ex <- tolower(excluded)
  ex_d <- if (length(ex) <= 1) paste(ex, collapse = "") else paste(paste(ex[-length(ex)], collapse = ", "), "and", ex[length(ex)])
  gsub("{excluded}", ex_d, template, fixed = TRUE)
}

# The sheet's `pick`: NULL outside the coalition and for a member without an allied pick; else the
# pick's cell, Strategic Index, runner-up and margin (near_tie below `near_tie`), the technologies left
# out by rule and the rule in words. `first` (the economy's top eligible sector, a check of the pick)
# is dropped by the caller after the check.
country_allied_pick <- function(tier, allied, si_rows, near_tie = 0.01, cells = country_cells_tbl(), rule_words = NULL) {
  if (!tier %in% c("core", "partner") || is.null(allied) || !nrow(allied)) return(NULL)
  pk <- country_cell_key(allied$tech[1], allied$supply_chain[1])
  ru <- country_pick_runner_up(si_rows, pk, cells)
  m <- if (is.null(ru)) NA_real_ else ru$margin
  pick <- list(cell = pk, si = country_fv(allied$strategic_index[1], "index"), runner_up = if (is.null(ru) || is.na(ru$runner_up)) NULL else ru$runner_up,
               margin = if (is.finite(m)) list(v = round(m, 6), d = country_fmt(m, "index3")) else list(v = NULL, d = COUNTRY_DASH),
               near_tie = is.finite(m) && m < near_tie, first = if (is.null(ru)) NULL else ru$first)
  # additive 2026-10-14: the technologies left out of allied picks by rule, and the rule in words
  pick$excluded <- I(COUNTRY_PICK_EXCLUDED_TECHS)
  if (!is.null(rule_words)) pick$rule_d <- rule_words
  pick
}
