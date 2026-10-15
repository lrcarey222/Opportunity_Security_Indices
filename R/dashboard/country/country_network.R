# The economy's role in the paper's allied network design (spec_country.md A4.2 S5) and its allied
# pick with the runner-up. Pure.
#
# member_shares: outputs/allied_network_member_shares.csv (tech, supply_chain, iso3c, network_node,
# selected, production_share_derived, share_is_derived). allied: the economy's row of
# data/processed/charts/allied_strategic.csv (tech, supply_chain, strategic_index) or NULL.

COUNTRY_PICK_EXCLUDED_TECHS <- c("Geothermal", "Green Hydrogen")

# The pick's runner-up among the economy's eligible sectors: Upstream or Midstream, in the Strategic
# Index sample, not geothermal or green hydrogen, not screened out (pick_exclusion NA).
country_pick_runner_up <- function(si_rows, pick_key, cells = country_cells_tbl()) {
  el <- si_rows[is.finite(si_rows$si) & si_rows$stage != "Downstream" & !si_rows$tech %in% COUNTRY_PICK_EXCLUDED_TECHS &
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

country_network_role <- function(iso3, tier, eu_member, node, member_shares, allied, si_rows, near_tie = 0.01, cells = country_cells_tbl(),
                                 rule_words = NULL) {
  if (!tier %in% c("core", "partner")) return(list(member = FALSE, node = NULL, via_eu = FALSE, n_stages = NULL, producer = list(), pick = NULL))
  ms <- member_shares[member_shares$iso3c == iso3 & member_shares$selected %in% TRUE & is.finite(member_shares$production_share_derived) &
                        member_shares$production_share_derived > 0, , drop = FALSE]
  ms$key <- if (nrow(ms)) country_cell_key(ms$tech, ms$supply_chain) else character()
  ms <- ms[order(-ms$production_share_derived, match(ms$key, cells$key)), , drop = FALSE]
  n_stages <- nrow(unique(member_shares[, c("tech", "supply_chain")]))
  producer <- lapply(seq_len(nrow(ms)), function(i) list(cell = ms$key[i], share = country_fv(round(ms$production_share_derived[i], 6), "share"),
                                                        derived = isTRUE(ms$share_is_derived[i])))
  pick <- NULL
  if (!is.null(allied) && nrow(allied)) {
    pk <- country_cell_key(allied$tech[1], allied$supply_chain[1])
    ru <- country_pick_runner_up(si_rows, pk, cells)
    m <- if (is.null(ru)) NA_real_ else ru$margin
    pick <- list(cell = pk, si = country_fv(allied$strategic_index[1], "index"), runner_up = if (is.null(ru) || is.na(ru$runner_up)) NULL else ru$runner_up,
                 margin = if (is.finite(m)) list(v = round(m, 6), d = country_fmt(m, "index3")) else list(v = NULL, d = COUNTRY_DASH),
                 near_tie = is.finite(m) && m < near_tie, first = if (is.null(ru)) NULL else ru$first)
    # additive 2026-10-14: the technologies left out of allied picks by rule, and the rule in words
    pick$excluded <- I(COUNTRY_PICK_EXCLUDED_TECHS)
    if (!is.null(rule_words)) pick$rule_d <- rule_words
  }
  list(member = TRUE, node = node, via_eu = isTRUE(eu_member), n_stages = country_fv(n_stages, "count"), producer = producer, pick = pick)
}
