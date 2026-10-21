# Allied Industrial Base (AIB): the investment-host alignment screen and the host shortlist
# (docs/aib_methodology.md, 7). The candidate-partner universe is global, so without a screen an economy
# voting with China (Russia for grain-oriented steel, Myanmar and Laos for rare-earth oxides) can rank
# as the best place for allied capital. The screen is political, not economic: it uses the UNGA voting
# bloc (scripts/12, geo_alignment_tbl.rds), not Friendshore, which is a composite (the bloc is about a
# quarter of its weight) whose missing values sit at the neutral 0.5. The screened hosts are still
# simulated, apart, so what the screen gives up is reported (aib_alignment_cost()).

# Which candidate partners may host. mode:
#   none          every partner (no screen)
#   bloc          partners in the demand scope (the economies the AIB protects: refusing their capacity
#                 while restoring their demand would be incoherent), or outside `exclude_blocs`; an
#                 economy without a bloc is kept
#   demand_scope  partners in the demand scope only
# bloc: named character, ISO3 -> bloc label. Returns one row per partner: iso3, bloc, in_demand_scope,
# screened (TRUE when kept out) and screen_reason (NA when kept).
aib_host_screen <- function(partner_iso, demand_iso, bloc = NULL, mode = c("bloc", "none", "demand_scope"),
                            exclude_blocs = "China-leaning") {
  mode <- match.arg(mode)
  partner_iso <- unique(as.character(partner_iso))
  b <- if (is.null(bloc) || !length(bloc)) rep(NA_character_, length(partner_iso)) else unname(as.character(bloc[partner_iso]))
  scope <- partner_iso %in% demand_iso
  screened <- switch(mode,
    none = rep(FALSE, length(partner_iso)),
    bloc = !scope & !is.na(b) & b %in% exclude_blocs,
    demand_scope = !scope
  )
  reason <- ifelse(!screened, NA_character_,
                   if (identical(mode, "bloc")) paste0("UNGA bloc: ", b, " (outside the demand scope)") else "outside the demand scope")
  tibble::tibble(iso3 = partner_iso, bloc = b, in_demand_scope = scope, screened = screened, screen_reason = reason)
}

# The hosts simulated in a node: the `n` largest by current scale in the node (host_scale_usd, which
# bounds a host's increment and so its resilience gain), then capability tier and value. Ordering by
# tier first filled the list with small RCA specialists and left large exporters without RCA >= 1 out
# (Germany in power electronics, 6.8% of world exports, behind hosts with 0.2%).
aib_host_shortlist <- function(hosts, n) {
  if (is.null(hosts) || !nrow(hosts)) return(hosts)
  scale <- if ("host_scale_usd" %in% names(hosts)) dplyr::coalesce(hosts$host_scale_usd, 0) else rep(0, nrow(hosts))
  o <- order(-scale, -dplyr::coalesce(hosts$capability_tier, 0L), -dplyr::coalesce(hosts$capability_value, 0), hosts$host_iso)
  hosts[utils::head(o, n), , drop = FALSE]
}

# What the screen gives up, per group (`keys`: node, shock, scenario, increment): the best resilience
# gain among the screened hosts minus the best among the eligible ones, floored at 0 (an eligible host
# doing as well costs nothing), with the best screened host (ties on the gain: the larger host, by
# host_scale_usd, then ISO3) and both gains. ranked: the eligible hosts' rows; screened: the screened
# hosts' rows, simulated the same way. Groups without screened hosts get no row.
aib_alignment_cost <- function(ranked, screened, keys = c("node_id", "shock_id", "scenario", "increment"),
                               gain_col = "marginal_resilience_gain_diversified") {
  empty <- tibble::tibble(best_screened_host = character(), best_screened_gain = numeric(),
                          best_eligible_gain = numeric(), alignment_cost = numeric())
  if (is.null(screened) || !nrow(screened)) {
    # No rows, with the keys typed as in `ranked` (so the result still joins onto it).
    base <- if (!is.null(ranked) && all(keys %in% names(ranked))) tibble::as_tibble(ranked[0, keys, drop = FALSE]) else
      tibble::as_tibble(stats::setNames(rep(list(character()), length(keys)), keys))
    return(dplyr::bind_cols(base, empty))
  }
  gain <- function(t) dplyr::coalesce(t[[if (gain_col %in% names(t)) gain_col else "marginal_resilience_gain"]], 0)
  scale <- if ("host_scale_usd" %in% names(screened)) dplyr::coalesce(screened$host_scale_usd, 0) else rep(0, nrow(screened))
  s <- screened %>% dplyr::mutate(.g = gain(screened), .s = scale) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::arrange(dplyr::desc(.g), dplyr::desc(.s), host_iso, .by_group = TRUE) %>%
    dplyr::summarise(best_screened_host = host_iso[1], best_screened_gain = .g[1], .groups = "drop")
  e <- if (is.null(ranked) || !nrow(ranked)) NULL else ranked %>% dplyr::mutate(.g = gain(ranked)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>% dplyr::summarise(best_eligible_gain = max(.g), .groups = "drop")
  out <- if (is.null(e)) dplyr::mutate(s, best_eligible_gain = 0) else dplyr::left_join(s, e, by = keys)
  out$best_eligible_gain <- dplyr::coalesce(out$best_eligible_gain, 0)
  out$alignment_cost <- pmax(out$best_screened_gain - out$best_eligible_gain, 0)
  out
}
