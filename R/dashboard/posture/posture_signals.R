# Landing signals (docs/change-layer-implementation-plan.md, Task 7): two current-state trend questions for the
# Explorer's Home page, built at export time from the strategic position table so Home reads one small file instead
# of the posture core and 214 drawer documents.
#
#   exports      "Where are exports growing?" Log export growth over the latest 12 months against the 12 before
#                (trade_momentum_raw, scripts/26 trade momentum), only where the momentum quality is "ok" (complete
#                24 months, enough coverage, not a small base: the tiny-base growth trap). The cells moving more than
#                the band are listed by world export share, largest first: the growth measure is winsorised at its
#                5th / 95th percentiles, so ranking by it would tie dozens of cells at the ceiling. A value at a
#                winsorisation bound is flagged `capped` ("+101% or more").
#   dependence   "Where is dependence on China rising?" China's share of the cell's imports in the latest 12
#                months against the base year (china_share_t12m vs china_share_base; current_trade_flag china_up:
#                10+ points on $100m+ of imports), with the coercive-supplier share beside it.
#
# These are trends in measured trade, not changes in the assessment (that is the change layer's job) and not a
# score. Pure: returns a list for jsonlite; scripts/91 writes it.

POSTURE_SIGNALS_SCHEMA <- "osi-posture-signals"
POSTURE_SIGNALS_VERSION <- 1L

posture_signals_doc <- function(tbl, techs, countries, treat_as_allied = character(), created, git_commit, top = 10L,
                                meta = list()) {
  col <- function(n, d = NA) if (n %in% names(tbl)) tbl[[n]] else rep(d, nrow(tbl))
  x <- tbl[!is.na(col("strategic_posture", NA_character_)) & tbl$tech %in% techs, , drop = FALSE]
  colx <- function(n, d = NA) if (n %in% names(x)) x[[n]] else rep(d, nrow(x))
  tier <- countries$tier[match(x$iso3, countries$iso3)]
  trusted <- dplyr::coalesce(tier %in% c("core", "partner"), FALSE) | x$iso3 %in% toupper(treat_as_allied)
  r3 <- function(v) round(as.numeric(v), 3)
  cname <- if ("name" %in% names(countries)) as.character(countries$name[match(x$iso3, countries$iso3)]) else rep(NA_character_, nrow(x))
  cname <- dplyr::coalesce(cname, x$iso3)
  row <- function(i, extra) c(list(iso3 = x$iso3[i], name = cname[i], tech = x$tech[i], stage = x$supply_chain[i], trusted = trusted[i]), extra)

  # exports: complete, adequately covered, not a small base
  g <- as.numeric(colx("trade_momentum_raw"))
  ok <- !is.na(g) & colx("trade_momentum_quality", NA_character_) %in% "ok"
  band <- meta$momentum_band %||% 0.1
  share <- as.numeric(dplyr::coalesce(colx("home_share_exposure"), colx("home_share")))
  hi <- if (any(ok)) max(g[ok]) else NA_real_
  lo <- if (any(ok)) min(g[ok]) else NA_real_
  growth_rows <- function(idx) lapply(idx, function(i) row(i, list(growth = r3(exp(g[i]) - 1), log_growth = r3(g[i]), world_share = r3(share[i]),
                                                                  capped = isTRUE(abs(g[i] - hi) < 1e-9 || abs(g[i] - lo) < 1e-9))))
  up <- which(ok & g > band)
  down <- which(ok & g < -band)
  by_share <- function(idx) idx[order(-dplyr::coalesce(share[idx], -1), -abs(g[idx]))]
  exports <- list(
    period = meta$momentum_period %||% NA_character_, previous_period = meta$momentum_previous_period %||% NA_character_,
    band = band, n_assessed = sum(ok), n_up = length(up), n_down = length(down),
    n_up_trusted = sum(trusted[up]), n_down_trusted = sum(trusted[down]),
    growing = growth_rows(utils::head(by_share(up), top)),
    growing_trusted = growth_rows(utils::head(by_share(up[trusted[up]]), top)),
    falling_trusted = growth_rows(utils::head(by_share(down[trusted[down]]), top))
  )

  # dependence: China's import share, latest 12 months vs base year
  cb <- as.numeric(colx("china_share_base")); cl <- as.numeric(colx("china_share_t12m"))
  kb <- as.numeric(colx("coercive_import_share_base")); kl <- as.numeric(colx("coercive_import_share_t12m"))
  flag <- colx("current_trade_flag", NA_character_)
  rise <- which(flag %in% "china_up")
  fall <- which(flag %in% "china_down")
  dep_rows <- function(idx) lapply(idx, function(i) row(i, list(china_base = r3(cb[i]), china_latest = r3(cl[i]), coercive_base = r3(kb[i]),
                                                               coercive_latest = r3(kl[i]), source = colx("exposure_current_source", NA_character_)[i])))
  ord_rise <- function(idx) idx[order(-(cl[idx] - cb[idx]))]
  dependence <- list(
    window_end = suppressWarnings(max(as.numeric(colx("exposure_window_end")), na.rm = TRUE)),
    n_refreshed = sum(!is.na(cl)), n_china_up = length(rise), n_china_down = length(fall),
    n_china_up_trusted = sum(trusted[rise]), n_china_down_trusted = sum(trusted[fall]),
    rising_trusted = dep_rows(utils::head(ord_rise(rise[trusted[rise]]), top)),
    rising = dep_rows(utils::head(ord_rise(rise), top))
  )
  if (!is.finite(dependence$window_end)) dependence$window_end <- NA_real_
  list(schema = POSTURE_SIGNALS_SCHEMA, schema_version = POSTURE_SIGNALS_VERSION, created = created, git_commit = git_commit,
       exports = exports, dependence = dependence)
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
