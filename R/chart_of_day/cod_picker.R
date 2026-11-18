# Chart of the Day picker (pure): candidate pool, weights, cooldowns and a date-seeded weighted draw.

#' All candidates from every template, with their interest scores.
cod_candidates <- function(d, cfg) {
  all_t <- c(COD_TEMPLATES, if (exists("COD_TEMPLATES_MORE")) COD_TEMPLATES_MORE)
  tmpl <- intersect(names(cfg$templates), all_t)
  out <- lapply(tmpl, function(t) get(paste0("cod_cand_", t))(d, cfg))
  out <- do.call(rbind, out)
  out$key <- paste(out$template, out$variable, out$measure, out$tech, out$stage, out$iso3, sep = ":")
  out
}

#' Focus-country weights: named weight, else coalition tier; excluded and small economies get 0.
cod_country_weights <- function(iso3, countries, cfg) {
  cc <- cfg$countries
  w <- unlist(cc$tier)[countries$tier[match(iso3, countries$iso3)]]
  w[is.na(w)] <- 0
  named <- unlist(cc$named)
  hit <- iso3 %in% names(named)
  w[hit] <- named[iso3[hit]]
  gdp <- countries$gdp_usd[match(iso3, countries$iso3)]
  w[!hit & (is.na(gdp) | gdp < cc$min_gdp_usd)] <- 0
  w[iso3 %in% unlist(cc$exclude)] <- 0
  unname(as.numeric(w))
}

cod_lookup <- function(x, table, default = 1) {
  v <- unlist(table)[x]
  v[is.na(v)] <- default
  unname(as.numeric(v))
}

#' Cooldown factors for each candidate given a history of earlier picks (data frame: date, key, template, iso3, tech):
#' a data frame with one column per rule (chart, country, template, tech), each 1 or the rule's factor.
cod_cooldown <- function(cand, history, date, cfg) {
  one <- rep(1, nrow(cand))
  out <- data.frame(chart = one, country = one, template = one, tech = one)
  if (is.null(history) || !nrow(history)) return(out)
  age <- as.numeric(as.Date(date) - as.Date(history$date))
  h <- history[age > 0, , drop = FALSE]; age <- age[age > 0]
  cd <- cfg$cooldown
  rule <- function(field_c, field_h, r) {
    recent <- h[[field_h]][age <= r$days]
    ifelse(cand[[field_c]] %in% recent, r$factor, 1)
  }
  out$chart <- rule("key", "key", cd$same_chart)
  out$country <- rule("iso3", "iso3", cd$same_country)
  out$template <- rule("template", "template", cd$same_template)
  out$tech <- ifelse(cand$tech == "All", 1, rule("tech", "tech", cd$same_tech))
  out
}

#' Scale `w` so each group's total equals that group's total in `ref` (groups with nothing left stay at 0).
cod_rescale <- function(w, ref, group) {
  t1 <- stats::ave(ref, group, FUN = sum); t2 <- stats::ave(w, group, FUN = sum)
  ifelse(t2 > 0, w / t2 * t1, 0)
}

#' Draw weights for the pool. Within each template, a country's candidates share that country's weight (spread by
#' technology, stage, variable and interest), so country weights act as odds whatever the number of candidates;
#' the vintage factor (fresh vs 2024 data) and the template weight apply on top. Cooldowns then apply in layers so
#' they never tilt the mix by accident: a used chart or a recent technology moves weight to the same country's other
#' charts of that kind; a recent country moves weight to other countries within the kind of chart; only a recent
#' kind of chart lowers that kind's share.
cod_weights <- function(cand, d, cfg, history = NULL, date = NULL) {
  inner <- cod_lookup(cand$tech, cfg$technologies, 1) *
    cod_lookup(cand$stage, cfg$stages, 1) *
    ifelse(cand$template == "leaderboard", cod_lookup(cand$variable, lapply(cfg$leaderboard_variables, `[[`, "weight"), 0), 1) *
    cand$interest
  grp <- paste(cand$template, cand$iso3)
  tot <- stats::ave(inner, grp, FUN = sum)
  inner <- ifelse(tot > 0, inner / tot, 0)
  vint <- ifelse(cand$current, cod_lookup("current", cfg$vintage, 1), cod_lookup("annual", cfg$vintage, 1))
  w <- inner * cod_country_weights(cand$iso3, d$countries, cfg) * vint
  tt <- stats::ave(w, cand$template, FUN = sum)
  w <- ifelse(tt > 0, w / tt * cod_lookup(cand$template, cfg$templates, 0), 0)
  if (!is.null(date)) {
    cd <- cod_cooldown(cand, history, date, cfg)
    w1 <- cod_rescale(w * cd$chart * cd$tech, w, grp)
    # a country whose charts of this kind are all used up loses its share of the kind
    w2 <- cod_rescale(w1 * cd$country, w, cand$template)
    w <- w2 * cd$template
  }
  w
}

#' A uniform number in [0, 1) from a date and a salt: R's Mersenne-Twister seeded by the date (independent from one
#' day to the next; a hand-rolled LCG here was correlated across neighbouring dates and skewed the mix). The caller's
#' RNG state is restored, so the function has no side effect.
cod_uniform <- function(date, salt = 0L) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()) else rm(".Random.seed", envir = globalenv()))
  set.seed(as.integer(as.Date(date)) * 16L + as.integer(salt), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  stats::runif(1)
}

#' Pick one candidate for `date`. Returns the row with its weight and probability.
cod_pick <- function(cand, w, date, salt = 0L) {
  if (!any(w > 0)) stop("no candidate has a positive weight")
  p <- w / sum(w)
  i <- which(cumsum(p) > cod_uniform(date, salt))[1]
  out <- cand[i, , drop = FALSE]
  out$weight <- w[i]; out$probability <- p[i]; out$pool <- sum(w > 0)
  out
}

#' Build the chart spec for a picked candidate.
cod_build <- function(c, d, cfg) {
  spec <- get(paste0("cod_build_", c$template))(c, d, cfg)
  # charts on annual 2024 trade (world rankings, the trade map) say so too
  if (is.null(spec$period) && (c$template == "trade_map" || grepl("^(trade_|mineral_exports)", c$variable)))
    spec$period <- as.character(d$trade_year)
  spec$pick <- list(probability = signif(c$probability, 3), pool = c$pool, interest = c$interest)
  spec
}

#' Explorer deep link for a spec.
cod_explore_href <- function(spec) {
  slug <- gsub(" ", "-", tolower(spec$tech))
  if (spec$tech %in% names(COD_TECH_NOUN)) {
    if (spec$stage %in% COD_STAGES) return(sprintf("/sector/%s/%s", slug, tolower(spec$stage)))
    return(sprintf("/sector/%s", slug))
  }
  if (spec$tech %in% COD_MINERALS) return("/sector/minerals")
  sprintf("/countries/%s", spec$focus$iso3)
}

#' Pick and build charts for a run of dates, carrying the history forward so cooldowns apply within the run.
#' `rebuild` lets a failing build (e.g. thin data) fall through to the next draw for that date.
cod_run <- function(d, cfg, dates, history = NULL) {
  cand <- cod_candidates(d, cfg)
  out <- list()
  for (dt in as.character(dates)) {
    w <- cod_weights(cand, d, cfg, history, dt)
    spec <- NULL
    for (salt in 0:9) {
      c <- cod_pick(cand, w, dt, salt)
      spec <- tryCatch(cod_build(c, d, cfg), error = function(e) NULL)
      if (!is.null(spec)) break
      w[cand$key == c$key] <- 0
    }
    if (is.null(spec)) stop("no buildable chart for ", dt)
    spec$date <- dt
    spec$explore <- cod_explore_href(spec)
    out[[dt]] <- spec
    history <- rbind(history, data.frame(date = dt, key = spec$key, template = spec$template, iso3 = spec$focus$iso3,
                                         tech = spec$tech, stringsAsFactors = FALSE))
  }
  out
}
