# Geopolitical alignment between countries from UN General Assembly ideal points.
#
# Input: one row per country x UNGA session (year) with a published Bailey-Strezhnev-Voeten
# (2017) ideal point: the all-votes series (data/raw/un_ga_ideal_points_all.csv, the default) or
# the final-passage series (data/raw/un_ga_ideal_points.csv, a sensitivity), both fetched by
# scripts/fetchers/un_ideal_points.R. Pure functions; scripts/12_build_geopolitical_alignment.R
# does the IO. Needs R/utils/country.R.
#
# The measure places every country on the US-China axis of UNGA voting and scores a pair of
# countries by whether they sit in the same bloc, after the bloc taxonomy of Gopinath,
# Gourinchas, Presbitero and Topalova ("Changing global linkages: a new Cold War?", IMF WP/24/76,
# 2024; Journal of International Economics 153, 2025):
#
#   theta_i  mean of country i's ideal points over the latest K sessions in the series (K = 5,
#            config/geopolitical_distance.yml alignment$sessions). A country that voted in only
#            some of them is averaged over those; one that voted in none has no theta.
#   d_iU = |theta_i - theta_USA|,  d_iC = |theta_i - theta_CHN|  (ideal-point distances, reported)
#   s_i  = (theta_i - theta_CHN) / (theta_USA - theta_CHN), clamped to [0, 1]
#        position on the axis: 1 at the US, 0 at China. Between the anchors it equals
#        d_iC / (d_iU + d_iC) (= (1 + seg_i) / 2, seg from core.R compute_seg_scores()). Beyond an
#        anchor it is clamped: a country that votes further from the US than China does (Cuba,
#        Iran, Nicaragua, North Korea, Syria, Venezuela and Zimbabwe in the all-votes estimates for
#        2019-2023) sits at 0 with China. The distance ratio would put it back towards the
#        middle, and the quartile rule below would then call it nonaligned.
#   p_i  membership of the US side, in [0, 1] (bloc rules below)
#   A_ij = p_i p_j + (1 - p_i)(1 - p_j)
#        the probability that i and j sit on the same side: 1 = same bloc, 0 = opposite blocs,
#        0.5 when either is nonaligned (p = 0.5). Symmetric. geo_alignment = A_ij.
#
# Bloc rules (alignment$bloc_rule; L is the logistic function, tau the smoothing width on the s
# axis, alignment$tau):
# - "quartile" (default) is adapted from the Gopinath et al. rule (WP/24/76, section 3.2), with
#   proximity measured along the axis: a country is U.S.-leaning if it is in the top quartile of
#   proximity to the US (s_i >= s_US, the 75th percentile of s over the other countries),
#   China-leaning if it is in the top quartile of proximity to China (s_i <= s_CN, the 25th
#   percentile), and nonaligned otherwise (the module's assign_blocs() in core.R cuts seg the
#   same way). u_i = L((s_i - s_US) / tau) is the membership of the U.S.-leaning bloc,
#   c_i = L((s_CN - s_i) / tau) that of the China-leaning bloc, and p_i = 0.5 + 0.5 (u_i - c_i).
#   A country inside both quartiles (possible only when s_CN >= s_US) counts as nonaligned.
# - "midpoint": the side of the US-China midpoint, p_i = u_i = L((s_i - 0.5) / tau); tau = 0
#   assigns every country to the nearer anchor. The US ideal point lies far from every other
#   country's, so the midpoint sits close to the US and puts Japan, Norway, New Zealand and
#   Switzerland on China's side in recent sessions. Kept as a sensitivity.
#
# Hard blocs (tau = 0, the default): u_i, c_i in {0, 1}, so p_i is 1 (U.S.-leaning), 0.5
# (nonaligned) or 0 (China-leaning), and A_ij is 1 within a bloc, 0 between the two blocs and
# 0.5 for any pair with a nonaligned or neutral economy. tau > 0 (a sensitivity) smooths the
# memberships, and not symmetrically: the China-leaning cut lies close to China on the axis
# (s_CN 0.142 against s_US 0.511 in the all-votes series for 2019-2023), so China's own p_i is
# well above 0 (0.03 at tau = 0.05) while the US-side members reach 1. geo_alignment is used as
# it is, not ranked within the reporter.
#
# Economies without a UN vote in the window (data/reference/territory_sovereign_map.csv,
# geo_apply_no_vote_map(), added after the bloc cuts so they do not move the quartiles):
# - territories and special administrative regions whose foreign relations a UN member conducts
#   (Hong Kong and Macao: China; Puerto Rico, Guam: the United States; Greenland: Denmark;
#   Bermuda, Gibraltar: the United Kingdom; New Caledonia, Reunion: France; Aruba, Curacao: the
#   Netherlands; ...) INHERIT that member's position (alignment_basis "inherited",
#   imputed_from = the member). Left without an alignment they escaped the component while their
#   sovereign was scored on it: with China penalised, Hong Kong and Macao rose as suppliers to
#   the coalition.
# - Taiwan, Palestine and Kosovo, which have no UN vote and no sovereign that votes for them, are
#   NEUTRAL: p = 0.5, so A_ij = 0.5 with every country (alignment_basis "neutral").
# Every other row has alignment_basis "own_votes", or "none" (no vote in the window and not in
# the map: geo_alignment NA, and the Friendshore weighted mean drops the component for its
# dyads). The coalition admits only economies with their own votes.
#
# Limitations. UNGA votes measure diplomatic positions, not alliances or supply-chain security.
# The first dimension of UNGA voting is driven by recurring resolutions (the Middle East,
# decolonisation, development), so in the all-votes series for 2019-2023 US security partners in
# the Gulf and the Middle East are China-leaning (United Arab Emirates, Oman, Jordan, Egypt) or
# nonaligned (Saudi Arabia, Bahrain, Kuwait), Mexico, India, Brazil, Turkey and Switzerland are
# nonaligned, and Indonesia and Russia are China-leaning (Russia is nonaligned in the
# final-passage series for 2021-2025). The all-votes series ends in 2023, two years before the
# final-passage series (the 2026 Dataverse release updated final-passage estimates only), so
# shifts since then (the 2025 US votes, Argentina from 2024) are not in it; the K-session mean
# only smooths such shifts. Japan (s 0.545), Ireland (0.532) and New Zealand (0.519) sit close to
# the U.S.-leaning cut (0.511) and Switzerland (0.508) just below it, within the uncertainty of
# their ideal points; under hard blocs a country just across a cut moves the whole step
# (1 -> 0.5 or 0.5 -> 0).
#
# The allied coalition (derive_alignment_coalition()) is the U.S.-leaning bloc (u_i >= 0.5, the
# hard bloc) restricted to economies with GDP of at least USD 100 billion.

GEO_BLOC_LABELS <- c(us = "U.S.-leaning", china = "China-leaning", none = "Nonaligned")

# How a country got its alignment (geo_apply_no_vote_map()).
GEO_ALIGNMENT_BASIS <- c(
  own = "own_votes", inherited = "inherited", neutral = "neutral", none = "none"
)

# COW codes whose upstream rows carry a historical name and code for a current state: ccode,
# first year, ISO3. Serbia is COW 345 ("Yugoslavia", "YUG") throughout the all-votes series
# (IdealpointsJuly2025.tab), while the final-passage file names it Serbia (SRB) from 2006, the
# year Montenegro separated (Serbia is the continuator state).
GEO_COW_SUCCESSORS <- data.frame(ccode = 345L, from_year = 2006L, iso3 = "SRB", stringsAsFactors = FALSE)

# Map the ideal-point records to ISO3: the country name through the repository's country helpers
# (country_name_to_iso3()); where the name does not resolve, the file's own iso3c when it is a
# current ISO3 code (the all-votes series spells Sao Tome with replacement characters); then the
# COW successor rule (`successors`, columns ccode, from_year, iso3; needs the ccode and year
# columns). Returns the data with `iso3` (NA where nothing resolves) and `iso3_source` ("name",
# "file iso3c", "COW successor"), the unmatched records, the records resolved by a fallback, and
# any name/code disagreements (the name resolves to a different code than the file's iso3c), so
# the caller can report them. Each summary has one row per name x file code x ISO3.
geo_resolve_ideal_point_iso3 <- function(ideal_points,
                                         name_col = "country_name",
                                         iso3_col = "iso3c",
                                         session_col = "session",
                                         ccode_col = "ccode",
                                         year_col = "year",
                                         successors = GEO_COW_SUCCESSORS) {
  require_cols <- c(name_col, iso3_col, session_col)
  missing_cols <- setdiff(require_cols, names(ideal_points))
  if (length(missing_cols) > 0) {
    stop("ideal_points lacks column(s): ", paste(missing_cols, collapse = ", "))
  }

  names_in <- as.character(ideal_points[[name_col]])
  file_iso <- normalize_iso3c(ideal_points[[iso3_col]])
  iso <- country_name_to_iso3(names_in)
  source <- ifelse(is.na(iso), NA_character_, "name")

  current <- suppressWarnings(countrycode::countrycode(file_iso, "iso3c", "iso3c", warn = FALSE))
  use_file <- is.na(iso) & !is.na(current)
  iso[use_file] <- current[use_file]
  source[use_file] <- "file iso3c"

  if (!is.null(successors) && nrow(successors) > 0 && all(c(ccode_col, year_col) %in% names(ideal_points))) {
    ccode <- suppressWarnings(as.integer(ideal_points[[ccode_col]]))
    year <- suppressWarnings(as.integer(ideal_points[[year_col]]))
    for (k in seq_len(nrow(successors))) {
      hit <- is.na(iso) & !is.na(ccode) & !is.na(year) &
        ccode == as.integer(successors$ccode[k]) & year >= as.integer(successors$from_year[k])
      iso[hit] <- as.character(successors$iso3[k])
      source[hit] <- "COW successor"
    }
  }

  out <- ideal_points
  out$iso3 <- iso
  out$iso3_source <- source

  sessions <- suppressWarnings(as.integer(ideal_points[[session_col]]))
  key <- paste(names_in, file_iso, iso, sep = "\r")
  summarise_rows <- function(sel) {
    if (!any(sel)) {
      return(data.frame(
        name = character(), file_iso3 = character(), iso3 = character(), iso3_source = character(),
        first_session = integer(), last_session = integer(), n_rows = integer(),
        stringsAsFactors = FALSE
      ))
    }
    keys <- unique(key[sel])
    first <- match(keys, key)
    data.frame(
      name = names_in[first],
      file_iso3 = file_iso[first],
      iso3 = iso[first],
      iso3_source = source[first],
      first_session = vapply(keys, function(k) as.integer(min(sessions[key == k], na.rm = TRUE)), integer(1), USE.NAMES = FALSE),
      last_session = vapply(keys, function(k) as.integer(max(sessions[key == k], na.rm = TRUE)), integer(1), USE.NAMES = FALSE),
      n_rows = vapply(keys, function(k) sum(key == k), integer(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  }

  list(
    data = out,
    unmatched = summarise_rows(is.na(iso)),
    fallback = summarise_rows(!is.na(source) & source != "name"),
    disagreements = summarise_rows(!is.na(source) & source == "name" & !is.na(file_iso) & iso != file_iso)
  )
}

# The latest k sessions present in the data, ascending.
geo_latest_sessions <- function(ideal_points, k = 5L, session_col = "session") {
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1) {
    stop("k must be a single positive number of sessions.")
  }
  sessions <- sort(unique(stats::na.omit(as.integer(ideal_points[[session_col]]))))
  if (length(sessions) == 0) stop("ideal_points has no sessions.")
  utils::tail(sessions, as.integer(k))
}

# Country-level ideal points over a window of sessions: one row per ISO3 in `countries` (default:
# every resolved ISO3 in the data, any session), with theta = mean over the window sessions the
# country voted in and NA when it voted in none.
geo_country_ideal_points <- function(ideal_points,
                                     sessions = 5L,
                                     session_ids = NULL,
                                     countries = NULL,
                                     iso3_col = "iso3",
                                     session_col = "session",
                                     year_col = "year",
                                     value_col = "ideal_point",
                                     votes_col = "n_votes") {
  missing_cols <- setdiff(c(iso3_col, session_col, year_col, value_col), names(ideal_points))
  if (length(missing_cols) > 0) {
    stop("ideal_points lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (is.null(session_ids)) {
    session_ids <- geo_latest_sessions(ideal_points, sessions, session_col)
  }
  session_ids <- sort(unique(as.integer(session_ids)))

  df <- data.frame(
    iso3 = as.character(ideal_points[[iso3_col]]),
    session = as.integer(ideal_points[[session_col]]),
    year = as.integer(ideal_points[[year_col]]),
    theta = suppressWarnings(as.numeric(ideal_points[[value_col]])),
    n_votes = if (!is.null(votes_col) && votes_col %in% names(ideal_points)) {
      suppressWarnings(as.integer(ideal_points[[votes_col]]))
    } else {
      NA_integer_
    },
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$iso3), , drop = FALSE]
  if (is.null(countries)) countries <- sort(unique(df$iso3))
  countries <- unique(as.character(countries))

  window <- df[df$session %in% session_ids & !is.na(df$theta), , drop = FALSE]
  dup <- duplicated(window[c("iso3", "session")])
  if (any(dup)) {
    stop(
      "Two ideal points for one ISO3 and session (", sum(dup), "), e.g. ",
      paste(utils::head(unique(paste(window$iso3[dup], window$session[dup])), 3), collapse = "; "),
      "; check the ISO3 mapping."
    )
  }

  by_iso <- split(window, window$iso3)
  years_of <- df$year[match(session_ids, df$session)]
  out <- data.frame(
    iso3 = countries,
    theta = NA_real_,
    n_sessions = 0L,
    sessions_used = NA_character_,
    n_votes = NA_integer_,
    stringsAsFactors = FALSE
  )
  for (k in seq_along(countries)) {
    rows <- by_iso[[countries[k]]]
    if (is.null(rows) || nrow(rows) == 0) next
    out$theta[k] <- mean(rows$theta)
    out$n_sessions[k] <- nrow(rows)
    out$sessions_used[k] <- paste(sort(rows$session), collapse = ",")
    out$n_votes[k] <- if (all(is.na(rows$n_votes))) NA_integer_ else as.integer(sum(rows$n_votes, na.rm = TRUE))
  }
  out$window_sessions <- paste(session_ids, collapse = ",")
  out$window_years <- paste(stats::na.omit(years_of), collapse = ",")
  tibble::as_tibble(out)
}

# All ordered pairs i != j of the countries in `country_points` with IPD_ij = |theta_i - theta_j|
# (NA when either theta is NA).
geo_ideal_point_distance <- function(country_points) {
  missing_cols <- setdiff(c("iso3", "theta"), names(country_points))
  if (length(missing_cols) > 0) {
    stop("country_points lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (anyDuplicated(country_points$iso3)) stop("country_points has duplicate ISO3 codes.")

  n <- nrow(country_points)
  idx <- expand.grid(i = seq_len(n), j = seq_len(n))
  idx <- idx[idx$i != idx$j, , drop = FALSE]
  idx <- idx[order(idx$i, idx$j), , drop = FALSE]
  tibble::tibble(
    reporter_iso = country_points$iso3[idx$i],
    partner_iso = country_points$iso3[idx$j],
    theta_reporter = country_points$theta[idx$i],
    theta_partner = country_points$theta[idx$j],
    ipd = abs(country_points$theta[idx$i] - country_points$theta[idx$j])
  )
}

# Logistic step L(x / tau); tau = 0 is the hard step (1 above 0, 0 below, `at_zero` at 0).
geo_logistic_step <- function(x, tau, at_zero = 0.5) {
  if (!is.numeric(tau) || length(tau) != 1 || is.na(tau) || tau < 0) {
    stop("tau must be a single number >= 0.")
  }
  if (tau == 0) {
    return(ifelse(is.na(x), NA_real_, ifelse(x > 0, 1, ifelse(x < 0, 0, at_zero))))
  }
  stats::plogis(x / tau)
}

# Position of every country on the US-China axis: adds d_us = |theta - theta_US|, d_china =
# |theta - theta_CHN|, s_axis = (theta - theta_CHN) / (theta_US - theta_CHN) clamped to [0, 1],
# seg = 2 s_axis - 1 and beyond_anchor (the anchor a country lies beyond, NA between them) to
# `country_points` (iso3, theta). Both anchors need a theta.
geo_bloc_axis <- function(country_points, us_iso3 = "USA", china_iso3 = "CHN") {
  missing_cols <- setdiff(c("iso3", "theta"), names(country_points))
  if (length(missing_cols) > 0) {
    stop("country_points lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (anyDuplicated(country_points$iso3)) stop("country_points has duplicate ISO3 codes.")

  theta_us <- country_points$theta[match(us_iso3, country_points$iso3)]
  theta_china <- country_points$theta[match(china_iso3, country_points$iso3)]
  if (length(theta_us) != 1 || is.na(theta_us) || length(theta_china) != 1 || is.na(theta_china)) {
    stop("Both anchors (", us_iso3, ", ", china_iso3, ") need an ideal point in the window.")
  }
  if (theta_us == theta_china) {
    stop("The ", us_iso3, " and ", china_iso3, " ideal points coincide; the axis is undefined.")
  }

  out <- country_points
  out$d_us <- abs(out$theta - theta_us)
  out$d_china <- abs(out$theta - theta_china)
  # The projection on the segment China -> US; works whichever anchor has the higher theta.
  position <- (out$theta - theta_china) / (theta_us - theta_china)
  out$s_axis <- pmin(pmax(position, 0), 1)
  out$seg <- 2 * out$s_axis - 1
  out$beyond_anchor <- ifelse(position < 0, china_iso3, ifelse(position > 1, us_iso3, NA_character_))
  out
}

# Bloc memberships (hard at tau = 0, smoothed above) for the countries on the axis
# (geo_bloc_axis() output). rule = "quartile" (after Gopinath et al.) or "midpoint"; tau >= 0 is
# the smoothing width on the s axis; quantile is the share of countries closest to each anchor
# that forms its bloc under the quartile rule. Adds
#   u_us     membership of the U.S.-leaning bloc (>= 0.5 exactly when the hard rule puts the
#            country on the US side of its cut; under the midpoint rule a country exactly at
#            s = 0.5 has u_us = 0.5 and is nonaligned)
#   u_china  membership of the China-leaning bloc
#   p_us     US-side membership: 0.5 + 0.5 (u_us - u_china) (quartile) or u_us (midpoint)
#   bloc     the hard bloc (U.S.-leaning / China-leaning / Nonaligned)
#   bloc_rule, tau, s_us_cut, s_china_cut (the cuts on the axis; both 0.5 under the midpoint rule)
geo_bloc_membership <- function(axis,
                                rule = c("quartile", "midpoint"),
                                tau = 0,
                                quantile = 0.25,
                                us_iso3 = "USA",
                                china_iso3 = "CHN") {
  rule <- match.arg(rule)
  missing_cols <- setdiff(c("iso3", "theta", "s_axis"), names(axis))
  if (length(missing_cols) > 0) {
    stop("axis lacks column(s): ", paste(missing_cols, collapse = ", "), "; use geo_bloc_axis().")
  }
  if (!is.numeric(quantile) || length(quantile) != 1 || is.na(quantile) || quantile <= 0 || quantile >= 1) {
    stop("quantile must be a single number in (0, 1).")
  }
  if (!all(c(us_iso3, china_iso3) %in% axis$iso3[!is.na(axis$s_axis)])) {
    stop("The axis has no position for ", us_iso3, " or ", china_iso3, ".")
  }

  has_theta <- !is.na(axis$theta) & !is.na(axis$s_axis)
  s <- axis$s_axis
  if (rule == "midpoint") {
    s_us_cut <- 0.5
    s_china_cut <- 0.5
    u_us <- geo_logistic_step(s - 0.5, tau, at_zero = 0.5)
    u_china <- 1 - u_us
    p_us <- u_us
    bloc <- ifelse(s > 0.5, GEO_BLOC_LABELS[["us"]], ifelse(s < 0.5, GEO_BLOC_LABELS[["china"]], GEO_BLOC_LABELS[["none"]]))
  } else {
    # The quartiles are over the countries with a theta, the anchor itself excluded: the top
    # quartile of proximity to the US is the top quartile of s, that to China the bottom one.
    s_us_cut <- stats::quantile(s[has_theta & axis$iso3 != us_iso3], 1 - quantile, names = FALSE)
    s_china_cut <- stats::quantile(s[has_theta & axis$iso3 != china_iso3], quantile, names = FALSE)
    # The boundary itself is inside the bloc (s >= s_us_cut, s <= s_china_cut).
    u_us <- geo_logistic_step(s - s_us_cut, tau, at_zero = 1)
    u_china <- geo_logistic_step(s_china_cut - s, tau, at_zero = 1)
    p_us <- 0.5 + 0.5 * (u_us - u_china)
    in_us <- s >= s_us_cut
    in_china <- s <= s_china_cut
    bloc <- ifelse(
      in_us & !in_china, GEO_BLOC_LABELS[["us"]],
      ifelse(in_china & !in_us, GEO_BLOC_LABELS[["china"]], GEO_BLOC_LABELS[["none"]])
    )
  }
  u_us[!has_theta] <- NA_real_
  u_china[!has_theta] <- NA_real_
  p_us[!has_theta] <- NA_real_
  bloc[!has_theta] <- NA_character_

  out <- axis
  out$u_us <- u_us
  out$u_china <- u_china
  out$p_us <- p_us
  out$bloc <- unname(bloc)
  out$bloc_rule <- rule
  out$tau <- tau
  out$s_us_cut <- s_us_cut
  out$s_china_cut <- s_china_cut
  out
}

# Country-level table: ideal point over the window, axis position and bloc membership. One row
# per ISO3 (see geo_country_ideal_points() for `countries` and the column arguments).
geo_country_bloc_membership <- function(ideal_points,
                                        sessions = 5L,
                                        session_ids = NULL,
                                        countries = NULL,
                                        rule = c("quartile", "midpoint"),
                                        tau = 0,
                                        quantile = 0.25,
                                        us_iso3 = "USA",
                                        china_iso3 = "CHN",
                                        iso3_col = "iso3",
                                        session_col = "session",
                                        year_col = "year",
                                        value_col = "ideal_point",
                                        votes_col = "n_votes") {
  rule <- match.arg(rule)
  points <- geo_country_ideal_points(
    ideal_points,
    sessions = sessions,
    session_ids = session_ids,
    countries = countries,
    iso3_col = iso3_col,
    session_col = session_col,
    year_col = year_col,
    value_col = value_col,
    votes_col = votes_col
  )
  axis <- geo_bloc_axis(points, us_iso3 = us_iso3, china_iso3 = china_iso3)
  geo_bloc_membership(axis, rule = rule, tau = tau, quantile = quantile, us_iso3 = us_iso3, china_iso3 = china_iso3)
}

# The no-vote map (data/reference/territory_sovereign_map.csv), checked: one row per economy
# with iso3, basis ("inherit" or "neutral") and sovereign_iso3 (the UN member whose position an
# inheriting economy takes; empty for a neutral one). Other columns (economy, status, note) are
# kept. Returns the map with codes upper-cased and empty sovereigns as NA.
geo_validate_no_vote_map <- function(map) {
  if (!is.data.frame(map)) stop("The no-vote map must be a data frame.")
  missing_cols <- setdiff(c("iso3", "basis", "sovereign_iso3"), names(map))
  if (length(missing_cols) > 0) {
    stop("The no-vote map lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  out <- map
  out$iso3 <- normalize_iso3c(out$iso3)
  out$sovereign_iso3 <- normalize_iso3c(out$sovereign_iso3)
  out$basis <- tolower(trimws(as.character(out$basis)))
  if (any(is.na(out$iso3) | !grepl("^[A-Z]{3}$", out$iso3))) stop("The no-vote map has a missing or malformed iso3.")
  if (anyDuplicated(out$iso3)) {
    stop("The no-vote map lists an economy twice: ", paste(unique(out$iso3[duplicated(out$iso3)]), collapse = ", "))
  }
  bad_basis <- is.na(out$basis) | !out$basis %in% c("inherit", "neutral")
  if (any(bad_basis)) {
    stop("The no-vote map's basis must be inherit or neutral: ", paste(out$iso3[bad_basis], collapse = ", "))
  }
  inherit <- out$basis == "inherit"
  no_sovereign <- inherit & (is.na(out$sovereign_iso3) | !grepl("^[A-Z]{3}$", out$sovereign_iso3))
  if (any(no_sovereign)) {
    stop("Inheriting economies need a sovereign_iso3: ", paste(out$iso3[no_sovereign], collapse = ", "))
  }
  neutral_with <- !inherit & !is.na(out$sovereign_iso3)
  if (any(neutral_with)) {
    stop("Neutral economies take no sovereign_iso3: ", paste(out$iso3[neutral_with], collapse = ", "))
  }
  chained <- inherit & out$sovereign_iso3 %in% out$iso3
  if (any(chained)) {
    stop("A sovereign cannot itself be in the no-vote map: ", paste(out$iso3[chained], collapse = ", "))
  }
  out
}

# Economies without a UN vote take an alignment by the no-vote map (geo_validate_no_vote_map()):
# - inherit: a copy of the sovereign's row of `membership` (geo_bloc_membership() output: theta,
#   axis position, memberships, p_us, bloc) with imputed_from = the sovereign, n_sessions 0 and
#   sessions_used / n_votes NA (it cast no vote);
# - neutral: p_us = neutral_p (0.5 gives A_ij = 0.5 with every country), u_us = u_china = 0,
#   bloc Nonaligned, no theta or axis position.
# Adds alignment_basis (GEO_ALIGNMENT_BASIS: own_votes, inherited, neutral, none) and
# imputed_from to every row; an `economy` column of the map fills `country` for the rows it adds.
# An economy with its own vote in the window keeps it, whatever the map says; a sovereign without
# one passes no alignment on (basis none). Apply after the bloc cuts, so the imputed economies do
# not move the quartiles. derive_alignment_coalition() admits only own_votes rows.
geo_apply_no_vote_map <- function(membership, map, neutral_p = 0.5) {
  missing_cols <- setdiff(c("iso3", "theta", "p_us", "bloc"), names(membership))
  if (length(missing_cols) > 0) {
    stop("membership lacks column(s): ", paste(missing_cols, collapse = ", "), "; use geo_bloc_membership().")
  }
  if (anyDuplicated(membership$iso3)) stop("membership has duplicate ISO3 codes.")
  if (!is.numeric(neutral_p) || length(neutral_p) != 1 || is.na(neutral_p) || neutral_p < 0 || neutral_p > 1) {
    stop("neutral_p must be a single number in [0, 1].")
  }

  base <- as.data.frame(membership, stringsAsFactors = FALSE)
  own <- !is.na(base$theta) & !is.na(base$p_us)
  base$alignment_basis <- ifelse(own, GEO_ALIGNMENT_BASIS[["own"]], GEO_ALIGNMENT_BASIS[["none"]])
  base$imputed_from <- NA_character_
  if (is.null(map) || nrow(map) == 0) return(tibble::as_tibble(base))

  map <- geo_validate_no_vote_map(map)
  map <- map[!map$iso3 %in% base$iso3[own], , drop = FALSE]
  if (nrow(map) == 0) return(tibble::as_tibble(base))
  unknown <- setdiff(map$sovereign_iso3[map$basis == "inherit"], base$iso3)
  if (length(unknown) > 0) {
    stop("Sovereign(s) of the no-vote map not in the membership table: ", paste(unknown, collapse = ", "))
  }

  # New rows start as all-NA copies of the table's columns; the window and rule settings are
  # the same for every row.
  added <- base[rep(NA_integer_, nrow(map)), , drop = FALSE]
  rownames(added) <- NULL
  constant_cols <- intersect(c("window_sessions", "window_years", "bloc_rule", "tau", "s_us_cut", "s_china_cut"), names(base))
  for (col in constant_cols) added[[col]] <- base[[col]][1]

  inherit <- map$basis == "inherit"
  if (any(inherit)) {
    src <- base[match(map$sovereign_iso3[inherit], base$iso3), , drop = FALSE]
    copy_cols <- setdiff(names(base), c("iso3", "country", "n_sessions", "sessions_used", "n_votes", "alignment_basis", "imputed_from"))
    for (col in copy_cols) added[[col]][inherit] <- src[[col]]
    passed_on <- !is.na(src$theta) & !is.na(src$p_us)
    added$alignment_basis[inherit] <- ifelse(passed_on, GEO_ALIGNMENT_BASIS[["inherited"]], GEO_ALIGNMENT_BASIS[["none"]])
    added$imputed_from[inherit] <- map$sovereign_iso3[inherit]
  }
  neutral <- map$basis == "neutral"
  if (any(neutral)) {
    added$p_us[neutral] <- neutral_p
    if ("u_us" %in% names(added)) added$u_us[neutral] <- 0
    if ("u_china" %in% names(added)) added$u_china[neutral] <- 0
    added$bloc[neutral] <- GEO_BLOC_LABELS[["none"]]
    added$alignment_basis[neutral] <- GEO_ALIGNMENT_BASIS[["neutral"]]
  }
  added$iso3 <- map$iso3
  if ("n_sessions" %in% names(added)) added$n_sessions <- 0L
  if ("country" %in% names(added)) {
    added$country <- if ("economy" %in% names(map)) as.character(map$economy) else NA_character_
  }

  kept <- base[!base$iso3 %in% map$iso3, , drop = FALSE]
  tibble::as_tibble(rbind(kept, added))
}

# Dyadic alignment from a country membership table (geo_bloc_membership() or
# geo_apply_no_vote_map() output): one row per ordered pair (reporter_iso, partner_iso),
# reporter != partner, with theta_reporter, theta_partner, ipd, s_reporter, s_partner,
# p_reporter, p_partner, bloc_reporter, bloc_partner, basis_reporter / basis_partner (the
# alignment_basis, when the table has one) and geo_alignment = p_i p_j + (1 - p_i)(1 - p_j) (NA
# when either p is NA).
geo_dyadic_alignment <- function(membership) {
  missing_cols <- setdiff(c("iso3", "theta", "s_axis", "p_us", "bloc"), names(membership))
  if (length(missing_cols) > 0) {
    stop("membership lacks column(s): ", paste(missing_cols, collapse = ", "), "; use geo_bloc_membership().")
  }
  pairs <- geo_ideal_point_distance(membership)
  i <- match(pairs$reporter_iso, membership$iso3)
  j <- match(pairs$partner_iso, membership$iso3)
  p_i <- membership$p_us[i]
  p_j <- membership$p_us[j]

  pairs$s_reporter <- membership$s_axis[i]
  pairs$s_partner <- membership$s_axis[j]
  pairs$p_reporter <- p_i
  pairs$p_partner <- p_j
  pairs$bloc_reporter <- membership$bloc[i]
  pairs$bloc_partner <- membership$bloc[j]
  if ("alignment_basis" %in% names(membership)) {
    pairs$basis_reporter <- membership$alignment_basis[i]
    pairs$basis_partner <- membership$alignment_basis[j]
  }
  pairs$geo_alignment <- p_i * p_j + (1 - p_i) * (1 - p_j)
  for (col in intersect(c("bloc_rule", "tau", "window_sessions", "window_years"), names(membership))) {
    pairs[[col]] <- membership[[col]][1]
  }
  pairs
}

# Dyadic alignment index for the Friendshore theme, straight from the ideal-point records: the
# country table of geo_country_bloc_membership(), the no-vote map applied when given
# (geo_apply_no_vote_map()), turned into pairs by geo_dyadic_alignment(). geo_alignment is in
# [0, 1], higher = more aligned, symmetric, NA when either country has no alignment.
compute_alignment_from_ideal_points <- function(ideal_points,
                                                sessions = 5L,
                                                session_ids = NULL,
                                                countries = NULL,
                                                rule = c("quartile", "midpoint"),
                                                tau = 0,
                                                quantile = 0.25,
                                                us_iso3 = "USA",
                                                china_iso3 = "CHN",
                                                no_vote_map = NULL,
                                                neutral_p = 0.5,
                                                iso3_col = "iso3",
                                                session_col = "session",
                                                year_col = "year",
                                                value_col = "ideal_point",
                                                votes_col = "n_votes") {
  rule <- match.arg(rule)
  membership <- geo_country_bloc_membership(
    ideal_points,
    sessions = sessions,
    session_ids = session_ids,
    countries = countries,
    rule = rule,
    tau = tau,
    quantile = quantile,
    us_iso3 = us_iso3,
    china_iso3 = china_iso3,
    iso3_col = iso3_col,
    session_col = session_col,
    year_col = year_col,
    value_col = value_col,
    votes_col = votes_col
  )
  if (!is.null(no_vote_map)) {
    membership <- geo_apply_no_vote_map(membership, no_vote_map, neutral_p = neutral_p)
  }
  geo_dyadic_alignment(membership)
}

# Latest non-missing GDP per ISO3 from a WDI-style long table (default columns as in
# data/raw/wdi_gdp.csv: iso3c, year, NY.GDP.MKTP.CD, current USD). One row per ISO3: iso3,
# gdp_usd, gdp_year. Two different values for one ISO3 and year are an error.
geo_latest_gdp <- function(gdp_tbl,
                           iso3_col = "iso3c",
                           year_col = "year",
                           value_col = "NY.GDP.MKTP.CD") {
  missing_cols <- setdiff(c(iso3_col, year_col, value_col), names(gdp_tbl))
  if (length(missing_cols) > 0) {
    stop("gdp_tbl lacks column(s): ", paste(missing_cols, collapse = ", "))
  }
  df <- data.frame(
    iso3 = toupper(trimws(as.character(gdp_tbl[[iso3_col]]))),
    year = suppressWarnings(as.integer(gdp_tbl[[year_col]])),
    gdp_usd = suppressWarnings(as.numeric(gdp_tbl[[value_col]])),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$iso3) & nzchar(df$iso3) & !is.na(df$year) & !is.na(df$gdp_usd), , drop = FALSE]
  df <- unique(df)
  dup <- duplicated(df[c("iso3", "year")])
  if (any(dup)) {
    stop("gdp_tbl has two GDP values for one ISO3 and year, e.g. ", df$iso3[dup][1], " ", df$year[dup][1], ".")
  }
  df <- df[order(df$iso3, -df$year), , drop = FALSE]
  df <- df[!duplicated(df$iso3), , drop = FALSE]
  rownames(df) <- NULL
  tibble::tibble(iso3 = df$iso3, gdp_usd = df$gdp_usd, gdp_year = df$year)
}

# The allied coalition derived from the alignment: the U.S.-leaning bloc (u_us >= p_threshold; at
# 0.5 this is the hard bloc of the rule, whatever tau) restricted to economies whose latest
# non-missing GDP is at least gdp_floor_usd (current USD). `bloc_tbl` is the country table of
# geo_bloc_membership() / geo_country_bloc_membership() (an optional `country` column carries
# the names); `gdp_tbl` a WDI-style GDP table (see geo_latest_gdp() for the column arguments).
# p_us is not the criterion: under the quartile rule with tau > 0 a nonaligned country on the US
# half of the axis has p_us > 0.5. Under the midpoint rule u_us = p_us. Only economies with their
# own UN votes can be members: a row of geo_apply_no_vote_map() with another alignment_basis
# (inherited, neutral) never is.
#
# Returns one row per country of bloc_tbl: iso3, country, theta, s_axis, p_us, u_us, bloc,
# gdp_usd, gdp_year, member (TRUE/FALSE) and reason; members first, then by descending s_axis.
derive_alignment_coalition <- function(bloc_tbl,
                                       gdp_tbl,
                                       p_threshold = 0.5,
                                       gdp_floor_usd = 1e11,
                                       gdp_iso3_col = "iso3c",
                                       gdp_year_col = "year",
                                       gdp_value_col = "NY.GDP.MKTP.CD") {
  missing_cols <- setdiff(c("iso3", "theta", "s_axis", "p_us", "u_us", "bloc"), names(bloc_tbl))
  if (length(missing_cols) > 0) {
    stop("bloc_tbl lacks column(s): ", paste(missing_cols, collapse = ", "), "; use geo_bloc_membership().")
  }
  if (anyDuplicated(bloc_tbl$iso3)) stop("bloc_tbl has duplicate ISO3 codes.")
  if (!is.numeric(p_threshold) || length(p_threshold) != 1 || is.na(p_threshold) || p_threshold <= 0 || p_threshold > 1) {
    stop("p_threshold must be a single number in (0, 1].")
  }
  if (!is.numeric(gdp_floor_usd) || length(gdp_floor_usd) != 1 || is.na(gdp_floor_usd) || gdp_floor_usd < 0) {
    stop("gdp_floor_usd must be a single number >= 0.")
  }

  gdp <- geo_latest_gdp(gdp_tbl, iso3_col = gdp_iso3_col, year_col = gdp_year_col, value_col = gdp_value_col)
  k <- match(bloc_tbl$iso3, gdp$iso3)
  gdp_usd <- gdp$gdp_usd[k]
  gdp_year <- gdp$gdp_year[k]

  window <- ""
  if ("window_years" %in% names(bloc_tbl)) {
    yrs <- unique(stats::na.omit(as.character(bloc_tbl$window_years)))
    if (length(yrs) == 1 && nzchar(yrs)) window <- paste0(" in ", gsub(",", ", ", yrs))
  }
  usd_bn <- function(x) paste0("USD ", formatC(x / 1e9, format = "f", digits = 1, big.mark = ","), "bn")
  floor_txt <- usd_bn(gdp_floor_usd)

  basis <- if ("alignment_basis" %in% names(bloc_tbl)) as.character(bloc_tbl$alignment_basis) else rep(NA_character_, nrow(bloc_tbl))
  imputed_from <- if ("imputed_from" %in% names(bloc_tbl)) as.character(bloc_tbl$imputed_from) else rep(NA_character_, nrow(bloc_tbl))
  imputed <- !is.na(basis) & basis != GEO_ALIGNMENT_BASIS[["own"]]
  has_vote <- !imputed & !is.na(bloc_tbl$theta) & !is.na(bloc_tbl$u_us)
  window <- ifelse(
    !is.na(basis) & basis == GEO_ALIGNMENT_BASIS[["inherited"]], paste0(window, "; inherits ", imputed_from, "'s position"),
    ifelse(!is.na(basis) & basis == GEO_ALIGNMENT_BASIS[["neutral"]], paste0(window, "; neutral"), window)
  )
  in_bloc <- has_vote & bloc_tbl$u_us >= p_threshold
  big <- !is.na(gdp_usd) & gdp_usd >= gdp_floor_usd
  member <- in_bloc & big

  bloc_txt <- ifelse(is.na(bloc_tbl$bloc), "no bloc", bloc_tbl$bloc)
  gdp_txt <- ifelse(is.na(gdp_usd), "", paste0(usd_bn(gdp_usd), " (", gdp_year, ")"))
  reason <- ifelse(
    !has_vote, paste0("no UN vote", window),
    ifelse(
      !in_bloc, sprintf("%s (u_us %.3f < %s)", bloc_txt, bloc_tbl$u_us, format(p_threshold)),
      ifelse(
        is.na(gdp_usd), paste0(bloc_txt, "; no WDI GDP"),
        ifelse(
          !big, paste0(bloc_txt, "; GDP ", gdp_txt, " < ", floor_txt),
          paste0(bloc_txt, "; GDP ", gdp_txt, " >= ", floor_txt)
        )
      )
    )
  )

  out <- tibble::tibble(
    iso3 = bloc_tbl$iso3,
    country = if ("country" %in% names(bloc_tbl)) as.character(bloc_tbl$country) else NA_character_,
    theta = bloc_tbl$theta,
    s_axis = bloc_tbl$s_axis,
    p_us = bloc_tbl$p_us,
    u_us = bloc_tbl$u_us,
    bloc = bloc_tbl$bloc,
    gdp_usd = gdp_usd,
    gdp_year = gdp_year,
    member = member,
    reason = reason
  )
  out[order(!out$member, -ifelse(is.na(out$s_axis), -Inf, out$s_axis), out$iso3), , drop = FALSE]
}
