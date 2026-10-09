# UN-voting alignment for the Friendshore theme (R/geopolitical_distance/alignment.R): the US-China
# bloc axis, bloc membership (hard by default, smoothed as a sensitivity), the dyadic same-bloc
# probability, economies without a UN vote and the coalition derived from the blocs.

geo_repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(geo_repo_root, "R", "utils", "country.R"))
source(file.path(geo_repo_root, "R", "geopolitical_distance", "core.R"))
source(file.path(geo_repo_root, "R", "geopolitical_distance", "alignment.R"))

# Fixture: ideal points for sessions 9-12. The window is the latest three (10-12); session 9 is
# older and must be ignored. TWN votes only in session 9; PRT only in session 12. Theta varies
# by session for USA and JPN so the window mean is tested. IRN (-1.3) and SYR (-2.6) vote
# further from the US than China (-1.0) does; SYR is far enough beyond China that its distance
# to China (1.6) is larger than most countries'.
geo_fixture_points <- function() {
  base <- c(
    USA = 2.0, GBR = 1.4, JPN = 1.2, DEU = 1.0, KOR = 0.9, MEX = 0.1, BRA = 0.0, IND = -0.1,
    ZAF = -0.3, EQD = 0.5, RUS = -0.8, VNM = -0.95, CHN = -1.0, IRN = -1.3, SYR = -2.6
  )
  rows <- expand.grid(iso3 = names(base), session = 10:12, stringsAsFactors = FALSE)
  rows$ideal_point <- unname(base[rows$iso3])
  # USA 1.9 / 2.0 / 2.1 and JPN 1.1 / 1.2 / 1.3: the window means stay 2.0 and 1.2.
  rows$ideal_point[rows$iso3 == "USA"] <- c(1.9, 2.0, 2.1)
  rows$ideal_point[rows$iso3 == "JPN"] <- c(1.1, 1.2, 1.3)
  rows <- rbind(
    rows,
    data.frame(iso3 = c("USA", "CHN", "TWN"), session = 9L, ideal_point = c(-5, 5, 1.5)),
    data.frame(iso3 = "PRT", session = 12L, ideal_point = 1.1)
  )
  rows$year <- 1945L + rows$session
  rows$n_votes <- 50L
  rows
}

# WDI-style GDP (current USD) for the coalition rule. JPN's latest year is missing, so its 2024
# value is used; PRT is below the USD 100bn floor; DEU has no GDP row at all.
geo_fixture_gdp <- function() {
  data.frame(
    country = c("United States", "United States", "United Kingdom", "Japan", "Japan", "Portugal", "Korea", "China", "Brazil"),
    iso3c = c("USA", "USA", "GBR", "JPN", "JPN", "PRT", "KOR", "CHN", "BRA"),
    year = c(2024L, 2025L, 2025L, 2024L, 2025L, 2025L, 2025L, 2025L, 2025L),
    `NY.GDP.MKTP.CD` = c(2.9e13, 3.0e13, 3.6e12, 4.0e12, NA, 9.0e10, 1.8e12, 1.9e13, 2.2e12),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("the window is the latest K sessions and a country is averaged over the sessions it voted in", {
  pts <- geo_fixture_points()
  expect_equal(geo_latest_sessions(pts, 3), 10:12)
  expect_equal(geo_latest_sessions(pts, 5), 9:12)

  cp <- geo_country_ideal_points(pts, sessions = 3)
  theta <- stats::setNames(cp$theta, cp$iso3)
  expect_equal(theta[["USA"]], 2.0)
  expect_equal(theta[["JPN"]], 1.2)
  expect_equal(theta[["CHN"]], -1.0)
  # Partial coverage: PRT voted only in session 12.
  expect_equal(theta[["PRT"]], 1.1)
  expect_equal(cp$n_sessions[cp$iso3 == "PRT"], 1L)
  # No vote in the window: NA, not 0.
  expect_true(is.na(theta[["TWN"]]))
  expect_equal(cp$n_sessions[cp$iso3 == "TWN"], 0L)
  expect_equal(unique(cp$window_sessions), "10,11,12")
  expect_equal(unique(cp$window_years), "1955,1956,1957")

  dup <- rbind(pts, pts[pts$iso3 == "USA" & pts$session == 12, ])
  expect_error(geo_country_ideal_points(dup, sessions = 3), "Two ideal points")
})

test_that("the axis runs from China (0) to the US (1), matches seg between the anchors and clamps beyond them", {
  cp <- geo_country_ideal_points(geo_fixture_points(), sessions = 3)
  axis <- geo_bloc_axis(cp)
  s <- stats::setNames(axis$s_axis, axis$iso3)
  expect_equal(s[["USA"]], 1)
  expect_equal(s[["CHN"]], 0)
  # EQD (0.5) is equidistant from USA (2.0) and CHN (-1.0).
  expect_equal(s[["EQD"]], 0.5)
  expect_true(all(s[!is.na(s)] >= 0 & s[!is.na(s)] <= 1))
  expect_true(is.na(s[["TWN"]]))
  # Closer to the US -> higher s, between the anchors.
  expect_gt(s[["JPN"]], s[["BRA"]])
  expect_gt(s[["BRA"]], s[["VNM"]])

  # Beyond China: clamped to China's position and flagged. The distance ratio d_C / (d_U + d_C)
  # would put SYR at 1.6 / 6.2 = 0.26, nearer the middle than RUS (0.07).
  expect_equal(s[["IRN"]], 0)
  expect_equal(s[["SYR"]], 0)
  expect_equal(axis$beyond_anchor[axis$iso3 %in% c("IRN", "SYR")], c("CHN", "CHN"))
  expect_true(all(is.na(axis$beyond_anchor[!axis$iso3 %in% c("IRN", "SYR")])))
  expect_equal(axis$d_china[axis$iso3 == "SYR"], 1.6)

  # Between the anchors s = (1 + seg) / 2, seg = (d_C - d_U) / (d_U + d_C) from compute_seg_scores().
  ipd <- geo_ideal_point_distance(cp[!is.na(cp$theta), ])
  ipd_df <- data.frame(
    iso3_i = ipd$reporter_iso, iso3_j = ipd$partner_iso, year = 2020, ipd = ipd$ipd, spec_name = "s"
  )
  seg <- compute_seg_scores(ipd_df, data.frame(iso3 = cp$iso3, country = cp$iso3), 2020, "s")
  seg <- seg[!seg$iso3 %in% c("USA", "CHN", "IRN", "SYR"), ]
  expect_equal(unname(s[seg$iso3]), (1 + seg$seg) / 2)

  # The orientation of the anchors does not matter.
  flipped <- cp
  flipped$theta <- -flipped$theta
  expect_equal(geo_bloc_axis(flipped)$s_axis, axis$s_axis)

  no_anchor <- cp[cp$iso3 != "CHN", ]
  expect_error(geo_bloc_axis(no_anchor), "anchors")
})

test_that("the midpoint rule is the logistic of s - 0.5 and tau = 0 assigns the nearer anchor", {
  axis <- geo_bloc_axis(geo_country_ideal_points(geo_fixture_points(), sessions = 3))
  m <- geo_bloc_membership(axis, rule = "midpoint", tau = 0.05)
  ok <- !is.na(m$theta)
  expect_equal(m$p_us[ok], stats::plogis((m$s_axis[ok] - 0.5) / 0.05))
  expect_equal(m$u_us, m$p_us)

  hard <- geo_bloc_membership(axis, rule = "midpoint", tau = 0)
  p <- stats::setNames(hard$p_us, hard$iso3)
  expect_equal(p[["JPN"]], 1)
  expect_equal(p[["BRA"]], 0)
  expect_equal(p[["SYR"]], 0)
  expect_equal(p[["EQD"]], 0.5)
  expect_equal(hard$bloc[hard$iso3 == "EQD"], "Nonaligned")
  expect_true(is.na(p[["TWN"]]))
  expect_error(geo_bloc_membership(axis, rule = "midpoint", tau = -1), "tau")
})

test_that("the quartile rule reproduces the Gopinath et al. blocs along the axis at tau = 0", {
  cp <- geo_country_ideal_points(geo_fixture_points(), sessions = 3)
  axis <- geo_bloc_axis(cp)
  hard <- geo_bloc_membership(axis, rule = "quartile", tau = 0, quantile = 0.25)

  # Hand-computed: the quarter of the other voting countries closest to each anchor on the axis.
  voting <- axis[!is.na(axis$theta), ]
  s <- stats::setNames(voting$s_axis, voting$iso3)
  s_us <- stats::quantile(s[names(s) != "USA"], 0.75, names = FALSE)
  s_cn <- stats::quantile(s[names(s) != "CHN"], 0.25, names = FALSE)
  expect_equal(s_us, 0.65)
  expect_equal(s_cn, 0.15)
  expect_equal(hard$s_us_cut[1], s_us)
  expect_equal(hard$s_china_cut[1], s_cn)
  expected <- ifelse(s >= s_us, "U.S.-leaning", ifelse(s <= s_cn, "China-leaning", "Nonaligned"))
  got <- stats::setNames(hard$bloc, hard$iso3)[names(expected)]
  expect_equal(unname(got), unname(expected))

  # The fixture's blocs (16 voting countries): the US and its four nearest, China and its four
  # nearest on the axis, the rest nonaligned. SYR is China-leaning although its ideal point is
  # 1.6 from China's; ranking raw distances to China would have left it nonaligned and put ZAF
  # in China's quartile.
  expect_setequal(names(expected)[expected == "U.S.-leaning"], c("USA", "GBR", "JPN", "PRT", "DEU"))
  expect_setequal(names(expected)[expected == "China-leaning"], c("CHN", "VNM", "RUS", "IRN", "SYR"))
  expect_setequal(names(expected)[expected == "Nonaligned"], c("KOR", "EQD", "MEX", "BRA", "IND", "ZAF"))
  q_cn_dist <- stats::quantile(voting$d_china[voting$iso3 != "CHN"], 0.25, names = FALSE)
  expect_gt(voting$d_china[voting$iso3 == "SYR"], q_cn_dist)

  # Hard memberships are 1 / 0.5 / 0, so alignment is 1 within a bloc, 0 across, 0.5 otherwise.
  p <- stats::setNames(hard$p_us, hard$iso3)
  expect_true(all(p[names(expected)] %in% c(0, 0.5, 1)))
  a <- geo_dyadic_alignment(hard)
  g <- function(i, j) a$geo_alignment[a$reporter_iso == i & a$partner_iso == j]
  expect_equal(g("JPN", "USA"), 1)
  expect_equal(g("VNM", "CHN"), 1)
  expect_equal(g("SYR", "CHN"), 1)
  expect_equal(g("JPN", "CHN"), 0)
  expect_equal(g("USA", "RUS"), 0)
  expect_equal(g("USA", "SYR"), 0)
  expect_equal(g("IND", "USA"), 0.5)
  expect_equal(g("IND", "BRA"), 0.5)
})

test_that("smoothing converges to the hard blocs, is monotone in s and keeps the hard labels", {
  axis <- geo_bloc_axis(geo_country_ideal_points(geo_fixture_points(), sessions = 3))
  hard <- geo_bloc_membership(axis, rule = "quartile", tau = 0)
  ok <- !is.na(hard$theta)
  # Away from the cut points a very small tau is the hard rule.
  tiny <- geo_bloc_membership(axis, rule = "quartile", tau = 1e-4)
  away <- ok & abs(hard$s_axis - hard$s_us_cut) > 0.01 & abs(hard$s_axis - hard$s_china_cut) > 0.01
  expect_equal(tiny$p_us[away], hard$p_us[away], tolerance = 1e-6)

  smooth <- geo_bloc_membership(axis, rule = "quartile", tau = 0.05)
  p <- stats::setNames(smooth$p_us, smooth$iso3)
  expect_true(all(p[ok] >= 0 & p[ok] <= 1))
  expect_gt(p[["USA"]], 0.95)
  expect_lt(p[["CHN"]], 0.1)
  expect_equal(p[["SYR"]], p[["CHN"]])
  expect_equal(smooth$p_us, 0.5 + 0.5 * (smooth$u_us - smooth$u_china))
  # Membership rises with s along the whole axis.
  on_axis <- smooth[ok, ]
  on_axis <- on_axis[order(on_axis$s_axis), ]
  expect_true(all(diff(on_axis$p_us) >= -1e-12))
  # u_us >= 0.5 is the hard U.S.-leaning bloc, for any tau.
  for (tau in c(0, 0.025, 0.05, 0.10)) {
    m <- geo_bloc_membership(axis, rule = "quartile", tau = tau)
    expect_equal(m$u_us[ok] >= 0.5, m$bloc[ok] == "U.S.-leaning")
    # The hard labels do not depend on tau.
    expect_equal(m$bloc, hard$bloc)
  }
})

test_that("dyadic alignment is oriented, symmetric, bounded, NA for countries without a vote, not ranked", {
  pts <- geo_fixture_points()
  a <- compute_alignment_from_ideal_points(pts, sessions = 3, rule = "quartile", tau = 0.05)
  n <- length(unique(pts$iso3))
  expect_equal(nrow(a), n * (n - 1))
  expect_false(any(a$reporter_iso == a$partner_iso))
  expect_false(anyDuplicated(a[c("reporter_iso", "partner_iso")]) > 0)

  g <- function(i, j) a$geo_alignment[a$reporter_iso == i & a$partner_iso == j]
  # Orientation: closer on the bloc axis = higher.
  expect_gt(g("JPN", "USA"), g("JPN", "CHN"))
  expect_gt(g("VNM", "CHN"), g("VNM", "USA"))
  expect_gt(g("USA", "GBR"), g("USA", "IND"))
  expect_gt(g("USA", "IND"), g("USA", "CHN"))
  expect_equal(g("USA", "SYR"), g("USA", "CHN"))

  # Symmetric, for IPD and for the alignment.
  rev <- merge(a, a, by.x = c("reporter_iso", "partner_iso"), by.y = c("partner_iso", "reporter_iso"))
  expect_equal(rev$ipd.x, rev$ipd.y)
  expect_equal(rev$geo_alignment.x, rev$geo_alignment.y)

  # Bounded; NA (never 0) for every pair with TWN, which has no vote in the window.
  expect_true(all(a$geo_alignment[!is.na(a$geo_alignment)] >= 0 & a$geo_alignment[!is.na(a$geo_alignment)] <= 1))
  twn <- a$reporter_iso == "TWN" | a$partner_iso == "TWN"
  expect_true(all(is.na(a$geo_alignment[twn])))
  expect_false(any(is.na(a$geo_alignment[!twn])))

  # Cardinal, not ranked within the reporter: exactly p_i p_j + (1 - p_i)(1 - p_j).
  expect_equal(a$geo_alignment, a$p_reporter * a$p_partner + (1 - a$p_reporter) * (1 - a$p_partner))
  hard <- compute_alignment_from_ideal_points(pts, sessions = 3, rule = "quartile", tau = 0)
  usa <- hard[hard$reporter_iso == "USA" & !is.na(hard$geo_alignment), ]
  # Ties survive: every nonaligned partner of the US scores 0.5.
  expect_equal(sum(usa$geo_alignment == 0.5), 6L)
  expect_equal(unique(a$bloc_rule), "quartile")
  expect_equal(unique(a$window_sessions), "10,11,12")
})

test_that("the coalition is the U.S.-leaning bloc above the GDP floor", {
  pts <- geo_fixture_points()
  bloc_tbl <- geo_country_bloc_membership(pts, sessions = 3, rule = "quartile", tau = 0.05)
  co <- derive_alignment_coalition(bloc_tbl, geo_fixture_gdp(), p_threshold = 0.5, gdp_floor_usd = 1e11)

  # One row per country, the documented columns, members first.
  expect_equal(nrow(co), nrow(bloc_tbl))
  expect_setequal(co$iso3, bloc_tbl$iso3)
  expect_true(all(c("iso3", "country", "theta", "s_axis", "p_us", "u_us", "bloc", "gdp_usd", "gdp_year", "member", "reason") %in% names(co)))
  expect_type(co$member, "logical")
  expect_false(anyNA(co$member))
  expect_true(all(co$member[seq_len(sum(co$member))]))

  # U.S.-leaning: USA GBR JPN PRT DEU. PRT is below USD 100bn and DEU has no GDP row.
  expect_setequal(co$iso3[co$member], c("USA", "GBR", "JPN"))
  row <- function(iso) co[co$iso3 == iso, ]
  # Latest non-missing GDP: JPN's 2025 value is NA, so 2024.
  expect_equal(row("JPN")$gdp_year, 2024L)
  expect_equal(row("JPN")$gdp_usd, 4.0e12)
  expect_equal(row("USA")$gdp_usd, 3.0e13)
  expect_match(row("PRT")$reason, "< USD 100.0bn", fixed = TRUE)
  expect_match(row("DEU")$reason, "no WDI GDP", fixed = TRUE)
  expect_match(row("USA")$reason, ">= USD 100.0bn", fixed = TRUE)
  expect_match(row("TWN")$reason, "no UN vote", fixed = TRUE)
  expect_false(row("TWN")$member)
  # Large economies outside the bloc stay out: KOR is nonaligned on the US half of the axis, so
  # its p_us is above 0.5 but its membership of the U.S.-leaning bloc is below it.
  expect_gt(row("KOR")$p_us, 0.5)
  expect_lt(row("KOR")$u_us, 0.5)
  expect_false(row("KOR")$member)
  expect_false(row("CHN")$member)
  expect_match(row("KOR")$reason, "Nonaligned", fixed = TRUE)

  # At p_threshold 0.5 the bloc is the hard bloc whatever tau.
  for (tau in c(0, 0.025, 0.10)) {
    b <- geo_country_bloc_membership(pts, sessions = 3, rule = "quartile", tau = tau)
    expect_setequal(derive_alignment_coalition(b, geo_fixture_gdp())$iso3[derive_alignment_coalition(b, geo_fixture_gdp())$member], c("USA", "GBR", "JPN"))
  }
  # The floor: at 0 every U.S.-leaning country with a GDP row is in; DEU still has none.
  all_in <- derive_alignment_coalition(bloc_tbl, geo_fixture_gdp(), gdp_floor_usd = 0)
  expect_setequal(all_in$iso3[all_in$member], c("USA", "GBR", "JPN", "PRT"))
  # A stricter threshold drops the bloc's margin.
  strict <- derive_alignment_coalition(bloc_tbl, geo_fixture_gdp(), p_threshold = 0.999)
  expect_true(all(strict$iso3[strict$member] %in% c("USA", "GBR", "JPN")))
  expect_lt(sum(strict$member), 3L)

  # Under the midpoint rule u_us = p_us and the bloc is the US half of the axis.
  mid <- geo_country_bloc_membership(pts, sessions = 3, rule = "midpoint", tau = 0.05)
  co_mid <- derive_alignment_coalition(mid, geo_fixture_gdp())
  expect_setequal(co_mid$iso3[co_mid$member], c("USA", "GBR", "JPN", "KOR"))

  # Names carried through when the bloc table has them.
  named <- bloc_tbl
  named$country <- paste("Name", named$iso3)
  expect_equal(derive_alignment_coalition(named, geo_fixture_gdp())$country[1], "Name USA")

  expect_error(derive_alignment_coalition(bloc_tbl[setdiff(names(bloc_tbl), "u_us")], geo_fixture_gdp()), "u_us")
  bad_gdp <- rbind(geo_fixture_gdp(), data.frame(country = "United States", iso3c = "USA", year = 2025L, `NY.GDP.MKTP.CD` = 1, check.names = FALSE))
  expect_error(derive_alignment_coalition(bloc_tbl, bad_gdp), "two GDP values")
  expect_error(derive_alignment_coalition(bloc_tbl, geo_fixture_gdp(), p_threshold = 0), "p_threshold")
})


test_that("bloc membership is hard by default: p in {1, 0.5, 0} and alignment in {1, 0.5, 0}", {
  pts <- geo_fixture_points()
  m <- geo_country_bloc_membership(pts, sessions = 3)
  expect_equal(unique(m$tau), 0)
  ok <- !is.na(m$theta)
  expect_true(all(m$p_us[ok] %in% c(0, 0.5, 1)))
  expect_equal(m$p_us[ok] == 1, m$bloc[ok] == "U.S.-leaning")
  expect_equal(m$p_us[ok] == 0, m$bloc[ok] == "China-leaning")
  a <- compute_alignment_from_ideal_points(pts, sessions = 3)
  expect_true(all(a$geo_alignment[!is.na(a$geo_alignment)] %in% c(0, 0.5, 1)))
  # Hard blocs are symmetric between the blocs: China's own p is 0, as the US's is 1.
  expect_equal(m$p_us[m$iso3 == "CHN"], 0)
  expect_equal(m$p_us[m$iso3 == "USA"], 1)
})

# The no-vote map in the layout of data/reference/territory_sovereign_map.csv.
geo_fixture_map <- function() {
  data.frame(
    iso3 = c("HKG", "PRI", "TWN", "PSE", "JPN"),
    economy = c("Hong Kong SAR, China", "Puerto Rico", "Taiwan", "Palestine", "Japan"),
    basis = c("inherit", "inherit", "neutral", "neutral", "inherit"),
    sovereign_iso3 = c("CHN", "USA", NA, NA, "CHN"),
    status = "s",
    note = "n",
    stringsAsFactors = FALSE
  )
}

test_that("economies without a UN vote inherit their sovereign's position or are neutral, outside the cuts and the coalition", {
  pts <- geo_fixture_points()
  bloc_tbl <- geo_country_bloc_membership(pts, sessions = 3, rule = "quartile", tau = 0)
  out <- geo_apply_no_vote_map(bloc_tbl, geo_fixture_map())
  row <- function(tbl, iso) tbl[tbl$iso3 == iso, ]
  # Names as scripts/12 adds them; the map's `economy` names the rows it adds.
  named <- geo_apply_no_vote_map(
    tibble::add_column(bloc_tbl, country = paste("Name", bloc_tbl$iso3), .after = "iso3"),
    geo_fixture_map()
  )
  expect_equal(row(named, "HKG")$country, "Hong Kong SAR, China")
  expect_equal(row(named, "TWN")$country, "Taiwan")
  expect_equal(row(named, "JPN")$country, "Name JPN")

  # HKG and PRI copy their sovereign; TWN (in the table without a vote) and PSE (absent) are
  # neutral; JPN has its own vote and keeps it although the map lists it.
  expect_equal(nrow(out), nrow(bloc_tbl) + 3L)
  expect_false(anyDuplicated(out$iso3) > 0)
  expect_equal(row(out, "HKG")$p_us, row(bloc_tbl, "CHN")$p_us)
  expect_equal(row(out, "HKG")$bloc, "China-leaning")
  expect_equal(row(out, "HKG")$imputed_from, "CHN")
  expect_equal(row(out, "HKG")$alignment_basis, "inherited")
  expect_equal(row(out, "HKG")$n_sessions, 0L)
  expect_true(is.na(row(out, "HKG")$n_votes))
  expect_equal(row(out, "PRI")$s_axis, 1)
  expect_equal(row(out, "PRI")$p_us, 1)
  for (iso in c("TWN", "PSE")) {
    expect_equal(row(out, iso)$p_us, 0.5)
    expect_equal(row(out, iso)$alignment_basis, "neutral")
    expect_equal(row(out, iso)$bloc, "Nonaligned")
    expect_true(is.na(row(out, iso)$imputed_from))
    expect_true(is.na(row(out, iso)$theta))
    expect_true(is.na(row(out, iso)$s_axis))
  }
  expect_equal(row(out, "JPN")$theta, row(bloc_tbl, "JPN")$theta)
  expect_equal(row(out, "JPN")$alignment_basis, "own_votes")
  expect_true(is.na(row(out, "JPN")$imputed_from))
  expect_setequal(out$alignment_basis, c("own_votes", "inherited", "neutral"))
  # Added after the cuts: the voting countries' rows and the cuts are unchanged.
  own <- out[out$alignment_basis == "own_votes", ]
  expect_equal(own$p_us, bloc_tbl$p_us[match(own$iso3, bloc_tbl$iso3)])
  expect_equal(unique(out$s_us_cut), unique(bloc_tbl$s_us_cut))
  expect_equal(unique(out$window_sessions), "10,11,12")

  # Dyads: an inheriting economy pairs like its sovereign; a neutral one scores 0.5 with everyone.
  a <- geo_dyadic_alignment(out)
  g <- function(i, j) a$geo_alignment[a$reporter_iso == i & a$partner_iso == j]
  expect_equal(g("USA", "HKG"), 0)
  expect_equal(g("CHN", "HKG"), 1)
  expect_equal(g("JPN", "PRI"), 1)
  expect_true(all(a$geo_alignment[a$reporter_iso == "TWN" | a$partner_iso == "TWN"] == 0.5))
  expect_equal(g("TWN", "PSE"), 0.5)
  expect_false(anyNA(a$geo_alignment))
  expect_equal(a$basis_partner[a$reporter_iso == "USA" & a$partner_iso == "HKG"], "inherited")
  expect_equal(a$basis_reporter[a$reporter_iso == "TWN" & a$partner_iso == "USA"], "neutral")

  # Smoothed memberships are copied as they are.
  smooth <- geo_country_bloc_membership(pts, sessions = 3, rule = "quartile", tau = 0.05)
  s_out <- geo_apply_no_vote_map(smooth, geo_fixture_map())
  expect_equal(row(s_out, "HKG")$p_us, row(smooth, "CHN")$p_us)
  expect_equal(row(s_out, "TWN")$p_us, 0.5)
  expect_equal(row(geo_apply_no_vote_map(smooth, geo_fixture_map(), neutral_p = 0.4), "TWN")$p_us, 0.4)

  # Neither an inheriting nor a neutral economy is ever a coalition member.
  gdp <- rbind(
    geo_fixture_gdp(),
    data.frame(country = c("Puerto Rico", "Taiwan"), iso3c = c("PRI", "TWN"), year = 2025L,
               `NY.GDP.MKTP.CD` = c(1.2e11, 7.5e11), check.names = FALSE)
  )
  co <- derive_alignment_coalition(out, gdp)
  expect_false(row(co, "PRI")$member)
  expect_match(row(co, "PRI")$reason, "inherits USA's position", fixed = TRUE)
  expect_false(row(co, "TWN")$member)
  expect_match(row(co, "TWN")$reason, "neutral", fixed = TRUE)
  expect_setequal(co$iso3[co$member], c("USA", "GBR", "JPN"))

  # A sovereign without a vote passes nothing on.
  orphan <- data.frame(iso3 = "XYZ", basis = "inherit", sovereign_iso3 = "TWN", stringsAsFactors = FALSE)
  no_vote <- geo_apply_no_vote_map(bloc_tbl, orphan)
  expect_true(is.na(row(no_vote, "XYZ")$p_us))
  expect_equal(row(no_vote, "XYZ")$alignment_basis, "none")
  expect_equal(row(no_vote, "TWN")$alignment_basis, "none")

  # No map: only the basis columns are added.
  none <- geo_apply_no_vote_map(bloc_tbl, NULL)
  expect_equal(nrow(none), nrow(bloc_tbl))
  expect_equal(none$alignment_basis, ifelse(is.na(bloc_tbl$theta), "none", "own_votes"))
  expect_true(all(is.na(none$imputed_from)))
  expect_equal(compute_alignment_from_ideal_points(pts, sessions = 3, no_vote_map = geo_fixture_map())$geo_alignment, a$geo_alignment)
})

test_that("the no-vote map is validated", {
  m <- geo_fixture_map()
  expect_equal(geo_validate_no_vote_map(m)$sovereign_iso3, c("CHN", "USA", NA, NA, "CHN"))
  bad <- function(f) { x <- m; f(x) }
  expect_error(geo_validate_no_vote_map(m[c("iso3", "basis")]), "sovereign_iso3")
  expect_error(geo_validate_no_vote_map(rbind(m, m[1, ])), "twice")
  expect_error(geo_validate_no_vote_map(bad(function(x) { x$basis[1] <- "copy"; x })), "inherit or neutral")
  expect_error(geo_validate_no_vote_map(bad(function(x) { x$sovereign_iso3[2] <- NA; x })), "need a sovereign_iso3")
  expect_error(geo_validate_no_vote_map(bad(function(x) { x$sovereign_iso3[3] <- "CHN"; x })), "take no sovereign_iso3")
  expect_error(geo_validate_no_vote_map(bad(function(x) { x$sovereign_iso3[1] <- "PRI"; x })), "itself be in the no-vote map")
  expect_error(geo_validate_no_vote_map(bad(function(x) { x$iso3[1] <- "HK"; x })), "malformed")
  bloc_tbl <- geo_country_bloc_membership(geo_fixture_points(), sessions = 3)
  expect_error(
    geo_apply_no_vote_map(bloc_tbl, data.frame(iso3 = "HKG", basis = "inherit", sovereign_iso3 = "XXX")),
    "not in the membership"
  )
  expect_error(geo_apply_no_vote_map(bloc_tbl, m, neutral_p = 2), "neutral_p")
})

test_that("ideal-point names resolve to ISO3, with the file's code and COW successors as fallbacks", {
  raw <- data.frame(
    country_name = c("United States", "Japan", "Yugoslavia", "Germany"),
    iso3c = c("USA", "JPN", "YUG", "FRA"),
    session = c(78L, 78L, 40L, 78L),
    stringsAsFactors = FALSE
  )
  res <- geo_resolve_ideal_point_iso3(raw)
  expect_equal(res$data$iso3[1:2], c("USA", "JPN"))
  expect_equal(res$data$iso3_source[1:2], c("name", "name"))
  expect_equal(res$unmatched$name, "Yugoslavia")
  expect_equal(res$unmatched$last_session, 40L)
  expect_equal(res$disagreements$name, "Germany")
  expect_equal(res$disagreements$iso3, "DEU")

  # The all-votes file: Sao Tome's name has replacement characters but a current iso3c, and
  # Serbia is COW 345 "Yugoslavia" / "YUG" after 2006 as well as before.
  all_votes <- data.frame(
    ccode = c(403L, 345L, 345L, 345L, 265L),
    country_name = c("S\ufffd\ufffdo Tom\ufffd & Pr\ufffdncipe", "Yugoslavia", "Yugoslavia", "Yugoslavia", "German Democratic Republic"),
    iso3c = c("STP", "YUG", "YUG", "YUG", "DDR"),
    session = c(78L, 40L, 61L, 78L, 40L),
    year = c(2023L, 1985L, 2006L, 2023L, 1985L),
    stringsAsFactors = FALSE
  )
  res <- geo_resolve_ideal_point_iso3(all_votes)
  expect_equal(res$data$iso3, c("STP", NA, "SRB", "SRB", NA))
  expect_equal(res$data$iso3_source, c("file iso3c", NA, "COW successor", "COW successor", NA))
  # Summaries are per name x code x ISO3: Yugoslavia is unmatched only up to session 40.
  expect_setequal(res$unmatched$name, c("Yugoslavia", "German Democratic Republic"))
  expect_equal(res$unmatched$last_session[res$unmatched$name == "Yugoslavia"], 40L)
  expect_equal(nrow(res$fallback), 2L)
  expect_equal(res$fallback$iso3[res$fallback$iso3_source == "COW successor"], "SRB")
  expect_equal(res$fallback$first_session[res$fallback$iso3 == "SRB"], 61L)
  expect_equal(nrow(res$disagreements), 0L)
  # Without the successor table Serbia stays unresolved.
  expect_true(all(is.na(geo_resolve_ideal_point_iso3(all_votes, successors = NULL)$data$iso3[2:4])))
})

test_that("the shipped territory map covers the no-vote economies, documents every row and keeps sovereigns voting", {
  map_path <- file.path(geo_repo_root, "data", "reference", "territory_sovereign_map.csv")
  expect_true(file.exists(map_path))
  map <- geo_validate_no_vote_map(utils::read.csv(map_path, stringsAsFactors = FALSE, na.strings = ""))
  expect_true(all(c("iso3", "economy", "basis", "sovereign_iso3", "status", "note") %in% names(map)))
  for (col in c("economy", "status", "note")) {
    expect_false(any(is.na(map[[col]]) | !nzchar(trimws(map[[col]]))), info = col)
  }
  expect_setequal(map$iso3[map$basis == "neutral"], c("TWN", "PSE", "XKX"))
  sov <- stats::setNames(map$sovereign_iso3, map$iso3)
  expected <- c(
    HKG = "CHN", MAC = "CHN", PRI = "USA", GUM = "USA", VIR = "USA", ASM = "USA", MNP = "USA",
    GRL = "DNK", FRO = "DNK", BMU = "GBR", CYM = "GBR", VGB = "GBR", TCA = "GBR", GIB = "GBR",
    MSR = "GBR", AIA = "GBR", FLK = "GBR", NCL = "FRA", PYF = "FRA", WLF = "FRA", SPM = "FRA",
    MAF = "FRA", BLM = "FRA", ABW = "NLD", CUW = "NLD", SXM = "NLD", BES = "NLD"
  )
  expect_equal(unname(sov[names(expected)]), unname(expected))
  # Every sovereign is a UN member with votes in the series' latest years.
  for (f in c("un_ga_ideal_points_all.csv", "un_ga_ideal_points.csv")) {
    raw_path <- file.path(geo_repo_root, "data", "raw", f)
    if (!file.exists(raw_path)) next
    raw <- utils::read.csv(raw_path, stringsAsFactors = FALSE)
    recent <- unique(raw$iso3c[raw$session >= max(raw$session) - 4])
    expect_true(all(stats::na.omit(map$sovereign_iso3) %in% recent), info = f)
    # And no map economy votes in its own right.
    expect_false(any(map$iso3 %in% recent), info = f)
  }
})

test_that("the configured settings are the all-votes series, hard blocs and a neutral 0.5", {
  cfg <- yaml::read_yaml(file.path(geo_repo_root, "config", "geopolitical_distance.yml"))$alignment
  expect_equal(cfg$series, "all")
  expect_equal(cfg$tau, 0)
  expect_equal(cfg$neutral_p, 0.5)
  expect_equal(cfg$bloc_rule, "quartile")
  expect_null(cfg$vote_proxies)
  # scripts/12 names both series' files the way raw-input discovery reads them.
  src <- paste(readLines(file.path(geo_repo_root, "scripts", "12_build_geopolitical_alignment.R"), warn = FALSE), collapse = "\n")
  expect_match(src, 'file.path(raw_data_path, "un_ga_ideal_points_all.csv")', fixed = TRUE)
  expect_match(src, 'file.path(raw_data_path, "un_ga_ideal_points.csv")', fixed = TRUE)
  expect_match(src, "territory_sovereign_map.csv", fixed = TRUE)
})
