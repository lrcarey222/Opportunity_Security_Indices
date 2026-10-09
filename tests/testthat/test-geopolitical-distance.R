testthat::test_that("IPD and normalization formulas work", {
  source(test_path("..", "..", "R", "geopolitical_distance", "core.R"))
  th <- data.frame(iso3 = c("USA", "CHN", "JPN"), country = c("United States", "China", "Japan"), year = 2021,
                   theta = c(1, -1, 0.5), theta_se = c(0.1, 0.1, 0.1), spec_name = "s")
  ipd <- compute_ipd_matrix(th, 2021, "s")
  testthat::expect_true(all(ipd$gp_norm >= 0 & ipd$gp_norm <= 1))
  testthat::expect_equal(ipd$ipd[ipd$iso3_i == "USA" & ipd$iso3_j == "CHN"], 2)
})

testthat::test_that("seg and bloc assignment are consistent", {
  source(test_path("..", "..", "R", "geopolitical_distance", "core.R"))
  ipd <- data.frame(
    iso3_i = c("USA", "USA", "CHN", "CHN", "AAA", "AAA"),
    iso3_j = c("USA", "CHN", "USA", "CHN", "USA", "CHN"),
    year = 2021,
    ipd = c(0, 2, 2, 0, 0.2, 1.8),
    gp_norm = c(0, 1, 1, 0, 0.1, 0.9),
    spec_name = "s"
  )
  cm <- data.frame(iso3 = c("USA", "CHN", "AAA"), country = c("United States", "China", "Aland"))
  seg <- compute_seg_scores(ipd, cm, 2021, "s")
  testthat::expect_equal(seg$seg[seg$iso3 == "USA"], 1)
  testthat::expect_equal(seg$seg[seg$iso3 == "CHN"], -1)
  blocs <- assign_blocs(seg)
  testthat::expect_true(all(blocs$bloc %in% c("U.S.-leaning", "China-leaning", "Nonaligned")))
})


testthat::test_that("required-column guard fails loudly", {
  source(test_path("..", "..", "R", "geopolitical_distance", "core.R"))
  bad <- data.frame(foo = 1, bar = 2)
  testthat::expect_error(
    compute_ipd_matrix(bad, 2021, "s"),
    "Missing required columns"
  )
})


testthat::test_that("vote normalization handles numeric and string codes", {
  source(test_path("..", "..", "R", "geopolitical_distance", "ideal_points.R"))
  vals <- normalize_vote_to_binary(c(1, 3, 2, "yes", "no", "abstain", "YEA", "NAY"))
  testthat::expect_equal(vals[1], 1)
  testthat::expect_equal(vals[2], 0)
  testthat::expect_true(is.na(vals[3]))
  testthat::expect_equal(vals[4], 1)
  testthat::expect_equal(vals[5], 0)
  testthat::expect_true(is.na(vals[6]))
  testthat::expect_equal(vals[7], 1)
  testthat::expect_equal(vals[8], 0)
})


testthat::test_that("build_vote_matrix keeps missing votes NA instead of summing them to 0", {
  source(test_path("..", "..", "R", "geopolitical_distance", "ideal_points.R"))
  sub <- data.frame(
    country = c("A", "A", "B"),
    rcid = c(1, 2, 1),
    vote_bin = c(1, 0, 1)
  )
  mat <- build_vote_matrix(sub)
  testthat::expect_true(is.na(mat["B", "2"]))
  testthat::expect_equal(mat["A", "1"], 1)
  testthat::expect_equal(mat["A", "2"], 0)

  dup <- rbind(sub, data.frame(country = "A", rcid = 1, vote_bin = 0))
  testthat::expect_error(build_vote_matrix(dup), "duplicate")
})


testthat::test_that("abstentions and absences are missing, not No votes, in the yearly estimates", {
  source(test_path("..", "..", "R", "geopolitical_distance", "ideal_points.R"))
  # Four countries always vote Yes, four always No, on 24 roll calls that split them. ABS
  # abstains on every roll call and is absent from the record on half of them. Under the old
  # xtabs() matrix ABS's empty row became all zeros, i.e. identical to the No bloc.
  yes_bloc <- paste0("Y", 1:4)
  no_bloc <- paste0("N", 1:4)
  rcids <- 1:24
  votes <- rbind(
    expand.grid(country = yes_bloc, rcid = rcids, vote = "yes", stringsAsFactors = FALSE),
    expand.grid(country = no_bloc, rcid = rcids, vote = "no", stringsAsFactors = FALSE),
    data.frame(country = "ABS", rcid = 1:12, vote = "abstain", stringsAsFactors = FALSE)
  )
  # One Yes-bloc member and one No-bloc member each cross over on a few votes, so the matrix has
  # a non-degenerate first component, and ABS casts two Yes votes.
  votes$vote[votes$country == "Y4" & votes$rcid %in% 1:3] <- "no"
  votes$vote[votes$country == "N4" & votes$rcid %in% 4:6] <- "yes"
  votes <- rbind(votes, data.frame(country = "ABS", rcid = 13:14, vote = "yes", stringsAsFactors = FALSE))
  votes$year <- 2020

  pts <- estimate_ideal_points_yearly(votes, estimator = "pca", min_countries = 8, min_rollcalls = 20)
  testthat::expect_equal(sort(pts$country), sort(c(yes_bloc, no_bloc, "ABS")))
  testthat::expect_true(all(pts$estimator == "pca"))

  theta <- stats::setNames(pts$theta, pts$country)
  yes_mean <- mean(theta[yes_bloc])
  no_mean <- mean(theta[no_bloc])
  # ABS sits between the blocs (it said Yes twice and nothing else), not on top of the No bloc.
  testthat::expect_true(abs(theta[["ABS"]] - no_mean) > 0.25 * abs(yes_mean - no_mean))
  testthat::expect_true((theta[["ABS"]] - no_mean) * (yes_mean - theta[["ABS"]]) > 0)

  # Its matrix row is NA except for the two Yes votes.
  mat <- build_vote_matrix(transform(votes[!is.na(normalize_vote_to_binary(votes$vote)), ],
                                     vote_bin = normalize_vote_to_binary(vote)))
  testthat::expect_equal(sum(!is.na(mat["ABS", ])), 2)
  testthat::expect_true(all(mat["ABS", c("13", "14")] == 1))
})
