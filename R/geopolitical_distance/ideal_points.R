# Ideal-point estimation from UN General Assembly roll calls (method reference).
#
# The production alignment measure (R/geopolitical_distance/alignment.R, scripts/12) uses the
# published Bailey-Strezhnev-Voeten (2017) estimates, which come from a dynamic IRT model with
# bridging votes. The functions here estimate a simpler one-dimensional model year by year and
# document the method; they are not on the pipeline path.
#
# Vote coding: Yes = 1, No = 0. Abstentions, absences and non-membership are missing (NA): they
# carry no information about the yes/no cut on that resolution, so they must not be scored as a
# vote. build_vote_matrix() keeps them NA; the earlier xtabs() construction summed vote_bin over
# country x roll call and filled every absent cell with 0, which coded each abstention and
# absence as a "No".

filter_votes_for_spec <- function(votes_df, issues_df, spec_cfg) {
  df <- votes_df[votes_df$year >= spec_cfg$estimation_start & votes_df$year <= spec_cfg$estimation_end, ]

  if (!is.null(spec_cfg$vote_filter) && spec_cfg$vote_filter == "economic") {
    econ_rcids <- unique(issues_df$rcid[grepl("econ", tolower(issues_df$issue)) | grepl("economic", tolower(issues_df$issue))])
    df <- df[df$rcid %in% econ_rcids, ]
  }
  df
}

# Voeten numeric codes: 1 = yes, 2 = abstain, 3 = no, 8 = absent, 9 = not a member.
normalize_vote_to_binary <- function(vote_vec) {
  if (is.numeric(vote_vec)) {
    return(ifelse(vote_vec == 1, 1, ifelse(vote_vec == 3, 0, NA_real_)))
  }

  vote_chr <- tolower(trimws(as.character(vote_vec)))
  yes_tokens <- c("yes", "yea", "1")
  no_tokens <- c("no", "nay", "3")

  out <- ifelse(vote_chr %in% yes_tokens, 1,
                ifelse(vote_chr %in% no_tokens, 0, NA_real_))
  as.numeric(out)
}

# Country x roll-call matrix of binary votes. A cell with no yes/no vote (abstention, absence,
# non-membership, or a row dropped upstream) is NA, never 0. Two records for one cell are an
# input error, not something to sum.
build_vote_matrix <- function(sub_votes_df) {
  required <- c("country", "rcid", "vote_bin")
  missing_cols <- setdiff(required, names(sub_votes_df))
  if (length(missing_cols) > 0) {
    stop("build_vote_matrix missing columns: ", paste(missing_cols, collapse = ", "))
  }

  dup <- duplicated(sub_votes_df[c("country", "rcid")])
  if (any(dup)) {
    stop("build_vote_matrix: ", sum(dup), " duplicate country x roll-call record(s).")
  }

  countries <- sort(unique(as.character(sub_votes_df$country)))
  rcids <- sort(unique(sub_votes_df$rcid))
  mat <- matrix(NA_real_, nrow = length(countries), ncol = length(rcids),
                dimnames = list(countries, as.character(rcids)))

  row_idx <- match(as.character(sub_votes_df$country), countries)
  col_idx <- match(as.character(sub_votes_df$rcid), as.character(rcids))
  mat[cbind(row_idx, col_idx)] <- as.numeric(sub_votes_df$vote_bin)
  mat
}

# First principal component of the vote matrix; missing votes are set to 0.5 (between yes and
# no) for the decomposition only. Standardized; the sign is arbitrary.
estimate_theta_pca <- function(mat) {
  mat_num <- as.matrix(mat)
  mat_num[is.na(mat_num)] <- 0.5

  if (nrow(mat_num) < 3 || ncol(mat_num) < 3) return(NULL)

  pca <- try(stats::prcomp(mat_num, center = TRUE, scale. = FALSE), silent = TRUE)
  if (inherits(pca, "try-error")) return(NULL)

  theta <- pca$x[, 1]
  if (is.null(theta) || length(theta) == 0) return(NULL)

  theta_centered <- as.numeric(scale(theta, center = TRUE, scale = TRUE))
  list(theta = theta_centered, theta_se = rep(NA_real_, length(theta_centered)), country = rownames(mat_num))
}

# One-dimensional Bayesian IRT (Clinton, Jackman and Rivers 2004) via pscl::ideal. Missing
# votes are NA and are skipped by the likelihood. pscl needs distinct codes for "missing" and
# "not in legislature"; the matrix never contains 9, so every NA is treated as missing.
estimate_theta_pscl <- function(mat) {
  if (!requireNamespace("pscl", quietly = TRUE)) return(NULL)

  rc <- try(pscl::rollcall(
    mat,
    yea = c(1),
    nay = c(0),
    missing = c(NA),
    notInLegis = c(9),
    legis.names = rownames(mat)
  ), silent = TRUE)
  if (inherits(rc, "try-error")) return(NULL)

  fit <- try(pscl::ideal(rc, d = 1, maxiter = 5000, burnin = 1000, thin = 10, store.item = FALSE, verbose = FALSE), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)

  draws <- fit$x
  theta <- apply(draws[, , 1, drop = FALSE], 2, mean, na.rm = TRUE)
  theta_se <- apply(draws[, , 1, drop = FALSE], 2, stats::sd, na.rm = TRUE)

  list(theta = as.numeric(theta), theta_se = as.numeric(theta_se), country = names(theta))
}

# Yearly ideal points. estimator = "pscl" tries pscl::ideal and falls back to PCA; "pca" uses
# PCA only (deterministic, used by the tests). Years with fewer than `min_countries` voting
# countries or `min_rollcalls` roll calls are skipped.
estimate_ideal_points_yearly <- function(votes_df,
                                         estimator = c("pscl", "pca"),
                                         min_countries = 8,
                                         min_rollcalls = 20) {
  estimator <- match.arg(estimator)
  votes_df$vote_bin <- normalize_vote_to_binary(votes_df$vote)
  votes_df <- votes_df[!is.na(votes_df$vote_bin), ]

  years <- sort(unique(votes_df$year))
  out <- list()

  for (yr in years) {
    sub <- votes_df[votes_df$year == yr, ]
    mat <- build_vote_matrix(sub)

    if (nrow(mat) < min_countries || ncol(mat) < min_rollcalls) next

    fit <- NULL
    used <- NA_character_
    if (estimator == "pscl") {
      fit <- estimate_theta_pscl(mat)
      used <- "pscl::ideal"
    }

    if (is.null(fit)) {
      fit <- estimate_theta_pca(mat)
      used <- if (estimator == "pscl") "pca_fallback" else "pca"
    }

    if (is.null(fit)) next

    tmp <- data.frame(
      country = fit$country,
      year = yr,
      theta = as.numeric(fit$theta),
      theta_se = as.numeric(fit$theta_se),
      estimator = used,
      stringsAsFactors = FALSE
    )
    out[[as.character(yr)]] <- tmp
  }

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}
