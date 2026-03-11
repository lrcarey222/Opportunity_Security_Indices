filter_votes_for_spec <- function(votes_df, issues_df, spec_cfg) {
  df <- votes_df[votes_df$year >= spec_cfg$estimation_start & votes_df$year <= spec_cfg$estimation_end, ]

  if (!is.null(spec_cfg$vote_filter) && spec_cfg$vote_filter == "economic") {
    econ_rcids <- unique(issues_df$rcid[grepl("econ", tolower(issues_df$issue)) | grepl("economic", tolower(issues_df$issue))])
    df <- df[df$rcid %in% econ_rcids, ]
  }
  df
}

estimate_ideal_points_yearly <- function(votes_df) {
  if (!requireNamespace("pscl", quietly = TRUE)) {
    stop("Package 'pscl' is required for ideal-point estimation.")
  }

  votes_df <- votes_df[votes_df$vote %in% c(1, 2, 3), ]
  votes_df$vote_bin <- ifelse(votes_df$vote == 1, 1, ifelse(votes_df$vote == 3, 0, NA))
  votes_df <- votes_df[!is.na(votes_df$vote_bin), ]

  years <- sort(unique(votes_df$year))
  out <- list()

  for (yr in years) {
    sub <- votes_df[votes_df$year == yr, ]
    mat <- stats::xtabs(vote_bin ~ country + rcid, data = sub)

    if (nrow(mat) < 8 || ncol(mat) < 20) next

    rc <- pscl::rollcall(mat, yea = c(1), nay = c(0), missing = c(NA), notInLegis = NA, legis.names = rownames(mat))
    fit <- try(pscl::ideal(rc, d = 1, maxiter = 5000, burnin = 1000, thin = 10, store.item = FALSE, verbose = FALSE), silent = TRUE)
    if (inherits(fit, "try-error")) next

    draws <- fit$x
    theta <- apply(draws[, , 1, drop = FALSE], 2, mean, na.rm = TRUE)
    theta_se <- apply(draws[, , 1, drop = FALSE], 2, stats::sd, na.rm = TRUE)

    tmp <- data.frame(country = names(theta), year = yr, theta = as.numeric(theta), theta_se = as.numeric(theta_se), stringsAsFactors = FALSE)
    out[[as.character(yr)]] <- tmp
  }

  do.call(rbind, out)
}
