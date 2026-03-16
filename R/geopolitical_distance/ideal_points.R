filter_votes_for_spec <- function(votes_df, issues_df, spec_cfg) {
  df <- votes_df[votes_df$year >= spec_cfg$estimation_start & votes_df$year <= spec_cfg$estimation_end, ]

  if (!is.null(spec_cfg$vote_filter) && spec_cfg$vote_filter == "economic") {
    econ_rcids <- unique(issues_df$rcid[grepl("econ", tolower(issues_df$issue)) | grepl("economic", tolower(issues_df$issue))])
    df <- df[df$rcid %in% econ_rcids, ]
  }
  df
}

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

build_vote_matrix <- function(sub_votes_df) {
  required <- c("country", "rcid", "vote_bin")
  missing_cols <- setdiff(required, names(sub_votes_df))
  if (length(missing_cols) > 0) {
    stop("build_vote_matrix missing columns: ", paste(missing_cols, collapse = ", "))
  }

  countries <- sort(unique(sub_votes_df$country))
  rcids <- sort(unique(sub_votes_df$rcid))
  mat <- matrix(NA_real_, nrow = length(countries), ncol = length(rcids),
                dimnames = list(countries, as.character(rcids)))

  row_idx <- match(sub_votes_df$country, countries)
  col_idx <- match(as.character(sub_votes_df$rcid), as.character(rcids))
  mat[cbind(row_idx, col_idx)] <- sub_votes_df$vote_bin
  mat
}

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

estimate_theta_pscl <- function(mat) {
  if (!requireNamespace("pscl", quietly = TRUE)) return(NULL)

  # Ensure unique coding sets for rollcall (previously NA was used for both
  # missing and notInLegis, which triggers "codes are not unique").
  rc <- pscl::rollcall(
    mat,
    yea = c(1),
    nay = c(0),
    missing = c(NA),
    notInLegis = c(9),
    legis.names = rownames(mat)
  )
  fit <- try(pscl::ideal(rc, d = 1, maxiter = 5000, burnin = 1000, thin = 10, store.item = FALSE, verbose = FALSE), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)

  draws <- fit$x
  theta <- apply(draws[, , 1, drop = FALSE], 2, mean, na.rm = TRUE)
  theta_se <- apply(draws[, , 1, drop = FALSE], 2, stats::sd, na.rm = TRUE)

  list(theta = as.numeric(theta), theta_se = as.numeric(theta_se), country = names(theta))
}

estimate_ideal_points_yearly <- function(votes_df) {
  votes_df$vote_bin <- normalize_vote_to_binary(votes_df$vote)
  votes_df <- votes_df[!is.na(votes_df$vote_bin), ]

  years <- sort(unique(votes_df$year))
  out <- list()

  for (yr in years) {
    sub <- votes_df[votes_df$year == yr, ]
    mat <- build_vote_matrix(sub)

    if (nrow(mat) < 8 || ncol(mat) < 20) next

    fit <- estimate_theta_pscl(mat)
    estimator <- "pscl::ideal"

    if (is.null(fit)) {
      fit <- estimate_theta_pca(mat)
      estimator <- "pca_fallback"
    }

    if (is.null(fit)) next

    tmp <- data.frame(
      country = fit$country,
      year = yr,
      theta = as.numeric(fit$theta),
      theta_se = as.numeric(fit$theta_se),
      estimator = estimator,
      stringsAsFactors = FALSE
    )
    out[[as.character(yr)]] <- tmp
  }

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}
