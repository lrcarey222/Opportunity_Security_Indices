# Shared helpers for the strategic position framework (docs/strategic_position.md).
# Pure functions: no IO.

# Mid-rank percentile of x among its non-missing values: (rank - 0.5) / n, ties averaged.
# A single observation gets 0.5; missing values stay NA.
sp_midrank_pct <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  n <- sum(ok)
  if (n == 0) return(out)
  out[ok] <- (rank(x[ok], ties.method = "average") - 0.5) / n
  out
}

# Rank with 1 = highest; ties share the minimum rank; missing values stay NA.
sp_rank_desc <- function(x) {
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) return(out)
  out[ok] <- as.integer(rank(-x[ok], ties.method = "min"))
  out
}

# Mean of the non-missing values, NA when there are none.
sp_mean_available <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

# Row-wise sp_mean_available over the columns of a data frame or matrix.
sp_row_mean_available <- function(m) {
  m <- as.matrix(m)
  n <- rowSums(!is.na(m))
  s <- rowSums(m, na.rm = TRUE)
  ifelse(n > 0, s / n, NA_real_)
}

# The six-stage cell key columns every strategic position table carries.
SP_CELL_KEYS <- c("Country", "tech", "supply_chain")
