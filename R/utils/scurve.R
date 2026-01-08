# Median-centered S-curve normalization.
median_scurve <- function(x, gamma = 0.5) {
  if (!is.numeric(x)) {
    stop("median_scurve() expects numeric input.")
  }
  n <- sum(!is.na(x))
  if (n == 0) {
    return(rep(NA_real_, length(x)))
  }
  if (n == 1) {
    r <- ifelse(is.na(x), NA_real_, 0)
  } else {
    ranks <- rank(x, na.last = "keep", ties.method = "average")
    r <- (ranks - 1) / (n - 1)
  }
  (r^gamma) / (r^gamma + (1 - r)^gamma)

}
