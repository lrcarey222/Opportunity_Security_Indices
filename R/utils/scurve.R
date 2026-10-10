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

# median_scurve() with a fixed reference set. Values with `in_ref` TRUE get exactly
# median_scurve() over the reference values alone. Every other value is placed by adding it,
# alone, to the reference set: r = (reference values below it + half those equal to it) / number
# of reference values, the percentile rank it would take there. So it is on the reference scale,
# and no value outside the reference can move a reference value. Without a reference value every
# result is NA.
median_scurve_ref <- function(x, in_ref, gamma = 0.5) {
  if (!is.numeric(x)) {
    stop("median_scurve_ref() expects numeric input.")
  }
  if (!is.logical(in_ref) || length(in_ref) != length(x)) {
    stop("median_scurve_ref() expects `in_ref` to be a logical vector as long as `x`.")
  }
  in_ref <- !is.na(in_ref) & in_ref
  out <- rep(NA_real_, length(x))
  out[in_ref] <- median_scurve(x[in_ref], gamma = gamma)

  ref <- x[in_ref & !is.na(x)]
  others <- which(!in_ref & !is.na(x))
  if (length(ref) == 0 || length(others) == 0) {
    return(out)
  }
  r <- vapply(x[others], function(v) (sum(ref < v) + 0.5 * sum(ref == v)) / length(ref), numeric(1))
  out[others] <- (r^gamma) / (r^gamma + (1 - r)^gamma)
  out
}
