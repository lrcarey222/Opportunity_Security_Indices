# Allied Industrial Base (AIB): linear programs and the lexicographic sequence
# (docs/aib_methodology.md, 5). GLPK through Rglpk (installed with the allied network's MILP stack);
# every AIB program is a pure LP, so no integer variables and no time-outs in practice.

aib_has_lp <- function() {
  requireNamespace("Rglpk", quietly = TRUE) && requireNamespace("slam", quietly = TRUE)
}

# Maximize obj'x subject to rows (i, j, v triplets; dir; rhs) and variable bounds.
# Returns list(x, value, status) with status 0 = optimal.
aib_lp_solve <- function(obj, rows, lower, upper, time_limit_ms = 120000) {
  n <- length(obj)
  mat <- slam::simple_triplet_matrix(rows$i, rows$j, rows$v, nrow = rows$nrow, ncol = n)
  up_idx <- which(is.finite(upper))
  bounds <- list(
    lower = list(ind = seq_len(n), val = lower),
    upper = list(ind = up_idx, val = upper[up_idx])
  )
  res <- Rglpk::Rglpk_solve_LP(
    obj = obj, mat = mat, dir = rows$dir, rhs = rows$rhs, bounds = bounds, max = TRUE,
    control = list(tm_limit = time_limit_ms)
  )
  list(x = res$solution, value = res$optimum, status = res$status)
}

aib_rows_add <- function(rows, j, v, dir, rhs) {
  r <- rows$nrow + 1L
  rows$i <- c(rows$i, rep(r, length(j)))
  rows$j <- c(rows$j, j)
  rows$v <- c(rows$v, v)
  rows$dir <- c(rows$dir, dir)
  rows$rhs <- c(rows$rhs, rhs)
  rows$nrow <- r
  rows
}

# Lexicographic maximization: objective k is maximized among solutions that keep every earlier
# objective within its tolerance of the earlier optimum (relative, with a small absolute floor).
# `steps` stops early (the coverage-only runs of the coalition test). Returns list(x, optima,
# status); status is the first failing step's GLPK status, 0 when every step solved.
aib_lp_lexicographic <- function(objectives, rows, lower, upper, tolerances, steps = length(objectives)) {
  steps <- min(steps, length(objectives))
  optima <- rep(NA_real_, length(objectives))
  x <- NULL
  status <- 0L
  for (k in seq_len(steps)) {
    res <- aib_lp_solve(objectives[[k]], rows, lower, upper)
    if (res$status != 0) {
      if (is.null(x)) return(list(x = NULL, optima = optima, status = res$status, failed_step = k))
      # A later step failed: keep the last solved step's solution, and say so.
      status <- res$status
      break
    }
    x <- res$x
    optima[k] <- res$value
    if (k < steps) {
      tol <- tolerances[min(k, length(tolerances))]
      nz <- which(objectives[[k]] != 0)
      floor_k <- res$value - max(tol * abs(res$value), 1e-9)
      rows <- aib_rows_add(rows, nz, objectives[[k]][nz], ">=", floor_k)
    }
  }
  list(x = x, optima = optima, status = status, failed_step = if (status != 0L) k else NA_integer_)
}
