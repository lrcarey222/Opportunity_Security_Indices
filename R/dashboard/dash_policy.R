# Dashboard L2: the composite Policy index for any component weights (pure; no IO).
#
# Exact port of build_composite_policy_index() (R/indices/build_policy_index.R):
#   P_its = sum_v w_v x^v_its / sum_v w_v over the components v observed in the cell;
#   a country is scored only if every REQUIRED component is observed in at least one of its cells.
#   bundle$policy$required (the pipeline's required_components, which scripts/85 reads from
#   policy_index_provenance) is a subset of the weighted components; absent, every weighted component
#   is required (the rule until 9 Oct 2026). No dial changes it (no weight reaches 0).
# Since 9 Oct 2026 IEA PAMS is the only required component and CAT is added where it rates the
# country, so an eligible country's index rests on "IEA PAMS + CAT" or on "IEA PAMS only":
# policy_basis (the pipeline's policy_basis_label()), carried on the eligibility rows, the index rows
# and, through dash_si(), the Strategic Index cells and the allied picks. An ineligible country has
# policy_basis NA and missing_required names the required components it lacks.
# bundle$policy (scripts/85): cells = tibble(Country, tech, supply_chain) (C-locale sorted),
# variables = the component variables with a baseline weight (C-locale sorted), values = matrix
# [cell x variable] of the per-cell means of duplicate rows (NA where absent) and components = the
# pipeline's component order (policy_index_provenance$components), which orders policy_basis,
# missing_components and missing_required as the pipeline does (absent: the variables' order).
# rowSums() accumulates in long double like the pipeline's sum(), over the same (alphabetical)
# component order.

# "IEA PAMS Policy Index" -> "IEA PAMS" (the pipeline's policy_component_label()).
dash_policy_component_label <- function(x) sub(" (Policy Index|policy score)$", "", x)

# The components a country's index rests on, in the pipeline's order: "IEA PAMS + CAT",
# "IEA PAMS only" (the pipeline's policy_basis_label()).
dash_policy_basis_label <- function(components) {
  labels <- dash_policy_component_label(components)
  if (length(labels) == 1) paste(labels, "only") else paste(labels, collapse = " + ")
}

dash_policy <- function(bundle, params) {
  pb <- bundle$policy
  w <- params$policy$weights
  vars <- pb$variables
  missing_vars <- setdiff(names(w), vars)
  if (length(missing_vars)) stop("Policy weights name components the bundle lacks: ", paste(missing_vars, collapse = ", "), ".")
  use <- vars[vars %in% names(w)]
  V <- pb$values[, use, drop = FALSE]
  Wm <- matrix(unname(w[use]), nrow = nrow(V), ncol = length(use), byrow = TRUE)
  ok <- !is.na(V)

  # Eligibility: each required component observed somewhere for the country
  has <- rowsum(ok * 1L, pb$cells$Country, reorder = TRUE) > 0
  req <- if (is.null(pb$required)) use else intersect(use, pb$required)
  if (!length(req)) req <- use
  eligible_all <- if (length(req) == length(use)) rowSums(has) == length(use) else rowSums(has[, req, drop = FALSE]) == length(req)
  # the pipeline's component order (its component_weights), for the basis label and the missing lists
  comp <- as.character(pb$components)
  ord <- c(intersect(comp, use), setdiff(use, comp))
  has_o <- has[, ord, drop = FALSE]
  req_o <- ord %in% req
  n_c <- nrow(has_o)
  eligible_tbl <- tibble::tibble(
    Country = rownames(has),
    n_components = unname(as.integer(rowSums(has))),
    eligible = unname(eligible_all),
    policy_basis = vapply(seq_len(n_c), function(i) {
      if (eligible_all[i]) dash_policy_basis_label(ord[has_o[i, ]]) else NA_character_
    }, character(1)),
    missing_components = vapply(seq_len(n_c), function(i) {
      if (all(has_o[i, ])) NA_character_ else paste(ord[!has_o[i, ]], collapse = "; ")
    }, character(1)),
    missing_required = vapply(seq_len(n_c), function(i) {
      if (eligible_all[i]) NA_character_ else paste(ord[!has_o[i, ] & req_o], collapse = "; ")
    }, character(1))
  )
  eligible_tbl <- eligible_tbl[order(!eligible_tbl$eligible, eligible_tbl$Country, method = "radix"), ]
  eligible <- eligible_tbl$Country[eligible_tbl$eligible]

  num <- rowSums(V * Wm, na.rm = TRUE)
  den <- rowSums(ifelse(ok, Wm, 0))
  value <- ifelse(den == 0, NA_real_, num / den)
  keep <- pb$cells$Country %in% eligible & !is.na(value)
  index <- tibble::tibble(
    Country = pb$cells$Country[keep],
    tech = pb$cells$tech[keep],
    supply_chain = pb$cells$supply_chain[keep],
    value = value[keep],
    n_components = as.integer(rowSums(ok))[keep],
    policy_basis = eligible_tbl$policy_basis[match(pb$cells$Country[keep], eligible_tbl$Country)]
  )
  list(index = index, eligibility = eligible_tbl, weights = w[use])
}
