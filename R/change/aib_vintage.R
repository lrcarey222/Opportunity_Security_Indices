# AIB vintages (change layer for partnerships; docs/change-layer-architecture.md): the strategic layer of the AIB
# compacts bundle (dashboard/public/data/aib/compacts.<md5>.json, strategic v1) normalized into three tables, and a
# deterministic diff between two of them.
#
#   theses      one row per economy pair: its priority compacts (shared compact work) and purposes (trade, market
#               access, investment ...). A pair whose shared priority compacts grow is becoming more complementary.
#   roles       one row per compact x economy: the economy's role in the compact (compactRoles).
#   profiles    one row per economy: its primary and secondary strategic roles.
#
# Attribution is coarser than for posture: AIB bundles carry no input ledger, so a change is "real_world" only when
# the AIB method fingerprint (config/change_layer.yml aib_method_paths) is unchanged, else "unattributed".
# Pure functions: no IO.

AIB_VINTAGE_SCHEMA <- "osi-aib-vintage"

aib_join <- function(x) if (!length(x)) "" else paste(sort(unique(unlist(x))), collapse = ";")
aib_split <- function(s) if (is.na(s) || !nzchar(s)) character() else strsplit(s, ";", fixed = TRUE)[[1]]

# The three tables from a parsed compacts document (jsonlite::fromJSON(simplifyVector = FALSE)). NULL when the document
# has no strategic layer.
aib_vintage_from_doc <- function(doc) {
  s <- doc$strategic
  if (is.null(s) || is.null(s$theses)) return(NULL)
  th <- s$theses
  a <- vapply(th, function(t) as.character(t[[1]]), "")
  b <- vapply(th, function(t) as.character(t[[2]]), "")
  lo <- pmin(a, b); hi <- pmax(a, b)
  theses <- tibble::tibble(a = lo, b = hi, compacts = vapply(th, function(t) aib_join(t[[3]]), ""),
                           purposes = vapply(th, function(t) aib_join(t[[4]]), ""))
  theses$n_compacts <- vapply(theses$compacts, function(x) length(aib_split(x)), integer(1), USE.NAMES = FALSE)
  theses <- theses[!duplicated(theses[, c("a", "b")]), , drop = FALSE]
  cr <- s$compactRoles %||% list()
  roles <- tibble::tibble(compact = vapply(cr, function(r) as.character(r$compact %||% NA), ""),
                          iso3 = vapply(cr, function(r) as.character(r$iso3 %||% NA), ""),
                          role = vapply(cr, function(r) as.character(r$role %||% NA), ""),
                          allied = vapply(cr, function(r) isTRUE(r$allied), logical(1)))
  ct <- s$countries %||% list()
  profiles <- tibble::tibble(iso3 = vapply(ct, function(r) as.character(r$iso3 %||% NA), ""),
                             allied = vapply(ct, function(r) isTRUE(r$allied), logical(1)),
                             primary = vapply(ct, function(r) aib_join(r$primary), ""),
                             secondary = vapply(ct, function(r) aib_join(r$secondary), ""))
  names_ <- stats::setNames(vapply(s$compacts %||% list(), function(c) as.character(c$name %||% c$id), ""),
                            vapply(s$compacts %||% list(), function(c) as.character(c$id), ""))
  list(theses = theses, roles = roles, profiles = profiles, compact_names = as.list(names_))
}

# The diff of two AIB vintages. prev / cur: aib_vintage_from_doc() results; headers carry vintage_id and method id.
# Returns list(pair = <summary>, pairs = <pair changes>, membership = <role changes>, profiles = <profile changes>).
aib_compare <- function(prev, cur, prev_header, cur_header) {
  method_changed <- !identical(prev_header$method$id, cur_header$method$id)
  attribution <- if (method_changed) "unattributed" else "real_world"

  # pairs: shared priority compacts and purposes
  j <- dplyr::full_join(prev$theses, cur$theses, by = c("a", "b"), suffix = c("_prev", "_cur"))
  j$n_compacts_prev <- dplyr::coalesce(j$n_compacts_prev, 0L)
  j$n_compacts_cur <- dplyr::coalesce(j$n_compacts_cur, 0L)
  added <- mapply(function(p, c) aib_join(setdiff(aib_split(c), aib_split(p))), dplyr::coalesce(j$compacts_prev, ""), dplyr::coalesce(j$compacts_cur, ""))
  removed <- mapply(function(p, c) aib_join(setdiff(aib_split(p), aib_split(c))), dplyr::coalesce(j$compacts_prev, ""), dplyr::coalesce(j$compacts_cur, ""))
  j$compacts_added <- unname(added); j$compacts_removed <- unname(removed)
  j$purposes_changed <- dplyr::coalesce(j$purposes_prev, "") != dplyr::coalesce(j$purposes_cur, "")
  j$kind <- ifelse(is.na(j$compacts_prev) & !is.na(j$compacts_cur), "new_pair",
             ifelse(!is.na(j$compacts_prev) & is.na(j$compacts_cur), "dropped_pair",
             ifelse(nzchar(j$compacts_added) | nzchar(j$compacts_removed), "compacts_changed",
             ifelse(j$purposes_changed, "purposes_changed", NA_character_))))
  pairs <- j[!is.na(j$kind), , drop = FALSE]
  pairs$delta <- pairs$n_compacts_cur - pairs$n_compacts_prev
  pairs$direction <- ifelse(pairs$delta > 0, "more", ifelse(pairs$delta < 0, "less", "same"))
  pairs <- pairs[order(-abs(pairs$delta), pairs$kind != "compacts_changed", pairs$a, pairs$b), , drop = FALSE]
  pairs$attribution <- rep(attribution, nrow(pairs))

  # compact membership and roles
  m <- dplyr::full_join(prev$roles, cur$roles, by = c("compact", "iso3"), suffix = c("_prev", "_cur"))
  m$kind <- ifelse(is.na(m$role_prev) & !is.na(m$role_cur), "joined",
            ifelse(!is.na(m$role_prev) & is.na(m$role_cur), "left",
            ifelse(dplyr::coalesce(m$role_prev, "") != dplyr::coalesce(m$role_cur, ""), "role_changed", NA_character_)))
  membership <- m[!is.na(m$kind), c("compact", "iso3", "kind", "role_prev", "role_cur"), drop = FALSE]
  membership$attribution <- rep(attribution, nrow(membership))

  # country profiles: primary roles
  p <- dplyr::full_join(prev$profiles, cur$profiles, by = "iso3", suffix = c("_prev", "_cur"))
  p <- p[dplyr::coalesce(p$primary_prev, "") != dplyr::coalesce(p$primary_cur, ""), c("iso3", "primary_prev", "primary_cur"), drop = FALSE]

  summary <- list(prev = prev_header$vintage_id, cur = cur_header$vintage_id, method_changed = method_changed, attribution = attribution,
                  n_pairs_prev = nrow(prev$theses), n_pairs_cur = nrow(cur$theses),
                  n_more = sum(pairs$direction == "more"), n_less = sum(pairs$direction == "less"),
                  n_new = sum(pairs$kind == "new_pair"), n_dropped = sum(pairs$kind == "dropped_pair"),
                  n_membership = nrow(membership), n_profiles = nrow(p))
  list(pair = summary, pairs = pairs, membership = membership, profiles = p)
}

# One sentence per pair change, from its fields.
aib_pair_explain <- function(pairs, compact_names) {
  nm <- function(ids) { ids <- aib_split(ids); if (!length(ids)) return(""); paste(vapply(ids, function(i) compact_names[[i]] %||% i, ""), collapse = ", ") }
  vapply(seq_len(nrow(pairs)), function(i) {
    r <- pairs[i, ]
    base <- switch(r$kind,
      new_pair = sprintf("New relationship thesis with %d shared priority compact%s", r$n_compacts_cur, if (r$n_compacts_cur == 1) "" else "s"),
      dropped_pair = "No longer a relationship thesis",
      compacts_changed = sprintf("Shared priority compacts %d → %d", r$n_compacts_prev, r$n_compacts_cur),
      purposes_changed = sprintf("Purpose changed: %s → %s", gsub(";", ", ", dplyr::coalesce(r$purposes_prev, "")), gsub(";", ", ", dplyr::coalesce(r$purposes_cur, ""))),
      "")
    extra <- if (r$kind == "new_pair") { if (nzchar(r$compacts_added)) nm(r$compacts_added) else character() }
             else c(if (nzchar(r$compacts_added)) sprintf("added %s", nm(r$compacts_added)), if (nzchar(r$compacts_removed)) sprintf("removed %s", nm(r$compacts_removed)))
    paste0(base, if (length(extra)) paste0(" (", paste(extra, collapse = "; "), ")") else "", ".")
  }, character(1))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
