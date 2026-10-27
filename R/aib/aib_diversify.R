# Allied Industrial Base (AIB): the diversification scenario (docs/aib_methodology.md, section 5). Instead
# of removing a supplier, it asks what trade and investment would look like if a supply-chain step were
# as diversified as its higher-level sector: the step's exporter concentration (HHI) is brought down to
# a benchmark by capping every supplier's share, and the supply the capped suppliers lose is what the
# rest of the model (existing relationships, optimized trade, new capacity) has to replace. Pure.

# Herfindahl-Hirschman index of a vector of shares or values (0-1; 1 = a single supplier).
aib_hhi <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(NA_real_)
  s <- x / sum(x)
  sum(s^2)
}

# Shares after capping every share at `cap`: the excess goes to the uncapped suppliers in proportion to
# their shares, repeatedly, until none exceeds the cap. Shares must sum to 1.
aib_capped_shares <- function(shares, cap) {
  s <- as.numeric(shares)
  capped <- rep(FALSE, length(s))
  repeat {
    over <- !capped & s > cap + 1e-12
    if (!any(over)) break
    capped <- capped | over
    rest <- sum(s[!capped])
    mass <- 1 - cap * sum(capped)
    if (rest <= 0 || mass <= 0) {
      s[!capped] <- if (sum(!capped)) mass / sum(!capped) else 0
      s[capped] <- cap
      break
    }
    s[!capped] <- s[!capped] * mass / rest
    s[capped] <- cap
  }
  stats::setNames(s, names(shares))
}

# The supplier-share cap that brings the HHI of `shares` down to `target`. NA when the shares are already
# at or below the target. When the target is below what the number of suppliers allows (1 / n), the cap
# is 1 / n (equal shares), and `reached` says so.
aib_diversify_cap <- function(shares, target) {
  s <- as.numeric(shares)
  s <- s[is.finite(s) & s > 0]
  if (!length(s) || is.na(target)) return(list(cap = NA_real_, reached = NA))
  s <- s / sum(s)
  if (sum(s^2) <= target + 1e-12) return(list(cap = NA_real_, reached = TRUE))
  floor_cap <- 1 / length(s)
  if (target <= floor_cap + 1e-12) return(list(cap = floor_cap, reached = FALSE))
  hhi_at <- function(c) sum(aib_capped_shares(s, c)^2)
  lo <- floor_cap
  hi <- max(s)
  for (i in 1:60) {
    mid <- (lo + hi) / 2
    if (hhi_at(mid) > target) hi <- mid else lo <- mid
  }
  list(cap = lo, reached = TRUE)
}

# HHI of each HS6 line's exporters. exports: hs6, iso3, value (one year). Returns hs6, world, hhi.
aib_product_hhi <- function(exports) {
  exports %>%
    dplyr::filter(is.finite(value), value > 0) %>%
    dplyr::group_by(hs6) %>%
    dplyr::summarise(world = sum(value), hhi = sum((value / sum(value))^2), .groups = "drop")
}

# The benchmark concentration of each node: the concentration of its higher-level sector.
#   hs4     the trade-weighted mean HHI of the other HS6 lines of the node's parent HS4 heading(s)
#           (electric cars: the other lines of 8703, petrol, diesel and hybrid cars). product_hhi is
#           per HS6 line (aib_product_hhi(); the Atlas HS92 panel); own_codes (node_id, hs6) are the
#           node's own lines in that nomenclature, left out unless include_own (with the dictionary,
#           its hs1992_join codes: HS92 870390 holds electric cars).
#   sector  the trade-weighted mean HHI of the other nodes of the node's sector (node_hhi: node_id,
#           sector, hhi, world_trade).
# Returns node_id, benchmark_hs4, n_benchmark_lines, benchmark_hs4_headings, benchmark_sector,
# n_benchmark_nodes.
aib_benchmark_hhi <- function(node_codes, product_hhi, own_codes = NULL, node_hhi = NULL, include_own = FALSE) {
  nc <- node_codes %>% dplyr::distinct(node_id, hs6) %>% dplyr::mutate(hs4 = substr(hs6, 1, 4))
  own <- if (is.null(own_codes)) nc %>% dplyr::select(node_id, hs6) else dplyr::distinct(own_codes, node_id, hs6)
  ph <- product_hhi %>% dplyr::mutate(hs4 = substr(hs6, 1, 4))
  hs4 <- nc %>% dplyr::distinct(node_id, hs4) %>%
    dplyr::inner_join(ph, by = "hs4", relationship = "many-to-many")
  if (!isTRUE(include_own)) hs4 <- hs4 %>% dplyr::anti_join(own, by = c("node_id", "hs6"))
  b_hs4 <- hs4 %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(benchmark_hs4 = sum(hhi * world) / sum(world), n_benchmark_lines = dplyr::n(),
                     benchmark_hs4_headings = paste(sort(unique(hs4)), collapse = "; "), .groups = "drop")
  out <- nc %>% dplyr::distinct(node_id) %>% dplyr::left_join(b_hs4, by = "node_id")
  if (!is.null(node_hhi) && nrow(node_hhi)) {
    b_sec <- node_hhi %>% dplyr::select(node_id, sector, hhi, world_trade) %>%
      dplyr::inner_join(node_hhi %>% dplyr::select(other = node_id, sector, ohhi = hhi, ow = world_trade), by = "sector",
                        relationship = "many-to-many") %>%
      dplyr::filter(other != node_id, is.finite(ohhi), ow > 0) %>%
      dplyr::group_by(node_id) %>%
      dplyr::summarise(benchmark_sector = sum(ohhi * ow) / sum(ow), n_benchmark_nodes = dplyr::n(), .groups = "drop")
    out <- out %>% dplyr::left_join(b_sec, by = "node_id")
  } else {
    out$benchmark_sector <- NA_real_
    out$n_benchmark_nodes <- NA_integer_
  }
  out
}

# The diversification shock of each node. exporter_metrics: node_id, exporter_iso, exports (the world
# market of the node); benchmarks: node_id plus the benchmark columns; level: "hs4" (falling back to
# "sector" where the heading has no other lines) or "sector". Re-export hubs count with their origin
# (Hong Kong and Macao with China) and are capped with it. A node above its benchmark gets the cap that
# brings its HHI down to the benchmark (both measured on the world market), and every supplier above
# the cap loses the excess: fraction 1 - cap / share of its exports (shock_key). `scope_supply` (node_id,
# exporter_iso, value: shipments into the demand scope) makes the shares that set the excess those of
# the scope's supply, where the cap then binds (a supplier below the cap worldwide can be above it in
# the allied market). Returns one row per node with current_hhi, benchmark_hhi, benchmark_basis, cap,
# cap_reaches_benchmark, target_iso ("CHN+DEU"), shock_key (NA: already as diversified as the
# benchmark) and excess_share (the scope supply above the cap).
aib_diversify_targets <- function(exporter_metrics, benchmarks, level = c("hs4", "sector"), reexport_hubs = list(),
                                  min_excess = 0, scope_supply = NULL) {
  level <- match.arg(level)
  hub_of <- unlist(lapply(names(reexport_hubs), function(o) stats::setNames(rep(o, length(reexport_hubs[[o]])), reexport_hubs[[o]])))
  em <- exporter_metrics %>% dplyr::filter(is.finite(exports), exports > 0) %>%
    dplyr::mutate(group = dplyr::coalesce(unname(hub_of[exporter_iso]), exporter_iso))
  grp <- em %>% dplyr::group_by(node_id, group) %>% dplyr::summarise(exports = sum(exports), .groups = "drop")
  sgrp <- if (is.null(scope_supply)) NULL else scope_supply %>% dplyr::filter(is.finite(value), value > 0) %>%
    dplyr::mutate(group = dplyr::coalesce(unname(hub_of[exporter_iso]), exporter_iso)) %>%
    dplyr::group_by(node_id, group) %>% dplyr::summarise(exports = sum(value), .groups = "drop")
  b <- benchmarks %>% dplyr::mutate(
    benchmark_hhi = if (level == "hs4") dplyr::coalesce(benchmark_hs4, benchmark_sector) else benchmark_sector,
    benchmark_basis = dplyr::case_when(
      level == "hs4" & !is.na(benchmark_hs4) ~ paste0("other HS6 lines of heading ", benchmark_hs4_headings),
      !is.na(benchmark_sector) ~ "other nodes of the sector",
      TRUE ~ NA_character_))
  rows <- lapply(split(grp, grp$node_id), function(g) {
    nid <- g$node_id[1]
    bb <- b[b$node_id == nid, , drop = FALSE]
    target <- if (nrow(bb)) bb$benchmark_hhi[1] else NA_real_
    s <- stats::setNames(g$exports / sum(g$exports), g$group)
    cur <- sum(s^2)
    res <- if (!is.na(target) && cur > target + min_excess) aib_diversify_cap(s, target) else list(cap = NA_real_, reached = NA)
    key <- NA_character_
    tgt <- NA_character_
    excess <- NA_real_
    if (!is.na(res$cap)) {
      ss <- s
      if (!is.null(sgrp)) {
        sg <- sgrp[sgrp$node_id == nid, , drop = FALSE]
        ss <- if (nrow(sg)) stats::setNames(sg$exports / sum(sg$exports), sg$group) else numeric()
      }
      over <- ss[ss > res$cap + 1e-12]
      excess <- sum(over - res$cap)
    }
    if (!is.na(res$cap) && length(over)) {
      frac <- 1 - res$cap / over
      iso <- unlist(lapply(names(over), function(o) c(o, names(hub_of)[hub_of == o])))
      fr <- unlist(lapply(seq_along(over), function(k) rep(frac[[k]], 1 + sum(hub_of == names(over)[k]))))
      key <- aib_shock_key(iso, round(fr, 6))
      tgt <- paste(names(over)[order(-over)], collapse = "+")
    }
    tibble::tibble(node_id = nid, current_hhi = cur, benchmark_hhi = target,
                   benchmark_basis = if (nrow(bb)) bb$benchmark_basis[1] else NA_character_,
                   cap = res$cap, cap_reaches_benchmark = res$reached, target_iso = tgt, shock_key = key, excess_share = excess)
  })
  dplyr::bind_rows(rows)
}
