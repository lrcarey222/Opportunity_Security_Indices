# The US electro-industrial trading system (opening section of the trade-fence explainer: charts 00a, 00b and the
# fence-change analysis). Pure: no IO. scripts/trade_fence_explainer/03_build_trade_overview.R reads the inputs.
#
# Conventions shared with the rest of the explainer: the HS6 basket of data/reference/energy_hs6_master.csv
# through tri_basket_cells(); Deployment is never a traded-goods stage (it repeats Assembly); a code in several
# technologies counts in each technology's total ("attributed") and once in any cross-technology total.

TFE_CLEAN_TECHS <- c("Batteries", "Electric Vehicles", "Solar", "Wind", "Electric Grid", "Nuclear")
TFE_FUEL_TECHS <- c("Oil", "Gas", "Coal")
TFE_TRADE_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Recovery")

# Unique (tech, stage, hs6) of the producing stages for `techs`.
tfe_producing_cells <- function(cells, techs) {
  x <- unique(cells[cells$tech %in% techs & cells$stage != "Deployment", c("tech", "stage", "hs6")])
  rownames(x) <- NULL
  x
}

# Each HS6 code's weight in each stage: a code that sits in different stages in different technologies is split
# equally across those stages, so stage totals add up to the unique-code total.
tfe_stage_weights <- function(pc) {
  s <- unique(pc[c("stage", "hs6")])
  n <- table(s$hs6)
  s$weight <- 1 / as.numeric(n[s$hs6])
  s
}

# Sum of `value` over `months` by key columns (`by`), from a long table with a `time` column ("YYYY-MM").
tfe_period_sum <- function(d, months, by) {
  d <- d[d$time %in% months, , drop = FALSE]
  if (nrow(d) == 0) return(cbind(unique(d[by]), value = numeric(0)))
  stats::aggregate(d["value"], by = d[by], FUN = sum)
}

# Imports and exports by technology for two periods. `imp` and `exp`: hs6 + value for each period (columns
# hs6, period, value); `pc`: tfe_producing_cells(). Attributed = every code of the technology; exclusive = codes
# in that technology only. Returns one row per technology with both periods and changes.
tfe_trade_by_tech <- function(imp, exp, pc, periods) {
  codes <- unique(pc[c("tech", "hs6")])
  ntech <- table(codes$hs6)
  out <- lapply(split(codes, codes$tech), function(cd) {
    ex_codes <- cd$hs6[as.numeric(ntech[cd$hs6]) == 1]
    v <- function(d, per, keep) sum(d$value[d$period == per & d$hs6 %in% keep])
    r <- data.frame(tech = cd$tech[1], n_hs6 = nrow(cd), n_hs6_exclusive = length(ex_codes))
    for (p in names(periods)) {
      r[[paste0("imports_", p)]] <- v(imp, p, cd$hs6)
      r[[paste0("exports_", p)]] <- v(exp, p, cd$hs6)
      r[[paste0("imports_exclusive_", p)]] <- v(imp, p, ex_codes)
    }
    r
  })
  out <- do.call(rbind, out)
  p1 <- names(periods)[1]
  p2 <- names(periods)[2]
  for (p in names(periods)) {
    out[[paste0("balance_", p)]] <- out[[paste0("exports_", p)]] - out[[paste0("imports_", p)]]
    out[[paste0("import_export_ratio_", p)]] <- ifelse(out[[paste0("exports_", p)]] > 0, out[[paste0("imports_", p)]] / out[[paste0("exports_", p)]], NA_real_)
  }
  out$imports_change_pct <- 100 * (out[[paste0("imports_", p2)]] / out[[paste0("imports_", p1)]] - 1)
  out$exports_change_pct <- 100 * (out[[paste0("exports_", p2)]] / out[[paste0("exports_", p1)]] - 1)
  out$group <- ifelse(out$tech %in% TFE_FUEL_TECHS, "Fuels (oil, gas, coal)", "Clean energy and grid")
  rownames(out) <- NULL
  out
}

# Imports and exports by stage (equal split of codes across the stages they sit in), for two periods.
tfe_trade_by_stage <- function(imp, exp, pc, periods, label) {
  sw <- tfe_stage_weights(pc)
  out <- lapply(TFE_TRADE_STAGES, function(st) {
    w <- sw[sw$stage == st, ]
    r <- data.frame(basket = label, stage = st, n_hs6 = nrow(w))
    for (p in names(periods)) {
      f <- function(d) { x <- d[d$period == p, ]; sum(x$value[match(w$hs6, x$hs6)] * w$weight, na.rm = TRUE) }
      r[[paste0("imports_", p)]] <- f(imp)
      r[[paste0("exports_", p)]] <- f(exp)
    }
    r
  })
  out <- do.call(rbind, out)
  p1 <- names(periods)[1]
  p2 <- names(periods)[2]
  for (p in names(periods)) out[[paste0("balance_", p)]] <- out[[paste0("exports_", p)]] - out[[paste0("imports_", p)]]
  out$imports_change_pct <- 100 * (out[[paste0("imports_", p2)]] / out[[paste0("imports_", p1)]] - 1)
  out$exports_change_pct <- 100 * (out[[paste0("exports_", p2)]] / out[[paste0("exports_", p1)]] - 1)
  out$import_share_of_basket <- out[[paste0("imports_", p2)]] / sum(out[[paste0("imports_", p2)]])
  out
}

# Sourcing and concentration of one set of HS6 codes in one period. `pm`: partner-level values (columns
# partner, iso3, group, hs6, value) for the named partners plus the rest-of-world residual (partner "ROW", group
# "Unassigned (rest of world)"). Shares are of total imports. Concentration is measured over the named partners
# only (the residual is many unnamed countries), so HHI is a lower bound and top shares exclude the residual.
tfe_sourcing <- function(pm, codes) {
  d <- pm[pm$hs6 %in% codes, , drop = FALSE]
  tot <- sum(d$value)
  nm <- d[d$group != "Unassigned (rest of world)", , drop = FALSE]
  named <- tapply(nm$value, nm$partner, sum)
  named <- named[!is.na(named) & named > 0]
  grp <- tapply(d$value, d$group, sum)
  g <- function(k) if (k %in% names(grp) && tot > 0) unname(grp[k]) / tot else 0
  s <- if (tot > 0) sort(named / tot, decreasing = TRUE) else numeric(0)
  cn <- if (tot > 0) sum(d$value[d$iso3 %in% "CHN"]) / tot else NA_real_
  sh_of <- function(iso) if (tot > 0) sum(d$value[d$iso3 %in% iso]) / tot else NA_real_
  hhi <- sum(s^2)
  data.frame(imports = tot, china_share = cn, coercive_share = g("Coercive"), coercive_excl_china_share = g("Coercive") - cn,
             trusted_share = g("Trusted"), nonaligned_share = g("Nonaligned"), unassigned_share = g("Unassigned (rest of world)"),
             north_america_share = sh_of(c("CAN", "MEX")), southeast_asia_named_share = sh_of(c("VNM", "THA", "MYS", "IDN", "SGP")),
             top_supplier = if (length(s)) names(s)[1] else NA_character_, top_supplier_share = if (length(s)) s[[1]] else NA_real_,
             top3_share = sum(utils::head(s, 3)), hhi = hhi, effective_suppliers = if (hhi > 0) 1 / hhi else NA_real_,
             n_suppliers_1pct = sum(s >= 0.01), stringsAsFactors = FALSE)
}

# Diversification metrics for every technology (all its producing-stage codes) and every technology x stage cell,
# for two periods, with 2024 -> 2026 changes (percentage points for shares).
tfe_diversification <- function(pm, pc, periods) {
  units <- c(lapply(split(pc, pc$tech), function(x) list(tech = x$tech[1], stage = "All producing stages", codes = unique(x$hs6))),
             lapply(split(pc, paste(pc$tech, pc$stage)), function(x) list(tech = x$tech[1], stage = x$stage[1], codes = unique(x$hs6))))
  p1 <- names(periods)[1]
  p2 <- names(periods)[2]
  out <- lapply(units, function(u) {
    a <- tfe_sourcing(pm[pm$period == p1, ], u$codes)
    b <- tfe_sourcing(pm[pm$period == p2, ], u$codes)
    names(a) <- paste0(names(a), "_", p1)
    names(b) <- paste0(names(b), "_", p2)
    r <- cbind(data.frame(tech = u$tech, stage = u$stage, n_hs6 = length(u$codes), stringsAsFactors = FALSE), a, b)
    r$imports_change_pct <- 100 * (r[[paste0("imports_", p2)]] / r[[paste0("imports_", p1)]] - 1)
    for (k in c("china_share", "coercive_share", "coercive_excl_china_share", "trusted_share", "nonaligned_share", "unassigned_share",
                "north_america_share", "southeast_asia_named_share", "top_supplier_share", "top3_share")) {
      r[[paste0(k, "_change_pts")]] <- 100 * (r[[paste0(k, "_", p2)]] - r[[paste0(k, "_", p1)]])
    }
    r$hhi_change <- r[[paste0("hhi_", p2)]] - r[[paste0("hhi_", p1)]]
    r$effective_suppliers_change <- r[[paste0("effective_suppliers_", p2)]] - r[[paste0("effective_suppliers_", p1)]]
    r$n_suppliers_1pct_change <- r[[paste0("n_suppliers_1pct_", p2)]] - r[[paste0("n_suppliers_1pct_", p1)]]
    r
  })
  out <- do.call(rbind, out)
  out$quadrant <- tfe_quadrant(out$imports_change_pct, out$china_share_change_pts)
  rownames(out) <- NULL
  out
}

# The four readings of chart 00b: import growth x change in China's share.
tfe_quadrant <- function(import_change_pct, china_change_pts) {
  ifelse(is.na(import_change_pct) | is.na(china_change_pts), NA_character_,
    ifelse(import_change_pct >= 0,
      ifelse(china_change_pts < 0, "Diversification during expansion", "Deepening dependence"),
      ifelse(china_change_pts < 0, "Diversification through contraction / substitution", "Concentration despite contraction")))
}

# Fence change vs coercive dependence (analysis): one row per cell with the change in all-supplier TRI (Census duty,
# 2024 rates to the data month) and the change in coercive-supplier share; archetype from two lines.
tfe_fence_vs_diversification <- function(tri, div, fence_line = 10, dep_line = -5) {
  d <- merge(tri, div, by = c("tech", "stage"))
  d$archetype <- ifelse(d$tri_change_pts >= fence_line,
    ifelse(d$coercive_share_change_pts <= dep_line, "Higher fence + lower coercive dependence", "Higher fence + little diversification"),
    ifelse(d$coercive_share_change_pts <= dep_line, "Low fence change + reduced dependence anyway", "Low fence change + persistent dependence"))
  d
}
