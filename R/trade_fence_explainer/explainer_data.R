# Data for the NEIS trade-fence explainer (docs: outputs/trade_fence_explainer/README.md). Pure: no IO.
# scripts/trade_fence_explainer/01_build_data.R reads the inputs and writes the chart CSVs.
#
# Builds on the US trade restrictiveness measure (R/strategic_position/trade_restrictiveness.R,
# scripts/28_build_us_trade_restrictiveness.R): the same HS10 x partner goods, 2024 weights, supplier groups,
# AD/CVD rule and verdict logic (tri_posture_verdict()). Nothing here changes those; it re-cuts them.
# Needs the tri_* functions to be sourced first.

# The four explainer verdicts, in display order, and the audit verdicts behind each. "No tariff fence" and
# "Partial fence" (a Secure cell without a high fence on coercive suppliers) are both a fence below the
# recommendation.
TFE_VERDICTS <- c("Aligned", "Taxes trusted partners", "Fence above recommendation",
                  "Fence below recommendation / missing")
tfe_verdict4 <- function(verdict) {
  out <- rep(NA_character_, length(verdict))
  out[verdict %in% "In line"] <- TFE_VERDICTS[1]
  out[verdict %in% "Taxes trusted partners"] <- TFE_VERDICTS[2]
  out[verdict %in% "Fence above recommendation"] <- TFE_VERDICTS[3]
  out[verdict %in% c("No tariff fence", "Partial fence")] <- TFE_VERDICTS[4]
  out
}

# The six choices as the dashboard names them (dashboard/src/content/vocabulary.ts).
TFE_CHOICE_LABEL <- c(secure = "Secure", preserve = "Preserve", catch_up = "Catch Up", partner = "Partner",
                      accelerate = "Accelerate", watch = "Watch", not_assessed = "Not assessed")

# Import-weighted shares and totals of `w` within groups: a small helper for the exposure tables.
tfe_sum_by <- function(x, by) {
  if (length(x) == 0) return(stats::setNames(numeric(0), character(0)))
  s <- tapply(x, by, sum)
  s[is.na(s)] <- 0
  s
}

# Rates of a set of goods lines on monthly columns: sum of duty over sum of value for the `months`
# (columns v_<m>, d_<m>). Returns c(value, duty, rate) with rate in percent.
tfe_collected_rate <- function(g, months) {
  v <- sum(vapply(months, function(m) sum(g[[paste0("v_", m)]]), numeric(1)))
  d <- sum(vapply(months, function(m) sum(g[[paste0("d_", m)]]), numeric(1)))
  c(value = v, duty = d, rate = if (v > 0) 100 * d / v else NA_real_)
}

# Lines of the monthly Census panel: the tracker's named partners and the rest-of-world residual. Partners
# carved out of the residual for the 2024 weights (tri_split_rest_of_world()) carry copies of the residual's
# monthly columns, so monthly sums must leave them out.
tfe_monthly_lines <- function(goods, named_codes) {
  goods[goods$cty_code %in% c(named_codes, TRI_ROW_CODE), , drop = FALSE]
}

# Dollar-weighted verdicts (chart 6). `goods`: lines with hs6, group and w (2024 imports); `cells`:
# tri_basket_cells(); `verdicts`: tech, stage, verdict4 (NA = not assessed). Returns one row per basis x
# supplier set x verdict with the import value (USD) and its share of the basis total:
#   gross                every assessed cell counts all of its imports, Deployment included (a code in four
#                        cells counts four times; Deployment repeats Assembly);
#   gross_no_deployment  the same without Deployment cells;
#   dedup_equal_split    each HS6 code counted once: its imports are split equally across the assessed
#                        producing-stage cells it sits in (Deployment excluded, since it repeats Assembly);
#   dedup_deployment     as dedup_equal_split, but an Assembly cell takes the verdict of its technology's
#                        Deployment cell (the build-out's posture rather than the producers').
# Supplier sets: all suppliers and trusted partners only.
tfe_dollar_weighted <- function(goods, cells, verdicts) {
  v <- verdicts[!is.na(verdicts$verdict4), c("tech", "stage", "verdict4")]
  cv <- merge(unique(cells[c("tech", "stage", "hs6")]), v, by = c("tech", "stage"))
  dep <- v[v$stage == "Deployment", c("tech", "verdict4")]
  names(dep)[2] <- "verdict_dep"
  sets <- list(`All suppliers` = rep(TRUE, nrow(goods)), `Trusted partners` = goods$group %in% "Trusted")
  out <- list()
  for (set in names(sets)) {
    w6 <- tfe_sum_by(goods$w[sets[[set]]], goods$hs6[sets[[set]]])
    x <- cv
    x$w <- unname(w6[x$hs6])
    x$w[is.na(x$w)] <- 0
    prod <- x[x$stage != "Deployment", , drop = FALSE]
    n_cells <- tfe_sum_by(rep(1, nrow(prod)), prod$hs6)
    prod$w_split <- prod$w / unname(n_cells[prod$hs6])
    prod2 <- merge(prod, dep, by = "tech", all.x = TRUE)
    prod2$verdict_alt <- ifelse(prod2$stage == "Assembly" & !is.na(prod2$verdict_dep), prod2$verdict_dep, prod2$verdict4)
    bases <- list(
      gross = tfe_sum_by(x$w, x$verdict4),
      gross_no_deployment = tfe_sum_by(prod$w, prod$verdict4),
      dedup_equal_split = tfe_sum_by(prod$w_split, prod$verdict4),
      dedup_deployment = tfe_sum_by(prod2$w_split, prod2$verdict_alt)
    )
    for (b in names(bases)) {
      s <- bases[[b]]
      tot <- sum(s)
      out[[length(out) + 1]] <- data.frame(
        basis = b, suppliers = set, verdict = TFE_VERDICTS,
        imports_usd = vapply(TFE_VERDICTS, function(k) if (k %in% names(s)) unname(s[k]) else 0, numeric(1)),
        basis_total_usd = tot, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out)
  out$share <- ifelse(out$basis_total_usd > 0, out$imports_usd / out$basis_total_usd, NA_real_)
  rownames(out) <- NULL
  out
}

# Basket imports covered by the dedup bases: unique HS6 imports of assessed producing cells against all basket
# imports (codes only in not-assessed cells drop out). One row per supplier set.
tfe_dedup_coverage <- function(goods, cells, verdicts) {
  v <- verdicts[!is.na(verdicts$verdict4) & verdicts$stage != "Deployment", c("tech", "stage")]
  assessed <- unique(merge(unique(cells[c("tech", "stage", "hs6")]), v, by = c("tech", "stage"))$hs6)
  basket <- unique(cells$hs6)
  sets <- list(`All suppliers` = rep(TRUE, nrow(goods)), `Trusted partners` = goods$group %in% "Trusted")
  do.call(rbind, lapply(names(sets), function(set) {
    g <- goods[sets[[set]], , drop = FALSE]
    data.frame(suppliers = set, basket_hs6 = length(basket), assessed_hs6 = length(assessed),
               basket_imports_usd = sum(g$w[g$hs6 %in% basket]), assessed_imports_usd = sum(g$w[g$hs6 %in% assessed]),
               stringsAsFactors = FALSE)
  }))
}

# Technology detail of the dollar-weighted verdicts: within a technology a code sits in one stage, so the
# producing-stage cells do not overlap; technologies do overlap (shared codes count in each).
tfe_dollar_weighted_by_tech <- function(goods, cells, verdicts) {
  v <- verdicts[!is.na(verdicts$verdict4) & verdicts$stage != "Deployment", c("tech", "stage", "verdict4")]
  cv <- merge(unique(cells[c("tech", "stage", "hs6")]), v, by = c("tech", "stage"))
  w_all <- tfe_sum_by(goods$w, goods$hs6)
  tr <- goods$group %in% "Trusted"
  w_tr <- tfe_sum_by(goods$w[tr], goods$hs6[tr])
  cv$imports_usd <- unname(w_all[cv$hs6]); cv$imports_usd[is.na(cv$imports_usd)] <- 0
  cv$trusted_imports_usd <- unname(w_tr[cv$hs6]); cv$trusted_imports_usd[is.na(cv$trusted_imports_usd)] <- 0
  a <- stats::aggregate(cbind(imports_usd, trusted_imports_usd) ~ tech + verdict4, data = cv, FUN = sum)
  tot <- stats::aggregate(cbind(imports_usd, trusted_imports_usd) ~ tech, data = cv, FUN = sum)
  names(tot)[2:3] <- c("tech_total_usd", "tech_trusted_total_usd")
  a <- merge(a, tot, by = "tech")
  a$share <- a$imports_usd / a$tech_total_usd
  a$trusted_share <- ifelse(a$tech_trusted_total_usd > 0, a$trusted_imports_usd / a$tech_trusted_total_usd, NA_real_)
  names(a)[names(a) == "verdict4"] <- "verdict"
  a[order(a$tech, match(a$verdict, TFE_VERDICTS)), ]
}

# Partner effective tariff (chart 3): duties collected over imports in `months` for each named partner, all
# goods. `goods` must be the monthly lines (tfe_monthly_lines()).
tfe_partner_rates <- function(goods, months) {
  key <- goods$cty_code
  v <- Reduce(`+`, lapply(months, function(m) tfe_sum_by(goods[[paste0("v_", m)]], key)))
  d <- Reduce(`+`, lapply(months, function(m) tfe_sum_by(goods[[paste0("d_", m)]], key)))
  out <- data.frame(cty_code = names(v), imports_usd = as.numeric(v), duty_usd = as.numeric(d[names(v)]),
                    stringsAsFactors = FALSE)
  out$effective_rate <- ifelse(out$imports_usd > 0, 100 * out$duty_usd / out$imports_usd, NA_real_)
  out
}

# Import-weighted average of partner rates by group (chart 3 annotations); `x` has group, iso3,
# effective_rate and imports_usd. Adds "Coercive excl. China".
tfe_group_rates <- function(x) {
  f <- function(d) sum(d$effective_rate * d$imports_usd) / sum(d$imports_usd)
  groups <- c("Trusted", "Nonaligned", "Coercive")
  rows <- lapply(groups, function(g) {
    d <- x[x$group %in% g, , drop = FALSE]
    data.frame(group = g, n_partners = nrow(d), effective_rate = if (nrow(d)) f(d) else NA_real_,
               imports_usd = sum(d$imports_usd), stringsAsFactors = FALSE)
  })
  d <- x[x$group %in% "Coercive" & !x$iso3 %in% "CHN", , drop = FALSE]
  rows[[length(rows) + 1]] <- data.frame(group = "Coercive excl. China", n_partners = nrow(d),
                                         effective_rate = if (nrow(d)) f(d) else NA_real_,
                                         imports_usd = sum(d$imports_usd), stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

# Comtrade numeric partner codes that differ from ISO 3166 numeric (Comtrade reporter/partner list).
TFE_COMTRADE_ISO3 <- c(`251` = "FRA", `699` = "IND", `757` = "CHE", `579` = "NOR", `842` = "USA",
                       `490` = "TWN", `711` = "ZAF", `381` = "ITA", `58` = "BEL", `56` = "BEL")

# Single-measure warnings of the fence-tools bands (chart 4): a High or Medium band that rests on one NIPO
# measure (the workbook's dagger). `stage_tbl`: fence_stage_tbl.csv; `measures`: fence_measures.csv.
TFE_TOOLS <- c(tariff = "Tariffs", trade_defence = "AD/CVD", import_ban = "Import bans",
               other_import_restriction = "Quotas / licensing", localisation_requirement = "Local-content rules",
               local_content_incentive = "Local-content incentives", procurement_preference = "Public purchasing",
               investment_restriction = "Investment restrictions", export_restriction = "Export controls")
TFE_TOOL_GROUP <- c(tariff = "At the border", trade_defence = "At the border", import_ban = "At the border",
                    other_import_restriction = "At the border", localisation_requirement = "Domestic market",
                    local_content_incentive = "Domestic market", procurement_preference = "Domestic market",
                    investment_restriction = "Capital / technology", export_restriction = "Capital / technology")
TFE_TOOL_NIPO <- c(trade_defence = "Anti-dumping, countervailing and safeguard duties", import_ban = "Import bans",
                   other_import_restriction = "Quotas, licensing and other import restrictions",
                   localisation_requirement = "Local-content requirements", local_content_incentive = "Local-content incentives",
                   procurement_preference = "Buy-domestic procurement rules",
                   investment_restriction = "Investment and ownership restrictions",
                   export_restriction = "Export controls (outward fence)")
tfe_fence_tools <- function(stage_tbl, measures, chains = c("Electric Grid" = "Grid equipment", Solar = "Solar",
                                                            Batteries = "Batteries")) {
  measures <- measures[!duplicated(measures[["Entry ID"]]), , drop = FALSE]
  rows <- list()
  for (tech in names(chains)) {
    chain <- chains[[tech]]
    for (stage in c("Extraction", "Refining", "Components", "Assembly", "Recovery")) {
      r <- stage_tbl[stage_tbl[["Supply chain"]] == chain & stage_tbl[["Stage"]] == stage, , drop = FALSE]
      if (nrow(r) == 0 || is.na(r[["tariff|band"]][1]) || !nzchar(r[["tariff|band"]][1])) next
      r <- r[1, , drop = FALSE]
      label <- paste(chain, stage)
      for (k in names(TFE_TOOLS)) {
        n <- if (k == "tariff") NA_integer_ else {
          m <- measures[measures[["Tool"]] == TFE_TOOL_NIPO[[k]] & grepl(label, measures[["Stages touched"]], fixed = TRUE), , drop = FALSE]
          length(unique(m[["Entry ID"]]))
        }
        band <- r[[paste0(k, "|band")]]
        rows[[length(rows) + 1]] <- data.frame(
          tech = tech, chain = chain, stage = if (stage == "Assembly") "Assembly / Deployment" else stage,
          tool_key = k, tool = TFE_TOOLS[[k]], tool_group = TFE_TOOL_GROUP[[k]], band = band,
          percentile = as.numeric(r[[paste0(k, "|pct")]]), value = as.numeric(r[[paste0(k, "|value")]]),
          import_coverage = as.numeric(r[[paste0(k, "|coverage")]]), n_measures = n,
          single_measure = !is.na(n) & n == 1 & band %in% c("High", "Medium"),
          imports_2024_usd = 1e9 * as.numeric(r[["US imports 2024 ($bn)"]]),
          tariff_incl_adcvd = as.numeric(r[["tariff_with_adcvd|value"]]), stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, rows)
}

# Counterfactual trusted-partner TRI by cell (chart 7) from statutory lines `st` (tri_by_cell() inputs plus a
# `variant` column and tau columns), for the cells in `keep` (tech|stage keys). Returns one row per cell x
# scenario with TRI and mean for the chosen supplier group.
tfe_counterfactuals <- function(st, cells, scenarios, group = "Trusted") {
  do.call(rbind, lapply(names(scenarios), function(sc) {
    spec <- scenarios[[sc]]
    d <- st[st$variant == spec$variant, , drop = FALSE]
    d$tau_sc <- d[[spec$tau]]
    r <- tri_by_cell(d, cells, "tau_sc", sc)
    r <- r[r$supplier_group == group, c("tech", "stage", "measure", "tri", "mean", "w_total", "s232_tri_sq_share")]
    names(r)[3] <- "scenario"
    r
  }))
}

# Line tariff of a scenario anchored to observed rates (percent): the observed rate (the 2024 rate where the
# line had no trade in the data month) plus the statutory change `delta`, floored at the lower of the observed
# and 2024 rates (switching a policy off cannot take a line below its pre-2025 rate), plus AD/CVD, capped.
tfe_scenario_tau <- function(tau_obs, tau_base, delta, adcvd, cap = 100) {
  obs <- ifelse(is.na(tau_obs), tau_base, tau_obs)
  pmin(pmax(obs + delta, pmin(obs, tau_base)) + adcvd, cap)
}

# Monthly imports by origin for a set of HS10 prefixes (chart 8): `m` has cty_code, hs10, time, value; origins
# named in `origins` (Census code -> label), every other named partner and the residual as "Other".
tfe_monthly_by_origin <- function(m, prefixes, origins, label) {
  d <- m[startsWith(m$hs10, prefixes[1]) | Reduce(`|`, lapply(prefixes, function(p) startsWith(m$hs10, p))), , drop = FALSE]
  d$origin <- ifelse(d$cty_code %in% names(origins), unname(origins[d$cty_code]), "All other")
  a <- stats::aggregate(value ~ time + origin, data = d, FUN = sum)
  a$product <- label
  a[order(a$time, a$origin), c("product", "time", "origin", "value")]
}
