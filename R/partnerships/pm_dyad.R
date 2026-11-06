# Partnership Memos: the dyad table and its relationship states (docs/partnership_memos.md, "Decision rules").
#
# One row per unit (cell or node) with country A's and country B's profile side by side, the unit's world
# supply context, bilateral trade, and the chain context (does the other country hold a strong position
# elsewhere in the same technology or compact?). pm_classify_dyad() then assigns one relationship state by an
# ordered rule list. Two strong positions are DIRECT COMPETITION, never complementarity.
#
# Pure functions.

PM_STATES <- c("a_supplies_b_needs", "b_supplies_a_needs", "complementary_specialization", "shared_vulnerability",
               "capacity_constrained_trusted_supply", "direct_competition", "low_strategic_case", "deployment_demand", "not_assessed")
PM_ACTIVE_STATES <- c("a_supplies_b_needs", "b_supplies_a_needs", "complementary_specialization",
                      "shared_vulnerability", "capacity_constrained_trusted_supply")

# imports: reporter_iso (importer), partner_iso (exporter), tech, supply_chain, imports_usd (scripts/15 table)
pm_bilateral_trade <- function(imports, a, b) {
  tot <- stats::aggregate(imports_usd ~ reporter_iso + tech + supply_chain, data = imports[imports$reporter_iso %in% c(a, b), ], FUN = sum)
  names(tot)[4] <- "total"
  f <- function(imp, exp) {
    x <- imports[imports$reporter_iso == imp & imports$partner_iso == exp, c("tech", "supply_chain", "imports_usd")]
    x <- stats::aggregate(imports_usd ~ tech + supply_chain, data = x, FUN = sum)
    t <- tot[tot$reporter_iso == imp, c("tech", "supply_chain", "total")]
    x <- merge(t, x, by = c("tech", "supply_chain"), all.x = TRUE)
    x$imports_usd[is.na(x$imports_usd)] <- 0
    data.frame(unit_id = paste(x$tech, x$supply_chain, sep = "|"), usd = x$imports_usd, share = ifelse(x$total > 0, x$imports_usd / x$total, NA_real_))
  }
  ab <- f(a, b); ba <- f(b, a)
  names(ab)[2:3] <- c("a_imports_from_b_usd", "a_imports_from_b_share")
  names(ba)[2:3] <- c("b_imports_from_a_usd", "b_imports_from_a_share")
  merge(ab, ba, by = "unit_id", all = TRUE)
}

pm_wide <- function(prof, a, b) {
  keep_world <- c("unit_type", "unit_id", "tech", "stage", "rollup", "node_id", "node_label", "compact", "step", "step_order",
                  "world_top_iso3", "world_top_share", "coercive_top_iso3", "aib_class", "residual_gap", "node_demand_usd", "largest_trusted_supplier")
  per <- setdiff(names(prof), c(keep_world, "iso3"))
  pa <- prof[prof$iso3 == a, , drop = FALSE]; pb <- prof[prof$iso3 == b, , drop = FALSE]
  wa <- pa[, c(keep_world, per)]; names(wa)[match(per, names(wa))] <- paste0(per, "_a")
  wb <- pb[, c("unit_id", per)]; names(wb)[match(per, names(wb))] <- paste0(per, "_b")
  merge(wa, wb, by = "unit_id", all = TRUE)
}

# The dyad table: profiles side by side, bilateral trade (nodes take their primary cell's), chain context.
pm_build_dyad <- function(prof, a, b, trade, cfg) {
  th <- cfg$thresholds
  d <- pm_wide(prof, a, b)
  d$pair <- paste(a, b, sep = "_")
  d$country_a <- a; d$country_b <- b
  # bilateral trade: cells by id; nodes by their primary tech at the node's stage
  key <- ifelse(d$unit_type == "cell", d$unit_id, paste(d$tech, d$stage, sep = "|"))
  m <- match(key, trade$unit_id)
  for (k in c("a_imports_from_b_usd", "a_imports_from_b_share", "b_imports_from_a_usd", "b_imports_from_a_share")) d[[k]] <- trade[[k]][m]
  d$bilateral_trade_basis <- ifelse(d$unit_type == "cell", "cell", "node_primary_cell_proxy")
  # world context
  cws <- pmax(d$coercive_world_share_a, d$coercive_world_share_b, na.rm = TRUE)
  d$coercive_world_share <- cws
  d$chokepoint <- !is.na(cws) & cws >= th$chokepoint_share
  d$severe_chokepoint <- !is.na(cws) & cws >= th$severe_chokepoint_share
  d$both_dependent <- !is.na(d$coercive_import_share_a) & !is.na(d$coercive_import_share_b) &
    d$coercive_import_share_a >= th$both_dependent_coercive_share & d$coercive_import_share_b >= th$both_dependent_coercive_share
  tws <- suppressWarnings(pmin(d$trusted_world_share_a, d$trusted_world_share_b, na.rm = TRUE))
  d$allied_short <- ifelse(d$unit_type == "node", !is.na(d$aib_class) & d$aib_class == "BUILD", !is.na(tws) & tws < th$trusted_capacity_min)
  d$aib_low <- d$unit_type == "node" & !is.na(d$aib_class) & d$aib_class == "LOW INTERVENTION"
  # named supplier: at cell level the engine named the other country as a recommended trusted partner;
  # at node level the country is the largest trusted supplier or a ranked host
  has <- function(lst, iso) !is.na(lst) & vapply(strsplit(ifelse(is.na(lst), "", lst), "\\s+"), function(z) iso %in% z, logical(1))
  d$a_named_by_b <- ifelse(d$unit_type == "cell", has(d$recommended_partners_b, a), (d$largest_trusted_a %in% TRUE) | !is.na(d$host_rank_a))
  d$b_named_by_a <- ifelse(d$unit_type == "cell", has(d$recommended_partners_a, b), (d$largest_trusted_b %in% TRUE) | !is.na(d$host_rank_b))
  # deployment need of the unit's technology in each country (emerging-bottleneck test)
  dep <- prof[prof$unit_type == "cell" & prof$stage == "Deployment", c("iso3", "tech", "strategic_need")]
  dn <- function(iso) dep$strategic_need[match(paste(iso, d$tech), paste(dep$iso3, dep$tech))]
  d$deployment_need_a <- dn(a); d$deployment_need_b <- dn(b)
  # chain context: the chain is the compact (nodes) or the technology's producing stages (cells)
  d$chain_key <- ifelse(d$unit_type == "node", d$compact, ifelse(d$stage == "Deployment", NA_character_, d$tech))
  strong_elsewhere <- function(band_col) {
    vapply(seq_len(nrow(d)), function(i) {
      k <- d$chain_key[i]; if (is.na(k)) return(FALSE)
      o <- d$unit_type == d$unit_type[i] & !is.na(d$chain_key) & d$chain_key == k & d$unit_id != d$unit_id[i]
      any(d[[band_col]][o] == "strong", na.rm = TRUE)
    }, logical(1))
  }
  d$a_strong_elsewhere_in_chain <- strong_elsewhere("position_band_a")
  d$b_strong_elsewhere_in_chain <- strong_elsewhere("position_band_b")
  d$chain_stressed <- vapply(seq_len(nrow(d)), function(i) {
    k <- d$chain_key[i]; if (is.na(k)) return(FALSE)
    o <- d$unit_type == d$unit_type[i] & !is.na(d$chain_key) & d$chain_key == k
    any(d$allied_short[o] | d$chokepoint[o], na.rm = TRUE) || any(d$need_band_a[o] == "high" | d$need_band_b[o] == "high", na.rm = TRUE)
  }, logical(1))
  d
}

# One state per row, ordered rules (first match). Returns the dyad table with state, supplier, buyer, reason.
pm_classify_dyad <- function(d, cfg) {
  rk <- function(x) unname(PM_POSITION_RANK[ifelse(is.na(x), "not_measured", x)])
  pa <- d$position_band_a; pb <- d$position_band_b
  na <- d$need_band_a; nb <- d$need_band_b
  more_cap <- d$chokepoint | d$allied_short
  need_av <- na == "high" | (na == "demand" & more_cap)
  need_bv <- nb == "high" | (nb == "demand" & more_cap)
  pres <- cfg$thresholds$node$presence_share
  share_ok_a <- d$unit_type == "cell" | (!is.na(d$world_share_a) & d$world_share_a >= pres)
  share_ok_b <- d$unit_type == "cell" | (!is.na(d$world_share_b) & d$world_share_b >= pres)
  cap_a <- pa == "strong" | (pa == "moderate" & d$a_named_by_b & share_ok_a)
  cap_b <- pb == "strong" | (pb == "moderate" & d$b_named_by_a & share_ok_b)
  sup_a <- cap_a & rk(pa) > rk(pb)
  sup_b <- cap_b & rk(pb) > rk(pa)
  state <- character(nrow(d)); supplier <- rep(NA_character_, nrow(d)); reason <- character(nrow(d))
  a <- d$country_a; b <- d$country_b
  for (i in seq_len(nrow(d))) {
    unmeasured <- pa[i] == "not_measured" && pb[i] == "not_measured" && na[i] == "not_measured" && nb[i] == "not_measured"
    if (unmeasured || (d$unit_type[i] == "cell" && d$strategic_choice_a[i] %in% "not_assessed" && d$strategic_choice_b[i] %in% "not_assessed")) {
      state[i] <- "not_assessed"; reason[i] <- "neither country's need or position is measured"; next
    }
    if (d$stage[i] == "Deployment") {
      # Deployment is a market, not a supplied product: two large deployers are not competitors. The cell is demand
      # context for the compacts upstream of it (docs/partnership_memos.md).
      if (na[i] == "high" || nb[i] == "high") {
        state[i] <- "deployment_demand"
        reason[i] <- sprintf("deployment need is high in %s: demand context for the supply chain upstream", paste(c(if (na[i] == "high") a[i], if (nb[i] == "high") b[i]), collapse = " and "))
      } else { state[i] <- "low_strategic_case"; reason[i] <- "deployment_cell;no_high_deployment_need" }
      next
    }
    if (pa[i] == "strong" && pb[i] == "strong") {
      state[i] <- "direct_competition"
      reason[i] <- if (more_cap[i]) "both are strong suppliers and more trusted capacity is needed: coordinate specialisation" else "both are strong suppliers and trusted supply is adequate: managed competition"
      next
    }
    if (!sup_a[i] && !sup_b[i] && na[i] == "high" && nb[i] == "high" && (d$chokepoint[i] || d$both_dependent[i])) {
      state[i] <- "shared_vulnerability"
      reason[i] <- if (d$chokepoint[i]) sprintf("both need the node and neither is a strong supplier; %s holds %.0f%% of world supply", d$coercive_top_iso3[i], 100 * d$coercive_world_share[i]) else "both need the node and both import heavily from coercive suppliers"
      next
    }
    ab <- sup_a[i] && need_bv[i]; ba <- sup_b[i] && need_av[i]
    if (ab || ba) {
      s <- if (ab && (!ba || rk(pa[i]) >= rk(pb[i]))) a[i] else b[i]
      supplier[i] <- s
      if (d$allied_short[i]) {
        state[i] <- "capacity_constrained_trusted_supply"
        reason[i] <- sprintf("%s is a capable trusted supplier, the other side needs the node, and allied capacity is short of demand", s)
      } else {
        state[i] <- if (s == a[i]) "a_supplies_b_needs" else "b_supplies_a_needs"
        reason[i] <- sprintf("%s is a capable trusted supplier and the other side needs the node", s)
      }
      next
    }
    ca <- pa[i] == "strong" && d$b_strong_elsewhere_in_chain[i] && d$chain_stressed[i]
    cb <- pb[i] == "strong" && d$a_strong_elsewhere_in_chain[i] && d$chain_stressed[i]
    if (ca || cb) {
      state[i] <- "complementary_specialization"
      supplier[i] <- if (ca) a[i] else b[i]
      reason[i] <- sprintf("%s is strong here and the other country is strong at another step of the same chain", supplier[i])
      next
    }
    state[i] <- "low_strategic_case"
    r <- character()
    if (!need_av[i] && !need_bv[i]) r <- c(r, "no_high_need")
    if (need_av[i] || need_bv[i]) if (!cap_a[i] && !cap_b[i]) r <- c(r, "neither_capable") else r <- c(r, "need_without_partner_capability")
    if (d$aib_low[i] %in% TRUE || (!more_cap[i])) r <- c(r, "adequate_trusted_supply")
    if (isTRUE(d$large_yard_a[i]) || isTRUE(d$large_yard_b[i])) r <- c(r, "large_yard")
    if ((pa[i] == "strong") != (pb[i] == "strong")) r <- c(r, "supplier_without_buyer_need")
    reason[i] <- paste(unique(r), collapse = ";")
  }
  d$relationship_state <- state
  d$supplier <- supplier
  d$buyer <- ifelse(is.na(supplier), NA_character_, ifelse(supplier == a, b, a))
  d$state_reason <- reason
  d
}

pm_state_label <- function(state, a_short, b_short) {
  lab <- c(a_supplies_b_needs = sprintf("%s supplies / %s needs", a_short, b_short),
           b_supplies_a_needs = sprintf("%s supplies / %s needs", b_short, a_short),
           complementary_specialization = "Complementary specialisation",
           shared_vulnerability = "Shared vulnerability",
           capacity_constrained_trusted_supply = "Capacity-constrained trusted supply",
           direct_competition = "Direct competition",
           low_strategic_case = "Low strategic case",
           deployment_demand = "Deployment demand (context)",
           not_assessed = "Not assessed")
  unname(lab[state])
}
