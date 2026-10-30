# Validation diagnostics of the canonical decision framework (spec Part XXI; docs/strategic_position.md,
# "Validation"). Each returns a table; scripts/26 and scripts/27 write them to diagnostics/strategic_position/.
# Pure: no IO. Needs decision_engine.R (SP_CHOICES, sp_consistency_counts) and sp_helpers.R.

# Cells per choice (the six verbs and not_assessed) by `by`, with the posture totals.
sp_diag_choice_distribution <- function(tbl, by) {
  x <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE]
  x$strategic_choice <- factor(x$strategic_choice, SP_CHOICES)
  out <- x %>%
    dplyr::count(dplyr::across(dplyr::all_of(by)), .data$strategic_choice, name = "n", .drop = FALSE) %>%
    tidyr::pivot_wider(names_from = "strategic_choice", values_from = "n", values_fill = 0)
  for (v in SP_CHOICES) if (!v %in% names(out)) out[[v]] <- 0L
  out$n_cells <- rowSums(out[, SP_CHOICES])
  out$compete <- out$preserve + out$catch_up + out$partner
  out$deploy <- out$accelerate + out$watch
  out[, c(by, "n_cells", SP_CHOICES, "compete", "deploy")]
}

# The legacy need x EO quadrant columns put back under their old names, so the legacy quadrant
# diagnostics (sp_diag_framework.R) can describe the rule the framework replaced.
sp_diag_legacy_view <- function(tbl) {
  x <- tbl
  x$strategic_posture <- x$legacy_quadrant_posture
  x$compete_mode <- x$legacy_quadrant_mode
  x$decision_quadrant <- x$legacy_quadrant
  x$quadrant_borderline <- x$legacy_quadrant_borderline
  x$quadrant_consistency_flag <- x$legacy_quadrant_flag
  x$opportunity_percentile <- x$legacy_eo_opportunity_pct
  x$partner_gap <- NA
  x
}

# What explains a disagreement between a legacy reading and the canonical choice, first that applies:
# secure_override (the new choice is Secure), deployment_need (Deployment & Use, decided on its own
# need), not_assessed (need or prize missing now), industrial_opportunity (the cleaned prize is high
# where the legacy opportunity was low, or the reverse), supply_chain_need (need high vs low differs),
# competitive_position (same triage, the response decided by position or the pathway).
sp_diag_attribution <- function(verb, stage, old_prize_high, old_need_high, new_prize_high, new_need_high) {
  dplyr::case_when(
    verb %in% "secure" ~ "secure_override",
    stage == "Deployment" ~ "deployment_need",
    verb %in% "not_assessed" ~ "not_assessed",
    !is.na(old_prize_high) & !is.na(new_prize_high) & old_prize_high != new_prize_high ~ "industrial_opportunity",
    !is.na(old_need_high) & !is.na(new_need_high) & old_need_high != new_need_high ~ "supply_chain_need",
    TRUE ~ "competitive_position"
  )
}

# The legacy need x EO quadrant (the posture until 2026-10-28) against the canonical choice: one row
# per transition with counts, and the attribution of each disagreement.
sp_diag_quadrant_vs_choice <- function(tbl, need_t = 0.67, opp_t = 0.67) {
  x <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE]
  old <- dplyr::case_when(x$legacy_quadrant_posture %in% "secure" ~ "secure", !is.na(x$legacy_quadrant_mode) ~ x$legacy_quadrant_mode,
                          x$supply_chain == "Deployment" ~ "deploy", x$legacy_quadrant %in% "watch" ~ "watch",
                          TRUE ~ "insufficient_data")
  map <- c(secure = "secure", preserve = "preserve", catch_up = "catch_up", partner = "partner", watch = "watch")
  agree <- !is.na(map[old]) & unname(map[old]) == x$strategic_choice
  driver <- ifelse(agree, "agree", sp_diag_attribution(
    x$strategic_choice, x$supply_chain,
    ifelse(is.na(x$legacy_eo_opportunity_pct), NA, x$legacy_eo_opportunity_pct >= opp_t),
    ifelse(is.na(x$strategic_need_percentile), NA, x$strategic_need_percentile >= need_t),
    x$prize_high, x$need_high))
  data.frame(legacy = old, choice = x$strategic_choice, driver = driver, stringsAsFactors = FALSE) %>%
    dplyr::count(.data$legacy, .data$choice, .data$driver, name = "n_cells") %>%
    dplyr::arrange(.data$legacy, dplyr::desc(.data$n_cells))
}

# The three position-choice contradictions (spec XXI.3) before (the legacy quadrant) and after.
sp_diag_consistency_before_after <- function(tbl) {
  x <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE]
  dplyr::bind_rows(
    sp_consistency_counts(x, x$legacy_quadrant_mode, "before: need x legacy EO quadrant (2026-10-28)"),
    sp_consistency_counts(x, x$compete_mode, "after: canonical choice (2026-10-29)")
  )
}

# The cells behind the remaining contradictions and review flags, for inspection.
sp_diag_consistency_cells <- function(tbl) {
  x <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE]
  cpx <- x$competitive_position_percentile
  home <- sp_measured_share(x)
  why <- dplyr::case_when(
    x$consistency_flag %in% SP_CHOICE_FLAGS ~ x$consistency_flag,
    x$compete_mode %in% "partner" & !is.na(cpx) & cpx >= 0.75 ~ "partner_despite_cp_x75",
    x$compete_mode %in% "catch_up" & !is.na(cpx) & cpx >= 0.9 ~ "catch_up_despite_cp_x90",
    x$compete_mode %in% "catch_up" & !is.na(home) & home >= 0.10 ~ "catch_up_despite_leading_share10",
    x$compete_mode %in% "preserve" & ((!is.na(cpx) & cpx < 0.25) | (!is.na(home) & home < 0.005)) ~ "preserve_without_capability",
    TRUE ~ NA_character_
  )
  x$check <- why
  x[!is.na(why), intersect(c("Country", "tech", "supply_chain", "check", "strategic_choice", "supply_chain_need", "industrial_opportunity",
                             "competitive_position_percentile", "cp_contender_pct", "cp_contender_basis", "home_share", "t3_capability_lo",
                             "pathway_credible", "pathway_basis", "legacy_quadrant_mode"), names(x)), drop = FALSE]
}

# Secure selectivity (spec XXI.5): Secure by rationale and stage, and how much of the high-need
# producing cells it takes (Secure must not absorb every high-need cell).
sp_diag_secure_selectivity <- function(tbl, need_t = 0.67) {
  x <- tbl[!is.na(tbl$strategic_choice) & tbl$supply_chain != "Deployment", , drop = FALSE]
  hn <- !is.na(x$supply_chain_need) & x$supply_chain_need >= need_t
  by_rat <- x[x$strategic_choice %in% "secure", , drop = FALSE] %>%
    dplyr::count(.data$security_rationale, .data$supply_chain, name = "n_cells")
  summary <- data.frame(
    measure = c("producing cells", "assessed producing cells (need measured)", "high-need producing cells", "Secure cells",
                "Secure cells with high need", "share of high-need cells that are Secure", "Secure cells with need below the threshold",
                "Secure cells with need not measured"),
    value = c(nrow(x), sum(!is.na(x$supply_chain_need)), sum(hn), sum(x$strategic_choice %in% "secure"),
              sum(x$strategic_choice %in% "secure" & hn), if (sum(hn)) round(sum(x$strategic_choice %in% "secure" & hn) / sum(hn), 3) else NA,
              sum(x$strategic_choice %in% "secure" & !is.na(x$supply_chain_need) & !hn),
              sum(x$strategic_choice %in% "secure" & is.na(x$supply_chain_need)))
  )
  list(summary = summary, by_rationale = by_rat)
}

# Watch nodes with high observed policy intensity (spec XXI.6): where current policy may be
# over-prioritising localisation or protection.
sp_diag_watch_policy <- function(tbl, min_pct = 0.67) {
  x <- tbl[tbl$strategic_choice %in% "watch", , drop = FALSE]
  col <- function(n) if (n %in% names(x)) x[[n]] else rep(NA_real_, nrow(x))
  hot <- (!is.na(col("policy_priority_percentile")) & col("policy_priority_percentile") >= min_pct) |
    (!is.na(col("policy_protection_priority")) & col("policy_protection_priority") >= min_pct) |
    (!is.na(col("deployment_localisation_priority")) & col("deployment_localisation_priority") >= min_pct)
  x <- x[hot, , drop = FALSE]
  x <- x[order(-dplyr::coalesce(x$policy_protection_priority, x$deployment_localisation_priority, -1), -dplyr::coalesce(x$policy_priority_percentile, -1)), , drop = FALSE]
  x[, intersect(c("Country", "tech", "supply_chain", "policy_priority_percentile", "policy_support_priority", "policy_protection_priority",
                  "deployment_localisation_priority", "policy_alignment", "policy_alignment_flag", "policy_alignment_confidence",
                  "watch_reasons", "supply_chain_need", "industrial_opportunity", "deployment_need", "cp_contender_pct"), names(x)), drop = FALSE]
}

# Deployment review (spec XXI.4): per country and deployment technology, the deployment need and its
# families with the manufacturing choices of the same technology, so a low manufacturing priority with
# a high deployment priority is visible.
sp_diag_deployment_review <- function(tbl, countries, techs) {
  d <- tbl[tbl$Country %in% countries & tbl$tech %in% techs & tbl$supply_chain == "Deployment", , drop = FALSE]
  mf <- tbl[tbl$Country %in% countries & tbl$tech %in% techs & tbl$supply_chain %in% c("Components", "Assembly"), , drop = FALSE]
  mf_w <- mf %>% dplyr::select("Country", "tech", "supply_chain", "strategic_choice") %>%
    tidyr::pivot_wider(names_from = "supply_chain", values_from = "strategic_choice", names_prefix = "choice_")
  keep <- intersect(c("Country", "tech", "deployment_need", grep("^dn_", names(d), value = TRUE), "strategic_choice", "deploy_mode",
                      "posture_confidence", "decision_borderline", "policy_priority_percentile", "policy_alignment",
                      "policy_alignment_confidence"), names(d))
  out <- d[, keep, drop = FALSE] %>% dplyr::left_join(mf_w, by = c("Country", "tech"))
  out$manufacturing_vs_deployment <- dplyr::case_when(
    out$strategic_choice %in% "accelerate" & (out$choice_Assembly %in% c("watch", "partner") | out$choice_Components %in% c("watch", "partner")) ~
      "deploy faster, manufacture elsewhere",
    out$strategic_choice %in% "accelerate" ~ "deploy faster and compete at home",
    out$strategic_choice %in% "watch" & (out$choice_Assembly %in% c("preserve", "catch_up") | out$choice_Components %in% c("preserve", "catch_up")) ~
      "manufacture, deployment on track",
    TRUE ~ NA_character_
  )
  out[order(out$Country, match(out$tech, techs)), , drop = FALSE]
}

# Technologies whose deployment priority and domestic manufacturing priority differ, all countries.
sp_diag_manufacturing_vs_deployment <- function(tbl) {
  d <- tbl[tbl$supply_chain == "Deployment" & !is.na(tbl$strategic_choice), c("Country", "iso3", "tech", "strategic_choice"), drop = FALSE]
  names(d)[4] <- "deployment_choice"
  m <- tbl[tbl$supply_chain %in% c("Components", "Assembly") & !is.na(tbl$strategic_choice), , drop = FALSE] %>%
    dplyr::group_by(.data$iso3, .data$tech) %>%
    dplyr::summarise(manufacturing_choice = {
      v <- .data$strategic_choice
      if (any(v %in% c("preserve", "catch_up"))) "compete_at_home" else if (any(v %in% "secure")) "secure" else if (any(v %in% "partner")) "partner"
      else if (all(v %in% "not_assessed")) "not_assessed" else "watch"
    }, .groups = "drop")
  dplyr::left_join(d, m, by = c("iso3", "tech")) %>%
    dplyr::count(.data$tech, .data$deployment_choice, .data$manufacturing_choice, name = "n_countries")
}

# Pathway evidence by stage, among the cells the pathway can decide: yes / no / unknown, how many have a
# capability anchor, each anchor type and supporting signal that fires (pathway.R catch_up_anchor_type,
# pathway_support; whatever the rule), and the cells decided Catch Up.
sp_diag_pathway_summary <- function(tbl) {
  x <- tbl[tbl$supply_chain != "Deployment" & !is.na(tbl$pathway_credible), , drop = FALSE]
  has <- function(v, l) grepl(paste0("(^|;)", l, "($|;)"), dplyr::coalesce(v, ""))
  d <- dplyr::bind_rows(lapply(split(x, x$supply_chain), function(s) {
    data.frame(supply_chain = s$supply_chain[1], n_cells = nrow(s), yes = sum(s$pathway_credible == "yes"), no = sum(s$pathway_credible == "no"),
               unknown = sum(s$pathway_credible == "unknown"), anchor = sum(s$catch_up_anchor %in% TRUE),
               t(vapply(SP_PATHWAY_ANCHORS, function(l) sum(has(s$catch_up_anchor_type, l)), numeric(1))),
               t(vapply(SP_PATHWAY_SUPPORT, function(l) sum(has(s$pathway_support, l)), numeric(1))),
               decided_catch_up = sum(s$strategic_choice %in% "catch_up"), check.names = FALSE)
  }))
  d[order(match(d$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
}

# The choice under alternative pathway rules, from the stored need, prize and position (the engine's Secure
# kept): pathways is a named list of pathway_credible vectors (e.g. legs_1, legs_2, anchor). One row per
# rule with the count of each choice and how many cells differ from the first rule.
sp_diag_pathway_rules <- function(tbl, pathways, cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  dep <- tbl$supply_chain == "Deployment"
  hc <- sp_choice_high_cp(tbl$cp_contender_pct, tbl$cp_contender_basis, tbl$cp_is_contender, tbl$t3_capability_lo, th$cp, th$floor)
  verbs <- lapply(pathways, function(pw) {
    v <- sp_choice_rule(tbl$supply_chain, tbl$engine_posture %in% "secure", ifelse(dep, NA_real_, tbl$supply_chain_need),
                        ifelse(dep, NA_real_, tbl$industrial_opportunity), hc, pw, ifelse(dep, tbl$deployment_need, NA_real_), th)
    v[is.na(tbl$strategic_choice)] <- NA_character_
    v
  })
  dplyr::bind_rows(lapply(names(verbs), function(r) {
    v <- verbs[[r]]
    counts <- vapply(SP_CHOICES, function(ch) sum(v %in% ch), numeric(1))
    data.frame(rule = r, t(counts), changed_vs_first = sum(!mapply(identical, v, verbs[[1]])), check.names = FALSE)
  }))
}

# Catch Up at near-zero or unmeasured capability (user spec 2026-10-30): per rule, the Catch Up cells with
# Competitive Position == 0, < 0.10, < 0.25 (cumulative) and NA, on the decision's measure (the percentile
# among the node's contenders) and on the cross-country percentile; and those without a capability anchor.
sp_diag_catch_up_capability <- function(tbl, pathways, cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  dep <- tbl$supply_chain == "Deployment"
  hc <- sp_choice_high_cp(tbl$cp_contender_pct, tbl$cp_contender_basis, tbl$cp_is_contender, tbl$t3_capability_lo, th$cp, th$floor)
  bands <- function(v, cu) c(eq_0 = sum(cu & v %in% 0), lt_0.10 = sum(cu & !is.na(v) & v < 0.10), lt_0.25 = sum(cu & !is.na(v) & v < 0.25),
                             na = sum(cu & is.na(v)))
  dplyr::bind_rows(lapply(names(pathways), function(r) {
    v <- sp_choice_rule(tbl$supply_chain, tbl$engine_posture %in% "secure", ifelse(dep, NA_real_, tbl$supply_chain_need),
                        ifelse(dep, NA_real_, tbl$industrial_opportunity), hc, pathways[[r]], ifelse(dep, tbl$deployment_need, NA_real_), th)
    cu <- v %in% "catch_up" & !is.na(tbl$strategic_choice)
    rbind(
      data.frame(rule = r, measure = "cp_contender_pct", catch_up = sum(cu), t(bands(tbl$cp_contender_pct, cu)),
                 without_anchor = sum(cu & !(tbl$catch_up_anchor %in% TRUE)), check.names = FALSE),
      data.frame(rule = r, measure = "competitive_position_percentile", catch_up = sum(cu), t(bands(tbl$competitive_position_percentile, cu)),
                 without_anchor = sum(cu & !(tbl$catch_up_anchor %in% TRUE)), check.names = FALSE)
    )
  }))
}

# The capability anchors of the producing-stage cells by choice: how often each anchor type fires among the
# cells of each choice, the cells with no anchor, and the greenfield options (decision_engine.R).
sp_diag_anchor_types <- function(tbl) {
  x <- tbl[tbl$supply_chain != "Deployment" & !is.na(tbl$strategic_choice), , drop = FALSE]
  has <- function(v, l) grepl(paste0("(^|;)", l, "($|;)"), dplyr::coalesce(v, ""))
  dplyr::bind_rows(lapply(split(x, x$strategic_choice), function(s) {
    data.frame(strategic_choice = s$strategic_choice[1], n_cells = nrow(s), anchor = sum(s$catch_up_anchor %in% TRUE),
               no_anchor = sum(s$catch_up_anchor %in% FALSE), anchor_unobserved = sum(is.na(s$catch_up_anchor)),
               t(vapply(SP_PATHWAY_ANCHORS, function(l) sum(has(s$catch_up_anchor_type, l)), numeric(1))),
               greenfield_option = sum(s$greenfield_option %in% TRUE), check.names = FALSE)
  }))
}

# Up to n examples per country and choice (the clearest: farthest from their thresholds).
sp_diag_choice_examples <- function(tbl, countries, n = 3) {
  x <- tbl[tbl$Country %in% countries & !is.na(tbl$strategic_choice) & tbl$strategic_choice != "not_assessed", , drop = FALSE]
  x$clarity <- abs(dplyr::coalesce(x$supply_chain_need, x$deployment_need, 0.5) - 0.67) + abs(dplyr::coalesce(x$industrial_opportunity, 0.6) - 0.6) +
    abs(dplyr::coalesce(x$cp_contender_pct, 0.75) - 0.75)
  x %>% dplyr::group_by(.data$Country, .data$strategic_choice) %>%
    dplyr::arrange(dplyr::desc(.data$clarity), .by_group = TRUE) %>% dplyr::slice_head(n = n) %>% dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c("Country", "strategic_choice", "tech", "supply_chain", "security_rationale", "supply_chain_need", "deployment_need",
                                  "industrial_opportunity", "cp_contender_pct", "pathway_credible", "posture_confidence", "consistency_flag",
                                  "delivery_mode", "recommended_partner_1", "partner_aib_status", "policy_alignment", "choice_reason")))
}

# ---- Legacy OSI corners vs the canonical choice (spec XXI.1; docs "Legacy OSI framework") -------------
# The legacy OSI corner of each country x six-stage sector (Energy Security x Economic Opportunity, the
# Country fact sheets): legacy_grow (high EO, high ES), legacy_upgrade (high EO, low ES), legacy_exposed
# (low EO, low ES), legacy_stable (low EO, high ES). The pre-2026-10-29 keys grow / upgrade / secure /
# watch are accepted.
SP_LEGACY_CORNER_MAP <- c(legacy_grow = "preserve", legacy_upgrade = "catch_up", legacy_exposed = "partner", legacy_stable = "watch")

sp_legacy_corner_std <- function(x) {
  x <- as.character(x)
  old <- c(grow = "legacy_grow", upgrade = "legacy_upgrade", secure = "legacy_exposed", watch = "legacy_stable",
           exposed = "legacy_exposed", stable = "legacy_stable")
  ifelse(x %in% names(old), unname(old[x]), x)
}

# corners: iso3, tech, supply_chain, legacy_corner. Returns list(pairs: the four conceptual pairs with
# their agreement rates; transitions: corner x choice x driver counts). A low Energy Security reads as a
# high need and a high Economic Opportunity as a high prize when a disagreement is attributed.
sp_diag_corner_vs_choice <- function(tbl, corners) {
  k <- corners
  k$legacy_corner <- sp_legacy_corner_std(k$legacy_corner)
  k <- k[k$legacy_corner %in% names(SP_LEGACY_CORNER_MAP), c("iso3", "tech", "supply_chain", "legacy_corner"), drop = FALSE]
  x <- dplyr::inner_join(tbl[!is.na(tbl$strategic_choice), , drop = FALSE], k, by = c("iso3", "tech", "supply_chain"))
  mapped <- unname(SP_LEGACY_CORNER_MAP[x$legacy_corner])
  agree <- x$strategic_choice == mapped
  old_need <- x$legacy_corner %in% c("legacy_upgrade", "legacy_exposed")
  old_prize <- x$legacy_corner %in% c("legacy_grow", "legacy_upgrade")
  driver <- ifelse(agree, "agree", sp_diag_attribution(x$strategic_choice, x$supply_chain, old_prize, old_need, x$prize_high, x$need_high))
  pairs <- data.frame(legacy_corner = names(SP_LEGACY_CORNER_MAP), maps_to = unname(SP_LEGACY_CORNER_MAP), stringsAsFactors = FALSE)
  pairs$n_cells <- vapply(pairs$legacy_corner, function(cn) sum(x$legacy_corner == cn), numeric(1))
  pairs$n_agree <- vapply(pairs$legacy_corner, function(cn) sum(x$legacy_corner == cn & agree), numeric(1))
  pairs$agreement <- round(ifelse(pairs$n_cells > 0, pairs$n_agree / pairs$n_cells, NA_real_), 3)
  for (v in SP_CHOICES) pairs[[paste0("to_", v)]] <- vapply(pairs$legacy_corner, function(cn) sum(x$legacy_corner == cn & x$strategic_choice == v), numeric(1))
  transitions <- data.frame(legacy_corner = x$legacy_corner, choice = x$strategic_choice, driver = driver, stringsAsFactors = FALSE) %>%
    dplyr::count(.data$legacy_corner, .data$choice, .data$driver, name = "n_cells") %>%
    dplyr::arrange(.data$legacy_corner, dplyr::desc(.data$n_cells))
  drivers <- data.frame(driver = driver[!agree], stringsAsFactors = FALSE) %>% dplyr::count(.data$driver, name = "n_cells") %>%
    dplyr::arrange(dplyr::desc(.data$n_cells))
  list(pairs = pairs, transitions = transitions, drivers = drivers, n_cells = nrow(x))
}

# ---- Markdown review (scripts/27) ---------------------------------------------------------------------
sp_md_table <- function(df, digits = 2) {
  if (is.null(df) || !nrow(df)) return("(none)")
  f <- function(v) if (is.numeric(v)) ifelse(is.na(v), "", format(round(v, digits), trim = TRUE, scientific = FALSE)) else ifelse(is.na(v), "", as.character(v))
  body <- vapply(seq_len(nrow(df)), function(i) paste0("| ", paste(vapply(df, function(col) f(col[i]), ""), collapse = " | "), " |"), "")
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"), body)
}

# Choice cards (design section 2): per country, the six-verb counts and the clearest examples of each
# verb with the reason the engine gives. phrases: named list "Tech|Stage" -> noun phrase.
sp_choice_cards_md <- function(tbl, countries, phrases = list(), n = 3) {
  out <- c("# Choice cards (canonical decision framework)", "",
           "Generated by scripts/27_review_strategic_position.R from strategic_position_tbl.rds (scripts/26). For each country:",
           "the six choices with counts, and up to three clear examples of each with the reason the engine gives.", "")
  verbs <- c(secure = "Secure", preserve = "Compete . Preserve", catch_up = "Compete . Catch Up", partner = "Compete . Partner",
             accelerate = "Deploy . Accelerate", watch = "Deploy . Watch")
  for (cn in countries) {
    x <- tbl[tbl$Country %in% cn & !is.na(tbl$strategic_choice), , drop = FALSE]
    if (!nrow(x)) next
    cnt <- table(factor(x$strategic_choice, SP_CHOICES))
    out <- c(out, paste0("## ", cn), "",
             paste0(cnt[["secure"]], " Secure . ", cnt[["preserve"]], " Preserve . ", cnt[["catch_up"]], " Catch Up . ", cnt[["partner"]],
                    " Partner . ", cnt[["accelerate"]], " Accelerate . ", cnt[["watch"]], " Watch (", cnt[["not_assessed"]], " not assessed)"), "")
    ex <- sp_diag_choice_examples(tbl, cn, n = n)
    for (v in names(verbs)) {
      e <- ex[ex$strategic_choice == v, , drop = FALSE]
      if (!nrow(e)) next
      out <- c(out, paste0("**", verbs[[v]], "**"), "")
      for (i in seq_len(nrow(e))) {
        key <- paste(e$tech[i], e$supply_chain[i], sep = "|")
        ph <- phrases[[key]] %||% paste(e$tech[i], e$supply_chain[i])
        extra <- c(if (!is.na(e$security_rationale[i])) paste0("rationale ", gsub("_", " ", e$security_rationale[i])),
                   if (!is.na(e$delivery_mode[i])) paste0("delivery ", gsub("_", " ", e$delivery_mode[i])),
                   if ("recommended_partner_1" %in% names(e) && !is.na(e$recommended_partner_1[i])) paste0("partner ", e$recommended_partner_1[i]),
                   if ("partner_aib_status" %in% names(e) && !is.na(e$partner_aib_status[i])) paste0("AIB ", e$partner_aib_status[i]),
                   if (!is.na(e$policy_alignment[i])) paste0("policy ", gsub("_", " ", e$policy_alignment[i])),
                   if (!is.na(e$posture_confidence[i])) paste0("confidence ", e$posture_confidence[i]))
        out <- c(out, paste0("- ", ph, " (", e$tech[i], ", ", e$supply_chain[i], "): ", dplyr::coalesce(e$choice_reason[i], ""),
                             if (length(extra)) paste0(" [", paste(extra, collapse = "; "), "]") else ""))
      }
      out <- c(out, "")
    }
  }
  out
}
