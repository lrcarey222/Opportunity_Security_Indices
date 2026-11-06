# Partnership Memos: the memo text (docs/partnership_memos.md, "Memo"). Builds partnership_memo.md from the
# tables. Every bullet carries a tag: [Data] (OSI tables), [External Exx] (curated evidence layer, with its id
# and verification), [Inference] (a rule applied to data) or [Recommendation]. Pure functions.

pm_md_table <- function(x, cols = names(x), header = cols) {
  if (!nrow(x)) return("_None._")
  esc <- function(v) { v <- ifelse(is.na(v), "", as.character(v)); gsub("\\|", "/", gsub("\n", " ", v)) }
  c(paste0("| ", paste(header, collapse = " | "), " |"), paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|"),
    vapply(seq_len(nrow(x)), function(i) paste0("| ", paste(vapply(cols, function(k) esc(x[[k]][i]), character(1)), collapse = " | "), " |"), character(1)))
}
pm_f2 <- function(x) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = 2))
pm_usd <- function(x) ifelse(is.na(x), "n/a", ifelse(abs(x) >= 1e9, sprintf("US$%.1fbn", x / 1e9), sprintf("US$%.0fm", x / 1e6)))
pm_ev_tag <- function(e) sprintf("[External %s, %s]", e$evidence_id, e$verification)
# The memo states the fact; the researcher's process notes ("I could not confirm ...") stay in the evidence
# register and the verification status goes with the source.
pm_claim_clean <- function(x) vapply(x, function(s) {
  cl <- trimws(strsplit(sub("\\.\\s*$", "", s), ";\\s+")[[1]])
  drop <- grepl("(^|\\s)I\\s|\\bmy (sources|searches)\\b|not opened|search summar|not confirmed|unverified|not checked|unchecked|not captured|not verified|was not (read|fetched)|did not (verify|resolve|open|find)",
                cl, ignore.case = FALSE)
  keep <- if (all(drop)) cl[1] else cl[!drop]
  paste0(paste(keep, collapse = "; "), ".")
}, character(1), USE.NAMES = FALSE)
pm_ev_line <- function(ev, ids, include_unverified = FALSE) {
  e <- ev[ev$evidence_id %in% ids, , drop = FALSE]
  if (!include_unverified) e <- e[e$verification != "unverified", , drop = FALSE]
  if (!nrow(e)) return(character())
  e <- e[order(e$publication_date, e$evidence_id), ]
  sprintf("- %s %s", pm_ev_tag(e), pm_claim_clean(e$claim))
}
pm_need_txt <- function(band, need) ifelse(is.na(band), "n/a", ifelse(band == "low" & !is.na(need) & need >= 0.67,
  sprintf("low (cell %s; not a material buyer)", pm_f2(need)), sprintf("%s %s", band, pm_f2(need))))
pm_pos_txt <- function(band, share, unit_type) ifelse(unit_type == "node",
  sprintf("%s (%s of world supply)", band, pm_pct(share, 1)), sprintf("%s", band))

pm_memo_sections <- function(d, prof, cc, arch, trade_tbl, inv_tbl, third, deals, mea, nonp, ev, mech, sroles,
                             agreements, compacts, pair_cfg, cfg) {
  a <- pair_cfg$a; b <- pair_cfg$b; A <- pair_cfg$a_short; B <- pair_cfg$b_short
  Ar <- if (is.null(pair_cfg$a_ref)) A else pair_cfg$a_ref; Br <- if (is.null(pair_cfg$b_ref)) B else pair_cfg$b_ref
  ucf <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  nm <- function(iso) ifelse(is.na(iso), "both", ifelse(iso == a, A, ifelse(iso == b, B, iso)))
  ev_vintage <- max(ev$access_date)
  # joint bets, grouped: one bundle per compact (or technology), best unit first
  # joint bets: tier-1 units inside the electro-industrial compacts, bundled one per compact, nodes before cells
  jb_all <- d[d$priority_tier == 1 & !is.na(d$primary_compact), , drop = FALSE]
  jb_all$group <- jb_all$primary_compact
  jb_all <- jb_all[order(jb_all$unit_type != "node", jb_all$priority_rank), ]
  grp_best <- jb_all[!duplicated(jb_all$group), ]
  gu <- tapply(jb_all$urgency, jb_all$group, max, na.rm = TRUE)
  grp_best <- grp_best[order(-(gu[grp_best$group] >= 0.4), -grp_best$strategic_importance, grp_best$priority_rank), ]
  jb <- utils::head(grp_best, cfg$priority$max_joint_bets)
  short_state <- function(r) ifelse(r$relationship_state == "shared_vulnerability", "joint",
                                    ifelse(is.na(r$supplier), "", sprintf("%s->%s", nm(r$supplier), nm(r$buyer))))
  jb$members <- vapply(jb$group, function(g) {
    m <- jb_all[jb_all$group == g & jb_all$unit_type == "node", ]
    if (!nrow(m)) m <- jb_all[jb_all$group == g, ][1, ]
    paste(sprintf("%s (%s)", m$node_label, vapply(seq_len(nrow(m)), function(i) short_state(m[i, ]), character(1))), collapse = "; ")
  }, character(1))
  jb_off <- d[d$priority_tier == 1 & is.na(d$primary_compact), , drop = FALSE]
  cname <- setNames(cc$strategic_compact_name, cc$strategic_compact_id)
  grp_name <- function(g) ifelse(g %in% names(cname), cname[g], g)
  contrib <- function(r, iso) {
    if (r$relationship_state == "shared_vulnerability") return("its demand for pooled offtake, and finance for non-Chinese supply")
    if (!is.na(r$supplier) && r$supplier == iso) {
      sh <- if (iso == a) r$world_share_a else r$world_share_b
      return(if (r$unit_type == "node") sprintf("trusted capacity (%s of world supply)", pm_pct(sh, 0)) else "trusted capacity (top-quarter position)")
    }
    if (!is.na(r$buyer) && r$buyer == iso) return("demand and long-term offtake")
    "-"
  }
  L <- character()
  add <- function(...) L <<- c(L, ...)

  # ---- Title ---------------------------------------------------------------------------------------------
  add(sprintf("# %s", pair_cfg$title), "",
      sprintf("_Partnership Memo, generated %s by scripts/34_build_partnership_memo.R. OSI data: six-stage strategic-position table (trade 2022-24 benchmark, flows 2024, NIPO policy as of 2025) and the Allied Industrial Base strategic layer (%s scenario). External evidence: %d curated items, accessed %s (verified %d, partial %d, unverified %d)._",
              format(Sys.Date()), cfg$inputs$aib_scenario, nrow(ev), ev_vintage, sum(ev$verification == "verified"), sum(ev$verification == "partial"), sum(ev$verification == "unverified")),
      "", "_Tags: **[Data]** OSI tables; **[External Exx, verification]** curated evidence layer; **[Inference]** a documented rule applied to the data; **[Recommendation]** what the two governments should do. Country A is the United States in every table (`_a`), country B is Japan (`_b`)._", "")

  # ---- Executive summary ---------------------------------------------------------------------------------
  st <- table(factor(d$relationship_state, levels = PM_STATES))
  roles <- function(iso) { r <- sroles[sroles$iso3 == iso & sroles$primary_or_secondary %in% c("primary", "secondary"), ]; r <- r[order(-as.numeric(r$role_strength)), ]; paste(sprintf("%s (%s)", r$strategic_role, r$primary_or_secondary), collapse = ", ") }
  sup_a <- d[d$relationship_state %in% PM_ACTIVE_STATES & d$supplier %in% a & d$priority_tier %in% 1:2 & d$unit_type == "node", ]
  sup_b <- d[d$relationship_state %in% PM_ACTIVE_STATES & d$supplier %in% b & d$priority_tier %in% 1:2 & d$unit_type == "node", ]
  shared <- d[d$relationship_state == "shared_vulnerability" & d$unit_type == "node", ]
  add("## Executive Decision Summary", "",
      "**Why this partnership matters.**",
      sprintf("- [Data] Of %d technology x stage cells and electro-industrial nodes assessed for the pair, %d show an active complementarity (a supplier meeting the other's need, a shared chokepoint, a capacity-constrained trusted supplier or a chain specialisation), %d direct competition and %d no strategic case. The relationship is not one of general similarity: most of the electro-industrial base needs no bilateral intervention.",
              nrow(d), sum(d$relationship_state %in% PM_ACTIVE_STATES), st[["direct_competition"]], st[["low_strategic_case"]]),
      sprintf("- [Data] AIB strategic roles: %s %s; %s %s.", A, roles(a), B, roles(b)),
      "", "**The central bilateral bargain.**",
      sprintf("- [Inference] %s supplies specialised processed materials and components where %s is short (%s); %s supplies %s where %s is short; and the two share exposure to the same coercive supplier in %s, where neither can supply the other and the answer is joint demand, finance and third-country capacity.",
              Br, Ar, paste(utils::head(tolower(sup_b$node_label[order(sup_b$priority_rank)]), 4), collapse = ", "), Ar,
              if (nrow(sup_a)) paste(utils::head(tolower(sup_a$node_label[order(sup_a$priority_rank)]), 3), collapse = ", ") else "energy and some materials", Br,
              if (nrow(shared)) paste(tolower(shared$node_label), collapse = " and ") else "critical-mineral refining"),
      sprintf("- [Inference] %s brings demand, offtake and capital to %s's capability; %s brings specialised capacity and patient finance (JBIC, NEXI, JOGMEC) to %s's market. That is the trade: market access and demand certainty for trusted capacity.", ucf(Ar), Br, Br, Ar),
      "", sprintf("**The %d highest-priority joint bets** (tier 1 by the rules in Section 3, one per compact; ranked by urgency then importance):", nrow(jb)), "")
  bundle_contrib <- function(g, iso) {
    m <- jb_all[jb_all$group == g, ]; m <- m[order(m$unit_type != "node"), ]
    v <- unique(vapply(seq_len(nrow(m)), function(i) { cx <- contrib(m[i, ], iso); if (cx == "-") "" else sprintf("%s: %s", m$node_label[i], cx) }, character(1)))
    v <- v[nzchar(v)]; if (!length(v)) "-" else paste(utils::head(v, 2), collapse = "; ")
  }
  jt <- data.frame(Priority = seq_len(nrow(jb)), Sector = vapply(jb$group, grp_name, character(1)),
                   Node = jb$members, Problem = gsub("_", " ", jb$constraint_type),
                   A = vapply(jb$group, bundle_contrib, character(1), iso = a),
                   B = vapply(jb$group, bundle_contrib, character(1), iso = b),
                   Instrument = gsub("_", " ", jb$main_instrument), Timing = pm_horizon(jb$execution_speed), stringsAsFactors = FALSE)
  add(pm_md_table(jt, names(jt), c("Priority", "Sector", "Nodes in the bet (direction)", "Strategic problem", paste(A, "contribution"), paste(B, "contribution"), "Main instrument (lead node)", "Timing")), "")
  if (nrow(jb_off)) add(sprintf("- [Inference] Outside the electro-industrial compacts, the rules also find fuel-trade lanes (%s); they are handled in the trade agenda, not as compacts.", paste(tolower(jb_off$node_label), collapse = "; ")), "")
  add("**Do not prioritise** (Section 10):", sprintf("- [Recommendation] %s", utils::head(nonp$text, 3)), "")
  add("**Existing mechanisms and the MEA question** (Section 8):",
      sprintf("- [Inference] MEA assessment: **%s**. %s", switch(mea$option, existing = "use the existing mechanisms", upgrade = sprintf("upgrade an existing framework (%s)", mea$host_mechanism), new_mea = "a new MEA-style framework is warranted"),
              if (length(mea$gaps)) paste0("Gaps: ", paste(mea$gaps, collapse = "; "), ".") else ""),
      sprintf("- [Recommendation] %s", switch(mea$option,
        existing = "Run the compacts through the existing mechanisms; no new umbrella.",
        upgrade = sprintf("Do not sign a new energy MOU. Give %s a sector strategy, reciprocal commitments and a KPI dashboard covering the recommended compacts, reviewed every %d months.", mea$host_mechanism, cfg$mea$review_cycle_months),
        new_mea = "Package the compacts into one multi-year framework with a shared pipeline, KPIs and review.")), "")

  # ---- 1. Current state ----------------------------------------------------------------------------------
  pair_ev <- ev[vapply(strsplit(ev$countries, "\\|"), function(z) all(c(a, b) %in% z), logical(1)), ]
  ids_of <- function(types, techs = NULL) { x <- pair_ev[pair_ev$item_type %in% types, ]; if (!is.null(techs)) x <- x[x$technology %in% techs | x$compact %in% techs, ]; x$evidence_id }
  ag <- agreements[agreements$iso3 %in% c(a, b), ]
  plur <- sort(unique(ag$agreement[ag$iso3 == b]))
  add("## 1. Current State of the Relationship", "",
      paste0("This section sets out what already exists before anything new is proposed. Facts are from the curated evidence layer (accessed ", ev_vintage, "); items marked partial or unverified rest on secondary sources and should be confirmed before citation."), "",
      "**Alliance and plurilateral architecture**",
      sprintf("- [Data] %s's memberships in OSI's economic-security agreements table: %s.", B, paste(plur, collapse = ", ")),
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type %in% c("plurilateral", "dialogue")]), "",
      "**Trade architecture and frictions**",
      sprintf("- [Data] In the OSI energy basket, %s imported %s from %s and %s imported %s from %s (bilateral flows 2024, summed over the technology x stage cells).",
              A, pm_usd(sum(d$a_imports_from_b_usd[d$unit_type == "cell"], na.rm = TRUE)), B, B, pm_usd(sum(d$b_imports_from_a_usd[d$unit_type == "cell"], na.rm = TRUE)), A),
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type %in% c("agreement") & pair_ev$instrument %in% c("trade_agreement", "tariff")]),
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type == "dispute"]), "",
      "**Industrial and energy agreements**",
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type %in% c("framework", "institution") | (pair_ev$item_type == "agreement" & !pair_ev$instrument %in% c("trade_agreement", "tariff"))]),
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$evidence_id == "E03"]), "",
      "**Investment vehicle, project pipeline and financing**",
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type %in% c("project", "financing") & pair_ev$compact %in% c("cross_sector", "SC-NUCLEAR")]), "",
      "**Corporate relationships in the recommended compacts**",
      pm_ev_line(ev, pair_ev$evidence_id[pair_ev$item_type %in% c("corporate", "project", "financing") & !pair_ev$compact %in% c("cross_sector", "SC-NUCLEAR") &
                                           pair_ev$compact %in% cc$strategic_compact_id[cc$recommended]]), "",
      "- [Data] Unverified items are left out of this section; they stay in the evidence register (`evidence_register.csv`) and the evidence layer.", "",
      sprintf("- [Inference] The relationship already has two working channels: an investment vehicle with a project pipeline and JBIC/NEXI finance (so far gas power, crude export, synthetic diamonds and SMRs), and a critical-minerals framework with financing, pricing and stockpile commitments but no published project list. What it lacks is a link between the two and any sector strategy for the electro-industrial chains where the data show complementarity."), "")

  # ---- 2. Profiles ---------------------------------------------------------------------------------------
  cells <- d[d$unit_type == "cell", ]
  code <- function(p, n) paste0(substr(ifelse(is.na(p), "-", c(strong = "S", moderate = "M", weak = "W", not_measured = "-")[p]), 1, 1),
                                "/", ifelse(is.na(n), "-", c(high = "H", low = "L", not_measured = "-", demand = "D")[n]))
  stages <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
  mat <- do.call(rbind, lapply(sort(unique(cells$tech)), function(t) {
    x <- cells[cells$tech == t, ]
    v <- vapply(stages, function(s) { r <- x[x$stage == s, ]; if (!nrow(r)) "" else sprintf("%s · %s", code(r$position_band_a, r$need_band_a), code(r$position_band_b, r$need_band_b)) }, character(1))
    data.frame(Technology = t, t(v), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  add("## 2. What Each Country Is Good At — and What It Needs", "",
      sprintf("Cell codes: position / need for **%s · %s**. Position S strong (top quarter of contenders and above the capability floor), M contender, W not a contender; need H high (supply-chain need >= %.2f, or deployment need >= %.2f at Deployment), L low; - not measured. Source: strategic_position_tbl (engine choices are in country_profiles.csv).", A, B, cfg$thresholds$need_high, cfg$thresholds$deployment_need_high), "",
      pm_md_table(mat, names(mat)), "")
  nd <- d[d$unit_type == "node" & (d$position_band_a %in% c("strong", "moderate") | d$position_band_b %in% c("strong", "moderate") | d$need_band_a %in% c("high", "demand") | d$need_band_b %in% c("high", "demand")), ]
  nd <- nd[order(nd$compact, nd$step_order), ]
  nt <- data.frame(Compact = nd$compact, Node = nd$node_label, Stage = nd$stage,
                   A_share = pm_pct(nd$world_share_a, 1), A_rca = pm_f2(nd$rca_a), A_need = pm_need_txt(nd$need_band_a, nd$strategic_need_a), A_demand = pm_pct(nd$demand_share_a, 0),
                   B_share = pm_pct(nd$world_share_b, 1), B_rca = pm_f2(nd$rca_b), B_need = pm_need_txt(nd$need_band_b, nd$strategic_need_b), B_demand = pm_pct(nd$demand_share_b, 0),
                   AIB = nd$aib_class, Coercive = sprintf("%s %s", ifelse(is.na(nd$coercive_top_iso3), "", nd$coercive_top_iso3), pm_pct(nd$coercive_world_share, 0)), stringsAsFactors = FALSE)
  add(sprintf("**Electro-industrial nodes where either country has a position or a need** (AIB division of labour, 2024 trade; share = world export or production share; need is the node's technology x stage need, inherited, so a proxy; demand = share of allied demand). %d of %d nodes:", nrow(nt), sum(d$unit_type == "node")), "",
      pm_md_table(nt, names(nt), c("Compact", "Node", "Stage", paste(A, "share"), paste(A, "RCA"), paste(A, "need"), paste(A, "demand"), paste(B, "share"), paste(B, "RCA"), paste(B, "need"), paste(B, "demand"), "AIB class", "Coercive top supplier")), "")
  pp <- cells[cells$stage != "Deployment", ]
  top_pp <- function(sfx) { v <- pp[[paste0("policy_priority_", sfx)]]; o <- order(-v); x <- pp[o, ][1:5, ]; paste(sprintf("%s %s (%.2f)", x$tech, tolower(x$stage), v[o][1:5]), collapse = "; ") }
  add(sprintf("- [Data] Revealed policy priority (NIPO / IEA PAMS within-country percentile; policy never enters the classification) is highest for %s in: %s; for %s in: %s.", A, top_pp("a"), B, top_pp("b")),
      sprintf("- [Data] Evidence quality: %d of %d cells have high stage-specific evidence for both countries; %d units are flagged low-evidence.", sum(cells$evidence_quality >= 1, na.rm = TRUE), nrow(cells), sum(d$low_evidence)), "")

  # ---- 3. Complementarity map ----------------------------------------------------------------------------
  cm <- d[d$relationship_state %in% c(PM_ACTIVE_STATES, "direct_competition"), ]
  cm <- cm[order(cm$priority_rank), ]
  ct <- data.frame(Rank = cm$priority_rank, Tier = cm$priority_label, Technology = ifelse(cm$unit_type == "node", vapply(cm$compact, grp_name, character(1)), cm$tech),
                   Unit = cm$node_label, Stage = cm$stage,
                   A = pm_pos_txt(cm$position_band_a, cm$world_share_a, cm$unit_type), B = pm_pos_txt(cm$position_band_b, cm$world_share_b, cm$unit_type),
                   State = cm$state_label, Case = sprintf("%s; need %s / %s", gsub("_", " ", cm$constraint_type), pm_f2(cm$strategic_need_a), pm_f2(cm$strategic_need_b)),
                   Confidence = pm_band(cm$evidence_quality), stringsAsFactors = FALSE)
  add("## 3. Bilateral Complementarity Map", "",
      sprintf("[Data] State counts over all units: %s.", paste(sprintf("%s %d", names(st)[st > 0], st[st > 0]), collapse = "; ")),
      "", "[Inference] Rules (Section 3 of docs/partnership_memos.md): two strong positions are **direct competition**, never complementarity; a supplier must be strong (or a contender the engine names as the other's trusted partner) and the buyer must have high need, or material demand where trusted capacity is short or a chokepoint exists; **capacity-constrained** where the AIB finds allied capacity short of demand; **shared vulnerability** where both need the node, neither supplies it and a coercive supplier holds at least half of world supply.", "",
      pm_md_table(ct, names(ct), c("Rank", "Tier", "Technology", "Unit", "Stage", paste(A, "position"), paste(B, "position"), "Dyadic relationship", "Strategic case", "Confidence")), "")
  low <- d[d$relationship_state == "low_strategic_case" & d$unit_type == "node", ]
  add(sprintf("- [Data] Low-priority / open-market nodes (%d): %s.", nrow(low), paste(tolower(low$node_label), collapse = ", ")),
      sprintf("- [Data] Deployment cells are demand context, not supply relations: high deployment need in %s.", paste(sprintf("%s (%s)", d$tech[d$relationship_state == "deployment_demand"], gsub(".*need is high in (.*): demand.*", "\\1", d$state_reason[d$relationship_state == "deployment_demand"])), collapse = "; ")), "")

  # ---- 4. Trade ------------------------------------------------------------------------------------------
  tt <- trade_tbl
  q <- function(type) tt[tt$action_type %in% type, ]
  add("## 4. Trade Agenda", "",
      sprintf("- [Inference] **What %s should export to %s:** %s.", Ar, Br, paste(unique(d$node_label[d$supplier %in% a & d$priority_tier %in% 1:2]), collapse = "; ")),
      sprintf("- [Inference] **What %s should export to %s:** %s.", Br, Ar, paste(unique(d$node_label[d$supplier %in% b & d$priority_tier %in% 1:2]), collapse = "; ")),
      sprintf("- [Inference] **Where tariffs would undermine allied specialisation:** %s.", if (nrow(q("tariff_risk"))) paste(q("tariff_risk")$unit_id, collapse = ", ") else "no node where the buyer's protection priority is in its top third and the partner is the trusted supplier"),
      sprintf("- [Inference] **Procurement, standards and content rules to recognise mutually:** %s.", paste(q(c("procurement_recognition", "mutual_recognition"))$unit_id, collapse = ", ")),
      sprintf("- [Inference] **Where third-country circumvention safeguards are genuinely required:** %s; elsewhere they would add cost without security.", if (nrow(q("anti_circumvention"))) paste(q("anti_circumvention")$unit_id, collapse = ", ") else "none"),
      sprintf("- [Inference] **Where unrestricted global sourcing is acceptable:** %s.", paste(q("open_sourcing")$unit_id, collapse = ", ")),
      "", pm_md_table(tt, c("rec_id", "direction", "unit_id", "action_type", "action"), c("ID", "Direction", "Unit", "Action type", "Action")), "")
  ef <- ev[ev$evidence_id %in% c("E08", "E09", "E10", "E06"), ]
  if (nrow(ef)) add("- [External] The tariff baseline is unsettled, which is itself a reason to bind the nodes above:", pm_ev_line(ev, ef$evidence_id), "")

  # ---- 5. Investment -------------------------------------------------------------------------------------
  add("## 5. Investment Agenda", "")
  lanes <- list(c(sprintf("%s -> %s", a, b), sprintf("### %s → %s investment (%s capital and firms expanding in %s)", A, B, A, B)),
                c(sprintf("%s -> %s", b, a), sprintf("### %s → %s investment (%s capital and firms expanding in %s)", B, A, B, A)),
                c("joint", "### Joint investment (at home or in third countries)"))
  for (ln in lanes) {
    x <- if (nrow(inv_tbl)) inv_tbl[inv_tbl$lane == ln[1], ] else inv_tbl
    add(ln[2], "", if (nrow(x)) pm_md_table(x, c("rec_id", "node_label", "capability_owner", "destination", "investment_logic", "demand_case", "private_actors", "public_instrument", "additionality_label", "additionality_rationale", "key_obstacle", "time_horizon"),
                                               c("ID", "Node", "Capital / capability owner", "Destination", "Logic", "Demand case", "Private actors (evidence)", "Public instrument", "Additionality", "Additionality components", "Key obstacle", "Horizon")) else "_No lane passes the rules._", "")
  }
  add("- [Inference] Additionality = mean of three named components: the market failure (chokepoint 1, capacity constraint 0.75, gap or emerging bottleneck 0.5, none 0), one minus an existing commitment in the evidence layer (committed 1, announced 0.5) and one minus a working market (the buyer already takes 30% or more of its imports from the supplier). It is **low** whenever there is no failure or the investment is already committed: public support would then not be additional.", "")

  # ---- 6. Third-country ----------------------------------------------------------------------------------
  add("## 6. Third-Country and Development Cooperation", "")
  if (nrow(third)) {
    add(sprintf("- [Data] From the development-partnership pairs (scripts/93: a donor's need meets a developing economy's credible Catch Up pathway in the same step), %s and %s are both donors at %d recipient steps. Ranked: both donors' own need first, then steps inside a recommended compact.", A, B, nrow(third)),
        sprintf("- [Inference] Roles: %s brings %s; %s brings %s (AIB strategic roles).", A, third$a_contribution[1], B, third$b_contribution[1]), "",
        pm_md_table(third, c("rec_id", "recipient", "technology", "stage", "compact", "recipient_choice", "recipient_anchor", "a_need", "b_need", "tools", "evidence_quality"),
                    c("ID", "Recipient", "Technology", "Stage", "Compact", "Recipient choice", "Recipient anchor", paste(A, "interest"), paste(B, "interest"), "Tools", "Evidence")), "")
  } else add("_No development-partnership table was supplied; run scripts/93 and pass --devpart._", "")
  add(pm_ev_line(ev, ev$evidence_id[vapply(strsplit(ev$countries, "\\|"), function(z) all(c(a, b) %in% z) && length(z) > 2, logical(1))]), "")

  # ---- 7. Compacts ---------------------------------------------------------------------------------------
  sel <- cc[cc$recommended, ]
  add("## 7. Recommended Sectoral Compacts", "",
      sprintf("[Inference] %d compacts pass (rule: at least one joint bet, at least two active nodes and both countries holding an AIB compact role; or, labelled **evidence-led**, an existing bilateral pipeline or at least %d bilateral project items where OSI measures a need but cannot see the partner's capability). The hypothesis test for every AIB compact:", nrow(sel), cfg$evidence_led_min_items), "",
      pm_md_table(cc, c("compact_rank", "strategic_compact_name", "compact_fit", "n_joint_bets", "n_coordinate", "n_manage_competition", "role_a", "role_b", "mechanisms", "n_evidence_items", "recommended"),
                  c("Rank", "Compact", "Verdict", "Joint bets", "Coordinate", "Manage competition", paste(A, "role"), paste(B, "role"), "Bilateral mechanisms", "Evidence items", "Recommended")), "")
  for (i in seq_len(nrow(sel))) {
    s <- sel[i, ]; cmid <- s$strategic_compact_id
    x <- d[d$unit_type == "node" & d$compact %in% cmid, ]; act <- x[x$relationship_state %in% PM_ACTIVE_STATES, ]; act <- act[order(act$priority_rank), ]
    meta <- compacts[compacts$strategic_compact_id == cmid, ]
    ar <- arch[arch$strategic_compact_id == cmid, ]
    third_h <- unique(unlist(strsplit(paste(act$aib_third_hosts, collapse = ";"), ";"))); third_h <- third_h[nzchar(third_h)]
    dl <- deals[deals$compact %in% cmid, ]
    add(sprintf("### %d. %s%s", i, s$strategic_compact_name, if (s$compact_fit == "evidence-led candidate") " (evidence-led)" else ""), "")
    if (s$compact_fit == "evidence-led candidate") {
      needx <- x[x$need_band_a %in% c("high", "demand") | x$need_band_b %in% c("high", "demand"), ]
      add(sprintf("- [Inference] **Basis: external evidence.** OSI trade data show no bilateral complementarity at this compact's nodes (%s world share: %s); the case rests on the bilateral pipeline and corporate evidence listed at the end of this compact.",
                  Br, paste(sprintf("%s %s", tolower(x$node_label), pm_pct(x$world_share_b, 1)), collapse = ", ")),
          sprintf("- [Data] Need that OSI does measure in the compact: %s.", if (nrow(needx)) paste(sprintf("%s (%s %s, %s of allied demand; %s %s)", tolower(needx$node_label), A, pm_need_txt(needx$need_band_a, needx$strategic_need_a), pm_pct(needx$demand_share_a, 0), B, pm_need_txt(needx$need_band_b, needx$strategic_need_b)), collapse = "; ") else "none at node level"))
    }
    add("**Strategic objective**",
        sprintf("- [Data] AIB mission: %s.", ifelse(nrow(meta) && !is.na(meta$mission_statement[1]), meta$mission_statement[1], "n/a")),
        if (nrow(act)) sprintf("- [Recommendation] Change the outcome at %s: %s.", paste(tolower(act$node_label[1:min(3, nrow(act))]), collapse = ", "), paste(unique(gsub("_", " ", act$constraint_type[1:min(3, nrow(act))])), collapse = "; ")) else
          sprintf("- [Recommendation] Use %s to tie the announced projects to the nodes where OSI measures need, and verify the partner capability that trade data cannot see (ownership abroad, domestic sales, heavy components) before any public finance is added.", if (nzchar(s$mechanisms)) s$mechanisms else "the existing bilateral channels"),
        "", "**Why this pair?**",
        if (nrow(act)) sprintf("- [Inference] %s", paste(sprintf("%s: %s (%s).", act$node_label, act$state_label, act$state_reason), collapse = " ")) else "- [Inference] No OSI-observed complementarity; see the evidence below.",
        sprintf("- [Data] AIB compact roles: %s %s (lead node %s); %s %s (lead node %s).", A, s$role_a, s$lead_node_a, B, s$role_b, s$lead_node_b),
        "", "**Supply-chain architecture**", "",
        pm_md_table(ar, c("stage", "unit", "basis", "position_a", "position_b", "relationship_state", "treatment", "main_instrument"),
                    c("Stage", "Unit", "Basis", paste(A, "position"), paste(B, "position"), "Desired bilateral arrangement", "Treatment", "Instrument")), "",
        "**Reciprocal bargain**",
        sprintf("- %s provides: %s.", A, paste(unique(c(if (any(act$supplier %in% a)) sprintf("supply of %s", paste(tolower(act$node_label[act$supplier %in% a]), collapse = ", ")),
                                                      if (any(act$buyer %in% a)) sprintf("long-term demand and offtake for %s", paste(tolower(act$node_label[act$buyer %in% a]), collapse = ", ")),
                                                      "procurement recognition of the partner as trusted supply")), collapse = "; ")),
        sprintf("- %s provides: %s.", B, paste(unique(c(if (any(act$supplier %in% b)) sprintf("capacity and technology in %s", paste(tolower(act$node_label[act$supplier %in% b]), collapse = ", ")),
                                                      if (any(act$buyer %in% b)) sprintf("long-term demand for %s", paste(tolower(act$node_label[act$buyer %in% b]), collapse = ", ")),
                                                      "JBIC/NEXI finance for capacity in the partner market")), collapse = "; ")),
        sprintf("- Jointly: %s.", if (any(act$relationship_state == "shared_vulnerability")) sprintf("pooled offtake and finance for non-coercive %s", paste(tolower(act$node_label[act$relationship_state == "shared_vulnerability"]), collapse = ", ")) else "common specifications and qualification"),
        sprintf("- Third-country partners needed: %s.", if (length(third_h)) paste(third_h, collapse = ", ") else "none identified by the AIB host model"),
        {bp <- x[!is.na(x$largest_trusted_supplier) & !x$largest_trusted_supplier %in% c(a, b) & x$aib_class %in% c("BUILD", "COORDINATE") &
                   !(x$position_band_a == "strong" | x$position_band_b == "strong"), ]
         if (nrow(bp)) sprintf("- [Data] Better placed than this pair (largest trusted supplier once China is removed, AIB): %s. The bilateral compact should not try to replace them there.",
                               paste(sprintf("%s at %s", bp$largest_trusted_supplier, tolower(bp$node_label)), collapse = "; ")) else character()},
        "", "**Policy instruments (minimum necessary)**",
        if (nrow(act)) sprintf("- [Recommendation] %s", paste(unique(sprintf("%s for %s", act$main_instrument, tolower(act$node_label))), collapse = "; ")) else "- [Recommendation] Use the existing investment vehicle; no new instrument.",
        "", "**Priority transactions**",
        if (nrow(dl)) sprintf("- [Recommendation] %s (%s).", dl$deal, dl$rec_id) else "- See Section 9; no OSI-ranked deal in this compact.",
        "", "**Success by 2030**",
        if (nrow(act)) sprintf("- %s: %s%s",
                               act$node_label,
                               ifelse(act$chokepoint, sprintf("coercive share of supply to the two buyers below %s (%s now holds %s of world supply)", pm_pct(cfg$thresholds$chokepoint_share), act$coercive_top_iso3, pm_pct(act$coercive_world_share)),
                                      ifelse(!is.na(act$residual_gap), sprintf("allied residual gap under the China shock closed (now %s of allied demand)", pm_pct(act$residual_gap)), "trusted capacity added and contracted")),
                               ifelse(is.na(act$supplier), ".", sprintf("; %s's share of %s's imports up from %s.", nm(act$supplier), nm(act$buyer),
                                                                        pm_pct(ifelse(act$supplier %in% a, act$b_imports_from_a_share, act$a_imports_from_b_share), 0)))) else "- Projects in the pipeline reach final investment decision with partner-made equipment in the bill of materials.",
        "- The AIB node class moves from BUILD to COORDINATE or LOW INTERVENTION on the next run.",
        "", "**Failure / exit conditions**",
        "- No signed offtake or financing within 24 months of launch: drop the node from the compact.",
        "- The coercive share of world supply falls below the chokepoint line, or the AIB class turns LOW INTERVENTION: move to open-market treatment.",
        "- Projects proceed on commercial terms without public support: stop the support (it is not additional).", "")
    eids <- unique(unlist(strsplit(paste(x$evidence_id_list, collapse = ";"), ";")))
    eids <- c(eids, ev$evidence_id[ev$compact %in% cmid & vapply(strsplit(ev$countries, "\\|"), function(z) all(c(a, b) %in% z), logical(1))])
    if (length(unique(eids[nzchar(eids)]))) add("**Evidence (external)**", pm_ev_line(ev, unique(eids)), "")
  }

  # ---- 8. MEA --------------------------------------------------------------------------------------------
  hm <- mech[mech$mechanism_id %in% mea$host_mechanism, ]
  add("## 8. Master Energy Agreement Assessment", "",
      sprintf("- [Inference] Rule: **existing** if every recommended compact is covered by a mechanism with both a pipeline and finance and a cross-sector mechanism has KPIs and a review cycle; **upgrade** if at least %s of compacts are covered by a mechanism with finance or a pipeline, or a cross-sector mechanism with a pipeline exists; **new MEA** otherwise.", pm_pct(cfg$mea$upgrade_min_coverage)),
      sprintf("- [Inference] Result: **%s**; coverage %s of recommended compacts; cross-sector mechanism with KPIs and review: %s.", mea$option, pm_pct(mea$coverage_share), ifelse(mea$cross_sector_with_kpis, "yes", "no")), "",
      pm_md_table(mea$per_compact, names(mea$per_compact), c("Compact", "Any mechanism", "Finance or pipeline", "Pipeline and finance")), "",
      pm_md_table(mech, c("mechanism_id", "name", "status", "scope_compacts", "cross_sector", "has_pipeline", "has_finance", "has_kpis", "has_review", "reciprocal", "sunset"),
                  c("ID", "Mechanism", "Status", "Scope", "Cross-sector", "Pipeline", "Finance", "KPIs", "Review", "Reciprocal", "Sunset")), "")
  if (mea$option != "existing") {
    sc <- sel$strategic_compact_name
    add(sprintf("**Recommended: %s**", if (mea$option == "upgrade") sprintf("upgrade %s into the execution framework for the compacts", if (nrow(hm)) hm$name else mea$host_mechanism) else "a new MEA-style framework"),
        sprintf("- **Scope:** %s; no other sectors.", paste(sc, collapse = "; ")),
        sprintf("- **Reciprocal commitments:** %s commits demand certainty (procurement recognition and anchor offtake for the nodes where %s supplies) and market-access predictability on those nodes; %s commits capacity and JBIC/NEXI/JOGMEC finance for the same nodes, in the US or with named third-country hosts, and offtake for %s-supplied nodes.", A, B, B, A),
        sprintf("- **Compact portfolio:** %s.", paste(sprintf("%s (%s)", sc, sel$compact_fit), collapse = "; ")),
        "- **Financing lanes:** project SPV loans and insurance as in the existing vehicle (JBIC, NEXI); US loan guarantees and offtake (DOE LPO, DoW OSC, DLA); third-country finance (DFC with JBIC).",
        sprintf("- **Governance:** one bilateral steering line inside %s, reporting to the Economic 2+2; the critical-minerals Rapid Response Group runs the minerals and magnets workstreams. Selection by joint committee, not by one side (the 2026 selection dispute is evidence the current process is not reciprocal).", mea$host_mechanism),
        "- **Milestones:** the 12-month milestones of the deal pipeline (Section 9).",
        "- **KPI dashboard:** per compact node, the coercive share of each country's supply, the bilateral import share, offtake signed (volume), capacity at FID and operating, and public money committed vs private.",
        sprintf("- **Review cycle:** every %d months, with the OSI tables re-run on new trade data.", cfg$mea$review_cycle_months),
        sprintf("- **Sunset / exit:** each compact carries the exit conditions in Section 7; the framework expires with the investment vehicle (%s) unless renewed on evidence.", if (nrow(hm) && !is.na(hm$sunset)) hm$sunset else "its own sunset"), "")
  }

  # ---- 9. Deals ------------------------------------------------------------------------------------------
  add("## 9. 12–36 Month Deal Pipeline", "",
      "[Inference] Ranked lexicographically on separate fields (importance, then additionality, urgency, tractability, execution speed, value-chain interdependency, evidence quality); urgency is not importance. At most two deals per compact.", "",
      pm_md_table(deals, c("rank", "deal", "compact", "stage", "constraint_type", "instrument", "responsible_institutions", "private_actors", "scale", "milestone_12m", "outcome_36m", "importance", "urgency", "additionality", "confidence"),
                  c("Rank", "Deal / action", "Compact", "Stage", "Problem solved", "Instrument", "Responsible institutions", "Private actors (evidence)", "Capital / demand scale", "12m milestone", "36m outcome", "Importance", "Urgency", "Additionality", "Confidence")), "")

  # ---- 10. Not to do -------------------------------------------------------------------------------------
  add("## 10. What Not to Do", "", sprintf("- [Recommendation] **%s** %s", nonp$rec_id, nonp$text), "")

  # ---- 11. Evidence --------------------------------------------------------------------------------------
  used <- unique(unlist(strsplit(paste(c(d$evidence_id_list, deals$evidence_ids, inv_tbl$evidence_ids), collapse = ";"), ";")))
  add("## 11. Evidence Register", "",
      "- [Data] Every recommendation (IDs J, C, T, I, X, D, N, M) has rows in `evidence_register.csv`: the OSI metrics behind it (variable, value for each country, year, source, evidence specificity) and any external items, with URL and access date. Recommendations that rest on external research only carry `basis = external`.",
      sprintf("- [Data] External evidence vintage: accessed %s; %d items (%s).", ev_vintage, nrow(ev), paste(sprintf("%s %d", names(table(ev$verification)), table(ev$verification)), collapse = ", ")),
      "", "**Where the findings are weak or counterintuitive**",
      sprintf("- Joint bets resting on a node need inherited from its cell (a proxy): %s.", paste(jb$node_label[jb$unit_type == "node"], collapse = "; ")),
      sprintf("- Evidence-led compacts (OSI cannot see the partner's capability in trade data): %s.", paste(cc$strategic_compact_name[cc$compact_fit == "evidence-led candidate"], collapse = "; ")),
      sprintf("- %s has no listed trade position (under 1%% of world supply) at nodes where %s has material demand: %s. Trade data miss capability that is owned abroad or sold at home, so these nodes cannot show complementarity even where corporate evidence suggests it.",
              Br, Ar, paste(tolower(d$node_label[which(d$unit_type == "node" & d$world_share_b < 0.01 & d$need_band_a %in% c("high", "demand"))]), collapse = ", ")),
      {cf <- d[which(d$unit_type == "node" & d$relationship_state == "low_strategic_case"), ]
       mf <- ev[ev$item_type == "market_fact" & ev$aib_node %in% cf$node_id & grepl("deficit|shortage|lead time|gap|only|sole", ev$claim, ignore.case = TRUE), ]
       if (nrow(mf)) sprintf("- OSI reads %s as low strategic case, but the evidence layer records a capacity problem the annual trade data cannot see: %s %s Treat it as an emerging bottleneck to re-test, not a settled open-market node.",
                             mf$aib_node, pm_ev_tag(mf), pm_claim_clean(mf$claim)) else character()},
      sprintf("- Low-evidence units among the active states: %s.", { z <- d$unit_id[d$low_evidence & d$relationship_state %in% PM_ACTIVE_STATES]; if (length(z)) paste(z, collapse = ", ") else "none" }),
      sprintf("- Fuel-trade lanes reach tier 1 on Japanese import need (%s): real energy-security lanes, but outside the electro-industrial compacts and with no market failure beyond the supply concentration the engine measures.", paste(tolower(jb_off$node_label), collapse = "; ")),
      "")
  # the summary boxes of the print document (the briefs' choice boxes)
  tsel <- function(types) { x <- trade_tbl[trade_tbl$action_type %in% types, , drop = FALSE]; unique(tolower(d$node_label[match(x$unit_id, d$unit_id)])) }
  box_build <- paste0(paste(sprintf("%s: %s", jt$Sector, jt$Node), collapse = ". "), ".")
  box_trade <- paste(c(
    if (length(tsel(c("procurement_recognition", "mutual_recognition")))) sprintf("Recognise trusted partner supply of %s in procurement and content rules.", paste(utils::head(tsel(c("procurement_recognition", "mutual_recognition")), 3), collapse = "; ")),
    if (length(tsel("anti_circumvention"))) sprintf("Common anti-circumvention rules for %s.", paste(tsel("anti_circumvention"), collapse = " and ")),
    if (length(tsel("tariff_risk"))) sprintf("Bind market access for %s.", paste(tsel("tariff_risk"), collapse = ", ")),
    if (length(tsel("open_sourcing"))) sprintf("Keep %s open to global sourcing.", paste(utils::head(tsel("open_sourcing"), 3), collapse = "; "))), collapse = " ")
  box_mea <- switch(mea$option,
    existing = "Run the compacts through the existing mechanisms; no new umbrella.",
    upgrade = sprintf("Upgrade %s: sector strategy, reciprocal commitments, a KPI dashboard and an annual review. No new MOU.", if (nrow(mech[mech$mechanism_id %in% mea$host_mechanism, ])) mech$name[mech$mechanism_id %in% mea$host_mechanism][1] else mea$host_mechanism),
    new_mea = "A new multi-year framework with a shared pipeline, KPIs and review is warranted.")
  box_out <- paste(vapply(utils::head(nonp$text, 3), function(t) sub("^(Do not [^:.;]*).*$", "\\1.", t), ""), collapse = " ")
  boxes <- pm_doc_boxes(box_build, box_trade, box_mea, box_out)
  list(markdown = L, boxes = boxes, recommendations = pm_recommendations(jb, cc, trade_tbl, inv_tbl, third, deals, nonp, mea, ev, mech, a, b), joint_bets = jb$unit_id)
}

# Every recommendation in one frame: rec_id, rec_type, unit_id, text, evidence_ids.
pm_recommendations <- function(jb, cc, trade_tbl, inv_tbl, third, deals, nonp, mea, ev, mech, a, b) {
  r <- list()
  pair_ev <- ev[vapply(strsplit(ev$countries, "\\|"), function(z) all(c(a, b) %in% z), logical(1)), , drop = FALSE]
  cc_ev <- function(cm) paste(pair_ev$evidence_id[pair_ev$compact == cm], collapse = ";")
  mech_ev <- function(ids) paste(unique(unlist(strsplit(mech$evidence_ids[mech$mechanism_id %in% ids], "\\|"))), collapse = ";")
  if (nrow(jb)) r$jb <- data.frame(rec_id = sprintf("J%02d", seq_len(nrow(jb))), rec_type = "joint_bet", unit_id = jb$unit_id,
                                   text = sprintf("%s: %s via %s", jb$node_label, jb$state_label, jb$main_instrument), evidence_ids = jb$evidence_id_list, stringsAsFactors = FALSE)
  sel <- cc[cc$recommended, , drop = FALSE]
  if (nrow(sel)) r$cc <- data.frame(rec_id = sprintf("C%02d", seq_len(nrow(sel))), rec_type = "compact", unit_id = sel$lead_node,
                                    text = sprintf("Bilateral compact priority (%s): %s", sel$compact_fit, sel$strategic_compact_name),
                                    evidence_ids = vapply(sel$strategic_compact_id, cc_ev, character(1)), stringsAsFactors = FALSE)
  if (nrow(trade_tbl)) r$tr <- data.frame(rec_id = trade_tbl$rec_id, rec_type = "trade", unit_id = trade_tbl$unit_id, text = trade_tbl$action, evidence_ids = "", stringsAsFactors = FALSE)
  if (nrow(inv_tbl)) r$iv <- data.frame(rec_id = inv_tbl$rec_id, rec_type = "investment", unit_id = inv_tbl$unit_id,
                                        text = sprintf("%s investment lane: %s (%s)", inv_tbl$lane, inv_tbl$node_label, inv_tbl$public_instrument), evidence_ids = inv_tbl$evidence_ids, stringsAsFactors = FALSE)
  if (nrow(third)) r$x <- data.frame(rec_id = third$rec_id, rec_type = "third_country", unit_id = paste(third$technology, third$stage, sep = "|"),
                                     text = sprintf("Joint third-country step: %s %s %s", third$recipient, third$technology, third$stage), evidence_ids = "", stringsAsFactors = FALSE)
  if (nrow(deals)) r$dl <- data.frame(rec_id = deals$rec_id, rec_type = "deal", unit_id = deals$unit_id, text = deals$deal, evidence_ids = deals$evidence_ids, stringsAsFactors = FALSE)
  if (nrow(nonp)) r$np <- data.frame(rec_id = nonp$rec_id, rec_type = "non_priority", unit_id = nonp$unit_id, text = nonp$text,
                                     evidence_ids = ifelse(nonp$kind == "institution", mech_ev(mech$mechanism_id), ""), stringsAsFactors = FALSE)
  r$mea <- data.frame(rec_id = "M01", rec_type = "mea", unit_id = NA_character_, text = sprintf("MEA assessment: %s (%s)", mea$option, mea$host_mechanism),
                      evidence_ids = mech_ev(mech$mechanism_id), stringsAsFactors = FALSE)
  out <- do.call(rbind, r)
  rownames(out) <- NULL
  out
}
