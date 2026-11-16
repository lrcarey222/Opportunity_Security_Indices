# Partnership Memos: the Compact memo text (docs/partnership_memos.md, "Compact memos"). Same sections, tags and
# print format as the bilateral memo (pm_memo.R), read across the compact's members instead of two countries.
# Every bullet carries a tag: [Data], [External Exx, verification], [Inference] or [Recommendation]. Pure functions.

# Evidence lines, most useful first (verified, then in force / operating, then recent), capped; the rest stays in
# the evidence register.
pm_ev_lines_capped <- function(ev, ids, cap) {
  e <- ev[ev$evidence_id %in% ids & ev$verification != "unverified", , drop = FALSE]
  if (!nrow(e)) return(list(lines = character(), n_more = 0L))
  live <- e$status %in% c("in_force", "operating", "under_construction", "signed")
  e <- e[order(e$verification != "verified", !live, -as.numeric(as.Date(ifelse(nzchar(e$publication_date), e$publication_date, "1900-01-01"))), e$evidence_id), , drop = FALSE]
  n_more <- max(0L, nrow(e) - cap)
  e <- utils::head(e, cap)
  e <- e[order(e$publication_date, e$evidence_id), , drop = FALSE]
  list(lines = sprintf("- %s %s", pm_ev_tag(e), pm_claim_clean(e$claim)), n_more = n_more)
}

# Position / need code for the member matrix: S strong, M contender, W weak; need H high, D material demand, L low.
pm_member_code <- function(band, need) paste0(ifelse(is.na(band), "-", c(strong = "S", moderate = "M", weak = "W", not_measured = "-")[band]), "/",
                                              ifelse(is.na(need), "-", c(high = "H", demand = "D", low = "L", not_measured = "-")[need]))

pm_compact_memo_sections <- function(na, prof, cells, members, lanes, trade_tbl, inv_tbl, third, deals, gov, nonp, ev, ev_ids,
                                     agreements, aib_meta, ccfg, cfg, names, institutions = NULL, parties = members$iso3, node_notes = NULL) {
  id <- ccfg$id; title <- ccfg$title
  nm1 <- function(iso) pm_names_of(iso, names)
  nmlist <- function(s, n = 3) { if (is.na(s) || !nzchar(s)) return("none"); pm_join(nm1(strsplit(s, ";")[[1]]), n) }
  ucf <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  ev_vintage <- if (nrow(ev)) max(ev$access_date) else NA
  core <- members$iso3[members$core]
  inst_of <- function(iso, fam) { if (is.null(institutions) || !nrow(institutions)) return(character()); institutions$abbreviation[institutions$iso3 %in% iso & grepl(fam, institutions$instrument_families)] }
  L <- character(); add <- function(...) L <<- c(L, ...)
  st <- table(factor(na$relationship_state, levels = PM_COMPACT_STATES))
  act <- na[na$relationship_state %in% PM_COMPACT_ACTIVE, , drop = FALSE]
  bets <- na[na$priority_tier == 1, , drop = FALSE]
  bets <- utils::head(bets[order(bets$priority_rank), , drop = FALSE], cfg$priority$max_joint_bets)
  caveat_re <- "Alternative|also (covers|feeds|makes)|Mostly|mixes|alike|dominates|semiconductor"
  if (!is.null(node_notes)) node_notes <- node_notes[vapply(node_notes, function(z) grepl(caveat_re, z), logical(1))]
  noted <- if (is.null(node_notes)) character() else intersect(names(node_notes), na$node_id)
  cexp <- ev[ev$evidence_id %in% ev_ids, , drop = FALSE]

  # ---- Title ---------------------------------------------------------------------------------------------
  add(sprintf("# %s", title), "",
      sprintf("_Compact Memo, generated %s by scripts/35_build_compact_memo.R. OSI data: six-stage strategic-position table (trade 2022-24 benchmark, flows 2024, NIPO policy as of 2025) and the Allied Industrial Base strategic layer (%s scenario). External evidence: %d curated items relevant to the compact, accessed %s (verified %d, partial %d, unverified %d)._",
              format(Sys.Date()), cfg$inputs$aib_scenario, nrow(cexp), ev_vintage, sum(cexp$verification == "verified"), sum(cexp$verification == "partial"), sum(cexp$verification == "unverified")),
      "", sprintf("_Tags: **[Data]** OSI tables; **[External Exx, verification]** curated evidence layer; **[Inference]** a documented rule applied to the data; **[Recommendation]** what the compact's governments should do. Members: the %d allied economies with a presence (at least %s of world supply, or an AIB anchor or specialist position) or material demand at a compact node; %d are shown in the tables, all are in compact_members.csv._",
                       nrow(members), pm_pct(cfg$thresholds$node$presence_share), min(nrow(members), cfg$compact_memo$max_members_shown)), "")
  if (!is.null(ccfg$scope_note)) add(sprintf("_%s_", ccfg$scope_note), "")

  # ---- Executive summary ---------------------------------------------------------------------------------
  choke <- na[na$chokepoint, , drop = FALSE]
  shared <- na[na$relationship_state == "shared_vulnerability", , drop = FALSE]
  # the largest existing lanes (2024 trade) name the leading suppliers and buyers
  top_sup <- if (nrow(lanes)) names(sort(tapply(lanes$flow_usd, lanes$supplier, sum), decreasing = TRUE)) else character()
  top_buy <- if (nrow(lanes)) names(sort(tapply(lanes$flow_usd, lanes$buyer, sum), decreasing = TRUE)) else character()
  core_roles <- members[members$core, , drop = FALSE]
  add("## Executive Decision Summary", "",
      "**Why this compact matters.**",
      sprintf("- [Data] Of %d electro-industrial nodes in the compact, %d show an active compact relation (member supply meeting member need, a capacity-constrained trusted supplier or a shared chokepoint), %d member competition and %d no strategic case. %s",
              nrow(na), sum(na$relationship_state %in% PM_COMPACT_ACTIVE), st[["member_competition"]], st[["low_strategic_case"]],
              if (mean(na$relationship_state %in% PM_COMPACT_ACTIVE) >= 0.75) sprintf("Nearly every step is under stress, so a compact is the right scale of response; the rules then rank where it acts first (%d nodes need new capacity, the rest coordinated trade).", sum(na$treatment == "BUILD"))
              else "Much of the chain needs no compact intervention: the rules single out where coordination or new capacity changes the outcome."),
      if (nrow(choke)) sprintf("- [Data] A coercive supplier holds at least %s of world supply at %d nodes: %s.", pm_pct(cfg$thresholds$chokepoint_share), nrow(choke),
                               paste(sprintf("%s (%s %s)", pm_lc1(choke$node_label), choke$coercive_top_iso3, pm_pct(choke$coercive_world_share)), collapse = "; ")) else
        "- [Data] No compact node has a coercive supplier at the chokepoint line.",
      sprintf("- [Data] AIB node classes under the China shock: %s; allied demand across the compact's nodes %s.", paste(sprintf("%s %d", names(table(na$aib_class)), table(na$aib_class)), collapse = ", "), pm_usd(sum(na$node_demand_usd, na.rm = TRUE))),
      if (!is.null(aib_meta$headline_result) && !is.na(aib_meta$headline_result)) sprintf("- [Data] AIB headline: %s", aib_meta$headline_result) else character(),
      sprintf("- [Data] %d member economies; core (AIB): %s.", nrow(members), if (nrow(core_roles)) paste(sprintf("%s (%s)", core_roles$country, ifelse(is.na(core_roles$compact_role), "no compact role", pm_lc1(core_roles$compact_role))), collapse = "; ") else "none listed"),
      "", "**The central compact bargain.**",
      sprintf("- [Inference] %s; %s.",
              if (length(top_sup)) sprintf("By 2024 trade, the largest member suppliers (%s) supply %s, and the largest member buyers are %s", pm_join(nm1(top_sup), 4), pm_join(unique(pm_lc1(lanes$node_label)), 5), pm_join(nm1(top_buy), 4)) else "No member supplier outranks a member buyer at any node",
              if (nrow(shared)) sprintf("at %s no member supplies the others and the answer is pooled demand, finance and third-country capacity%s", pm_join(pm_lc1(shared$node_label), 5),
                                        { h <- unique(unlist(strsplit(paste(shared$aib_third_hosts, collapse = ";"), ";"))); h <- h[nzchar(h)]; if (length(h)) sprintf(" (AIB-ranked hosts: %s)", pm_join(nm1(h), 5)) else "" })
              else "no node is a shared vulnerability that no member can supply"),
      sprintf("- [Inference] The trade is market access and demand certainty for trusted capacity: suppliers commit capacity and priority supply, buyers commit offtake and procurement recognition, and %s.",
              if (nrow(shared)) "all members pool finance for the nodes none of them can supply" else "the compact keeps the remaining nodes open"),
      "", sprintf("**The %d highest-priority compact bets** (tier 1 by the rules in Section 3, in Section 3 order):", nrow(bets)), "")
  bt <- data.frame(Priority = seq_len(nrow(bets)), Step = bets$step, Node = paste0(bets$node_label, ifelse(bets$node_id %in% noted, " †", "")), Problem = gsub("_", " ", bets$constraint_type),
                   Suppliers = vapply(ifelse(nzchar(bets$lane_suppliers), bets$lane_suppliers, NA), nmlist, character(1), USE.NAMES = FALSE),
                   Buyers = vapply(ifelse(nzchar(bets$lane_buyers), bets$lane_buyers, bets$buyers), nmlist, character(1), USE.NAMES = FALSE, n = 4),
                   Instrument = gsub("_", " ", bets$main_instrument), Timing = pm_horizon(bets$execution_speed), stringsAsFactors = FALSE)
  bt$Suppliers[bets$relationship_state == "shared_vulnerability"] <- "none among members (pooled)"
  add(pm_md_table(bt, names(bt), c("Priority", "Step", "Node", "Strategic problem", "Member suppliers", "Member buyers", "Main instrument", "Timing")), "",
      if (any(bets$node_id %in% noted)) "† The node's product definition has a caveat (AIB mapping note, Section 11); read it before citing." else character(), "")
  add("**Do not prioritise** (Section 10):", sprintf("- [Recommendation] %s", utils::head(nonp$text, 3)), "")
  gov_txt <- switch(gov$option, existing = "use the existing plurilateral mechanism", upgrade = sprintf("upgrade an existing plurilateral mechanism (%s)", gov$host_mechanism),
                    network = "link the members' bilateral frameworks", new_compact = "a new compact framework is warranted")
  add("**Existing mechanisms and the governance question** (Section 8):",
      sprintf("- [Inference] Governance assessment: **%s**.%s", gov_txt, if (length(gov$gaps)) paste0(" Gaps: ", paste(gov$gaps, collapse = "; "), ".") else ""),
      sprintf("- [Recommendation] %s", switch(gov$option,
        existing = sprintf("Run the compact through %s; no new body.", gov$host_name),
        upgrade = if (isTRUE(gov$host_partial)) sprintf("Do not create a new compact body. Give %s a compact mandate for the critical-mineral nodes (a shared project pipeline, reciprocal supplier and buyer commitments, a KPI dashboard reviewed every %d months), and set up a working group of the supplier and buyer members for the downstream steps, reporting to the same dashboard.", gov$host_name, cfg$mea$review_cycle_months)
                  else sprintf("Do not create a new compact body. Give %s a compact mandate for these nodes: a shared project pipeline, reciprocal supplier and buyer commitments and a KPI dashboard, reviewed every %d months.", gov$host_name, cfg$mea$review_cycle_months),
        network = sprintf("Link the members' bilateral frameworks (%s) through a light compact secretariat with one pipeline and one KPI dashboard; no treaty.", paste(utils::head(gov$bilateral_channels, 6), collapse = ", ")),
        new_compact = sprintf("Launch a compact framework with a shared pipeline, reciprocal commitments, KPIs and an annual review, open to the members in Section 7%s.",
                              if (!is.na(gov$candidate)) sprintf("; build it on the closest existing body, %s, rather than a new treaty", gov$candidate) else ""))), "")

  # ---- 1. Current state ------------------------------------------------------------------------------------
  ag <- agreements[agreements$iso3 %in% parties, , drop = FALSE]
  agl <- vapply(split(ag$iso3, ag$agreement), function(z) sprintf("%d (%s)", length(unique(z)), pm_join(sort(unique(z)), 12)), character(1))
  sec <- function(types, cap, extra = rep(TRUE, nrow(cexp))) pm_ev_lines_capped(ev, cexp$evidence_id[cexp$item_type %in% types & extra], cap)
  s_pl <- sec(c("plurilateral", "dialogue", "institution"), 10)
  s_tr <- sec(c("dispute", "policy"), 12, cexp$instrument %in% c("tariff", "export_control", "standards", "other", "procurement", "price_floor", "stockpile"))
  s_tr2 <- sec(c("agreement"), 4, cexp$instrument %in% c("tariff", "trade_agreement"))
  s_fw <- sec(c("framework", "agreement"), 10, !cexp$instrument %in% c("tariff", "trade_agreement"))
  s_pr <- sec(c("project", "financing", "corporate", "procurement"), 16)
  s_mf <- sec(c("market_fact"), 6)
  more <- function(s) if (s$n_more > 0) sprintf("- [Data] %d further item%s in the evidence register (`compact_evidence.csv`).", s$n_more, if (s$n_more > 1) "s are" else " is") else character()
  none_if <- function(s) if (!length(s$lines)) "- [Data] None in the evidence layer." else character()
  add("## 1. Current State of the Compact", "",
      paste0("This section sets out what already exists before anything new is proposed. Facts are from the curated evidence layer (accessed ", ev_vintage, "); items marked partial rest on secondary sources and should be confirmed before citation; unverified items are left out here and stay in the evidence register."), "",
      "**Alliance and plurilateral architecture**",
      if (length(agl)) sprintf("- [Data] Members in OSI's economic-security agreements table: %s.", paste(sprintf("%s %s", names(agl), agl), collapse = "; ")) else character(),
      s_pl$lines, more(s_pl), if (!length(agl)) none_if(s_pl) else character(), "",
      "**Market structure**", s_mf$lines, if (!length(s_mf$lines)) "- [Data] See Section 2 for OSI's own concentration measures." else character(), "",
      "**Trade measures, controls and frictions**", s_tr2$lines, s_tr$lines, more(s_tr), if (!length(s_tr2$lines)) none_if(s_tr) else character(), "",
      "**Industrial and minerals frameworks**", s_fw$lines, more(s_fw), none_if(s_fw), "",
      "**Projects, financing and corporate capacity**", s_pr$lines, more(s_pr), none_if(s_pr), "",
      {cm <- gov$mechanisms; ch <- if (!is.null(cm) && nrow(cm)) cm[cm$status %in% PM_ACTIVE_MECH, , drop = FALSE] else data.frame()
       if (nrow(ch)) sprintf("- [Inference] Working government channels covering the compact: %s.", paste(sprintf("%s (%s%s)", ch$name, ifelse(ch$plurilateral, sprintf("%d parties", ch$n_parties), "bilateral"),
                                                                                                                  ifelse(ch$has_finance %in% c("TRUE", "partial"), ", finance", "")), collapse = "; "))
       else "- [Inference] The evidence layer records no active government mechanism covering the compact; it would start from dialogue."}, "")

  # ---- 2. Members ------------------------------------------------------------------------------------------
  ms <- utils::head(members, cfg$compact_memo$max_members_shown)
  supplies <- function(iso) { x <- prof[prof$iso3 == iso & prof$position_band %in% "strong", , drop = FALSE]; x <- x[order(-x$world_share), ]; if (nrow(x)) paste(sprintf("%s %s", pm_lc1(x$node_label), pm_pct(x$world_share)), collapse = ", ") else "-" }
  needs <- function(iso) { x <- prof[prof$iso3 == iso & prof$need_band %in% c("high", "demand"), , drop = FALSE]; x <- x[order(x$need_band != "high", -ifelse(is.na(x$demand_share), 0, x$demand_share)), ]
    if (nrow(x)) paste(sprintf("%s (%s)", pm_lc1(utils::head(x$node_label, 5)), utils::head(x$need_band, 5)), collapse = ", ") else "-" }
  mt <- data.frame(Member = ms$country, Role = ifelse(is.na(ms$compact_role), "-", ms$compact_role), Core = ifelse(ms$core, "core", ""),
                   Supplies = vapply(ms$iso3, supplies, character(1)), Needs = vapply(ms$iso3, needs, character(1)), stringsAsFactors = FALSE)
  add("## 2. Who Is Good at What — and Who Needs What", "",
      sprintf("**Members** (ranked by demand-weighted presence and net imports; supplies = nodes where the member is strong, with its share of world supply; needs = nodes where it has high need or material demand). %d of %d members:", nrow(ms), nrow(members)), "",
      pm_md_table(mt, names(mt), c("Member", "AIB compact role", "Core", "Strong at (share of world supply)", "Needs")), "")
  nt <- data.frame(Step = na$step, Node = na$node_label, Stage = na$stage, AIB = na$aib_class,
                   Coercive = sprintf("%s %s", ifelse(is.na(na$coercive_top_iso3), "", na$coercive_top_iso3), pm_pct(na$coercive_world_share)),
                   Suppliers = ifelse(nzchar(na$supplier_shares), na$supplier_shares, "none"), Buyers = ifelse(nzchar(na$buyers), gsub(";", ", ", na$buyers), "none"),
                   Trusted = pm_pct(na$trusted_member_share), Largest = ifelse(is.na(na$largest_trusted_supplier), "", na$largest_trusted_supplier), stringsAsFactors = FALSE)
  nt <- nt[order(na$step_order, na$node_id), ]
  add("**Nodes** (AIB division of labour, 2024 trade; every share is the economy's larger of its world export share and its production share, so an exporter and a producer of the same material can together pass 100%; capable member suppliers with that share; member buyers = high need or material demand where capacity is short; largest trusted supplier once China is removed, AIB):", "",
      pm_md_table(nt, names(nt), c("Step", "Node", "Stage", "AIB class", "Coercive top supplier", "Member suppliers", "Member buyers", "Supply held by member suppliers", "Largest trusted supplier after AIB reallocation")), "")
  mm <- utils::head(ms$iso3, 8)
  mat <- do.call(rbind, lapply(seq_len(nrow(na)), function(i) {
    x <- prof[prof$node_id == na$node_id[i], , drop = FALSE]
    v <- vapply(mm, function(iso) { r <- x[x$iso3 == iso, , drop = FALSE]; if (!nrow(r)) "-/-" else pm_member_code(r$position_band[1], r$need_band[1]) }, character(1))
    data.frame(Node = na$node_label[i], t(v), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  mat <- mat[order(na$step_order, na$node_id), , drop = FALSE]
  add(sprintf("Position / need by member (the %d most relevant): S strong, M present (>= %s or a ranked host), W weak; need H high, D material demand, L low (node need is inherited from the member's technology x stage cell, so a proxy).",
              length(mm), pm_pct(cfg$thresholds$node$presence_share)), "",
      pm_md_table(mat, names(mat), c("Node", nm1(mm))), "")
  ccells <- cells[cells$tech %in% unique(na$tech) & cells$stage != "Deployment", , drop = FALSE]
  if (nrow(ccells)) {
    pp <- sort(tapply(ccells$policy_priority, ccells$iso3, mean, na.rm = TRUE), decreasing = TRUE)
    add(sprintf("- [Data] Revealed policy priority on the compact's technologies (NIPO / IEA PAMS within-country percentile, mean over producing stages; policy never enters the classification) is highest in: %s.",
                paste(sprintf("%s (%.2f)", nm1(utils::head(names(pp), 6)), utils::head(pp, 6)), collapse = "; ")))
  }
  add(sprintf("- [Data] Evidence quality: %d of %d nodes are flagged low-evidence (below %.2f).", sum(na$low_evidence), nrow(na), cfg$evidence$low_evidence_below), "")

  # ---- 3. Map ----------------------------------------------------------------------------------------------
  cmap <- na[na$relationship_state %in% c(PM_COMPACT_ACTIVE, "member_competition"), , drop = FALSE]
  ct <- data.frame(Rank = cmap$priority_rank, Tier = cmap$priority_label, Step = cmap$step, Node = cmap$node_label, State = cmap$state_label,
                   Suppliers = vapply(ifelse(cmap$relationship_state == "member_competition", cmap$strong_members, ifelse(nzchar(cmap$lane_suppliers), cmap$lane_suppliers, NA)), nmlist, character(1), USE.NAMES = FALSE),
                   Buyers = vapply(ifelse(nzchar(cmap$lane_buyers), cmap$lane_buyers, ifelse(nzchar(cmap$buyers), cmap$buyers, NA)), nmlist, character(1), USE.NAMES = FALSE, n = 4),
                   Case = sprintf("%s; urgency %s, importance %s, additionality %s", gsub("_", " ", cmap$constraint_type), pm_f2(cmap$urgency), pm_f2(cmap$strategic_importance), cmap$additionality_label),
                   Confidence = pm_band(cmap$evidence_quality), stringsAsFactors = FALSE)
  low <- na[na$relationship_state == "low_strategic_case", , drop = FALSE]
  dep <- cells[cells$stage == "Deployment" & cells$tech %in% unique(na$tech) & !is.na(cells$strategic_need) & cells$strategic_need >= cfg$thresholds$deployment_need_high, , drop = FALSE]
  add("## 3. Compact Division-of-Labour Map", "",
      sprintf("[Data] State counts over the compact's nodes: %s.", paste(sprintf("%s %d", names(st)[st > 0], st[st > 0]), collapse = "; ")), "",
      "[Inference] Rules (docs/partnership_memos.md, Compact memos): a node is **member supply** where a capable member supplier outranks a member buyer (high need, or material demand where trusted capacity is short or a chokepoint exists), **capacity-constrained** where the AIB also finds allied capacity short of demand, **shared vulnerability** where at least two members need the node, none supplies it and a coercive supplier holds at least half of world supply, and **member competition** where two or more members are strong and no other member needs more. Strength alone is never complementarity.", "",
      pm_md_table(ct, names(ct), c("Rank", "Tier", "Step", "Node", "Compact relationship", "Member suppliers", "Member buyers", "Strategic case", "Confidence")), "",
      sprintf("- [Data] Low-priority / open-market nodes (%d): %s.", nrow(low), if (nrow(low)) paste(sprintf("%s (%s)", pm_lc1(low$node_label), gsub("_", " ", gsub(";", ", ", low$state_reason))), collapse = "; ") else "none"),
      sprintf("- [Data] Deployment demand behind the compact (deployment need >= %.2f): %s.", cfg$thresholds$deployment_need_high,
              if (nrow(dep)) paste(vapply(split(dep$iso3, dep$tech), function(z) pm_join(nm1(sort(unique(z))), 8), character(1)), names(split(dep$iso3, dep$tech)), sep = " in ", collapse = "; ") else "none above the line"), "")

  # ---- 4. Trade --------------------------------------------------------------------------------------------
  q <- function(type) if (nrow(trade_tbl)) trade_tbl[trade_tbl$action_type %in% type, , drop = FALSE] else trade_tbl
  lt <- if (nrow(lanes)) utils::head(lanes, 14) else lanes
  add("## 4. Trade Agenda", "",
      "**Supplier-buyer lanes inside the compact** (2024 node trade; share = the supplier's share of the buyer's imports of the node):", "",
      if (nrow(lt)) pm_md_table(data.frame(Node = lt$node_label, Supplier = nm1(lt$supplier), Buyer = nm1(lt$buyer), Flow = pm_usd(lt$flow_usd), Share = pm_pct(lt$share_of_buyer_imports),
                                           Need = lt$buyer_need, Demand = pm_pct(lt$buyer_demand_share), stringsAsFactors = FALSE),
                                c("Node", "Supplier", "Buyer", "Flow", "Share", "Need", "Demand"),
                                c("Node", "Supplier", "Buyer", "Flow 2024", "Share of buyer's imports", "Buyer need", "Buyer share of allied demand")) else "_No member supplier outranks a member buyer._", "",
      sprintf("- [Inference] **Where tariffs among members would undermine the compact:** %s.", if (nrow(q("tariff_risk"))) pm_join(pm_lc1(q("tariff_risk")$node_label), 6) else "no node where a buyer's protection priority is in its top third against a member supplier"),
      sprintf("- [Inference] **Procurement and content rules to recognise across members:** %s.", if (nrow(q("procurement_recognition"))) pm_join(pm_lc1(q("procurement_recognition")$node_label), 8) else "none"),
      sprintf("- [Inference] **Where common origin and anti-circumvention rules are genuinely required:** %s; elsewhere they would add cost without security.", if (nrow(q("anti_circumvention"))) pm_join(pm_lc1(q("anti_circumvention")$node_label), 6) else "none"),
      sprintf("- [Inference] **Where unrestricted global sourcing is acceptable:** %s.", if (nrow(q("open_sourcing"))) pm_join(pm_lc1(q("open_sourcing")$node_label), 6) else "none identified"), "",
      if (nrow(trade_tbl)) pm_md_table(trade_tbl, c("rec_id", "node_label", "direction", "action_type", "action"), c("ID", "Node", "Direction", "Action type", "Action")) else "_No trade action passes the rules._", "")
  tf <- pm_ev_lines_capped(ev, cexp$evidence_id[cexp$instrument %in% c("tariff", "export_control")], 8)
  if (length(tf$lines)) add("- [External] Tariffs and export controls in force or in dispute, the baseline any compact trade rule works from:", tf$lines, "")

  # ---- 5. Investment ---------------------------------------------------------------------------------------
  add("## 5. Investment Agenda", "",
      if (nrow(inv_tbl)) pm_md_table(inv_tbl, c("rec_id", "node_label", "capability_owner", "destination", "investment_logic", "demand_case", "private_actors", "public_instrument", "additionality_label", "additionality_rationale", "key_obstacle", "time_horizon"),
                                     c("ID", "Node", "Capital / capability owner", "Destination (AIB-ranked hosts)", "Logic", "Demand case", "Private actors (evidence)", "Public instrument", "Additionality", "Additionality components", "Key obstacle", "Horizon")) else "_No node passes the investment rules._", "",
      "- [Inference] Additionality = mean of three named components: the market failure (chokepoint 1, capacity constraint 0.75, gap or emerging bottleneck 0.5, none 0), one minus existing commitments in the evidence layer (in a compact a committed project is partial coverage, 0.5; an announced one 0.25) and one minus a working market (member buyers already take 30% or more of their imports of the node from member suppliers). It is **low** where there is no failure, or where the market already works and projects are committed.", "")

  # ---- 6. Third-country ------------------------------------------------------------------------------------
  add("## 6. Third-Country and Development Cooperation", "")
  if (nrow(third)) {
    add(sprintf("- [Data] From the development-partnership pairs (scripts/93: a donor's need meets a developing economy's credible Catch Up pathway in the same step), at least two members are donors at %d recipient step%s in the compact's cells. Ranked: number of donors with their own need, then number of member donors.", nrow(third), if (nrow(third) == 1) "" else "s"), "",
        pm_md_table(third, c("rec_id", "recipient", "technology", "stage", "donor_names", "recipient_choice", "recipient_anchor", "tools", "evidence_quality"),
                    c("ID", "Recipient", "Technology", "Stage", "Member donors", "Recipient choice", "Recipient anchor", "Tools", "Evidence")), "")
  } else add("_No recipient step has two or more member donors (or no development-partnership table was supplied)._", "")
  nonmem <- cexp[vapply(strsplit(cexp$countries, "\\|"), function(z) any(!z %in% parties) && any(z %in% parties), logical(1)) &
                   cexp$item_type %in% c("project", "financing", "framework", "agreement"), , drop = FALSE]
  tc <- pm_ev_lines_capped(ev, nonmem$evidence_id, 8)
  if (length(tc$lines)) add("- [External] Projects and frameworks that already join members with non-member producers:", tc$lines, "")

  # ---- 7. Architecture -------------------------------------------------------------------------------------
  steps <- unique(na[order(na$step_order), c("step_order", "step")])
  arch <- do.call(rbind, lapply(seq_len(nrow(steps)), function(k) {
    x <- na[na$step == steps$step[k], , drop = FALSE]
    x <- x[order(!x$relationship_state %in% PM_COMPACT_ACTIVE, x$priority_rank), , drop = FALSE]
    r <- x[1, ]
    data.frame(Step = steps$step[k], Nodes = paste(pm_lc1(x$node_label), collapse = ", "), Lead = r$node_label, State = r$state_label, Treatment = r$treatment,
               Suppliers = nmlist(ifelse(nzchar(r$lane_suppliers), r$lane_suppliers, r$strong_members)),
               Buyers = nmlist(ifelse(nzchar(r$lane_buyers), r$lane_buyers, r$buyers), 4),
               Hosts = nmlist(r$aib_hosts), Instrument = ifelse(is.na(r$main_instrument), "-", gsub("_", " ", r$main_instrument)), stringsAsFactors = FALSE)
  }))
  better <- na[!is.na(na$largest_trusted_supplier) & !na$largest_trusted_supplier %in% members$iso3 & na$aib_class %in% c("BUILD", "COORDINATE"), , drop = FALSE]
  hosts_out <- unique(unlist(strsplit(paste(na$aib_third_hosts[na$relationship_state %in% PM_COMPACT_ACTIVE], collapse = ";"), ";"))); hosts_out <- setdiff(hosts_out[nzchar(hosts_out)], members$iso3)
  sup_mem <- unique(unlist(strsplit(paste(act$lane_suppliers, collapse = ";"), ";"))); sup_mem <- sup_mem[nzchar(sup_mem)]
  buy_mem <- unique(unlist(strsplit(paste(c(act$lane_buyers, act$buyers[act$relationship_state == "shared_vulnerability"]), collapse = ";"), ";"))); buy_mem <- buy_mem[nzchar(buy_mem)]
  add("## 7. Compact Architecture and Workstreams", "",
      if (!is.null(aib_meta$mission_statement) && !is.na(aib_meta$mission_statement)) sprintf("- [Data] AIB mission: %s", aib_meta$mission_statement) else character(),
      if (!is.null(aib_meta$purpose) && !is.na(aib_meta$purpose)) sprintf("- [Data] Purpose: %s", aib_meta$purpose) else character(),
      if (!is.null(aib_meta$workstreams) && length(aib_meta$workstreams)) sprintf("- [Data] AIB workstreams in the compact: %s.", paste(aib_meta$workstreams, collapse = "; ")) else character(), "",
      "**Supply-chain architecture** (the lead node of each step: active state first, then priority):", "",
      pm_md_table(arch, names(arch), c("Step", "Nodes", "Lead node", "Compact relationship", "Treatment", "Member suppliers", "Member buyers", "AIB-ranked hosts", "Instrument")), "",
      "**Membership tiers**",
      sprintf("- Core: %s.", if (length(core)) pm_join(nm1(core), 10) else "none listed by the AIB"),
      sprintf("- Supplier members (capable suppliers at an active node): %s.", if (length(sup_mem)) pm_join(nm1(sup_mem), 12) else "none"),
      sprintf("- Buyer members (need at an active node): %s.", if (length(buy_mem)) pm_join(nm1(buy_mem), 12) else "none"),
      sprintf("- Third-country hosts the AIB ranks for new capacity: %s.", if (length(hosts_out)) pm_join(nm1(hosts_out), 10) else "none outside the membership"),
      if (nrow(better)) sprintf("- [Data] Better placed outside the membership (largest trusted supplier once China is removed, AIB): %s. The compact should bring them in or buy from them, not try to replace them.",
                                paste(sprintf("%s at %s", nm1(better$largest_trusted_supplier), pm_lc1(better$node_label)), collapse = "; ")) else character(), "",
      "**Reciprocal bargain**",
      sprintf("- Supplier members (%s) commit: capacity expansion and priority supply to members at the capacity-constrained nodes, qualification of new member suppliers, and technology licensing where a member buyer builds its own base.", if (length(sup_mem)) pm_join(nm1(sup_mem), 5) else "none"),
      sprintf("- Buyer members (%s) commit: long-term offtake and procurement recognition of member output as trusted supply, and no new tariffs on member suppliers of compact nodes.", if (length(buy_mem)) pm_join(nm1(buy_mem), 6) else "none"),
      sprintf("- Jointly: %s.", if (nrow(shared)) sprintf("pooled offtake, stockpiles and finance for non-coercive %s", pm_join(pm_lc1(shared$node_label), 5)) else "common specifications and qualification"),
      "", "**Policy instruments (minimum necessary)**",
      {ai <- act[!is.na(act$main_instrument), , drop = FALSE]; if (nrow(ai)) sprintf("- [Recommendation] %s.", paste(unique(sprintf("%s for %s", gsub("_", " ", ai$main_instrument), pm_lc1(ai$node_label))), collapse = "; ")) else "- [Recommendation] No new instrument."},
      "", "**Success by 2030**",
      if (nrow(act)) sprintf("- %s: %s.", act$node_label,
                             ifelse(act$chokepoint, sprintf("coercive share of member buyers' supply below %s (%s now holds %s of world supply)", pm_pct(cfg$thresholds$chokepoint_share), act$coercive_top_iso3, pm_pct(act$coercive_world_share)),
                                    ifelse(!is.na(act$residual_gap), sprintf("allied residual gap under the China shock closed (now %s of allied demand); member buyers' imports from member suppliers up from %s", pm_pct(act$residual_gap), pm_pct(act$buyers_from_member_suppliers)),
                                           "trusted member capacity added and contracted"))) else "- No active node.",
      "- The AIB node class moves from BUILD to COORDINATE or LOW INTERVENTION on the next run.",
      "", "**Failure / exit conditions**",
      "- No signed offtake or financing at a node within 24 months of launch: drop the node from the compact.",
      "- The coercive share of world supply falls below the chokepoint line, or the AIB class turns LOW INTERVENTION: move the node to open-market treatment.",
      "- Projects proceed on commercial terms without public support: stop the support (it is not additional).", "")
  bev <- pm_ev_lines_capped(ev, unique(unlist(na$evidence_ids[na$priority_tier %in% 1:2])), 12)
  if (length(bev$lines)) add("**Evidence behind the compact bets (external)**", bev$lines, more(bev), "")

  # ---- 8. Governance ---------------------------------------------------------------------------------------
  cm <- gov$mechanisms
  add("## 8. Governance Assessment", "",
      sprintf("- [Inference] Rule: **existing** if an active plurilateral mechanism covering the compact has a project pipeline, finance, KPIs and a review cycle and includes at least %s of the core members; **upgrade** if an active plurilateral mechanism covering the compact has finance, a pipeline or a framework mandate and that share of the core; **network** if no such host exists but at least two active bilateral mechanisms among members carry finance or a pipeline; **new compact framework** otherwise.",
              pm_pct(if (is.null(cfg$compact_memo$core_coverage_min)) 0.5 else cfg$compact_memo$core_coverage_min)),
      sprintf("- [Inference] Result: **%s**%s.", gov$option, if (!is.na(gov$host_mechanism)) sprintf(" (host: %s, %s)", gov$host_name, gov$host_features) else ""), "",
      if (!is.null(cm) && nrow(cm)) pm_md_table(data.frame(ID = cm$mechanism_id, Mechanism = cm$name, Parties = ifelse(cm$plurilateral, sprintf("%d (%d members)", cm$n_parties, cm$n_member_parties), gsub("\\|", "-", cm$pair)),
                                                        Status = cm$status, Scope = gsub("\\|", ", ", cm$scope_compacts), Pipeline = cm$has_pipeline, Finance = cm$has_finance, KPIs = cm$has_kpis, Review = cm$has_review, stringsAsFactors = FALSE),
                                             c("ID", "Mechanism", "Parties", "Status", "Scope", "Pipeline", "Finance", "KPIs", "Review")) else "_No government mechanism covering the compact is in the evidence layer._", "")
  if (gov$option != "existing") {
    fin <- unique(unlist(lapply(core, function(i) { z <- inst_of(i, "finance"); if (length(z)) sprintf("%s (%s)", nm1(i), paste(z, collapse = ", ")) else character() })))
    dev <- unique(unlist(lapply(core, function(i) { z <- inst_of(i, "development"); if (length(z)) sprintf("%s (%s)", nm1(i), paste(z, collapse = ", ")) else character() })))
    add(sprintf("**Recommended: %s**", switch(gov$option, upgrade = if (isTRUE(gov$host_partial)) sprintf("give %s a compact mandate for the mineral steps, with a downstream working group", gov$host_name) else sprintf("give %s a compact mandate", gov$host_name), network = "a light compact secretariat linking the bilateral frameworks", new_compact = "a new compact framework")),
        sprintf("- **Scope:** %s; %d nodes in %d steps; no other sectors.", title, nrow(na), nrow(steps)),
        sprintf("- **Membership:** core %s; supplier and buyer members as in Section 7; third-country hosts join project by project.", if (length(core)) pm_join(nm1(core), 10) else "to be agreed"),
        "- **Reciprocal commitments:** supplier members commit capacity and priority supply at the capacity-constrained nodes; buyer members commit offtake, procurement recognition and no new tariffs on member suppliers of compact nodes; all members commit finance for the shared-vulnerability nodes.",
        sprintf("- **Compact portfolio:** %s.", if (nrow(bets)) paste(sprintf("%s (%s)", bets$node_label, gsub("_", " ", bets$main_instrument)), collapse = "; ") else "the tier-2 nodes of Section 3"),
        sprintf("- **Financing lanes:** %s; third-country finance: %s.", if (length(fin)) paste(fin, collapse = "; ") else "the members' export-credit and development banks", if (length(dev)) paste(dev, collapse = "; ") else "none listed"),
        sprintf("- **Governance:** %s; projects selected by a joint committee of supplier and buyer members, not by one government.", switch(gov$option, upgrade = sprintf("a compact working group inside %s", gov$host_name),
                                                                                                                                  network = "a small secretariat hosted by one core member, rotating", "a steering committee of the core members")),
        "- **Milestones:** the 12-month milestones of the deal pipeline (Section 9).",
        "- **KPI dashboard:** per node, the coercive share of member buyers' supply, member buyers' imports from member suppliers, offtake signed (volume), capacity at FID and operating, and public money committed vs private.",
        sprintf("- **Review cycle:** every %d months, with the OSI tables re-run on new trade data.", cfg$mea$review_cycle_months),
        "- **Sunset / exit:** each node carries the exit conditions in Section 7; the compact mandate lapses after five years unless renewed on evidence.", "")
  }

  # ---- 9. Deals ----------------------------------------------------------------------------------------------
  add("## 9. 12–36 Month Deal Pipeline", "",
      "[Inference] Ranked lexicographically on separate fields (importance, then additionality, urgency, tractability, execution speed, value-chain interdependency, evidence quality); urgency is not importance. At most two deals per step.", "",
      if (nrow(deals)) pm_md_table(deals, c("rank", "deal", "step", "stage", "constraint_type", "instrument", "responsible_institutions", "private_actors", "scale", "milestone_12m", "outcome_36m", "importance", "urgency", "additionality", "confidence"),
                                   c("Rank", "Deal / action", "Step", "Stage", "Problem solved", "Instrument", "Responsible institutions", "Private actors (evidence)", "Capital / demand scale", "12m milestone", "36m outcome", "Importance", "Urgency", "Additionality", "Confidence")) else "_No deal passes the rules._", "")

  # ---- 10. Not to do ---------------------------------------------------------------------------------------
  add("## 10. What Not to Do", "", if (nrow(nonp)) sprintf("- [Recommendation] **%s** %s", nonp$rec_id, nonp$text) else "- [Recommendation] Nothing beyond the rules above.", "")

  # ---- 11. Evidence ----------------------------------------------------------------------------------------
  unlisted <- members[members$n_presence_nodes == 0 & members$n_buyer_nodes > 0, , drop = FALSE]
  mf <- cexp[cexp$item_type == "market_fact" & cexp$aib_node %in% low$node_id & grepl("deficit|shortage|lead time|gap|only|sole", cexp$claim, ignore.case = TRUE), , drop = FALSE]
  add("## 11. Evidence Register", "",
      "- [Data] Every recommendation (IDs J, T, I, X, D, N, G) has rows in `evidence_register.csv`: the OSI metrics behind it (variable, value, year, source, evidence specificity) and any external items, with URL and access date.",
      sprintf("- [Data] External evidence vintage: accessed %s; %d items relevant to the compact (%s).", ev_vintage, nrow(cexp), paste(sprintf("%s %d", names(table(cexp$verification)), table(cexp$verification)), collapse = ", ")),
      "", "**Where the findings are weak or counterintuitive**",
      sprintf("- Compact bets rest on a node need inherited from each buyer's technology x stage cell (a proxy): %s.", if (nrow(bets)) pm_join(bets$node_label, 8) else "none"),
      if (nrow(better)) sprintf("- At %s the largest trusted supplier is outside the membership, so member-only rules understate the trusted supply that exists.", pm_join(pm_lc1(better$node_label), 6)) else character(),
      if (nrow(unlisted)) sprintf("- %s have material demand but no listed position at any compact node: trade data miss capability that is owned abroad or sold at home.", pm_join(unlisted$country, 8)) else character(),
      if (nrow(mf)) sprintf("- OSI reads %s as low strategic case, but the evidence layer records a capacity problem the annual trade data cannot see: %s %s Treat it as an emerging bottleneck to re-test.", mf$aib_node, pm_ev_tag(mf), pm_claim_clean(mf$claim)) else character(),
      sprintf("- Low-evidence nodes among the active states: %s.", { z <- na$node_label[na$low_evidence & na$relationship_state %in% PM_COMPACT_ACTIVE]; if (length(z)) paste(z, collapse = ", ") else "none" }),
      {nn <- if (is.null(node_notes)) character() else unlist(node_notes[intersect(names(node_notes), na$node_id[na$priority_tier %in% 1:2])])
       if (length(nn)) sprintf("- Node definition to read before citing (AIB mapping note), %s: %s", na$node_label[match(names(nn), na$node_id)], nn) else character()},
      if (!is.null(ccfg$steps)) "- This is a cross-cutting compact: each node also sits in its own AIB compact (source_compact in the tables), where the downstream steps are assessed. Read the two together." else character(),
      {bs <- cfg$trade_blind_spots; z <- intersect(names(bs), na$node_id)
       if (length(z)) sprintf("- Trade data understate concentration at %s: %s", pm_lc1(na$node_label[match(z, na$node_id)]), unlist(bs[z])) else character()},
      "- Membership is read from trade and demand data; a member's policy alignment, its other trade measures and its domestic politics are not in the rules.", "")

  # summary boxes
  box_build <- if (nrow(bets)) paste0(paste(sprintf("%s: %s", bets$step, bets$node_label), collapse = ". "), ".") else "No node passes the compact-bet rules."
  tsel <- function(types) if (nrow(trade_tbl)) unique(pm_lc1(trade_tbl$node_label[trade_tbl$action_type %in% types])) else character()
  box_trade <- paste(c(
    if (length(tsel(c("procurement_recognition", "predictability")))) sprintf("Recognise member supply of %s in procurement and content rules.", pm_join(tsel(c("procurement_recognition", "predictability")), 3)),
    if (length(tsel("anti_circumvention"))) sprintf("Common anti-circumvention rules for %s.", pm_join(tsel("anti_circumvention"), 3)),
    if (length(tsel("tariff_risk"))) sprintf("Bind market access among members for %s.", pm_join(tsel("tariff_risk"), 3)),
    if (length(tsel("open_sourcing"))) sprintf("Keep %s open to global sourcing.", pm_join(tsel("open_sourcing"), 3))), collapse = " ")
  if (!nzchar(box_trade)) box_trade <- "No compact trade preference passes the rules."
  box_gov <- switch(gov$option, existing = sprintf("Run the compact through %s; no new body.", gov$host_name),
                    upgrade = sprintf("Give %s a compact mandate: pipeline, reciprocal commitments, KPIs and an annual review. No new body.", gov$host_name),
                    network = "Link the members' bilateral frameworks through a light secretariat with one pipeline and one KPI dashboard.",
                    new_compact = "A new compact framework with a shared pipeline, KPIs and review is warranted.")
  box_out <- paste(vapply(utils::head(nonp$text, 3), function(t) sub("^(Do not [^:.;]*).*$", "\\1.", t), ""), collapse = " ")
  boxes <- pm_doc_boxes(box_build, box_trade, box_gov, if (nzchar(box_out)) box_out else "Nothing beyond the rules.")
  list(markdown = L, boxes = boxes, recommendations = pm_compact_recommendations(bets, trade_tbl, inv_tbl, third, deals, nonp, gov), compact_bets = bets$node_id)
}

pm_compact_recommendations <- function(bets, trade_tbl, inv_tbl, third, deals, nonp, gov) {
  r <- list()
  if (nrow(bets)) r$jb <- data.frame(rec_id = sprintf("J%02d", seq_len(nrow(bets))), rec_type = "compact_bet", unit_id = bets$node_id,
                                     text = sprintf("%s: %s via %s", bets$node_label, bets$state_label, bets$main_instrument), evidence_ids = bets$evidence_id_list, stringsAsFactors = FALSE)
  if (nrow(trade_tbl)) r$tr <- data.frame(rec_id = trade_tbl$rec_id, rec_type = "trade", unit_id = trade_tbl$node_id, text = trade_tbl$action, evidence_ids = "", stringsAsFactors = FALSE)
  if (nrow(inv_tbl)) r$iv <- data.frame(rec_id = inv_tbl$rec_id, rec_type = "investment", unit_id = inv_tbl$node_id, text = sprintf("Investment: %s (%s)", inv_tbl$node_label, inv_tbl$public_instrument), evidence_ids = inv_tbl$evidence_ids, stringsAsFactors = FALSE)
  if (nrow(third)) r$x <- data.frame(rec_id = third$rec_id, rec_type = "third_country", unit_id = NA_character_, text = sprintf("Joint third-country step: %s %s %s", third$recipient, third$technology, third$stage), evidence_ids = "", stringsAsFactors = FALSE)
  if (nrow(deals)) r$dl <- data.frame(rec_id = deals$rec_id, rec_type = "deal", unit_id = deals$node_id, text = deals$deal, evidence_ids = deals$evidence_ids, stringsAsFactors = FALSE)
  if (nrow(nonp)) r$np <- data.frame(rec_id = nonp$rec_id, rec_type = "non_priority", unit_id = nonp$node_id, text = nonp$text, evidence_ids = "", stringsAsFactors = FALSE)
  cm <- gov$mechanisms
  r$g <- data.frame(rec_id = "G01", rec_type = "governance", unit_id = NA_character_, text = sprintf("Governance: %s (%s)", gov$option, gov$host_mechanism),
                    evidence_ids = if (!is.null(cm) && nrow(cm)) paste(unique(unlist(strsplit(cm$evidence_ids, "\\|"))), collapse = ";") else "", stringsAsFactors = FALSE)
  out <- do.call(rbind, r); rownames(out) <- NULL
  out
}
