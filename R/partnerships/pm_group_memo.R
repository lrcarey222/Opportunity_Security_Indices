# Partnership Memos: the group memo text (docs/partnership_memos.md, "Group memos"). The compact memo's node
# reading, tags and print format, read across a fixed group (the Quad), with three sections the compact memo does
# not have: each member's priorities, the members' AIB roles and the group's pairs. Pure functions.

#   na, prof, cells, members, lanes, trade_tbl, inv_tbl, third, deals, gov, nonp: the compact engine's tables for the group
#   ev_group: evidence ids naming at least two members; pri, pmat: pm_group_priorities() / pm_group_partner_matrix();
#   roles: pm_group_roles(); dyads: pm_group_dyads(); briefs: iso3 -> the country strategy brief's opening sentence
#   gcfg: config groups[[id]] (id, title, short, members, mechanism_pattern); names: iso3 -> display name
#   node_notes: node id -> AIB mapping note that is a caveat (PM_CAVEAT_RE, filtered by the script); a dagger and a note
#   only for the nodes the memo acts on (tiers 1-3). data_notes: node id -> config data caveat for a member, always shown.
#   full_names: iso3 -> full name (headings); gcfg$ref_names: iso3 -> prose reference ("the US")
#   friction: node id -> evidence ids of trade measures in force between members at the node (tariffs, trade remedies,
#   disputes), read from the whole evidence layer; a bet with friction says so
pm_group_memo_sections <- function(na, prof, cells, members, lanes, trade_tbl, inv_tbl, third, deals, gov, nonp, ev, ev_group,
                                   agreements, pri, pmat, roles, dyads, briefs, gcfg, cfg, names, institutions = NULL, node_notes = NULL,
                                   data_notes = NULL, full_names = names, friction = list()) {
  G <- gcfg$short; mem <- members$iso3
  nm1 <- function(iso) pm_names_of(iso, names)
  ref1 <- function(iso) { v <- unlist(gcfg$ref_names)[iso]; ifelse(is.na(v), nm1(iso), v) }
  nmlist <- function(s, n = 4) { if (is.na(s) || !nzchar(s)) return("none"); pm_join(nm1(strsplit(s, ";")[[1]]), n) }
  ucf <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  inst_of <- function(iso, fam) { if (is.null(institutions) || !nrow(institutions)) return(character()); institutions$abbreviation[institutions$iso3 %in% iso & grepl(fam, institutions$instrument_families)] }
  L <- character(); add <- function(...) L <<- c(L, ...)
  st <- table(factor(na$relationship_state, levels = PM_COMPACT_STATES))
  act <- na[na$relationship_state %in% PM_COMPACT_ACTIVE, , drop = FALSE]
  bets <- na[na$priority_tier == 1, , drop = FALSE]
  bets <- utils::head(bets[order(bets$priority_rank), , drop = FALSE], cfg$priority$max_joint_bets)
  # caveats: mapping notes for the nodes the memo acts on, plus every data caveat for a member
  acted <- na$node_id[na$priority_tier %in% 1:3]
  notes <- c(if (length(node_notes)) node_notes[intersect(names(node_notes), acted)], if (length(data_notes)) data_notes[intersect(names(data_notes), na$node_id)])
  if (length(notes)) notes <- vapply(split(unlist(notes), names(notes)), function(z) paste(sub("\\.?\\s*$", ".", z), collapse = " "), character(1))
  noted <- names(notes)
  dag <- function(id, lab) paste0(lab, ifelse(id %in% noted, " †", ""))
  gev <- ev[ev$evidence_id %in% ev_group, , drop = FALSE]
  ev_vintage <- if (nrow(ev)) max(ev$access_date) else NA
  gpz <- strsplit(gev$countries, "\\|")
  # the group's own record: items whose parties are all members (two or more); wider bodies that include members apart
  inside <- vapply(gpz, function(z) all(z %in% mem), logical(1))
  all4 <- vapply(gpz, function(z) all(mem %in% z), logical(1)) & (inside | grepl(gcfg$mechanism_pattern, gev$name, ignore.case = TRUE))
  shared <- na[na$relationship_state == "shared_vulnerability", , drop = FALSE]
  choke <- na[na$chokepoint, , drop = FALSE]
  ord_nodes <- function(x) x[order(x$compact_order, x$step_order, x$node_id), , drop = FALSE]

  # ---- Title ---------------------------------------------------------------------------------------------
  add(sprintf("# %s", gcfg$title), "",
      sprintf("_Group Memo, generated %s by scripts/36_build_group_memo.R. OSI data: six-stage strategic-position table (trade 2022-24 benchmark, flows 2024, NIPO policy as of 2025) and the Allied Industrial Base strategic layer (%s scenario). External evidence: %d curated items naming at least two members, accessed %s (verified %d, partial %d, unverified %d)._",
              format(Sys.Date()), cfg$inputs$aib_scenario, nrow(gev), ev_vintage, sum(gev$verification == "verified"), sum(gev$verification == "partial"), sum(gev$verification == "unverified")),
      "", sprintf("_Tags: **[Data]** OSI tables; **[External Exx, verification]** curated evidence layer; **[Inference]** a documented rule applied to the data; **[Recommendation]** what the %s governments should do. Members: %s. The node reading covers all %d electro-industrial nodes of the ten AIB compacts._",
                  G, pm_join(nm1(mem), 4), nrow(na)), "")

  # ---- Executive summary ---------------------------------------------------------------------------------
  lp <- pm_group_lane_pairs(lanes)
  rs <- roles$strategic
  pcount <- function(iso, ch) sum(pri$iso3 == iso & !pri$deployment & pri$choice %in% ch)
  top_cells <- function(iso, ch, n = 3) { x <- pri[pri$iso3 == iso & !pri$deployment & pri$choice %in% ch, , drop = FALSE]
    if (any(ch %in% c("accelerate", "catch_up", "preserve"))) x <- x[order(-ifelse(is.na(x$value), 0, x$value)), , drop = FALSE]
    if (!nrow(x)) "none" else pm_join(utils::head(x$cell, n), n) }
  # the group's latest record: group-wide items dated within 30 days of the evidence vintage, newest first
  recent <- gev[all4 & gev$verification != "unverified" & nzchar(gev$publication_date) &
                  as.Date(ifelse(nzchar(gev$publication_date), gev$publication_date, "1900-01-01")) >= as.Date(ev_vintage) - 30, , drop = FALSE]
  recent <- utils::head(recent[order(recent$publication_date, decreasing = TRUE), , drop = FALSE], 3)
  fr_txt <- function(id) { z <- friction[[id]]; if (is.null(z) || !length(z)) "-" else paste(z, collapse = ", ") }
  deploy_of <- function(iso, n = 4) { x <- pri[pri$iso3 == iso & pri$deployment & pri$choice == "accelerate", , drop = FALSE]; x <- x[order(-x$need), , drop = FALSE]; if (!nrow(x)) "none" else pm_join(tolower(utils::head(x$tech, n)), n) }
  add("## Executive Decision Summary", "",
      sprintf("**Why the %s matters.**", G),
      sprintf("- [Data] Of %d electro-industrial nodes across the AIB compacts, %d show an active %s relation (a member supplying another member's need, a capacity-constrained trusted supplier, or a chokepoint at least two members share with no member supplier), %d member competition and %d no %s case.",
              nrow(na), sum(na$relationship_state %in% PM_COMPACT_ACTIVE), G, st[["member_competition"]], st[["low_strategic_case"]], G),
      sprintf("- [Data] A coercive supplier holds at least %s of world supply at %d nodes; the AIB classes %d nodes BUILD (allied capacity short of demand under the China shock).",
              pm_pct(cfg$thresholds$chokepoint_share), nrow(choke), sum(na$aib_class %in% "BUILD")),
      sprintf("- [Inference] What each member brings (AIB strategic roles): %s.", paste(sprintf("%s %s", nm1(rs$iso3), rs$brings), collapse = "; ")),
      if (nrow(recent)) c("", sprintf("**The %s now** (the latest group-wide record, Section 1):", G), sprintf("- %s %s", pm_ev_tag(recent), pm_claim_clean(recent$claim))) else character(),
      "", "**What each member wants** (its own strategy engine, Section 2):",
      sprintf("- [Data] %s: Secure %d (%s); Compete %d (%s); Partner %d; deploys %s.", nm1(mem),
              vapply(mem, pcount, integer(1), ch = "secure"), vapply(mem, top_cells, character(1), ch = "secure"),
              vapply(mem, pcount, integer(1), ch = c("accelerate", "catch_up", "preserve")), vapply(mem, top_cells, character(1), ch = c("accelerate", "catch_up", "preserve")),
              vapply(mem, pcount, integer(1), ch = "partner"), vapply(mem, deploy_of, character(1))),
      "", sprintf("**The central %s bargain.**", G),
      if (nrow(lp)) sprintf("- [Inference] The largest supply lanes inside the %s (2024 trade at the active nodes, supplier to buyer): %s.", G,
                            paste(sprintf("%s to %s (%s; %s)", nm1(utils::head(lp$supplier, 5)), nm1(utils::head(lp$buyer, 5)), pm_usd(utils::head(lp$flow_usd, 5)),
                                          vapply(utils::head(lp$nodes, 5), function(s) pm_join(pm_lc(strsplit(s, ";")[[1]]), 3), character(1))), collapse = "; "))
      else "- [Inference] No member supplier outranks a member buyer at any node.",
      if (nrow(shared)) sprintf("- [Inference] At %s no member supplies the others: the answer is pooled demand, finance and third-country capacity.", pm_join(pm_lc(shared$node_label), 6))
      else "- [Inference] No node is a shared vulnerability that no member can supply.",
      "", sprintf("**The %d highest-priority %s bets** (tier 1 by the rules in Section 4):", nrow(bets), G), "")
  bt <- data.frame(Priority = seq_len(nrow(bets)), Step = bets$step, Node = dag(bets$node_id, bets$node_label), Problem = gsub("_", " ", bets$constraint_type),
                   Suppliers = vapply(ifelse(nzchar(bets$lane_suppliers), bets$lane_suppliers, NA), nmlist, character(1), USE.NAMES = FALSE),
                   Buyers = vapply(ifelse(nzchar(bets$lane_buyers), bets$lane_buyers, bets$buyers), nmlist, character(1), USE.NAMES = FALSE),
                   Instrument = gsub("_", " ", bets$main_instrument), Timing = pm_horizon(bets$execution_speed),
                   Friction = vapply(bets$node_id, fr_txt, character(1), USE.NAMES = FALSE), stringsAsFactors = FALSE)
  bt$Suppliers[bets$relationship_state == "shared_vulnerability"] <- "none among members (pooled)"
  frb <- bets[vapply(bets$node_id, function(n) length(friction[[n]]) > 0, logical(1)), , drop = FALSE]
  add(if (nrow(bt)) pm_md_table(bt, names(bt), c("Priority", "Compact: step", "Node", "Strategic problem", "Member suppliers", "Member buyers", "Main instrument", "Timing", "Member trade measures in force")) else sprintf("_No node passes the %s-bet rules; Section 5 lists the coordinate-tier nodes._", G), "",
      if (nrow(frb)) sprintf("- [Inference] At %s, members already restrict each other's trade (last column; Section 6). A pooled bet there needs the trade measure settled first, or it pays for capacity that one member's duties then shut out of another's market.", pm_join(pm_lc(frb$node_label), 4)) else character(),
      if (any(bets$node_id %in% noted)) "† The node's product definition or data has a caveat (Section 12); read it before citing." else character(), "")
  dy <- dyads[!is.na(dyads$n_joint_bets), , drop = FALSE]
  pname <- function(a, b) sprintf("%s–%s", nm1(a), nm1(b))
  add("**The pairs** (bilateral engine, Section 5):",
      if (nrow(dy)) sprintf("- [Data] %s: %d joint bet%s (headline, one per compact: %s); framework: %s.", pname(dy$a, dy$b), dy$n_joint_bets, ifelse(dy$n_joint_bets == 1, "", "s"),
                            vapply(dy$joint_bets, function(s) pm_join(strsplit(s, "; ")[[1]], 3), character(1)), dy$mea) else "- [Data] No pair has been run.", "")
  # one of each kind first, so three lines of the same kind do not crowd the summary
  np3 <- if (nrow(nonp)) utils::head(nonp$text[order(duplicated(nonp$kind), seq_len(nrow(nonp)))], 3) else character()
  add("**Do not prioritise** (Section 11):", sprintf("- [Recommendation] %s", np3), "")
  gov_txt <- switch(gov$option, existing = sprintf("use the existing %s mechanism", G), upgrade = sprintf("upgrade an existing %s mechanism (%s)", G, gov$host_mechanism),
                    network = "link the members' bilateral frameworks", new_compact = sprintf("a %s framework is warranted", G))
  fin_txt <- paste0(if (length(gov$workstreams)) sprintf(" Run the sector work through the %s bodies already set up for it: %s.", G, pm_join(gov$workstreams, 4)) else "",
                    if (!is.null(gov$finance_channel) && !is.na(gov$finance_channel)) sprintf(" Run its finance through %s.", gov$finance_channel) else "")
  gov_rec <- switch(gov$option,
    existing = sprintf("Run the %s agenda through %s; no new body.%s", G, gov$host_name, fin_txt),
    upgrade = sprintf("Do not create a new %s body. Give %s a mandate for the nodes in Section 5: a shared project pipeline, reciprocal supplier and buyer commitments and a KPI dashboard, reviewed every %d months.%s", G, gov$host_name, cfg$mea$review_cycle_months, fin_txt),
    network = sprintf("Do not build a new %s institution. Link the members' bilateral frameworks (%s) through a light %s working group with one project pipeline and one KPI dashboard%s.", G,
                      paste(utils::head(gov$bilateral_channels, 6), collapse = ", "), G, if (!is.na(gov$candidate)) sprintf(", anchored in %s", gov$candidate) else ""),
    new_compact = sprintf("Launch a %s framework with a shared pipeline, reciprocal commitments, KPIs and an annual review%s.", G,
                          if (!is.na(gov$candidate)) sprintf("; build it on the closest existing body, %s, rather than a new treaty", gov$candidate) else ""))
  add("**Existing mechanisms and the governance question** (Section 9):",
      sprintf("- [Inference] Governance assessment: **%s**.%s", gov_txt, if (length(gov$gaps)) paste0(" Gaps: ", paste(gov$gaps, collapse = "; "), ".") else ""),
      sprintf("- [Recommendation] %s", gov_rec), "")

  # ---- 1. Current state ------------------------------------------------------------------------------------
  ag <- agreements[agreements$iso3 %in% mem, , drop = FALSE]
  agl <- vapply(split(ag$iso3, ag$agreement), function(z) sprintf("%d of %d (%s)", length(unique(z)), length(mem), pm_join(nm1(sort(unique(z))), 4)), character(1))
  capd <- function(ids, cap) pm_ev_lines_capped(ev, ids, cap)
  more <- function(s) if (s$n_more > 0) sprintf("- [Data] %d further item%s in the evidence register (`group_evidence.csv`).", s$n_more, if (s$n_more > 1) "s are" else " is") else character()
  none_if <- function(s) if (!length(s$lines)) "- [Data] None in the evidence layer." else character()
  # group-wide items: the newest first, so the latest meeting is never capped away behind older verified items
  qd <- gev[all4 & gev$verification != "unverified", , drop = FALSE]
  qd <- utils::head(qd[order(qd$publication_date, decreasing = TRUE), , drop = FALSE], 12)
  s_q <- list(lines = if (nrow(qd)) { qd <- qd[order(qd$publication_date, qd$evidence_id), , drop = FALSE]; sprintf("- %s %s", pm_ev_tag(qd), pm_claim_clean(qd$claim)) } else character(),
              n_more = max(0L, sum(all4 & gev$verification != "unverified") - 12L))
  s_fw <- capd(gev$evidence_id[inside & !all4 & gev$item_type %in% c("framework", "agreement", "plurilateral", "dialogue", "institution") & !gev$instrument %in% c("tariff")], 12)
  s_tr <- capd(gev$evidence_id[inside & !all4 & (gev$instrument %in% c("tariff", "export_control") | gev$item_type == "dispute")], 8)
  s_pr <- capd(gev$evidence_id[inside & !all4 & gev$item_type %in% c("project", "financing", "corporate", "procurement")], 14)
  s_wd <- capd(gev$evidence_id[!inside & !all4 & gev$item_type %in% c("framework", "agreement", "plurilateral", "dialogue", "institution")], 6)
  cmx <- gov$mechanisms
  chx <- if (!is.null(cmx) && nrow(cmx)) cmx[cmx$status %in% PM_ACTIVE_MECH & !cmx$mechanism_type %in% "trade_policy", , drop = FALSE] else data.frame()
  add(sprintf("## 1. The %s Today", G), "",
      paste0("This section sets out what already exists before anything new is proposed. Facts are from the curated evidence layer (accessed ", ev_vintage, "); items marked partial rest on secondary sources and should be confirmed before citation; unverified items are left out here and stay in the evidence register."), "",
      sprintf("**%s-wide architecture**", G),
      if (length(agl)) sprintf("- [Data] Members in OSI's economic-security agreements table: %s.", paste(sprintf("%s %s", names(agl), agl), collapse = "; ")) else character(),
      s_q$lines, more(s_q), none_if(s_q), "",
      "**Frameworks between members**", s_fw$lines, more(s_fw), none_if(s_fw), "",
      "**Trade measures and frictions between members**", s_tr$lines, more(s_tr), none_if(s_tr), "",
      "**Projects, financing and corporate links between members**", s_pr$lines, more(s_pr), none_if(s_pr), "",
      "**Wider frameworks that include two or more members** (not group channels; context for what the group adds)", s_wd$lines, more(s_wd), none_if(s_wd), "",
      if (nrow(chx)) sprintf("- [Inference] Working government channels among members: %s.", paste(sprintf("%s (%s%s)", chx$name, ifelse(chx$plurilateral, sprintf("%d parties", chx$n_parties), gsub("\\|", "-", chx$pair)),
                                                                                                          ifelse(chx$has_finance %in% "TRUE", ", finance", ifelse(chx$has_finance %in% "partial", ", partial finance", ""))), collapse = "; "))
      else "- [Inference] The evidence layer records no signed or operating government mechanism among the members.", "")

  # ---- 2. Country priorities ---------------------------------------------------------------------------------
  add("## 2. Country Priorities", "",
      "[Data] Each member's own strategy engine (six-stage strategic position, scripts/26): **Secure** where a cutoff cannot be survived, **Compete** where the member should build or hold a position (accelerate, catch up or preserve), **Partner** where it should source from trusted partners, and Deployment cells where deployment need is high. The engine's recommended partners for each cell are named where they are other members. Strong at / needs are the member's electro-industrial node positions and needs (AIB division of labour).", "")
  lst <- function(x, n = 6, with_partner = FALSE) {
    if (!nrow(x)) return("none")
    more <- nrow(x) - n
    x <- utils::head(x, n)
    v <- if (with_partner) ifelse(nzchar(x$group_partners), sprintf("%s (%s)", x$cell, vapply(x$group_partners, function(s) pm_join(nm1(strsplit(s, ";")[[1]]), 3), character(1))), x$cell) else x$cell
    paste0(paste(v, collapse = "; "), if (more > 0) sprintf("; and %d other%s (country_priorities.csv)", more, if (more > 1) "s" else "") else "")
  }
  for (iso in mem) {
    p <- pri[pri$iso3 == iso & !pri$deployment, , drop = FALSE]
    sec <- p[p$choice == "secure", , drop = FALSE]; cmp <- p[p$choice %in% c("accelerate", "catch_up", "preserve"), , drop = FALSE]; par <- p[p$choice == "partner", , drop = FALSE]
    cmp <- cmp[order(-ifelse(is.na(cmp$value), 0, cmp$value)), , drop = FALSE]
    np <- prof[prof$iso3 == iso, , drop = FALSE]
    strong <- np[np$position_band %in% "strong", , drop = FALSE]; strong <- strong[order(-strong$world_share), , drop = FALSE]
    needs <- np[np$need_band %in% c("high", "demand"), , drop = FALSE]; needs <- needs[order(needs$need_band != "high", -ifelse(is.na(needs$demand_share), 0, needs$demand_share)), , drop = FALSE]
    add(sprintf("### %s", ucf(unname(ifelse(is.na(full_names[iso]), nm1(iso), full_names[iso])))),
        if (!is.null(briefs[[iso]]) && nzchar(briefs[[iso]])) sprintf("- [Data] Country strategy brief: %s", briefs[[iso]]) else character(),
        sprintf("- [Data] **Secure** (%d): %s.", nrow(sec), lst(sec, 6, TRUE)),
        sprintf("- [Data] **Compete** (%d: accelerate %d, catch up %d, preserve %d): %s.", nrow(cmp), sum(cmp$choice == "accelerate"), sum(cmp$choice == "catch_up"), sum(cmp$choice == "preserve"),
                lst(transform(cmp, cell = sprintf("%s (%s)", cmp$cell, sub("^Compete \\((.*)\\)$", "\\1", cmp$choice_label))), 6)),
        sprintf("- [Data] **Partner** (%d): %s.", nrow(par), lst(par, 8, TRUE)),
        sprintf("- [Data] **Deploy** (high deployment need): %s.", deploy_of(iso, 6)),
        sprintf("- [Data] Strong at (share of world exports or production, the larger; a coercive supplier's share elsewhere in this memo is its production share): %s.", if (nrow(strong)) paste(sprintf("%s %s", dag(strong$node_id, pm_lc(strong$node_label)), pm_pct(strong$world_share)), collapse = ", ") else "no node above the strong line"),
        sprintf("- [Data] Needs (high need or material demand): %s.", if (nrow(needs)) paste(sprintf("%s (%s)", pm_lc(utils::head(needs$node_label, 8)), utils::head(needs$need_band, 8)), collapse = ", ") else "none"), "")
  }
  pm_df <- data.frame(Member = nm1(rownames(pmat)), stringsAsFactors = FALSE)
  for (b in colnames(pmat)) pm_df[[nm1(b)]] <- ifelse(rownames(pmat) == b, "-", as.character(pmat[, b]))
  flows_txt <- { z <- which(pmat > 0, arr.ind = TRUE); if (!nrow(z)) character() else { o <- order(-pmat[z]); z <- z[o, , drop = FALSE]
    sprintf("%s's priorities name %s %d time%s", ucf(ref1(rownames(pmat)[z[, 1]])), ref1(colnames(pmat)[z[, 2]]), pmat[z], ifelse(pmat[z] == 1, "", "s")) } }
  add("**Where one member's priorities name another as the partner** (count of the row member's Secure, Compete and Partner cells whose recommended partners include the column member):", "",
      pm_md_table(pm_df, names(pm_df)), "",
      if (length(flows_txt)) sprintf("- [Inference] %s. A member that appears often in another's list is the trusted source the other's own engine already looks to; a blank pair is a relationship the data do not yet support.", paste(utils::head(flows_txt, 6), collapse = "; ")) else
        "- [Inference] No member's priorities name another member as a recommended partner.", "")

  # ---- 3. AIB roles ----------------------------------------------------------------------------------------
  rc <- roles$compact
  has <- function(code) rs$iso3[vapply(strsplit(rs$codes, ";"), function(z) code %in% z, logical(1))]
  chain <- c(RPA = "resources and processing", STM = "specialised technology", SMI = "manufacturing at scale", DSP = "development and scale-up hosting",
             DMA = "demand and offtake", CIA = "capital", CBH = "coordination across chains")
  dl <- vapply(names(chain), function(k) { h <- has(k); if (length(h)) sprintf("%s from %s", chain[[k]], pm_join(nm1(h), 4)) else "" }, character(1))
  add("## 3. AIB Roles", "",
      "**Strategic roles** (AIB strategic layer: a primary role clears both gates, a secondary one clears one):", "",
      pm_md_table(data.frame(Member = nm1(rs$iso3), Primary = rs$primary, Secondary = rs$secondary, Brings = rs$brings, stringsAsFactors = FALSE),
                  c("Member", "Primary", "Secondary", "Brings"), c("Member", "Primary roles", "Secondary roles", "What it brings")), "",
      sprintf("- [Inference] The %s's division of labour by role: %s.", G, paste(dl[nzchar(dl)], collapse = "; ")), "",
      "**Compact roles** (role and lead node in each AIB compact; - = no role):", "",
      pm_md_table(rc, c("compact", mem), c("Compact", nm1(mem))), "",
      {nr <- vapply(mem, function(i) sum(rc[[i]] != "-"), integer(1))
       sprintf("- [Data] Compact roles held: %s.", paste(sprintf("%s %d of %d", nm1(mem), nr, nrow(rc)), collapse = "; "))}, "")

  # ---- 4. Map ----------------------------------------------------------------------------------------------
  cmap <- na[na$relationship_state %in% c(PM_COMPACT_ACTIVE, "member_competition"), , drop = FALSE]
  ct <- data.frame(Rank = cmap$priority_rank, Tier = sub("compact bet", sprintf("%s bet", G), cmap$priority_label), Step = cmap$step, Node = dag(cmap$node_id, cmap$node_label), State = cmap$state_label,
                   Suppliers = vapply(ifelse(cmap$relationship_state == "member_competition", cmap$strong_members, ifelse(nzchar(cmap$lane_suppliers), cmap$lane_suppliers, NA)), nmlist, character(1), USE.NAMES = FALSE),
                   Buyers = vapply(ifelse(nzchar(cmap$lane_buyers), cmap$lane_buyers, ifelse(nzchar(cmap$buyers), cmap$buyers, NA)), nmlist, character(1), USE.NAMES = FALSE),
                   Case = sprintf("%s; urgency %s, importance %s, additionality %s", gsub("_", " ", cmap$constraint_type), pm_f2(cmap$urgency), pm_f2(cmap$strategic_importance), cmap$additionality_label),
                   Confidence = pm_band(cmap$evidence_quality), stringsAsFactors = FALSE)
  shown <- ord_nodes(na[vapply(na$node_id, function(n) { x <- prof[prof$node_id == n, , drop = FALSE]; any(x$position_band %in% c("strong", "moderate") | x$need_band %in% c("high", "demand")) }, logical(1)), , drop = FALSE])
  mat <- do.call(rbind, lapply(seq_len(nrow(shown)), function(i) {
    x <- prof[prof$node_id == shown$node_id[i], , drop = FALSE]
    v <- vapply(mem, function(iso) { r <- x[x$iso3 == iso, , drop = FALSE]; if (!nrow(r)) "-/-" else pm_member_code(r$position_band[1], r$need_band[1]) }, character(1))
    data.frame(Step = shown$step[i], Node = dag(shown$node_id[i], shown$node_label[i]), t(v), AIB = shown$aib_class[i],
               Coercive = if (is.na(shown$coercive_top_iso3[i]) || !nzchar(shown$coercive_top_iso3[i])) "-" else sprintf("%s %s", shown$coercive_top_iso3[i], pm_pct(shown$coercive_world_share[i])),
               stringsAsFactors = FALSE, check.names = FALSE)
  }))
  low <- na[na$relationship_state == "low_strategic_case" & !na$node_id %in% names(cfg$trade_blind_spots), , drop = FALSE]
  add(sprintf("## 4. Where the %s Members Fit Together", G), "",
      sprintf("[Data] State counts over the %d nodes: %s.", nrow(na), paste(sprintf("%s %d", unname(PM_COMPACT_STATE_LABEL[names(st)[st > 0]]), st[st > 0]), collapse = "; ")), "",
      sprintf("[Inference] Rules (docs/partnership_memos.md, Compact memos, with the %s as the members): a node is **member supply** where a capable member supplier outranks a member buyer (high need, or material demand where trusted capacity is short or a chokepoint exists), **capacity-constrained** where the AIB also finds allied capacity short of demand, **shared vulnerability** where at least two members need the node, none supplies it and a coercive supplier holds at least half of world supply (or the buyers import heavily from coercive suppliers), and **member competition** where two or more members are strong and no other member needs more. A high-need net importer is never a supplier. Strength alone is never complementarity.", G), "",
      pm_md_table(ct, names(ct), c("Rank", "Tier", "Compact: step", "Node", "Relationship", "Member suppliers", "Member buyers", "Strategic case", "Confidence")), "",
      sprintf("**Position / need by member** at the %d nodes where any member has a position or a need: S strong, M present (>= %s or a ranked host), W weak; need H high, D material demand, L low (node need is inherited from the member's technology x stage cell, so a proxy).", nrow(shown), pm_pct(cfg$thresholds$node$presence_share)), "",
      pm_md_table(mat, names(mat), c("Compact: step", "Node", nm1(mem), "AIB class", "Coercive top supplier")), "",
      if (length(noted)) sprintf("- [Data] † marks a node whose product definition or data has a caveat (%d nodes); the notes are in Section 12.", length(noted)) else character(),
      sprintf("- [Data] Nodes with no %s case (low priority for the group; not necessarily open-market): %d%s.", G, nrow(low),
              { nb <- sum(na$relationship_state == "low_strategic_case") - nrow(low); if (nb > 0) sprintf(", plus %d trade blind spot%s that trade data cannot read (Section 12)", nb, if (nb > 1) "s" else "") else "" }), "")
  lt <- if (nrow(lanes)) utils::head(lanes, 16) else lanes
  add(sprintf("**Supplier-buyer lanes inside the %s** (2024 node trade; share = the supplier's share of the buyer's imports of the node):", G), "",
      if (nrow(lt)) pm_md_table(data.frame(Node = lt$node_label, Supplier = nm1(lt$supplier), Buyer = nm1(lt$buyer), Flow = pm_usd(lt$flow_usd), Share = pm_pct(lt$share_of_buyer_imports),
                                           Need = lt$buyer_need, Demand = pm_pct(lt$buyer_demand_share), stringsAsFactors = FALSE),
                                c("Node", "Supplier", "Buyer", "Flow", "Share", "Need", "Demand"),
                                c("Node", "Supplier", "Buyer", "Flow 2024", "Share of buyer's imports", "Buyer need", "Buyer share of allied demand")) else "_No member supplier outranks a member buyer._", "")

  # ---- 5. Partnership opportunities ----------------------------------------------------------------------------
  # nodes that take three or four members: a supplier serving two or more member buyers, two or more member suppliers,
  # or a vulnerability three or more members share
  span <- function(r) length(unique(c(strsplit(r$lane_suppliers, ";")[[1]], strsplit(ifelse(nzchar(r$lane_buyers), r$lane_buyers, r$buyers), ";")[[1]])))
  wide <- if (nrow(act)) act[vapply(seq_len(nrow(act)), function(i) span(act[i, ]) >= 3, logical(1)), , drop = FALSE] else act
  wide <- wide[order(wide$priority_rank), , drop = FALSE]
  coord <- na[na$priority_tier == 2, , drop = FALSE]; coord <- coord[order(coord$priority_rank), , drop = FALSE]
  dt <- dyads
  add("## 5. Partnership Opportunities", "",
      sprintf("**Where the %s adds to its pairs** (active nodes that take three or four members, so a pair cannot settle them alone):", G),
      if (nrow(wide)) sprintf("- [Inference] %s: %s.", dag(wide$node_id, wide$node_label),
                              ifelse(wide$relationship_state == "shared_vulnerability",
                                     sprintf("%s all need it, none supplies it; pool offtake and finance for non-coercive supply%s", vapply(wide$buyers, nmlist, character(1)),
                                             ifelse(nzchar(wide$aib_third_hosts), sprintf(" (AIB-ranked hosts: %s)", vapply(wide$aib_third_hosts, nmlist, character(1))), "")),
                                     sprintf("%s can supply %s (%s)", vapply(wide$lane_suppliers, nmlist, character(1)), vapply(ifelse(nzchar(wide$lane_buyers), wide$lane_buyers, wide$buyers), nmlist, character(1)), tolower(wide$state_label))))
      else sprintf("- [Inference] No active node takes three or more members: the %s's value is in coordinating its pairs, not in group-wide deals.", G), "",
      sprintf("**Coordinate-tier nodes** (active, but below the bet line on importance, evidence or additionality): %s.", if (nrow(coord)) pm_join(pm_lc(coord$node_label), 12) else "none"), "",
      if (any(vapply(friction, length, integer(1)) > 0)) {
        fz <- na[na$node_id %in% names(friction)[vapply(friction, length, integer(1)) > 0] & na$priority_tier %in% 1:2, , drop = FALSE]
        if (nrow(fz)) c(sprintf("**Where members already restrict each other's trade** (in-force tariffs, trade remedies or disputes at a %s node; settle them before pooling money):", G),
                        sprintf("- [External] %s: %s.", dag(fz$node_id, fz$node_label), vapply(fz$node_id, function(n) paste(pm_ev_tag(ev[ev$evidence_id %in% friction[[n]], , drop = FALSE]), collapse = "; "), character(1))), "") else character()
      } else character(),
      sprintf("**The %d pairs** (bilateral engine, scripts/34: joint bets are the pair's tier-1 units; the list shows its headline bets, one per compact; the framework verdict is the pair's MEA test against its bilateral mechanisms):", nrow(dt)), "",
      pm_md_table(data.frame(Pair = pname(dt$a, dt$b), Bets = ifelse(is.na(dt$n_joint_bets), "-", dt$n_joint_bets), Joint = dt$joint_bets, Compacts = dt$compacts, Framework = ifelse(nzchar(dt$mea), dt$mea, "-"),
                             Memo = ifelse(dt$published, "published", "tables only"), stringsAsFactors = FALSE),
                  c("Pair", "Bets", "Joint", "Compacts", "Framework", "Memo"), c("Pair", "Joint bets", "Headline bets (one per compact)", "Recommended compacts", "Framework verdict", "Pair memo")), "",
      {z <- dt[!is.na(dt$n_joint_bets), , drop = FALSE]; z <- z[order(-z$n_joint_bets), , drop = FALSE]
       if (nrow(z)) sprintf("- [Inference] The pairs with the most joint bets are %s; %s.", pm_join(sprintf("%s (%d)", pname(z$a, z$b), z$n_joint_bets)[seq_len(min(3, nrow(z)))], 3),
                            {w <- z[z$n_joint_bets == 0, , drop = FALSE]; if (nrow(w)) sprintf("%s %s no joint bet: their value runs through the group or through third countries", pm_join(pname(w$a, w$b), 3), if (nrow(w) == 1) "has" else "have") else "every pair has at least one joint bet"}) else character()},
      if (any(!dt$published & !is.na(dt$n_joint_bets))) sprintf("- [Data] %s have no published pair memo: their verdicts rest on the OSI tables and the evidence layer only.", pm_join(pname(dt$a[!dt$published & !is.na(dt$n_joint_bets)], dt$b[!dt$published & !is.na(dt$n_joint_bets)]), 3)) else character(), "")

  # ---- 6. Trade --------------------------------------------------------------------------------------------
  q <- function(type) if (nrow(trade_tbl)) trade_tbl[trade_tbl$action_type %in% type, , drop = FALSE] else trade_tbl
  add("## 6. Trade Agenda", "",
      sprintf("- [Inference] **Where tariffs among members would undermine the %s:** %s.", G, if (nrow(q("tariff_risk"))) pm_join(pm_lc(q("tariff_risk")$node_label), 6) else "no node where a buyer's protection priority is in its top third against a member supplier"),
      sprintf("- [Inference] **Procurement and content rules to recognise across members:** %s.", if (nrow(q("procurement_recognition"))) pm_join(pm_lc(q("procurement_recognition")$node_label), 8) else "none"),
      sprintf("- [Inference] **Where common origin and anti-circumvention rules are genuinely required:** %s; elsewhere they would add cost without security.", if (nrow(q("anti_circumvention"))) pm_join(pm_lc(q("anti_circumvention")$node_label), 6) else "none"),
      sprintf("- [Inference] **Where unrestricted global sourcing is acceptable:** %s.", if (nrow(q("open_sourcing"))) pm_join(pm_lc(q("open_sourcing")$node_label), 6) else "none identified"), "",
      if (nrow(trade_tbl)) pm_md_table(trade_tbl, c("rec_id", "node_label", "direction", "action_type", "action"), c("ID", "Node", "Direction", "Action type", "Action")) else "_No trade action passes the rules._", "")
  tf <- pm_ev_lines_capped(ev, gev$evidence_id[gev$instrument %in% c("tariff", "export_control")], 8)
  if (length(tf$lines)) add("- [External] Tariffs and trade measures in force or in dispute between members, the baseline any group trade rule works from:", tf$lines, "")

  # ---- 7. Investment ---------------------------------------------------------------------------------------
  add("## 7. Investment Agenda", "",
      if (nrow(inv_tbl)) pm_md_table(transform(inv_tbl, public_instrument = gsub("_", " ", inv_tbl$public_instrument)),
                                     c("rec_id", "node_label", "capability_owner", "destination", "investment_logic", "demand_case", "private_actors", "public_instrument", "additionality_label", "key_obstacle", "time_horizon"),
                                     c("ID", "Node", "Capital / capability owner", "Destination (AIB-ranked hosts)", "Logic", "Demand case", "Private actors (evidence)", "Public instrument", "Additionality", "Key obstacle", "Horizon")) else "_No node passes the investment rules._", "",
      "- [Inference] Additionality = mean of three named components: the market failure (chokepoint 1, capacity constraint 0.75, gap or emerging bottleneck 0.5, none 0), one minus existing commitments in the evidence layer (a committed project at the node is partial coverage, 0.5; an announced one 0.25) and one minus a working market (member buyers already take 30% or more of their imports of the node from member suppliers). It is **low** where there is no failure, or where the market already works and projects are committed.", "",
      sprintf("- [Data] Public finance on each side: %s.", paste(sprintf("%s %s", nm1(mem), vapply(mem, function(i) { z <- inst_of(i, "finance"); if (length(z)) paste(z, collapse = ", ") else "none listed" }, character(1))), collapse = "; ")), "")

  # ---- 8. Third-country ------------------------------------------------------------------------------------
  add("## 8. Third-Country and Development Cooperation", "")
  if (nrow(third)) {
    add(sprintf("- [Data] From the development-partnership pairs (scripts/93: a donor's need meets a developing economy's credible Catch Up pathway in the same step), at least two members are donors at %d recipient step%s in the group's cells. Ranked: number of donors with their own need, then number of member donors.", nrow(third), if (nrow(third) == 1) "" else "s"), "",
        pm_md_table(transform(third, recipient_choice = gsub("_", " ", third$recipient_choice),
                              recipient_anchor = gsub(";", ", ", gsub("_", " ", ifelse(is.na(third$recipient_anchor), "", third$recipient_anchor)))),
                    c("rec_id", "recipient", "technology", "stage", "donor_names", "recipient_choice", "recipient_anchor", "tools", "evidence_quality"),
                    c("ID", "Recipient", "Technology", "Stage", "Member donors", "Recipient choice", "Recipient anchor", "Tools", "Evidence")), "")
  } else add("_No recipient step has two or more member donors (or no development-partnership table was supplied)._", "")
  hosts_out <- unique(unlist(strsplit(paste(na$aib_third_hosts[na$relationship_state %in% PM_COMPACT_ACTIVE], collapse = ";"), ";"))); hosts_out <- setdiff(hosts_out[nzchar(hosts_out)], mem)
  add(sprintf("- [Data] Third-country hosts the AIB ranks for new capacity at the active nodes: %s.", if (length(hosts_out)) pm_join(nm1(hosts_out), 10) else "none outside the group"), "")
  nonmem <- gev[vapply(strsplit(gev$countries, "\\|"), function(z) any(!z %in% mem), logical(1)) & gev$item_type %in% c("project", "financing"), , drop = FALSE]
  tc <- pm_ev_lines_capped(ev, nonmem$evidence_id, 8)
  if (length(tc$lines)) add("- [External] Projects and finance that already join two or more members with third countries:", tc$lines, "")

  # ---- 9. Governance ---------------------------------------------------------------------------------------
  cm <- gov$mechanisms
  add("## 9. Governance Assessment", "",
      sprintf("- [Inference] Rule (the compact memo's, with every member required): **existing** if an active %s-wide mechanism covering the nodes has a project pipeline, finance, KPIs and a review cycle; **upgrade** if an active %s-wide mechanism has finance, a pipeline or a framework mandate; **network** if no such host exists but at least two active bilateral mechanisms among members carry finance or a pipeline; **new framework** otherwise. Other plurilateral bodies that include members are listed as related channels, not as hosts.", G, G),
      sprintf("- [Inference] Result: **%s**%s.", gov$option, if (!is.na(gov$host_mechanism)) sprintf(" (host: %s, %s)", gov$host_name, gov$host_features) else ""), "",
      if (!is.null(cm) && nrow(cm)) pm_md_table(data.frame(ID = cm$mechanism_id, Mechanism = cm$name, Parties = ifelse(cm$plurilateral, sprintf("%d (%d members)", cm$n_parties, cm$n_member_parties), gsub("\\|", "-", cm$pair)),
                                                        Kind = ifelse(cm$group_body %in% TRUE, sprintf("%s body", G), ifelse(cm$plurilateral, "related", "bilateral")),
                                                        Status = cm$status, Pipeline = cm$has_pipeline, Finance = cm$has_finance, KPIs = cm$has_kpis, Review = cm$has_review, stringsAsFactors = FALSE),
                                             c("ID", "Mechanism", "Parties", "Kind", "Status", "Pipeline", "Finance", "KPIs", "Review")) else "_No government mechanism among the members is in the evidence layer._", "",
      sprintf("- [Recommendation] %s", gov_rec),
      "- **Scope:** the nodes in Section 5 (the group bets and the nodes that take three or four members); everything else stays with the pairs or the compacts.",
      "- **Reciprocal commitments:** supplier members commit capacity and priority supply at the capacity-constrained nodes; buyer members commit offtake, procurement recognition and no new tariffs on member suppliers of those nodes; all members commit finance for the shared-vulnerability nodes.",
      sprintf("- **KPI dashboard:** per node, the coercive share of member buyers' supply, member buyers' imports from member suppliers, offtake signed, capacity at FID and operating, and public money committed vs private; reviewed every %d months with the OSI tables re-run.", cfg$mea$review_cycle_months),
      "- **Sunset / exit:** each node leaves the agenda when its chokepoint or capacity case ends (Section 4 rules); the mandate lapses after five years unless renewed on evidence.", "")

  # ---- 10. Deals -------------------------------------------------------------------------------------------
  add("## 10. 12–36 Month Deal Pipeline", "",
      "[Inference] Ranked lexicographically on separate fields (importance, then additionality, urgency, tractability, execution speed, value-chain interdependency, evidence quality); urgency is not importance. At most two deals per compact step.", "",
      if (nrow(deals)) pm_md_table(transform(deals, constraint_type = gsub("_", " ", deals$constraint_type), instrument = gsub("_", " ", deals$instrument)),
                                   c("rank", "deal", "step", "constraint_type", "instrument", "responsible_institutions", "private_actors", "scale", "milestone_12m", "importance", "urgency", "additionality", "confidence"),
                                   c("Rank", "Deal / action", "Compact: step", "Problem solved", "Instrument", "Responsible institutions", "Private actors (evidence)", "Capital / demand scale", "12m milestone", "Importance", "Urgency", "Additionality", "Confidence")) else "_No deal passes the rules._", "")

  # ---- 11. Not to do ---------------------------------------------------------------------------------------
  add("## 11. What Not to Do", "", if (nrow(nonp)) sprintf("- [Recommendation] **%s** %s", nonp$rec_id, nonp$text) else "- [Recommendation] Nothing beyond the rules above.",
      sprintf("- [Recommendation] Do not turn the %s into a fifth layer over the pairs: where a pair already carries a node (Section 5), the group adds a shared dashboard, not a new negotiation.", G), "")

  # ---- 12. Evidence ----------------------------------------------------------------------------------------
  better <- na[!is.na(na$largest_trusted_supplier) & !na$largest_trusted_supplier %in% mem & na$aib_class %in% c("BUILD", "COORDINATE") & na$relationship_state %in% PM_COMPACT_ACTIVE, , drop = FALSE]
  add("## 12. Evidence Register and Limits", "",
      "- [Data] Every recommendation (IDs J, T, I, D, N, G; the third-country steps X rest on the development-partnership table) has rows in `evidence_register.csv`: the OSI metrics behind it and any external items, with URL and access date. The pairs' tables are in output/partnerships/<pair>/.",
      sprintf("- [Data] External evidence vintage: accessed %s; %d items name at least two members (%s).", ev_vintage, nrow(gev), paste(sprintf("%s %d", names(table(gev$verification)), table(gev$verification)), collapse = ", ")),
      "", "**Where the findings are weak or counterintuitive**",
      sprintf("- Group bets rest on a node need inherited from each buyer's technology x stage cell (a proxy): %s.", if (nrow(bets)) pm_join(bets$node_label, 8) else "none"),
      if (nrow(better)) sprintf("- At %s the largest trusted supplier is outside the group (%s), so group-only rules understate the trusted supply that exists; buy from it or bring it in, do not try to replace it.",
                                pm_join(pm_lc(better$node_label), 6), pm_join(nm1(unique(better$largest_trusted_supplier)), 6)) else character(),
      "- Country priorities come from each member's own engine and are not negotiated positions; the partner counts in Section 2 read the engine's recommended partners, which favour trusted, capable suppliers and do not weigh politics.",
      if (length(noted)) sprintf("- † %s: %s", na$node_label[match(noted, na$node_id)], unname(notes[noted])) else character(),
      {bs <- cfg$trade_blind_spots; z <- intersect(names(bs), na$node_id)
       if (length(z)) sprintf("- Trade data understate concentration at %s: %s", pm_lc(na$node_label[match(z, na$node_id)]), unlist(bs[z])) else character()},
      "- The node reading uses only evidence items whose parties are all members (a member's own project counts as partial coverage); an item that also names a non-member, such as a non-member firm's plant in a member, is left to that relationship's memo.",
      "- Trade data miss capability owned abroad or sold at home (a member's firms' plants in another member, a large domestic market), so a member can look weaker in the node tables than its firms are.", "")

  # summary boxes
  box_build <- if (nrow(bets)) paste0(paste(sprintf("%s: %s", bets$step, bets$node_label), collapse = ". "), ".") else sprintf("No node passes the %s-bet rules; see the coordinate-tier nodes.", G)
  tsel <- function(types) if (nrow(trade_tbl)) unique(pm_lc(trade_tbl$node_label[trade_tbl$action_type %in% types])) else character()
  box_trade <- paste(c(
    if (length(tsel(c("procurement_recognition", "predictability")))) sprintf("Recognise member supply of %s in procurement and content rules.", pm_join(tsel(c("procurement_recognition", "predictability")), 3)),
    if (length(tsel("anti_circumvention"))) sprintf("Common anti-circumvention rules for %s.", pm_join(tsel("anti_circumvention"), 3)),
    if (length(tsel("tariff_risk"))) sprintf("Keep market access open among members for %s.", pm_join(tsel("tariff_risk"), 3)),
    if (length(tsel("open_sourcing"))) sprintf("Keep %s open to global sourcing.", pm_join(tsel("open_sourcing"), 3))), collapse = " ")
  if (!nzchar(box_trade)) box_trade <- sprintf("No %s trade preference passes the rules.", G)
  box_out <- paste(vapply(np3, function(t) sub("^(Do not [^:.;]*).*$", "\\1.", t), ""), collapse = " ")
  boxes <- pm_doc_boxes(box_build, box_trade, gov_rec, if (nzchar(box_out)) box_out else "Nothing beyond the rules.")
  list(markdown = L, boxes = boxes, recommendations = pm_compact_recommendations(bets, trade_tbl, inv_tbl, third, deals, nonp, gov), group_bets = bets$node_id)
}
