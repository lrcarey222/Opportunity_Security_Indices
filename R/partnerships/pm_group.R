# Partnership Memos: group memos (scripts/36_build_group_memo.R; docs/partnership_memos.md, "Group memos").
#
# A group memo asks the compact memo's question for a fixed group of economies (the Quad: the United States, Japan,
# India and Australia) across every electro-industrial node, and adds three things a compact memo does not carry:
#   - country priorities: what each member's own strategy engine asks for (Secure / Compete / Partner / Deploy, from
#     the six-stage strategic position table), and where those priorities name another member as the partner;
#   - AIB roles: what each member brings to the allied base (strategic roles) and to each compact (compact roles);
#   - the group's pairs: the bilateral engine's joint bets and framework verdict for every pair of members.
# The node reading is the compact engine's (pm_compact_assess) with the group as its members. Pure functions: no IO.

PM_GROUP_CHOICE_LABEL <- c(secure = "Secure", accelerate = "Compete (accelerate)", catch_up = "Compete (catch up)",
                           preserve = "Compete (preserve)", partner = "Partner", watch = "Watch")

PM_ROLE_BRINGS <- c(CBH = "coordination across allied chains", CIA = "capital and outward investment", DMA = "demand and offtake",
                    DSP = "sites for development and scale-up", RPA = "resources and processing", SMI = "manufacturing at scale",
                    STM = "specialised technology and manufacturing")

# The group's node frame: every node of the source compacts (all AIB compacts unless `ccfg$source_compacts` lists
# them), each keeping its source compact. A step is named "<compact>: <step>" and ordered by compact, then step;
# `value_chain_order` and `n_steps` stay those of the node's own compact (interdependency reads them).
pm_group_frame <- function(frame, id, ccfg, compact_names) {
  src <- if (is.null(ccfg$source_compacts)) sort(unique(stats::na.omit(frame$strategic_compact_id))) else unlist(ccfg$source_compacts)
  fr <- pm_compact_frame(frame, id, list(source_compacts = src))
  lab <- unname(compact_names[fr$source_compact]); lab[is.na(lab)] <- fr$source_compact[is.na(lab)]
  fr$value_chain_step <- sprintf("%s: %s", lab, fr$value_chain_step)
  fr$compact_order <- match(fr$source_compact, src)
  fr[order(fr$compact_order, pm_num(fr$value_chain_order), fr$node_id), , drop = FALSE]
}

# The group's members in the configured order, with the compact engine's relevance and presence counts.
pm_group_members <- function(frame_g, dol, croles, members, names, cfg) {
  m <- pm_compact_members(frame_g, dol, croles, members, names, cfg)
  m <- m[m$iso3 %in% members, , drop = FALSE]
  miss <- setdiff(members, m$iso3)
  if (length(miss)) m <- rbind(m, data.frame(iso3 = miss, country = pm_names_of(miss, names), relevance = 0, n_presence_nodes = 0L,
                                             n_buyer_nodes = 0L, compact_role = NA_character_, core = TRUE, stringsAsFactors = FALSE))
  m$core <- TRUE
  m <- m[match(members, m$iso3), , drop = FALSE]
  rownames(m) <- NULL
  m
}

# Each member's own priorities, one row per technology x stage cell with a decided choice (Deployment cells are
# deployment priorities: accelerate where deployment need is high). `group_partners`: the engine's recommended
# partners for the cell that are group members.
pm_group_priorities <- function(sp, members) {
  x <- sp[sp$iso3 %in% members & sp$strategic_choice %in% names(PM_GROUP_CHOICE_LABEL), , drop = FALSE]
  rp <- intersect(c("recommended_partner_1", "recommended_partner_2", "recommended_partner_3"), names(x))
  recs <- if (length(rp)) apply(x[, rp, drop = FALSE], 1, function(z) { z <- z[!is.na(z) & nzchar(z)]; paste(unique(z), collapse = ";") }) else rep("", nrow(x))
  num <- function(k) if (k %in% names(x)) suppressWarnings(as.numeric(x[[k]])) else rep(NA_real_, nrow(x))
  out <- data.frame(iso3 = x$iso3, tech = x$tech, stage = x$supply_chain, choice = x$strategic_choice,
                    choice_label = unname(PM_GROUP_CHOICE_LABEL[x$strategic_choice]),
                    deployment = x$supply_chain == "Deployment",
                    need = ifelse(x$supply_chain == "Deployment", num("deployment_need"), num("supply_chain_need")),
                    position = num("competitive_position_percentile"), value = num("strategic_industrial_value"),
                    partners = unname(recs), stringsAsFactors = FALSE)
  out$group_partners <- vapply(seq_len(nrow(out)), function(i) {
    z <- strsplit(out$partners[i], ";")[[1]]; paste(z[z %in% setdiff(members, out$iso3[i])], collapse = ";")
  }, character(1))
  out$cell <- sprintf("%s %s", out$tech, tolower(out$stage))
  out[order(match(out$iso3, members), match(out$choice, names(PM_GROUP_CHOICE_LABEL)), -ifelse(is.na(out$need), 0, out$need), out$tech), , drop = FALSE]
}

# Member x member: how many of A's Secure, Compete and Partner cells (producing stages) name B as a recommended partner.
pm_group_partner_matrix <- function(pri, members) {
  x <- pri[!pri$deployment & pri$choice %in% c("secure", "accelerate", "catch_up", "preserve", "partner"), , drop = FALSE]
  m <- matrix(0L, length(members), length(members), dimnames = list(members, members))
  for (i in seq_len(nrow(x))) for (b in strsplit(x$group_partners[i], ";")[[1]]) if (nzchar(b)) m[x$iso3[i], b] <- m[x$iso3[i], b] + 1L
  m
}

# AIB roles: each member's primary and secondary strategic roles, and its role (and lead node) in every compact.
pm_group_roles <- function(sroles, croles, members, compacts, node_labels) {
  sr <- sroles[sroles$iso3 %in% members & sroles$primary_or_secondary %in% c("primary", "secondary"), , drop = FALSE]
  sr <- sr[order(match(sr$iso3, members), sr$primary_or_secondary != "primary", -pm_num(sr$role_strength)), , drop = FALSE]
  strategic <- do.call(rbind, lapply(members, function(i) {
    r <- sr[sr$iso3 == i, , drop = FALSE]
    data.frame(iso3 = i,
               primary = if (any(r$primary_or_secondary == "primary")) paste(r$strategic_role[r$primary_or_secondary == "primary"], collapse = "; ") else "none",
               secondary = if (any(r$primary_or_secondary == "secondary")) paste(r$strategic_role[r$primary_or_secondary == "secondary"], collapse = "; ") else "none",
               codes = paste(r$role_code, collapse = ";"),
               brings = { v <- unique(stats::na.omit(PM_ROLE_BRINGS[r$role_code])); if (length(v)) paste(utils::head(v, 3), collapse = ", ") else "no AIB anchor role" },
               stringsAsFactors = FALSE)
  }))
  cm <- unique(compacts[, c("strategic_compact_id", "strategic_compact_name")])
  cm <- cm[order(cm$strategic_compact_id), , drop = FALSE]
  cr <- croles[croles$iso3 %in% members, , drop = FALSE]
  compact <- do.call(rbind, lapply(seq_len(nrow(cm)), function(k) {
    v <- vapply(members, function(i) {
      r <- cr[cr$strategic_compact_id == cm$strategic_compact_id[k] & cr$iso3 == i, , drop = FALSE]
      if (!nrow(r)) return("-")
      ln <- r$lead_node[1]; lab <- if (!is.na(ln) && nzchar(ln)) unname(node_labels[ln]) else NA_character_
      if (is.na(lab) && !is.na(ln) && nzchar(ln)) lab <- ln
      if (is.na(lab)) r$compact_role[1] else sprintf("%s (%s)", r$compact_role[1], pm_lc(lab))
    }, character(1))
    data.frame(strategic_compact_id = cm$strategic_compact_id[k], compact = cm$strategic_compact_name[k], t(v), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  list(strategic = strategic, compact = compact)
}

# The group's pairs from the bilateral engine's summaries (partnership_summary.json, scripts/34). `sums`: named list
# pair id -> summary (NULL where the pair has not been run); `labels`: unit id -> label.
pm_group_dyads <- function(pairs, sums, labels, compact_names, published = character()) {
  lab <- function(u) { v <- unname(labels[u]); ifelse(is.na(v), sub("\\|", " ", u), v) }
  do.call(rbind, lapply(names(pairs), function(k) {
    s <- sums[[k]]
    if (is.null(s)) return(data.frame(pair = k, a = pairs[[k]][1], b = pairs[[k]][2], n_joint_bets = NA_integer_, joint_bets = "not run",
                                      compacts = "", mea = "", published = k %in% published, stringsAsFactors = FALSE))
    # the count is every tier-1 unit of the pair; the list is its headline (one bet per compact)
    jb <- unlist(s$joint_bets); rc <- unlist(s$recommended_compacts)
    n_jb <- if (!is.null(s$tier_counts[["joint bet"]])) as.integer(s$tier_counts[["joint bet"]]) else length(jb)
    mea <- s$mea
    mtxt <- if (is.null(mea$option)) "" else switch(mea$option, none = "no framework (no compact passes)", existing = "use the existing mechanisms",
                                                    upgrade = sprintf("upgrade %s", pm_or(mea$host_mechanism, "a framework")), new_mea = "a new framework", mea$option)
    data.frame(pair = k, a = pairs[[k]][1], b = pairs[[k]][2], n_joint_bets = n_jb,
               joint_bets = if (length(jb)) paste(pm_lc(lab(jb)), collapse = "; ") else "none",
               compacts = if (length(rc)) paste(unname(ifelse(is.na(compact_names[rc]), rc, compact_names[rc])), collapse = "; ") else "none",
               mea = mtxt, published = k %in% published, stringsAsFactors = FALSE)
  }))
}

# Governance for a group: only the group's own bodies (all members are parties and the name matches `pattern`) can
# host; other plurilateral bodies are related channels, and the bilateral channels among members still count for
# the network option. The rule is otherwise the compact memo's (pm_compact_governance) with full member coverage.
#   bet_compacts: the compacts of the group bets; the host is the active political group body (plurilateral or
#   framework) that covers most of them (a cross_sector scope covers all), and the other group bodies that cover some
#   are its workstreams
pm_group_governance <- function(cm, members, cfg, pattern, bet_compacts = character()) {
  cfg2 <- cfg; cfg2$compact_memo$core_coverage_min <- 1
  if (is.null(cm) || !nrow(cm)) return(pm_compact_governance(cm, members, cfg2))
  pz <- strsplit(cm$pair, "\\|")
  whole <- vapply(pz, function(z) all(members %in% z), logical(1))
  cm$group_body <- whole & grepl(pattern, cm$name, ignore.case = TRUE)
  # a bilateral row counts only between two members; a bundle of one party's MOUs with non-members is not a group
  # channel, nor is a plurilateral body recorded against one pair (MSP, Pax Silica)
  among <- vapply(pz, function(z) all(z %in% members), logical(1))
  cm <- cm[cm$group_body | (cm$plurilateral & cm$n_member_parties >= 2) | (!cm$plurilateral & among & !cm$mechanism_type %in% "plurilateral"), , drop = FALSE]
  cover <- function(sc) { z <- strsplit(sc, "|", fixed = TRUE)[[1]]; if ("cross_sector" %in% z) length(bet_compacts) else sum(bet_compacts %in% z) }
  cm$bet_coverage <- vapply(cm$scope_compacts, cover, numeric(1))
  # the host is the group's political umbrella: where an active plurilateral or framework body of the group exists,
  # a finance channel (an export-credit MoC) is its finance lane, not the host; among equals the broader scope first
  hostcm <- cm[cm$group_body | !cm$plurilateral, , drop = FALSE]
  pol <- hostcm$group_body & hostcm$mechanism_type %in% c("plurilateral", "framework") & hostcm$status %in% PM_ACTIVE_MECH
  if (any(pol)) hostcm <- hostcm[!hostcm$group_body | (pol & hostcm$bet_coverage == max(hostcm$bet_coverage[pol])), , drop = FALSE]
  hostcm <- hostcm[order(-lengths(strsplit(hostcm$scope_compacts, "\\|"))), , drop = FALSE]
  g <- pm_compact_governance(hostcm, members, cfg2)
  fin <- cm[cm$group_body & cm$mechanism_type %in% "finance_cooperation" & cm$status %in% PM_ACTIVE_MECH & cm$has_finance %in% c("TRUE", "partial"), , drop = FALSE]
  g$finance_channel <- if (nrow(fin) && !identical(g$host_mechanism, fin$mechanism_id[1])) sprintf("%s (%s)", fin$name[1], fin$mechanism_id[1]) else NA_character_
  # workstreams: the other group bodies whose own scope (not cross_sector) covers some of the bets' compacts
  ws <- cm[cm$group_body & !cm$status %in% c("cancelled", "suspended") & !cm$mechanism_id %in% c(g$host_mechanism, fin$mechanism_id[1]) &
             vapply(strsplit(cm$scope_compacts, "|", fixed = TRUE), function(z) any(setdiff(z, "cross_sector") %in% bet_compacts), logical(1)), , drop = FALSE]
  g$workstreams <- if (nrow(ws)) sprintf("%s (%s, %s)", ws$name, ws$mechanism_id, ws$status) else character()
  g$gaps <- sub("no active plurilateral mechanism covers the compact", "no active group-wide mechanism", g$gaps, fixed = TRUE)
  # the anchor when no group body qualifies: the group body that is furthest along (active, finance, pipeline, scope)
  gb <- cm[cm$group_body & !cm$status %in% c("cancelled", "suspended"), , drop = FALSE]
  if (g$option %in% c("network", "new_compact") && nrow(gb)) {
    gb <- gb[order(!gb$status %in% PM_ACTIVE_MECH, !gb$has_finance %in% c("TRUE", "partial"), !gb$has_pipeline %in% c("TRUE", "partial"),
                   -lengths(strsplit(gb$scope_compacts, "\\|")), gb$mechanism_id), , drop = FALSE]
    g$candidate <- sprintf("%s (%s, %s)", gb$name[1], gb$mechanism_id[1], gb$status[1])
  }
  g$mechanisms <- cm[order(!cm$group_body, cm$plurilateral, !cm$status %in% PM_ACTIVE_MECH, cm$mechanism_id), , drop = FALSE]
  g
}

# Evidence naming at least two group members (the group's own record, for Section 1 and the evidence lists).
pm_group_evidence_ids <- function(ev, members, min_members = 2) {
  if (is.null(ev) || !nrow(ev)) return(character())
  ev$evidence_id[vapply(strsplit(ev$countries, "\\|"), function(z) sum(members %in% z) >= min_members, logical(1))]
}

# Supplier -> buyer member pairs by 2024 node trade inside the group, with the nodes that carry most of each lane.
pm_group_lane_pairs <- function(lanes, n_nodes = 3) {
  if (is.null(lanes) || !nrow(lanes)) return(data.frame())
  k <- paste(lanes$supplier, lanes$buyer)
  out <- do.call(rbind, lapply(split(lanes, k), function(x) {
    x <- x[order(-x$flow_usd), , drop = FALSE]
    data.frame(supplier = x$supplier[1], buyer = x$buyer[1], flow_usd = sum(x$flow_usd), n_nodes = nrow(x),
               nodes = paste(utils::head(x$node_label, n_nodes), collapse = ";"), stringsAsFactors = FALSE)
  }))
  out <- out[order(-out$flow_usd), , drop = FALSE]; rownames(out) <- NULL
  out
}
