# Development partnerships: the development arm of the Allied Industrial Base, at donor x recipient x
# technology x supply-chain step (docs/development_partnerships.md; rules in
# config/development_partnerships.yml). A pair needs evidence from both sides:
#
#   donor      a high-income economy in the AIB demand scope whose step has a real strategic reason for
#              more allied capacity: Partner; Secure with allied or trusted supply in its delivery; a
#              High-fence step with few trusted alternatives; high supply-chain need; allied supply that
#              falls short (posture detail partner_aib_status "gap"); or a BUILD node of the AIB in the
#              step. A choice with none of these (Watch alone) is no reason.
#   recipient  a non-high-income economy whose same step is Catch Up with a credible pathway (the engine's
#              capability anchor plus corroboration from another evidence family), corroborated by more
#              than context-only signals (else a thin anchor), with sufficient evidence and confidence, no
#              review flag or known data artifact, and not screened out by the AIB host screen.
#   route      development (a Catch Up the recipient cannot yet finance commercially), commercial (the
#              recipient already hosts the step's BUILD capacity in the AIB, or Preserves the step: trade,
#              offtake, investment and export finance rather than grants) or review (the donor's call is
#              of low confidence or a known data artifact).
#
# The relationship evidence (OECD CRS aid by step, OCI export and lender finance, IMF direct investment,
# CM-FDI factory projects, AIB compact roles and simulated investment pairings) describes each pair and
# orders the list; it never makes or breaks one. Legacy Development Potential is shown as context only.
# Missing data stay NA: a donor that does not report purpose-coded aid has NA aid, not zero. No
# composite score: the order is lexicographic on the reasons (country_devpart_order()).
#
# Pure functions: the bundles and tables arrive as read by scripts/93_export_development_partnerships.R.
# Needs country_brief.R (country_brief_core_cells, country_brief_fence, country_brief_split) and
# R/strategic_position/pathway.R (SP_PATHWAY_ANCHORS, SP_PATHWAY_SUPPORT, SP_PATHWAY_FAMILY).

COUNTRY_DEVPART_SCHEMA <- "opsi.country.devpart"
COUNTRY_DEVPART_SCHEMA_VERSION <- 1L
COUNTRY_DEVPART_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")

country_devpart_chr <- function(x) if (is.null(x) || !length(x) || (length(x) == 1 && is.na(x))) NA_character_ else as.character(x[[1]])
country_devpart_num <- function(x) if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
country_devpart_join <- function(x) { x <- unique(x[!is.na(x) & nzchar(x)]); if (length(x)) paste(x, collapse = ";") else NA_character_ }
country_devpart_words <- function(codes, words) {
  codes <- codes[!is.na(codes)]
  w <- unlist(words[codes])
  unname(ifelse(is.na(w) | !nzchar(w), codes, w))
}

# ---- Steps ------------------------------------------------------------------------------------------------

# The posture detail fields the tests read, one row per economy x technology x step.
country_devpart_detail_table <- function(details) {
  rows <- lapply(names(details), function(iso) {
    cl <- details[[iso]]$cells
    if (!length(cl)) return(NULL)
    data.frame(
      iso3 = iso,
      tech = vapply(cl, function(x) country_devpart_chr(x$tech), ""),
      stage = vapply(cl, function(x) country_devpart_chr(x$stage), ""),
      delivery = vapply(cl, function(x) country_devpart_chr(x$delivery), ""),
      aib_status = vapply(cl, function(x) country_devpart_chr(x$partner_aib_status), ""),
      trusted_alt = vapply(cl, function(x) country_devpart_num(x$trusted_alternatives), 0),
      security_rationale = vapply(cl, function(x) country_devpart_chr(x$security_rationale), ""),
      pathway_basis = vapply(cl, function(x) country_devpart_chr(x$pathway_basis), ""),
      partner_evidence = vapply(cl, function(x) country_devpart_chr(x$partner_evidence), ""),
      trade_share = vapply(cl, function(x) country_devpart_num(x$trade$benchmark_share), 0),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Every assessed step of every economy with its detail fields, income group, tier, host screen and key.
# cells: country_brief_core_cells(core); income, tier, name: named character by ISO3; screen: the AIB host
# screen (iso3, bloc, in_demand_scope, screened, screen_reason).
country_devpart_steps <- function(cells, dtab, income, tier, screen, phrases = NULL) {
  x <- cells[cells$choice != "not_assessed", , drop = FALSE]
  x <- merge(x, dtab, by = c("iso3", "tech", "stage"), all.x = TRUE, sort = FALSE)
  x$key <- country_cell_key(x$tech, x$stage)
  x$node <- paste(x$tech, x$stage, sep = "|")
  x$step_name <- if (is.null(phrases)) paste(x$tech, tolower(x$stage)) else unname(ifelse(is.na(phrases[x$node]), paste(x$tech, tolower(x$stage)), phrases[x$node]))
  x$fence <- country_brief_fence(x$choice)
  x$income_group <- unname(income[x$iso3])
  x$tier <- unname(tier[x$iso3])
  m <- match(x$iso3, screen$iso3)
  x$bloc <- as.character(screen$bloc[m])
  x$in_demand_scope <- as.logical(screen$in_demand_scope[m])
  x$screened <- as.logical(screen$screened[m])
  x$screen_reason <- as.character(screen$screen_reason[m])
  x[order(x$iso3, x$tech, match(x$stage, COUNTRY_DEVPART_STAGES)), , drop = FALSE]
}

# The AIB nodes by posture step (nodes[].yfCells), with their class, workstream, Sectoral Compact, capacity
# hosts and (legacy, Development Potential driven) development hosts.
country_devpart_nodes <- function(nodes, strategic) {
  ws <- strategic$workstreams %||% list()
  ws_compact <- stats::setNames(vapply(ws, function(w) country_devpart_chr(w$compact), ""), vapply(ws, function(w) country_devpart_chr(w$id), ""))
  cname <- stats::setNames(vapply(strategic$compacts %||% list(), function(k) country_devpart_chr(k$name), ""),
                           vapply(strategic$compacts %||% list(), function(k) country_devpart_chr(k$id), ""))
  rows <- lapply(nodes, function(n) {
    cells <- unlist(n$yfCells)
    if (!length(cells)) return(NULL)
    wsid <- country_devpart_chr(n$compact)
    sc <- if (is.na(wsid)) NA_character_ else unname(ws_compact[wsid])
    data.frame(key = cells, node = country_devpart_chr(n$id), label = country_devpart_chr(n$label), cls = country_devpart_chr(n$cls),
               workstream = wsid, compact = sc, compact_name = if (is.na(sc)) NA_character_ else unname(cname[sc]),
               hosts = country_devpart_join(unlist(n$hosts)), dev_hosts = country_devpart_join(unlist(n$developmentHosts)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(key = character(), node = character(), label = character(), cls = character(), workstream = character(),
                                      compact = character(), compact_name = character(), hosts = character(), dev_hosts = character())
  out
}

# ---- Donor test --------------------------------------------------------------------------------------------

# The donor signals of each step (columns sig_<signal>, logical) and their summary: signals (";"-joined in
# the configured order), national, allied, donor_tier (1 = a choice-based reason and allied supply that falls
# short for the donor; 2 = a choice-based reason and a BUILD node; 3 = a choice-based reason; 4 = high need
# without a choice-based reason; 5 = allied reasons only; NA = none). Choice-based: Partner, Secure with
# allied delivery, or a High fence with few trusted alternatives.
# th: posture core meta.thresholds (need); nodes: country_devpart_nodes().
country_devpart_donor_signals <- function(x, nodes, th, cfg) {
  bar <- as.numeric(th$need)
  build_keys <- unique(nodes$key[nodes$cls %in% "BUILD"])
  ind <- x$stage != "Deployment"
  x$sig_partner <- x$choice %in% "partner"
  x$sig_secure_allied <- x$choice %in% "secure" & x$delivery %in% unlist(cfg$donor$secure_allied_delivery)
  x$sig_high_fence_untrusted <- x$choice %in% "secure" & !is.na(x$trusted_alt) & x$trusted_alt >= bar
  x$sig_high_need <- ind & !is.na(x$need) & x$need >= bar
  x$sig_aib_shortfall <- x$aib_status %in% "gap"
  x$sig_build <- ind & x$key %in% build_keys
  sigs <- unlist(cfg$donor$signals)
  mat <- as.matrix(x[paste0("sig_", sigs)])
  x$signals <- apply(mat, 1, function(r) country_devpart_join(sigs[r]))
  choice_based <- x$sig_partner | x$sig_secure_allied | x$sig_high_fence_untrusted
  allied <- x$sig_aib_shortfall | x$sig_build
  x$national <- choice_based | x$sig_high_need
  x$allied <- allied
  x$donor_tier <- ifelse(choice_based & x$sig_aib_shortfall, 1L, ifelse(choice_based & x$sig_build, 2L, ifelse(choice_based, 3L,
                    ifelse(x$sig_high_need, 4L, ifelse(allied, 5L, NA_integer_)))))
  x
}

# ---- Recipient test ---------------------------------------------------------------------------------------

# A pathway basis (posture detail pathway_basis) read with the engine's families: the anchors that fired,
# the supporting signals, the corroborating signals (another evidence family than an anchor's), whether
# the corroboration is context only (a thin anchor), and the constraints of the config. A constraint is
# raised only where its input is observed (obs: the engine's pw_* values of the step, NA when not observed):
# `observed` inputs must be non-NA; `zero` inputs must all be observed and 0 (any above 0 cancels it);
# `positive` needs one observed above 0. A constraint whose input is not observed is listed in `unobserved`
# ("not covered by the data"), never stated as a constraint.
country_devpart_pathway <- function(basis, cfg, obs = list()) {
  b <- country_brief_split(basis)
  fam <- SP_PATHWAY_FAMILY
  anchors <- intersect(b, SP_PATHWAY_ANCHORS)
  supports <- intersect(b, SP_PATHWAY_SUPPORT)
  sig <- c(anchors, supports)
  afam <- unique(unname(fam[anchors]))
  corr <- sig[vapply(sig, function(s) any(afam != fam[[s]]), TRUE)]
  ctx <- unlist(cfg$recipient$context_only_support)
  strong <- setdiff(corr, ctx)
  val <- function(k) { v <- obs[[k]]; if (is.null(v) || !length(v)) NA_real_ else suppressWarnings(as.numeric(v[[1]])) }
  cons <- character()
  unobs <- character()
  for (k in names(cfg$constraints)) {
    r <- cfg$constraints[[k]]
    absent <- unlist(r$when_absent)
    present <- unlist(r$when_present)
    only <- unlist(r$only_anchors)
    hit <- (!length(absent) || !any(absent %in% b)) && (!length(present) || any(present %in% b)) &&
      (!length(only) || (length(anchors) && all(anchors %in% only)))
    if (!hit || !(length(absent) || length(present) || length(only))) next
    oc <- unlist(r$observed)
    zc <- unlist(r$zero)
    pc <- unlist(r$positive)
    if (length(oc) && anyNA(vapply(oc, val, 0))) { unobs <- c(unobs, k); next }
    if (length(zc)) {
      v <- vapply(zc, val, 0)
      if (any(v > 0, na.rm = TRUE)) next
      if (anyNA(v)) { unobs <- c(unobs, k); next }
    }
    if (length(pc) && !any(vapply(pc, val, 0) > 0, na.rm = TRUE)) next
    cons <- c(cons, k)
  }
  list(anchors = anchors, supports = supports, families = afam, corroboration = corr, thin = length(anchors) > 0 && !length(strong),
       constraints = cons, unobserved = unobs)
}

# The recipient test of each step: eligible (logical) and fail (the first test it fails, in the order income,
# catch_up, trust, pathway, anchor, thin_anchor, capability, evidence, confidence, review_flag, artifact), with
# the pathway reading; and preserve_fail, the test a Preserve fails (trust, capability, evidence, confidence,
# review_flag, artifact). review: data.frame(iso3, tech, stage, reason) of known data artifacts. capability:
# config capability_checks, for steps whose measures describe different products: the Catch Up or Preserve
# must rest on an anchor that measures the product the need is about (match_anchors), or the economy's trade
# share in another step that holds that product (match_step_share, at the engine's direct_share_min), or the
# step cannot be verified at all (unverifiable). The pw_* columns (scripts/93, the engine's pathway inputs)
# gate the constraints; without them every constraint input counts as not observed.
country_devpart_recipient_test <- function(x, cfg, review = NULL) {
  hi <- cfg$income$high
  pwc <- grep("^pw_", names(x), value = TRUE)
  pw <- lapply(seq_len(nrow(x)), function(i) country_devpart_pathway(x$pathway_basis[i], cfg, as.list(x[i, pwc, drop = FALSE])))
  x$anchors <- vapply(pw, function(p) country_devpart_join(p$anchors), "")
  x$supports <- vapply(pw, function(p) country_devpart_join(p$supports), "")
  x$corroboration <- vapply(pw, function(p) country_devpart_join(p$corroboration), "")
  x$n_anchor_families <- vapply(pw, function(p) length(p$families), 0L)
  x$thin_anchor <- vapply(pw, function(p) isTRUE(p$thin), TRUE)
  x$constraints <- vapply(pw, function(p) country_devpart_join(p$constraints), "")
  x$unobserved <- vapply(pw, function(p) country_devpart_join(p$unobserved), "")
  art <- if (is.null(review) || !nrow(review)) rep(NA_character_, nrow(x)) else
    review$reason[match(paste(x$iso3, x$tech, x$stage), paste(review$iso3, review$tech, review$stage))]
  x$artifact <- art
  cap_ok <- rep(TRUE, nrow(x))
  cap_note <- rep(NA_character_, nrow(x))
  share_of <- function(iso, node) { i <- match(paste(iso, node), paste(x$iso3, paste(x$tech, x$stage, sep = "|"))); if (is.na(i)) NA_real_ else x$trade_share[i] }
  for (ck in cfg$capability_checks %||% list()) {
    on <- x$tech == ck$tech & x$stage %in% unlist(ck$stages) & x$choice %in% c("catch_up", "preserve")
    if (!any(on)) next
    ok <- if (isTRUE(ck$unverifiable)) rep(FALSE, nrow(x)) else if (!is.null(ck$match_step_share)) {
      v <- vapply(x$iso3, share_of, 0, node = ck$match_step_share)
      !is.na(v) & v >= as.numeric(ck$min_share)
    } else vapply(strsplit(ifelse(is.na(x$anchors), "", x$anchors), ";", fixed = TRUE), function(a) any(a %in% unlist(ck$match_anchors)), TRUE)
    bad <- on & !ok
    cap_ok[bad] <- FALSE
    cap_note[bad] <- sprintf("the position rests on %s, not on %s", ck$other, ck$measured)
  }
  x$capability_split <- !cap_ok
  x$capability_note <- cap_note
  tests <- list(
    income = !is.na(x$income_group) & x$income_group != hi & x$income_group != "Not classified",
    catch_up = x$choice %in% "catch_up",
    trust = !is.na(x$screened) & !x$screened,
    pathway = x$pw %in% "yes",
    anchor = !is.na(x$anchors),
    thin_anchor = !x$thin_anchor,
    capability = cap_ok,
    evidence = x$ev %in% unlist(cfg$recipient$evidence_ok),
    confidence = x$conf %in% unlist(cfg$recipient$confidence_ok),
    review_flag = is.na(x$fl) | !nzchar(x$fl),
    artifact = is.na(art)
  )
  first_fail <- function(names) {
    f <- rep(NA_character_, nrow(x))
    for (k in rev(names)) f[!tests[[k]]] <- k
    f
  }
  x$recipient_fail <- first_fail(names(tests))
  x$recipient_ok <- is.na(x$recipient_fail)
  x$preserve_fail <- first_fail(c("trust", "capability", "evidence", "confidence", "review_flag", "artifact"))
  x
}

# The donor scope of each economy: high income and in the AIB demand scope (the host screen's
# in_demand_scope; without a screen row, the configured Core / Partner tiers).
country_devpart_donor_scope <- function(x, cfg) {
  scope <- ifelse(is.na(x$in_demand_scope), x$tier %in% unlist(cfg$donor$scope_tiers), x$in_demand_scope)
  x$income_group %in% cfg$income$high & scope
}

# ---- Relationship evidence --------------------------------------------------------------------------------

# OECD CRS purpose-coded aid by donor -> recipient and step, over the window's years (constant USD
# millions, the flow of the settings; ODA and other official finance apart). crs: year, donor_iso3,
# recipient_iso3, purpose_code, measure, usd_disbursement / usd_commitment; crosswalk: the six-stage
# purpose crosswalk (purpose_code, supply_chain, tech, method). Returns:
#   tech   donor, recipient, tech, stage, oda, other, named (direct codes naming only this tech), shared
#          (direct codes naming it with others), proxy (direct codes that only approximate the technology,
#          config aid.proxy_codes), pooled (stage-wide codes listing it), codes; amounts are totals over
#          the window, not annual means;
#   total  donor, recipient, oda_mapped (crosswalk codes), oda_all (every code of the extract);
#   donors the donors with any purpose-coded record in the window.
country_devpart_aid <- function(crs, crosswalk, years, flow = "disbursement", cfg) {
  col <- if (identical(flow, "commitment")) "usd_commitment" else "usd_disbursement"
  a <- crs[crs$year %in% years, , drop = FALSE]
  donors <- sort(unique(as.character(a$donor_iso3)))
  a$amount <- suppressWarnings(as.numeric(a[[col]]))
  a$kind <- ifelse(a$measure %in% unlist(cfg$aid$oda_measures), "oda", ifelse(a$measure %in% unlist(cfg$aid$other_measures), "other", NA_character_))
  a <- a[!is.na(a$kind) & !is.na(a$amount), , drop = FALSE]
  a$purpose_code <- as.character(a$purpose_code)
  tot <- stats::aggregate(amount ~ donor_iso3 + recipient_iso3, a[a$kind == "oda", ], sum)
  names(tot)[3] <- "oda_all"
  cw <- crosswalk
  cw$purpose_code <- as.character(cw$purpose_code)
  ntech <- tapply(cw$tech, paste(cw$purpose_code, cw$supply_chain), function(t) length(unique(t)))
  cw$n_tech <- as.integer(ntech[paste(cw$purpose_code, cw$supply_chain)])
  cw$basis <- ifelse(cw$method == "direct", ifelse(cw$purpose_code %in% as.character(unlist(cfg$aid$proxy_codes)), "proxy", ifelse(cw$n_tech == 1L, "named", "shared")), "pooled")
  am <- stats::aggregate(amount ~ donor_iso3 + recipient_iso3 + purpose_code + kind, a, sum)
  mapped <- stats::aggregate(amount ~ donor_iso3 + recipient_iso3, am[am$kind == "oda" & am$purpose_code %in% cw$purpose_code, ], sum)
  names(mapped)[3] <- "oda_mapped"
  j <- merge(am, cw[c("purpose_code", "supply_chain", "tech", "basis")], by = "purpose_code")
  key <- c("donor_iso3", "recipient_iso3", "tech", "supply_chain")
  one <- function(kind, basis = NULL) {
    s <- j[j$kind == kind & (is.null(basis) | j$basis %in% basis), , drop = FALSE]
    if (!nrow(s)) return(NULL)
    stats::aggregate(stats::as.formula(paste("amount ~", paste(key, collapse = " + "))), s, sum)
  }
  parts <- list(oda = one("oda"), other = one("other"), named = one("oda", "named"), shared = one("oda", "shared"), proxy = one("oda", "proxy"), pooled = one("oda", "pooled"))
  tech <- unique(j[key])
  for (k in names(parts)) {
    p <- parts[[k]]
    tech[[k]] <- if (is.null(p)) 0 else { v <- p$amount[match(do.call(paste, tech[key]), do.call(paste, p[key]))]; ifelse(is.na(v), 0, v) }
  }
  codes <- stats::aggregate(purpose_code ~ donor_iso3 + recipient_iso3 + tech + supply_chain, j[j$kind == "oda", ], function(v) paste(sort(unique(v)), collapse = ";"))
  tech$codes <- codes$purpose_code[match(do.call(paste, tech[key]), do.call(paste, codes[key]))]
  names(tech)[names(tech) == "supply_chain"] <- "stage"
  total <- merge(tot, mapped, by = c("donor_iso3", "recipient_iso3"), all = TRUE)
  total$oda_mapped[is.na(total$oda_mapped)] <- 0
  list(tech = tech, total = total, donors = donors)
}

# The aid fields of one pair at one step. Recovery has no CRS purpose code: its aid is NA and the
# Extraction + Refining aid of the technology is given as a flagged proxy (the Development Potential
# roll-up rule). Components and Assembly share every purpose code, so the same aid appears at both. The
# specificity says where the money is (config aid.technology_share_min, a labelling rule for words, not a
# selection threshold): "technology" when codes naming only the technology hold that share of the aid,
# "shared_technology" when those plus codes naming it with others do, "proxy" when an approximate code
# (aid.proxy_codes) does, else "stage" (stage-wide codes; the technology is not identified).
country_devpart_aid_pair <- function(aid, donor, recipient, tech, stage, share_min = 0.5) {
  out <- list(status = NA_character_, oda = NA_real_, other = NA_real_, named = NA_real_, shared = NA_real_, proxy = NA_real_, pooled = NA_real_,
              codes = NA_character_, specificity = NA_character_, recovery = NA_real_, oda_pair = NA_real_, stages = NA_character_)
  if (!donor %in% aid$donors) { out$status <- "not_reporting"; return(out) }
  t <- aid$tech[aid$tech$donor_iso3 == donor & aid$tech$recipient_iso3 == recipient, , drop = FALSE]
  tt <- t[t$tech == tech, , drop = FALSE]
  pair_total <- aid$total$oda_mapped[aid$total$donor_iso3 == donor & aid$total$recipient_iso3 == recipient]
  out$oda_pair <- if (length(pair_total)) pair_total[1] else 0
  st <- unique(tt$stage[tt$oda > 0])
  out$stages <- country_devpart_join(st[order(match(st, COUNTRY_DEVPART_STAGES))])
  if (identical(stage, "Recovery")) {
    out$recovery <- sum(tt$oda[tt$stage %in% c("Extraction", "Refining")])
    out$status <- if (out$recovery > 0) "proxy" else if (out$oda_pair > 0) "other_stage" else "none"
    return(out)
  }
  r <- tt[tt$stage == stage, , drop = FALSE]
  for (k in c("oda", "other", "named", "shared", "proxy", "pooled")) out[[k]] <- if (nrow(r) && k %in% names(r)) sum(r[[k]]) else 0
  out$codes <- if (nrow(r)) country_devpart_chr(r$codes) else NA_character_
  if (out$oda > 0) {
    out$specificity <- if (out$named / out$oda >= share_min) "technology" else if ((out$named + out$shared) / out$oda >= share_min) "shared_technology" else
      if (out$proxy / out$oda >= share_min) "proxy" else "stage"
  }
  out$status <- if (out$oda > 0) "recorded" else if (out$oda_pair > 0) "other_stage" else "none"
  out
}

# OCI amounts (partnership_strength_oci_amounts() on rows keyed "<lender>|<recipient>") by lender ->
# recipient, technology and step, USD millions (current prices, commitments). method "pooled" rows count
# the full amount at each listed step and are flagged.
country_devpart_oci_cells <- function(oci) {
  a <- merge(oci$amounts, oci$crosswalk, by = "purpose_code")
  if (!nrow(a)) return(data.frame(lender = character(), recipient = character(), tech = character(), stage = character(), usd_m = numeric(), pooled = logical()))
  pr <- do.call(rbind, strsplit(a$recipient_iso3, "|", fixed = TRUE))
  a$lender <- pr[, 1]
  a$recipient <- pr[, 2]
  a$pooled <- a$method == "pooled"
  s <- stats::aggregate(cbind(usd_m = amount, pooled = pooled) ~ lender + recipient + tech + supply_chain, a, sum)
  s$pooled <- s$pooled > 0
  names(s)[names(s) == "supply_chain"] <- "stage"
  s
}

# CM-FDI factory projects by home -> destination and technology: the projects the table itself counts
# (index_use, which leaves out R&D, design, logistics, paused and ceased sites; else not cancelled or
# closed). Capital is NA when no project of the group discloses it, with the number that do. The projects'
# stages are Components and Assembly together: the database does not split them. attr "techs": the
# technologies the table covers at all (any status), so a pair outside them is not observed, not zero.
country_devpart_cmfdi <- function(projects, dead = c("Cancelled", "Closed")) {
  techs <- sort(unique(stats::na.omit(as.character(projects$tech))))
  keep <- if ("index_use" %in% names(projects)) projects$index_use %in% TRUE else !projects$status %in% dead
  p <- projects[keep & !is.na(projects$home_iso3) & !is.na(projects$destination_iso3), , drop = FALSE]
  empty <- data.frame(home = character(), dest = character(), tech = character(), n = integer(), usd_m = numeric(), n_disclosed = integer(), stages = character())
  if (!nrow(p)) return(structure(empty, techs = techs))
  p$usd <- suppressWarnings(as.numeric(p$capital_usd_m))
  g <- split(seq_len(nrow(p)), paste(p$home_iso3, p$destination_iso3, p$tech, sep = "|"))
  out <- do.call(rbind, lapply(g, function(i) data.frame(
    home = p$home_iso3[i[1]], dest = p$destination_iso3[i[1]], tech = p$tech[i[1]], n = length(unique(p$project_id[i])),
    usd_m = if (all(is.na(p$usd[i]))) NA_real_ else sum(p$usd[i], na.rm = TRUE), n_disclosed = sum(!is.na(p$usd[i])),
    stages = paste(sort(unique(p$stages[i])), collapse = ";"), stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  structure(out, techs = techs)
}

# The Sectoral Compacts of a step, from the brief's curated mapping (config/country_brief.yml compacts:
# techs, steps "Tech|Stage", exclude), so a pair names the compact the step belongs to rather than every
# compact one of its products touches.
country_devpart_step_compacts <- function(tech, stage, compact_map) {
  node <- paste(tech, stage, sep = "|")
  ids <- names(compact_map %||% list())
  ids[vapply(ids, function(id) {
    m <- compact_map[[id]]
    (tech %in% unlist(m$techs) || node %in% unlist(m$steps)) && !node %in% unlist(m$exclude)
  }, TRUE)]
}

# ---- Tools ------------------------------------------------------------------------------------------------

# The development tools of a step for a recipient's constraints: the stage's support tools the constraints
# point to first (config constraints[].tools that are in the stage's list), then the rest of the stage's
# list, `n` in all; and the stage's scale instruments.
country_devpart_tools <- function(stage, constraints, cfg, n = 4L) {
  st <- cfg$tools[[stage]]
  sup <- unlist(st$support)
  lead <- unlist(lapply(constraints, function(k) unlist(cfg$constraints[[k]]$tools)))
  lead <- unique(lead[lead %in% sup])
  list(support = utils::head(unique(c(lead, sup)), n), scale = unlist(st$scale), led = length(lead) > 0)
}

# The recommended tool family in words. Commercial route: the commercial instruments only. Development
# route: for an upper-middle-income recipient (config tools.lead_with_finance), the scale instruments lead and
# public support is kept to the first two support tools (public goods such as standards, testing and
# skills); otherwise the support tools lead and the scale instruments follow once the capability is bankable.
country_devpart_tool_text <- function(route, income_group, tl, cfg) {
  if (identical(route, "commercial")) return(paste(unlist(cfg$commercial$tools), collapse = "; "))
  if (income_group %in% unlist(cfg$tools_lead_with_finance))
    return(sprintf("lead with %s; public support for %s", paste(utils::head(tl$scale, 4), collapse = ", "), paste(utils::head(tl$support, 2), collapse = " and ")))
  sprintf("%s; then, at scale, %s", paste(tl$support, collapse = ", "), paste(utils::head(tl$scale, 4), collapse = ", "))
}

# ---- Pairs ------------------------------------------------------------------------------------------------

# Every donor step that qualifies, joined with every non-high-income economy whose same step is Catch Up
# (passing or failing the recipient test) or Preserve (the commercial route), with the relationship evidence
# and the route. steps: country_devpart_recipient_test(country_devpart_donor_signals(country_devpart_steps()));
# rel: list(aid, oci (country_devpart_oci_cells), oci_lenders, dip (reporter, counterpart, value, conduit, basis),
# compact_map (config/country_brief.yml compacts),
# cmfdi (country_devpart_cmfdi), dp (partner_iso, development), flows (investor, host, node, usd), roles
# (strategic compactRoles)); name_of: ISO3 -> name.
country_devpart_pairs <- function(steps, nodes, rel, cfg, name_of = NULL) {
  nm <- function(i) if (is.null(name_of)) i else unname(ifelse(is.na(name_of[i]), i, name_of[i]))
  steps$donor_scope <- country_devpart_donor_scope(steps, cfg)
  rev_conf <- unlist(cfg$donor$review_confidence)
  don <- steps[steps$donor_scope & !is.na(steps$donor_tier), , drop = FALSE]
  rec <- steps[!is.na(steps$income_group) & steps$income_group != cfg$income$high & steps$choice %in% c("catch_up", "preserve"), , drop = FALSE]
  p <- merge(don, rec, by = c("tech", "stage"), suffixes = c("_d", "_r"))
  p <- p[p$iso3_d != p$iso3_r, , drop = FALSE]
  if (!nrow(p)) return(NULL)
  build <- nodes[nodes$cls %in% "BUILD", , drop = FALSE]
  node_lab <- function(k, cls = NULL) { s <- nodes[nodes$key == k & (is.null(cls) | nodes$cls %in% cls), , drop = FALSE]; country_devpart_join(s$label) }
  host_of <- function(k, iso) any(vapply(strsplit(as.character(build$hosts[build$key == k & !is.na(build$hosts)]), ";", fixed = TRUE), function(h) iso %in% h, TRUE))
  devhost_of <- function(k, iso) any(vapply(strsplit(as.character(nodes$dev_hosts[nodes$key == k & !is.na(nodes$dev_hosts)]), ";", fixed = TRUE), function(h) iso %in% h, TRUE))
  roles <- rel$roles %||% list()
  role_tbl <- if (length(roles)) data.frame(compact = vapply(roles, function(r) country_devpart_chr(r$compact), ""), iso3 = vapply(roles, function(r) country_devpart_chr(r$iso3), ""),
                                            role = vapply(roles, function(r) country_devpart_chr(r$role), ""),
                                            functions = vapply(roles, function(r) country_devpart_join(unlist(r$functions)), ""), stringsAsFactors = FALSE) else NULL
  role_of <- function(compacts, iso) {
    if (is.null(role_tbl) || all(is.na(compacts))) return(NA_character_)
    r <- role_tbl[role_tbl$compact %in% compacts & role_tbl$iso3 == iso, , drop = FALSE]
    if (!nrow(r)) NA_character_ else country_devpart_join(paste0(r$compact, ": ", r$role))
  }
  fl <- rel$flows
  out <- vector("list", nrow(p))
  for (i in seq_len(nrow(p))) {
    r <- p[i, ]
    d <- r$iso3_d; q <- r$iso3_r
    k <- r$key_d
    cmp <- country_devpart_step_compacts(r$tech, r$stage, rel$compact_map)
    if (!length(cmp)) cmp <- unique(nodes$compact[nodes$key == k & nodes$cls %in% "BUILD" & !is.na(nodes$compact)])
    ws <- unique(nodes$workstream[nodes$key == k & nodes$cls %in% "BUILD" & !is.na(nodes$workstream)])
    aid <- country_devpart_aid_pair(rel$aid, d, q, r$tech, r$stage, as.numeric(cfg$aid$technology_share_min %||% 0.5))
    # OCI: NA for a donor whose lenders the data do not cover, and at a step the OCI crosswalk never maps
    oci <- if (!d %in% rel$oci_lenders || (!is.null(rel$oci_steps) && !paste(r$tech, r$stage, sep = "|") %in% rel$oci_steps)) NA_real_ else {
      o <- rel$oci[rel$oci$lender == d & rel$oci$recipient == q & rel$oci$tech == r$tech & rel$oci$stage == r$stage, , drop = FALSE]
      if (nrow(o)) sum(o$usd_m) else 0
    }
    dp <- rel$dip[rel$dip$reporter == d & rel$dip$counterpart == q, , drop = FALSE]
    dip <- if (nrow(dp)) dp$value[1] else NA_real_
    dip_conduit <- if (nrow(dp)) dp$conduit[1] else NA_character_
    dip_basis <- if (nrow(dp)) dp$basis[1] else NA_character_
    # CM-FDI: only at Components and Assembly and for the technologies the table covers (else NA)
    cm_n <- NA_integer_; cm_usd <- NA_real_; cm_disc <- NA_integer_
    cm_cov <- attr(rel$cmfdi, "techs") %||% unique(rel$cmfdi$tech)
    if (r$stage %in% c("Components", "Assembly") && r$tech %in% cm_cov) {
      cm <- rel$cmfdi[rel$cmfdi$home == d & rel$cmfdi$dest == q & rel$cmfdi$tech == r$tech, , drop = FALSE]
      cm_n <- if (nrow(cm)) as.integer(sum(cm$n)) else 0L
      cm_usd <- if (!nrow(cm)) 0 else if (all(is.na(cm$usd_m))) NA_real_ else sum(cm$usd_m, na.rm = TRUE)
      cm_disc <- if (nrow(cm)) as.integer(sum(cm$n_disclosed)) else 0L
    }
    pair_nodes <- unique(nodes$node[nodes$key == k])
    fp <- if (is.null(fl)) NULL else fl[fl$investor == d & fl$host == q & fl$node %in% pair_nodes, , drop = FALSE]
    aib_pairing <- !is.null(fp) && nrow(fp) > 0
    host <- host_of(k, q)
    # route
    # a donor's low-confidence, flagged or artifact call cannot carry a national reason: its national signals
    # are dropped and an allied reason, which does not rest on the donor's own call, carries the pair at tier 5;
    # without an allied reason the pair is a review case
    call_review <- r$conf_d %in% rev_conf || !is.na(r$artifact_d) || (!is.na(r$fl_d) && nzchar(r$fl_d))
    tier <- r$donor_tier_d
    national_dropped <- call_review && tier < 5L && isTRUE(r$allied_d)
    sig_d <- r$signals_d
    if (national_dropped) {
      tier <- 5L
      sig_d <- country_devpart_join(intersect(country_brief_split(sig_d), unlist(cfg$donor$allied)))
    }
    donor_review <- call_review && tier < 5L
    # a Preserve takes the trust, capability and evidence tests (recipient_test preserve_fail)
    fail <- if (r$choice_r == "preserve") r$preserve_fail_r else r$recipient_fail_r
    route <- if (!is.na(fail) && !fail %in% unlist(cfg$review_fails)) "excluded" else if (donor_review || !is.na(fail)) "review" else
      if (r$choice_r == "preserve" || host) "commercial" else "development"
    cons <- country_brief_split(r$constraints_r)
    tl <- country_devpart_tools(r$stage, cons, cfg)
    # deployment demand of the technology, context only (never in the tier, the route or the order)
    dep_of <- function(iso) { j <- match(paste(iso, r$tech, "Deployment"), paste(steps$iso3, steps$tech, steps$stage)); if (is.na(j)) c(NA, NA) else c(steps$choice[j], steps$conf[j]) }
    dd <- dep_of(d); dq <- dep_of(q)
    acc <- function(v) identical(v[1], "accelerate") && !identical(v[2], "low")
    demand_anchor <- if (acc(dd) && acc(dq)) "both" else if (acc(dd)) "allied" else if (acc(dq)) "local" else NA_character_
    tool <- country_devpart_tool_text(route, r$income_group_r, tl, cfg)
    rel_n <- sum(c(isTRUE(aid$oda > 0) || isTRUE(aid$recovery > 0), isTRUE(dip > 0), isTRUE(cm_n > 0), isTRUE(oci > 0), aib_pairing,
                   !is.na(role_of(cmp, q)) && !is.na(role_of(cmp, d))))
    caveats <- c(
      if (r$stage %in% c("Components", "Assembly")) "CRS purpose codes do not separate Components from Assembly: the same aid appears at both steps",
      if (identical(r$stage, "Recovery")) "no CRS purpose code funds recycling: the aid shown is an Extraction + Refining proxy",
      if (identical(aid$specificity, "stage")) "the aid at this step is stage-wide (technology not identified)",
      if (identical(aid$specificity, "shared_technology")) "the aid at this step is coded to several technologies at once",
      if (!is.na(cm_n)) "CM-FDI factory projects do not separate Components from Assembly",
      if (!is.na(oci) && oci > 0) "OCI amounts are export-credit and lender commitments, not aid",
      if (!is.na(dip_conduit) && dip_conduit != "none") "the direct-investment position involves a conduit financial centre",
      if (identical(dip_basis, "derived")) "the direct-investment position is derived from the recipient's reports (the donor reports none)",
      if (isTRUE(r$thin_anchor_r)) "the Catch Up pathway is corroborated by context signals only",
      if (isTRUE(r$capability_split_r)) paste0("capability unverified: ", r$capability_note_r),
      if (identical(aid$specificity, "proxy")) "the aid at this step is under a code that only approximates the technology",
      if (national_dropped) "the donor's own call is a review case: only its allied reasons count",
      if (!is.na(cm_n) && cm_n > 0 && !is.na(cm_disc) && cm_disc < cm_n) sprintf("CM-FDI capital is disclosed for %d of %d projects", cm_disc, cm_n),
      if (!is.na(r$artifact_r)) paste("known data artifact:", r$artifact_r),
      if (!is.na(r$artifact_d)) paste("known data artifact in the donor's call:", r$artifact_d)
    )
    ev <- if (r$conf_r %in% "high" && r$ev_r %in% "high" && r$conf_d %in% "high") "strong" else "moderate"
    out[[i]] <- data.frame(
      donor_iso3 = d, donor = nm(d), recipient_iso3 = q, recipient = nm(q), technology = r$tech, stage = r$stage, key = k,
      capability = r$step_name_d, aib_nodes = node_lab(k), aib_build_nodes = node_lab(k, "BUILD"),
      donor_choice = r$choice_d, donor_fence = r$fence_d, donor_need = r$need_d, donor_confidence = r$conf_d,
      donor_delivery = r$delivery_d, donor_security_rationale = r$security_rationale_d, donor_signals = sig_d,
      donor_tier = tier, donor_national_dropped = national_dropped, aib_gap_status = r$aib_status_d,
      recipient_choice = r$choice_r, recipient_siv = r$prize_r, recipient_competitive_position = r$cp_r,
      recipient_confidence = r$conf_r, recipient_evidence = r$ev_r, recipient_income_group = r$income_group_r,
      recipient_bloc = r$bloc_r, recipient_screened = r$screened_r, recipient_screen_reason = r$screen_reason_r,
      catch_up_anchor = !is.na(r$anchors_r), catch_up_anchor_type = r$anchors_r, pathway_signals = r$supports_r,
      pathway_corroboration = r$corroboration_r, n_anchor_families = r$n_anchor_families_r, thin_anchor = r$thin_anchor_r,
      recipient_constraints = r$constraints_r, recipient_unobserved = r$unobserved_r, recipient_fail = fail, donor_review = donor_review,
      capability_unverified = isTRUE(r$capability_split_r), capability_note = r$capability_note_r,
      aib_capacity_host = host, aib_development_host_legacy = devhost_of(k, q),
      development_potential_legacy = { v <- rel$dp$development[match(q, rel$dp$partner_iso)]; if (length(v)) v else NA_real_ },
      oda_status = aid$status, current_oda = aid$oda, other_official_finance = aid$other, oda_named = aid$named, oda_shared = aid$shared,
      oda_proxy = aid$proxy, oda_pooled = aid$pooled, oda_codes = aid$codes, oda_specificity = aid$specificity, oda_recovery_proxy = aid$recovery,
      oda_pair_total = aid$oda_pair, oda_stage = aid$stages, oda_stage_match = if (is.na(aid$status) || aid$status == "not_reporting") NA else aid$status %in% "recorded",
      oda_components_assembly_shared = r$stage %in% c("Components", "Assembly"), oda_proxy_flag = identical(r$stage, "Recovery"),
      oci_finance = oci, imf_dip_tie = dip, imf_dip_basis = dip_basis, imf_dip_conduit = dip_conduit, cm_fdi_projects = cm_n, cm_fdi_usd_m = cm_usd, cm_fdi_capital_disclosed = cm_disc,
      aib_pairing = aib_pairing, aib_pairing_usd = if (aib_pairing) sum(fp$usd, na.rm = TRUE) / 1e6 else NA_real_,
      donor_deployment_choice = dd[1], recipient_deployment_choice = dq[1], demand_anchor = demand_anchor,
      compact = country_devpart_join(cmp), workstream = country_devpart_join(ws),
      donor_compact_role = role_of(cmp, d), recipient_compact_role = role_of(cmp, q), n_relationship = rel_n,
      route = route, recommended_tool_family = tool, tools_led_by_constraints = tl$led,
      evidence_quality = ev, caveats = country_devpart_join(caveats), stringsAsFactors = FALSE
    )
  }
  pairs <- do.call(rbind, out)
  pairs <- country_devpart_order(pairs)
  pairs$rationale <- country_devpart_rationale(pairs, cfg)
  pairs
}

# The fixed order of the pairs: route (development, commercial, review, excluded), donor tier, an anchor
# measured on the step's own supply or export share first, recipient confidence and evidence, the number of
# anchor families, the number of relationship ties, lower income first (additionality), the recipient's
# Competitive Position, then names. Adds rank within (donor, route) and within (recipient, route).
country_devpart_order <- function(p) {
  lvl <- function(x, v) match(x, v, nomatch = length(v) + 1L)
  measured <- grepl("direct_(supply|export)_share", p$catch_up_anchor_type)
  o <- order(lvl(p$route, c("development", "commercial", "review", "excluded")), p$donor_tier, !measured,
             lvl(p$recipient_confidence, c("high", "moderate", "low")), lvl(p$recipient_evidence, c("high", "medium", "low", "none")),
             -p$n_anchor_families, -p$n_relationship, lvl(p$recipient_income_group, c("Low income", "Lower middle income", "Upper middle income")),
             -dplyr::coalesce(p$recipient_competitive_position, -1),
             p$donor_iso3, p$recipient_iso3, p$technology, match(p$stage, COUNTRY_DEVPART_STAGES), method = "radix")
  p <- p[o, , drop = FALSE]
  p$rank_donor <- stats::ave(seq_len(nrow(p)), p$donor_iso3, p$route, FUN = seq_along)
  p$rank_recipient <- stats::ave(seq_len(nrow(p)), p$recipient_iso3, p$route, FUN = seq_along)
  rownames(p) <- NULL
  p
}

# One plain sentence per pair: the donor reason, the recipient reason and the route.
country_devpart_rationale <- function(p, cfg) {
  w <- cfg$words
  vapply(seq_len(nrow(p)), function(i) {
    r <- p[i, ]
    sig <- country_devpart_words(country_brief_split(r$donor_signals), w$signal)
    don <- sprintf("%s: %s (%s).", r$donor, paste(sig, collapse = "; "), w$choice[[r$donor_choice]])
    anc <- country_devpart_words(country_brief_split(r$catch_up_anchor_type), w$anchor)
    fw <- if (is.na(r$recipient_fail)) NA_character_ else (w$fail[[r$recipient_fail]] %||% r$recipient_fail)
    rec <- if (r$route %in% c("excluded", "review") && !is.na(fw)) sprintf("%s does not qualify yet (%s).", r$recipient, fw) else
      if (r$recipient_choice == "preserve") sprintf("%s already Preserves the step: trade and investment, not aid.", r$recipient) else
        sprintf("%s: Catch Up anchored by %s.", r$recipient, paste(anc, collapse = " and "))
    tail <- if (r$route == "commercial" && r$recipient_choice != "preserve") sprintf(" %s already hosts this capacity in the AIB, so finance and offtake rather than grants.", r$recipient) else
      if (r$route == "review" && isTRUE(r$donor_review)) " The donor's call needs review before it can carry a recommendation." else ""
    paste0(don, " ", rec, tail)
  }, "")
}

# ---- Audit ------------------------------------------------------------------------------------------------

# TRUE where a pair's aid label matches the composition of its aid (config aid.technology_share_min): no
# label without aid, "technology" only with named codes holding that share, "shared_technology" only with
# named + shared codes holding it, "proxy" only with proxy codes holding it, never a proxy code labelled
# technology, and no label at all when there is no aid at the step.
country_devpart_specificity_ok <- function(p, share_min = 0.5) {
  oda <- p$current_oda
  sh <- function(v) ifelse(!is.na(oda) & oda > 0, v / oda, NA_real_)
  sp <- p$oda_specificity
  ok <- ifelse(is.na(oda) | oda <= 0, is.na(sp),
        ifelse(sp %in% "technology", sh(p$oda_named) >= share_min,
        ifelse(sp %in% "shared_technology", sh(p$oda_named + p$oda_shared) >= share_min & sh(p$oda_named) < share_min,
        ifelse(sp %in% "proxy", sh(p$oda_proxy) >= share_min & sh(p$oda_named + p$oda_shared) < share_min,
        ifelse(sp %in% "stage", sh(p$oda_named + p$oda_shared) < share_min & sh(p$oda_proxy) < share_min, FALSE)))))
  !is.na(ok) & ok
}

# The eight checks of a donor's development pairs (user spec 2026-11-03 section 19), one row per pair, each
# check TRUE / FALSE with the evidence that decides it:
#   1 donor_reason      a strategic or allied reason for more capacity (signals), from a call that is not a review case
#   2 eligible          the recipient is not high income
#   3 catch_up          a Catch Up with an anchor corroborated beyond context signals, of sufficient evidence
#   4 same_capability   the same step, not a capability-split step whose anchor measures another product
#   5 aid_specificity   the aid label matches where the money is (country_devpart_specificity_ok)
#   6 tool_bottleneck   the tools follow the stage and, where the data show one, the recipient's constraint
#   7 productive        the partner builds a refining or manufacturing capability (not extraction alone)
#   8 additional        not Preserve, not top-quarter, not already a capacity host of the step (else commercial)
country_devpart_audit <- function(pairs, donor, share_min = 0.5) {
  p <- pairs[pairs$donor_iso3 == donor & pairs$route == "development", , drop = FALSE]
  if (!nrow(p)) return(data.frame())
  data.frame(
    rank = p$rank_donor, step = p$capability, technology = p$technology, stage = p$stage, partner = p$recipient, partner_iso3 = p$recipient_iso3,
    c1_donor_reason = !is.na(p$donor_signals) & !p$donor_review, c1_evidence = p$donor_signals,
    c2_eligible = !is.na(p$recipient_income_group) & p$recipient_income_group != "High income", c2_evidence = p$recipient_income_group,
    c3_catch_up = p$recipient_choice == "catch_up" & !is.na(p$catch_up_anchor_type) & !p$thin_anchor & p$recipient_evidence %in% c("high", "medium"),
    c3_evidence = paste0(p$catch_up_anchor_type, " | ", p$pathway_signals, " | evidence ", p$recipient_evidence, ", confidence ", p$recipient_confidence),
    c4_same_capability = !p$capability_unverified, c4_evidence = paste0(p$key, if (is.null(p$aib_build_nodes)) "" else paste0(" | BUILD: ", p$aib_build_nodes)),
    c5_aid_specificity = country_devpart_specificity_ok(p, share_min), c5_evidence = paste0(p$oda_status, " | ", ifelse(is.na(p$oda_specificity), "no aid at the step", p$oda_specificity),
                                                    ifelse(!is.na(p$current_oda) & p$current_oda > 0, sprintf(" | named %.2f, shared %.2f, proxy %.2f, pooled %.2f of %.2f", p$oda_named, p$oda_shared, p$oda_proxy, p$oda_pooled, p$current_oda), ""),
                                                    ifelse(p$oda_components_assembly_shared, " | Components/Assembly codes shared", ""), ifelse(p$oda_proxy_flag, " | Recovery proxy", "")),
    c6_tool_bottleneck = p$tools_led_by_constraints | is.na(p$recipient_constraints), c6_evidence = paste0(p$recipient_constraints, " -> ", p$recommended_tool_family),
    c7_productive = p$stage %in% c("Refining", "Components", "Assembly", "Recovery"), c7_evidence = p$stage,
    c8_additional = p$recipient_choice != "preserve" & !p$aib_capacity_host & !is.na(p$recipient_competitive_position) & p$recipient_competitive_position < 0.75,
    c8_evidence = sprintf("CP %s; capacity host %s; %s", round(100 * p$recipient_competitive_position), p$aib_capacity_host, p$recipient_income_group),
    stringsAsFactors = FALSE)
}

# ---- Current aid vs the AIB strategy ----------------------------------------------------------------------

# The donor's current-aid comparison (user spec 2026-11-03 section 15): each development or commercial
# pair classed aligned (aid at the step coded to the technology), stage_wide (aid at the step under stage-wide or
# proxy codes only: the technology is not identified), proxy (Recovery: no CRS code, so only a proxy), wrong_stage
# (aid to the partner at other steps only), no_support (no aid to the partner), graduate (a commercial-route pair
# that still receives technology-coded aid at the step) or stage_wide_commercial (its stage-wide counterpart); and the
# donor's largest aid recipients (energy, industry and mining purpose codes) with no pair, no_aib. The
# comparison is against this energy-industrial objective only: aid outside it is not judged.
country_devpart_aid_gap <- function(pairs, aid, donor, cfg, name_of = NULL) {
  nm <- function(i) if (is.null(name_of)) i else unname(ifelse(is.na(name_of[i]), i, name_of[i]))
  pp <- pairs[pairs$donor_iso3 == donor & pairs$route %in% c("development", "commercial"), , drop = FALSE]
  # aid at the step counts as aligned (or, on the commercial route, to graduate) only when it is coded to the
  # technology (alone or with others); stage-wide or proxy-coded aid at the step is its own class
  tech_coded <- pp$oda_specificity %in% c("technology", "shared_technology")
  rec <- pp$oda_status %in% "recorded"
  cls <- ifelse(pp$route == "commercial", ifelse(rec & tech_coded, "graduate", ifelse(rec, "stage_wide_commercial", NA_character_)),
                ifelse(rec & tech_coded, "aligned", ifelse(rec, "stage_wide", ifelse(pp$oda_status %in% "proxy", "proxy",
                                                                 ifelse(pp$oda_status %in% "other_stage", "wrong_stage",
                                                                        ifelse(pp$oda_status %in% "none", "no_support", NA_character_))))))
  rows <- pp[!is.na(cls), c("recipient_iso3", "recipient", "technology", "stage", "capability", "route", "current_oda", "oda_named", "oda_specificity", "oda_pair_total", "oda_stage")]
  rows$gap <- cls[!is.na(cls)]
  tot <- aid$total[aid$total$donor_iso3 == donor, , drop = FALSE]
  tot <- tot[order(-tot$oda_all), , drop = FALSE]
  top <- utils::head(tot, as.integer(cfg$aid$top_recipients %||% 10L))
  paired <- unique(pairs$recipient_iso3[pairs$donor_iso3 == donor & pairs$route %in% c("development", "commercial")])
  no_aib <- top[!top$recipient_iso3 %in% paired, , drop = FALSE]
  list(pairs = rows,
       top = data.frame(recipient_iso3 = top$recipient_iso3, recipient = nm(top$recipient_iso3), oda_all = top$oda_all, oda_mapped = top$oda_mapped,
                        paired = top$recipient_iso3 %in% paired, stringsAsFactors = FALSE),
       no_aib = data.frame(recipient_iso3 = no_aib$recipient_iso3, recipient = nm(no_aib$recipient_iso3), oda_all = no_aib$oda_all, oda_mapped = no_aib$oda_mapped,
                           stringsAsFactors = FALSE),
       reporting = donor %in% aid$donors)
}

# ---- Country files ----------------------------------------------------------------------------------------

# The economy's perspective: donor (high income, in the AIB demand scope), recipient (non-high income) or
# none (high income outside the scope, or income group unknown), with the reason.
country_devpart_perspective <- function(steps, iso3, cfg) {
  s <- steps[steps$iso3 == iso3, , drop = FALSE]
  if (!nrow(s)) return(list(kind = "none", reason = "no assessed steps"))
  inc <- s$income_group[1]
  if (is.na(inc) || identical(inc, "Not classified")) return(list(kind = "none", reason = "income group unknown"))
  if (identical(inc, cfg$income$high)) {
    if (isTRUE(country_devpart_donor_scope(s[1, ], cfg))) return(list(kind = "donor", reason = NA_character_))
    return(list(kind = "none", reason = "high-income economy outside the Allied Industrial Base demand scope"))
  }
  list(kind = "recipient", reason = NA_character_)
}

# The pair fields one Country file carries: those the page and the brief read (the export carries every field,
# among them the rationale sentence and legacy Development Potential).
COUNTRY_DEVPART_FILE_FIELDS <- c("donor_iso3", "recipient_iso3", "technology", "stage", "key", "capability", "aib_build_nodes", "donor_choice",
                                 "donor_need", "donor_signals", "donor_tier", "aib_gap_status", "recipient_choice", "recipient_competitive_position",
                                 "recipient_confidence", "recipient_evidence", "recipient_income_group", "catch_up_anchor_type", "pathway_signals",
                                 "thin_anchor", "recipient_constraints", "recipient_unobserved", "recipient_fail", "donor_review",
                                 "aib_capacity_host", "demand_anchor", "oda_status", "current_oda", "oda_named", "oda_specificity",
                                 "oda_recovery_proxy", "oda_pair_total", "oda_stage", "oda_components_assembly_shared", "oci_finance", "imf_dip_tie",
                                 "imf_dip_basis", "imf_dip_conduit", "cm_fdi_projects", "aib_pairing", "compact", "donor_compact_role",
                                 "recipient_compact_role", "route", "recommended_tool_family", "evidence_quality", "caveats")

# A data.frame as a column list for JSON, NA kept as null (array fields stay arrays).
country_devpart_columns <- function(df) {
  lapply(as.list(df), function(v) I(if (is.logical(v) || is.numeric(v) || is.character(v)) v else as.character(v)))
}

# One economy's development-partnership file (dashboard/public/data/devpart). Donor: its qualifying
# steps with the pairs, the steps without any qualifying partner, the commercial and review cases and the
# current-aid comparison. Recipient: its Catch Up steps with the test result, the pairs of those that pass
# (the donors that need the capability) and the commercial route. meta: as_of and provenance.
country_devpart_country_file <- function(iso3, steps, pairs, aid, cfg, meta, name_of = NULL, compact_names = NULL) {
  nm <- function(i) if (is.null(name_of)) i else unname(ifelse(is.na(name_of[i]), i, name_of[i]))
  per <- country_devpart_perspective(steps, iso3, cfg)
  w <- cfg$words
  words <- list(signal = w$signal, signal_short = w$signal_short, anchor = w$anchor, support = w$support, route = w$route, aid_status = w$aid_status, aid_gap = w$aid_gap,
                constraint = lapply(cfg$constraints, function(k) k$text), benefit_donor = cfg$benefits$donor, benefit_recipient = cfg$benefits$recipient,
                fail = w$fail, unobserved = w$unobserved, compact = as.list(compact_names %||% list()))
  base <- list(schema = COUNTRY_DEVPART_SCHEMA, schema_version = COUNTRY_DEVPART_SCHEMA_VERSION, iso3 = iso3, country = nm(iso3),
               perspective = per$kind, perspective_reason = per$reason, meta = meta, words = words)
  keep <- intersect(COUNTRY_DEVPART_FILE_FIELDS, names(pairs))
  if (identical(per$kind, "none")) {
    # the perspective and its reason only (the page shows that sentence)
    base$words <- structure(list(), names = character())
    base$meta <- meta[c("created", "as_of_label")]
    return(base)
  }
  if (identical(per$kind, "donor")) {
    pp <- pairs[pairs$donor_iso3 == iso3 & pairs$route != "excluded", , drop = FALSE]
    s <- steps[steps$iso3 == iso3 & !is.na(steps$donor_tier), , drop = FALSE]
    has <- unique(pairs$key[pairs$donor_iso3 == iso3 & pairs$route == "development"])
    s$has_development <- s$key %in% has
    s$n_considered <- vapply(s$key, function(k) sum(pairs$donor_iso3 == iso3 & pairs$key == k), 0L)
    # why the candidates of a step do not qualify: each failed test with its count, most frequent first
    # (review and excluded pairs), and the partners already on the commercial route
    s$fails <- vapply(s$key, function(k) {
      f <- pairs$recipient_fail[pairs$donor_iso3 == iso3 & pairs$key == k & pairs$route %in% c("excluded", "review") & !is.na(pairs$recipient_fail)]
      if (!length(f)) return(NA_character_)
      tb <- sort(table(f), decreasing = TRUE)
      paste(sprintf("%s:%d", names(tb), as.integer(tb)), collapse = ";")
    }, "")
    s$commercial <- vapply(s$key, function(k) country_devpart_join(nm(pairs$recipient_iso3[pairs$donor_iso3 == iso3 & pairs$key == k & pairs$route == "commercial"])), "")
    gap <- country_devpart_aid_gap(pairs, aid, iso3, cfg, name_of)
    base$steps <- country_devpart_columns(s[c("key", "tech", "stage", "step_name", "choice", "fence", "need", "conf", "signals", "donor_tier",
                                              "aib_status", "has_development", "n_considered", "fails", "commercial")])
    base$pairs <- country_devpart_columns(pp[keep])
    base$aid <- list(reporting = gap$reporting, gap = country_devpart_columns(gap$pairs), top = country_devpart_columns(gap$top),
                     no_aib = country_devpart_columns(gap$no_aib))
  } else if (identical(per$kind, "recipient")) {
    s <- steps[steps$iso3 == iso3 & steps$choice %in% "catch_up", , drop = FALSE]
    pp <- pairs[pairs$recipient_iso3 == iso3 & pairs$route %in% c("development", "commercial", "review"), , drop = FALSE]
    s$n_donors <- vapply(s$key, function(k) length(unique(pp$donor_iso3[pp$key == k & pp$route %in% c("development", "commercial")])), 0L)
    base$steps <- country_devpart_columns(s[c("key", "tech", "stage", "step_name", "choice", "prize", "cp", "conf", "ev", "anchors", "supports",
                                              "thin_anchor", "constraints", "unobserved", "capability_note", "recipient_fail", "recipient_ok", "n_donors")])
    base$pairs <- country_devpart_columns(pp[keep])
    # its Preserve steps: the commercial route (trade and investment, not aid), or a review case
    ps <- steps[steps$iso3 == iso3 & steps$choice %in% "preserve" & steps$stage != "Deployment", , drop = FALSE]
    pres <- pairs[pairs$recipient_iso3 == iso3 & pairs$recipient_choice == "preserve", , drop = FALSE]
    base$preserve <- unique(pres$key)
    if (nrow(ps)) {
      ps$n_commercial <- vapply(ps$key, function(k) length(unique(pres$donor_iso3[pres$key == k & pres$route == "commercial"])), 0L)
      ps$n_review <- vapply(ps$key, function(k) length(unique(pres$donor_iso3[pres$key == k & pres$route == "review"])), 0L)
      base$preserve_steps <- country_devpart_columns(ps[c("key", "tech", "stage", "step_name", "conf", "ev", "preserve_fail", "n_commercial", "n_review")])
    }
  }
  base
}
