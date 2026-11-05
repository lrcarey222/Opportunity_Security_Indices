# Country strategy briefs, 2026-11-03 additions (user spec "development cooperation first-class"): evidence
# gating of headline calls, the synthesis of physical capabilities shared by several steps, and the facts of
# the development section from the economy's development-partnership file (scripts/93; country_devpart.R).
# Pure: tables and lists in, facts out. Needs country_brief.R, country_brief_objects.R and country_devpart.R.

# ---- Evidence gating ----------------------------------------------------------------------------------------

# Why a step's call cannot carry a headline recommendation (tb$cells$review; NA when it can), first match:
#   artifact          a known data artifact (config/development_partnerships.yml review_cells)
#   review_flag       the engine's review flag (posture core cells.fl)
#   low_confidence    posture confidence low; at Deployment, deployment-need confidence low
#   single_indicator  at Deployment, a deployment need of low confidence resting on one indicator (17B)
#   thin_anchor       a Catch Up whose pathway is corroborated by context signals only (country_devpart_pathway())
# Such calls stay on the map and in the appendix, labelled "Review / insufficient evidence"; they never enter
# the Bottom line, the Agenda, the policy-gap table or a chart's headline marks.
country_brief_review <- function(tb, dcfg) {
  cc <- tb$cells
  rc <- dcfg$review_cells %||% list()
  art <- vapply(seq_len(nrow(cc)), function(i) {
    r <- Filter(function(x) identical(x$iso3, cc$iso3[i]) && identical(x$tech, cc$tech[i]) && identical(x$stage, cc$stage[i]), rc)
    if (length(r)) r[[1]]$reason else NA_character_
  }, "")
  out <- rep(NA_character_, nrow(cc))
  for (i in seq_len(nrow(cc))) {
    if (cc$choice[i] == "not_assessed") next
    d <- tb$detail[[cc$node[i]]]
    if (cc$stage[i] == "Deployment") {
      dc <- country_brief_chr(d[["dn_confidence"]])
      nf <- country_brief_num(d[["dn_families_observed"]])
      if (identical(dc, "low")) out[i] <- if (!is.na(nf) && nf < 2) "single_indicator" else "low_confidence"
      next
    }
    out[i] <- if (!is.na(art[i])) "artifact" else if (!is.na(cc$fl[i]) && nzchar(cc$fl[i])) "review_flag" else if (identical(cc$conf[i], "low")) "low_confidence" else
      if (cc$choice[i] == "catch_up" && isTRUE(country_devpart_pathway(d[["pathway_basis"]], dcfg)$thin)) "thin_anchor" else NA_character_
  }
  tb$cells$review <- out
  tb$cells$review_note <- art
  tb
}

# ---- Shared physical capabilities ---------------------------------------------------------------------------

# The physical capabilities (AIB nodes) that sit in several of the economy's steps whose choices differ, because
# supply-chain need differs by technology (17C): one row per node with its steps, their choices and the
# national reading. nodes: country_devpart_nodes(); contributes: names of the AIB nodes the economy contributes
# to (country_brief_aib_facts()$contributes). Only material rows: a node the economy contributes to allies, or a
# BUILD node where a Secure, Preserve or Catch Up call differs from the rest (contributed first); one row per set
# of steps and choices; at most `max`.
country_brief_shared_capabilities <- function(tb, nodes, contributes = character(), max = 6L) {
  cc <- tb$cells
  rows <- list()
  for (nd in unique(nodes$node)) {
    keys <- unique(nodes$key[nodes$node == nd])
    s <- cc[cc$key %in% keys & cc$choice != "not_assessed", , drop = FALSE]
    if (nrow(s) < 2 || length(unique(s$choice)) < 2) next
    strong <- s$choice %in% c("secure", "preserve", "catch_up")
    rev_ok <- is.na(s$review)
    # the national reading: the call that is not a review case when the others are; else the need decides
    keep <- s[rev_ok, , drop = FALSE]
    reading <- if (nrow(keep) && length(unique(keep$choice)) == 1) "one" else "by_need"
    rows[[length(rows) + 1]] <- data.frame(node = nd, label = nodes$label[nodes$node == nd][1], cls = nodes$cls[nodes$node == nd][1],
                                           keys = paste(s$key, collapse = ";"), choices = paste(s$choice, collapse = ";"),
                                           reviews = paste(ifelse(is.na(s$review), "", s$review), collapse = ";"),
                                           contributes = nd %in% contributes, strong = any(strong), reading = reading,
                                           reading_choice = if (reading == "one") keep$choice[1] else NA_character_, stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(data.frame(node = character(), label = character(), keys = character(), choices = character(), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows)
  # material: a node the economy contributes to allies, or a BUILD node where a Secure, Preserve or Catch Up call
  # differs from the rest; one row per set of steps and choices
  out <- out[out$contributes | (out$strong & out$cls %in% "BUILD"), , drop = FALSE]
  out <- out[order(!out$contributes, !out$strong, out$label), , drop = FALSE]
  out <- out[!duplicated(paste(out$keys, out$choices)), , drop = FALSE]
  utils::head(out, max)
}

# The facts of one shared capability (facts$shared$<node>): its label, its steps with their choices (and
# review marks), the national reading and the AIB role, kept apart (17A: the country's own choice is one
# question, what allies rely on it for another).
country_brief_shared_facts <- function(row, tb, aibf, cfg) {
  S <- c("neis_strategy", "neis_aib")
  cc <- tb$cells
  keys <- country_brief_split(row$keys)
  ch <- country_brief_split(row$choices)
  rv <- strsplit(row$reviews, ";", fixed = TRUE)[[1]]
  rv <- c(rv, rep("", length(keys) - length(rv)))
  cname <- function(x) unname(COUNTRY_BRIEF_CHOICE_LABEL[x])
  rw <- cfg$words$review
  step_words <- vapply(seq_along(keys), function(i) {
    nm <- cc$name[match(keys[i], cc$key)]
    sprintf("%s (%s%s)", nm, cname(ch[i]), if (nzchar(rv[i])) paste0(", review: ", rw[[rv[i]]] %||% rv[i]) else "")
  }, "")
  reading <- if (identical(row$reading, "one")) {
    other <- which(ch != row$reading_choice)
    sprintf("%s; the %s call%s %s a review case", cname(row$reading_choice), paste(unique(cname(ch[other])), collapse = " and "), if (length(other) > 1) "s" else "",
            if (length(other) > 1) "are" else "is")
  } else {
    # no single cause is asserted: the steps' own measures, side by side
    f <- function(v) if (is.na(v)) "n.a." else as.character(country_brief_round(100 * v))
    ii <- match(keys, cc$key)
    same <- length(unique(cc$tech[ii])) == 1
    msr <- vapply(ii, function(i) sprintf("%s: need %s, value %s, position %s", cc$name[i], f(cc$need[i]), f(cc$prize[i]), f(cc$cp[i])), "")
    sprintf("the choices differ %s with each step's own measures (%s)", if (same) "within the same technology" else "across technologies", paste(msr, collapse = "; "))
  }
  ct <- aibf$contributes[[row$node]]
  dv <- Filter(Negate(is.null), lapply(keys, function(k) aibf$division[[k]]))
  role <- if (!is.null(ct)) {
    paste0("supplier: ", ct$share$d, " of world exports or production of ", tolower(row$label), " (the AIB measure of the product, not a step's share)",
           if (length(dv)) "; the largest allied holder of the step" else "")
  } else if (identical(row$cls, "BUILD")) "allies need new capacity here (BUILD); no supplier role" else "no AIB supplier role"
  list(
    label = country_brief_fv(row$node, row$label, S, "a physical capability (AIB node) that sits in several supply-chain steps"),
    steps = country_brief_list(step_words, S, sprintf("the steps that hold %s, with their choices", row$label), keys),
    reading = country_brief_fv(row$reading, reading, S, sprintf("the national reading of %s across its steps", row$label)),
    aib_role = country_brief_fv(row$node, role, S, sprintf("what allies rely on the economy for in %s (the Allied Industrial Base analysis)", row$label))
  )
}

# ---- Development section facts ------------------------------------------------------------------------------

# The pairs the brief's development table shows: the top development pair of each step, steps in the file's
# canonical order (country_devpart_order()), each partner once, at most `max` rows (user spec: 3-5 priority
# partnerships). pairs: devpart file pairs as a data frame.
country_brief_dev_select <- function(pairs, max = 5L) {
  d <- pairs[pairs$route == "development", , drop = FALSE]
  if (!nrow(d)) return(d)
  pick <- integer()
  for (i in seq_len(nrow(d))) {
    if (length(pick) >= max) break
    if (d$key[i] %in% d$key[pick] || d$recipient_iso3[i] %in% d$recipient_iso3[pick]) next
    pick <- c(pick, i)
  }
  d[pick, , drop = FALSE]
}

# A devpart file's column list as a data frame (NULL -> NA).
country_brief_dev_frame <- function(cols) {
  if (is.null(cols) || !length(cols)) return(data.frame())
  as.data.frame(lapply(cols, function(v) { v <- as.list(v); v[vapply(v, is.null, TRUE)] <- NA; unlist(v) }), stringsAsFactors = FALSE)
}

# The facts of the development section (facts$dev): the perspective, the counts, the selected rows (each with
# the problem, the partner, its anchor, what constrains it, what to fund, what makes it durable, what it
# retains), the steps no partner qualifies for, the commercial-route partners and the current-aid comparison.
# dev: the economy's devpart file (simplifyVector = FALSE); dcfg: config/development_partnerships.yml;
# name_of: ISO3 -> name; tb: the brief tables (step names).
country_brief_dev_facts <- function(dev, dcfg, name_of, tb, max = 5L) {
  if (is.null(dev)) return(NULL)
  S <- c("neis_strategy", "neis_aib", "oecd_crs")
  w <- dev$words
  per <- dev$perspective
  out <- list(perspective = country_brief_fv(per, per, "neis_strategy", "the development perspective of the economy (donor: high income in the AIB demand scope; recipient: not high income)"))
  if (identical(per, "none")) return(out)
  pairs <- country_brief_dev_frame(dev$pairs)
  sp <- function(x) country_brief_split(x)
  nm <- function(i) vapply(i, function(x) name_of(x), "")
  wl <- function(codes, words) unname(vapply(codes, function(k) words[[k]] %||% gsub("_", " ", k), ""))
  if (identical(per, "donor")) {
    dvp <- pairs[pairs$route == "development", , drop = FALSE]
    com <- pairs[pairs$route == "commercial", , drop = FALSE]
    out$n_pairs <- country_brief_count(nrow(dvp), S, "development partnerships: donor steps with a strategic reason matched with a developing partner's credible Catch Up in the same step")
    out$n_partners <- country_brief_count(length(unique(dvp$recipient_iso3)), S, "developing partners in the development partnerships")
    out$n_steps <- country_brief_count(length(unique(dvp$key)), S, "supply-chain steps with a development partnership")
    out$n_commercial <- country_brief_count(nrow(com), S, "pairs on the commercial route (the partner already hosts the step's new capacity in the AIB, or Preserves it)")
    out$n_review <- country_brief_count(sum(pairs$route == "review"), S, "pairs that are review cases (insufficient evidence)")
    # the donor's own need (tiers 1-4) against the allied gap alone (tier 5: the AIB BUILD signal, the same
    # for every donor in the AIB demand scope)
    tier <- suppressWarnings(as.integer(dvp$donor_tier))
    own <- dvp[!is.na(tier) & tier <= 4, , drop = FALSE]
    out$n_own_pairs <- country_brief_count(nrow(own), S, "development partnerships where the donor's own supply chain needs more trusted capacity (tiers 1-4)")
    out$n_own_partners <- country_brief_count(length(unique(own$recipient_iso3)), S, "developing partners in the donor's own-need partnerships")
    out$n_own_steps <- country_brief_count(length(unique(own$key)), S, "supply-chain steps with an own-need partnership")
    if (nrow(own)) out$own_steps <- country_brief_list(unique(own$capability), S, "the steps of the donor's own-need partnerships, in rank order", unique(own$key))
    out$n_allied_pairs <- country_brief_count(sum(!is.na(tier) & tier == 5L), S, "development partnerships that rest on the allied gap alone (tier 5: allies need new capacity, the same for every donor in scope)")
    if (nrow(com)) out$commercial <- country_brief_list(utils::head(unique(nm(com$recipient_iso3)), 8), S, "developing partners already at commercial scale in a step the economy needs (trade, offtake and investment rather than aid)")
    sel <- country_brief_dev_select(pairs, max)
    out$rows <- stats::setNames(lapply(seq_len(nrow(sel)), function(i) {
      r <- sel[i, ]
      cell <- r$capability
      partner <- name_of(r$recipient_iso3)
      anc <- wl(sp(r$catch_up_anchor_type), w$anchor)
      sup <- wl(sp(r$pathway_signals), w$support)
      cons <- wl(sp(r$recipient_constraints), w$constraint)
      unobs <- unique(wl(sp(r$recipient_unobserved), w$unobserved %||% list()))
      sig <- sp(r$donor_signals)
      role <- country_brief_dev_role(r, w)
      donor <- if (is.null(dev$country)) "the donor" else country_prose_name(dev$country)
      ties <- country_brief_dev_ties(r, w, donor)
      durable <- country_brief_dev_durable(r, w, role, donor)
      # what the donor gains: one national gain and one allied gain, the most specific of each
      nat <- intersect(c("secure_allied", "high_fence_untrusted", "partner", "high_need"), sig)
      ald <- intersect(c("aib_shortfall", "build"), sig)
      gain <- wl(c(utils::head(nat, 1), utils::head(ald, 1)), w$benefit_donor)
      Filter(Negate(is.null), list(
        key = country_brief_fv(r$key, r$key, S, "the step"),
        capability = country_brief_fv(r$key, cell, S, sprintf("the supply-chain step of the partnership (%s)", cell)),
        problem = country_brief_list(wl(setdiff(sig, "partner"), w$signal_short %||% w$signal), S, sprintf("the donor's reasons for more allied capacity in %s", cell)),
        donor_choice = country_brief_fv(r$donor_choice, unname(unlist(dcfg$words$choice[r$donor_choice])), S, sprintf("the donor's choice in %s", cell)),
        partner = country_brief_fv(r$recipient_iso3, partner, S, sprintf("the developing partner in %s", cell)),
        income = country_brief_fv(r$recipient_income_group, tolower(r$recipient_income_group), "neis_strategy", sprintf("the income group of %s", partner)),
        anchors = if (length(anc)) country_brief_list(anc, c(S, "comtrade_matrix", "cim"), sprintf("the capability anchors of %s's Catch Up in %s", partner, cell)),
        supports = if (length(sup)) country_brief_list(sup, S, sprintf("the corroborating evidence of %s's pathway in %s", partner, cell)),
        cp = country_brief_score(r$recipient_competitive_position, c(S, "comtrade_matrix"), sprintf("%s's Competitive Position in %s (0-100)", partner, cell)),
        constraints = if (length(cons)) country_brief_list(cons, S, sprintf("what the data say constrains %s's scale in %s (pathway inputs the engine observed below its bars)", partner, cell)),
        unobserved = if (length(unobs)) country_brief_list(unobs, S, sprintf("pathway inputs of %s in %s that the data do not cover (not stated as constraints)", partner, cell)),
        tools = country_brief_fv(r$recommended_tool_family, r$recommended_tool_family, c(S, "dev_tools"), sprintf("the development tools for %s in %s (by stage and constraint; income-aware)", partner, cell)),
        donor_gain = country_brief_list(gain, S, sprintf("what the donor gains from the partnership in %s", cell)),
        n_build = if (!is.na(r$aib_build_nodes)) country_brief_count(length(sp(r$aib_build_nodes)), "neis_aib", sprintf("product groups (AIB nodes) of %s where allies need new capacity (BUILD)", cell)),
        partner_gain = country_brief_fv(r$stage, unname(unlist(dcfg$benefits$recipient[r$stage])), S, sprintf("what %s builds in %s", partner, cell)),
        aid = country_brief_fv(r$oda_status, country_brief_dev_aid_words(r, w), "oecd_crs", sprintf("the donor's current aid to %s for %s (OECD CRS, %s)", partner, cell, paste(unlist(dev$meta$aid_years), collapse = "-"))),
        compact_role = if (!is.na(role)) country_brief_fv(r$recipient_compact_role, role, "neis_aib", sprintf("%s's role in the compact of %s", partner, cell)),
        ties = country_brief_list(ties, c("oecd_crs", "imf_dip", "cm_fdi", "oci_finance", "neis_aib"), sprintf("the relationship today between the donor and %s (aid at the step; economy-wide direct investment; factory projects; public energy finance; AIB roles)", partner)),
        durable = if (length(durable)) { x <- country_brief_list(durable, c(S, "dev_tools"), sprintf("what makes the supply relationship with %s in %s durable", partner, cell)); x$d <- paste(durable, collapse = "; "); x }
      ))
    }), paste0("r", seq_len(nrow(sel))))
    out$rows_order <- I(names(out$rows))
    # every step with a pair, for the appendix: its partners by route and the aid at the step
    keys <- unique(pairs$key[pairs$route %in% c("development", "commercial", "review")])
    out$by_step <- stats::setNames(lapply(keys, function(k) {
      x <- pairs[pairs$key == k, , drop = FALSE]
      cell <- x$capability[1]
      by <- function(route) { y <- x[x$route == route, , drop = FALSE]; if (nrow(y)) country_brief_list(unique(nm(y$recipient_iso3)), S, sprintf("%s pairs in %s", route, cell)) else NULL }
      aid <- x[x$oda_status %in% "recorded", , drop = FALSE]
      Filter(Negate(is.null), list(
        name = country_brief_fv(k, cell, S, "the supply-chain step"),
        reason = country_brief_list(wl(sp(x$donor_signals[1]), w$signal_short %||% w$signal), S, sprintf("the donor's reasons in %s", cell)),
        development = by("development"), commercial = by("commercial"), review = by("review"),
        review_capability = { y <- x[x$route == "review" & x$recipient_fail %in% "capability", , drop = FALSE]; if (nrow(y)) country_brief_list(unique(nm(y$recipient_iso3)), S, sprintf("review cases in %s whose position measures another product than the need", cell)) },
        review_thin_anchor = { y <- x[x$route == "review" & x$recipient_fail %in% "thin_anchor", , drop = FALSE]; if (nrow(y)) country_brief_list(unique(nm(y$recipient_iso3)), S, sprintf("review cases in %s whose pathway is corroborated by context signals only", cell)) },
        review_other = { y <- x[x$route == "review" & !x$recipient_fail %in% c("capability", "thin_anchor"), , drop = FALSE]; if (nrow(y)) country_brief_list(unique(nm(y$recipient_iso3)), S, sprintf("review cases in %s for low confidence, a review flag, evidence or the donor's own call", cell)) },
        hosts = { h <- x[x$route == "commercial" & x$aib_capacity_host %in% TRUE, , drop = FALSE]; if (nrow(h)) country_brief_list(unique(nm(h$recipient_iso3)), S, sprintf("developing partners that already host new capacity in %s in the AIB (the commercial route)", cell)) },
        n_development = country_brief_count(length(unique(x$recipient_iso3[x$route == "development"])), S, sprintf("developing partners with a credible Catch Up in %s", cell)),
        development_top = if (any(x$route == "development")) country_brief_list(utils::head(unique(nm(x$recipient_iso3[x$route == "development"])), 2), S, sprintf("the first-ranked developing partners in %s", cell)),
        aid = if (nrow(aid)) country_brief_list(unique(nm(aid$recipient_iso3)), "oecd_crs", sprintf("partners with the donor's aid recorded at %s", cell))
      ))
    }), keys)
    out$by_step_order <- I(keys)
    st <- country_brief_dev_frame(dev$steps)
    if (nrow(st)) {
      # the Partner and Secure steps (choice-based reasons, tiers 1-3) of a call that is not low confidence
      none <- st[!st$has_development %in% TRUE & st$stage != "Deployment" & suppressWarnings(as.integer(st$donor_tier)) <= 3 & !st$conf %in% "low", , drop = FALSE]
      if (nrow(none)) out$none_steps <- country_brief_list(none$step_name, S, "the donor's Partner and Secure steps for which no developing partner passes the tests")
    }
    a <- dev$aid
    gap <- country_brief_dev_frame(a$gap)
    out$aid_reporting <- country_brief_fv(isTRUE(a$reporting), if (isTRUE(a$reporting)) "reports" else "does not report", "oecd_crs", "whether the donor reports purpose-coded aid to the OECD CRS")
    g <- function(cls, m) { x <- gap[gap$gap == cls, , drop = FALSE]; if (!nrow(x)) NULL else country_brief_list(unique(nm(x$recipient_iso3)), "oecd_crs", m) }
    out$aid_aligned <- g("aligned", "development partners that already receive the donor's aid at the step of the partnership, coded to the technology")
    out$aid_stage_wide <- g("stage_wide", "development partners whose aid from the donor at the step is under stage-wide codes only (the technology is not identified)")
    out$aid_wrong_stage <- g("wrong_stage", "development partners whose aid from the donor sits at other steps only")
    out$aid_none <- g("no_support", "development partners with no aid from the donor recorded at any supply-chain step (energy, industry and mining purpose codes only)")
    out$aid_graduate <- g("graduate", "commercial-route partners still receiving the donor's technology-coded aid at the step")
    top <- country_brief_dev_frame(a$top)
    if (nrow(top)) {
      out$aid_top <- country_brief_list(nm(top$recipient_iso3)[1:min(5, nrow(top))], "oecd_crs", "the donor's largest recipients of energy, industry and mining aid")
      noaib <- top[!top$paired %in% TRUE, , drop = FALSE]
      if (nrow(noaib)) out$aid_no_aib <- country_brief_list(nm(noaib$recipient_iso3), "oecd_crs", "the donor's largest aid recipients with no partnership in this analysis (aid outside the energy-industrial objective, not judged)")
      out$aid_top_1 <- country_brief_fv(top$recipient_iso3[1], name_of(top$recipient_iso3[1]), "oecd_crs", "the donor's largest recipient of energy, industry and mining aid")
      out$aid_top_1_usd <- country_brief_fv(top$oda_all[1], sprintf("$%s million", format(country_brief_round(top$oda_all[1]), big.mark = ",")), "oecd_crs",
                                            sprintf("the donor's energy, industry and mining aid to its largest recipient, %s", paste(unlist(dev$meta$aid_years), collapse = "-")))
    }
    out$aid_years <- country_brief_fv(paste(unlist(dev$meta$aid_years), collapse = "-"), paste(unlist(dev$meta$aid_years), collapse = " to "), "oecd_crs", "the years of the aid data")
  } else {
    st <- country_brief_dev_frame(dev$steps)
    ok <- st[st$recipient_ok %in% TRUE, , drop = FALSE]
    out$n_catch_up <- country_brief_count(nrow(st), S, "the economy's Catch Up steps")
    out$n_credible <- country_brief_count(nrow(ok), S, "Catch Up steps that pass the development test")
    out$rows <- stats::setNames(lapply(seq_len(nrow(ok)), function(i) {
      s <- ok[i, ]
      dd <- pairs[pairs$key == s$key & pairs$route %in% c("development", "commercial"), , drop = FALSE]
      own <- dd[suppressWarnings(as.integer(dd$donor_tier)) <= 4, , drop = FALSE]
      cons <- c(wl(sp(s$constraints), w$constraint), unique(wl(sp(s$unobserved), w$unobserved %||% list())))
      gain <- unique(unlist(lapply(sp(paste(utils::head(own$donor_signals, 4), collapse = ";")), function(z) w$benefit_donor[[z]])))
      Filter(Negate(is.null), list(
        key = country_brief_fv(s$key, s$key, S, "the step"),
        capability = country_brief_fv(s$key, s$step_name, S, "the Catch Up step"),
        anchors = country_brief_list(wl(sp(s$anchors), w$anchor), S, sprintf("the capability anchors of %s", s$step_name)),
        constraints = if (length(cons)) country_brief_list(cons, S, sprintf("what constrains scale in %s (observed pathway inputs; inputs the data do not cover say so)", s$step_name)),
        partner_gain = if (length(gain)) country_brief_list(utils::head(gain, 2), S, sprintf("what the developed partners gain in %s", s$step_name)),
        builds = country_brief_fv(s$stage, unname(unlist(dcfg$benefits$recipient[s$stage])), S, sprintf("what the economy builds in %s", s$step_name)),
        partners = if (nrow(own)) country_brief_list(nm(utils::head(own$donor_iso3, 4)), S, sprintf("developed partners with their own need for %s", s$step_name)),
        tools = if (nrow(dd)) country_brief_fv(dd$recommended_tool_family[1], dd$recommended_tool_family[1], c(S, "dev_tools"), sprintf("the useful support in %s", s$step_name))
      ))
    }), paste0("r", seq_len(nrow(ok))))
    out$rows_order <- I(names(out$rows))
  }
  out
}

# The pair's existing aid in words (missing stays missing).
country_brief_dev_aid_words <- function(r, w) {
  s <- r$oda_status
  usd <- function(v) if (is.na(v)) "no" else if (v < 0.1) "under $0.1 million" else sprintf("$%s million", format(country_brief_round(v, if (v < 10) 1 else 0), nsmall = if (v < 10) 1 else 0))
  if (identical(s, "not_reporting")) return(w$aid_status$not_reporting)
  if (is.na(s) || identical(s, "none")) return(w$aid_status$none)
  if (identical(s, "other_stage")) return(sprintf("%s of energy, industry and mining aid, none at this step", usd(as.numeric(r$oda_pair_total))))
  if (identical(s, "proxy")) return("no recycling purpose code (proxy only)")
  how <- switch(as.character(r$oda_specificity %||% NA), technology = "mostly coded to the technology", shared_technology = "mostly under codes naming the technology with others",
                proxy = "mostly under a code that only approximates the technology", "under stage-wide codes (the technology is not identified)")
  sprintf("%s at this step, %s", usd(as.numeric(r$current_oda)), how)
}

# A US$ million amount in words.
country_brief_dev_usd <- function(v) if (v < 0.1) "under $0.1 million" else sprintf("$%s million", format(country_brief_round(v, if (v < 10) 1 else 0), nsmall = if (v < 10) 1 else 0, big.mark = ","))

# The recipient's compact role in the compact of the pair's own step (never a role in another compact of the
# technology), as "<role> in the <compact name> compact"; NA when none.
country_brief_dev_role <- function(r, w) {
  if (is.na(r$recipient_compact_role)) return(NA_character_)
  own <- country_brief_split(r$compact)
  parts <- country_brief_split(r$recipient_compact_role)
  id <- sub(":.*$", "", parts)
  role <- sub("^[^:]+: ", "", parts)
  keep <- id %in% own
  if (!any(keep)) return(NA_character_)
  cn <- vapply(id[keep], function(k) { v <- w$compact[[k]]; if (is.null(v)) k else as.character(v) }, "")
  paste(sprintf("%s in the %s compact", role[keep], cn), collapse = "; ")
}

# The relationship today (spec 7): aid at the step, the economy-wide direct-investment position (partner
# reported when derived; the conduit caveat), CM-FDI factory projects and OCI public energy finance at the step
# (observed zeros as "none recorded"; missing data left out), and the AIB simulated investment pairing.
country_brief_dev_ties <- function(r, w, donor) {
  num <- function(x) suppressWarnings(as.numeric(x))
  dip <- num(r$imf_dip_tie); cm <- num(r$cm_fdi_projects); oci <- num(r$oci_finance)
  c(country_brief_dev_aid_words(r, w),
    if (!is.na(dip) && dip <= 0) sprintf("a net direct investment position %s (economy-wide%s)", if (dip < 0) "below zero" else "of zero",
                                         if (identical(r$imf_dip_basis, "derived")) ", reported by the partner" else ""),
    if (!is.na(dip) && dip > 0) sprintf("a direct investment position of %s (economy-wide%s%s)", country_brief_dev_usd(dip),
                             if (identical(r$imf_dip_basis, "derived")) ", reported by the partner" else "",
                             if (!is.na(r$imf_dip_conduit) && !identical(r$imf_dip_conduit, "none")) "; may pass through a financial centre" else ""),
    if (!is.na(cm)) (if (cm > 0) sprintf("%d factory project%s by firms from %s in the technology", as.integer(cm), if (cm == 1) "" else "s", donor) else sprintf("no factory projects by firms from %s recorded in the technology", donor)),
    if (!is.na(oci)) (if (oci > 0) sprintf("%s of export-credit or lender finance at the step", country_brief_dev_usd(oci)) else "no export-credit or lender finance recorded at the step"),
    if (isTRUE(as.logical(r$aib_pairing))) "an AIB simulated investment pairing (a model result, not an existing tie)")
}

# What makes the relationship durable (spec 7 Q6), from the pair's own data: the scale instruments of the
# recommended tools, the recipient's compact role at the step, and deployment demand for the technology.
country_brief_dev_durable <- function(r, w, role, donor) {
  tl <- tolower(r$recommended_tool_family %||% "")
  inst <- c(if (grepl("offtake", tl)) "long-term offtake", if (grepl("joint venture", tl)) "joint ventures", if (grepl("guarantee", tl)) "investment guarantees")
  c(if (length(inst)) paste0(country_brief_join(inst), " as the capability scales"),
    if (!is.na(role)) sprintf("the partner's role as %s", role),
    if (r$demand_anchor %in% c("allied", "both")) sprintf("%s own deployment demand for the technology (Accelerate)", country_poss(donor)),
    if (r$demand_anchor %in% c("local", "both")) "the partner's own deployment demand for the technology (Accelerate)")
}

# The development table of the brief (donor: the user's six columns; recipient: the inverse six), the appendix
# table of every development and commercial pair, the review cases, and the shared capabilities.
country_brief_dev_tables <- function(facts, dev, cfg, name_of) {
  out <- list()
  t <- function(path, filter = NULL) country_brief_tok(facts, path, filter)
  rows <- facts$dev$rows_order %||% character()
  if (identical(facts$dev$perspective$v, "donor")) {
    out$development <- list(head = c("Capability / allied gap", "Developing partner", "Why this partner can Catch Up", "What the donor should support", "Security / AIB benefit", "Development benefit"),
                            rows = lapply(rows, function(r) {
                              p <- function(f, filter = NULL) t(paste("dev.rows", r, f, sep = "."), filter)
                              c(paste0(p("capability", "cap"), ": ", p("problem")), paste0(p("partner"), " (", p("income"), ")"),
                                paste0(p("anchors", "cap"), if (!is.null(facts$dev$rows[[r]]$supports)) paste0("; corroborated by ", p("supports")) else ""),
                                paste0(if (!is.null(facts$dev$rows[[r]]$constraints)) paste0("Constraint: ", p("constraints"), ". ") else "",
                                       if (!is.null(facts$dev$rows[[r]]$unobserved)) paste0(p("unobserved", "cap"), ". ") else "", p("tools", "cap")),
                                paste0(p("donor_gain", "cap"), if (!is.null(facts$dev$rows[[r]]$n_build)) paste0(" (allies need new capacity in ", p("n_build"), if (identical(as.numeric(facts$dev$rows[[r]]$n_build$v), 1)) " product group" else " product groups", " of the step)") else ""),
                                p("partner_gain", "cap"))
                            }))
  } else if (identical(facts$dev$perspective$v, "recipient")) {
    out$development <- list(head = c("Catch Up capability", "What constrains scale", "Potential development partners", "Useful support", "What the partners gain", "What it builds at home"),
                            rows = lapply(rows, function(r) {
                              p <- function(f, filter = NULL) t(paste("dev.rows", r, f, sep = "."), filter)
                              has <- function(f) !is.null(facts$dev$rows[[r]][[f]])
                              c(p("capability", "cap"), if (has("constraints")) p("constraints", "cap") else "\u2014", if (has("partners")) p("partners") else "\u2014",
                                if (has("tools")) p("tools", "cap") else "\u2014", if (has("partner_gain")) p("partner_gain", "cap") else "\u2014", p("builds", "cap"))
                            }))
  } else out$development <- list(head = c("Development partnerships"), rows = list(c("No development-partnership perspective for this economy.")))
  if (identical(facts$dev$perspective$v, "donor") && length(rows))
    out$development_ties <- list(head = c("Partnership", "Relationship today", "What makes it durable"),
                                 rows = lapply(rows, function(r) {
                                   p <- function(f, filter = NULL) t(paste("dev.rows", r, f, sep = "."), filter)
                                   c(paste0(p("partner"), ": ", p("capability")), p("ties", "cap"),
                                     if (!is.null(facts$dev$rows[[r]]$durable)) p("durable", "cap") else "\u2014")
                                 }))
  bs <- facts$dev$by_step_order %||% character()
  out$development_table <- list(head = c("Step", "Donor reason", "Development partners", "Commercial route", "Review cases", "Aid at the step"),
                                rows = lapply(bs, function(k) {
                                  p <- function(f, filter = NULL) t(paste("dev.by_step", k, f, sep = "."), filter)
                                  c(p("name", "cap"), p("reason", "cap"), p("development"), p("commercial"), p("review"), p("aid"))
                                }))
  out
}
