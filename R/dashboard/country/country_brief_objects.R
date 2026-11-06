# The structured objects of a country strategy brief, built from the same tidy tables as its facts
# (country_brief.R): the configured policy design of each step, the deployment ranking (Figure 4), the
# strategy-policy gaps (the policy section's table), the low-fence cases, the compact x role matrix of the
# Allied Industrial Base (Figure 3), the generated tables (@table <id> in the source: first-class objects
# whose cells are facts, so they carry claims like the prose) and the content trace (each claim with its
# dashboard field, source dataset and evidence quality). Pure: lists in, lists out.

# ---- Extra facts of a step ------------------------------------------------------------------------------

# The configured policy design (posture core design["variant|delivery"]: fence, gate, build, benchmark,
# exit), the policy-alignment note and gap, the partner evidence and, at deployment, the main driver of the
# deployment need.
country_brief_cell_extra <- function(row, d, design, cfg) {
  w <- cfg$words
  S <- "neis_strategy"
  cell <- row$name
  f <- list()
  key <- paste(row$variant, d[["delivery"]] %||% "", sep = "|")
  ds <- design[[key]]
  if (!is.null(ds)) {
    f$design <- Filter(Negate(is.null), lapply(stats::setNames(c("fence", "gate", "build", "benchmark", "exit"), c("fence", "gate", "build", "benchmark", "exit")), function(k) {
      x <- country_brief_chr(ds[[k]])
      if (is.na(x)) NULL else country_brief_fv(x, x, c(S, "posture_design"), sprintf("the configured %s of the policy design for %s (%s)", k, cell, key))
    }))
  }
  pol <- d[["policy"]]
  flag <- country_brief_chr(pol[["flag"]])
  if (!is.na(flag)) f$policy_flag <- country_brief_word(flag, w$alignment_flag, c(S, "gta_nipo", "iea_pams"), sprintf("the policy-alignment note of %s", cell))
  g <- country_brief_gap_code(row$choice, country_brief_chr(pol[["alignment"]]))
  if (!is.na(g)) f$gap <- country_brief_word(g, w$gap, c(S, "gta_nipo", "iea_pams"), sprintf("what the mismatch between the choice and recorded policy for %s means", cell))
  if (!is.null(d[["partner_evidence"]])) f$partner_evidence <- country_brief_word(d[["partner_evidence"]], w$partner_evidence, c(S, "neis_aib"), sprintf("evidence behind the partner recommendation for %s", cell))
  if (row$stage == "Deployment") {
    fam <- c("dn_demand", "dn_policy_gap", "dn_system")
    vals <- vapply(fam, function(k) country_brief_num(d[[k]][["value"]]), 0)
    if (any(!is.na(vals))) f$dn_driver <- country_brief_word(fam[which.max(vals)], w$dn_family, c(S, "ember", "iea_weo"), sprintf("the largest of the deployment-need measures of %s", cell))
  }
  f
}

# The gap of a step's choice against recorded policy: Secure or a build choice with weak policy, Partner with
# localization, Watch with protection, Accelerate with weak deployment policy; NA otherwise.
country_brief_gap_code <- function(choice, alignment) {
  if (is.na(alignment)) return(NA_character_)
  if (alignment == "under_prioritized" && identical(choice, "secure")) return("secure_under")
  if (alignment %in% c("under_prioritized", "misaligned", "over_prioritized", "deployment_gap")) return(alignment)
  NA_character_
}

# ---- Rankings and selections ------------------------------------------------------------------------------

# The deployment steps by deployment need, with their choice, main driver and confidence (Figure 4).
country_brief_deployment_rank <- function(tb, cfg) {
  cc <- tb$cells[tb$cells$stage == "Deployment" & !is.na(tb$cells$need), , drop = FALSE]
  fam <- c("dn_demand", "dn_policy_gap", "dn_system")
  out <- do.call(rbind, lapply(seq_len(nrow(cc)), function(i) {
    d <- tb$detail[[cc$node[i]]]
    vals <- vapply(fam, function(k) country_brief_num(d[[k]][["value"]]), 0)
    data.frame(key = cc$key[i], node = cc$node[i], tech = cc$tech[i], name = cc$name[i], dn = cc$need[i], choice = cc$choice[i],
               driver = if (any(!is.na(vals))) unname(unlist(cfg$words$dn_family[fam[which.max(vals)]])) else NA_character_,
               families = country_brief_num(d[["dn_families_observed"]]), confidence = country_brief_chr(d[["dn_confidence"]]),
               review = if (is.null(cc$review)) NA_character_ else cc$review[i], stringsAsFactors = FALSE)
  }))
  out[order(-out$dn), , drop = FALSE]
}

# The strategy-policy gaps worth a government's attention: the mismatches of country_brief_gap_code(), most
# consequential first (Secure, then Partner with localization, build choices with weak policy, deployment,
# Watch with protection; within each, by value or deployment need), leaving out choices of low confidence
# (a weak case does not make a strong gap) and every other review case (country_brief_review()); at most `per_kind` of each kind, so one kind cannot crowd out
# the rest, and at most `max` rows. Returns the rows with their category.
country_brief_policy_gaps <- function(tb, cfg, max = cfg$brief$policy_gaps_max %||% 10, per_kind = 3) {
  cc <- tb$cells
  cat_order <- c(secure_under = 1, misaligned = 2, under_prioritized = 3, deployment_gap = 4, over_prioritized = 5)
  rows <- list()
  for (i in seq_len(nrow(cc))) {
    d <- tb$detail[[cc$node[i]]]
    g <- country_brief_gap_code(cc$choice[i], country_brief_chr(d[["policy"]][["alignment"]]))
    if (is.na(g)) next
    conf <- if (cc$stage[i] == "Deployment") country_brief_chr(d[["dn_confidence"]]) else cc$conf[i]
    if (identical(conf, "low")) next
    # a review case (country_brief_review(): low confidence, a review flag, a thin anchor, a data artifact)
    if (!is.null(cc$review) && !is.na(cc$review[i])) next
    weight <- if (cc$stage[i] == "Deployment") cc$need[i] else cc$prize[i]
    rows[[length(rows) + 1]] <- data.frame(key = cc$key[i], node = cc$node[i], name = cc$name[i], gap = g, order = cat_order[[g]],
                                           weight = weight %||% 0, alc = country_brief_chr(d[["policy"]][["alignment_confidence"]]), stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(data.frame(key = character(), node = character(), name = character(), gap = character(), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows)
  out <- out[order(out$order, -out$weight), , drop = FALSE]
  out <- out[stats::ave(seq_len(nrow(out)), out$gap, FUN = seq_along) <= per_kind, , drop = FALSE]
  utils::head(out, max)
}

# The low-fence cases worth naming: processing and manufacturing steps left open (Watch; mining is a
# resource question, not a restraint decision) that the energy system needs, that recorded policy treats as
# a priority, or whose value is high, leaving out review cases (country_brief_review()). Ranked: the technology's deployment is Accelerate (with more than low
# confidence) first, then over-prioritized, then by value.
country_brief_low_cases <- function(tb, th, max = 5) {
  cc <- tb$cells
  w <- cc[cc$choice == "watch" & !cc$stage %in% c("Deployment", "Extraction") & (if (is.null(cc$review)) TRUE else is.na(cc$review)), , drop = FALSE]
  al <- vapply(w$node, function(k) country_brief_chr(tb$detail[[k]][["policy"]][["alignment"]]), "")
  dep <- vapply(w$tech, function(t) {
    k <- paste(t, "Deployment", sep = "|")
    i <- match(k, cc$node)
    !is.na(i) && cc$choice[i] == "accelerate" && !identical(country_brief_chr(tb$detail[[k]][["dn_confidence"]]), "low") && (is.null(cc$review) || is.na(cc$review[i]))
  }, TRUE)
  keep <- dep | al %in% "over_prioritized" | (!is.na(w$prize) & w$prize >= (th$prize %||% 0.6))
  score <- 2 * dep + (al %in% "over_prioritized") + ifelse(is.na(w$prize), 0, w$prize)
  w$deploy_accelerate <- dep
  w <- w[keep, , drop = FALSE][order(-score[keep]), , drop = FALSE]
  utils::head(w, max)
}

# ---- The Allied Industrial Base matrix (Figure 3) ----------------------------------------------------------

# One row per strategic compact in which the economy holds a role or relies on partners; one column per
# role group (config aib_matrix): level essential (a core seat, or a supplier role on a step where it is the
# allies' single point of failure), role (holds the function, substitutable), relies (partner or Secure steps
# of the compact sourced from trusted partners) or gap (relies, and allied supply falls short).
country_brief_aib_matrix <- function(aib, managed, tb, iso3, name_of, cfg, division = data.frame()) {
  st <- aib$strategic
  sc <- st$compacts
  roles <- Filter(function(r) identical(r$iso3, iso3), st$compactRoles)
  node_label <- stats::setNames(vapply(aib$nodes, function(n) as.character(n$label), ""), vapply(aib$nodes, function(n) as.character(n$id), ""))
  for (k in names(cfg$words$aib_node)) node_label[[k]] <- cfg$words$aib_node[[k]]
  hosts <- c(Filter(function(h) identical(h$host, iso3), aib$investmentFlows$hosts %||% list()),
             Filter(function(h) identical(h$host, iso3), managed$investmentFlows$hosts %||% list()))
  cols <- cfg$aib_matrix$columns
  out <- list()
  add <- function(id, col, level, text) out[[length(out) + 1]] <<- data.frame(compact = id, col = col, level = level, text = text, stringsAsFactors = FALSE)
  for (s in sc) {
    id <- s$id
    r <- Filter(function(x) identical(x$compact, id), roles)
    r <- if (length(r)) r[[1]] else NULL
    core <- iso3 %in% vapply(s$core %||% list(), function(x) as.character(x$iso3), "")
    nodes <- unlist(lapply(s$steps, function(x) unlist(x$nodes)))
    fn <- unlist(r$functions %||% list())
    # supplier: the lead step and share; essential with a core seat or the allies' single point of failure.
    # A demand anchor's lead share is a share of world imports: it goes in the demand cell, not the supplier one.
    imp <- identical(r$leadBasis, "share of world imports")
    share <- if (!is.null(r$leadShare)) country_brief_pct_text(r$leadShare) else NULL
    step <- if (!is.null(r$leadNode)) unname(node_label[r$leadNode]) %||% r$leadStep else r$leadStep %||% r$role
    if (any(fn %in% unlist(cols$supplier$functions))) {
      spof <- nrow(division) && any(division$node %in% unlist(cfg$compacts[[id]]$division) & division$gap %in% "single_point_of_failure")
      lead <- if (imp) intersect(unlist(cols$supplier$functions), fn)[1] else step %||% r$role
      text <- paste0(country_brief_cap1(lead), if (!is.null(share) && !imp) paste0(" ", share) else "", if (core) "; core seat" else "")
      add(id, "supplier", if (core || spof) "essential" else "role", text)
    }
    # capacity host: a ranked host of a node of the compact (either scenario), else the capable-host function
    hn <- Filter(function(h) h$node %in% nodes, hosts)
    if (length(hn)) {
      best <- hn[[which.min(vapply(hn, function(h) as.numeric(h$rank), 0))]]
      add(id, "host", "role", sprintf("%s host: %s", country_brief_ordinal(best$rank), country_brief_lc1(unname(node_label[best$node]) %||% best$node)))
    } else if (any(fn %in% unlist(cols$host$functions))) add(id, "host", "role", "Capable host")
    if (any(fn %in% unlist(cols$demand$functions))) add(id, "demand", "role", paste0("Demand anchor", if (imp && !is.null(share)) sprintf(": %s of world imports", share) else ""))
    if (any(fn %in% unlist(cols$bridge$functions))) add(id, "bridge", "role", paste(intersect(unlist(cols$bridge$functions), fn)[1]))
    # relies: the compact's Partner and Secure steps (and its Secure layers' steps) sourced from partners
    m <- cfg$compacts[[id]] %||% list()
    cc <- tb$cells
    pick <- (cc$tech %in% unlist(m$techs) | cc$node %in% unlist(m$steps)) & !(cc$node %in% unlist(m$exclude))
    lay <- tb$layers$node[tb$layers$layer_id %in% unlist(m$layers) & tb$layers$choice == "secure"]
    pick <- pick & (cc$choice %in% c("partner", "secure") | cc$node %in% lay) & cc$stage != "Deployment"
    dep <- cc[pick, , drop = FALSE]
    if (nrow(dep)) {
      gap <- any(vapply(dep$node, function(k) identical(tb$detail[[k]][["partner_aib_status"]], "gap"), TRUE))
      ps <- unique(unlist(lapply(dep$node, function(k) vapply(tb$detail[[k]][["partners"]] %||% list(), function(p) as.character(p$iso3), ""))))
      text <- if (length(ps)) paste(name_of(utils::head(ps, 3)), collapse = ", ") else "no credible allied solution yet"
      add(id, "relies", if (gap) "gap" else "relies", text)
    }
  }
  if (!length(out)) return(data.frame(compact = character(), name = character(), col = character(), level = character(), text = character()))
  df <- do.call(rbind, out)
  nm <- stats::setNames(vapply(sc, function(s) s$name, ""), vapply(sc, function(s) s$id, ""))
  df$name <- unname(nm[df$compact])
  # rows: essential first, then by number of roles held, then relies-only
  score <- tapply(ifelse(df$level == "essential", 10, ifelse(df$col == "relies", 0.5, 1)), df$compact, sum)
  df$row_order <- unname(rank(-score[df$compact], ties.method = "first"))
  df
}

# A label lower-cased for use after a colon or mid-sentence: only its first letter, and not when it starts an
# acronym (AC generators, PGMs, PV cover glass); acronyms later in the label (GOES, DRI/HBI) keep their case.
country_brief_lc1 <- function(s) ifelse(grepl("^[A-Z][a-z]", s), paste0(tolower(substr(s, 1, 1)), substring(s, 2)), s)

country_brief_ordinal <- function(n) {
  n <- as.integer(n)
  suf <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), `1` = "st", `2` = "nd", `3` = "rd", "th")
  paste0(n, suf)
}

# The matrix as facts (aib.matrix.<compact>.<column>: the level and its words), for the prose and the trace.
country_brief_matrix_facts <- function(mx, cfg) {
  if (!nrow(mx)) return(list())
  lv <- cfg$words$matrix_level
  out <- list()
  for (id in unique(mx$compact)) {
    r <- mx[mx$compact == id, , drop = FALSE]
    cells <- stats::setNames(lapply(seq_len(nrow(r)), function(i) list(
      level = country_brief_word(r$level[i], lv, "neis_aib", sprintf("the economy's standing in the %s column for %s", r$col[i], r$name[i])),
      text = country_brief_fv(r$text[i], r$text[i], c("neis_aib", "neis_strategy"), sprintf("what the %s column of %s shows", r$col[i], r$name[i])))), r$col)
    out[[id]] <- c(list(name = country_brief_fv(id, r$name[1], "neis_aib", "strategic compact")), cells)
  }
  out
}

# ---- Generated tables -------------------------------------------------------------------------------------

# A cell of a generated table: a token when the fact exists, else an en dash.
country_brief_tok <- function(facts, path, filter = NULL) {
  x <- country_memo_get(facts, path)
  if (is.null(x) || (is.list(x) && !is.null(x$d) && !nzchar(x$d))) return("–")
  paste0("{{", path, if (!is.null(filter)) paste0("|", filter) else "", "}}")
}

# The generated tables of a brief (config brief.tables), as rows of source text whose cells are tokens.
country_brief_generated_tables <- function(facts, tb, cfg) {
  k <- function(key, field, filter = NULL) country_brief_tok(facts, paste("cells", key, field, sep = "."), filter)
  cc <- tb$cells
  out <- list()
  # the strategy-policy gaps
  gaps <- facts$policy_gaps_order %||% character()
  out$policy_gaps <- list(head = c("Capability", "Strategic choice", "Current policy", "Gap"),
                          rows = lapply(gaps, function(key) c(k(key, "name", "cap"), k(key, "choice"),
                                                              if (!is.null(facts$cells[[key]]$policy_flag)) k(key, "policy_flag", "cap") else k(key, "alignment", "cap"),
                                                              k(key, "gap"))))
  # every step of the Strategy Map
  fence_of <- function(key) {
    f <- facts$cells[[key]]
    if (!is.null(f$yard)) k(key, "yard") else if (!is.null(f$fence)) k(key, "fence") else "–"
  }
  out$strategy_table <- list(head = c("Step", "Stage", "Choice", "Fence", "Need", "Value", "Position", "Evidence", "Policy"),
                             rows = lapply(cc$key, function(key) {
                               dep <- !is.null(facts$cells[[key]]$dn)
                               c(k(key, "name", "cap"), k(key, "stage_id"), k(key, "choice"), fence_of(key), if (dep) k(key, "dn") else k(key, "need"),
                                 k(key, "siv"), k(key, "cp"), k(key, "evidence"), k(key, "alignment"))
                             }))
  # every named layer
  ly <- tb$layers[tb$layers$layer_id != "rest", , drop = FALSE]
  out$layer_table <- list(head = c("Layer", "Step", "Choice", "Fence"),
                          rows = lapply(seq_len(nrow(ly)), function(i) {
                            key <- cc$key[match(ly$node[i], cc$node)]
                            p <- paste("layers", ly$layer_id[i], sep = ".")
                            c(k(key, paste0(p, ".name")), k(key, "name"), k(key, paste0(p, ".choice")), k(key, paste0(p, ".fence")))
                          }))
  # partner recommendations
  pk <- cc$key[vapply(cc$key, function(key) !is.null(facts$cells[[key]]$partners) || !is.null(facts$cells[[key]]$partner_gap), TRUE)]
  out$partner_table <- list(head = c("Step", "Choice", "Trusted partners", "Allied supply", "Evidence"),
                            rows = lapply(pk, function(key) c(k(key, "name", "cap"), k(key, "choice"),
                                                              if (!is.null(facts$cells[[key]]$partners)) k(key, "partners") else k(key, "partner_gap", "cap"),
                                                              k(key, "aib_status", "cap"), k(key, "partner_evidence"))))
  # compact roles
  cp <- names(facts$aib$compacts %||% list())
  out$compact_table <- list(head = c("Strategic compact", "Lead role", "Functions", "Lead step", "Core countries"),
                            rows = lapply(cp, function(id) {
                              a <- function(f, filter = NULL) country_brief_tok(facts, paste("aib.compacts", id, f, sep = "."), filter)
                              c(a("name"), a("role", "cap"), a("functions", "cap"), a("lead_step", "cap"), a("core"))
                            }))
  # the review cases (Review / insufficient evidence): kept out of the headlines, listed here
  rv <- facts$review_order %||% character()
  out$review_table <- list(head = c("Step", "Choice", "Why it is a review case"),
                           rows = lapply(rv, function(key) c(k(key, "name", "cap"), k(key, "choice"), k(key, "review", "cap"))))
  # physical capabilities shared by several steps whose choices differ (17C)
  sh <- facts$shared_order %||% character()
  out$shared_table <- list(head = c("Shared capability", "Steps and their choices", "National reading", "AIB role"),
                           rows = lapply(sh, function(id) {
                             s <- function(f, filter = NULL) country_brief_tok(facts, paste("shared", id, f, sep = "."), filter)
                             c(s("label", "cap"), s("steps"), s("reading", "cap"), s("aib_role", "cap"))
                           }))
  # data coverage and freshness
  out$coverage_table <- list(head = c("Item", "Value"), rows = list(
    c("Data release", "{{meta.as_of_label}}"),
    c("Steps with a choice", "{{counts.assessed}}"),
    c("Steps not assessed", "{{counts.not_assessed}}"),
    c("Named layers with a choice", "{{counts.layers_assessed}}"),
    c("Steps with strong position evidence", "{{counts.ev_high}}"),
    c("Steps with moderate or weak position evidence", "{{counts.ev_partial}}"),
    c("Steps with no position evidence", "{{counts.ev_none}}")))
  out
}

# ---- The content trace ------------------------------------------------------------------------------------

# The dashboard field behind a fact path (the bundle and field a reader can open).
country_brief_field_origin <- function(path) {
  rules <- list(
    c("^cells\\.[^.]+\\.(name|tech|stage)$", "posture core meta.phrases / meta.stage_display"),
    c("^cells\\.[^.]+\\.choice$", "posture core cells.p, m, dm (the dashboard's choice)"),
    c("^cells\\.[^.]+\\.fence$", "posture core cells choice -> fence (grammar.ts nodeFence)"),
    c("^cells\\.[^.]+\\.yard$", "posture core cells.yd (yard roll-up)"),
    c("^cells\\.[^.]+\\.need$", "posture core cells.need (supply-chain need)"),
    c("^cells\\.[^.]+\\.siv$", "posture core cells.prize (Strategic Industrial Value)"),
    c("^cells\\.[^.]+\\.market$", "posture core cells.io (market prize)"),
    c("^cells\\.[^.]+\\.cp$", "posture core cells.cp (Competitive Position among contenders)"),
    c("^cells\\.[^.]+\\.share$", "posture detail cells[].home_share"),
    c("^cells\\.[^.]+\\.(anchors|supports|pathway)$", "posture detail cells[].pathway_basis, pathway_credible"),
    c("^cells\\.[^.]+\\.(dn|dn_demand|dn_policy_gap|dn_system|dn_families|dn_confidence|dn_driver)$", "posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence"),
    c("^cells\\.[^.]+\\.target\\.", "posture detail cells[].dn_policy_gap.note (national target)"),
    c("^cells\\.[^.]+\\.deployment_(trend|years)$", "posture detail cells[].deployment_trend"),
    c("^cells\\.[^.]+\\.(alignment|protection|policy_priority|policy_flag|gap)$", "posture detail cells[].policy (alignment, priority, protection, flag)"),
    c("^cells\\.[^.]+\\.(partners|partner_1|aib_status|partner_gap|partner_evidence)$", "posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence"),
    c("^cells\\.[^.]+\\.design\\.", "posture core design[variant|delivery] (configured policy design)"),
    c("^cells\\.[^.]+\\.layers\\.", "posture detail cells[].yard_children; core meta.yf_nodes labels"),
    c("^cells\\.[^.]+\\.(rationale|delivery|evidence|confidence)$", "posture detail cells[].security_rationale, delivery; core cells.ev, conf"),
    c("^cells\\.[^.]+\\.review$", "derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells"),
    c("^shared\\.", "derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities())"),
    c("^dev\\.", "devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison"),
    c("^cells\\.[^.]+\\.", "posture detail cells[] (need components and trade)"),
    c("^high_fence\\.", "derived: Secure steps and layers grouped by config high_fence_groups"),
    c("^counts\\.", "derived: counts over the posture core and detail"),
    c("^lists\\.", "derived: lists over the posture core and detail"),
    c("^meta\\.bars\\.", "posture core meta.thresholds"),
    c("^meta\\.", "Country manifest as_of_label"),
    c("^aib\\.matrix\\.", "derived: AIB strategic.compactRoles, investmentFlows.hosts and posture partner recommendations (Figure 3)"),
    c("^aib\\.compacts\\.", "AIB strategic.compactRoles and strategic.compacts[].core"),
    c("^aib\\.division\\.", "posture core division (allied division of labor)"),
    c("^aib\\.recommended_", "posture core cells.p1 (best-placed trusted partner of every economy)"),
    c("^aib\\.investment_", "AIB investmentFlows.pairs (Decoupling; Managed Interdependence file)"),
    c("^aib\\.lost_usd$", "AIB network.countries[].lostUsd"),
    c("^aib\\.scenarios\\.", "AIB strategic.scenarios"),
    c("^aib\\.ws_", "AIB strategic.countries[].metrics; scenario-managed compacts[].coalition"),
    c("^aib\\.", "AIB strategic.countries[].slots and items"))
  for (r in rules) if (grepl(r[1], path, perl = TRUE)) return(r[2])
  "brief facts"
}

# The evidence quality a claim rests on: the step's position evidence and confidence, its deployment-need
# confidence, its partner evidence; the AIB model for AIB facts.
country_brief_evidence_of <- function(path, facts) {
  m <- regmatches(path, regexec("^cells\\.([^.]+)\\.", path))[[1]]
  if (length(m) == 2) {
    f <- facts$cells[[m[2]]]
    bits <- c(if (!is.null(f$evidence)) paste("position evidence", f$evidence$d), if (!is.null(f$confidence)) paste("choice confidence", f$confidence$d),
              if (!is.null(f$dn_confidence)) paste("deployment-need confidence", f$dn_confidence$d),
              if (grepl("partner", path) && !is.null(f$partner_evidence)) paste("partner evidence", f$partner_evidence$d))
    return(if (length(bits)) paste(bits, collapse = "; ") else "not rated")
  }
  if (grepl("uranium-fuel-upstream", path)) return("AIB model; thin, aggregated trade line")
  if (startsWith(path, "aib.")) return("AIB model run")
  if (startsWith(path, "high_fence.")) return("curated Secure routes and layers (Yards & Fences node attributes)")
  "derived"
}

# One row per claim: section, sentence, display, fact path, dashboard field, sources and evidence quality.
country_brief_trace <- function(claims, facts, cfg) {
  src_label <- function(ids) paste(vapply(ids, function(i) cfg$sources[[i]]$label %||% i, ""), collapse = " | ")
  do.call(rbind, lapply(claims, function(c) data.frame(
    claim = c$id, section = c$section, sentence_id = c$sentence_id %||% NA_character_, sentence = c$sentence %||% NA_character_,
    display = c$d %||% NA_character_, path = c$path, field = country_brief_field_origin(c$path),
    sources = src_label(unlist(c$source)), evidence = country_brief_evidence_of(c$path, facts), stringsAsFactors = FALSE)))
}

# The trace as Markdown, sentence by sentence.
country_brief_trace_markdown <- function(trace, title) {
  out <- c(paste0("# Content trace: ", title), "",
           "Each claim of the brief with the dashboard field it comes from, the source datasets and the evidence quality it rests on.", "")
  for (sid in unique(trace$sentence_id)) {
    r <- trace[trace$sentence_id %in% sid, , drop = FALSE]
    out <- c(out, sprintf("**%s** %s", sid, r$sentence[1]), "")
    out <- c(out, sprintf("- `%s` = %s | %s | %s | evidence: %s", r$path, r$display, r$field, r$sources, r$evidence), "")
  }
  paste(out, collapse = "\n")
}
