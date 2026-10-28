# The Secure route rule table (config/security_route_rules.csv; docs/strategic_position.md, section 7.2):
# one row per node (technology x six-stage x subnode, subnode "stage" = the whole node) saying which of
# the three Secure routes the node may take and the curated facts the routes test.
#
#   operational_control  a foreign actor could command, update or disable it (control layers and
#                        digitally active nodes), or a dual-use node with national-security end uses
#   coercive_chokepoint  a coercive supplier could withhold a physical input (concentration alone is
#                        not enough: substitution must be hard and trusted supply too slow to restore)
#   systemic_resilience  an essential function, substantial foreign dependency, a long lead time to
#                        rebuild and too little trusted supply
#
# The routes are curated, never inferred from trade: trade data only decide whether a country meets a
# route the node allows. substitutability and reconstitution_time_class repeat the node attributes
# (data/reference/posture_node_attributes.csv) and must agree with them where both are known.
#
# Pure: no IO.

SR_COLUMNS <- c("technology", "six_stage", "subnode", "critical_function", "digitally_active", "dual_use",
                "systemic_essentiality", "substitutability", "reconstitution_time_class", "rationale",
                "evidence_source", "evidence_confidence", "notes")
SR_ESSENTIALITY <- c("low", "moderate", "high", "n_a")
SR_CONFIDENCE <- c("low", "moderate", "high")

# Check the rule table. nodes (optional): the resolved node attributes (pn_resolve(), one country) to
# check coverage and agreement with. Returns the rules with blanks as "" and "Unknown" kept.
sr_read_check <- function(rules, nodes = NULL) {
  miss <- setdiff(SR_COLUMNS, names(rules))
  if (length(miss)) stop("security_route_rules: missing column(s) ", paste(miss, collapse = ", "), call. = FALSE)
  rules <- as.data.frame(lapply(rules[, SR_COLUMNS], function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }), stringsAsFactors = FALSE)
  bad <- character()
  key <- paste(rules$technology, rules$six_stage, rules$subnode, sep = "|")
  if (anyDuplicated(key)) bad <- c(bad, paste("duplicate node:", paste(unique(key[duplicated(key)]), collapse = ", ")))
  if (!all(is_supply_chain_stage(rules$six_stage))) bad <- c(bad, "six_stage has values that are not six-stage names")
  chk <- function(col, vocab, allow_blank = FALSE) {
    v <- rules[[col]]
    no <- !(v %in% vocab | (allow_blank & v == ""))
    if (any(no)) bad <<- c(bad, sprintf("%s: invalid value(s) %s", col, paste(unique(v[no]), collapse = ", ")))
  }
  chk("dual_use", c("yes", "no"))
  chk("digitally_active", c("yes", "no", "Unknown"), allow_blank = TRUE)
  chk("systemic_essentiality", SR_ESSENTIALITY)
  chk("substitutability", c("easy", "moderate", "hard", "impossible", "Unknown"))
  chk("reconstitution_time_class", c("lt6m", "m6to12", "y1to3", "gt3y", "Unknown"))
  chk("evidence_confidence", SR_CONFIDENCE)
  routes <- unlist(strsplit(rules$rationale[nzchar(rules$rationale)], ";", fixed = TRUE))
  if (length(setdiff(routes, SP_SECURITY_RATIONALES))) bad <- c(bad, paste("rationale: unknown route(s)", paste(unique(setdiff(routes, SP_SECURITY_RATIONALES)), collapse = ", ")))
  # a route's own facts must back it
  res <- grepl("systemic_resilience", rules$rationale, fixed = TRUE)
  if (any(res & rules$systemic_essentiality != "high")) bad <- c(bad, paste("systemic_resilience needs systemic_essentiality high:", paste(key[res & rules$systemic_essentiality != "high"], collapse = ", ")))
  dep <- rules$six_stage == "Deployment"
  if (any(dep & grepl("coercive_chokepoint|systemic_resilience", rules$rationale))) bad <- c(bad, "Deployment nodes take no physical route: their supply is assessed at the producing stages")
  if (!is.null(nodes)) {
    nkey <- paste(nodes$tech, nodes$stage, nodes$layer_id, sep = "|")
    if (length(setdiff(nkey, key))) bad <- c(bad, paste("nodes without a rule:", paste(setdiff(nkey, key), collapse = ", ")))
    if (length(setdiff(key, nkey))) bad <- c(bad, paste("rules for unknown nodes:", paste(setdiff(key, nkey), collapse = ", ")))
    j <- match(key, nkey)
    for (pair in list(c("substitutability", "substitutability"), c("reconstitution_time_class", "reconstitution_time"))) {
      a <- rules[[pair[1]]]
      b <- as.character(nodes[[pair[2]]])[j]
      known <- !is.na(j) & !a %in% c("", "Unknown") & !is.na(b) & !b %in% c("", "Unknown")
      diff <- known & a != b
      if (any(diff)) bad <- c(bad, sprintf("%s disagrees with the node attributes at %s", pair[1], paste(key[diff], collapse = ", ")))
    }
  }
  if (length(bad)) stop("security_route_rules:\n  ", paste(bad, collapse = "\n  "), call. = FALSE)
  rules
}

# Add the route columns to the engine input frame by (tech, supply_chain, layer_id): sr_rationale,
# dual_use, systemic_essentiality, sr_evidence_confidence. A node the table does not list gets NA (the
# engine's legacy routes).
sp_attach_security_rules <- function(x, rules) {
  r <- data.frame(
    tech = rules$technology, supply_chain = rules$six_stage, layer_id = rules$subnode,
    sr_rationale = rules$rationale, dual_use = rules$dual_use,
    systemic_essentiality = rules$systemic_essentiality, sr_evidence_confidence = rules$evidence_confidence,
    stringsAsFactors = FALSE
  )
  x <- x[, setdiff(names(x), setdiff(names(r), c("tech", "supply_chain", "layer_id"))), drop = FALSE]
  dplyr::left_join(x, r, by = c("tech", "supply_chain", "layer_id"))
}
