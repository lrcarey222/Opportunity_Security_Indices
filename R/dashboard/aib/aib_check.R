# The R mirror of the runtime checks of the additive AIB bundle sections in dashboard/src/aib/contract.ts
# (checkNetwork(), checkInvestmentFlows()): the emergent network (`network`) and the investor -> host
# flows (`investmentFlows`) of the compacts document. scripts/84_export_aib_data.R stops on any problem
# before it writes. Pure: takes the list aib_dash_bundle() returns (before JSON), returns a character
# vector of problems (empty when valid). The rules are the TypeScript ones: same paths, same bounds.

aib_check_is_str <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
aib_check_is_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)

aib_check_problems <- function() {
  env <- new.env()
  env$list <- character()
  env$add <- function(msg) if (length(env$list) < 200) env$list <- c(env$list, msg)
  env$str <- function(x, path, nullable = FALSE) {
    ok <- aib_check_is_str(x) || (nullable && (is.null(x) || (length(x) == 1 && is.na(x))))
    if (!ok) env$add(sprintf("%s: expected a string%s", path, if (nullable) " or null" else ""))
  }
  env$num <- function(x, path, nullable = FALSE, min = NULL, max = NULL, int = FALSE) {
    if (nullable && (is.null(x) || (length(x) == 1 && is.na(x)))) return(invisible())
    if (!aib_check_is_num(x)) return(env$add(sprintf("%s: expected a number%s", path, if (nullable) " or null" else "")))
    if (!is.null(min) && x < min - 1e-9) env$add(sprintf("%s: %s below %s", path, format(x), format(min)))
    if (!is.null(max) && x > max + 1e-9) env$add(sprintf("%s: %s above %s", path, format(x), format(max)))
    if (int && x != round(x)) env$add(sprintf("%s: %s is not an integer", path, format(x)))
  }
  # an array field must be wrapped in I() (or have length != 1) so that jsonlite keeps it an array
  env$str_arr <- function(x, path) {
    if (!is.character(x) || (length(x) == 1 && !inherits(x, "AsIs"))) return(env$add(sprintf("%s: expected an array of strings", path)))
    if (anyNA(x)) env$add(sprintf("%s: expected an array of strings", path))
  }
  env
}

aib_check_side <- function(p, s, at, trade) {
  if (!is.list(s)) return(p$add(sprintf("%s: not an object", at)))
  for (k in c("partners", "selected", "upstream", "midstream", "downstream")) p$num(s[[k]], sprintf("%s.%s", at, k), min = 0, int = TRUE)
  for (k in c("headline", "cumulative")) p$num(s[[k]], sprintf("%s.%s", at, k), min = 0)
  if (trade) {
    p$num(s$dividend, sprintf("%s.dividend", at), min = 0)
    p$num(s$volumeUsd, sprintf("%s.volumeUsd", at), min = 0)
  }
}

aib_check_variant <- function(p, v, at) {
  if (!is.list(v)) return(p$add(sprintf("%s: not an object", at)))
  for (k in c("shock", "scenario")) p$str(v[[k]], sprintf("%s.%s", at, k))
  for (k in c("shockSemantics", "supplyConstraint", "strategicScope", "demandScope", "candidateUniverse", "universe")) p$str(v[[k]], sprintf("%s.%s", at, k), nullable = TRUE)
}

# `network`: absent is fine; else variant, countries (unique ISO3, four sides) and edges.
aib_check_network <- function(doc, p = aib_check_problems()) {
  n <- doc$network
  if (is.null(n)) return(p$list)
  if (!is.list(n)) {
    p$add("network: not an object")
    return(p$list)
  }
  aib_check_variant(p, n$variant, "network.variant")
  node_ids <- vapply(doc$nodes, function(x) x$id, "")
  seen <- character()
  for (i in seq_along(n$countries)) {
    r <- n$countries[[i]]
    at <- sprintf("network.countries[%d]", i - 1)
    p$str(r$iso3, paste0(at, ".iso3"))
    if (aib_check_is_str(r$iso3)) {
      if (r$iso3 %in% seen) p$add(sprintf("%s.iso3: duplicate '%s'", at, r$iso3))
      seen <- c(seen, r$iso3)
    }
    p$str_arr(r$roles, paste0(at, ".roles"))
    if (is.character(r$roles)) for (x in setdiff(r$roles, c("supplier", "buyer", "investment_host", "investor"))) p$add(sprintf("%s.roles: '%s' is not one of supplier, buyer, investment_host, investor", at, x))
    p$str_arr(r$technologies, paste0(at, ".technologies"))
    p$num(r$lostUsd, paste0(at, ".lostUsd"), nullable = TRUE, min = 0)
    p$num(r$exposedNodes, paste0(at, ".exposedNodes"), nullable = TRUE, min = 0, int = TRUE)
    for (k in c("supplier", "buyer")) aib_check_side(p, r[[k]], paste0(at, ".", k), TRUE)
    for (k in c("host", "investor")) aib_check_side(p, r[[k]], paste0(at, ".", k), FALSE)
  }
  for (i in seq_along(n$edges)) {
    e <- n$edges[[i]]
    at <- sprintf("network.edges[%d]", i - 1)
    for (k in c("from", "to")) p$str(e[[k]], paste0(at, ".", k))
    if (aib_check_is_str(e$from) && identical(e$from, e$to)) p$add(sprintf("%s: supplier and buyer are the same economy", at))
    for (k in c("nTrade", "nInvestment", "nShocks", "nHighSeverity")) p$num(e[[k]], paste0(at, ".", k), min = 0, int = TRUE)
    for (k in c("nodes", "techs", "stages")) p$str_arr(e[[k]], paste0(at, ".", k))
    if (is.character(e$nodes)) for (x in setdiff(e$nodes, node_ids)) p$add(sprintf("%s.nodes: '%s' is not a node of the document", at, x))
    if (is.character(e$stages)) for (x in setdiff(e$stages, c("Upstream", "Midstream", "Downstream"))) p$add(sprintf("%s.stages: '%s' is not a stage", at, x))
    for (k in c("tradeUsd", "trade", "investment", "cumTrade", "cumDividend", "cumInvestment")) p$num(e[[k]], paste0(at, ".", k), min = 0)
    for (k in c("opportunity", "friendshore")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  }
  p$list
}

# `investmentFlows`: absent is fine; else variant (with the increment and the deepest rank), pairs
# (investor != host, rank 1..maxRank, a node of the document) and ranked hosts.
aib_check_investment_flows <- function(doc, p = aib_check_problems()) {
  f <- doc$investmentFlows
  if (is.null(f)) return(p$list)
  if (!is.list(f)) {
    p$add("investmentFlows: not an object")
    return(p$list)
  }
  aib_check_variant(p, f$variant, "investmentFlows.variant")
  p$num(f$variant$increment, "investmentFlows.variant.increment", min = 0, max = 1)
  p$num(f$variant$maxRank, "investmentFlows.variant.maxRank", min = 1, int = TRUE)
  max_rank <- if (aib_check_is_num(f$variant$maxRank)) f$variant$maxRank else Inf
  node_ids <- vapply(doc$nodes, function(x) x$id, "")
  for (i in seq_along(f$pairs)) {
    e <- f$pairs[[i]]
    at <- sprintf("investmentFlows.pairs[%d]", i - 1)
    for (k in c("investor", "host", "node")) p$str(e[[k]], paste0(at, ".", k))
    if (aib_check_is_str(e$investor) && identical(e$investor, e$host)) p$add(sprintf("%s: investor and host are the same economy", at))
    if (aib_check_is_str(e$node) && !e$node %in% node_ids) p$add(sprintf("%s.node: '%s' is not a node of the document", at, e$node))
    p$num(e$rank, paste0(at, ".rank"), min = 1, max = max_rank, int = TRUE)
    p$num(e$usd, paste0(at, ".usd"), min = 0)
    for (k in c("share", "lostShare")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
    for (k in c("friendshore", "opportunity")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  }
  for (i in seq_along(f$hosts)) {
    h <- f$hosts[[i]]
    at <- sprintf("investmentFlows.hosts[%d]", i - 1)
    for (k in c("node", "host")) p$str(h[[k]], paste0(at, ".", k))
    if (aib_check_is_str(h$node) && !h$node %in% node_ids) p$add(sprintf("%s.node: '%s' is not a node of the document", at, h$node))
    p$num(h$rank, paste0(at, ".rank"), min = 1, max = max_rank, int = TRUE)
    p$num(h$gain, paste0(at, ".gain"), nullable = TRUE, min = -1, max = 1)
    for (k in c("before", "after", "newCapacityUsed")) p$num(h[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
    p$num(h$servedUsd, paste0(at, ".servedUsd"), nullable = TRUE, min = 0)
    p$num(h$capabilityTier, paste0(at, ".capabilityTier"), nullable = TRUE, min = 0, int = TRUE)
    p$str(h$capabilityBasis, paste0(at, ".capabilityBasis"), nullable = TRUE)
  }
  p$list
}

# Both additive sections.
aib_check_bundle_additions <- function(doc) c(aib_check_network(doc), aib_check_investment_flows(doc))
