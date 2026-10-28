# The R mirror of the runtime checks of the additive AIB bundle sections in dashboard/src/aib/contract.ts
# (checkNetwork(), checkInvestmentFlows()): the emergent network (`network`) and the investor -> host
# flows (`investmentFlows`) of the compacts document, the taxonomy fields of 2026-10-19 (meta.taxonomy,
# dictionaryScope, indices; node.indexCode; compact.indices) and the diversification scenario (meta.diversify,
# node.diversify; checkNodeDiversify()) and the scenario toggle (2026-10-19: meta.scenarios /
# supplyScenarios, node.baseline, the node.diversify run fields, compact.managed / baseline:
# aib_check_scenarios(); the scenario files: aib_check_scenario_doc(), checkScenarioDoc(); the manifest's
# files.scenarios: aib_check_scenario_files()) and the strategic layer (2026-10-20: the optional `strategic`
# section, aib_check_strategic(); its rules, listed above it, are the contract the TypeScript reader
# mirrors) and the six stages (2026-10-28: node.stage, compact.stages and the network edge stages, each list
# unique and in chain order, and the nodes' three-stage cells: aib_check_stages(), checkStageList() and
# checkLegacyCells()). scripts/84_export_aib_data.R stops on any problem
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
  env$bool <- function(x, path) {
    if (!(is.logical(x) && length(x) == 1 && !is.na(x))) env$add(sprintf("%s: expected true or false", path))
  }
  env
}

# A list that jsonlite writes as an array (unnamed, or empty; a zero-length vector is written [] too), not as an object.
aib_check_is_arr <- function(x) (is.list(x) && (!length(x) || is.null(names(x)) || !any(nzchar(names(x))))) || (is.atomic(x) && !length(x))
# A list that jsonlite writes as an object (named), or an empty list (written [] and read as {} by TypeScript where allowed).
aib_check_is_obj <- function(x) is.list(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x)))

# A list of stages (compact.stages, a network edge's stages; checkStageList() in contract.ts): an array (I())
# of the six stages, each once, in chain order. The messages are the TypeScript ones; a duplicate is reported
# at each repeat, the order once, over the known stages only.
aib_check_stage_list <- function(p, x, at) {
  p$str_arr(x, at)
  if (!is.character(x) || anyNA(x) || (length(x) == 1 && !inherits(x, "AsIs"))) return(invisible())
  x <- as.vector(unclass(x))
  for (s in x[!x %in% SUPPLY_CHAIN_STAGES]) p$add(sprintf("%s: '%s' is not one of %s", at, s, paste(SUPPLY_CHAIN_STAGES, collapse = ", ")))
  for (s in x[duplicated(x)]) p$add(sprintf("%s: duplicate '%s'", at, s))
  r <- stats::na.omit(match(x, SUPPLY_CHAIN_STAGES))
  if (length(r) > 1 && any(diff(r) < 0)) p$add(sprintf("%s: not in chain order (%s)", at, paste(x, collapse = ", ")))
}

# The three-stage cells of node.yfCellsLegacy ("<tech slug>-<upstream|midstream|downstream>") and the
# sub-components of node.yfSubcomponentsLegacy ("<that cell>::<name>"); isLegacyCellSlug() and
# isLegacySubcomponentId() of dashboard/src/lib/stages.ts.
aib_check_legacy_cell_pattern <- function() "^[a-z0-9]+(-[a-z0-9]+)*-(upstream|midstream|downstream)$"
aib_check_legacy_subcomponent_pattern <- function() "^[a-z0-9]+(-[a-z0-9]+)*-(upstream|midstream|downstream)::.+$"

# The six stages and the three-stage cells (2026-10-28; checkNode(), checkCompact() and checkLegacyCells() in
# contract.ts): node.stage one of the six when present (the R mirror checks the documents of the tests, whose
# nodes may carry an id only; the exporter always writes it); compact.stages a stage list when present;
# node.yfCellsLegacy / yfSubcomponentsLegacy absent, or arrays of three-stage cells / sub-components. The
# network edge stages are checked in aib_check_network().
aib_check_stages <- function(doc, p = aib_check_problems()) {
  for (i in seq_along(doc$nodes)) {
    n <- doc$nodes[[i]]
    if (!is.list(n)) next
    at <- sprintf("nodes[%d]", i - 1)
    if ("stage" %in% names(n)) {
      if (!aib_check_is_str(n$stage)) p$add(sprintf("%s.stage: expected a string", at))
      else if (!n$stage %in% SUPPLY_CHAIN_STAGES) p$add(sprintf("%s.stage: '%s' is not one of %s", at, n$stage, paste(SUPPLY_CHAIN_STAGES, collapse = ", ")))
    }
    for (k in c("yfCellsLegacy", "yfSubcomponentsLegacy")) {
      x <- n[[k]]
      if (is.null(x)) next
      p$str_arr(x, sprintf("%s.%s", at, k))
      if (!is.character(x) || anyNA(x)) next
      cell <- identical(k, "yfCellsLegacy")
      bad <- which(!grepl(if (cell) aib_check_legacy_cell_pattern() else aib_check_legacy_subcomponent_pattern(), x))
      for (j in bad) {
        p$add(sprintf(if (cell) "%s.%s[%d]: '%s' is not a three-stage cell (<tech>-<upstream|midstream|downstream>)" else
          "%s.%s[%d]: '%s' is not a three-stage sub-component (<tech>-<upstream|midstream|downstream>::<name>)", at, k, j - 1, x[j]))
      }
    }
  }
  for (i in seq_along(doc$compacts)) {
    c <- doc$compacts[[i]]
    if (!is.list(c) || !"stages" %in% names(c)) next
    aib_check_stage_list(p, c$stages, sprintf("compacts[%d].stages", i - 1))
  }
  p$list
}

aib_check_side <- function(p, s, at, trade) {
  if (!is.list(s)) return(p$add(sprintf("%s: not an object", at)))
  # partner counts by stage: the six stages in lower case (aib_dash_stage_keys())
  for (k in c("partners", "selected", tolower(SUPPLY_CHAIN_STAGES))) p$num(s[[k]], sprintf("%s.%s", at, k), min = 0, int = TRUE)
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

# `network`: absent is fine; else variant, countries (unique ISO3, four sides) and edges. `node_ids`: the
# nodes an edge may name (default: the document's; NULL: not cross-checked, a scenario file without the
# main document).
aib_check_network <- function(doc, p = aib_check_problems(), node_ids = vapply(doc$nodes, function(x) x$id, "")) {
  n <- doc$network
  if (is.null(n)) return(p$list)
  if (!is.list(n)) {
    p$add("network: not an object")
    return(p$list)
  }
  aib_check_variant(p, n$variant, "network.variant")
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
    for (k in c("nodes", "techs")) p$str_arr(e[[k]], paste0(at, ".", k))
    if (is.character(e$nodes) && !is.null(node_ids)) for (x in setdiff(e$nodes, node_ids)) p$add(sprintf("%s.nodes: '%s' is not a node of the document", at, x))
    # the six stages, each once, in chain order (checkStageList() in contract.ts)
    aib_check_stage_list(p, e$stages, paste0(at, ".stages"))
    for (k in c("tradeUsd", "trade", "investment", "cumTrade", "cumDividend", "cumInvestment")) p$num(e[[k]], paste0(at, ".", k), min = 0)
    for (k in c("opportunity", "friendshore")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  }
  p$list
}

# `investmentFlows`: absent is fine; else variant (with the increment and the deepest rank), pairs
# (investor != host, rank 1..maxRank, a node of the document) and ranked hosts.
aib_check_investment_flows <- function(doc, p = aib_check_problems(), node_ids = vapply(doc$nodes, function(x) x$id, "")) {
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
  for (i in seq_along(f$pairs)) {
    e <- f$pairs[[i]]
    at <- sprintf("investmentFlows.pairs[%d]", i - 1)
    for (k in c("investor", "host", "node")) p$str(e[[k]], paste0(at, ".", k))
    if (aib_check_is_str(e$investor) && identical(e$investor, e$host)) p$add(sprintf("%s: investor and host are the same economy", at))
    if (aib_check_is_str(e$node) && !is.null(node_ids) && !e$node %in% node_ids) p$add(sprintf("%s.node: '%s' is not a node of the document", at, e$node))
    p$num(e$rank, paste0(at, ".rank"), min = 1, max = max_rank, int = TRUE)
    p$num(e$usd, paste0(at, ".usd"), min = 0)
    for (k in c("share", "lostShare")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
    for (k in c("friendshore", "opportunity")) p$num(e[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  }
  for (i in seq_along(f$hosts)) {
    h <- f$hosts[[i]]
    at <- sprintf("investmentFlows.hosts[%d]", i - 1)
    for (k in c("node", "host")) p$str(h[[k]], paste0(at, ".", k))
    if (aib_check_is_str(h$node) && !is.null(node_ids) && !h$node %in% node_ids) p$add(sprintf("%s.node: '%s' is not a node of the document", at, h$node))
    p$num(h$rank, paste0(at, ".rank"), min = 1, max = max_rank, int = TRUE)
    p$num(h$gain, paste0(at, ".gain"), nullable = TRUE, min = -1, max = 1)
    for (k in c("before", "after", "newCapacityUsed")) p$num(h[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
    p$num(h$servedUsd, paste0(at, ".servedUsd"), nullable = TRUE, min = 0)
    p$num(h$capabilityTier, paste0(at, ".capabilityTier"), nullable = TRUE, min = 0, int = TRUE)
    p$str(h$capabilityBasis, paste0(at, ".capabilityBasis"), nullable = TRUE)
    if (!is.null(h$hostIncrement)) p$num(h$hostIncrement, paste0(at, ".hostIncrement"), nullable = TRUE, min = 0, max = 1)
    if (!is.null(h$rankBasis)) p$str(h$rankBasis, paste0(at, ".rankBasis"), nullable = TRUE)
    if (!is.null(h$type)) p$str(h$type, paste0(at, ".type"), nullable = TRUE)
  }
  p$list
}

# The taxonomy fields (2026-10-19; checkMeta(), checkNode() and checkCompact() in contract.ts): all
# optional, so a bundle written before them (no taxonomy, OSI technology names) passes. meta.taxonomy and
# meta.dictionaryScope: strings or null (a value the app does not know reads as master); meta.indices: an
# array of {code (one capital letter, unique), name, sectors (an array of strings)}; node.indexCode: null
# or a string, one of meta.indices when it lists any; compact.indices: an array of strings, each one of
# meta.indices when it lists any.
aib_check_taxonomy <- function(doc, p = aib_check_problems()) {
  m <- doc$meta
  if (!is.null(m)) {
    if (!is.null(m$taxonomy)) p$str(m$taxonomy, "meta.taxonomy", nullable = TRUE)
    if (!is.null(m$dictionaryScope)) p$str(m$dictionaryScope, "meta.dictionaryScope", nullable = TRUE)
  }
  codes <- character()
  ix <- if (is.null(m)) NULL else m$indices
  if (!is.null(ix)) {
    if (!is.list(ix) || (length(ix) && !is.null(names(ix)) && all(nzchar(names(ix))))) {
      p$add("meta.indices: expected an array")
    } else {
      for (i in seq_along(ix)) {
        x <- ix[[i]]
        at <- sprintf("meta.indices[%d]", i - 1)
        if (!is.list(x)) {
          p$add(sprintf("%s: not an object", at))
          next
        }
        p$str(x$code, paste0(at, ".code"))
        if (aib_check_is_str(x$code)) {
          if (!grepl("^[A-Z]$", x$code)) p$add(sprintf("%s.code: '%s' is not one capital letter", at, x$code))
          if (x$code %in% codes) p$add(sprintf("%s.code: duplicate '%s'", at, x$code))
          codes <- c(codes, x$code)
        }
        p$str(x$name, paste0(at, ".name"))
        p$str_arr(x$sectors, paste0(at, ".sectors"))
      }
    }
  }
  for (i in seq_along(doc$nodes)) {
    k <- doc$nodes[[i]]$indexCode
    if (is.null(k) || (length(k) == 1 && is.na(k))) next
    at <- sprintf("nodes[%d].indexCode", i - 1)
    if (!aib_check_is_str(k)) p$add(sprintf("%s: expected a string or null", at))
    else if (length(codes) && !k %in% codes) p$add(sprintf("%s: '%s' is not an index of meta.indices", at, k))
  }
  for (i in seq_along(doc$compacts)) {
    k <- doc$compacts[[i]]$indices
    if (is.null(k)) next
    at <- sprintf("compacts[%d].indices", i - 1)
    p$str_arr(k, at)
    if (is.character(k) && length(codes)) for (x in setdiff(k, codes)) p$add(sprintf("%s: '%s' is not an index of meta.indices", at, x))
  }
  p$list
}

# The diversification scenario (2026-10-19; checkMeta() and checkNodeDiversify() in contract.ts): all
# optional, so a bundle written before it passes. meta.diversify: absent, or {shockId (a string), label and
# benchmark (strings or null)}; present but NULL is JSON null, which TypeScript rejects ("not an object").
# node.diversify: absent, or an object (present but NULL: "not an object") with all 15 fields, as
# TypeScript reads a missing key as undefined and rejects it: status one of the four statuses; currentHhi,
# benchmarkHhi, cap, excess, static, existing, optimized and residual shares in 0-1 or null (NA);
# residualUsd >= 0 or null; basis and partners strings or null; capped and hosts arrays of strings (I(), so
# a one-item array stays an array); cls null or one of meta.classes (the five classes when the bundle lists
# none, as TypeScript falls back to AIB_CLASSES).
aib_diversify_statuses <- function() {
  c("diversified to the benchmark", "within the cap in the allied market", "already as diversified as its benchmark", "no benchmark")
}

# AIB_CLASSES of contract.ts: the classes a class is checked against when meta.classes is absent.
aib_check_classes <- function() c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA")

aib_check_diversify <- function(doc, p = aib_check_problems()) {
  m <- doc$meta
  if (is.list(m) && "diversify" %in% names(m)) {
    dv <- m$diversify
    if (!is.list(dv)) {
      p$add("meta.diversify: not an object")
    } else {
      p$str(dv$shockId, "meta.diversify.shockId")
      if (!is.null(dv$label)) p$str(dv$label, "meta.diversify.label", nullable = TRUE)
      if (!is.null(dv$benchmark)) p$str(dv$benchmark, "meta.diversify.benchmark", nullable = TRUE)
      if (!is.null(dv$minExcess)) p$num(dv$minExcess, "meta.diversify.minExcess", nullable = TRUE, min = 0, max = 1)
    }
  }
  classes <- if (is.null(m) || is.null(m$classes)) aib_check_classes() else as.character(m$classes)
  for (i in seq_along(doc$nodes)) {
    node <- doc$nodes[[i]]
    if (!is.list(node) || !"diversify" %in% names(node)) next
    d <- node$diversify
    at <- sprintf("nodes[%d].diversify", i - 1)
    if (!is.list(d)) {
      p$add(sprintf("%s: not an object", at))
      next
    }
    has <- function(k) k %in% names(d)
    if (!has("status")) {
      p$add(sprintf("%s.status: 'undefined' is not one of %s", at, paste(aib_diversify_statuses(), collapse = ", ")))
    } else if (!aib_check_is_str(d$status) || !d$status %in% aib_diversify_statuses()) {
      p$add(sprintf("%s.status: '%s' is not one of %s", at, paste(format(d$status), collapse = ","), paste(aib_diversify_statuses(), collapse = ", ")))
    }
    for (k in c("currentHhi", "benchmarkHhi", "cap", "excess", "static", "existing", "optimized", "residual", "residualUsd")) {
      if (!has(k)) p$add(sprintf("%s.%s: expected a number or null", at, k))
      else if (k == "residualUsd") p$num(d[[k]], paste0(at, ".", k), nullable = TRUE, min = 0)
      else p$num(d[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
    }
    for (k in c("basis", "partners")) {
      if (!has(k)) p$add(sprintf("%s.%s: expected a string or null", at, k))
      else p$str(d[[k]], paste0(at, ".", k), nullable = TRUE)
    }
    for (k in c("capped", "hosts")) p$str_arr(d[[k]], paste0(at, ".", k))
    for (k in c("specialtyHosts", "developmentHosts")) if (!is.null(d[[k]])) p$str_arr(d[[k]], paste0(at, ".", k))
    cls <- d$cls
    if (!has("cls")) {
      p$add(sprintf("%s.cls: expected a string or null", at))
    } else if (!(is.null(cls) || (length(cls) == 1 && is.na(cls)))) {
      if (!aib_check_is_str(cls)) p$add(sprintf("%s.cls: expected a string or null", at))
      else if (!cls %in% classes) p$add(sprintf("%s.cls: '%s' is not one of meta.classes", at, cls))
    }
  }
  p$list
}

# ---- The scenario toggle (2026-10-19) ------------------------------------------------------------------
# The main document's fields (checkMetaScenarios(), checkNodeBaseline(), checkNodeDiversify()'s run fields,
# checkCompactManaged(), checkCompactBaseline() in contract.ts) and the scenario files (checkScenarioDoc()).
# All optional in the main document, so a bundle written before them passes.

aib_scenario_ids <- function() c("baseline", "managed", "decoupling")
aib_scenario_file_ids <- function() c("managed", "baseline")
# AIB_COMPACT_TYPES and AIB_ARCHETYPES of contract.ts (a scenario compact's type and archetype).
aib_check_compact_types <- function() c("candidate compact", "bilateral arrangement sufficient", "compact unnecessary: existing relationships do nearly as well")
aib_check_archetypes <- function() c("Trade / Coordination Compact", "Investment / Capacity Compact", "Specialization Compact", "Integrated Compact")

# [ISO3, share] (n = 2) or [ISO3, USD, share] (n = 3) tuples: list(list("CHN", 0.4), ...).
aib_check_tuples <- function(p, x, at, n) {
  if (!aib_check_is_arr(x)) return(p$add(sprintf("%s: expected an array", at)))
  for (i in seq_along(x)) {
    t <- x[[i]]
    ta <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(t) || length(t) != n) {
      p$add(sprintf("%s: expected [%s]", ta, if (n == 3) "ISO3, USD, share" else "ISO3, share"))
      next
    }
    p$str(t[[1]], paste0(ta, "[0]"))
    if (n == 3) p$num(t[[2]], paste0(ta, "[1]"), nullable = TRUE, min = 0)
    p$num(t[[n]], sprintf("%s[%d]", ta, n - 1), nullable = TRUE, min = 0, max = 1)
  }
}

aib_check_scenarios <- function(doc, p = aib_check_problems()) {
  m <- doc$meta
  sc <- if (is.list(m)) m$scenarios else NULL
  # meta.scenarios: the supply scenarios (a character vector) or the scenario objects, not both
  all_names <- is.list(sc) && length(sc) > 0 && all(vapply(sc, aib_check_is_str, logical(1)))
  if (!is.null(sc) && !is.character(sc) && !all_names) {
    if (!is.list(sc)) {
      p$add("meta.scenarios: expected an array")
    } else {
      seen <- character()
      for (i in seq_along(sc)) {
        x <- sc[[i]]
        at <- sprintf("meta.scenarios[%d]", i - 1)
        if (!aib_check_is_obj(x)) {
          p$add(sprintf("%s: expected a scenario object (meta.scenarios mixes names and scenario objects)", at))
          next
        }
        if (!(aib_check_is_str(x$id) && x$id %in% aib_scenario_ids())) {
          p$add(sprintf("%s.id: '%s' is not one of %s", at, if (is.null(x$id)) "undefined" else paste(format(x$id), collapse = ","), paste(aib_scenario_ids(), collapse = ", ")))
        } else {
          if (x$id %in% seen) p$add(sprintf("%s.id: duplicate '%s'", at, x$id))
          seen <- c(seen, x$id)
        }
        p$str(x$label, paste0(at, ".label"))
        if (!"shockId" %in% names(x)) p$add(sprintf("%s.shockId: expected a string or null", at)) else p$str(x$shockId, paste0(at, ".shockId"), nullable = TRUE)
        p$str(x$words, paste0(at, ".words"))
      }
    }
  }
  if (is.list(m) && !is.null(m$supplyScenarios)) p$str_arr(m$supplyScenarios, "meta.supplyScenarios")
  for (i in seq_along(doc$nodes)) {
    node <- doc$nodes[[i]]
    if (!is.list(node)) next
    if ("baseline" %in% names(node)) {
      b <- node$baseline
      at <- sprintf("nodes[%d].baseline", i - 1)
      if (!is.list(b)) {
        p$add(sprintf("%s: not an object", at))
      } else {
        for (k in c("hhi", "top3Share", "largestShare")) {
          if (!k %in% names(b)) p$add(sprintf("%s.%s: expected a number or null", at, k)) else p$num(b[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
        }
        if (!"largest" %in% names(b)) p$add(sprintf("%s.largest: expected a string or null", at)) else p$str(b$largest, paste0(at, ".largest"), nullable = TRUE)
        aib_check_tuples(p, b$suppliers, paste0(at, ".suppliers"), 2)
      }
    }
    d <- node$diversify
    if (is.list(d)) {
      at <- sprintf("nodes[%d].diversify", i - 1)
      has <- function(k) k %in% names(d)
      if (has("lostUsd")) p$num(d$lostUsd, paste0(at, ".lostUsd"), nullable = TRUE, min = 0)
      if (has("dividend")) p$num(d$dividend, paste0(at, ".dividend"), nullable = TRUE, min = -1, max = 1)
      for (k in c("hhi", "largestShare", "top3Share")) if (has(k)) p$num(d[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
      if (has("largest")) p$str(d$largest, paste0(at, ".largest"), nullable = TRUE)
      for (k in c("newLinks", "expandedLinks")) if (has(k)) p$num(d[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, int = TRUE)
    }
  }
  for (i in seq_along(doc$compacts)) {
    c <- doc$compacts[[i]]
    if (!is.list(c)) next
    at <- sprintf("compacts[%d]", i - 1)
    if ("managed" %in% names(c)) {
      mg <- c$managed
      ma <- paste0(at, ".managed")
      if (!is.list(mg)) {
        p$add(sprintf("%s: not an object", ma))
      } else {
        n_nodes <- if (is.null(c$nodes)) NULL else length(c$nodes)
        p$num(mg$nodesRun, paste0(ma, ".nodesRun"), min = 0, int = TRUE, max = n_nodes)
        for (k in c("demandUsd", "lostUsd", "residualUsd")) {
          if (!k %in% names(mg)) p$add(sprintf("%s.%s: expected a number or null", ma, k)) else p$num(mg[[k]], paste0(ma, ".", k), nullable = TRUE, min = 0)
        }
        for (k in c("static", "existing", "optimized", "residual")) {
          if (!k %in% names(mg)) p$add(sprintf("%s.%s: expected a number or null", ma, k)) else p$num(mg[[k]], paste0(ma, ".", k), nullable = TRUE, min = 0, max = 1)
        }
        if (!is.null(mg$classMix)) p$str(mg$classMix, paste0(ma, ".classMix"), nullable = TRUE)
      }
    }
    if ("baseline" %in% names(c)) {
      b <- c$baseline
      ba <- paste0(at, ".baseline")
      if (!is.list(b)) {
        p$add(sprintf("%s: not an object", ba))
      } else {
        if (!"hhi" %in% names(b)) p$add(sprintf("%s.hhi: expected a number or null", ba)) else p$num(b$hhi, paste0(ba, ".hhi"), nullable = TRUE, min = 0, max = 1)
        aib_check_tuples(p, b$suppliers, paste0(ba, ".suppliers"), 2)
      }
    }
  }
  p$list
}

# manifest.files.scenarios (checkScenarioFiles()): NULL is fine; else each managed / baseline entry NULL or a
# scenario-<id>.<8 hex>.json name of that scenario; other entries are ignored.
aib_check_scenario_files <- function(s) {
  p <- aib_check_problems()
  if (is.null(s)) return(p$list)
  if (!is.list(s) && !is.character(s)) return("manifest.files.scenarios: not an object")
  for (id in aib_scenario_file_ids()) {
    f <- s[[id]]
    if (is.null(f) || (length(f) == 1 && is.na(f))) next
    ok <- aib_check_is_str(f) && grepl("^scenario-(managed|baseline)\\.[0-9a-f]{8}\\.json$", f) && identical(sub("^scenario-([a-z]+)\\..*$", "\\1", f), id)
    if (!ok) p$add(sprintf("manifest.files.scenarios.%s: not a scenario-%s.<8 hex>.json file name", id, id))
  }
  p$list
}

aib_check_coalition <- function(p, co, at) {
  if (!aib_check_is_obj(co)) return(p$add(sprintf("%s: not an object", at)))
  for (k in c("members", "t90", "t95", "t99")) p$str_arr(co[[k]], paste0(at, ".", k))
  p$bool(co$reached99, paste0(at, ".reached99"))
  p$num(co$maxCoverage, paste0(at, ".maxCoverage"), nullable = TRUE, min = 0, max = 1)
  if (!is.null(co$method)) p$str(co$method, paste0(at, ".method"), nullable = TRUE)
  for (k in c("greedy", "criticalPartners", "prospectiveHosts", "criticalExternal", "criticalExternalMarginal", "external")) if (!is.null(co[[k]])) p$str_arr(co[[k]], paste0(at, ".", k))
  for (k in c("associatesReach99", "robustTieBreakSolved")) if (!is.null(co[[k]])) p$bool(co[[k]], paste0(at, ".", k))
}

aib_check_roles <- function(p, roles, at) {
  if (!aib_check_is_arr(roles)) return(p$add(sprintf("%s: expected an array", at)))
  for (i in seq_along(roles)) {
    r <- roles[[i]]
    ra <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(r)) {
      p$add(sprintf("%s: not an object", ra))
      next
    }
    for (k in c("iso3", "role", "basis")) p$str(r[[k]], paste0(ra, ".", k))
    p$bool(r$member, paste0(ra, ".member"))
    if (!is.null(r$tier)) p$str(r$tier, paste0(ra, ".tier"), nullable = TRUE)
    for (k in c("critical", "looCritical")) if (!is.null(r[[k]])) p$bool(r[[k]], paste0(ra, ".", k))
    if (!is.null(r$substitutes)) p$str_arr(r$substitutes, paste0(ra, ".substitutes"))
    if ("substitutesNeeded" %in% names(r)) p$num(r$substitutesNeeded, paste0(ra, ".substitutesNeeded"), nullable = TRUE, min = 0, int = TRUE)
  }
}

aib_check_metrics <- function(p, m, at) {
  if (!aib_check_is_obj(m)) return(p$add(sprintf("%s: not an object", at)))
  for (k in c("demandUsd", "lostUsd")) p$num(m[[k]], paste0(at, ".", k), nullable = TRUE, min = 0)
  for (k in c("exposure", "static", "existing", "trade", "investment", "residual", "residualAfterInvestment")) p$num(m[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  p$num(m$dividend, paste0(at, ".dividend"), nullable = TRUE, min = -1, max = 1)
  for (k in c("globalOptimum", "anchorFragmented", "anchorCompact")) if (k %in% names(m)) p$num(m[[k]], paste0(at, ".", k), nullable = TRUE, min = 0, max = 1)
  if ("compactGain" %in% names(m)) p$num(m$compactGain, paste0(at, ".compactGain"), nullable = TRUE, min = -1, max = 1)
}

aib_check_path <- function(p, path, at) {
  if (!aib_check_is_arr(path)) return(p$add(sprintf("%s: expected an array", at)))
  for (i in seq_along(path)) {
    s <- path[[i]]
    sa <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(s)) {
      p$add(sprintf("%s: not an object", sa))
      next
    }
    p$num(s$step, paste0(sa, ".step"), min = 0, int = TRUE)
    p$str(s$added, paste0(sa, ".added"), nullable = TRUE)
    p$str_arr(s$members, paste0(sa, ".members"))
    for (k in c("minCoverage", "meanCoverage")) p$num(s[[k]], paste0(sa, ".", k), nullable = TRUE, min = 0, max = 1)
    nc <- s$nodeCoverage
    if (!(is.list(nc) && (!length(nc) || aib_check_is_obj(nc)))) p$add(sprintf("%s.nodeCoverage: expected an object of node coverages", sa))
    else for (k in names(nc)) p$num(nc[[k]], sprintf("%s.nodeCoverage.%s", sa, k), nullable = TRUE, min = 0, max = 1)
  }
}

aib_check_trade_edges <- function(p, trade, at) {
  if (!aib_check_is_arr(trade)) return(p$add(sprintf("%s: expected an array", at)))
  for (i in seq_along(trade)) {
    e <- trade[[i]]
    ea <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(e)) {
      p$add(sprintf("%s: not an object", ea))
      next
    }
    for (k in c("from", "to", "node")) p$str(e[[k]], paste0(ea, ".", k))
    for (k in c("usd", "share", "opportunity")) p$num(e[[k]], paste0(ea, ".", k), nullable = TRUE)
    if (!(aib_check_is_str(e$relationship) && e$relationship %in% c("new", "expanded"))) {
      p$add(sprintf("%s.relationship: '%s' is not one of new, expanded", ea, if (is.null(e$relationship)) "undefined" else paste(format(e$relationship), collapse = ",")))
    }
  }
}

aib_check_investment_edges <- function(p, inv, at) {
  if (!aib_check_is_arr(inv)) return(p$add(sprintf("%s: expected an array", at)))
  for (i in seq_along(inv)) {
    e <- inv[[i]]
    ea <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(e)) {
      p$add(sprintf("%s: not an object", ea))
      next
    }
    for (k in c("investor", "host", "node")) p$str(e[[k]], paste0(ea, ".", k))
    for (k in c("usd", "share", "friendshore")) p$num(e[[k]], paste0(ea, ".", k), nullable = TRUE)
  }
}

# {id, suppliers: [ISO3, USD, share][]} rows of scenario-baseline (ids unique; of the main document when known).
aib_check_supplier_lists <- function(p, x, at, known, what) {
  if (!aib_check_is_arr(x)) return(p$add(sprintf("%s: expected an array", at)))
  seen <- character()
  for (i in seq_along(x)) {
    r <- x[[i]]
    ra <- sprintf("%s[%d]", at, i - 1)
    if (!is.list(r)) {
      p$add(sprintf("%s: not an object", ra))
      next
    }
    p$str(r$id, paste0(ra, ".id"))
    if (aib_check_is_str(r$id)) {
      if (r$id %in% seen) p$add(sprintf("%s.id: duplicate '%s'", ra, r$id))
      seen <- c(seen, r$id)
      if (!is.null(known) && !r$id %in% known) p$add(sprintf("%s.id: '%s' is not a %s of the main document", ra, r$id, what))
    }
    aib_check_tuples(p, r$suppliers, paste0(ra, ".suppliers"), 3)
  }
}

# A scenario file (scenario-managed / scenario-baseline; checkScenarioDoc() in contract.ts): `main` (the
# main document, the list aib_dash_bundle() returns) adds the cross-checks of node and compact ids;
# `expect` the scenario the manifest names the file for. Tolerant as TypeScript is: network and
# investmentFlows may be absent, a managed compact's arrays, metrics and coalitionReason too, and its
# coalition may be NULL (JSON null) with a coalitionReason.
aib_check_scenario_doc <- function(doc, main = NULL, expect = NULL) {
  p <- aib_check_problems()
  if (!aib_check_is_obj(doc)) return("document: not an object")
  if (!identical(doc$schema, "osi-aib-scenario")) p$add("document.schema: expected 'osi-aib-scenario'")
  if (!aib_check_is_num(doc$version)) p$add("document.version: not a number")
  else if (floor(doc$version) > 1) p$add(sprintf("document: version %s needs a newer app", format(doc$version)))
  sc <- doc$scenario
  if (!(aib_check_is_str(sc) && sc %in% aib_scenario_file_ids())) {
    p$add(sprintf("document.scenario: '%s' is not one of managed, baseline", if (is.null(sc)) "undefined" else paste(format(sc), collapse = ",")))
    sc <- ""
  }
  if (!is.null(expect) && nzchar(sc) && !identical(sc, expect)) p$add(sprintf("document.scenario: '%s' where the manifest names a %s file", sc, expect))
  if (!"shockId" %in% names(doc)) p$add(sprintf("document.shockId: expected a string%s", if (identical(sc, "managed")) "" else " or null"))
  else p$str(doc$shockId, "document.shockId", nullable = !identical(sc, "managed"))
  p$str(doc$created, "document.created")
  cn <- doc$countries
  if (!(is.list(cn) && (!length(cn) || aib_check_is_obj(cn)))) p$add("countries: expected an object of ISO3 -> name")
  else for (k in names(cn)) p$str(cn[[k]], paste0("countries.", k))
  node_ids <- if (is.null(main)) NULL else vapply(main$nodes, function(x) x$id, "")
  compact_ids <- if (is.null(main)) NULL else vapply(main$compacts, function(x) x$id, "")
  if (identical(sc, "managed")) {
    cs <- doc$compacts
    if (!aib_check_is_arr(cs)) {
      p$add("compacts: expected an array")
    } else {
      seen <- character()
      for (i in seq_along(cs)) {
        c <- cs[[i]]
        at <- sprintf("compacts[%d]", i - 1)
        if (!is.list(c)) {
          p$add(sprintf("%s: not an object", at))
          next
        }
        p$str(c$id, paste0(at, ".id"))
        if (aib_check_is_str(c$id)) {
          if (c$id %in% seen) p$add(sprintf("%s.id: duplicate '%s'", at, c$id))
          seen <- c(seen, c$id)
          if (!is.null(compact_ids) && !c$id %in% compact_ids) p$add(sprintf("%s.id: '%s' is not a compact of the main document", at, c$id))
        }
        p$num(c$nodesRun, paste0(at, ".nodesRun"), min = 0, int = TRUE)
        null_coalition <- "coalition" %in% names(c) && is.null(c$coalition)
        if (!null_coalition) aib_check_coalition(p, c$coalition, paste0(at, ".coalition"))
        if (!is.null(c$coalitionReason)) p$str(c$coalitionReason, paste0(at, ".coalitionReason"), nullable = TRUE)
        if (null_coalition && !aib_check_is_str(c$coalitionReason)) p$add(sprintf("%s.coalitionReason: expected a string when coalition is null", at))
        # The compact's type, archetype, policy needs and secondary class under the scenario: optional, null without a coalition.
        for (k in c("type", "archetype")) {
          v <- c[[k]]
          ok <- is.null(v) || (length(v) == 1 && is.na(v)) || (aib_check_is_str(v) && v %in% (if (k == "type") aib_check_compact_types() else aib_check_archetypes()))
          if (!ok) p$add(sprintf("%s.%s: '%s' is not one of %s", at, k, paste(format(v), collapse = ","),
                                 paste(if (k == "type") aib_check_compact_types() else aib_check_archetypes(), collapse = ", ")))
        }
        if (!is.null(c$secondaryClass)) p$str(c$secondaryClass, paste0(at, ".secondaryClass"), nullable = TRUE)
        for (k in c("anchors", "suppliers", "hosts", "specialtyHosts", "developmentHosts", "technologyPartners", "bridges", "associates", "policyNeeds")) if (!is.null(c[[k]])) p$str_arr(c[[k]], paste0(at, ".", k))
        if (!is.null(c$roles)) aib_check_roles(p, c$roles, paste0(at, ".roles"))
        if (!is.null(c$metrics)) aib_check_metrics(p, c$metrics, paste0(at, ".metrics"))
        if (!is.null(c$path)) aib_check_path(p, c$path, paste0(at, ".path"))
        if (!is.null(c$trade)) aib_check_trade_edges(p, c$trade, paste0(at, ".trade"))
        if (!is.null(c$investment)) aib_check_investment_edges(p, c$investment, paste0(at, ".investment"))
      }
    }
    aib_check_investment_flows(doc, p, node_ids = node_ids)
  } else if (identical(sc, "baseline")) {
    aib_check_supplier_lists(p, doc$nodes, "nodes", node_ids, "node")
    aib_check_supplier_lists(p, doc$compacts, "compacts", compact_ids, "compact")
  }
  aib_check_network(doc, p, node_ids = node_ids)
  p$list
}

# The alignment screen (2026-10-20; checkNodeScreenedHost() in contract.ts): node.screenedHost absent, or an
# object with host (a string), gain (-1..1 or null) and cost (0..1 or null).
aib_check_screened_hosts <- function(doc, p = aib_check_problems()) {
  for (i in seq_along(doc$nodes)) {
    node <- doc$nodes[[i]]
    if (!is.list(node) || !"screenedHost" %in% names(node)) next
    s <- node$screenedHost
    at <- sprintf("nodes[%d].screenedHost", i - 1)
    if (!aib_check_is_obj(s)) {
      p$add(sprintf("%s: not an object", at))
      next
    }
    p$str(s$host, paste0(at, ".host"))
    p$num(s$gain, paste0(at, ".gain"), nullable = TRUE, min = -1, max = 1)
    p$num(s$cost, paste0(at, ".cost"), nullable = TRUE, min = 0, max = 1)
  }
  p$list
}

# Both additive sections, the taxonomy fields, the diversification scenario, the scenario toggle's fields,
# the alignment screen, the strategic layer and the six stages.
aib_check_bundle_additions <- function(doc) c(aib_check_network(doc), aib_check_investment_flows(doc), aib_check_taxonomy(doc), aib_check_diversify(doc), aib_check_scenarios(doc),
                                                aib_check_screened_hosts(doc), aib_check_strategic(doc), aib_check_stages(doc))

# ---- The strategic layer (2026-10-20) ------------------------------------------------------------------
# `strategic` (aib_dash_strategic(); docs/aib_methodology.md 16.9): optional, so a bundle without it passes.
# The rules below are the contract the TypeScript reader is to mirror (same paths, same bounds):
#   version             a positive integer
#   framing             a string or null (absent is fine)
#   scenarios           an array of {id (one of baseline / managed / partial / decoupling, unique), label,
#                       shockId (string or null), words (string or null), investment, coalitions (booleans)}
#   defaultScenario     one of the listed scenario ids
#   roleDefs            {code (one of the seven, unique), name, noun, gates and gateLabels (2 strings each),
#                       supplyRole (boolean)}
#   thresholds          pPrimary >= pSecondary in 0-1; maxPrimary, maxSecondary integers >= 0
#   compacts            id unique, "SC-..." (never an AIB-C id or a reserved path segment); name; family and
#                       purpose strings or null; steps with strictly increasing integer `order`, a label, and
#                       aligned `nodes` / `functions` (resource, processing, components, systems); a node in
#                       one step of one compact only, and a node of the document when it lists nodes;
#                       workstreams (compacts of the document, each assigned to this compact in
#                       strategic.workstreams), alsoTouching; core / nonAllied [{iso3, role, step|null}];
#                       scenarios: an object with exactly the listed scenario ids (a compact persists in every
#                       scenario), each {counts (7 integers >= 0, plus notTested when present, that match the
#                       statuses of `nodes`), needs (3 integers <= the node count), missions (<= 3, slots
#                       primary / secondary / supporting in that order, cls one of BUILD / COORDINATE / MANAGE
#                       SPECIALIZATION, text, basis|null, targeted boolean when present),
#                       statement, policyMix, menu [{cls, instruments, nodes}], demandUsd and lostUsd >= 0 or
#                       null, exposure and coverage {static, existing, coordinated, postInvestment} in 0-1 or
#                       null, activeWorkstreams (a subset of workstreams), criticalPartners, criticalExternal and
#                       coalition (arrays, or null where the scenario has no coalitions), alliedOutsideCore and
#                       nonAlliedReliedOn (when present: arrays or null), buildHosts, buildHostsNonAllied (an
#                       array when present), buildHostTies (an integer >= 0 or null when present), classMix
#                       (string or null), headline, stressNote (string or null when present), nodes ([node id,
#                       status, coverage|null, host|null] or [..., tied hosts >= 1|null] in spine order: the
#                       steps' nodes, every one, in that order)};
#                       division [[ISO3, node id of the compact, share 0-1|null, share 0-1|null, rank >= 1|null,
#                       flags of a / s / h / m]]
#   workstreams         {id unique, compact (a strategic compact or null), basis (demand / override / none),
#                       share 0-1|null, split [[compact, share]], alsoSpans (strategic compacts), straddles,
#                       overrideReason|null, active (listed scenario ids)}; every compact of the document
#                       appears exactly once
#   countries           {iso3 unique, allied, majorSupplier, primary (<= maxPrimary), secondary (<=
#                       maxSecondary), beyondCap (role codes; a non-allied economy only the supply roles),
#                       roles [[code, status|null, rank|null, strength, gate 1 value, gate 1 pct, gate 2 value,
#                       gate 2 pct]] (one per code, statuses matching the three lists), metrics (an object of
#                       numbers or null), slots (null or the ten slots, strings or null), items (absent, null
#                       or {firstOn: role gates, focus: node functions, compacts: strategic compacts, partners:
#                       economies of strategic.countries, contributes / needs [[node of a strategic compact,
#                       share 0-1|null]], capacity [[node, US$bn >= 0|null]], capacityUsdBn >= 0|null}),
#                       narrative|null}
#   compactRoles        {compact (a strategic compact), iso3, unique per pair, allied, role, detail|null,
#                       functions, leadNode (a node of the compact or null), leadStep|null, leadShare 0-1|null,
#                       leadBasis|null, keyNodes, supplier, demand, investment 0-1|null, resilienceManaged and
#                       resilienceDecoupling >= 0|null, criticality|null, relevance >= 0|null}
#   theses              [[ISO3, ISO3, strategic compact ids, logic labels]]

aib_strategic_check_scenarios <- function() c("baseline", "managed", "partial", "decoupling")
aib_strategic_check_codes <- function() c("RPA", "STM", "SMI", "DMA", "CIA", "DSP", "CBH")
aib_strategic_check_statuses <- function() c(aib_check_classes(), "NOT STRESSED", "NOT TESTED", "NO SHOCK")
aib_strategic_check_logic <- function() c("trade", "investment", "technology", "development / capacity building", "market access", "resilience / second sourcing")

aib_check_strategic <- function(doc, p = aib_check_problems()) {
  if (!"strategic" %in% names(doc)) return(p$list)
  s <- doc$strategic
  if (!aib_check_is_obj(s)) {
    p$add("strategic: not an object")
    return(p$list)
  }
  nullish <- function(x) is.null(x) || (length(x) == 1 && is.na(x))
  arr <- function(x, at) {
    ok <- aib_check_is_arr(x)
    if (!ok) p$add(sprintf("%s: expected an array", at))
    ok
  }
  str_arr_in <- function(x, at, allowed, what) {
    p$str_arr(x, at)
    if (is.character(x)) for (v in setdiff(x, allowed)) p$add(sprintf("%s: '%s' is not %s", at, v, what))
  }
  int <- function(x, at, min = 0, max = NULL, nullable = FALSE) p$num(x, at, nullable = nullable, min = min, max = max, int = TRUE)
  share <- function(x, at, nullable = TRUE) p$num(x, at, nullable = nullable, min = 0, max = 1)
  doc_nodes <- if (is.list(doc$nodes)) vapply(doc$nodes, function(n) if (is.list(n) && aib_check_is_str(n$id)) n$id else NA_character_, "") else NULL
  doc_compacts <- if (is.list(doc$compacts)) vapply(doc$compacts, function(c) if (is.list(c) && aib_check_is_str(c$id)) c$id else NA_character_, "") else NULL
  int(s$version, "strategic.version", min = 1)
  if (!is.null(s$framing)) p$str(s$framing, "strategic.framing", nullable = TRUE)
  # scenarios
  sids <- character()
  if (arr(s$scenarios, "strategic.scenarios")) {
    for (i in seq_along(s$scenarios)) {
      x <- s$scenarios[[i]]
      at <- sprintf("strategic.scenarios[%d]", i - 1)
      if (!aib_check_is_obj(x)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      if (!(aib_check_is_str(x$id) && x$id %in% aib_strategic_check_scenarios())) {
        p$add(sprintf("%s.id: '%s' is not one of %s", at, if (is.null(x$id)) "undefined" else paste(format(x$id), collapse = ","), paste(aib_strategic_check_scenarios(), collapse = ", ")))
      } else if (x$id %in% sids) {
        p$add(sprintf("%s.id: duplicate '%s'", at, x$id))
      } else {
        sids <- c(sids, x$id)
      }
      p$str(x$label, paste0(at, ".label"))
      p$str(x$shockId, paste0(at, ".shockId"), nullable = TRUE)
      p$str(x$words, paste0(at, ".words"), nullable = TRUE)
      p$bool(x$investment, paste0(at, ".investment"))
      p$bool(x$coalitions, paste0(at, ".coalitions"))
    }
    if (!length(s$scenarios)) p$add("strategic.scenarios: expected at least one scenario")
  }
  coalition_s <- if (is.list(s$scenarios)) vapply(s$scenarios, function(x) is.list(x) && isTRUE(x$coalitions), logical(1)) else logical()
  names(coalition_s) <- if (is.list(s$scenarios)) vapply(s$scenarios, function(x) if (is.list(x) && aib_check_is_str(x$id)) x$id else "", "") else character()
  if (!(aib_check_is_str(s$defaultScenario) && s$defaultScenario %in% sids)) {
    p$add(sprintf("strategic.defaultScenario: '%s' is not a listed scenario", if (is.null(s$defaultScenario)) "undefined" else paste(format(s$defaultScenario), collapse = ",")))
  }
  # role vocabulary and thresholds
  if (arr(s$roleDefs, "strategic.roleDefs")) {
    seen <- character()
    for (i in seq_along(s$roleDefs)) {
      x <- s$roleDefs[[i]]
      at <- sprintf("strategic.roleDefs[%d]", i - 1)
      if (!aib_check_is_obj(x)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      if (!(aib_check_is_str(x$code) && x$code %in% aib_strategic_check_codes())) p$add(sprintf("%s.code: not one of the seven role codes", at))
      else if (x$code %in% seen) p$add(sprintf("%s.code: duplicate '%s'", at, x$code))
      else seen <- c(seen, x$code)
      p$str(x$name, paste0(at, ".name"))
      p$str(x$noun, paste0(at, ".noun"))
      for (k in c("gates", "gateLabels")) {
        p$str_arr(x[[k]], paste0(at, ".", k))
        if (is.character(x[[k]]) && length(x[[k]]) != 2) p$add(sprintf("%s.%s: expected 2 entries", at, k))
      }
      p$bool(x$supplyRole, paste0(at, ".supplyRole"))
    }
  }
  th <- s$thresholds
  max_p <- 3L
  max_s <- 2L
  if (!aib_check_is_obj(th)) {
    p$add("strategic.thresholds: not an object")
  } else {
    share(th$pPrimary, "strategic.thresholds.pPrimary", nullable = FALSE)
    share(th$pSecondary, "strategic.thresholds.pSecondary", nullable = FALSE)
    if (aib_check_is_num(th$pPrimary) && aib_check_is_num(th$pSecondary) && th$pSecondary > th$pPrimary) p$add("strategic.thresholds.pSecondary: above pPrimary")
    int(th$maxPrimary, "strategic.thresholds.maxPrimary")
    int(th$maxSecondary, "strategic.thresholds.maxSecondary")
    if (aib_check_is_num(th$maxPrimary)) max_p <- th$maxPrimary
    if (aib_check_is_num(th$maxSecondary)) max_s <- th$maxSecondary
  }
  # workstreams (read first: compacts cross-check against them)
  sc_ids <- if (is.list(s$compacts)) vapply(s$compacts, function(k) if (is.list(k) && aib_check_is_str(k$id)) k$id else NA_character_, "") else character()
  ws_compact <- character()
  if (arr(s$workstreams, "strategic.workstreams")) {
    for (i in seq_along(s$workstreams)) {
      w <- s$workstreams[[i]]
      at <- sprintf("strategic.workstreams[%d]", i - 1)
      if (!aib_check_is_obj(w)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      p$str(w$id, paste0(at, ".id"))
      if (aib_check_is_str(w$id)) {
        if (w$id %in% names(ws_compact)) p$add(sprintf("%s.id: duplicate '%s'", at, w$id))
        if (!is.null(doc_compacts) && !w$id %in% doc_compacts) p$add(sprintf("%s.id: '%s' is not a compact of the document", at, w$id))
      }
      p$str(w$compact, paste0(at, ".compact"), nullable = TRUE)
      if (aib_check_is_str(w$compact) && !w$compact %in% sc_ids) p$add(sprintf("%s.compact: '%s' is not a strategic compact", at, w$compact))
      if (aib_check_is_str(w$id)) ws_compact[w$id] <- if (aib_check_is_str(w$compact)) w$compact else NA_character_
      if (!(aib_check_is_str(w$basis) && w$basis %in% c("demand", "override", "none"))) p$add(sprintf("%s.basis: not one of demand, override, none", at))
      share(w$share, paste0(at, ".share"))
      if (arr(w$split, paste0(at, ".split"))) {
        for (j in seq_along(w$split)) {
          t <- w$split[[j]]
          ta <- sprintf("%s.split[%d]", at, j - 1)
          if (!is.list(t) || length(t) != 2) {
            p$add(sprintf("%s: expected [compact, share]", ta))
            next
          }
          if (!(aib_check_is_str(t[[1]]) && t[[1]] %in% sc_ids)) p$add(sprintf("%s[0]: not a strategic compact", ta))
          share(t[[2]], paste0(ta, "[1]"), nullable = FALSE)
        }
      }
      str_arr_in(w$alsoSpans, paste0(at, ".alsoSpans"), sc_ids, "a strategic compact")
      p$bool(w$straddles, paste0(at, ".straddles"))
      p$str(w$overrideReason, paste0(at, ".overrideReason"), nullable = TRUE)
      str_arr_in(w$active, paste0(at, ".active"), sids, "a listed scenario")
    }
    if (!is.null(doc_compacts)) for (id in setdiff(stats::na.omit(doc_compacts), names(ws_compact))) p$add(sprintf("strategic.workstreams: compact '%s' of the document is not a workstream", id))
  }
  # compacts
  node_seen <- character()
  compact_nodes <- list()
  if (arr(s$compacts, "strategic.compacts")) {
    seen <- character()
    for (i in seq_along(s$compacts)) {
      k <- s$compacts[[i]]
      at <- sprintf("strategic.compacts[%d]", i - 1)
      if (!aib_check_is_obj(k)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      if (!(aib_check_is_str(k$id) && grepl("^SC-[A-Z0-9-]+$", k$id))) {
        p$add(sprintf("%s.id: expected an SC-... id", at))
      } else if (k$id %in% seen) {
        p$add(sprintf("%s.id: duplicate '%s'", at, k$id))
      } else {
        seen <- c(seen, k$id)
      }
      p$str(k$name, paste0(at, ".name"))
      p$str(k$family, paste0(at, ".family"), nullable = TRUE)
      p$str(k$purpose, paste0(at, ".purpose"), nullable = TRUE)
      spine <- character()
      if (arr(k$steps, paste0(at, ".steps"))) {
        if (!length(k$steps)) p$add(sprintf("%s.steps: expected at least one step", at))
        last <- -Inf
        for (j in seq_along(k$steps)) {
          x <- k$steps[[j]]
          sa <- sprintf("%s.steps[%d]", at, j - 1)
          if (!aib_check_is_obj(x)) {
            p$add(sprintf("%s: not an object", sa))
            next
          }
          int(x$order, paste0(sa, ".order"), min = -Inf)
          if (aib_check_is_num(x$order)) {
            if (x$order <= last) p$add(sprintf("%s.order: %s does not follow %s (orders rise strictly)", sa, format(x$order), format(last)))
            last <- x$order
          }
          p$str(x$label, paste0(sa, ".label"))
          p$str(x$description, paste0(sa, ".description"), nullable = TRUE)
          p$str_arr(x$nodes, paste0(sa, ".nodes"))
          str_arr_in(x$functions, paste0(sa, ".functions"), c("resource", "processing", "components", "systems"), "a node function")
          if (is.character(x$nodes)) {
            if (!length(x$nodes)) p$add(sprintf("%s.nodes: expected at least one node", sa))
            if (is.character(x$functions) && length(x$functions) != length(x$nodes)) p$add(sprintf("%s.functions: expected one per node", sa))
            for (n in x$nodes) {
              if (n %in% node_seen) p$add(sprintf("%s.nodes: '%s' is in more than one step or compact", sa, n))
              if (!is.null(doc_nodes) && !n %in% doc_nodes) p$add(sprintf("%s.nodes: '%s' is not a node of the document", sa, n))
            }
            node_seen <- c(node_seen, x$nodes)
            spine <- c(spine, x$nodes)
          }
        }
      }
      kid <- if (aib_check_is_str(k$id)) k$id else ""
      compact_nodes[[kid]] <- spine
      p$str_arr(k$workstreams, paste0(at, ".workstreams"))
      if (is.character(k$workstreams)) for (w in k$workstreams) {
        if (!w %in% names(ws_compact)) p$add(sprintf("%s.workstreams: '%s' is not in strategic.workstreams", at, w))
        else if (!identical(unname(ws_compact[w]), kid)) p$add(sprintf("%s.workstreams: '%s' is assigned to another compact", at, w))
      }
      str_arr_in(k$alsoTouching, paste0(at, ".alsoTouching"), names(ws_compact), "a workstream")
      for (side in c("core", "nonAllied")) if (arr(k[[side]], paste0(at, ".", side))) for (j in seq_along(k[[side]])) {
        x <- k[[side]][[j]]
        ca <- sprintf("%s.%s[%d]", at, side, j - 1)
        if (!aib_check_is_obj(x)) {
          p$add(sprintf("%s: not an object", ca))
          next
        }
        p$str(x$iso3, paste0(ca, ".iso3"))
        p$str(x$role, paste0(ca, ".role"))
        p$str(x$step, paste0(ca, ".step"), nullable = TRUE)
      }
      sc <- k$scenarios
      if (!aib_check_is_obj(sc)) {
        p$add(sprintf("%s.scenarios: not an object", at))
      } else {
        for (miss in setdiff(sids, names(sc))) p$add(sprintf("%s.scenarios: no '%s' (a strategic compact exists in every scenario)", at, miss))
        for (extra in setdiff(names(sc), sids)) p$add(sprintf("%s.scenarios.%s: not a listed scenario", at, extra))
        for (sid in intersect(names(sc), sids)) {
          x <- sc[[sid]]
          xa <- sprintf("%s.scenarios.%s", at, sid)
          if (!aib_check_is_obj(x)) {
            p$add(sprintf("%s: not an object", xa))
            next
          }
          cnt_keys <- c(build = "BUILD", coordinate = "COORDINATE", manage = "MANAGE SPECIALIZATION", low = "LOW INTERVENTION",
                        insufficient = "INSUFFICIENT DATA", notStressed = "NOT STRESSED", notTested = "NOT TESTED", noShock = "NO SHOCK")
          statuses <- character()
          if (arr(x$nodes, paste0(xa, ".nodes"))) {
            ids <- character()
            for (j in seq_along(x$nodes)) {
              t <- x$nodes[[j]]
              ta <- sprintf("%s.nodes[%d]", xa, j - 1)
              if (!is.list(t) || !length(t) %in% c(4, 5)) {
                p$add(sprintf("%s: expected [node, status, coverage, host] or [node, status, coverage, host, tied hosts]", ta))
                next
              }
              if (length(t) == 5) int(t[[5]], paste0(ta, "[4]"), min = 1, nullable = TRUE)
              p$str(t[[1]], paste0(ta, "[0]"))
              ids <- c(ids, if (aib_check_is_str(t[[1]])) t[[1]] else NA_character_)
              if (!(aib_check_is_str(t[[2]]) && t[[2]] %in% aib_strategic_check_statuses())) p$add(sprintf("%s[1]: not a node status", ta))
              else statuses <- c(statuses, t[[2]])
              share(t[[3]], paste0(ta, "[2]"))
              p$str(t[[4]], paste0(ta, "[3]"), nullable = TRUE)
            }
            if (!identical(ids, spine)) p$add(sprintf("%s.nodes: not the compact's spine nodes in step order", xa))
          }
          if (!aib_check_is_obj(x$counts)) {
            p$add(sprintf("%s.counts: not an object", xa))
          } else {
            for (ck in names(cnt_keys)) {
              if (ck == "notTested" && is.null(x$counts[[ck]])) {
                if ("NOT TESTED" %in% statuses) p$add(sprintf("%s.counts.notTested: absent where the spine has NOT TESTED nodes", xa))
                next
              }
              int(x$counts[[ck]], sprintf("%s.counts.%s", xa, ck))
              if (aib_check_is_num(x$counts[[ck]]) && length(statuses) == length(spine) && x$counts[[ck]] != sum(statuses == cnt_keys[[ck]])) {
                p$add(sprintf("%s.counts.%s: %s where the spine has %d", xa, ck, format(x$counts[[ck]]), sum(statuses == cnt_keys[[ck]])))
              }
            }
          }
          if (!aib_check_is_obj(x$needs)) p$add(sprintf("%s.needs: not an object", xa))
          else for (nk in c("build", "coordinate", "manage")) int(x$needs[[nk]], sprintf("%s.needs.%s", xa, nk), max = length(spine))
          if (arr(x$missions, paste0(xa, ".missions"))) {
            if (length(x$missions) > 3) p$add(sprintf("%s.missions: more than 3", xa))
            slots <- character()
            for (j in seq_along(x$missions)) {
              mm <- x$missions[[j]]
              ma <- sprintf("%s.missions[%d]", xa, j - 1)
              if (!aib_check_is_obj(mm)) {
                p$add(sprintf("%s: not an object", ma))
                next
              }
              if (!(aib_check_is_str(mm$slot) && mm$slot %in% c("primary", "secondary", "supporting"))) p$add(sprintf("%s.slot: not primary, secondary or supporting", ma))
              else slots <- c(slots, mm$slot)
              if (!(aib_check_is_str(mm$cls) && mm$cls %in% c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION"))) p$add(sprintf("%s.cls: not BUILD, COORDINATE or MANAGE SPECIALIZATION", ma))
              p$str(mm$text, paste0(ma, ".text"))
              p$str(mm$basis, paste0(ma, ".basis"), nullable = TRUE)
              if (!is.null(mm$targeted)) p$bool(mm$targeted, paste0(ma, ".targeted"))
            }
            if (!identical(slots, c("primary", "secondary", "supporting")[seq_along(slots)])) p$add(sprintf("%s.missions: slots out of order (primary, secondary, supporting)", xa))
          }
          p$str(x$statement, paste0(xa, ".statement"))
          str_arr_in(x$policyMix, paste0(xa, ".policyMix"), c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION", "LOW INTERVENTION"), "a policy class")
          if (arr(x$menu, paste0(xa, ".menu"))) for (j in seq_along(x$menu)) {
            mm <- x$menu[[j]]
            ma <- sprintf("%s.menu[%d]", xa, j - 1)
            if (!aib_check_is_obj(mm)) {
              p$add(sprintf("%s: not an object", ma))
              next
            }
            p$str(mm$cls, paste0(ma, ".cls"))
            p$str_arr(mm$instruments, paste0(ma, ".instruments"))
            str_arr_in(mm$nodes, paste0(ma, ".nodes"), spine, "a node of the compact")
          }
          for (uk in c("demandUsd", "lostUsd")) p$num(x[[uk]], paste0(xa, ".", uk), nullable = TRUE, min = 0)
          share(x$exposure, paste0(xa, ".exposure"))
          if (!aib_check_is_obj(x$coverage)) p$add(sprintf("%s.coverage: not an object", xa))
          else for (ck in c("static", "existing", "coordinated", "postInvestment")) {
            if (!ck %in% names(x$coverage)) p$add(sprintf("%s.coverage.%s: expected a number or null", xa, ck)) else share(x$coverage[[ck]], sprintf("%s.coverage.%s", xa, ck))
          }
          str_arr_in(x$activeWorkstreams, paste0(xa, ".activeWorkstreams"), if (is.character(k$workstreams)) k$workstreams else character(), "a workstream of this compact")
          has_co <- isTRUE(coalition_s[sid])
          for (ck in c("criticalPartners", "criticalExternal", "coalition")) {
            v <- x[[ck]]
            if (nullish(v)) {
              if (has_co) p$add(sprintf("%s.%s: expected an array (the scenario has coalitions)", xa, ck))
            } else {
              p$str_arr(v, sprintf("%s.%s", xa, ck))
            }
          }
          for (ck in c("alliedOutsideCore", "nonAlliedReliedOn")) if (!nullish(x[[ck]])) p$str_arr(x[[ck]], sprintf("%s.%s", xa, ck))
          p$str_arr(x$buildHosts, paste0(xa, ".buildHosts"))
          if (!is.null(x$buildHostsNonAllied)) p$str_arr(x$buildHostsNonAllied, paste0(xa, ".buildHostsNonAllied"))
          if (!is.null(x$buildHostTies)) int(x$buildHostTies, paste0(xa, ".buildHostTies"), nullable = TRUE)
          p$str(x$classMix, paste0(xa, ".classMix"), nullable = TRUE)
          p$str(x$headline, paste0(xa, ".headline"))
          if (!is.null(x$stressNote)) p$str(x$stressNote, paste0(xa, ".stressNote"), nullable = TRUE)
        }
      }
      if (arr(k$division, paste0(at, ".division"))) for (j in seq_along(k$division)) {
        t <- k$division[[j]]
        ta <- sprintf("%s.division[%d]", at, j - 1)
        if (!is.list(t) || length(t) != 6) {
          p$add(sprintf("%s: expected [ISO3, node, supply share, demand share, host rank, flags]", ta))
          next
        }
        p$str(t[[1]], paste0(ta, "[0]"))
        if (!(aib_check_is_str(t[[2]]) && t[[2]] %in% spine)) p$add(sprintf("%s[1]: not a node of the compact", ta))
        share(t[[3]], paste0(ta, "[2]"))
        share(t[[4]], paste0(ta, "[3]"))
        int(t[[5]], paste0(ta, "[4]"), min = 1, nullable = TRUE)
        if (!(is.character(t[[6]]) && length(t[[6]]) == 1 && !is.na(t[[6]]) && grepl("^[ashm]*$", t[[6]]))) p$add(sprintf("%s[5]: expected flags of a, s, h, m", ta))
      }
    }
  }
  # countries
  codes <- aib_strategic_check_codes()
  gate_names <- if (is.list(s$roleDefs)) unique(unlist(lapply(s$roleDefs, function(x) if (is.list(x) && is.character(x$gates)) x$gates)))
  if (is.null(gate_names)) gate_names <- character()
  cty_iso <- if (is.list(s$countries)) unlist(lapply(s$countries, function(x) if (is.list(x) && aib_check_is_str(x$iso3)) x$iso3)) else character()
  if (is.null(cty_iso)) cty_iso <- character()
  if (arr(s$countries, "strategic.countries")) {
    seen <- character()
    slot_keys <- c("headline", "secondary", "firstOn", "contributes", "focus", "needs", "dependency", "compacts", "partners", "capacity")
    for (i in seq_along(s$countries)) {
      x <- s$countries[[i]]
      at <- sprintf("strategic.countries[%d]", i - 1)
      if (!aib_check_is_obj(x)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      p$str(x$iso3, paste0(at, ".iso3"))
      if (aib_check_is_str(x$iso3)) {
        if (x$iso3 %in% seen) p$add(sprintf("%s.iso3: duplicate '%s'", at, x$iso3))
        seen <- c(seen, x$iso3)
      }
      p$bool(x$allied, paste0(at, ".allied"))
      p$bool(x$majorSupplier, paste0(at, ".majorSupplier"))
      allowed <- if (isFALSE(x$allied)) c("RPA", "STM", "SMI") else codes
      for (k in c("primary", "secondary", "beyondCap")) str_arr_in(x[[k]], paste0(at, ".", k), allowed, if (isFALSE(x$allied)) "a supply role (non-allied economy)" else "a role code")
      if (is.character(x$primary) && length(x$primary) > max_p) p$add(sprintf("%s.primary: more than %d roles", at, max_p))
      if (is.character(x$secondary) && length(x$secondary) > max_s) p$add(sprintf("%s.secondary: more than %d roles", at, max_s))
      if (arr(x$roles, paste0(at, ".roles"))) {
        got <- list(primary = character(), secondary = character(), `qualifies beyond the cap` = character())
        rc <- character()
        for (j in seq_along(x$roles)) {
          t <- x$roles[[j]]
          ta <- sprintf("%s.roles[%d]", at, j - 1)
          if (!is.list(t) || length(t) != 8) {
            p$add(sprintf("%s: expected [code, status, rank, strength, gate 1 value, gate 1 pct, gate 2 value, gate 2 pct]", ta))
            next
          }
          if (!(aib_check_is_str(t[[1]]) && t[[1]] %in% codes)) {
            p$add(sprintf("%s[0]: not a role code", ta))
          } else {
            if (t[[1]] %in% rc) p$add(sprintf("%s[0]: duplicate '%s'", ta, t[[1]]))
            rc <- c(rc, t[[1]])
          }
          if (!nullish(t[[2]])) {
            if (!(aib_check_is_str(t[[2]]) && t[[2]] %in% names(got))) p$add(sprintf("%s[1]: not primary, secondary, qualifies beyond the cap or null", ta))
            else if (aib_check_is_str(t[[1]])) got[[t[[2]]]] <- c(got[[t[[2]]]], t[[1]])
          }
          int(t[[3]], paste0(ta, "[2]"), min = 1, nullable = TRUE)
          share(t[[4]], paste0(ta, "[3]"))
          p$num(t[[5]], paste0(ta, "[4]"), nullable = TRUE)
          share(t[[6]], paste0(ta, "[5]"))
          p$num(t[[7]], paste0(ta, "[6]"), nullable = TRUE)
          share(t[[8]], paste0(ta, "[7]"))
        }
        lists <- c(primary = "primary", secondary = "secondary", `qualifies beyond the cap` = "beyondCap")
        for (k in names(lists)) if (is.character(x[[lists[[k]]]]) && !setequal(x[[lists[[k]]]], got[[k]])) p$add(sprintf("%s.%s: does not match the roles marked %s", at, lists[[k]], k))
      }
      if (!is.null(x$metrics)) {
        if (!is.list(x$metrics) || (length(x$metrics) && is.null(names(x$metrics)))) p$add(sprintf("%s.metrics: not an object", at))
        else for (mk in names(x$metrics)) p$num(x$metrics[[mk]], sprintf("%s.metrics.%s", at, mk), nullable = TRUE)
      }
      if (!nullish(x$slots)) {
        if (!aib_check_is_obj(x$slots)) p$add(sprintf("%s.slots: not an object", at))
        else for (sk in slot_keys) p$str(x$slots[[sk]], sprintf("%s.slots.%s", at, sk), nullable = TRUE)
      }
      if (!nullish(x$items)) {
        it <- x$items
        ia <- paste0(at, ".items")
        if (!aib_check_is_obj(it)) {
          p$add(sprintf("%s: not an object", ia))
        } else {
          str_arr_in(it$firstOn, paste0(ia, ".firstOn"), gate_names, "a role gate")
          str_arr_in(it$focus, paste0(ia, ".focus"), c("resource", "processing", "components", "systems"), "a node function")
          str_arr_in(it$compacts, paste0(ia, ".compacts"), sc_ids, "a strategic compact")
          str_arr_in(it$partners, paste0(ia, ".partners"), cty_iso, "an economy of strategic.countries")
          for (k in c("contributes", "needs", "capacity")) if (arr(it[[k]], sprintf("%s.%s", ia, k))) {
            for (j in seq_along(it[[k]])) {
              t <- it[[k]][[j]]
              ta <- sprintf("%s.%s[%d]", ia, k, j - 1)
              if (!is.list(t) || length(t) != 2) {
                p$add(sprintf("%s: expected [node id, number]", ta))
                next
              }
              if (!(aib_check_is_str(t[[1]]) && t[[1]] %in% node_seen)) p$add(sprintf("%s[0]: not a node of a strategic compact", ta))
              if (k == "capacity") p$num(t[[2]], paste0(ta, "[1]"), nullable = TRUE, min = 0) else share(t[[2]], paste0(ta, "[1]"))
            }
          }
          p$num(it$capacityUsdBn, paste0(ia, ".capacityUsdBn"), nullable = TRUE, min = 0)
        }
      }
      p$str(x$narrative, paste0(at, ".narrative"), nullable = TRUE)
    }
  }
  # compact-specific roles
  if (arr(s$compactRoles, "strategic.compactRoles")) {
    seen <- character()
    for (i in seq_along(s$compactRoles)) {
      x <- s$compactRoles[[i]]
      at <- sprintf("strategic.compactRoles[%d]", i - 1)
      if (!aib_check_is_obj(x)) {
        p$add(sprintf("%s: not an object", at))
        next
      }
      if (!(aib_check_is_str(x$compact) && x$compact %in% sc_ids)) p$add(sprintf("%s.compact: not a strategic compact", at))
      p$str(x$iso3, paste0(at, ".iso3"))
      if (aib_check_is_str(x$compact) && aib_check_is_str(x$iso3)) {
        key <- paste(x$compact, x$iso3)
        if (key %in% seen) p$add(sprintf("%s: duplicate %s / %s", at, x$compact, x$iso3))
        seen <- c(seen, key)
      }
      p$bool(x$allied, paste0(at, ".allied"))
      p$str(x$role, paste0(at, ".role"))
      for (k in c("detail", "leadStep", "leadBasis", "criticality")) p$str(x[[k]], paste0(at, ".", k), nullable = TRUE)
      p$str_arr(x$functions, paste0(at, ".functions"))
      p$str_arr(x$keyNodes, paste0(at, ".keyNodes"))
      p$str(x$leadNode, paste0(at, ".leadNode"), nullable = TRUE)
      if (aib_check_is_str(x$leadNode) && aib_check_is_str(x$compact) && !x$leadNode %in% compact_nodes[[x$compact]]) {
        p$add(sprintf("%s.leadNode: '%s' is not a node of %s", at, x$leadNode, x$compact))
      }
      for (k in c("leadShare", "supplier", "demand", "investment")) share(x[[k]], paste0(at, ".", k))
      for (k in c("resilienceManaged", "resilienceDecoupling", "relevance")) p$num(x[[k]], paste0(at, ".", k), nullable = TRUE, min = 0)
    }
  }
  # bilateral theses
  if (arr(s$theses, "strategic.theses")) for (i in seq_along(s$theses)) {
    t <- s$theses[[i]]
    at <- sprintf("strategic.theses[%d]", i - 1)
    if (!is.list(t) || length(t) != 4) {
      p$add(sprintf("%s: expected [ISO3, ISO3, compacts, logic]", at))
      next
    }
    p$str(t[[1]], paste0(at, "[0]"))
    p$str(t[[2]], paste0(at, "[1]"))
    str_arr_in(t[[3]], paste0(at, "[2]"), sc_ids, "a strategic compact")
    str_arr_in(t[[4]], paste0(at, "[3]"), aib_strategic_check_logic(), "a relationship logic")
  }
  p$list
}
