# The R mirror of the runtime checks of the additive AIB bundle sections in dashboard/src/aib/contract.ts
# (checkNetwork(), checkInvestmentFlows()): the emergent network (`network`) and the investor -> host
# flows (`investmentFlows`) of the compacts document, the taxonomy fields of 2026-10-19 (meta.taxonomy,
# dictionaryScope, indices; node.indexCode; compact.indices) and the diversification scenario (meta.diversify,
# node.diversify; checkNodeDiversify()) and the scenario toggle (2026-10-19: meta.scenarios /
# supplyScenarios, node.baseline, the node.diversify run fields, compact.managed / baseline:
# aib_check_scenarios(); the scenario files: aib_check_scenario_doc(), checkScenarioDoc(); the manifest's
# files.scenarios: aib_check_scenario_files()). scripts/84_export_aib_data.R stops on any problem
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
    for (k in c("nodes", "techs", "stages")) p$str_arr(e[[k]], paste0(at, ".", k))
    if (is.character(e$nodes) && !is.null(node_ids)) for (x in setdiff(e$nodes, node_ids)) p$add(sprintf("%s.nodes: '%s' is not a node of the document", at, x))
    if (is.character(e$stages)) for (x in setdiff(e$stages, c("Upstream", "Midstream", "Downstream"))) p$add(sprintf("%s.stages: '%s' is not a stage", at, x))
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
  for (k in c("greedy", "criticalPartners", "criticalExternal", "criticalExternalMarginal", "external")) if (!is.null(co[[k]])) p$str_arr(co[[k]], paste0(at, ".", k))
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
        for (k in c("anchors", "suppliers", "hosts", "technologyPartners", "bridges", "associates", "policyNeeds")) if (!is.null(c[[k]])) p$str_arr(c[[k]], paste0(at, ".", k))
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

# Both additive sections, the taxonomy fields, the diversification scenario and the scenario toggle's fields.
aib_check_bundle_additions <- function(doc) c(aib_check_network(doc), aib_check_investment_flows(doc), aib_check_taxonomy(doc), aib_check_diversify(doc), aib_check_scenarios(doc))
