# PRIZE as STRATEGIC INDUSTRIAL VALUE, and the LARGE-YARD case (docs/strategic_position.md, "2. Prize:
# Strategic Industrial Value" and "The large-yard case"; user spec 2026-10-30, Yards & Fences items 2 and 3).
#
# Strategic industrial value answers one question: is domestic productive capability in this specific node
# (a six-stage cell or a sensitive layer) strategically valuable? A large future market alone does not make
# it so. Three families per row:
#   market   the market prize of industrial_opportunity.R, unchanged: world demand growth and global
#            investment momentum (io_node_prize), lifted by national home-market growth;
#   commons  industrial-commons value, from the curated node attributes: process_knowledge (the
#            production-innovation linkage), workforce_suppliers (the supplier ecosystem) and
#            adjacent_innovation (cross-sector spillovers); yes 1, partly 0.5, no 0, the mean of those observed;
#   option   future-option value: leap_candidate, successor (named = 1), beyond_novelty, cost_perf_advantage,
#            incumbent_vulnerable, scale_pathway; the same scale, the mean of those observed. A node that is
#            not a leap candidate has no leap tests (posture_nodes.R): it is an observed 0.
# siv_score is the weighted mean of the observed families (config weights, renormalised over the families
# observed); strategic_industrial_value is its mid-rank percentile across the assessed producing-stage rows
# (as the market prize is a percentile across the producing nodes), so the prize threshold keeps its
# selectivity. A market-only node (commons and option both observed and both below the gate) is capped just
# below the prize threshold (siv_cap "market_only"): market size alone never makes a prize high.
#
# Nothing that measures Competitive Position (cp_*, home_share*, export or production shares, the capability
# floor) or need (supply_chain_need, import dependence, concentration) enters: sp_siv_check_inputs() refuses
# them. Current capability stays in Position. The commons and option attributes describe the node, never the
# country (complementary_capabilities, a country's own strengths, is not read).
#
# The large-yard case is the countervailing test (spec item 3): a capability where affordable, diversified
# access should dominate domestic industrial policy, whatever the size of the market. Ten signals, each
# TRUE / FALSE / NA, from the node attributes and the world supply structure; market size is never one. The
# Yards & Fences engine reaches its Large Yard when no capability rationale holds (dashboard/src/fence/engine/
# gates.ts: no position, weak learning, no leap) and names the route to it "production becomes commoditised
# and manufacturing generates little future learning" (classify.ts); the signals spell that out.
#
# Pure functions: no IO, no yard placement from the paper (paper_placement is never read here). Needs
# sp_helpers.R and industrial_opportunity.R (SP_IO_FORBIDDEN, SP_IO_PRODUCING_STAGES).

SP_SIV_FAMILIES <- c("market", "commons", "option")
SP_SIV_COMMONS <- c("process_knowledge", "workforce_suppliers", "adjacent_innovation")
SP_SIV_OPTION <- c("leap_candidate", "successor", "beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway")
SP_SIV_COLUMNS <- c("siv_market", "siv_commons", "siv_option", "siv_score", "strategic_industrial_value", "siv_cap",
                    "siv_families_observed")
SP_PRIZE_MEASURES <- c("strategic_value", "market")
# Column-name patterns of Competitive Position, capability and need that may never enter the prize (on top of
# SP_IO_FORBIDDEN).
SP_SIV_FORBIDDEN_PATTERNS <- c("^cp_", "^home_share", "^pw_", "^pathway_", "^competitive_position", "^t3_", "^sn_",
                               "^supply_chain_need", "^deployment_need", "^strategic_need")

SP_LY_SIGNALS <- c("mature", "passive", "standardized", "substitutable", "easy_qualification", "diversified",
                   "low_learning", "low_spillover", "low_differentiation", "commoditized")
SP_LY_WORDS <- c(mature = "mature", passive = "passive", standardized = "standardized",
                 substitutable = "substitutable", easy_qualification = "quick to qualify", diversified = "diversified world supply",
                 low_learning = "little process learning", low_spillover = "little cross-sector spillover",
                 low_differentiation = "little product differentiation", commoditized = "commoditized")
SP_LY_ATTRS <- c("maturity", "digitally_active", "standardization", "substitutability", "qualification_time",
                 "process_knowledge", "adjacent_innovation", "differentiation", "commoditization", "leap_candidate")

# ---- Configuration ------------------------------------------------------------------------------------
# decision_framework$strategic_value with its defaults (config/strategic_position.yml documents each).
sp_siv_cfg <- function(cfg = list()) {
  s <- cfg$strategic_value %||% list()
  w <- unlist(s$weights %||% list(market = 1, commons = 1, option = 1))
  w <- stats::setNames(vapply(SP_SIV_FAMILIES, function(f) if (f %in% names(w)) as.numeric(w[[f]]) else 1, numeric(1)), SP_SIV_FAMILIES)
  if (any(is.na(w) | w < 0) || sum(w) <= 0) stop("sp_siv_cfg(): strategic_value weights must be non-negative with a positive sum", call. = FALSE)
  measure <- s$prize_measure %||% "strategic_value"
  if (!measure %in% SP_PRIZE_MEASURES) {
    stop("sp_siv_cfg(): strategic_value$prize_measure must be one of ", paste(SP_PRIZE_MEASURES, collapse = ", "), call. = FALSE)
  }
  list(weights = w, gate = s$gate %||% 0.5, cap_margin = s$cap_margin %||% 0.01, prize_measure = measure,
       market_column = s$market_column %||% "industrial_opportunity", require_market = !isFALSE(s$require_market),
       prize_threshold = cfg$prize_threshold %||% 0.6)
}

# decision_framework$large_yard with its defaults.
sp_ly_cfg <- function(cfg = list()) {
  l <- cfg$large_yard %||% list()
  req <- as.character(unlist(l$required %||% c("mature", "passive")))
  bad <- setdiff(req, SP_LY_SIGNALS)
  if (length(bad)) stop("sp_ly_cfg(): unknown required large-yard signal(s): ", paste(bad, collapse = ", "), call. = FALSE)
  k <- as.integer(l$min_signals %||% 5L)
  n_other <- length(setdiff(SP_LY_SIGNALS, req))
  if (is.na(k) || k < 0 || k > n_other) stop("sp_ly_cfg(): large_yard$min_signals must be between 0 and ", n_other, call. = FALSE)
  list(enabled = !isFALSE(l$enabled), required = req, min_signals = k,
       diversified_top_share_max = l$diversified_top_share_max %||% 0.5, option_veto = l$option_veto %||% 0.5,
       position_first = isTRUE(l$position_first))
}

# ---- Guards and attribute lookup ------------------------------------------------------------------------
# Refuse a list of prize inputs that holds a Competitive Position, capability or need variable.
sp_siv_check_inputs <- function(components) {
  components <- as.character(components)
  hit <- Reduce(`|`, lapply(SP_SIV_FORBIDDEN_PATTERNS, grepl, x = components), rep(FALSE, length(components)))
  bad <- unique(c(intersect(components, SP_IO_FORBIDDEN), components[hit]))
  if (length(bad)) {
    stop("sp_siv_check_inputs(): Competitive Position, capability or need variable(s) in the strategic industrial value: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# The node attributes `fields` aligned to the rows of tbl, as character with "" and "Unknown" read as NA.
# attrs: the resolved node table (posture_nodes.R pn_resolve(): iso3, tech, stage, layer_id, ...), or any
# table keyed by tech, stage (or supply_chain), optionally layer_id (absent = "stage") and iso3 (absent =
# country-invariant). A tbl without layer_id is a cell table (layer "stage"). attrs NULL: tbl's own columns.
sp_node_attrs <- function(tbl, attrs = NULL, fields) {
  n <- nrow(tbl)
  if (is.null(attrs)) {
    src <- as.data.frame(tbl, stringsAsFactors = FALSE)
    idx <- seq_len(n)
  } else {
    src <- as.data.frame(attrs, stringsAsFactors = FALSE)
    if (!"supply_chain" %in% names(src) && "stage" %in% names(src)) src$supply_chain <- src$stage
    if (!all(c("tech", "supply_chain") %in% names(src))) stop("sp_node_attrs(): attrs need tech and stage (or supply_chain)", call. = FALSE)
    if (!"layer_id" %in% names(src)) src$layer_id <- "stage"
    by_iso <- "iso3" %in% names(src)
    if (by_iso && !"iso3" %in% names(tbl)) stop("sp_node_attrs(): attrs are per country (iso3) but the table has no iso3", call. = FALSE)
    layer_t <- if ("layer_id" %in% names(tbl)) dplyr::coalesce(as.character(tbl$layer_id), "stage") else rep("stage", n)
    k_src <- paste(if (by_iso) src$iso3 else "", src$tech, src$supply_chain, src$layer_id, sep = "|")
    k_tbl <- paste(if (by_iso) tbl$iso3 else "", tbl$tech, tbl$supply_chain, layer_t, sep = "|")
    if (anyDuplicated(k_src)) stop("sp_node_attrs(): attrs hold more than one row per node: ", k_src[anyDuplicated(k_src)], call. = FALSE)
    idx <- match(k_tbl, k_src)
  }
  out <- lapply(fields, function(f) {
    v <- if (f %in% names(src)) trimws(as.character(src[[f]]))[idx] else rep(NA_character_, n)
    v[v %in% c("", "Unknown")] <- NA_character_
    v
  })
  out <- as.data.frame(stats::setNames(out, fields), stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# yes 1, partly 0.5, no 0; anything else NA.
sp_siv_level <- function(v) unname(c(no = 0, partly = 0.5, yes = 1)[as.character(v)])

# The commons and option families of aligned attributes (sp_node_attrs() over SP_SIV_COMMONS and SP_SIV_OPTION).
sp_siv_families <- function(a) {
  n <- nrow(a)
  get <- function(f) if (f %in% names(a)) a[[f]] else rep(NA_character_, n)
  cm <- do.call(cbind, lapply(SP_SIV_COMMONS, function(f) sp_siv_level(get(f))))
  succ <- get("successor")
  om <- cbind(sp_siv_level(get("leap_candidate")), ifelse(is.na(succ), NA_real_, 1),
              do.call(cbind, lapply(setdiff(SP_SIV_OPTION, c("leap_candidate", "successor")), function(f) sp_siv_level(get(f)))))
  list(commons = sp_row_mean_available(matrix(cm, nrow = n)), option = sp_row_mean_available(matrix(om, nrow = n)))
}

# Mid-rank position of each value among a reference set: (below + half of equal) / n.
sp_siv_ref_pct <- function(v, ref) {
  ref <- ref[!is.na(ref)]
  if (!length(ref)) return(rep(NA_real_, length(v)))
  vapply(v, function(z) if (is.na(z)) NA_real_ else (sum(ref < z) + 0.5 * sum(ref == z)) / length(ref), numeric(1))
}

# ---- Strategic industrial value ---------------------------------------------------------------------------
# tbl: cells or layers (iso3, tech, supply_chain, optional layer_id, the market column); attrs: as for
# sp_node_attrs(); cfg: the decision_framework list; ref: optional reference siv_score values (the cells')
# to place these rows among, so layers share the cells' scale; NULL ranks the rows among themselves.
# Adds SP_SIV_COLUMNS. Deployment rows get none (Deployment is decided on deployment_need).
sp_strategic_value <- function(tbl, attrs = NULL, cfg = list(), ref = NULL) {
  sc <- sp_siv_cfg(cfg)
  sp_siv_check_inputs(c(sc$market_column, SP_SIV_COMMONS, SP_SIV_OPTION))
  n <- nrow(tbl)
  prod <- tbl$supply_chain %in% SP_IO_PRODUCING_STAGES
  market <- if (sc$market_column %in% names(tbl)) suppressWarnings(as.numeric(tbl[[sc$market_column]])) else rep(NA_real_, n)
  fam <- sp_siv_families(sp_node_attrs(tbl, attrs, c(SP_SIV_COMMONS, SP_SIV_OPTION)))
  m <- cbind(market = market, commons = fam$commons, option = fam$option)
  m[!prod, ] <- NA_real_
  w <- matrix(rep(sc$weights, each = n), nrow = n, ncol = 3)
  w[is.na(m)] <- 0
  den <- rowSums(w)
  score <- ifelse(den > 0, rowSums(w * ifelse(is.na(m), 0, m)) / den, NA_real_)
  if (sc$require_market) score[is.na(m[, "market"])] <- NA_real_
  pct <- if (is.null(ref)) sp_midrank_pct(score) else sp_siv_ref_pct(score, ref)
  market_only <- !is.na(score) & !is.na(m[, "commons"]) & !is.na(m[, "option"]) &
    m[, "commons"] < sc$gate & m[, "option"] < sc$gate
  cap <- sc$prize_threshold - sc$cap_margin
  tbl$siv_market <- unname(m[, "market"])
  tbl$siv_commons <- unname(m[, "commons"])
  tbl$siv_option <- unname(m[, "option"])
  tbl$siv_score <- score
  tbl$strategic_industrial_value <- ifelse(market_only, pmin(pct, cap), pct)
  tbl$siv_cap <- ifelse(market_only, "market_only", NA_character_)
  tbl$siv_families_observed <- ifelse(prod, as.integer(rowSums(!is.na(m) & matrix(rep(sc$weights > 0, each = n), nrow = n, ncol = 3))), NA_integer_)
  tbl
}

# ---- The large-yard case -----------------------------------------------------------------------------------
# The ten signals, one logical column each (TRUE / FALSE / NA = not observed):
#   mature               maturity mature
#   passive              digitally_active no (a whole cell is judged on the physical path, so it is passive by
#                        convention; control risks live in control layers, which are digitally active)
#   standardized         standardization high
#   substitutable        substitutability easy or moderate
#   easy_qualification   qualification_time lt3m or m3to12
#   diversified          world supply is not concentrated: the largest supplier's share of world supply
#                        (world_top_share, supply_structure.R; for a mineral cell its driving mineral) below
#                        diversified_top_share_max. Where world_top_share is missing, a coercive_top_share at or
#                        above the line settles it as FALSE (the largest coercive supplier is a lower bound of the
#                        largest supplier); otherwise NA. Layers read their parent cell's world supply.
#   low_learning         process_knowledge no
#   low_spillover        adjacent_innovation no
#   low_differentiation  differentiation low
#   commoditized         commoditization yes
sp_large_yard_signals <- function(tbl, attrs = NULL, cfg = list()) {
  lc <- sp_ly_cfg(cfg)
  n <- nrow(tbl)
  a <- sp_node_attrs(tbl, attrs, SP_LY_ATTRS)
  is_ <- function(v, yes) ifelse(is.na(v), NA, v %in% yes)
  num <- function(nm) if (nm %in% names(tbl)) suppressWarnings(as.numeric(tbl[[nm]])) else rep(NA_real_, n)
  top <- num("world_top_share")
  coer <- num("coercive_top_share")
  mx <- lc$diversified_top_share_max
  data.frame(
    mature = is_(a$maturity, "mature"),
    passive = is_(a$digitally_active, "no"),
    standardized = is_(a$standardization, "high"),
    substitutable = is_(a$substitutability, c("easy", "moderate")),
    easy_qualification = is_(a$qualification_time, c("lt3m", "m3to12")),
    diversified = ifelse(!is.na(top), top < mx, ifelse(!is.na(coer) & coer >= mx, FALSE, NA)),
    low_learning = is_(a$process_knowledge, "no"),
    low_spillover = is_(a$adjacent_innovation, "no"),
    low_differentiation = is_(a$differentiation, "low"),
    commoditized = is_(a$commoditization, "yes"),
    stringsAsFactors = FALSE
  )
}

# The large-yard case per row. TRUE when every required signal holds (config, default mature AND passive),
# at least min_signals of the other signals hold, and no future-option veto applies (a leap candidate, or
# future-option value siv_option at or above option_veto); FALSE when a required signal fails, the veto
# holds, or too few of the other signals can hold even if every unknown one did; NA when the evidence cannot
# decide. Producing stages only (Deployment & Use is decided on deployment_need). siv_option is read from
# tbl where sp_strategic_value() has run, else computed from attrs. Adds large_yard_case, large_yard_signals
# (";" list of the signals that hold), large_yard_n (how many of the non-required signals hold: the count
# compared with min_signals), large_yard_unknown (";" list of the signals not observed) and
# large_yard_reason (one plain sentence). Market size is never a signal.
sp_large_yard_case <- function(tbl, attrs = NULL, cfg = list()) {
  lc <- sp_ly_cfg(cfg)
  n <- nrow(tbl)
  if (!n) {
    tbl$large_yard_case <- logical(); tbl$large_yard_signals <- character(); tbl$large_yard_n <- integer()
    tbl$large_yard_unknown <- character(); tbl$large_yard_reason <- character()
    return(tbl)
  }
  prod <- tbl$supply_chain %in% SP_IO_PRODUCING_STAGES
  sig <- sp_large_yard_signals(tbl, attrs, cfg)
  others <- setdiff(SP_LY_SIGNALS, lc$required)
  req <- matrix(as.matrix(sig[lc$required]), nrow = n)
  oth <- matrix(as.matrix(sig[others]), nrow = n)
  req_fail <- rowSums(!is.na(req) & !req) > 0
  req_ok <- rowSums(!is.na(req) & req) == length(lc$required)
  n_hold <- as.integer(rowSums(!is.na(oth) & oth))
  n_na <- as.integer(rowSums(is.na(oth)))
  leap <- sp_node_attrs(tbl, attrs, "leap_candidate")$leap_candidate
  opt <- if ("siv_option" %in% names(tbl)) tbl$siv_option else
    sp_siv_families(sp_node_attrs(tbl, attrs, c(SP_SIV_COMMONS, SP_SIV_OPTION)))$option
  veto_leap <- leap %in% "yes"
  veto_opt <- !is.na(opt) & opt >= lc$option_veto
  veto <- ifelse(veto_leap | veto_opt, TRUE, ifelse(is.na(leap) & is.na(opt), NA, FALSE))
  case <- dplyr::case_when(
    !prod ~ NA,
    veto %in% TRUE ~ FALSE,
    req_fail ~ FALSE,
    n_hold + n_na < lc$min_signals ~ FALSE,
    req_ok & n_hold >= lc$min_signals & veto %in% FALSE ~ TRUE,
    TRUE ~ NA
  )
  sm <- as.matrix(sig)
  listing <- function(hit) apply(matrix(hit, nrow = n), 1, function(z) if (any(z)) paste(SP_LY_SIGNALS[z], collapse = ";") else NA_character_)
  holds <- listing(!is.na(sm) & sm)
  unknown <- listing(is.na(sm))
  words <- function(ids) paste(unname(SP_LY_WORDS[ids]), collapse = ", ")
  k <- length(others)
  attr_val <- sp_node_attrs(tbl, attrs, c("maturity", "digitally_active"))
  reason <- vapply(seq_len(n), function(i) {
    if (!prod[i]) return(NA_character_)
    h <- others[!is.na(oth[i, ]) & oth[i, ]]
    u <- others[is.na(oth[i, ])]
    if (isTRUE(case[i])) {
      return(paste0("Large yard: ", if (length(lc$required)) paste0(paste(lc$required, collapse = ", "), ", and ") else "",
                    n_hold[i], " of ", k, " commodity signals (", words(h), "), with no future option to keep: affordable, ",
                    "diversified access should dominate whatever the size of the market."))
    }
    if (isFALSE(case[i])) {
      if (veto_leap[i]) return(paste0("Not a large yard: a leap candidate keeps a future option open (option value ",
                                      format(round(opt[i], 2), nsmall = 2), ")."))
      if (veto_opt[i]) return(paste0("Not a large yard: future-option value ", format(round(opt[i], 2), nsmall = 2),
                                     " keeps a domestic option open."))
      if (req_fail[i]) {
        miss <- lc$required[!is.na(req[i, ]) & !req[i, ]]
        why <- vapply(miss, function(s) switch(s,
          mature = paste0("not mature (", dplyr::coalesce(attr_val$maturity[i], "unknown"), ")"),
          passive = "digitally active (a control path to exploit)",
          paste0("not ", unname(SP_LY_WORDS[s]))), character(1))
        return(paste0("Not a large yard: ", paste(why, collapse = " and "), "."))
      }
      return(paste0("Not a large yard: ", if (n_hold[i] == 0) paste0("none of the ", k, " commodity signals holds") else
        paste0("only ", n_hold[i], " of ", k, " commodity signals hold (", words(h), ")"), ", where ", lc$min_signals, " are required."))
    }
    parts <- c(
      if (is.na(veto[i])) "leap candidacy not assessed",
      if (!req_ok[i]) paste0(words(lc$required[is.na(req[i, ])]), " not assessed"),
      if (n_hold[i] < lc$min_signals) paste0(n_hold[i], " of ", k, " commodity signals hold and ", length(u), " are not assessed (",
                                              words(u), ")")
    )
    paste0("Large-yard case undecided: ", paste(parts, collapse = "; "), ".")
  }, character(1))
  tbl$large_yard_case <- case
  tbl$large_yard_signals <- ifelse(prod, holds, NA_character_)
  tbl$large_yard_n <- ifelse(prod, n_hold, NA_integer_)
  tbl$large_yard_unknown <- ifelse(prod, unknown, NA_character_)
  tbl$large_yard_reason <- reason
  tbl
}

# ---- Diagnostics helpers (pure) ------------------------------------------------------------------------------
# The three families and the value per node (tech, supply_chain, layer_id), over countries: medians of the
# country-varying market prize and the value, the node-level commons and option, and how many rows are capped.
sp_siv_by_node <- function(tbl) {
  x <- tbl[tbl$supply_chain %in% SP_IO_PRODUCING_STAGES, , drop = FALSE]
  if (!"layer_id" %in% names(x)) x$layer_id <- rep("stage", nrow(x))
  if (!nrow(x)) return(data.frame())
  key <- paste(x$tech, x$supply_chain, x$layer_id, sep = "|")
  med <- function(v) if (all(is.na(v))) NA_real_ else round(stats::median(v, na.rm = TRUE), 3)
  out <- do.call(rbind, lapply(split(seq_len(nrow(x)), key), function(i) data.frame(
    tech = x$tech[i[1]], supply_chain = x$supply_chain[i[1]], layer_id = x$layer_id[i[1]], n_rows = length(i),
    siv_market_median = med(x$siv_market[i]), siv_commons = med(x$siv_commons[i]), siv_option = med(x$siv_option[i]),
    siv_score_median = med(x$siv_score[i]), strategic_industrial_value_median = med(x$strategic_industrial_value[i]),
    industrial_opportunity_median = if ("industrial_opportunity" %in% names(x)) med(x$industrial_opportunity[i]) else NA_real_,
    n_market_only = sum(x$siv_cap[i] %in% "market_only"), families_observed = max(c(0L, x$siv_families_observed[i]), na.rm = TRUE),
    stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out[order(out$tech, match(out$supply_chain, SP_IO_PRODUCING_STAGES), out$layer_id != "stage", out$layer_id), , drop = FALSE]
}

# The large-yard case per node, over countries: how many rows are TRUE / FALSE / undecided, and the signals and
# reason of the most common outcome (the signals are node attributes and world supply, nearly country-invariant).
sp_large_yard_by_node <- function(tbl) {
  x <- tbl[tbl$supply_chain %in% SP_IO_PRODUCING_STAGES, , drop = FALSE]
  if (!"layer_id" %in% names(x)) x$layer_id <- rep("stage", nrow(x))
  if (!nrow(x)) return(data.frame())
  key <- paste(x$tech, x$supply_chain, x$layer_id, sep = "|")
  out <- do.call(rbind, lapply(split(seq_len(nrow(x)), key), function(i) {
    case <- ifelse(is.na(x$large_yard_case[i]), "undecided", ifelse(x$large_yard_case[i], "large", "not_large"))
    top <- names(sort(table(case), decreasing = TRUE))[1]
    j <- i[case == top][1]
    data.frame(tech = x$tech[j], supply_chain = x$supply_chain[j], layer_id = x$layer_id[j], n_rows = length(i),
               n_large = sum(case == "large"), n_not_large = sum(case == "not_large"), n_undecided = sum(case == "undecided"),
               modal_case = top, large_yard_n = x$large_yard_n[j], large_yard_signals = x$large_yard_signals[j],
               large_yard_unknown = x$large_yard_unknown[j], large_yard_reason = x$large_yard_reason[j], stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out[order(out$tech, match(out$supply_chain, SP_IO_PRODUCING_STAGES), out$layer_id != "stage", out$layer_id), , drop = FALSE]
}
