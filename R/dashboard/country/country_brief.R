# Country strategy briefs ("[Country] and the Allied Industrial Base"): the facts a brief may cite, built
# from the published dashboard bundles, so a brief says exactly what the Country page shows.
#
#   posture core   dashboard/public/data/posture/posture-core.<h8>.json     (columnar, every economy)
#   posture detail dashboard/public/data/posture/posture-detail-<ISO3>.<h8>.json
#   AIB            dashboard/public/data/aib/compacts.<h8>.json (Decoupling) and scenario-managed.<h8>.json
#
# Every printable fact is an Fv: list(v = value, d = display string, src = source ids, m = meaning). The
# brief source cites facts by dot path ({{cells.electric-vehicles-refining.siv}}); the claims file records
# each citation with its value, meaning and sources. Choices, fences and yards follow the dashboard's own
# rules (dashboard/src/posture/grammar.ts verbOf / nodeFence, yards.ts): nothing is re-scored here.
# Pure: lists in, lists out. config/country_brief.yml holds the words, groups and colours.

COUNTRY_BRIEF_FACTS_SCHEMA <- "osi-country-brief-facts"

COUNTRY_BRIEF_CHOICES <- c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed")
COUNTRY_BRIEF_PRODUCING <- c("Extraction", "Refining", "Components", "Assembly", "Recovery")
# the yard of a choice (R sp_yard_of_choice(), yard_rollup.R): Secure small, Watch large, the rest medium
COUNTRY_BRIEF_YARD <- c(secure = "small", preserve = "medium", catch_up = "medium", partner = "medium", accelerate = "medium", watch = "large")
COUNTRY_BRIEF_FENCE_OF_YARD <- c(small = "high", medium = "medium", large = "low")
# the Secure rationale a layer's variant stands for (the variant words of yards.ts VARIANT_WORDS)
COUNTRY_BRIEF_VARIANT_ROUTE <- c(secure_control = "operational_control", secure_integrated = "operational_control", secure_whole = "operational_control",
                                 secure_unclear = "operational_control", secure_dual_use = "operational_control", secure_both = "coercive_chokepoint",
                                 secure_input = "coercive_chokepoint", secure_resilience = "systemic_resilience")
# a contender holds at least 0.5% of world supply (decision_engine.R; below it Competitive Position reads 0)
COUNTRY_BRIEF_CONTENDER_SHARE <- 0.005

# ---- Formatting -----------------------------------------------------------------------------------------

country_brief_fv <- function(v, d, src = character(), m = NULL) {
  if (is.null(v) || (length(v) == 1 && is.atomic(v) && is.na(v))) return(NULL)
  list(v = v, d = d, src = I(unique(as.character(src))), m = m)
}
country_brief_num <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) NA_real_ else as.numeric(x[1])
country_brief_chr <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) NA_character_ else as.character(x[1])

# Half up, as the dashboard rounds (posture/derive.ts score100: Math.round); R's round() goes half to even.
country_brief_round <- function(x, digits = 0) floor(x * 10^digits + 0.5) / 10^digits

# A 0-1 score as the dashboard's 0-100 score ("88").
country_brief_score <- function(x, src, m) {
  x <- country_brief_num(x)
  if (is.na(x)) return(NULL)
  country_brief_fv(x, as.character(country_brief_round(100 * x)), src, m)
}

# A share as a percent ("53%", "<1%" under half a percent, "0%" only when exactly zero).
country_brief_pct_text <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (x > 0 && x < 0.005) return("<1%")
  if (x >= 0.995 && x < 1) return(">99%")
  paste0(country_brief_round(100 * x), "%")
}
country_brief_pct <- function(x, src, m) {
  x <- country_brief_num(x)
  if (is.na(x)) return(NULL)
  country_brief_fv(x, country_brief_pct_text(x), src, m)
}

# US dollars in running text ("US$1.3 billion", "US$265 million").
country_brief_usd_text <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (x >= 1e9) return(sprintf("US$%s billion", format(round(x / 1e9, 1), nsmall = 1)))
  if (x >= 1e6) return(sprintf("US$%s million", format(round(x / 1e6), big.mark = ",")))
  sprintf("US$%s thousand", format(round(x / 1e3), big.mark = ","))
}
country_brief_usd <- function(x, src, m) {
  x <- country_brief_num(x)
  if (is.na(x)) return(NULL)
  country_brief_fv(x, country_brief_usd_text(x), src, m)
}

country_brief_count <- function(n, src, m, keys = NULL) {
  out <- country_brief_fv(as.integer(n), as.character(as.integer(n)), src, m)
  if (!is.null(out) && length(keys)) out$k <- I(as.character(keys))
  out
}

# A list of names in running text: "Japan, the United Kingdom and Italy". Three or more items of which one
# holds a comma or an "and" are joined with semicolons ("blades, towers and nacelle parts; wind turbines;
# and PV modules and inverters"); two items always read "a and b".
country_brief_join <- function(x, conj = "and") {
  x <- as.character(x[!is.na(x) & nzchar(x)])
  if (!length(x)) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x[1], conj, x[2]))
  if (any(grepl(",| and ", x))) return(paste0(paste(x[-length(x)], collapse = "; "), "; ", conj, " ", x[length(x)]))
  paste(paste(x[-length(x)], collapse = ", "), conj, x[length(x)])
}
# keys: the steps the list names (the lint keeps review cases out of the headlines by them)
country_brief_list <- function(x, src, m, keys = NULL) {
  x <- as.character(x[!is.na(x) & nzchar(x)])
  out <- list(v = I(x), d = country_brief_join(x), n = length(x), src = I(unique(as.character(src))), m = m)
  if (length(keys)) out$k <- I(as.character(keys))
  out
}
country_brief_word <- function(code, words, src, m) {
  code <- country_brief_chr(code)
  if (is.na(code)) return(NULL)
  w <- words[[code]] %||% gsub("_", " ", code)
  country_brief_fv(code, w, src, m)
}

country_brief_cap1 <- function(x) if (is.na(x) || !nzchar(x)) x else paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# ---- Decoding the bundles -----------------------------------------------------------------------------

# The posture core's cell columns as a data frame (one row per economy x technology x stage), with the
# dashboard's choice (grammar.ts verbOf) and yard roll-up words.
country_brief_core_cells <- function(core) {
  cl <- lapply(core$cells, function(x) if (is.list(x)) { x[vapply(x, is.null, TRUE)] <- NA; unlist(x) } else x)
  n <- length(cl$c)
  get <- function(k) { x <- cl[[k]]; if (is.null(x)) rep(NA, n) else x }
  iso <- unlist(core$countries$iso3)
  techs <- unlist(core$meta$techs)
  stages <- unlist(core$meta$stages)
  codes <- unlist(core$meta$yard_codes)
  yard_of <- stats::setNames(names(codes), codes)
  df <- data.frame(iso3 = iso[get("c") + 1L], tech = techs[get("t") + 1L], stage = stages[get("s") + 1L],
                   p = get("p"), m = get("m"), dm = get("dm"), variant = get("v"), sec = get("sec"),
                   need = as.numeric(get("need")), prize = as.numeric(get("prize")), io = as.numeric(get("io")),
                   cp = as.numeric(get("cp")), share = as.numeric(get("share")), pw = get("pw"), ev = get("ev"),
                   conf = get("conf"), al = get("al"), alc = get("alc"), alf = get("alf"), p1 = get("p1"), gap = get("gap"),
                   yd = get("yd"), bd = get("bd"), fl = get("fl"), stringsAsFactors = FALSE)
  df$choice <- ifelse(df$p == "S", "secure", ifelse(df$p == "C", df$m, ifelse(df$p == "D", df$dm, "not_assessed")))
  df$choice[is.na(df$choice)] <- "not_assessed"
  df$yard_rollup <- unname(yard_of[df$yd])
  df
}

# Layer labels of the posture core (meta.yf_nodes "Tech|Stage|layer" -> label).
country_brief_layer_labels <- function(core) {
  yf <- core$meta$yf_nodes
  lab <- as.list(yf$label)
  lab[vapply(lab, is.null, TRUE)] <- NA
  stats::setNames(as.character(unlist(lab)), as.character(unlist(yf$key)))
}

country_brief_split <- function(x) {
  x <- country_brief_chr(x)
  if (is.na(x) || !nzchar(x)) character() else strsplit(x, ";", fixed = TRUE)[[1]]
}

# The fence a choice sits behind (grammar.ts nodeFence): Secure high; Preserve, Catch Up, Partner and
# Accelerate medium; Watch low; none when not assessed.
country_brief_fence <- function(choice) {
  y <- COUNTRY_BRIEF_YARD[choice]
  unname(ifelse(is.na(y), NA_character_, COUNTRY_BRIEF_FENCE_OF_YARD[y]))
}

# One economy's 66 steps as a tidy table joined with its posture detail, and its layers. Returns
# list(cells = data.frame, detail = list keyed "Tech|Stage", layers = data.frame).
country_brief_tables <- function(core, detail, iso3, core_cells = country_brief_core_cells(core)) {
  cc <- core_cells[core_cells$iso3 == iso3, , drop = FALSE]
  det <- stats::setNames(detail$cells, vapply(detail$cells, function(c) paste(c$tech, c$stage, sep = "|"), ""))
  phrases <- unlist(core$meta$phrases)
  sd <- unlist(core$meta$stage_display)
  techs <- unlist(core$meta$techs)
  stages <- unlist(core$meta$stages)
  cc$node <- paste(cc$tech, cc$stage, sep = "|")
  cc <- cc[order(match(cc$tech, techs), match(cc$stage, stages)), , drop = FALSE]
  cc$key <- country_cell_key(cc$tech, cc$stage)
  cc$name <- unname(phrases[cc$node])
  cc$stage_display <- unname(sd[cc$stage])
  cc$fence <- country_brief_fence(cc$choice)
  hs <- vapply(cc$node, function(k) country_brief_num(det[[k]][["home_share"]]), 0)
  cc$share <- ifelse(is.na(hs), cc$share, hs)
  cc$contender <- !is.na(cc$share) & cc$share >= COUNTRY_BRIEF_CONTENDER_SHARE
  cc$siv <- ifelse(cc$stage == "Deployment", NA_real_, cc$prize)
  cc$dn <- ifelse(cc$stage == "Deployment", cc$need, NA_real_)
  cc$sc_need <- ifelse(cc$stage == "Deployment", NA_real_, cc$need)
  cc$mixed <- !is.na(cc$yard_rollup) & startsWith(cc$yard_rollup, "mixed_")
  labels <- country_brief_layer_labels(core)
  lay <- list()
  for (k in cc$node) {
    d <- det[[k]]
    own <- cc$choice[cc$node == k]
    lay[[length(lay) + 1]] <- data.frame(node = k, layer_id = "rest", label = "Rest of the step", choice = own,
                                         yard = unname(COUNTRY_BRIEF_YARD[own]), variant = cc$variant[cc$node == k], stringsAsFactors = FALSE)
    for (ch in d[["yard_children"]] %||% list()) {
      lab <- unname(labels[paste(k, ch$layer_id, sep = "|")])
      if (is.na(lab)) lab <- ch$layer_id
      lay[[length(lay) + 1]] <- data.frame(node = k, layer_id = ch$layer_id, label = lab,
                                           choice = ch$choice %||% "not_assessed", yard = ch$yard %||% NA_character_, variant = ch$variant %||% NA_character_,
                                           stringsAsFactors = FALSE)
    }
  }
  layers <- do.call(rbind, lay)
  layers$parent_choice <- cc$choice[match(layers$node, cc$node)]
  layers$fence <- unname(COUNTRY_BRIEF_FENCE_OF_YARD[layers$yard])
  list(cells = cc, detail = det, layers = layers)
}

# ---- The High fence: Secure steps and layers, gathered into capabilities -----------------------------------

# Secure units (steps and layers) with their need, rationale and parent step.
country_brief_secure_units <- function(tb) {
  cc <- tb$cells
  u <- list()
  for (i in which(cc$choice == "secure")) {
    d <- tb$detail[[cc$node[i]]]
    u[[length(u) + 1]] <- data.frame(unit = cc$node[i], kind = "step", node = cc$node[i], label = cc$name[i], need = cc$need[i],
                                     rationale = d[["security_rationale"]] %||% cc$sec[i], variant = cc$variant[i], parent_choice = "secure",
                                     stringsAsFactors = FALSE)
  }
  ly <- tb$layers[tb$layers$layer_id != "rest" & tb$layers$choice == "secure" & tb$layers$parent_choice != "secure", , drop = FALSE]
  for (j in seq_len(nrow(ly))) {
    i <- match(ly$node[j], cc$node)
    u[[length(u) + 1]] <- data.frame(unit = ly$layer_id[j], kind = "layer", node = ly$node[j], label = ly$label[j], need = cc$need[i],
                                     rationale = unname(COUNTRY_BRIEF_VARIANT_ROUTE[ly$variant[j]]) %||% NA_character_, variant = ly$variant[j],
                                     parent_choice = cc$choice[i], stringsAsFactors = FALSE)
  }
  if (!length(u)) return(data.frame(unit = character(), kind = character(), node = character(), label = character(), need = numeric(),
                                    rationale = character(), variant = character(), parent_choice = character(), stringsAsFactors = FALSE))
  do.call(rbind, u)
}

# The capabilities of the High-fence table: config groups over the Secure units, ranked by their highest
# need, then by size. A unit in no group stands alone.
country_brief_high_fence <- function(units, groups) {
  if (!nrow(units)) return(list())
  gid <- rep(NA_character_, nrow(units))
  for (g in groups) gid[is.na(gid) & units$unit %in% unlist(g$members)] <- g$id
  alone <- which(is.na(gid))
  gid[alone] <- paste0("unit_", country_slug(units$unit[alone]))
  out <- lapply(unique(gid), function(id) {
    u <- units[gid == id, , drop = FALSE]
    g <- Filter(function(x) identical(x$id, id), groups)
    name <- if (length(g)) g[[1]]$name else u$label[1]
    list(id = id, name = name, units = u, need = max(u$need, na.rm = TRUE), n = nrow(u))
  })
  ord <- order(-vapply(out, function(g) g$need, 0), -vapply(out, function(g) g$n, 0))
  out[ord]
}

# ---- Facts --------------------------------------------------------------------------------------------

# The facts of one step (cells.<key>).
country_brief_cell_facts <- function(row, d, tb, cfg, name_of, th = list(need = 0.67, prize = 0.6, cp = 0.75, deployment = 0.6)) {
  bar <- function(k) country_brief_round(100 * (th[[k]] %||% NA_real_))
  w <- cfg$words
  S <- "neis_strategy"
  cell <- row$name
  f <- list(
    name = country_brief_fv(cell, cell, S, "the supply-chain step's name"),
    tech = country_brief_fv(row$tech, row$tech, S, "technology"),
    stage = country_brief_fv(row$stage, row$stage_display, S, "supply-chain stage"),
    stage_id = country_brief_fv(row$stage, row$stage, S, "supply-chain stage (short name)"),
    choice = country_brief_word(row$choice, w$choice, S, sprintf("the strategic choice for %s", cell)),
    fence = if (!is.na(row$fence)) country_brief_word(row$fence, w$fence, S, sprintf("the fence of %s's own choice", cell)) else NULL,
    yard = if (isTRUE(row$mixed)) country_brief_word(row$yard_rollup, w$mixed, S, sprintf("%s holds parts behind more than one fence", cell)) else NULL
  )
  if (row$stage == "Deployment") {
    f$dn <- country_brief_score(row$need, c(S, "ember", "iea_weo"), sprintf("deployment need of %s (0-100; Accelerate at %s or above)", cell, bar("deployment")))
    for (fam in c("dn_demand", "dn_policy_gap", "dn_system")) {
      x <- d[[fam]]
      if (!is.null(x[["value"]])) f[[fam]] <- country_brief_score(x[["value"]], c(S, "ember", "iea_weo"), sprintf("%s family of the deployment need of %s (0-100)", sub("^dn_", "", fam), cell))
    }
    if (!is.null(d[["dn_families_observed"]])) f$dn_families <- country_brief_count(d[["dn_families_observed"]], S, sprintf("deployment-need families observed for %s (of 3)", cell))
    if (!is.null(d[["dn_confidence"]])) f$dn_confidence <- country_brief_word(d[["dn_confidence"]], list(high = "high", medium = "medium", low = "low"), S, sprintf("confidence of the deployment need of %s", cell))
    if (!is.null(d[["deployment_trend"]])) {
      trend_src <- switch(row$tech, Oil = "ei", `Electric Vehicles` = "iea_ev", "ember")
      tr <- country_brief_trend_text(d[["deployment_trend"]])
      f$deployment_trend <- country_brief_fv(d[["deployment_trend"]], sub(" [(]([0-9]{4})-([0-9]{4})[)]$", "", tr), trend_src, sprintf("observed deployment of %s", cell))
      yrs <- regmatches(tr, regexec("[(]([0-9]{4})-([0-9]{4})[)]$", tr))[[1]]
      if (length(yrs) == 3) f$deployment_years <- country_brief_fv(paste(yrs[2], yrs[3], sep = "-"), sprintf("%s and %s", yrs[2], yrs[3]), trend_src, sprintf("the first and last years of the observed deployment of %s", cell))
    }
    gap <- country_brief_targets(d[["dn_policy_gap"]][["note"]])
    if (length(gap)) f$target <- gap
  } else {
    f$need <- country_brief_score(row$need, c(S, "comtrade_matrix"), sprintf("supply-chain need of %s (0-100; high at %s or above)", cell, bar("need")))
    f$siv <- country_brief_score(row$prize, c(S, "iea_weo", "cim", "yf_attributes"), sprintf("Strategic Industrial Value of %s (0-100; high at %s or above)", cell, bar("prize")))
    f$market <- country_brief_score(row$io, c(S, "iea_weo", "cim"), sprintf("market prize of %s (0-100)", cell))
    if (row$stage != "Recovery") f$cp <- country_brief_score(row$cp, c(S, "comtrade_matrix"), sprintf("Competitive Position of %s among contenders (0-100; top quarter at %s)", cell, bar("cp")))
    f$share <- country_brief_pct(row$share, c(S, "comtrade_matrix"), sprintf("the economy's share of world supply of %s", cell))
    f$pathway <- country_brief_word(row$pw, list(yes = "credible", no = "not credible", unknown = "unknown"), S, sprintf("pathway to scale of %s", cell))
    basis <- country_brief_split(d[["pathway_basis"]])
    anch <- basis[basis %in% names(w$anchor)]
    sup <- basis[basis %in% names(w$support)]
    if (length(anch)) f$anchors <- country_brief_list(unname(unlist(w$anchor[anch])), S, sprintf("capability anchors of %s", cell))
    if (length(sup)) f$supports <- country_brief_list(unname(unlist(w$support[sup])), S, sprintf("corroborating evidence of %s's pathway", cell))
    f$import_dependence <- country_brief_score(d[["import_dependence"]], c(S, "comtrade_matrix"), sprintf("import dependence of %s (0-100, percentile among importers of the step: net imports against GDP)", cell))
    f$concentration <- country_brief_score(d[["import_hhi"]], c(S, "comtrade_matrix"), sprintf("supplier concentration of the economy's imports of %s (0-100 percentile)", cell))
    f$china_share <- country_brief_pct(d[["china_share"]], "comtrade_matrix", sprintf("China's share of the economy's imports of %s", cell))
    f$trusted_share <- country_brief_pct(d[["trusted_share"]], "comtrade_matrix", sprintf("share of world supply of %s held by suppliers the economy trusts", cell))
    if (!is.null(d[["coercive_top"]])) {
      f$coercive_top <- country_brief_fv(d[["coercive_top"]], name_of(d[["coercive_top"]]), "comtrade_matrix", sprintf("largest potentially coercive supplier of %s", cell))
      f$coercive_top_share <- country_brief_pct(d[["coercive_top_share"]], "comtrade_matrix", sprintf("world supply share of the largest potentially coercive supplier of %s", cell))
    }
    f$coercive_import_share <- country_brief_pct(d[["coercive_import_share"]], "comtrade_matrix", sprintf("share of the economy's imports of %s from potentially coercive suppliers", cell))
    if (!is.null(d[["top_supplier"]])) {
      f$top_supplier <- country_brief_fv(d[["top_supplier"]], name_of(d[["top_supplier"]]), "comtrade_matrix", sprintf("largest supplier of the economy's imports of %s", cell))
      f$top_supplier_share <- country_brief_pct(d[["top_supplier_share"]], "comtrade_matrix", sprintf("largest supplier's share of the economy's imports of %s", cell))
    }
    f$substitutability <- country_brief_word(d[["substitutability"]], w$substitutability, "yf_attributes", sprintf("how easily %s can be substituted", cell))
    f$reconstitution <- country_brief_word(d[["reconstitution_time"]], w$reconstitution, "yf_attributes", sprintf("time to rebuild supply of %s", cell))
    if (!is.null(d[["trade"]][["benchmark_share"]])) f$export_share <- country_brief_pct(d[["trade"]][["benchmark_share"]], "comtrade_matrix", sprintf("share of world exports of %s, 2022-2024", cell))
    if (!is.null(d[["trade"]][["benchmark_rca"]])) f$rca <- country_brief_fv(d[["trade"]][["benchmark_rca"]], format(round(d[["trade"]][["benchmark_rca"]], 1), nsmall = 1), "comtrade_matrix", sprintf("revealed comparative advantage in %s, 2022-2024", cell))
  }
  if (!is.null(d[["security_rationale"]])) f$rationale <- country_brief_word(d[["security_rationale"]], w$rationale, c(S, "yf_attributes"), sprintf("why %s is Secure", cell))
  if (!is.null(d[["delivery"]])) f$delivery <- country_brief_word(d[["delivery"]], w$delivery, S, sprintf("how the choice for %s is delivered", cell))
  if (!is.na(row$ev)) f$evidence <- country_brief_word(row$ev, w$evidence, S, sprintf("evidence quality of the Competitive Position of %s", cell))
  if (!is.na(row$conf)) f$confidence <- country_brief_word(row$conf, list(high = "high", moderate = "moderate", low = "low"), S, sprintf("confidence in the choice for %s", cell))
  pol <- d[["policy"]]
  if (!is.null(pol[["alignment"]])) {
    f$alignment <- country_brief_word(pol[["alignment"]], w$alignment, c(S, "gta_nipo", "iea_pams"), sprintf("policy alignment of %s", cell))
    if (!is.null(pol[["protection"]]) && isTRUE(pol[["protection_recorded"]] %||% TRUE)) f$protection <- country_brief_score(pol[["protection"]], c(S, "gta_nipo", "iea_pams"), sprintf("observed protection of %s (0-100, within the country)", cell))
    if (!is.null(pol[["priority"]])) f$policy_priority <- country_brief_score(pol[["priority"]], c(S, "gta_nipo", "iea_pams"), sprintf("observed policy priority of %s (0-100, within the country)", cell))
  }
  ps <- vapply(d[["partners"]] %||% list(), function(p) as.character(p$iso3), "")
  if (length(ps)) {
    f$partners <- country_brief_list(name_of(ps), c(S, "neis_aib", "comtrade_matrix"), sprintf("trusted partners recommended for %s, best placed first", cell))
    f$partner_1 <- country_brief_fv(ps[1], name_of(ps[1]), c(S, "neis_aib"), sprintf("best-placed trusted partner for %s", cell))
    f$aib_status <- country_brief_word(d[["partner_aib_status"]], w$aib_status, c(S, "neis_aib"), sprintf("whether allied supply covers %s", cell))
  }
  if (isTRUE(d[["partner_gap"]])) f$partner_gap <- country_brief_fv(TRUE, "no credible allied solution yet", c(S, "neis_aib"), sprintf("no credible allied solution for %s", cell))
  ly <- tb$layers[tb$layers$node == row$node & tb$layers$layer_id != "rest", , drop = FALSE]
  if (nrow(ly)) {
    f$layers <- stats::setNames(lapply(seq_len(nrow(ly)), function(j) list(
      name = country_brief_fv(ly$label[j], ly$label[j], c(S, "yf_attributes"), "the layer's name"),
      choice = country_brief_word(ly$choice[j], w$choice, S, sprintf("the strategic choice for the layer %s of %s", ly$label[j], cell)),
      fence = if (!is.na(ly$fence[j])) country_brief_word(ly$fence[j], w$fence, S, sprintf("the fence of the layer %s", ly$label[j])) else NULL,
      variant = if (!is.na(ly$variant[j]) && ly$variant[j] %in% names(w$variant)) country_brief_word(ly$variant[j], w$variant, c(S, "yf_attributes"), sprintf("why the layer %s is Secure", ly$label[j])) else NULL
    )), ly$layer_id)
  }
  Filter(Negate(is.null), f)
}

# The national target of a deployment-need note ("renewables_target_shortfall: Renewables target 143.4 GW
# by 2030 vs 66.0 GW installed (2025), recent build ... shortfall 49.8 GW"): target, its year, installed
# capacity and its year, and the shortfall at the recent build rate, or an empty list for any other note.
country_brief_targets <- function(note) {
  note <- country_brief_chr(note)
  if (is.na(note) || !grepl("renewables_target_shortfall:", note, fixed = TRUE)) return(list())
  num <- function(re) { m <- regmatches(note, regexec(re, note, perl = TRUE))[[1]]; if (length(m) < 2) NA_real_ else as.numeric(m[2]) }
  tgt <- num("[Rr]enewables target ([0-9.]+) GW by [0-9]{4}")
  yr <- num("[Rr]enewables target [0-9.]+ GW by ([0-9]{4})")
  inst <- num("vs ([0-9.]+) GW installed")
  inst_yr <- num("GW installed \\(([0-9]{4})\\)")
  short <- num("shortfall ([0-9.]+) GW")
  if (is.na(tgt)) return(list())
  gw <- function(x, m) if (is.na(x)) NULL else country_brief_fv(x, sprintf("%s GW", format(country_brief_round(x), big.mark = ",")), "ember", m)
  Filter(Negate(is.null), list(
    target_gw = gw(tgt, "national renewables target, GW"),
    target_year = if (is.na(yr)) NULL else country_brief_fv(yr, as.character(yr), "ember", "the year of the national renewables target"),
    installed_gw = gw(inst, sprintf("renewable capacity installed%s, GW", if (is.na(inst_yr)) "" else sprintf(" in %d", inst_yr))),
    installed_year = if (is.na(inst_yr)) NULL else country_brief_fv(inst_yr, as.character(inst_yr), "ember", "the year of the installed capacity"),
    shortfall_gw = gw(short, sprintf("projected shortfall against the target at the recent build rate%s, GW", if (is.na(yr)) "" else sprintf(" by %d", yr)))))
}

# A deployment trend in words: "19.8 -> 41.1 GW (2020-2025)" -> "19.8 to 41.1 GW (2020-2025)"; a label
# before the arrowed numbers is dropped ("oil use 1.88 -> 2.24 EJ" -> "1.88 to 2.24 EJ"); a mean keeps its
# word ("mean 116000 EV car sales a year" -> "an average of 116,000 EV car sales a year").
country_brief_trend_text <- function(x) {
  x <- country_brief_chr(x)
  if (is.na(x)) return(x)
  if (grepl(" -> ", x, fixed = TRUE)) x <- sub("^[A-Za-z][A-Za-z ]*? (?=[0-9.]+ -> )", "", x, perl = TRUE)
  x <- gsub(" -> ", " to ", x, fixed = TRUE)
  x <- sub("^mean ", "an average of ", x)
  m <- gregexpr("(?<![0-9.,])[0-9]{5,}(?![0-9.])", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) format(as.numeric(v), big.mark = ",", scientific = FALSE, trim = TRUE))
  x
}

# A named list of names in running text for ISO3 codes (country_prose_name: "the United States").
country_brief_name_fn <- function(core) {
  nm <- stats::setNames(as.character(unlist(core$countries$name)), as.character(unlist(core$countries$iso3)))
  function(iso) {
    out <- unname(nm[iso])
    out[is.na(out)] <- iso[is.na(out)]
    vapply(out, function(x) country_prose_name(x), "", USE.NAMES = FALSE)
  }
}

# Lists of steps (and differing layers) by choice, in the dashboard's fence-section order.
country_brief_lists <- function(tb, cfg) {
  cc <- tb$cells
  S <- "neis_strategy"
  ly <- tb$layers[tb$layers$layer_id != "rest" & tb$layers$choice != tb$layers$parent_choice, , drop = FALSE]
  by_siv <- function(ch) { x <- cc[cc$choice == ch, , drop = FALSE]; x[order(-x$prize, -x$cp), , drop = FALSE] }
  nm <- function(x) x$name
  L <- function(x, src, m) country_brief_list(nm(x), src, m, x$key)
  pres <- by_siv("preserve"); cu <- by_siv("catch_up"); pa <- by_siv("partner")
  acc <- cc[cc$choice == "accelerate", , drop = FALSE]; acc <- acc[order(-acc$need), , drop = FALSE]
  wsup <- cc[cc$choice == "watch" & cc$stage != "Deployment", , drop = FALSE]; wsup <- wsup[order(-wsup$prize), , drop = FALSE]
  wdep <- cc[cc$choice == "watch" & cc$stage == "Deployment", , drop = FALSE]; wdep <- wdep[order(-wdep$need), , drop = FALSE]
  na <- cc[cc$choice == "not_assessed", , drop = FALSE]
  al <- function(code) { x <- cc[!is.na(cc$al) & cc$al == code, , drop = FALSE]; x[order(-x$prize), , drop = FALSE] }
  lay_of <- function(ch) { x <- ly[ly$choice == ch, , drop = FALSE]; paste0(tolower(substr(x$label, 1, 1)), substr(x$label, 2, nchar(x$label))) }
  list(
    preserve = L(pres, S, "Preserve steps, by Strategic Industrial Value"),
    catch_up = L(cu, S, "Catch Up steps, by Strategic Industrial Value"),
    catch_up_layers = country_brief_list(lay_of("catch_up"), S, "Catch Up layers inside steps with another choice"),
    partner = L(pa, S, "Partner steps, by Strategic Industrial Value"),
    partner_layers = country_brief_list(lay_of("partner"), S, "Partner layers inside steps with another choice"),
    partner_gap = L(pa[vapply(pa$node, function(k) identical(tb$detail[[k]]$partner_aib_status, "gap"), TRUE), , drop = FALSE], c(S, "neis_aib"),
                    "Partner steps where allied supply falls short"),
    accelerate = L(acc, S, "Accelerate steps, by deployment need"),
    watch_top = L(utils::head(wsup, 6), S, "the most valuable producing steps left open (Watch), by Strategic Industrial Value"),
    watch_deploy = L(wdep, S, "deployment steps below the Accelerate bar, by deployment need"),
    not_assessed = country_brief_list(nm(na), S, "steps not assessed"),
    misaligned = L(al("misaligned"), c(S, "gta_nipo", "iea_pams"), "steps where policy uses the wrong tool (protection of a Partner step)"),
    over_prioritized = L(al("over_prioritized"), c(S, "gta_nipo", "iea_pams"), "steps with too much intervention (protection of a Watch step)"),
    under_prioritized = L(al("under_prioritized"), c(S, "gta_nipo", "iea_pams"), "steps that need more policy attention"),
    deployment_gap = L(al("deployment_gap"), c(S, "gta_nipo", "iea_pams"), "Accelerate steps where deployment policy lags"),
    aligned = L(al("aligned"), c(S, "gta_nipo", "iea_pams"), "steps where policy is broadly aligned with the choice")
  )
}

# The facts of the country's place in the Allied Industrial Base (structural roles from the strategic layer;
# workstreams and investment from the Decoupling and Managed Interdependence runs; the division of labor
# and partner recommendations from the posture core).
country_brief_aib_facts <- function(aib, managed, core, core_cells, iso3, name_of, tb, words = list()) {
  A <- "neis_aib"
  st <- aib$strategic
  me <- Filter(function(x) identical(x$iso3, iso3), st$countries)
  me <- if (length(me)) me[[1]] else NULL
  node_label <- stats::setNames(vapply(aib$nodes, function(n) as.character(n$label), ""), vapply(aib$nodes, function(n) as.character(n$id), ""))
  for (k in names(words$aib_node)) node_label[[k]] <- words$aib_node[[k]]
  out <- list()
  if (!is.null(me)) {
    out$role <- country_brief_fv(me$slots$headline, me$slots$headline, A, "strategic role in the Allied Industrial Base")
    if (!is.null(me$slots$firstOn)) out$first_on <- country_brief_fv(me$slots$firstOn, me$slots$firstOn, A, "the measure on which the economy ranks first among allies")
    con <- me$items$contributes %||% list()
    out$contributes <- stats::setNames(lapply(con, function(x) list(
      name = country_brief_fv(x[[1]], unname(node_label[x[[1]]]) %||% x[[1]], A, "a supply-chain step the economy contributes to allies"),
      share = country_brief_pct(x[[2]], c(A, "comtrade_matrix"), sprintf("share of world exports or production of %s", unname(node_label[x[[1]]]) %||% x[[1]])))),
      vapply(con, function(x) as.character(x[[1]]), ""))
    out$partners <- country_brief_list(name_of(unlist(me$items$partners)), A, "allied economies with the most supply restored to and from the economy by new and expanded trade")
    m <- me$metrics
    out$ws_decoupling <- country_brief_count(m$ws_member %||% 0, A, "Sectoral Compacts (Decoupling) in whose minimum coalition the economy sits")
    out$ws_demand_anchor <- country_brief_count(m$ws_demand_anchor %||% 0, A, "Sectoral Compacts (Decoupling) where the economy is a demand anchor")
  }
  sc <- st$compacts
  roles <- Filter(function(r) identical(r$iso3, iso3), st$compactRoles)
  out$n_compacts <- country_brief_count(length(sc), A, "strategic compacts of the Allied Industrial Base")
  out$all_compacts <- stats::setNames(lapply(sc, function(s) country_brief_fv(s$id, s$name, A, "a strategic compact of the Allied Industrial Base")),
                                      vapply(sc, function(s) s$id, ""))
  out$scenarios <- stats::setNames(lapply(st$scenarios %||% list(), function(x) list(label = country_brief_fv(x$id, x$label, A, sprintf("the name of the %s scenario", x$id)))),
                                   vapply(st$scenarios %||% list(), function(x) as.character(x$id), ""))
  out$n_compact_roles <- country_brief_count(length(roles), A, "strategic compacts in which the economy holds a role")
  core_in <- Filter(function(s) iso3 %in% vapply(s$core %||% list(), function(x) as.character(x$iso3), ""), sc)
  out$core_in <- country_brief_list(vapply(core_in, function(s) s$name, ""), A, "strategic compacts in which the economy is a core country")
  sc_name <- stats::setNames(vapply(sc, function(s) s$name, ""), vapply(sc, function(s) s$id, ""))
  out$compacts <- stats::setNames(lapply(roles, function(r) {
    s <- Filter(function(x) identical(x$id, r$compact), sc)[[1]]
    core_iso <- vapply(s$core %||% list(), function(x) as.character(x$iso3), "")
    core_role <- vapply(s$core %||% list(), function(x) as.character(x$role %||% ""), "")
    core_names <- name_of(core_iso)
    upstream <- core_role %in% c("Resource supplier", "Processing anchor")
    industry <- core_role %in% c("Processing anchor", "Specialized technology supplier", "Scale manufacturer")
    makers <- core_role %in% c("Specialized technology supplier", "Scale manufacturer")
    Filter(Negate(is.null), list(
      name = country_brief_fv(r$compact, sc_name[[r$compact]], A, "strategic compact"),
      role = country_brief_fv(r$role, tolower(r$role), A, sprintf("the economy's lead role in %s", sc_name[[r$compact]])),
      functions = country_brief_list(tolower(unlist(r$functions)), A, sprintf("the economy's functions in %s", sc_name[[r$compact]])),
      lead_step = if (!is.null(r$leadStep)) country_brief_fv(r$leadStep, tolower(r$leadStep), A, sprintf("the step the economy leads in %s", sc_name[[r$compact]])) else NULL,
      lead_share = if (!is.null(r$leadShare)) country_brief_pct(r$leadShare, c(A, "comtrade_matrix"), sprintf("%s behind the lead role in %s", r$leadBasis %||% "share of world exports or production", sc_name[[r$compact]])) else NULL,
      core = country_brief_list(core_names, A, sprintf("core countries of %s", sc_name[[r$compact]])),
      core_others = country_brief_list(name_of(setdiff(core_iso, iso3)), A, sprintf("the other core countries of %s", sc_name[[r$compact]])),
      core_upstream = country_brief_list(name_of(setdiff(core_iso[upstream], iso3)), A, sprintf("the other core countries of %s that hold its raw-material or processing seats", sc_name[[r$compact]])),
      core_industry = country_brief_list(name_of(setdiff(core_iso[industry], iso3)), A, sprintf("the other core countries of %s that process or manufacture in it", sc_name[[r$compact]])),
      core_makers = country_brief_list(name_of(setdiff(core_iso[makers], iso3)), A, sprintf("the other core countries of %s that manufacture its components and systems", sc_name[[r$compact]]))))
  }), vapply(roles, function(r) r$compact, ""))
  out$compact_names <- country_brief_list(vapply(roles, function(r) sc_name[[r$compact]], ""), A, "strategic compacts in which the economy holds a role")
  # Managed Interdependence (the default scenario): Sectoral Compact seats
  mi <- Filter(function(c) iso3 %in% unlist(c$coalition$members %||% list()), managed$compacts %||% list())
  out$ws_managed <- country_brief_count(length(mi), A, "Sectoral Compacts (Managed Interdependence) in whose minimum coalition the economy sits")
  # what the economy loses if China's supply stops (Decoupling)
  net <- Filter(function(x) identical(x$iso3, iso3), aib$network$countries %||% list())
  if (length(net) && !is.null(net[[1]]$lostUsd)) out$lost_usd <- country_brief_usd(net[[1]]$lostUsd, c(A, "comtrade_matrix"), "the economy's 2024 imports from China (with Hong Kong and Macao) of all goods in the Allied Industrial Base basket, minerals to finished equipment")
  # new capacity that would serve the economy's lost supply (Decoupling, rank-1 hosts)
  inv <- function(flows, what) {
    pr <- Filter(function(p) identical(p$investor, iso3) && identical(as.integer(p$rank), 1L), flows$pairs %||% list())
    if (!length(pr)) return(NULL)
    hosts <- vapply(pr, function(p) as.character(p$host), "")
    usd <- vapply(pr, function(p) as.numeric(p$usd), 0)
    by <- sort(tapply(usd, hosts, sum), decreasing = TRUE)
    list(total = country_brief_usd(sum(usd), A, sprintf("supply a year that new capacity at the best-placed hosts would restore to the economy (%s)", what)),
         hosts = country_brief_list(name_of(names(by)), A, sprintf("hosts of the new capacity that would serve the economy (%s), largest first", what)),
         top_host = country_brief_fv(names(by)[1], name_of(names(by)[1]), A, sprintf("the host of most of that new capacity (%s)", what)),
         top_usd = country_brief_usd(by[[1]], A, sprintf("supply a year from new capacity at the leading host (%s)", what)))
  }
  out$investment_decoupling <- inv(aib$investmentFlows, "Decoupling")
  out$investment_managed <- inv(managed$investmentFlows, "Managed Interdependence")
  # the allied division of labor where the economy holds the largest allied supply (posture core division)
  dv <- country_brief_division(core, iso3, tb)
  if (nrow(dv)) {
    out$division <- stats::setNames(lapply(seq_len(nrow(dv)), function(i) Filter(Negate(is.null), list(
      name = country_brief_fv(dv$node[i], dv$name[i], A, "a step where the economy holds the largest allied supply"),
      share = country_brief_pct(dv$share[i], c(A, "comtrade_matrix"), sprintf("the economy's share of world supply of %s (%s), the largest of any ally", dv$name[i],
                                                                         if (identical(dv$source[i], "aib_position_share")) "world exports or production, the Allied Industrial Base measure" else "world supply")),
      allied_part = if (!is.na(dv$allied_share[i]) && dv$allied_share[i] > 0 && dv$allied_share[i] <= 1) country_brief_pct(dv$share[i] / dv$allied_share[i], c(A, "comtrade_matrix"),
                                                                         sprintf("the economy's part of the supply of %s held by allied economies", dv$name[i])) else NULL,
      backups = country_brief_list(name_of(c(dv$backup_1[i], dv$backup_2[i])), A, sprintf("the next allied holders of %s", dv$name[i])),
      single_point = if (identical(dv$gap[i], "single_point_of_failure")) country_brief_fv(TRUE, "a single point of failure", A, sprintf("allied supply of %s rests on one holder", dv$name[i])) else NULL))),
      dv$key)
    out$division_names <- country_brief_list(dv$name, A, "steps where the economy holds the largest allied supply")
  }
  # how many economies' recommended first partner the economy is, by step
  rec <- core_cells[!is.na(core_cells$p1) & core_cells$p1 == iso3 & core_cells$iso3 != iso3, , drop = FALSE]
  if (nrow(rec)) {
    tabn <- sort(table(paste(rec$tech, rec$stage, sep = "|")), decreasing = TRUE)
    keys <- names(tabn)
    out$recommended_by <- stats::setNames(lapply(seq_along(keys), function(i) list(
      name = country_brief_fv(keys[i], tb$cells$name[match(keys[i], tb$cells$node)], "neis_strategy", "a step where other economies are advised to partner with the economy"),
      n = country_brief_count(tabn[[i]], "neis_strategy", sprintf("economies whose best-placed trusted partner for %s is the economy", tb$cells$name[match(keys[i], tb$cells$node)])))),
      country_cell_key(sub("\\|.*$", "", keys), sub("^.*\\|", "", keys)))
    out$recommended_economies <- country_brief_count(length(unique(rec$iso3)), "neis_strategy", "economies for which the economy is the best-placed trusted partner for at least one step")
  }
  Filter(Negate(is.null), out)
}

# The steps where the economy is the primary allied holder (posture core `division`: one row per
# technology x stage, keyed by the meta.techs / meta.stages indexes), largest share first.
country_brief_division <- function(core, iso3, tb) {
  dv <- core$division
  if (is.null(dv) || !length(dv$t)) return(data.frame())
  v <- function(k) { x <- dv[[k]]; if (is.null(x)) rep(NA, length(dv$t)) else { x <- as.list(x); x[vapply(x, is.null, TRUE)] <- NA; unlist(x) } }
  techs <- unlist(core$meta$techs); stages <- unlist(core$meta$stages)
  df <- data.frame(tech = techs[v("t") + 1L], stage = stages[v("s") + 1L], primary = v("primary"), share = as.numeric(v("primary_share")),
                   allied_share = as.numeric(v("allied_share")), backup_1 = v("backup_1"), backup_2 = v("backup_2"), gap = v("gap"),
                   source = v("source"), stringsAsFactors = FALSE)
  df <- df[!is.na(df$primary) & df$primary == iso3, , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  df$node <- paste(df$tech, df$stage, sep = "|")
  df$key <- country_cell_key(df$tech, df$stage)
  df$name <- tb$cells$name[match(df$node, tb$cells$node)]
  df[order(-df$share), , drop = FALSE]
}

# The full facts object of one economy's brief.
country_brief_facts <- function(core, detail, aib, managed, iso3, cfg, as_of_label, editions = list(), aib_created = NULL, policy_design = NULL,
                                dcfg = NULL, dev = NULL) {
  core_cells <- country_brief_core_cells(core)
  tb <- country_brief_tables(core, detail, iso3, core_cells)
  # evidence gating (2026-11-03): the review cases, kept out of every headline
  if (!is.null(dcfg)) tb <- country_brief_review(tb, dcfg)
  name_of <- country_brief_name_fn(core)
  cc <- tb$cells
  S <- "neis_strategy"
  # the name as it reads in running text ("the United States"); titles capitalize their first letter
  country <- country_prose_name(detail$country %||% name_of(iso3))
  cc <- tb$cells
  cells <- stats::setNames(lapply(seq_len(nrow(cc)), function(i) {
    rv <- if (!is.null(cc$review) && !is.na(cc$review[i])) list(review = country_brief_word(cc$review[i], cfg$words$review, S, sprintf("why the call on %s is a review case (insufficient evidence for a headline)", cc$name[i])))
    c(country_brief_cell_facts(cc[i, ], tb$detail[[cc$node[i]]], tb, cfg, name_of, core$meta$thresholds),
      country_brief_cell_extra(cc[i, ], tb$detail[[cc$node[i]]], core$design %||% list(), cfg), rv)
  }), cc$key)
  units <- country_brief_secure_units(tb)
  hf <- country_brief_high_fence(units, cfg$high_fence_groups)
  high <- stats::setNames(lapply(seq_along(hf), function(i) {
    g <- hf[[i]]
    u <- g$units
    techs <- unique(sub("\\|.*$", "", u$node))
    parents <- unique(cc$name[match(u$node, cc$node)])
    Filter(Negate(is.null), list(
      name = country_brief_fv(g$id, g$name, c(S, "yf_attributes"), "a High-fence capability (Secure steps and layers one answer covers)"),
      rank = country_brief_count(i, S, "rank of the capability by its highest supply-chain need"),
      need = country_brief_score(g$need, c(S, "comtrade_matrix"), sprintf("the highest supply-chain need among the parts of %s (0-100)", g$name)),
      rationale = country_brief_list(unname(unlist(cfg$words$rationale[unique(stats::na.omit(u$rationale))])), c(S, "yf_attributes"), sprintf("why %s is Secure", g$name)),
      parts = country_brief_list(unique(u$label), c(S, "yf_attributes"), sprintf("the Secure steps and layers of %s", g$name)),
      steps = country_brief_list(parents, S, sprintf("the supply-chain steps that hold %s", g$name)),
      technologies = country_brief_list(techs, S, sprintf("technologies that contain %s", g$name)),
      n_parts = country_brief_count(g$n, S, sprintf("Secure steps and layers in %s", g$name)),
      # the configured instruments of its Secure variants (config/posture_policy_design.csv)
      fence_tools = country_brief_tools(policy_design, u$variant, "fence_instruments", sprintf("the configured fence instruments for %s", g$name)),
      build_tools = country_brief_tools(policy_design, u$variant, "build_instruments", sprintf("the configured build instruments for %s", g$name))))
  }), vapply(hf, function(g) g$id, ""))
  w <- cfg$words
  ch <- table(factor(cc$choice, levels = COUNTRY_BRIEF_CHOICES))
  lyr <- tb$layers[tb$layers$layer_id != "rest", , drop = FALSE]
  counts <- list(
    steps = country_brief_count(nrow(cc), S, "technology x supply-chain steps on the Strategy Map"),
    assessed = country_brief_count(sum(cc$choice != "not_assessed"), S, "steps with a strategic choice"),
    not_assessed = country_brief_count(ch[["not_assessed"]], S, "steps not assessed (the data are not strong enough to make a call)"),
    secure = country_brief_count(ch[["secure"]], S, "Secure steps", cc$key[cc$choice == "secure"]),
    preserve = country_brief_count(ch[["preserve"]], S, "Preserve steps", cc$key[cc$choice == "preserve"]),
    catch_up = country_brief_count(ch[["catch_up"]], S, "Catch Up steps", cc$key[cc$choice == "catch_up"]),
    partner = country_brief_count(ch[["partner"]], S, "Partner steps", cc$key[cc$choice == "partner"]),
    accelerate = country_brief_count(ch[["accelerate"]], S, "Accelerate steps", cc$key[cc$choice == "accelerate"]),
    watch = country_brief_count(ch[["watch"]], S, "Watch steps"),
    mixed = country_brief_count(sum(cc$mixed), S, "steps whose parts sit behind more than one fence"),
    layers = country_brief_count(nrow(lyr), S, "named layers (capabilities inside a step) with a choice of their own or not assessed"),
    layers_assessed = country_brief_count(sum(lyr$choice != "not_assessed"), S, "named layers with a choice of their own"),
    secure_layers = country_brief_count(sum(lyr$choice == "secure" & lyr$parent_choice != "secure"), S, "Secure layers inside steps with another choice"),
    high_units = country_brief_count(nrow(units), S, "Secure steps and layers"),
    high_capabilities = country_brief_count(length(hf), S, "High-fence capabilities (Secure steps and layers grouped by the answer they need)"),
    medium = country_brief_count(sum(cc$fence %in% "medium"), S, "steps whose own choice sits behind a medium fence"),
    low = country_brief_count(sum(cc$fence %in% "low"), S, "steps whose own choice sits behind a low fence"),
    ev_high = country_brief_count(sum(cc$ev %in% "high"), S, "steps whose Competitive Position rests mostly on observed, stage-specific inputs"),
    ev_partial = country_brief_count(sum(cc$ev %in% c("medium", "low")), S, "steps whose Competitive Position rests partly on observed inputs"),
    ev_none = country_brief_count(sum(cc$ev %in% "none" | is.na(cc$ev)), S, "steps with no observed position evidence"),
    review = country_brief_count(sum(!is.na(cc$review %||% rep(NA, nrow(cc)))), S, "steps whose call is a review case (insufficient evidence for a headline)")
  )
  meta <- list(
    country = country_brief_fv(iso3, country, character(), "the economy"),
    country_poss = country_brief_fv(iso3, country_poss(country), character(), "the economy (possessive)"),
    iso3 = iso3, as_of_label = country_brief_fv(as_of_label, as_of_label, S, "the date of the data release"),
    posture_created = core$created, aib_created = aib_created, git_commit = core$git_commit,
    editions = editions,
    thresholds = core$meta$thresholds,
    # the bars of the choice rules (posture core meta.thresholds) as 0-100 scores
    bars = Filter(Negate(is.null), list(
      need = country_brief_score(core$meta$thresholds$need, S, "the bar of high supply-chain need (0-100)"),
      prize = country_brief_score(core$meta$thresholds$prize, S, "the bar of high Strategic Industrial Value (0-100)"),
      cp = country_brief_score(core$meta$thresholds$cp, S, "the bar of a top-quarter Competitive Position (0-100)"),
      deployment = country_brief_score(core$meta$thresholds$deployment, S, "the bar of Accelerate on deployment need (0-100)")))
  )
  lists <- country_brief_lists(tb, cfg)
  gp <- country_brief_policy_gaps(tb, cfg)
  lc <- country_brief_low_cases(tb, core$meta$thresholds)
  dr <- country_brief_deployment_rank(tb, cfg)
  lists$policy_gaps <- country_brief_list(gp$name, c(S, "gta_nipo", "iea_pams"), "the most consequential gaps between the choice and recorded policy (low-confidence choices left out)")
  lists$low_cases <- country_brief_list(lc$name, S, "producing steps left open (Watch) with a high value or a recorded policy priority, most valuable first")
  lists$deploy_rank <- country_brief_list(dr$name, S, "deployment steps by deployment need, highest first")
  aibf <- country_brief_aib_facts(aib, managed, core, core_cells, iso3, name_of, tb, cfg$words)
  mx <- country_brief_aib_matrix(aib, managed, tb, iso3, name_of, cfg, country_brief_division(core, iso3, tb))
  aibf$matrix <- country_brief_matrix_facts(mx, cfg)
  # review cases, shared physical capabilities and the development section (2026-11-03)
  rvk <- if (is.null(cc$review)) character() else cc$key[!is.na(cc$review)]
  lists$review_cases <- if (length(rvk)) country_brief_list(cc$name[match(rvk, cc$key)], S, "steps whose call is a review case, kept out of the headlines")
  nodes <- country_devpart_nodes(aib$nodes %||% list(), aib$strategic %||% list())
  sh <- if (nrow(nodes)) country_brief_shared_capabilities(tb, nodes, names(aibf$contributes %||% list()), cfg$brief$shared_max %||% 4L) else data.frame()
  shared <- if (nrow(sh)) stats::setNames(lapply(seq_len(nrow(sh)), function(i) country_brief_shared_facts(sh[i, ], tb, aibf, cfg)), sh$node) else list()
  dvf <- country_brief_dev_facts(dev, dcfg, name_of, tb)
  out <- list(schema = COUNTRY_BRIEF_FACTS_SCHEMA, iso3 = iso3, meta = meta, counts = counts, cells = cells, high_fence = high,
              high_fence_order = I(names(high)), lists = lists, policy_gaps_order = I(gp$key), low_cases_order = I(lc$key),
              deploy_order = I(dr$key), review_order = I(rvk), shared = shared, shared_order = I(names(shared)), aib = aibf)
  if (!is.null(dvf)) out$dev <- dvf
  out
}

# The configured instruments of a set of posture variants (config/posture_policy_design.csv: ";"-joined
# lists), as a name list; NULL without the table.
country_brief_tools <- function(ppd, variants, column, m) {
  if (is.null(ppd) || !column %in% names(ppd)) return(NULL)
  x <- ppd[[column]][ppd$posture_variant %in% unique(stats::na.omit(variants))]
  tools <- unique(trimws(unlist(strsplit(x[!is.na(x)], ";", fixed = TRUE))))
  tools <- tools[nzchar(tools)]
  if (!length(tools)) return(NULL)
  country_brief_list(tolower(substr(tools, 1, 1)) |> paste0(substring(tools, 2)), c("neis_strategy", "posture_design"), m)
}

# The tidy tables a brief's charts and appendix read (no facts formatting): with the AIB bundles and the
# config, also the compact x role matrix (Figure 3) and the deployment ranking (Figure 4).
country_brief_chart_data <- function(core, detail, iso3, aib = NULL, managed = NULL, cfg = NULL, dcfg = NULL) {
  tb <- country_brief_tables(core, detail, iso3)
  if (!is.null(dcfg)) tb <- country_brief_review(tb, dcfg)
  out <- list(cells = tb$cells, layers = tb$layers, units = country_brief_secure_units(tb))
  if (!is.null(cfg)) out$deployment <- country_brief_deployment_rank(tb, cfg)
  if (!is.null(aib) && !is.null(cfg)) {
    out$matrix <- country_brief_aib_matrix(aib, managed %||% list(), tb, iso3, country_brief_name_fn(core), cfg, country_brief_division(core, iso3, tb))
  }
  out
}
