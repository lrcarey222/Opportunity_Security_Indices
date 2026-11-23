# QC: end-to-end, source-to-conclusion audit of one Country x Technology x Stage cell (docs/QC_SYSTEM_DESIGN.md
# 6.4). From the pipeline tables (strategic_position_tbl and the processed source tables that feed it) and the
# configuration, it lays out ten numbered sections:
#    1 source datasets      2 source rows          3 cleaning and exclusions   4 transformations
#    5 normalized indicators 6 weights             7 intermediate scores        8 threshold decisions
#    9 final classification 10 resulting recommendation
# plus a comparison with the shipped dashboard bundle (posture detail and core JSON) for the same cell.
#
# Every link of the chain is re-computed where the stored columns allow it and marked reproduced or differs;
# a link that cannot be re-computed from the shipped or pipeline data is marked QC_AUDIT_NOT_REPRO explicitly,
# never left out. Labels (choice, yard and fence names) come from the canonical constants the script parses out
# of dashboard/src/content/vocabulary.ts and posture/grammar.ts (qc_audit_ts_record()), never re-typed here.
#
# Pure functions: no IO (scripts/qc/build_case_audit.R reads every input and writes the JSON / Markdown). Needs
# R/strategic_position/{sp_helpers, industrial_opportunity, strategic_value, decision_engine, yard_rollup}.R and
# R/utils/supply_chain_stages.R sourced by the caller.

QC_AUDIT_SCHEMA <- "aib-qc-case-audit"
QC_AUDIT_NOT_REPRO <- "not reproducible from shipped/pipeline data"
# provenance_gap: a source the chain reads has no recorded vintage or licence (a provenance finding, not a
# reproduction failure).
QC_AUDIT_STATUSES <- c("reproduced", "differs", QC_AUDIT_NOT_REPRO, "provenance_gap", "recorded", "not_applicable")
QC_AUDIT_SECTIONS <- c(source_datasets = "Source datasets", source_rows = "Source rows", cleaning = "Cleaning and exclusions",
                       transformations = "Transformations", indicators = "Normalized indicators", weights = "Weights",
                       scores = "Intermediate scores", thresholds = "Threshold decisions",
                       classification = "Final classification", recommendation = "Resulting recommendation")
QC_AUDIT_CP_FAMILIES <- c("export_position", "production_position", "manufacturing_capacity", "investment_position", "cost_position",
                          "resource_endowment", "deployment_scale", "innovation_position")
QC_AUDIT_DN_FAMILIES <- c("dn_demand", "dn_policy_gap", "dn_system")

# Where each input family comes from: the processed table and the script that writes it (the raw inputs are the
# raw_inputs_manifest.yml entries whose required_by names one of those scripts). stages: the stages a family
# applies to. Documented from scripts/26_build_strategic_position.R:106-135, 415-460 and the family recipes in
# config/strategic_position.yml.
QC_AUDIT_FAMILY_SOURCES <- data.frame(
  family = c("need: import dependence", "need: supplier concentration and risky suppliers", "need: current-trade blend",
             "need: trusted alternatives and world supply structure", "position: CP families", "position: trade benchmark and momentum",
             "prize: market (demand growth, investment momentum, home market)", "prize: commons and option; large-yard signals",
             "deployment need", "Secure engine (Tests 1-5) and capability floor", "pathway (Catch Up anchors)"),
  processed_table = c("outputs/index_outputs.rds (energy_security_variable_contributions: deficit_gdp, Production surplus/deficit)",
                      "import_exposure_tbl.rds; import_partner_cells_tbl.rds", "current_import_exposure_tbl.rds",
                      "trade_concentration_tbl.rds; major_supplier_shares_tbl.rds; critical_minerals_production_tbl.rds",
                      "outputs/index_outputs.rds (economic_opportunity / energy_security variable contributions); outputs/innovation/innovation_theme.csv (innovation_position)",
                      "Comtrade energy trade (read inside scripts/26; no processed table)",
                      "iea_pathways_*_tbl.rds; investment_momentum_tbl.rds; home_market_tbl.rds",
                      "data/reference/posture_node_attributes.csv; data/reference/yards_fences_assessments.csv",
                      "deployment_need_tbl.rds", "outputs/strategic_position_inputs.rds (posture inputs)",
                      "GCIM pipeline and capacity side tables (read inside scripts/26; not saved)"),
  scripts = c("scripts/20_build_indices.R;scripts/10_build_themes.R", "scripts/15_build_partner_themes.R",
              "scripts/38_build_current_import_exposure.R;scripts/37_pull_comtrade_monthly_dyads.R", "scripts/10_build_themes.R",
              "scripts/20_build_indices.R;scripts/10_build_themes.R;scripts/29_build_innovation_position.R", "scripts/26_build_strategic_position.R",
              "scripts/17_build_iea_pathways.R;scripts/10_build_themes.R;scripts/95_build_deployment_need.R", "",
              "scripts/95_build_deployment_need.R", "scripts/26_build_strategic_position.R", "scripts/26_build_strategic_position.R"),
  stages = c("producing", "producing", "producing", "producing", "all", "traded", "producing", "producing", "Deployment", "all", "producing"),
  stringsAsFactors = FALSE
)

# ---- small helpers ------------------------------------------------------------------------------------------
qc_audit_fmt <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) x <- unlist(x)
  if (length(x) > 1) return(paste(vapply(x, qc_audit_fmt, character(1)), collapse = "; "))
  if (is.na(x)) return(NA_character_)
  if (is.logical(x)) return(if (x) "TRUE" else "FALSE")
  if (is.numeric(x)) {
    if (abs(x) >= 1e5) return(format(round(x), big.mark = ",", scientific = FALSE))
    return(as.character(signif(x, 4)))
  }
  as.character(x)
}
qc_audit_get <- function(row, col) if (col %in% names(row)) row[[col]][1] else NA
qc_audit_num <- function(row, col) suppressWarnings(as.numeric(qc_audit_get(row, col)))

# A key-value item table: item, value, detail (all character).
qc_audit_items <- function(values, detail = NULL) {
  if (!length(values)) return(data.frame(item = character(), value = character(), detail = character(), stringsAsFactors = FALSE))
  d <- if (is.null(detail)) rep(NA_character_, length(values)) else vapply(names(values), function(n) {
    v <- detail[[n]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1))
  data.frame(item = names(values), value = vapply(values, qc_audit_fmt, character(1)), detail = unname(d), stringsAsFactors = FALSE)
}

# One link of the chain. status one of QC_AUDIT_STATUSES.
qc_audit_link <- function(link, status, expected = NA, observed = NA, detail = NA_character_) {
  stopifnot(status %in% QC_AUDIT_STATUSES)
  data.frame(link = link, status = status, expected = qc_audit_fmt(expected), observed = qc_audit_fmt(observed),
             detail = as.character(detail), stringsAsFactors = FALSE)
}

# A recomputed value against the stored one: reproduced within tol, else differs; NA recomputed (the inputs are
# missing) -> not reproducible; both NA -> reproduced (consistently absent).
qc_audit_cmp <- function(link, expected, observed, tol = 1e-6, detail = NA_character_) {
  e <- expected
  o <- observed
  if (is.null(e) || length(e) == 0) e <- NA
  if (is.null(o) || length(o) == 0) o <- NA
  if (is.na(e) && is.na(o)) return(qc_audit_link(link, "reproduced", e, o, dplyr::coalesce(detail, "both absent")))
  if (is.na(e)) return(qc_audit_link(link, QC_AUDIT_NOT_REPRO, e, o, detail))
  same <- if (is.numeric(e) && is.numeric(o)) !is.na(o) && abs(e - o) <= tol else !is.na(o) && identical(as.character(e), as.character(o))
  qc_audit_link(link, if (same) "reproduced" else "differs", e, o, detail)
}

qc_audit_links_empty <- function() qc_audit_link("x", "recorded")[0, ]

qc_audit_section <- function(id, items = qc_audit_items(list()), links = qc_audit_links_empty(), tables = list(), notes = character()) {
  list(n = match(id, names(QC_AUDIT_SECTIONS)), id = id, title = unname(QC_AUDIT_SECTIONS[id]), items = items, links = links,
       tables = tables, notes = notes)
}

# Parse a TypeScript object-literal constant of string values (export const NAME ... = { key: 'Value', ... };)
# out of a source file's text. Returns a named character vector (empty when the constant is absent).
qc_audit_ts_record <- function(text, name) {
  text <- paste(text, collapse = "\n")
  start <- regexpr(paste0("export const ", name, "\\b[^=]*=\\s*\\{"), text, perl = TRUE)
  if (start < 0) return(stats::setNames(character(), character()))
  rest <- substring(text, start + attr(start, "match.length"))
  end <- regexpr("\\n?\\s*\\};?", rest, perl = TRUE)
  body <- substring(rest, 1, if (end > 0) end - 1 else nchar(rest))
  m <- regmatches(body, gregexpr("([A-Za-z_][A-Za-z0-9_]*)\\s*:\\s*'((?:[^'\\\\]|\\\\.)*)'", body, perl = TRUE))[[1]]
  if (!length(m)) return(stats::setNames(character(), character()))
  keys <- sub("^([A-Za-z_][A-Za-z0-9_]*)\\s*:.*$", "\\1", m, perl = TRUE)
  vals <- sub("^[A-Za-z_][A-Za-z0-9_]*\\s*:\\s*'(.*)'$", "\\1", m, perl = TRUE)
  stats::setNames(gsub("\\\\'", "'", vals), keys)
}

# The canonical labels from the vocabulary sources' text (vocabulary.ts and grammar.ts).
qc_audit_labels <- function(vocabulary_text, grammar_text = character()) {
  list(choice_name = qc_audit_ts_record(vocabulary_text, "CHOICE_NAME"),
       choice_action = qc_audit_ts_record(vocabulary_text, "CHOICE_ACTION"),
       yard_name = qc_audit_ts_record(vocabulary_text, "YARD_NAME"),
       fence_name = qc_audit_ts_record(vocabulary_text, "FENCE_NAME"),
       yard_and_fence = qc_audit_ts_record(vocabulary_text, "YARD_AND_FENCE"),
       alignment_name = qc_audit_ts_record(vocabulary_text, "ALIGNMENT_NAME"),
       posture_label = qc_audit_ts_record(grammar_text, "POSTURE_LABEL"))
}

# The display label of a canonical choice key (not_assessed: grammar.ts POSTURE_LABEL N).
qc_audit_choice_label <- function(choice, labels) {
  if (is.na(choice)) return(NA_character_)
  if (choice == "not_assessed") return(unname(labels$posture_label["N"]) %||% NA_character_)
  v <- unname(labels$choice_name[choice])
  if (length(v) && !is.na(v)) v else NA_character_
}

# Manifest entries whose required_by names one of `scripts` (";"-separated), as a table.
qc_audit_manifest_entries <- function(manifest, scripts) {
  s <- trimws(unlist(strsplit(scripts, ";", fixed = TRUE)))
  s <- s[nzchar(s)]
  ents <- manifest$inputs %||% list()
  hit <- Filter(function(e) length(intersect(basename(as.character(unlist(e$required_by %||% character()))), basename(s))) > 0, ents)
  if (!length(hit)) return(data.frame(id = character(), source_name = character(), vintage = character(), licence = character(),
                                      url = character(), stringsAsFactors = FALSE))
  f <- function(e, k) if (is.null(e[[k]])) NA_character_ else as.character(e[[k]])[1]
  data.frame(id = vapply(hit, f, character(1), "id"), source_name = vapply(hit, f, character(1), "source_name"),
             vintage = vapply(hit, f, character(1), "vintage"), licence = vapply(hit, f, character(1), "licence"),
             url = vapply(hit, f, character(1), "url"), stringsAsFactors = FALSE)
}

# ---- sections ------------------------------------------------------------------------------------------------

qc_audit_s1_sources <- function(ctx) {
  stage <- ctx$row$supply_chain
  traded <- stage %in% c("Extraction", "Refining", "Components", "Assembly")
  fs <- QC_AUDIT_FAMILY_SOURCES
  applies <- ifelse(fs$stages == "all", TRUE, ifelse(fs$stages == "Deployment", stage == "Deployment",
                    ifelse(fs$stages == "traded", traded, stage != "Deployment")))
  fs <- fs[applies, , drop = FALSE]
  raw <- lapply(seq_len(nrow(fs)), function(i) qc_audit_manifest_entries(ctx$manifest, fs$scripts[i]))
  fs$raw_inputs <- vapply(raw, function(r) if (nrow(r)) paste(r$id, collapse = ", ") else NA_character_, character(1))
  fs$n_raw_inputs <- vapply(raw, nrow, integer(1))
  allraw <- unique(do.call(rbind, raw))
  links <- do.call(rbind, lapply(seq_len(nrow(fs)), function(i) {
    if (fs$n_raw_inputs[i] == 0) {
      qc_audit_link(paste0(fs$family[i], ": raw inputs"), QC_AUDIT_NOT_REPRO, NA, NA,
                    "no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded")
    } else {
      qc_audit_link(paste0(fs$family[i], ": raw rows -> this cell"), QC_AUDIT_NOT_REPRO, NA, fs$n_raw_inputs[i],
                    "the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded")
    }
  }))
  if (!is.null(allraw) && nrow(allraw)) {
    miss <- allraw[is.na(allraw$vintage) | is.na(allraw$licence), , drop = FALSE]
    if (nrow(miss)) {
      links <- rbind(links, qc_audit_link("candidate raw inputs: vintage and licence recorded", "provenance_gap", nrow(allraw),
                                          nrow(allraw) - nrow(miss),
                                          paste0(nrow(miss), " of ", nrow(allraw), " lack a vintage or a licence in config/raw_inputs_manifest.yml: ",
                                                 paste(miss$id, collapse = ", "))))
    }
  }
  qc_audit_section("source_datasets", qc_audit_items(list(families_applicable = nrow(fs),
                                                          candidate_raw_inputs = if (is.null(allraw)) 0L else nrow(allraw)),
                                                     list(candidate_raw_inputs = paste0("raw_inputs_manifest.yml entries whose required_by names a ",
                                                                                        "script that builds a family's processed table (an upper bound: ",
                                                                                        "the manifest records inputs per script, not per cell)"))),
                   links, tables = list(families = fs, raw_inputs = if (is.null(allraw)) data.frame() else allraw))
}

# src: list(import_exposure, current_import_exposure, import_partners, production, deployment_need) of the
# country's rows for this tech x stage (any may be NULL or empty).
qc_audit_s2_rows <- function(ctx) {
  r <- ctx$row
  src <- ctx$sources %||% list()
  stage <- r$supply_chain
  links <- qc_audit_links_empty()
  tables <- list()
  items <- list()
  if (stage != "Deployment") {
    ie <- src$import_exposure
    cie <- src$current_import_exposure
    w <- qc_audit_num(r, "exposure_weight")
    basis <- qc_audit_get(r, "exposure_basis")
    items$exposure_basis <- basis
    items$exposure_weight <- w
    if (!is.null(ie) && nrow(ie)) {
      tables$import_exposure <- ie
      for (v in c("imports_usd", "import_hhi", "china_share", "opposite_bloc_share", "top_supplier_share", "imports_to_gdp")) {
        b <- suppressWarnings(as.numeric(ie[[v]][1]))
        cur <- if (!is.null(cie) && nrow(cie) && v %in% names(cie)) suppressWarnings(as.numeric(cie[[v]][1])) else NA_real_
        blend <- !is.na(w) && w > 0 && identical(basis, "blend") && !is.na(cur)
        e <- if (blend) (1 - w) * b + w * cur else b
        # strategic_need.R rule 6: a nonaligned importer's opposite-bloc share is not applicable (NA), not 0
        if (v == "opposite_bloc_share" && identical(as.character(qc_audit_get(r, "sn_opposite_bloc_basis")), "nonaligned")) {
          links <- rbind(links, qc_audit_cmp("opposite_bloc_share <- not applicable for a nonaligned importer (strategic_need.R rule 6)",
                                             NA_real_, qc_audit_num(r, v), detail = paste0("import exposure ", qc_audit_fmt(e))))
          next
        }
        links <- rbind(links, qc_audit_cmp(paste0(v, " <- import exposure", if (blend) " (base/current blend)" else " (base year)"),
                                           e, qc_audit_num(r, v), tol = max(1e-6, 1e-6 * abs(e %||% 0)),
                                           detail = if (blend) paste0("(1 - ", w, ") x base ", qc_audit_fmt(b), " + ", w, " x current ", qc_audit_fmt(cur)) else NA))
      }
      links <- rbind(links, qc_audit_cmp("top supplier <- import exposure", as.character(ie$top_supplier_iso3[1]),
                                         as.character(qc_audit_get(r, "top_supplier_iso3"))))
    } else {
      links <- rbind(links, qc_audit_link("import exposure row", QC_AUDIT_NOT_REPRO, NA, qc_audit_num(r, "imports_usd"),
                                          "no import_exposure_tbl row for this importer and cell"))
    }
    if (!is.null(cie) && nrow(cie)) tables$current_import_exposure <- cie
    ip <- src$import_partners
    if (!is.null(ip) && nrow(ip)) {
      ip <- ip[order(-ip$imports_usd), , drop = FALSE]
      # Self-trade (re-import) rows: import_partner_cells_tbl keeps the reporter as its own partner, while
      # import_exposure_tbl (and so the need) leaves them out. Reported as a link of its own; the HHI and top-supplier
      # checks below use the partner rows without them, so they test the arithmetic rather than the self rows.
      self_row <- ip$partner_iso %in% r$iso3
      self_usd <- sum(ip$imports_usd[self_row], na.rm = TRUE)
      links <- rbind(links, qc_audit_cmp(
        "partner rows: self-trade (re-import) imports, USD", 0, self_usd, tol = 0.5,
        detail = paste0("import_partner_cells_tbl.rds lists the importer as its own supplier",
                        if (self_usd > 0) paste0(" (", qc_audit_fmt(self_usd / sum(ip$imports_usd, na.rm = TRUE)), " of its partner rows); ",
                                                 "import_exposure_tbl.rds leaves these out, but sp_partner_recs() diversification and the ",
                                                 "partnership memo read the partner table as it is") else "")))
      tables$import_partners_self <- ip[self_row, , drop = FALSE]
      ip <- ip[!self_row, , drop = FALSE]
      tot <- sum(ip$imports_usd, na.rm = TRUE)
      ip$share <- ip$imports_usd / tot
      tables$import_partners_top10 <- utils::head(ip, 10)
      items$import_partners_n <- nrow(ip)
      items$import_partners_total_usd <- tot
      hhi <- sum(ip$share^2)
      if (!is.null(ie) && nrow(ie)) {
        links <- rbind(links, qc_audit_cmp("base import HHI <- partner rows without self-trade (sum of squared shares)", hhi,
                                           as.numeric(ie$import_hhi[1]), tol = 1e-3,
                                           detail = "import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year"))
        links <- rbind(links, qc_audit_cmp("base imports_usd <- partner rows without self-trade", tot, as.numeric(ie$imports_usd[1]),
                                           tol = max(1, 1e-6 * tot)))
        links <- rbind(links, qc_audit_cmp("base top supplier <- partner rows", ip$partner_iso[1], as.character(ie$top_supplier_iso3[1])))
      }
    } else {
      links <- rbind(links, qc_audit_link("import partner rows", QC_AUDIT_NOT_REPRO, NA, NA, "no import_partner_cells_tbl rows for this importer and cell"))
    }
    pr <- src$production
    if (!is.null(pr) && nrow(pr)) tables$production <- pr
    links <- rbind(links, qc_audit_link(
      "home_share (world supply share) <- production / supply structure", QC_AUDIT_NOT_REPRO, NA, qc_audit_num(r, "home_share"),
      paste0("the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; ",
             if (!is.null(pr) && nrow(pr)) paste0(nrow(pr), " production-table rows shown for context") else "no production-table rows for this cell")))
    if (stage %in% c("Extraction", "Refining", "Components", "Assembly")) {
      items$trade_benchmark_source <- qc_audit_get(r, "trade_benchmark_source")
      items$trade_benchmark_period <- qc_audit_get(r, "trade_benchmark_period")
      items$trade_benchmark_exports_usd <- qc_audit_num(r, "trade_benchmark_exports")
      items$trade_benchmark_share <- qc_audit_num(r, "trade_benchmark_share")
      items$trade_benchmark_rca <- qc_audit_num(r, "trade_benchmark_rca")
      links <- rbind(links, qc_audit_link("trade benchmark <- Comtrade energy trade rows", QC_AUDIT_NOT_REPRO, NA,
                                          qc_audit_num(r, "trade_benchmark_share"),
                                          "the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows"))
    }
  } else {
    dn <- src$deployment_need
    if (!is.null(dn) && nrow(dn)) {
      tables$deployment_need <- dn
      if ("deployment_need" %in% names(dn)) {
        links <- rbind(links, qc_audit_cmp("deployment_need <- deployment_need_tbl", suppressWarnings(as.numeric(dn$deployment_need[1])),
                                           qc_audit_num(r, "deployment_need"), tol = 1e-9))
      }
    } else {
      links <- rbind(links, qc_audit_link("deployment-need row", QC_AUDIT_NOT_REPRO, NA, qc_audit_num(r, "deployment_need"),
                                          "no deployment_need_tbl row for this cell"))
    }
  }
  qc_audit_section("source_rows", qc_audit_items(items), links, tables)
}

qc_audit_s3_cleaning <- function(ctx) {
  r <- ctx$row
  cfg <- ctx$cfg
  sn <- cfg$strategic_need %||% list()
  df <- cfg$decision_framework %||% list()
  stage <- r$supply_chain
  excl <- toupper(unlist(sn$exclude_iso3 %||% character()))
  min_usd <- as.numeric(sn$min_import_usd %||% 1e6)
  self_line <- as.numeric(df$self_supply_share %||% 0.10)
  items <- list(
    entrepot_or_registry = r$iso3 %in% excl, sn_exclusion = qc_audit_get(r, "sn_exclusion"),
    posture_exclusion = qc_audit_get(r, "posture_exclusion"), thin_need_cell = qc_audit_get(r, "sn_thin_cell"),
    thin_reason = qc_audit_get(r, "sn_thin_reason"), material_importers_in_cell = qc_audit_num(r, "sn_cell_material_importers"),
    import_material = qc_audit_get(r, "sn_import_material"), self_supplied = qc_audit_get(r, "sn_self_supplied"),
    cp_thin_cell = qc_audit_get(r, "cp_thin_cell"), current_trade_flag = qc_audit_get(r, "current_trade_flag"),
    opposite_bloc_basis = qc_audit_get(r, "sn_opposite_bloc_basis"))
  det <- list(entrepot_or_registry = paste0("exclude_iso3: ", paste(excl, collapse = ", ")),
              material_importers_in_cell = paste0("thin below ", sn$min_material_importers_per_cell %||% 5),
              import_material = paste0("immaterial below USD ", format(min_usd, big.mark = ",", scientific = FALSE)),
              self_supplied = paste0("self-supply guard at home_share >= ", self_line))
  links <- qc_audit_links_empty()
  if (stage != "Deployment") {
    links <- rbind(links, qc_audit_cmp("entrepot rule -> need not ranked", r$iso3 %in% excl,
                                       identical(as.character(qc_audit_get(r, "sn_exclusion")), "entrepot")))
    imp <- qc_audit_num(r, "imports_usd")
    if (!is.na(qc_audit_get(r, "sn_import_material"))) {
      links <- rbind(links, qc_audit_cmp("immaterial-importer rule (imports_usd vs min_import_usd)", !is.na(imp) && imp >= min_usd,
                                         isTRUE(as.logical(qc_audit_get(r, "sn_import_material"))),
                                         detail = paste0("imports_usd ", qc_audit_fmt(imp))))
    }
    hs <- qc_audit_num(r, "home_share")
    if (!is.na(qc_audit_get(r, "sn_self_supplied"))) {
      links <- rbind(links, qc_audit_cmp("self-supply guard (home_share >= self_supply_share)", !is.na(hs) && hs >= self_line,
                                         isTRUE(as.logical(qc_audit_get(r, "sn_self_supplied"))), detail = paste0("home_share ", qc_audit_fmt(hs))))
    }
    links <- rbind(links, qc_audit_link("which countries enter the cell's percentiles", "recorded", NA, qc_audit_num(r, "sn_cell_countries"),
                                        "the ranked universe (es_contributions countries less exclude_iso3) is rebuilt only inside scripts/26"))
  } else {
    links <- rbind(links, qc_audit_link("supply-chain cleaning rules", "not_applicable", NA, NA, "Deployment is decided on deployment need"))
  }
  qc_audit_section("cleaning", qc_audit_items(items, det), links)
}

qc_audit_s4_transformations <- function(ctx) {
  r <- ctx$row
  cfg <- ctx$cfg
  stage <- r$supply_chain
  rows <- list()
  if (stage != "Deployment") {
    for (fam in names(cfg$strategic_need$families %||% list())) {
      for (m in cfg$strategic_need$families[[fam]]$measures) {
        rows[[length(rows) + 1]] <- data.frame(component = paste0("need: ", fam), variable = m$variable, source = m$source %||% NA_character_,
                                               orient = m$orient %||% "as_is", rule = "within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures",
                                               stringsAsFactors = FALSE)
      }
    }
  }
  cp_cfg <- cfg$competitive_position$families %||% list()
  for (fam in names(cp_cfg)) {
    st <- unlist(cp_cfg[[fam]]$stages)
    if (!is.null(st) && !stage %in% st) next
    for (v in cp_cfg[[fam]]$variables) {
      vs <- unlist(v$stages)
      if (!is.null(vs) && !stage %in% vs) next
      rows[[length(rows) + 1]] <- data.frame(component = paste0("position: ", fam), variable = v$variable %||% v$variable_pattern %||% NA_character_,
                                             source = paste0(v$pillar %||% "", " ", v$category %||% ""), orient = paste0("fill ", v$fill %||% "na"),
                                             rule = "pillar component value (within-cell index); family = mean of observed variables", stringsAsFactors = FALSE)
    }
  }
  if (stage != "Deployment") {
    rows[[length(rows) + 1]] <- data.frame(component = "prize", variable = "siv_score", source = "industrial_opportunity; node attributes",
                                           orient = "as_is", rule = "weighted mean of market, commons, option; strategic_industrial_value = mid-rank percentile across all scored rows; market-only cap at prize_threshold - cap_margin",
                                           stringsAsFactors = FALSE)
  } else {
    rows[[length(rows) + 1]] <- data.frame(component = "deployment need", variable = "dn_demand; dn_policy_gap; dn_system", source = "deployment_need_tbl",
                                           orient = "config/deployment_need.yml", rule = "family = percentile of the mean of its measure percentiles; deployment_need = mean of observed families",
                                           stringsAsFactors = FALSE)
  }
  tab <- do.call(rbind, rows)
  links <- qc_audit_links_empty()
  # the CP families the table carries for this cell against the families the configuration defines
  have <- QC_AUDIT_CP_FAMILIES[vapply(QC_AUDIT_CP_FAMILIES, function(f) !is.na(qc_audit_num(r, paste0("cp_", f))), logical(1))]
  undefined <- setdiff(have, names(cp_cfg))
  links <- rbind(links, if (length(undefined)) {
    qc_audit_link("CP families in the table are defined in the configuration", "differs", paste(names(cp_cfg), collapse = ", "),
                  paste(have, collapse = ", "), paste0("the table carries ", paste(undefined, collapse = ", "),
                                                       ", which this configuration does not define: the table was built with other code or config"))
  } else qc_audit_link("CP families in the table are defined in the configuration", "reproduced", NA, paste(have, collapse = ", ")))
  if (stage != "Deployment" && is.null(cfg$strategic_need$share_floor)) {
    links <- rbind(links, qc_audit_link("risky-supplier share floor", "recorded", NA, NA,
                                        "this configuration has no strategic_need.share_floor; a table built with one ranks China / opposite-bloc shares below it as 0"))
  }
  links <- rbind(links, qc_audit_link("measure-level percentiles (raw value -> measure percentile)", QC_AUDIT_NOT_REPRO, NA, NA,
                                      "measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions)"))
  qc_audit_section("transformations", qc_audit_items(list(percentile_rule = "mid-rank (rank - 0.5) / n, ties averaged (sp_helpers.R sp_midrank_pct)",
                                                          thresholds_inclusive = TRUE)), links, list(recipes = tab))
}

qc_audit_s5_indicators <- function(ctx) {
  r <- ctx$row
  cfg <- ctx$cfg
  peers <- ctx$peers
  stage <- r$supply_chain
  items <- list()
  links <- qc_audit_links_empty()
  if (stage != "Deployment") {
    for (f in c("sn_import_dependence", "sn_supplier_concentration", "sn_risky_supplier_exposure", "sn_trusted_alternatives")) items[[f]] <- qc_audit_num(r, f)
    # supplier concentration and risky suppliers from the stored import measures (strategic_need.R: measure
    # percentile among the cell's observed countries, immaterial importers 0)
    if (!is.null(peers) && nrow(peers) && !(r$iso3 %in% toupper(unlist(cfg$strategic_need$exclude_iso3)))) {
      ok <- !(peers$iso3 %in% toupper(unlist(cfg$strategic_need$exclude_iso3)))
      p <- peers[ok, , drop = FALSE]
      me <- which(p$iso3 == r$iso3)
      imm <- p$sn_import_material %in% FALSE
      # strategic_need.R: an immaterial importer's value is 0 before ranking (it joins the zero block) and its
      # percentile is then set to 0 (immaterial_need: zero)
      pct <- function(v) {
        v <- ifelse(imm & !is.na(v), 0, v)
        x <- sp_midrank_pct(v)
        x[imm & !is.na(x)] <- 0
        x
      }
      sf <- cfg$strategic_need$share_floor
      flo <- function(v) if (is.null(sf)) v else ifelse(!is.na(v) & v < sf, 0, v)
      if (length(me) == 1) {
        conc <- pct(p$import_hhi)[me]
        links <- rbind(links, qc_audit_cmp("sn_supplier_concentration <- percentile of import HHI in the cell", conc,
                                           qc_audit_num(r, "sn_supplier_concentration"), tol = 1e-6,
                                           detail = paste0("import_hhi ", qc_audit_fmt(qc_audit_num(r, "import_hhi")), " among ", sum(!is.na(p$import_hhi)), " observed importers")))
        ch <- pct(flo(p$china_share))[me]
        ob <- pct(flo(p$opposite_bloc_share))[me]
        risky <- mean(c(ch, ob), na.rm = TRUE)
        if (is.nan(risky)) risky <- NA_real_
        links <- rbind(links, qc_audit_cmp("sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares", risky,
                                           qc_audit_num(r, "sn_risky_supplier_exposure"), tol = 1e-6,
                                           detail = paste0("china_share ", qc_audit_fmt(qc_audit_num(r, "china_share")), ", opposite_bloc_share ",
                                                           qc_audit_fmt(qc_audit_num(r, "opposite_bloc_share")),
                                                           if (is.null(sf)) "; no share floor in this configuration" else paste0("; share floor ", sf))))
      }
    }
    links <- rbind(links, qc_audit_link("sn_import_dependence <- deficit_gdp / production surplus percentiles", QC_AUDIT_NOT_REPRO, NA,
                                        qc_audit_num(r, "sn_import_dependence"), "its measures live in index_outputs.rds contributions, not in the cell table"))
    links <- rbind(links, qc_audit_link("sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile", "recorded", NA,
                                        qc_audit_num(r, "sn_trusted_alternatives"),
                                        paste0("trusted_world_share ", qc_audit_fmt(qc_audit_num(r, "trusted_world_share")), ", home_share ",
                                               qc_audit_fmt(qc_audit_num(r, "home_share")))))
    for (f in c("siv_market", "siv_commons", "siv_option", "io_node_prize", "io_growth_pct", "io_investment_momentum_pct", "io_home_market")) items[[f]] <- qc_audit_num(r, f)
    items$io_outlook <- qc_audit_get(r, "io_outlook")
    items$io_growth_series <- qc_audit_get(r, "io_growth_series")
    links <- rbind(links, qc_audit_cmp("siv_market = industrial_opportunity", qc_audit_num(r, "industrial_opportunity"), qc_audit_num(r, "siv_market")))
    nodep <- qc_audit_num(r, "io_node_prize")
    home <- qc_audit_num(r, "io_home_market")
    if (!is.na(nodep)) {
      lift <- isTRUE(as.logical(qc_audit_get(r, "io_home_market_scored"))) && !is.na(home) && home > nodep
      links <- rbind(links, qc_audit_cmp("industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher",
                                         if (lift) (nodep + home) / 2 else nodep, qc_audit_num(r, "industrial_opportunity"), tol = 1e-6))
    }
    links <- rbind(links, qc_audit_link("io_node_prize <- WEO / minerals demand growth and capex momentum", QC_AUDIT_NOT_REPRO, NA, nodep,
                                        "node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26"))
  } else {
    for (f in QC_AUDIT_DN_FAMILIES) items[[f]] <- qc_audit_num(r, f)
    items$dn_families_observed <- qc_audit_num(r, "dn_families_observed")
  }
  for (f in QC_AUDIT_CP_FAMILIES) {
    v <- qc_audit_num(r, paste0("cp_", f))
    if (!is.na(v)) items[[paste0("cp_", f)]] <- v
  }
  links <- rbind(links, qc_audit_link("CP family values <- pillar variable contributions", QC_AUDIT_NOT_REPRO, NA, NA,
                                      "variable contributions live in index_outputs.rds, not in the cell table"))
  qc_audit_section("indicators", qc_audit_items(items), links)
}

qc_audit_s6_weights <- function(ctx) {
  r <- ctx$row
  cfg <- ctx$cfg
  df <- cfg$decision_framework %||% list()
  sv <- sp_siv_cfg(df)
  stage <- r$supply_chain
  items <- list(
    need_families = if (stage != "Deployment") paste0("equal over observed families (", qc_audit_fmt(qc_audit_num(r, "supply_chain_need_n_families")),
                                                     "; trusted alternatives in the score: ", qc_audit_fmt(qc_audit_get(r, "sn_trusted_in_score")), ")") else NA,
    cp_families = paste0("equal over observed families (", qc_audit_fmt(qc_audit_num(r, "cp_n_families")), " of ",
                         qc_audit_fmt(qc_audit_num(r, "cp_n_families_applicable")), " applicable)"),
    export_position_momentum_weight = qc_audit_num(r, "trade_momentum_weight"),
    siv_weights = paste0("market ", sv$weights[["market"]], ", commons ", sv$weights[["commons"]], ", option ", sv$weights[["option"]]),
    io_investment_momentum_weight = (df$industrial_opportunity$investment_momentum$weight %||% NA),
    deployment_need = if (stage == "Deployment") "equal over observed families" else NA)
  links <- qc_audit_links(list(qc_audit_link("weights", "recorded", NA, NA, "read from config/strategic_position.yml; no sensitivity of the SIV weights exists")))
  qc_audit_section("weights", qc_audit_items(items[!vapply(items, function(v) length(v) == 1 && is.na(v), logical(1))]), links)
}
qc_audit_links <- function(l) do.call(rbind, l)

# The Catch Up pathway (pathway.R sp_pathway(), rule "anchor") re-thresholded from the cell's stored pw_* signals:
# an anchor (measured share, export share, operating capacity, committed pipeline, adjacent-stage share, or the
# resource base at Extraction) corroborated by a second evidence family. A QC re-implementation of the
# thresholding only (the signals themselves need side tables scripts/26 does not save); a mismatch with the
# stored pathway_credible is reported as differs, never silently tolerated. pcfg: decision_framework$pathway.
# Returns list(credible (yes / no / unknown; NA at Deployment or under another rule), anchors, support
# (";" lists), n_fam_fired, n_fam_obs).
qc_audit_pathway_anchor <- function(r, pcfg = list()) {
  if (identical(r$supply_chain, "Deployment") || !identical(pcfg$rule %||% "anchor", "anchor")) {
    return(list(credible = NA_character_, anchors = NA_character_, support = NA_character_, n_fam_fired = NA_integer_, n_fam_obs = NA_integer_))
  }
  n <- function(c) qc_audit_num(r, c)
  ge <- function(v, t) !is.na(v) && v >= t
  ext <- identical(r$supply_chain, "Extraction")
  rb <- as.logical(qc_audit_get(r, "resource_base"))
  anchors <- c(direct_supply_share = ge(n("pw_direct_share"), pcfg$direct_share_min %||% 0.005),
               direct_export_share = ge(n("pw_export_share"), pcfg$direct_share_min %||% 0.005),
               operating_capacity = ge(n("pw_operating_share"), pcfg$operating_capacity_share_min %||% 0.005),
               committed_pipeline = ge(n("pw_committed_share"), pcfg$committed_pipeline_share_min %||% 0.01),
               adjacent_stage_share = ge(n("pw_adjacent_share_measured"), pcfg$adjacent_share %||% 0.05),
               resource_base = ext && isTRUE(rb))
  support <- c(feasibility = ge(n("pw_feasibility_pct"), pcfg$feasibility_pct %||% 0.75),
               input_cost = ge(n("pw_input_cost_pct"), pcfg$input_cost_pct %||% 0.75),
               adjacent_stage_relative = ge(n("pw_adjacent_cp_within"), pcfg$adjacent_cp_within_pct %||% 0.6),
               learning = qc_audit_get(r, "pw_learning") %in% unlist(pcfg$learning_levels %||% c("moderate", "strong")),
               announced_pipeline = ge(n("pw_announced_share"), pcfg$announced_pipeline_share_min %||% 0.02))
  fired <- c(anchors, support)
  fam_fired <- unique(unname(QC_AUDIT_PATHWAY_FAMILY[names(fired)[fired]]))
  fam_obs <- c(direct = !is.na(n("pw_direct_share")) || !is.na(n("pw_export_share")), capacity = !is.na(n("pw_operating_share")),
               pipeline = !is.na(n("pw_committed_share")),
               adjacency = !is.na(n("pw_adjacent_share_measured")) || !is.na(n("pw_adjacent_cp_within")),
               resource = ext && !is.na(rb), feasibility = !is.na(n("pw_feasibility_pct")), input_cost = !is.na(n("pw_input_cost_pct")),
               learning = !is.na(qc_audit_get(r, "pw_learning")))
  any_anchor <- any(anchors)
  anchor_obs <- any(fam_obs[c("direct", "capacity", "pipeline", "adjacency", "resource")])
  nf <- length(fam_fired)
  no <- sum(fam_obs)
  corroborated <- any_anchor && nf >= 2
  credible <- if (ext && isFALSE(rb)) "no" else if (corroborated && ext && is.na(rb)) "unknown" else if (corroborated) "yes" else
    if (no == 0) "unknown" else if (!any_anchor && !anchor_obs) "unknown" else if (!any_anchor) "no" else if (no > nf) "no" else "unknown"
  list(credible = credible, anchors = if (any(anchors)) paste(names(anchors)[anchors], collapse = ";") else "none",
       support = if (any(support)) paste(names(support)[support], collapse = ";") else "none", n_fam_fired = nf, n_fam_obs = no)
}
# the evidence family of each anchor and signal (as pathway.R SP_PATHWAY_FAMILY; restated so the audit does not
# need pathway.R sourced, and a divergence shows up as a differing pathway link)
QC_AUDIT_PATHWAY_FAMILY <- c(direct_supply_share = "direct", direct_export_share = "direct", operating_capacity = "capacity",
                             committed_pipeline = "pipeline", adjacent_stage_share = "adjacency", resource_base = "resource",
                             feasibility = "feasibility", input_cost = "input_cost", adjacent_stage_relative = "adjacency",
                             learning = "learning", announced_pipeline = "pipeline")

qc_audit_s7_scores <- function(ctx) {
  r <- ctx$row
  cfg <- ctx$cfg
  df <- cfg$decision_framework %||% list()
  th <- sp_choice_thresholds(df)
  peers <- ctx$peers
  stage <- r$supply_chain
  items <- list()
  links <- qc_audit_links_empty()
  me <- if (!is.null(peers)) which(peers$iso3 == r$iso3) else integer()
  if (stage != "Deployment") {
    items$supply_chain_need_score <- qc_audit_num(r, "supply_chain_need_score")
    items$supply_chain_need_uncapped <- qc_audit_num(r, "supply_chain_need_uncapped")
    items$supply_chain_need <- qc_audit_num(r, "supply_chain_need")
    items$supply_chain_need_cap <- qc_audit_get(r, "supply_chain_need_cap")
    fams <- c("sn_import_dependence", "sn_supplier_concentration", "sn_risky_supplier_exposure",
              if (isTRUE(as.logical(qc_audit_get(r, "sn_trusted_in_score")))) "sn_trusted_alternatives")
    v <- vapply(fams, function(f) qc_audit_num(r, f), numeric(1))
    if (isTRUE(as.logical(qc_audit_get(r, "sn_self_supplied")))) v[intersect(names(v), c("sn_supplier_concentration", "sn_risky_supplier_exposure", "sn_trusted_alternatives"))] <-
      ifelse(is.na(v[intersect(names(v), c("sn_supplier_concentration", "sn_risky_supplier_exposure", "sn_trusted_alternatives"))]), NA, 0)
    sc <- if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    links <- rbind(links, qc_audit_cmp("need score = mean of observed need families (self-supply guard applied)", sc,
                                       qc_audit_num(r, "supply_chain_need_score"), tol = 1e-9))
    if (length(me) == 1) {
      unc <- sp_midrank_pct(peers$supply_chain_need_score)[me]
      links <- rbind(links, qc_audit_cmp("need percentile = within-cell mid-rank of the need score", unc,
                                         qc_audit_num(r, "supply_chain_need_uncapped"), tol = 1e-9,
                                         detail = paste0(sum(!is.na(peers$supply_chain_need_score)), " scored economies in the cell")))
    }
    floor0 <- (df$supply_chain_need %||% list())$score_floor %||% 0
    u <- qc_audit_num(r, "supply_chain_need_uncapped")
    s <- qc_audit_num(r, "supply_chain_need_score")
    capped <- !is.na(u) && !is.na(s) && u >= th$need - th$band && s < floor0
    links <- rbind(links, qc_audit_cmp("need-floor cap (score below score_floor with percentile >= need - band)",
                                       if (is.na(u)) NA_real_ else if (capped) min(u, th$need - th$band - 0.01) else u,
                                       qc_audit_num(r, "supply_chain_need"), tol = 1e-9,
                                       detail = paste0("score_floor ", floor0, if (capped) ": capped" else ": not capped")))
    items$siv_score <- qc_audit_num(r, "siv_score")
    items$strategic_industrial_value <- qc_audit_num(r, "strategic_industrial_value")
    items$siv_cap <- qc_audit_get(r, "siv_cap")
    sv <- sp_siv_cfg(df)
    m <- c(qc_audit_num(r, "siv_market"), qc_audit_num(r, "siv_commons"), qc_audit_num(r, "siv_option"))
    w <- sv$weights
    w[is.na(m)] <- 0
    sscore <- if (is.na(m[1]) && sv$require_market) NA_real_ else if (sum(w) > 0) sum(w * ifelse(is.na(m), 0, m)) / sum(w) else NA_real_
    links <- rbind(links, qc_audit_cmp("siv_score = weighted mean of market, commons, option", sscore, qc_audit_num(r, "siv_score"), tol = 1e-9))
    if (!is.null(ctx$siv_ref) && length(ctx$siv_ref)) {
      pct <- sp_siv_ref_pct(qc_audit_num(r, "siv_score"), ctx$siv_ref)
      mo <- !is.na(pct) && !(isTRUE(m[2] >= sv$gate) || isTRUE(m[3] >= sv$gate))
      links <- rbind(links, qc_audit_cmp("strategic_industrial_value = mid-rank among all scored rows, market-only cap",
                                         if (mo) min(pct, sv$prize_threshold - sv$cap_margin) else pct, qc_audit_num(r, "strategic_industrial_value"),
                                         tol = 1e-9, detail = paste0(length(ctx$siv_ref[!is.na(ctx$siv_ref)]), " scored rows; gate ", sv$gate,
                                                                     if (mo) ": market only, capped" else "")))
      links <- rbind(links, qc_audit_cmp("siv_cap", if (is.na(pct)) NA else if (mo) "market_only" else NA_character_,
                                         as.character(qc_audit_get(r, "siv_cap"))))
    }
    items$large_yard_case <- qc_audit_get(r, "large_yard_case")
    items$large_yard_n <- qc_audit_num(r, "large_yard_n")
    items$large_yard_signals <- qc_audit_get(r, "large_yard_signals")
    items$large_yard_unknown <- qc_audit_get(r, "large_yard_unknown")
    sig <- qc_audit_get(r, "large_yard_signals")
    req <- sp_ly_cfg(df)$required
    n_sig <- if (is.na(sig)) 0L else length(setdiff(strsplit(sig, ";", fixed = TRUE)[[1]], req))
    links <- rbind(links, qc_audit_cmp("large_yard_n = commodity signals holding (required signals excluded)", n_sig, qc_audit_num(r, "large_yard_n")))
    links <- rbind(links, qc_audit_link("large-yard signals <- node attributes and world_top_share", "recorded", NA, sig,
                                        "recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check)"))
    items$pathway_credible <- qc_audit_get(r, "pathway_credible")
    items$pathway_basis <- qc_audit_get(r, "pathway_basis")
    items$catch_up_anchor <- qc_audit_get(r, "catch_up_anchor")
    pw <- qc_audit_pathway_anchor(r, df$pathway %||% list())
    items$pathway_anchors_fired <- pw$anchors
    items$pathway_support_fired <- pw$support
    links <- rbind(links, qc_audit_cmp("pathway_credible <- anchor rule re-thresholded from the stored pw_* signals", pw$credible,
                                       as.character(qc_audit_get(r, "pathway_credible")),
                                       detail = paste0("rule ", df$pathway$rule %||% "anchor", "; ", pw$n_fam_fired, " evidence families fired of ",
                                                       pw$n_fam_obs, " observed")))
    links <- rbind(links, qc_audit_link("pw_* signals <- GCIM pipeline, capacity and input-cost side tables", QC_AUDIT_NOT_REPRO, NA, NA,
                                        "sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run"))
  } else {
    items$deployment_need <- qc_audit_num(r, "deployment_need")
    items$dn_confidence <- qc_audit_get(r, "dn_confidence")
    v <- vapply(QC_AUDIT_DN_FAMILIES, function(f) qc_audit_num(r, f), numeric(1))
    links <- rbind(links, qc_audit_cmp("deployment_need = mean of observed family percentiles", if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE),
                                       qc_audit_num(r, "deployment_need"), tol = 1e-9))
    links <- rbind(links, qc_audit_cmp("dn_families_observed", sum(!is.na(v)), qc_audit_num(r, "dn_families_observed")))
  }
  items$competitive_position_score <- qc_audit_num(r, "competitive_position_score")
  items$competitive_position_percentile <- qc_audit_num(r, "competitive_position_percentile")
  items$cp_contender_pct <- qc_audit_num(r, "cp_contender_pct")
  items$cp_contender_basis <- qc_audit_get(r, "cp_contender_basis")
  items$cp_n_contenders <- qc_audit_num(r, "cp_n_contenders")
  items$cp_is_contender <- qc_audit_get(r, "cp_is_contender")
  items$home_share_measured <- dplyr::coalesce(qc_audit_num(r, "home_share_exposure"), qc_audit_num(r, "home_share"))
  items$t3_capability_lo <- qc_audit_get(r, "t3_capability_lo")
  cpv <- vapply(QC_AUDIT_CP_FAMILIES, function(f) qc_audit_num(r, paste0("cp_", f)), numeric(1))
  links <- rbind(links, qc_audit_cmp("CP score = mean of observed CP families", if (all(is.na(cpv))) NA_real_ else mean(cpv, na.rm = TRUE),
                                     qc_audit_num(r, "competitive_position_score"), tol = 1e-9,
                                     detail = paste0("families: ", paste(QC_AUDIT_CP_FAMILIES[!is.na(cpv)], collapse = ", "))))
  if (length(me) == 1 && "competitive_position_score" %in% names(peers)) {
    cc <- df$contenders %||% list(min_share = 0.005, min_n = 5)
    share <- dplyr::coalesce(peers$home_share_exposure, peers$home_share)
    is_c <- !is.na(peers$competitive_position_score) & !is.na(share) & share >= (cc$min_share %||% 0.005)
    if (sum(is_c) >= (cc$min_n %||% 5)) {
      e <- sp_choice_ref_pct(peers$competitive_position_score[me], peers$competitive_position_score[is_c])
      links <- rbind(links, qc_audit_cmp("cp_contender_pct = CP score placed among the node's contenders", e, qc_audit_num(r, "cp_contender_pct"),
                                         tol = 1e-9, detail = paste0(sum(is_c), " contenders (measured share >= ", cc$min_share %||% 0.005, ")")))
    } else {
      links <- rbind(links, qc_audit_cmp("cp_contender_pct = cross-country percentile (fewer than min_n contenders)",
                                         qc_audit_num(r, "competitive_position_percentile"), qc_audit_num(r, "cp_contender_pct"), tol = 1e-9))
    }
  }
  links <- rbind(links, qc_audit_link("capability lower bound (Secure engine Test 3)", QC_AUDIT_NOT_REPRO, NA, qc_audit_get(r, "t3_capability_lo"),
                                      "the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds)"))
  qc_audit_section("scores", qc_audit_items(items), links)
}

# The decision steps the canonical rule takes (sp_choice_rule), each with value, threshold, margin and result,
# compared with the stored decision_trace.
qc_audit_s8_thresholds <- function(ctx) {
  r <- ctx$row
  df <- ctx$cfg$decision_framework %||% list()
  th <- sp_choice_thresholds(df)
  stage <- r$supply_chain
  dep <- stage == "Deployment"
  steps <- list()
  add <- function(step, test, value, threshold, result, note = NA_character_) {
    steps[[length(steps) + 1]] <<- data.frame(step = step, test = test, value = qc_audit_fmt(value), threshold = qc_audit_fmt(threshold),
                                              margin = if (is.numeric(value) && is.numeric(threshold) && !is.na(value) && !is.na(threshold))
                                                qc_audit_fmt(round(value - threshold, 4)) else NA_character_,
                                              result = as.character(result), note = note, stringsAsFactors = FALSE)
  }
  secure <- identical(as.character(qc_audit_get(r, "engine_posture")), "secure")
  # the posture engine's tests as stored (recorded, not rerun: see section 7)
  for (tt in c("t1_consequence", "t2_vulnerability", "t2_path", "t3_capability", "t3_position", "t4_strategy", "t5_proportionality")) {
    v <- qc_audit_get(r, tt)
    if (!is.na(v)) add("secure_engine", tt, v, NA, v, "posture engine test as stored (not rerun)")
  }
  add("secure", "secure_override (posture engine)", qc_audit_get(r, "engine_posture"), "secure", if (secure) "yes" else "no",
      qc_audit_get(r, "security_rationale"))
  cp <- qc_audit_num(r, "cp_contender_pct")
  cap_lo <- qc_audit_get(r, "t3_capability_lo")
  is_c <- qc_audit_get(r, "cp_is_contender")
  basis <- qc_audit_get(r, "cp_contender_basis")
  cp_high <- sp_choice_high_cp(cp, basis, is_c, cap_lo, th$cp, th$floor)
  if (!secure) {
    if (dep) {
      dn <- qc_audit_num(r, "deployment_need")
      add("deployment", "deployment_need", dn, th$deployment, if (is.na(dn)) "not_assessed" else if (dn >= th$deployment) "high" else "low",
          paste0(qc_audit_fmt(qc_audit_num(r, "dn_families_observed")), " families observed"))
    } else {
      need <- qc_audit_num(r, "supply_chain_need")
      prize <- qc_audit_num(r, sp_choice_prize_col(df))
      capped <- identical(as.character(qc_audit_get(r, "siv_cap")), "market_only")
      ly <- as.logical(qc_audit_get(r, "large_yard_case"))
      if (is.na(need) || is.na(prize)) add("assessed", "need and prize observed", NA, NA, "not_assessed")
      add("large_yard", "large_yard_case", qc_audit_num(r, "large_yard_n"), th$large_min,
          if (isTRUE(ly)) (if (th$large_position_first && cp_high) "yes, bypassed by a high position (position_first)" else "yes") else if (isFALSE(ly)) "no" else "undecided",
          qc_audit_get(r, "large_yard_reason"))
      add("need", "supply_chain_need", need, th$need, if (is.na(need)) "missing" else if (need >= th$need) "high" else "low",
          if (!is.na(qc_audit_get(r, "supply_chain_need_cap"))) "capped by the need score floor" else NA_character_)
      add("prize", sp_choice_prize_col(df), prize, th$prize,
          if (is.na(prize)) "missing" else if (capped) "low (market-only cap)" else if (prize >= th$prize) "high" else "low")
      add("pathway", "pathway_credible", qc_audit_get(r, "pathway_credible"), "yes", qc_audit_get(r, "pathway_credible"),
          "read only for a high prize without a high position")
    }
  }
  add("position", "cp_contender_pct", cp, th$cp, if (cp_high) "high" else if (is.na(cp)) "not measured" else "not_top_quarter",
      paste0("contender ", qc_audit_fmt(is_c), ", capability lower bound ", qc_audit_fmt(cap_lo), " (floor ", paste(th$floor, collapse = "/"), ")"))
  tab <- do.call(rbind, steps)
  # the stored trace
  links <- qc_audit_links_empty()
  tr <- tryCatch(jsonlite::fromJSON(as.character(qc_audit_get(r, "decision_trace")), simplifyVector = TRUE), error = function(e) NULL)
  if (is.data.frame(tr) && nrow(tr)) {
    for (k in seq_len(nrow(tr))) {
      st <- tr$step[k]
      mine <- tab[tab$step == st, , drop = FALSE]
      if (!nrow(mine)) next
      if ("value" %in% names(tr) && !is.null(tr$value[k]) && !is.na(suppressWarnings(as.numeric(tr$value[k])))) {
        links <- rbind(links, qc_audit_cmp(paste0("trace ", st, ": value"), suppressWarnings(as.numeric(mine$value[1])),
                                           as.numeric(tr$value[k]), tol = 5e-4 + 1e-9, detail = "stored trace rounds to 3 decimals"))
      }
      if ("threshold" %in% names(tr) && !is.na(suppressWarnings(as.numeric(tr$threshold[k])))) {
        links <- rbind(links, qc_audit_cmp(paste0("trace ", st, ": threshold"), suppressWarnings(as.numeric(mine$threshold[1])),
                                           as.numeric(tr$threshold[k]), tol = 1e-9))
      }
      if (st %in% c("need", "prize", "position")) {
        hi_mine <- startsWith(mine$result[1], "high")
        hi_tr <- identical(tr$result[k], "high")
        links <- rbind(links, qc_audit_cmp(paste0("trace ", st, ": result"), hi_mine, hi_tr, detail = paste0("stored ", tr$result[k])))
      }
    }
  } else {
    links <- rbind(links, qc_audit_link("stored decision_trace", QC_AUDIT_NOT_REPRO, NA, NA, "no parseable decision_trace on the row"))
  }
  qc_audit_section("thresholds", qc_audit_items(list(decision_step = qc_audit_get(r, "decision_step"),
                                                     borderline = qc_audit_get(r, "decision_borderline"))),
                   links, list(steps = tab))
}

qc_audit_s9_classification <- function(ctx) {
  r <- ctx$row
  df <- ctx$cfg$decision_framework %||% list()
  th <- sp_choice_thresholds(df)
  inp <- sp_choice_inputs(r, df)
  ch <- sp_choice_high_cp(r$cp_contender_pct, r$cp_contender_basis, r$cp_is_contender, r$t3_capability_lo, th$cp, th$floor)
  rerun <- sp_choice_rule(r$supply_chain, r$engine_posture %in% "secure", inp$need, inp$prize, ch, r$pathway_credible, inp$dn, th,
                          inp$large, inp$capped)
  stored <- qc_audit_get(r, "strategic_choice")
  items <- list(strategic_choice = stored, choice_label = qc_audit_choice_label(stored, ctx$labels),
                strategic_posture = qc_audit_get(r, "strategic_posture"), compete_mode = qc_audit_get(r, "compete_mode"),
                deploy_mode = qc_audit_get(r, "deploy_mode"), decision_step = qc_audit_get(r, "decision_step"),
                posture_variant = qc_audit_get(r, "posture_variant"), posture_confidence = qc_audit_get(r, "posture_confidence"),
                choice_possible = qc_audit_get(r, "choice_possible"), pivotal_inputs = qc_audit_get(r, "pivotal_inputs"),
                consistency_flag = qc_audit_get(r, "consistency_flag"))
  rb <- ctx$robustness
  if (!is.null(rb) && nrow(rb)) {
    items$robustness_class <- rb$class[1]
    items$robustness_local_flips <- rb$local_flips[1]
    items$robustness_structural_flips <- rb$structural_flips[1]
    items$robustness_fence_change <- rb$fence_change[1]
  }
  links <- qc_audit_cmp("choice = canonical rule rerun from the stored inputs", if (is.na(stored)) NA else rerun, stored)
  if (is.null(rb) || !nrow(rb)) links <- rbind(links, qc_audit_link("robustness class", "recorded", NA, NA,
                                                                    "qc/results/qc_sensitivity_pipeline.csv not given: run scripts/qc/run_sensitivity.R"))
  qc_audit_section("classification", qc_audit_items(items), links)
}

qc_audit_s10_recommendation <- function(ctx) {
  r <- ctx$row
  ch <- qc_audit_get(r, "strategic_choice")
  yard <- sp_yard_of_choice(ch, r$supply_chain)
  fence <- if (is.na(yard)) NA_character_ else unname(SP_YARD_FENCE[yard])
  lb <- ctx$labels
  items <- list(choice = ch, choice_label = qc_audit_choice_label(ch, lb),
                choice_action = if (!is.na(ch)) unname(lb$choice_action[ch]) else NA,
                yard = yard, yard_and_fence = if (!is.na(yard)) unname(lb$yard_and_fence[yard]) else NA,
                fence = if (!is.na(fence)) unname(lb$fence_name[fence]) else NA,
                yard_rollup = qc_audit_get(r, "yard_rollup"), fence_level = qc_audit_get(r, "fence_level"),
                fence_posture = qc_audit_get(r, "fence_posture"), gate_level = qc_audit_get(r, "gate_level"),
                build_posture = qc_audit_get(r, "build_posture"), benchmark = qc_audit_get(r, "benchmark"),
                success_metrics = qc_audit_get(r, "success_metrics"), exit_condition = qc_audit_get(r, "exit_condition"),
                review_interval_years = qc_audit_get(r, "review_interval_years"), reason = qc_audit_get(r, "choice_reason"),
                watch_reasons = qc_audit_get(r, "watch_reasons"),
                recommended_partners = paste(stats::na.omit(c(qc_audit_get(r, "recommended_partner_1"), qc_audit_get(r, "recommended_partner_2"),
                                                              qc_audit_get(r, "recommended_partner_3"))), collapse = ", "),
                policy_alignment = qc_audit_get(r, "policy_alignment"),
                policy_alignment_label = if (!is.na(qc_audit_get(r, "policy_alignment"))) unname(lb$alignment_name[qc_audit_get(r, "policy_alignment")]) else NA)
  items <- items[!vapply(items, function(v) is.null(v) || (length(v) == 1 && (is.na(v) || identical(v, ""))), logical(1))]
  links <- qc_audit_cmp("yard = yard of the choice (yard_rollup.R)", yard, as.character(qc_audit_get(r, "yard")))
  if (!is.na(ch) && ch != "not_assessed" && is.na(qc_audit_get(r, "fence_posture"))) {
    links <- rbind(links, qc_audit_link("policy-design text", QC_AUDIT_NOT_REPRO, NA, NA, "no fence / build posture text on the row"))
  } else {
    links <- rbind(links, qc_audit_link("policy-design text <- config/posture_policy_design.csv by posture variant", "recorded", NA,
                                        qc_audit_get(r, "posture_variant")))
  }
  links <- rbind(links, qc_audit_link("policy alignment", "recorded", NA, qc_audit_get(r, "policy_alignment"),
                                      "after the choice; never an input to it"))
  qc_audit_section("recommendation", qc_audit_items(items), links)
}

# ---- shipped bundle comparison --------------------------------------------------------------------------------
# bundle: list(detail = the cell's object from posture-detail-<ISO3>.json (list) or NULL, core = list(p, m, dm, yd,
# need, prize, cp, ...) for the cell from posture-core, created, git_commit, detail_file, core_file). tol: the
# bundle rounds to 3 decimals (posture_r3).
qc_audit_bundle_compare <- function(row, bundle, labels = NULL) {
  r <- row
  dep <- r$supply_chain == "Deployment"
  d <- bundle$detail
  core <- bundle$core
  g <- function(path) {
    x <- d
    for (p in path) {
      if (is.null(x)) return(NULL)
      x <- if (is.numeric(p)) (if (length(x) >= p) x[[p]] else NULL) else x[[p]]
    }
    x
  }
  pipe_choice <- qc_audit_get(r, "strategic_choice")
  core_choice <- if (is.null(core)) NULL else switch(as.character(core$p %||% NA), S = "secure", C = core$m %||% NA, D = core$dm %||% NA,
                                                     N = "not_assessed", NA)
  yard_code <- c(small = "S", medium = "M", large = "L", mixed_high_medium = "SM", mixed_medium_low = "ML", mixed_high_low = "SL",
                 mixed_high_medium_low = "SML")
  map <- list(
    list("choice (core)", pipe_choice, core_choice),
    list("yard roll-up (core yd)", unname(yard_code[as.character(qc_audit_get(r, "yard_rollup"))]), core$yd),
    list("posture", qc_audit_get(r, "strategic_posture"), g("posture")),
    list("variant", qc_audit_get(r, "posture_variant"), g("variant")),
    list("mode", qc_audit_get(r, "compete_mode"), g("mode")),
    list("deploy_mode", qc_audit_get(r, "deploy_mode"), g("deploy_mode")),
    list("confidence", qc_audit_get(r, "posture_confidence"), g("confidence")),
    list("need", if (dep) qc_audit_num(r, "deployment_need") else qc_audit_num(r, "supply_chain_need"), g("need")),
    list("prize.value", if (dep) NA else qc_audit_num(r, if ("choice_prize" %in% names(r)) "choice_prize" else "industrial_opportunity"), g(c("prize", "value"))),
    list("prize.market", if (dep) NA else qc_audit_num(r, "industrial_opportunity"), g(c("prize", "market"))),
    list("prize.node_prize", if (dep) NA else qc_audit_num(r, "io_node_prize"), g(c("prize", "node_prize"))),
    list("prize.outlook", if (dep) NA else qc_audit_get(r, "io_outlook"), g(c("prize", "outlook"))),
    list("prize.measure", if (dep) NA else qc_audit_get(r, "prize_measure"), g(c("prize", "measure"))),
    list("prize.investment_momentum", if (dep) NA else qc_audit_num(r, "io_investment_momentum_pct"), g(c("prize", "investment_momentum"))),
    list("prize.home_market", if (dep) NA else qc_audit_num(r, "io_home_market"), g(c("prize", "home_market"))),
    list("pivotal", qc_audit_get(r, "pivotal_inputs"), g("pivotal")),
    list("cp", qc_audit_num(r, "cp_contender_pct"), g("cp")),
    list("home_share", dplyr::coalesce(qc_audit_num(r, "home_share_exposure"), qc_audit_num(r, "home_share")), g("home_share")),
    list("pathway_credible", if (dep) NA else qc_audit_get(r, "pathway_credible"), g("pathway_credible")),
    list("pathway_basis", if (dep) NA else qc_audit_get(r, "pathway_basis"), g("pathway_basis")),
    list("borderline", qc_audit_get(r, "decision_borderline"), g("borderline")),
    list("choice_possible", { v <- qc_audit_get(r, "choice_possible"); if (!is.na(v) && grepl("|", v, fixed = TRUE)) v else NA }, g("choice_possible")),
    list("consistency_flag", qc_audit_get(r, "consistency_flag"), g("consistency_flag")),
    list("reason", if (identical(pipe_choice, "not_assessed")) NA else qc_audit_get(r, "choice_reason"), g("reason")),
    list("policy.alignment", qc_audit_get(r, "policy_alignment"), g(c("policy", "alignment"))),
    list("partners[1].iso3", qc_audit_get(r, "recommended_partner_1"), g(list("partners", 1, "iso3")))
  )
  if (!dep) map <- c(map, list(
    list("import_dependence (sn_import_dependence, a percentile)", qc_audit_num(r, "sn_import_dependence"), g("import_dependence")),
    list("import_hhi (sn_supplier_concentration, a percentile)", qc_audit_num(r, "sn_supplier_concentration"), g("import_hhi")),
    list("china_bloc (sn_risky_supplier_exposure, a percentile)", qc_audit_num(r, "sn_risky_supplier_exposure"), g("china_bloc")),
    list("trusted_alternatives", qc_audit_num(r, "sn_trusted_alternatives"), g("trusted_alternatives")),
    list("top_supplier", qc_audit_get(r, "top_supplier_iso3"), g("top_supplier")),
    list("top_supplier_share", qc_audit_num(r, "top_supplier_share"), g("top_supplier_share")),
    list("china_share", qc_audit_num(r, "china_share"), g("china_share")),
    list("opposite_bloc_share", qc_audit_num(r, "opposite_bloc_share"), g("opposite_bloc_share")),
    list("imports_to_gdp", qc_audit_num(r, "imports_to_gdp"), g("imports_to_gdp")),
    list("trusted_share", qc_audit_num(r, "trusted_world_share"), g("trusted_share")),
    list("coercive_top", qc_audit_get(r, "coercive_top_iso3"), g("coercive_top")),
    list("coercive_top_share", qc_audit_num(r, "coercive_top_share"), g("coercive_top_share")),
    list("coercive_import_share", qc_audit_num(r, "coercive_import_share"), g("coercive_import_share")),
    list("trade.benchmark_share", qc_audit_num(r, "trade_benchmark_share"), g(c("trade", "benchmark_share"))),
    list("trade.benchmark_rca", qc_audit_num(r, "trade_benchmark_rca"), g(c("trade", "benchmark_rca"))),
    list("trade.momentum_score", qc_audit_num(r, "trade_momentum_score"), g(c("trade", "momentum_score"))),
    list("trade.benchmark_exports", qc_audit_num(r, "trade_benchmark_exports"), g(c("trade", "benchmark_exports"))),
    list("current_trade.weight", { w <- qc_audit_num(r, "exposure_weight"); if (identical(qc_audit_get(r, "exposure_basis"), "blend")) w else NA },
         g(c("current_trade", "weight"))),
    list("current_trade.china_share_base", qc_audit_num(r, "china_share_base"), g(c("current_trade", "china_share_base"))),
    list("current_trade.china_share_latest", qc_audit_num(r, "china_share_t12m"), g(c("current_trade", "china_share_latest")))
  )) else map <- c(map, lapply(QC_AUDIT_DN_FAMILIES, function(f) list(paste0(f, ".value"), qc_audit_num(r, f), g(c(f, "value")))),
                   list(list("dn_families_observed", qc_audit_num(r, "dn_families_observed"), g("dn_families_observed"))))
  for (f in QC_AUDIT_CP_FAMILIES) {
    map[[length(map) + 1]] <- list(paste0("cp_families.", f), qc_audit_num(r, paste0("cp_", f)), g(c("cp_families", f)))
  }
  rows <- lapply(map, function(m) {
    pv <- m[[2]]
    bv <- m[[3]]
    if (is.list(bv)) bv <- unlist(bv)
    if (length(bv) == 0) bv <- NA
    if (length(pv) == 0) pv <- NA
    pna <- is.na(pv)
    bna <- is.na(bv[1])
    status <- if (pna && bna) "both_absent" else if (bna) "missing_in_bundle" else if (pna) "missing_in_pipeline" else {
      pn <- suppressWarnings(as.numeric(pv))
      bn <- suppressWarnings(as.numeric(bv[1]))
      if (is.numeric(pv) && !is.na(bn)) {
        if (abs(pn - bn) <= 5e-4 + 5e-3 * abs(pn) + 1e-12) "same" else "differs"
      } else if (identical(as.character(pv), as.character(bv[1]))) "same" else "differs"
    }
    data.frame(field = m[[1]], pipeline = qc_audit_fmt(pv), bundle = qc_audit_fmt(bv[1]), status = status, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab <- tab[tab$status != "both_absent", , drop = FALSE]
  rownames(tab) <- NULL
  list(available = !is.null(d) || !is.null(core), bundle_created = bundle$created %||% NA_character_,
       bundle_git_commit = bundle$git_commit %||% NA_character_, detail_file = bundle$detail_file %||% NA_character_,
       core_file = bundle$core_file %||% NA_character_, fields = tab,
       n_differences = sum(tab$status != "same"), choice_pipeline = pipe_choice, choice_bundle = core_choice %||% NA_character_)
}

# ---- the audit ------------------------------------------------------------------------------------------------
# row: the cell's row of strategic_position_tbl (one-row data frame); peers: the same tech x stage rows (every
# economy: iso3 and the score columns); siv_ref: every siv_score of the table (the SIV percentile's reference);
# cfg: config/strategic_position.yml (list); manifest: config/raw_inputs_manifest.yml (list); sources: see
# qc_audit_s2_rows(); bundle: see qc_audit_bundle_compare() (NULL: not compared); labels: qc_audit_labels();
# robustness: the cell's cell_class row of qc_sensitivity_pipeline.csv (or NULL); meta: list(case_id,
# generated_at, pipeline_table, pipeline_release, sp_config, case_title).
qc_audit_cell <- function(row, peers = NULL, siv_ref = NULL, cfg = list(), manifest = list(), sources = list(), bundle = NULL,
                          labels = list(), robustness = NULL, meta = list()) {
  stopifnot(is.data.frame(row), nrow(row) == 1)
  ctx <- list(row = row, peers = peers, siv_ref = siv_ref, cfg = cfg, manifest = manifest, sources = sources, labels = labels,
              robustness = robustness)
  sections <- list(qc_audit_s1_sources(ctx), qc_audit_s2_rows(ctx), qc_audit_s3_cleaning(ctx), qc_audit_s4_transformations(ctx),
                   qc_audit_s5_indicators(ctx), qc_audit_s6_weights(ctx), qc_audit_s7_scores(ctx), qc_audit_s8_thresholds(ctx),
                   qc_audit_s9_classification(ctx), qc_audit_s10_recommendation(ctx))
  cmp <- if (is.null(bundle)) list(available = FALSE, fields = data.frame(field = character(), pipeline = character(), bundle = character(),
                                                                          status = character(), stringsAsFactors = FALSE),
                                   n_differences = NA_integer_, choice_pipeline = qc_audit_get(row, "strategic_choice"), choice_bundle = NA_character_)
  else qc_audit_bundle_compare(row, bundle, labels)
  all_links <- do.call(rbind, lapply(sections, function(s) if (nrow(s$links)) cbind(section = s$n, s$links) else NULL))
  gaps <- if (is.null(all_links)) character() else
    with(all_links[all_links$status %in% c("differs", QC_AUDIT_NOT_REPRO, "provenance_gap"), , drop = FALSE],
         paste0(section, ". ", link, ": ", status, ifelse(is.na(detail), "", paste0(" (", detail, ")"))))
  if (!isTRUE(cmp$available)) gaps <- c(gaps, "Shipped bundle comparison: no shipped bundle cell to compare")
  key <- list(iso3 = row$iso3, tech = row$tech, stage = row$supply_chain)
  list(
    schema = QC_AUDIT_SCHEMA, schema_version = 1L, case_id = meta$case_id %||% NA_character_, case_title = meta$case_title %||% NA_character_,
    key = key, country = qc_audit_get(row, "Country"), generated_at = meta$generated_at %||% NA_character_,
    inputs = list(pipeline_table = meta$pipeline_table %||% NA_character_, pipeline_release = meta$pipeline_release %||% NA_character_,
                  sp_config = meta$sp_config %||% NA_character_, bundle_created = cmp$bundle_created %||% NA_character_,
                  bundle_git_commit = cmp$bundle_git_commit %||% NA_character_),
    summary = list(n_links = if (is.null(all_links)) 0L else nrow(all_links),
                   reproduced = sum(all_links$status == "reproduced"), differs = sum(all_links$status == "differs"),
                   not_reproducible = sum(all_links$status == QC_AUDIT_NOT_REPRO), provenance_gaps = sum(all_links$status == "provenance_gap"),
                   recorded = sum(all_links$status == "recorded"),
                   choice_pipeline = cmp$choice_pipeline %||% NA_character_, choice_bundle = cmp$choice_bundle %||% NA_character_,
                   bundle_differences = cmp$n_differences),
    sections = sections, bundle_comparison = cmp, gaps = gaps
  )
}

# A file stem for a cell: ISO3_Tech_Stage with spaces as hyphens.
qc_audit_stem <- function(iso3, tech, stage) paste(iso3, gsub("[^A-Za-z0-9]+", "-", tech), stage, sep = "_")

# ---- rendering --------------------------------------------------------------------------------------------------
qc_audit_md_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", x))
}
qc_audit_md_table <- function(df, max_rows = 50) {
  if (is.null(df) || !nrow(df)) return("_(none)_")
  more <- nrow(df) - max_rows
  df <- utils::head(df, max_rows)
  h <- paste0("| ", paste(names(df), collapse = " | "), " |")
  s <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  b <- apply(df, 1, function(z) paste0("| ", paste(qc_audit_md_escape(z), collapse = " | "), " |"))
  c(h, s, b, if (more > 0) paste0("\n_", more, " more rows in the JSON._") else NULL)
}

# The audit as Markdown (numbered sections 1-10, then the bundle comparison and the gaps).
qc_audit_markdown <- function(audit) {
  k <- audit$key
  title <- paste0("# Case audit: ", if (!is.na(audit$case_id)) paste0(audit$case_id, " ") else "", audit$country, " / ", k$tech, " / ", k$stage)
  s <- audit$summary
  out <- c(title, "",
           paste0("Generated ", audit$generated_at, " by scripts/qc/build_case_audit.R from ", audit$inputs$pipeline_table,
                  " (", audit$inputs$pipeline_release, ") and ", audit$inputs$sp_config, ". Shipped posture bundle: created ",
                  audit$inputs$bundle_created %||% "n/a", ", git ", audit$inputs$bundle_git_commit %||% "n/a", "."), "",
           paste0("**Choice (pipeline): ", s$choice_pipeline %||% "n/a", ". Choice (shipped bundle): ", s$choice_bundle %||% "n/a", ".** ",
                  s$n_links, " links checked: ", s$reproduced, " reproduced, ", s$differs, " differ, ", s$not_reproducible, " ",
                  QC_AUDIT_NOT_REPRO, ", ", s$provenance_gaps, " provenance gaps, ", s$recorded, " recorded only. Bundle fields differing or missing: ",
                  if (is.na(s$bundle_differences)) "not compared" else s$bundle_differences, "."), "")
  if (!is.na(audit$case_title)) out <- c(out, paste0("Case: ", audit$case_title), "")
  for (sec in audit$sections) {
    out <- c(out, paste0("## ", sec$n, ". ", sec$title), "")
    if (nrow(sec$items)) out <- c(out, qc_audit_md_table(sec$items), "")
    for (tn in names(sec$tables)) {
      t <- sec$tables[[tn]]
      if (is.data.frame(t) && nrow(t)) out <- c(out, paste0("**", gsub("_", " ", tn), "**"), "", qc_audit_md_table(t, 25), "")
    }
    if (nrow(sec$links)) out <- c(out, "**Links**", "", qc_audit_md_table(sec$links), "")
  }
  cmp <- audit$bundle_comparison
  out <- c(out, "## Shipped bundle comparison", "")
  if (isTRUE(cmp$available)) {
    out <- c(out, paste0("Posture bundle created ", cmp$bundle_created, " (git ", cmp$bundle_git_commit, "), detail file ", cmp$detail_file, ". ",
                         cmp$n_differences, " of ", nrow(cmp$fields), " fields differ from the pipeline or are missing on one side. ",
                         "Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares."), "",
             qc_audit_md_table(cmp$fields[order(cmp$fields$status == "same"), , drop = FALSE], 80), "")
  } else out <- c(out, "No shipped bundle cell was given: not compared.", "")
  out <- c(out, "## Gaps: links not reproduced", "")
  out <- c(out, if (length(audit$gaps)) paste0("- ", audit$gaps) else "- none", "")
  out
}
