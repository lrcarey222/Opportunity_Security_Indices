# Compact Memo for one Strategic Sectoral Compact or a cross-cutting compact (docs/partnership_memos.md, "Compact
# memos"). Reads the same inputs as scripts/34 (strategic position, AIB strategic layer, node flows, development
# partnerships, the curated evidence layer and plurilateral_mechanisms.csv), applies R/partnerships/pm_compact*.R
# and config/partnership_memos.yml (`compacts`, `compact_memo`), and writes the tables and compact_memo.md, then the
# PDF and Word files in the country briefs' format.
#
#   Rscript scripts/35_build_compact_memo.R --compact SC-SOLAR --processed-dir <stages>/data/processed \
#       --devpart <memos>/data/processed/development_partnerships/development_partnerships.rds
#
# Options as scripts/34 (--processed-dir, --aib-dir, --devpart, --reference-dir, --out-dir (output/compacts/<id>),
# --tables-only, --no-pdf, --edge, --python, --font, --publish-dir); the published files are memo-<id>.<h8>.* with
# the entry under `compacts` in <dir>/manifest.json.

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))
    candidate_starts <- unique(c(candidate_starts, getwd()))
    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[[i + 1]] }
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
need_file <- function(p) { if (!file.exists(p)) stop("input not found: ", p); p }

for (f in c("R/utils/supply_chain_stages.R", "R/dashboard/country/country_util.R", "R/dashboard/country/country_memo.R",
            "R/dashboard/country/country_memo_lint.R", "R/dashboard/country/country_brief.R", "R/dashboard/country/country_brief_doc.R",
            "scripts/utils/country_memos.R", "scripts/utils/git_stamp.R", "scripts/utils/partnership_memo_print.R",
            list.files(file.path(repo_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE)))
  source(if (file.exists(f)) f else file.path(repo_root, f))
suppressPackageStartupMessages(library(dplyr))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_memos.yml"))
id <- arg_value("--compact", "SC-SOLAR")
ccfg <- cfg$compacts[[id]]
if (is.null(ccfg)) stop("compact not configured in config/partnership_memos.yml `compacts`: ", id)
ccfg$id <- id
cross_cutting <- !is.null(ccfg$steps)
processed_dir <- abs_path(arg_value("--processed-dir", "data/processed"))
aib_dir <- abs_path(arg_value("--aib-dir", file.path(processed_dir, "outputs", "aib")))
ref_dir <- abs_path(arg_value("--reference-dir", "data/reference"))
devpart_path <- abs_path(arg_value("--devpart", file.path(processed_dir, "development_partnerships", "development_partnerships.rds")))
out_dir <- abs_path(arg_value("--out-dir", file.path("output", "compacts", id)))
tables_only <- "--tables-only" %in% args
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
rcsv <- function(p) utils::read.csv(need_file(p), stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
rchr <- function(p) utils::read.csv(need_file(p), stringsAsFactors = FALSE, colClasses = "character")

step("reading inputs for ", id)
sp <- as.data.frame(readRDS(need_file(file.path(processed_dir, "outputs", "strategic_position_tbl.rds"))))
aib_in <- readRDS(need_file(file.path(aib_dir, "aib_inputs.rds")))
nodes <- as.data.frame(aib_in$nodes)[, c("node_id", "supply_chain", "score_cells")]
flows <- as.data.frame(aib_in$flows)[, c("node_id", "exporter_iso", "importer_iso", "value")]
names_iso <- stats::setNames(aib_in$country_names$country, aib_in$country_names$iso3)
for (b in names(cfg$blocs)) names_iso[[b]] <- cfg$blocs[[b]]$name
for (k in names(cfg$display_names)) names_iso[[k]] <- cfg$display_names[[k]]
rm(aib_in); invisible(gc())
spine <- rcsv(file.path(aib_dir, "aib_strategic_spine.csv"))
spine_base <- spine[spine$scenario == "baseline", , drop = FALSE]
nclass <- rcsv(file.path(aib_dir, "aib_node_classification.csv"))
dol <- pm_apply_share_corrections(rcsv(file.path(aib_dir, "aib_compact_division_of_labour.csv")), cfg$data_caveats)
croles <- rcsv(file.path(aib_dir, "aib_compact_country_roles.csv"))
compacts <- rcsv(file.path(aib_dir, "aib_strategic_compacts.csv"))
compacts <- compacts[compacts$scenario == cfg$inputs$aib_scenario, , drop = FALSE]
devpart <- if (file.exists(devpart_path)) as.data.frame(readRDS(devpart_path)) else { warning("no development-partnership table: ", devpart_path); NULL }
evdir <- file.path(ref_dir, "partnership_evidence")
ev <- rchr(file.path(evdir, "policy_evidence.csv"))
mech_bi <- rchr(file.path(evdir, "bilateral_mechanisms.csv"))
mech_pl <- rchr(file.path(evdir, "plurilateral_mechanisms.csv"))
# plurilateral bodies from their own file; the bilateral file adds the two-party channels among members
mech <- rbind(mech_pl, mech_bi[vapply(strsplit(mech_bi$pair, "\\|"), length, integer(1)) == 2, , drop = FALSE])
institutions <- rchr(file.path(evdir, "institutions.csv"))
agreements <- rcsv(file.path(ref_dir, "economic_security_agreements.csv"))
inst <- utils::read.csv(file.path(repo_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)

step("assessing the compact")
frame <- pm_node_frame(nodes, spine_base, nclass, dol, cfg$inputs$coercive_suppliers)
frame_c <- pm_compact_frame(frame, id, ccfg)
src <- unique(frame_c$source_compact)
aib_core <- unique(unlist(strsplit(paste(compacts$core_countries[compacts$strategic_compact_id %in% src], collapse = " "), "\\s+")))
members <- pm_compact_members(frame_c, dol, croles, if (cross_cutting) character() else aib_core, names_iso, cfg)
if (cross_cutting) members$core <- seq_len(nrow(members)) <= (if (is.null(ccfg$n_core)) 8 else ccfg$n_core)
core <- members$iso3[members$core]
# a bloc whose member is a compact member is a party to the compact's mechanisms and evidence
parties <- unique(c(members$iso3, names(cfg$blocs)[vapply(cfg$blocs, function(b) any(unlist(b$members) %in% members$iso3), logical(1))]))
prof <- pm_compact_profiles(sp, frame_c, dol, members$iso3, cfg)
# an AIB compact with critical-mineral nodes also counts the minerals compact's mechanisms (flagged partial_scope)
up_id <- cfg$compact_memo$upstream_compact
up_nodes <- if (is.null(up_id) || is.null(cfg$compacts[[up_id]]$steps)) character() else unlist(lapply(cfg$compacts[[up_id]]$steps, function(z) unlist(z$nodes)))
upstream_scope <- if (!cross_cutting && any(frame_c$node_id %in% up_nodes)) up_id else character()
cm <- pm_compact_mechanisms(mech, parties, id, src, if (cross_cutting) ccfg$mechanism_name_pattern else NULL, upstream_scope)
na <- pm_compact_assess(prof$nodes, prof$cells, frame_c, flows, members$iso3, cfg, ev, cm[cm$status %in% PM_ACTIVE_MECH, , drop = FALSE], inst, devpart, parties)
lanes <- pm_compact_lanes(na, prof$nodes, flows)
public <- c(institutions$abbreviation, institutions$institution, unname(names_iso[parties]), unlist(cfg$evidence$public_actor_prefixes))
trade_tbl <- pm_compact_trade(na, cfg, names_iso)
inv_tbl <- pm_compact_investment(na, cfg, ev, inst, names_iso, public)
third <- pm_compact_third(devpart, frame_c, members$iso3, cfg, names_iso)
deals <- pm_compact_deals(na, inst, institutions, ev, cfg, names_iso, public)
gov <- pm_compact_governance(cm, core, cfg)
nonp <- pm_compact_non_priorities(na, gov, cfg, names_iso, ev)
ev_ids <- pm_compact_evidence_ids(frame_c, ev, parties, id, cross_cutting)
meta_row <- compacts[compacts$strategic_compact_id %in% src, , drop = FALSE]
aib_meta <- if (cross_cutting) list(purpose = ccfg$purpose) else
  list(purpose = meta_row$purpose[1], mission_statement = meta_row$mission_statement[1], headline_result = meta_row$headline_result[1],
       workstreams = if (!is.na(meta_row$active_workstreams[1]) && nzchar(meta_row$active_workstreams[1])) trimws(strsplit(meta_row$active_workstreams[1], ";")[[1]]) else character())
memo <- pm_compact_memo_sections(na = na, prof = prof$nodes, cells = prof$cells, members = members, lanes = lanes, trade_tbl = trade_tbl,
                                 inv_tbl = inv_tbl, third = third, deals = deals, gov = gov, nonp = nonp, ev = ev, ev_ids = ev_ids,
                                 agreements = agreements, aib_meta = aib_meta, ccfg = ccfg, cfg = cfg, names = names_iso,
                                 institutions = institutions, parties = parties,
                                 node_notes = yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_compacts.yml"))$mapping_notes)
reg <- pm_compact_evidence_register(memo$recommendations, na, ev, id)

step("writing tables to ", out_dir)
flat <- function(x) { for (k in names(x)) if (is.list(x[[k]])) x[[k]] <- vapply(x[[k]], function(z) paste(z, collapse = ";"), character(1)); x }
wcsv <- function(x, f) utils::write.csv(flat(x), file.path(out_dir, f), row.names = FALSE, na = "")
wcsv(members, "compact_members.csv")
wcsv(prof$nodes, "compact_member_profiles.csv")
wcsv(na[, setdiff(names(na), "evidence_ids")], "compact_nodes.csv")
wcsv(lanes, "compact_lanes.csv")
wcsv(trade_tbl, "trade_priorities.csv")
wcsv(inv_tbl, "investment_priorities.csv")
wcsv(third, "third_country_opportunities.csv")
wcsv(deals, "deal_pipeline.csv")
wcsv(nonp, "non_priorities.csv")
wcsv(if (is.null(cm)) data.frame() else cm, "compact_mechanisms.csv")
wcsv(ev[ev$evidence_id %in% ev_ids, , drop = FALSE], "compact_evidence.csv")
wcsv(reg, "evidence_register.csv")
summary_obj <- list(
  compact = id, title = ccfg$title, cross_cutting = cross_cutting, source_compacts = I(src),
  osi_vintage = list(strategic_position = "scripts/26 run on the six-stage table", trade_year = "2024", aib_scenario = cfg$inputs$aib_scenario),
  external_evidence_vintage = list(access_date = max(ev$access_date), n_items = length(ev_ids), verification = as.list(table(ev$verification[ev$evidence_id %in% ev_ids]))),
  members = I(members$iso3), core = I(core), state_counts = as.list(table(na$relationship_state)), tier_counts = as.list(table(na$priority_label)),
  compact_bets = I(memo$compact_bets), governance = gov[c("option", "host_mechanism", "gaps")], non_priorities = I(nonp$text))
jsonlite::write_json(summary_obj, file.path(out_dir, "compact_summary.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
if (!tables_only) {
  writeLines(memo$markdown, file.path(out_dir, "compact_memo.md"), useBytes = TRUE)
  step("memo: ", file.path(out_dir, "compact_memo.md"))
}

if (!tables_only && !("--no-pdf" %in% args)) {
  dcfg <- utils::modifyList(cfg$document, cfg$compact_document)
  pm_print_publish(memo$markdown, memo$boxes, ev, list(title = ccfg$title), dcfg, id, "compacts",
                   list(compact = id, members = I(members$iso3)),
                   list(args = args, processed_dir = processed_dir, out_dir = out_dir, repo_root = repo_root, step = step, stem = "compact_memo"))
}
step("done: ", nrow(na), " nodes; ", nrow(members), " members; ", sum(na$priority_tier == 1), " compact bets; governance ", gov$option)
