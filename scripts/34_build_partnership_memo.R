# Partnership Memo for one country pair (docs/partnership_memos.md). Reads the six-stage strategic-position
# table (scripts/26), the AIB strategic layer (scripts/31-33), the bilateral import table (scripts/15), the
# development-partnership pairs (scripts/93) and the curated external evidence layer
# (data/reference/partnership_evidence/), applies the rules in R/partnerships/ and config/partnership_memos.yml,
# and writes the machine-readable tables and partnership_memo.md.
#
#   Rscript scripts/34_build_partnership_memo.R --pair USA_JPN
#   Rscript scripts/34_build_partnership_memo.R --pair USA_JPN --processed-dir <stages>/data/processed \
#       --devpart <memos>/data/processed/development_partnerships/development_partnerships.rds
#
# Options (repo-relative defaults): --pair (a key of config/partnership_memos.yml `pairs`), --processed-dir
# (data/processed), --aib-dir (<processed>/outputs/aib), --devpart (<processed>/development_partnerships/
# development_partnerships.rds; optional: without it Section 6 says so), --reference-dir (data/reference),
# --out-dir (output/partnerships/<pair>), --tables-only (skip the memo), --no-pdf (Markdown only), --edge <msedge.exe>,
# --python <python.exe>, --font <roboto .woff2>, --publish-dir <dir> (also write the dashboard set
# memo-<PAIR>.<h8>.{json,pdf,docx,md} and its entry in <dir>/manifest.json, the country briefs' convention).
#
# The print document uses the country strategy briefs' renderer (R/dashboard/country/country_brief_doc.R,
# scripts/utils/country_memos.R, scripts/utils/country_memo_docx.py): same print style, page geometry, Word
# builder and render checks. It needs Edge or Chrome (PDF) and python with python-docx and pypdf (Word, checks).

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
            "scripts/utils/country_memos.R", "scripts/utils/partnership_memo_print.R",
            list.files(file.path(repo_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE)))
  source(if (file.exists(f)) f else file.path(repo_root, f))
suppressPackageStartupMessages(library(dplyr))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_memos.yml"))
pair <- arg_value("--pair", "USA_JPN")
pair_cfg <- cfg$pairs[[pair]]
if (is.null(pair_cfg)) stop("pair not configured in config/partnership_memos.yml: ", pair)
# a bloc side (config `blocs`): its members are merged into one economy (R/partnerships/pm_bloc.R)
bloc_code <- intersect(c(pair_cfg$b, pair_cfg$a), names(cfg$blocs))[1]
members <- if (is.na(bloc_code)) character() else unlist(cfg$blocs[[bloc_code]]$members)
processed_dir <- abs_path(arg_value("--processed-dir", "data/processed"))
aib_dir <- abs_path(arg_value("--aib-dir", file.path(processed_dir, "outputs", "aib")))
ref_dir <- abs_path(arg_value("--reference-dir", "data/reference"))
devpart_path <- abs_path(arg_value("--devpart", file.path(processed_dir, "development_partnerships", "development_partnerships.rds")))
out_dir <- abs_path(arg_value("--out-dir", file.path("output", "partnerships", pair)))
tables_only <- "--tables-only" %in% args
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
a <- pair_cfg$a; b <- pair_cfg$b
rcsv <- function(p) utils::read.csv(need_file(p), stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))

step("reading inputs for ", pair)
sp <- readRDS(need_file(file.path(processed_dir, "outputs", "strategic_position_tbl.rds")))
sp <- as.data.frame(sp[sp$iso3 %in% c(a, b, members), , drop = FALSE])
aib_in <- readRDS(need_file(file.path(aib_dir, "aib_inputs.rds")))
nodes <- as.data.frame(aib_in$nodes)[, c("node_id", "supply_chain", "score_cells")]
if (length(members)) {
  flows <- as.data.frame(aib_in$flows); production <- as.data.frame(aib_in$production)
  node_rca <- as.data.frame(aib_in$exporter_metrics)[, c("node_id", "exporter_iso", "exports", "export_rca")]
  cnames <- stats::setNames(aib_in$country_names$country, aib_in$country_names$iso3)
}
rm(aib_in); invisible(gc())
spine <- rcsv(file.path(aib_dir, "aib_strategic_spine.csv"))
spine_base <- spine[spine$scenario == "baseline", , drop = FALSE]
nclass <- rcsv(file.path(aib_dir, "aib_node_classification.csv"))
dol <- pm_apply_share_corrections(rcsv(file.path(aib_dir, "aib_compact_division_of_labour.csv")), cfg$data_caveats)
croles <- rcsv(file.path(aib_dir, "aib_compact_country_roles.csv"))
sroles <- rcsv(file.path(aib_dir, "aib_country_strategic_roles.csv"))
compacts <- rcsv(file.path(aib_dir, "aib_strategic_compacts.csv"))
compacts <- compacts[compacts$scenario == cfg$inputs$aib_scenario, , drop = FALSE]
ipairs <- rcsv(file.path(aib_dir, "allied_investment_pairs.csv"))
ipairs <- ipairs[ipairs$investor_iso %in% c(a, b, members) & ipairs$host_iso %in% c(a, b, members) & !is.na(ipairs$host_rank) & ipairs$host_rank <= 5, c("node_id", "investor_iso", "host_iso", "host_rank")]
imports <- as.data.frame(readRDS(need_file(file.path(processed_dir, "import_partner_cells_tbl.rds"))))
imports <- imports[imports$reporter_iso %in% c(a, b, members), , drop = FALSE]
devpart <- if (file.exists(devpart_path)) as.data.frame(readRDS(devpart_path)) else { warning("no development-partnership table: ", devpart_path); NULL }
evdir <- file.path(ref_dir, "partnership_evidence")
ev <- utils::read.csv(need_file(file.path(evdir, "policy_evidence.csv")), stringsAsFactors = FALSE, colClasses = "character")
mech <- utils::read.csv(need_file(file.path(evdir, "bilateral_mechanisms.csv")), stringsAsFactors = FALSE, colClasses = "character")
mech <- mech[vapply(strsplit(mech$pair, "\\|"), function(z) all(c(a, b) %in% z), logical(1)), , drop = FALSE]
# a plurilateral platform of more than three economies is not a bilateral mechanism: it neither covers a compact
# for the MEA test nor hosts one (the compact memos read it from plurilateral_mechanisms.csv; 2026-11-16)
mech <- mech[!(mech$mechanism_type %in% "plurilateral" & lengths(strsplit(mech$pair, "\\|")) > 3), , drop = FALSE]
institutions <- utils::read.csv(need_file(file.path(evdir, "institutions.csv")), stringsAsFactors = FALSE, colClasses = "character")
agreements <- rcsv(file.path(ref_dir, "economic_security_agreements.csv"))
inst <- utils::read.csv(file.path(repo_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)

if (length(members)) {
  step("merging the ", length(members), " members of ", bloc_code, " into one economy")
  roles_cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_layer.yml"))$roles
  size <- tapply(suppressWarnings(as.numeric(sp$imports_usd[sp$iso3 %in% members])), sp$iso3[sp$iso3 %in% members], sum, na.rm = TRUE)
  dol <- pm_bloc_dol(dol, flows, production, node_rca, members, bloc_code, roles_cfg)
  spine_base <- pm_bloc_spine(spine_base, dol)
  nclass <- pm_bloc_nclass(nclass, members, bloc_code)
  sp <- pm_bloc_sp(sp, imports, members, bloc_code, cfg$blocs[[bloc_code]]$name, cfg$inputs$coercive_suppliers, china_iso = "CHN")
  sp <- sp[sp$iso3 %in% c(a, b), , drop = FALSE]
  imports <- pm_bloc_imports(imports, members, bloc_code)
  devpart <- pm_bloc_devpart(devpart, members, bloc_code)
  croles <- pm_bloc_croles(croles, members, bloc_code, cnames)
  sroles <- pm_bloc_sroles(sroles, members, bloc_code, size)
  ipairs <- pm_bloc_invest_pairs(ipairs, members, bloc_code)
  ev <- pm_bloc_evidence(ev, members, bloc_code)
  other <- setdiff(c(a, b), bloc_code)
  bname <- if (bloc_code == b) pair_cfg$b_name else pair_cfg$a_name
  pair_cfg$note <- pm_bloc_note(paste0(toupper(substr(bname, 1, 1)), substr(bname, 2, nchar(bname))), if (bloc_code == b) pair_cfg$b_short else pair_cfg$a_short,
                                if (bloc_code == b) pair_cfg$b_ref else pair_cfg$a_ref, length(members),
                                sprintf("%s's", if (other == a) pair_cfg$a_ref else pair_cfg$b_ref))
  rm(flows); invisible(gc())
}

step("assessing the pair")
frame <- pm_node_frame(nodes, spine_base, nclass, dol, cfg$inputs$coercive_suppliers)
res <- pm_assess_pair(list(sp = sp, frame = frame, dol = dol, trade = imports, ev = ev, mech = mech, inst = inst,
                           devpart = devpart, invest_pairs = ipairs, croles = croles), pair_cfg, cfg)
d <- res$dyad
names_short <- list(a_short = pair_cfg$a_short, b_short = pair_cfg$b_short)
cc <- pm_compact_candidates(d, compacts, croles, mech, cfg, a, b, ev)
sel <- cc$strategic_compact_id[cc$recommended]
arch <- do.call(rbind, lapply(cc$strategic_compact_id, function(cm) pm_compact_architecture(d, cm, res$links)))
fta <- any(mech$mechanism_type %in% "trade_agreement" & mech$status %in% PM_ACTIVE_MECH & vapply(strsplit(mech$pair, "\\|"), length, integer(1)) <= 3)
trade_tbl <- pm_trade_agenda(d, cfg, a, b, names_short, fta)
public_names <- pm_public_names(institutions, pair_cfg, cfg)
inv_tbl <- pm_investment_agenda(d, cfg, ev, inst, a, b, public_names)
third <- pm_third_country(res$devpart, sroles, d, sel, cfg, a, b)
deals <- pm_deal_pipeline(d, inst, institutions, ev, cfg, a, b, names_short, public_names, fta)
mea <- pm_mea(sel, mech, cfg)
nonp <- pm_non_priorities(d, mea, cfg, names_short, a, b, ev)
# product-definition caveats: AIB mapping notes that change how a node's numbers read, and config data_caveats for
# this pair's countries (a bloc side counts its members)
map_notes <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_compacts.yml"))$mapping_notes
node_notes <- map_notes[vapply(map_notes, function(z) grepl(PM_CAVEAT_RE, z), logical(1))]
data_notes <- list()
for (dc in cfg$data_caveats) if (dc$iso3 %in% c(a, b, members))
  data_notes[[dc$node]] <- if (is.null(data_notes[[dc$node]])) dc$note else paste(data_notes[[dc$node]], dc$note)
memo <- pm_memo_sections(d = d, prof = res$profiles, cc = cc, arch = arch, trade_tbl = trade_tbl, inv_tbl = inv_tbl,
                         third = third, deals = deals, mea = mea, nonp = nonp, ev = ev, mech = mech, sroles = sroles,
                         agreements = agreements, compacts = compacts, pair_cfg = pair_cfg, cfg = cfg, institutions = institutions,
                         node_notes = node_notes, data_notes = data_notes)
recs <- memo$recommendations
reg <- pm_evidence_register(recs, d, ev, mech, a, b)

step("writing tables to ", out_dir)
flat <- function(x) { for (k in names(x)) if (is.list(x[[k]])) x[[k]] <- vapply(x[[k]], function(z) paste(z, collapse = ";"), character(1)); x }
wcsv <- function(x, f) utils::write.csv(flat(x), file.path(out_dir, f), row.names = FALSE, na = "")
wcsv(res$profiles, "country_profiles.csv")
dyad_cols <- c("priority_rank", "priority_tier", "priority_label", "unit_type", "unit_id", "tech", "stage", "rollup", "node_label", "compact",
               "compacts_of_cell", "position_band_a", "position_band_b", "competitive_position_a", "competitive_position_b",
               "world_share_a", "world_share_b", "need_band_a", "need_band_b", "strategic_need_a", "strategic_need_b",
               "relationship_state", "state_label", "supplier", "buyer", "state_reason", "treatment", "constraint_type",
               "constraint_secondary", "market_failure", "main_instrument", "instruments", "chokepoint", "coercive_top_iso3",
               "coercive_world_share", "allied_short", "aib_class",
               "competitive_position_a", "competitive_position_b", "strategic_need_a_score", "strategic_need_b_score",
               "policy_priority_a", "policy_priority_b", "policy_momentum_a", "policy_momentum_b",
               "bilateral_complementarity", "shared_vulnerability", "trade_fit", "investment_fit", "third_country_fit", "compact_fit",
               "strategic_importance", "urgency", "tractability", "additionality", "additionality_label", "market_failure_score",
               "existing_commitment", "market_already_working", "execution_speed", "interdependency", "evidence_quality",
               "low_evidence", "evidence_basis", "a_imports_from_b_share", "b_imports_from_a_share", "bilateral_trade_basis",
               "third_country_n", "aib_third_hosts", "evidence_id_list")
dy <- d[, unique(dyad_cols)]
names(dy) <- sub("^policy_priority_(a|b)$", "policy_alignment_\\1", names(dy))
wcsv(dy, "dyad_complementarity.csv")
wcsv(trade_tbl, "trade_priorities.csv")
wcsv(inv_tbl, "investment_priorities.csv")
wcsv(third, "third_country_opportunities.csv")
wcsv(merge(cc, aggregate(stage ~ strategic_compact_id, arch, length), by = "strategic_compact_id", all.x = TRUE)[, setdiff(names(cc), character())], "compact_candidates.csv")
wcsv(arch, "compact_architecture.csv")
wcsv(deals, "deal_pipeline.csv")
wcsv(nonp, "non_priorities.csv")
wcsv(reg, "evidence_register.csv")
summary_obj <- list(
  pair = pair, country_a = a, country_b = b, title = pair_cfg$title,
  osi_vintage = list(strategic_position = "scripts/26 run on the six-stage table", trade_year = "2024", aib_scenario = cfg$inputs$aib_scenario),
  external_evidence_vintage = list(access_date = max(ev$access_date), n_items = nrow(ev), verification = as.list(table(ev$verification))),
  state_counts = as.list(table(d$relationship_state)), tier_counts = as.list(table(d$priority_label)),
  joint_bets = memo$joint_bets, recommended_compacts = sel, mea = mea[c("option", "host_mechanism", "coverage_share", "gaps")],
  non_priorities = nonp$text)
jsonlite::write_json(summary_obj, file.path(out_dir, "partnership_summary.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
if (!tables_only) {
  writeLines(memo$markdown, file.path(out_dir, "partnership_memo.md"), useBytes = TRUE)
  step("memo: ", file.path(out_dir, "partnership_memo.md"))
}

# ---- Print document: PDF and Word in the country briefs' format ------------------------------------------

if (!tables_only && !("--no-pdf" %in% args))
  pm_print_publish(memo$markdown, memo$boxes, ev, pair_cfg, cfg$document, pair, "pairs",
                   list(pair = pair, country_a = a, country_b = b),
                   list(args = args, processed_dir = processed_dir, out_dir = out_dir, repo_root = repo_root, step = step))
step("done: ", nrow(d), " units; ", sum(d$priority_tier == 1), " joint bets; ", length(sel), " compacts; MEA ", mea$option)
