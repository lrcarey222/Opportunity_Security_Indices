# Group Memo for a fixed group of economies such as the Quad (docs/partnership_memos.md, "Group memos"). Reads the
# same inputs as scripts/35 plus the members' AIB strategic roles, the bilateral engine's summaries for every pair in
# the group (output/partnerships/<pair>/partnership_summary.json, from scripts/34) and the members' country strategy
# briefs (dashboard/public/data/country), applies R/partnerships/pm_group*.R and config/partnership_memos.yml
# (`groups`), and writes the tables and group_memo.md, then the PDF and Word files in the country briefs' format.
#
#   Rscript scripts/36_build_group_memo.R --group QUAD --processed-dir <stages>/data/processed \
#       --devpart <memos>/data/processed/development_partnerships/development_partnerships.rds
#
# Options as scripts/35 (--processed-dir, --aib-dir, --devpart, --reference-dir, --out-dir (output/groups/<id>),
# --pairs-dir (output/partnerships), --country-dir (dashboard/public/data/country), --tables-only, --no-pdf, --edge,
# --python, --font, --publish-dir); the published files are memo-<id>.<h8>.* with the entry under `groups` in
# <dir>/manifest.json. Run scripts/34 for every pair of the group first (--tables-only is enough).

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
id <- arg_value("--group", "QUAD")
gcfg <- cfg$groups[[id]]
if (is.null(gcfg)) stop("group not configured in config/partnership_memos.yml `groups`: ", id)
gcfg$id <- id
mem <- unlist(gcfg$members)
processed_dir <- abs_path(arg_value("--processed-dir", "data/processed"))
aib_dir <- abs_path(arg_value("--aib-dir", file.path(processed_dir, "outputs", "aib")))
ref_dir <- abs_path(arg_value("--reference-dir", "data/reference"))
devpart_path <- abs_path(arg_value("--devpart", file.path(processed_dir, "development_partnerships", "development_partnerships.rds")))
out_dir <- abs_path(arg_value("--out-dir", file.path("output", "groups", id)))
pairs_dir <- abs_path(arg_value("--pairs-dir", file.path("output", "partnerships")))
country_dir <- abs_path(arg_value("--country-dir", file.path("dashboard", "public", "data", "country")))
publish_dir <- arg_value("--publish-dir", NULL)
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
full_names <- names_iso
for (k in names(gcfg$short_names)) names_iso[[k]] <- gcfg$short_names[[k]]
rm(aib_in); invisible(gc())
spine <- rcsv(file.path(aib_dir, "aib_strategic_spine.csv"))
spine_base <- spine[spine$scenario == "baseline", , drop = FALSE]
nclass <- rcsv(file.path(aib_dir, "aib_node_classification.csv"))
dol <- pm_apply_share_corrections(rcsv(file.path(aib_dir, "aib_compact_division_of_labour.csv")), cfg$data_caveats)
croles <- rcsv(file.path(aib_dir, "aib_compact_country_roles.csv"))
sroles <- rcsv(file.path(aib_dir, "aib_country_strategic_roles.csv"))
compacts <- rcsv(file.path(aib_dir, "aib_strategic_compacts.csv"))
compacts <- compacts[compacts$scenario == cfg$inputs$aib_scenario, , drop = FALSE]
devpart <- if (file.exists(devpart_path)) as.data.frame(readRDS(devpart_path)) else { warning("no development-partnership table: ", devpart_path); NULL }
evdir <- file.path(ref_dir, "partnership_evidence")
ev <- rchr(file.path(evdir, "policy_evidence.csv"))
mech_bi <- rchr(file.path(evdir, "bilateral_mechanisms.csv"))
mech_pl <- rchr(file.path(evdir, "plurilateral_mechanisms.csv"))
# plurilateral bodies from their own file; the bilateral file adds the two-party channels among members (a row in both
# files keeps its plurilateral copy)
mech <- rbind(mech_pl, mech_bi[vapply(strsplit(mech_bi$pair, "\\|"), length, integer(1)) == 2 & !mech_bi$mechanism_id %in% mech_pl$mechanism_id, , drop = FALSE])
institutions <- rchr(file.path(evdir, "institutions.csv"))
agreements <- rcsv(file.path(ref_dir, "economic_security_agreements.csv"))
inst <- utils::read.csv(file.path(repo_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)

# the bilateral engine's summary for every pair of members (either order), and the published pair memos
pair_key <- function(a, b) { k <- names(cfg$pairs)[vapply(cfg$pairs, function(p) setequal(c(p$a, p$b), c(a, b)), logical(1))]; if (length(k)) k[1] else paste(a, b, sep = "_") }
cmb <- utils::combn(mem, 2)
pairs <- stats::setNames(lapply(seq_len(ncol(cmb)), function(j) { k <- pair_key(cmb[1, j], cmb[2, j]); p <- cfg$pairs[[k]]; if (is.null(p)) cmb[, j] else c(p$a, p$b) }),
                         vapply(seq_len(ncol(cmb)), function(j) pair_key(cmb[1, j], cmb[2, j]), character(1)))
sums <- lapply(names(pairs), function(k) { f <- file.path(pairs_dir, k, "partnership_summary.json"); if (file.exists(f)) jsonlite::read_json(f) else { warning("no pair summary (run scripts/34 --pair ", k, "): ", f); NULL } })
names(sums) <- names(pairs)
pman <- file.path(abs_path(if (is.null(publish_dir)) file.path("dashboard", "public", "data", "partnerships") else publish_dir), "manifest.json")
published <- if (file.exists(pman)) names(jsonlite::read_json(pman)$pairs) else character()

# each member's country strategy brief: the first sentence of its bottom line
briefs <- list()
cman <- file.path(country_dir, "manifest.json")
if (file.exists(cman)) {
  cm_json <- jsonlite::read_json(cman)
  for (cty in cm_json$countries) if (cty$iso3 %in% mem && !is.null(cty$memo) && identical(cty$memo$format, "brief") && !isTRUE(cty$memo$stale)) {
    f <- file.path(country_dir, cty$memo$json)
    if (!file.exists(f)) next
    bj <- jsonlite::read_json(f)
    bl <- Filter(function(s) identical(s$id, "bottom_line"), bj$sections)
    if (!length(bl)) next
    p1 <- Filter(function(b) identical(b$type, "p"), bl[[1]]$blocks)
    if (!length(p1)) next
    txt <- paste(vapply(p1[[1]]$inlines, function(z) as.character(z$v), ""), collapse = "")
    briefs[[cty$iso3]] <- sub("^(.*?[.!?])(\\s.*)?$", "\\1", txt, perl = TRUE)
  }
}

step("assessing the group")
frame <- pm_node_frame(nodes, spine_base, nclass, dol, cfg$inputs$coercive_suppliers)
cnames <- stats::setNames(compacts$strategic_compact_name, compacts$strategic_compact_id)
cnames <- cnames[!duplicated(names(cnames))]
frame_g <- pm_group_frame(frame, id, gcfg, cnames)
src <- unique(frame_g$source_compact)
members <- pm_group_members(frame_g, dol, croles, mem, names_iso, cfg)
prof <- pm_compact_profiles(sp, frame_g, dol, mem, cfg)
# a group spans every compact, so its economy-wide channels (cross_sector) and the cross-cutting compacts' count too
cross_cut <- names(cfg$compacts)[vapply(cfg$compacts, function(z) !is.null(z$steps), logical(1))]
cm <- pm_compact_mechanisms(mech, mem, id, c(src, "cross_sector", cross_cut))
# the node engine reads the members' own record: items whose parties are all members (a member alone included); an
# item that also names a non-member is about that other relationship (a Korean plant in the US, a Canadian refinery)
ev_parties <- strsplit(ev$countries, "|", fixed = TRUE)
in_group <- vapply(ev_parties, function(z) all(z %in% mem), logical(1))
# a coercive supplier's export controls bear on the members' supply wherever they are recorded (urgency)
coercive_ctrl <- ev$instrument %in% "export_control" & vapply(ev_parties, function(z) any(z %in% cfg$inputs$coercive_suppliers) && any(z %in% mem), logical(1))
ev_in <- ev[in_group | coercive_ctrl, , drop = FALSE]
na <- pm_compact_assess(prof$nodes, prof$cells, frame_g, flows, mem, cfg, ev_in, cm[cm$status %in% PM_ACTIVE_MECH, , drop = FALSE], inst, devpart, mem)
na$compact_order <- frame_g$compact_order[match(na$node_id, frame_g$node_id)]
lanes <- pm_compact_lanes(na, prof$nodes, flows)
public <- c(institutions$abbreviation, institutions$institution, unname(names_iso[mem]), unlist(cfg$evidence$public_actor_prefixes))
trade_tbl <- pm_compact_trade(na, cfg, names_iso)
inv_tbl <- pm_compact_investment(na, cfg, ev_in, inst, names_iso, public)
third <- pm_compact_third(devpart, frame_g, mem, cfg, names_iso)
# members with a trade agreement in force between them (market-access deals hold its terms)
fta_m <- cm[cm$mechanism_type %in% "trade_agreement" & cm$status %in% PM_ACTIVE_MECH & lengths(strsplit(cm$pair, "|", fixed = TRUE)) == 2, , drop = FALSE]
fta_pairs <- vapply(strsplit(fta_m$pair, "|", fixed = TRUE), function(z) paste(sort(z), collapse = "|"), character(1))
deals <- pm_compact_deals(na, inst, institutions, ev_in, cfg, names_iso, public, fta_pairs)
bet_compacts <- unique(na$source_compact[na$priority_tier == 1])
gov <- pm_group_governance(cm, mem, cfg, gcfg$mechanism_pattern, bet_compacts)
# member-on-member trade measures in force at a node (from the whole evidence layer: a trade-remedy order often names
# third countries too): tariffs, export controls and disputes naming two or more members, matched to the node or, for a
# measure with no node, to its compact and stage
fr_ev <- ev[(ev$instrument %in% c("tariff", "export_control") | ev$item_type %in% "dispute") & ev$status %in% c("in_force", "operating", "signed") &
              vapply(ev_parties, function(z) sum(mem %in% z) >= 2, logical(1)), , drop = FALSE]
friction <- stats::setNames(lapply(seq_len(nrow(na)), function(i) {
  hit <- (nzchar(fr_ev$aib_node) & fr_ev$aib_node == na$node_id[i]) |
    (!nzchar(fr_ev$aib_node) & fr_ev$compact %in% na$source_compact[i] & fr_ev$stage == na$stage[i])
  fr_ev$evidence_id[hit] }), na$node_id)
nonp <- pm_compact_non_priorities(na, gov, cfg, names_iso, ev_in)
# the compact engine's wording, read for the group ("a compact preference" -> "a Quad preference")
gword <- function(x) gsub("\\bcompact\\b", gcfg$short, gsub("dock the compact in it", sprintf("dock the %s agenda in it", gcfg$short), x, fixed = TRUE))
if (nrow(nonp)) nonp$text <- gword(nonp$text)
if (nrow(trade_tbl)) trade_tbl$action <- gword(trade_tbl$action)
if (nrow(deals)) deals$deal <- gword(deals$deal)
# product-definition caveats: AIB mapping notes that change how a node reads, and config data caveats for members
map_notes <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_compacts.yml"))$mapping_notes
node_notes <- map_notes[vapply(map_notes, function(z) grepl(PM_CAVEAT_RE, z), logical(1))]
data_notes <- list()
for (dc in cfg$data_caveats) if (dc$iso3 %in% mem) data_notes[[dc$node]] <- if (is.null(data_notes[[dc$node]])) dc$note else paste(data_notes[[dc$node]], dc$note)
ev_group <- pm_group_evidence_ids(ev, mem)
pri <- pm_group_priorities(sp, mem)
pmat <- pm_group_partner_matrix(pri, mem)
labels <- c(stats::setNames(frame$node_label, frame$node_id),
            stats::setNames(sprintf("%s %s", sp$tech, tolower(sp$supply_chain)), paste(sp$tech, sp$supply_chain, sep = "|")))
labels <- labels[!duplicated(names(labels))]
roles <- pm_group_roles(sroles, croles, mem, compacts, labels)
dyads <- pm_group_dyads(pairs, sums, labels, cnames, published)
memo <- pm_group_memo_sections(na = na, prof = prof$nodes, cells = prof$cells, members = members, lanes = lanes, trade_tbl = trade_tbl,
                               inv_tbl = inv_tbl, third = third, deals = deals, gov = gov, nonp = nonp, ev = ev, ev_group = ev_group,
                               agreements = agreements, pri = pri, pmat = pmat, roles = roles, dyads = dyads, briefs = briefs,
                               gcfg = gcfg, cfg = cfg, names = names_iso, institutions = institutions,
                               node_notes = node_notes, data_notes = data_notes, full_names = full_names, friction = friction)
reg <- pm_compact_evidence_register(memo$recommendations, na, ev_in, id)

step("writing tables to ", out_dir)
flat <- function(x) { for (k in names(x)) if (is.list(x[[k]])) x[[k]] <- vapply(x[[k]], function(z) paste(z, collapse = ";"), character(1)); x }
wcsv <- function(x, f) utils::write.csv(flat(x), file.path(out_dir, f), row.names = FALSE, na = "")
wcsv(members, "group_members.csv")
wcsv(pri, "country_priorities.csv")
wcsv(data.frame(member = rownames(pmat), as.data.frame.matrix(pmat), check.names = FALSE), "partner_matrix.csv")
wcsv(roles$strategic, "aib_strategic_roles.csv")
wcsv(roles$compact, "aib_compact_roles.csv")
wcsv(dyads, "group_pairs.csv")
wcsv(prof$nodes, "group_member_profiles.csv")
wcsv(na[, setdiff(names(na), "evidence_ids")], "group_nodes.csv")
wcsv(lanes, "group_lanes.csv")
wcsv(trade_tbl, "trade_priorities.csv")
wcsv(inv_tbl, "investment_priorities.csv")
wcsv(third, "third_country_opportunities.csv")
wcsv(deals, "deal_pipeline.csv")
wcsv(nonp, "non_priorities.csv")
wcsv(if (is.null(gov$mechanisms)) data.frame() else gov$mechanisms, "group_mechanisms.csv")
wcsv(ev[ev$evidence_id %in% ev_group, , drop = FALSE], "group_evidence.csv")
wcsv(reg, "evidence_register.csv")
summary_obj <- list(
  group = id, title = gcfg$title, members = I(mem), source_compacts = I(src),
  osi_vintage = list(strategic_position = "scripts/26 run on the six-stage table", trade_year = "2024", aib_scenario = cfg$inputs$aib_scenario),
  external_evidence_vintage = list(access_date = max(ev$access_date), n_items = length(ev_group), verification = as.list(table(ev$verification[ev$evidence_id %in% ev_group]))),
  state_counts = as.list(table(na$relationship_state)), tier_counts = as.list(table(na$priority_label)),
  group_bets = I(memo$group_bets), pairs = lapply(stats::setNames(seq_len(nrow(dyads)), dyads$pair), function(i) list(n_joint_bets = dyads$n_joint_bets[i], framework = dyads$mea[i])),
  governance = gov[c("option", "host_mechanism", "gaps")], non_priorities = I(nonp$text))
jsonlite::write_json(summary_obj, file.path(out_dir, "group_summary.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
if (!tables_only) {
  writeLines(memo$markdown, file.path(out_dir, "group_memo.md"), useBytes = TRUE)
  step("memo: ", file.path(out_dir, "group_memo.md"))
}

if (!tables_only && !("--no-pdf" %in% args)) {
  dcfg <- utils::modifyList(cfg$document, cfg$group_document)
  pm_print_publish(memo$markdown, memo$boxes, ev, list(title = gcfg$title), dcfg, id, "groups",
                   list(group = id, members = I(mem)),
                   list(args = args, processed_dir = processed_dir, out_dir = out_dir, repo_root = repo_root, step = step, stem = "group_memo"))
}
step("done: ", nrow(na), " nodes; ", length(mem), " members; ", sum(na$priority_tier == 1), " group bets; governance ", gov$option)
