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
            "scripts/utils/country_memos.R", list.files(file.path(repo_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE)))
  source(if (file.exists(f)) f else file.path(repo_root, f))
suppressPackageStartupMessages(library(dplyr))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_memos.yml"))
pair <- arg_value("--pair", "USA_JPN")
pair_cfg <- cfg$pairs[[pair]]
if (is.null(pair_cfg)) stop("pair not configured in config/partnership_memos.yml: ", pair)
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
sp <- as.data.frame(sp[sp$iso3 %in% c(a, b), , drop = FALSE])
aib_in <- readRDS(need_file(file.path(aib_dir, "aib_inputs.rds")))
nodes <- as.data.frame(aib_in$nodes)[, c("node_id", "supply_chain", "score_cells")]
rm(aib_in); invisible(gc())
spine <- rcsv(file.path(aib_dir, "aib_strategic_spine.csv"))
spine_base <- spine[spine$scenario == "baseline", , drop = FALSE]
nclass <- rcsv(file.path(aib_dir, "aib_node_classification.csv"))
dol <- rcsv(file.path(aib_dir, "aib_compact_division_of_labour.csv"))
croles <- rcsv(file.path(aib_dir, "aib_compact_country_roles.csv"))
sroles <- rcsv(file.path(aib_dir, "aib_country_strategic_roles.csv"))
compacts <- rcsv(file.path(aib_dir, "aib_strategic_compacts.csv"))
compacts <- compacts[compacts$scenario == cfg$inputs$aib_scenario, , drop = FALSE]
ipairs <- rcsv(file.path(aib_dir, "allied_investment_pairs.csv"))
ipairs <- ipairs[ipairs$investor_iso %in% c(a, b) & ipairs$host_iso %in% c(a, b) & !is.na(ipairs$host_rank) & ipairs$host_rank <= 5, c("node_id", "investor_iso", "host_iso", "host_rank")]
imports <- as.data.frame(readRDS(need_file(file.path(processed_dir, "import_partner_cells_tbl.rds"))))
imports <- imports[imports$reporter_iso %in% c(a, b), , drop = FALSE]
devpart <- if (file.exists(devpart_path)) as.data.frame(readRDS(devpart_path)) else { warning("no development-partnership table: ", devpart_path); NULL }
evdir <- file.path(ref_dir, "partnership_evidence")
ev <- utils::read.csv(need_file(file.path(evdir, "policy_evidence.csv")), stringsAsFactors = FALSE, colClasses = "character")
mech <- utils::read.csv(need_file(file.path(evdir, "bilateral_mechanisms.csv")), stringsAsFactors = FALSE, colClasses = "character")
mech <- mech[vapply(strsplit(mech$pair, "\\|"), function(z) all(c(a, b) %in% z), logical(1)), , drop = FALSE]
institutions <- utils::read.csv(need_file(file.path(evdir, "institutions.csv")), stringsAsFactors = FALSE, colClasses = "character")
agreements <- rcsv(file.path(ref_dir, "economic_security_agreements.csv"))
inst <- utils::read.csv(file.path(repo_root, "config", "partnership_instruments.csv"), stringsAsFactors = FALSE)

step("assessing the pair")
frame <- pm_node_frame(nodes, spine_base, nclass, dol, cfg$inputs$coercive_suppliers)
res <- pm_assess_pair(list(sp = sp, frame = frame, dol = dol, trade = imports, ev = ev, mech = mech, inst = inst,
                           devpart = devpart, invest_pairs = ipairs, croles = croles), pair_cfg, cfg)
d <- res$dyad
names_short <- list(a_short = pair_cfg$a_short, b_short = pair_cfg$b_short)
cc <- pm_compact_candidates(d, compacts, croles, mech, cfg, a, b, ev)
sel <- cc$strategic_compact_id[cc$recommended]
arch <- do.call(rbind, lapply(cc$strategic_compact_id, function(cm) pm_compact_architecture(d, cm, res$links)))
trade_tbl <- pm_trade_agenda(d, cfg, a, b, names_short)
public_names <- pm_public_names(institutions, pair_cfg, cfg)
inv_tbl <- pm_investment_agenda(d, cfg, ev, inst, a, b, public_names)
third <- pm_third_country(res$devpart, sroles, d, sel, cfg, a, b)
deals <- pm_deal_pipeline(d, inst, institutions, ev, cfg, a, b, names_short, public_names)
mea <- pm_mea(sel, mech, cfg)
nonp <- pm_non_priorities(d, mea, cfg, names_short, a, b)
memo <- pm_memo_sections(d = d, prof = res$profiles, cc = cc, arch = arch, trade_tbl = trade_tbl, inv_tbl = inv_tbl,
                         third = third, deals = deals, mea = mea, nonp = nonp, ev = ev, mech = mech, sroles = sroles,
                         agreements = agreements, compacts = compacts, pair_cfg = pair_cfg, cfg = cfg, institutions = institutions)
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

if (!tables_only && !("--no-pdf" %in% args)) {
  dcfg <- cfg$document
  sp_file <- file.path(processed_dir, "outputs", "strategic_position_tbl.rds")
  as_of <- format(as.Date(file.mtime(sp_file)), "%Y-%m-%d")
  as_of_label <- trimws(format(as.Date(as_of), "%e %B %Y"))
  ev_access <- max(ev$access_date)
  fillv <- list(as_of_label = as_of_label, evidence_access = ev_access, pair = pair_cfg$title)
  fillf <- function(s) { for (k in names(fillv)) s <- gsub(paste0("{", k, "}"), fillv[[k]], s, fixed = TRUE); s }
  doc <- pm_document(memo$markdown, ev, memo$boxes, pair_cfg, dcfg,
                     list(pair_id = pair, as_of = as_of, as_of_label = as_of_label, evidence_access = ev_access,
                          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), fillf(dcfg$osi_source))
  font <- abs_path(arg_value("--font", file.path("dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2")))
  if (!file.exists(font)) {
    alt <- normalizePath(file.path(processed_dir, "..", "..", "dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2"), winslash = "/", mustWork = FALSE)
    if (file.exists(alt)) font <- alt
  }
  font_b64 <- if (file.exists(font)) jsonlite::base64_enc(readBin(font, "raw", file.size(font))) else NULL
  html <- country_brief_html(doc, list(), font_b64, "Letter", list())
  # long URLs and node ids must wrap: an over-wide line makes Edge shrink the whole page to fit (memo only;
  # the briefs' stylesheet is untouched)
  html <- sub("</style>", ".sources li,p,li,td,th{overflow-wrap:anywhere}</style>", html, fixed = TRUE)
  work <- tempfile("pmemo"); dir.create(work); on.exit(unlink(work, recursive = TRUE), add = TRUE)
  edge <- country_memo_edge(args); python <- country_memo_python(args)
  if (is.na(edge)) stop("no Edge or Chrome for the PDF (pass --edge, or --no-pdf)")
  if (is.na(python)) stop("no python with python-docx and pypdf (pass --python, or --no-pdf)")
  step("PDF (headless Edge)")
  pdf <- country_memo_pdf(html, edge, work)
  ast_path <- file.path(work, "memo.ast.json")
  country_memo_write_text(as.character(country_json(doc)), ast_path)
  docx_script <- file.path(repo_root, "scripts", "utils", "country_memo_docx.py")
  docx <- file.path(work, "memo.docx")
  step("Word (python-docx)")
  country_memo_py(python, docx_script, c("--in", ast_path, "--out", docx))
  checks <- list()
  chk <- function(check, ok, note = NA_character_, warn = FALSE)
    checks[[length(checks) + 1]] <<- data.frame(check = check, status = if (ok) "PASS" else if (warn) "WARN" else "FAIL", note = note, stringsAsFactors = FALSE)
  chk("print HTML: Roboto embedded", !is.null(font_b64), if (is.null(font_b64)) "font file missing: Arial (pass --font)" else NA, warn = TRUE)
  ext <- country_memo_html_external(html)
  chk("print HTML: no external reference", !length(ext), if (length(ext)) paste(utils::head(ext, 3), collapse = " ") else NA)
  back <- tryCatch(country_memo_py(python, docx_script, c("--check", docx, "--ast", ast_path)), error = function(e) structure(conditionMessage(e), failed = TRUE))
  chk("DOCX: read-back paragraphs equal the document", is.null(attr(back, "failed")), if (!is.null(attr(back, "failed"))) substr(back, 1, 300) else NA)
  pt_path <- file.path(work, "pdf_text.json")
  country_memo_py(python, docx_script, c("--pdf-text", pdf, "--out", pt_path))
  pt <- jsonlite::fromJSON(pt_path)
  norm <- function(x) gsub("[[:space:]]+", " ", gsub("\u00ad", "", x))
  text <- norm(pt$text)
  pg <- unlist(dcfg$pdf_pages)
  chk(sprintf("PDF: %d pages (%d-%d)", pt$pages, pg[1], pg[2]), pt$pages >= pg[1] && pt$pages <= pg[2])
  chk("PDF: contains the title and the label", grepl(norm(doc$title), text, fixed = TRUE) && grepl(norm(doc$label), text, fixed = TRUE))
  heads <- vapply(doc$sections, function(s) if (identical(as.integer(s$n), 0L)) s$title else sprintf("%d. %s", s$n, s$title), "")
  chk("PDF: every section heading is in its text", all(vapply(heads, function(h) grepl(norm(h), text, fixed = TRUE), TRUE)))
  md <- country_brief_markdown(doc)
  jtxt <- as.character(country_json(doc))
  jgz <- length(memCompress(charToRaw(enc2utf8(jtxt)), "gzip"))
  chk(sprintf("sizes: JSON %s B gz, PDF %s B, DOCX %s B (limits 61,440 gz / 2 MB / 2 MB)", format(jgz, big.mark = ","), format(file.size(pdf), big.mark = ","),
              format(file.size(docx), big.mark = ",")), jgz <= 61440 && file.size(pdf) <= 2097152 && file.size(docx) <= 2097152)
  checks <- do.call(rbind, checks)
  for (i in seq_len(nrow(checks))) message(sprintf("  %-4s %s%s", checks$status[i], checks$check[i], if (!is.na(checks$note[i])) paste0(" (", checks$note[i], ")") else ""))
  # a viewer holding the old file makes file.copy() fail silently: check, and say so
  put <- function(src, dest) {
    if (file.exists(dest)) unlink(dest)
    if (!isTRUE(file.copy(src, dest, overwrite = TRUE)) || !identical(unname(tools::md5sum(src)), unname(tools::md5sum(dest))))
      warning("could not write ", dest, " (is it open in a viewer?); the published set below is unaffected", call. = FALSE)
  }
  put(pdf, file.path(out_dir, "partnership_memo.pdf"))
  put(docx, file.path(out_dir, "partnership_memo.docx"))
  country_memo_write_text(as.character(country_json(doc, pretty = TRUE)), file.path(out_dir, "partnership_memo.ast.json"))
  country_memo_write_text(html, file.path(out_dir, "partnership_memo.print.html"))
  utils::write.csv(checks, file.path(out_dir, "render_checks.csv"), row.names = FALSE, na = "")
  step("PDF: ", file.path(out_dir, "partnership_memo.pdf"), " (", pt$pages, " pages)")
  if (any(checks$status == "FAIL")) stop("a render check failed (render_checks.csv): nothing published")
  publish_dir <- arg_value("--publish-dir")
  if (!is.null(publish_dir)) {
    publish_dir <- abs_path(publish_dir)
    dir.create(publish_dir, recursive = TRUE, showWarnings = FALSE)
    jp <- file.path(work, "m.json"); country_memo_write_text(jtxt, jp)
    mp <- file.path(work, "m.md"); country_memo_write_text(md, mp)
    nm <- c(json = sprintf("memo-%s.%s.json", pair, country_memo_md5_8(jp)), pdf = sprintf("memo-%s.%s.pdf", pair, country_memo_md5_8(pdf)),
            docx = sprintf("memo-%s.%s.docx", pair, country_memo_md5_8(docx)), md = sprintf("memo-%s.%s.md", pair, country_memo_md5_8(mp)))
    old <- list.files(publish_dir, pattern = sprintf("^memo-%s\\.[0-9a-f]{8}\\.(json|pdf|docx|md)$", pair))
    file.copy(c(jp, pdf, docx, mp), file.path(publish_dir, nm[c("json", "pdf", "docx", "md")]), overwrite = TRUE)
    stale <- setdiff(old, nm); if (length(stale)) unlink(file.path(publish_dir, stale))
    mf <- file.path(publish_dir, "manifest.json")
    man <- if (file.exists(mf)) jsonlite::fromJSON(mf, simplifyVector = FALSE) else list(schema = "osi-partnership-memos", schema_version = 1L, pairs = list())
    man$pairs[[pair]] <- list(pair = pair, country_a = a, country_b = b, title = pair_cfg$title, json = nm[["json"]], pdf = nm[["pdf"]],
                              docx = nm[["docx"]], md = nm[["md"]], words = doc$words, generated_at = doc$generated_at, as_of = as_of,
                              evidence_access = ev_access, format = doc$format, verified = FALSE)
    jsonlite::write_json(man, mf, auto_unbox = TRUE, pretty = TRUE, null = "null")
    step("published ", paste(nm, collapse = ", "), " to ", publish_dir)
  }
}
step("done: ", nrow(d), " units; ", sum(d$priority_tier == 1), " joint bets; ", length(sel), " compacts; MEA ", mea$option)
