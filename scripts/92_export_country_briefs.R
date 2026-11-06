# Country strategy briefs: "[Country] and the Allied Industrial Base — What to secure, build, source from
# partners, and deploy". The successor of the Country Insights policy memos (scripts/88 --render-memos):
# the same writer -> lint -> independent verifier -> render loop, on the Country page's current method
# (Yards & Fences choices, Strategic Industrial Value, Competitive Position, deployment need, policy
# alignment) and the Allied Industrial Base, with two figures.
#
#   Rscript scripts/92_export_country_briefs.R --iso3 AUS --facts-only        write the facts the writer cites
#   Rscript scripts/92_export_country_briefs.R --iso3 AUS --lint-only         lint brief.src.md (the writer's loop)
#   Rscript scripts/92_export_country_briefs.R --iso3 AUS --preview <dir>     render into <dir> without verification
#                                                                              or publication (review copies)
#   Rscript scripts/92_export_country_briefs.R --iso3 AUS                     verify, render and publish
#
# Options: --memo-dir (dashboard/country-memos), --out-dir (dashboard/public/data/country), --posture-dir
# (dashboard/public/data/posture), --aib-dir (dashboard/public/data/aib), --edge <msedge.exe>, --python
# <python.exe>. Inputs are the published dashboard bundles (run after scripts/84, 88 and 91). Per country,
# dashboard/country-memos/<ISO3>/ holds brief.src.md (the writer's), brief.facts.json, brief.lint.json and
# brief.claims.json (written here) and brief.verify.json (the verifier's). Publication writes
# memo-<ISO3>.<h8>.{json,pdf,docx,md} and the figures memo-<ISO3>-f<n>.<h8>.{svg,png} into --out-dir and
# the manifest's memo entry (format "brief"), replacing the country's legacy memo files. Rules live in
# R/dashboard/country/country_brief*.R and config/country_brief.yml.

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
        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

args <- commandArgs(trailingOnly = TRUE)
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

for (f in c("R/dashboard/country/country_util.R", "R/dashboard/country/country_check.R", "R/dashboard/country/country_memo.R", "R/dashboard/country/country_memo_lint.R",
            "R/strategic_position/pathway.R", "R/dashboard/country/country_devpart.R",
            "R/dashboard/country/country_brief.R", "R/dashboard/country/country_brief_objects.R", "R/dashboard/country/country_brief_dev.R",
            "R/dashboard/country/country_brief_doc.R", "R/dashboard/country/country_brief_charts.R",
            "scripts/utils/country_memos.R")) source(file.path(repo_root, f))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_brief.yml"))
dcfg <- yaml::read_yaml(file.path(repo_root, "config", "development_partnerships.yml"))
iso3 <- toupper(arg_value("--iso3", ""))
if (!grepl("^[A-Z]{3}$", iso3)) stop("--iso3 <ISO3> is required (one economy per run while the briefs are piloted)")
memo_dir <- abs_path(arg_value("--memo-dir", "dashboard/country-memos"))
out_dir <- abs_path(arg_value("--out-dir", "dashboard/public/data/country"))
posture_dir <- abs_path(arg_value("--posture-dir", "dashboard/public/data/posture"))
aib_dir <- abs_path(arg_value("--aib-dir", "dashboard/public/data/aib"))
preview_dir <- arg_value("--preview")
if (!is.null(preview_dir)) preview_dir <- abs_path(preview_dir)
dir_c <- file.path(memo_dir, iso3)
dir.create(dir_c, showWarnings = FALSE, recursive = TRUE)
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)

# ---- Inputs and facts ------------------------------------------------------------------------------------

step("reading the posture, AIB and Country bundles")
pm <- read_json(file.path(posture_dir, "manifest.json"))
if (is.null(pm$details[[iso3]])) stop("no posture detail for ", iso3, " in ", posture_dir)
core <- read_json(file.path(posture_dir, pm$core))
detail <- read_json(file.path(posture_dir, pm$details[[iso3]]))
am <- read_json(file.path(aib_dir, "manifest.json"))
aib <- read_json(file.path(aib_dir, am$files$compacts))
managed <- read_json(file.path(aib_dir, am$files$scenarios$managed))
cm <- read_json(file.path(out_dir, "manifest.json"))
entry_i <- which(vapply(cm$countries, function(c) identical(c$iso3, iso3), TRUE))
if (!length(entry_i)) stop(iso3, " is not in the Country manifest")
entry <- cm$countries[[entry_i]]
if (!identical(entry$tier, "core") && !iso3 %in% unlist(cfg$brief$beyond_core)) {
  stop("briefs are for core coalition members (and config brief.beyond_core) only; ", iso3, " is '", entry$tier, "'")
}
date_label <- function(ts) { d <- as.Date(substr(ts %||% "", 1, 10)); if (is.na(d)) "" else sub("^0", "", format(d, "%d %B %Y")) }
editions <- list(posture_edition = sprintf("dashboard data of %s", date_label(pm$created)), aib_edition = sprintf("run of %s", date_label(am$created)))
ppd <- utils::read.csv(file.path(repo_root, "config", "posture_policy_design.csv"), stringsAsFactors = FALSE, check.names = FALSE)
# the development-partnership file (scripts/93), the economy's entry `devpart`: run scripts/93 first
if (is.null(entry$devpart) || !file.exists(file.path(out_dir, entry$devpart))) stop(iso3, " has no development-partnership file: run scripts/93_export_development_partnerships.R first")
dev <- read_json(file.path(out_dir, entry$devpart))
facts <- country_brief_facts(core, detail, aib, managed, iso3, cfg, cm$as_of_label, editions, am$created, ppd, dcfg, dev)
facts_txt <- as.character(country_json(facts))
facts_sha <- country_sha256(facts_txt)
country_memo_write_text(as.character(country_json(facts, pretty = TRUE)), file.path(dir_c, "brief.facts.json"))
step(sprintf("facts: %d steps, %d Secure units in %d capabilities, sha256 %s", facts$counts$steps$v, facts$counts$high_units$v,
             facts$counts$high_capabilities$v, substr(facts_sha, 1, 12)))
if (arg_flag("--facts-only")) quit(status = 0)

src_path <- file.path(dir_c, "brief.src.md")
if (!file.exists(src_path)) { step("no brief.src.md yet: the facts are written for the writer"); quit(status = 0) }

# ---- Lint ------------------------------------------------------------------------------------------------

country <- facts$meta$country$d
fill <- list(country = country, country_poss = facts$meta$country_poss$d, as_of_label = cm$as_of_label)
fill$development_title <- country_fill(cfg$brief$development_titles[[dev$perspective]] %||% cfg$brief$development_titles$none, fill)
src <- country_memo_read_text(src_path)
src_sha <- country_sha256(src)
cd <- country_brief_chart_data(core, detail, iso3, aib, managed, cfg, dcfg)
gen <- c(country_brief_generated_tables(facts, cd, cfg), country_brief_dev_tables(facts, dev, cfg, country_brief_name_fn(core)))
parsed <- country_brief_expand_tables(country_brief_parse(src, cfg, fill), gen)
res <- country_brief_resolve(parsed, facts)
sents <- country_brief_sentences(res)
claims <- country_brief_claims(res, sents)
names_all <- unique(as.character(unlist(core$countries$name)))
rv_keys <- as.character(unlist(facts$review_order))
rv_names <- stats::setNames(vapply(rv_keys, function(k) as.character(facts$cells[[k]]$name$d %||% ""), ""), rv_keys)
lint <- country_brief_lint(parsed, res, sents, cfg, fill, names_all, review_keys = rv_keys,
                           dev_empty = identical(dev$perspective, "donor") && identical(facts$dev$n_pairs$v, 0L), review_names = rv_names,
                           n_high = facts$counts$high_capabilities$v)
claims_doc <- country_brief_claims_doc(iso3, claims, sents)
claims_txt <- as.character(country_json(claims_doc, pretty = TRUE))
claims_sha <- country_sha256(claims_txt)
country_memo_write_text(claims_txt, file.path(dir_c, "brief.claims.json"))
# the content trace: each claim with its dashboard field, source datasets and evidence quality
trace <- country_brief_trace(claims, facts, cfg)
utils::write.csv(trace, file.path(dir_c, "brief.trace.csv"), row.names = FALSE, fileEncoding = "UTF-8")
country_memo_write_text(country_brief_trace_markdown(trace, country_fill(cfg$brief$title, fill)), file.path(dir_c, "brief.trace.md"))
lint_doc <- list(schema = "osi-country-brief-lint", iso3 = iso3, src_sha256 = src_sha, facts_sha256 = facts_sha, claims_sha256 = claims_sha,
                 words = lint$words, words_tables = lint$words_tables, words_sections = lint$words_sections, n_claims = length(claims),
                 n_sentences = length(sents$sentences), n_errors = lint$n_errors, n_warnings = lint$n_warnings, items = lint$items)
country_memo_write_text(as.character(country_json(lint_doc, pretty = TRUE)), file.path(dir_c, "brief.lint.json"))
step(sprintf("lint: %d errors, %d warnings; %d words of main text (+%d in tables); %d claims, %d sentences", lint$n_errors, lint$n_warnings,
             lint$words, lint$words_tables, length(claims), length(sents$sentences)))
for (it in lint$items) message(sprintf("  %-7s %-13s %s", toupper(it$severity), it$check, it$detail))
if (arg_flag("--lint-only")) quit(status = if (lint$n_errors) 1L else 0L)
if (lint$n_errors) stop("the brief does not lint: fix the errors in brief.lint.json")

# ---- Verification ------------------------------------------------------------------------------------------

verify_path <- file.path(dir_c, "brief.verify.json")
verify <- if (file.exists(verify_path)) read_json(verify_path) else NULL
vc <- country_brief_verify_check(verify, claims_doc, lint, src_sha, facts_sha, claims_sha)
if (is.null(preview_dir)) {
  if (!isTRUE(vc$ok)) stop("brief.verify.json does not sign this source: ", paste(vc$problems, collapse = "; "))
  if (isTRUE(vc$reuse)) {
    verify$reused_for <- c(unlist(verify$reused_for), facts_sha)
    country_memo_write_text(as.character(country_json(verify, pretty = TRUE)), verify_path)
  }
} else step(sprintf("preview: verification %s (%s)", if (isTRUE(vc$ok)) "passes" else "not complete", paste(utils::head(vc$problems, 2), collapse = "; ")))

# ---- Figures ---------------------------------------------------------------------------------------------

work <- tempfile("brief")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
ch <- cfg$charts
plots <- list(
  strategy_map = list(p = country_brief_map_plot(cd$cells, cd$layers, cfg, unlist(core$meta$techs), unlist(core$meta$stages), core$meta$stage_display, "Arial"),
                      h = ch$map_height_in),
  value_position = list(p = country_brief_scatter_plot(cd$cells, cd$layers, cfg, core$meta$thresholds, "Arial"), h = ch$scatter_height_in),
  aib_roles = list(p = country_brief_matrix_plot(cd$matrix, cfg, "Arial"), h = country_brief_matrix_layout(cd$matrix, cfg)$height_in),
  deployment = list(p = country_brief_deployment_plot(cd$deployment, cfg, core$meta$thresholds, "Arial"), h = ch$deployment_height_in))
fig_ids <- unlist(lapply(parsed$sections, function(s) unlist(lapply(s$blocks, function(b) if (identical(b$type, "figure")) b$id else NULL))))
svgs <- list()
fig_paths <- list()
for (k in seq_along(fig_ids)) {
  id <- fig_ids[k]
  svg_p <- file.path(work, sprintf("f%d.svg", k))
  png_p <- file.path(work, sprintf("f%d.png", k))
  svglite::svglite(svg_p, width = ch$width_in, height = plots[[id]]$h, bg = "white")
  print(plots[[id]]$p)
  grDevices::dev.off()
  ragg::agg_png(png_p, width = ch$width_in, height = plots[[id]]$h, units = "in", res = ch$dpi, background = "white")
  print(plots[[id]]$p)
  grDevices::dev.off()
  # the charts are flat colour: a 256-colour palette PNG keeps them sharp at about 40% of the size
  # (2026-11-04, the brief rollout's Country-data budget); the Word file embeds the same PNG
  if (requireNamespace("magick", quietly = TRUE)) {
    magick::image_write(magick::image_quantize(magick::image_read(png_p), max = 256, dither = FALSE), png_p, format = "png", depth = 8)
  }
  svg_txt <- country_memo_read_text(svg_p)
  svgs[[id]] <- sub("^<\\?xml[^>]*>\\s*", "", svg_txt)
  fig_paths[[id]] <- list(k = k, svg = svg_p, png = png_p)
}
fig_files <- lapply(fig_ids, function(id) {
  fp <- fig_paths[[id]]
  list(svg = sprintf("memo-%s-f%d.%s.svg", iso3, fp$k, country_memo_md5_8(fp$svg)), png = sprintf("memo-%s-f%d.%s.png", iso3, fp$k, country_memo_md5_8(fp$png)),
       alt = country_brief_alt(id, cd$cells, country))
})
names(fig_files) <- fig_ids

# ---- Document and renderings ------------------------------------------------------------------------------

utc <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
meta <- list(generated_at = utc(file.mtime(src_path)), verified_at = if (!is.null(verify$verified_at)) verify$verified_at else NULL,
             writer = verify$writer %||% "AI writer", verifier = verify$verifier %||% if (is.null(preview_dir)) "independent AI verifier" else "not verified (preview)",
             src_sha256 = src_sha, claims_sha256 = claims_sha, facts_sha256 = facts_sha, as_of = cm$as_of)
doc <- country_brief_document(parsed, res, claims, facts, cfg, fill, meta, fig_files)
font <- file.path(repo_root, "dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2")
font_b64 <- if (file.exists(font)) jsonlite::base64_enc(readBin(font, "raw", file.size(font))) else NULL
html <- country_brief_html(doc, svgs, font_b64, cfg$brief$page_size %||% "Letter", cfg$charts$colors)
checks <- list()
chk <- function(check, ok, note = NA_character_, warn = FALSE) {
  checks[[length(checks) + 1]] <<- data.frame(check = check, status = if (ok) "PASS" else if (warn) "WARN" else "FAIL", note = note, stringsAsFactors = FALSE)
}
chk("print HTML: Roboto embedded", !is.null(font_b64), if (is.null(font_b64)) "font file missing: Arial" else NA, warn = TRUE)
ext <- country_memo_html_external(html)
chk("print HTML: no external reference", !length(ext), if (length(ext)) paste(utils::head(ext, 3), collapse = " ") else NA)
edge <- country_memo_edge(args)
python <- country_memo_python(args)
if (is.na(edge)) stop("no Edge or Chrome for the PDF (pass --edge)")
if (is.na(python)) stop("no python with python-docx and pypdf (pass --python)")
step("PDF (headless Edge)")
pdf <- country_memo_pdf(html, edge, work)
ast_path <- file.path(work, "brief.ast.json")
country_memo_write_text(as.character(country_json(doc)), ast_path)
docx_script <- file.path(repo_root, "scripts", "utils", "country_memo_docx.py")
docx <- file.path(work, "brief.docx")
step("Word (python-docx)")
country_memo_py(python, docx_script, c("--in", ast_path, "--out", docx, "--figures", work))
back <- tryCatch(country_memo_py(python, docx_script, c("--check", docx, "--ast", ast_path)), error = function(e) structure(conditionMessage(e), failed = TRUE))
chk("DOCX: read-back paragraphs equal the document", is.null(attr(back, "failed")), if (!is.null(attr(back, "failed"))) substr(back, 1, 300) else NA)
pt_path <- file.path(work, "pdf_text.json")
country_memo_py(python, docx_script, c("--pdf-text", pdf, "--out", pt_path))
pt <- jsonlite::fromJSON(pt_path)
norm <- function(x) gsub("[[:space:]]+", " ", gsub("­", "", x))
text <- norm(pt$text)
pg <- unlist(cfg$brief$pdf_pages %||% list(4, 14))
chk(sprintf("PDF: %d pages (%d-%d: about 8-9 of main text, then the appendix)", pt$pages, pg[1], pg[2]), pt$pages >= pg[1] && pt$pages <= pg[2], NA)
chk("PDF: contains the title and the label", grepl(norm(doc$title), text, fixed = TRUE) && grepl(norm(doc$label), text, fixed = TRUE))
squash <- gsub(" ", "", text, fixed = TRUE)
vals <- country_brief_values(doc)
miss <- vals[!vapply(vals, function(v) grepl(gsub(" ", "", norm(v), fixed = TRUE), squash, fixed = TRUE), TRUE)]
chk("PDF: every value of the document is in its text", !length(miss), if (length(miss)) paste(utils::head(miss, 5), collapse = " | ") else NA)
md <- country_brief_markdown(doc)
jtxt <- as.character(country_json(doc))
lim <- list(json = 61440, pdf = 2097152, docx = 2097152)
jgz <- length(memCompress(charToRaw(enc2utf8(jtxt)), "gzip"))
chk(sprintf("sizes: JSON %s B gz, PDF %s B, DOCX %s B (limits 61,440 gz / 2 MB / 2 MB)", format(jgz, big.mark = ","), format(file.size(pdf), big.mark = ","),
            format(file.size(docx), big.mark = ",")), jgz <= lim$json && file.size(pdf) <= lim$pdf && file.size(docx) <= lim$docx)
chk("document words equal the lint words", identical(as.integer(doc$words), as.integer(lint$words)))
checks <- do.call(rbind, checks)
for (i in seq_len(nrow(checks))) message(sprintf("  %-4s %s%s", checks$status[i], checks$check[i], if (!is.na(checks$note[i])) paste0(" (", checks$note[i], ")") else ""))
country_memo_write_text(as.character(country_json(doc, pretty = TRUE)), file.path(dir_c, "brief.ast.json"))
country_memo_write_text(html, file.path(dir_c, "brief.print.html"))
if (any(checks$status == "FAIL") && is.null(preview_dir)) stop("a render check failed: nothing published")

# ---- Publish ---------------------------------------------------------------------------------------------

write_set <- function(dest) {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  jp <- file.path(work, "m.json"); country_memo_write_text(jtxt, jp)
  mp <- file.path(work, "m.md"); country_memo_write_text(md, mp)
  nm <- c(json = sprintf("memo-%s.%s.json", iso3, country_memo_md5_8(jp)), pdf = sprintf("memo-%s.%s.pdf", iso3, country_memo_md5_8(pdf)),
          docx = sprintf("memo-%s.%s.docx", iso3, country_memo_md5_8(docx)), md = sprintf("memo-%s.%s.md", iso3, country_memo_md5_8(mp)))
  old <- list.files(dest, pattern = sprintf("^memo-%s(-f[0-9]+)?\\.[0-9a-f]{8}\\.(json|pdf|docx|md|svg|png)$", iso3))
  file.copy(jp, file.path(dest, nm[["json"]]), overwrite = TRUE)
  file.copy(pdf, file.path(dest, nm[["pdf"]]), overwrite = TRUE)
  file.copy(docx, file.path(dest, nm[["docx"]]), overwrite = TRUE)
  file.copy(mp, file.path(dest, nm[["md"]]), overwrite = TRUE)
  figs <- character()
  for (id in fig_ids) {
    file.copy(fig_paths[[id]]$svg, file.path(dest, fig_files[[id]]$svg), overwrite = TRUE)
    file.copy(fig_paths[[id]]$png, file.path(dest, fig_files[[id]]$png), overwrite = TRUE)
    figs <- c(figs, fig_files[[id]]$svg, fig_files[[id]]$png)
  }
  stale <- setdiff(old, c(nm, figs))
  if (length(stale)) unlink(file.path(dest, stale))
  list(json = nm[["json"]], pdf = nm[["pdf"]], docx = nm[["docx"]], md = nm[["md"]], figures = I(figs), words = doc$words,
       generated_at = doc$generated_at, verified_at = doc$verified_at, facts_sha256 = facts_sha, stale = FALSE, format = COUNTRY_BRIEF_FORMAT)
}
if (!is.null(preview_dir)) {
  e <- write_set(preview_dir)
  country_memo_write_text(html, file.path(preview_dir, sprintf("memo-%s.print.html", iso3)))
  step(sprintf("preview written to %s (%s)", preview_dir, e$pdf))
  quit(status = 0)
}
e <- write_set(out_dir)
cm$countries[[entry_i]]$memo <- e
country_memo_write_manifest(cm, out_dir)
step(sprintf("published %s, %s, %s, %s and %d figure files", e$json, e$pdf, e$docx, e$md, length(e$figures)))
