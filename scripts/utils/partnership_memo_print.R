# Print and publish a Partnership or Compact memo (IO): the country strategy briefs' renderer, print style, page
# geometry, Word builder and render checks (docs/partnership_memos.md, "Print document"). Used by
# scripts/34_build_partnership_memo.R and scripts/35_build_compact_memo.R; expects R/partnerships/pm_document.R,
# R/dashboard/country/country_brief_doc.R and scripts/utils/country_memos.R to be sourced.
#
#   markdown, boxes   the memo text and its summary boxes (pm_memo_sections() / pm_compact_memo_sections())
#   ev                the evidence layer (sources); doc_cfg: config `document` (with any overrides applied)
#   title_cfg         list(title = ...): the document title (pair_cfg or the compact config)
#   id                the pair or compact id (file names, manifest key); section: manifest section ("pairs", "compacts")
#   entry             extra manifest fields for this id
#   opts              list(args, processed_dir, out_dir, repo_root, step = message-like function, stem = output file stem
#                     (default "partnership_memo"))
pm_print_publish <- function(markdown, boxes, ev, title_cfg, doc_cfg, id, section, entry, opts) {
  args <- opts$args; step <- opts$step; repo_root <- opts$repo_root
  arg_value <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[[i + 1]] }
  abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
  sp_file <- file.path(opts$processed_dir, "outputs", "strategic_position_tbl.rds")
  as_of <- format(as.Date(file.mtime(sp_file)), "%Y-%m-%d")
  as_of_label <- trimws(format(as.Date(as_of), "%e %B %Y"))
  ev_access <- max(ev$access_date)
  fillv <- list(as_of_label = as_of_label, evidence_access = ev_access, pair = title_cfg$title)
  fillf <- function(s) { for (k in names(fillv)) s <- gsub(paste0("{", k, "}"), fillv[[k]], s, fixed = TRUE); s }
  doc <- pm_document(markdown, ev, boxes, title_cfg, doc_cfg,
                     list(pair_id = id, as_of = as_of, as_of_label = as_of_label, evidence_access = ev_access,
                          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), fillf(doc_cfg$osi_source))
  font <- abs_path(arg_value("--font", file.path("dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2")))
  if (!file.exists(font)) {
    alt <- normalizePath(file.path(opts$processed_dir, "..", "..", "dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2"), winslash = "/", mustWork = FALSE)
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
  norm <- function(x) gsub("[[:space:]]+", " ", gsub("­", "", x))
  text <- norm(pt$text)
  pg <- unlist(doc_cfg$pdf_pages)
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
  out_dir <- opts$out_dir
  stem <- if (is.null(opts$stem)) "partnership_memo" else opts$stem
  put(pdf, file.path(out_dir, paste0(stem, ".pdf")))
  put(docx, file.path(out_dir, paste0(stem, ".docx")))
  country_memo_write_text(as.character(country_json(doc, pretty = TRUE)), file.path(out_dir, paste0(stem, ".ast.json")))
  country_memo_write_text(html, file.path(out_dir, paste0(stem, ".print.html")))
  utils::write.csv(checks, file.path(out_dir, "render_checks.csv"), row.names = FALSE, na = "")
  step("PDF: ", file.path(out_dir, paste0(stem, ".pdf")), " (", pt$pages, " pages)")
  if (any(checks$status == "FAIL")) stop("a render check failed (render_checks.csv): nothing published")
  publish_dir <- arg_value("--publish-dir")
  if (!is.null(publish_dir)) {
    publish_dir <- abs_path(publish_dir)
    dir.create(publish_dir, recursive = TRUE, showWarnings = FALSE)
    jp <- file.path(work, "m.json"); country_memo_write_text(jtxt, jp)
    mp <- file.path(work, "m.md"); country_memo_write_text(md, mp)
    nm <- c(json = sprintf("memo-%s.%s.json", id, country_memo_md5_8(jp)), pdf = sprintf("memo-%s.%s.pdf", id, country_memo_md5_8(pdf)),
            docx = sprintf("memo-%s.%s.docx", id, country_memo_md5_8(docx)), md = sprintf("memo-%s.%s.md", id, country_memo_md5_8(mp)))
    old <- list.files(publish_dir, pattern = sprintf("^memo-%s\\.[0-9a-f]{8}\\.(json|pdf|docx|md)$", gsub("-", "\\\\-", id)))
    file.copy(c(jp, pdf, docx, mp), file.path(publish_dir, nm[c("json", "pdf", "docx", "md")]), overwrite = TRUE)
    stale <- setdiff(old, nm); if (length(stale)) unlink(file.path(publish_dir, stale))
    mf <- file.path(publish_dir, "manifest.json")
    man <- if (file.exists(mf)) jsonlite::fromJSON(mf, simplifyVector = FALSE) else list(schema = "osi-partnership-memos", schema_version = 1L, pairs = list())
    if (is.null(man[[section]])) man[[section]] <- list()
    man[[section]][[id]] <- c(entry, list(title = title_cfg$title, json = nm[["json"]], pdf = nm[["pdf"]], docx = nm[["docx"]], md = nm[["md"]],
                                          words = doc$words, generated_at = doc$generated_at, as_of = as_of, evidence_access = ev_access,
                                          format = doc$format, verified = FALSE))
    jsonlite::write_json(man, mf, auto_unbox = TRUE, pretty = TRUE, null = "null")
    step("published ", paste(nm, collapse = ", "), " to ", publish_dir)
  }
  invisible(list(pages = pt$pages, checks = checks))
}
