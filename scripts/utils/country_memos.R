# IO of the Country Insights policy memos (spec_country.md part B), sourced by
# scripts/88_export_country_data.R:
#
#   Rscript scripts/88_export_country_data.R --render-memos [--iso3 USA,JPN] [--lint-only]
#       [--memo-dir <dir>] [--out-dir <dir>] [--edge <msedge.exe>] [--python <python.exe>] [--force-stale]
#   Rscript scripts/88_export_country_data.R --render-memos --placeholder --out-dir <dir>
#
# For each core member with dashboard/country-memos/<ISO3>/memo.src.md: parse, resolve and lint the
# source against the published fact sheet (with its companion drivers file merged back), write
# lint.json and claims.json beside it (--lint-only stops there: the memo writers' loop). Otherwise
# require a complete, passing verify.json from the independent verifier (B6.3; claims, sentences,
# lint warnings, hashes), build the memo document, print it to PDF with headless Edge from a
# self-contained HTML page, build the Word file with python-docx (scripts/utils/country_memo_docx.py),
# run the render checks (B7.5), and publish memo-<ISO3>.<h8>.{json,pdf,docx,md} with the manifest's
# memo entry. --placeholder renders the PDF and Word files of the synthetic fixture's placeholder memos
# (no lint, no verification). Every rule lives in R/dashboard/country/country_memo*.R.

country_memo_arg <- function(args, flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}

country_memo_edge <- function(args) {
  cand <- c(country_memo_arg(args, "--edge"), Sys.getenv("OSI_EDGE"),
            file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft/Edge/Application/msedge.exe"),
            file.path(Sys.getenv("ProgramFiles"), "Microsoft/Edge/Application/msedge.exe"),
            file.path(Sys.getenv("ProgramFiles"), "Google/Chrome/Application/chrome.exe"),
            file.path(Sys.getenv("ProgramFiles(x86)"), "Google/Chrome/Application/chrome.exe"),
            Sys.which("msedge"), Sys.which("chrome"))
  cand <- cand[!is.na(cand) & nzchar(cand)]
  hit <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else NA_character_
}

country_memo_python <- function(args) {
  cand <- c(country_memo_arg(args, "--python"), Sys.getenv("OSI_PYTHON"), Sys.which("python"), Sys.which("python3"))
  cand <- cand[!is.na(cand) & nzchar(cand)]
  for (p in cand) {
    ok <- tryCatch(system2(p, c("-I", "-c", shQuote("import docx, pypdf")), stdout = FALSE, stderr = FALSE), error = function(e) 1L)
    if (identical(as.integer(ok), 0L)) return(p)
  }
  NA_character_
}

country_memo_read_text <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  txt <- rawToChar(readBin(con, "raw", file.size(path)))
  Encoding(txt) <- "UTF-8"
  txt
}

country_memo_write_text <- function(txt, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
}

country_memo_md5_8 <- function(path) substr(unname(tools::md5sum(path)), 1, 8)

# HTML -> PDF with headless Edge (B7.3). Returns the PDF path or stops.
country_memo_pdf <- function(html, edge, work) {
  html_path <- file.path(work, "memo.print.html")
  pdf_path <- file.path(work, "memo.pdf")
  prof <- file.path(work, "edge-profile")
  dir.create(prof, showWarnings = FALSE)
  country_memo_write_text(html, html_path)
  if (file.exists(pdf_path)) unlink(pdf_path)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(html_path, winslash = "/")))
  a <- c("--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--disable-extensions",
         paste0("--user-data-dir=", normalizePath(prof, winslash = "/")), "--no-pdf-header-footer",
         paste0("--print-to-pdf=", normalizePath(pdf_path, winslash = "/", mustWork = FALSE)), url)
  system2(edge, shQuote(a), stdout = FALSE, stderr = FALSE, wait = TRUE, timeout = 180)
  for (k in 1:30) { if (file.exists(pdf_path) && file.size(pdf_path) > 0) break; Sys.sleep(1) }
  unlink(prof, recursive = TRUE)
  if (!file.exists(pdf_path) || file.size(pdf_path) == 0) stop("Edge did not write the PDF (", edge, ")")
  pdf_path
}

# Run the DOCX script: build, read back, PDF text. Returns stdout lines; stops on a non-zero exit.
country_memo_py <- function(python, script, a) {
  out <- suppressWarnings(system2(python, c("-I", shQuote(script), shQuote(a)), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (!is.null(st) && st != 0) stop("python ", basename(script), " ", a[1], " failed: ", paste(utils::tail(out, 5), collapse = " | "))
  out
}

# Render one memo document to PDF, DOCX and Markdown in `work`; run the render checks (B7.5).
# Returns list(pdf, docx, md, checks = data.frame(check, status, note)).
country_memo_render_files <- function(doc, cfg, repo_root, edge, python, work, placeholder = FALSE) {
  checks <- list()
  chk <- function(check, ok, note = NA_character_, warn = FALSE) {
    checks[[length(checks) + 1]] <<- data.frame(check = check, status = if (ok) "PASS" else if (warn) "WARN" else "FAIL", note = note, stringsAsFactors = FALSE)
  }
  font <- file.path(repo_root, "dashboard", "node_modules", "@fontsource-variable", "roboto", "files", "roboto-latin-wght-normal.woff2")
  font_b64 <- if (file.exists(font)) jsonlite::base64_enc(readBin(font, "raw", file.size(font))) else NULL
  chk("print HTML: Roboto embedded", !is.null(font_b64), if (is.null(font_b64)) "font file missing: Arial" else NA, warn = TRUE)
  html <- country_memo_html(doc, font_b64, cfg$memo$page_size %||% "Letter")
  ext <- country_memo_html_external(html)
  chk("print HTML: no external reference", !length(ext), if (length(ext)) paste(utils::head(ext, 3), collapse = " ") else NA)
  pdf <- country_memo_pdf(html, edge, work)
  ast_path <- file.path(work, "memo.ast.json")
  country_memo_write_text(as.character(country_json(doc)), ast_path)
  docx_script <- file.path(repo_root, "scripts", "utils", "country_memo_docx.py")
  docx <- file.path(work, "memo.docx")
  country_memo_py(python, docx_script, c("--in", ast_path, "--out", docx))
  back <- tryCatch(country_memo_py(python, docx_script, c("--check", docx, "--ast", ast_path)), error = function(e) structure(conditionMessage(e), failed = TRUE))
  chk("DOCX: read-back paragraphs equal the document", is.null(attr(back, "failed")), if (!is.null(attr(back, "failed"))) substr(back, 1, 300) else NA)
  # the text comes back through a file: system2() splits a long stdout line (about 8 kB) into pieces
  pt_path <- file.path(work, "pdf_text.json")
  country_memo_py(python, docx_script, c("--pdf-text", pdf, "--out", pt_path))
  pt <- jsonlite::fromJSON(pt_path)
  norm <- function(x) gsub("[[:space:]]+", " ", gsub("­", "", x))
  text <- norm(pt$text)
  chk(sprintf("PDF: %d pages (2-4)", pt$pages), pt$pages >= 2 && pt$pages <= 4, NA, warn = placeholder)
  chk("PDF: contains the title and the label", grepl(norm(doc$title), text, fixed = TRUE) && grepl(norm(doc$label), text, fixed = TRUE))
  items <- doc$summary
  for (s in doc$sections) for (b in s$blocks) items <- c(items, if (identical(b$type, "ul")) b$items else list(b))
  vals <- unique(unlist(lapply(items, function(it) vapply(Filter(function(n) n$t == "value", it$inlines), function(n) n$v, ""))))
  # a line break can fall inside a value: compare with the spaces removed
  squash <- gsub(" ", "", text, fixed = TRUE)
  miss <- vals[!vapply(vals, function(v) grepl(gsub(" ", "", norm(v), fixed = TRUE), squash, fixed = TRUE), TRUE)]
  chk("PDF: every value of the document is in its text", !length(miss), if (length(miss)) paste(utils::head(miss, 5), collapse = " | ") else NA)
  md <- country_memo_markdown(doc)
  lim <- list(json = 25600, pdf = 409600, docx = 102400)
  jtxt <- as.character(country_json(doc))
  chk(sprintf("sizes: JSON %s B, PDF %s B, DOCX %s B (limits 25,600 / 409,600 / 102,400 gz)", format(nchar(jtxt, "bytes"), big.mark = ","),
              format(file.size(pdf), big.mark = ","), format(file.size(docx), big.mark = ",")),
      length(memCompress(charToRaw(enc2utf8(jtxt)), "gzip")) <= lim$json && file.size(pdf) <= lim$pdf && file.size(docx) <= lim$docx)
  list(pdf = pdf, docx = docx, md = md, json = jtxt, html = html, checks = do.call(rbind, checks))
}

# Publish rendered files into out_dir and return the manifest memo entry.
country_memo_publish <- function(iso3, r, out_dir, doc) {
  old <- list.files(out_dir, pattern = sprintf("^memo-%s\\.[0-9a-f]{8}\\.(json|pdf|docx|md)$", iso3))
  tmp <- tempfile("memo")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  jp <- file.path(tmp, "m.json"); country_memo_write_text(r$json, jp)
  mp <- file.path(tmp, "m.md"); country_memo_write_text(r$md, mp)
  names <- c(json = sprintf("memo-%s.%s.json", iso3, country_memo_md5_8(jp)), pdf = sprintf("memo-%s.%s.pdf", iso3, country_memo_md5_8(r$pdf)),
             docx = sprintf("memo-%s.%s.docx", iso3, country_memo_md5_8(r$docx)), md = sprintf("memo-%s.%s.md", iso3, country_memo_md5_8(mp)))
  file.copy(jp, file.path(out_dir, names[["json"]]), overwrite = TRUE)
  file.copy(r$pdf, file.path(out_dir, names[["pdf"]]), overwrite = TRUE)
  file.copy(r$docx, file.path(out_dir, names[["docx"]]), overwrite = TRUE)
  file.copy(mp, file.path(out_dir, names[["md"]]), overwrite = TRUE)
  stale <- setdiff(old, names)
  if (length(stale)) unlink(file.path(out_dir, stale))
  list(json = names[["json"]], pdf = names[["pdf"]], docx = names[["docx"]], md = names[["md"]], words = doc$words,
       generated_at = doc$generated_at, verified_at = doc$verified_at, facts_sha256 = doc$facts_sha256, stale = FALSE)
}

country_memo_write_manifest <- function(manifest, out_dir) {
  manifest$counts$memos <- sum(vapply(manifest$countries, function(c) !is.null(c$memo), TRUE))
  txt <- as.character(country_json(manifest))
  country_memo_write_text(txt, file.path(out_dir, "manifest.json"))
  # the memo entries grow the manifest after scripts/88's own size check: check it again (spec A10)
  gz <- length(memCompress(charToRaw(enc2utf8(txt)), "gzip"))
  lim <- COUNTRY_BUDGET_GZ[["manifest"]]
  message(sprintf("  %-4s manifest: gzip size %s bytes <= %s", if (gz <= lim) "PASS" else "WARN", format(gz, big.mark = ","), format(lim, big.mark = ",")))
  invisible(gz <= lim)
}

# A fact sheet of the published export with its companion drivers merged back.
country_memo_sheet <- function(entry, out_dir) {
  sheet <- jsonlite::fromJSON(file.path(out_dir, entry$file), simplifyVector = FALSE)
  if (!is.null(entry$drivers) && file.exists(file.path(out_dir, entry$drivers))) {
    sheet <- country_merge_drivers(sheet, jsonlite::fromJSON(file.path(out_dir, entry$drivers), simplifyVector = FALSE))
  }
  sheet
}

# Lint one memo source: writes lint.json and claims.json into dir; returns the pieces.
country_memo_lint_one <- function(src, sheet, cfg, names, dir) {
  parsed <- country_memo_parse(src, cfg)
  res <- country_memo_resolve(parsed, sheet, cfg)
  sents <- country_memo_sentences(res)
  claims <- country_memo_claims(res, sents, sheet, cfg)
  lint <- country_memo_lint(parsed, res, sents, claims, sheet, cfg, names)
  claims_doc <- country_memo_claims_doc(sheet$iso3, claims, sents, lint$triggered)
  claims_txt <- as.character(country_json(claims_doc, pretty = TRUE))
  claims_sha <- country_sha256(claims_txt)
  src_sha <- country_sha256(src)
  lint_doc <- list(schema = "osi-country-memo-lint", iso3 = sheet$iso3, src_sha256 = src_sha, facts_sha256 = sheet$checks$facts_sha256,
                   claims_sha256 = claims_sha, n_errors = lint$n_errors, n_warnings = lint$n_warnings, words = lint$words,
                   triggered_caveats = I(lint$triggered), items = lint$items)
  country_memo_write_text(claims_txt, file.path(dir, "claims.json"))
  country_memo_write_text(as.character(country_json(lint_doc, pretty = TRUE)), file.path(dir, "lint.json"))
  list(parsed = parsed, res = res, sents = sents, claims = claims, lint = lint, claims_doc = claims_doc, claims_sha = claims_sha, src_sha = src_sha)
}

country_render_memos_main <- function(args, repo_root, cfg, dict) {
  abs <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
  out_dir <- abs(country_memo_arg(args, "--out-dir", "dashboard/public/data/country"))
  memo_dir <- abs(country_memo_arg(args, "--memo-dir", "dashboard/country-memos"))
  lint_only <- "--lint-only" %in% args
  placeholder <- "--placeholder" %in% args
  force_stale <- "--force-stale" %in% args
  only <- country_memo_arg(args, "--iso3")
  only <- if (is.null(only)) NULL else toupper(trimws(strsplit(only, ",", fixed = TRUE)[[1]]))
  say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
  mpath <- file.path(out_dir, "manifest.json")
  if (!file.exists(mpath)) { message("No manifest in ", out_dir, ": run scripts/88 first."); return(1L) }
  manifest <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
  names <- vapply(manifest$countries, function(c) c$name, "")
  pos <- stats::setNames(seq_along(manifest$countries), vapply(manifest$countries, function(c) c$iso3, ""))
  edge <- if (lint_only) NA_character_ else country_memo_edge(args)
  python <- if (lint_only) NA_character_ else country_memo_python(args)
  if (!lint_only && (is.na(edge) || is.na(python))) {
    message("FAIL: rendering needs Edge (or Chrome) and Python with python-docx and pypdf: ",
            if (is.na(edge)) "no browser found (--edge or OSI_EDGE). " else "", if (is.na(python)) "no python with docx and pypdf (--python or OSI_PYTHON)." else "")
    return(1L)
  }
  fails <- 0L
  # ---- placeholder memos of the synthetic fixture ------------------------------------------------------
  if (placeholder) {
    for (k in seq_along(manifest$countries)) {
      e <- manifest$countries[[k]]
      if (is.null(e$memo) || (!is.null(only) && !e$iso3 %in% only)) next
      doc <- jsonlite::fromJSON(file.path(out_dir, e$memo$json), simplifyVector = FALSE)
      if (!isTRUE(doc$placeholder)) next
      work <- tempfile("memo")
      dir.create(work)
      r <- country_memo_render_files(doc, cfg, repo_root, edge, python, work, placeholder = TRUE)
      for (i in seq_len(nrow(r$checks))) message(sprintf("  %-4s %s %s%s", r$checks$status[i], e$iso3, r$checks$check[i], ifelse(is.na(r$checks$note[i]), "", paste0(" (", r$checks$note[i], ")"))))
      if (any(r$checks$status == "FAIL")) { fails <- fails + 1L; unlink(work, recursive = TRUE); next }
      entry <- country_memo_publish(e$iso3, r, out_dir, doc)
      manifest$countries[[k]]$memo <- utils::modifyList(e$memo, entry[c("json", "pdf", "docx", "md")])
      unlink(work, recursive = TRUE)
      say("Placeholder memo of ", e$iso3, " rendered (PDF, Word, Markdown)")
    }
    country_memo_write_manifest(manifest, out_dir)
    return(if (fails) 1L else 0L)
  }
  # ---- real memos ----------------------------------------------------------------------------------------
  isos <- if (!is.null(only)) only else basename(list.dirs(memo_dir, recursive = FALSE))
  isos <- isos[file.exists(file.path(memo_dir, isos, "memo.src.md"))]
  if (!length(isos)) { message("No memo sources (", memo_dir, "/<ISO3>/memo.src.md)."); return(if (is.null(only)) 0L else 1L) }
  if (!isTRUE(manifest$validated) && !lint_only) { message("FAIL: the manifest is not validated (an export with --no-validate-bundles or --synthetic): memos are not published from it."); return(1L) }
  for (iso in isos) {
    k <- pos[iso]
    if (is.na(k)) { message("FAIL ", iso, ": not in the manifest"); fails <- fails + 1L; next }
    e <- manifest$countries[[k]]
    if (!identical(e$tier, "core") || is.null(e$file)) { message("FAIL ", iso, ": memos are for core coalition members with a fact sheet"); fails <- fails + 1L; next }
    dir <- file.path(memo_dir, iso)
    sheet <- country_memo_sheet(e, out_dir)
    src <- country_memo_read_text(file.path(dir, "memo.src.md"))
    L <- country_memo_lint_one(src, sheet, cfg, names, dir)
    say(sprintf("%s: %d words, %d errors, %d warnings, %d claims, %d sentences; caveats triggered: %s", iso, L$lint$words$total, L$lint$n_errors,
                L$lint$n_warnings, length(L$claims), length(L$sents$sentences), paste(L$lint$triggered, collapse = ", ")))
    for (it in Filter(function(x) x$severity == "error", L$lint$items)) message("  ERROR ", it$id, " [", it$check, "] ", it$detail)
    if (lint_only) { if (L$lint$n_errors) fails <- fails + 1L; next }
    if (L$lint$n_errors) { message("FAIL ", iso, ": lint errors (see lint.json)"); fails <- fails + 1L; next }
    vpath <- file.path(dir, "verify.json")
    verify <- if (file.exists(vpath)) jsonlite::fromJSON(vpath, simplifyVector = FALSE) else NULL
    vc <- country_memo_verify_check(verify, L$claims_doc, L$lint, L$src_sha, sheet$checks$facts_sha256, L$claims_sha)
    if (!vc$ok) {
      if (!is.null(e$memo) && force_stale) {
        message("WARN ", iso, ": --force-stale: the published memo is re-marked current although its record does not match (", paste(vc$problems, collapse = "; "), ")")
        manifest$countries[[k]]$memo$stale <- FALSE
        next
      }
      if (!is.null(e$memo)) manifest$countries[[k]]$memo$stale <- TRUE
      message("FAIL ", iso, ": the verification record is not a complete pass: ", paste(vc$problems, collapse = "; "))
      fails <- fails + 1L
      next
    }
    if (vc$reuse) {
      verify$reused_for <- I(unique(c(unlist(verify$reused_for), sheet$checks$facts_sha256)))
      country_memo_write_text(as.character(country_json(verify, pretty = TRUE)), vpath)
      say(iso, ": claims unchanged on the new fact sheet: the sign-off is reused (B6.5)")
    }
    meta <- list(generated_at = format(file.mtime(file.path(dir, "memo.src.md")), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 verified_at = verify$verified_at %||% format(file.mtime(vpath), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 writer = verify$writer %||% "AI writer agent", verifier = verify$verifier %||% "AI verifier agent",
                 src_sha256 = L$src_sha, claims_sha256 = L$claims_sha)
    doc <- country_memo_document(L$res, L$claims, sheet, cfg, meta)
    work <- tempfile("memo")
    dir.create(work)
    r <- tryCatch(country_memo_render_files(doc, cfg, repo_root, edge, python, work), error = function(err) err)
    if (inherits(r, "error")) { message("FAIL ", iso, ": ", conditionMessage(r)); fails <- fails + 1L; unlink(work, recursive = TRUE); next }
    wc <- data.frame(check = sprintf("words: document %d = lint %d", doc$words, L$lint$words$total), status = if (doc$words == L$lint$words$total) "PASS" else "FAIL", note = NA)
    hc <- data.frame(check = "facts_sha256 of the document = the published sheet's", status = if (identical(doc$facts_sha256, sheet$checks$facts_sha256)) "PASS" else "FAIL", note = NA)
    checks <- rbind(r$checks, wc, hc)
    for (i in seq_len(nrow(checks))) message(sprintf("  %-4s %s", checks$status[i], checks$check[i]), if (!is.na(checks$note[i])) paste0(" (", checks$note[i], ")") else "")
    country_memo_write_text(as.character(country_json(doc, pretty = TRUE)), file.path(dir, "memo.ast.json"))
    country_memo_write_text(r$html, file.path(dir, "memo.print.html"))
    if (any(checks$status == "FAIL")) { message("FAIL ", iso, ": render checks"); fails <- fails + 1L; unlink(work, recursive = TRUE); next }
    manifest$countries[[k]]$memo <- country_memo_publish(iso, r, out_dir, doc)
    unlink(work, recursive = TRUE)
    say(iso, ": published ", manifest$countries[[k]]$memo$json)
  }
  if (!lint_only) country_memo_write_manifest(manifest, out_dir)
  if (fails) message(fails, " memo(s) failed")
  if (fails) 1L else 0L
}
