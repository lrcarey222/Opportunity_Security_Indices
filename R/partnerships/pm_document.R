# Partnership Memos: the print document. Converts the memo (pm_memo_sections()$markdown, the single source of
# the text) into the country-brief document AST (format "brief", R/dashboard/country/country_brief_doc.R), so
# the PDF, Word and Markdown renderings use the same renderer, print style and page geometry as the country
# strategy briefs (country_brief_html(), country_memo_pdf(), scripts/utils/country_memo_docx.py).
#
# Mapping: "## n. Title" -> a numbered section (the Executive Decision Summary is section 0, printed without a
# number); "### x" -> h3; a line that is wholly bold -> a bold paragraph; "- " lines -> a bullet list; "|" lines
# -> a table (tables longer than `max_rows` move to the appendix and leave a pointer); other lines -> paragraphs.
# Evidence tags: "[External E12, verified]" becomes a superscript source number (the item is listed under
# Sources with its URL, access date and verification); "[Data]" cites source 1 (the OSI tables); "[Inference]"
# and "[Recommendation]" stay as an italic lead-in. Pure functions.

PM_DOC_TAG_RE <- "\\[(Data|Inference|Recommendation|External)( [A-Za-z0-9-]+)?(, [a-z_]+)?\\]"

# Inline markdown (**bold**) to brief inline nodes.
pm_doc_inlines <- function(x) {
  parts <- regmatches(x, gregexpr("\\*\\*[^*]+\\*\\*", x), invert = NA)[[1]]
  out <- list()
  for (p in parts) {
    if (!nzchar(p)) next
    if (grepl("^\\*\\*.*\\*\\*$", p)) out[[length(out) + 1]] <- list(t = "strong", v = sub("^\\*\\*(.*)\\*\\*$", "\\1", p))
    else out[[length(out) + 1]] <- list(t = "text", v = gsub("`", "", p))
  }
  out
}

# One text item: strips the evidence tags into refs and an italic lead-in.
pm_doc_item <- function(x, src_index) {
  tags <- regmatches(x, gregexpr(PM_DOC_TAG_RE, x))[[1]]
  refs <- integer(); lead <- character()
  for (tg in tags) {
    kind <- sub("^\\[(\\w+).*$", "\\1", tg)
    if (kind == "External") {
      id <- trimws(sub("^\\[External ([A-Za-z0-9-]+).*$", "\\1", tg))
      if (!is.na(src_index[id])) refs <- c(refs, unname(src_index[id]))
    } else if (kind == "Data") {
      refs <- c(refs, 1L)
    } else lead <- c(lead, kind)
  }
  txt <- trimws(gsub("\\s{2,}", " ", gsub(PM_DOC_TAG_RE, "", x)))
  txt <- gsub(";(?=\\S)", "; ", txt, perl = TRUE)
  txt <- sub("^\\s*:\\s*", "", txt)
  inl <- pm_doc_inlines(txt)
  if (length(lead)) inl <- c(list(list(t = "em", v = paste0(lead[1], ". "))), inl)
  list(inlines = inl, refs = I(sort(unique(refs))))
}

pm_doc_split_row <- function(line) {
  z <- strsplit(sub("^\\|\\s?", "", sub("\\s?\\|\\s*$", "", line)), " | ", fixed = TRUE)[[1]]
  trimws(z)
}

# markdown lines -> list(sections, appendix_blocks). `src_index`: evidence id -> source number.
# A table wider than `max_cols` drops columns whose header matches `drop` (regexes, in order) until it fits:
# an over-wide table makes Edge shrink every page. The CSV outputs and the Markdown keep every column.
pm_doc_trim_cols <- function(head, max_cols, drop) {
  keep <- rep(TRUE, length(head))
  for (re in drop) {
    if (sum(keep) <= max_cols) break
    hit <- which(keep & grepl(re, head, ignore.case = TRUE))
    for (h in rev(hit)) { if (sum(keep) <= max_cols) break; keep[h] <- FALSE }
  }
  keep
}

pm_doc_parse <- function(lines, src_index, max_rows = 14, max_cols = 9, drop = character()) {
  sections <- list(); cur <- NULL; app <- list(); n_app <- 0L
  flush <- function() if (!is.null(cur)) sections[[length(sections) + 1]] <<- cur
  add_block <- function(b) cur$blocks[[length(cur$blocks) + 1]] <<- b
  i <- 1L; n <- length(lines)
  while (i <= n) {
    ln <- lines[i]
    if (grepl("^# ", ln) || grepl("^_", ln) || !nzchar(trimws(ln))) { i <- i + 1L; next }
    if (grepl("^## ", ln)) {
      flush()
      h <- sub("^## ", "", ln)
      num <- suppressWarnings(as.integer(sub("^(\\d+)\\..*$", "\\1", h)))
      cur <- list(n = if (is.na(num)) 0L else num, id = if (is.na(num)) "summary" else paste0("s", num),
                  title = if (is.na(num)) h else sub("^\\d+\\.\\s*", "", h), blocks = list())
      i <- i + 1L; next
    }
    if (is.null(cur)) { i <- i + 1L; next }
    if (grepl("^### ", ln)) { add_block(list(type = "h3", text = sub("^### ", "", ln))); i <- i + 1L; next }
    if (grepl("^\\|", ln)) {
      j <- i
      while (j <= n && grepl("^\\|", lines[j])) j <- j + 1L
      tl <- lines[i:(j - 1)]
      head <- pm_doc_split_row(tl[1])
      body <- tl[-c(1, 2)]
      # table cells: identifiers become breakable words (an unbreakable cell makes Edge shrink the whole page)
      cellify <- function(v) gsub(";(?=\\S)", "; ", gsub("_", " ", v), perl = TRUE)
      rows <- lapply(body, function(r) lapply(cellify(pm_doc_split_row(r)), function(cl) { it <- pm_doc_item(cl, src_index); it$refs <- I(integer()); it }))
      head <- cellify(head)
      keep <- pm_doc_trim_cols(head, max_cols, drop)
      trimmed <- !all(keep)
      head <- head[keep]
      rows <- lapply(rows, function(r) r[keep[seq_along(r)]])
      tab <- list(type = "table", n = 0L, head = I(head), rows = rows, refs = I(1L))
      if (trimmed) add_block(list(type = "p", inlines = list(list(t = "em", v = "Some columns of the next table are left out of the print edition; the Markdown and CSV outputs carry them all.")), refs = I(integer())))
      if (length(rows) > max_rows) {
        n_app <- n_app + 1L
        tid <- sprintf("A%d", n_app)
        add_block(list(type = "p", inlines = list(list(t = "em", v = sprintf("Table %s in the appendix (%d rows) carries this table in full; the CSV outputs carry every column.", tid, length(rows)))), refs = I(integer())))
        app[[length(app) + 1]] <- list(type = "h3", text = sprintf("Table %s. %s", tid, if (cur$n == 0L) cur$title else sprintf("Section %d, %s", cur$n, cur$title)))
        app[[length(app) + 1]] <- tab
      } else add_block(tab)
      i <- j; next
    }
    if (grepl("^- ", ln)) {
      j <- i
      while (j <= n && grepl("^- ", lines[j])) j <- j + 1L
      add_block(list(type = "ul", items = lapply(sub("^- ", "", lines[i:(j - 1)]), pm_doc_item, src_index = src_index)))
      i <- j; next
    }
    if (grepl("^\\*\\*[^*]+\\*\\*:?$", trimws(ln))) {
      add_block(list(type = "p", inlines = list(list(t = "strong", v = sub("^\\*\\*(.*)\\*\\*:?$", "\\1", trimws(ln)))), refs = I(integer())))
      i <- i + 1L; next
    }
    add_block(c(list(type = "p"), pm_doc_item(ln, src_index)))
    i <- i + 1L
  }
  flush()
  list(sections = sections, appendix = app)
}

# The four summary boxes (the briefs' choice boxes): build together, trade and standards, institutions, stay out.
pm_doc_boxes <- function(jb_text, trade_text, mea_text, nonp_text) {
  mk <- function(label, kind, txt) c(list(label = label, kind = kind), list(inlines = list(list(t = "text", v = txt)), refs = I(integer())))
  list(type = "choices", items = list(mk("Build together", "preserve", jb_text), mk("Trade and standards", "partner", trade_text),
                                      mk("Institutions", "accelerate", mea_text), mk("Stay out", "watch", nonp_text)))
}

# The document. `meta`: as_of (YYYY-MM-DD), as_of_label, generated_at, evidence_access, pair_id; `ev`: the
# evidence layer; `cfg_doc`: config partnership_memos.yml `document`.
pm_document <- function(markdown, ev, boxes, pair_cfg, cfg_doc, meta, osi_source) {
  # sources: 1 = the OSI tables; then every external item the text cites, in order of first citation
  cited <- unique(unlist(regmatches(markdown, gregexpr("(?<=\\[External )[A-Za-z0-9-]+", markdown, perl = TRUE))))
  cited <- cited[cited %in% ev$evidence_id]
  src_index <- stats::setNames(seq_along(cited) + 1L, cited)
  parsed <- pm_doc_parse(markdown, src_index, if (is.null(cfg_doc$max_table_rows)) 14 else cfg_doc$max_table_rows,
                         if (is.null(cfg_doc$max_table_cols)) 9 else cfg_doc$max_table_cols, unlist(cfg_doc$print_drop_columns))
  secs <- parsed$sections
  if (length(secs) && identical(secs[[1]]$n, 0L) && !is.null(boxes)) {
    b <- secs[[1]]$blocks
    secs[[1]]$blocks <- c(list(boxes), b)
  }
  ev_src <- lapply(seq_along(cited), function(k) {
    e <- ev[ev$evidence_id == cited[k], ][1, ]
    text <- sprintf("%s. %s. %s%s; %s; accessed %s; %s.", e$evidence_id, e$name, e$source,
                    if (nzchar(e$publication_date)) paste0(", ", e$publication_date) else "", if (nzchar(e$url)) e$url else "no URL",
                    e$access_date, e$verification)
    list(n = k + 1L, id = e$evidence_id, text = text)
  })
  srcs <- c(list(list(n = 1L, id = "osi", text = osi_source)), ev_src)
  fill <- list(pair = pair_cfg$title, as_of_label = meta$as_of_label, evidence_access = meta$evidence_access)
  sub_fill <- function(s) { for (k in names(fill)) s <- gsub(paste0("{", k, "}"), fill[[k]], s, fixed = TRUE); s }
  app_blocks <- c(list(list(type = "p", inlines = list(list(t = "text", v = cfg_doc$appendix_intro)), refs = I(integer()))), parsed$appendix)
  n_tab <- 0L
  number <- function(blocks) lapply(blocks, function(b) { if (identical(b$type, "table")) { n_tab <<- n_tab + 1L; b$n <- n_tab }; b })
  secs <- lapply(secs, function(s) { s$blocks <- number(s$blocks); s })
  app_blocks <- number(app_blocks)
  words <- sum(vapply(secs, function(s) sum(vapply(s$blocks, function(b) {
    txt <- if (!is.null(b$inlines)) paste(vapply(b$inlines, function(z) z$v, ""), collapse = " ")
           else if (!is.null(b$items)) paste(vapply(b$items, function(it) paste(vapply(it$inlines, function(z) z$v, ""), collapse = " "), ""), collapse = " ") else ""
    length(strsplit(trimws(txt), "\\s+")[[1]]) }, numeric(1))), numeric(1)))
  list(
    schema = "osi-partnership-memo", schema_version = 1L, format = "brief", format_version = 1L,
    iso3 = meta$pair_id, pair = meta$pair_id, title = pair_cfg$title, subtitle = sub_fill(cfg_doc$subtitle), eyebrow = sub_fill(cfg_doc$eyebrow),
    label = sub_fill(cfg_doc$label), disclosure = sub_fill(cfg_doc$disclosure), as_of = meta$as_of, as_of_label = meta$as_of_label,
    generated_at = meta$generated_at, verified_at = NULL, writer = "rules (R/partnerships)", verifier = "not independently verified",
    words = as.integer(words), words_tables = 0L, evidence_access = meta$evidence_access,
    summary = list(), sections = secs, appendix = list(title = sub_fill(cfg_doc$appendix_title), blocks = app_blocks), sources = srcs,
    footer = sub_fill(cfg_doc$footer), doc_kind = cfg_doc$doc_kind, keywords = cfg_doc$keywords
  )
}
