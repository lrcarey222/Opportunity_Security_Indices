# Country strategy briefs ("[Country] and the Allied Industrial Base"): parse the writer's source
# (dashboard/country-memos/<ISO3>/brief.src.md), resolve its {{tokens}} against the brief facts
# (country_brief.R), split sentences, build the claims file (one row per token occurrence with its value,
# meaning and sources), lint, and build the document: the AST the screen, the PDF, the Word file and the
# Markdown are rendered from, so all four carry the same words. Pure: text and lists in, text and lists out
# (scripts/92_export_country_briefs.R does the IO). Shares the inline parser, the dot-path lookup and the
# word count of the legacy memos (country_memo_lint.R, country_memo.R).
#
# Source grammar (a Markdown subset):
#   ## <title>            the sections of config/country_brief.yml brief.sections, verbatim and in order,
#                         then optionally "## <appendix_title>"
#   ### <heading>         a sub-heading (plain text, no tokens)
#   paragraph lines       joined until a blank line
#   - item                a bullet list (indented continuation lines allowed)
#   :::choices ... :::    the box of strategic choices: one "- **Label.** text" item per choice
#   | a | b |             a pipe table: header row, a |---| separator row, body rows (cells take inline syntax)
#   @figure <id>          a figure of brief.figures; the lines after it, to a blank line, are its caption
#   @table <id>           a generated table of brief.tables (country_brief_generated_tables(): its cells are
#                         facts, so they carry claims; expanded by country_brief_expand_tables())
# Inline: **strong**, *em*, {{path}} and {{path|filter}}, filters lower, cap (capital first letter), n (the
# count of a list) and nw (a count up to twelve in words).

COUNTRY_BRIEF_FILTERS <- c("lower", "cap", "n", "nw")
COUNTRY_BRIEF_FORMAT <- "brief"
COUNTRY_BRIEF_FORMAT_VERSION <- 1L
COUNTRY_BRIEF_NUMBER_WORDS <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
# the colour key of a choice-box label (its first word)
COUNTRY_BRIEF_BOX_KIND <- c(Secure = "secure", Build = "preserve", Preserve = "preserve", Partner = "partner", Deploy = "accelerate")

# The section titles of a brief with its names filled in ({country}, {country_poss}).
country_brief_titles <- function(cfg, fill) vapply(cfg$brief$sections, function(s) country_brief_cap1(country_fill(s$title, fill)), "")

# ---- Parse -------------------------------------------------------------------------------------------

# Parse a brief source. Returns list(sections = list(list(n, id, title, blocks)), appendix = NULL |
# list(title, blocks), errors). Blocks hold raw text: p (text), ul (items), h3 (text), choices (items),
# table (head, rows), figure (id, caption).
country_brief_parse <- function(md, cfg, fill) {
  lines <- strsplit(gsub("\r\n?", "\n", md), "\n", fixed = TRUE)[[1]]
  titles <- country_brief_titles(cfg, fill)
  ids <- vapply(cfg$brief$sections, function(s) s$id, "")
  app_title <- cfg$brief$appendix_title
  errors <- character()
  err <- function(...) errors <<- c(errors, paste0(...))
  secs <- list()
  app <- NULL
  cur <- NULL
  blocks <- list()
  para <- character()
  items <- NULL
  table <- NULL
  caption <- NULL
  choices <- NULL
  add <- function(b) blocks[[length(blocks) + 1]] <<- b
  flush <- function() {
    if (length(para)) { add(list(type = "p", text = paste(para, collapse = " "))); para <<- character() }
    if (!is.null(items)) { add(list(type = "ul", items = items)); items <<- NULL }
    if (!is.null(table)) {
      if (length(table) < 2 || !grepl("^\\|?\\s*:?-{3,}", table[2])) err("a table needs a header row and a |---| separator row: ", substr(table[1], 1, 60))
      else {
        # split on the pipes outside {{tokens}} (a token's filter is written {{path|filter}})
        cells <- function(l) {
          l <- sub("^\\|", "", sub("\\|\\s*$", "", trimws(l)))
          m <- gregexpr("\\{\\{[^{}]*\\}\\}", l, perl = TRUE)
          regmatches(l, m) <- lapply(regmatches(l, m), function(x) gsub("|", "\003", x, fixed = TRUE))
          gsub("\003", "|", trimws(strsplit(l, "|", fixed = TRUE)[[1]]), fixed = TRUE)
        }
        head <- cells(table[1])
        rows <- lapply(table[-(1:2)], cells)
        bad <- which(lengths(rows) != length(head))
        if (length(bad)) err("table row ", bad[1], " has ", length(rows[[bad[1]]]), " cells, the header ", length(head), ": ", substr(table[1], 1, 60))
        add(list(type = "table", head = head, rows = rows))
      }
      table <<- NULL
    }
    if (!is.null(caption)) { add(list(type = "figure", id = caption$id, caption = paste(caption$lines, collapse = " "))); caption <<- NULL }
  }
  close_section <- function() {
    flush()
    if (is.null(cur)) return(invisible())
    if (identical(cur$id, "appendix")) app <<- list(title = cur$title, blocks = blocks)
    else secs[[length(secs) + 1]] <<- c(cur, list(blocks = blocks))
    blocks <<- list()
  }
  for (k in seq_along(lines)) {
    l <- lines[k]
    t <- trimws(l)
    if (!is.null(choices)) {
      if (identical(t, ":::")) { add(list(type = "choices", items = choices)); choices <- NULL }
      else if (startsWith(t, "- ")) choices <- c(choices, substring(t, 3))
      else if (nzchar(t)) { if (!length(choices)) err("line ", k, ": a choices box holds '- ' items only") else choices[length(choices)] <- paste(choices[length(choices)], t) }
      next
    }
    if (startsWith(t, "## ")) {
      close_section()
      title <- trimws(substring(t, 4))
      if (identical(title, app_title)) cur <- list(n = NA_integer_, id = "appendix", title = title)
      else {
        n <- length(secs) + 1L
        if (!is.null(app)) err("line ", k, ": a section after the appendix")
        if (n > length(titles)) err("line ", k, ": more sections than the ", length(titles), " of the brief: '", title, "'")
        else if (!identical(title, titles[n])) err("line ", k, ": section ", n, " must be titled '", titles[n], "', not '", title, "'")
        cur <- list(n = n, id = if (n <= length(ids)) ids[n] else paste0("s", n), title = title)
      }
      next
    }
    if (is.null(cur)) { if (nzchar(t)) err("line ", k, ": text before the first section"); next }
    if (startsWith(t, "### ")) { flush(); add(list(type = "h3", text = trimws(substring(t, 5)))); next }
    if (startsWith(t, "#")) { err("line ", k, ": only '## ' and '### ' headings"); next }
    if (identical(t, ":::choices")) { flush(); choices <- character(); next }
    if (startsWith(t, ":::")) { err("line ", k, ": unknown block '", t, "'"); next }
    if (startsWith(t, "@figure ")) { flush(); caption <- list(id = trimws(substring(t, 9)), lines = character()); next }
    if (startsWith(t, "@table ")) { flush(); add(list(type = "table", gen = trimws(substring(t, 8)), head = character(), rows = list())); next }
    if (!nzchar(t)) { flush(); next }
    if (!is.null(caption)) { caption$lines <- c(caption$lines, t); next }
    if (startsWith(t, "|")) {
      if (length(para) || !is.null(items)) { tb <- table; table <- NULL; flush(); table <- tb }
      table <- c(table, t)
      next
    }
    if (grepl("^[0-9]+[.)] ", t) || startsWith(t, ">") || startsWith(t, "```") || grepl("!\\[|\\]\\(|<[a-zA-Z/]", t)) { err("line ", k, ": not allowed (numbered lists, quotes, code, images, links, HTML)"); next }
    if (startsWith(t, "- ") || startsWith(t, "* ")) {
      if (length(para)) { add(list(type = "p", text = paste(para, collapse = " "))); para <- character() }
      items <- c(items, substring(t, 3))
      next
    }
    if (!is.null(items) && grepl("^\\s+", l)) { items[length(items)] <- paste(items[length(items)], t); next }
    if (!is.null(items)) { add(list(type = "ul", items = items)); items <- NULL }
    para <- c(para, t)
  }
  if (!is.null(choices)) err("an unclosed :::choices box")
  close_section()
  if (length(secs) < length(titles)) err(sprintf("%d of the %d sections: missing '%s'", length(secs), length(titles), titles[length(secs) + 1]))
  list(sections = secs, appendix = app, errors = errors)
}

# Fill the generated tables of a parsed brief (blocks with `gen`) from `tables` (id -> list(head, rows));
# an unknown id is an error.
country_brief_expand_tables <- function(parsed, tables) {
  fill <- function(blocks) lapply(blocks, function(b) {
    if (!identical(b$type, "table") || is.null(b$gen)) return(b)
    t <- tables[[b$gen]]
    if (is.null(t)) { parsed$errors <<- c(parsed$errors, sprintf("unknown generated table '%s'", b$gen)); return(b) }
    b$head <- t$head
    b$rows <- t$rows
    b
  })
  parsed$sections <- lapply(parsed$sections, function(s) { s$blocks <- fill(s$blocks); s })
  if (!is.null(parsed$appendix)) parsed$appendix$blocks <- fill(parsed$appendix$blocks)
  parsed
}

# ---- Resolve -------------------------------------------------------------------------------------------

# The display of a token: list(ok, d, v, src, m, error). A fact is an Fv (list with d) or a name list
# (d, n); a bare string prints as itself and carries no source.
country_brief_value <- function(facts, path, filter = NA_character_) {
  x <- country_memo_get(facts, path)
  bad <- function(e) list(ok = FALSE, error = e)
  if (is.null(x)) return(bad("does not resolve"))
  if (is.list(x) && !is.null(x$d)) { d <- as.character(x$d); v <- x$v; src <- unlist(x$src); m <- x$m; n <- x$n }
  else if (is.character(x) && length(x) == 1) { d <- x; v <- x; src <- character(); m <- NULL; n <- NULL }
  else return(bad("is not a printable fact (an object without a display string)"))
  if (!nzchar(d) && !(filter %in% c("n", "nw"))) return(bad("is empty (a list with no items)"))
  if (!is.na(filter)) {
    cnt <- n %||% (if (is.numeric(v) && length(v) == 1) v else length(v))
    d <- switch(filter,
      lower = tolower(d),
      cap = country_brief_cap1(d),
      n = as.character(cnt),
      nw = if (is.numeric(cnt) && cnt == round(cnt) && cnt >= 0 && cnt <= 12) COUNTRY_BRIEF_NUMBER_WORDS[cnt + 1] else as.character(cnt))
  }
  list(ok = TRUE, d = d, v = v, src = as.character(src %||% character()), m = m %||% NA_character_)
}

# The steps a token carries: a cell's key, a list's or count's stored keys (k), a development row's or step's
# key, or a shared capability's steps.
country_brief_token_keys <- function(facts, path) {
  seg <- strsplit(path, ".", fixed = TRUE)[[1]]
  x <- country_memo_get(facts, path)
  k <- if (is.list(x)) unlist(x$k) else NULL
  if (length(seg) >= 2 && seg[1] == "cells") k <- c(k, seg[2])
  if (length(seg) >= 3 && seg[1] == "dev" && seg[2] == "rows") k <- c(k, unlist(facts$dev$rows[[seg[3]]]$key$v))
  if (length(seg) >= 3 && seg[1] == "dev" && seg[2] == "by_step") k <- c(k, seg[3])
  if (length(seg) >= 2 && seg[1] == "shared") k <- c(k, unlist(facts$shared[[seg[2]]]$steps$k))
  unique(as.character(k))
}

# Resolve every text unit of a parsed brief. A unit is one paragraph, list item, choice item, table cell,
# heading or caption: list(section, sid, block, kind, row, col, nodes, rendered, masked). `masked` has each
# token's display replaced by X's of the same length, so lint and sentence splitting skip the data.
country_brief_resolve <- function(parsed, facts) {
  units <- list()
  tokens <- list()
  errors <- character()
  unit <- function(text, section, sid, block, kind, row = NA_integer_, col = NA_integer_) {
    pr <- country_memo_inlines(text, COUNTRY_BRIEF_FILTERS)
    if (length(pr$errors)) errors <<- c(errors, paste0(sid, ": ", pr$errors))
    nodes <- list()
    rendered <- ""
    masked <- ""
    for (n in pr$nodes) {
      if (n$t == "token") {
        r <- country_brief_value(facts, n$path, n$filter)
        k <- length(tokens) + 1L
        tokens[[k]] <<- list(token = k, path = n$path, filter = n$filter, raw = n$raw, ok = r$ok, d = r$d %||% NA_character_, v = r$v,
                             src = r$src %||% character(), m = r$m %||% NA_character_, section = section, sid = sid, error = r$error %||% NULL,
                             unit = length(units) + 1L, keys = if (isTRUE(r$ok)) country_brief_token_keys(facts, n$path) else character())
        if (!isTRUE(r$ok)) { errors <<- c(errors, sprintf("%s: %s %s", sid, n$raw, r$error)); d <- n$raw } else d <- r$d
        nodes[[length(nodes) + 1]] <- list(t = "value", v = d, path = n$path, token = k)
        rendered <- paste0(rendered, d)
        masked <- paste0(masked, strrep("X", nchar(d)))
      } else {
        nodes[[length(nodes) + 1]] <- list(t = n$t, v = n$v)
        rendered <- paste0(rendered, n$v)
        masked <- paste0(masked, n$v)
      }
    }
    units[[length(units) + 1]] <<- list(section = section, sid = sid, block = block, kind = kind, row = row, col = col, nodes = nodes,
                                        rendered = rendered, masked = masked)
  }
  walk <- function(blocks, section, sid) {
    for (b in seq_along(blocks)) {
      x <- blocks[[b]]
      switch(x$type,
        p = unit(x$text, section, sid, b, "p"),
        ul = for (it in x$items) unit(it, section, sid, b, "li"),
        choices = for (it in x$items) unit(it, section, sid, b, "choice"),
        h3 = unit(x$text, section, sid, b, "h3"),
        figure = unit(x$caption, section, sid, b, "caption"),
        table = {
          # the cells of a generated table are facts laid out by the generator: kind gtd (checked as data)
          kd <- if (is.null(x$gen)) "td" else "gtd"
          for (j in seq_along(x$head)) unit(x$head[j], section, sid, b, "th", 0L, j)
          for (i in seq_along(x$rows)) for (j in seq_along(x$rows[[i]])) unit(x$rows[[i]][j], section, sid, b, kd, i, j)
        })
    }
  }
  for (s in parsed$sections) walk(s$blocks, s$n, s$id)
  if (!is.null(parsed$appendix)) walk(parsed$appendix$blocks, 0L, "appendix")
  list(units = units, tokens = tokens, errors = errors)
}

# Sentences of every unit (split after . ? ! followed by a space, outside tokens): ids s<section>.<n>
# (a.<n> in the appendix). Returns list(sentences, tok_sent).
country_brief_sentences <- function(res) {
  out <- list()
  count <- list()
  tok_sent <- character(length(res$tokens))
  for (ui in seq_along(res$units)) {
    u <- res$units[[ui]]
    key <- if (u$section == 0L) "a" else paste0("s", u$section)
    m <- u$masked
    ends <- gregexpr("[.?!](?=\\s|$)", m, perl = TRUE)[[1]]
    ends <- sort(unique(c(if (ends[1] > 0) as.integer(ends) else integer(), nchar(m))))
    # where each token's display sits in the unit text
    pos <- 1L
    tpos <- integer()
    for (n in u$nodes) { if (n$t == "value") tpos[as.character(n$token)] <- pos; pos <- pos + nchar(n$v) }
    st <- 1L
    for (e in ends) {
      if (e < st) next
      txt <- trimws(substr(u$rendered, st, e))
      msk <- trimws(substr(m, st, e))
      if (nzchar(gsub("[[:space:][:punct:]]", "", msk))) {
        count[[key]] <- (count[[key]] %||% 0L) + 1L
        sid <- sprintf("%s.%d", key, count[[key]])
        tk <- as.integer(names(tpos)[tpos >= st & tpos <= e])
        tok_sent[tk] <- sid
        out[[length(out) + 1]] <- list(id = sid, section = u$section, unit = ui, kind = u$kind, text = txt, masked = msk, tokens = tk)
      }
      st <- e + 1L
    }
  }
  list(sentences = out, tok_sent = tok_sent)
}

# One claim per token occurrence: id, section, sentence id and text, path, filter, value, display,
# sources and meaning.
country_brief_claims <- function(res, sents) {
  st <- stats::setNames(vapply(sents$sentences, function(s) s$text, ""), vapply(sents$sentences, function(s) s$id, ""))
  lapply(seq_along(res$tokens), function(k) {
    t <- res$tokens[[k]]
    sid <- sents$tok_sent[k]
    list(id = sprintf("c%d", k), token = k, section = t$section, sentence_id = sid, sentence = unname(st[sid]) %||% NA_character_,
         path = t$path, filter = if (is.na(t$filter)) NULL else t$filter, v = if (is.list(t$v)) unlist(t$v) else t$v, d = t$d,
         source = I(t$src), meaning = t$m)
  })
}

# The claims file of a brief: its sha256 is taken over this canonical JSON (no facts hash, so a re-export
# that changes nothing a claim says keeps the sign-off).
country_brief_claims_doc <- function(iso3, claims, sents) {
  list(schema = "osi-country-brief-claims", iso3 = iso3,
       claims = lapply(claims, function(c) { c$token <- NULL; c }),
       sentences = lapply(sents$sentences, function(s) list(id = s$id, section = s$section, kind = s$kind, text = s$text)))
}

# ---- Lint ----------------------------------------------------------------------------------------------

country_brief_has <- function(text, term, ignore_case = FALSE) {
  re <- paste0("(?<![A-Za-z0-9])", gsub("([][{}()+*^$|\\\\.?-])", "\\\\\\1", term), "(?![A-Za-z0-9])")
  grepl(re, text, perl = TRUE, ignore.case = ignore_case)
}

# The words of the main text (paragraphs, list items and the choice box of the numbered sections; not
# headings, tables, captions or the appendix) and of the tables.
country_brief_word_counts <- function(res) {
  main <- Filter(function(u) u$section > 0 && u$kind %in% c("p", "li", "choice"), res$units)
  tabs <- Filter(function(u) u$section > 0 && u$kind %in% c("th", "td", "gtd"), res$units)
  by_sec <- tapply(vapply(main, function(u) country_memo_words_of(u$rendered), 0), vapply(main, function(u) u$section, 0L), sum)
  list(main = sum(vapply(main, function(u) country_memo_words_of(u$rendered), 0)),
       tables = sum(vapply(tabs, function(u) country_memo_words_of(u$rendered), 0)),
       sections = by_sec)
}

# Lint a resolved brief. Errors stop publication; warnings must be cleared by the verifier. `names`:
# economy names a sentence should cite through a token; `review_keys`: the steps whose calls are review cases
# (country_brief_review()), which the Bottom line and the Agenda may not cite (17A-B) through any token that
# carries them (a cell, a list, a development row or step, a shared capability) or by name in plain text
# (`review_names`: key -> display name); a count that includes one is a warning; `dev_empty`: a donor
# with no credible development pair, whose development section must say so in the configured words.
# Returns list(items, n_errors, n_warnings, words).
country_brief_lint <- function(parsed, res, sents, cfg, fill, names = character(), review_keys = character(), dev_empty = FALSE,
                               review_names = character(), n_high = NULL) {
  items <- list()
  add <- function(severity, check, detail, section = NA, sentence_id = NA) {
    items[[length(items) + 1]] <<- list(id = sprintf("l%d", length(items) + 1), severity = severity, check = check, detail = detail,
                                        section = section, sentence_id = sentence_id)
  }
  lc <- cfg$lint
  for (e in parsed$errors) add("error", "structure", e)
  for (e in res$errors) add("error", "token", e)
  # required figures, in their sections
  for (fid in names(cfg$brief$figures)) {
    want <- cfg$brief$figures[[fid]]$section
    hit <- Filter(function(s) any(vapply(s$blocks, function(b) identical(b$type, "figure") && identical(b$id, fid), TRUE)), parsed$sections)
    if (!length(hit)) { if (!isFALSE(cfg$brief$figures[[fid]]$required)) add("error", "structure", sprintf("figure '%s' is missing", fid)) }
    else if (!identical(hit[[1]]$id, want)) add("error", "structure", sprintf("figure '%s' must sit in section '%s'", fid, want))
  }
  # the generated tables: required ones present, each in its section
  where_gen <- function(blocks) as.character(unlist(lapply(blocks, function(b) if (identical(b$type, "table") && !is.null(b$gen)) b$gen else NULL)))
  gen_at <- c(unlist(lapply(parsed$sections, function(s) stats::setNames(where_gen(s$blocks), rep(s$id, length(where_gen(s$blocks)))))),
              if (!is.null(parsed$appendix)) stats::setNames(where_gen(parsed$appendix$blocks), rep("appendix", length(where_gen(parsed$appendix$blocks)))))
  for (tid in names(cfg$brief$tables)) {
    tc <- cfg$brief$tables[[tid]]
    at <- names(gen_at)[gen_at == tid]
    if (!length(at)) { if (isTRUE(tc$required)) add("error", "structure", sprintf("generated table '%s' is missing", tid)) }
    else if (!identical(at[1], tc$section)) add("error", "structure", sprintf("generated table '%s' must sit in section '%s'", tid, tc$section))
  }
  for (s in parsed$sections) for (b in s$blocks) if (identical(b$type, "figure") && !b$id %in% names(cfg$brief$figures)) add("error", "structure", sprintf("unknown figure '%s'", b$id), s$n)
  # the choice box in the bottom line, and the High-fence table (the first table after a "High fence" heading)
  s1 <- Filter(function(s) identical(s$id, "bottom_line"), parsed$sections)
  if (length(s1) && !any(vapply(s1[[1]]$blocks, function(b) identical(b$type, "choices"), TRUE))) add("error", "structure", "the bottom line needs its :::choices box")
  s2 <- Filter(function(s) identical(s$id, "fences"), parsed$sections)
  if (length(s2)) {
    bl <- s2[[1]]$blocks
    h <- which(vapply(bl, function(b) identical(b$type, "h3") && startsWith(b$text, "High fence"), TRUE))
    tb <- if (length(h)) which(vapply(bl, function(b) identical(b$type, "table"), TRUE)) else integer()
    tb <- tb[tb > (h[1] %||% Inf)]
    rng <- unlist(cfg$brief$high_fence_rows)
    # an economy with fewer High-fence capabilities than the minimum (n_high: facts counts.high_capabilities)
    # lists them all; with none, the sub-section says so in text and needs no table
    if (!is.null(n_high)) rng[1] <- min(rng[1], n_high)
    if (!length(h)) add("error", "structure", "section 2 needs a '### High fence' sub-section")
    else if (!length(tb) && !identical(as.integer(n_high), 0L)) add("error", "structure", "the High fence sub-section needs its table")
    else if (!length(tb)) NULL
    else {
      nr <- length(bl[[tb[1]]]$rows)
      if (nr < rng[1] || nr > rng[2]) add("error", "structure", sprintf("the High-fence table has %d rows (%d-%d)", nr, rng[1], rng[2]), 2)
    }
  }
  # the headlines (Bottom line, Agenda) never rest on a review case
  heads <- c("bottom_line", "agenda")
  if (length(review_keys)) for (tk in res$tokens) {
    if (!tk$sid %in% heads) next
    hit <- intersect(tk$keys %||% character(), review_keys)
    if (!length(hit)) next
    is_count <- !is.na(tk$filter) && tk$filter %in% c("n", "nw") || grepl("^counts[.]", tk$path)
    add(if (is_count) "warning" else "error", "headline_review",
        sprintf("%s cites %s (%s), %s a review case (insufficient evidence)", tk$sid, tk$raw, paste(hit, collapse = ", "), if (is_count) "a count that includes" else "which carries"), tk$section)
  }
  rn <- review_names[names(review_names) %in% review_keys & nzchar(review_names)]
  if (length(rn)) for (u in res$units) {
    if (!u$sid %in% heads) next
    low <- tolower(u$masked)
    for (k in names(rn)) if (grepl(tolower(rn[[k]]), low, fixed = TRUE))
      add("error", "headline_review", sprintf("%s names %s in plain text, a review case (insufficient evidence)", u$sid, rn[[k]]), u$section)
  }
  if (isTRUE(dev_empty)) {
    sd <- Filter(function(s) identical(s$id, "development"), parsed$sections)
    txt <- if (length(sd)) paste(vapply(sd[[1]]$blocks, function(b) b$text %||% "", ""), collapse = " ") else ""
    if (!grepl("No credible development-led supply solution identified.", txt, fixed = TRUE)) add("error", "structure", "a donor with no credible development pair: the development section must say 'No credible development-led supply solution identified.'")
  }
  digit_ok <- lc$digit_ok %||% "$^"
  for (s in sents$sentences) {
    m <- gsub(digit_ok, "", s$masked, perl = TRUE)
    sec <- s$section
    if (grepl("[0-9]", m)) add("error", "digit", sprintf("a digit outside a token: \"%s\"", substr(s$text, 1, 90)), sec, s$id)
    for (w in unlist(lc$banned)) if (country_brief_has(s$masked, w)) add("error", "banned", sprintf("'%s' in \"%s\"", w, substr(s$text, 1, 90)), sec, s$id)
    for (w in unlist(lc$avoid)) if (country_brief_has(s$masked, w, TRUE)) add("warning", "avoid", sprintf("'%s' in \"%s\"", w, substr(s$text, 1, 90)), sec, s$id)
    if (s$kind %in% c("p", "li", "choice", "td", "caption")) {
      nw <- unlist(lc$number_words)
      hit <- nw[vapply(nw, function(w) country_brief_has(s$masked, w, TRUE), TRUE)]
      if (length(hit)) add("warning", "number_word", sprintf("'%s' outside a token: \"%s\"", paste(hit, collapse = "', '"), substr(s$text, 1, 90)), sec, s$id)
      n_w <- country_memo_words_of(s$text)
      if (n_w > (cfg$brief$sentence_max_words %||% 40)) add("warning", "long_sentence", sprintf("%d words: \"%s\"", n_w, substr(s$text, 1, 90)), sec, s$id)
      # the brief's own economy, in running text ("the United States") or as the names list holds it
      own <- c(fill$country, sub("^the ", "", fill$country))
      nm <- setdiff(names, own)
      hitn <- nm[vapply(nm, function(w) country_brief_has(s$masked, w), TRUE)]
      if (length(hitn)) add("warning", "economy_name", sprintf("'%s' not from a token: \"%s\"", paste(hitn, collapse = "', '"), substr(s$text, 1, 90)), sec, s$id)
    }
  }
  wc <- country_brief_word_counts(res)
  hard <- unlist(cfg$brief$words_total_hard)
  soft <- unlist(cfg$brief$words_total)
  if (wc$main < hard[1] || wc$main > hard[2]) add("error", "words", sprintf("main text %d words (hard bounds %d-%d)", wc$main, hard[1], hard[2]))
  else if (wc$main < soft[1] || wc$main > soft[2]) add("warning", "words", sprintf("main text %d words (aim %d-%d)", wc$main, soft[1], soft[2]))
  sev <- vapply(items, function(x) x$severity, "")
  list(items = items, n_errors = sum(sev == "error"), n_warnings = sum(sev == "warning"), words = wc$main, words_tables = wc$tables,
       words_sections = as.list(wc$sections))
}

# ---- Document ------------------------------------------------------------------------------------------

# The brief document (the AST of every rendering). `meta`: generated_at, verified_at, writer, verifier,
# src_sha256, claims_sha256, facts_sha256, as_of. `figure_files`: id -> list(svg, png) published names.
country_brief_document <- function(parsed, res, claims, facts, cfg, fill, meta, figure_files = list()) {
  claim_of <- vapply(claims, function(c) c$id, "")
  order_src <- character()
  for (u in res$units) for (n in u$nodes) if (n$t == "value") for (s in res$tokens[[n$token]]$src) if (!s %in% order_src) order_src <- c(order_src, s)
  src_n <- stats::setNames(seq_along(order_src), order_src)
  conv_nodes <- function(nodes) {
    refs <- integer()
    inl <- lapply(nodes, function(n) {
      if (n$t != "value") return(list(t = n$t, v = n$v))
      tk <- res$tokens[[n$token]]
      sn <- unname(src_n[tk$src])
      refs <<- c(refs, sn)
      list(t = "value", v = n$v, path = n$path, claim = claim_of[n$token], src = if (length(sn)) sn[1] else NULL)
    })
    list(inlines = inl, refs = I(sort(unique(refs))))
  }
  units_of <- function(section, block) Filter(function(u) u$section == section && u$block == block, res$units)
  n_fig <- 0L
  n_tab <- 0L
  conv_blocks <- function(blocks, section) {
    out <- list()
    for (b in seq_along(blocks)) {
      x <- blocks[[b]]
      us <- units_of(section, b)
      out[[length(out) + 1]] <- switch(x$type,
        p = c(list(type = "p"), conv_nodes(us[[1]]$nodes)),
        ul = list(type = "ul", items = lapply(us, function(u) conv_nodes(u$nodes))),
        h3 = list(type = "h3", text = us[[1]]$rendered),
        choices = list(type = "choices", items = lapply(us, function(u) {
          nd <- u$nodes
          label <- if (length(nd) && nd[[1]]$t == "strong") sub("[.:]\\s*$", "", nd[[1]]$v) else ""
          if (nzchar(label)) { nd <- nd[-1]; if (length(nd) && nd[[1]]$t == "text") nd[[1]]$v <- sub("^\\s+", "", nd[[1]]$v) }
          kind <- unname(COUNTRY_BRIEF_BOX_KIND[sub("[^A-Za-z].*$", "", label)]) %||% "watch"
          c(list(label = label, kind = kind), conv_nodes(nd))
        })),
        table = {
          n_tab <<- n_tab + 1L
          head <- Filter(function(u) identical(u$kind, "th"), us)
          body <- Filter(function(u) u$kind %in% c("td", "gtd"), us)
          # a table carries its source numbers once, under it, not on every cell
          trefs <- integer()
          rows <- lapply(seq_along(x$rows), function(i) lapply(Filter(function(u) identical(u$row, i), body), function(u) {
            cl <- conv_nodes(u$nodes)
            trefs <<- c(trefs, unlist(cl$refs))
            cl$refs <- I(integer())
            cl
          }))
          list(type = "table", n = n_tab, id = x$gen %||% NULL, head = I(vapply(head, function(u) u$rendered, "")), rows = rows, refs = I(sort(unique(trefs))))
        },
        figure = {
          n_fig <<- n_fig + 1L
          f <- cfg$brief$figures[[x$id]]
          ff <- figure_files[[x$id]] %||% list()
          c(list(type = "figure", id = x$id, n = n_fig, title = country_brief_cap1(country_fill(f$title %||% x$id, fill)), svg = ff$svg %||% NULL, png = ff$png %||% NULL,
                 alt = ff$alt %||% NULL), list(caption = conv_nodes(us[[1]]$nodes)))
        })
    }
    out
  }
  sections <- lapply(parsed$sections, function(s) list(n = s$n, id = s$id, title = s$title, blocks = conv_blocks(s$blocks, s$n)))
  appendix <- if (is.null(parsed$appendix)) NULL else list(title = parsed$appendix$title, blocks = conv_blocks(parsed$appendix$blocks, 0L))
  srcs <- lapply(seq_along(order_src), function(i) {
    id <- order_src[i]
    s <- cfg$sources[[id]] %||% list(label = id)
    ed <- country_fill(s$edition %||% "", facts$meta$editions %||% list())
    text <- country_memo_source_edition(s$label, if (nzchar(ed)) ed else NULL)
    if (!is.null(s$licence) && nzchar(s$licence %||% "")) text <- paste0(text, "; ", s$licence)
    if (!is.null(s$url) && nzchar(s$url %||% "")) text <- paste0(text, "; ", s$url)
    list(n = i, id = id, text = paste0(text, "."))
  })
  wc <- country_brief_word_counts(res)
  list(
    schema = COUNTRY_MEMO_SCHEMA, schema_version = COUNTRY_SCHEMA_VERSION, format = COUNTRY_BRIEF_FORMAT, format_version = COUNTRY_BRIEF_FORMAT_VERSION,
    iso3 = facts$iso3, title = country_brief_cap1(country_fill(cfg$brief$title, fill)), subtitle = cfg$brief$subtitle, eyebrow = cfg$brief$eyebrow,
    label = country_fill(cfg$brief$label, list(as_of_label = fill$as_of_label)), disclosure = cfg$brief$disclosure %||% NULL, as_of = meta$as_of, as_of_label = fill$as_of_label,
    generated_at = meta$generated_at, verified_at = meta$verified_at, writer = meta$writer, verifier = meta$verifier,
    words = as.integer(wc$main), words_tables = as.integer(wc$tables), facts_sha256 = meta$facts_sha256, src_sha256 = meta$src_sha256,
    claims_sha256 = meta$claims_sha256, summary = list(), sections = sections, appendix = appendix, sources = srcs,
    footer = country_fill(cfg$brief$footer, fill)
  )
}

# ---- Renderings ----------------------------------------------------------------------------------------

country_brief_inline_html <- function(xs) paste(vapply(xs, function(n) {
  v <- country_memo_esc(n$v)
  switch(n$t, strong = paste0("<strong>", v, "</strong>"), em = paste0("<em>", v, "</em>"), v)
}, ""), collapse = "")
country_brief_item_html <- function(x) paste0(country_brief_inline_html(x$inlines), if (length(x$refs)) paste0("<sup>", paste(unlist(x$refs), collapse = ","), "</sup>") else "")

# The HTML of a list of blocks (shared by the sections and the appendix). `svgs`: figure id -> inline SVG.
country_brief_blocks_html <- function(blocks, svgs) {
  out <- character()
  for (b in blocks) {
    out <- c(out, switch(b$type,
      p = paste0("<p>", country_brief_item_html(b), "</p>"),
      ul = paste0("<ul>", paste(vapply(b$items, function(x) paste0("<li>", country_brief_item_html(x), "</li>"), ""), collapse = ""), "</ul>"),
      h3 = paste0("<h3>", country_memo_esc(b$text), "</h3>"),
      choices = paste0("<div class=\"choices\">", paste(vapply(b$items, function(x) paste0(
        "<div class=\"choice ", x$kind, "\"><div class=\"clabel\">", country_memo_esc(x$label), "</div><div>", country_brief_item_html(x), "</div></div>"), ""),
        collapse = ""), "</div>"),
      table = paste0("<table class=\"t\"><thead><tr>", paste(vapply(unlist(b$head), function(h) paste0("<th>", country_memo_esc(h), "</th>"), ""), collapse = ""),
                     "</tr></thead><tbody>", paste(vapply(b$rows, function(r) paste0("<tr>", paste(vapply(r, function(cl) paste0("<td>", country_brief_item_html(cl), "</td>"), ""),
                                                                                                    collapse = ""), "</tr>"), ""), collapse = ""), "</tbody></table>",
                     if (length(b$refs)) paste0("<p class=\"tsrc\">Sources: ", paste(unlist(b$refs), collapse = ", "), ".</p>") else ""),
      figure = paste0("<figure><div class=\"ftitle\">Figure ", b$n, ". ", country_memo_esc(b$title), "</div>",
                      svgs[[b$id]] %||% "<div class=\"missing\">[figure not rendered]</div>",
                      "<figcaption>", country_brief_item_html(b$caption), "</figcaption></figure>")))
  }
  out
}

# The print HTML: self-contained (figures as inline SVG), the NEIS print style of the memos, Roboto
# embedded when font_b64 is given; Letter, 0.75 in margins.
country_brief_html <- function(doc, svgs = list(), font_b64 = NULL, page_size = "Letter", colors = list()) {
  font <- if (!is.null(font_b64) && nzchar(font_b64)) {
    paste0("@font-face{font-family:'Roboto';font-style:normal;font-weight:100 900;src:url(data:font/woff2;base64,", font_b64, ") format('woff2');}")
  } else ""
  family <- if (nzchar(font)) "'Roboto',Arial,sans-serif" else "Arial,sans-serif"
  col <- function(k, d) colors[[k]] %||% d
  css <- paste0(font,
    "@page{size:", page_size, ";margin:0.7in 0.75in}",
    "html{-webkit-print-color-adjust:exact;print-color-adjust:exact}",
    "body{font-family:", family, ";font-size:10pt;line-height:1.4;color:#111827;margin:0}",
    ".rule{width:85px;height:6px;background:#f58229;margin:0 0 12px 0}",
    ".eyebrow{font-size:8.5pt;text-transform:uppercase;letter-spacing:0.1em;color:#4b5563;margin:0 0 4px 0}",
    "h1{font-size:21pt;font-weight:500;margin:0 0 4px 0;line-height:1.15}",
    ".sub{font-size:12pt;color:#374151;margin:0 0 10px 0}",
    ".label{font-size:8.5pt;font-style:italic;color:#4b5563;border-top:1px solid #d1d5db;padding-top:5px;margin:0 0 14px 0}",
    "h2{font-size:13.5pt;font-weight:500;margin:18px 0 6px 0;break-after:avoid}",
    "h3{font-size:11pt;font-weight:600;margin:12px 0 4px 0;break-after:avoid}",
    "p{margin:0 0 7px 0}ul{margin:0 0 8px 0;padding-left:18px}li{margin:0 0 3px 0}",
    "sup{font-size:7pt;color:#6b7280;line-height:0}",
    ".choices{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin:8px 0 12px 0;break-inside:avoid}",
    ".choice{border-top:4px solid #9ca3af;background:#f9fafb;padding:6px 9px 7px 9px;font-size:9.4pt;line-height:1.36}",
    ".clabel{font-weight:600;font-size:9pt;text-transform:uppercase;letter-spacing:0.05em;margin:0 0 2px 0}",
    ".choice.secure{border-top-color:", col("secure", "#f58229"), "}.choice.preserve{border-top-color:", col("preserve", "#1889b0"), "}",
    ".choice.partner{border-top-color:", col("partner", "#55c4c5"), "}.choice.accelerate{border-top-color:", col("accelerate", "#a3b63b"), "}",
    "table.t{border-collapse:collapse;width:100%;font-size:8.8pt;line-height:1.32;margin:6px 0 10px 0}",
    "table.t th{text-align:left;font-weight:600;font-size:8pt;text-transform:uppercase;letter-spacing:0.04em;color:#374151;border-bottom:1.5px solid #111827;padding:3px 6px 3px 0}",
    "table.t td{vertical-align:top;border-bottom:1px solid #e5e7eb;padding:4px 6px 4px 0}table.t tr{break-inside:avoid}",
    ".tsrc{font-size:7.5pt;color:#6b7280;margin:-6px 0 10px 0}",
    "figure{margin:8px 0 12px 0;break-inside:avoid}figure svg{width:100%;height:auto;display:block}",
    ".ftitle{font-weight:600;font-size:10pt;margin:0 0 4px 0}figcaption{font-size:8.5pt;color:#4b5563;margin-top:3px;line-height:1.35}",
    ".appendix{break-before:page}.appendix h2{font-size:12.5pt}.appendix p,.appendix li{font-size:9pt}",
    ".appendix table.t{font-size:7pt;line-height:1.2;margin:3px 0 6px 0}.appendix table.t td{padding:1.5px 5px 1.5px 0}",
    ".appendix table.t th{font-size:6.5pt;padding:2px 5px 2px 0}",
    ".sources{font-size:8pt;color:#374151;border-top:1px solid #d1d5db;margin-top:10px;padding-top:4px}",
    ".sources h2{font-size:10pt;margin:4px 0}.sources ol{margin:0;padding-left:18px;line-height:1.3}.sources li{margin:0 0 1px 0}",
    ".end{font-size:8.5pt;font-style:italic;color:#4b5563;border-top:1px solid #d1d5db;margin-top:12px;padding-top:5px}")
  body <- c("<div class=\"rule\"></div>",
            paste0("<p class=\"eyebrow\">", country_memo_esc(doc$eyebrow %||% ""), "</p>"),
            paste0("<h1>", country_memo_esc(doc$title), "</h1>"),
            paste0("<p class=\"sub\">", country_memo_esc(doc$subtitle), "</p>"),
            paste0("<p class=\"label\">", country_memo_esc(doc$label), "</p>"))
  for (s in doc$sections) body <- c(body, sprintf("<h2>%d. %s</h2>", s$n, country_memo_esc(s$title)), country_brief_blocks_html(s$blocks, svgs))
  body <- c(body, "<div class=\"sources\"><h2>Sources</h2><ol>",
            vapply(doc$sources, function(s) paste0("<li value=\"", s$n, "\">", country_memo_esc(s$text), "</li>"), ""), "</ol></div>")
  if (!is.null(doc$appendix)) body <- c(body, "<div class=\"appendix\">", paste0("<h2>", country_memo_esc(doc$appendix$title), "</h2>"),
                                        country_brief_blocks_html(doc$appendix$blocks, svgs), "</div>")
  body <- c(body, paste0("<p class=\"end\">", country_memo_esc(doc$disclosure %||% doc$label), "<br>", country_memo_esc(doc$footer %||% ""), "</p>"))
  paste0("<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>", country_memo_esc(doc$title), "</title><style>", css,
         "</style></head><body>", paste(body, collapse = "\n"), "</body></html>")
}

# Plain Markdown of the rendered brief (figures as relative links to their published PNG files).
country_brief_markdown <- function(doc) {
  inl <- function(xs) paste(vapply(xs, function(n) switch(n$t, strong = paste0("**", n$v, "**"), em = paste0("*", n$v, "*"), n$v), ""), collapse = "")
  item <- function(x) paste0(inl(x$inlines), if (length(x$refs)) paste0(" [", paste(unlist(x$refs), collapse = ", "), "]") else "")
  cell <- function(x) gsub("|", "\\|", item(x), fixed = TRUE)
  blocks_md <- function(blocks) {
    out <- character()
    for (b in blocks) out <- c(out, switch(b$type,
      p = c(item(b), ""),
      ul = c(vapply(b$items, function(x) paste0("- ", item(x)), ""), ""),
      h3 = c(paste0("### ", b$text), ""),
      choices = c(vapply(b$items, function(x) paste0("- **", x$label, ".** ", item(x)), ""), ""),
      table = c(paste0("| ", paste(unlist(b$head), collapse = " | "), " |"), paste0("|", paste(rep("---", length(b$head)), collapse = "|"), "|"),
                vapply(b$rows, function(r) paste0("| ", paste(vapply(r, cell, ""), collapse = " | "), " |"), ""), "",
                if (length(b$refs)) c(paste0("Sources: ", paste(unlist(b$refs), collapse = ", "), "."), "") else NULL),
      figure = c(sprintf("**Figure %d. %s**", b$n, b$title), "", if (!is.null(b$png)) sprintf("![%s](%s)", b$alt %||% b$title, b$png) else "[figure]", "",
                 paste0("*", item(b$caption), "*"), "")))
    out
  }
  out <- c(paste0("# ", doc$title), "", paste0("*", doc$subtitle, "*"), "", paste0("*", doc$label, "*"), "")
  for (s in doc$sections) out <- c(out, sprintf("## %d. %s", s$n, s$title), "", blocks_md(s$blocks))
  out <- c(out, "## Sources", "", vapply(doc$sources, function(s) paste0(s$n, ". ", s$text), ""), "")
  if (!is.null(doc$appendix)) out <- c(out, paste0("## ", doc$appendix$title), "", blocks_md(doc$appendix$blocks))
  out <- c(out, paste0("*", doc$disclosure %||% doc$label, "*"), "", paste0("*", doc$footer %||% "", "*"), "")
  paste(out, collapse = "\n")
}

# Every value display string of a document (the PDF text check).
country_brief_values <- function(doc) {
  vals <- character()
  grab <- function(x) if (!is.null(x$inlines)) vals <<- c(vals, vapply(Filter(function(n) n$t == "value", x$inlines), function(n) n$v, ""))
  walk <- function(blocks) for (b in blocks) switch(b$type,
    p = grab(b), ul = for (x in b$items) grab(x), choices = for (x in b$items) grab(x),
    table = for (r in b$rows) for (cl in r) grab(cl), figure = grab(b$caption), NULL)
  for (s in doc$sections) walk(s$blocks)
  if (!is.null(doc$appendix)) walk(doc$appendix$blocks)
  unique(vals)
}

# Completeness of a verifier's record for a brief: every claim and sentence id, every lint warning cleared,
# matching hashes, no open error, status pass (the memos' rule, country_memo_verify_check()).
country_brief_verify_check <- function(verify, claims_doc, lint, src_sha, facts_sha, claims_sha) {
  country_memo_verify_check(verify, claims_doc, lint, src_sha, facts_sha, claims_sha)
}
