# The policy memo pipeline of Country Insights (spec_country.md part B), after the writer's source:
# parse the source (a :::summary block of bullets and the five "## " sections, Markdown subset), resolve
# its {{tokens}} against the fact sheet, split sentences, build the claims file (one row per token
# occurrence with its meaning, source and context), evaluate the caveat triggers, lint (structure, word
# bounds, digits and number words, economy names, corner and secure/insecure words, causal and order
# words, coverage, caveats, acronyms, tone), build the memo document (the AST the screen, the PDF and the
# Word file are rendered from), its print HTML and Markdown, and check a verifier's record for
# completeness. Pure: text and lists in, text and lists out (scripts/utils/country_memos.R does the IO).

COUNTRY_MEMO_FILTERS <- c("lower", "list", "n")
COUNTRY_MEMO_NO_SOURCE <- "^(caveats\\.|meta\\.(country|as_of_label|trade_year|un_window|cmfdi_edition|rank_threshold|tier)$|cells\\.[^.]+\\.(label|label_prose|tech|stage|trade\\.year)$|cells\\.[^.]+\\.drivers\\.[^.]+\\.[^.]+\\.(category|vars\\.[^.]+\\.label)$)"

# ---- Parse -------------------------------------------------------------------------------------------

# Split inline Markdown (**strong**, *em*, _em_) and {{tokens}} into nodes: list(t = "text" | "strong" |
# "em" | "token", v, path, filter). Returns list(nodes, errors).
country_memo_inlines <- function(text) {
  errors <- character()
  tok_m <- gregexpr("\\{\\{([^{}]*)\\}\\}", text, perl = TRUE)[[1]]
  toks <- if (tok_m[1] > 0) regmatches(text, list(tok_m))[[1]] else character()
  prot <- text
  if (length(toks)) {
    pieces <- regmatches(text, list(tok_m), invert = TRUE)[[1]]
    prot <- pieces[1]
    for (i in seq_along(toks)) prot <- paste0(prot, "\001", i, "\002", pieces[i + 1])
  }
  if (grepl("\\{\\{|\\}\\}", prot)) errors <- c(errors, "a malformed token (unbalanced braces)")
  em_re <- "\\*\\*([^*]+?)\\*\\*|(?<![*A-Za-z0-9])\\*([^*\\s][^*]*?)\\*(?![*A-Za-z0-9])|(?<![A-Za-z0-9_])_([^_\\s][^_]*?)_(?![A-Za-z0-9_])"
  m <- gregexpr(em_re, prot, perl = TRUE)[[1]]
  nodes <- list()
  add_text <- function(s) {
    if (!nzchar(s)) return(invisible())
    parts <- regmatches(s, gregexpr("\001[0-9]+\002", s), invert = NA)[[1]]
    for (p in parts) {
      if (!nzchar(p)) next
      if (grepl("^\001[0-9]+\002$", p)) {
        raw <- toks[as.integer(gsub("[^0-9]", "", p))]
        inner <- trimws(sub("^\\{\\{(.*)\\}\\}$", "\\1", raw))
        bits <- trimws(strsplit(inner, "|", fixed = TRUE)[[1]])
        path <- bits[1]
        filter <- if (length(bits) > 1) bits[2] else NA_character_
        if (length(bits) > 2 || (!is.na(filter) && !filter %in% COUNTRY_MEMO_FILTERS)) errors <<- c(errors, paste0(raw, ": unknown filter"))
        if (!grepl("^[A-Za-z0-9_.-]+$", path %||% "")) errors <<- c(errors, paste0(raw, ": not a path"))
        nodes[[length(nodes) + 1]] <<- list(t = "token", raw = raw, path = path, filter = filter)
      } else {
        if (grepl("*", p, fixed = TRUE)) errors <<- c(errors, paste0("a stray '*' in \"", substr(p, 1, 40), "\""))
        nodes[[length(nodes) + 1]] <<- list(t = "text", v = p)
      }
    }
  }
  if (m[1] < 0) {
    add_text(prot)
  } else {
    pos <- 1L
    lens <- attr(m, "match.length")
    for (i in seq_along(m)) {
      add_text(substr(prot, pos, m[i] - 1L))
      s <- substr(prot, m[i], m[i] + lens[i] - 1L)
      strong <- startsWith(s, "**")
      inner <- if (strong) substr(s, 3, nchar(s) - 2) else substr(s, 2, nchar(s) - 1)
      if (grepl("\001", inner, fixed = TRUE)) errors <- c(errors, "a token inside bold or italic text")
      inner <- gsub("\001([0-9]+)\002", "{{token}}", inner)
      nodes[[length(nodes) + 1]] <- list(t = if (strong) "strong" else "em", v = inner)
      pos <- m[i] + lens[i]
    }
    add_text(substr(prot, pos, nchar(prot)))
  }
  list(nodes = nodes, errors = errors)
}

# Parse a memo source. Returns list(summary = list(items), sections = list(list(n, title, blocks)),
# errors). An item is list(text, nodes); a block list(type = "p" | "ul", items).
country_memo_parse <- function(md, cfg) {
  titles <- unlist(cfg$memo$sections)
  md <- sub("^\ufeff", "", gsub("\r\n?", "\n", md))
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  errors <- character()
  err <- function(...) errors <<- c(errors, paste0(...))
  bad_line <- function(l, where) {
    t <- trimws(l)
    if (grepl("^[0-9]+[.)][[:space:]]", t)) err(where, ": a numbered list (\"", substr(t, 1, 30), "\")")
    if (startsWith(t, ">")) err(where, ": a block quote")
    if (grepl("^\\|.*\\|$", t) || grepl("^[-|: ]{3,}$", t) && grepl("|", t, fixed = TRUE)) err(where, ": a table")
    if (grepl("`", t, fixed = TRUE)) err(where, ": code")
    if (grepl("!\\[", t)) err(where, ": an image")
    if (grepl("\\[[^]]*\\]\\([^)]*\\)", t)) err(where, ": a link")
    if (grepl("<[A-Za-z/!]", t)) err(where, ": raw HTML")
    if (grepl("^(\\*\\*\\*+|---+|___+)$", t)) err(where, ": a horizontal rule")
    if (grepl("^#", t) && !grepl("^## [^#]", t)) err(where, ": a heading other than the five sections (\"", substr(t, 1, 40), "\")")
  }
  item <- function(text, where) {
    p <- country_memo_inlines(text)
    if (length(p$errors)) err(where, ": ", paste(unique(p$errors), collapse = "; "))
    list(text = text, nodes = p$nodes)
  }
  # summary block
  i <- 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1L
  summary <- list()
  if (i > length(lines) || trimws(lines[i]) != ":::summary") {
    err("structure: the source must start with the :::summary block")
  } else {
    i <- i + 1L
    cur <- NULL
    closed <- FALSE
    while (i <= length(lines)) {
      l <- lines[i]
      t <- trimws(l)
      if (t == ":::") { closed <- TRUE; i <- i + 1L; break }
      bad_line(l, "summary")
      if (grepl("^[-*][[:space:]]+", t)) {
        if (!is.null(cur)) summary[[length(summary) + 1]] <- cur
        cur <- sub("^[-*][[:space:]]+", "", t)
      } else if (nzchar(t)) {
        if (is.null(cur)) err("summary: text outside a bullet (\"", substr(t, 1, 40), "\")") else cur <- paste(cur, t)
      }
      i <- i + 1L
    }
    if (!is.null(cur)) summary[[length(summary) + 1]] <- cur
    if (!closed) err("structure: the :::summary block is not closed with :::")
  }
  summary <- lapply(seq_along(summary), function(k) item(summary[[k]], paste0("summary bullet ", k)))
  # sections
  sections <- list()
  cur_sec <- NULL
  blocks <- list()
  para <- character()
  ul <- list()
  li <- NULL
  flush <- function() {
    if (!is.null(li)) { ul[[length(ul) + 1]] <<- li; li <<- NULL }
    if (length(ul)) { blocks[[length(blocks) + 1]] <<- list(type = "ul", items = ul); ul <<- list() }
    if (length(para)) { blocks[[length(blocks) + 1]] <<- list(type = "p", text = paste(para, collapse = " ")); para <<- character() }
  }
  close_section <- function() {
    flush()
    if (!is.null(cur_sec)) {
      cur_sec$blocks <<- blocks
      sections[[length(sections) + 1]] <<- cur_sec
    } else if (length(blocks)) err("structure: text between the summary block and the first section")
    blocks <<- list()
  }
  while (i <= length(lines)) {
    l <- lines[i]
    t <- trimws(l)
    if (grepl("^## ", t)) {
      close_section()
      cur_sec <- list(n = length(sections) + 1L, title = trimws(sub("^## ", "", t)))
    } else {
      where <- if (is.null(cur_sec)) "before the first section" else paste0("section ", cur_sec$n)
      bad_line(l, where)
      if (t == ":::summary" || t == ":::") err(where, ": a second summary block")
      if (!nzchar(t)) {
        flush()
      } else if (grepl("^[-*][[:space:]]+", t)) {
        if (length(para)) { blocks[[length(blocks) + 1]] <- list(type = "p", text = paste(para, collapse = " ")); para <- character() }
        if (!is.null(li)) ul[[length(ul) + 1]] <- li
        li <- sub("^[-*][[:space:]]+", "", t)
      } else if (!is.null(li) && grepl("^[[:space:]]", l)) {
        li <- paste(li, t)
      } else {
        if (!is.null(li) || length(ul)) flush()
        para <- c(para, t)
      }
    }
    i <- i + 1L
  }
  close_section()
  got <- vapply(sections, function(s) s$title, "")
  if (!identical(got, titles)) {
    err("structure: the sections must be exactly, in order: ", paste(sprintf("\"## %s\"", titles), collapse = ", "),
        "; found ", if (length(got)) paste(sprintf("\"## %s\"", got), collapse = ", ") else "none")
  }
  sections <- lapply(sections, function(s) {
    s$blocks <- lapply(seq_along(s$blocks), function(j) {
      b <- s$blocks[[j]]
      where <- sprintf("section %d, block %d", s$n, j)
      if (b$type == "p") c(list(type = "p"), item(b$text, where)) else list(type = "ul", items = lapply(b$items, function(x) item(x, where)))
    })
    s
  })
  list(summary = summary, sections = sections, errors = unique(errors))
}

# ---- Paths: meanings, sources, context -----------------------------------------------------------------

# Does a dot path match a pattern ("cells.*.es.pct"; `*` = one segment; "next*" = a segment prefix)?
country_memo_path_match <- function(pattern, path) {
  ps <- strsplit(pattern, ".", fixed = TRUE)[[1]]
  xs <- strsplit(path, ".", fixed = TRUE)[[1]]
  if (length(ps) != length(xs)) return(FALSE)
  for (k in seq_along(ps)) {
    p <- ps[k]
    if (p == "*") next
    if (endsWith(p, "*")) { if (!startsWith(xs[k], substr(p, 1, nchar(p) - 1))) return(FALSE); next }
    if (p != xs[k]) return(FALSE)
  }
  TRUE
}

# A section-field pattern (B3): `*` matches one segment and a final ".*" the rest of the path.
country_memo_field_match <- function(pattern, path) {
  ps <- strsplit(pattern, ".", fixed = TRUE)[[1]]
  xs <- strsplit(path, ".", fixed = TRUE)[[1]]
  n <- length(ps)
  if (n > 1 && ps[n] == "*" && length(xs) >= n) {
    return(country_memo_path_match(paste(ps[-n], collapse = "."), paste(xs[seq_len(n - 1)], collapse = ".")))
  }
  country_memo_path_match(pattern, path)
}

country_memo_cell_of <- function(path) {
  m <- regmatches(path, regexec("^cells\\.([^.]+)", path))[[1]]
  if (length(m) == 2) m[2] else NA_character_
}

country_memo_partner_of <- function(path) {
  m <- regmatches(path, regexec("^partners\\.([A-Z0-9]{3})(\\.|$)", path))[[1]]
  if (length(m) >= 2) m[2] else NA_character_
}

# The values that fill a path's meaning template and the claim's context (B6.2).
country_memo_path_vals <- function(idx, path, element = NULL) {
  m <- idx$meta
  v <- list(country = m$country, trade_year = m$trade_year, key = sub("^.*\\.", "", path))
  k <- country_memo_cell_of(path)
  if (is.na(k) && grepl("^summary\\.top_si", path)) k <- if (grepl("next", path)) idx$summary$top_si$`next` %||% NA else idx$summary$top_si$cell %||% NA
  if (is.na(k) && grepl("^network\\.pick", path)) k <- if (grepl("runner_up", path)) idx$network$pick$runner_up %||% NA else idx$network$pick$cell %||% NA
  if (is.na(k) && grepl("^network\\.producer\\.[0-9]+", path)) {
    j <- as.integer(sub("^network\\.producer\\.([0-9]+).*$", "\\1", path)) + 1L
    k <- idx$network$producer[[j]]$cell %||% NA
  }
  if (is.na(k) && grepl("^policy\\.nipo\\.n_by_cell\\.", path)) k <- sub("^policy\\.nipo\\.n_by_cell\\.", "", path)
  if (is.na(k) && !is.null(element) && !is.null(idx$cells[[element]])) k <- element
  cell <- if (!is.na(k)) idx$cells[[k]] else NULL
  if (!is.null(cell)) {
    v$label <- cell$label
    v$tech <- cell$tech
    pl <- regmatches(path, regexec("\\.(es|eo)(\\.|$)", path))[[1]]
    if (length(pl) >= 2) v$n_others <- cell[[pl[2]]]$n_others$d %||% "–"
  }
  dm <- regmatches(path, regexec("^cells\\.[^.]+\\.drivers\\.(es|eo)\\.([^.]+)(\\.vars\\.([^.]+))?", path))[[1]]
  if (length(dm) >= 3 && !is.null(cell)) {
    v$pillar <- if (dm[2] == "es") "Energy Security" else "Economic Opportunity"
    v$direction <- if (dm[2] == "es") "secure" else "opportunity"
    d <- cell$drivers[[dm[2]]][[dm[3]]]
    v$category <- d$category %||% dm[3]
    if (length(dm) >= 5 && nzchar(dm[5])) {
      var <- d$vars[[dm[5]]]
      v$variable <- var$label %||% dm[5]
      v$year <- var$raw$year %||% "–"
      v$source <- var$raw$source_label %||% var$raw$source %||% "–"
    }
  }
  tm <- regmatches(path, regexec("^cells\\.[^.]+\\.trade\\.(sources|markets)\\.([0-9]+)", path))[[1]]
  if (length(tm) == 3 && !is.null(cell)) {
    j <- as.integer(tm[3]) + 1L
    p <- cell$trade[[tm[2]]][[j]]
    v$party <- p$name %||% "–"
    v$i <- if (j == 1) "first" else country_ordinal(j)
  }
  iso <- country_memo_partner_of(path)
  if (is.na(iso) && grepl("^summary\\.psi_1", path)) iso <- idx$summary$psi_1$iso3 %||% NA
  if (is.na(iso) && !is.null(element) && !is.null(idx$partners$rows[[element]])) iso <- element
  if (!is.na(iso) && !is.null(idx$partners$rows[[iso]])) v$name <- idx$partners$rows[[iso]]$name
  qm <- regmatches(path, regexec("^summary\\.quadrants\\.([a-z]+)$", path))[[1]]
  if (length(qm) == 2) v$q <- idx$words$quadrant[[qm[2]]] %||% qm[2]
  cm <- regmatches(path, regexec("^policy\\.agreement\\.by_class\\.([a-z_]+)$", path))[[1]]
  if (length(cm) == 2) v$class <- idx$words$class[[cm[2]]] %||% cm[2]
  if (grepl("^policy\\.cat", path)) v$scope <- if (identical(idx$policy$cat$scope, "eu_bloc")) "the EU's rating, applied to its member states" else "its own rating"
  list(vals = v, cell = if (!is.null(cell)) k else NA_character_, partner = iso)
}

country_memo_meaning <- function(cfg, idx, path, element = NULL) {
  for (e in cfg$path_meanings) {
    if (country_memo_path_match(e$path, path)) {
      vals <- country_memo_path_vals(idx, path, element)$vals
      return(country_fill(e$meaning, lapply(vals, function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "–" else x)))
    }
  }
  NA_character_
}

# The source id of a token path: a raw value's own source, else the longest src_map prefix (B4).
country_memo_source <- function(sheet, idx, path) {
  if (grepl(COUNTRY_MEMO_NO_SOURCE, path)) return(NA_character_)
  rm <- regmatches(path, regexec("^(cells\\.[^.]+\\.drivers\\.[^.]+\\.[^.]+\\.vars\\.[^.]+\\.raw)(\\.[a-z_]+)?$", path))[[1]]
  if (length(rm) >= 2) {
    r <- country_memo_get(idx, rm[2])
    if (!is.null(r$source)) return(r$source)
  }
  best <- NA_character_
  best_n <- -1L
  for (e in sheet$src_map) {
    re <- paste0("^", gsub("\\*", "[^.]+", gsub(".", "\\.", e$prefix, fixed = TRUE)))
    if (grepl(re, path) && nchar(e$prefix) > best_n) { best <- e$src; best_n <- nchar(e$prefix) }
  }
  best
}

# What the verifier needs to judge the words around a number (B6.2).
country_memo_context <- function(idx, path, element = NULL) {
  pv <- country_memo_path_vals(idx, path, element)
  ctx <- list()
  s <- idx$summary
  pos <- function(x, k) { p <- match(k, unlist(x)); if (is.na(p)) NULL else p }
  if (!is.na(pv$cell)) {
    c <- idx$cells[[pv$cell]]
    tops <- function(pl) unname(unlist(lapply(c$drivers[[pl]], function(d) if (isTRUE(d$top)) d$key else NULL)))
    ctx$cell <- list(key = pv$cell, label = c$label, placed = isTRUE(c$placed), quadrant = c$quadrant %||% NULL,
                     es_pct = c$es$pct$d %||% NULL, eo_pct = c$eo$pct$d %||% NULL,
                     es_order = pos(s$es_order, pv$cell), eo_order = pos(s$eo_order, pv$cell), si_order = pos(s$si_order, pv$cell),
                     si_rank = c$si$rank$d %||% NULL, margin_next = c$si$margin_next$v %||% NULL,
                     priority_third = c$priority$third %||% NULL, policy_third = c$policy$third %||% NULL, policy_class = c$policy$class %||% NULL,
                     top_es = tops("es"), top_eo = tops("eo"))
    dm <- regmatches(path, regexec("^cells\\.[^.]+\\.drivers\\.(es|eo)\\.([^.]+)(\\.vars\\.([^.]+))?", path))[[1]]
    if (length(dm) >= 3) {
      d <- c$drivers[[dm[2]]][[dm[3]]]
      ctx$driver <- list(pillar = dm[2], category = d$category, top = isTRUE(d$top), impact_sign = if (is.null(d$impact$v)) NULL else sign(d$impact$v),
                         imputed = isTRUE(d$imputed), rule = d$rule %||% NULL)
      # 2026-10-14: a driver resting on the EU's regional figure (a variable with raw note eu_region_share)
      if (any(vapply(d$vars, function(v) identical(v$raw$note, "eu_region_share"), TRUE))) ctx$driver$eu_region <- TRUE
      if (length(dm) >= 5 && nzchar(dm[5])) {
        var <- d$vars[[dm[5]]]
        ctx$variable <- list(key = dm[5], imputed = isTRUE(var$imputed), rule = var$rule %||% NULL, note = var$raw$note %||% NULL)
      }
    }
    tm <- regmatches(path, regexec("^cells\\.[^.]+\\.trade\\.(sources|markets)\\.([0-9]+)", path))[[1]]
    if (length(tm) == 3) {
      p <- c$trade[[tm[2]]][[as.integer(tm[3]) + 1L]]
      ctx$trade <- list(side = if (tm[2] == "sources") "imports" else "exports", position = as.integer(tm[3]) + 1L, party = p$iso3 %||% NULL,
                        status = p$status %||% NULL, total = if (tm[2] == "sources") c$trade$imports$d else c$trade$exports$d)
    } else if (grepl("^cells\\.[^.]+\\.trade\\.", path)) {
      ctx$trade <- list(imports = c$trade$imports$d %||% NULL, exports = c$trade$exports$d %||% NULL)
    }
  }
  if (!is.na(pv$partner) && !is.null(idx$partners$rows[[pv$partner]])) {
    r <- idx$partners$rows[[pv$partner]]
    ctx$partner <- list(iso3 = pv$partner, name = r$name, rank = r$rank$d %||% NULL, psi_order = pos(idx$partners$psi_order, pv$partner),
                        tie_order = pos(idx$partners$tie_order, pv$partner), reason = r$reason %||% NULL, match = r$match %||% NULL,
                        in_psi_top = isTRUE(r$in_psi_top), in_tie_top = isTRUE(r$in_tie_top))
  }
  if (grepl("^summary\\.top_si|^network\\.pick", path)) {
    ctx$margin <- if (grepl("^summary", path)) s$top_si$margin$v %||% NULL else idx$network$pick$margin$v %||% NULL
    ctx$near_tie <- if (grepl("^summary", path)) isTRUE(s$top_si$near_tie) else isTRUE(idx$network$pick$near_tie)
  }
  ctx
}

# ---- Resolve -----------------------------------------------------------------------------------------

# Resolve one token node; extends country_memo_resolve_token() with key-to-name printing for strings
# that are sector keys or ISO3 codes and with agreement names in |list.
country_memo_value <- function(idx, path, filter) {
  f <- if (is.na(filter)) NULL else filter
  if (!is.null(f) && f %in% c("list", "n")) {
    x <- country_memo_get(idx, path)
    els <- unlist(x)
    if (is.character(els) && length(els) && all(els %in% names(idx$words$agreement %||% list())) && !any(els %in% names(idx$cells))) {
      if (f == "n") return(list(ok = TRUE, text = country_fmt(length(els), "count"), kind = "count", v = length(els), elements = els))
      w <- vapply(els, function(a) idx$words$agreement[[a]], "")
      txt <- if (length(w) == 1) w else paste0(paste(utils::head(w, -1), collapse = ", "), " and ", utils::tail(w, 1))
      return(list(ok = TRUE, text = txt, kind = "list", elements = els))
    }
    return(country_memo_resolve_token(idx, path, f))
  }
  x <- country_memo_get(idx, path)
  if (is.character(x) && length(x) == 1 && grepl("\\.(cell|next|runner_up)$", path) && !is.null(idx$cells[[x]])) {
    t <- idx$cells[[x]]$label_prose
    return(list(ok = TRUE, text = if (identical(f, "lower")) tolower(t) else t, kind = "string", element = x))
  }
  if (is.character(x) && length(x) == 1 && grepl("\\.iso3$", path) && !is.null(idx$partners$rows[[x]])) {
    return(list(ok = TRUE, text = idx$partners$rows[[x]]$name, kind = "string", element = x))
  }
  if (is.logical(x)) return(list(ok = FALSE, error = paste0("{{", path, "}} resolves to true/false, which cannot be printed")))
  country_memo_resolve_token(idx, path, f)
}

# Resolve every token of a parsed memo. Returns list(items, tokens, errors) where items are the
# summary bullets, paragraphs and list items in reading order, each list(section, block, item, nodes =
# resolved nodes, rendered, masked, spans), and tokens one row per token occurrence.
country_memo_resolve <- function(parsed, sheet, cfg) {
  idx <- country_memo_index(sheet)
  errors <- character()
  items <- list()
  tokens <- list()
  add_item <- function(it, section, block, k, type) {
    nodes <- list()
    rendered <- ""
    masked <- ""
    spans <- list()
    for (n in it$nodes) {
      if (n$t == "token") {
        r <- country_memo_value(idx, n$path, n$filter)
        if (!isTRUE(r$ok)) {
          errors <<- c(errors, r$error)
          txt <- n$raw
        } else txt <- r$text
        start <- nchar(rendered) + 1L
        tid <- length(tokens) + 1L
        els <- if (isTRUE(r$ok) && identical(r$kind, "list")) r$elements else if (!is.null(r$element)) r$element else NULL
        tokens[[tid]] <<- list(id = tid, section = section, path = n$path, filter = n$filter, ok = isTRUE(r$ok), d = txt,
                               v = if (is.null(r$v)) NULL else r$v, kind = r$kind %||% NA_character_, elements = els,
                               start = start, end = start + nchar(txt) - 1L, item = length(items) + 1L)
        nodes[[length(nodes) + 1]] <- list(t = "value", v = txt, path = if (is.na(n$filter)) n$path else paste0(n$path, "|", n$filter), token = tid)
        rendered <- paste0(rendered, txt)
        masked <- paste0(masked, strrep("\u00a7", nchar(txt)))
        spans[[length(spans) + 1]] <- c(start, start + nchar(txt) - 1L, tid)
      } else {
        nodes[[length(nodes) + 1]] <- list(t = n$t, v = n$v)
        rendered <- paste0(rendered, n$v)
        masked <- paste0(masked, n$v)
      }
    }
    items[[length(items) + 1]] <<- list(section = section, block = block, item = k, type = type, nodes = nodes, rendered = rendered, masked = masked, spans = spans)
  }
  for (k in seq_along(parsed$summary)) add_item(parsed$summary[[k]], 0L, k, 1L, "li")
  for (s in parsed$sections) for (j in seq_along(s$blocks)) {
    b <- s$blocks[[j]]
    if (b$type == "p") add_item(b, s$n, j, 1L, "p") else for (k in seq_along(b$items)) add_item(b$items[[k]], s$n, j, k, "li")
  }
  list(items = items, tokens = tokens, errors = unique(errors), idx = idx)
}

# ---- Sentences and claims ------------------------------------------------------------------------------

# Split each item into sentences (". ", "? ", "! " outside tokens, and the end of the item). Returns the
# sentences (id s<section>.<n>, section, text, masked, tokens) and the token -> sentence map.
country_memo_sentences <- function(res) {
  out <- list()
  tok_sent <- character(length(res$tokens))
  count <- integer(0)
  for (ii in seq_along(res$items)) {
    it <- res$items[[ii]]
    sec <- as.character(it$section)
    m <- it$masked
    ends <- gregexpr("[.?!](?=\\s|$)", m, perl = TRUE)[[1]]
    ends <- if (ends[1] > 0) as.integer(ends) else integer()
    ends <- sort(unique(c(ends, nchar(m))))
    st <- 1L
    for (e in ends) {
      if (e < st) next
      txt <- trimws(substr(it$rendered, st, e))
      msk <- trimws(substr(m, st, e))
      if (nzchar(gsub("[[:space:][:punct:]]", "", msk))) {
        count[sec] <- (if (is.na(count[sec])) 0L else count[sec]) + 1L
        sid <- sprintf("s%s.%d", sec, count[sec])
        tk <- integer()
        for (sp in it$spans) if (sp[1] >= st && sp[1] <= e) { tk <- c(tk, sp[3]); tok_sent[sp[3]] <- sid }
        out[[length(out) + 1]] <- list(id = sid, section = it$section, item = ii, text = txt, masked = msk, tokens = tk)
      }
      st <- e + 1L
    }
  }
  list(sentences = out, tok_sent = tok_sent)
}

# One claim row per token occurrence (a |list token: one per element). B6.2.
country_memo_claims <- function(res, sents, sheet, cfg) {
  idx <- res$idx
  claims <- list()
  stext <- stats::setNames(vapply(sents$sentences, function(s) s$text, ""), vapply(sents$sentences, function(s) s$id, ""))
  for (t in res$tokens) {
    if (!isTRUE(t$ok)) next
    els <- if (identical(t$kind, "list")) t$elements else list(NULL)
    for (el in els) {
      cid <- paste0("c", length(claims) + 1L)
      claims[[length(claims) + 1]] <- list(
        id = cid, token = t$id, section = t$section, sentence_id = sents$tok_sent[t$id], sentence = unname(stext[sents$tok_sent[t$id]]),
        path = t$path, filter = if (is.na(t$filter)) NULL else t$filter, element = el,
        v = t$v, d = t$d, source = country_chr_or_null(country_memo_source(sheet, idx, t$path)),
        meaning = country_chr_or_null(country_memo_meaning(cfg, idx, t$path, el)),
        context = country_memo_context(idx, t$path, el)
      )
    }
  }
  claims
}

# ---- Caveat triggers (A8.4) ---------------------------------------------------------------------------

country_memo_triggers <- function(claims, sheet, cfg) {
  m <- sheet$meta
  cl <- Filter(function(c) c$section %in% 0:4, claims)
  paths <- vapply(cl, function(c) c$path, "")
  idx <- country_memo_index(sheet)
  cited_cells <- unique(stats::na.omit(c(vapply(cl, function(c) c$context$cell$key %||% NA_character_, ""))))
  any_p <- function(re) any(grepl(re, paths))
  on <- c(relative_scores = TRUE, baseline = TRUE)
  on["rankability"] <- any(vapply(cited_cells, function(k) !isTRUE(idx$cells[[k]]$placed), TRUE))
  on["imputed_driver"] <- any(vapply(cl, function(c) isTRUE(c$context$driver$imputed) || isTRUE(c$context$variable$imputed), TRUE))
  on["policy_pams_only"] <- identical(m$policy_basis, "IEA PAMS only") && any_p("^policy\\.|^cells\\.[^.]+\\.policy\\.|^meta\\.policy_basis$")
  on["no_si"] <- !isTRUE(m$has_si)
  near_cited <- any(vapply(cl, function(c) isTRUE(c$context$near_tie), TRUE))
  si_cells <- unique(unlist(lapply(cl, function(c) if (grepl("\\.si(\\.|$)", c$path)) c$context$cell$key else NULL)))
  so <- unlist(sheet$summary$si_order)
  adj <- FALSE
  if (length(si_cells) > 1) {
    p <- sort(stats::na.omit(match(si_cells, so)))
    for (j in seq_len(max(0, length(p) - 1))) if (p[j + 1] == p[j] + 1 && isTRUE((idx$cells[[so[p[j]]]]$si$margin_next$v %||% 1) < (cfg$near_tie %||% 0.01))) adj <- TRUE
  }
  on["near_tie"] <- near_cited || adj
  on["eu_node"] <- isTRUE(m$eu_member) && any_p("^network\\.")
  on["inherited_alignment"] <- identical(m$alignment_basis, "inherited") && any_p("alignment|^meta\\.bloc$|\\.ties\\.bloc$")
  on["assumed_zero_production"] <- any(vapply(cl, function(c) identical(c$context$variable$note, "production_assumed_zero"), TRUE))
  on["cmfdi_coverage"] <- any_p("\\.fdi(\\.|_)")
  on["trade_mirror"] <- any(vapply(cl, function(c) identical(c$context$trade$status, "mirror"), TRUE))
  few <- cfg$few_economies %||% 10
  on["few_economies"] <- any(vapply(cited_cells, function(k) isTRUE((idx$cells[[k]]$es$n$v %||% 99) < few), TRUE))
  on["cat_eu_bloc"] <- identical(sheet$policy$cat$scope, "eu_bloc") && any_p("^policy\\.cat")
  on["intra_eu"] <- isTRUE(m$eu_member) && any_p("^cells\\.[^.]+\\.trade\\.|\\.ties\\.(imp_mean|exp_mean|tie)$")
  hkg <- any(vapply(cl, function(c) identical(c$context$partner$iso3, "HKG") || identical(c$context$trade$party, "HKG") || identical(c$element, "HKG"), TRUE))
  on["hkg_reexport"] <- hkg
  on["pick_stability"] <- any_p("^network\\.pick")
  on["ownership_parent"] <- any_p("china")
  on["eu_policy_records"] <- isTRUE(m$eu_member) && !is.null(sheet$policy$pams_eu_records) && any_p("^policy\\.pams")
  on["nipo_context"] <- any_p("^policy\\.nipo")
  on["eu_region_share"] <- any(vapply(cl, function(c) identical(c$context$variable$note, "eu_region_share") || isTRUE(c$context$driver$eu_region), TRUE))
  ids <- names(on)[on]
  ids[ids %in% names(cfg$caveats)]
}

# ---- Lint (B6.1) ---------------------------------------------------------------------------------------

country_memo_words_of <- function(x) sum(lengths(regmatches(x, gregexpr("[^[:space:]]+", x))))

country_memo_has_word <- function(text, words, ignore_case = TRUE) {
  hits <- character()
  for (w in words) {
    re <- paste0("(?<![A-Za-z])", gsub("([.?+()\\[\\]])", "\\\\\\1", w), "(?![A-Za-z])")
    if (grepl(re, text, perl = TRUE, ignore.case = ignore_case)) hits <- c(hits, w)
  }
  hits
}

# res: country_memo_resolve(); sents: country_memo_sentences(); claims; parsed (parse errors);
# names: economy names of the roster (the memo's own country is removed here).
country_memo_lint <- function(parsed, res, sents, claims, sheet, cfg, names = character()) {
  out <- list()
  add <- function(severity, check, detail, section = NA, sentence_id = NA) {
    out[[length(out) + 1]] <<- list(id = paste0("l", length(out) + 1L), severity = severity, check = check,
                                    section = if (is.na(section)) NULL else as.integer(section), sentence_id = if (is.na(sentence_id)) NULL else sentence_id,
                                    detail = detail)
  }
  m <- sheet$meta
  idx <- res$idx
  own <- m$country
  mc <- cfg$memo
  # structure and tokens
  for (e in parsed$errors) add("error", "structure", e)
  for (e in res$errors) add("error", "token", e)
  for (t in res$tokens) if (isTRUE(t$ok)) {
    if (is.na(country_memo_meaning(cfg, idx, t$path))) add("error", "token", paste0("{{", t$path, "}}: no path_meanings template"), t$section)
    if (!grepl(COUNTRY_MEMO_NO_SOURCE, t$path) && is.na(country_memo_source(sheet, idx, t$path))) add("error", "token", paste0("{{", t$path, "}}: no source (src_map)"), t$section)
  }
  # word bounds
  sec_text <- function(s) paste(vapply(Filter(function(it) it$section == s, res$items), function(it) it$rendered, ""), collapse = " ")
  words <- vapply(0:5, function(s) country_memo_words_of(sec_text(s)), 1)
  total <- sum(words)
  wt <- unlist(mc$words_total)
  if (total < wt[1] || total > wt[2]) add("error", "words", sprintf("%d words in the summary and sections 1-5 (bounds %d-%d)", total, wt[1], wt[2]))
  ws <- unlist(mc$words_summary)
  if (words[1] < ws[1] || words[1] > ws[2]) add("error", "words", sprintf("summary: %d words (bounds %d-%d)", words[1], ws[1], ws[2]), 0)
  nb <- length(parsed$summary)
  bb <- unlist(mc$summary_bullets)
  if (nb < bb[1] || nb > bb[2]) add("error", "structure", sprintf("summary: %d bullets (%d-%d)", nb, bb[1], bb[2]), 0)
  for (s in 1:5) {
    hard <- unlist(mc$words_sections_hard[[s]])
    tgt <- unlist(mc$words_sections_target[[s]])
    w <- words[s + 1]
    if (w < hard[1] || w > hard[2]) add("error", "words", sprintf("section %d: %d words (hard bounds %d-%d)", s, w, hard[1], hard[2]), s)
    else if (w < tgt[1] || w > tgt[2]) add("warning", "words", sprintf("section %d: %d words (target %d-%d)", s, w, tgt[1], tgt[2]), s)
  }
  # sentence-level checks
  num_words <- c(unlist(cfg$number_words), unlist(cfg$number_words_fraction))
  phrases <- unlist(cfg$number_phrases)
  digit_ok <- unique(c(unlist(cfg$digit_phrases %||% list("RMI (2025)")), grep("[0-9]", unlist(cfg$allowed_acronyms), value = TRUE)))
  allowed_acr <- unlist(cfg$allowed_acronyms)
  name_list <- setdiff(unique(c(names, unlist(cfg$name_aliases))), c(own, "European Union", "World", ""))
  name_list <- name_list[!vapply(name_list, function(n) grepl(paste0("(?<![A-Za-z])", gsub("([.?+()\\[\\]])", "\\\\\\1", n), "(?![A-Za-z])"), own, perl = TRUE), TRUE)]
  tok_by_id <- res$tokens
  allow_caps <- unlist(cfg$capital_allowlist)
  for (s in sents$sentences) {
    sid <- s$id
    sec <- s$section
    msk <- s$masked
    # the memo's own name is plain text by right
    msk_own <- gsub(own, "\u00a7", msk, fixed = TRUE)
    toks <- lapply(s$tokens, function(i) tok_by_id[[i]])
    tpaths <- vapply(toks, function(t) t$path, "")
    lower <- tolower(msk_own)
    # digits
    dtxt <- msk_own
    for (p in digit_ok) dtxt <- gsub(p, "", dtxt, fixed = TRUE)
    if (grepl("[0-9]", dtxt)) add("error", "digit", sprintf("a digit outside a token: \"%s\"", s$text), sec, sid)
    # number words
    ntxt <- lower
    for (p in phrases) ntxt <- gsub(tolower(p), " ", ntxt, fixed = TRUE)
    hits <- country_memo_has_word(ntxt, num_words)
    if (length(hits)) add("error", "number_word", sprintf("number word(s) %s outside a token: \"%s\"", paste(sprintf("'%s'", hits), collapse = ", "), s$text), sec, sid)
    hw <- country_memo_has_word(ntxt, unlist(cfg$number_warnings))
    if (length(hw)) add("warning", "number_word", sprintf("'%s' (often not a count; check): \"%s\"", paste(hw, collapse = "', '"), s$text), sec, sid)
    # economy names
    nh <- character()
    for (n in name_list) if (grepl(paste0("(?<![A-Za-z])", gsub("([.?+()\\[\\]])", "\\\\\\1", n), "(?![A-Za-z])"), msk_own, perl = TRUE)) nh <- c(nh, n)
    if (length(nh)) add("error", "economy_name", sprintf("economy name(s) outside a token: %s in \"%s\"", paste(nh, collapse = ", "), s$text), sec, sid)
    # corner words beside a sector token
    cw <- country_memo_has_word(msk_own, unlist(cfg$corner_words), ignore_case = FALSE)
    if (length(cw) && any(grepl("^cells\\.", tpaths))) add("error", "corner_word", sprintf("corner word %s in plain text beside a sector token (use {{cells.<key>.quadrant}}): \"%s\"", paste(cw, collapse = ", "), s$text), sec, sid)
    # secure / insecure as a label
    bl <- character()
    for (p in unlist(cfg$banned_labels)) if (grepl(paste0("(?<![A-Za-z])", p, "(?![A-Za-z])"), msk_own, perl = TRUE)) bl <- c(bl, p)
    if (grepl("(?<![A-Za-z])Insecure(?![A-Za-z])", msk_own, perl = TRUE)) bl <- c(bl, "Insecure")
    if (length(bl)) add("error", "secure_label", sprintf("'secure' or 'insecure' as a label (%s): \"%s\"", paste(unique(bl), collapse = ", "), s$text), sec, sid)
    # causal words
    cz <- country_memo_has_word(lower, unlist(cfg$causal_words))
    if (length(cz)) {
      bad <- character()
      for (t in toks) {
        p <- t$path
        dm <- regmatches(p, regexec("^cells\\.([^.]+)\\.drivers\\.(es|eo)\\.([^.]+)", p))[[1]]
        if (length(dm) == 4) {
          d <- idx$cells[[dm[2]]]$drivers[[dm[3]]][[dm[4]]]
          if (!isTRUE(d$top)) bad <- c(bad, paste0(p, " (not a top driver)"))
        }
        if (grepl("^cells\\.[^.]+\\.(trade|fdi)\\.|\\.ties\\.|\\.policy\\.records_|^policy\\.(pams|nipo)", p)) bad <- c(bad, paste0(p, " (context, not a cause)"))
      }
      if (length(bad)) add("error", "causal", sprintf("causal word(s) %s beside %s: \"%s\"", paste(sprintf("'%s'", cz), collapse = ", "), paste(unique(bad), collapse = "; "), s$text), sec, sid)
    }
    # order words
    ow <- country_memo_has_word(lower, unlist(cfg$order_words))
    if (length(ow)) {
      r <- country_memo_order_check(ow, s, toks, sheet, idx)
      if (!is.null(r)) add(r$severity, "order", sprintf("%s: \"%s\"", r$detail, s$text), sec, sid)
    }
    # acronyms (source names such as "IEA Policies and Measures" may stand in plain text)
    acr_txt <- msk_own
    for (p in unlist(cfg$acronym_phrases)) acr_txt <- gsub(p, " ", acr_txt, fixed = TRUE)
    acr <- unique(regmatches(acr_txt, gregexpr("(?<![A-Za-z0-9])[A-Z][A-Z0-9]+(?![A-Za-z0-9])", acr_txt, perl = TRUE))[[1]])
    acr <- setdiff(acr, allowed_acr)
    if (length(acr)) add("error", "acronym", sprintf("bare acronym(s) %s: \"%s\"", paste(acr, collapse = ", "), s$text), sec, sid)
    # tone
    tw <- country_memo_has_word(lower, unlist(cfg$tone_words))
    if (length(tw)) add("warning", "tone", sprintf("tone word(s) %s: \"%s\"", paste(sprintf("'%s'", tw), collapse = ", "), s$text), sec, sid)
    # sentence length
    nw <- country_memo_words_of(s$text)
    if (nw > (mc$sentence_words_max %||% 35)) add("warning", "sentence_length", sprintf("%d words: \"%s\"", nw, s$text), sec, sid)
    # capitalised words not on the allowlist (not sentence-initial)
    cap_txt <- msk_own
    for (a in allow_caps[order(-nchar(allow_caps))]) cap_txt <- gsub(paste0("(?<![A-Za-z])", gsub("([.?+()\\[\\]])", "\\\\\\1", a), "(?![A-Za-z])"), " ", cap_txt, perl = TRUE)
    caps <- regmatches(cap_txt, gregexpr("(?<![A-Za-z])[A-Z][a-z]+", cap_txt, perl = TRUE))[[1]]
    first <- regmatches(trimws(msk_own), regexpr("^[A-Z][a-z]+", trimws(msk_own)))
    # drop the sentence-initial word once, by logical index (caps[-match(..., nomatch = 0)] emptied caps
    # whenever that word was allowlisted, and so already masked: x[-0] is empty in R)
    if (length(first)) caps <- caps[!(seq_along(caps) %in% match(first, caps))]
    caps <- unique(caps)
    if (length(caps)) add("warning", "capitalised", sprintf("capitalised word(s) not on the allowlist: %s in \"%s\"", paste(caps, collapse = ", "), s$text), sec, sid)
    # fields outside the section's list
    if (sec >= 1) {
      allowed <- c(unlist(cfg$section_fields$common), unlist(cfg$section_fields[[as.character(sec)]]))
      for (p in unique(tpaths)) if (!any(vapply(allowed, function(a) country_memo_field_match(a, p), TRUE))) {
        add("warning", "section_field", sprintf("{{%s}} is outside the fields of section %d", p, sec), sec, sid)
      }
    }
  }
  # summary bullets carry a value each
  for (k in seq_along(res$items)) {
    it <- res$items[[k]]
    if (it$section == 0 && !length(it$spans)) add("error", "coverage", sprintf("summary bullet %d has no value (token)", it$block), 0)
  }
  # coverage (B3)
  tok_sec <- function(s) Filter(function(t) isTRUE(t$ok) && t$section == s, res$tokens)
  cells_with <- function(s, re) unique(unlist(lapply(tok_sec(s), function(t) if (grepl(re, t$path)) country_memo_cell_of(t$path) else NULL)))
  has <- function(s, re) any(vapply(tok_sec(s), function(t) grepl(re, t$path), TRUE))
  partners_with <- function(s, re) unique(unlist(lapply(tok_sec(s), function(t) if (grepl(re, t$path)) country_memo_partner_of(t$path) else NULL)))
  need <- function(s, ok, what) if (!isTRUE(ok)) add("error", "coverage", sprintf("section %d must cite %s", s, what), s)
  n_placed <- sheet$summary$n_placed$v %||% 0
  need(1, has(1, "^summary\\.n_insecure$"), "{{summary.n_insecure}}")
  need(1, has(1, "^summary\\.n_placed$"), "{{summary.n_placed}}")
  if (n_placed > 0) {
    need(1, length(cells_with(1, "^cells\\.[^.]+\\.es\\.pct$")) >= min(2, n_placed), "the Energy Security percentile ({{cells.<key>.es.pct}}) of two sectors or more")
    need(1, has(1, "^cells\\.[^.]+\\.quadrant$"), "a corner ({{cells.<key>.quadrant}})")
    raw_any <- any(vapply(sheet$cells, function(c) any(vapply(c$drivers$es, function(d) isTRUE(d$top) && any(vapply(d$vars, function(v) !is.null(v$raw$v), TRUE)), TRUE)), TRUE))
    if (raw_any) need(1, has(1, "^cells\\.[^.]+\\.drivers\\.es\\.[^.]+\\.vars\\.[^.]+\\.raw$"), "a raw figure behind an Energy Security driver ({{cells.<key>.drivers.es.<category>.vars.<variable>.raw}})")
    need(2, has(2, "^summary\\.n_high_eo$"), "{{summary.n_high_eo}}")
    need(2, length(cells_with(2, "^cells\\.[^.]+\\.eo\\.pct$")) >= min(2, n_placed), "the Economic Opportunity percentile ({{cells.<key>.eo.pct}}) of two sectors or more")
    need(2, has(2, "^cells\\.[^.]+\\.drivers\\.eo\\."), "an Economic Opportunity driver ({{cells.<key>.drivers.eo....}})")
  }
  if (isTRUE(m$has_si)) {
    n_si <- length(unlist(sheet$summary$si_order))
    need(3, length(cells_with(3, "^cells\\.[^.]+\\.si\\.rank$")) >= min(3, n_si), "the Strategic Index rank ({{cells.<key>.si.rank}}) of three sectors or more")
    if (isTRUE(sheet$policy$agreement$variation)) need(3, has(3, "^policy\\.agreement\\.by_class\\."), "the priorities-and-policy result ({{policy.agreement.by_class.<class>|list}})")
  }
  need(3, has(3, "^policy\\.cat\\.overall$|^meta\\.policy_basis$"), "{{policy.cat.overall}} or {{meta.policy_basis}}")
  need(3, has(3, "^policy\\.pams_total$"), "{{policy.pams_total}}")
  n_ranked <- sheet$partners$n_ranked$v %||% 0
  if (n_ranked > 0) {
    need(4, length(partners_with(4, "^partners\\.[A-Z0-9]{3}\\.rank$")) >= min(3, n_ranked), "the Partnership Strength rank ({{partners.<ISO3>.rank}}) of three partners or more")
    need(4, has(4, "^partners\\.[A-Z0-9]{3}\\.reason$"), "a partner's main reason ({{partners.<ISO3>.reason}})")
    need(4, has(4, "^partners\\.[A-Z0-9]{3}\\.(ties\\.tie_rank|match)$"), "a partner's trade rank or match ({{partners.<ISO3>.ties.tie_rank}} or {{partners.<ISO3>.match}})")
  }
  if (!is.null(sheet$summary$as_partner$rank$v)) need(4, has(4, "^summary\\.as_partner\\.rank$"), "{{summary.as_partner.rank}}")
  if (isTRUE(sheet$network$member)) need(4, has(4, "^network\\."), "the network role ({{network....}})")
  need(5, has(5, "^meta\\.as_of_label$"), "{{meta.as_of_label}}")
  # caveats
  trig <- country_memo_triggers(claims, sheet, cfg)
  printed <- unique(sub("^caveats\\.", "", vapply(Filter(function(t) grepl("^caveats\\.", t$path), tok_sec(5)), function(t) t$path, "")))
  sheet_cav <- vapply(sheet$caveats, function(c) c$id, "")
  for (id in trig) {
    if (!id %in% sheet_cav) add("error", "caveat", sprintf("caveat '%s' is triggered but the fact sheet does not carry it (exporter)", id), 5)
    else if (!id %in% printed) add("error", "caveat", sprintf("caveat '%s' is triggered by sections 1-4 (or always required) but not printed in section 5 as {{caveats.%s}}", id, id), 5)
  }
  list(items = out, triggered = trig, words = list(total = total, summary = words[1], sections = words[2:6]),
       n_errors = sum(vapply(out, function(x) x$severity == "error", TRUE)), n_warnings = sum(vapply(out, function(x) x$severity == "warning", TRUE)))
}

# Superlatives and order words follow the sheet's order lists (B5.6). Returns NULL (fine) or
# list(severity, detail).
country_memo_order_check <- function(ow, s, toks, sheet, idx) {
  paths <- vapply(toks, function(t) t$path, "")
  txt <- s$text
  fam <- character()
  if (any(grepl("^cells\\.[^.]+\\.(es(\\.|$)|drivers\\.es)", paths))) fam <- c(fam, "es")
  if (any(grepl("^cells\\.[^.]+\\.(eo(\\.|$)|drivers\\.eo)", paths))) fam <- c(fam, "eo")
  if (any(grepl("^cells\\.[^.]+\\.si(\\.|$)|^summary\\.top_si|^summary\\.si_order", paths))) fam <- c(fam, "si")
  if (any(grepl("^partners\\.[A-Z0-9]{3}\\.(rank|psi|fs|po|dp|reason)$|^summary\\.psi_1|^partners\\.psi_order", paths))) fam <- c(fam, "psi")
  if (any(grepl("^partners\\.[A-Z0-9]{3}\\.ties\\.|^partners\\.tie_order", paths))) fam <- c(fam, "tie")
  if (any(grepl("^cells\\.[^.]+\\.trade\\.(sources|markets)\\.[0-9]+", paths))) fam <- c(fam, "trade")
  if (!length(fam)) {
    if (grepl("Energy Security", txt)) fam <- c(fam, "es")
    if (grepl("Economic Opportunity", txt)) fam <- c(fam, "eo")
    if (grepl("Strategic Index", txt)) fam <- c(fam, "si")
    if (grepl("Partnership Strength", txt)) fam <- c(fam, "psi")
  }
  fam <- unique(fam)
  if (length(fam) != 1) {
    if (!length(fam)) return(NULL)
    return(list(severity = "warning", detail = sprintf("order word(s) %s with several possible orders (%s): the verifier checks it", paste(ow, collapse = ", "), paste(fam, collapse = ", "))))
  }
  low <- any(ow %in% c("lowest", "weakest", "smallest"))
  high <- any(ow %in% c("highest", "top", "largest", "strongest", "leading", "main", "biggest"))
  if (low && high) return(list(severity = "warning", detail = "both high and low order words: the verifier checks the order"))
  cells <- unique(stats::na.omit(unlist(lapply(toks, function(t) {
    k <- country_memo_cell_of(t$path)
    if (!is.na(k)) return(k)
    if (identical(t$kind, "list")) return(t$elements[t$elements %in% names(idx$cells)])
    if (!is.null(t$elements) && t$elements[1] %in% names(idx$cells)) return(t$elements[1])
    NA_character_
  }))))
  partners <- unique(stats::na.omit(unlist(lapply(toks, function(t) {
    k <- country_memo_partner_of(t$path)
    if (!is.na(k)) return(k)
    if (!is.null(t$elements) && any(t$elements %in% names(idx$partners$rows))) return(t$elements[t$elements %in% names(idx$partners$rows)])
    NA_character_
  }))))
  check_first <- function(order, cited, what, from_end = FALSE) {
    order <- unlist(order)
    k <- length(cited)
    if (!k) return(list(severity = "warning", detail = sprintf("order word(s) %s with no %s cited: the verifier checks it", paste(ow, collapse = ", "), what)))
    if (k > length(order)) return(list(severity = "warning", detail = sprintf("order word(s) with %d %s cited, beyond the order list", k, what)))
    want <- if (from_end) utils::tail(order, k) else utils::head(order, k)
    if (setequal(cited, want)) return(NULL)
    list(severity = "error", detail = sprintf("order word(s) %s: the %s cited are not the %s %d of the order list (%s; expected %s)",
                                              paste(sprintf("'%s'", ow), collapse = ", "), what, if (from_end) "last" else "first", k,
                                              paste(cited, collapse = ", "), paste(want, collapse = ", ")))
  }
  s_ <- sheet$summary
  switch(fam,
    es = check_first(s_$es_order, cells, "sectors", from_end = !low),
    eo = check_first(s_$eo_order, cells, "sectors", from_end = low),
    si = check_first(s_$si_order, cells, "sectors", from_end = low),
    psi = if (low) list(severity = "warning", detail = "a low order word about partners: the verifier checks it") else check_first(sheet$partners$psi_order, partners, "partners"),
    tie = if (low) list(severity = "warning", detail = "a low order word about trade partners: the verifier checks it") else check_first(sheet$partners$tie_order, partners, "partners"),
    trade = {
      pos <- unique(as.integer(sub("^cells\\.[^.]+\\.trade\\.(sources|markets)\\.([0-9]+).*$", "\\2", paths[grepl("^cells\\.[^.]+\\.trade\\.(sources|markets)\\.[0-9]+", paths)])))
      if (low) list(severity = "warning", detail = "a low order word about trade parties: the verifier checks it")
      else if (setequal(pos, seq_len(length(pos)) - 1L)) NULL
      else list(severity = "error", detail = sprintf("order word(s) %s: the trade parties cited are not the largest (positions %s)", paste(sprintf("'%s'", ow), collapse = ", "), paste(pos + 1L, collapse = ", ")))
    },
    NULL
  )
}

# ---- Document, sources, HTML, Markdown --------------------------------------------------------------------

# The memo document (B7.2) from the resolved items: value inlines carry their path, claim and source
# number; each item's refs are the distinct source numbers of its tokens; sources numbered in order of
# first use. A raw figure whose note names another source (config raw_note_sources: the eia_production
# note -> the U.S. Energy Information Administration, 2026-10-14) also credits that source when the sheet
# lists it; the claims are not touched (the extra credit is the document's, B6.5).
country_memo_document <- function(res, claims, sheet, cfg, meta) {
  src_of_tok <- list()
  extra_of_tok <- list()
  claim_of_tok <- list()
  sheet_src <- vapply(sheet$sources %||% list(), function(s) as.character(s$id %||% ""), "")
  for (c in claims) {
    k <- as.character(c$token)
    if (is.null(claim_of_tok[[k]])) claim_of_tok[[k]] <- c$id
    if (is.null(src_of_tok[[k]]) && !is.null(c$source)) src_of_tok[[k]] <- c$source
    note <- c$context$variable$note
    ex <- if (is.null(note)) NULL else cfg$raw_note_sources[[note]]
    if (!is.null(ex) && ex %in% sheet_src) extra_of_tok[[k]] <- unique(c(extra_of_tok[[k]], ex))
  }
  srcs_of <- function(tok) {
    s <- c(src_of_tok[[tok]], extra_of_tok[[tok]])
    s[!is.na(s)]
  }
  order_src <- character()
  for (it in res$items) for (n in it$nodes) if (n$t == "value") {
    for (s in srcs_of(as.character(n$token))) if (!s %in% order_src) order_src <- c(order_src, s)
  }
  src_n <- stats::setNames(seq_along(order_src), order_src)
  conv <- function(it) {
    refs <- integer()
    inl <- lapply(it$nodes, function(n) {
      if (n$t != "value") return(list(t = n$t, v = n$v))
      s <- src_of_tok[[as.character(n$token)]]
      sn <- if (is.null(s)) NULL else unname(src_n[[s]])
      ex <- unname(src_n[extra_of_tok[[as.character(n$token)]] %||% character()])
      refs <<- c(refs, sn, ex)
      list(t = "value", v = n$v, path = n$path, claim = claim_of_tok[[as.character(n$token)]] %||% "", src = sn)
    })
    list(inlines = inl, refs = I(sort(unique(refs))))
  }
  summary <- lapply(Filter(function(it) it$section == 0, res$items), conv)
  titles <- unlist(cfg$memo$sections)
  sections <- lapply(seq_along(titles), function(s) {
    its <- Filter(function(it) it$section == s, res$items)
    blocks <- list()
    for (b in unique(vapply(its, function(it) it$block, 1L))) {
      bi <- Filter(function(it) it$block == b, its)
      if (identical(bi[[1]]$type, "p")) {
        blocks[[length(blocks) + 1]] <- c(list(type = "p"), conv(bi[[1]]))
      } else blocks[[length(blocks) + 1]] <- list(type = "ul", items = lapply(bi, conv))
    }
    list(n = s, title = titles[s], blocks = blocks)
  })
  srcs <- lapply(seq_along(order_src), function(i) {
    id <- order_src[i]
    s <- Filter(function(x) identical(x$id, id), sheet$sources)
    s <- if (length(s)) s[[1]] else list(label = id)
    text <- country_memo_source_edition(s$label, s$edition)
    if (!is.null(s$licence) && nzchar(s$licence %||% "")) text <- paste0(text, "; ", s$licence)
    if (!is.null(s$url) && nzchar(s$url %||% "")) text <- paste0(text, "; ", s$url)
    list(n = i, id = id, text = paste0(text, "."))
  })
  m <- sheet$meta
  doc <- list(
    schema = COUNTRY_MEMO_SCHEMA, schema_version = COUNTRY_SCHEMA_VERSION, iso3 = sheet$iso3,
    title = country_fill(cfg$memo$title, list(country = m$country)), subtitle = cfg$memo$subtitle,
    label = country_fill(cfg$label, list(as_of_label = m$as_of_label)), as_of = m$as_of, as_of_label = m$as_of_label,
    generated_at = meta$generated_at, verified_at = meta$verified_at, writer = meta$writer, verifier = meta$verifier,
    words = 0L, facts_sha256 = sheet$checks$facts_sha256, src_sha256 = meta$src_sha256, claims_sha256 = meta$claims_sha256,
    summary = summary, sections = sections, sources = srcs, footer = country_fill(cfg$memo$footer, list(country = m$country))
  )
  doc$words <- country_memo_words(doc)
  doc
}

# A source's label with its edition, without nested brackets (2026-10-14): an edition with no brackets
# goes in brackets ("label (2025)"); one with brackets whose lead is already in the label keeps only its
# bracket ("Statistical Review of World Energy 2026" + "2026 (data to 2025)" -> "... 2026 (data to
# 2025)"); otherwise it follows a comma ("..., October 2026 extract (as of 5 October 2026)").
country_memo_source_edition <- function(label, edition) {
  label <- as.character(label %||% "")
  ed <- trimws(as.character(edition %||% ""))
  if (!length(ed) || is.na(ed[1]) || !nzchar(ed[1])) return(label)
  ed <- ed[1]
  if (!grepl("(", ed, fixed = TRUE)) return(paste0(label, " (", ed, ")"))
  lead <- trimws(sub("[(].*$", "", ed))
  rest <- trimws(sub("^[^(]*", "", ed))
  if (nzchar(lead) && grepl(lead, label, fixed = TRUE) && grepl("^[(][^()]*[)]$", rest)) return(paste0(label, " ", rest))
  paste0(label, ", ", ed)
}

# Paragraph texts of a memo document in reading order, as Word reads them back (superscript source
# numbers joined to the text: "...in 2024.1,3"): title, subtitle, label, "Summary", bullets, numbered
# headings, blocks, "Sources", sources, the label again. scripts/utils/country_memo_docx.py builds the
# same list from the AST for its read-back check.
country_memo_texts <- function(doc) {
  it <- function(x) paste0(paste(vapply(x$inlines, function(n) n$v, ""), collapse = ""), if (length(x$refs)) paste(unlist(x$refs), collapse = ",") else "")
  out <- c(doc$title, doc$subtitle, doc$label, "Summary", vapply(doc$summary, it, ""))
  for (s in doc$sections) {
    out <- c(out, sprintf("%d. %s", s$n, s$title))
    for (b in s$blocks) out <- c(out, if (identical(b$type, "ul")) vapply(b$items, it, "") else it(b))
  }
  c(out, "Sources", vapply(doc$sources, function(s) s$text, ""), doc$label)
}

country_memo_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# The print HTML (B7.3): self-contained, light NEIS print style, Roboto embedded when font_b64 is given.
country_memo_html <- function(doc, font_b64 = NULL, page_size = "Letter") {
  inl <- function(xs) paste(vapply(xs, function(n) {
    v <- country_memo_esc(n$v)
    switch(n$t, strong = paste0("<strong>", v, "</strong>"), em = paste0("<em>", v, "</em>"), v)
  }, ""), collapse = "")
  item <- function(x) paste0(inl(x$inlines), if (length(x$refs)) paste0("<sup>", paste(unlist(x$refs), collapse = ","), "</sup>") else "")
  font <- if (!is.null(font_b64) && nzchar(font_b64)) {
    paste0("@font-face{font-family:'Roboto';font-style:normal;font-weight:100 900;src:url(data:font/woff2;base64,", font_b64, ") format('woff2');}")
  } else ""
  family <- if (nzchar(font)) "'Roboto',Arial,sans-serif" else "Arial,sans-serif"
  css <- paste0(font,
    "@page{size:", page_size, ";margin:0.75in}",
    "html{-webkit-print-color-adjust:exact;print-color-adjust:exact}",
    "body{font-family:", family, ";font-size:10.5pt;line-height:1.45;color:#111827;margin:0}",
    ".rule{width:85px;height:6px;background:#f58229;margin:0 0 14px 0}",
    "h1{font-size:20pt;font-weight:500;margin:0 0 6px 0;line-height:1.2}",
    ".sub{font-size:9pt;text-transform:uppercase;letter-spacing:0.08em;color:#4b5563;margin:0 0 10px 0}",
    ".label{font-size:9.5pt;font-style:italic;color:#4b5563;border-top:1px solid #d1d5db;padding-top:6px;margin:0 0 16px 0}",
    ".summary{background:#f3f4f6;border-left:4px solid #f58229;padding:10px 14px 6px 14px;margin:0 0 18px 0;break-inside:avoid}",
    ".summary h2{font-size:12pt;font-weight:500;margin:0 0 6px 0}",
    "ul{margin:0 0 10px 0;padding-left:18px}li{margin:0 0 4px 0}",
    "h2{font-size:13pt;font-weight:500;margin:16px 0 6px 0;break-after:avoid}",
    "p{margin:0 0 8px 0}sup{font-size:7.5pt;color:#4b5563;line-height:0}",
    ".sources{font-size:8.5pt;color:#374151;border-top:1px solid #d1d5db;margin-top:16px;padding-top:6px}",
    ".sources h2{font-size:10pt;margin:4px 0 4px 0}.sources ol{margin:0;padding-left:18px}",
    ".end{font-size:9pt;font-style:italic;color:#4b5563;border-top:1px solid #d1d5db;margin-top:14px;padding-top:6px}")
  body <- c(
    "<div class=\"rule\"></div>",
    paste0("<h1>", country_memo_esc(doc$title), "</h1>"),
    paste0("<p class=\"sub\">", country_memo_esc(doc$subtitle), "</p>"),
    paste0("<p class=\"label\">", country_memo_esc(doc$label), "</p>"),
    "<div class=\"summary\"><h2>Summary</h2><ul>",
    vapply(doc$summary, function(x) paste0("<li>", item(x), "</li>"), ""),
    "</ul></div>"
  )
  for (s in doc$sections) {
    body <- c(body, sprintf("<h2>%d. %s</h2>", s$n, country_memo_esc(s$title)))
    for (b in s$blocks) {
      body <- c(body, if (identical(b$type, "ul")) paste0("<ul>", paste(vapply(b$items, function(x) paste0("<li>", item(x), "</li>"), ""), collapse = ""), "</ul>") else paste0("<p>", item(b), "</p>"))
    }
  }
  body <- c(body, "<div class=\"sources\"><h2>Sources</h2><ol>",
            vapply(doc$sources, function(s) paste0("<li value=\"", s$n, "\">", country_memo_esc(s$text), "</li>"), ""),
            "</ol></div>",
            paste0("<p class=\"end\">", country_memo_esc(doc$label), "<br>", country_memo_esc(doc$footer %||% ""), "</p>"))
  html <- paste0("<!doctype html><html lang=\"en-GB\"><head><meta charset=\"utf-8\"><title>", country_memo_esc(doc$title), "</title><style>", css,
                 "</style></head><body>", paste(body, collapse = "\n"), "</body></html>")
  html
}

# No external reference of any kind in the print HTML (B7.3).
country_memo_html_external <- function(html) {
  m <- regmatches(html, gregexpr("(src|href)\\s*=\\s*\"[^\"]*\"|url\\([^)]*\\)", html, perl = TRUE))[[1]]
  m <- m[grepl("https?:|//", m) & !grepl("^url\\(data:", m)]
  unique(m)
}

# Plain Markdown of the rendered memo (copy-paste; linked from About).
country_memo_markdown <- function(doc) {
  inl <- function(xs) paste(vapply(xs, function(n) switch(n$t, strong = paste0("**", n$v, "**"), em = paste0("*", n$v, "*"), n$v), ""), collapse = "")
  item <- function(x) paste0(inl(x$inlines), if (length(x$refs)) paste0(" [", paste(unlist(x$refs), collapse = ", "), "]") else "")
  out <- c(paste0("# ", doc$title), "", doc$subtitle, "", paste0("*", doc$label, "*"), "", "## Summary", "",
           vapply(doc$summary, function(x) paste0("- ", item(x)), ""), "")
  for (s in doc$sections) {
    out <- c(out, sprintf("## %d. %s", s$n, s$title), "")
    for (b in s$blocks) out <- c(out, if (identical(b$type, "ul")) c(vapply(b$items, function(x) paste0("- ", item(x)), ""), "") else c(item(b), ""))
  }
  out <- c(out, "## Sources", "", vapply(doc$sources, function(s) paste0(s$n, ". ", s$text), ""), "", paste0("*", doc$label, "*"), "")
  paste(out, collapse = "\n")
}

# ---- The claims file and the verifier's record ----------------------------------------------------------

# The claims file (B6.2): claims, sentences and the triggered caveats. Its sha256 is taken over this
# canonical JSON, which carries no fact-sheet hash, so a re-export that changes nothing a claim says
# keeps it (B6.5).
country_memo_claims_doc <- function(iso3, claims, sents, triggered) {
  list(schema = "osi-country-memo-claims", iso3 = iso3,
       claims = lapply(claims, function(c) { c$token <- NULL; c }),
       sentences = lapply(sents$sentences, function(s) list(id = s$id, section = s$section, text = s$text)),
       triggered_caveats = I(triggered))
}

# Completeness of a verifier's record (B6.3): every claim and sentence id, every lint warning cleared,
# matching hashes, zero error issues, status pass. Returns list(ok, problems, reuse) where reuse = the
# record signed an earlier fact sheet but the claims are unchanged (B6.5).
country_memo_verify_check <- function(verify, claims_doc, lint, src_sha, facts_sha, claims_sha) {
  pr <- character()
  if (is.null(verify)) return(list(ok = FALSE, problems = "no verify.json", reuse = FALSE))
  if (!identical(verify$status, "pass")) pr <- c(pr, sprintf("status is '%s', not 'pass'", verify$status %||% "missing"))
  ids <- function(x) vapply(x %||% list(), function(r) as.character(r$id %||% ""), "")
  ok_ids <- function(x) vapply(x %||% list(), function(r) identical(r$verdict, "ok"), TRUE)
  want_c <- vapply(claims_doc$claims, function(c) c$id, "")
  want_s <- vapply(claims_doc$sentences, function(s) s$id, "")
  got_c <- ids(verify$claims_checked)
  got_s <- ids(verify$sentences_checked)
  if (!setequal(got_c, want_c)) pr <- c(pr, sprintf("claims_checked covers %d of %d claims (missing %s)", length(intersect(got_c, want_c)), length(want_c),
                                                    paste(utils::head(setdiff(want_c, got_c), 5), collapse = ", ")))
  if (!setequal(got_s, want_s)) pr <- c(pr, sprintf("sentences_checked covers %d of %d sentences (missing %s)", length(intersect(got_s, want_s)), length(want_s),
                                                    paste(utils::head(setdiff(want_s, got_s), 5), collapse = ", ")))
  warn_ids <- vapply(Filter(function(x) x$severity == "warning", lint$items), function(x) x$id, "")
  cleared <- vapply(verify$warnings_cleared %||% list(), function(w) as.character(w$lint_id %||% ""), "")
  if (length(setdiff(warn_ids, cleared))) pr <- c(pr, sprintf("lint warnings not cleared: %s", paste(utils::head(setdiff(warn_ids, cleared), 8), collapse = ", ")))
  errs <- Filter(function(i) identical(i$severity, "error"), verify$issues %||% list())
  if (length(errs)) pr <- c(pr, sprintf("%d error issue(s) open", length(errs)))
  issue_ids <- c(got_c[!ok_ids(verify$claims_checked)], got_s[!ok_ids(verify$sentences_checked)])
  if (length(issue_ids) && !length(errs)) pr <- c(pr, sprintf("%d claim or sentence verdicts are not 'ok'", length(issue_ids)))
  if (!identical(verify$claims_sha256, claims_sha)) pr <- c(pr, "claims_sha256 does not match the current claims file")
  if (!identical(verify$src_sha256, src_sha)) pr <- c(pr, "src_sha256 does not match the current memo source")
  # a later fact sheet whose claims are all unchanged (values, meanings, contexts, triggers) keeps the
  # sign-off (B6.5); the caller records the new hash in reused_for
  reuse <- !identical(verify$facts_sha256, facts_sha) && identical(verify$claims_sha256, claims_sha)
  list(ok = !length(pr), problems = pr, reuse = reuse)
}
