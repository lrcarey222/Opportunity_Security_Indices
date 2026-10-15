# The policy memos of Country Insights (spec_country.md part B): parse a memo source (Markdown with a
# :::summary block and {{token}} placeholders), resolve tokens against the fact sheet, build the
# claims file, lint, count words, and build the document (AST) that the screen, the PDF and the Word
# file are rendered from. Pure: text and lists in, text and lists out.

COUNTRY_MEMO_SCHEMA <- "osi-country-memo"

# ---- Token resolution ---------------------------------------------------------------------------------

# Index a fact sheet for path lookups: arrays of objects addressed by their key (cells by key,
# partners.rows by iso3 and also partners.<ISO3>, drivers and variables by key, caveats by id).
country_memo_index <- function(sheet) {
  keyed <- function(xs, field) stats::setNames(xs, vapply(xs, function(x) as.character(x[[field]] %||% ""), ""))
  idx <- sheet
  idx$cells <- keyed(lapply(sheet$cells, function(c) {
    c$drivers <- list(es = keyed(lapply(c$drivers$es, function(d) { d$vars <- keyed(d$vars, "key"); d }), "key"),
                      eo = keyed(lapply(c$drivers$eo, function(d) { d$vars <- keyed(d$vars, "key"); d }), "key"))
    c
  }), "key")
  rows <- keyed(sheet$partners$rows, "iso3")
  idx$partners <- c(sheet$partners, rows)
  idx$partners$rows <- rows
  idx$caveats <- keyed(sheet$caveats, "id")
  idx
}

# The node at a dot path ("cells.oil-upstream.es.pct"); NULL when absent. Numeric segments index
# plain arrays (0-based, as in the spec: trade.sources.0).
country_memo_get <- function(idx, path) {
  x <- idx
  for (seg in strsplit(path, ".", fixed = TRUE)[[1]]) {
    if (is.null(x)) return(NULL)
    if (grepl("^[0-9]+$", seg) && is.null(names(x))) {
      i <- as.integer(seg) + 1L
      x <- if (i <= length(x)) x[[i]] else NULL
    } else if (is.list(x) && !is.null(names(x)) && seg %in% names(x)) {
      x <- x[[seg]]
    } else return(NULL)
  }
  x
}

# The enum group of a path whose value prints through `words` ("cells.*.quadrant" -> "quadrant").
country_memo_enum_group <- function(path) {
  last <- sub("^.*\\.", "", path)
  p <- gsub("\\.[^.]+", ".*", path)
  if (grepl("^cells\\.[^.]+\\.quadrant$", path)) return("quadrant")
  if (grepl("^cells\\.[^.]+\\.priority\\.third$", path)) return("third")
  if (grepl("^cells\\.[^.]+\\.policy\\.third$", path)) return("policy_third")
  if (grepl("^cells\\.[^.]+\\.policy\\.class$", path)) return("class")
  if (grepl("^partners\\.[A-Z0-9]{3}\\.reason$", path)) return("reason")
  if (grepl("^partners\\.[A-Z0-9]{3}\\.match$", path)) return("match")
  if (grepl("^partners\\.[A-Z0-9]{3}\\.tier$", path) || identical(path, "meta.tier")) return("tier")
  if (grepl("^partners\\.[A-Z0-9]{3}\\.group$", path)) return("group")
  # 2026-10-14: the Policy index basis in words (no bare acronyms) and the scope of a listed NIPO measure
  if (identical(path, "meta.policy_basis")) return("policy_basis")
  if (grepl("^policy\\.nipo\\.policies\\.[0-9]+\\.scope$", path)) return("nipo_scope")
  NULL
}

# Resolve one token (path and optional filter) to its display text, or an error string.
# Returns list(ok, text, kind = "fv"|"string"|"enum"|"list", elements (for |list), v, error).
country_memo_resolve_token <- function(idx, path, filter = NULL) {
  x <- country_memo_get(idx, path)
  if (is.null(x)) return(list(ok = FALSE, error = paste0("{{", path, "}} does not resolve")))
  if (!is.null(filter) && filter %in% c("list", "n")) {
    els <- unlist(x)
    if (!is.character(els)) return(list(ok = FALSE, error = paste0("{{", path, "|", filter, "}}: not an array of keys")))
    if (filter == "n") return(list(ok = TRUE, text = country_fmt(length(els), "count"), kind = "count", v = length(els), elements = els))
    names_of <- vapply(els, function(k) {
      c <- idx$cells[[k]]
      if (!is.null(c)) return(c$label_prose)
      r <- idx$partners$rows[[k]]
      if (!is.null(r)) return(r$name)
      NA_character_
    }, "")
    if (anyNA(names_of)) return(list(ok = FALSE, error = paste0("{{", path, "|list}}: unknown element ", els[is.na(names_of)][1])))
    txt <- if (length(names_of) <= 1) paste(names_of, collapse = "") else paste0(paste(utils::head(names_of, -1), collapse = ", "), " and ", utils::tail(names_of, 1))
    if (!length(names_of)) return(list(ok = FALSE, error = paste0("{{", path, "|list}}: empty list")))
    return(list(ok = TRUE, text = txt, kind = "list", elements = els))
  }
  grp <- country_memo_enum_group(path)
  if (!is.null(grp) && is.character(x) && length(x) == 1) {
    w <- idx$words[[grp]][[x]]
    if (is.null(w)) return(list(ok = FALSE, error = paste0("{{", path, "}}: no word for ", grp, ".", x)))
    txt <- w
  } else if (is.list(x) && "d" %in% names(x)) {
    if (is.null(x$v) && !grepl("\\.(dp)$", path)) return(list(ok = FALSE, error = paste0("{{", path, "}} resolves to null")))
    txt <- x$d
    if (!nzchar(txt)) return(list(ok = FALSE, error = paste0("{{", path, "}}: empty display string")))
    return(list(ok = TRUE, text = if (!is.null(filter) && filter == "lower") tolower(txt) else txt, kind = "fv", v = x$v))
  } else if (is.list(x) && "text" %in% names(x) && "id" %in% names(x)) {
    txt <- x$text
  } else if (is.character(x) && length(x) == 1) {
    if (grepl("^partners\\.[A-Z0-9]{3}\\.ties\\.agreements$|^meta\\.agreements$", path)) return(list(ok = FALSE, error = paste0("{{", path, "}}: use |list on arrays")))
    txt <- x
  } else if (is.numeric(x)) {
    return(list(ok = FALSE, error = paste0("{{", path, "}} resolves to a plain number")))
  } else {
    return(list(ok = FALSE, error = paste0("{{", path, "}} resolves to an object without a display string")))
  }
  if (!is.null(filter) && filter == "lower") txt <- tolower(txt)
  list(ok = TRUE, text = txt, kind = if (!is.null(grp)) "enum" else "string", v = NULL)
}

# ---- Placeholder (the synthetic fixture, until the memo phase writes real memos) ------------------------

country_placeholder_memo <- function(sheet, cfg) {
  m <- sheet$meta
  idx <- country_memo_index(sheet)
  val <- function(path, claim) {
    r <- country_memo_resolve_token(idx, path)
    list(t = "value", v = if (isTRUE(r$ok)) r$text else COUNTRY_DASH, path = path, claim = claim)
  }
  txt <- function(s) list(t = "text", v = s)
  top <- sheet$summary$top_si$cell %||% sheet$cells[[1]]$key
  p1 <- sheet$partners$psi_order[[1]] %||% NULL
  summary <- list(
    list(inlines = list(txt("Placeholder memo for the dashboard fixture. "), val("summary.n_insecure", "c1"), txt(" of "), val("summary.n_placed", "c2"),
                        txt(" placed sectors sit in the two low-energy-security corners.")), refs = list(1L)),
    list(inlines = list(txt("Its top Strategic Index sector among its own sectors is "), val(paste0("cells.", top, ".label_prose"), "c3"), txt(".")), refs = list(1L)),
    list(inlines = list(txt("The memo phase replaces this text with an AI-generated memo checked against the fact sheet.")), refs = list()),
    list(inlines = c(list(txt("The Partnership Strength Index rates ")), if (!is.null(p1)) list(val(paste0("partners.", p1, ".name"), "c4")) else list(txt("no partner")),
                     list(txt(" highest."))), refs = list(1L))
  )
  sec <- function(n, title, body) list(n = n, title = title, blocks = list(list(type = "p", inlines = list(txt(body)), refs = list())))
  titles <- cfg$memo$sections
  sections <- lapply(seq_along(titles), function(i) sec(i, titles[[i]], "Placeholder section: the memo phase writes this section from the fact sheet."))
  sections[[5]]$blocks <- list(list(type = "p", inlines = list(txt("All index values are the paper's baseline as of "), val("meta.as_of_label", "c5"), txt(".")), refs = list(1L)))
  doc <- list(
    schema = COUNTRY_MEMO_SCHEMA, schema_version = COUNTRY_SCHEMA_VERSION, iso3 = sheet$iso3,
    title = country_fill(cfg$memo$title, list(country = m$country)), subtitle = cfg$memo$subtitle,
    label = country_fill(cfg$label, list(as_of_label = m$as_of_label)), as_of = m$as_of, as_of_label = m$as_of_label,
    generated_at = m$created, verified_at = m$created, writer = "placeholder", verifier = "placeholder",
    words = 0L, facts_sha256 = sheet$checks$facts_sha256, src_sha256 = "placeholder", claims_sha256 = "placeholder",
    summary = summary, sections = sections,
    sources = list(list(n = 1L, id = "osi_indices", text = "Energy Security, Economic Opportunity, Strategic Index and Partnership Strength Index, paper baseline.")),
    placeholder = TRUE
  )
  doc$words <- country_memo_words(doc)
  doc
}

# Words of a memo document: the summary and sections 1-5, tokens as printed, split on white space.
country_memo_words <- function(doc) {
  item_text <- function(it) paste(vapply(it$inlines, function(n) n$v, ""), collapse = "")
  t <- c(vapply(doc$summary, item_text, ""),
         unlist(lapply(doc$sections, function(s) unlist(lapply(s$blocks, function(b) if (identical(b$type, "ul")) vapply(b$items, item_text, "") else item_text(b))))))
  sum(lengths(regmatches(t, gregexpr("[^[:space:]]+", t))))
}
