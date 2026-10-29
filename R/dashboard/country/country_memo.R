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

# The node at a dot path ("cells.oil-extraction.es.pct"); NULL when absent. Numeric segments index
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
  sections[[5]]$blocks <- list(list(type = "p", inlines = list(txt("All index values are the NEIS baseline as of "), val("meta.as_of_label", "c5"), txt(".")), refs = list(1L)))
  doc <- list(
    schema = COUNTRY_MEMO_SCHEMA, schema_version = COUNTRY_SCHEMA_VERSION, iso3 = sheet$iso3,
    title = country_fill(cfg$memo$title, list(country = m$country)), subtitle = cfg$memo$subtitle,
    label = country_fill(cfg$label, list(as_of_label = m$as_of_label)), as_of = m$as_of, as_of_label = m$as_of_label,
    generated_at = m$created, verified_at = m$created, writer = "placeholder", verifier = "placeholder",
    words = 0L, facts_sha256 = sheet$checks$facts_sha256, src_sha256 = "placeholder", claims_sha256 = "placeholder",
    summary = summary, sections = sections,
    sources = list(list(n = 1L, id = "osi_indices", text = "Energy Security, Economic Opportunity, Strategic Index and Partnership Strength Index, NEIS baseline at six supply-chain stages.")),
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

# ---- Memos kept under an earlier quadrant rule (the user's decision of 2026-10-15) -----------------------
# The quadrant rule changed on 2026-10-15 (a sector's corner is set against the country's own median
# sector, COUNTRY_QUADRANT_RULES); the 31 memos were written on 2026-10-14 under the earlier rule (the
# sector's median economy). scripts/88 keeps such a memo published, with a note, when its claims differ
# from the new fact sheet only where the quadrant rule decides them: every claim of claims.json must
# resolve to the same value on a copy of the new sheet built under the earlier rule ("sector_median").

COUNTRY_MEMO_RULE_NOTE <- "quadrant_rule_2026_10_15"

# Re-resolve a memo's claims against a sheet with the memo tooling (parse, resolve, sentences, claims).
country_memo_reclaim <- function(src, sheet, cfg) {
  parsed <- country_memo_parse(src, cfg)
  res <- country_memo_resolve(parsed, sheet, cfg)
  sents <- country_memo_sentences(res)
  country_memo_claims(res, sents, sheet, cfg)
}

# One entry per token occurrence of a claims list (claims.json$claims or country_memo_claims()): the
# element claims of a |list token collapse into one entry with its elements. list(path, filter, d, v,
# elements).
country_memo_claim_tokens <- function(claims) {
  # plain values only: a re-resolved display string can carry names (a one-element agreement list)
  txt <- function(x) if (is.null(x)) NA_character_ else unname(as.character(x))[1]
  out <- list()
  for (c in claims) {
    f <- txt(c$filter)
    el <- if (is.null(c$element)) NULL else unname(as.character(unlist(c$element)))
    p <- txt(c$path)
    sid <- txt(c$sentence_id)
    d <- txt(c$d)
    n <- length(out)
    if (n && identical(f, "list") && identical(out[[n]]$filter, "list") && identical(out[[n]]$sentence_id, sid) &&
        identical(out[[n]]$path, p) && identical(out[[n]]$d, d) && !is.null(el) && !el %in% out[[n]]$elements) {
      out[[n]]$elements <- c(out[[n]]$elements, el)
      next
    }
    out[[n + 1]] <- list(path = p, filter = f, sentence_id = sid, d = d,
                         v = if (is.null(c$v)) NA_real_ else suppressWarnings(as.numeric(unname(c$v)))[1], elements = if (is.null(el)) character() else el)
  }
  out
}

# The paths of the tokens whose value differs between two claims lists (stored = the memo's claims.json,
# fresh = re-resolved). A different number of tokens (a token that no longer resolves) gives "(structure)".
country_memo_claim_diff <- function(stored, fresh) {
  a <- country_memo_claim_tokens(stored)
  b <- country_memo_claim_tokens(fresh)
  if (length(a) != length(b)) return("(structure)")
  same_v <- function(x, y) (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && abs(x - y) <= 1e-9 * max(1, abs(x)))
  bad <- vapply(seq_along(a), function(i) {
    !identical(a[[i]]$path, b[[i]]$path) || !identical(a[[i]]$filter, b[[i]]$filter) || !identical(a[[i]]$d, b[[i]]$d) ||
      !same_v(a[[i]]$v, b[[i]]$v) || !identical(a[[i]]$elements, b[[i]]$elements)
  }, TRUE)
  unique(vapply(a[bad], function(x) x$path, ""))
}

# The sectors whose corner differs between two sheets (the new rule and the earlier one).
country_quadrants_moved <- function(sheet_new, sheet_old) {
  q <- function(s) stats::setNames(vapply(s$cells, function(c) c$quadrant %||% "", ""), vapply(s$cells, function(c) c$key, ""))
  a <- q(sheet_new)
  b <- q(sheet_old)
  k <- intersect(names(a), names(b))
  k[a[k] != b[k]]
}

# The status of a published memo whose fact sheet changed. stored: claims.json$claims; fresh_new /
# fresh_old: the claims re-resolved on the new sheet and on its copy under the earlier rule; moved:
# country_quadrants_moved(). Returns list(status = "current" | "rule_note" | "stale", changed = token
# paths that differ on the new sheet, other = token paths that differ even under the earlier rule).
# "stale": some claim changed for another reason than the rule; "rule_note": only quadrant-derived claims
# changed, or the country's corners moved (memo prose about corners may no longer match the chart);
# "current": nothing the memo states changed.
country_memo_rule_status <- function(stored, fresh_new, fresh_old, moved = character()) {
  other <- country_memo_claim_diff(stored, fresh_old)
  changed <- country_memo_claim_diff(stored, fresh_new)
  status <- if (length(other)) "stale" else if (length(changed) || length(moved)) "rule_note" else "current"
  list(status = status, changed = changed, other = other)
}

# The note's words for a memo kept under the earlier rule (config memo_rule_notes; {country}, {written}).
country_memo_rule_note_words <- function(cfg, note, country, generated_at) {
  tpl <- cfg$memo_rule_notes[[note]] %||% stop("config memo_rule_notes has no '", note, "'")
  day <- tryCatch(as.Date(substr(generated_at %||% "", 1, 10)), error = function(e) NA)
  written <- if (is.na(day)) "an earlier date" else paste(as.integer(format(day, "%d")), month.name[as.integer(format(day, "%m"))], format(day, "%Y"))
  country_fill_caveat(tpl, list(country = country, written = written))
}

# ---- Memos under revision (the user's decision of 2026-10-17) ---------------------------------------------
# The original allied network design is retired, so the 31 memos written before it went are being
# rewritten. config memo_revision$before (an ISO 8601 UTC time) hides every memo generated before it: the
# manifest marks it stale and the UI says the memo is being revised; the memo files are never touched. A
# memo rendered after the cut-off is shown as usual. TRUE when `memo` (a manifest memo entry) is under
# revision; a memo without generated_at counts as written before.
country_memo_under_revision <- function(memo, revision = NULL) {
  before <- revision$before %||% NULL
  if (is.null(before) || !nzchar(before)) return(FALSE)
  at <- memo$generated_at %||% ""
  !nzchar(at) || at < before
}
