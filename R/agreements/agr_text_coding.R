# Keyword coding of agreement titles and texts into sectors and instruments
# (config/partnership_agreements.yml: sectors, instruments, text_coding). Pure.
#
# Two bases (config coding_bases):
#   text_title  a pattern matches the title;
#   text_body   a pattern matches a sentence of the body that also matches a cooperation pattern
#               (text_coding$cooperation_patterns), so a sector named in passing, in a complaint or
#               in a list of grievances is not read as cooperation on it.
# Each code keeps the number of matching sentences (n_hits) and a short snippet around the first
# match, so that the dashboard can show why a document was coded. Keyword coding finds what a
# text mentions; it does not establish that the parties created the instrument, and the codes'
# basis says so wherever they are shown.

# Split texts into sentences: on . ! ? ; and the CJK full stops, then cut pieces longer than
# max_chars. Returns data.frame(doc, sentence).
agr_split_sentences <- function(ids, texts, max_chars = 600L) {
  texts <- agr_chr(texts)
  out_id <- list()
  out_s <- list()
  for (i in seq_along(texts)) {
    t <- texts[i]
    if (!nzchar(t)) next
    t <- gsub("[\r\n\t]+", " ", t)
    parts <- unlist(strsplit(t, "(?<=[.!?;])\\s+|(?<=[。！？；])", perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts <- parts[nchar(parts) >= 3]
    long <- nchar(parts) > max_chars
    if (any(long)) {
      pieces <- lapply(parts, function(p) {
        if (nchar(p) <= max_chars) return(p)
        starts <- seq(1L, nchar(p), by = max_chars)
        substring(p, starts, pmin(starts + max_chars - 1L, nchar(p)))
      })
      parts <- unlist(pieces, use.names = FALSE)
    }
    if (length(parts) == 0) next
    out_id[[length(out_id) + 1]] <- rep(ids[i], length(parts))
    out_s[[length(out_s) + 1]] <- parts
  }
  data.frame(doc = as.character(unlist(out_id)), sentence = as.character(unlist(out_s)),
             stringsAsFactors = FALSE)
}

agr_pattern_union <- function(patterns) {
  patterns <- unlist(patterns, use.names = FALSE)
  patterns <- patterns[nzchar(patterns)]
  if (length(patterns) == 0) return(NA_character_)
  paste0("(?:", paste(patterns, collapse = ")|(?:"), ")")
}

# A snippet of `width` characters around the first match of `pattern` in each text.
agr_snippet <- function(texts, pattern, width = 160L) {
  m <- regexpr(pattern, texts, perl = TRUE, ignore.case = TRUE)
  start <- pmax(1L, m - as.integer(width / 2))
  out <- substr(texts, start, start + width - 1L)
  out <- gsub("\\s+", " ", out)
  ifelse(m > 0, trimws(out), "")
}

# Code titles and bodies. `ids` are source_record_ids; `titles` and `bodies` align with them
# (bodies may be ""). `dictionary` is cfg$sectors or cfg$instruments; `dimension` is "sector" or
# "instrument_types". Returns a codes data.frame (AGR_CODE_COLUMNS).
agr_code_text <- function(ids, titles, bodies, dictionary, dimension, text_cfg) {
  ids <- agr_chr(ids)
  titles <- agr_chr(titles)
  bodies <- if (is.null(bodies)) rep("", length(ids)) else agr_chr(bodies)
  coop <- agr_pattern_union(text_cfg$cooperation_patterns)
  max_chars <- if (is.null(text_cfg$max_sentence_chars)) 600L else as.integer(text_cfg$max_sentence_chars)
  width <- if (is.null(text_cfg$snippet_chars)) 160L else as.integer(text_cfg$snippet_chars)
  sentences <- agr_split_sentences(ids, bodies, max_chars)
  if (nrow(sentences) > 0 && !is.na(coop)) {
    sentences <- sentences[grepl(coop, sentences$sentence, perl = TRUE, ignore.case = TRUE), , drop = FALSE]
  }
  out <- list()
  for (code in names(dictionary)) {
    pat <- agr_pattern_union(dictionary[[code]]$patterns)
    if (is.na(pat)) next
    hit_t <- nzchar(titles) & grepl(pat, titles, perl = TRUE, ignore.case = TRUE)
    if (any(hit_t)) {
      out[[length(out) + 1]] <- data.frame(
        source_record_id = ids[hit_t], dimension = dimension, code = code, basis = "text_title",
        n_hits = 1L, snippet = agr_snippet(titles[hit_t], pat, width), stringsAsFactors = FALSE)
    }
    if (nrow(sentences) > 0) {
      hit_s <- grepl(pat, sentences$sentence, perl = TRUE, ignore.case = TRUE)
      if (any(hit_s)) {
        hs <- sentences[hit_s, , drop = FALSE]
        n <- table(hs$doc)
        first <- hs[!duplicated(hs$doc), , drop = FALSE]
        out[[length(out) + 1]] <- data.frame(
          source_record_id = first$doc, dimension = dimension, code = code, basis = "text_body",
          n_hits = as.integer(n[first$doc]), snippet = agr_snippet(first$sentence, pat, width),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out) == 0) return(agr_empty_codes())
  agr_as_codes(do.call(rbind, out))
}

# Sector and instrument codes of titles and bodies, with the technology and segment codes the
# sectors imply.
agr_code_documents <- function(ids, titles, bodies, cfg) {
  sec <- agr_code_text(ids, titles, bodies, cfg$sectors, "sector", cfg$text_coding)
  ins <- agr_code_text(ids, titles, bodies, cfg$instruments, "instrument_types", cfg$text_coding)
  agr_derive_tech_codes(rbind(sec, ins), cfg)
}
