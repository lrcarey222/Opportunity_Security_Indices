# Pipeline-level QC: the check-row contract shared by every R QC module (docs/QC_SYSTEM_DESIGN.md 6.2).
#
# One check is one row: {check_id, layer, area, table, status, n, detail, examples, needs_human_explanation}.
# status is info | warn | fail. 'fail' is reserved for a result the pipeline cannot be trusted with (an
# irreproducible choice, a broken key, an impossible value in a decision input); a gap that a human should
# explain is warn with needs_human_explanation; a fact worth recording (a gap made explicit, a count) is info.
# A passing check is still written as an info row so the absence of a problem is visible, and so a reader can
# tell "checked, nothing found" from "not checked". Passing checks are not evidence that the analysis is right.
#
# Pure functions: no IO.

QC_STATUSES <- c("info", "warn", "fail")
QC_CHECK_COLS <- c("check_id", "layer", "area", "table", "status", "n", "detail", "examples", "needs_human_explanation")
QC_MAX_EXAMPLES <- 8L

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a

# One check row. examples: a character vector (the first QC_MAX_EXAMPLES are kept, joined by " | ").
qc_check <- function(check_id, area, table, status = "info", n = 0L, detail = "", examples = character(),
                     needs_human_explanation = FALSE, layer = "pipeline") {
  if (!status %in% QC_STATUSES) stop("qc_check(): status must be one of ", paste(QC_STATUSES, collapse = ", "), call. = FALSE)
  ex <- as.character(stats::na.omit(examples))
  ex <- ex[nzchar(ex)]
  ex_str <- if (length(ex)) {
    more <- if (length(ex) > QC_MAX_EXAMPLES) sprintf(" | ... (+%d more)", length(ex) - QC_MAX_EXAMPLES) else ""
    paste0(paste(utils::head(ex, QC_MAX_EXAMPLES), collapse = " | "), more)
  } else ""
  tibble::tibble(check_id = as.character(check_id), layer = layer, area = as.character(area), table = as.character(table),
                 status = status, n = as.integer(n), detail = as.character(detail), examples = ex_str,
                 needs_human_explanation = isTRUE(needs_human_explanation))
}

# An empty check table with the contract's columns.
qc_empty <- function() {
  tibble::tibble(check_id = character(), layer = character(), area = character(), table = character(), status = character(),
                 n = integer(), detail = character(), examples = character(), needs_human_explanation = logical())
}

# Bind check rows (tibbles, or lists of them; NULL dropped) into one table with the contract's columns.
qc_bind <- function(...) {
  parts <- list(...)
  flat <- list()
  for (p in parts) {
    if (is.null(p)) next
    if (is.data.frame(p)) flat[[length(flat) + 1L]] <- p else flat <- c(flat, Filter(Negate(is.null), p))
  }
  if (!length(flat)) return(qc_empty())
  # unnamed: bind_rows() reads a named list as one row of columns, not as a list of tables
  out <- do.call(dplyr::bind_rows, c(list(qc_empty()), unname(flat)))
  out[, QC_CHECK_COLS]
}

# The worst status of a vector (fail > warn > info); info for an empty vector.
qc_status_worst <- function(status) {
  if (!length(status)) return("info")
  QC_STATUSES[max(match(status, QC_STATUSES), na.rm = TRUE)]
}

# warn when n > 0 (else info); fail_if lifts a positive count to fail.
qc_status_if <- function(n, fail_if = FALSE) if (n > 0) (if (fail_if) "fail" else "warn") else "info"

qc_has_fail <- function(checks) any(checks$status %in% "fail")

qc_count_status <- function(checks) {
  stats::setNames(vapply(QC_STATUSES, function(s) sum(checks$status %in% s), integer(1)), QC_STATUSES)
}

# Example strings "col=value; col=value" for the first n rows of df (numbers rounded to 4 significant digits).
qc_examples <- function(df, cols = names(df), n = QC_MAX_EXAMPLES + 2L) {
  if (is.null(df) || !nrow(df)) return(character())
  df <- utils::head(as.data.frame(df), n)
  cols <- intersect(cols, names(df))
  vapply(seq_len(nrow(df)), function(i) {
    paste(vapply(cols, function(cn) {
      v <- df[[cn]][i]
      v <- if (is.numeric(v) && !is.na(v)) format(signif(v, 4), trim = TRUE) else as.character(v)
      paste0(cn, "=", v)
    }, character(1)), collapse = "; ")
  }, character(1))
}

# Run a check function; an error becomes a warn row naming the check (a QC bug must not pass silently, and
# must not fail the data either).
qc_try <- function(expr, check_id, area, table) {
  tryCatch(expr, error = function(e) {
    qc_check(check_id, area, table, "warn", 0L, paste("check could not run:", conditionMessage(e)), needs_human_explanation = TRUE)
  })
}

# Labelled example strings: paste0(prefix, x), or character() when x is empty (paste0() would return the bare
# prefix for an empty x, which reads as an example of nothing).
qc_lab <- function(prefix, x, suffix = "") {
  if (!length(x)) return(character())
  s <- paste0(prefix, x, suffix)
  s[!is.na(x)]
}

# ISO3 code format: three upper-case letters (XKX and similar user-assigned codes pass the format).
qc_iso3_ok <- function(x) !is.na(x) & grepl("^[A-Z]{3}$", x)

# A column of a data frame, or NA of length nrow when absent.
qc_col <- function(tbl, nm, default = NA) if (nm %in% names(tbl)) tbl[[nm]] else rep(default, nrow(tbl))

# The canonical string the data release ID hashes (dashboard/src/review/release.ts releaseKey: a fixed field
# order, '-' for anything missing). stamps: list(posture = list(created, git_commit), country = list(created,
# git_commit), sector = list(bundle_id), aib = list(created), strategic = list(bundle_id), memos =
# list(manifest_sha256)). The ID is "rel-" + the first 12 hex of its sha256 (computed by the caller).
qc_release_key <- function(stamps) {
  v <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "-" else as.character(x)
  paste(c(
    paste0("posture:", v(stamps$posture$created), "@", v(stamps$posture$git_commit)),
    paste0("country:", v(stamps$country$created), "@", v(stamps$country$git_commit)),
    paste0("sector:", v(stamps$sector$bundle_id)),
    paste0("aib:", v(stamps$aib$created)),
    paste0("strategic:", v(stamps$strategic$bundle_id)),
    paste0("memos:", v(stamps$memos$manifest_sha256))
  ), collapse = "|")
}

# The text the memos stamp hashes (dashboard/src/review/release.ts memosManifestSha256): the partnerships manifest
# with a leading BOM dropped and line endings normalised to LF, so a CRLF checkout (core.autocrlf) gives the same
# release ID as the LF copy Pages serves. text: one string (the file's bytes as UTF-8). Pure.
qc_memos_manifest_text <- function(text) {
  text <- sub("^\ufeff", "", text)
  gsub("\r\n?", "\n", text)
}

# The ISO3 column of a table: iso3, iso3c or iso3_code (NULL when none).
qc_iso3_col <- function(tbl) {
  hit <- intersect(c("iso3", "iso3c", "iso3_code"), names(tbl))
  if (length(hit)) hit[[1]] else NULL
}
