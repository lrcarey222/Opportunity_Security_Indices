# The dashboard change bundle (dashboard/public/data/changes/changes.<hash>.json; contract in
# dashboard/src/changes/contract.ts). Vintage headers, pair summaries and the meaningful change records with their
# previous / current sides nested. Pure: returns a list for jsonlite; the script writes it.

CHANGE_SCHEMA <- "osi-changes"
CHANGE_SCHEMA_VERSION <- 2L   # 2: change records are columnar (dashboard/src/changes/contract.ts decodes both)

change_num3 <- function(x) ifelse(is.na(x), NA_real_, round(as.numeric(x), 3))
change_split <- function(x) if (is.na(x) || !nzchar(x)) I(character()) else I(strsplit(x, ";", fixed = TRUE)[[1]])

change_side <- function(r, side) {
  g <- function(f) r[[paste0(side, "_", f)]]
  list(choice = g("choice"), posture = g("posture"), needType = g("need_type"),
       need = change_num3(g("need")), prize = change_num3(g("prize")), cp = change_num3(g("cp")),
       market = change_num3(g("market")), share = change_num3(g("share")),
       confidence = g("confidence"), evidence = g("evidence"), evidenceStatus = g("evidence_status"))
}

change_record_json <- function(r) {
  list(id = r$id, iso3 = r$iso3, tech = r$tech, stage = r$stage, prev = r$prev_vintage, cur = r$cur_vintage,
       types = change_split(r$types), subtype = r$subtype, severity = r$severity, magnitude = change_num3(r$magnitude),
       attribution = r$attribution, methodChanged = isTRUE(r$method_changed), methodUnverified = isTRUE(r$method_unverified),
       crossed = change_split(gsub("; ", ";", r$crossed %||% NA_character_)),
       evidenceDirection = r$evidence_direction, confidenceDirection = r$confidence_direction,
       sources = change_split(r$sources), impacts = change_split(r$impacts), changedInputs = change_split(r$changed_inputs %||% NA_character_),
       before = change_side(r, "prev"), after = change_side(r, "cur"),
       delta = list(need = change_num3(r$d_need), prize = change_num3(r$d_prize), cp = change_num3(r$d_cp)),
       explanation = r$explanation)
}

change_vintage_json <- function(h) {
  list(id = h$vintage_id, created = h$created, gitCommit = h$git_commit, provisional = isTRUE(h$provisional),
       stampSource = h$stamp_source, coreFile = h$core_file, coreCommit = h$core_commit,
       method = list(id = h$method$id, engine = h$method$engine, upstream = h$method$upstream, presentation = h$method$presentation,
                     label = h$method$label, unverified = isTRUE(h$method$unverified), source = h$method$source),
       data = h$data[setdiff(names(h$data), "raw_inputs")], nCells = h$n_cells, bars = h$bars)
}

change_pair_json <- function(p) {
  list(prev = p$prev, cur = p$cur, comparable = isTRUE(p$comparable), reason = p$reason,
       methodChanged = isTRUE(p$method_changed), methodUnverified = isTRUE(p$method_unverified),
       dataChanged = isTRUE(p$data_changed), dataRelation = p$data_relation %||% "unknown", bridge = isTRUE(p$bridge), detail = !isFALSE(p$detail),
       nCells = p$n_cells %||% NA_integer_, nUnchanged = p$n_unchanged %||% NA_integer_, counts = p$counts %||% list(),
       inputsChanged = if (is.null(p$inputs_changed)) NULL else lapply(seq_len(nrow(p$inputs_changed)), function(i) as.list(p$inputs_changed[i, ])))
}

# headers: list of vintage headers (oldest first); results: list of posture_compare() outputs; impacts: accepted
# impact records referenced by the changes; sources: cfg$sources (field -> ids, doc) for the provenance panel.
change_bundle <- function(headers, results, impacts = list(), sources = list(), created, git_commit) {
  ch <- dplyr::bind_rows(Filter(nrow, lapply(results, `[[`, "changes")))
  used <- unique(unlist(lapply(ch$impacts, function(x) if (is.na(x)) character() else strsplit(x, ";")[[1]])))
  list(schema = CHANGE_SCHEMA, schema_version = CHANGE_SCHEMA_VERSION, created = created, git_commit = git_commit,
       vintages = lapply(headers, change_vintage_json), pairs = lapply(results, function(r) change_pair_json(r$pair)),
       changes = change_columns(if (nrow(ch)) lapply(seq_len(nrow(ch)), function(i) change_record_json(as.list(ch[i, ]))) else list()),
       impacts = Filter(function(r) r$impact_id %in% used, impacts),
       sources = sources)
}

# The records as columns (schema 2): one array per field (nested sides flattened to before_<f> / after_<f>, delta_<f>),
# array fields as arrays of arrays. Removes the per-record key names, the bulk of the row encoding.
change_columns <- function(rows) {
  flat <- lapply(rows, function(r) {
    out <- r[setdiff(names(r), c("before", "after", "delta"))]
    for (s in c("before", "after", "delta")) for (k in names(r[[s]])) out[[paste0(s, "_", k)]] <- r[[s]][[k]]
    out
  })
  keys <- unique(unlist(lapply(flat, names)))
  arrays <- c("types", "crossed", "sources", "impacts", "changedInputs")
  cols <- stats::setNames(lapply(keys, function(k) {
    v <- lapply(flat, function(r) r[[k]])
    if (k %in% arrays) return(lapply(v, function(z) I(unname(unlist(z) %||% character()))))
    I(unlist(lapply(v, function(z) if (is.null(z) || !length(z)) NA else z)))
  }), keys)
  list(n = length(rows), columns = cols)
}
