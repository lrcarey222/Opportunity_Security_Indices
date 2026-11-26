# The dashboard change bundle (dashboard/public/data/changes/changes.<hash>.json; contract in
# dashboard/src/changes/contract.ts). Vintage headers, pair summaries and the meaningful change records with their
# previous / current sides nested. Pure: returns a list for jsonlite; the script writes it.

CHANGE_SCHEMA <- "osi-changes"
CHANGE_SCHEMA_VERSION <- 1L

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
       sources = change_split(r$sources), impacts = change_split(r$impacts),
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
       nCells = p$n_cells %||% NA_integer_, nUnchanged = p$n_unchanged %||% NA_integer_, counts = p$counts %||% list())
}

# headers: list of vintage headers (oldest first); results: list of posture_compare() outputs; impacts: accepted
# impact records referenced by the changes; sources: cfg$sources (field -> ids, doc) for the provenance panel.
change_bundle <- function(headers, results, impacts = list(), sources = list(), created, git_commit) {
  ch <- dplyr::bind_rows(Filter(nrow, lapply(results, `[[`, "changes")))
  used <- unique(unlist(lapply(ch$impacts, function(x) if (is.na(x)) character() else strsplit(x, ";")[[1]])))
  list(schema = CHANGE_SCHEMA, schema_version = CHANGE_SCHEMA_VERSION, created = created, git_commit = git_commit,
       vintages = lapply(headers, change_vintage_json), pairs = lapply(results, function(r) change_pair_json(r$pair)),
       changes = if (nrow(ch)) lapply(seq_len(nrow(ch)), function(i) change_record_json(as.list(ch[i, ]))) else list(),
       impacts = Filter(function(r) r$impact_id %in% used, impacts),
       sources = sources)
}
