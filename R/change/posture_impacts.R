# NEIS Intelligence bridge (docs/change-layer-architecture.md, "analytical impact"). An Intelligence event can be
# linked to the posture assessments it may affect, but only as a reviewed record: proposed -> accepted / rejected
# by a named person. Accepted impacts explain a change in the vintage pair whose window holds the review; they never
# change a score. Nothing here calls a model.
#
# Pure functions: no IO.

IMPACT_DIMENSIONS <- c("need", "prize", "position", "evidence", "confidence", "recommendation")
IMPACT_STATUSES <- c("proposed", "accepted", "rejected")
IMPACT_REASON_METHODS <- c("rule", "model", "editorial")
IMPACT_ID_RE <- "^aib:posture:[A-Z]{3}:[a-z0-9-]+:[a-z0-9-]+$"

# Candidate assessments for an event's taxonomy tags. tags: list(geography = <slugs>, sector = <slugs>,
# value_chain_node = <slugs>); crosswalk: config/neis_intelligence_crosswalk.yml; cells: a vintage table (only cells
# that exist and are assessed are candidates). A geography and a sector are required; no stage tag means all stages.
posture_impact_candidates <- function(tags, crosswalk, cells) {
  map <- function(slugs, table) unique(unlist(lapply(slugs, function(s) table[[s]])))
  isos <- map(tags$geography, crosswalk$geography)
  techs <- map(tags$sector, crosswalk$sector)
  stages <- if (length(tags$value_chain_node)) map(tags$value_chain_node, crosswalk$value_chain_node) else unique(cells$stage)
  empty <- tibble::tibble(assessment_id = character(), iso3 = character(), tech = character(), stage = character())
  if (!length(isos) || !length(techs) || !length(stages)) return(empty)
  hit <- cells[cells$iso3 %in% isos & cells$tech %in% techs & cells$stage %in% stages &
               dplyr::coalesce(cells$choice, "not_assessed") != "not_assessed", c("iso3", "tech", "stage")]
  if (!nrow(hit)) return(empty)
  tibble::tibble(assessment_id = assessment_id(hit$iso3, hit$tech, hit$stage), iso3 = hit$iso3, tech = hit$tech, stage = hit$stage)
}

# Validate impact records (a list of lists, as read from config/review/analytical_impacts.yml). Returns a character
# vector of problems (empty when valid).
validate_analytical_impacts <- function(impacts) {
  req <- c("impact_id", "event_id", "assessment_id", "dimension", "reason", "reason_method", "status", "proposed_by", "proposed_at")
  out <- character()
  ids <- character()
  for (i in seq_along(impacts)) {
    r <- impacts[[i]]
    who <- r$impact_id %||% sprintf("#%d", i)
    miss <- req[!vapply(req, function(k) !is.null(r[[k]]) && nzchar(as.character(r[[k]])), logical(1))]
    if (length(miss)) out <- c(out, sprintf("%s: missing %s", who, paste(miss, collapse = ", ")))
    if (!is.null(r$assessment_id) && !grepl(IMPACT_ID_RE, r$assessment_id)) out <- c(out, sprintf("%s: bad assessment_id %s", who, r$assessment_id))
    if (!is.null(r$dimension) && !r$dimension %in% IMPACT_DIMENSIONS) out <- c(out, sprintf("%s: bad dimension %s", who, r$dimension))
    if (!is.null(r$status) && !r$status %in% IMPACT_STATUSES) out <- c(out, sprintf("%s: bad status %s", who, r$status))
    if (!is.null(r$reason_method) && !r$reason_method %in% IMPACT_REASON_METHODS) out <- c(out, sprintf("%s: bad reason_method %s", who, r$reason_method))
    if (!is.null(r$confidence) && (!is.numeric(r$confidence) || r$confidence < 0 || r$confidence > 1)) out <- c(out, sprintf("%s: confidence outside 0-1", who))
    if (identical(r$status, "accepted") || identical(r$status, "rejected")) {
      if (is.null(r$reviewer) || is.null(r$review_timestamp)) out <- c(out, sprintf("%s: a reviewed impact needs reviewer and review_timestamp", who))
    }
    if (who %in% ids) out <- c(out, sprintf("%s: duplicate impact_id", who))
    ids <- c(ids, who)
  }
  out
}

# Attach accepted impacts to change records: an impact attaches to the record of its assessment in the pair whose
# window (prev created, cur created] holds its review_timestamp. headers: named list of vintage headers by id.
# Returns changes with impacts = ";"-joined impact ids.
attach_impacts <- function(changes, impacts, headers) {
  if (!nrow(changes) || !length(impacts)) return(changes)
  acc <- Filter(function(r) identical(r$status, "accepted") && !is.null(r$review_timestamp), impacts)
  if (!length(acc)) return(changes)
  ts <- function(x) as.POSIXct(sub("Z$", "", sub("T", " ", x)), tz = "UTC")
  t_imp <- ts(vapply(acc, function(r) as.character(r$review_timestamp), character(1)))
  a_id <- vapply(acc, function(r) r$assessment_id, character(1))
  i_id <- vapply(acc, function(r) r$impact_id, character(1))
  changes$impacts <- vapply(seq_len(nrow(changes)), function(k) {
    lo <- ts(headers[[changes$prev_vintage[k]]]$created)
    hi <- ts(headers[[changes$cur_vintage[k]]]$created)
    h <- i_id[a_id == changes$id[k] & t_imp > lo & t_imp <= hi]
    if (length(h)) paste(sort(h), collapse = ";") else NA_character_
  }, character(1))
  changes
}
