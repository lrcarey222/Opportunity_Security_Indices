# Posture vintages (docs/change-layer-architecture.md): one dated, normalized table of posture cells
# (iso3 x tech x stage) plus a header carrying the method fingerprint and data stamp. A vintage is derived
# analysis only: engine outputs and their stamps, never raw source values.
#
# Pure functions: no IO. Needs dplyr, tibble, digest and R/qc/qc_core.R + R/qc/qc_drift.R (the core decoder).

POSTURE_VINTAGE_COLS <- c("iso3", "tech", "stage", "choice", "posture", "variant", "need_type", "need", "prize", "cp",
                          "market", "share", "confidence", "evidence", "borderline", "pathway", "partner1", "deploy_case",
                          "evidence_status")
POSTURE_VINTAGE_NUM <- c("need", "prize", "cp", "market", "share")
POSTURE_FAMILY <- c(S = "secure", C = "compete", D = "deploy", N = "not_assessed")
# The dominant evidence basis of a cell (R/utils/variable_stage_evidence.R VSE_STATUSES) and its one-letter code in
# the posture core (cells.es, written by scripts/91 since the bridge-runs slice).
EVIDENCE_STATUSES <- c("exact", "split", "shared", "proxy", "tech_wide", "no_data")
EVIDENCE_STATUS_CODE <- c(exact = "E", split = "S", shared = "H", proxy = "P", tech_wide = "T", no_data = "N")

# The dominant evidence status per row from the variable-stage evidence shares (exact_evidence_share<suffix>, ...,
# no_data_share<suffix>): the status with the largest share; ties go to the more specific status; NA when no share
# is known.
evidence_status_from_shares <- function(tbl, suffix = "_cp") {
  cols <- paste0(c("exact_evidence_share", "split_evidence_share", "shared_evidence_share", "proxy_evidence_share",
                   "tech_wide_evidence_share", "no_data_share"), suffix)
  if (!all(cols %in% names(tbl))) return(rep(NA_character_, nrow(tbl)))
  m <- as.matrix(tbl[, cols, drop = FALSE])
  storage.mode(m) <- "double"
  vapply(seq_len(nrow(m)), function(i) {
    r <- m[i, ]
    if (all(is.na(r))) NA_character_ else EVIDENCE_STATUSES[which.max(replace(r, is.na(r), -Inf))]
  }, character(1))
}

change_hash <- function(text, n = 8L) substr(digest::digest(enc2utf8(paste(text, collapse = "\n")), algo = "sha256", serialize = FALSE), 1L, n)

# 'Electric Grid' -> 'electric-grid'
change_slug <- function(x) gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-", tolower(x)))

# aib:posture:USA:electric-grid:components
assessment_id <- function(iso3, tech, stage, prefix = "aib:posture") paste(prefix, toupper(iso3), change_slug(tech), change_slug(stage), sep = ":")

# The family of a choice (secure / compete / deploy / not_assessed).
posture_family_of <- function(choice) {
  dplyr::case_when(choice == "secure" ~ "secure", choice %in% c("preserve", "catch_up", "partner") ~ "compete",
                   choice %in% c("accelerate", "watch") ~ "deploy", choice == "not_assessed" ~ "not_assessed",
                   TRUE ~ NA_character_)
}

# A normalized vintage table from a parsed posture core (jsonlite::fromJSON(simplifyVector = FALSE)). Reuses the QC
# drift decoder for the key, choice and scores and adds the fields drift does not carry. Row order follows the core.
posture_vintage_from_core <- function(core) {
  base <- qc_posture_core_to_table(core)
  cells <- core$cells
  n <- nrow(base)
  chr <- function(nm) {
    x <- cells[[nm]]
    if (is.null(x)) return(rep(NA_character_, n))
    vapply(x, function(z) if (is.null(z) || !length(z)) NA_character_ else as.character(z), character(1))
  }
  dep <- base$supply_chain == "Deployment"
  nt <- chr("nt")
  nt <- ifelse(is.na(nt), ifelse(dep, "deployment", "supply_chain"), nt)
  tibble::tibble(
    iso3 = base$iso3, tech = base$tech, stage = base$supply_chain,
    choice = base$choice, posture = unname(POSTURE_FAMILY[chr("p")]), variant = chr("v"),
    need_type = nt, need = dplyr::coalesce(base$need, base$dn),
    prize = base$prize, cp = base$cp, market = base$market, share = base$share,
    confidence = chr("conf"), evidence = chr("ev"), borderline = chr("bd"), pathway = chr("pw"),
    partner1 = chr("p1"), deploy_case = chr("dc"),
    evidence_status = unname(stats::setNames(names(EVIDENCE_STATUS_CODE), EVIDENCE_STATUS_CODE)[chr("es")])
  )
}

# A normalized vintage table from a strategic_position table (outputs/strategic_position_tbl.rds), e.g. for a
# provisional run that has not been exported, or a bridge run (new method, previous inputs).
posture_vintage_from_table <- function(tbl) {
  s <- qc_sp_standardize(tbl)
  dep <- s$supply_chain == "Deployment"
  tibble::tibble(
    iso3 = s$iso3, tech = s$tech, stage = s$supply_chain, choice = s$choice,
    posture = dplyr::coalesce(qc_col(tbl, "strategic_posture", NA_character_), posture_family_of(s$choice)),
    variant = qc_col(tbl, "posture_variant", NA_character_),
    need_type = ifelse(dep, "deployment", "supply_chain"), need = dplyr::coalesce(s$need, s$dn),
    prize = s$prize, cp = s$cp, market = s$market, share = s$share,
    confidence = qc_col(tbl, "posture_confidence", NA_character_),
    evidence = qc_col(tbl, "stage_evidence_quality_cp", NA_character_), borderline = s$borderline,
    pathway = qc_col(tbl, "pathway_credible", NA_character_), partner1 = qc_col(tbl, "recommended_partner_1", NA_character_),
    deploy_case = qc_col(tbl, "deploy_case", NA_character_),
    evidence_status = evidence_status_from_shares(tbl, "_cp")
  ) |> dplyr::filter(!is.na(.data$choice))
}

# Method fingerprint id from named git object ids (path -> sha). Order-independent.
change_method_id <- function(path_shas) {
  if (!length(path_shas)) return(NA_character_)
  k <- sort(names(path_shas))
  paste0("m-", change_hash(paste0(k, "=", unlist(path_shas[k]))))
}

# Data stamp id from the core meta's input windows (and resolved raw-input vintages when known).
change_data_id <- function(meta, fields = c("benchmark_period", "momentum_period", "momentum_through"), raw_inputs = NULL) {
  v <- vapply(fields, function(f) { x <- meta[[f]]; if (is.null(x)) "-" else paste(unlist(x), collapse = ",") }, character(1))
  ri <- if (is.null(raw_inputs)) character() else paste0(names(raw_inputs), "=", unlist(raw_inputs))
  paste0("d-", change_hash(c(paste0(fields, "=", v), sort(ri))))
}

# The decision bars a vintage was built with (core meta thresholds), falling back to config defaults.
posture_vintage_bars <- function(meta, defaults) {
  th <- meta$thresholds %||% list()
  pick <- function(...) { for (k in c(...)) if (!is.null(th[[k]])) return(as.numeric(th[[k]])); NULL }
  list(need = pick("need", "need_threshold") %||% defaults$need,
       deployment = pick("deployment", "deployment_threshold") %||% defaults$deployment,
       prize = pick("prize", "prize_threshold") %||% defaults$prize,
       cp = pick("cp", "cp_threshold") %||% defaults$cp)
}

# The vintage header. core_meta: the core's meta list; method: list(id, paths, unverified, label).
posture_vintage_header <- function(vintage_id, created, git_commit, core_file, core_commit, method, data_id, data_stamp,
                                   techs, stages, bars, n_cells, sha256, stamp_source = "bundle", provisional = FALSE) {
  list(vintage_id = vintage_id, created = created, git_commit = git_commit,
       commit_dirty = isTRUE(grepl("-dirty$", git_commit %||% "")), stamp_source = stamp_source,
       provisional = provisional, core_file = core_file, core_commit = core_commit,
       method = method, data = c(list(id = data_id), data_stamp),
       techs = as.list(techs), stages = as.list(stages), bars = bars, n_cells = n_cells, sha256 = sha256)
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
