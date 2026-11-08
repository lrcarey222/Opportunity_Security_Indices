source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

# Partnership agreements, step 1 of 3 (docs/partnership_agreements.md, section 3): ingest one source
# (or all) into the common schema.
#
#   Rscript scripts/35_ingest_agreements.R --source <gsp|desta|memberships|curated|pmemo|iisd|all>
#     [--raw-dir data/raw] [--reference-dir data/reference] [--pmemo-dir <dir>]
#     [--out-dir data/processed/agreements] [--as-of YYYY-MM-DD]
#
# Inputs:
# - data/raw (config raw_data_dir, or --raw-dir): gsp_partnership_documents.csv
#   (scripts/fetchers/gsp_partnerships.R); the DESTA files desta_list_of_treaties.csv,
#   desta_provisions.csv, desta_indices.csv and desta_withdrawals.csv (scripts/fetchers/desta.R;
#   desta_dyads.csv is fetched for reference and not read: parties come from the list file);
#   iisd_trade_climate_tracker.csv (optional, staged by hand with IISD's permission);
#   economic_security_agreements.csv (copied from data/reference by scripts/05; read from
#   data/reference otherwise, as scripts/13 does).
# - data/reference (--reference-dir): partnership_agreements/curated_agreements.csv.
# - --pmemo-dir (default <reference-dir>/partnership_evidence when it exists):
#   bilateral_mechanisms.csv and policy_evidence.csv of the Partnership Memos evidence layer.
#   Without it the pmemo source is skipped with a message.
# Outputs (--out-dir, default <processed_dir>/agreements):
# - source_<id>.rds: list(records, codes, extras, meta = list(source, n, ingested_on, as_of, inputs))
#   where inputs is a data.frame(file, md5).
# - source_<id>_records.csv and source_<id>_codes.csv.
# A run stops on any agr_validate() problem and writes nothing for that source. Reruns overwrite
# the files whole (deterministic ids), so they are idempotent.
# Functions: R/agreements/ (adapters, schema). Settings: config/partnership_agreements.yml.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

for (f in c("agr_schema.R", "agr_text_coding.R", "adapter_gsp.R", "adapter_desta.R", "adapter_memberships.R",
            "adapter_curated.R", "adapter_pmemo.R", "adapter_iisd.R")) {
  source(file.path(repo_root, "R", "agreements", f))
}

cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_agreements.yml"))
coalition_cfg <- yaml::read_yaml(file.path(repo_root, "config", "coalition.yml"))

source_arg <- tolower(arg_value("--source", "all"))
raw_data_path <- abs_path(arg_value("--raw-dir", config$raw_data_dir))
reference_dir <- abs_path(arg_value("--reference-dir", file.path("data", "reference")))
pmemo_default <- file.path(reference_dir, "partnership_evidence")
pmemo_dir <- arg_value("--pmemo-dir", if (dir.exists(pmemo_default)) pmemo_default else "")
pmemo_dir <- if (nzchar(pmemo_dir)) abs_path(pmemo_dir) else ""
out_dir <- abs_path(arg_value("--out-dir", file.path(config$processed_dir, "agreements")))
as_of <- agr_norm_date(arg_value("--as-of", as.character(cfg$pairs$as_of)))
if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", as_of)) stop("--as-of must be YYYY-MM-DD")

sources <- if (source_arg == "all") AGR_SOURCES else source_arg
bad <- setdiff(sources, AGR_SOURCES)
if (length(bad)) stop("unknown --source: ", paste(bad, collapse = ", "), " (one of ", paste(c(AGR_SOURCES, "all"), collapse = ", "), ")")

# Raw and reference files are read as text: every column character, nothing turned into NA.
read_text_csv <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = character(), encoding = "UTF-8",
                  check.names = FALSE, stringsAsFactors = FALSE)
}
need_file <- function(p) { if (!file.exists(p)) stop("input not found: ", p); p }
md5_of <- function(paths) data.frame(file = basename(paths), md5 = unname(tools::md5sum(paths)), stringsAsFactors = FALSE)

# Raw inputs (literal names under raw_data_path, so scripts/01 and the manifest test see them).
gsp_path <- file.path(raw_data_path, "gsp_partnership_documents.csv")
desta_list_path <- file.path(raw_data_path, "desta_list_of_treaties.csv")
desta_provisions_path <- file.path(raw_data_path, "desta_provisions.csv")
desta_indices_path <- file.path(raw_data_path, "desta_indices.csv")
desta_withdrawals_path <- file.path(raw_data_path, "desta_withdrawals.csv")
iisd_path <- file.path(raw_data_path, "iisd_trade_climate_tracker.csv")
memberships_path <- file.path(raw_data_path, "economic_security_agreements.csv")
if (!file.exists(memberships_path)) memberships_path <- file.path(reference_dir, "economic_security_agreements.csv")
curated_path <- file.path(reference_dir, "partnership_agreements", "curated_agreements.csv")

# One source -> list(result, inputs), or NULL when an optional source is absent.
ingest <- function(id) {
  switch(id,
    gsp = {
      raw <- read_text_csv(need_file(gsp_path))
      list(result = agr_adapt_gsp(raw, cfg), inputs = gsp_path)
    },
    desta = {
      paths <- c(desta_list_path, desta_provisions_path, desta_indices_path, desta_withdrawals_path)
      for (p in paths) need_file(p)
      res <- agr_adapt_desta(read_text_csv(desta_list_path), read_text_csv(desta_provisions_path),
                             read_text_csv(desta_indices_path), read_text_csv(desta_withdrawals_path), cfg,
                             eu_members = unlist(coalition_cfg$eu_node$members),
                             as_of_year = as.integer(substr(as_of, 1, 4)))
      list(result = res, inputs = paths)
    },
    memberships = {
      list(result = agr_adapt_memberships(read_text_csv(need_file(memberships_path)), cfg), inputs = memberships_path)
    },
    curated = {
      list(result = agr_adapt_curated(read_text_csv(need_file(curated_path)), cfg), inputs = curated_path)
    },
    pmemo = {
      if (!nzchar(pmemo_dir)) {
        step("pmemo: no --pmemo-dir and no ", pmemo_default, "; skipped")
        return(NULL)
      }
      paths <- file.path(pmemo_dir, c("bilateral_mechanisms.csv", "policy_evidence.csv"))
      for (p in paths) need_file(p)
      list(result = agr_adapt_pmemo(read_text_csv(paths[1]), read_text_csv(paths[2]), cfg), inputs = paths)
    },
    iisd = {
      if (!file.exists(iisd_path)) {
        step("iisd: ", iisd_path, " is not staged (optional; see the manifest entry); skipped")
        return(NULL)
      }
      export_date <- format(as.Date(file.info(iisd_path)$mtime), "%Y-%m-%d")
      list(result = agr_adapt_iisd(read_text_csv(iisd_path), cfg, export_date = export_date), inputs = iisd_path)
    })
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (id in sources) {
  t0 <- Sys.time()
  step(id, ": ingesting")
  got <- ingest(id)
  if (is.null(got)) next
  res <- got$result
  problems <- agr_validate(res, cfg)
  if (length(problems)) stop(id, ": ", length(problems), " validation problem(s):\n  ", paste(problems, collapse = "\n  "))
  meta <- list(source = id, n = nrow(res$records), ingested_on = format(Sys.Date()), as_of = as_of,
               inputs = md5_of(got$inputs))
  out <- list(records = res$records, codes = res$codes, extras = res$extras %||% list(), meta = meta)
  saveRDS(out, file.path(out_dir, sprintf("source_%s.rds", id)))
  utils::write.csv(res$records, file.path(out_dir, sprintf("source_%s_records.csv", id)), row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  utils::write.csv(res$codes, file.path(out_dir, sprintf("source_%s_codes.csv", id)), row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  types <- table(res$records$agreement_type)
  step(sprintf("%s: %d records, %d codes (%s) in %.0f s", id, nrow(res$records), nrow(res$codes),
               paste(names(types), types, sep = " ", collapse = ", "), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
step("wrote ", out_dir)
