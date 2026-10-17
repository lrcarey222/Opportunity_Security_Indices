# Allied Industrial Base (AIB): the Sectoral Compacts dashboard bundle (dashboard/src/aib).
#
# Usage:
#   Rscript scripts/84_export_aib_data.R [--in-dir DIR] [--out-dir DIR] [--top-edges N]
#
#   --in-dir     where scripts/32 wrote aib_results.rds (default: <outputs_dir>/aib)
#   --out-dir    bundle directory (default: dashboard/public/data/aib)
#   --top-edges  trade and investment relationships kept per compact (default 40)
#
# Writes compacts.<md5_8>.json (schema osi-aib v1 with optional additions: meta, country names, compacts, nodes,
# and since 2026-10-17 the emergent network `network` (emergent_aib_country_summary + emergent_aib_edges)
# and the investor -> host flows `investmentFlows` (allied_investment_pairs + the ranked hosts of
# allied_investment_partnerships), headline variant) and, last, manifest.json naming it; older
# compacts.*.json files are removed. Plain JSON, every array field wrapped in I()
# (R/dashboard/aib/aib_export.R); the additive sections are checked first (R/dashboard/aib/aib_check.R,
# the mirror of the TypeScript contract) and the script stops on a problem.

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

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "R", "aib", "aib_compacts.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_export.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_check.R"))
source(file.path(repo_root, "R", "dashboard", "country", "country_util.R"))

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(repo_root, config$processed_dir, "outputs")
in_dir <- abs_path(arg_value("--in-dir", file.path(outputs_dir, "aib")))
out_dir <- abs_path(arg_value("--out-dir", "dashboard/public/data/aib"))
top_edges <- as.integer(arg_value("--top-edges", "40"))
res_path <- file.path(in_dir, "aib_results.rds")
if (!file.exists(res_path)) stop("Run scripts/32_build_aib_partnerships.R first: ", res_path, " not found.")
res <- readRDS(res_path)

bundle <- aib_dash_bundle(res, top_edges = top_edges)
problems <- aib_check_bundle_additions(bundle)
if (length(problems)) stop("The AIB bundle does not match the contract:\n  ", paste(utils::head(problems, 20), collapse = "\n  "))
txt <- country_json(bundle)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tmp <- tempfile(fileext = ".json")
writeLines(txt, tmp, useBytes = TRUE)
hash <- substr(unname(tools::md5sum(tmp)), 1, 8)
fname <- sprintf("compacts.%s.json", hash)
writeLines(txt, file.path(out_dir, fname), useBytes = TRUE)
old <- setdiff(list.files(out_dir, pattern = "^compacts\\.[0-9a-f]{8}\\.json$"), fname)
if (length(old)) file.remove(file.path(out_dir, old))
manifest <- list(schema = AIB_DASH_SCHEMA, version = AIB_DASH_VERSION, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 demandScope = res$meta$demand_scope, candidateUniverse = res$meta$candidate_universe,
                 shockSemantics = res$meta$headline_semantics, supplyConstraint = res$meta$headline_constraint,
                 universe = res$meta$universe, headlineShock = res$meta$headline_shock, headlineScenario = res$meta$headline_scenario,
                 files = list(compacts = fname))
writeLines(country_json(manifest, pretty = TRUE), file.path(out_dir, "manifest.json"), useBytes = TRUE)
message(sprintf("AIB dashboard bundle: %s (%.0f kB, %d compacts, %d nodes, %d network countries, %d network edges, %d investment flows) in %s",
                fname, file.size(file.path(out_dir, fname)) / 1024, length(bundle$compacts), length(bundle$nodes),
                length(bundle$network$countries), length(bundle$network$edges), length(bundle$investmentFlows$pairs), out_dir))
