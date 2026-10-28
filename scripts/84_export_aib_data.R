# Allied Industrial Base (AIB): the Sectoral Compacts dashboard bundle (dashboard/src/aib).
#
# Usage:
#   Rscript scripts/84_export_aib_data.R [--in-dir DIR] [--out-dir DIR] [--top-edges N] [--strategic FILE] [--inputs FILE]
#
#   --in-dir     where scripts/32 wrote aib_results.rds (default: <outputs_dir>/aib)
#   --out-dir    bundle directory (default: dashboard/public/data/aib)
#   --top-edges  trade and investment relationships kept per compact (default 40)
#   --strategic  the strategic layer of scripts/33 (default: <in-dir>/aib_strategic.rds; absent: the
#                document has no `strategic` section)
#   --inputs     scripts/31's aib_inputs.rds, for the nodes' three-stage cells (default:
#                <in-dir>/aib_inputs.rds; absent: the nodes carry none)
#
# Reads also data/reference/energy_hs6_master.csv (its three-stage supply_chain column: the nodes' three-stage
# cells, node.yfCellsLegacy / yfSubcomponentsLegacy, since 2026-10-28).
#
# Writes compacts.<md5_8>.json (schema osi-aib, version 3 since 2026-10-28; first written as version 1 with
# optional additions: meta, country names, compacts, nodes,
# and since 2026-10-17 the emergent network `network` (emergent_aib_country_summary + emergent_aib_edges)
# and the investor -> host flows `investmentFlows` (allied_investment_pairs + the ranked hosts of
# allied_investment_partnerships), headline variant) and, last, manifest.json naming it; older
# compacts.*.json files are removed. Since 2026-10-19 (the scenario toggle) the document is the
# Decoupling scenario with meta.scenarios, node.baseline, compact.managed and compact.baseline added (bundle
# version 2: meta.scenarios became the scenario list, so an app reading v1 asks for a reload), and
# two lazily loaded scenario files (schema osi-aib-scenario v1) are written beside it and named in
# manifest.files.scenarios: scenario-managed.<md5_8>.json (Managed Interdependence: the compacts solved
# under the diversification shock, its network and investment flows) and scenario-baseline.<md5_8>.json
# (Baseline: the observed network and each node's and compact's top suppliers); stale scenario files are
# removed. Since 2026-10-20 the main document also carries the optional `strategic` section (scripts/33's
# Strategic Sectoral Compacts, workstreams, Strategic AIB Roles, compact-specific roles and bilateral theses;
# aib_dash_strategic()) when aib_strategic.rds exists. Since 2026-10-27 every stage in the bundle is one of the six
# supply-chain stages (R/utils/supply_chain_stages.R), and the network's per-side stage counts are keyed
# extraction ... recovery; since 2026-10-28 the bundle is version 3 (the dashboard's TypeScript contract reads
# the six stages, so an app reading version 2 asks for a reload), each node also carries its three-stage cells
# (yfCellsLegacy, yfSubcomponentsLegacy: the cells of the three-stage Country, Sector and Strategic bundles,
# computed here from aib_inputs.rds and the OSI master) and an aggregated node's label names its six-stage
# stage. Plain JSON, every array field wrapped in I() (R/dashboard/aib/aib_export.R); the additive
# sections are checked first (R/dashboard/aib/aib_check.R, the mirror of the TypeScript contract) and the
# script stops on a problem.

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

source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "aib", "aib_nodes.R"))
source(file.path(repo_root, "R", "aib", "aib_compacts.R"))
source(file.path(repo_root, "R", "aib", "aib_strategic.R"))
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
# The bundle's stages are the six supply-chain stages (node.stage, compact.stages, the network's stage
# counts and edge stages); results from before 2026-10-27 are Upstream / Midstream / Downstream.
assert_supply_chain_stages(res$classification$supply_chain, paste0(res_path, " classification (rerun scripts/31 and 32)"))
# The strategic layer (scripts/33), optional: absent, the document has no `strategic` section.
strategic_path <- abs_path(arg_value("--strategic", file.path(in_dir, "aib_strategic.rds")))
st <- if (file.exists(strategic_path)) readRDS(strategic_path) else NULL
if (is.null(st)) {
  message("No strategic layer (", strategic_path, "): run scripts/33_build_aib_strategic_layer.R to add the `strategic` section.")
} else if (!identical(st$meta$results_built, res$meta$built)) {
  warning("The strategic layer was built from another AIB run (", st$meta$results_built, " against ", res$meta$built,
          "): rerun scripts/33_build_aib_strategic_layer.R.", call. = FALSE)
}
strategic <- if (is.null(st)) NULL else aib_dash_strategic(st, aib_strategic_gate_labels())

# The nodes' three-stage cells (node.yfCellsLegacy / yfSubcomponentsLegacy, 2026-10-28) for the joins with the
# three-stage Country, Sector and Strategic bundles, which the roll-up of the six-stage cells does not give back:
# the node codes of scripts/31's aib_inputs.rds crosswalked to the OSI master's three-stage supply_chain column
# (aib_dash_legacy_cells()), the master read as scripts/31 reads it. The codes are weighed by inputs$code_trade
# (hs6, value) where the inputs keep it; otherwise they are counted, which orders a node's cells but does not
# change them. Without aib_inputs.rds the nodes carry none (the contract allows it; the views fall back to the
# approximate roll-up).
inputs_path <- abs_path(arg_value("--inputs", file.path(in_dir, "aib_inputs.rds")))
legacy_cells <- NULL
if (!file.exists(inputs_path)) {
  warning("No ", inputs_path, ": the nodes carry no three-stage cells (node.yfCellsLegacy); run scripts/31_build_aib_trade_network.R.", call. = FALSE)
} else {
  inputs <- readRDS(inputs_path)
  osi_master <- utils::read.csv(file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv"),
                                colClasses = c(hs6 = "character"), stringsAsFactors = FALSE)
  code_trade <- if (is.data.frame(inputs$code_trade) && all(c("hs6", "value") %in% names(inputs$code_trade))) as.data.frame(inputs$code_trade) else NULL
  taxonomy <- if (is.null(res$meta$taxonomy)) "master" else res$meta$taxonomy
  legacy_cells <- aib_dash_legacy_cells(inputs$node_codes, osi_master, code_trade, taxonomy = taxonomy,
                                        node_cells = inputs$node_cells, node_cells_detail = inputs$node_cells_detail)
  ids <- res$classification$node_id
  unknown <- setdiff(ids, attr(legacy_cells, "known_nodes"))
  if (length(unknown)) {
    warning(length(unknown), " of ", length(ids), " nodes of ", res_path, " are not nodes of ", inputs_path, " (another run?): ",
            paste(utils::head(unknown, 5), collapse = ", "), ". They carry no three-stage cells (the dashboard uses the approximate roll-up).", call. = FALSE)
  }
  message(sprintf("Three-stage cells: %d of %d nodes (%s), from %s and energy_hs6_master.csv supply_chain", sum(ids %in% legacy_cells$node_id), length(ids),
                  if (is.null(code_trade)) "codes counted: aib_inputs.rds keeps no code trade" else "codes weighed by world trade", basename(inputs_path)))
}

bundle <- aib_dash_bundle(res, top_edges = top_edges, strategic = strategic, legacy_cells = legacy_cells)
problems <- aib_check_bundle_additions(bundle)
if (length(problems)) stop("The AIB bundle does not match the contract:\n  ", paste(utils::head(problems, 20), collapse = "\n  "))
# The scenario files (Managed Interdependence, Baseline), each checked against the contract, the main
# document and the scenario the manifest will name it for (aib_check_scenario_doc(), the R mirror of
# checkScenarioDoc()).
scenario_docs <- Filter(Negate(is.null), list(managed = aib_dash_scenario_managed(res, top_edges = top_edges),
                                              baseline = aib_dash_scenario_baseline(res)))
for (id in names(scenario_docs)) {
  sp <- aib_check_scenario_doc(scenario_docs[[id]], main = bundle, expect = id)
  if (length(sp)) stop("The AIB ", id, " scenario file does not match the contract:\n  ", paste(utils::head(sp, 20), collapse = "\n  "))
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# Each file is named by the first 8 hex digits of its content's md5.
write_hashed <- function(txt, prefix) {
  tmp <- tempfile(fileext = ".json")
  writeLines(txt, tmp, useBytes = TRUE)
  f <- sprintf("%s.%s.json", prefix, substr(unname(tools::md5sum(tmp)), 1, 8))
  writeLines(txt, file.path(out_dir, f), useBytes = TRUE)
  unlink(tmp)
  f
}
fname <- write_hashed(country_json(bundle), "compacts")
scenario_files <- lapply(names(scenario_docs), function(id) write_hashed(country_json(scenario_docs[[id]]), paste0("scenario-", id)))
names(scenario_files) <- names(scenario_docs)
old <- setdiff(list.files(out_dir, pattern = "^compacts\\.[0-9a-f]{8}\\.json$"), fname)
old <- c(old, setdiff(list.files(out_dir, pattern = "^scenario-(managed|baseline)\\.[0-9a-f]{8}\\.json$"), unlist(scenario_files)))
if (length(old)) invisible(file.remove(file.path(out_dir, old)))
files <- list(compacts = fname)
if (length(scenario_files)) files$scenarios <- scenario_files
manifest <- list(schema = AIB_DASH_SCHEMA, version = AIB_DASH_VERSION, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 demandScope = res$meta$demand_scope, candidateUniverse = res$meta$candidate_universe,
                 shockSemantics = res$meta$headline_semantics, supplyConstraint = res$meta$headline_constraint,
                 universe = res$meta$universe, headlineShock = res$meta$headline_shock, headlineScenario = res$meta$headline_scenario,
                 files = files)
writeLines(country_json(manifest, pretty = TRUE), file.path(out_dir, "manifest.json"), useBytes = TRUE)
gz_kb <- function(f) length(memCompress(readBin(file.path(out_dir, f), "raw", file.size(file.path(out_dir, f))), "gzip")) / 1024
message(sprintf("AIB dashboard bundle: %s (%.0f kB, gzip %.1f kB; %d compacts, %d nodes, %d network countries, %d network edges, %d investment flows) in %s",
                fname, file.size(file.path(out_dir, fname)) / 1024, gz_kb(fname), length(bundle$compacts), length(bundle$nodes),
                length(bundle$network$countries), length(bundle$network$edges), length(bundle$investmentFlows$pairs), out_dir))
if (!is.null(bundle$strategic)) {
  sj <- country_json(bundle$strategic)
  message(sprintf("  strategic section: %.0f kB, gzip %.1f kB; %d strategic compacts, %d workstreams, %d countries, %d compact roles, %d theses",
                  nchar(sj, "bytes") / 1024, length(memCompress(charToRaw(as.character(sj)), "gzip")) / 1024, length(bundle$strategic$compacts),
                  length(bundle$strategic$workstreams), length(bundle$strategic$countries), length(bundle$strategic$compactRoles),
                  length(bundle$strategic$theses)))
}
for (id in names(scenario_files)) {
  d <- scenario_docs[[id]]
  message(sprintf("  scenario %s: %s (%.0f kB, gzip %.1f kB; %d compacts, %d network countries, %d network edges%s)", id, scenario_files[[id]],
                  file.size(file.path(out_dir, scenario_files[[id]])) / 1024, gz_kb(scenario_files[[id]]), length(d$compacts),
                  length(d$network$countries), length(d$network$edges),
                  if (identical(id, "managed")) sprintf(", %d with a coalition, %d investment flows", sum(!vapply(d$compacts, function(c) is.null(c$coalition), logical(1))),
                                                        length(d$investmentFlows$pairs)) else sprintf(", %d nodes", length(d$nodes))))
}
