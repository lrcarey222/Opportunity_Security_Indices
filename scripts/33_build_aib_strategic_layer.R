# Allied Industrial Base (AIB), step 3: the strategic layer (docs/aib_methodology.md, section 16):
# Strategic Sectoral Compacts above the compact workstreams, their policy portfolios and missions per
# scenario, the Strategic AIB Roles of countries, compact-specific country roles, template narratives and
# bilateral relationship theses. Reads scripts/32's outputs and never changes them; run_pipeline.R runs it
# after scripts/32 (OPSI_RUN_AIB), and scripts/84 exports its aib_strategic.rds as the bundle's `strategic`
# section. The rules are pure functions in R/aib/aib_strategic.R.
#
# Usage:
#   Rscript scripts/33_build_aib_strategic_layer.R [--in-dir DIR] [--out-dir DIR] [--partner-dir DIR] [--dip FILE]
#     [--compacts FILE] [--node-functions FILE] [--layer FILE]
#
#   --in-dir          where scripts/32 wrote aib_results.rds and aib_inputs.rds (default: <outputs_dir>/aib)
#   --out-dir         where the outputs go (default: --in-dir)
#   --partner-dir     the processed partner bundle holding cm_fdi_projects_tbl.rds (scripts/15; default:
#                     <processed_dir>)
#   --dip             IMF Direct Investment Positions: the raw imf_dip.csv (default <raw_data_dir>/imf_dip.csv;
#                     only the needed columns are read) or an .rds of its rows
#   --compacts, --node-functions, --layer
#                     other config files than config/aib_strategic_compacts.yml, aib_node_functions.yml and
#                     aib_strategic_layer.yml (a revision under review)
#
# A missing CM-FDI or DIP input leaves the Capital & Investment Anchor role "not assessed" (with a warning);
# the run does not fail. A compact map that breaks its validation contract stops the run.
# Writes (CSV, into the out dir):
#   aib_strategic_compacts.csv              strategic compact x scenario: portfolio, missions, policy menu,
#                                           coverage, core countries, critical partners
#   aib_compact_workstreams.csv             each compact candidate (AIB-Cxx) as a workstream of one compact
#   aib_strategic_spine.csv                 compact x value-chain step x node x scenario: node status and coverage
#   aib_compact_country_roles.csv           country x strategic compact: compact-specific role and evidence
#   aib_compact_division_of_labour.csv      country x node evidence behind the division-of-labour matrix
#   aib_country_strategic_roles.csv         country x strategic role: gates, percentiles, primary / secondary
#   aib_country_strategic_role_metrics.csv  every role metric per assessed economy
#   aib_country_role_narratives.csv         template narratives, one column per slot
#   aib_relationship_theses.csv             allied pairs with a modelled relationship: thesis, logic, compacts
#   aib_strategic.rds                       everything above in one list (for scripts/84)

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

t_script <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(repo_root, "scripts", "utils", "aib_strategic_config.R"))

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(repo_root, config$processed_dir, "outputs")
in_dir <- abs_path(arg_value("--in-dir", file.path(outputs_dir, "aib")))
out_dir <- abs_path(arg_value("--out-dir", in_dir))
partner_dir <- abs_path(arg_value("--partner-dir", file.path(repo_root, config$processed_dir)))
dip_path <- abs_path(arg_value("--dip", file.path(repo_root, aib_or(config$raw_data_dir, "data/raw"), "imf_dip.csv")))
res_path <- file.path(in_dir, "aib_results.rds")
inputs_path <- file.path(in_dir, "aib_inputs.rds")
for (p in c(res_path, inputs_path)) if (!file.exists(p)) stop("Run scripts/31 and scripts/32 first: ", p, " not found.")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scfg <- aib_read_strategic_config(repo_root, compacts = arg_value("--compacts"), node_functions = arg_value("--node-functions"),
                                  layer = arg_value("--layer"))
step("AIB strategic layer: ", paste(basename(scfg$files), collapse = ", "))
res <- readRDS(res_path)
inputs <- readRDS(inputs_path)

# ---- Capital evidence (read-only partner data) ----------------------------------------------------------
cmfdi_path <- file.path(partner_dir, "cm_fdi_projects_tbl.rds")
cmfdi <- if (file.exists(cmfdi_path)) as.data.frame(readRDS(cmfdi_path)) else NULL
if (is.null(cmfdi)) warning("CM-FDI projects not found (", cmfdi_path, "): outward cleantech-manufacturing FDI not assessed.", call. = FALSE)
read_dip <- function(path, cfg) {
  if (!file.exists(path)) return(NULL)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) return(as.data.frame(readRDS(path)))
  years <- as.character(unlist(aib_or(cfg$dip_years, list(2023, 2022))))
  if (!requireNamespace("data.table", quietly = TRUE)) return(utils::read.csv(path, check.names = FALSE))
  hdr <- names(data.table::fread(path, nrows = 0))
  keep <- c(intersect(c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"), hdr), intersect(c(years, paste0("X", years)), hdr))
  d <- data.table::fread(path, select = keep, showProgress = FALSE)
  ind <- aib_or(cfg$dip_indicator, "Outward Direct investment, Net (assets less liabilities), All financial instruments, All entities")
  as.data.frame(d[d$INDICATOR == ind, ])
}
dip <- read_dip(dip_path, scfg$layer$capital)
if (is.null(dip)) warning("IMF DIP not found (", dip_path, "): outward direct investment not assessed.", call. = FALSE)
step("  capital inputs: CM-FDI ", if (is.null(cmfdi)) "absent" else sprintf("%d projects", nrow(cmfdi)),
     "; IMF DIP ", if (is.null(dip)) "absent" else sprintf("%s rows (%s)", format(nrow(dip), big.mark = ","), basename(dip_path)))
conduits <- partnership_strength_conduit_centres()$iso3

# ---- The layer (pure) -------------------------------------------------------------------------------------
st <- aib_strategic_layer(inputs, res, scfg$compacts, scfg$node_functions, scfg$layer,
                          capital = list(dip = dip, cmfdi = cmfdi), conduits = conduits)
has_dip <- !is.null(dip)
rm(dip)
if (length(st$problems)) stop("The strategic compact map breaks its validation contract:\n  ", paste(st$problems, collapse = "\n  "))
for (n in st$notes) step("  note: ", n)

st$meta <- list(built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), results_built = res$meta$built, taxonomy = res$meta$taxonomy,
                dictionary_scope = res$meta$dictionary_scope, headline_shock = res$meta$headline_shock, managed_shock = res$scenario_shock,
                default_scenario = aib_or(scfg$layer$scenarios$default, "managed"),
                config_files = unname(basename(scfg$files)), cmfdi = if (is.null(cmfdi)) NA_character_ else basename(cmfdi_path),
                dip = if (has_dip) basename(dip_path) else NA_character_)

# ---- Write -------------------------------------------------------------------------------------------------
write_out <- function(tbl, name) {
  utils::write.csv(tbl, file.path(out_dir, name), row.names = FALSE, na = "")
  step("  wrote ", name, " (", format(NROW(tbl), big.mark = ","), " rows)")
}
write_out(st$compacts, "aib_strategic_compacts.csv")
write_out(st$workstreams, "aib_compact_workstreams.csv")
write_out(st$spine, "aib_strategic_spine.csv")
write_out(st$compact_roles, "aib_compact_country_roles.csv")
write_out(st$division, "aib_compact_division_of_labour.csv")
write_out(st$roles, "aib_country_strategic_roles.csv")
write_out(st$metrics, "aib_country_strategic_role_metrics.csv")
write_out(st$narratives, "aib_country_role_narratives.csv")
write_out(st$theses, "aib_relationship_theses.csv")
saveRDS(st, file.path(out_dir, "aib_strategic.rds"))
step("  wrote aib_strategic.rds")

# ---- Summary ------------------------------------------------------------------------------------------------
asg <- st$roles[st$roles$primary_or_secondary %in% c("primary", "secondary"), , drop = FALSE]
q <- st$roles[st$roles$primary_ok | st$roles$secondary_ok, , drop = FALSE]
step(sprintf("  %d strategic compacts x %d scenarios; %d workstreams (%d straddling, %d by override); %d economies assessed (%d allied), %d with a role; max qualifying roles %d",
             nrow(st$compact_defs), nrow(st$scenarios), nrow(st$workstreams), sum(st$workstreams$straddles),
             sum(grepl("^override", st$workstreams$assignment_basis)), nrow(st$metrics), sum(st$metrics$allied),
             length(unique(asg$iso3)), if (nrow(q)) max(table(q$iso3)) else 0L))
step("AIB strategic layer done in ", format(round(difftime(Sys.time(), t_script, units = "mins"), 1)))
