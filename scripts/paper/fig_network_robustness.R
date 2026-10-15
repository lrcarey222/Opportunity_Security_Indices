# Figure, table and numbers: robustness of the allied network design (paper Section 10 and
# Appendix C; fig:network_robustness, tab:network_robustness, results/network_robustness.json).
#
# Re-runs allied_network_design() with its inputs assembled exactly as
# scripts/30_build_allied_network_design.R assembles them (same sources; the derived two-tier
# coalition of coalition_tbl.rds plus the six EU members outside the tiers, the EU-27 bundled as one
# node with the stage weights of eu_stage_weights_tbl.rds, the coalition's friendshore and
# opportunity dyads, the 2024 reconciled basket trade for the EU node's edges, WDI GDP), first in the
# production configuration and then with one setting changed at a time:
#   no_caps        portfolio caps off                    portfolio_enable = FALSE
#   greedy         greedy heuristic instead of the MILP  method = "greedy"
#   w_edge_0/1     no edge term / heavier edge term      w_edge = 0, w_edge = 1
#   demand_need    demand from need only                 demand_mode = "need"
#   demand_size    demand from size only                 demand_mode = "size"
#   lambda_lo/hi   the neighbouring grid points of the   diversity_lambda = lambda -/+ 0.01
#                  calibrated diversification weight     (0.77 and 0.79 around the pick 0.78)
# plus one auxiliary run (greedy without caps) for the solver comparison on identical eligible sets.
# For every variant and stage it reports the Jaccard overlap of the producer set (nodes with a
# positive production share) with the production design and the share of production reassigned
# (total-variation distance of the share vectors, 0.5 * sum_p |x_alt - x_base|).
#
# The production re-run must reproduce data/processed/outputs/allied_network_specialization.csv
# (shares to 1e-6) or nothing is written. The no-caps run and the two lambda neighbours are compared
# with the lambda study's runs (<study dir>/runs/nc37_l0.78.rds, g37_l0.77.rds, g37_l0.79.rds) for
# the record. The two stage solvers are wrapped (the wrapper calls the original function with the
# original arguments, so results are unchanged) to log the solve time of every stage.
#
# Also computes the edge coverage of the node pairs (both edge components, friendshore only,
# opportunity only, neither) with the design's own edge builder, and the flow-weighted edge weight.
#
# Reads: data/processed/outputs/index_outputs.rds, data/processed/coalition_tbl.rds,
# data/processed/eu_stage_weights_tbl.rds, data/processed/partner_{friendshore,opportunity}_tbl.rds,
# data/processed/partner_development_country_tbl.rds, data/raw/{wdi_country_info,wdi_gdp,
# comtrade_energy_dyads_all,energy_hs6_master}.csv, config/partnership.yml,
# data/processed/outputs/allied_network_*.csv, and the lambda study runs named above.
# Writes: paper/figures/network_robustness.pdf (+ png), paper/tables/network_robustness.tex and
# paper/results/network_robustness.json.
#
# Run from the repository root (about 45-60 minutes; one R process; the MILP runs dominate):
#   Rscript scripts/paper/fig_network_robustness.R

local({
  find_theme <- function() {
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    while (dirname(d) != d) {
      f <- file.path(d, "scripts", "paper", "_paper_theme.R")
      if (file.exists(f)) return(f)
      d <- dirname(d)
    }
    stop("Run from inside the repository.")
  }
  source(find_theme(), local = FALSE)
})

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(rlang)
})

root <- paper_repo_root()
# Same sources as scripts/30_build_allied_network_design.R
source(file.path(root, "R", "utils", "scurve.R"))
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "country.R"))
source(file.path(root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(root, "R", "indices", "allied_network_design.R"))
source(file.path(root, "R", "indices", "build_indices.R"))  # coalition_members()
design_src <- file.path(root, "R", "indices", "allied_network_design.R")
design_src_md5 <- unname(tools::md5sum(design_src))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
raw_dir <- file.path(root, config$raw_data_dir)
outputs_dir <- file.path(processed_dir, "outputs")
study_dir <- Sys.getenv("OPSI_NETWORK_STUDY_DIR", file.path(dirname(root), "scratch_final", "network"))

if (!allied_network_has_milp()) stop("The MILP stack (ompr, ompr.roi, ROI, ROI.plugin.glpk) is required.")

stored_spec <- utils::read.csv(file.path(outputs_dir, "allied_network_specialization.csv"), stringsAsFactors = FALSE)
stored_diag <- utils::read.csv(file.path(outputs_dir, "allied_network_diagnostics.csv"), stringsAsFactors = FALSE)
lambda_prod <- unique(stored_diag$diversity_lambda)
stopifnot(length(lambda_prod) == 1)
# The two neighbouring points of the calibration grid (step 0.01 around the pick)
lambda_lo <- round(lambda_prod - 0.01, 2)
lambda_hi <- round(lambda_prod + 0.01, 2)

# ---------------------------------------------------------------------------
# Inputs, loaded as scripts/30 loads them
index_outputs <- readRDS(file.path(outputs_dir, "index_outputs.rds"))
economic_opportunity_index <- index_outputs$economic_opportunity_index
energy_security_index      <- index_outputs$energy_security_index
policy_index               <- index_outputs$policy_index
rm(index_outputs); invisible(gc())

# Coalition (derived) + the EU members outside the tiers, the EU-27 as one node
coalition_tbl <- readRDS(file.path(processed_dir, "coalition_tbl.rds"))
coalition <- coalition_members(coalition_tbl)
coalition_network <- allied_network_add_node_members(coalition, coalition_tbl)
iso3c_network <- coalition_network$iso3
network_nodes <- unique(coalition_network$network_node)
node_member_weights <- readRDS(file.path(processed_dir, "eu_stage_weights_tbl.rds"))
network_node_labels <- c(EUU = "European Union")
message(sprintf("Coalition: %d members + %d node-only = %d economies, %d nodes", nrow(coalition),
                sum(coalition_network$node_only), length(iso3c_network), length(network_nodes)))

partner_tbl_cols <- c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable",
                      "data_type", "value", "Year")
read_coalition_dyads <- function(name, variable_name) {
  tbl <- readRDS(file.path(processed_dir, paste0(name, ".rds")))
  out <- tbl %>%
    dplyr::filter(variable == variable_name, reporter_iso %in% iso3c_network,
                  partner_iso %in% iso3c_network) %>%
    dplyr::select(dplyr::any_of(partner_tbl_cols)) %>%
    tibble::as_tibble()
  rm(tbl); invisible(gc())
  out
}
partner_friendshore_tbl <- read_coalition_dyads("partner_friendshore_tbl", "Friendshore Index")
partner_opportunity_tbl <- read_coalition_dyads("partner_opportunity_tbl", "Opportunity Index")
partner_development_country_tbl <- readRDS(file.path(processed_dir, "partner_development_country_tbl.rds"))

country_info <- standardize_country_info(read.csv(file.path(raw_dir, "wdi_country_info.csv")))
country_size_tbl <- allied_network_country_size_tbl(
  read.csv(file.path(raw_dir, "wdi_gdp.csv"), stringsAsFactors = FALSE), country_info, iso3c_network
)

# Latest-year reconciled basket trade (scripts/30's trade block)
partnership_cfg_path <- file.path(root, "config", "partnership.yml")
partnership_cfg <- if (file.exists(partnership_cfg_path)) yaml::read_yaml(partnership_cfg_path) else list()
comtrade_all_path <- file.path(raw_dir, "comtrade_energy_dyads_all.csv")
comtrade_allied_path <- file.path(raw_dir, "allied_comtrade_energy_data.csv")
hs6_master_path <- file.path(raw_dir, "energy_hs6_master.csv")
trade_source <- partnership_strength_resolve_trade_source(
  partnership_cfg$trade$source,
  all_reporters_exists = file.exists(comtrade_all_path),
  allied_exists = file.exists(comtrade_allied_path)
)
trade_path <- if (identical(trade_source$source, "all_reporters")) comtrade_all_path else comtrade_allied_path
trade_cols <- c("reporter_iso", "partner_iso", "period", "ref_year", "flow_direction", "flow_code", "cmd_code", "primary_value")
trade_header <- names(data.table::fread(trade_path, nrows = 0))
trade_dyads <- as.data.frame(data.table::fread(
  trade_path,
  select = intersect(trade_cols, trade_header),
  colClasses = list(character = intersect(c("reporter_iso", "partner_iso", "flow_direction", "cmd_code"), trade_header)),
  na.strings = c("", "NA"),
  showProgress = FALSE
))
trade_year_col <- intersect(c("period", "ref_year"), names(trade_dyads))[1]
trade_years <- suppressWarnings(as.integer(trade_dyads[[trade_year_col]]))
network_trade_year <- max(trade_years, na.rm = TRUE)
trade_dyads <- trade_dyads[!is.na(trade_years) & trade_years == network_trade_year, , drop = FALSE]
rm(trade_years)
trade_cells <- partnership_strength_build_trade_cells(
  trade_dyads,
  read.csv(hs6_master_path),
  years = network_trade_year,
  cif_fob_factor = if (is.null(partnership_cfg$trade$cif_fob_factor)) PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR else as.numeric(partnership_cfg$trade$cif_fob_factor)
)
rm(trade_dyads); invisible(gc())
node_trade_tbl <- allied_network_member_trade(trade_cells, iso3c_network)
rm(trade_cells); invisible(gc())
message(sprintf("Trade: %d basket cells within the network economies (%d).", nrow(node_trade_tbl), network_trade_year))

# ---------------------------------------------------------------------------
# Solver wrappers: time every stage solve. `key` ("tech||supply_chain") is a local of the calling
# solve_one_stage() closure.
orig_milp <- allied_network_solve_stage_milp
orig_greedy <- allied_network_solve_stage_greedy
solve_log <- new.env()
solve_log$run <- NA_character_
solve_log$rows <- list()
# CPU seconds of this R process (GLPK runs in-process), less sensitive than wall-clock time to
# other jobs on a shared machine
cpu_now <- function() {
  p <- proc.time()
  p[["user.self"]] + p[["sys.self"]]
}
log_solve <- function(fun, solver, caller_env, args) {
  key <- tryCatch(get("key", envir = caller_env, inherits = TRUE), error = function(e) NA_character_)
  if (!is.character(key)) key <- NA_character_
  forb <- args$producer_forbidden
  if (is.null(forb)) forb <- character()
  t0 <- proc.time()[["elapsed"]]
  c0 <- cpu_now()
  ok <- FALSE
  on.exit({
    solve_log$rows[[length(solve_log$rows) + 1L]] <- tibble::tibble(
      run = solve_log$run, stage_key = key, solver = solver, ok = ok,
      elapsed_sec = proc.time()[["elapsed"]] - t0,
      cpu_sec = cpu_now() - c0,
      n_forbidden = length(forb), forbidden = paste(sort(forb), collapse = " ")
    )
  })
  out <- do.call(fun, args)
  ok <- TRUE
  out
}
allied_network_solve_stage_milp <- function(...) {
  caller <- parent.frame()
  log_solve(orig_milp, "milp", caller, list(...))
}
allied_network_solve_stage_greedy <- function(...) {
  caller <- parent.frame()
  log_solve(orig_greedy, "greedy", caller, list(...))
}

# Production arguments of scripts/30 (the progress callback is omitted; it only prints)
production_args <- list(
  economic_opportunity_index = economic_opportunity_index,
  energy_security_index = energy_security_index,
  policy_index = policy_index,
  partner_friendshore_tbl = partner_friendshore_tbl,
  partner_opportunity_tbl = partner_opportunity_tbl,
  partner_development_country_tbl = partner_development_country_tbl,
  country_info = country_info,
  iso3c_network = iso3c_network,
  network_members = coalition_network,
  node_member_weights = node_member_weights,
  node_trade_tbl = node_trade_tbl,
  node_labels = network_node_labels,
  method = "auto",
  min_producers = 3,
  max_share = 0.40,
  min_share = 0.05,
  min_suppliers_per_consumer = 2,
  epsilon_supplier_share = 0.10,
  allow_self = TRUE,
  w_node = 1.0,
  w_edge = 0.5,
  w_dev = 0.0,
  auto_milp_max_nodes = 60,
  milp_stage_time_limit_sec = 300,
  node_policy = FALSE,
  country_size_tbl = country_size_tbl,
  demand_mode = "mixed",
  demand_weights = list(need = 0.5, size = 0.5),
  portfolio_enable = TRUE,
  portfolio_top_k = 5,
  portfolio_min_cap = 2,
  portfolio_max_cap = 10,
  portfolio_transform = "log",
  portfolio_max_iters = 30,
  policy_missing = "coalition_median",
  missing_components = "renormalize",
  friendshore_orientation = "reporter_is_consumer",
  supply_chains = c("Upstream", "Midstream"),
  diversity_lambda = lambda_prod,
  diversity_breakpoint_step = 0.01
)

run_design <- function(tag, ...) {
  args <- utils::modifyList(production_args, list(...))
  solve_log$run <- tag
  t0 <- proc.time()[["elapsed"]]
  c0 <- cpu_now()
  warns <- character()
  res <- withCallingHandlers(
    do.call(allied_network_design, args),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  res$runtime_sec <- proc.time()[["elapsed"]] - t0
  res$cpu_sec <- cpu_now() - c0
  res$warnings <- unique(warns)
  message(sprintf("[%s] %.1f s (CPU %.1f s); methods: %s%s", tag, res$runtime_sec, res$cpu_sec,
                  paste(names(table(res$diagnostics$method)), table(res$diagnostics$method),
                        sep = " = ", collapse = ", "),
                  if (!is.null(res$portfolio)) sprintf("; portfolio converged %s after %d passes",
                                                       res$portfolio$converged, res$portfolio$iters) else ""))
  res$build_candidates <- NULL
  res
}

variants <- tibble::tribble(
  ~tag,          ~label,                      ~short,          ~setting,
  "no_caps",     "No portfolio caps",          "No caps",       "portfolio_enable = FALSE",
  "greedy",      "Greedy heuristic",           "Greedy",        "method = 'greedy'",
  "w_edge_0",    "Edge weight 0",              "Edge wt. 0",    "w_edge = 0",
  "w_edge_1",    "Edge weight 1",              "Edge wt. 1",    "w_edge = 1",
  "demand_need", "Demand from need only",      "Need only",     "demand_mode = 'need'",
  "demand_size", "Demand from size only",      "Size only",     "demand_mode = 'size'",
  "lambda_lo",   sprintf("Diversification weight %s", format(lambda_lo)), sprintf("lambda %s", format(lambda_lo)),
                 sprintf("diversity_lambda = %s", format(lambda_lo)),
  "lambda_hi",   sprintf("Diversification weight %s", format(lambda_hi)), sprintf("lambda %s", format(lambda_hi)),
                 sprintf("diversity_lambda = %s", format(lambda_hi))
)

# OPSI_NETWORK_ROBUSTNESS_CACHE (optional, for development only): an .rds path where the solved runs
# are stored after solving and read back on the next call instead of re-solving. Unset (the default),
# every call solves all runs and writes nothing besides the paper outputs.
cache_path <- Sys.getenv("OPSI_NETWORK_ROBUSTNESS_CACHE", "")
if (nzchar(cache_path) && file.exists(cache_path)) {
  cached <- readRDS(cache_path)
  stopifnot(identical(cached$design_src_md5, design_src_md5), isTRUE(all.equal(cached$lambda_prod, lambda_prod)))
  runs <- cached$runs
  solve_tbl <- cached$solve_tbl
  rm(cached)
  message("Runs read from the cache ", cache_path)
} else {
  runs <- list()
  runs$production     <- run_design("production")
  runs$no_caps        <- run_design("no_caps", portfolio_enable = FALSE)
  runs$greedy         <- run_design("greedy", method = "greedy")
  runs$greedy_no_caps <- run_design("greedy_no_caps", method = "greedy", portfolio_enable = FALSE)
  runs$w_edge_0       <- run_design("w_edge_0", w_edge = 0)
  runs$w_edge_1       <- run_design("w_edge_1", w_edge = 1)
  runs$demand_need    <- run_design("demand_need", demand_mode = "need")
  runs$demand_size    <- run_design("demand_size", demand_mode = "size")
  runs$lambda_lo      <- run_design("lambda_lo", diversity_lambda = lambda_lo)
  runs$lambda_hi      <- run_design("lambda_hi", diversity_lambda = lambda_hi)
  solve_tbl <- dplyr::bind_rows(solve_log$rows) %>%
    tidyr::separate(stage_key, into = c("tech", "supply_chain"), sep = "\\|\\|", remove = FALSE)
  if (nzchar(cache_path)) {
    saveRDS(list(runs = runs, solve_tbl = solve_tbl, design_src_md5 = design_src_md5, lambda_prod = lambda_prod),
            cache_path)
  }
}

# ---------------------------------------------------------------------------
# Check: the production re-run reproduces the stored outputs of scripts/30
repro <- stored_spec %>%
  dplyr::select(tech, supply_chain, iso3c, share_stored = production_share) %>%
  dplyr::full_join(runs$production$specialization %>%
                     dplyr::select(tech, supply_chain, iso3c, share_rerun = production_share),
                   by = c("tech", "supply_chain", "iso3c"))
repro_max_abs_diff <- max(abs(repro$share_stored - repro$share_rerun))
repro_rows_unmatched <- sum(is.na(repro$share_stored) | is.na(repro$share_rerun))
message(sprintf("Reproduction of stored outputs: max |share diff| = %.2e, unmatched rows = %d",
                repro_max_abs_diff, repro_rows_unmatched))
if (repro_max_abs_diff > 1e-6 || repro_rows_unmatched > 0) {
  stop("The production re-run does not reproduce data/processed/outputs/allied_network_specialization.csv; ",
       "nothing is written.")
}

# The lambda study's runs at the same settings (for the record: identical inputs and settings must
# give identical designs)
study_match <- function(tag, file) {
  f <- file.path(study_dir, "runs", file)
  if (!file.exists(f)) return(list(file = file, available = FALSE))
  s <- readRDS(f)$specialization
  x <- runs[[tag]]$specialization %>% dplyr::select(tech, supply_chain, iso3c, a = production_share) %>%
    dplyr::full_join(s %>% dplyr::select(tech, supply_chain, iso3c, b = production_share), by = c("tech", "supply_chain", "iso3c"))
  list(file = file, available = TRUE, rows = nrow(x), unmatched = sum(is.na(x$a) | is.na(x$b)),
       max_abs_share_diff = max(abs(x$a - x$b), na.rm = TRUE))
}
study_checks <- list(
  production = study_match("production", sprintf("g%d_l%s.rds", length(network_nodes), format(lambda_prod))),
  no_caps = study_match("no_caps", sprintf("nc%d_l%s.rds", length(network_nodes), format(lambda_prod))),
  lambda_lo = study_match("lambda_lo", sprintf("g%d_l%s.rds", length(network_nodes), format(lambda_lo))),
  lambda_hi = study_match("lambda_hi", sprintf("g%d_l%s.rds", length(network_nodes), format(lambda_hi)))
)

# ---------------------------------------------------------------------------
# Producer sets, Jaccard overlap and production reassigned relative to the production design
eps <- 1e-9
producer_sets <- function(res) {
  res$specialization %>%
    dplyr::filter(production_share > eps) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(set = list(sort(unique(iso3c))), .groups = "drop")
}
jaccard <- function(a, b) {
  if (is.null(a) || is.null(b) || !length(union(a, b))) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}
stage_grid <- runs$production$diagnostics %>% dplyr::distinct(tech, supply_chain)
base_sets <- producer_sets(runs$production)
base_shares <- runs$production$specialization %>% dplyr::select(tech, supply_chain, iso3c, x_base = production_share)
compare_run <- function(tag) {
  v_sets <- producer_sets(runs[[tag]])
  tv <- base_shares %>%
    dplyr::full_join(runs[[tag]]$specialization %>% dplyr::select(tech, supply_chain, iso3c, x_alt = production_share),
                     by = c("tech", "supply_chain", "iso3c")) %>%
    dplyr::mutate(dplyr::across(c(x_base, x_alt), ~ dplyr::coalesce(.x, 0))) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(reassigned = 0.5 * sum(abs(x_alt - x_base)),
                     max_abs_share_change = max(abs(x_alt - x_base)), .groups = "drop")
  stage_grid %>%
    dplyr::left_join(base_sets %>% dplyr::rename(base = set), by = c("tech", "supply_chain")) %>%
    dplyr::left_join(v_sets %>% dplyr::rename(alt = set), by = c("tech", "supply_chain")) %>%
    dplyr::left_join(tv, by = c("tech", "supply_chain")) %>%
    dplyr::mutate(
      tag = tag,
      jaccard = purrr::map2_dbl(base, alt, jaccard),
      identical = purrr::map2_lgl(base, alt, ~ !is.null(.x) && !is.null(.y) && identical(.x, .y)),
      n_base = lengths(base), n_alt = lengths(alt),
      n_common = purrr::map2_int(base, alt, ~ length(intersect(.x, .y))),
      entered = purrr::map2_chr(base, alt, ~ paste(setdiff(.y, .x), collapse = " ")),
      left = purrr::map2_chr(base, alt, ~ paste(setdiff(.x, .y), collapse = " "))
    ) %>%
    dplyr::select(tag, tech, supply_chain, jaccard, identical, reassigned, max_abs_share_change,
                  n_base, n_alt, n_common, entered, left)
}
jac_tbl <- purrr::map_dfr(variants$tag, compare_run)

summ_design <- function(res) {
  d <- res$diagnostics
  list(
    n_stages_solved = sum(!grepl("^skipped", d$method)),
    methods = as.list(table(d$method)),
    n_fallback = sum(!is.na(d$fallback_reason) & nzchar(dplyr::coalesce(d$fallback_reason, ""))),
    hhi_min = min(d$hhi, na.rm = TRUE), hhi_median = stats::median(d$hhi, na.rm = TRUE),
    hhi_max = max(d$hhi, na.rm = TRUE),
    effective_producers_median = stats::median(d$effective_producers, na.rm = TRUE),
    n_producers_median = stats::median(d$n_producers, na.rm = TRUE),
    n_producers_min = min(d$n_producers, na.rm = TRUE), n_producers_max = max(d$n_producers, na.rm = TRUE),
    n_producer_roles = sum(res$specialization$production_share > eps),
    max_share_max = max(d$max_production_share, na.rm = TRUE),
    min_suppliers_realized_min = min(d$min_suppliers_realized, na.rm = TRUE),
    min_supplier_share_realized_min = min(d$min_supplier_share_realized, na.rm = TRUE),
    objective_sum = sum(d$objective, na.rm = TRUE),
    n_nodes_with_role = dplyr::n_distinct(res$specialization$iso3c[res$specialization$production_share > eps]),
    eu_n_stages = sum(res$specialization$iso3c == "EUU" & res$specialization$production_share > eps),
    portfolio_converged = if (!is.null(res$portfolio)) res$portfolio$converged else NA,
    portfolio_passes = if (!is.null(res$portfolio)) res$portfolio$iters else NA,
    portfolio_violations_remaining = if (!is.null(res$portfolio)) res$portfolio$violations_remaining else NA,
    runtime_sec = res$runtime_sec, cpu_sec = res$cpu_sec,
    warnings = I(res$warnings)
  )
}
var_summary <- jac_tbl %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(
    mean_jaccard = mean(jaccard, na.rm = TRUE), median_jaccard = stats::median(jaccard, na.rm = TRUE),
    min_jaccard = min(jaccard, na.rm = TRUE), n_identical = sum(identical),
    mean_reassigned = mean(reassigned), median_reassigned = stats::median(reassigned),
    max_reassigned = max(reassigned),
    stage_max_reassigned = paste(tech, supply_chain)[which.max(reassigned)],
    roles_kept = sum(n_common), roles_base = sum(n_base), roles_alt = sum(n_alt),
    .groups = "drop"
  ) %>%
  dplyr::left_join(variants, by = "tag") %>%
  dplyr::arrange(match(tag, variants$tag)) %>%
  dplyr::mutate(
    hhi_median = purrr::map_dbl(tag, ~ stats::median(runs[[.x]]$diagnostics$hhi)),
    n_producers_median = purrr::map_dbl(tag, ~ stats::median(runs[[.x]]$diagnostics$n_producers)),
    max_share = purrr::map_dbl(tag, ~ max(runs[[.x]]$diagnostics$max_production_share)),
    n_milp = purrr::map_int(tag, ~ sum(runs[[.x]]$diagnostics$method == "milp")),
    objective_change_pct = purrr::map_dbl(tag, ~ 100 * (sum(runs[[.x]]$diagnostics$objective) -
                                                           sum(runs$production$diagnostics$objective)) /
                                            sum(runs$production$diagnostics$objective))
  )
print(as.data.frame(var_summary %>% dplyr::select(tag, mean_jaccard, min_jaccard, n_identical, mean_reassigned,
                                                  max_reassigned, hhi_median, n_milp)))

# Node roles that change: which nodes enter or leave the design in any stage, by variant
node_moves <- purrr::map(stats::setNames(variants$tag, variants$tag), function(tg) {
  v <- jac_tbl[jac_tbl$tag == tg, ]
  ent <- unlist(strsplit(v$entered[nzchar(v$entered)], " "))
  lef <- unlist(strsplit(v$left[nzchar(v$left)], " "))
  list(n_roles_entered = length(ent), n_roles_left = length(lef),
       entered_by_node = as.list(sort(table(ent), decreasing = TRUE)),
       left_by_node = as.list(sort(table(lef), decreasing = TRUE)),
       nodes_without_role = I(sort(setdiff(network_nodes, unique(runs[[tg]]$specialization$iso3c[runs[[tg]]$specialization$production_share > eps])))))
})

# Stages stable across all variants, and across all but the caps / solver variants
stable <- jac_tbl %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(n_identical = sum(identical), min_jaccard = min(jaccard), mean_reassigned = mean(reassigned),
                   .groups = "drop")

# Solver comparison without caps (identical eligible sets, so J_MILP >= J_greedy must hold)
obj_cmp <- runs$no_caps$diagnostics %>%
  dplyr::select(tech, supply_chain, J_milp = objective, hhi_milp = hhi) %>%
  dplyr::inner_join(runs$greedy_no_caps$diagnostics %>% dplyr::select(tech, supply_chain, J_greedy = objective, hhi_greedy = hhi),
                    by = c("tech", "supply_chain")) %>%
  dplyr::mutate(gap = J_milp - J_greedy, gap_pct = 100 * gap / J_milp)
solver_nocaps <- producer_sets(runs$no_caps) %>% dplyr::rename(a = set) %>%
  dplyr::inner_join(producer_sets(runs$greedy_no_caps) %>% dplyr::rename(b = set), by = c("tech", "supply_chain")) %>%
  dplyr::mutate(j = purrr::map2_dbl(a, b, jaccard), same = purrr::map2_lgl(a, b, identical))

# Objective cost of the caps (same eligible sets only without caps; with caps the loop bars nodes)
J_stage <- runs$production$diagnostics %>%
  dplyr::select(tech, supply_chain, J_capped = objective) %>%
  dplyr::inner_join(runs$no_caps$diagnostics %>% dplyr::select(tech, supply_chain, J_uncapped = objective),
                    by = c("tech", "supply_chain")) %>%
  dplyr::mutate(loss_pct = 100 * (J_uncapped - J_capped) / J_uncapped)

# ---------------------------------------------------------------------------
# Edge coverage of the node pairs, with the design's own edge builder: one call with both components,
# one with friendshore only and one with opportunity only (presence of each component per pair)
# As allied_network_design() calls it: member weights of the solved (Upstream and Midstream) stages
# from allied_network_member_weights(), and the trade table passed through allied_network_member_trade()
edge_member_weights <- allied_network_member_weights(
  allied_network_check_members(coalition_network, iso3c_network), node_member_weights,
  runs$production$diagnostics %>% dplyr::distinct(tech, supply_chain)
)
edge_args <- list(iso3c_network = iso3c_network, friendshore_orientation = "reporter_is_consumer",
                  network_members = coalition_network, member_weights = edge_member_weights,
                  node_trade_tbl = allied_network_member_trade(node_trade_tbl, iso3c_network))
edges_both <- do.call(allied_network_extract_edges, c(list(partner_friendshore_tbl, partner_opportunity_tbl), edge_args))
edges_fs <- do.call(allied_network_extract_edges, c(list(partner_friendshore_tbl, partner_opportunity_tbl[0, ]), edge_args))
edges_op <- do.call(allied_network_extract_edges, c(list(partner_friendshore_tbl[0, ], partner_opportunity_tbl), edge_args))
rm(partner_friendshore_tbl, partner_opportunity_tbl); invisible(gc())
keys <- c("reporter_iso", "partner_iso", "tech", "supply_chain")
stage_nodes <- runs$production$specialization %>% dplyr::distinct(tech, supply_chain, iso3c)
pairs <- stage_nodes %>%
  dplyr::rename(reporter_iso = iso3c) %>%
  dplyr::inner_join(stage_nodes %>% dplyr::rename(partner_iso = iso3c),
                    by = c("tech", "supply_chain"), relationship = "many-to-many") %>%
  dplyr::filter(reporter_iso != partner_iso) %>%
  dplyr::left_join(edges_fs %>% dplyr::transmute(reporter_iso, partner_iso, tech, supply_chain, has_fs = TRUE), by = keys) %>%
  dplyr::left_join(edges_op %>% dplyr::transmute(reporter_iso, partner_iso, tech, supply_chain, has_op = TRUE), by = keys) %>%
  dplyr::left_join(edges_both %>% dplyr::select(dplyr::all_of(keys), edge_weight), by = keys) %>%
  dplyr::mutate(
    has_fs = dplyr::coalesce(has_fs, FALSE), has_op = dplyr::coalesce(has_op, FALSE),
    coverage = dplyr::case_when(has_fs & has_op ~ "both", has_fs ~ "fs_only", has_op ~ "op_only", TRUE ~ "neither"),
    edge_weight = dplyr::coalesce(edge_weight, 0)
  )
edge_cov_stage <- pairs %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(n_pairs = dplyr::n(), share_both = mean(coverage == "both"), share_fs_only = mean(coverage == "fs_only"),
                   share_op_only = mean(coverage == "op_only"), share_neither = mean(coverage == "neither"),
                   mean_edge_weight_offdiag = mean(edge_weight), .groups = "drop")
flow_w <- runs$production$flows %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(
    flow_weighted_edge_weight_offdiag = sum((flow_share * edge_weight)[reporter_iso != partner_iso]) /
      sum(flow_share[reporter_iso != partner_iso]),
    self_share = sum(flow_share[reporter_iso == partner_iso]),
    .groups = "drop"
  ) %>%
  dplyr::left_join(edge_cov_stage %>% dplyr::select(tech, supply_chain, mean_edge_weight_offdiag), by = c("tech", "supply_chain"))

# ---------------------------------------------------------------------------
# Runtime of the production configuration
prod_solves <- solve_tbl %>% dplyr::filter(run == "production")
first_solve <- prod_solves %>% dplyr::group_by(stage_key) %>% dplyr::slice(1) %>% dplyr::ungroup()
runtime <- list(
  production_total_sec = runs$production$runtime_sec,
  production_total_cpu_sec = runs$production$cpu_sec,
  production_n_stage_solves = nrow(prod_solves),
  production_n_resolves = nrow(prod_solves) - dplyr::n_distinct(prod_solves$stage_key),
  production_first_pass_median_stage_sec = stats::median(first_solve$elapsed_sec),
  production_first_pass_min_stage_sec = min(first_solve$elapsed_sec),
  production_first_pass_max_stage_sec = max(first_solve$elapsed_sec),
  production_first_pass_slowest_stage = paste(first_solve$tech, first_solve$supply_chain)[which.max(first_solve$elapsed_sec)],
  production_first_pass_median_stage_cpu_sec = stats::median(first_solve$cpu_sec),
  production_all_solves_max_stage_sec = max(prod_solves$elapsed_sec),
  production_failed_milp_solves = sum(prod_solves$solver == "milp" & !prod_solves$ok),
  milp_stage_time_limit_sec = production_args$milp_stage_time_limit_sec,
  by_run_total_sec = as.list(purrr::map_dbl(runs, "runtime_sec")),
  by_run_total_cpu_sec = as.list(purrr::map_dbl(runs, "cpu_sec")),
  greedy_no_caps_median_stage_sec = stats::median(solve_tbl$elapsed_sec[solve_tbl$run == "greedy_no_caps"]),
  note = paste("*_sec: wall-clock seconds; *_cpu_sec: CPU seconds of the R process (GLPK runs in-process), on a",
               "shared Windows workstation (R 4.4.1, GLPK via ompr/ROI) with other jobs running, so wall-clock",
               "times vary between runs. Stage times include building the ompr model. The design run excludes",
               "loading the inputs.")
)

# ---------------------------------------------------------------------------
results <- list(
  description = paste(
    "Robustness of the allied network design (Section 10). Each variant changes one setting of the production",
    "configuration of scripts/30 (", length(network_nodes), "nodes with the EU-27 as one node; Upstream and Midstream;",
    "MILP via method = 'auto'; GDP-log portfolio caps on; w_edge = 0.5; demand_mode = 'mixed'; diversity_lambda =",
    lambda_prod, "). Producer set = nodes with production share > 0 in a stage. Jaccard = |A and B| / |A or B| of",
    "the variant's and the production design's producer sets, per stage. reassigned = 0.5 * sum_p |x_alt - x_base|",
    "(share of the stage's production assigned to different nodes; 0 = identical shares, 1 = disjoint)."),
  reproduction_check = list(
    max_abs_share_diff_vs_stored_csv = repro_max_abs_diff, unmatched_rows = repro_rows_unmatched,
    design_code_md5 = design_src_md5, n_nodes = length(network_nodes), n_economies = length(iso3c_network),
    trade_year = network_trade_year
  ),
  lambda_study_runs_match = study_checks,
  production = summ_design(runs$production),
  variants = purrr::map(stats::setNames(variants$tag, variants$tag), function(tg) {
    s <- var_summary[var_summary$tag == tg, ]
    c(as.list(s %>% dplyr::select(label, setting, mean_jaccard, median_jaccard, min_jaccard, n_identical,
                                  mean_reassigned, median_reassigned, max_reassigned, stage_max_reassigned,
                                  roles_kept, roles_base, roles_alt, objective_change_pct)),
      list(design = summ_design(runs[[tg]]), node_moves = node_moves[[tg]]))
  }),
  by_stage = jac_tbl,
  stable = list(
    n_stages_identical_all_variants = sum(stable$n_identical == nrow(variants)),
    stages_identical_all_variants = I(paste(stable$tech, stable$supply_chain)[stable$n_identical == nrow(variants)]),
    n_stages_changed_in_every_variant = sum(stable$n_identical == 0),
    by_stage = stable
  ),
  solver_comparison_no_caps = list(
    description = "MILP (method auto) vs greedy, both without portfolio caps (identical eligible sets).",
    n_stages = nrow(obj_cmp),
    n_stages_milp_ge_greedy = sum(obj_cmp$gap >= -1e-9),
    objective_gap_pct_median = stats::median(obj_cmp$gap_pct), objective_gap_pct_max = max(obj_cmp$gap_pct),
    objective_sum_gap_pct = 100 * (sum(obj_cmp$J_milp) - sum(obj_cmp$J_greedy)) / sum(obj_cmp$J_milp),
    hhi_median_milp = stats::median(obj_cmp$hhi_milp), hhi_median_greedy = stats::median(obj_cmp$hhi_greedy),
    producer_set_jaccard_mean = mean(solver_nocaps$j), producer_set_jaccard_min = min(solver_nocaps$j),
    n_identical_sets = sum(solver_nocaps$same),
    greedy_min_suppliers_realized_min = min(runs$greedy_no_caps$diagnostics$min_suppliers_realized),
    greedy_min_supplier_share_realized_min = min(runs$greedy_no_caps$diagnostics$min_supplier_share_realized),
    greedy_max_share = max(runs$greedy_no_caps$diagnostics$max_production_share)
  ),
  caps_cost = list(
    objective_sum_capped = sum(J_stage$J_capped), objective_sum_uncapped = sum(J_stage$J_uncapped),
    objective_loss_pct = 100 * (sum(J_stage$J_uncapped) - sum(J_stage$J_capped)) / sum(J_stage$J_uncapped),
    objective_loss_pct_stage_max = max(J_stage$loss_pct),
    stage_max_loss = paste(J_stage$tech, J_stage$supply_chain)[which.max(J_stage$loss_pct)],
    n_stages_objective_lower = sum(J_stage$loss_pct > 1e-7),
    hhi_median_uncapped = stats::median(runs$no_caps$diagnostics$hhi)
  ),
  edges = list(
    description = paste(
      "Off-diagonal ordered node pairs (producer p != consumer c) of each stage. both = FS(c,p) and PO(p,c) observed",
      "(after the EU node's aggregation); fs_only / op_only = one component; neither = no observation (W = 0).",
      "mean_edge_weight_offdiag = unweighted mean of W over these pairs (zeros included).",
      "flow_weighted_edge_weight_offdiag = sum f W / sum f over the production design's flows between distinct nodes."),
    n_pairs = nrow(pairs),
    n_pairs_both = sum(pairs$coverage == "both"), n_pairs_fs_only = sum(pairs$coverage == "fs_only"),
    n_pairs_op_only = sum(pairs$coverage == "op_only"), n_pairs_neither = sum(pairs$coverage == "neither"),
    share_both = mean(pairs$coverage == "both"),
    fs_only_stages = I(paste(edge_cov_stage$tech, edge_cov_stage$supply_chain)[edge_cov_stage$share_fs_only > 0.999]),
    by_stage = edge_cov_stage %>% dplyr::left_join(flow_w %>% dplyr::select(-mean_edge_weight_offdiag), by = c("tech", "supply_chain")),
    flow_weighted_edge_weight_offdiag_median = stats::median(flow_w$flow_weighted_edge_weight_offdiag),
    mean_edge_weight_offdiag_median = stats::median(flow_w$mean_edge_weight_offdiag),
    n_stages_flow_weighted_above_mean = sum(flow_w$flow_weighted_edge_weight_offdiag > flow_w$mean_edge_weight_offdiag),
    self_share_mean = mean(flow_w$self_share), self_share_max = max(flow_w$self_share),
    n_stages_with_self_flow = sum(flow_w$self_share > 1e-12)
  ),
  runtime = runtime,
  stage_solve_times_production_first_pass = first_solve %>% dplyr::select(tech, supply_chain, solver, ok, elapsed_sec, cpu_sec)
)
write_paper_results(results, "network_robustness")

# ---------------------------------------------------------------------------
# Table: summary by variant
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
signed <- function(x) sprintf("$%s%s$", ifelse(x < 0, "-", "+"), formatC(abs(x), format = "f", digits = 2))
tex_label <- c(no_caps = "No portfolio caps", greedy = "Greedy heuristic",
               w_edge_0 = "Edge weight $w_E=0$", w_edge_1 = "Edge weight $w_E=1$",
               demand_need = "Demand from need only", demand_size = "Demand from size only",
               lambda_lo = sprintf("$\\lambda=%s$", format(lambda_lo)),
               lambda_hi = sprintf("$\\lambda=%s$", format(lambda_hi)))
pd <- runs$production$diagnostics
prod_row <- sprintf("Production design & -- & -- & -- & -- & %s & %d & %s & %d \\\\",
                    fmt(stats::median(pd$hhi), 3), as.integer(stats::median(pd$n_producers)),
                    fmt(max(pd$max_production_share), 3), sum(pd$method == "milp"))
var_rows <- purrr::map_chr(seq_len(nrow(var_summary)), function(i) {
  r <- var_summary[i, ]
  sprintf("%s & %s & %s & %d & %s & %s & %s & %s & %d \\\\", tex_label[[r$tag]], fmt(r$mean_jaccard, 3), fmt(r$min_jaccard, 3),
          as.integer(r$n_identical), fmt(r$mean_reassigned, 3), fmt(r$hhi_median, 3),
          formatC(r$n_producers_median, format = "fg", digits = 3), fmt(r$max_share, 3), as.integer(r$n_milp))
})
tab_lines <- c(
  "% Generated by scripts/paper/fig_network_robustness.R",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Robustness of the network design to one setting at a time}",
  "\\label{tab:network_robustness}",
  "\\footnotesize",
  "\\begin{tabular}{@{}l r r r r r r r r@{}}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Against the production design} & \\multicolumn{4}{c}{Design of the variant} \\\\",
  "\\cmidrule(lr){2-5}\\cmidrule(l){6-9}",
  paste("Variant & Jaccard & Min. & \\makecell[r]{Identical\\\\stages} & \\makecell[r]{Re-\\\\assigned}",
        "& \\makecell[r]{Median\\\\$\\HHI^{\\mathrm{D}}$} & \\makecell[r]{Pro-\\\\ducers} & \\makecell[r]{Max.\\\\share}",
        "& \\makecell[r]{MILP\\\\stages} \\\\"),
  "\\midrule",
  prod_row,
  "\\addlinespace",
  var_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\emph{Notes:} Production design: $\\lambda=", format(lambda_prod), "$, GDP-log portfolio caps, ",
         "edge weight $w_E=0.5$, demand from the equal mix of need and size, MILP. Each other row re-solves the ", nrow(pd), " Upstream and Midstream stage programs on the ",
         length(network_nodes), " network nodes with one setting of the production design changed. Jaccard: mean across ",
         "stages of the Jaccard overlap $|A\\cap B|/|A\\cup B|$ of the stage's producer set with that of the production ",
         "design; Min.: its smallest value; Identical stages: stages with the same producer set. Reassigned: mean across ",
         "stages of the share of production assigned to different nodes, $\\tfrac12\\sum_p|x^{\\mathrm{alt}}_{p,ts}-",
         "x_{p,ts}|$ (0 when the shares are unchanged, 1 when the producer sets are disjoint). Median ",
         "$\\HHI^{\\mathrm{D}}$: median across stages of the Herfindahl index of designed shares. Producers: median ",
         "number of producers per stage. Max. share: largest designed share in any stage. MILP stages: stages solved ",
         "by the MILP (the greedy row solves all stages heuristically). Source: ",
         "\\texttt{scripts/paper/fig\\_network\\_robustness.R}."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tab_lines, file.path(tab_dir, "network_robustness.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# Figure: change against the production design, by stage (rows) and variant (columns). Panel (a):
# producer-set change, 1 - Jaccard; panel (b): production reassigned. Both 0 = unchanged.
stage_levels <- c("Upstream", "Midstream")
tech_levels <- sort(unique(jac_tbl$tech), decreasing = TRUE)
fig_tbl <- jac_tbl %>%
  dplyr::transmute(tag, tech, supply_chain, `(a) Producer set changed (1 - Jaccard)` = 1 - jaccard,
                   `(b) Production reassigned` = reassigned) %>%
  tidyr::pivot_longer(-c(tag, tech, supply_chain), names_to = "measure", values_to = "value") %>%
  dplyr::mutate(
    variant = factor(variants$short[match(tag, variants$tag)], levels = variants$short),
    supply_chain = factor(supply_chain, levels = stage_levels),
    tech = factor(tech, levels = tech_levels),
    zero = value < 1e-9
  )
p_rob <- ggplot(fig_tbl, aes(x = variant, y = tech)) +
  geom_tile(aes(fill = value), colour = "white", linewidth = 0.5) +
  geom_point(data = fig_tbl %>% dplyr::filter(zero), shape = 16, size = 0.5, colour = paper_ink$muted) +
  facet_grid(supply_chain ~ measure) +
  # plotmath for the Greek letter (the pdf device's standard fonts carry it in the Symbol font)
  scale_x_discrete(labels = as.expression(c(as.list(variants$short[1:6]),
                                            list(bquote(lambda == .(lambda_lo)), bquote(lambda == .(lambda_hi)))))) +
  scale_fill_gradientn(colours = c("#ffffff", paper_seq_blue), limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = c("0", "0.25", "0.5", "0.75", "1"),
                       name = "Change against the production design (0 = unchanged)") +
  guides(fill = guide_colourbar(barwidth = unit(2.4, "in"), barheight = unit(0.12, "in"),
                                title.position = "top", title.hjust = 0)) +
  labs(x = NULL, y = NULL) +
  theme_paper(base_size = 8.5, grid = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
        axis.text.y = element_text(size = 7.5),
        axis.line.x = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8),
        panel.spacing.x = unit(0.8, "lines"), panel.spacing.y = unit(0.5, "lines"))
save_paper_fig(p_rob, "network_robustness", width = paper_width_full, height = 4.9)
message("Done: network_robustness.json, tables/network_robustness.tex, figures/network_robustness.")
