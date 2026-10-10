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

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  cran <- if (is.null(repos) || is.null(repos[["CRAN"]])) "" else repos[["CRAN"]]
  if (is.null(repos) || identical(cran, "@CRAN@") || is.na(cran) || cran == "") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

ensure_user_lib <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path(
      Sys.getenv("USERPROFILE"),
      "Documents",
      "R",
      "win-library",
      paste0(getRversion()[1, 1:2], collapse = ".")
    )
  }
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
  invisible(user_lib)
}

ensure_milp_pkgs <- function(install_if_missing = TRUE, require_milp = FALSE) {
  milp_pkgs <- c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk")
  missing <- milp_pkgs[!vapply(milp_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(TRUE)

  if (!install_if_missing) {
    if (require_milp) stop("MILP required but missing packages: ", paste(missing, collapse = ", "))
    return(FALSE)
  }

  ensure_cran_repo()
  ensure_user_lib()
  message("MILP packages missing; attempting install on Windows: ", paste(missing, collapse = ", "))
  ok <- TRUE
  tryCatch({
    install.packages(missing, dependencies = TRUE)
  }, error = function(e) {
    ok <<- FALSE
    message("Automatic install failed: ", conditionMessage(e))
  })

  missing2 <- milp_pkgs[!vapply(milp_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing2) == 0) return(TRUE)

  msg <- paste0(
    "MILP packages still missing after install attempt: ", paste(missing2, collapse = ", "), "\n",
    "On Windows this usually means you need permissions to write to your R library or a build toolchain.\n",
    "Try running R/RStudio as Administrator, or set a user library (R_LIBS_USER), or install Rtools if compilation is required."
  )
  if (require_milp) stop(msg) else message(msg)
  FALSE
}

install_if_missing <- tolower(Sys.getenv("OPSI_INSTALL_MILP_PKGS", "true")) %in% c("1", "true", "yes")
require_milp <- tolower(Sys.getenv("OPSI_REQUIRE_MILP", "false")) %in% c("1", "true", "yes")
milp_available <- ensure_milp_pkgs(install_if_missing = install_if_missing, require_milp = require_milp)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))  # coalition_members()

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) stop("Processed data directory not found: ", processed_dir)

read_processed_tbl <- function(name) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Processed table not found: ", path)
  readRDS(path)
}

# Index outputs path
outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds_path)) {
  outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
    file.path(repo_root, config$outputs_rds)
  } else {
    file.path(processed_dir, "outputs", "index_outputs.rds")
  }
}
if (!file.exists(outputs_rds_path)) stop("Index outputs not found: ", outputs_rds_path)
index_outputs <- readRDS(outputs_rds_path)

economic_opportunity_index <- index_outputs$economic_opportunity_index
energy_security_index      <- index_outputs$energy_security_index
# Not used by the design: the node policy term is off (network_node_policy below)
policy_index               <- index_outputs$policy_index
rm(index_outputs)
invisible(gc())

if (is.null(economic_opportunity_index) || is.null(energy_security_index)) {
  stop("Index outputs missing one of: economic_opportunity_index, energy_security_index")
}

# Coalition: derived, not listed by hand (coalition_members() in R/indices/build_indices.R). The
# two-tier coalition of coalition_tbl.rds (scripts/13_build_coalition.R; members are tier core or
# partner) or, until that table exists, the single-tier alignment coalition of
# geo_coalition_tbl.rds (scripts/12_build_geopolitical_alignment.R, member == TRUE: the
# U.S.-leaning UN-vote bloc with GDP >= the floor in config/geopolitical_distance.yml, 31 members
# with the five-session window, Oct 2026; coalition_members() warns). It replaces the hand-picked
# 23-member list, which survives only as the legacy/test default allied_network_default_iso3c().
#
# Network nodes (Oct 2026 decision): the EU is ONE node ("EUU", coalition_tbl$network_node). The
# design aggregates its members' inputs with the stage weights of eu_stage_weights_tbl.rds
# (scripts/13: production or export shares, else GDP shares) and weights its edges by the members'
# 2024 basket trade (rules in R/indices/allied_network_design.R, "Network nodes"). The EU node is
# the whole EU-27: network_node_all_members = TRUE adds the six EU members that do not pass the
# coalition's own tier rules (tier "none" in coalition_tbl.rds, below the GDP floor or Nonaligned:
# CYP EST LTU LVA MLT SVN, ~1.5% of EU GDP) to the node, because the node stands for the EU, whose
# stage weights, combined GDP and extra-EU trade are EU-27 quantities. FALSE bundles only the 21
# EU coalition members (the stage weights are then renormalised over them). With the 54 coalition
# members (31 core, 23 partner) this gives 34 network nodes.
network_node_all_members <- TRUE
coalition_path <- file.path(processed_dir, "coalition_tbl.rds")
geo_coalition_path <- file.path(processed_dir, "geo_coalition_tbl.rds")
if (!file.exists(coalition_path) && !file.exists(geo_coalition_path)) {
  stop(
    "No coalition table in ", processed_dir, ": neither coalition_tbl.rds ",
    "(scripts/13_build_coalition.R) nor geo_coalition_tbl.rds ",
    "(scripts/12_build_geopolitical_alignment.R). The allied coalition is derived, not listed by ",
    "hand; run scripts/12_build_geopolitical_alignment.R and scripts/13_build_coalition.R first."
  )
}
coalition_tbl <- if (file.exists(coalition_path)) readRDS(coalition_path) else NULL
coalition <- if (!is.null(coalition_tbl)) {
  coalition_members(coalition_tbl)
} else {
  coalition_members(NULL, geo_coalition_tbl = readRDS(geo_coalition_path))
}
# The countries whose inputs are read: the coalition members plus (network_node_all_members) the
# rest of each bundled node; node_only flags the latter
coalition_network <- if (isTRUE(network_node_all_members) && !is.null(coalition_tbl)) {
  allied_network_add_node_members(coalition, coalition_tbl)
} else {
  dplyr::mutate(coalition, node_only = FALSE)
}
iso3c_network <- coalition_network$iso3
network_nodes <- unique(coalition_network$network_node)
bundled_nodes <- unique(coalition_network$network_node[duplicated(coalition_network$network_node)])
message(
  "Allied network coalition (", coalition$source[1], "): ", nrow(coalition), " members (",
  paste(names(table(coalition$tier)), table(coalition$tier), sep = " ", collapse = ", "), ")",
  if (any(coalition_network$node_only)) paste0(" + ", sum(coalition_network$node_only), " countries of bundled nodes (",
                                             paste(coalition_network$iso3[coalition_network$node_only], collapse = " "), ")") else "",
  "; ", length(network_nodes), " network nodes",
  if (length(bundled_nodes)) paste0(" (bundled: ", paste(bundled_nodes, collapse = ", "), ")") else "",
  ": ", paste(network_nodes, collapse = " ")
)

# Member weights of the bundled nodes per (tech, stage) (scripts/13_build_coalition.R)
node_member_weights <- NULL
if (length(bundled_nodes)) {
  eu_weights_path <- file.path(processed_dir, "eu_stage_weights_tbl.rds")
  if (!file.exists(eu_weights_path)) {
    stop("The coalition bundles node(s) ", paste(bundled_nodes, collapse = ", "), " but ", eu_weights_path,
         " is missing; run scripts/13_build_coalition.R.")
  }
  node_member_weights <- readRDS(eu_weights_path)
}
# Display names of bundled nodes
network_node_labels <- c(EUU = "European Union")

# Partner dyad tables (from scripts/15_build_partner_themes.R). The friendshore table has ~17M rows;
# keep only the coalition's index rows as each table is read, so the full tables are never held
# together. allied_network_design() collapses whatever duplicates remain.
partner_tbl_cols <- c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value", "Year")
read_coalition_dyads <- function(name, variable_name) {
  tbl <- read_processed_tbl(name)
  out <- tbl %>%
    dplyr::filter(
      variable == variable_name,
      reporter_iso %in% iso3c_network,
      partner_iso %in% iso3c_network
    ) %>%
    dplyr::select(dplyr::any_of(partner_tbl_cols)) %>%
    tibble::as_tibble()
  rm(tbl)
  invisible(gc())
  out
}
partner_friendshore_tbl <- read_coalition_dyads("partner_friendshore_tbl", "Friendshore Index")
partner_opportunity_tbl <- read_coalition_dyads("partner_opportunity_tbl", "Opportunity Index")
partner_development_country_tbl <- NULL
dev_path <- file.path(processed_dir, "partner_development_country_tbl.rds")
if (file.exists(dev_path)) {
  partner_development_country_tbl <- readRDS(dev_path)
}

# Load country_info from the configured raw data directory.
is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

resolve_raw_data_dir <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) return(NULL)
    stop("Raw data directory not found: ", raw_base_dir)
  }
  raw_base_dir
}

raw_data_path <- resolve_raw_data_dir(repo_root, config$raw_data_dir, skip_data_downloads)
if (is.null(raw_data_path)) stop("Raw data directory not found (and SKIP_DATA_DOWNLOADS is set).")

wdi_country_path <- file.path(raw_data_path, "wdi_country_info.csv")
country_info <- read.csv(wdi_country_path)
country_info <- standardize_country_info(country_info)

# Coalition GDP: latest finite WDI year per member (see allied_network_country_size_tbl)
gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
if (!file.exists(gdp_path)) stop("GDP file not found: ", gdp_path)
country_size_tbl <- allied_network_country_size_tbl(
  read.csv(gdp_path, stringsAsFactors = FALSE),
  country_info,
  iso3c_network
)

# Basket trade, latest year, for the bundled nodes' edge weights and the observed export
# concentration: the reconciled exporter -> importer matrix of the partner themes (scripts/15:
# importer's declaration, else the exporter's at the CIF/FOB factor; config/partnership.yml) from
# the all-reporter Comtrade dyad file (scripts/98), only the latest year. Without the file the
# bundled edges fall back to the stage weights.
partnership_cfg_path <- file.path(repo_root, "config", "partnership.yml")
partnership_cfg <- if (file.exists(partnership_cfg_path)) yaml::read_yaml(partnership_cfg_path) else list()
comtrade_all_path <- file.path(raw_data_path, "comtrade_energy_dyads_all.csv")
comtrade_allied_path <- file.path(raw_data_path, "allied_comtrade_energy_data.csv")
hs6_master_path <- file.path(raw_data_path, "energy_hs6_master.csv")
trade_cells <- NULL
if ((file.exists(comtrade_all_path) || file.exists(comtrade_allied_path)) && file.exists(hs6_master_path)) {
  trade_source <- partnership_strength_resolve_trade_source(
    partnership_cfg$trade$source,
    all_reporters_exists = file.exists(comtrade_all_path),
    allied_exists = file.exists(comtrade_allied_path)
  )
  trade_path <- if (identical(trade_source$source, "all_reporters")) comtrade_all_path else comtrade_allied_path
  if (isTRUE(trade_source$fallback)) {
    warning("Allied network: comtrade_energy_dyads_all.csv is missing; trade weights from the 22-reporter allied file.", call. = FALSE)
  }
  # Only the columns the trade builder uses (the all-reporter file is ~700 MB), cmd_code as text
  trade_cols <- c("reporter_iso", "partner_iso", "period", "ref_year", "flow_direction", "flow_code", "cmd_code", "primary_value")
  trade_dyads <- if (requireNamespace("data.table", quietly = TRUE)) {
    trade_header <- names(data.table::fread(trade_path, nrows = 0))
    as.data.frame(data.table::fread(
      trade_path,
      select = intersect(trade_cols, trade_header),
      colClasses = list(character = intersect(c("reporter_iso", "partner_iso", "flow_direction", "cmd_code"), trade_header)),
      na.strings = c("", "NA"),
      showProgress = FALSE
    ))
  } else {
    utils::read.csv(trade_path, colClasses = c(cmd_code = "character"))
  }
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
  rm(trade_dyads)
  invisible(gc())
  message("Allied network: ", network_trade_year, " basket trade from ", basename(trade_path), " (", format(nrow(trade_cells), big.mark = ","), " exporter-importer cells).")
} else if (length(bundled_nodes)) {
  warning("Allied network: no Comtrade dyad file or HS6 crosswalk in ", raw_data_path,
          "; the bundled nodes' edges are weighted by the stage weights only.", call. = FALSE)
}
node_trade_tbl <- if (is.null(trade_cells)) NULL else allied_network_member_trade(trade_cells, iso3c_network)

# Run
fmt_time <- function(seconds) {
  if (!is.finite(seconds) || is.na(seconds)) return("--:--")
  seconds <- max(0, round(seconds))
  mins <- seconds %/% 60
  secs <- seconds %% 60
  sprintf("%02d:%02d", mins, secs)
}

progress_bar <- NULL
on.exit({
  if (!is.null(progress_bar)) close(progress_bar)
}, add = TRUE)

progress_callback <- function(info) {
  if (!is.list(info) || is.null(info$event)) return(invisible(NULL))
  if (info$event == "start_stage") {
    if (is.null(progress_bar)) {
      progress_bar <<- utils::txtProgressBar(min = 0, max = info$total, style = 3)
    }
    return(invisible(NULL))
  }
  if (info$event == "fallback_greedy") {
    cat(sprintf("  fallback to greedy for %s / %s: %s\n", info$tech, info$supply_chain, info$reason))
    return(invisible(NULL))
  }
  if (info$event == "end_stage" && !is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, info$current)
    pct_remaining <- max(0, min(100, 100 * info$pct_remaining))
    msg <- sprintf(
      "  %s/%s complete | %0.1f%% remaining | ETA %s | method: %s | stage: %s / %s",
      info$current,
      info$total,
      pct_remaining,
      fmt_time(info$eta_sec),
      info$method,
      info$tech,
      info$supply_chain
    )
    cat(msg, "\n")
  }
  invisible(NULL)
}


# method = "auto" solves a stage by MILP when it has at most this many network nodes and by greedy
# above it (greedy also stays the fallback when the solver fails or hits its time limit). Every stage
# is a MILP stage: with the EU bundled the network has 34 nodes (Oct 2026), and 60 also covers the
# 54-member coalition solved member by member. The function default 30 dates from the 23-member
# coalition. In a timing probe on 57 unbundled members (lambda 1.5, one pass, no caps) the stage MILPs
# reached optimality in 23 to 245 s. A node set larger than this is warned about below rather than
# silently solved by greedy.
auto_milp_max_nodes <- 60
if (length(network_nodes) > auto_milp_max_nodes) {
  warning(
    "The network has ", length(network_nodes), " nodes, more than auto_milp_max_nodes = ",
    auto_milp_max_nodes, ": method = 'auto' solves every stage by greedy. Raise auto_milp_max_nodes ",
    "(and check the stage time limit) or bundle members into shared network nodes.",
    call. = FALSE
  )
}
# GLPK time limit per stage solve (seconds); a stage that hits it falls back to greedy and the reason
# is written to diagnostics$fallback_reason. Raised from 120 s (Oct 2026): with 31 members per stage the
# MILP is larger, and in probe runs at 120 s Green Hydrogen Midstream hit the limit during the
# portfolio-cap passes (lambda >= 2), which changes the path of the caps loop.
network_milp_time_limit_sec <- 300

# Producer (node) score without the policy term: EO and ES only (node_weights eo 0.5 and resilience
# 0.2, renormalized to 5/7 and 2/7). While the Policy index required CAT it covered fewer coalition
# members than EO and ES, so with the term on, uncovered members got the stage median imputed
# (policy_imputed); since Oct 2026 the network does not use it (since the IEA PAMS-only fallback of
# 9 Oct 2026 every member has a Policy index, 11 of them on IEA PAMS alone). allied_network_design()'s default node_policy = TRUE keeps the legacy
# three-component score for other callers; the choice is recorded in params and diagnostics.
network_node_policy <- FALSE

# Stages in the network design: Upstream and Midstream only. Downstream deployment is not part of the
# allied division of labour; the portfolio caps count top-5 appearances over these 22 stages only.
network_supply_chains <- c("Upstream", "Midstream")

# Diversification weight: each stage maximises its linear score objective minus lambda * HHI of the
# designed production shares (score units per unit of HHI; lambda = 0 is the linear design, which
# puts two producers at the 0.40 cap in most stages). Calibration rule: the smallest lambda on a grid
# at which the median designed HHI over the 22 Upstream/Midstream stages is at most the median
# observed export HHI of the network's own nodes (allied_network_observed_export_hhi(): 2024 basket
# trade from the reconciled Comtrade matrix built above, shares within the nodes, the EU-27 as ONE
# exporter counting only its extra-EU exports, every other node its exports to all other economies;
# median over the network stages with an HS6 basket, 14 since Electric Grid Upstream's copper and
# aluminium basket of 2026-10-09, 13 between the basket review and then; written to
# allied_network_observed_hhi.csv).
# Extra-EU, because trade among the EU members is the bundled node's internal trade, as a country's
# domestic sales are; counting it would make the EU the leading exporter of nearly every stage.
# lambda = 0.95 is the pick of the 2026-10-09 evening calibration on the final node set (57 coalition
# members, 31 core and 26 partners with Brazil, plus the 6 other EU members: 37 nodes with the EU
# bundled; caps on, node policy term off, Upstream and Midstream, MILP for every stage, 300 s stage
# limit, 30 cap passes, the settings of this script; inputs of the evening full rerun: USGS MCS 2026
# mineral reserves, the rebuilt Comtrade energy-trade file, Hong Kong out of the Comtrade export
# measures, the Electric Grid Upstream copper/aluminium basket): target 0.1352 (7.40 equivalent
# exporters, 14 basket stages); grid 0 to 1.5 with a 0.01 step from 0.85 to 1.0: 0.93 -> 0.1390 and
# 0.94 -> 0.1370 (above), 0.95 -> 0.1349 (7.41 effective producers, 10 producers per stage at the
# median, max share 0.278), 0.96 -> 0.1312, 1.0 -> 0.1300. Every grid run solved all 22 stages by
# MILP with no fallback and the caps loop converged. Study: osi-worktrees/scratch_final/network
# (log.md "Lambda study (37 nodes, final)", lambda_study_g37.csv), outside the repository. Earlier
# picks: 0.92 (36 nodes, morning of 2026-10-09), 0.94 (34 nodes, 2026-10-08), 1.5 (the hand-picked
# 23-member coalition with the node policy term on). The pick is knife-edge: its margin to the
# target is 0.0003 and the designed median is not monotone in lambda (0.89 -> 0.1409 but 0.90 ->
# 0.1429; 0.98 -> 0.1305 but 0.99 -> 0.1315) because the caps loop changes the barred sets.
# Recalibrate on the inputs of each full rerun (rebuilding the study's trade matrix on the current
# basket); the run warns below when the designed median exceeds the observed one.
network_diversity_lambda <- 0.95
# Share step of the MILP's tangent approximation of HHI (error <= step^2 / 4 = 2.5e-5 per producer)
network_diversity_breakpoint_step <- 0.01
# Interior shares move top-5 membership in more stages per pass than the bang-bang design did, so
# the portfolio-cap loop gets more passes than the default 10 (the 23-member calibration ran with 30).
network_portfolio_max_iters <- 30

if (milp_available) {
  message("MILP solver stack detected (ompr/ROI/GLPK). method='auto' uses MILP for stages with up to ", auto_milp_max_nodes, " nodes; greedy if a stage is larger or the solver fails.")
} else {
  message("MILP solver stack not detected. method='auto' will use greedy fallback.")
}

res <- allied_network_design(
  economic_opportunity_index = economic_opportunity_index,
  energy_security_index = energy_security_index,
  policy_index = policy_index,
  partner_friendshore_tbl = partner_friendshore_tbl,
  partner_opportunity_tbl = partner_opportunity_tbl,
  partner_development_country_tbl = partner_development_country_tbl,
  country_info = country_info,
  iso3c_network = iso3c_network,
  # tier and network_node per member: members sharing a node (the EU) are solved as one node, with
  # the stage weights, the members' basket trade on each edge and the node's display name
  network_members = coalition_network,
  node_member_weights = node_member_weights,
  node_trade_tbl = node_trade_tbl,
  node_labels = network_node_labels,
  method = "auto",        # MILP per stage when installed and the stage is small enough, else greedy
  min_producers = 3,
  max_share = 0.40,
  min_share = 0.05,
  min_suppliers_per_consumer = 2,
  epsilon_supplier_share = 0.10,
  allow_self = TRUE,
  w_node = 1.0,
  w_edge = 0.5,
  w_dev = 0.0,
  progress_callback = progress_callback,
  auto_milp_max_nodes = auto_milp_max_nodes,
  milp_stage_time_limit_sec = network_milp_time_limit_sec,
  node_policy = network_node_policy,
  country_size_tbl = country_size_tbl,
  demand_mode = "mixed",
  demand_weights = list(need = 0.5, size = 0.5),
  portfolio_enable = TRUE,
  portfolio_top_k = 5,
  portfolio_min_cap = 2,
  portfolio_max_cap = 10,
  portfolio_transform = "log",
  portfolio_max_iters = network_portfolio_max_iters,
  # missing-data rules and edge orientation (the function defaults, stated for the record)
  policy_missing = "coalition_median",
  missing_components = "renormalize",
  friendshore_orientation = "reporter_is_consumer",
  supply_chains = network_supply_chains,
  diversity_lambda = network_diversity_lambda,
  diversity_breakpoint_step = network_diversity_breakpoint_step
)

diag_tbl <- res$diagnostics
message(
  "Allied network: ", length(res$params$network_nodes), " nodes, ",
  nrow(diag_tbl), " stages (", paste(network_supply_chains, collapse = ", "), "); method per stage: ",
  paste(names(table(diag_tbl$method)), table(diag_tbl$method), sep = " = ", collapse = ", "),
  "; ", sum(!is.na(diag_tbl$fallback_reason)), " MILP fallbacks to greedy",
  "; diversity_lambda = ", network_diversity_lambda,
  sprintf("; median HHI %.3f (%.1f effective producers)", stats::median(diag_tbl$hhi, na.rm = TRUE), stats::median(diag_tbl$effective_producers, na.rm = TRUE)),
  "; node policy term ", if (network_node_policy) {
    paste0("on (policy-imputed members selected in ", sum(dplyr::coalesce(diag_tbl$n_selected_policy_imputed, 0L) > 0), " stages)")
  } else {
    "off (producer score = EO and ES)"
  },
  if (!is.null(res$portfolio)) paste0("; portfolio converged: ", res$portfolio$converged, " after ", res$portfolio$iters, " iterations") else ""
)

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(repo_root, config$processed_dir, "outputs")
}
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

utils::write.csv(res$specialization, file.path(outputs_dir, "allied_network_specialization.csv"), row.names = FALSE)
utils::write.csv(res$flows,          file.path(outputs_dir, "allied_network_flows.csv"),         row.names = FALSE)
utils::write.csv(res$diagnostics,    file.path(outputs_dir, "allied_network_diagnostics.csv"),   row.names = FALSE)
utils::write.csv(res$input_checks,   file.path(outputs_dir, "allied_network_input_checks.csv"),  row.names = FALSE)
# The countries the design read (coalition_members(): iso3, country, tier, network_node, node_tier,
# source; node_only = TRUE for the countries of a bundled node outside the coalition's tier rules)
utils::write.csv(coalition_network, file.path(outputs_dir, "allied_network_members.csv"), row.names = FALSE)
# Each node's production share split back to its members by stage weight; for a bundled node
# (share_is_derived = TRUE) production_share_derived is derived, not designed
if (!is.null(res$member_shares)) {
  utils::write.csv(res$member_shares, file.path(outputs_dir, "allied_network_member_shares.csv"), row.names = FALSE)
}
if (!is.null(res$node_aggregation)) {
  utils::write.csv(res$node_aggregation$nodes, file.path(outputs_dir, "allied_network_nodes.csv"), row.names = FALSE)
  utils::write.csv(res$node_aggregation$edges, file.path(outputs_dir, "allied_network_node_edges.csv"), row.names = FALSE)
}
# Observed export concentration of the network's nodes in the stages solved (a bundled node counts
# its exports outside the node only): the reference for the diversification calibration above
if (!is.null(trade_cells)) {
  observed_hhi <- allied_network_observed_export_hhi(
    trade_cells,
    res$params$network_members,
    stages = dplyr::distinct(diag_tbl, tech, supply_chain)
  ) %>%
    dplyr::mutate(year = network_trade_year)
  utils::write.csv(observed_hhi, file.path(outputs_dir, "allied_network_observed_hhi.csv"), row.names = FALSE)
  message(sprintf(
    "Allied network: observed node export HHI, median over %d basket stages %.3f (%.1f equivalent exporters); designed median %.3f.",
    nrow(observed_hhi), stats::median(observed_hhi$hhi), 1 / stats::median(observed_hhi$hhi), stats::median(diag_tbl$hhi, na.rm = TRUE)
  ))
  # The calibration rule (see network_diversity_lambda) must hold on these inputs
  if (stats::median(diag_tbl$hhi, na.rm = TRUE) > stats::median(observed_hhi$hhi)) {
    warning(sprintf(
      paste0(
        "Allied network: the designed median HHI %.5f exceeds the observed median %.5f, so diversity_lambda = %s ",
        "no longer meets its calibration rule on these inputs; recalibrate network_diversity_lambda."
      ),
      stats::median(diag_tbl$hhi, na.rm = TRUE), stats::median(observed_hhi$hhi), format(network_diversity_lambda)
    ), call. = FALSE)
  }
}
if (!is.null(res$build_candidates)) {
  utils::write.csv(res$build_candidates, file.path(outputs_dir, "allied_network_build_candidates.csv"), row.names = FALSE)
}

if (!is.null(res$portfolio)) {
  utils::write.csv(res$portfolio$caps, file.path(outputs_dir, "allied_network_portfolio_caps.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$counts, file.path(outputs_dir, "allied_network_portfolio_counts.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$topk, file.path(outputs_dir, "allied_network_topk_by_stage.csv"), row.names = FALSE)
}

message("Wrote allied network outputs to: ", outputs_dir)
