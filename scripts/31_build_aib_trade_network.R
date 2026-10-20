# Allied Industrial Base (AIB), step 1: the observed sub-sector trade network and every input the
# partnership models read (docs/aib_methodology.md; config/aib.yml; pure code in R/aib/).
#
# Usage:
#   Rscript scripts/31_build_aib_trade_network.R [--raw-dir DIR] [--processed-dir DIR] [--out-dir DIR]
#     [--taxonomy dictionary|master] [--dictionary-scope core|extended] [--dyads FILE]
#
#   --raw-dir           raw inputs (default: config raw_data_dir)
#   --processed-dir     processed tables of the OSI pipeline (default: config processed_dir); read only
#   --out-dir           where the AIB writes (default: <outputs_dir>/aib)
#   --taxonomy          the HS6 taxonomy of the nodes (default: config/aib.yml taxonomy; see
#                       config/aib_taxonomy_<taxonomy>.yml)
#   --dictionary-scope  core (headline) or extended rows of the electro-industrial dictionary
#   --dyads             the bilateral HS6 trade file (default: <raw>/comtrade_energy_dyads_all.csv); it
#                       must hold every code of the taxonomy (the dictionary's new codes were pulled
#                       2026-10-19 into a combined file, see docs/electro_industrial_dictionary/README.md)
#
# Reads:
#   data/reference/electro_industrial_hs6_dictionary.csv  EIS index > sector > stage > HS6 (taxonomy
#                                                 dictionary, the default)
#   data/reference/energy_hs6_master.csv          Technology > stage > sub-sector > HS6 (taxonomy master;
#                                                 with the dictionary, the OSI crosswalk of the scores)
#   <raw>/comtrade_energy_dyads_all.csv            all-reporter bilateral HS6 trade (scripts/98)
#   <raw>/hs92_country_product_year_6.csv          Atlas HS92 exports by country and HS6 line: the benchmark
#                                                 concentration of the diversification scenario
#   <raw>/comtrade_total_export.csv                total goods exports (RCA denominators)
#   <raw>/comtrade_bulk_annual_totals.csv          earlier-year totals for non-reporters
#   <raw>/wdi_country_info.csv, <raw>/CAT_country ratings data.csv
#   <raw>/GCIM_Investment_Capacity_aggregated.xlsx operational manufacturing capacity (optional)
#   <processed>/partner_opportunity_tbl.rds, partner_friendshore_tbl.rds  (Export Opportunity,
#     Friendshore), policy_outputs.rds (IEA PAMS), major_supplier_shares_tbl.rds, coalition_tbl.rds
# Writes <out>/aib_inputs.rds: nodes, flows, metrics, data quality, dyad indices, policy inputs.

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

for (f in c("utils/scurve.R", "utils/schema.R", "utils/country.R", "utils/hs6_crosswalk.R",
            "themes/partnership_strength/partnership_strength_helpers.R",
            "categories/trade/trade_core.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)

raw_data_path <- abs_path(arg_value("--raw-dir", config$raw_data_dir))
processed_dir <- abs_path(arg_value("--processed-dir", config$processed_dir))
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(repo_root, config$processed_dir, "outputs")
out_dir <- abs_path(arg_value("--out-dir", file.path(outputs_dir, "aib")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(repo_root, "scripts", "utils", "aib_config.R"))
cfg <- aib_read_config(repo_root, arg_value("--taxonomy"))
step("AIB inputs: raw ", raw_data_path, "; processed ", processed_dir, "; out ", out_dir, "; taxonomy ", cfg$taxonomy)

# ---- Nodes ------------------------------------------------------------------------------------
# The OSI basket is read in every case: it is the master taxonomy, and with the dictionary it is the
# crosswalk through which the partner indices and the policy compatibility (kept by OSI technology x
# stage) reach the nodes.
osi_master <- utils::read.csv(file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv"),
                              colClasses = c(hs6 = "character"), stringsAsFactors = FALSE)
dictionary_scope <- NA_character_
if (identical(aib_or(cfg$nodes$source, "master"), "dictionary")) {
  dict_file <- aib_or(cfg$nodes$dictionary$file, "electro_industrial_hs6_dictionary.csv")
  dict <- utils::read.csv(file.path(raw_inputs_reference_dir(repo_root), dict_file),
                          colClasses = c(hs6 = "character", hs1992_join = "character"), stringsAsFactors = FALSE)
  dictionary_scope <- arg_value("--dictionary-scope", aib_or(cfg$nodes$dictionary$scope, "core"))
  master <- aib_dictionary_master(dict, dictionary_scope, primary_only = !identical(cfg$nodes$dictionary$primary_only, FALSE))
  taxonomy_tbl <- master %>% dplyr::distinct(tech, index_code, index_name) %>% dplyr::arrange(index_code, tech)
  step("Dictionary ", dict_file, ": ", dplyr::n_distinct(master$hs6), " codes (", dictionary_scope, ", primary rows) in ",
       dplyr::n_distinct(master$tech), " sectors of ", dplyr::n_distinct(master$index_code), " indices")
} else {
  master <- osi_master
  taxonomy_tbl <- tibble::tibble(tech = sort(unique(stringr::str_squish(master$tech))), index_code = NA_character_, index_name = NA_character_)
}
cells <- aib_master_cells(master, cfg$nodes$splits)
nd <- aib_build_nodes(cells)
step("Nodes: ", nrow(nd$nodes), " physical nodes from ", nrow(nd$node_cells), " technology/stage/sub-sector cells (",
     sum(nd$nodes$n_techs > 1), " shared by several technologies)")

# ---- Bilateral flows at node grain ---------------------------------------------------------------
trade_year <- as.integer(cfg$trade$year)
hist_year <- as.integer(cfg$trade$history_year)
dyad_path <- abs_path(arg_value("--dyads", file.path(raw_data_path, "comtrade_energy_dyads_all.csv")))
if (!file.exists(dyad_path)) stop(dyad_path, " not found (scripts/98; --dyads).")
trade_cols <- c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value")
dyads <- data.table::fread(
  dyad_path, select = trade_cols,
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")),
  na.strings = c("", "NA"), showProgress = FALSE
)
dyads <- dyads[dyads$period %in% c(hist_year, trade_year), ]
dyads$cmd_code <- hs6_clean_code(dyads$cmd_code)
dyads <- dyads[dyads$cmd_code %in% nd$node_codes$hs6, ]
step("Dyads: ", format(nrow(dyads), big.mark = ","), " rows in ", hist_year, " and ", trade_year, " on basket codes")
missing_codes <- setdiff(nd$node_codes$hs6, unique(dyads$cmd_code))
if (length(missing_codes)) {
  warning(length(missing_codes), " taxonomy codes have no trade in ", basename(dyad_path), ": ", paste(utils::head(missing_codes, 20), collapse = ", "),
          if (length(missing_codes) > 20) " ..." else "", call. = FALSE)
}
# World imports per code in the reference year: the weights of the OSI crosswalk.
code_trade <- dyads[dyads$period == trade_year & tolower(dyads$flow_direction) == "import", ]
code_trade <- code_trade[, list(value = sum(primary_value, na.rm = TRUE)), by = list(hs6 = cmd_code)]

build_flows <- function(node_ids) {
  xw <- aib_node_crosswalk(nd$node_codes, node_ids)
  sub <- as.data.frame(dyads[dyads$cmd_code %in% xw$hs6, ])
  if (!nrow(sub)) return(NULL)
  tc <- partnership_strength_build_trade_cells(sub, xw, years = c(hist_year, trade_year),
                                               cif_fob_factor = as.numeric(cfg$trade$cif_fob_factor))
  aib_node_flows(tc, min_flow_usd = as.numeric(cfg$trade$min_flow_usd))
}
batches <- split(nd$nodes$node_id, ceiling(seq_along(nd$nodes$node_id) / 8))
flows <- dplyr::bind_rows(lapply(seq_along(batches), function(b) {
  out <- build_flows(batches[[b]])
  invisible(gc())
  out
}))
rm(dyads)
invisible(gc())
step("Flows: ", format(nrow(flows), big.mark = ","), " exporter-importer-node rows; ",
     format(round(sum(flows$value) / 1e9), big.mark = ","), "bn USD in ", trade_year)

# ---- Totals, metrics ----------------------------------------------------------------------------
read_totals <- function(path) {
  if (!file.exists(path)) return(NULL)
  hdr <- names(data.table::fread(path, nrows = 0))
  as.data.frame(data.table::fread(path, select = intersect(c("reporter_iso", "period", "ref_year", "flow_code", "partner_iso", "cmd_code", "primary_value"), hdr),
                                  colClasses = list(character = intersect(c("reporter_iso", "flow_code", "partner_iso", "cmd_code"), hdr)), showProgress = FALSE))
}
totals <- aib_total_exports(
  read_totals(file.path(raw_data_path, "comtrade_total_export.csv")),
  read_totals(file.path(raw_data_path, "comtrade_bulk", "comtrade_bulk_annual_totals.csv")),
  year = trade_year
)
step("Total exports: ", nrow(totals), " economies (", sum(totals$total_year < trade_year), " from an earlier year)")

metrics_for <- function(fl) {
  ex <- aib_exporter_metrics(fl, totals, year = trade_year)
  list(
    exporter = ex,
    importer = aib_importer_metrics(fl),
    summary = aib_node_summary(fl, ex, material_flow_usd = as.numeric(cfg$data_quality$material_flow_usd))
  )
}
mt <- metrics_for(flows)

# ---- Data quality and the upward aggregation of insufficient nodes -----------------------------------
quality <- aib_data_quality(nd$nodes, nd$node_codes, mt$summary, cfg$data_quality, history_year = hist_year)
edges0 <- aib_supply_chain_edges(nd$node_cells, cfg$material_links)
cells_detail <- nd$node_cells
merge_map <- if (isTRUE(cfg$data_quality$aggregate_insufficient)) aib_quality_merge_map(quality, nd$nodes, nd$node_cells, edges0) else NULL
if (!is.null(merge_map) && nrow(merge_map)) {
  step("Data quality: aggregating upward ", sum(merge_map$node_id != merge_map$merged_id), " insufficient sub-sectors into ",
       dplyr::n_distinct(merge_map$merged_id), " broader nodes: ", paste(unique(merge_map$merged_label), collapse = " | "))
  members <- merge_map %>% dplyr::inner_join(nd$nodes, by = "node_id")
  merged_nodes <- members %>%
    dplyr::group_by(merged_id) %>%
    dplyr::summarise(
      sub_sector = dplyr::first(merged_label),
      supply_chain = names(sort(table(supply_chain), decreasing = TRUE))[1],
      techs = paste(sort(unique(unlist(strsplit(techs, "; ")))), collapse = "; "),
      primary_tech = if (any(node_id == merged_id)) primary_tech[node_id == merged_id][1] else dplyr::first(primary_tech),
      hs6 = paste(sort(unique(unlist(strsplit(hs6, ",")))), collapse = ","),
      stage_conflict = any(stage_conflict), .groups = "drop"
    ) %>%
    dplyr::rename(node_id = merged_id) %>%
    dplyr::mutate(n_techs = lengths(strsplit(techs, "; ")), n_hs6 = lengths(strsplit(hs6, ",")))
  member_cells <- nd$node_cells %>% dplyr::inner_join(merge_map, by = "node_id")
  nd$nodes <- dplyr::bind_rows(nd$nodes[!nd$nodes$node_id %in% merge_map$node_id, ], merged_nodes)
  nd$node_cells <- dplyr::bind_rows(
    nd$node_cells[!nd$node_cells$node_id %in% merge_map$node_id, ],
    member_cells %>% dplyr::distinct(node_id = merged_id, tech, supply_chain, sub_sector = merged_label)
  )
  nd$node_codes <- nd$node_codes %>%
    dplyr::left_join(merge_map[, c("node_id", "merged_id")], by = "node_id") %>%
    dplyr::mutate(node_id = dplyr::coalesce(merged_id, node_id)) %>%
    dplyr::group_by(node_id, hs6) %>%
    dplyr::summarise(essential = any(essential), legacy_hs = any(legacy_hs), note = dplyr::first(note), .groups = "drop")
  nd$node_codes <- nd$node_codes %>% dplyr::select(-dplyr::any_of("n_nodes_sharing")) %>%
    dplyr::left_join(dplyr::count(nd$node_codes, hs6, name = "n_nodes_sharing"), by = "hs6")
  flows <- aib_merge_node_flows(flows, merge_map)
  mt <- metrics_for(flows)
  quality_merged <- aib_data_quality(nd$nodes, nd$node_codes, mt$summary, cfg$data_quality, history_year = hist_year)
  quality <- dplyr::bind_rows(
    quality %>% dplyr::inner_join(merge_map, by = "node_id") %>% dplyr::filter(node_id != merged_id) %>%
      dplyr::mutate(aggregated_into = merged_id) %>% dplyr::select(-merged_id, -merged_label),
    quality_merged
  )
}
if (!is.null(merge_map) && nrow(merge_map)) {
  k <- match(cells_detail$node_id, merge_map$node_id)
  cells_detail$node_id[!is.na(k)] <- merge_map$merged_id[k[!is.na(k)]]
}
quality$optimize <- quality$data_quality != "Insufficient for optimization" & quality$node_id %in% nd$nodes$node_id

# Crosswalk to the OSI technology x stage cells (the partner indices and the policy compatibility are
# kept there). With the master taxonomy every node is its own cell.
if (identical(aib_or(cfg$nodes$source, "master"), "dictionary")) {
  osi_xw <- aib_osi_crosswalk(nd$node_codes, osi_master, as.data.frame(code_trade), min_share = aib_or(cfg$nodes$osi_crosswalk_min_share, 0.2))
  nd$nodes <- nd$nodes %>% dplyr::select(-dplyr::any_of(c("score_techs", "score_stage", "osi_cells", "yf_cells", "osi_subcomponents", "osi_coverage"))) %>%
    dplyr::left_join(osi_xw, by = "node_id")
  step("OSI crosswalk: ", sum(!is.na(nd$nodes$score_techs)), " of ", nrow(nd$nodes), " nodes have basket codes (median coverage ",
       round(100 * stats::median(nd$nodes$osi_coverage, na.rm = TRUE)), "% of their trade); the rest get neutral partner scores")
} else {
  nd$nodes <- nd$nodes %>% dplyr::mutate(score_techs = techs, score_stage = supply_chain,
                                         osi_cells = NA_character_, yf_cells = NA_character_, osi_subcomponents = NA_character_,
                                         osi_coverage = 1)
}
step("Data quality: ", paste(names(table(quality$data_quality)), table(quality$data_quality), sep = " ", collapse = "; "),
     "; ", sum(quality$optimize), " nodes enter the optimization")

# ---- Benchmark concentration (the diversification scenario) -------------------------------------
# The concentration of each node's higher-level sector: the trade-weighted mean exporter HHI of the
# other HS6 lines of its parent HS4 heading(s) in the Atlas HS92 panel (latest year up to the trade
# year; electric cars against petrol, diesel and hybrid cars), and of the other nodes of its sector.
# A node's own lines in HS92 are the dictionary's hs1992_join codes (electric cars: 870390), else its codes.
benchmarks <- NULL
product_hhi <- NULL
atlas_path <- file.path(raw_data_path, "hs92_country_product_year_6.csv")
if (file.exists(atlas_path)) {
  headings <- unique(substr(nd$node_codes$hs6, 1, 4))
  atlas <- data.table::fread(atlas_path, select = c("country_iso3_code", "product_hs92_code", "year", "export_value"),
                             colClasses = list(character = c("country_iso3_code", "product_hs92_code")), showProgress = FALSE)
  atlas <- atlas[substr(atlas$product_hs92_code, 1, 4) %in% headings & atlas$year <= trade_year, ]
  bench_year <- max(atlas$year)
  atlas <- atlas[atlas$year == bench_year, ]
  product_hhi <- aib_product_hhi(tibble::tibble(hs6 = atlas$product_hs92_code, iso3 = atlas$country_iso3_code,
                                                value = as.numeric(atlas$export_value)))
  rm(atlas)
  invisible(gc())
  own_hs92 <- nd$node_codes %>% dplyr::distinct(node_id, hs6)
  if (identical(aib_or(cfg$nodes$source, "master"), "dictionary") && "hs1992_join" %in% names(dict)) {
    joins <- dict %>% dplyr::transmute(hs6 = hs6_clean_code(hs6), hs92 = hs6_clean_code(hs1992_join)) %>%
      dplyr::filter(!is.na(hs92), nzchar(hs92)) %>% dplyr::distinct()
    own_hs92 <- dplyr::bind_rows(own_hs92, own_hs92 %>% dplyr::inner_join(joins, by = "hs6") %>% dplyr::transmute(node_id, hs6 = hs92)) %>%
      dplyr::distinct()
  }
  node_hhi <- mt$summary %>% dplyr::select(node_id, hhi = export_hhi, world_trade) %>%
    dplyr::inner_join(nd$nodes %>% dplyr::select(node_id, sector = primary_tech), by = "node_id")
  benchmarks <- aib_benchmark_hhi(nd$node_codes, product_hhi, own_hs92, node_hhi) %>%
    dplyr::left_join(node_hhi %>% dplyr::select(node_id, node_export_hhi = hhi), by = "node_id") %>%
    dplyr::mutate(benchmark_year = bench_year)
  step("Benchmarks: ", sum(!is.na(benchmarks$benchmark_hs4)), " nodes with a parent-heading benchmark (Atlas HS92 ", bench_year,
       "), ", sum(!is.na(benchmarks$benchmark_sector)), " with a sector benchmark")
} else {
  warning("hs92_country_product_year_6.csv not found in ", raw_data_path, "; the diversification scenario has no benchmark.", call. = FALSE)
}

# ---- Production / capacity proxies --------------------------------------------------------------
mss_path <- file.path(processed_dir, "major_supplier_shares_tbl.rds")
mss <- if (file.exists(mss_path)) readRDS(mss_path) else NULL
gcim <- NULL
gcim_path <- file.path(raw_data_path, "GCIM_Investment_Capacity_aggregated.xlsx")
if (file.exists(gcim_path)) {
  gcim <- tryCatch({
    tmp <- file.path(tempdir(), "gcim.xlsx")
    file.copy(gcim_path, tmp, overwrite = TRUE)
    g <- readxl::read_excel(tmp, sheet = "mfg_ind_capacity", skip = 9)
    names(g) <- tolower(names(g))
    g <- g[grepl("current operational", tolower(g$category)), , drop = FALSE]
    tibble::tibble(
      iso3 = suppressWarnings(countrycode::countrycode(g$country, "country.name", "iso3c", warn = FALSE)),
      technology = as.character(g$technology), product = as.character(g$product),
      value = suppressWarnings(as.numeric(g$value))
    ) %>% dplyr::filter(!is.na(iso3), !is.na(value), value > 0)
  }, error = function(e) {
    warning("GCIM capacity not read (", conditionMessage(e), "); production proxies from IEA only.", call. = FALSE)
    NULL
  })
}
production <- aib_production_shares(cfg$production_proxies, cells_detail, mss, gcim)
mt$exporter <- mt$exporter %>%
  dplyr::left_join(dplyr::rename(production, exporter_iso = iso3), by = c("node_id", "exporter_iso"))
quality <- aib_production_flags(quality, production, mt$summary)
step("Production proxies: ", dplyr::n_distinct(production$node_id), " nodes, ", nrow(production), " country shares")

# ---- Partner indices (Export Opportunity, Friendshore) -----------------------------------------------
# The OSI technologies the scores are read for (the crosswalk's score_techs; with the master taxonomy
# the node technologies themselves).
techs_used <- unique(c(unlist(strsplit(stats::na.omit(nd$nodes$score_techs), "; ")),
                       if (identical(aib_or(cfg$nodes$source, "master"), "master")) unique(nd$node_cells$tech)))
read_index <- function(name, variable) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    warning(name, " not found; the ", variable, " tie-break is neutral.", call. = FALSE)
    return(NULL)
  }
  t <- readRDS(path)
  keep <- t$variable == variable & t$tech %in% techs_used & t$supply_chain %in% c("Upstream", "Midstream")
  out <- aib_dyad_index(t[keep, c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable", "value")], variable)
  rm(t)
  invisible(gc())
  out
}
op_index <- read_index("partner_opportunity_tbl", "Opportunity Index")
fs_index <- read_index("partner_friendshore_tbl", "Friendshore Index")
step("Export Opportunity: ", format(NROW(op_index), big.mark = ","), " dyad-cells; Friendshore: ", format(NROW(fs_index), big.mark = ","))

# ---- Revealed policy / climate compatibility ----------------------------------------------------
pams <- NULL
pol_path <- file.path(processed_dir, "policy_outputs.rds")
if (file.exists(pol_path)) {
  pol <- readRDS(pol_path)
  if (!is.null(pol$policy_agg)) pams <- aib_pams_intensity(pol$policy_agg)
  rm(pol)
}
country_info <- utils::read.csv(file.path(raw_data_path, "wdi_country_info.csv"), stringsAsFactors = FALSE)
cat_scores <- NULL
cat_path <- file.path(raw_data_path, "CAT_country ratings data.csv")
if (file.exists(cat_path)) {
  cat_raw <- utils::read.csv(cat_path, stringsAsFactors = FALSE, check.names = TRUE, fileEncoding = "UTF-8-BOM")
  cat_clean <- partnership_strength_clean_policy(cat_prepare_ratings(cat_raw, country_info), country_info)
  cat_scores <- tibble::tibble(iso3 = cat_clean$iso3c, cat_score = cat_clean$climate_policy_index) %>%
    dplyr::filter(!is.na(iso3), !is.na(cat_score)) %>%
    dplyr::distinct(iso3, .keep_all = TRUE)
}
step("Compatibility: PAMS ", dplyr::n_distinct(pams$iso3), " economies; CAT ", NROW(cat_scores), " economies")

coalition <- NULL
coal_path <- file.path(processed_dir, "coalition_tbl.rds")
if (file.exists(coal_path)) {
  coalition <- readRDS(coal_path) %>% dplyr::select(dplyr::any_of(c("iso3", "tier", "network_node")))
}

names_tbl <- tibble::tibble(iso3 = country_info$iso3c, country = country_info$country) %>%
  dplyr::bind_rows(tibble::tibble(iso3 = c("TWN", "XKX"), country = c("Taiwan", "Kosovo"))) %>%
  dplyr::distinct(iso3, .keep_all = TRUE)

inputs <- list(
  meta = list(
    built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), trade_year = trade_year, history_year = hist_year,
    dyad_file = basename(dyad_path), cif_fob_factor = as.numeric(cfg$trade$cif_fob_factor),
    n_dyad_flows = nrow(flows), taxonomy = cfg$taxonomy, dictionary_scope = dictionary_scope, config = cfg
  ),
  taxonomy = taxonomy_tbl, benchmarks = benchmarks, product_hhi = product_hhi,
  nodes = nd$nodes, node_cells = nd$node_cells, node_cells_detail = cells_detail, node_codes = nd$node_codes,
  flows = flows, exporter_metrics = mt$exporter, importer_metrics = mt$importer, node_summary = mt$summary,
  quality = quality, merge_map = merge_map, production = production, total_exports = totals,
  op_index = op_index, fs_index = fs_index, pams = pams, cat_scores = cat_scores,
  coalition = coalition, country_names = names_tbl,
  supply_chain_edges = aib_remap_edges(edges0, merge_map)
)
saveRDS(inputs, file.path(out_dir, "aib_inputs.rds"))
step("Wrote ", file.path(out_dir, "aib_inputs.rds"), " in ", round(as.numeric(difftime(Sys.time(), t_script, units = "mins")), 1), " min")
