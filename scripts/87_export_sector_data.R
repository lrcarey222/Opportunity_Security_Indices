# Export the data of Sector Insights, the descriptive module of the Allied Industrial Base Explorer
# (dashboard/; spec: scratch_dash/sector/spec_sector.md section 8).
#
#   Rscript scripts/87_export_sector_data.R [--out-dir <dir>] [--phase 1|2] [--skip-trade]
#                                           [--no-policy-text] [--no-series] [--synthetic <dir>]
#                                           [--check-only] [--cmfdi <csv>] [--no-fdi] [--no-dev]
#
# IO only: every rule lives in the pure helpers of R/dashboard/sector/ (sector_files()). It reads
# config/sector_dictionary.yml, data/processed (raw rows only; never an index, weight or contribution
# row, never the partner dyad tables or the partnership outputs RDS) and, through the pipeline's own
# functions, the few data/raw files the processed tables do not carry (the Comtrade dyads, the IEA
# Critical Minerals workbook, USGS MCS, the IEA EV and manufacturing extracts, the Red Book series,
# IEA PAMS, CAT, the EI long CSV, the staged EIA International Energy Statistics extract (the EIA
# years and comparability of the fossil production the pipeline took from EIA), IMF monthly prices,
# the CIM workbook and the CM-FDI projects, cm_fdi_projects.csv in data/raw, or the file given with
# --cmfdi; and, for Development & finance, the raw inputs of Development Potential and of Friendshore's
# outbound component: wb_wdi.csv, wb_doingbusiness.csv, oecd_crs_api.csv, imf_dip.csv (selected
# columns) and wdi_gdp.csv, parsed with the builders' own functions; partner_development_tbl is read
# only to check that the builder on these inputs reproduces its stored components). It writes
#
#   <out-dir>/manifest.json            schema "osi-sector", headlines, coverage glyph, validation,
#                                      sources, budget (served no-cache; written last)
#   <out-dir>/sector-<name>.<h8>.osb   gzipped OSB1 chunks (R/dashboard/dash_osb.R, lossless):
#                                      meta, facts, trade-<tech> x 10, policy, policy-text, series,
#                                      fdi (CM-FDI; --no-fdi leaves it and its measures out),
#                                      dev (CRS aid flows and IMF DIP positions; --no-dev leaves it and
#                                      the Development & finance raw measures out)
#   <processed_dir>/dashboard/sector_validation.csv   the full validation report (not deployed)
#
# <out-dir> defaults to <repo>/dashboard/public/data/sector. Every chunk is decoded again and must be
# identical() to what was encoded and pass the R mirror of the contract checks
# (sector_check_chunk()); every prov-P value must be identical() to data/processed. --check-only runs
# every guard and check without writing anything. --synthetic <dir> writes the small deterministic
# fixture (sector_synthetic_parts()) the dashboard tests use, without reading any data.
#
# Restricted sources (BNEF, BCG) are never built: there is no flag for them.

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
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
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
if (arg_flag("--include-restricted")) stop("--include-restricted is not supported: BNEF and BCG data never ship (user decision 2026-10-10).")
synthetic_dir <- arg_value("--synthetic")
check_only <- arg_flag("--check-only")
skip_trade <- arg_flag("--skip-trade")
policy_text <- !arg_flag("--no-policy-text")
with_series <- !arg_flag("--no-series")
with_fdi <- !arg_flag("--no-fdi")
with_dev <- !arg_flag("--no-dev")
cmfdi_arg <- arg_value("--cmfdi")
phase <- as.integer(arg_value("--phase", "1"))
if (!phase %in% 1:2) stop("--phase must be 1 or 2")
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
rel <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- paste0(normalizePath(repo_root, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, root)) substring(p, nchar(root) + 1) else basename(p)
}
sources_used <- character()
use_src <- function(path) {
  if (!file.exists(path)) stop("Missing input: ", path)
  sources_used <<- unique(c(sources_used, normalizePath(path, winslash = "/", mustWork = TRUE)))
  path
}
val_rows <- list()
add_check <- function(check, status, n = NA_integer_, detail = NA_character_) {
  val_rows[[length(val_rows) + 1]] <<- tibble::tibble(check = check, status = status, n = as.integer(n), detail = as.character(detail))
}
notes <- character()

# ---- Dictionary and the contract's measure catalogue -----------------------------------------------

dict_path <- use_src(file.path(repo_root, "config", "sector_dictionary.yml"))
contract_path <- file.path(repo_root, "dashboard", "src", "sector", "contract.ts")
catalogue <- NULL
if (file.exists(contract_path)) {
  ct <- readLines(contract_path, warn = FALSE, encoding = "UTF-8")
  rows <- regmatches(ct, regexec("\\{ id: '([a-z_.0-9]+)', phase: '([^']+)', level: '([^']+)', chunks: \\[([^]]*)\\], scope: '([a-z]+)' \\}", ct))
  rows <- rows[lengths(rows) == 6]
  catalogue <- tibble::tibble(id = vapply(rows, `[`, "", 2), chunks = vapply(rows, `[`, "", 5), scope = vapply(rows, `[`, "", 6))
  if (!nrow(catalogue)) stop("Could not read VAR_CATALOGUE from ", rel(contract_path))
}
step("Dictionary ", rel(dict_path))
dict_raw <- yaml::read_yaml(dict_path)
problems <- sector_dictionary_validate(dict_raw, var_ids = catalogue$id)
if (length(problems)) stop("Dictionary problems:\n  ", paste(problems, collapse = "\n  "))
# --no-fdi: the CM-FDI measures (contract chunks ['fdi']) leave this build with their chunk.
fdi_var_ids <- if (!is.null(catalogue)) catalogue$id[grepl("'fdi'", catalogue$chunks, fixed = TRUE)] else SECTOR_FDI_VARS
if (!with_fdi) dict_raw$vars <- Filter(function(v) !v$id %in% fdi_var_ids, dict_raw$vars)
# --no-dev: the Development & finance raw measures (SECTOR_DEV_VARS) leave this build with sector-dev.
if (!with_dev) dict_raw$vars <- Filter(function(v) !v$id %in% SECTOR_DEV_VARS, dict_raw$vars)
dict_ids <- vapply(dict_raw$vars, function(v) v$id, "")
if (!is.null(catalogue)) {
  build_ids <- catalogue$id[catalogue$scope == "build" & (with_fdi | !catalogue$id %in% fdi_var_ids) & (with_dev | !catalogue$id %in% SECTOR_DEV_VARS)]
  missing_build <- setdiff(build_ids, dict_ids)
  if (length(missing_build)) stop("Measures of this build without a dictionary entry: ", paste(missing_build, collapse = ", "))
  extra <- setdiff(dict_ids, build_ids)
  if (length(extra)) stop("Dictionary entries outside this build's scope (contract scope 'build'): ", paste(extra, collapse = ", "))
}
add_check("dictionary: valid, every measure of the build has an entry", "PASS", length(dict_ids))

# ---- Writing (both modes) ---------------------------------------------------------------------------

gzip_bytes <- function(bytes, path) {
  con <- gzfile(path, "wb", compression = 9)
  writeBin(bytes, con)
  close(con)
}

# Encode, gzip, decode and check every chunk; write them unless check_only. Returns the chunk table.
write_chunks <- function(chunks, out_dir, check_only) {
  tmp_dir <- file.path(tempdir(), "sector_chunks")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- list()
  meta_sizes <- NULL
  decoded <- list()
  var_ids <- if (!is.null(catalogue)) catalogue$id else NULL
  for (nm in names(chunks)) {
    bytes <- dash_osb_encode(chunks[[nm]], nm, "chunk")
    tmp <- file.path(tmp_dir, paste0(nm, ".gz"))
    gzip_bytes(bytes, tmp)
    md5 <- unname(tools::md5sum(tmp))
    file_name <- paste0(nm, ".", substr(md5, 1, 8), ".osb")
    con <- gzfile(tmp, "rb")
    back <- readBin(con, "raw", n = length(bytes) + 1L)
    close(con)
    if (!identical(back, bytes)) stop("Chunk ", nm, ": the gzip file does not read back the encoded bytes.")
    dec <- dash_osb_decode(back)
    if (!identical(dec$objects, dash_osb_normalize(chunks[[nm]]))) stop("Chunk ", nm, " does not round-trip identical().")
    if (nm == "sector-meta") meta_sizes <- list(geo = nrow(dec$objects$dims$geo), items = nrow(dec$objects$dims$items), vars = nrow(dec$objects$dict$vars))
    errs <- sector_check_chunk(nm, dec$objects, meta_sizes, var_ids)
    if (length(errs)) stop("Chunk ", nm, " breaks the contract:\n  ", paste(errs, collapse = "\n  "))
    if (nm %in% c("sector-meta", "sector-facts", "sector-series", "sector-fdi", "sector-dev") || startsWith(nm, "sector-trade-")) decoded[[nm]] <- dec$objects
    rows[[nm]] <- tibble::tibble(name = nm, file = file_name, bytes = length(bytes), bytes_gz = file.size(tmp), md5 = md5, tmp = tmp)
    step(sprintf("  %-26s %8.1f kB, %7.1f kB gz; round trip identical; contract checks pass", nm, length(bytes) / 1024, file.size(tmp) / 1024))
  }
  add_check("chunks: decode identical() to the encoded objects (NA patterns included)", "PASS", length(rows))
  add_check("chunks: R mirror of the contract checks (types, NA, indices, sort, keys, slices)", "PASS", length(rows))
  list(ci = dplyr::bind_rows(rows), decoded = decoded)
}

# post_decode(decoded): further checks on the decoded meta / facts / series (parity with
# data/processed, summary parity); they add validation rows and stop on a failure.
finish <- function(bundle, chunks, out_dir, created, git_commit, strategic_bundle_id, processed_mtime_max, pipeline_marker,
                   flags, validation_csv = NULL, post_decode = NULL) {
  wc <- write_chunks(chunks, out_dir, check_only)
  ci <- wc$ci
  if (!is.null(post_decode)) post_decode(wc$decoded)
  over <- sector_over_budget(ci)
  if (length(over)) stop("Over the per-chunk gzip budget: ", paste(over, collapse = ", "))
  sector_gz <- sum(ci$bytes_gz)
  strategic_manifest <- file.path(dirname(out_dir), "manifest.json")
  combined <- NULL
  if (is.null(synthetic_dir) && file.exists(strategic_manifest)) {
    sm <- jsonlite::fromJSON(strategic_manifest, simplifyVector = FALSE)
    if (!is.null(sm$budget$all_gz)) combined <- sector_gz + as.numeric(sm$budget$all_gz)
  }
  if (sector_gz > SECTOR_BUDGET_GZ$sector_total) stop(sprintf("Sector data %.2f MB gz exceeds the 9 MB budget.", sector_gz / 2^20))
  if (!is.null(combined) && combined > SECTOR_BUDGET_GZ$combined_total) stop(sprintf("Sector + Strategic %.2f MB gz exceeds 25 MB.", combined / 2^20))
  add_check("budget: per chunk and in total (sector <= 9 MB gz; with Strategic <= 25 MB gz)", "PASS", nrow(ci),
            sprintf("sector %.0f kB gz%s", sector_gz / 1024, if (is.null(combined)) "" else sprintf("; combined %.2f MB gz", combined / 2^20)))
  id_file <- tempfile(fileext = ".txt")
  writeBin(charToRaw(paste0(paste(sort(ci$file), collapse = "\n"), "\n")), id_file)
  bundle_id <- unname(tools::md5sum(id_file))
  unlink(id_file)
  validation <- dplyr::bind_rows(val_rows)
  if (any(validation$status == "FAIL")) stop("Validation FAIL:\n", paste(validation$check[validation$status == "FAIL"], collapse = "\n"))
  src <- unique(sources_used)
  sources <- tibble::tibble(path = vapply(src, rel, character(1), USE.NAMES = FALSE),
                            mtime = format(file.mtime(src), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                            bytes = as.numeric(file.size(src)), md5 = unname(tools::md5sum(src)))
  if (!is.null(synthetic_dir)) sources$mtime <- "synthetic"
  man <- sector_manifest(
    bundle_id = bundle_id, created = created, git_commit = git_commit, phase = phase,
    strategic_bundle_id = strategic_bundle_id, processed_mtime_max = processed_mtime_max, pipeline_marker = pipeline_marker,
    flags = flags, chunk_info = ci, headlines = bundle$headlines, glyph = bundle$glyph, notes = unique(notes),
    validation = validation[validation$status %in% c("PASS", "WARN"), , drop = FALSE], sources = sources,
    budget = list(sector_gz = sector_gz, combined_gz = combined, limit_sector_gz = SECTOR_BUDGET_GZ$sector_total,
                  limit_combined_gz = SECTOR_BUDGET_GZ$combined_total)
  )
  txt <- sector_manifest_json(man)
  if (!is.null(validation_csv)) {
    dir.create(dirname(validation_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(validation, validation_csv, row.names = FALSE)
    step("Validation report ", rel(validation_csv))
  }
  if (check_only) {
    step("--check-only: nothing written. ", nrow(ci), " chunks, ", sprintf("%.0f kB gz", sector_gz / 1024))
    return(invisible(list(chunk_info = ci, manifest = man, validation = validation)))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(ci))) {
    dest <- file.path(out_dir, ci$file[i])
    if (file.exists(dest)) file.remove(dest)
    if (!file.copy(ci$tmp[i], dest)) stop("Could not write ", dest)
  }
  tmp_man <- file.path(out_dir, "manifest.json.tmp")
  writeBin(charToRaw(enc2utf8(txt)), tmp_man)
  if (file.exists(file.path(out_dir, "manifest.json"))) file.remove(file.path(out_dir, "manifest.json"))
  if (!file.rename(tmp_man, file.path(out_dir, "manifest.json"))) stop("Could not write manifest.json")
  stale <- sector_stale_files(list.files(out_dir), ci$file)
  if (length(stale)) {
    file.remove(file.path(out_dir, stale))
    step("Removed ", length(stale), " stale chunk file(s)")
  }
  step("Wrote ", rel(out_dir), ": ", nrow(ci), " chunks + manifest.json, ", sprintf("%.0f kB gz", sector_gz / 1024))
  invisible(list(chunk_info = ci, manifest = man, validation = validation))
}

# ---- Synthetic fixture ------------------------------------------------------------------------------

if (!is.null(synthetic_dir)) {
  out_dir <- abs_path(synthetic_dir)
  step("Synthetic fixture -> ", rel(out_dir))
  tabs <- sector_dictionary_tables(dict_raw, fill = sector_synthetic_fill())
  unfilled <- sector_dictionary_unfilled(tabs)
  if (length(unfilled)) stop("Unfilled dictionary placeholders: ", paste(unfilled, collapse = ", "))
  parts <- sector_synthetic_parts(tabs)
  if (!with_fdi) {
    parts$fdi <- NULL
    parts$roster_extra <- parts$roster_extra[parts$roster_extra$roster != "cmfdi_home", , drop = FALSE]
  }
  if (!with_dev) {
    parts$dev <- NULL
    parts$facts <- parts$facts[!parts$facts$var %in% SECTOR_DEV_VARS, , drop = FALSE]
  }
  # Ship the measures the fixture has data for (a measure without data would have no roster).
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  bundle <- sector_assemble(parts, tabs, ship_vars = tabs$vars$id[tabs$vars$id %in% have])
  chunks <- sector_chunks(bundle)
  if (!policy_text) chunks$`sector-policy-text` <- NULL
  if (!with_series) chunks$`sector-series` <- NULL
  if (!is.null(parts$fdi)) {
    fp <- sector_fdi_problems(parts$fdi, parts$geo)
    if (length(fp)) stop("CM-FDI (synthetic): ", paste(fp, collapse = "; "))
  }
  syn_post <- function(decoded) {
    if (!is.null(decoded$`sector-dev`)) {
      pd <- sector_dev_parity(decoded$`sector-dev`, decoded$`sector-meta`, parts$dev)
      for (i in seq_len(nrow(pd))) add_check(pd$check[i], pd$status[i], pd$n[i], pd$detail[i])
      if (any(pd$status == "FAIL")) stop("Development & finance parity FAIL: ", paste(pd$check[pd$status == "FAIL"], collapse = "; "))
    }
    if (is.null(decoded$`sector-fdi`)) return(invisible())
    pr <- sector_fdi_parity(decoded$`sector-fdi`, decoded$`sector-meta`, sector_synthetic_cmfdi_raw(), sector_synthetic_iso3)
    for (i in seq_len(nrow(pr))) add_check(pr$check[i], pr$status[i], pr$n[i], pr$detail[i])
    if (any(pr$status == "FAIL")) stop("CM-FDI parity FAIL: ", paste(pr$check[pr$status == "FAIL"], collapse = "; "))
  }
  res <- finish(bundle, chunks, out_dir, created = "2026-10-10T00:00:00Z", git_commit = NA_character_, strategic_bundle_id = NA_character_,
                processed_mtime_max = "synthetic", pipeline_marker = NA_character_,
                flags = list(include_restricted = FALSE, trade = length(bundle$trade) > 0, atlas = FALSE,
                             policy_text = "sector-policy-text" %in% names(chunks), series = "sector-series" %in% names(chunks),
                             fdi = "sector-fdi" %in% names(chunks), dev = "sector-dev" %in% names(chunks)),
                post_decode = syn_post)
  message(sprintf("Synthetic fixture: %d chunks, %.1f kB gz. %.0f s.", nrow(res$chunk_info), sum(res$chunk_info$bytes_gz) / 1024,
                  as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
  quit(save = "no", status = 0)
}

# ---- Real export -------------------------------------------------------------------------------------

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
for (f in sector_pipeline_files()) source(file.path(repo_root, f))
processed_dir <- file.path(repo_root, config$processed_dir)
raw_dir <- file.path(repo_root, config$raw_data_dir)
ref_dir <- raw_inputs_reference_dir(repo_root)
out_dir <- abs_path(arg_value("--out-dir", file.path("dashboard", "public", "data", "sector")))
marker_path <- Sys.getenv("OPSI_PIPELINE_MARKER", file.path(dirname(repo_root), "scratch_final", "rerun", "THEMES_FINAL_OK"))
pipeline_marker <- if (file.exists(marker_path)) paste(trimws(readLines(marker_path, warn = FALSE)), collapse = " ") else NA_character_
processed_read <- character()
rd <- function(name) {
  p <- file.path(processed_dir, paste0(name, ".rds"))
  use_src(p)
  processed_read <<- c(processed_read, p)
  readRDS(p)
}
tmp_copy <- function(path) {
  # xlsx inputs are copied before reading: OneDrive and Excel lock files in data/raw.
  dest <- file.path(tempdir(), basename(path))
  if (!file.copy(use_src(path), dest, overwrite = TRUE)) stop("Could not copy ", path)
  dest
}
iso3_of <- function(names) {
  x <- trimws(as.character(names))
  out <- country_name_to_iso3(x)
  # Names double-encoded upstream ("Saint BarthÃ©lemy"): read the bytes back as UTF-8 and retry.
  miss <- is.na(out) & grepl("Ã", x)
  if (any(miss)) {
    fixed <- iconv(x[miss], from = "UTF-8", to = "latin1")
    Encoding(fixed) <- "UTF-8"
    out[miss] <- country_name_to_iso3(fixed)
  }
  out[tolower(x) %in% c("eu", "european union", "eu27", "eu-27")] <- "EUU"
  out
}
results <- list()
worlds <- list()

step("Processed tables (raw rows only) -> facts")
coal <- rd("coalition_tbl")
ideal <- rd("geo_ideal_points_tbl")
eu_members <- coal$iso3[coal$eu_member %in% TRUE]
tc <- rd("trade_concentration_tbl")
results$atlas <- sector_facts_atlas(tc)
worlds$atlas <- results$atlas$world
reported <- sector_reported_trade(tc)
rm(tc)
ef <- rd("export_feasibility_tbl")
results$rca <- sector_facts_rca(ef)
# Export feasibility (Atlas relatedness): its data year is the Atlas HS4 year the rows' explanation
# names in brackets, not the table's Year column (the Comtrade year); it must be the dictionary's.
feas_year <- sector_feasibility_year(ef)
feas_dict_year <- as.integer(unlist(Filter(function(v) identical(v$id, "trade.feasibility"), dict_raw$vars)[[1]]$years))
if (!identical(feas_year, feas_dict_year)) {
  stop("export_feasibility_tbl feasibility is Atlas ", feas_year, " data; the dictionary entry trade.feasibility says ",
       paste(feas_dict_year, collapse = ", "), ": update its years and data_year.")
}
results$feasibility <- sector_facts_feasibility(ef, feas_year)
add_check(paste0("export feasibility: Atlas HS4 data year ", feas_year, " (explanation) = the dictionary's; every basket cell covered"),
          if (length(unique(results$feasibility$facts$item_key)) >= 18) "PASS" else "WARN", nrow(results$feasibility$facts),
          sprintf("%d cells, %d economies", length(unique(results$feasibility$facts$item_key)), length(unique(results$feasibility$facts$geo))))
ef_raw <- sector_raw_rows(ef)
n_rca_reporters <- length(unique(ef_raw$iso3c[ef_raw$variable == "exports"]))
rm(ef, ef_raw)
fl <- rd("critical_minerals_trade_flows_tbl")
results$critmin <- sector_facts_critmin(fl)
worlds$critmin <- results$critmin$world
critmin_year <- paste(sort(unique(fl$Year)), collapse = ", ")
n_critmin_reporters <- length(unique(fl$iso3c[fl$export_source == "reported"]))
if (n_critmin_reporters < 100) stop("Critical-minerals trade: only ", n_critmin_reporters, " reporting exporters (< 100): a stub vintage?")
add_check("guard: critical-mineral exports from at least 100 reporting economies", "PASS", n_critmin_reporters)
add_check("parity X: critical-mineral export shares = exports / world (processed market_share)",
          if (results$critmin$check$max_rel <= 1e-12) "PASS" else "WARN", nrow(fl), sprintf("max relative difference %.3g", results$critmin$check$max_rel))
rm(fl)

usgs_path <- file.path(raw_dir, "MCS2026_World_Data.csv")
mcs <- readr::read_csv(use_src(usgs_path), col_types = readr::cols(.default = readr::col_character()),
                       locale = readr::locale(encoding = "windows-1252"), na = character(), progress = FALSE)
usgs_res <- usgs_mcs_reserves(mcs)
usgs_unq <- usgs_mcs_unquantified(usgs_res)
usgs_world <- usgs_mcs_world_reserves(usgs_res)
usgs_prod <- sector_usgs_production(mcs, usgs_mcs_parse_value, iso3_of, is_aggregate_or_residual, year = 2025L)
rm(mcs)
res_tbl <- rd("reserves_tbl")
results$reserves <- sector_facts_reserves(res_tbl, usgs_unq)
rm(res_tbl)
worlds$reserves <- tibble::tibble(var = "resources.mineral_reserves", item_key = paste0("mineral:", sector_mineral_slug(usgs_world$tech)),
                                  year = 2025L, value = usgs_world$world, basis = "source_world")
# X cross-check: the processed USGS reserves equal usgs_mcs_reserves() of the raw table.
ur <- usgs_res[usgs_res$row_type == "country" & !is.na(usgs_res$value), , drop = FALSE]
mr <- results$reserves$facts[results$reserves$facts$var == "resources.mineral_reserves" & !is.na(results$reserves$facts$value), , drop = FALSE]
uk <- paste0("mineral:", sector_mineral_slug(ur$tech), " ", ur$ISO3166_alpha3)
mk <- paste(mr$item_key, mr$geo)
same <- identical(sort(uk), sort(mk)) && identical(ur$value[match(mk, uk)], mr$value)
add_check("parity X: USGS reserves in reserves_tbl = usgs_mcs_reserves(MCS2026_World_Data.csv)", if (same) "PASS" else "FAIL", nrow(mr))
rm(ur, mr)

results$potential <- sector_facts_potential(rd("solar_pv_potential_tbl"), rd("wind_potential_tbl"), rd("geothermal_potential_tbl"))

ei_path <- file.path(raw_dir, "ei_stat_review_world_energy.csv")
ei <- as.data.frame(data.table::fread(use_src(ei_path), select = c("Country", "Year", "ISO3166_alpha3", "Var", "Value"), showProgress = FALSE))
ei_latest <- max(ei$Year[ei$Var == "oilprod_kbd"], na.rm = TRUE)
ei_present <- unique(data.frame(ei_var = ei$Var, iso3 = ei$ISO3166_alpha3)[ei$Year == ei_latest & !is.na(ei$Value), ])
ei_values <- ei[ei$Year == ei_latest & !is.na(ei$Value) & grepl("^[A-Z]{3}$", ei$ISO3166_alpha3), c("Var", "ISO3166_alpha3", "Value")]
names(ei_values) <- c("ei_var", "iso3", "value")
# The fossil production EI does not report: the pipeline took it from the staged EIA International
# Energy Statistics extract (R/utils/eia_intl.R; scripts/10 reads the same file the same way). The
# lookup gives each such cell its EIA year and whether EIA's value failed the comparability check.
eia_path <- file.path(raw_dir, "eia_intl_fossil_production.csv")
if (!file.exists(eia_path)) {
  stop("Missing ", eia_path, ": stage the EIA International Energy Statistics extract (Rscript scripts/fetchers/eia_intl.R, ",
       "or copy eia_intl_fossil_production.csv from the SharePoint raw folder), as scripts/10 requires.")
}
eia_intl <- read.csv(use_src(eia_path), stringsAsFactors = FALSE, na.strings = "")
# As the themes call it (import_dependence() and production_depth_momentum(), eia_max_lag = 1L).
eia_max_lag <- 1L
pdm_tbl <- rd("production_depth_momentum_tbl")
idp_tbl <- rd("import_dependence_tbl")
eia_gap <- sector_eia_gap_cells(c(pdm_tbl$iso3c, idp_tbl$iso3c), ei_values, ei_latest)
eia_cells <- eia_intl_lookup(eia_gap, eia_intl, max_lag = eia_max_lag, ei_consumption = eia_gap$ei_consumption)
rm(eia_intl, eia_gap)
results$production <- sector_facts_production(pdm_tbl, ei_present, latest_year = as.integer(ei_latest), eia_cells = eia_cells)
eia_chk <- sector_eia_check(results$production, eia_cells)
add_check("parity X: fossil production the pipeline took from EIA = eia_intl_lookup() of the staged extract (1e-12 relative)",
          if (eia_chk$n_missing == 0 && eia_chk$max_rel <= 1e-12) "PASS" else "FAIL", eia_chk$n,
          sprintf("%d facts; %d without an EIA value; max relative difference %.3g", eia_chk$n, eia_chk$n_missing, eia_chk$max_rel))
if (eia_chk$n_missing > 0 || eia_chk$max_rel > 1e-12) {
  stop("EIA-filled production differs from eia_intl_lookup() of ", rel(eia_path), ": stage the extract scripts/10 read, or rerun scripts/10.")
}
# The caveat ei_oil_shale describes Estonia coal; any other fuel whose EIA value is not comparable needs its own text.
nc_other <- setdiff(eia_chk$not_comparable, "EST Coal")
add_check("EIA values not comparable with EI's fuel are Estonia coal only (caveat ei_oil_shale)", if (length(nc_other)) "WARN" else "PASS",
          length(eia_chk$not_comparable), if (length(eia_chk$not_comparable)) paste(eia_chk$not_comparable, collapse = ", ") else NA_character_)
if (length(nc_other)) notes <- c(notes, paste("EIA production not comparable with EI's fuel beyond Estonia coal (review caveat ei_oil_shale):", paste(nc_other, collapse = ", ")))
mss <- rd("major_supplier_shares_tbl")
results$mining_ei <- sector_facts_mining_ei(rd("critical_minerals_production_tbl"), mss, year = 2025L)
worlds$mining_ei <- results$mining_ei$world
results$consumption <- sector_facts_consumption(rd("energy_access_tbl"), rd("energy_consumption_tbl"), ei_present, year = as.integer(ei_latest))
results$balance <- sector_facts_balance(idp_tbl, ei_present, year = as.integer(ei_latest), eia_cells = eia_cells,
                                       consumption = sector_ei_consumption(ei_values))
rm(pdm_tbl, idp_tbl)
fn_all <- dplyr::bind_rows(results$production$notes, results$balance$notes)
fn_count <- function(v, note) sum(fn_all$var == v & fn_all$note == note)
add_check("EI production gaps: from EIA (obs / zero, note eia_production) or assumed 0 (nsr, note production_assumed_zero; user rule 2026-10-11)",
          "PASS", nrow(fn_all),
          sprintf("production.fuels from EIA %d; balances with EIA production %d, with EIA consumption %d; assumed zero: balances %d, self-sufficiencies %d; fossil with a fuel counted as 0: %d",
                  fn_count("production.fuels", "eia_production"), fn_count("consumption.balance", "eia_production"),
                  fn_count("consumption.balance", "eia_consumption"), fn_count("consumption.balance", "production_assumed_zero"),
                  fn_count("consumption.self_sufficiency", "production_assumed_zero"), fn_count("consumption.balance", "fossil_assumed_zero")))
rm(fn_all)
results$mfg <- sector_facts_mfg(rd("foreign_dependency_tbl"), mss, eu_members)
worlds$mfg <- results$mfg$world
results$ev <- sector_facts_ev_regions(rd("market_share_manufacturing_tbl"))
results$investment <- sector_facts_investment(rd("investment_momentum_tbl"))
results$future <- sector_facts_future(rd("future_demand_tbl"))
ep <- rd("energy_prices_tbl")
ep_raw <- sector_raw_rows(ep)
commodities <- sector_commodity_items(ep_raw$sub_sector)
price_techs <- unique(ep_raw[c("sub_sector", "tech")])
results$prices <- sector_facts_prices(ep, commodities)
rm(ep, ep_raw)
results$costs <- sector_facts_costs(rd("cost_competitiveness_tbl"))
results$trl <- sector_facts_trl(rd("technological_readiness_tbl"))
results$alignment <- sector_facts_alignment(ideal)
invisible(gc())

# EI world and EU rows of the processed cross-sections (X), from the EI long CSV.
ei_map <- sector_ei_var_map()
ei_cs <- ei[ei$Year == ei_latest & ei$Var %in% ei_map$ei_var & ei$ISO3166_alpha3 %in% c("WLD", "T-EU") & !is.na(ei$Value), , drop = FALSE]
ei_cs$var <- ei_map$var[match(ei_cs$Var, ei_map$ei_var)]
ei_cs$item_key <- ei_map$item_key[match(ei_cs$Var, ei_map$ei_var)]
ei_cs <- ei_cs[ei_cs$var %in% c("production.fuels", "production.generation") & ei_cs$item_key %in% unique(results$production$facts$item_key), , drop = FALSE]
w_ei <- ei_cs[ei_cs$ISO3166_alpha3 == "WLD", , drop = FALSE]
worlds$ei <- tibble::tibble(var = w_ei$var, item_key = w_ei$item_key, year = as.integer(ei_latest), value = w_ei$Value, basis = "source_world")
eu_ei <- ei_cs[ei_cs$ISO3166_alpha3 == "T-EU", , drop = FALSE]
results$ei_eu <- sector_result(tibble::tibble(var = eu_ei$var, item_key = eu_ei$item_key, geo = "EUU", year = as.integer(ei_latest),
                                              value = eu_ei$Value, status = ifelse(eu_ei$Value == 0, "zero", "obs"), obs_year = NA_integer_))

# ---- Raw cross-sections ----------------------------------------------------------------------------

step("IEA Critical Minerals, USGS production, Red Book, CAT")
# The pipeline's own pattern (scripts/10, 13, 40): the newest vintage resolves as it does there, and
# scripts/01 registers a single pattern for this input.
cm_path <- resolve_versioned_raw_input(raw_dir, pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
                                       fallback = "IEA Critical Minerals Dataset 2026.xlsx", label = "IEA Critical Minerals Dataset")
critical <- read_iea_critical_minerals(tmp_copy(cm_path), quiet = TRUE)
mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)
headline_techs <- unname(SECTOR_TECH_LABELS[SECTOR_TECH_SLUGS != "semiconductors"])
mineral_links <- coalition_mineral_links(mineral_demand_clean, headline_techs)
sup <- sector_iea_cm_supply(critical, iso3_of)
top3 <- sector_iea_cm_top3_check(sup)
if (!all(top3$ok)) stop("IEA Critical Minerals: recomputed top-3 shares differ from IEA's by more than 0.001: ",
                        paste(top3$key[!top3$ok], collapse = "; "))
add_check("guard: IEA CM top-3 share recomputed = IEA 'Top 3 share' (0.001) for every block and year", "PASS", nrow(top3),
          sprintf("max difference %.4f", max(top3$diff)))
# World = the block's country rows plus Rest of world (as major_supplier_shares and the processing
# theme divide); IEA's own Total row is checked against it.
listed <- stats::aggregate(list(v = sup$facts$value), list(var = sup$facts$var, item_key = sup$facts$item_key, year = sup$facts$year), sum)
rest <- sup$residual
listed$rest <- rest$value[match(paste(listed$var, listed$item_key, listed$year), paste(rest$var, rest$item_key, rest$year))]
listed$rest[is.na(listed$rest)] <- 0
w_iea <- tibble::tibble(var = listed$var, item_key = listed$item_key, year = as.integer(listed$year), value = listed$v + listed$rest, basis = "source_world")
tot <- sup$world$value[match(paste(w_iea$var, w_iea$item_key, w_iea$year), paste(sup$world$var, sup$world$item_key, sup$world$year))]
add_check("IEA CM: world = country rows + Rest of world, against IEA's rounded Total row", "PASS", nrow(w_iea),
          sprintf("max relative gap to the Total row %.4f", max(abs(w_iea$value - tot) / tot)))
worlds$iea <- w_iea
results$iea_cm <- sector_result(sup$facts, tibble::tibble(var = sup$facts$var, item_key = sup$facts$item_key, geo = sup$facts$geo,
                                                          year = sup$facts$year, prov = "X", table = "major_supplier_shares_tbl",
                                                          variable = "iea_mineral_mining / refining world_share", key = NA_character_,
                                                          source_value = sup$facts$value, formula_value = NA_real_))
# X cross-check: 2025 shares = major_supplier_shares_tbl world_share.
ms <- sector_supplier_dedupe(mss, c("iea_mineral_mining", "iea_mineral_refining"))
f25 <- sup$facts[sup$facts$year == 2025, , drop = FALSE]
W25 <- w_iea$value[match(paste(f25$var, f25$item_key, 2025L), paste(w_iea$var, w_iea$item_key, w_iea$year))]
mvar <- ifelse(f25$var == "production.mining", "iea_mineral_mining", "iea_mineral_refining")
ref <- ms$world_share[match(paste(mvar, sub("^mineral:", "", f25$item_key), f25$geo),
                            paste(ms$measure, sector_mineral_slug(ms$mineral), ms$iso3))]
rel_cm <- abs(f25$value / W25 - ref) / ref
add_check("parity X: IEA CM 2025 shares = major_supplier_shares_tbl world_share (1e-12 relative)",
          if (all(!is.na(rel_cm)) && max(rel_cm) <= 1e-12) "PASS" else "FAIL", length(rel_cm),
          sprintf("max relative difference %.3g; %d rows without a supplier-table match", max(rel_cm, na.rm = TRUE), sum(is.na(rel_cm))))
mineral_demand <- sector_iea_cm_demand(critical, sector_iea_demand_tech)
rm(critical, ms, f25, rel_cm)

results$usgs <- sector_result(usgs_prod$facts)
worlds$usgs <- usgs_prod$world

uprod_path <- file.path(raw_dir, "uranium_production.csv")
if (!file.exists(uprod_path)) uprod_path <- file.path(ref_dir, "uranium_production.csv")
uref <- read.csv(use_src(uprod_path), stringsAsFactors = FALSE)
u_latest <- max(uref$year[uref$variable == "mine_production"])
results$uranium_series <- sector_result(sector_uranium_series(uref, exclude_year = u_latest))
pu <- results$production$facts[results$production$facts$var == "production.uranium", , drop = FALSE]
ru <- uref[uref$variable == "mine_production" & uref$year == u_latest, , drop = FALSE]
ok_u <- identical(as.double(ru$value[match(pu$geo, ru$iso3c)][!is.na(match(pu$geo, ru$iso3c))]), pu$value[!is.na(match(pu$geo, ru$iso3c))])
add_check(paste0("parity X: uranium ", u_latest, " in production_depth_momentum_tbl = the Red Book reference table"), if (ok_u) "PASS" else "FAIL", nrow(ru))

cat_path <- file.path(raw_dir, "CAT_country ratings data.csv")
cat_raw <- readr::read_csv(use_src(cat_path), show_col_types = FALSE)
cat_tbl <- sector_cat(cat_raw, iso3_of(cat_raw[[1]]))

# ---- Trade matrix -----------------------------------------------------------------------------------

trade <- list()
eu_intra <- NULL
basket_cells <- character()
n_codes <- 0L
n_shared <- 0L
appendix_trade <- NULL
if (!skip_trade) {
  step("Comtrade dyads -> reconciled matrix")
  dyad_path <- file.path(raw_dir, "comtrade_energy_dyads_all.csv")
  hs6_raw <- read.csv(use_src(file.path(raw_dir, "energy_hs6_master.csv")), stringsAsFactors = FALSE)
  hs6_ref <- read.csv(use_src(file.path(ref_dir, "energy_hs6_master.csv")), stringsAsFactors = FALSE)
  basket <- sector_trade_basket(hs6_raw)
  bad <- sector_trade_excluded_in_basket(basket, hs6_ref)
  if (length(bad)) stop("Codes excluded in data/reference are still in the pipeline basket (rerun scripts/04): ", paste(bad, collapse = ", "))
  basket_cells <- unique(basket$cell)
  n_codes <- length(unique(basket$hs6))
  n_shared <- length(unique(basket$hs6[basket$shared_n > 0]))
  add_check("guard: HS6 basket distinct, one sub-sector per code and cell, no excluded code in the basket", "PASS", nrow(basket),
            sprintf("%d codes, %d in more than one cell (max %d cells)", n_codes, n_shared, max(basket$shared_n) + 1L))
  cols <- c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value")
  td <- data.table::fread(use_src(dyad_path), select = cols,
                          colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")),
                          na.strings = c("", "NA"), showProgress = FALSE)
  td <- as.data.frame(td[td$period %in% SECTOR_TRADE_YEARS, ])
  n_imp <- length(unique(td$reporter_iso[td$period == max(SECTOR_TRADE_YEARS) & tolower(td$flow_direction) == "import"]))
  if (n_imp < 100) stop("Comtrade dyads: only ", n_imp, " importer reporters in ", max(SECTOR_TRADE_YEARS), " (< 100): a stub vintage?")
  add_check(paste0("guard: Comtrade dyads have at least 100 importer reporters in ", max(SECTOR_TRADE_YEARS)), "PASS", n_imp)
  std <- partnership_strength_standardize_comtrade(td, years = SECTOR_TRADE_YEARS)
  reporters <- sector_trade_reporters(std, basket)
  declared <- sector_trade_declared(reporters)
  pcfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))
  cif <- if (is.null(pcfg$trade$cif_fob_factor)) PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR else as.numeric(pcfg$trade$cif_fob_factor)
  # Trade by sub-component (trade.cty_sub): the same dyads summed over each sub-component's codes with
  # the cell's source rule, checked below to add up to the cell totals.
  step("Trade by sub-component")
  sub <- sector_trade_sub(std, basket, declared, eu_members, cif_fob_factor = cif)
  rm(std)
  invisible(gc())
  trade_cells <- partnership_strength_build_trade_cells(td, hs6_raw, years = SECTOR_TRADE_YEARS, cif_fob_factor = cif)
  rm(td)
  invisible(gc())
  # The importer rule of the matrix agrees with the declared table.
  imp_rows <- unique(paste0("cell:", sector_cell_slug(trade_cells$tech, trade_cells$supply_chain), " ", trade_cells$importer_iso, " ",
                            ifelse(trade_cells$source_last %in% "importer", SECTOR_TRADE_YEARS[2], NA)))
  imp_rows <- imp_rows[!grepl(" NA$", imp_rows)]
  dk <- paste(declared$item_key, declared$geo, declared$year)[declared$imports_declared]
  add_check("trade: every importer-sourced 2024 cell of the matrix is a declared importer", if (all(imp_rows %in% dk)) "PASS" else "FAIL", length(imp_rows))
  zr <- sector_trade_zero_rule_check(trade_cells, declared)
  add_check("trade: the zero-rebuild rule (importer declared, else exporter declared: mirror) reproduces every matrix row",
            if (zr$bad == 0) "PASS" else "FAIL", zr$n, if (zr$bad) paste0(zr$bad, " rows break it, e.g. ", paste(zr$example, collapse = "; ")) else NA_character_)
  rm(zr)
  country <- sector_trade_country(trade_cells, declared)
  # Totals equal an independent sum of the pipeline function's output (identical doubles).
  ex24 <- trade_cells[!is.na(trade_cells$value_last), , drop = FALSE]
  chk <- tapply(ex24$value_last, paste0("cell:", sector_cell_slug(ex24$tech, ex24$supply_chain), " ", ex24$exporter_iso), sum)
  ce <- country[country$var == "trade.exports" & country$year == SECTOR_TRADE_YEARS[2], , drop = FALSE]
  add_check("parity: matrix country totals identical() to sums of partnership_strength_build_trade_cells()",
            if (identical(as.vector(chk[paste(ce$item_key, ce$geo)]), ce$value)) "PASS" else "FAIL", nrow(ce))
  rm(ex24, chk, ce)
  results$trade <- sector_result(country)
  worlds$trade <- sector_trade_world(country)
  cty_sub <- sector_trade_sub_table(sub, country)
  sc <- sector_trade_sub_check(cty_sub, country, worlds$trade)
  add_check("trade by sub-component: sub-components add up to the cell's exports and imports for every economy and year, and to the world total (1e-9 relative)",
            if (sc$max_rel <= 1e-9 && sc$n_unmatched == 0) "PASS" else "FAIL", sc$n,
            sprintf("max relative difference %.3g; %d sub-component totals without a cell fact%s", sc$max_rel, sc$n_unmatched,
                    if (length(sc$examples)) paste0("; e.g. ", paste(sc$examples, collapse = "; ")) else ""))
  if (sc$max_rel > 1e-9 || sc$n_unmatched > 0) stop("Trade by sub-component does not add up to the cell totals: ", paste(sc$examples, collapse = "; "))
  sub_intra <- sub$intra
  n_sub <- unique(cty_sub[c("item_key", "sub_sector")])
  rm(sub, sc)
  eu_intra <- sector_eu_intra(trade_cells, eu_members)
  flows <- sector_trade_flows(trade_cells)
  econ <- sort(unique(c(trade_cells$exporter_iso, trade_cells$importer_iso)))
  rm(trade_cells)
  invisible(gc())
  dd <- data.table::fread(dyad_path, select = c("cmd_code", "cmd_desc"), colClasses = list(character = "cmd_code"), showProgress = FALSE)
  dd <- unique(dd)
  desc <- stats::setNames(as.character(dd$cmd_desc), formatC(as.integer(dd$cmd_code), width = 6, flag = "0"))
  desc <- desc[!duplicated(names(desc))]
  rm(dd)
  codes <- sector_trade_codes(basket, desc)
  excluded <- sector_trade_excluded(hs6_ref, basket_cells)
  tech_of_cell <- sub("-(upstream|midstream|downstream)$", "", basket_cells)
  for (tech in sort(unique(tech_of_cell))) {
    ck <- paste0("cell:", basket_cells[tech_of_cell == tech])
    trade[[tech]] <- list(flows = flows[flows$item_key %in% ck, , drop = FALSE], declared = declared[declared$item_key %in% ck, , drop = FALSE],
                          codes = codes[codes$item_key %in% ck, , drop = FALSE], excluded = excluded[excluded$item_key %in% ck, , drop = FALSE],
                          cty_sub = cty_sub[cty_sub$item_key %in% ck, , drop = FALSE],
                          sub_intra = sub_intra[sub_intra$item_key %in% ck, , drop = FALSE])
  }
  # Every sub-component of trade.cty_sub is a trade.codes sub-sector of its cell.
  ck_codes <- unique(paste(codes$item_key, codes$sub_sector))
  ck_sub <- unique(paste(n_sub$item_key, n_sub$sub_sector))
  add_check("trade by sub-component: sub-components are the basket's sub-sectors (trade.codes)", if (all(ck_sub %in% ck_codes)) "PASS" else "FAIL",
            length(ck_sub), sprintf("%d of %d basket sub-sectors traded in 2020 or 2024; cells with one sub-component: %d", length(ck_sub), length(ck_codes),
                                     sum(table(unique(codes[c("item_key", "sub_sector")])$item_key) == 1)))
  rm(flows, declared, codes, cty_sub, sub_intra)
} else {
  notes <- c(notes, "Trade flows were not exported in this build (--skip-trade).")
}

# ---- Policy -----------------------------------------------------------------------------------------

step("IEA PAMS -> policy records")
pams_path <- file.path(raw_dir, "IEA_PAMS_Export.csv")
pams_raw <- readr::read_csv(use_src(pams_path), show_col_types = FALSE, progress = FALSE)
ptbl <- build_policy_table(clean_pams_raw(pams_raw))
rm(pams_raw)
pol <- sector_policy_compact(ptbl)
rm(ptbl)
invisible(gc())
pol$cat <- cat_tbl
if (!policy_text) pol$text <- NULL

# ---- CM-FDI ------------------------------------------------------------------------------------------

fdi_clean <- NULL
cmfdi_raw <- NULL
# The Partnership indices' CM-FDI settings (project filter, cells, Chinese HQ economies): the
# descriptive views describe the same projects.
fdi_settings <- sector_fdi_settings(yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))$cm_fdi)
if (with_fdi) {
  cmfdi_path <- if (!is.null(cmfdi_arg)) abs_path(cmfdi_arg) else file.path(raw_dir, "cm_fdi_projects.csv")
  if (!file.exists(cmfdi_path)) {
    stop("CM-FDI input missing: stage data/raw/cm_fdi_projects.csv (Cleantech Manufacturing FDI Dataset), pass --cmfdi <csv>, ",
         "or export without it (--no-fdi).")
  }
  step("CM-FDI projects ", rel(cmfdi_path))
  # Every column as text, nothing converted on read: sector_fdi_clean() is the whole transformation.
  cmfdi_raw <- utils::read.csv(use_src(cmfdi_path), colClasses = "character", check.names = FALSE, na.strings = character(),
                               fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  fdi_clean <- sector_fdi_clean(cmfdi_raw, iso3_of, fdi_settings)
  add_check("guard: CM-FDI columns, vocabularies, years, flags and country names as reviewed (stops otherwise)", "PASS",
            nrow(fdi_clean$projects),
            sprintf("%d projects, %d manufacturing, %d used by the indices, %d without a disclosed value, %d source cells (%d links)",
                    nrow(fdi_clean$projects), sum(fdi_clean$projects$manufacturing), sum(fdi_clean$projects$index_use),
                    sum(is.na(fdi_clean$projects$value)), nrow(fdi_clean$sources),
                    length(unlist(strsplit(stats::na.omit(fdi_clean$sources$url), "|", fixed = TRUE)))))
  # Cross-check: the projects flagged index_use are the ones the pipeline's own reader flags
  # (R/themes/partnership_strength/cm_fdi.R, when present).
  cm_pipeline <- file.path(repo_root, "R", "themes", "partnership_strength", "cm_fdi.R")
  if (file.exists(cm_pipeline)) {
    source(cm_pipeline)
    pset <- cm_fdi_resolve_settings(yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))$cm_fdi)
    pfl <- cm_fdi_flag_index_use(cm_fdi_clean_projects(cmfdi_raw, pset$technologies), pset)
    mine <- fdi_clean$projects
    same_set <- setequal(pfl$project_id[pfl$index_use], mine$pid[mine$index_use]) &&
      setequal(pfl$project_id[pfl$china_parent], mine$pid[mine$home %in% fdi_settings$china_hq_iso3])
    add_check("parity X: CM-FDI projects used by the indices and Chinese parents = the pipeline's cm_fdi_flag_index_use()",
              if (same_set) "PASS" else "FAIL", sum(mine$index_use),
              if (same_set) NA_character_ else sprintf("pipeline %d, export %d", sum(pfl$index_use), sum(mine$index_use)))
    if (!same_set) stop("CM-FDI: the export's index_use flags differ from the pipeline's cm_fdi_flag_index_use(); align sector_fdi_flags().")
    rm(pfl, pset, mine)
  }
} else {
  notes <- c(notes, "CM-FDI cross-border projects were not exported in this build (--no-fdi).")
}

# ---- Development & finance ---------------------------------------------------------------------------
# The raw data behind Development Potential (WDI, Doing Business, OECD CRS) and Friendshore's outbound
# component (IMF DIP), read as scripts/15_build_partner_themes.R reads them and parsed with the
# builders' own functions (R/dashboard/sector/sector_dev.R). Every shipped value is checked against
# the builder on the same input; the builder on these inputs must reproduce the components stored in
# data/processed. A missing input stops the export (stage it, or export with --no-dev).

dev_part <- NULL
dev_fill <- NULL
dev_items <- NULL
dev_ref <- NULL
if (with_dev) {
  step("Development & finance: WDI, Doing Business, OECD CRS, IMF DIP")
  dev_files <- c(ci = "wdi_country_info.csv", gdp = "wdi_gdp.csv", wdi = "wb_wdi.csv", db = "wb_doingbusiness.csv",
                 crs = "oecd_crs_api.csv", dip = "imf_dip.csv")
  dev_paths <- stats::setNames(file.path(raw_dir, dev_files), names(dev_files))
  miss <- dev_files[!file.exists(dev_paths)]
  if (length(miss)) stop("Development & finance input missing in data/raw: ", paste(miss, collapse = ", "), " (stage it, or export with --no-dev).")
  dev_ci <- standardize_country_info(read.csv(use_src(dev_paths[["ci"]])))
  dev_iso <- dev_ci$iso3c
  dev_gdp <- read.csv(use_src(dev_paths[["gdp"]]))
  dev_ind <- sector_dev_indicators(dict_raw)
  wdi_raw <- read.csv(use_src(dev_paths[["wdi"]]))
  db_raw <- read.csv(use_src(dev_paths[["db"]]))
  dev_wdi <- sector_dev_wdi(wdi_raw, dev_iso, dev_ind$code)
  dev_db <- sector_dev_doing_business(db_raw, dev_iso)
  crs_raw <- read.csv(use_src(dev_paths[["crs"]]))
  dev_crs <- sector_dev_crs(crs_raw, dev_iso)
  if (!identical(dev_crs$meta$BaseYear, "2023") || !identical(dev_crs$meta$UnitMult, "Millions") || !identical(dev_crs$meta$Currency, "US dollar")) {
    stop("OECD CRS extract: prices of ", dev_crs$meta$BaseYear, ", ", dev_crs$meta$UnitMult, " ", dev_crs$meta$Currency,
         "; the dictionary unit usd_m_2023 says constant 2023 US dollars, millions: update it.")
  }
  dev_stage <- sector_dev_crs_stage(crs_raw, dev_gdp, SECTOR_DEV_GDP_YEAR, dev_iso)
  crs_builder <- partnership_strength_build_oecd_supplychain(crs_raw, dev_gdp, SECTOR_DEV_GDP_YEAR, valid_iso = dev_iso)
  pc <- sector_dev_crs_parity(dev_stage, crs_builder, dev_crs$flows)
  for (i in seq_len(nrow(pc))) add_check(pc$check[i], pc$status[i], pc$n[i], pc$detail[i])
  if (any(pc$status == "FAIL")) stop("OECD CRS parity FAIL: ", paste(pc$check[pc$status == "FAIL"], collapse = "; "))
  # The builder on these raw inputs reproduces the Development Potential components in data/processed.
  dev_proc <- readRDS(use_src(file.path(processed_dir, "partner_development_tbl.rds")))
  pp <- sector_dev_processed_parity(dev_proc, partnership_strength_build_development_indicators(wdi_raw, db_raw, valid_iso = dev_iso), crs_builder)
  for (i in seq_len(nrow(pp))) add_check(pp$check[i], pp$status[i], pp$n[i], pp$detail[i])
  if (any(pp$status == "FAIL")) stop("Development Potential: the builder on data/raw does not reproduce data/processed: ",
                                     paste(pp$check[pp$status == "FAIL"], pp$detail[pp$status == "FAIL"], collapse = "; "))
  wl <- partnership_strength_build_wdi_long(wdi_raw, valid_iso = dev_iso)
  dbb <- partnership_strength_build_doing_business(db_raw, valid_iso = dev_iso)
  dev_ref <- list(
    wdi = tibble::tibble(key = paste(wl$Country.Code, wl$Series.Code), value = wl$value),
    db = tibble::tibble(key = as.character(dbb$Country.Code), value = dbb$doingbusiness),
    stage = dev_stage
  )
  rm(dev_proc, crs_raw, wl, dbb, crs_builder)
  invisible(gc())
  # IMF DIP: the columns the outbound builder uses (as scripts/15 read_imf_dip()).
  dip_header <- names(data.table::fread(use_src(dev_paths[["dip"]]), nrows = 0))
  dip_keep <- c(intersect(c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"), dip_header), grep("^X?[0-9]{4}$", dip_header, value = TRUE))
  dip_raw <- as.data.frame(data.table::fread(dev_paths[["dip"]], select = dip_keep, showProgress = FALSE))
  pcfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))$outbound_fdi
  dev_conduit <- partnership_strength_conduit_rule(rule = pcfg$conduit_rule %||% "conduits", scope = pcfg$conduit_scope %||% "both")
  dev_centres <- partnership_strength_conduit_centres()
  if (!all(dev_conduit$iso3 %in% dev_centres$iso3)) stop("IMF DIP: the configured conduit rule names economies outside partnership_strength_conduit_centres()")
  dev_dip <- sector_dev_dip(dip_raw, dev_ci, SECTOR_DEV_DIP_YEARS, dev_centres$iso3)
  dip_builder <- partnership_strength_build_outbound_edges(dip_raw, dev_ci, dev_gdp, gdp_year = SECTOR_DEV_GDP_YEAR, years = SECTOR_DEV_DIP_YEARS,
                                                           conduit_iso = dev_conduit$iso3, conduit_scope = dev_conduit$scope)
  rm(dip_raw)
  invisible(gc())
  # The index drops the conduit positions of the configured rule; the flag marks every centre.
  pdp <- sector_dev_dip_parity(dev_dip, dip_builder, dev_gdp, SECTOR_DEV_GDP_YEAR, dev_conduit$scope, dev_conduit$iso3)
  add_check(pdp$check, pdp$status, pdp$n, pdp$detail)
  if (pdp$status == "FAIL") stop("IMF DIP parity FAIL: ", pdp$check)
  rm(dip_builder)
  # Years most donors had not reported yet: in the totals (as the builder counts them), not shown as years.
  dev_inc <- sector_dev_crs_incomplete(dev_crs$flows)
  dev_crs_ship <- sector_dev_crs_ship(dev_crs, dev_inc)
  dev_part <- sector_dev_part(dev_crs_ship, dev_dip, dev_centres)
  dev_fill <- sector_dev_fill(dev_wdi, dev_db, dev_crs, dev_dip, SECTOR_DEV_DIP_YEARS, dev_centres, dev_conduit$scope, dev_inc)
  results$dev <- sector_dev_facts(dev_wdi$values, dev_db, dev_crs_ship, dev_stage, max(dev_wdi$values$year_max), max(dev_crs$flows$year))
  add_check("Development & finance: raw inputs parsed with the builders' functions", "PASS",
            nrow(dev_wdi$values) + nrow(dev_db) + nrow(dev_crs$flows) + nrow(dev_dip),
            sprintf("WDI %d values (%d series; none in the window: %s); Doing Business %d; CRS %d dyad-years (%s donors, %s recipients, %s; years shown %s; incomplete: %s); DIP %d positions (%d with a conduit centre)",
                    nrow(dev_wdi$values), length(unique(dev_wdi$values$code)), dev_fill$wdi_empty, nrow(dev_db), nrow(dev_crs$flows),
                    dev_fill$crs_n_donors, dev_fill$crs_n_recipients, dev_fill$crs_years_total, dev_fill$crs_years, dev_fill$crs_incomplete,
                    nrow(dev_dip), sum(dev_dip$conduit != "none")))
  rm(wdi_raw, db_raw)
} else {
  notes <- c(notes, "Development & finance raw data (WDI, Doing Business, OECD CRS, IMF DIP) were not exported in this build (--no-dev).")
}

# ---- Series -----------------------------------------------------------------------------------------

series <- NULL
if (with_series) {
  step("Series: EI, IEA EV regions, CIM, IMF monthly")
  geo_codes_all <- unique(c(coal$iso3, ideal$iso3[grepl("^[A-Z]{3}$", ideal$iso3)]))
  ei_ser <- sector_ei_series(ei, ei_map, geo_codes = unique(c(geo_codes_all, ei$ISO3166_alpha3[grepl("^[A-Z]{3}$", ei$ISO3166_alpha3)])),
                             years = 1965:as.integer(ei_latest))
  add_check("EI series: successor states' zeros before they exist and the USSR's zeros after it ends are nsr (NaN)", "PASS",
            ei_ser$successor_nsr)
  nr <- ei_ser$negative_residual
  add_check("EI series: world minus the listed economies is not negative (RES:ei rows; dust below 1e-6 of W is zero)",
            if (nrow(nr)) "WARN" else "PASS", nrow(nr),
            if (nrow(nr)) paste0("not shipped: ", paste(sprintf("%s %s %d (%.2f%% of W)", nr$var, sub("^fuel:", "", nr$item_key), nr$year,
                                                                100 * nr$residual / nr$world), collapse = "; ")) else NA_character_)
  ev_path <- file.path(raw_dir, "ev_Midstream_capacity.csv")
  ev_ser <- sector_iea_ev_series(read.csv(use_src(ev_path), check.names = FALSE, stringsAsFactors = FALSE))
  g <- tmp_copy(file.path(raw_dir, "GCIM_Investment_Capacity_aggregated.xlsx"))
  pw <- investment_read_gcim_sheet(g, "electric_power_capacity", c("Country", "Segment", "Technology", "Year", "Value"), "power additions")
  cap <- investment_read_gcim_sheet(g, "mfg_ind_capacity", c("Country", "Segment", "Technology", "Product", "Category", "Value"), "capacity")
  power <- sector_cim_power(pw, iso3_of)
  capacity <- sector_cim_capacity(cap, iso3_of)
  rm(pw, cap)
  imf_path <- file.path(raw_dir, "imf_commodity_prices.csv")
  imf <- read.csv(use_src(imf_path), stringsAsFactors = FALSE)
  monthly <- sector_imf_monthly(energy_prices_imf_clean(energy_prices_imf_monthly_usd_long(imf)), commodities)
  rm(imf)
  # X cross-check: each commodity's last monthly value = the processed latest price.
  pl <- results$prices$facts[results$prices$facts$var == "prices.latest", , drop = FALSE]
  last <- monthly[order(monthly$item_key, -monthly$year, -monthly$month), , drop = FALSE]
  last <- last[!duplicated(last$item_key), , drop = FALSE]
  lv <- last$value[match(pl$item_key, last$item_key)]
  rel_px <- abs(lv - pl$value) / abs(pl$value)
  add_check("parity X: IMF monthly price in each commodity's last month = prices.latest (1e-12 relative)",
            if (all(!is.na(rel_px)) && max(rel_px) <= 1e-12) "PASS" else "WARN", nrow(pl),
            sprintf("max relative difference %.3g; last months %s", max(rel_px, na.rm = TRUE),
                    paste(unique(sprintf("%d-%02d", last$year, last$month)), collapse = ", ")))
  rm(pl, last, lv, rel_px)
  series <- list(obs = dplyr::bind_rows(ei_ser$obs, ev_ser, power), world = ei_ser$world, monthly = monthly, capacity = capacity)
  invisible(gc())
}
rm(ei)
invisible(gc())

# ---- Assemble ---------------------------------------------------------------------------------------

step("Assemble the bundle")
all_res <- sector_bind_results(results)
facts <- all_res$facts
ledger <- all_res$ledger
fact_notes <- all_res$notes
world <- dplyr::bind_rows(worlds)
world <- world[!duplicated(world[c("var", "item_key", "year")]), , drop = FALSE]
# Series rows that are also facts must carry the identical value (contract rule).
if (!is.null(series)) {
  fk <- paste(facts$var, facts$item_key, facts$geo, facts$year)
  sk <- paste(series$obs$var, series$obs$item_key, series$obs$geo, series$obs$year)
  both <- match(sk, fk)
  hit <- !is.na(both)
  same <- identical(series$obs$value[hit], facts$value[both[hit]])
  if (!same) {
    d <- abs(series$obs$value[hit] - facts$value[both[hit]])
    stop("Series rows differ from the facts for the same key: ", sum(d != 0), " rows, max ", max(d))
  }
  add_check("series: rows shared with facts carry identical values", "PASS", sum(hit))
}

data_codes <- unique(c(facts$geo, if (!skip_trade) econ, pol$pams$geo, pol$cat$geo, if (!is.null(series)) c(series$obs$geo, series$capacity$geo),
                       if (!is.null(fdi_clean)) c(fdi_clean$projects$home, fdi_clean$projects$dest),
                       if (!is.null(dev_part)) c(dev_part$oda$donor, dev_part$oda$recipient, dev_part$dip$reporter, dev_part$dip$counterpart, dev_part$conduits$geo)))
data_codes <- data_codes[grepl("^[A-Z]{3}$", data_codes) & !data_codes %in% c("EUU", "WLD")]
country_info <- read.csv(use_src(file.path(raw_dir, "wdi_country_info.csv")), stringsAsFactors = FALSE)
extra <- tibble::tibble(code = data_codes, name = country_name_from_iso3(data_codes))
# Former economies in the EI series that WDI and countrycode do not name.
former <- c(SUN = "USSR (former)", ANT = "Netherlands Antilles (former)", YUG = "Yugoslavia (former)", CSK = "Czechoslovakia (former)")
hit <- extra$code %in% names(former) & (is.na(extra$name) | extra$name == extra$code)
extra$name[hit] <- unname(former[extra$code[hit]])
hubs <- unlist(getOption("opportunity_security.index_definition")$trade$re_export_hubs)
geo <- sector_geo(country_info, coal, ideal, hubs = hubs, extra = extra)
unknown <- setdiff(data_codes, geo$code)
if (length(unknown)) stop("Economies in the data without a dims.geo row: ", paste(unknown, collapse = ", "))
region_members <- sector_region_members(geo, results$ev$region_of)
fdi <- NULL
if (!is.null(fdi_clean)) {
  fdi <- sector_fdi_part(fdi_clean, geo, fdi_settings)
  fp <- sector_fdi_problems(fdi, geo, fdi_settings)
  if (length(fp)) stop("CM-FDI: ", paste(fp, collapse = "; "))
  add_check("CM-FDI: flows, group splits and years count every project once per basis; shares sum to 1; links are clean http(s)", "PASS",
            nrow(fdi$flows) + nrow(fdi$groups) + nrow(fdi$years),
            sprintf("flows %d, groups %d, years %d rows; projects used by the indices %d; Chinese-HQ parents (%s) %d",
                    nrow(fdi$flows), nrow(fdi$groups), nrow(fdi$years), sum(fdi$projects$index_use),
                    paste(fdi_settings$china_hq_iso3, collapse = ", "), sum(fdi$projects$home_group == "china")))
}

minerals <- unique(c(sub("^mineral:", "", facts$item_key[startsWith(facts$item_key, "mineral:")]), mineral_demand$mineral))
fuels <- unique(c(sub("^fuel:", "", facts$item_key[startsWith(facts$item_key, "fuel:")]),
                  if (!is.null(series)) sub("^fuel:", "", series$obs$item_key[startsWith(series$obs$item_key, "fuel:")])))
items <- sector_items(basket_cells, minerals, fuels, commodities)
cells <- items[items$axis == "cell", , drop = FALSE]
item_links <- sector_item_links(mineral_links, price_techs, commodities, cells)
if (!is.null(dev_part)) {
  dev_items <- sector_dev_items(dev_ind, cells)
  items <- dplyr::bind_rows(items, dev_items$items[names(items)])
  item_links <- dplyr::bind_rows(item_links, dev_items$links)
}
mineral_demand <- mineral_demand[mineral_demand$mineral %in% items$slug[items$axis == "mineral"], , drop = FALSE]

fill <- list(n_shared_codes = n_shared, n_codes = n_codes, n_rca_reporters = n_rca_reporters, critmin_year = critmin_year,
             pams_export_date = format(file.mtime(pams_path), "%Y-%m-%d"), pams_year_max = max(pol$pams$year, na.rm = TRUE),
             trl_fallback_cells = paste(results$trl$fallback, collapse = ", "),
             pct_cross_cutting = round(100 * mean(pol$pams$cross_cutting)))
fill <- c(fill, if (!is.null(fdi)) sector_fdi_fill(fdi, fdi_settings) else sector_fdi_fill_none())
fill <- c(fill, if (!is.null(dev_fill)) dev_fill else sector_dev_fill_none())
tabs <- sector_dictionary_tables(dict_raw, fill = fill)
unfilled <- sector_dictionary_unfilled(tabs)
if (length(unfilled)) stop("Unfilled dictionary placeholders: ", paste(unfilled, collapse = ", "))

# Item units: the dictionary's, IMF price units, USGS production units.
pu <- energy_prices_pcps_units()
price_unit <- tibble::tibble(sub_sector = commodities$sub_sector, text = pu$unit[match(commodities$sub_sector, pu$clean)])
price_unit$unit <- unname(tabs$price_unit_ids[price_unit$text])
price_unit$unit[is.na(price_unit$unit)] <- "price_tbc"
iu <- tabs$item_units
iu$item_key <- sector_item_key(iu$axis, iu$item)
item_units <- dplyr::bind_rows(
  iu[c("var", "item_key", "unit")],
  tibble::tibble(var = "prices.latest", item_key = paste0("commodity:", commodities$slug), unit = price_unit$unit),
  tibble::tibble(var = "prices.monthly", item_key = paste0("commodity:", commodities$slug), unit = price_unit$unit),
  tibble::tibble(var = "production.mine_usgs", item_key = usgs_prod$units$item_key,
                 unit = ifelse(usgs_prod$units$unit == "thousand metric tons", "kt", "t"))
)
# The dictionary's item units win over the generic USGS kt / t (lithium content, REO, metal
# content); they must agree with the USGS scale (thousand metric tons <-> a kt unit).
usgs_dict <- iu[iu$var == "production.mine_usgs", , drop = FALSE]
if (nrow(usgs_dict)) {
  kind <- tabs$units$unit_kind[match(usgs_dict$unit, tabs$units$id)]
  src_unit <- usgs_prod$units$unit[match(usgs_dict$item_key, usgs_prod$units$item_key)]
  bad <- !is.na(src_unit) & (kind == "kt") != (src_unit == "thousand metric tons")
  if (any(bad)) stop("Dictionary item units disagree with the USGS scale: ", paste(usgs_dict$item_key[bad], collapse = ", "))
}
if (!is.null(dev_part)) item_units <- dplyr::bind_rows(item_units, sector_dev_item_units(dev_ind))
item_units <- item_units[!duplicated(item_units[c("var", "item_key")]), , drop = FALSE]
if (any(price_unit$unit == "price_tbc")) notes <- c(notes, paste("IMF price units to confirm:", paste(price_unit$sub_sector[price_unit$unit == "price_tbc"], collapse = ", ")))

parts <- list(
  geo = geo, region_members = region_members, items = items, item_links = item_links, mineral_demand = mineral_demand,
  eu_outside_coalition = geo$code[geo$kind == "country" & geo$eu_member & geo$tier == "none"],
  item_units = item_units,
  roster_extra = dplyr::bind_rows(
    if (!skip_trade) tibble::tibble(roster = "comtrade_matrix", geo = econ),
    tibble::tibble(roster = "ei", geo = unique(facts$geo[facts$var %in% c("production.fuels", "consumption.per_capita")])),
    if (!is.null(fdi)) tibble::tibble(roster = "cmfdi_home", geo = sort(unique(fdi$projects$home)))
  ),
  facts = facts, fact_notes = fact_notes, world = world, series = series, trade = trade, eu_intra = eu_intra, policy = pol, fdi = fdi,
  dev = dev_part, dev_year = if (is.null(dev_part)) NA_integer_ else max(dev_part$dip$year_max),
  reported = if (!skip_trade) reported else NULL,
  licensed = tibble::tibble(item_key = paste0("cell:", c("solar-downstream", "wind-downstream", "gas-downstream", "coal-downstream", "batteries-downstream")),
                            family = "prices", sources = "BNEF LCOE"),
  headline_sources = sector_headline_sources()
)
bundle <- sector_assemble(parts, tabs)
if (length(bundle$world_missing)) stop("Source-world measures without a world total: ", paste(utils::head(bundle$world_missing, 8), collapse = "; "))
# Every measure of the build has data.
have <- unique(sector_data_keys(parts, geo)$var)
no_data <- setdiff(tabs$vars$id, have)
if (length(no_data)) stop("Measures of the build without data: ", paste(no_data, collapse = ", "))
add_check("dictionary: every measure of the build has data", "PASS", nrow(tabs$vars))
# Native levels: no region / entity / EU / world value on a country geo and vice versa.
kind <- geo$kind[match(facts$geo, geo$code)]
lvl <- tabs$vars$native_level[match(facts$var, tabs$vars$id)]
allowed <- (lvl == "country" & kind %in% c("country", "eu")) | (lvl == "region" & kind == "region") |
  (lvl == "entity" & kind == "entity") | (lvl %in% c("world", "cell_global") & kind == "world")
if (!all(allowed)) stop("Facts at the wrong native level: ", paste(utils::head(unique(paste(facts$var[!allowed], facts$geo[!allowed])), 8), collapse = "; "))
add_check("native levels: region, entity, EU and world values never on a country geo", "PASS", nrow(facts))
# Shares within [0, 1 + 1e-9]; listed sums within the world total.
share_vars <- tabs$vars$id[tabs$vars$unit %in% c("share", "share_tbc")]
sv <- facts$value[facts$var %in% share_vars & is.finite(facts$value)]
if (any(sv < 0 | sv > 1 + 1e-9)) stop("Share values outside [0, 1]")
clamp <- max(c(0, bundle$summary$clamped))
add_check("shares in [0, 1]; listed sums within the world total (1e-9)", if (clamp <= 1e-9) "PASS" else "WARN", length(sv),
          sprintf("largest excess of the listed sum over W: %.3g", clamp))
if (clamp > 1e-9) {
  bad <- bundle$summary[bundle$summary$clamped > 1e-9, c("var", "item_key", "year", "eu_mode", "clamped")]
  notes <- c(notes, paste0("Listed values exceed the source's world total (shares then sum to more than 1): ",
                           paste(unique(paste(bad$var, bad$item_key, bad$year)), collapse = "; ")))
}
if (!is.null(bundle$reconcile) && nrow(bundle$reconcile)) {
  w <- bundle$reconcile[bundle$reconcile$warn, , drop = FALSE]
  add_check("trade bases: matrix and exporter-reported China shares within 3 points", if (nrow(w)) "WARN" else "PASS", nrow(bundle$reconcile),
            if (nrow(w)) paste(sub("^cell:", "", w$item_key), collapse = ", ") else NA_character_)
}
for (i in seq_len(nrow(all_res$excluded))) {
  e <- all_res$excluded[i, ]
  add_check(paste0("excluded from ", e$table, " ", e$variable, ": ", e$reason), "PASS", e$n)
}
chunks <- sector_chunks(bundle)
if (!policy_text) chunks$`sector-policy-text` <- NULL

# Parity on the decoded chunks.
post <- function(decoded) {
  meta <- decoded$`sector-meta`
  dfx <- sector_decoded_obs(meta, decoded$`sector-facts`$facts$obs)
  pr <- sector_parity(dfx, ledger)
  for (i in seq_len(nrow(pr))) add_check(pr$check[i], pr$status[i], pr$n[i], pr$detail[i])
  if (any(pr$status == "FAIL")) {
    stop("Parity FAIL:\n  ", paste(pr$check[pr$status == "FAIL"], pr$detail[pr$status == "FAIL"], collapse = "\n  "))
  }
  # P rows shipped per (table, variable) = processed raw keys - documented exclusions (reported).
  np <- table(paste(ledger$table, ledger$variable)[ledger$prov == "P"])
  add_check("parity P: rows shipped verbatim per processed (table, variable)", "PASS", sum(np),
            paste(names(np), as.integer(np), sep = ": ", collapse = "; "))
  # Summary parity: the summary recomputed from the decoded facts equals meta.summary.
  dw <- sector_decoded_world(meta, decoded$`sector-facts`$facts$world)
  recomputed <- sector_summary(dfx, dw, meta$dims$geo, bundle$dict$vars, eu_intra)
  sm <- meta$summary
  it <- meta$dims$items
  ms <- tibble::tibble(item_key = paste0(it$axis, ":", it$slug)[sm$item + 1L], var = meta$dict$vars$id[sm$var + 1L], year = sm$year,
                       eu_mode = sm$eu_mode, W = sm$W, n = sm$n, top1_geo = sm$top1_geo + 1L, top1_share = sm$top1_share,
                       top3_share = sm$top3_share, hhi = sm$hhi, eff_n = sm$eff_n, china = sm$china, core = sm$core,
                       partner = sm$partner, rest = sm$rest, unattributed = sm$unattributed, growth = sm$growth)
  d <- sector_summary_diff(recomputed, ms)
  add_check("summary parity: recomputed from the decoded facts = meta.summary (1e-12)", if (d <= 1e-12) "PASS" else "FAIL", nrow(ms),
            sprintf("max relative difference %.3g", d))
  if (d > 1e-12) stop("Summary parity FAIL (", d, ")")
  # Trade by sub-component on the decoded chunks: the sums equal the decoded facts and
  # trade.sub_summary equals sector_summary() recomputed from the decoded trade.cty_sub.
  geo_codes <- meta$dims$geo$code
  it_keys <- paste0(it$axis, ":", it$slug)
  sub_rel <- 0
  sub_unmatched <- 0L
  sum_d <- 0
  n_sub_rows <- 0L
  for (nm in grep("^sector-trade-", names(decoded), value = TRUE)) {
    tr <- decoded[[nm]]$trade
    if (is.null(tr$cty_sub)) next
    cs <- tibble::tibble(item_key = it_keys[tr$cty_sub$item + 1L], sub_sector = tr$cty_sub$sub_sector, geo = geo_codes[tr$cty_sub$geo + 1L],
                         year = tr$cty_sub$year, exports = tr$cty_sub$exports, imports = tr$cty_sub$imports)
    n_sub_rows <- n_sub_rows + nrow(cs)
    ch <- sector_trade_sub_check(cs, dfx[dfx$var %in% c("trade.exports", "trade.imports"), , drop = FALSE], dw)
    sub_rel <- max(sub_rel, ch$max_rel)
    sub_unmatched <- sub_unmatched + ch$n_unmatched
    si <- if (is.null(tr$sub_intra)) NULL else
      tibble::tibble(item_key = it_keys[tr$sub_intra$item + 1L], sub_sector = tr$sub_intra$sub_sector, year = tr$sub_intra$year, intra = tr$sub_intra$intra)
    re <- sector_trade_sub_summary(cs, si, meta$dims$geo, bundle$dict$vars)
    ss <- tr$sub_summary
    got <- tibble::tibble(item_key = paste0(it_keys[ss$item + 1L], "|", ss$sub_sector), var = meta$dict$vars$id[ss$var + 1L], year = ss$year,
                          eu_mode = ss$eu_mode, W = ss$W, n = ss$n, top1_geo = ss$top1_geo + 1L, top1_share = ss$top1_share,
                          top3_share = ss$top3_share, hhi = ss$hhi, eff_n = ss$eff_n, china = ss$china, core = ss$core, partner = ss$partner,
                          rest = ss$rest, unattributed = ss$unattributed, growth = ss$growth)
    re$item_key <- paste0(re$item_key, "|", re$sub_sector)
    sum_d <- max(sum_d, sector_summary_diff(re, got))
  }
  add_check("trade by sub-component (decoded): sums equal the decoded facts and world totals (1e-9 relative)",
            if (sub_rel <= 1e-9 && sub_unmatched == 0) "PASS" else "FAIL", n_sub_rows, sprintf("max relative difference %.3g", sub_rel))
  add_check("trade by sub-component (decoded): trade.sub_summary = sector_summary() of the decoded trade.cty_sub (1e-12)",
            if (sum_d <= 1e-12) "PASS" else "FAIL", n_sub_rows, sprintf("max relative difference %.3g", sum_d))
  if (sub_rel > 1e-9 || sub_unmatched > 0 || sum_d > 1e-12) stop("Trade by sub-component FAIL on the decoded chunks")
  # Development & finance: decoded flows and positions identical() to the parsed raw rows; decoded
  # facts identical() to the builders' values (WDI, Doing Business, the CRS stage totals and ratios).
  if (!is.null(decoded$`sector-dev`)) {
    pd <- sector_dev_parity(decoded$`sector-dev`, meta, dev_part)
    dv <- dfx[startsWith(dfx$var, "development.") & dfx$var != "development.alignment", , drop = FALSE]
    pd <- dplyr::bind_rows(pd, sector_dev_facts_parity(dv, dev_ref))
    for (i in seq_len(nrow(pd))) add_check(pd$check[i], pd$status[i], pd$n[i], pd$detail[i])
    if (any(pd$status == "FAIL")) stop("Development & finance parity FAIL:\n  ", paste(pd$check[pd$status == "FAIL"], collapse = "\n  "))
  }
  # CM-FDI parity: decoded projects identical() to the CSV after cleaning; aggregates recomputed.
  if (!is.null(decoded$`sector-fdi`)) {
    pf <- sector_fdi_parity(decoded$`sector-fdi`, meta, cmfdi_raw, iso3_of, fdi_settings)
    for (i in seq_len(nrow(pf))) add_check(pf$check[i], pf$status[i], pf$n[i], pf$detail[i])
    if (any(pf$status == "FAIL")) stop("CM-FDI parity FAIL:\n  ", paste(pf$check[pf$status == "FAIL"], pf$detail[pf$status == "FAIL"], collapse = "\n  "))
  }
}
add_check("no index, weight or contribution rows: only data_type raw rows of data/processed are read", "PASS", nrow(ledger))

strategic_path <- file.path(dirname(out_dir), "manifest.json")
strategic_bundle_id <- if (file.exists(strategic_path)) jsonlite::fromJSON(strategic_path)$bundle_id else NA_character_
processed_mtime_max <- format(max(file.mtime(unique(processed_read))), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
git_commit <- tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
                       error = function(e) NA_character_)
notes <- c(notes,
           "Coalition: 57 economies (31 core, 26 partners); the EU-27 is one node in the Strategic network.",
           "Trade on screen is the reconciled Comtrade matrix (importer-declared; exporters' reports x1.06 where an importer did not declare); Hong Kong is kept and tagged as a re-export hub.",
           "Hong Kong is out of the exporter-reported Comtrade measures (RCA and the reported side of the reconciliation), as in Strategic Insights.",
           "Critical-mineral reserves: USGS Mineral Commodity Summaries 2026.",
           paste0("Fossil production EI does not report separately comes from the EIA International Energy Statistics where the pipeline took it ",
                  "(converted to EI's units; gas and coal from EIA's latest year); where neither reports it, production is assumed to be 0 ",
                  "and the net balance and self-sufficiency say so (marked 'not reported separately')."),
           "Policy: IEA Policies and Measures records; Climate Action Tracker ratings where CAT rates the economy.",
           if (!is.null(fdi)) paste0("Cross-border clean-tech manufacturing projects: Cleantech Manufacturing FDI Dataset (CM-FDI), P. Bandara and ",
                                     "the Net Zero Industrial Policy Lab, Johns Hopkins University, edition 2026.10.05, CC BY 4.0; ",
                                     "project counts and sums of disclosed capital computed by the OSI authors, no record altered; ",
                                     "undisclosed capital investment is shown as not disclosed, never zero."),
           paste0("Trade baskets cover Electric Vehicles, Solar, Wind, Nuclear and Semiconductors Upstream since the 2026-10 basket review; ",
                  "trade is also shown by sub-component (the groups of HS6 codes a basket is built from), which add up to each sector's total."),
           "Scenario dials apply to Strategic Insights only.")
res <- finish(bundle, chunks, out_dir, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), git_commit = git_commit,
              strategic_bundle_id = strategic_bundle_id, processed_mtime_max = processed_mtime_max, pipeline_marker = pipeline_marker,
              flags = list(include_restricted = FALSE, trade = length(trade) > 0, atlas = FALSE,
                           policy_text = "sector-policy-text" %in% names(chunks), series = "sector-series" %in% names(chunks),
                           fdi = "sector-fdi" %in% names(chunks), dev = "sector-dev" %in% names(chunks)),
              validation_csv = file.path(processed_dir, "dashboard", "sector_validation.csv"), post_decode = post)

# Reference values for the spec's Appendix B (recomputed on this data).
sm <- bundle$summary
geo_code <- function(i) bundle$geo$code[i]
pick <- function(v, it, y, mode = "members") sm[sm$var == v & sm$item_key == it & sm$year == y & sm$eu_mode == mode, , drop = FALSE]
ref_rows <- list()
for (cl in c("batteries-midstream", "solar-midstream", "electric-vehicles-midstream", "electric-grid-midstream", "wind-midstream",
             "oil-upstream", "gas-upstream", "coal-upstream", "semiconductors-midstream", "electric-vehicles-upstream", "solar-upstream",
             "wind-upstream", "nuclear-upstream", "semiconductors-upstream", "nuclear-midstream")) {
  s <- pick("trade.exports", paste0("cell:", cl), 2024L)
  if (nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("%s matrix 2024: W USD %.1f bn; %s %.1f%%; top-3 %.1f%%; HHI %.3f; eff n %.1f; n %d; core %.1f%%, partners %.1f%%; growth since 2020 %+.0f%%",
                                                          cl, s$W / 1e9, geo_code(s$top1_geo), 100 * s$top1_share, 100 * s$top3_share, s$hhi, s$eff_n, s$n,
                                                          100 * s$core, 100 * s$partner, 100 * s$growth)
}
s <- pick("investment.annual", "cell:batteries-midstream", 2025L)
if (nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("CIM batteries-midstream 2025: USD %.1f bn; %s %.1f%%; core %.1f%%, partners %.1f%%; HHI %.3f; CAGR 2018-25 %+.1f%%",
                                                        s$W, geo_code(s$top1_geo), 100 * s$top1_share, 100 * s$core, 100 * s$partner, s$hhi, 100 * s$growth)
for (k in c("mineral:lithium", "mineral:graphite", "mineral:rare-earths", "mineral:cobalt")) {
  s <- pick("production.refining", k, 2025L)
  if (nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("IEA refining 2025 %s: W %.1f kt; %s %.1f%%; top-3 %.3f", k, s$W, geo_code(s$top1_geo), 100 * s$top1_share, s$top3_share)
}
s <- pick("production.uranium", "cell:nuclear-upstream", u_latest)
if (nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("Red Book uranium %d: %s %.1f%%", u_latest, geo_code(s$top1_geo), 100 * s$top1_share)
s <- pick("trade.atlas_share", "cell:batteries-midstream", 2023L)
if (nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("Atlas 2023 batteries-midstream: %s %.3f", geo_code(s$top1_geo), s$top1_share)
rc <- bundle$reconcile[bundle$reconcile$item_key == "cell:batteries-midstream", , drop = FALSE]
if (nrow(rc)) ref_rows[[length(ref_rows) + 1]] <- sprintf("Exporter-reported batteries-midstream 2024: USD %.1f bn; CHN %.1f%%; HHI %.3f; %d reporters",
                                                         rc$reported_total / 1e9, 100 * rc$reported_china, rc$reported_hhi, rc$n_reporters)
# Batteries Midstream in detail: 2020 W, rest / unattributed, USA and HKG, coalition presence, top 10.
bm <- "cell:batteries-midstream"
s24 <- pick("trade.exports", bm, 2024L)
s20 <- pick("trade.exports", bm, 2020L)
ex <- facts[facts$var == "trade.exports" & facts$item_key == bm & facts$year == 2024L, , drop = FALSE]
if (nrow(s24) && nrow(ex)) {
  ex <- ex[order(-ex$value, match(ex$geo, geo$code)), , drop = FALSE]
  sh <- function(g) ex$value[ex$geo == g] / s24$W
  coal_codes <- geo$code[geo$kind == "country" & geo$tier %in% c("core", "partner")]
  ref_rows[[length(ref_rows) + 1]] <- sprintf(
    "batteries-midstream 2024 detail: 2020 W USD %.1f bn; rest %.1f%%; unattributed %.1f%%; USA %.1f%% (#%d of %d with exports > 0); HKG %.1f%%; coalition members with exports %d of %d",
    s20$W / 1e9, 100 * s24$rest, 100 * s24$unattributed, 100 * sh("USA"), match("USA", ex$geo[ex$value > 0]), sum(ex$value > 0), 100 * sh("HKG"),
    sum(coal_codes %in% ex$geo[ex$value > 0]), length(coal_codes))
  top <- utils::head(ex, 10)
  ref_rows[[length(ref_rows) + 1]] <- paste0("batteries-midstream top exporters 2024 (USD bn): ", paste(sprintf("%s %.2f", top$geo, top$value / 1e9), collapse = "; "))
}
iv <- facts[facts$var == "investment.annual" & facts$item_key == bm & facts$year == 2025L, , drop = FALSE]
s25 <- pick("investment.annual", bm, 2025L)
s22 <- pick("investment.annual", bm, 2022L)
if (nrow(iv) && nrow(s25) && nrow(s22)) {
  ref_rows[[length(ref_rows) + 1]] <- sprintf("CIM batteries-midstream 2025 detail: CHN USD %.2f bn; USA USD %.2f bn (%.1f%%); CAGR 2022-25 %+.1f%%",
                                              iv$value[iv$geo == "CHN"], iv$value[iv$geo == "USA"], 100 * iv$value[iv$geo == "USA"] / s25$W,
                                              100 * ((s25$W / s22$W)^(1 / 3) - 1))
}
for (h in bundle$headlines) if (h$id == "investment") ref_rows[[length(ref_rows) + 1]] <- sprintf("All CIM cells 2025: USD %.0f bn; China %.1f%%", h$values$total_bn, 100 * h$values$china)
if (!skip_trade) {
  bb <- basket[basket$cell == "batteries-midstream", , drop = FALSE]
  ref_rows[[length(ref_rows) + 1]] <- sprintf("Basket: %d codes, %d in more than one cell (max %d); batteries-midstream %d codes, %d shared, %d sub-sectors; %d excluded reference rows",
                                              n_codes, n_shared, max(basket$shared_n) + 1L, nrow(bb), sum(bb$shared_n > 0), length(unique(bb$sub_sector)),
                                              sum(as.logical(hs6_ref$excluded) %in% TRUE))
}
for (k in c("mineral:graphite", "mineral:lithium")) {
  f <- facts[facts$var == "production.refining" & facts$item_key == k & facts$year == 2025L & facts$geo == "CHN", , drop = FALSE]
  s <- pick("production.refining", k, 2025L)
  if (nrow(f) && nrow(s)) ref_rows[[length(ref_rows) + 1]] <- sprintf("IEA refining 2025 %s: CHN %.0f of %.0f kt", k, f$value, s$W)
}
if (!is.null(fdi)) {
  pr <- fdi$projects
  ref_rows[[length(ref_rows) + 1]] <- sprintf(
    "CM-FDI: %d projects (%d manufacturing; %d used by the indices), %d destinations, %d home economies, %d parents, announced %d-%d; %d without a disclosed value; disclosed total USD %.1f bn",
    nrow(pr), sum(pr$manufacturing), sum(pr$index_use), length(unique(pr$dest)), length(unique(pr$home)),
    length(unique(pr$parent)), min(pr$year_announced), max(pr$year_announced), sum(is.na(pr$value)), sum(pr$value, na.rm = TRUE) / 1e3)
  gw <- fdi$groups[fdi$groups$dest == "WLD", , drop = FALSE]
  for (b in SECTOR_FDI_BASES) for (ck in unique(gw$item_key)) {
    x <- gw[gw$basis == b & gw$item_key == ck, , drop = FALSE]
    ref_rows[[length(ref_rows) + 1]] <- sprintf(
      "CM-FDI %s %s: %d projects; by parent HQ (count) china %.1f%% core %.1f%% partner %.1f%% rest %.1f%%; (disclosed value, USD %.1f bn) china %.1f%% core %.1f%% partner %.1f%% rest %.1f%%",
      b, sub("^cell:", "", ck), sum(x$n), 100 * x$share_n[1], 100 * x$share_n[2], 100 * x$share_n[3], 100 * x$share_n[4],
      sum(x$value, na.rm = TRUE) / 1e3, 100 * x$share_value[1], 100 * x$share_value[2], 100 * x$share_value[3], 100 * x$share_value[4])
  }
}
for (tech in names(bundle$trade)) {
  ss <- bundle$trade[[tech]]$sub_summary
  if (is.null(ss)) next
  ss <- ss[ss$var == "trade.sub_exports" & ss$year == 2024L & ss$eu_mode == "members", , drop = FALSE]
  for (ck in unique(ss$item_key)) {
    x <- ss[ss$item_key == ck, , drop = FALSE]
    x <- x[order(-x$W), , drop = FALSE]
    ref_rows[[length(ref_rows) + 1]] <- paste0(sub("^cell:", "", ck), " sub-components 2024 (W USD bn, top-1 share): ",
                                               paste(sprintf("%s %.2f (%s %.0f%%)", x$sub_sector, x$W / 1e9, geo_code(x$top1_geo), 100 * x$top1_share), collapse = "; "))
  }
}
fe <- facts[facts$var == "trade.feasibility", , drop = FALSE]
if (nrow(fe)) {
  for (ck in c("cell:batteries-midstream", "cell:solar-upstream", "cell:semiconductors-upstream")) {
    x <- fe[fe$item_key == ck, , drop = FALSE]
    x <- x[order(-x$value), , drop = FALSE]
    ref_rows[[length(ref_rows) + 1]] <- sprintf("Export feasibility %d %s: %d economies; top %s; median %.3f", feas_year, sub("^cell:", "", ck), nrow(x),
                                                paste(sprintf("%s %.3f", utils::head(x$geo, 3), utils::head(x$value, 3)), collapse = ", "), stats::median(x$value))
  }
}
message("\nReference values (Appendix B, recomputed):\n  ", paste(unlist(ref_rows), collapse = "\n  "))
message("\nHeadlines:")
for (h in bundle$headlines) message("  ", h$id, ": ", paste(names(h$values), signif(unlist(h$values), 4), sep = "=", collapse = ", "), " | ",
                                    paste(names(h$labels), unlist(h$labels), sep = "=", collapse = ", "))
v <- res$validation
message(sprintf("\nValidation: %d PASS, %d WARN, %d FAIL. %.0f s.", sum(v$status == "PASS"), sum(v$status == "WARN"), sum(v$status == "FAIL"),
                as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
if (any(v$status == "WARN")) message("WARN: ", paste(v$check[v$status == "WARN"], v$detail[v$status == "WARN"], sep = " -- ", collapse = "\n      "))
