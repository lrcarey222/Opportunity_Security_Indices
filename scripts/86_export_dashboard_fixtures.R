# Parity fixtures of the Allied Industrial Base Explorer's TypeScript engine.
#
#   Rscript scripts/86_export_dashboard_fixtures.R [--set synthetic|real|both|numeric] [--out <dir>]
#                                                  [--scenarios S00,S05] [--skip-network]
#                                                  [--full-arrays]
#
# Runs the R reference engine (R/dashboard) on a bundle for the fixture scenarios of
# dash_fixture_scenarios() and writes, per scenario, the output of every layer as an OSB1 container
# (dash_osb_encode(kind = "fixture"), gzipped) that the vitest parity tests in
# dashboard/src/engine/__tests__/parity read (spec_react.md section 5 and 6.9).
#
#   <out>/numeric.json                 R numerics (long-double sums, mean, median, round, format,
#                                      median_scurve) for the engine's numeric unit tests
#   <out>/synthetic/s1/, s2/           the synthetic bundles: data/ in deploy format (manifest.json +
#                                      chunks, the same dash_web_chunks() as scripts/85), index.json
#                                      and <scenario>.osb; s1 = dash_fixture_bundle(seed = 1),
#                                      s2 = dash_fixture_bundle(seed = 2) with inputs rounded to one
#                                      decimal (heavy ties). Small; committed; run by CI.
#   <out>/real/                        index.json and <scenario>.osb computed on
#                                      <processed_dir>/dashboard/bundle.rds, the bundle scripts/85
#                                      exported to dashboard/public/data/ (refused when their
#                                      bundle_id differ). Kept small: the FS / PO index arrays as
#                                      per-cell statistics plus 20,000 sampled cells (in full only
#                                      with --full-arrays, S00, +25 MB), the PSI dyads of 24
#                                      reporters (all in S00 and S13), and every object identical to
#                                      S00's stored as a reference to it. Gitignored; npm run
#                                      test:parity.
#
# <out> defaults to <repo>/dashboard/src/engine/__fixtures__. Rerun after scripts/85 (the real set
# must match the shipped bundle) and after any change to R/dashboard.

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
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

set_arg <- arg_value("--set", "both")
if (!set_arg %in% c("synthetic", "real", "both", "numeric")) stop("--set must be synthetic, real, both or numeric.")
skip_network <- arg_flag("--skip-network")
full_arrays <- arg_flag("--full-arrays")
only <- arg_value("--scenarios", NULL)
only <- if (is.null(only)) NULL else trimws(strsplit(only, ",", fixed = TRUE)[[1]])

source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
for (f in dash_engine_files()) source(file.path(repo_root, f))
source(file.path(repo_root, "R", "dashboard", "dash_scenarios.R"))

config <- getOption("opportunity_security.config")
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)
out_root <- abs_path(arg_value("--out", file.path("dashboard", "src", "engine", "__fixtures__")))
processed_dir <- abs_path(config$processed_dir)
public_data <- abs_path(file.path("dashboard", "public", "data"))
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# ---- IO helpers ------------------------------------------------------------------------------------

write_gz <- function(bytes, path) {
  con <- gzfile(path, "wb", compression = 9)
  writeBin(bytes, con)
  close(con)
  con <- gzfile(path, "rb")
  back <- readBin(con, "raw", n = length(bytes) + 1L)
  close(con)
  if (!identical(back, bytes)) stop(path, ": the gzip file does not read back the encoded bytes.")
  invisible(file.size(path))
}

write_json_file <- function(x, path) writeBin(charToRaw(enc2utf8(dash_json_text(x, pretty = TRUE))), path)

# A bundle in the deploy format of scripts/85 (manifest.json + gzipped OSB chunks named by md5).
write_web_bundle <- function(bundle, res, out_dir, created) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  old <- list.files(out_dir, pattern = "^(core|vars|partner|network)\\.[0-9a-f]{8}\\.osb$", full.names = TRUE)
  if (length(old)) file.remove(old)
  chunks <- dash_web_chunks(bundle, res)
  rows <- list()
  for (nm in names(chunks)) {
    bytes <- dash_osb_encode(chunks[[nm]], nm, "chunk")
    tmp <- file.path(out_dir, paste0(nm, ".tmp.gz"))
    write_gz(bytes, tmp)
    md5 <- unname(tools::md5sum(tmp))
    file_name <- dash_web_chunk_file(nm, md5)
    dest <- file.path(out_dir, file_name)
    if (file.exists(dest)) file.remove(dest)
    if (!file.rename(tmp, dest)) stop("Could not write ", dest)
    if (!identical(dash_osb_decode(bytes)$objects, dash_osb_normalize(chunks[[nm]]))) stop("Chunk ", nm, " does not round-trip identical().")
    rows[[nm]] <- tibble::tibble(name = nm, file = file_name, bytes = length(bytes), bytes_gz = file.size(dest), md5 = md5, initial = nm == "core")
  }
  chunk_info <- dplyr::bind_rows(rows)
  id_file <- tempfile(fileext = ".txt")
  writeBin(charToRaw(paste0(paste(sort(chunk_info$file), collapse = "\n"), "\n")), id_file)
  bundle_id <- unname(tools::md5sum(id_file))
  unlink(id_file)
  paper <- list(title = "Synthetic fixture bundle", url = NA_character_, pdf = NA_character_)
  man0 <- dash_web_manifest(bundle, chunk_info, NULL, bundle_id, created, paper)
  manifest_gz <- length(memCompress(charToRaw(enc2utf8(dash_json_text(man0, pretty = TRUE))), "gzip")) + 18
  budget <- dash_web_budget(chunk_info, manifest_gz)
  write_json_file(dash_web_manifest(bundle, chunk_info, budget, bundle_id, created, paper), file.path(out_dir, "manifest.json"))
  list(bundle_id = bundle_id, chunks = chunk_info)
}

# One fixture set: every scenario through dash_fixture_outputs(), one gzipped OSB file each.
# refs = TRUE: objects identical to S00's are written as references to S00 (dash_fixture_refs()).
write_fixture_set <- function(bundle, set_name, out_dir, bundle_id, base, opts_fun, refs = FALSE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  scen <- dash_fixture_scenarios(bundle)
  if (!is.null(only)) scen <- Filter(function(s) s$id %in% only, scen)
  if (refs && !any(vapply(scen, function(s) s$id == "S00", logical(1)))) {
    step("  (no references: S00 is not among the scenarios written)")
    refs <- FALSE
  }
  ref_objects <- NULL
  index_rows <- list()
  for (s in scen) {
    t0 <- proc.time()[["elapsed"]]
    opts <- opts_fun(s$id)
    fo <- dash_fixture_outputs(bundle, s, base, opts)
    if (refs && s$id == "S00") ref_objects <- fo$objects
    if (refs && s$id != "S00") fo$objects <- dash_fixture_refs(fo$objects, ref_objects, "S00")
    n_refs <- dash_fixture_count_refs(fo$objects)
    bytes <- dash_osb_encode(fo$objects, s$id, "fixture", meta = list(set = set_name, scenario = s$id, bundle_id = bundle_id))
    file <- paste0(s$id, ".osb")
    gz <- write_gz(bytes, file.path(out_dir, file))
    el <- proc.time()[["elapsed"]] - t0
    step(sprintf("  %s %-40s %6.2f MB, %5.2f MB gz, %5.1f s%s", set_name, paste(s$id, s$label), length(bytes) / 2^20, gz / 2^20, el,
                 if (n_refs) sprintf(", %d refs to S00", n_refs) else ""))
    index_rows[[length(index_rows) + 1]] <- list(
      id = s$id, label = s$label, exercises = s$exercises,
      dials = s$dials, advanced = dash_json_value(s$advanced), file = file,
      layers = I(names(fo$objects)), options = opts, refs = n_refs,
      elapsed_sec = as.list(round(fo$timings, 3)), total_sec = round(el, 3)
    )
    rm(fo, bytes)
    invisible(gc())
  }
  index <- list(
    set = set_name,
    bundle_id = bundle_id,
    r_version = R.version.string,
    platform = R.version$platform,
    long_double = isTRUE(capabilities("long.double")),
    created = dash_osb_format_time(Sys.time()),
    generator = "scripts/86_export_dashboard_fixtures.R",
    share_rank_digits = DASH_FIXTURE_SHARE_RANK_DIGITS,
    scenarios = index_rows
  )
  index_json <- jsonlite::toJSON(index, auto_unbox = TRUE, digits = I(17), na = "null", null = "null", pretty = TRUE)
  writeBin(charToRaw(enc2utf8(as.character(index_json))), file.path(out_dir, "index.json"))
  invisible(index)
}

# ---- Numeric fixture -------------------------------------------------------------------------------

write_numeric_fixture <- function(path) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  set.seed(20261008)
  vecs <- lapply(1:300, function(i) {
    n <- sample(c(1:12, 50, 200), 1)
    switch(i %% 5 + 1,
           stats::runif(n),
           stats::rnorm(n) * 10^sample(-5:5, n, replace = TRUE),
           c(1, stats::runif(n - 1) * 2^-sample(40:70, n - 1, replace = TRUE)) * sample(c(-1, 1), n, replace = TRUE),
           round(stats::runif(n), 3),
           (stats::runif(n) - 0.5) * 2^sample(-60:60, n, replace = TRUE))
  })
  m <- matrix(c(stats::runif(60), stats::rnorm(60) * 1e-8), 30, 4)
  m[sample(length(m), 10)] <- NA
  rx <- c(stats::runif(200), (1:50) / 7, 0.1234567895, 0.0000000005, 1.0000000005, 0.15, 0.25, 0.35)
  taus <- c(0.25, 0.3, 0.35, 0.5, 1, 0, 1 / 3, 0.05, 0.123456789, 1e-5, 123456789, 0.1 + 0.2)
  gx <- c(0.123456, 1.5, 2.25, 0.000123, 1234.5, 0.0625, 0.4, 0.33333, 4, 0.25, 12.345, 1e-7)
  f2x <- c(gx, 0.125, 0.375, 2.675, 1.005)
  sc_in <- list(c(3, 1, 2, NA, 2, 5), c(1, 1, 1, 1), c(NA, 4), 0.5, c(stats::runif(30), rep(0.25, 10), NA))
  # x^0.5 as median_scurve() computes it (R_pow -> powl on x86-64: double rounding), on two roots
  # where it differs from the correctly rounded sqrt, an S-curve grid and full-mantissa values
  px <- c(16740 / 45155, 13510.5 / 47305, (0:999) / 999, stats::runif(2000) + stats::runif(2000) * 2^-32)
  if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv())
  num <- function(x) ifelse(is.na(x), "null", sprintf("%.17g", x))
  arr <- function(x) paste0("[", paste(num(x), collapse = ","), "]")
  strs <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")
  lst <- function(l) paste0("[", paste(vapply(l, arr, ""), collapse = ","), "]")
  fields <- c(
    vecs = lst(vecs),
    sums = arr(vapply(vecs, sum, 0)), means = arr(vapply(vecs, mean, 0)), medians = arr(vapply(vecs, stats::median, 0)),
    sds = arr(vapply(vecs, function(v) if (length(v) > 1) stats::sd(v) else NA_real_, 0)),
    mat = arr(as.vector(m)), mat_nrow = "30", mat_ncol = "4",
    rowSums = arr(rowSums(m, na.rm = TRUE)), colSums = arr(colSums(m, na.rm = TRUE)), rowMeans = arr(rowMeans(m, na.rm = TRUE)),
    rx = arr(rx), r9 = arr(round(rx, 9)), r0 = arr(round(rx * 20 - 3, 0)), r2 = arr(round(rx, 2)),
    taus = arr(taus), fmt = strs(vapply(taus, format, "")),
    gx = arr(gx), g3 = strs(sprintf("%.3g", gx)), f2x = arr(f2x), f2 = strs(sprintf("%.2f", f2x)),
    sc_in = lst(sc_in), sc_out = lst(lapply(sc_in, median_scurve)),
    px = arr(px), p_half = arr(px^0.5)
  )
  txt <- paste0("{\n", paste0('"', names(fields), '":', fields, collapse = ",\n"), "\n}\n")
  writeBin(charToRaw(txt), path)
  step("numeric.json (", round(file.size(path) / 1024), " kB)")
}

# ---- Sets ------------------------------------------------------------------------------------------

if (set_arg %in% c("synthetic", "both", "numeric")) write_numeric_fixture(file.path(out_root, "numeric.json"))

if (set_arg %in% c("synthetic", "both")) {
  synth <- list(
    s1 = function() dash_fixture_bundle(seed = 1),
    s2 = function() dash_fixture_tie_bundle(dash_fixture_bundle(seed = 2, policy_required = "IEA PAMS Policy Index"), digits = 1)
  )
  for (nm in names(synth)) {
    step("Synthetic set ", nm)
    bundle <- synth[[nm]]()
    dash_validate_bundle(bundle)
    base <- dash_fixture_baseline(bundle, network = !skip_network)
    if (!is.null(base$network)) {
      bundle$network$greedy_baseline <- base$network
    }
    dir <- file.path(out_root, "synthetic", nm)
    web <- write_web_bundle(bundle, base$result, file.path(dir, "data"), created = bundle$manifest$created)
    old <- list.files(dir, pattern = "^S[0-9]+\\.osb$", full.names = TRUE)
    if (is.null(only) && length(old)) file.remove(old)
    write_fixture_set(bundle, paste0("synthetic/", nm), dir, web$bundle_id, base, function(id) {
      list(full_arrays = TRUE, contrib = TRUE, var = TRUE, sample = FALSE, network = !skip_network, n_decompose = 2L)
    })
  }
}

if (set_arg %in% c("real", "both")) {
  bundle_path <- file.path(processed_dir, "dashboard", "bundle.rds")
  man_path <- file.path(public_data, "manifest.json")
  if (!file.exists(bundle_path) || !file.exists(man_path)) {
    msg <- paste0("Real set skipped: run scripts/85_export_dashboard_data.R first (needs ", bundle_path, " and ", man_path, ").")
    if (set_arg == "real") stop(msg) else step(msg)
  } else {
    step("Real set (", bundle_path, ")")
    bundle <- readRDS(bundle_path)
    web_man <- jsonlite::fromJSON(man_path, simplifyVector = FALSE)
    bid <- bundle$manifest$bundle_id
    if (is.null(bid) || !identical(bid, web_man$bundle_id)) {
      stop("bundle.rds (bundle_id ", if (is.null(bid)) "none" else bid, ") is not the bundle in ", man_path, " (", web_man$bundle_id,
           "): rerun scripts/85_export_dashboard_data.R.")
    }
    base <- dash_fixture_baseline(bundle, network = !skip_network)
    old <- list.files(file.path(out_root, "real"), pattern = "^S[0-9]+[.]osb$", full.names = TRUE)
    if (is.null(only) && length(old)) file.remove(old)
    write_fixture_set(bundle, "real", file.path(out_root, "real"), bid, base, function(id) {
      list(full_arrays = full_arrays && id == "S00", contrib = id %in% c("S00", "S14"), var = id %in% c("S00", "S14"),
           sample = TRUE, network = !skip_network, n_decompose = 3L,
           dyad_reporters = if (id %in% c("S00", "S13")) NULL else 24L)
    }, refs = TRUE)
  }
}

step(sprintf("Done in %.0f s.", as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
