# Trade War Tracker inputs of scripts/28_build_us_trade_restrictiveness.R, from the tracker's public GitHub
# repositories (MIT licence; data from the US Census Bureau International Trade API):
#   how-restrictive-us-trade   data/country-list.csv (the 30 partners), data/imports-hs10/<code>data-current.parquet
#                              (HS10 x month import value and calculated duty for each partner and TOTAL, from
#                              2013) and data/metrics/country.parquet (the published TRI, for the reproduction check);
#   trade-war-redux-2025       data/top_50_non_eu.csv, data/<year>-hs10-imports<code>.parquet (weight-year HS10
#                              imports of its other partners, which the build splits out of the rest of world)
#                              and the Section 232 product lists in tariff-lists/.
# Files are written to <dir>/<path in the repository>, so a full clone of either repository serves as well.
# Each download goes to a temporary file first and replaces the target only when complete; fetch_info.csv in
# each directory records the commit the files came from.
#
# Defines functions only (scripts/utils/fetchers.R sources every file in scripts/fetchers/); needs
# opsi_http_get(), opsi_http_get_json() and opsi_fetch_enabled() from scripts/utils/fetchers.R.

twt_raw_url <- function(repo, ref, path) sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, ref, path)

twt_fetch_file <- function(repo, ref, path, dest_dir, overwrite = FALSE) {
  dest <- file.path(dest_dir, path)
  if (file.exists(dest) && !overwrite) return(invisible(dest))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  bytes <- opsi_http_get(twt_raw_url(repo, ref, path), timeout_seconds = 900)
  tmp <- paste0(dest, ".part")
  writeBin(bytes, tmp)
  if (!file.rename(tmp, dest)) {
    file.copy(tmp, dest, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(dest)
}

twt_record_commit <- function(repo, ref, dest_dir) {
  sha <- tryCatch(opsi_http_get_json(sprintf("https://api.github.com/repos/%s/commits/%s", repo, ref))$sha,
                  error = function(e) NA_character_)
  utils::write.csv(data.frame(repo = repo, ref = ref, commit = sha, fetched_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
                   file.path(dest_dir, "fetch_info.csv"), row.names = FALSE)
  invisible(sha)
}

# Download whatever the build needs and is missing (everything when `refresh`). `cfg` is
# config/us_trade_restrictiveness.yml. Returns the two directories.
fetch_trade_war_tracker <- function(tri_dir, redux_dir, cfg, refresh = FALSE) {
  tk <- cfg$tracker
  ref <- if (is.null(tk$ref)) "main" else tk$ref
  get_tri <- function(path) twt_fetch_file(tk$tri_repo, ref, path, tri_dir, overwrite = refresh)
  get_redux <- function(path) twt_fetch_file(tk$redux_repo, ref, path, redux_dir, overwrite = refresh)

  get_tri("data/country-list.csv")
  codes <- utils::read.csv(file.path(tri_dir, "data", "country-list.csv"), header = FALSE, colClasses = "character")[[1]]
  for (code in c(codes, "TOTAL")) get_tri(file.path("data", "imports-hs10", paste0(code, "data-current.parquet")))
  get_tri("data/metrics/country.parquet")

  get_redux("data/top_50_non_eu.csv")
  partners <- utils::read.csv(file.path(redux_dir, "data", "top_50_non_eu.csv"), colClasses = "character")$CTY_CODE
  for (code in setdiff(partners, codes)) {
    get_redux(file.path("data", paste0(cfg$weight_year, "-hs10-imports", code, ".parquet")))
  }
  for (f in unlist(tk$section232_lists)) get_redux(file.path("tariff-lists", f))

  if (refresh || !file.exists(file.path(tri_dir, "fetch_info.csv"))) twt_record_commit(tk$tri_repo, ref, tri_dir)
  if (refresh || !file.exists(file.path(redux_dir, "fetch_info.csv"))) twt_record_commit(tk$redux_repo, ref, redux_dir)
  invisible(c(tri_dir = tri_dir, redux_dir = redux_dir))
}
