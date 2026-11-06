# Where Japan's critical mineral imports come from, by material.
#
# Evidence for the claim that Japan's exposure to China varies by material: China
# supplies most of Japan's diammonium phosphate and lithium chemicals and a large share
# of its rare earths, vanadium and silicon, while nickel, cobalt and manganese come
# mainly from Indonesia, Finland and South Africa. One stacked bar per material:
#   China | largest other supplier | all other suppliers   (percent of import value)
#
# What the chart cannot show. Import shares are by country of origin as Japan's customs
# records it, so a mineral refined in China and shipped from a third country - or mined
# elsewhere and processed in China before re-export - is not attributed to China. The
# processing-concentration figures that make that point (rare-earth separation
# capacity, refining shares) come from the IEA, not from trade data.
#
# Data
# ----
# UN Comtrade, Japan (reporter 392) imports, 2024, by partner, value in USD (CIF).
# Pulled from the Comtrade API - the repo's staged Comtrade files are partner-World
# only, and the one bilateral pull (allied_comtrade_energy_data.csv) does not carry
# rare earths, DAP, low-grade silicon or the ferro-alloys. Uses the keyed endpoint
# when COMTRADE_API_KEY is set and the free preview endpoint otherwise (no key, 500
# rows a call, which is why codes are requested a few at a time). The pull is cached
# under <processed_dir>/charts/cache/ and reused unless JCI_REFRESH=true.
#
# Materials and HS6 codes
# -----------------------
# Groupings follow the HS codes the Columbia University critical minerals dashboard
# lists for each material (data/raw/Columbia University Critical Minerals Dashboard/
# unique_comtrade.csv). With these groupings the 2024 shares reproduce the paragraph's
# figures to the decimal for all materials except nickel (28.7% Indonesia here, 29.2%
# in the text), whose exact grouping could not be recovered. Broad headings are flagged:
#   * Vanadium includes 8112.92/8112.99, which also hold gallium, germanium, indium
#     and niobium.
#   * Cobalt includes 2827.39, "chlorides, other", which holds cobalt chloride among
#     many others.
#   * Silicon includes ferro-silicon, silicon carbide and ferro-silico-chromium as well
#     as silicon metal.
#
# Output (data/processed/charts/):
#   japan_critmin_import_sources_<year>_wide.csv
# Datawrapper-ready for one stacked bar chart: `Material`, then the three share
# columns (summing to 100), then tooltip-only columns - the largest other supplier's
# name, total import value and the HS codes. Rows are in the paragraph's order.
#
# Run:
#   Rscript R/charts/japan_critmin_import_sources.R
#
# Environment:
#   JCI_YEAR      trade year (default 2024)
#   JCI_REFRESH   "true" to re-pull from Comtrade instead of using the cache

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

## Configuration -------------------------------------------------------------

JCI_DEFAULT_YEAR <- 2024L
JCI_REPORTER <- 392L      # Japan
JCI_CHINA <- 156L

JCI_PREVIEW_API <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"
JCI_KEYED_API <- "https://comtradeapi.un.org/data/v1/get/C/A/HS"
JCI_PARTNERS_REF <- "https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json"
JCI_BATCH <- 4L           # codes per preview call; ~70 partners each stays under 500 rows
JCI_ROW_LIMIT <- 500L

# One row per bar, in the paragraph's order: China-dominated materials first, then
# the three that are sourced elsewhere.
JCI_MATERIALS <- tibble::tribble(
  ~material,                    ~hs,
  "Diammonium phosphate",       "310530",
  "Lithium",                    "282520|283691",
  "Rare earths",                "280530|284610|284690",
  "Vanadium",                   "282530|720292|261590|811292|811299",
  "Silicon",                    "280461|280469|720221|720229|720250|284920",
  "Nickel",                     "260400|282540|282735|283324|720260|750110|750120|750210|750220|750300|750400|750512|750522|750610|750620|750712|750890",
  "Cobalt",                     "260500|282200|282739|810520|810530|810590",
  "Manganese",                  "260200|282010|282090|720211|720219|720230|811100"
)

## Helpers -------------------------------------------------------------------

jci_split <- function(x) {
  out <- strsplit(x, "|", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

jci_http_get <- function(url, headers = character(), retries = 3L, backoff_seconds = 5) {
  if (!requireNamespace("curl", quietly = TRUE)) stop("Package 'curl' is required.")
  last_error <- "unknown error"
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setheaders(h, .list = as.list(c("User-Agent" = "OSI-pipeline/1.0", headers)))
      curl::handle_setopt(h, timeout = 120, connecttimeout = 60, followlocation = TRUE)
      r <- curl::curl_fetch_memory(url, handle = h)
      if (r$status_code >= 400L) stop("HTTP ", r$status_code)
      rawToChar(r$content)
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(result)) return(result)
    # The preview endpoint rate-limits bursts; back off before retrying.
    if (attempt < retries) Sys.sleep(backoff_seconds * attempt)
  }
  stop("Comtrade request failed after ", retries, " attempts: ", last_error, "\n  ", url)
}

## Comtrade pull -------------------------------------------------------------

jci_pull_batch <- function(codes, year, key = "") {
  base <- if (nzchar(key)) JCI_KEYED_API else JCI_PREVIEW_API
  url <- sprintf(
    "%s?reporterCode=%d&period=%d&cmdCode=%s&flowCode=M&partner2Code=0&customsCode=C00&motCode=0",
    base, JCI_REPORTER, year, paste(codes, collapse = ",")
  )
  headers <- if (nzchar(key)) c("Ocp-Apim-Subscription-Key" = key) else character()
  parsed <- jsonlite::fromJSON(jci_http_get(url, headers), simplifyVector = TRUE)

  if (!is.null(parsed$error) && nzchar(parsed$error)) stop("Comtrade error: ", parsed$error)
  data <- parsed$data
  if (!nzchar(key) && NROW(data) >= JCI_ROW_LIMIT) {
    stop("Preview call hit its ", JCI_ROW_LIMIT, "-row cap for ", paste(codes, collapse = ","),
         "; lower JCI_BATCH.")
  }
  if (NROW(data) == 0) return(data.frame(cmd_code = character(), partner_code = integer(),
                                         value = numeric()))
  data.frame(cmd_code = as.character(data$cmdCode), partner_code = as.integer(data$partnerCode),
             value = as.numeric(data$primaryValue), stringsAsFactors = FALSE)
}

jci_partner_names <- function() {
  ref <- jsonlite::fromJSON(jci_http_get(JCI_PARTNERS_REF), simplifyVector = TRUE)$results
  data.frame(partner_code = as.integer(ref$PartnerCode), partner = ref$PartnerDesc,
             stringsAsFactors = FALSE)
}

# Japan's imports of every configured code, by partner, cached on disk.
jci_bilateral <- function(repo_root, year, refresh = FALSE) {
  cache_dir <- file.path(repo_root, config$processed_dir, "charts", "cache")
  cache <- file.path(cache_dir, sprintf("comtrade_jpn_critmin_bilateral_%d.csv", year))
  codes <- unique(unlist(lapply(JCI_MATERIALS$hs, jci_split)))

  if (file.exists(cache) && !refresh) {
    out <- utils::read.csv(cache, colClasses = c(cmd_code = "character"), stringsAsFactors = FALSE)
    if (all(codes %in% out$cmd_code)) {
      message("Using cached Comtrade pull: ", cache)
      return(out)
    }
    message("Cache is missing codes; re-pulling.")
  }

  key <- Sys.getenv("COMTRADE_API_KEY", "")
  message("Pulling Japan ", year, " imports from Comtrade (",
          if (nzchar(key)) "keyed" else "preview", " endpoint) ...")
  batches <- split(codes, ceiling(seq_along(codes) / JCI_BATCH))
  parts <- lapply(seq_along(batches), function(i) {
    if (i > 1 && !nzchar(key)) Sys.sleep(2)
    jci_pull_batch(batches[[i]], year, key)
  })
  out <- do.call(rbind, parts)
  out <- merge(out, jci_partner_names(), by = "partner_code", all.x = TRUE)
  out$partner[is.na(out$partner)] <- paste0("Partner ", out$partner_code[is.na(out$partner)])
  out <- out[order(out$cmd_code, -out$value), c("cmd_code", "partner_code", "partner", "value")]

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  utils::write.csv(out, cache, row.names = FALSE)
  message("Cached ", nrow(out), " rows to ", cache)
  out
}

## Shares --------------------------------------------------------------------

# One row per material: China's share, the largest non-China supplier and its share,
# and the remainder, all against Japan's reported World total for those codes.
jci_shares <- function(bilateral, materials = JCI_MATERIALS, china = JCI_CHINA) {
  rows <- lapply(seq_len(nrow(materials)), function(i) {
    codes <- jci_split(materials$hs[i])
    d <- bilateral[bilateral$cmd_code %in% codes, , drop = FALSE]

    missing <- setdiff(codes, d$cmd_code[d$partner_code == 0L])
    if (length(missing) == length(codes)) {
      stop("No Japan imports reported for any ", materials$material[i], " code.")
    }

    world <- sum(d$value[d$partner_code == 0L])
    partners <- d[d$partner_code != 0L, , drop = FALSE]
    by_partner <- stats::aggregate(value ~ partner_code + partner, data = partners, FUN = sum)
    by_partner <- by_partner[order(-by_partner$value), , drop = FALSE]

    china_value <- sum(by_partner$value[by_partner$partner_code == china])
    others <- by_partner[by_partner$partner_code != china, , drop = FALSE]
    top <- if (nrow(others) > 0) others[1, ] else data.frame(partner = NA, value = 0)

    data.frame(
      material = materials$material[i],
      china = 100 * china_value / world,
      top_other = 100 * top$value / world,
      rest = 100 * (world - china_value - top$value) / world,
      top_other_name = top$partner,
      import_value_m = world / 1e6,
      codes_reported = sum(codes %in% d$cmd_code[d$partner_code == 0L]),
      hs = paste(codes, collapse = " "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

jci_wide <- function(shares, digits = 1L) {
  out <- shares
  out$china <- round(out$china, digits)
  out$top_other <- round(out$top_other, digits)
  # Computed from the rounded parts so each row's bars sum to exactly 100.
  out$rest <- round(100 - out$china - out$top_other, digits)
  out$import_value_m <- round(out$import_value_m, 1)
  out <- out[, c("material", "china", "top_other", "rest", "top_other_name",
                 "import_value_m", "hs")]
  names(out) <- c("Material", "China", "Largest other supplier", "All other suppliers",
                  "Largest other supplier (name)", "Japan imports (USD m)", "HS6 codes")
  rownames(out) <- NULL
  out
}

## Runner --------------------------------------------------------------------

run_japan_critmin_import_sources <- function(repo_root, year = JCI_DEFAULT_YEAR,
                                             refresh = FALSE, write_output = TRUE) {
  bilateral <- jci_bilateral(repo_root, year, refresh)
  shares <- jci_shares(bilateral)

  for (i in seq_len(nrow(shares))) {
    message(sprintf("  %-22s China %5.1f%%  | %-28s %5.1f%%  | $%7.1fm",
                    shares$material[i], shares$china[i], shares$top_other_name[i],
                    shares$top_other[i], shares$import_value_m[i]))
  }
  wide <- jci_wide(shares)
  if (!write_output) return(invisible(wide))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  path <- file.path(out_dir, sprintf("japan_critmin_import_sources_%d_wide.csv", year))
  utils::write.csv(wide, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(wide)
}

opsi_japan_critmin_sources_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_critmin_import_sources.R")
}

if (opsi_japan_critmin_sources_run_directly()) {
  invisible(run_japan_critmin_import_sources(
    repo_root = repo_root,
    year = as.integer(Sys.getenv("JCI_YEAR", as.character(JCI_DEFAULT_YEAR))),
    refresh = tolower(Sys.getenv("JCI_REFRESH", "false")) %in% c("1", "true", "yes")
  ))
}
