# UN Comtrade public preview API (no key) downloads for the trade-fence explainer: US goods trade by partner in
# 2024 (chart 3 bubble size) and US monthly imports of HS 854143 (PV modules) by partner (chart 8 origins the
# Trade War Tracker's 30-partner files do not separate). Comtrade's US figures are the US Census Bureau's,
# reported to the UN. Writes into data/raw/comtrade_us_partner_2024/ and data/raw/comtrade_us_monthly/; files
# already present are kept unless OPSI_COMTRADE_REFRESH=true. The preview API allows one period per call.
#
# Usage: Rscript scripts/trade_fence_explainer/fetch_comtrade.R [first YYYYMM] [last YYYYMM]

args <- commandArgs(trailingOnly = TRUE)
first <- if (length(args) >= 1) args[1] else "202401"
last <- if (length(args) >= 2) args[2] else format(Sys.Date() - 60, "%Y%m")
fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", ".."), winslash = "/")
refresh <- tolower(Sys.getenv("OPSI_COMTRADE_REFRESH", "false")) %in% c("1", "true", "yes")
base <- "https://comtradeapi.un.org/public/v1/preview/C"
get <- function(url, dest) {
  if (file.exists(dest) && !refresh) return(invisible(TRUE))
  for (attempt in 1:3) {
    ok <- tryCatch({
      utils::download.file(url, dest, quiet = TRUE, mode = "wb", headers = c(`User-Agent` = "Mozilla/5.0 (research script)"))
      n <- NROW(jsonlite::fromJSON(dest)$data)
      n > 0
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(invisible(TRUE))
    Sys.sleep(4 * attempt)
  }
  warning("No records for ", url)
}
d1 <- file.path(repo_root, "data", "raw", "comtrade_us_partner_2024")
dir.create(d1, recursive = TRUE, showWarnings = FALSE)
get(paste0(base, "/A/HS?reporterCode=842&period=2024&flowCode=M,X&cmdCode=TOTAL&partner2Code=0&customsCode=C00&motCode=0"),
    file.path(d1, "us_partner_trade_2024.json"))
d2 <- file.path(repo_root, "data", "raw", "comtrade_us_monthly")
dir.create(d2, recursive = TRUE, showWarnings = FALSE)
months <- format(seq(as.Date(paste0(first, "01"), "%Y%m%d"), as.Date(paste0(last, "01"), "%Y%m%d"), by = "month"), "%Y%m")
for (p in months) {
  get(sprintf("%s/M/HS?reporterCode=842&period=%s&flowCode=M&cmdCode=854143&partner2Code=0&customsCode=C00&motCode=0", base, p),
      file.path(d2, sprintf("854143_%s.json", p)))
  Sys.sleep(1.5)
}
message("Comtrade files in ", d1, " and ", d2)
