# Datawrapper versions of the trade-fence explainer charts (outputs/trade_fence_explainer/README.md). Reads the chart
# CSVs in outputs/trade_fence_explainer/data/, builds one spec per chart (R/trade_fence_explainer/datawrapper_specs.R)
# and creates or updates the charts in the NEIS team folder through the Datawrapper API, in the NEIS theme. Chart IDs
# are kept in outputs/trade_fence_explainer/datawrapper_charts.csv, so a rerun updates the same charts instead of
# creating new ones. Charts are left unpublished unless OPSI_DW_PUBLISH=true.
#
# The API token is read from DATAWRAPPER_KEY; when that is not set, from the .Renviron file named by OPSI_RENVIRON.
# Run after 02_draw_charts.R: Rscript scripts/trade_fence_explainer/04_datawrapper.R

fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", ".."), winslash = "/")
source(file.path(repo_root, "R", "trade_fence_explainer", "datawrapper_specs.R"))
cfg <- yaml::read_yaml(Sys.getenv("OPSI_TRADE_FENCE_EXPLAINER", file.path(repo_root, "config", "trade_fence_explainer.yml")))
dwc <- cfg$datawrapper
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
data_dir <- file.path(abs_path(cfg$output_dir), "data")
registry_path <- abs_path(dwc$registry)

if (!nzchar(Sys.getenv("DATAWRAPPER_KEY")) && nzchar(Sys.getenv("OPSI_RENVIRON"))) readRenviron(Sys.getenv("OPSI_RENVIRON"))
key <- Sys.getenv("DATAWRAPPER_KEY")
if (!nzchar(key)) stop("Set DATAWRAPPER_KEY (or OPSI_RENVIRON to the .Renviron that holds it).")

api <- function(path, method = "GET", json = NULL, file = NULL) {
  r <- httr2::request(paste0("https://api.datawrapper.de/v3", path)) |> httr2::req_auth_bearer_token(key) |>
    httr2::req_method(method) |> httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503))
  if (!is.null(json)) r <- httr2::req_body_json(r, json, auto_unbox = TRUE, null = "null", na = "null")
  if (!is.null(file)) r <- httr2::req_body_file(r, file, type = "text/csv; charset=utf-8")
  resp <- httr2::req_perform(r)
  st <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (st >= 300) stop(method, " ", path, " returned ", st, ": ", substr(tryCatch(httr2::resp_body_string(resp), error = function(e) ""), 1, 300))
  body
}
csv_file <- function(df) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(df, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  f
}
rd <- function(f) utils::read.csv(file.path(data_dir, f), check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), encoding = "UTF-8")

info <- rd("build_info.csv")
I <- stats::setNames(info$value, info$item)
dd <- rd("D_domestic_response.csv")
cap <- dd[dd$topic %in% "solar_module_capacity_operating" & grepl("SEIA/Wood Mackenzie, US Solar Market Insight", dd$source_name) & !grepl("as first", dd$period), ]
cap$p <- sub(" \\(.*", "", cap$period)
cap <- cap[!duplicated(cap$p), ]
cap$label <- c(`end-2023` = "End 2023", `end-2024` = "End 2024", `end-2025` = "End 2025", `end-Q2 2026` = "Mid-2026")[cap$p]
cap <- cap[!is.na(cap$label), ]
d <- list(tech = rd("00a_trade_by_technology.csv"), stage = rd("00a_trade_by_stage.csv"), rewiring = rd("00b_trade_rewiring.csv"),
          timeline = rd("00_policy_timeline.csv"), rising = rd("01_fence_rising.csv"), map = rd("02_tariff_map.csv"),
          partners = rd("03_who_gets_through_gate.csv"), tools = rd("04_multi_tool_fence.csv"), audit = rd("05_yards_fences_audit.csv"),
          summary = rd("05b_verdict_summary.csv"), threshold = rd("05_threshold_sensitivity.csv"), dollars = rd("06_dollar_weighted_misalignment.csv"),
          allies = rd("07_why_allies_pay.csv"), solar = rd("08a_solar_trade_response.csv"), capacity = cap,
          battery = rd("08b_battery_trade_response.csv"), ownership = rd("08b_battery_ownership.csv"), fence = rd("fence_change_vs_diversification.csv"))
data_month <- format(as.Date(paste0(I[["data_month"]], "-01")), "%B %Y")
snapshot <- sub("^0", "", format(as.Date(I[["snapshot_date"]]), "%d %B %Y"))
specs <- tfe_dw_specs(d, period = "January-August", data_month = data_month, snapshot = snapshot)

reg <- if (file.exists(registry_path)) utils::read.csv(registry_path, colClasses = "character") else
  data.frame(key = character(), id = character(), title = character(), type = character(), edit_url = character(), status = character())
publish <- tolower(Sys.getenv("OPSI_DW_PUBLISH", "false")) %in% c("1", "true", "yes")
for (s in specs) {
  id <- reg$id[match(s$key, reg$key)]
  if (is.na(id) || !nzchar(id)) {
    ch <- api("/charts", "POST", list(title = s$title, type = s$type, theme = dwc$theme, folderId = dwc$folder_id))
    id <- ch$id
    message("created ", s$key, " -> ", id)
  } else {
    message("updating ", s$key, " (", id, ")")
  }
  api(paste0("/charts/", id, "/data"), "PUT", file = csv_file(s$data))
  api(paste0("/charts/", id), "PATCH", list(title = s$title, type = s$type, theme = dwc$theme, folderId = dwc$folder_id, metadata = s$metadata))
  status <- "draft"
  if (publish) { api(paste0("/charts/", id, "/publish"), "POST"); status <- "published" }
  row <- data.frame(key = s$key, id = id, title = s$title, type = s$type, edit_url = sprintf("https://app.datawrapper.de/chart/%s/visualize", id), status = status)
  reg <- rbind(reg[reg$key != s$key, ], row)
}
reg <- reg[match(vapply(specs, function(s) s$key, character(1)), reg$key), ]
utils::write.csv(reg, registry_path, row.names = FALSE)
message("Datawrapper: ", nrow(reg), " charts in folder ", dwc$folder_id, " (", dwc$folder_name, "); registry ", registry_path)
