# Chart of the Day: pick, build and export one chart per day (config/chart_of_day.yml; pure code in R/chart_of_day/).
#
# Writes <output.dir>/<date>.json (the chart, its data and its pick) and <output.dir>/index.json (every day so far),
# which the Explorer reads (/chart-of-the-day and the Home strip). With --png it then exports each new day as a PNG
# drawn by the Explorer itself, in the dashboard's NEIS style (dashboard/scripts/chart-of-day-png.mjs: Vite +
# headless Edge), into OPSI_COD_PNG_DIR if set, else output.png_dir. --datawrapper (optional, older route) creates or updates an unpublished draft per
# day in the NEIS team's "Chart of the Day" Datawrapper folder.
#
# Usage:
#   Rscript scripts/chart_of_day/01_chart_of_day.R --processed-dir <data/processed> [--current-dyads <rds>]
#                [--hs6-master <csv>] [--start 2026-11-18] [--days 7] [--force] [--png] [--datawrapper] [--mix]
# --processed-dir defaults to OPSI_PROCESSED_DIR, then to data/processed in this repo.
# --current-dyads: the latest-12-month HS6 dyads (current_trade_dyads_tbl.rds, written by
#   scripts/38_build_current_import_exposure.R from the scripts/37 monthly Comtrade pull); defaults to
#   OPSI_CURRENT_DYADS, then <processed-dir>/current_trade_dyads_tbl.rds. --hs6-master: the stage crosswalk
#   (energy_hs6_master.csv); defaults to OPSI_HS6_MASTER, then <processed-dir>/../raw/energy_hs6_master.csv. Without
#   them every trade chart falls back to 2024 annual data (and says so).
# --raw-dir (default OPSI_RAW_DIR, then <processed-dir>/../raw) holds the non-trade sources: the Energy Institute
#   Statistical Review (ei_stat_review_world_energy.csv), the Clean Investment Monitor
#   (GCIM_Investment_Capacity_aggregated.xlsx) and the Atlas HS92 panel (hs92_country_product_year_6.csv, ~1 GB: its
#   basket aggregate is cached in outputs/chart_of_day/cache/ and rebuilt when the file changes). Policy measures and
#   FDI projects come from <processed-dir> (nipo_stage_measures/links, cm_fdi_projects). A missing source just
#   switches its chart types off.
# Days already in the index are kept as they are (the history drives the cooldowns) unless --force. --mix prints the
# expected focus-country mix of the weights and stops. The Datawrapper token is read from DATAWRAPPER_KEY, or from the
# .Renviron file named by OPSI_RENVIRON.

fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", ".."), winslash = "/")
for (f in list.files(file.path(repo_root, "R", "chart_of_day"), pattern = "[.]R$", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) { i <- match(paste0("--", name), args); if (is.na(i)) default else args[i + 1] }
flag <- function(name) paste0("--", name) %in% args

cfg <- yaml::read_yaml(Sys.getenv("OPSI_CHART_OF_DAY", file.path(repo_root, "config", "chart_of_day.yml")))
processed <- opt("processed-dir", Sys.getenv("OPSI_PROCESSED_DIR", file.path(repo_root, "data", "processed")))
start <- as.Date(opt("start", as.character(Sys.Date())))
days <- as.integer(opt("days", "7"))
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
out_dir <- abs_path(cfg$output$dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tables <- c("trade_concentration", "import_partner_cells", "import_exposure", "investment_momentum", "critical_minerals_trade_flows",
            "critical_minerals_production", "reserves", "lcoe_competitiveness", "coalition")
raw <- lapply(stats::setNames(tables, tables), function(t) {
  f <- file.path(processed, paste0(t, "_tbl.rds"))
  if (!file.exists(f)) stop("missing ", f, " (pass --processed-dir)")
  readRDS(f)
})
cur_path <- opt("current-dyads", Sys.getenv("OPSI_CURRENT_DYADS", file.path(processed, "current_trade_dyads_tbl.rds")))
master_path <- opt("hs6-master", Sys.getenv("OPSI_HS6_MASTER", file.path(dirname(processed), "raw", "energy_hs6_master.csv")))
current <- NULL
if (file.exists(cur_path) && file.exists(master_path)) {
  current <- cod_current_cells(readRDS(cur_path), utils::read.csv(master_path, stringsAsFactors = FALSE))
  w <- current$windows[current$windows$flow == "import", ]
  message("latest-12-month trade: ", length(unique(current$cells$reporter)), " reporters; import windows end ",
          paste(names(table(w$window_end)), table(w$window_end), sep = " x", collapse = ", "))
} else {
  message("no latest-12-month trade (", cur_path, " / ", master_path, "): trade charts use 2024 annual data")
}
d <- cod_prepare(raw, current)
message("prepared inputs from ", processed, ": ", nrow(d$trade), " trade cells, ", nrow(d$partners), " partner flows")

# non-trade sources
raw_dir <- opt("raw-dir", Sys.getenv("OPSI_RAW_DIR", file.path(dirname(processed), "raw")))
if (!file.exists(master_path)) master_path <- file.path(raw_dir, "energy_hs6_master.csv")
more <- list()
f_ei <- file.path(raw_dir, "ei_stat_review_world_energy.csv")
if (file.exists(f_ei)) more$ei <- as.data.frame(data.table::fread(f_ei, select = c("Country", "ISO3166_alpha3", "Year", "Var", "Value")))
f_gc <- file.path(raw_dir, "GCIM_Investment_Capacity_aggregated.xlsx")
if (file.exists(f_gc)) {
  more$gcim_invest <- suppressMessages(readxl::read_excel(f_gc, sheet = "annual_actual_investment", skip = 2))
  more$gcim_power <- suppressMessages(readxl::read_excel(f_gc, sheet = "electric_power_capacity", skip = 2))
}
f_at <- file.path(raw_dir, "hs92_country_product_year_6.csv")
if (file.exists(f_at) && file.exists(master_path)) {
  cache_dir <- file.path(repo_root, "outputs", "chart_of_day", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- paste(file.mtime(f_at), file.size(f_at), file.mtime(master_path), sep = "|")
  cache <- file.path(cache_dir, "atlas_cells.rds")
  hit <- file.exists(cache) && identical(attr(readRDS(cache), "stamp"), stamp)
  if (hit) more$atlas <- readRDS(cache) else {
    message("aggregating the Atlas panel (first run or changed file; a few minutes) ...")
    master <- utils::read.csv(master_path, stringsAsFactors = FALSE)
    at <- data.table::fread(f_at, select = c("country_iso3_code", "product_hs92_code", "year", "export_value"))
    at <- at[at$product_hs92_code %in% unique(as.integer(master$hs6)) & at$year >= 2000, ]
    more$atlas <- cod_atlas_cells(as.data.frame(at), master)
    attr(more$atlas, "stamp") <- stamp
    saveRDS(more$atlas, cache)
    rm(at); invisible(gc())
  }
  message("Atlas: ", attr(more$atlas, "codes_matched"), " of ", attr(more$atlas, "codes_basket"), " basket HS6 codes found in HS 1992")
}
for (t in c("nipo_stage_measures", "nipo_stage_links", "cm_fdi_projects")) {
  f <- file.path(processed, paste0(t, "_tbl.rds"))
  if (file.exists(f)) more[[sub("^nipo_stage_", "nipo_", sub("^cm_fdi_projects$", "fdi", t))]] <- readRDS(f)
}
d <- cod_prepare_more(d, more)
message("non-trade sources: ", paste(intersect(c("ei", "gcim_invest", "atlas", "nipo", "fdi"), names(d)), collapse = ", "))

if (flag("mix")) {
  cand <- cod_candidates(d, cfg)
  w <- cod_weights(cand, d, cfg)
  by <- sort(tapply(w, cand$iso3, sum) / sum(w), decreasing = TRUE)
  brief <- c("USA", "JPN", "KOR", "DEU", "AUS", "IND", "GBR", "FRA", "CAN")
  named <- setdiff(names(unlist(cfg$countries$named)), brief)
  grp <- ifelse(names(by) %in% brief, "briefs", ifelse(names(by) %in% named, "memo partners", "others"))
  message("expected focus mix: ", paste(sprintf("%s %.0f%%", c("briefs", "memo partners", "others"),
                                                 100 * tapply(by, factor(grp, c("briefs", "memo partners", "others")), sum)), collapse = ", "))
  message("top 25: ", paste(sprintf("%s %.1f%%", names(by)[1:25], 100 * by[1:25]), collapse = ", "))
  tv <- tapply(w, cand$template, sum) / sum(w); cv <- sum(w[cand$current]) / sum(w)
  message("by template: ", paste(sprintf("%s %.0f%%", names(tv), 100 * tv), collapse = ", "), "; latest data ", sprintf("%.0f%%", 100 * cv))
  quit(save = "no")
}

index_path <- file.path(out_dir, "index.json")
index <- if (file.exists(index_path)) jsonlite::read_json(index_path, simplifyVector = TRUE)$days else NULL
dates <- as.character(seq(start, by = "day", length.out = days))
if (!is.null(index) && length(index) && nrow(index)) {
  if (flag("force")) index <- index[!index$date %in% dates, , drop = FALSE] else dates <- setdiff(dates, index$date)
}
history <- if (!is.null(index) && length(index) && nrow(index))
  data.frame(date = index$date, key = index$key, template = index$template, iso3 = index$iso3, tech = index$tech, stringsAsFactors = FALSE)
if (!length(dates)) { message("nothing to do: every date is already in ", index_path, " (use --force)"); quit(save = "no") }
history <- if (!is.null(history)) history[history$date < min(dates), , drop = FALSE]

specs <- cod_run(d, cfg, dates, history)

write_spec <- function(s) jsonlite::write_json(s, file.path(out_dir, paste0(s$date, ".json")), auto_unbox = TRUE, digits = I(6),
                                               na = "null", null = "null", pretty = FALSE)

# Datawrapper drafts
if (flag("datawrapper")) {
  if (!nzchar(Sys.getenv("DATAWRAPPER_KEY")) && nzchar(Sys.getenv("OPSI_RENVIRON"))) readRenviron(Sys.getenv("OPSI_RENVIRON"))
  key <- Sys.getenv("DATAWRAPPER_KEY")
  if (!nzchar(key)) stop("Set DATAWRAPPER_KEY (or OPSI_RENVIRON to the .Renviron that holds it).")
  dwc <- cfg$datawrapper
  api <- function(path, method = "GET", json = NULL, file = NULL) {
    r <- httr2::request(paste0("https://api.datawrapper.de/v3", path)) |> httr2::req_auth_bearer_token(key) |>
      httr2::req_method(method) |> httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_retry(max_tries = 3, is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503))
    if (!is.null(json)) r <- httr2::req_body_json(r, json, auto_unbox = TRUE, null = "null", na = "null")
    if (!is.null(file)) r <- httr2::req_body_file(r, file, type = "text/csv; charset=utf-8")
    resp <- httr2::req_perform(r)
    st <- httr2::resp_status(resp)
    if (st >= 300) stop(method, " ", path, " returned ", st, ": ", substr(tryCatch(httr2::resp_body_string(resp), error = function(e) ""), 1, 300))
    tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  }
  folder_file <- file.path(out_dir, "datawrapper_folder.txt")
  folder_id <- dwc$folder_id
  if (is.null(folder_id) && file.exists(folder_file)) folder_id <- trimws(readLines(folder_file, warn = FALSE)[1])
  if (is.null(folder_id) || !nzchar(folder_id)) {
    fo <- api("/folders", "POST", list(name = dwc$folder_name, teamId = dwc$team_id))
    folder_id <- as.character(fo$id)
    writeLines(folder_id, folder_file)
    message("created Datawrapper folder '", dwc$folder_name, "' (", folder_id, "); set datawrapper.folder_id in the config")
  }
  # one draft per date, reused on reruns: datawrapper_charts.csv (date, id) survives regenerating the day files
  reg_path <- file.path(out_dir, "datawrapper_charts.csv")
  reg <- if (file.exists(reg_path)) utils::read.csv(reg_path, colClasses = "character") else data.frame(date = character(), id = character())
  for (dt in names(specs)) {
    s <- specs[[dt]]
    dw <- cod_dw_spec(s, dwc$byline)
    id <- reg$id[match(dt, reg$date)]
    if (is.na(id) || !nzchar(id)) {
      id <- api("/charts", "POST", list(title = dw$title, type = dw$type, theme = dwc$theme, folderId = folder_id))$id
      reg <- rbind(reg, data.frame(date = dt, id = id))
      utils::write.csv(reg[order(reg$date), ], reg_path, row.names = FALSE)
    }
    f <- tempfile(fileext = ".csv")
    utils::write.csv(dw$data, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    api(paste0("/charts/", id, "/data"), "PUT", file = f)
    api(paste0("/charts/", id), "PATCH", list(title = dw$title, type = dw$type, theme = dwc$theme, folderId = folder_id, metadata = dw$metadata))
    specs[[dt]]$datawrapper <- list(id = id, edit_url = sprintf("https://app.datawrapper.de/chart/%s/visualize", id), status = "draft")
    message(dt, " -> Datawrapper ", id)
  }
}

for (s in specs) write_spec(s)
new_rows <- do.call(rbind, lapply(specs, function(s) data.frame(
  date = s$date, title = s$title, template = s$template, period = if (is.null(s$period)) "" else s$period, chart = s$chart$type, iso3 = s$focus$iso3, country = s$focus$name,
  tech = s$tech, stage = s$stage, key = s$key, file = paste0(s$date, ".json"), stringsAsFactors = FALSE)))
all_rows <- if (!is.null(index) && length(index) && nrow(index)) rbind(index[, names(new_rows)], new_rows) else new_rows
all_rows <- all_rows[order(all_rows$date, decreasing = TRUE), ]
rownames(all_rows) <- NULL
jsonlite::write_json(list(schema = "osi-chart-of-day-index", version = 1L, days = all_rows), index_path, auto_unbox = TRUE, pretty = TRUE)
for (s in specs) message(sprintf("%s  %-17s %-4s %s", s$date, s$template, s$focus$iso3, s$title))

if (flag("png")) {
  # OPSI_COD_PNG_DIR (e.g. a shared team folder) wins over output.png_dir in the config
  png_dir <- Sys.getenv("OPSI_COD_PNG_DIR", "")
  if (!nzchar(png_dir)) png_dir <- abs_path(if (is.null(cfg$output$png_dir)) "outputs/chart_of_day/png" else cfg$output$png_dir)
  node <- Sys.which("node")
  if (!nzchar(node)) stop("--png needs Node.js 24+ on the PATH (dashboard/scripts/chart-of-day-png.mjs)")
  st <- system2(node, c(shQuote(file.path(repo_root, "dashboard", "scripts", "chart-of-day-png.mjs")), "--dates",
                        paste(names(specs), collapse = ","), "--out", shQuote(png_dir)))
  if (!identical(st, 0L)) stop("PNG export failed (exit ", st, ")")
}
message("wrote ", length(specs), " day(s) to ", out_dir)
