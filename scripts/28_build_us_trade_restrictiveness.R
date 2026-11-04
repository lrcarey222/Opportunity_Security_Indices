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


# US trade restrictiveness by technology x supply-chain stage, compared with the recommended posture
# (docs/us_trade_restrictiveness.md; policy alignment, Step 4 of the decision chain: nothing here feeds the
# posture engine). The Trade War Tracker's Trade Restrictiveness Index (Waugh) on the project's HS6 basket:
# HS10 x partner goods, weight-year import weights, the data month's calculated duty, plus AD/CVD deposits
# from data/reference/adcvd_orders.csv, by supplier group (the posture engine's coercive / trusted rule).
# Reads the tracker's files (data/raw/trade_war_tracker/, downloaded by scripts/fetchers/trade_war_tracker.R
# when missing; OPSI_TWT_TRI_DIR / OPSI_TWT_REDUX_DIR point elsewhere), the basket, geo_alignment_tbl.rds
# (scripts/12) and strategic_position_tbl.csv (scripts/26). Optional: the statutory rates of the tracker's
# engine (scripts/utils/tracker_statutory_rates.py). Writes us_trade_restrictiveness_cells.{rds,csv} to the
# outputs directory and diagnostics/us_trade_restrictiveness/.
# OPSI_TWT_REFRESH=true downloads the tracker files again (a new Census month); OPSI_SKIP_FETCH=true never
# downloads.

library(dplyr)

for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/supply_structure.R",
            "R/strategic_position/trade_restrictiveness.R", "scripts/utils/fetchers.R",
            "scripts/fetchers/trade_war_tracker.R")) source(file.path(repo_root, f))
if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' is required to read the tracker's parquet files.")

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
cfg <- yaml::read_yaml(Sys.getenv("OPSI_TRADE_RESTRICTIVENESS", file.path(repo_root, "config", "us_trade_restrictiveness.yml")))
processed_dir <- file.path(repo_root, config$processed_dir)
raw_dir <- file.path(repo_root, config$raw_data_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(processed_dir, "outputs")
}
diagnostics_dir <- file.path(repo_root, "diagnostics", "us_trade_restrictiveness")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

tri_dir <- Sys.getenv("OPSI_TWT_TRI_DIR", file.path(raw_dir, "trade_war_tracker", "how-restrictive-us-trade"))
redux_dir <- Sys.getenv("OPSI_TWT_REDUX_DIR", file.path(raw_dir, "trade_war_tracker", "trade-war-redux-2025"))
refresh <- tolower(Sys.getenv("OPSI_TWT_REFRESH", "false")) %in% c("1", "true", "yes")
if (opsi_fetch_enabled()) {
  message("Checking Trade War Tracker files (downloading what is missing", if (refresh) ", refreshing all" else "", ")...")
  fetch_trade_war_tracker(tri_dir, redux_dir, cfg, refresh = refresh)
}
need_file <- function(path) {
  if (!file.exists(path)) stop(path, " not found; let scripts/fetchers/trade_war_tracker.R download it (unset OPSI_SKIP_FETCH).")
  path
}
read_ref <- function(path) utils::read.csv(file.path(repo_root, path), colClasses = "character", check.names = FALSE, na.strings = character())

reporter <- cfg$reporter
weight_year <- cfg$weight_year
techs <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen",
           "Nuclear", "Oil", "Solar", "Wind")
caps <- as.numeric(unlist(cfg$caps))
open <- cfg$thresholds$open
high <- cfg$thresholds$high

# --- Census HS10 panel ---------------------------------------------------------------------------------
entity_file <- function(code) need_file(file.path(tri_dir, "data", "imports-hs10", paste0(code, "data-current.parquet")))
codes <- utils::read.csv(need_file(file.path(tri_dir, "data", "country-list.csv")), header = FALSE, colClasses = "character")[[1]]
data_month <- cfg$data_month
if (identical(data_month, "latest")) {
  data_month <- max(arrow::read_parquet(entity_file("TOTAL"), col_select = "time")$time)
}
now_months <- tri_month_window(data_month, cfg$volume_months)
base_months <- tri_same_months(now_months, weight_year)
message("Data month ", data_month, "; weights ", weight_year, "; volume ", paste(now_months, collapse = ","), " vs ", paste(base_months, collapse = ","), ".")

read_entity <- function(code) {
  t <- arrow::read_parquet(entity_file(code), col_select = c("CON_VAL_MO", "CAL_DUT_MO", "I_COMMODITY", "time"))
  t <- t[substr(t$time, 1, 4) == as.character(weight_year) | t$time %in% now_months, , drop = FALSE]
  data.frame(cty_code = code, hs10 = t$I_COMMODITY, time = t$time,
             value = suppressWarnings(as.numeric(t$CON_VAL_MO)), duty = suppressWarnings(as.numeric(t$CAL_DUT_MO)))
}
rows <- do.call(rbind, lapply(c(codes, "TOTAL"), read_entity))
if (!any(rows$time == data_month)) stop("No Census data for ", data_month, " in the tracker files.")
panel <- tri_build_panel(rows, weight_year, unique(c(base_months, now_months)))
rm(rows)

partner_codes <- read_ref(cfg$partner_codes)
iso_of <- c(stats::setNames(partner_codes$iso3, partner_codes$census_code), ROW = TRI_ROW_CODE)
geo <- readRDS(need_file(file.path(processed_dir, "geo_alignment_tbl.rds")))
pairs <- sp_coercive_pairs(geo)
tag_partners <- function(df) {
  df$iso3 <- unname(iso_of[df$cty_code])
  if (any(is.na(df$iso3))) stop("Census partner code(s) without ISO3 in ", cfg$partner_codes, ": ", paste(unique(df$cty_code[is.na(df$iso3)]), collapse = ", "))
  df$group <- tri_partner_groups(df$iso3, pairs, reporter)
  df$hs6 <- substr(df$hs10, 1, 6)
  df
}
panel <- tag_partners(panel)

# --- line tariffs ----------------------------------------------------------------------------------------
goods <- tri_line_rates(panel, data_month)
redux_partners <- utils::read.csv(need_file(file.path(redux_dir, "data", "top_50_non_eu.csv")), colClasses = "character")
carve <- do.call(rbind, lapply(setdiff(redux_partners$CTY_CODE, codes), function(code) {
  f <- need_file(file.path(redux_dir, "data", paste0(weight_year, "-hs10-imports", code, ".parquet")))
  t <- arrow::read_parquet(f, col_select = c("HS10", "imports", "duty"))
  data.frame(cty_code = code, hs10 = t$HS10, imports = as.numeric(t$imports), duty = as.numeric(t$duty))
}))
carve <- stats::aggregate(cbind(imports, duty) ~ cty_code + hs10, data = carve, FUN = sum)
goods <- tag_partners(tri_split_rest_of_world(goods, carve))

read_list <- function(f) utils::read.csv(need_file(file.path(redux_dir, "tariff-lists", f)), colClasses = "character")$HTSUS
s232_lists <- lapply(cfg$tracker$section232_lists, read_list)
goods$s232 <- tri_section232_tags(goods$hs10, s232_lists)

orders <- read_ref(cfg$adcvd_orders)
ad <- tri_adcvd_rates(goods$hs10, goods$iso3, orders)
goods$adcvd <- ad$adcvd
goods$adcvd_family <- ad$adcvd_family
goods$covered <- goods$adcvd > 0
goods$matched <- goods$match != "absent"
goods$matched_adcvd <- goods$matched | goods$covered
for (cap in caps) goods[[paste0("tau_adcvd_", cap)]] <- tri_adcvd_tau(goods$tau_collected, goods$tau_base, goods$adcvd, cap)

# --- cells -----------------------------------------------------------------------------------------------
cells <- tri_basket_cells(read_ref("data/reference/energy_hs6_master.csv"), techs)
m_duty <- paste0("collected_", data_month)
m_head <- paste0(m_duty, "_adcvd")
m_base <- paste0("collected_", weight_year)
long <- rbind(
  tri_by_cell(goods, cells, "tau_collected", m_duty, keep = "matched"),
  tri_by_cell(goods, cells, "tau_universe", paste0(m_duty, "_universe")),
  tri_by_cell(goods, cells, "tau_base", m_base),
  do.call(rbind, lapply(seq_along(caps), function(i) {
    tri_by_cell(goods, cells, paste0("tau_adcvd_", caps[i]), if (i == 1) m_head else paste0(m_head, "_cap", caps[i]),
                keep = "matched_adcvd", cap = caps[i])
  }))
)

statutory_path <- Sys.getenv("OPSI_TWT_STATUTORY", if (is.null(cfg$statutory_rates)) "" else cfg$statutory_rates)
if (nzchar(statutory_path) && !grepl("^([A-Za-z]:)?[/\\\\]", statutory_path)) statutory_path <- file.path(repo_root, statutory_path)
stat_dates <- character()
if (nzchar(statutory_path) && file.exists(statutory_path)) {
  st <- utils::read.csv(statutory_path, colClasses = c(date = "character", cty_code = "character", hs10 = "character"))
  st <- tag_partners(st[st$imports > 0, , drop = FALSE])
  st$w <- st$imports
  st$tau_statutory <- 100 * st$duty / st$imports + st$tariff
  st$s232 <- tri_section232_tags(st$hs10, s232_lists)
  st$covered <- FALSE
  stat_dates <- sort(unique(st$date))
  long <- rbind(long, do.call(rbind, lapply(stat_dates, function(d) {
    tri_by_cell(st[st$date == d, , drop = FALSE], cells, "tau_statutory", paste0("statutory_", d))
  })))
  message("Statutory cross-check for ", paste(stat_dates, collapse = ", "), ".")
} else if (nzchar(statutory_path)) {
  message("No statutory rates at ", statutory_path, "; the cross-check is skipped (scripts/utils/tracker_statutory_rates.py writes them).")
}

vol <- tri_volume_ratios(panel, cells, now_months, base_months)
long <- merge(long, vol[c("tech", "stage", "supplier_group", "v_now", "v_base", "volume_ratio")],
              by = c("tech", "stage", "supplier_group"), all.x = TRUE)
long <- merge(long, tri_match_shares(goods, cells), by = c("tech", "stage"), all.x = TRUE)
long$data_month <- data_month
long <- long[order(long$measure, long$tech, match(long$stage, SUPPLY_CHAIN_STAGES), match(long$supplier_group, TRI_GROUPS)), ]
saveRDS(long, file.path(outputs_dir, "us_trade_restrictiveness_cells.rds"))
utils::write.csv(long, file.path(outputs_dir, "us_trade_restrictiveness_cells.csv"), row.names = FALSE, na = "")

# --- posture comparison ------------------------------------------------------------------------------------
pos_cols <- c("iso3", "tech", "supply_chain", "strategic_posture", "strategic_choice", "fence_level", "yard")
posture <- readr::read_csv(need_file(file.path(outputs_dir, "strategic_position_tbl.csv")), col_select = dplyr::all_of(pos_cols),
                           col_types = readr::cols(.default = "c"), progress = FALSE)
posture <- as.data.frame(posture[posture$iso3 == reporter, setdiff(pos_cols, "iso3")])
names(posture)[names(posture) == "supply_chain"] <- "stage"

pick <- function(measure, grp, col = "tri") {
  d <- long[long$measure == measure & long$supplier_group == grp, c("tech", "stage", col)]
  stats::setNames(d[[col]], paste(d$tech, d$stage, sep = "|"))
}
keys <- unique(paste(cells$tech, cells$stage, sep = "|"))
cmp <- data.frame(tech = sub("\\|.*", "", keys), stage = sub(".*\\|", "", keys))
cmp <- merge(cmp, posture, by = c("tech", "stage"), all.x = TRUE)
k <- paste(cmp$tech, cmp$stage, sep = "|")
add <- function(name, measure, grp, col = "tri") cmp[[name]] <<- unname(pick(measure, grp, col)[k])
add("imports_usd", m_head, "All suppliers", "w_total")
add("tri_all", m_head, "All suppliers"); add("tri_trusted", m_head, "Trusted"); add("tri_coercive", m_head, "Coercive")
add("tri_china", m_head, "China"); add("tri_nonaligned", m_head, "Nonaligned"); add("mean_all", m_head, "All suppliers", "mean")
add("tri_all_duty_only", m_duty, "All suppliers"); add("tri_trusted_duty_only", m_duty, "Trusted")
add("tri_coercive_duty_only", m_duty, "Coercive"); add("tri_china_duty_only", m_duty, "China")
add("tri_all_weight_year", m_base, "All suppliers"); add("tri_trusted_weight_year", m_base, "Trusted")
add("tri_all_universe", paste0(m_duty, "_universe"), "All suppliers")
for (cap in caps[-1]) add(paste0("tri_all_cap", cap), paste0(m_head, "_cap", cap), "All suppliers")
add("adcvd_weight_share", m_head, "All suppliers", "adcvd_weight_share")
add("adcvd_weight_share_trusted", m_head, "Trusted", "adcvd_weight_share")
add("at_cap_weight_share", m_head, "All suppliers", "at_cap_weight_share")
add("s232_tri_sq_share_trusted", m_duty, "Trusted", "s232_tri_sq_share")
add("tri_trusted_excl_232", m_duty, "Trusted", "tri_excl_232")
add("weight_coverage", m_duty, "All suppliers", "weight_coverage")
add("volume_ratio_all", m_duty, "All suppliers", "volume_ratio")
add("volume_ratio_trusted", m_duty, "Trusted", "volume_ratio")
add("volume_ratio_coercive", m_duty, "Coercive", "volume_ratio")
for (d in stat_dates) add(paste0("tri_statutory_", d), paste0("statutory_", d), "All suppliers")
cmp$coercive_share_2024 <- unname(pick(m_head, "Coercive", "w_total")[k]) / cmp$imports_usd
cmp$thin <- cmp$imports_usd < cfg$thin_imports_usd
v <- tri_posture_verdict(cmp$strategic_choice, cmp$tri_all, cmp$tri_trusted, cmp$tri_coercive, open, high)
cmp$verdict <- v$verdict
cmp$verdict_reason <- v$verdict_reason
cmp$verdict_duty_only <- tri_posture_verdict(cmp$strategic_choice, cmp$tri_all_duty_only, cmp$tri_trusted_duty_only,
                                             cmp$tri_coercive_duty_only, open, high)$verdict
cmp <- cmp[order(match(cmp$tech, techs), match(cmp$stage, SUPPLY_CHAIN_STAGES)), ]

sens <- do.call(rbind, lapply(c(open, unlist(cfg$thresholds$open_sensitivity)), function(o) {
  vv <- tri_posture_verdict(cmp$strategic_choice, cmp$tri_all, cmp$tri_trusted, cmp$tri_coercive, o, max(high, o))$verdict
  as.data.frame(table(choice = cmp$strategic_choice, verdict = vv), stringsAsFactors = FALSE) |>
    dplyr::filter(.data$Freq > 0) |>
    dplyr::mutate(open_threshold = o)
}))

bench <- rbind(
  tri_benchmarks(goods, "tau_collected", m_duty, keep = "matched"),
  tri_benchmarks(goods, "tau_universe", paste0(m_duty, "_universe")),
  tri_benchmarks(goods, "tau_base", m_base)
)

s232_hs6 <- with(stats::aggregate(w ~ hs6 + s232, data = goods, FUN = sum), {
  o <- order(hs6, -w)
  d <- data.frame(hs6 = hs6[o], s232 = s232[o])
  d <- d[!duplicated(d$hs6), ]
  stats::setNames(d$s232, d$hs6)
})
drivers <- tri_cell_drivers(panel, cells[cells$stage != "Deployment", ], data_month, s232_hs6)

# Reproduction of the tracker's published country TRI for the data month (its default: lines with no
# trade count as tariff-free), from the same goods before the rest-of-world split.
repro_path <- file.path(tri_dir, "data", "metrics", "country.parquet")
if (file.exists(repro_path)) {
  pub <- as.data.frame(arrow::read_parquet(repro_path))
  pub <- pub[format(as.Date(pub$date), "%Y-%m") == data_month & pub$CTY_CODE %in% codes, c("CTY_CODE", "CTY_NAME", "sqrtariff")]
  ours <- vapply(pub$CTY_CODE, function(code) {
    g <- goods[goods$cty_code == code, , drop = FALSE]
    unname(tri_index(g$w, g$tau_universe)["tri"])
  }, numeric(1))
  repro <- data.frame(cty_code = pub$CTY_CODE, country = pub$CTY_NAME, tracker_tri = 100 * pub$sqrtariff, this_build_tri = ours)
  repro$difference <- repro$this_build_tri - repro$tracker_tri
  utils::write.csv(repro, file.path(diagnostics_dir, "tracker_reproduction.csv"), row.names = FALSE)
  message("Tracker reproduction: ", nrow(repro), " partners, largest difference ", signif(max(abs(repro$difference)), 3), " points.")
  if (nrow(repro) > 0 && max(abs(repro$difference)) > 1e-6) warning("The build does not reproduce the tracker's published TRI; see tracker_reproduction.csv.")
}

fetch_info <- function(dir) {
  f <- file.path(dir, "fetch_info.csv")
  if (file.exists(f)) utils::read.csv(f, colClasses = "character")$commit[1] else NA_character_
}
run_info <- data.frame(
  item = c("data_month", "weight_year", "volume_months", "volume_base_months", "reporter", "adcvd_orders_in_force",
           "caps", "thresholds_open_high", "statutory_dates", "tri_repo_commit", "redux_repo_commit"),
  value = c(data_month, weight_year, paste(now_months, collapse = " "), paste(base_months, collapse = " "), reporter,
            sum(toupper(orders$in_force) == "TRUE"), paste(caps, collapse = " "), paste(open, high), paste(stat_dates, collapse = " "),
            fetch_info(tri_dir), fetch_info(redux_dir))
)

wcsv <- function(x, f) utils::write.csv(x, file.path(diagnostics_dir, f), row.names = FALSE, na = "")
wcsv(cmp, "tri_vs_posture.csv")
wcsv(sens, "verdict_threshold_sensitivity.csv")
wcsv(bench, "tri_benchmarks.csv")
wcsv(drivers, "tri_cell_drivers.csv")
wcsv(run_info, "run_info.csv")

tab <- table(cmp$verdict[!cmp$verdict %in% "Not assessed"])
message("Verdicts (", sum(tab), " assessed cells with traded goods): ", paste(names(tab), tab, sep = " ", collapse = "; "), ".")
message("Wrote ", file.path(outputs_dir, "us_trade_restrictiveness_cells.rds"), " and ", diagnostics_dir, ".")
