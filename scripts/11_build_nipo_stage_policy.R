# Build the NIPO direct six-stage policy layer (docs/strategic_position.md, section 4 "NIPO";
# R/categories/policy/nipo_stage_policy.R).
#
# Reads the newest GTA NIPO extract, the six-stage HS6 master (data/raw/energy_hs6_master.csv),
# the policy_priority section of config/strategic_position.yml and the instrument posture table
# it names (policy_priority$nipo$posture_table, config/nipo_instrument_posture.csv). Writes data/processed/nipo_stage_measures_tbl.rds,
# nipo_stage_links_tbl.rds, nipo_stage_cells_tbl.rds and nipo_stage_country_tbl.rds and prints a
# coverage summary. Independent of scripts/10 (the legacy three-stage DIS): it neither reads nor
# changes the nipo_policy_* tables. Filtering to basket codes before anything is widened keeps
# the run well under 1 GB, unlike the legacy DIS.

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

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
# For its pure parsing helpers (clean_code, as_date_safe, as_bool, check_required_columns).
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_stage_policy.R"))

config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
config <- yaml::read_yaml(config_path)
raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) stop("Processed data directory not found: ", processed_dir)

sp_config <- yaml::read_yaml(file.path(repo_root, "config", "strategic_position.yml"))
policy_cfg <- sp_config$policy_priority

# Same resolution as scripts/10: the newest dated extract.
nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - October 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
if (!file.exists(nipo_policy_path)) stop("GTA NIPO extract not found: ", nipo_policy_path)
message("GTA NIPO extract: ", basename(nipo_policy_path))

# readxl reports "zip file cannot be opened" while another process (Excel, OneDrive) holds the
# workbook; a temporary copy reads fine and leaves data/raw untouched.
read_gta <- function(path) {
  tryCatch(
    readxl::read_excel(path, sheet = 1),
    error = function(e) {
      if (!grepl("zip file", conditionMessage(e), fixed = TRUE)) stop(e)
      copy <- tempfile(fileext = ".xlsx")
      on.exit(unlink(copy), add = TRUE)
      if (!file.copy(path, copy)) stop(e)
      message("Workbook locked; reading a temporary copy.")
      readxl::read_excel(copy, sheet = 1)
    }
  )
}
nipo_raw <- read_gta(nipo_policy_path)

master_path <- file.path(raw_data_path, "energy_hs6_master.csv")
if (!file.exists(master_path)) stop("HS6 master not found: ", master_path)
hs6_master <- utils::read.csv(master_path, colClasses = "character", check.names = FALSE)

# The posture table named in the config (a repo-relative path), else the committed default.
posture_rel <- policy_cfg$nipo$posture_table
if (is.null(posture_rel) || !nzchar(posture_rel)) posture_rel <- file.path("config", "nipo_instrument_posture.csv")
posture_path <- file.path(repo_root, posture_rel)
if (!file.exists(posture_path)) stop("NIPO instrument posture table not found: ", posture_path)
posture <- utils::read.csv(posture_path, stringsAsFactors = FALSE, check.names = FALSE)

# Every rule (narrow test, one-off stock window, assessment equivalents, EU-act threshold,
# repeated-value split) comes from policy_cfg$nipo; nipo_stage_policy_outputs() falls back to its
# defaults for keys the config leaves out.
nipo_stage <- nipo_stage_policy_outputs(
  raw = nipo_raw,
  master = hs6_master,
  posture = posture,
  policy_cfg = policy_cfg
)
if (length(nipo_stage$settings$one_off_families_unknown) > 0) {
  stop(
    "policy_priority$nipo$one_off_families names families that ", basename(posture_path), " does not have: ",
    paste(nipo_stage$settings$one_off_families_unknown, collapse = ", "),
    ". Fix the family names (the posture table's `family` column) before rebuilding."
  )
}
posture_check <- nipo_stage_posture_flag_check(nipo_raw, posture)
n_raw_rows <- nrow(nipo_raw)
rm(nipo_raw)
invisible(gc())

saveRDS(nipo_stage$measures, file.path(processed_dir, "nipo_stage_measures_tbl.rds"))
saveRDS(nipo_stage$links, file.path(processed_dir, "nipo_stage_links_tbl.rds"))
saveRDS(nipo_stage$cells, file.path(processed_dir, "nipo_stage_cells_tbl.rds"))
saveRDS(nipo_stage$country, file.path(processed_dir, "nipo_stage_country_tbl.rds"))

# ---- Coverage summary ---------------------------------------------------------------------
m <- nipo_stage$measures
cells <- nipo_stage$cells
pct <- function(x) sprintf("%.1f%%", 100 * x)
main_w <- nipo_stage$settings$flow_window_years

cat("\nNIPO six-stage layer, as of ", format(nipo_stage$as_of), " (", basename(nipo_policy_path), ")\n", sep = "")
cat(sprintf(
  "  %d extract rows -> %d measures (%d EU acts as EUU), %d implementing jurisdictions\n",
  n_raw_rows, nrow(m), sum(m$is_eu_supranational), dplyr::n_distinct(m$iso3)
))
cat(sprintf(
  "  counted %d (%s); excluded: %s\n", sum(m$counted), pct(mean(m$counted)),
  paste(names(table(m$exclusion_reason)), table(m$exclusion_reason), sep = " ", collapse = ", ")
))
status_tab <- table(factor(m$mapping_status[m$counted], levels = NIPO_STAGE_MAPPING_STATUSES))
cat("  counted by mapping status: ", paste(names(status_tab), status_tab, collapse = ", "), "\n", sep = "")
cat(sprintf(
  "  counted attributed in stock %d, announced in the %d-year window %d\n",
  sum(m$counted & m$mapping_status == "attributed" & m$active_at_as_of),
  main_w,
  sum(m$counted & m$mapping_status == "attributed" &
    nipo_stage_announced_within(m$announcement_date, nipo_stage$as_of, main_w))
))
s <- nipo_stage$settings
ca <- m$counted & m$mapping_status == "attributed"
lit <- ca & nipo_stage_active(m$implementation_date, m$removal_date, nipo_stage$as_of)
cat(sprintf(
  "  one-off stock window (%s; %d years): %d attributed measures on it, %d of them lapsed from stock; stock implemented before 2020: %s\n",
  paste(s$one_off_families, collapse = ", "), s$one_off_stock_years,
  sum(ca & m$stock_rule == "one_off_window"), sum(lit & !m$active_at_as_of),
  pct(mean(m$implementation_date[ca & m$active_at_as_of] < as.Date("2020-01-01")))
))
count_floor <- m$n_basket_codes > 0 & m$n_codes <= s$narrow_max_codes &
  m$basket_share < s$narrow_min_basket_share & m$basket_share < s$narrow_count_min_basket_share
cat(sprintf(
  "  narrow count clause (<= %d codes) needs %s of codes in the basket: %d measures (%d counted, %d of them in stock) left broad by the floor\n",
  s$narrow_max_codes, pct(s$narrow_count_min_basket_share), sum(count_floor), sum(count_floor & m$counted),
  sum(count_floor & m$counted & m$active_at_as_of)
))
cat(sprintf(
  "  cells: %d rows = %d countries x %d observable tech x stage cells; %d cells with stock > 0\n",
  nrow(cells), dplyr::n_distinct(cells$iso3), nrow(dplyr::distinct(cells, tech, supply_chain)),
  sum(cells$all_stock > 0)
))
by_stage <- cells %>%
  dplyr::group_by(.data$supply_chain) %>%
  dplyr::summarise(
    # Counted before the sum below reuses the name all_stock.
    countries_with_stock = dplyr::n_distinct(.data$iso3[.data$all_stock > 0]),
    all_stock = round(sum(.data$all_stock), 1),
    support_stock = round(sum(.data$support_stock), 1),
    protection_stock = round(sum(.data$protection_stock), 1),
    .groups = "drop"
  )
cat("  attributed stock by stage:\n")
print(as.data.frame(by_stage), row.names = FALSE)
disagree <- posture_check %>% dplyr::filter(!is.na(.data$agrees), !.data$agrees)
if (nrow(disagree) > 0) {
  cat("  posture rows that differ from the GTA family flag (reviewed, kept):\n")
  print(as.data.frame(disagree[, c("gta_intervention_type", "n_rows", "gta_flag", "support", "protection")]), row.names = FALSE)
}
unflagged <- posture_check %>% dplyr::filter(is.na(.data$gta_flag))
if (nrow(unflagged) > 0) {
  cat("  types with no GTA family flag: ", paste(unflagged$gta_intervention_type, collapse = "; "), "\n", sep = "")
}
