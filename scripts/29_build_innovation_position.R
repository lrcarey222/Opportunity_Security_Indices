# Innovation position prototype (2026-11-17): patents and public RD&D by technology and country.
#
# Inputs (each part is skipped with a message when its input is missing)
#   data/raw/uspto_y02_patents.csv     scripts/fetchers/uspto_patents.R
#   data/raw/iea_rdd/RDDPUBLIC.zip     IEA Energy Technology RD&D Budgets (manual download, sign-in)
# Outputs, in <outputs_dir>/innovation/ (data/processed/outputs/innovation by default), where
# scripts/26 reads innovation_theme.csv for the Competitive Position innovation family
#   patent_position.csv   country x technology: US grants, share of world, RTA, growth, scores
#   rdd_position.csv      country x technology (and critical-minerals stage): public RD&D, share,
#                         intensity, specialisation, growth, scores
#   innovation_theme.csv  both as a standard theme table (category Innovation)
#
# Windows follow CFR's Global Energy Innovation Index: a recent level plus the change over five
# years, weighted 80/20. Patents: the last three complete grant years vs the same years five earlier.
# RD&D: 2022-2024 vs 2017-2019 (2025 is provisional and only partly reported).
# Not yet wired into config/weights.yml: this is a prototype for review.
#
#   Rscript scripts/29_build_innovation_position.R

local({
  script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
  root <- if (is.na(script)) normalizePath(".", winslash = "/") else normalizePath(file.path(dirname(script), ".."), winslash = "/")
  assign("repo_root", root, envir = globalenv())
})

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "uspto_patents.R"))
source(file.path(repo_root, "R", "utils", "iea_rdd.R"))

config_path <- file.path(repo_root, "config", "config.yml")
config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
raw_data_path <- file.path(repo_root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
# Same convention as scripts/20 and scripts/26: config$outputs_dir, else <processed_dir>/outputs.
processed_dir <- file.path(repo_root, if (is.null(config$processed_dir)) "data/processed" else config$processed_dir)
outputs_dir <- file.path(
  if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(processed_dir, "outputs"),
  "innovation"
)
dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

theme_parts <- list()

## Patents --------------------------------------------------------------------------------------
counts_path <- file.path(raw_data_path, "uspto_y02_patents.csv")
if (file.exists(counts_path)) {
  counts <- read.csv(counts_path, stringsAsFactors = FALSE, na.strings = "")
  last_year <- max(counts$grant_year)
  recent <- (last_year - 2):last_year
  earlier <- recent - 5
  window_label <- sprintf("%d-%d vs %d-%d", min(recent), max(recent), min(earlier), max(earlier))

  indicators <- uspto_patent_indicators(counts, recent, earlier, min_patents = 5)
  universe <- uspto_patent_universe(indicators, min_total = 100)
  indicators <- indicators[indicators$inventor_iso2 %in% universe, , drop = FALSE]
  pat <- uspto_patent_scores(indicators)
  pat$iso3 <- countrycode::countrycode(pat$inventor_iso2, "iso2c", "iso3c", warn = FALSE)
  pat$Country <- country_name_from_iso3(pat$iso3)
  pat <- pat[order(pat$tech, -pat$innovation_index), , drop = FALSE]
  write.csv(pat, file.path(outputs_dir, "patent_position.csv"), row.names = FALSE)
  theme_parts$patents <- uspto_patent_theme_table(pat, last_year, window_label)
  message(sprintf("Patents: %d countries x %d technologies (%s)", length(universe), length(unique(pat$tech)), window_label))
} else {
  message("Patents skipped: ", counts_path, " is missing (run scripts/fetchers/uspto_patents.R).")
}

## Public RD&D ----------------------------------------------------------------------------------
rdd_path <- file.path(raw_data_path, "iea_rdd/RDDPUBLIC.zip")
if (file.exists(rdd_path)) {
  inner <- utils::unzip(rdd_path, list = TRUE)$Name
  inner <- inner[grepl("\\.csv$", inner, ignore.case = TRUE)][1]
  raw <- readr::read_csv(
    unz(rdd_path, inner),
    col_select = c("COUNTRY", "RDD_SECTOR", "RDD_TYPE", "RDD_TECH", "UNIT", "TIME_PERIOD", "OBS_VALUE"),
    col_types = readr::cols(.default = readr::col_character()), progress = FALSE
  )
  values <- iea_rdd_public_values(as.data.frame(raw))
  rm(raw)
  values <- iea_rdd_allocate(values[values$year >= 2015L, , drop = FALSE])

  map <- yaml::read_yaml(file.path(repo_root, "config", "iea_rdd_tech_map.yml"))
  tech_codes <- lapply(map$techs, function(t) unlist(t$codes))
  cm_codes <- lapply(map$critical_minerals, unlist)
  names(cm_codes) <- paste0("Critical Minerals|", names(cm_codes))

  recent <- 2022:2024
  earlier <- recent - 5
  window_label <- sprintf("%d-%d vs %d-%d", min(recent), max(recent), min(earlier), max(earlier))
  tv <- iea_rdd_tech_values(values, c(tech_codes, cm_codes))
  rdd <- iea_rdd_scores(iea_rdd_indicators(tv, recent, earlier))
  is_cm <- startsWith(rdd$tech, "Critical Minerals|")
  rdd$supply_chain <- ifelse(is_cm, sub("^Critical Minerals\\|", "", rdd$tech), "All")
  rdd$tech[is_cm] <- "Critical Minerals"
  rdd$Country <- country_name_from_iso3(rdd$iso3)
  rdd <- rdd[order(rdd$tech, rdd$supply_chain, -rdd$rdd_index), , drop = FALSE]
  write.csv(rdd, file.path(outputs_dir, "rdd_position.csv"), row.names = FALSE)
  theme_parts$rdd <- iea_rdd_theme_table(rdd, max(recent), window_label)
  message(sprintf("Public RD&D: %d countries x %d technologies (%s); no US after 2015, no China or India",
                  length(unique(rdd$iso3)), length(unique(paste(rdd$tech, rdd$supply_chain))), window_label))
} else {
  message("Public RD&D skipped: ", rdd_path, " is missing (IEA Energy Technology RD&D Budgets, sign-in download).")
}

if (length(theme_parts)) {
  theme <- do.call(rbind, unname(theme_parts))
  validate_schema(theme)
  write.csv(theme, file.path(outputs_dir, "innovation_theme.csv"), row.names = FALSE)
  message("Wrote ", nrow(theme), " theme rows to ", outputs_dir)
}
