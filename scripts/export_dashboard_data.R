# =============================================================================
# scripts/export_dashboard_data.R
# =============================================================================
# Regenerates dashboard/js/data.js from live R pipeline index outputs.
#
# Usage:
#   Step 1 — build pipeline indices (once per data refresh):
#     Rscript run_pipeline.R
#     (or just the index step: Rscript scripts/20_build_indices.R after setup)
#
#   Step 2 — export to dashboard:
#     Rscript scripts/export_dashboard_data.R
#
# If data/processed/outputs/index_outputs.rds already exists, Step 1 can be
# skipped between data refreshes.
#
# Outputs:
#   dashboard/js/data.js                        — updated dashboard data layer
#   data/processed/outputs/dashboard_data.json  — intermediate JSON (for debugging)
#
# Notes:
#   - ES scores are automatically inverted: es_display = 1 - ES_risk_index,
#     so higher = more energy-secure (matches dashboard convention).
#   - PSI is approximated as 0.4*eo + 0.4*(es+eo)/2 + 0.2*pol if not present
#     in pipeline outputs; use partnership_index_outputs.rds for full PSI.
#   - Category scores are averaged across all tech × supply_chain combinations
#     to produce country-level summaries shown in the radar / bar charts.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

# ── 0. Resolve repo root ──────────────────────────────────────────────────────
args      <- commandArgs(trailingOnly = FALSE)
file_arg  <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
repo_root <- if (nzchar(script_path)) {
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
while (!file.exists(file.path(repo_root, ".git")) && dirname(repo_root) != repo_root) {
  repo_root <- dirname(repo_root)
}
if (!file.exists(file.path(repo_root, ".git"))) {
  stop("Could not locate repo root (.git). Run from within the repository.")
}
message("Repo root: ", repo_root)

# ── 1. Source pipeline setup ──────────────────────────────────────────────────
message("\n[1/6] Loading pipeline config...")
setup_script <- file.path(repo_root, "scripts", "00_setup.R")
if (!file.exists(setup_script)) stop("scripts/00_setup.R not found at: ", setup_script)
source(setup_script)
cfg <- getOption("opportunity_security.config")
if (is.null(cfg)) stop("Config not set after sourcing 00_setup.R.")

# ── 2. Locate and read index_outputs.rds ─────────────────────────────────────
outputs_dir <- {
  if (!is.null(cfg$outputs_dir) && nzchar(cfg$outputs_dir)) {
    file.path(repo_root, cfg$outputs_dir)
  } else {
    file.path(repo_root, cfg$processed_dir, "outputs")
  }
}
rds_env <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
rds_path <- if (nzchar(rds_env)) {
  rds_env
} else if (!is.null(cfg$outputs_rds) && nzchar(cfg$outputs_rds)) {
  file.path(repo_root, cfg$outputs_rds)
} else {
  file.path(outputs_dir, "index_outputs.rds")
}

if (!file.exists(rds_path)) {
  stop(
    "\nindex_outputs.rds not found at:\n  ", rds_path,
    "\n\nRun the pipeline first:\n",
    "  Rscript run_pipeline.R\n\n",
    "Or run just the index step (after 00_setup.R and 10_build_themes.R):\n",
    "  Rscript scripts/20_build_indices.R\n\n",
    "Override the path with: OPSI_OUTPUTS_RDS=/path/to/index_outputs.rds\n"
  )
}

message("[2/6] Reading: ", rds_path)
idx <- readRDS(rds_path)

# Validate expected tables are present
required <- c(
  "energy_security_index", "economic_opportunity_index", "policy_index",
  "energy_security_category_scores", "economic_opportunity_category_scores"
)
missing_tbl <- setdiff(required, names(idx))
if (length(missing_tbl) > 0) {
  stop("index_outputs.rds is missing expected tables: ", paste(missing_tbl, collapse=", "),
       "\nRe-run run_pipeline.R to regenerate it.")
}

es_idx  <- idx$energy_security_index
eo_idx  <- idx$economic_opportunity_index
pol_idx <- idx$policy_index
es_cats <- idx$energy_security_category_scores
eo_cats <- idx$economic_opportunity_category_scores

message("  ES index:  ", nrow(es_idx),  " rows, ",
        length(unique(es_idx$Country)),  " countries")
message("  EO index:  ", nrow(eo_idx),  " rows, ",
        length(unique(eo_idx$Country)),  " countries")
message("  Pol index: ", nrow(pol_idx), " rows, ",
        length(unique(pol_idx$Country)), " countries")

# ── 3. Compute country × tech × SC scores ────────────────────────────────────
message("\n[3/6] Computing country scores...")

# ES: pipeline stores risk scores; invert for dashboard (higher = more secure)
tech_sc_es <- es_idx %>%
  filter(!is.na(Energy_Security_Index)) %>%
  transmute(
    Country, tech, supply_chain,
    es = round(1 - Energy_Security_Index, 4)
  )

tech_sc_eo <- eo_idx %>%
  filter(!is.na(Economic_Opportunity_Index)) %>%
  transmute(
    Country, tech, supply_chain,
    eo = round(Economic_Opportunity_Index, 4)
  )

tech_sc_pol <- pol_idx %>%
  filter(!is.na(value)) %>%
  transmute(
    Country, tech, supply_chain,
    pol = round(value, 4)
  )

# Merge tech × SC scores (outer join — not all combos have all three indices)
tech_sc_all <- tech_sc_es %>%
  full_join(tech_sc_eo,  by = c("Country", "tech", "supply_chain")) %>%
  full_join(tech_sc_pol, by = c("Country", "tech", "supply_chain"))

# Check if partnership outputs are available for PSI
psi_available <- !is.null(idx$partnership_index) && nrow(idx$partnership_index) > 0
psi_tbl <- if (psi_available) idx$partnership_index else NULL

# Overall country-level scores (mean across all tech × SC combos)
overall_es  <- tech_sc_es  %>%
  group_by(Country) %>%
  summarize(es  = round(mean(es,  na.rm = TRUE), 4), .groups = "drop")

overall_eo  <- tech_sc_eo  %>%
  group_by(Country) %>%
  summarize(eo  = round(mean(eo,  na.rm = TRUE), 4), .groups = "drop")

overall_pol <- tech_sc_pol %>%
  group_by(Country) %>%
  summarize(pol = round(mean(pol, na.rm = TRUE), 4), .groups = "drop")

overall <- overall_es %>%
  full_join(overall_eo,  by = "Country") %>%
  full_join(overall_pol, by = "Country") %>%
  mutate(
    es  = ifelse(is.na(es),  0.5, es),
    eo  = ifelse(is.na(eo),  0.5, eo),
    pol = ifelse(is.na(pol), 0.5, pol),
    # Composite PSI approximation: 0.4 opportunity + 0.4 friendshore + 0.2 development
    psi = round(0.4 * eo + 0.4 * ((1 - es) * 0.5 + eo * 0.5) + 0.2 * pol, 4)
  )

# Category-level summaries (per country, averaged across tech × SC)
# ES categories are risk-oriented in the pipeline; we invert here for consistency
es_cat_summary <- es_cats %>%
  filter(!is.na(category_score)) %>%
  group_by(Country, category) %>%
  summarize(
    score = round(mean(1 - category_score, na.rm = TRUE), 4),  # invert risk→security
    .groups = "drop"
  ) %>%
  mutate(
    # Normalise category names to JS-compatible keys (lowercase, underscores)
    key = tolower(gsub("[^a-zA-Z0-9]", "_", trimws(category)))
  )

eo_cat_summary <- eo_cats %>%
  filter(!is.na(category_score)) %>%
  group_by(Country, category) %>%
  summarize(
    score = round(mean(category_score, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  mutate(
    key = tolower(gsub("[^a-zA-Z0-9]", "_", trimws(category)))
  )

message("  Countries with ES: ", n_distinct(overall_es$Country))
message("  Countries with EO: ", n_distinct(overall_eo$Country))
message("  Countries with Policy: ", n_distinct(overall_pol$Country))

# ── 4. Load existing country metadata from data.js ───────────────────────────
message("\n[4/6] Reading country metadata from dashboard/js/data.js...")
data_js_path <- file.path(repo_root, "dashboard", "js", "data.js")
if (!file.exists(data_js_path)) {
  stop("dashboard/js/data.js not found at: ", data_js_path)
}
js_lines <- readLines(data_js_path, warn = FALSE)

begin_idx <- grep("\\[BEGIN_COUNTRY_DATA\\]", js_lines)[1]
end_idx   <- grep("\\[END_COUNTRY_DATA\\]",   js_lines)[1]
if (is.na(begin_idx) || is.na(end_idx)) {
  stop(
    "Could not find [BEGIN_COUNTRY_DATA] / [END_COUNTRY_DATA] markers in data.js.\n",
    "These markers are required for safe injection. Ensure data.js was generated\n",
    "by this pipeline (version with marker comments)."
  )
}

# Find the country-data array boundaries inside the marked block
rc_start <- grep("^const RAW_COUNTRIES = \\[", js_lines)[1]
if (is.na(rc_start) || rc_start < begin_idx || rc_start > end_idx) {
  stop("Could not find 'const RAW_COUNTRIES = [' between the data markers.")
}
after_rc    <- js_lines[(rc_start + 1):end_idx]
close_rel   <- which(trimws(after_rc) == "];")[1]
if (is.na(close_rel)) stop("Could not find closing '];" in the RAW_COUNTRIES block.")
rc_close    <- rc_start + close_rel

# Extract the raw JS lines for each country entry
country_js_lines_raw <- js_lines[(rc_start + 1):(rc_close - 1)]
country_js_lines_raw <- country_js_lines_raw[nchar(trimws(country_js_lines_raw)) > 2]

# Parse metadata (iso3, isoN, name, region, align, eTypes) from each JS line
parse_js_meta <- function(line) {
  extract <- function(pattern) {
    m <- regmatches(line, regexpr(pattern, line, perl = TRUE))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m
  }
  iso3   <- sub('iso3:"([A-Z]{2,3})"', '\\1',
                extract('iso3:"[A-Z]{2,3}"'))
  isoN   <- suppressWarnings(as.integer(
               sub('isoN:(\\d+)', '\\1', extract('isoN:\\d+'))))
  name   <- sub('name:"([^"]+)"', '\\1',
                extract('name:"[^"]+"'))
  region <- sub('region:"([^"]+)"', '\\1',
                extract('region:"[^"]+"'))
  align  <- sub('align:"([^"]+)"', '\\1',
                extract('align:"[^"]+"'))
  etypes_str <- sub('.*eTypes:\\[([^\\]]*)\\].*', '\\1', line, perl = TRUE)
  etypes <- trimws(strsplit(gsub('"', '', etypes_str), ",")[[1]])
  etypes <- etypes[nchar(etypes) > 0]
  list(iso3 = iso3, isoN = isoN, name = name,
       region = region, align = align, eTypes = etypes)
}

meta_list <- lapply(country_js_lines_raw, parse_js_meta)
# Remove any parse failures (no iso3)
meta_list <- meta_list[!sapply(meta_list, function(m) is.na(m$iso3))]

message("  Found ", length(meta_list), " existing country entries in data.js")

# ── 5. Build updated country entries ─────────────────────────────────────────
message("\n[5/6] Merging pipeline scores with metadata...")

# Build lookup: country name (from metadata) → overall scores
# Pipeline uses the same country name conventions as countrycode; try exact match first
overall_by_name <- overall
names_in_meta   <- sapply(meta_list, `[[`, "name")

matched   <- 0
unmatched <- character(0)

build_entry <- function(meta) {
  cname <- meta$name

  # Exact name match to pipeline outputs
  ov <- overall_by_name[overall_by_name$Country == cname, ]

  # Fallback: try common name substitutions used across the pipeline
  if (nrow(ov) == 0) {
    alt_names <- c(
      "United States" = "USA", "South Korea" = "Republic of Korea",
      "North Korea"   = "Dem. People's Rep. of Korea",
      "Ivory Coast"   = "Cote d'Ivoire",
      "Czech Republic"= "Czechia",
      "Viet Nam"      = "Vietnam"
    )
    # Reverse lookup: meta name → alt pipeline name
    pipeline_name <- names(alt_names)[match(cname, alt_names)]
    if (!is.na(pipeline_name)) {
      ov <- overall_by_name[overall_by_name$Country == pipeline_name, ]
    }
  }

  # Country-level scores
  if (nrow(ov) > 0) {
    es_val  <- ov$es[1]
    eo_val  <- ov$eo[1]
    pol_val <- ov$pol[1]
    psi_val <- ov$psi[1]
  } else {
    es_val <- eo_val <- pol_val <- psi_val <- 0.5
  }

  # Tech × SC scores for this country
  tc <- tech_sc_all[tech_sc_all$Country == cname, ]
  if (nrow(tc) == 0) {
    # Try alternate name
    tc <- tech_sc_all[tech_sc_all$Country %in% names(which(
      sapply(c(cname), function(n) {
        alt <- names(alt_names)[match(n, alt_names)]
        if (!is.na(alt)) alt else character(0)
      })
    )), ]
  }

  scores_obj <- list()
  if (nrow(tc) > 0) {
    for (i in seq_len(nrow(tc))) {
      key <- paste0(tc$tech[i], "|", tc$supply_chain[i])
      entry <- list()
      if (!is.na(tc$es[i]))  entry[["es"]]  <- tc$es[i]
      if (!is.na(tc$eo[i]))  entry[["eo"]]  <- tc$eo[i]
      if (!is.na(tc$pol[i])) entry[["pol"]] <- tc$pol[i]
      if (length(entry) > 0) scores_obj[[key]] <- entry
    }
  }

  # Category breakdowns
  es_c <- es_cat_summary[es_cat_summary$Country == cname, ]
  eo_c <- eo_cat_summary[eo_cat_summary$Country == cname, ]

  es_cats_obj <- if (nrow(es_c) > 0) {
    setNames(as.list(es_c$score), es_c$key)
  } else list()

  eo_cats_obj <- if (nrow(eo_c) > 0) {
    setNames(as.list(eo_c$score), eo_c$key)
  } else list()

  list(
    iso3         = meta$iso3,
    isoN         = meta$isoN,
    name         = cname,
    region       = meta$region,
    align        = meta$align,
    eTypes       = if (length(meta$eTypes) > 0) meta$eTypes else list("developing"),
    es           = es_val,
    eo           = eo_val,
    pol          = pol_val,
    psi          = psi_val,
    scores       = scores_obj,
    esCategories = es_cats_obj,
    eoCategories = eo_cats_obj
  )
}

country_entries <- lapply(meta_list, build_entry)

# Report match statistics
has_scores <- sapply(country_entries, function(e) length(e$scores) > 0)
message("  Countries with tech×SC scores: ", sum(has_scores), " / ", length(country_entries))
message("  Countries without scores (using overall defaults): ",
        sum(!has_scores))

# ── 6. Write outputs ──────────────────────────────────────────────────────────
message("\n[6/6] Writing outputs...")

# ── 6a. Intermediate JSON ─────────────────────────────────────────────────────
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)
json_path <- file.path(outputs_dir, "dashboard_data.json")
writeLines(
  toJSON(country_entries, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  json_path
)
message("  Wrote: ", json_path)

# ── 6b. Serialize each country as a compact JS object line ───────────────────
js_country_lines <- vapply(country_entries, function(e) {
  paste0("  ", toJSON(e, auto_unbox = TRUE, null = "null", digits = 4))
}, character(1))

# Insert comma between entries (all but last get a trailing comma)
n <- length(js_country_lines)
if (n > 1) {
  js_country_lines[seq_len(n - 1)] <- paste0(js_country_lines[seq_len(n - 1)], ",")
}

new_data_block <- c(
  "// [BEGIN_COUNTRY_DATA] \u2014 replaced by scripts/export_dashboard_data.R",
  paste0("// \u2500\u2500\u2500 Real country data (", n, " countries from pipeline computed indices) ",
         paste(rep("\u2500", 4), collapse="")),
  "// Fields: iso3, isoN (ISO-3166 numeric), name, region, align, eTypes,",
  "//         es, eo, pol, psi (overall scores), scores (tech\u00d7SC breakdowns)",
  "const RAW_COUNTRIES = [",
  js_country_lines,
  "];",
  "// [END_COUNTRY_DATA]"
)

# ── 6c. Patch data.js between the markers ────────────────────────────────────
new_js <- c(
  js_lines[seq_len(begin_idx - 1)],
  new_data_block,
  js_lines[(end_idx + 1):length(js_lines)]
)

writeLines(new_js, data_js_path)
message("  Updated: ", data_js_path)

# ── Summary ───────────────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("Export complete.")
message("  Countries written : ", n)
message("  Tech\u00d7SC entries   : ", sum(sapply(country_entries, function(e) length(e$scores))))
message(strrep("=", 60))
message("\nNext steps:")
message("  1. Rebuild the standalone file (optional):")
message("       python3 scripts/build_standalone.py   (or equivalent)")
message("  2. Open dashboard/index.html in a browser.")
message("")
