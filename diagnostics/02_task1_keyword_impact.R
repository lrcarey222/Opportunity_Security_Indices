# ==============================================================================
# Task 1 - keyword boundary fix: verification and impact measurement
# ------------------------------------------------------------------------------
# Writes diagnostics/keyword_audit.csv (the neis_audit_keywords() report on the
# real Title|Source text) and prints the before/after corroboration counts.
#
# Run from the repo root:
#   Rscript diagnostics/02_task1_keyword_impact.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("OPSI_REPO_ROOT"))) Sys.getenv("OPSI_REPO_ROOT") else getwd(),
  winslash = "/", mustWork = TRUE
)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))

out_dir <- file.path(repo_root, "diagnostics")
hr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

# ---- 1) Helper contract ------------------------------------------------------
hr("neis_bound_kws() contract")
bd <- neis_bound_kws(c("ai", "\\bpv\\b", "cells?", "\\bmanufactur\\w*", "turbine"))
stopifnot(identical(bd[1], "\\bai\\b"))
stopifnot(identical(bd[2], "\\bpv\\b"))            # already bounded, untouched
stopifnot(identical(bd[3], "cells?"))              # regex syntax, untouched
stopifnot(identical(bd[4], "\\bmanufactur\\w*"))   # prefix regex, untouched
stopifnot(identical(bd[5], "\\bturbine\\b"))
bdp <- neis_bound_kws(c("turbine", "gas"), allow_plural = TRUE)
stopifnot(identical(bdp[1], "\\bturbines?\\b"))
stopifnot(identical(bdp[2], "\\bgas\\b"))          # already ends in s, no suffix
cat("all helper assertions passed\n")

# ---- 2) Prefix terms still work ---------------------------------------------
hr("prefix terms survive bounding")
prefix_checks <- list(
  c("manufactur", "Japan: Subsidy for battery manufacturing plant", "Midstream"),
  c("refin", "Indonesia: Support for nickel refining capacity", "Upstream"),
  c("smelt", "Chile: Aid for copper smelting operations", "Upstream"),
  c("install", "Germany: Rebate for rooftop solar installation", "Downstream"),
  c("deploy", "India: Grid deployment programme", "Downstream")
)
for (p in prefix_checks) {
  got <- supply_chain_keyword_evidence(p[3], p[2], "")
  cat(sprintf("%-12s %-52s -> %s %s\n", p[1], p[3], got, if (got) "OK" else "*** BROKEN ***"))
  stopifnot(isTRUE(got))
}

# ---- 3) The four review titles ----------------------------------------------
hr("Task 1 - the four verified false-positive titles")
titles <- c(
  "Russia: Sanctions on entities in Ukraine-related list",
  "Germany: Financial grant for rail freight operators",
  "Japan: Subsidy for offshore wind supply chain development",
  "EU: State aid for maintenance of hydropower plants"
)

cmp <- lapply(titles, function(t) {
  tibble::tibble(
    title = substr(t, 1, 52),
    semis_legacy = keyword_evidence("Semiconductors", t, "", dict = TECH_KEYWORDS_LEGACY),
    semis_fixed = keyword_evidence("Semiconductors", t, "", dict = TECH_KEYWORDS),
    down_legacy = supply_chain_keyword_evidence("Downstream", t, "", dict = SUPPLY_CHAIN_KEYWORDS_LEGACY),
    down_fixed = supply_chain_keyword_evidence("Downstream", t, "", dict = SUPPLY_CHAIN_KEYWORDS)
  )
})
print(as.data.frame(dplyr::bind_rows(cmp)), right = FALSE)

cat("\nNOTE: title 4 still matches Downstream after the fix, via the standalone\n")
cat("word 'maintenance' - a deliberate Downstream term the review did not ask to\n")
cat("remove. That is a TRUE positive (O&M of a generation asset), not the 'ai'\n")
cat("bug. The other three clear both dictionaries.\n")

# ---- 4) Impact on the real export -------------------------------------------
hr("impact on the real export")
raw_data_path <- file.path(repo_root, "data", "raw")
nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
raw <- readxl::read_excel(nipo_policy_path, sheet = 1)
hay <- paste(dplyr::coalesce(raw$Title, ""), dplyr::coalesce(raw$Source, ""), sep = " | ")
n <- length(hay)
cat("records: ", n, "\n", sep = "")

# str_detect() is vectorised over the text, so evaluate each term against the
# whole vector once and OR the results, rather than looping per record.
any_term_hit <- function(terms, text_vec) {
  out <- rep(FALSE, length(text_vec))
  for (k in terms) out <- out | stringr::str_detect(text_vec, k)
  out
}

hay_lower <- stringr::str_to_lower(hay)

impact <- lapply(
  list(
    list(grp = "Semiconductors", new = TECH_KEYWORDS, leg = TECH_KEYWORDS_LEGACY),
    list(grp = "Solar", new = TECH_KEYWORDS, leg = TECH_KEYWORDS_LEGACY),
    list(grp = "Upstream", new = SUPPLY_CHAIN_KEYWORDS, leg = SUPPLY_CHAIN_KEYWORDS_LEGACY),
    list(grp = "Midstream", new = SUPPLY_CHAIN_KEYWORDS, leg = SUPPLY_CHAIN_KEYWORDS_LEGACY),
    list(grp = "Downstream", new = SUPPLY_CHAIN_KEYWORDS, leg = SUPPLY_CHAIN_KEYWORDS_LEGACY)
  ),
  function(s) {
    leg <- any_term_hit(s$leg[[s$grp]], hay_lower)
    new <- any_term_hit(s$new[[s$grp]], hay_lower)
    tibble::tibble(
      group = s$grp,
      legacy_hits = sum(leg),
      legacy_rate = mean(leg),
      fixed_hits = sum(new),
      fixed_rate = mean(new),
      lost = sum(leg & !new),
      gained = sum(!leg & new)
    )
  }
)
impact <- dplyr::bind_rows(impact)
print(as.data.frame(impact %>% mutate(across(ends_with("rate"), ~ round(.x, 4)))), right = FALSE)
utils::write.csv(impact, file.path(out_dir, "keyword_impact.csv"), row.names = FALSE)

# ---- 5) Audit of remaining terms --------------------------------------------
hr("neis_audit_keywords() - remaining over-firing terms (top 25)")
audit <- dplyr::bind_rows(
  neis_audit_keywords(TECH_KEYWORDS, hay_lower) %>% mutate(dict = "tech"),
  neis_audit_keywords(SUPPLY_CHAIN_KEYWORDS, hay_lower) %>% mutate(dict = "supply_chain")
) %>%
  arrange(desc(.data$hit_rate))

print(as.data.frame(audit %>% mutate(hit_rate = round(hit_rate, 4)) %>% head(25)), right = FALSE)
utils::write.csv(audit, file.path(out_dir, "keyword_audit.csv"), row.names = FALSE)

cat("\nflagged terms (hit_rate > 5%): ", sum(audit$flagged), " of ", nrow(audit), "\n", sep = "")
message("\nWrote keyword_audit.csv and keyword_impact.csv")
