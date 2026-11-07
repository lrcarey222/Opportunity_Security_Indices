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

# Partnership agreements, step 2 of 3 (docs/partnership_agreements.md, sections 5-8): link the
# ingested source records into canonical agreements, roll them up to country pairs with a depth
# class, and join the pairs to the need / capability data (agreement gaps).
#
#   Rscript scripts/36_build_agreement_partnerships.R [--in-dir data/processed/agreements]
#     [--processed-dir <config processed_dir>] [--out-dir <in-dir>] [--as-of YYYY-MM-DD]
#
# Inputs:
# - --in-dir: source_<id>.rds written by scripts/35 (every source present is used).
# - --processed-dir (capability data): outputs/strategic_position_tbl.rds (scripts/26),
#   partner_compat_cells_tbl.rds (scripts/15; looked for in the processed dir, then outputs/) and
#   outputs/aib/aib_relationship_theses.csv (scripts/33). Without strategic_position_tbl the gap
#   tables are written empty, with a message. partnership_index_outputs.rds is never read.
# - config/coalition.yml eu_node$members (EU member states, for via-EU pairs).
# Outputs (--out-dir): canonical_agreements, agreement_links, agreement_candidates,
#   canonical_codes, pair_profiles, agreement_gaps, execution_gaps, agreement_delivery
#   (.rds and .csv), build_meta.rds and build_report.md. Files are overwritten whole.
# Functions: R/agreements/agr_link.R, agr_pairs.R, agr_gaps.R, agr_export.R (pure).

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

for (f in c("agr_schema.R", "agr_text_coding.R", "agr_link.R", "agr_pairs.R", "agr_gaps.R", "agr_export.R")) {
  source(file.path(repo_root, "R", "agreements", f))
}

cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_agreements.yml"))
coalition_cfg <- yaml::read_yaml(file.path(repo_root, "config", "coalition.yml"))
eu_members <- sort(unlist(coalition_cfg$eu_node$members))

in_dir <- abs_path(arg_value("--in-dir", file.path(config$processed_dir, "agreements")))
processed_dir <- abs_path(arg_value("--processed-dir", config$processed_dir))
out_dir <- abs_path(arg_value("--out-dir", in_dir))
as_of <- agr_norm_date(arg_value("--as-of", as.character(cfg$pairs$as_of)))
if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", as_of)) stop("--as-of must be YYYY-MM-DD")

write_both <- function(df, name) {
  saveRDS(df, file.path(out_dir, paste0(name, ".rds")))
  utils::write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
first_existing <- function(paths) { p <- paths[file.exists(paths)]; if (length(p)) p[1] else "" }

# --- 1. sources --------------------------------------------------------------------------------
src_files <- file.path(in_dir, sprintf("source_%s.rds", AGR_SOURCES))
src_files <- src_files[file.exists(src_files)]
if (!length(src_files)) stop("no source_<id>.rds in ", in_dir, " (run scripts/35 first)")
src <- lapply(src_files, readRDS)
names(src) <- vapply(src, function(s) s$meta$source, "")
step("sources: ", paste(sprintf("%s %d", names(src), vapply(src, function(s) nrow(s$records), 0L)), collapse = ", "))
records <- do.call(rbind, lapply(src, `[[`, "records")); rownames(records) <- NULL
codes <- do.call(rbind, lapply(src, `[[`, "codes")); rownames(codes) <- NULL
extras <- lapply(src, function(s) s$extras %||% list())
dup <- records$source_record_id[duplicated(records$source_record_id)]
if (length(dup)) stop("duplicate source_record_id across sources: ", paste(head(dup, 5), collapse = ", "))

# --- 2. linkage --------------------------------------------------------------------------------
t0 <- Sys.time()
L <- agr_link_records(records, codes, cfg, extras)
if (nrow(L$links) != nrow(records)) stop("linkage lost source records: ", nrow(records), " in, ", nrow(L$links), " linked")
step(sprintf("linkage: %d records -> %d canonical agreements in %.0f s", nrow(records), nrow(L$canonical),
             as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# --- 3. pair roll-up ---------------------------------------------------------------------------
desta_design <- extras$desta$desta_design
flags <- agr_agreement_flags(L$canonical, L$canonical_codes, cfg, as_of, desta_design)
canonical <- cbind(L$canonical, flags[match(L$canonical$canonical_agreement_id, flags$canonical_agreement_id),
                                      c("active", "recent", "eligible", "ei", "trade", "in_effect", "trade_depth_index")])
rownames(canonical) <- NULL
pairs <- agr_pair_profiles(L$canonical, L$canonical_codes, cfg, eu_members, as_of, desta_design)
step(sprintf("pairs: %d pair profiles", nrow(pairs)))

# --- 4. gaps -----------------------------------------------------------------------------------
sp_path <- file.path(processed_dir, "outputs", "strategic_position_tbl.rds")
pc_path <- first_existing(file.path(processed_dir, c("partner_compat_cells_tbl.rds", file.path("outputs", "partner_compat_cells_tbl.rds"))))
th_path <- file.path(processed_dir, "outputs", "aib", "aib_relationship_theses.csv")
gap_inputs <- character()
if (file.exists(sp_path)) {
  sp <- readRDS(sp_path)
  keep <- intersect(c("iso3", "Country", "tech", "supply_chain", "supply_chain_need", "deployment_need_pct", "deployment_need",
                      "competitive_position_percentile", paste0("recommended_partner_", 1:3), "strategic_choice",
                      "policy_priority_percentile"), names(sp))
  position <- as.data.frame(sp)[, keep]
  rm(sp); invisible(gc())
  compat <- NULL
  if (nzchar(pc_path)) {
    pc <- readRDS(pc_path)
    compat <- as.data.frame(pc)[, c("reporter_iso", "partner_iso", "tech", "supply_chain", "friendshore")]
    rm(pc); invisible(gc())
  } else step("gaps: partner_compat_cells_tbl.rds not found under ", processed_dir, "; friendshore left empty")
  al_path <- first_existing(file.path(processed_dir, c("geo_alignment_tbl.rds", file.path("outputs", "geo_alignment_tbl.rds"))))
  alignment <- NULL
  if (nzchar(al_path)) {
    al <- readRDS(al_path)
    alignment <- as.data.frame(al)[, c("reporter_iso", "partner_iso", "geo_alignment")]
    rm(al)
  } else step("gaps: geo_alignment_tbl.rds not found under ", processed_dir, "; complementarity not alignment-gated")
  theses <- if (file.exists(th_path)) utils::read.csv(th_path, colClasses = "character", na.strings = character(),
                                                      encoding = "UTF-8", check.names = FALSE) else NULL
  if (is.null(theses)) step("gaps: ", th_path, " not found; aib_shared left empty")
  gap_inputs <- c(sp_path, pc_path[nzchar(pc_path)], al_path[nzchar(al_path)], th_path[file.exists(th_path)])
  t0 <- Sys.time()
  G <- agr_gaps(pairs, canonical, L$canonical_codes, position, compat, theses, cfg, alignment_tbl = alignment)
  rm(compat); invisible(gc())
  step(sprintf("gaps: %d directed pair x technology rows, %d execution gaps in %.0f s", nrow(G$gaps), nrow(G$execution_gaps),
               as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  universe <- sort(unique(position$iso3))
} else {
  step("gaps: ", sp_path, " not found; agreement_gaps and execution_gaps written empty")
  G <- agr_gaps(pairs[0, ], canonical[0, ], L$canonical_codes[0, ], data.frame(iso3 = character(), tech = character(),
                supply_chain = character()), NULL, NULL, cfg)
  universe <- character()
}

# --- 5. write ----------------------------------------------------------------------------------
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
delivery <- agr_export_delivery(extras, L$links)
if (is.null(delivery)) delivery <- data.frame(canonical_agreement_id = character(), text = character(), status = character(),
                                              url = character(), stringsAsFactors = FALSE)
write_both(canonical, "canonical_agreements")
write_both(L$links, "agreement_links")
write_both(L$candidates, "agreement_candidates")
write_both(L$canonical_codes, "canonical_codes")
write_both(pairs, "pair_profiles")
write_both(G$gaps, "agreement_gaps")
write_both(G$execution_gaps, "execution_gaps")
write_both(delivery, "agreement_delivery")
src_meta <- lapply(src, `[[`, "meta")
saveRDS(list(as_of = as_of, built_on = format(Sys.Date()), eu_members = eu_members, universe = universe,
             sources = src_meta, gap_inputs = if (length(gap_inputs)) data.frame(file = gap_inputs, md5 = unname(tools::md5sum(gap_inputs))) else NULL),
        file.path(out_dir, "build_meta.rds"))

# --- 6. build report ---------------------------------------------------------------------------
tab_md <- function(x, h1, h2) {
  x <- x[order(names(x))]
  c(sprintf("| %s | %s |", h1, h2), "|---|---:|", sprintf("| %s | %s |", names(x), format(as.integer(x), big.mark = ",")))
}
depth_md <- function() {
  cl <- as.character(0:5)
  a <- table(factor(pairs$depth_all, levels = cl)); e <- table(factor(pairs$depth_ei, levels = cl))
  c("| Class | All agreements | Electro-industrial |", "|---|---:|---:|",
    sprintf("| %s %s | %s | %s |", cl, unlist(cfg$depth$classes[cl]), format(as.integer(a), big.mark = ","), format(as.integer(e), big.mark = ",")))
}
pair_isos <- unique(c(pairs$a, pairs$b))
pair_isos_d3 <- unique(c(pairs$a[pairs$depth_all >= 3], pairs$b[pairs$depth_all >= 3]))
top <- pairs[order(-pairs$depth_all, -pairs$depth_ei, -pairs$n_sectors, -pairs$n_agreements, pairs$pair, method = "radix"), ][1:min(20, nrow(pairs)), ]
cand <- L$candidates
cand_tab <- if (nrow(cand)) table(paste(cand$rule, cand$reason)) else table(character())
rep_lines <- c(
  "# Partnership agreements build report", "",
  sprintf("Built %s by scripts/36 (as of %s) from %s.", format(Sys.Date()), as_of, in_dir), "",
  "> There is no comprehensive global registry of non-binding MOUs, joint statements and joint initiatives.",
  "> Every source here misses some of them. An empty cell means no agreement was found in these sources,",
  "> not that no cooperation exists. Depth classes are provisional (docs/partnership_agreements.md, section 7).", "",
  "## Records per source", "", tab_md(vapply(src, function(s) nrow(s$records), 0L), "Source", "Records"), "",
  sprintf("Canonical agreements: %s (from %s records); with two or more datasets: %s.",
          format(nrow(L$canonical), big.mark = ","), format(nrow(records), big.mark = ","), sum(L$canonical$source_count >= 2)), "",
  "## Links by rule", "", tab_md(table(L$links$rule), "Rule", "Records"), "",
  "Candidates considered (rule and outcome):", "", tab_md(cand_tab, "Rule / outcome", "Candidates"), "",
  "## Confidence", "", tab_md(table(L$canonical$confidence), "Confidence", "Canonical agreements"), "",
  "## Depth distribution (pairs)", "", depth_md(), "",
  "## Coverage of the scored universe", "",
  sprintf("Pairs with an agreement: %s. Economies in pairs: %d (EU counted as one party).", format(nrow(pairs), big.mark = ","), length(pair_isos)),
  if (length(universe)) sprintf("Of the %d scored economies, %d have at least one agreement and %d a partner at depth 3 or more; none found for: %s.",
                                length(universe), sum(universe %in% pair_isos), sum(universe %in% pair_isos_d3),
                                paste(setdiff(universe, pair_isos), collapse = ", ")) else "Scored universe not available (no strategic_position_tbl).", "",
  "## Top 20 pairs by depth, then breadth", "",
  "| Pair | Depth all | Depth EI | Sectors | Agreements | Sources |", "|---|---:|---:|---:|---:|---|",
  sprintf("| %s | %d | %d | %d | %d | %s |", top$pair, top$depth_all, top$depth_ei, top$n_sectors, top$n_agreements, top$sources), "",
  "## Agreement gaps", "",
  if (nrow(G$gaps)) tab_md(table(G$gaps$quadrant), "Quadrant", "Directed pair x technology rows") else "No gap rows.", "",
  sprintf("Execution gaps (economy x technology): %d.", nrow(G$execution_gaps)), ""
)
writeLines(rep_lines, file.path(out_dir, "build_report.md"), useBytes = TRUE)
step("wrote ", out_dir)
