# Cross-border clean-tech factory projects of one economy (spec_country.md A9.9, CM-FDI): inbound and
# outbound counts by cell and by partner. Counts are projects (a joint venture counts once, under its
# listed parent, as in Sector Insights); only the projects the indices use (index_use). Pure.
#
# projects: pid, key (cell key), home, dest (ISO3), index_use; one row per project and cell. Since
# 2026-10-28 a project counts in every stage the pipeline gives it (config/partnership.yml cm_fdi.stages:
# Components and Assembly, which carry the same projects; country_fdi_expand()), so a project has one row
# per stage: the cell counts count it in each of its cells, and the partner and economy totals count it
# once (unique pid), never once per stage.

# The projects table of the Country export from CM-FDI rows (cm_fdi_projects_tbl.rds): one row per project
# and stage of its `stages` list ("Components|Assembly"), keyed on the technology's cell at that stage.
# stages: the stage list per project; unknown stage names stop the export.
country_fdi_expand <- function(pid, tech, stages, home, dest, index_use) {
  st <- strsplit(rep_len(as.character(stages), length(pid)), "|", fixed = TRUE)
  st <- lapply(st, function(v) trimws(v[nzchar(trimws(v))]))
  bad <- setdiff(unique(unlist(st)), COUNTRY_STAGES)
  if (length(bad)) stop("country_fdi_expand(): stages that are not supply-chain stages: ", paste(bad, collapse = ", "))
  if (any(lengths(st) == 0)) stop("country_fdi_expand(): projects without a stage: ", paste(utils::head(pid[lengths(st) == 0], 5), collapse = ", "))
  n <- lengths(st)
  i <- rep(seq_along(pid), n)
  data.frame(pid = pid[i], key = country_cell_key(tech[i], unlist(st)), stage = unlist(st), home = home[i], dest = dest[i],
             index_use = index_use[i], stringsAsFactors = FALSE)
}

# The Sector fdi chunk's project counts in the Country's cells, for the scripts/88 cross-check. The Sector
# chunk keys each project on the cell of the first of its stages (fdi.stages; sector_fdi.R) and its other
# stages' cells carry the same projects, so every row counts in the cell of each listed stage, as
# country_fdi_expand() does. slug: the rows' item slugs ("batteries-components"); home, dest: ISO3; n: the
# rows' project counts. Returns the counts named "<home> <dest> <cell key>". Stops when a row is not on the
# first stage's cell or a stage is not a supply-chain stage.
country_fdi_sector_counts <- function(slug, home, dest, n, stages) {
  stages <- as.character(unlist(stages))
  if (!length(stages) || anyNA(stages) || !all(stages %in% COUNTRY_STAGES)) {
    stop("country_fdi_sector_counts(): fdi.stages must list supply-chain stages, not ", paste(stages, collapse = ", "))
  }
  suf <- paste0("-", tolower(stages[1]))
  off <- !endsWith(slug, suf)
  if (any(off)) stop("country_fdi_sector_counts(): Sector fdi rows not keyed on the ", stages[1], " cell: ", paste(utils::head(unique(slug[off]), 3), collapse = ", "))
  # no rows: nothing to count (paste0() would turn the empty slugs into one "-<stage>" key)
  if (!length(slug)) return(stats::setNames(integer(), character()))
  base <- substr(slug, 1, nchar(slug) - nchar(suf))
  k <- unlist(lapply(stages, function(s) paste(home, dest, paste0(base, "-", tolower(s)))))
  out <- tapply(rep(n, length(stages)), k, sum)
  stats::setNames(as.vector(out), names(out))
}

# The stages the projects count in, in chain order (the sheet's meta.cmfdi_stages): Components and Assembly.
country_fdi_stages <- function(projects, cells = country_cells_tbl()) {
  st <- cells$stage[match(unique(projects$key), cells$key)]
  COUNTRY_STAGES[COUNTRY_STAGES %in% st]
}

country_fdi <- function(projects, iso3, cells = country_cells_tbl()) {
  p <- projects[projects$index_use %in% TRUE & projects$home != projects$dest, , drop = FALSE]
  inb <- p[p$dest == iso3, , drop = FALSE]
  out <- p[p$home == iso3, , drop = FALSE]
  fdi_keys <- sort(unique(projects$key))
  by_cell <- lapply(cells$key, function(k) {
    if (!k %in% fdi_keys) return(NULL)
    list(in_n = country_fv(length(unique(inb$pid[inb$key == k])), "count"),
         in_china_n = country_fv(length(unique(inb$pid[inb$key == k & inb$home %in% COUNTRY_CHINA_HOMES])), "count"),
         out_n = country_fv(length(unique(out$pid[out$key == k])), "count"))
  })
  names(by_cell) <- cells$key
  # partner and economy totals: each project once, whatever the number of stages it counts in
  inb1 <- inb[!duplicated(inb$pid), , drop = FALSE]
  out1 <- out[!duplicated(out$pid), , drop = FALSE]
  in_by <- table(inb1$home)
  out_by <- table(out1$dest)
  list(
    cells = by_cell,
    partner_in = function(j) as.integer(if (j %in% names(in_by)) in_by[[j]] else 0L),
    partner_out = function(j) as.integer(if (j %in% names(out_by)) out_by[[j]] else 0L),
    n_in = nrow(inb1), n_out = nrow(out1)
  )
}
