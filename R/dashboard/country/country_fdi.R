# Cross-border clean-tech factory projects of one economy (spec_country.md A9.9, CM-FDI): inbound and
# outbound counts by cell and by partner. Counts are projects (a joint venture counts once, under its
# listed parent, as in Sector Insights); only the projects the indices use (index_use). Pure.
#
# projects: pid, key (cell key), home, dest (ISO3), index_use.

country_fdi <- function(projects, iso3, cells = country_cells_tbl()) {
  p <- projects[projects$index_use %in% TRUE & projects$home != projects$dest, , drop = FALSE]
  inb <- p[p$dest == iso3, , drop = FALSE]
  out <- p[p$home == iso3, , drop = FALSE]
  fdi_keys <- sort(unique(projects$key))
  by_cell <- lapply(cells$key, function(k) {
    if (!k %in% fdi_keys) return(NULL)
    list(in_n = country_fv(sum(inb$key == k), "count"),
         in_china_n = country_fv(sum(inb$key == k & inb$home %in% COUNTRY_CHINA_HOMES), "count"),
         out_n = country_fv(sum(out$key == k), "count"))
  })
  names(by_cell) <- cells$key
  in_by <- table(inb$home)
  out_by <- table(out$dest)
  list(
    cells = by_cell,
    partner_in = function(j) as.integer(if (j %in% names(in_by)) in_by[[j]] else 0L),
    partner_out = function(j) as.integer(if (j %in% names(out_by)) out_by[[j]] else 0L),
    n_in = nrow(inb), n_out = nrow(out)
  )
}
