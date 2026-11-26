# Bridge runs (docs/change-layer-architecture.md 4): the current engine re-run on a previous vintage's exact input
# files, so a pair whose method and data both changed can be split exactly (posture_compare(bridge = ...)).
# The IO lives in scripts/utils/change_stamps.R; these are the pure parts.
#
# Pure functions: no IO.

# Where each data directory of a bridge run lives inside its sandbox. dirs: named real directories (outputs,
# processed, raw, diagnostics); a directory nested in another listed one keeps its place under the parent's sandbox
# (outputs inside processed -> <sandbox>/processed/outputs). Returns the named sandbox directories.
bridge_sandbox_dirs <- function(dirs, sandbox) {
  norm <- function(p) sub("/+$", "", gsub("\\\\", "/", p))
  real <- vapply(dirs, norm, character(1))
  sb <- norm(sandbox)
  out <- stats::setNames(character(length(real)), names(real))
  for (k in names(real)[order(nchar(real))]) {
    parent <- names(real)[vapply(names(real), function(o) o != k && nchar(real[[o]]) < nchar(real[[k]]) && startsWith(real[[k]], paste0(real[[o]], "/")), logical(1))]
    if (length(parent)) {
      p <- parent[which.max(nchar(real[parent]))]
      out[[k]] <- paste0(out[[p]], substring(real[[k]], nchar(real[[p]]) + 1L))
    } else {
      out[[k]] <- paste(sb, k, sep = "/")
    }
  }
  as.list(out)
}

# The plan for a bridge run. files: the previous vintage's ledger (path -> md5; a path is relative to root unless
# absolute). A file under a listed data directory is materialized at the same place in the sandbox; any other (a
# reference table in the repository) is served by redirecting the read. Returns a data frame: abs, md5, store (the
# content-addressed copy), target (sandbox path, or NA for a redirected read).
bridge_plan <- function(files, root, dirs, sandbox_dirs, store_dir, ext_of = function(p) tools::file_ext(p)) {
  norm <- function(p) sub("/+$", "", gsub("\\\\", "/", p))
  rel <- names(files)
  is_abs <- grepl("^([A-Za-z]:)?/", rel)
  abs <- ifelse(is_abs, norm(rel), paste(norm(root), rel, sep = "/"))
  md5 <- unname(unlist(files))
  ext <- ext_of(abs)
  store <- paste0(norm(store_dir), "/", substr(md5, 1, 2), "/", md5, ifelse(nzchar(ext), paste0(".", ext), ""))
  real <- vapply(dirs, norm, character(1))
  ord <- order(-nchar(real))
  target <- vapply(abs, function(a) {
    for (k in names(real)[ord]) if (startsWith(a, paste0(real[[k]], "/"))) return(paste0(sandbox_dirs[[k]], substring(a, nchar(real[[k]]) + 1L)))
    NA_character_
  }, character(1), USE.NAMES = FALSE)
  data.frame(abs = abs, md5 = md5, store = store, target = target, stringsAsFactors = FALSE)
}

# Is a bridge usable for a pair? It must be pure (no read outside the previous inputs, no input missing from the
# store), built from the pair's previous inputs and with the pair's current method.
bridge_usable <- function(report, prev_header, cur_header) {
  isTRUE(report$pure) && identical(report$inputs_id, prev_header$data$inputs_id) && identical(report$method$id, cur_header$method$id)
}
