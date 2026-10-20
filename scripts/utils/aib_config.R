# Allied Industrial Base (AIB) settings: config/aib.yml with the taxonomy overlay
# config/aib_taxonomy_<taxonomy>.yml on top (its keys replace those of aib.yml). The taxonomy is the
# `taxonomy` key of aib.yml unless `taxonomy` is given (the scripts' --taxonomy flag).
# Used by scripts/31_build_aib_trade_network.R and scripts/32_build_aib_partnerships.R.
aib_read_config <- function(repo_root, taxonomy = NULL) {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))
  tx <- if (is.null(taxonomy) || !nzchar(taxonomy)) cfg$taxonomy else taxonomy
  if (is.null(tx) || !nzchar(tx)) tx <- "master"
  path <- file.path(repo_root, "config", paste0("aib_taxonomy_", tx, ".yml"))
  if (!file.exists(path)) stop("Unknown AIB taxonomy '", tx, "': ", path, " not found.")
  overlay <- yaml::read_yaml(path)
  for (k in names(overlay)) cfg[[k]] <- overlay[[k]]
  cfg$taxonomy <- tx
  cfg
}
