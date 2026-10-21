# The AIB strategic layer's three configs (scripts/33_build_aib_strategic_layer.R):
#   config/aib_strategic_compacts.yml   the strategic compact taxonomy (curated value chains)
#   config/aib_node_functions.yml       node id -> resource / processing / components / systems
#   config/aib_strategic_layer.yml      scenarios, missions, policy menu, role thresholds, capital evidence
aib_read_strategic_config <- function(repo_root, compacts = NULL, node_functions = NULL, layer = NULL) {
  path <- function(p, default) {
    f <- if (is.null(p) || !nzchar(p)) file.path(repo_root, "config", default) else p
    if (!file.exists(f)) stop("AIB strategic layer config not found: ", f)
    f
  }
  files <- c(compacts = path(compacts, "aib_strategic_compacts.yml"), node_functions = path(node_functions, "aib_node_functions.yml"),
             layer = path(layer, "aib_strategic_layer.yml"))
  list(compacts = yaml::read_yaml(files[["compacts"]]), node_functions = yaml::read_yaml(files[["node_functions"]]),
       layer = yaml::read_yaml(files[["layer"]]), files = files)
}
