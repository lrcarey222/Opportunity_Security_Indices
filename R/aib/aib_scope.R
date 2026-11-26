# The AIB's country sets (config/aib.yml demand_scope and candidate_partner_universe). Pure.
#
# set_cfg: one set's config (mode, tiers, countries, include_treat_as_allied); override: a command-line mode
# or a comma list of ISO3 codes (NULL: none); all_econ: every economy of the trade data; coalition: the
# coalition table (iso3, tier) or NULL; exclusions: ISO3 codes never in a set (open ship registries);
# treat_as_allied: config/aib_strategic_layer.yml allied.treat_as_allied.
#
# Modes: global (every economy), coalition (the listed tiers of the coalition table), configured (the listed
# countries). With include_treat_as_allied (2026-11-25, user decision: "allied" is one set, the trusted
# network), a coalition set also takes the strategic layer's treat_as_allied economies (Taiwan, Switzerland,
# Slovenia, Iceland, the UAE): the demand scope is then the same 64-economy trusted network that holds the
# AIB roles, with the treat_as_allied list kept in one place (the strategic layer's config). A comma-list
# override is taken as given. Returns list(id = the mode, iso = ISO3 codes, added = treat_as_allied codes the
# set took beyond its tiers).
aib_resolve_set <- function(set_cfg, override = NULL, all_econ = character(), coalition = NULL, exclusions = character(),
                            treat_as_allied = character()) {
  or <- function(x, y) if (is.null(x)) y else x
  mode <- if (!is.null(override)) override else or(set_cfg$mode, "global")
  if (grepl(",", mode)) {
    return(list(id = "configured", iso = setdiff(toupper(trimws(strsplit(mode, ",", fixed = TRUE)[[1]])), exclusions), added = character()))
  }
  iso <- switch(mode,
    global = all_econ,
    coalition = {
      if (is.null(coalition)) stop("A coalition set needs data/processed/coalition_tbl.rds.")
      coalition$iso3[coalition$tier %in% unlist(or(set_cfg$tiers, list("core", "partner")))]
    },
    configured = toupper(unlist(set_cfg$countries)),
    stop("Unknown set mode: ", mode)
  )
  added <- character()
  if (identical(mode, "coalition") && isTRUE(set_cfg$include_treat_as_allied)) {
    added <- setdiff(toupper(as.character(unlist(treat_as_allied))), c(iso, exclusions))
    iso <- c(iso, added)
  }
  list(id = mode, iso = setdiff(unique(iso), exclusions), added = added)
}
