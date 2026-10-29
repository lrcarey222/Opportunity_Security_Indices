# Why a score is what it is (spec_country.md A9.6): the categories of Energy Security and Economic
# Opportunity of one cell compared with the middle economy of the sector, their variables, and the raw
# figure behind each variable where one exists. Pure.
#
# Category table (one pillar): iso3, key, category, category_score, category_weight, imputed,
# missing_rule_applied. Variable table: iso3, key, category, variable, component_value,
# component_weight_within_overall (the weight within the category, summing to 1 per category),
# imputed, missing_rule_applied. Both come from index_outputs.rds (keyed by name there; the exporter maps
# names to ISO3 first).

# Members of S_c as a data frame (key, iso3).
country_sc_members <- function(stats) {
  ks <- names(stats)
  do.call(rbind, lapply(ks, function(k) data.frame(key = rep(k, length(stats[[k]]$iso3)), iso3 = stats[[k]]$iso3, stringsAsFactors = FALSE)))
}

# Medians and constancy over S_c: list(cat = env keyed "key\rcategory", var = env keyed
# "key\rcategory\rvariable"), each entry list(iso3, value, median, constant).
country_driver_stats <- function(cat_tbl, var_tbl, stats, tol = 1e-12) {
  sc <- country_sc_members(stats)
  sck <- paste(sc$key, sc$iso3, sep = "\r")
  one <- function(d, by, value) {
    d <- d[paste(d$key, d$iso3, sep = "\r") %in% sck & is.finite(d[[value]]), , drop = FALSE]
    k <- do.call(paste, c(d[by], sep = "\r"))
    sp <- split(seq_len(nrow(d)), k)
    env <- new.env(hash = TRUE, size = length(sp))
    for (nm in names(sp)) {
      i <- sp[[nm]]
      v <- d[[value]][i]
      assign(nm, list(iso3 = d$iso3[i], value = v, median = stats::median(v), constant = (max(v) - min(v)) <= tol), envir = env)
    }
    env
  }
  list(cat = one(cat_tbl, c("key", "category"), "category_score"),
       var = one(var_tbl, c("key", "category", "variable"), "component_value"))
}

country_get <- function(env, key) if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL

# Missing-rule codes ("global_average; zero") -> words; unknown codes are kept as they are.
country_rule_words <- function(rule, words, country) {
  if (is.null(rule) || length(rule) == 0 || is.na(rule) || !nzchar(rule)) return(NA_character_)
  codes <- unique(trimws(strsplit(rule, ";", fixed = TRUE)[[1]]))
  codes <- codes[nzchar(codes)]
  out <- vapply(codes, function(cd) if (!is.null(words[[cd]])) country_fill(words[[cd]], list(country = country)) else cd, "")
  paste(unique(out), collapse = " and ")
}

# Unique, stable keys within a group: slug, then "-2", "-3" for repeats.
country_unique_keys <- function(x) {
  s <- country_slug(x)
  s <- gsub("^-+|-+$", "", s)
  dup <- stats::ave(seq_along(s), s, FUN = seq_along)
  ifelse(dup > 1, paste0(s, "-", dup), s)
}

# Pick the top drivers (A9.6): ES below the median -> the two most negative impacts; EO at or above the
# median -> the two most positive; otherwise the two largest absolute. Constant categories never count.
country_top_drivers <- function(impact, constant, pillar, above_median, n = 2) {
  ok <- is.finite(impact) & !constant & impact != 0
  idx <- which(ok)
  if (!length(idx)) return(integer())
  pick <- if (pillar == "es" && !isTRUE(above_median)) {
    neg <- idx[impact[idx] < 0]
    if (length(neg)) neg[order(impact[neg])] else idx[order(-abs(impact[idx]))]
  } else if (pillar == "eo" && isTRUE(above_median)) {
    pos <- idx[impact[idx] > 0]
    if (length(pos)) pos[order(-impact[pos])] else idx[order(-abs(impact[idx]))]
  } else idx[order(-abs(impact[idx]))]
  utils::head(pick, n)
}

# The drivers of one cell and pillar for one economy. cat_c / var_c: the economy's rows for the cell;
# dstats: country_driver_stats() of the pillar; raw_fn(category, variable) -> RawValue list or NULL
# plus attribute "link"; cfg: config/country_insights.yml.
country_cell_drivers <- function(iso3, country, key, pillar, cat_c, var_c, dstats, above_median, raw_fn, cfg) {
  if (!nrow(cat_c)) return(list())
  max_vars <- cfg$max_vars_per_category %||% 4
  cat_c <- cat_c[order(cat_c$category), , drop = FALSE]
  ck <- country_unique_keys(cat_c$category)
  med <- numeric(nrow(cat_c))
  const <- logical(nrow(cat_c))
  for (i in seq_len(nrow(cat_c))) {
    s <- country_get(dstats$cat, paste(key, cat_c$category[i], sep = "\r"))
    med[i] <- if (is.null(s)) NA_real_ else s$median
    const[i] <- !is.null(s) && isTRUE(s$constant)
  }
  impact <- ifelse(const, 0, cat_c$category_weight * (cat_c$category_score - med))
  top <- country_top_drivers(impact, const, pillar, above_median)
  lapply(seq_len(nrow(cat_c)), function(i) {
    cat_name <- cat_c$category[i]
    # Variables only for the top categories (the "What's behind it" list and the memo's raw figures).
    vars <- list()
    if (i %in% top) {
      vr <- var_c[var_c$category == cat_name, , drop = FALSE]
      vkeys <- country_unique_keys(vr$variable)
      vars <- lapply(seq_len(nrow(vr)), function(j) {
        s <- country_get(dstats$var, paste(key, cat_name, vr$variable[j], sep = "\r"))
        x <- vr$component_value[j]
        others <- if (is.null(s)) numeric() else s$value[s$iso3 != iso3]
        pct <- country_percentile(others, x)
        vmed <- if (is.null(s)) NA_real_ else s$median
        rf <- raw_fn(cat_name, vr$variable[j])
        list(
          key = vkeys[j], variable = vr$variable[j],
          label = rf$label %||% country_var_label(cfg$var_labels, cat_name, vr$variable[j]) %||% vr$variable[j],
          score = country_fv(x, "index", digits = 4), median = country_fv(vmed, "index", digits = 4),
          pct = country_fv(if (is.finite(pct)) round(pct, 2) else NA, "pct"),
          weight = country_fv(vr$component_weight_within_overall[j], "share", digits = 8),
          imputed = isTRUE(vr$imputed[j]),
          rule = country_chr_or_null(vr$missing_rule_applied[j]),
          rule_d = country_chr_or_null(country_rule_words(vr$missing_rule_applied[j], cfg$missing_rules, country)),
          constant = (!is.null(s) && isTRUE(s$constant)) || isTRUE(rf$constant),
          raw = rf$raw,
          sector_link = rf$link,
          .gap = if (is.finite(x) && is.finite(vmed)) abs(x - vmed) else -1
        )
      })
      # 2026-10-14: an index made only of the EU's regional figures (config var_labels `eu_region_mean`: EV
      # manufacturing "Overall Production", the mean of the IEA EV-region indices) is the EU's too when every
      # sibling raw figure is (raw note eu_region_share): it gets the note on a raw value without a figure
      sib <- Filter(function(v) !is.null(v$raw) && !is.null(v$raw$v), vars)
      if (length(sib) && all(vapply(sib, function(v) identical(v$raw$note, "eu_region_share"), TRUE))) {
        for (j in seq_along(vars)) if (is.null(vars[[j]]$raw)) {
          w <- country_var_label_entry(cfg$var_labels, cat_name, vars[[j]]$variable)$eu_region_mean
          if (!is.null(w)) {
            vars[[j]]$raw <- country_fv_raw(NA, "index", unit = NA, year = sib[[1]]$raw$year %||% NA, source = sib[[1]]$raw$source %||% NA,
                                            source_label = sib[[1]]$raw$source_label %||% NA, note = "eu_region_share", note_d = w)
          }
        }
      }
      if (length(vars) > max_vars) vars <- vars[order(-vapply(vars, function(v) v$.gap, 1))][seq_len(max_vars)]
      vars <- lapply(vars, function(v) { v$.gap <- NULL; v })
    }
    list(
      key = ck[i], category = cat_name,
      score = country_fv(cat_c$category_score[i], "index", digits = 4), median = country_fv(med[i], "index", digits = 4),
      weight = country_fv(cat_c$category_weight[i], "share", digits = 8),
      impact = country_fv(if (is.finite(impact[i])) round(impact[i], 4) else NA, "signed_index"),
      imputed = isTRUE(cat_c$imputed[i]),
      rule = country_chr_or_null(cat_c$missing_rule_applied[i]),
      rule_d = country_chr_or_null(country_rule_words(cat_c$missing_rule_applied[i], cfg$missing_rules, country)),
      top = i %in% top, constant = const[i],
      vars = vars
    )
  })
}

# ---- Raw figures ------------------------------------------------------------------------------------

# The unit of a Sector dictionary measure for an item: list(short, kind, scale).
country_dict_unit <- function(dict, var_id, item) {
  v <- NULL
  for (x in dict$vars) if (identical(x$id, var_id)) { v <- x; break }
  if (is.null(v)) return(NULL)
  uid <- v$unit
  for (iu in dict$item_units %||% list()) if (identical(iu$var, var_id) && identical(iu$item, item)) { uid <- iu$unit; break }
  u <- NULL
  for (x in dict$units) if (identical(x$id, uid)) { u <- x; break }
  list(id = uid, short = u$short %||% "", kind = u$unit_kind %||% "number", scale = v$display_scale %||% 1,
       source = v$source, theme = v$theme, data_year = v$data_year, years = v$years)
}

country_source_short <- function(dict, id, names = NULL) {
  if (is.null(id) || is.na(id)) return(NA_character_)
  # config source_names: spelled-out short names (no bare "IEA" in the raw-figure source lines)
  if (!is.null(names[[id]]$short)) return(names[[id]]$short)
  for (s in dict$sources) if (identical(s$id, id)) return(s$short %||% id)
  id
}

# The EU members' shared value of each raw_tables entry marked `eu_region` (a regional figure the pipeline
# copies to every EU member), per (table, tech, stage, category, raw variable): the value most EU members
# carry, kept only when at least `min_share` of the members with a finite value carry it. raw_rows as in
# country_raw_index(); eu_iso3: the EU members. Returns a named numeric (names: the raw key without iso3).
country_eu_region_values <- function(raw_rows, raw_map, eu_iso3, min_share = 0.9, tol = 1e-12) {
  ents <- Filter(function(e) !is.null(e$eu_region), raw_map)
  out <- numeric()
  for (e in ents) {
    r <- raw_rows[raw_rows$table == e$table & raw_rows$category == e$category & raw_rows$variable == e$raw_variable &
                    raw_rows$iso3 %in% eu_iso3 & is.finite(raw_rows$value), , drop = FALSE]
    if (!nrow(r)) next
    g <- paste(r$table, r$tech, r$stage, r$category, r$variable, sep = "\r")
    for (k in unique(g)) {
      v <- r$value[g == k]
      cand <- unique(v)
      hits <- vapply(cand, function(x) sum(abs(v - x) <= tol * max(1, abs(x))), 1)
      if (max(hits) >= min_share * length(v)) out[k] <- cand[which.max(hits)]
    }
  }
  out
}

# The year to print: the bracketed year of the explanation ("[2023]") when there is one, else Year.
country_raw_year <- function(year, explanation) {
  m <- regmatches(explanation %||% "", regexpr("\\[(19|20)\\d{2}\\]", explanation %||% ""))
  if (length(m) && nzchar(m)) return(gsub("[^0-9]", "", m))
  if (is.null(year) || is.na(year)) NA_character_ else as.character(year)
}

# Index the raw rows of the theme tables named in raw_tables. raw_rows: table, iso3, tech, stage,
# category, variable, value, Year, source, explanation. Returns an environment keyed
# "table\riso3\rtech\rstage\rcategory\rvariable" (stage "*" for mineral rows) -> row indices.
country_raw_index <- function(raw_rows) {
  k <- paste(raw_rows$table, raw_rows$iso3, raw_rows$tech, raw_rows$stage, raw_rows$category, raw_rows$variable, sep = "\r")
  km <- paste(raw_rows$table, raw_rows$iso3, raw_rows$tech, "*", raw_rows$category, raw_rows$variable, sep = "\r")
  env <- new.env(hash = TRUE)
  sp <- split(seq_along(k), k)
  for (nm in names(sp)) assign(nm, sp[[nm]], envir = env)
  spm <- split(seq_along(km), km)
  for (nm in names(spm)) assign(nm, spm[[nm]], envir = env)
  env
}

# The plain label of an index component without a raw_tables entry (config var_labels: category and
# variable, or a variable pattern such as "^.* Reserves$"), or NULL.
country_var_label <- function(var_labels, category, variable) {
  country_var_label_entry(var_labels, category, variable)$label
}

# The config var_labels entry of an index component (label, and eu_region_mean for an index of the EU's
# regional figures), or NULL.
country_var_label_entry <- function(var_labels, category, variable) {
  for (e in var_labels %||% list()) {
    if (!is.null(e$category) && !identical(e$category, category)) next
    hit <- if (!is.null(e$variable)) identical(e$variable, variable) else grepl(e$pattern %||% "^$", variable)
    if (hit) return(e)
  }
  NULL
}

# The raw_tables entry for (pillar, category, variable), or NULL.
country_raw_entry <- function(raw_map, pillar, category, variable) {
  for (e in raw_map) if (identical(e$pillar, pillar) && identical(e$category, category) && identical(e$variable, variable)) return(e)
  NULL
}

# A raw-figure function for one economy and cell: f(category, variable) -> list(raw = RawValue or NULL,
# label, link, constant, dict, item); NULL when raw_tables has no entry (no single raw figure). Duplicates are reported
# through dup_fn(entry, n) (validation A7.3 #9). note_fn(var_id, item, iso3, year) -> note or NA. eu_member and
# eu_vals (country_eu_region_values()): an EU member's figure of an `eu_region` entry that equals the EU members'
# shared value gets the note eu_region_share and the entry's words as note_d; other notes take their words from
# cfg$raw_note_words.
country_raw_fn <- function(iso3, cell, pillar, raw_rows, raw_idx, raw_map, dict, cfg, dup_fn = function(...) NULL,
                           note_fn = function(...) NA_character_, eu_member = FALSE, eu_vals = numeric()) {
  restricted <- cfg$restricted_sources %||% character()
  function(category, variable) {
    e <- country_raw_entry(raw_map, pillar, category, variable)
    if (is.null(e)) return(NULL)
    tech <- if (identical(e$raw_tech, "mineral")) sub(" (Reserves|Production)$", "", variable) else cell$tech
    stage <- if (identical(e$raw_tech, "mineral")) "*" else cell$stage
    idx <- country_get(raw_idx, paste(e$table, iso3, tech, stage, category, e$raw_variable, sep = "\r"))
    lab <- e$label %||% variable
    base <- list(raw = NULL, label = lab, link = NULL, constant = isTRUE(e$constant), dict = e$dict, item = NULL)
    if (length(idx) > 1) { dup_fn(e, length(idx)); return(base) }
    if (!length(idx)) return(base)
    r <- raw_rows[idx, , drop = FALSE]
    if (any(vapply(restricted, function(s) grepl(s, r$source, fixed = TRUE), TRUE))) return(base)
    item <- switch(e$item %||% "", fuel = tolower(cell$tech), mineral = country_slug(tech), cell = cell$key, NA_character_)
    du <- if (!is.null(e$dict)) country_dict_unit(dict, e$dict, item) else NULL
    ubc <- e$unit_by_cell[[cell$key]]
    unit <- if (!is.null(ubc)) ubc[[1]] else e[["unit"]] %||% du$short %||% ""
    kind <- if (!is.null(ubc)) ubc[[2]] else e$unit_kind %||% du$kind %||% (if (grepl("^Share", r$explanation %||% "")) "share" else "number")
    scale <- if (!is.null(ubc) || !is.null(e$unit_kind)) 1 else du$scale %||% 1
    src <- e[["source"]] %||% du$source %||% NA_character_
    # a measure with a single data year in the Sector dictionary (EI reserves: end-2020) prints that year
    year <- e[["year"]] %||% (if (!is.null(du) && length(du$years) == 1) as.character(du$years[[1]]) else country_raw_year(r$Year, r$explanation))
    note <- if (!is.null(e$dict)) note_fn(e$dict, item, iso3, year) else NA_character_
    note_d <- NA_character_
    if (isTRUE(eu_member) && !is.null(e$eu_region) && is.finite(r$value)) {
      ev <- eu_vals[paste(e$table, tech, stage, category, e$raw_variable, sep = "\r")]
      if (length(ev) == 1 && !is.na(ev) && abs(r$value - ev) <= 1e-12 * max(1, abs(ev))) {
        note <- "eu_region_share"
        note_d <- e$eu_region
      }
    }
    if (is.na(note_d) && !is.na(note)) note_d <- cfg$raw_note_words[[note]] %||% NA_character_
    out <- country_fv_raw(r$value, kind, unit, scale, year = year, source = src,
                          source_label = country_source_short(dict, src, cfg$source_names), note = note, note_d = note_d)
    link <- if (!is.null(du) && !is.null(du$theme) && !is.na(item)) {
      sprintf("/sector/explore/%s?c=%s&m=%s&item=%s", du$theme, iso3, e$dict, item)
    } else NULL
    list(raw = out, label = lab, link = link, constant = isTRUE(e$constant), dict = e$dict, item = item)
  }
}
