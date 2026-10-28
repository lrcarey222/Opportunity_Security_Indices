# Review charts for the strategic position framework (docs/strategic_position.md, sections 5-7).
#
# Every chart reads the canonical strategic_position_tbl (one row per Country x tech x
# supply_chain) and returns a ggplot object (chart C can also return its panels as a list);
# scripts save them (no IO here). Six charts:
#
#   A.  sp_plot_country_matrix()          one country: position x policy priority, sized by need,
#                                         coloured by recommended posture, shaped by policy alignment
#   B.  sp_plot_specialization_heatmap()  several countries: competitive position by tech x stage
#   C.  sp_plot_alignment_heatmap()       one country: position, priority, need and posture grids
#   D.  sp_plot_evidence_heatmap()        evidence specificity by tech x stage (one country or mean)
#   E.  sp_plot_posture_map()             several countries: recommended posture by tech x stage
#   E2. sp_plot_posture_mix()             several countries: share of classified cells per posture
#
# Conventions shared by the charts:
#   * percentiles are shown on a 0-100 scale (the table stores 0-1);
#   * the stage axis is always SUPPLY_CHAIN_STAGES in chain order, and every tech x stage cell is
#     drawn, so a cell the table lacks shows as "n/a" rather than vanishing;
#   * missing is never drawn as zero: n/a tiles are light grey with "n/a" printed, distinct from
#     the dark low end of the viridis scale and from every posture hue;
#   * the recommended posture (strategic_posture: secure, compete, deploy) has three hues checked
#     with the dataviz palette validator on white (all pairs: worst CVD delta E 13.9 deutan,
#     worst normal-vision delta E 23.7). Secure is the salient vermillion, Compete the Okabe-Ito
#     blue, Deploy (the most common posture) a calmer sage. Deploy sits below 3:1 contrast on
#     white, so its identity is never colour alone: legends are always drawn and tiles carry a
#     printed code;
#   * policy_alignment (recommended posture vs observed policy) is a printed code on tiles
#     (ok / U / O / M / ?) and a point shape on the matrix, so where policy and posture disagree
#     shows without colour;
#   * the competitive-policy alignment classes (competitive_policy_alignment) are read off the
#     matrix quadrants, named in grey; they are a supporting diagnostic, not a colour.
#
# Needs ggplot2, ggrepel (chart A labels), cowplot (chart C grid) and scales. Source
# R/utils/supply_chain_stages.R and R/strategic_position/sp_helpers.R first.
#
# Sizes scripts/27 renders at (ragg::agg_png(res = 144)): 1600 x 1000 px for A, D and E2,
# 2000 x 1400 for C, 2400 x 1600 for B and E.

# Competitive-policy alignment quadrants (alignment.R), named on the matrix corners.
SP_PLOT_ALIGNMENT_CLASSES <- c("aligned_strength", "latent_advantage", "policy_led_bet", "low_priority")

SP_PLOT_ALIGNMENT_LABELS <- c(
  aligned_strength = "Aligned strength",
  latent_advantage = "Latent advantage",
  policy_led_bet = "Policy-led bet",
  low_priority = "Low priority"
)

# Recommended Yards & Fences posture (posture_engine.R): small, medium and large yard.
SP_PLOT_POSTURE_LEVELS <- c("secure", "compete", "deploy")

SP_PLOT_POSTURE_LABELS <- c(
  secure = "Secure (small yard)",
  compete = "Compete (medium yard)",
  deploy = "Deploy (large yard)"
)

SP_PLOT_POSTURE_COLOURS <- c(
  secure = "#D55E00", # Okabe-Ito vermillion: the salient posture
  compete = "#0072B2", # Okabe-Ito blue
  deploy = "#5CB894" # calm sage: the default posture for most cells
)

# Matrix points the table gives no posture.
SP_PLOT_UNCLASSIFIED_COLOUR <- "grey62"

# Recommended posture vs observed policy (sp_policy_alignment()): tile code, legend label and the
# matrix point shape (fillable shapes 21-25, so the fill can carry the posture).
SP_PLOT_POLICY_ALIGNMENT <- data.frame(
  policy_alignment = c("aligned", "under_prioritized", "over_prioritized", "mixed", "insufficient_data"),
  code = c("ok", "U", "O", "M", "?"),
  label = c("Aligned", "Under-prioritized", "Over-prioritized", "Mixed", "No policy data"),
  shape = c(21, 25, 24, 23, 22),
  stringsAsFactors = FALSE
)

# The policy_alignment values that mean policy and posture disagree (dark outline on the matrix).
SP_PLOT_POLICY_DISAGREE <- c("under_prioritized", "over_prioritized", "mixed")

# Legend entry for the tile marker of a cell with a Secure sensitive layer.
SP_PLOT_LAYER_MARKER <- "A sensitive layer in the cell is Secure"

# Fill and text colour for cells the table does not score.
SP_PLOT_NA_FILL <- "grey90"
SP_PLOT_NA_TEXT <- "grey45"

SP_PLOT_PRIORITY_CAVEAT <- paste(
  "Policy priority = observed NIPO and IEA PAMS attention within the country;",
  "low = low observed intervention, not proof of no priority"
)

SP_PLOT_POLICY_CODE_KEY <- "ok aligned, U under-prioritized, O over-prioritized, M mixed, ? no policy data"

SP_PLOT_EVIDENCE_DIMENSIONS <- c(
  cp = "Competitive Position",
  sn = "Strategic Need",
  pp = "Revealed Policy Priority"
)

SP_PLOT_EVIDENCE_BANDS <- c("high", "medium", "low", "none")

# ---- Shared helpers ------------------------------------------------------------------------------

# House style (theme_minimal, bold title, small grey left-aligned caption) on an explicit white
# background so saved PNGs never come out transparent.
sp_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = "grey25", size = ggplot2::rel(0.85)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.7), hjust = 0, colour = "grey35"),
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

sp_plot_require <- function(tbl, cols, fn) {
  if (!is.data.frame(tbl)) stop(fn, "(): tbl must be a data frame (strategic_position_tbl).")
  missing <- setdiff(cols, names(tbl))
  if (length(missing) > 0) {
    stop(fn, "(): tbl lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

# The requested countries that are in the table, as the table's Country names, in the order
# asked. Accepts Country names or iso3 codes. The requests that matched nothing are kept in
# attr(, "absent") so a review script can say which countries it skipped.
sp_review_country_set <- function(tbl, countries) {
  sp_plot_require(tbl, "Country", "sp_review_country_set")
  asked <- unique(as.character(countries[!is.na(countries)]))
  names_in <- as.character(tbl$Country)
  by_name <- match(asked, names_in)
  by_iso <- if ("iso3" %in% names(tbl)) match(toupper(asked), toupper(as.character(tbl$iso3))) else rep(NA_integer_, length(asked))
  hit <- ifelse(is.na(by_name), by_iso, by_name)
  out <- unique(names_in[hit[!is.na(hit)]])
  attr(out, "absent") <- asked[is.na(hit)]
  out
}

# The requested countries for a multi-country chart; every one must be in the table.
sp_plot_country_set_all <- function(tbl, countries, fn) {
  present <- sp_review_country_set(tbl, countries)
  absent <- attr(present, "absent")
  if (length(present) == 0 || length(absent) > 0) {
    stop(
      fn, "(): ", if (length(absent) > 0) paste0("not in the table: ", paste(absent, collapse = ", ")) else "no countries given",
      ". Use sp_review_country_set() to keep only the countries present."
    )
  }
  as.vector(present)
}

# One country's rows, or a clear error naming the missing country.
sp_plot_country_rows <- function(tbl, country, fn) {
  if (length(country) != 1 || is.na(country)) stop(fn, "(): country must be a single Country name or iso3 code.")
  found <- sp_review_country_set(tbl, country)
  if (length(found) == 0) {
    stop(
      fn, "(): country '", country, "' is not in the table (", length(unique(tbl$Country)),
      " countries present). Use sp_review_country_set() to keep only the countries present."
    )
  }
  tbl[as.character(tbl$Country) == found, , drop = FALSE]
}

# A table that repeats a cell would draw two tiles on top of each other; refuse it.
sp_plot_check_unique <- function(d, keys, fn) {
  dup <- duplicated(as.data.frame(d[keys]))
  if (any(dup)) {
    first <- d[which(dup)[1], keys, drop = FALSE]
    stop(
      fn, "(): the table has more than one row per ", paste(keys, collapse = " x "), " (first: ",
      paste(vapply(first, as.character, character(1)), collapse = " / "), ")."
    )
  }
  invisible(TRUE)
}

sp_plot_stage_factor <- function(x, fn) {
  assert_supply_chain_stages(x, label = paste0(fn, "(): supply_chain"))
  factor(as.character(x), levels = SUPPLY_CHAIN_STAGES)
}

# Facet columns for n countries: one row up to four, then balanced rows of at most four (six
# countries as 3 x 2, not 4 + 2, so every column ends on the same row and no stage axis sits
# beside a panel of the row below).
sp_plot_facet_ncol <- function(n) {
  if (n <= 4) return(max(1L, as.integer(n)))
  as.integer(ceiling(n / ceiling(n / 4)))
}

sp_plot_tech_levels <- function(tech, techs = NULL) {
  if (is.null(techs)) sort(unique(as.character(tech[!is.na(tech)]))) else unique(as.character(techs))
}

# Every tech x stage cell for the given rows (and, with `countries`, every country), joined to the
# table's values so absent cells become NA rows. Tech is a factor whose first level is drawn at
# the top of the grid.
sp_plot_complete_grid <- function(d, techs, value_cols, countries = NULL) {
  grid <- expand.grid(
    tech = techs, supply_chain = SUPPLY_CHAIN_STAGES,
    stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
  )
  keys <- c("tech", "supply_chain")
  if (!is.null(countries)) {
    grid <- merge(data.frame(Country = countries, stringsAsFactors = FALSE), grid, by = NULL)
    keys <- c("Country", keys)
  }
  vals <- as.data.frame(d[c(keys, value_cols)], stringsAsFactors = FALSE)
  for (k in keys) vals[[k]] <- as.character(vals[[k]])
  out <- dplyr::left_join(grid, vals, by = keys)
  out$tech <- factor(out$tech, levels = rev(techs))
  out$supply_chain <- factor(out$supply_chain, levels = SUPPLY_CHAIN_STAGES)
  if (!is.null(countries)) out$Country <- factor(out$Country, levels = countries)
  out
}

# WCAG relative luminance; white text on fills darker than ~0.18 (where white and near-black
# give equal contrast), near-black otherwise.
sp_plot_text_on <- function(fill) {
  out <- rep(SP_PLOT_NA_TEXT, length(fill))
  ok <- !is.na(fill)
  if (!any(ok)) return(out)
  m <- grDevices::col2rgb(fill[ok]) / 255
  lin <- ifelse(m <= 0.03928, m / 12.92, ((m + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  out[ok] <- ifelse(lum < 0.179, "white", "grey10")
  out
}

# The colour scale_fill_viridis_c() gives a value in [0, 1] (its default: six viridis stops,
# interpolated), so tile text can pick its contrast colour.
sp_plot_viridis_at <- function(v) {
  out <- rep(NA_character_, length(v))
  ok <- !is.na(v)
  out[ok] <- scales::gradient_n_pal(scales::viridis_pal()(6))(pmin(pmax(v[ok], 0), 1))
  out
}

sp_plot_pct_label <- function(v) ifelse(is.na(v), "n/a", sprintf("%.0f", 100 * v))

# Wrap each paragraph to `width` characters and join them with line breaks (ggplot does not wrap
# captions, and a long one runs off the canvas).
sp_plot_wrap <- function(paragraphs, width) {
  paste(vapply(paragraphs, function(p) paste(strwrap(p, width), collapse = "\n"), character(1)), collapse = "\n")
}

# "secure 4, compete 12": counts of x over `levels` (absent levels left out), with display labels.
sp_plot_count_levels <- function(x, levels, labels = stats::setNames(levels, levels)) {
  tab <- table(factor(as.character(x), levels = levels))
  tab <- tab[tab > 0]
  if (length(tab) == 0) return("none")
  paste0(unname(labels[names(tab)]), " ", as.integer(tab), collapse = ", ")
}

# Recommended postures as a factor over SP_PLOT_POSTURE_LEVELS; anything else (NA, an unknown
# value) becomes NA.
sp_plot_posture_values <- function(x) {
  x <- as.character(x)
  x[!x %in% SP_PLOT_POSTURE_LEVELS] <- NA_character_
  x
}

# Tile code for the policy alignment of a classified cell: "n/a" where there is no posture,
# "-" where the posture has no policy_alignment value.
sp_plot_policy_code <- function(posture, policy_alignment) {
  code <- SP_PLOT_POLICY_ALIGNMENT$code[match(as.character(policy_alignment), SP_PLOT_POLICY_ALIGNMENT$policy_alignment)]
  ifelse(is.na(posture), "n/a", ifelse(is.na(code), "-", code))
}

# Tech x stage tile grid with a printed label on each tile. `fill_col` holds either a numeric
# percentile (viridis, 0-100) or a factor (with `fill_values`, a named colour vector by level).
# A missing categorical value becomes its own legend entry (`na_label`) rather than ggplot's
# bare "NA".
sp_plot_tile_grid <- function(g, fill_col, label_col, fill_title, text_size, base_size,
                              fill_values = NULL, fill_labels = NULL, stage_angle = 0,
                              na_label = "n/a  Not scored") {
  continuous <- is.null(fill_values)
  missing_value <- is.na(g[[fill_col]])
  if (continuous) {
    g$.fill_hex <- sp_plot_viridis_at(g[[fill_col]])
  } else {
    lv <- c(levels(factor(g[[fill_col]], levels = names(fill_values))), ".na")
    g[[fill_col]] <- factor(ifelse(missing_value, ".na", as.character(g[[fill_col]])), levels = lv)
    fill_values <- c(fill_values, .na = SP_PLOT_NA_FILL)
    fill_labels <- c(if (is.null(fill_labels)) stats::setNames(names(fill_values)[-length(fill_values)], names(fill_values)[-length(fill_values)]) else fill_labels, .na = na_label)
    g$.fill_hex <- unname(fill_values[as.character(g[[fill_col]])])
  }
  g$.text_colour <- ifelse(missing_value, SP_PLOT_NA_TEXT, sp_plot_text_on(g$.fill_hex))

  p <- ggplot2::ggplot(g, ggplot2::aes(x = .data$supply_chain, y = .data$tech)) +
    # show.legend fill = TRUE keeps keys for levels absent from this grid (ggplot2 >= 3.5 drops
    # them); shape = FALSE keeps a tile key out of the posture marker legend (a lone TRUE would
    # count for every legend)
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data[[fill_col]]),
      colour = "white", linewidth = 0.7, show.legend = c(fill = TRUE, shape = FALSE)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[label_col]], colour = .data$.text_colour),
      size = text_size, show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(drop = FALSE, expand = c(0, 0)) +
    ggplot2::scale_y_discrete(drop = FALSE, expand = c(0, 0)) +
    sp_plot_theme(base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = if (stage_angle == 0) {
        ggplot2::element_text(colour = "grey20")
      } else {
        ggplot2::element_text(colour = "grey20", angle = stage_angle, hjust = 1, vjust = 1)
      },
      axis.text.y = ggplot2::element_text(colour = "grey20")
    ) +
    ggplot2::labs(x = NULL, y = NULL)

  if (continuous) {
    p + ggplot2::scale_fill_viridis_c(
      name = fill_title, limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = function(b) sprintf("%.0f", 100 * b), na.value = SP_PLOT_NA_FILL,
      guide = ggplot2::guide_colourbar(barheight = ggplot2::unit(6, "lines"), barwidth = ggplot2::unit(0.7, "lines"))
    )
  } else {
    p + ggplot2::scale_fill_manual(
      name = fill_title, values = fill_values, labels = fill_labels, breaks = names(fill_values),
      drop = FALSE, na.value = SP_PLOT_NA_FILL
    )
  }
}

# Recommended-posture tiles (charts C and E): posture fill (n/a grey where the table gives no
# posture), the policy-alignment code printed on each tile, and a small diamond in the tile's
# top-right corner where sensitive_layers_secure names a layer (a layer's posture never changes
# its cell's posture, so the marker sits next to the cell's own posture rather than recolouring it).
sp_plot_posture_tiles <- function(g, base_size, text_size, stage_angle = 0) {
  posture <- sp_plot_posture_values(g$strategic_posture)
  g$posture <- factor(posture, levels = SP_PLOT_POSTURE_LEVELS)
  g$code <- sp_plot_policy_code(posture, g$policy_alignment)
  layers <- if ("sensitive_layers_secure" %in% names(g)) as.character(g$sensitive_layers_secure) else rep(NA_character_, nrow(g))
  g$secure_layer <- !is.na(layers) & nzchar(trimws(layers))

  p <- sp_plot_tile_grid(
    g, "posture", "code",
    fill_title = "Recommended posture", text_size = text_size, base_size = base_size,
    fill_values = SP_PLOT_POSTURE_COLOURS, fill_labels = SP_PLOT_POSTURE_LABELS,
    na_label = "n/a  Not classified", stage_angle = stage_angle
  ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, order = 1, keyheight = ggplot2::unit(1, "lines"))) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)))

  marks <- g[g$secure_layer, , drop = FALSE]
  if (nrow(marks) == 0) return(p)
  fill_hex <- ifelse(is.na(marks$posture), SP_PLOT_NA_FILL, unname(SP_PLOT_POSTURE_COLOURS[as.character(marks$posture)]))
  marks$.mark_colour <- sp_plot_text_on(fill_hex)
  marks$.mark <- SP_PLOT_LAYER_MARKER
  p +
    ggplot2::geom_point(
      data = marks, ggplot2::aes(shape = .data$.mark, colour = .data$.mark_colour),
      position = ggplot2::position_nudge(x = 0.33, y = 0.25), size = 0.8 * text_size
    ) +
    ggplot2::scale_shape_manual(
      name = NULL, values = stats::setNames(18, SP_PLOT_LAYER_MARKER),
      guide = ggplot2::guide_legend(order = 2, override.aes = list(colour = "grey10", size = 3))
    )
}

sp_plot_check_thresholds <- function(thresholds, fn) {
  if (!is.numeric(thresholds) || !all(c("cp", "pp") %in% names(thresholds)) ||
    any(is.na(thresholds[c("cp", "pp")])) || any(thresholds[c("cp", "pp")] < 0 | thresholds[c("cp", "pp")] > 1)) {
    stop(fn, "(): thresholds must be a named numeric vector c(cp = , pp = ) with values in [0, 1].")
  }
  thresholds[c("cp", "pp")]
}

# ---- A. Country matrix ----------------------------------------------------------------------------

# The cells to label on the country matrix. Policy-posture mismatches come first, up to
# `mismatch_share` of the labels: under-prioritized Secure and Compete cells (Secure first, then
# lowest priority) alternating with over-prioritized Deploy cells (highest priority first). The
# rest rotate through four lists so each kind of outlier is represented: largest |priority_gap|
# (capability and policy attention furthest apart), the cell nearest each quadrant's outer corner
# (the most extreme member of each competitive-policy quadrant, one quadrant at a time), highest
# strategic need, and the remaining mismatches. Returns those rows of `pts` in pick order with
# `label_reason`. Cells missing the measure a list ranks on are not in that list; without
# strategic_posture and policy_alignment columns there are no mismatches.
sp_matrix_label_cells <- function(pts, thresholds = c(cp = 0.5, pp = 0.5), n_labels = 12,
                                  x_col = "competitive_position_percentile", mismatch_share = 0.5) {
  out <- pts[0, , drop = FALSE]
  out$label_reason <- character(0)
  n <- nrow(pts)
  if (n == 0 || n_labels <= 0) return(out)

  x <- pts[[x_col]]
  y <- pts$policy_priority_percentile
  high_x <- x >= thresholds[["cp"]]
  high_y <- y >= thresholds[["pp"]]
  corner_dist <- sqrt((ifelse(high_x, 1, 0) - x)^2 + (ifelse(high_y, 1, 0) - y)^2)
  quadrant <- paste(high_x, high_y)
  rank_in_quadrant <- stats::ave(corner_dist, quadrant, FUN = function(v) rank(v, ties.method = "first"))

  mismatch <- integer(0)
  if (all(c("strategic_posture", "policy_alignment") %in% names(pts))) {
    post <- as.character(pts$strategic_posture)
    pa <- as.character(pts$policy_alignment)
    under <- which(pa %in% "under_prioritized" & post %in% c("secure", "compete"))
    under <- under[order(match(post[under], c("secure", "compete")), y[under])]
    over <- which(pa %in% "over_prioritized" & post %in% "deploy")
    over <- over[order(-y[over])]
    k <- max(length(under), length(over))
    mismatch <- as.vector(rbind(under[seq_len(k)], over[seq_len(k)]))
    mismatch <- mismatch[!is.na(mismatch)]
  }

  lists <- list(
    priority_gap = order(-abs(pts$priority_gap), na.last = NA),
    corner = order(rank_in_quadrant, corner_dist),
    strategic_need = order(-pts$strategic_need_percentile, na.last = NA),
    policy_mismatch = mismatch
  )
  target <- min(n_labels, n)
  quota <- min(length(mismatch), ceiling(target * mismatch_share), target)
  chosen <- mismatch[seq_len(quota)]
  reason <- rep("policy_mismatch", quota)
  pos <- rep(1L, length(lists))
  pos[4] <- quota + 1L
  repeat {
    progressed <- FALSE
    for (k in seq_along(lists)) {
      if (length(chosen) >= target) break
      cand <- lists[[k]]
      while (pos[k] <= length(cand) && cand[pos[k]] %in% chosen) pos[k] <- pos[k] + 1L
      if (pos[k] <= length(cand)) {
        chosen <- c(chosen, cand[pos[k]])
        reason <- c(reason, names(lists)[k])
        pos[k] <- pos[k] + 1L
        progressed <- TRUE
      }
    }
    if (length(chosen) >= target || !progressed) break
  }
  out <- pts[chosen, , drop = FALSE]
  out$label_reason <- reason
  out
}

# "Deployment 11, Recovery 3": counts of flagged cells by stage, in chain order.
sp_plot_count_by_stage <- function(stage, flag) {
  tab <- table(factor(as.character(stage[flag]), levels = SUPPLY_CHAIN_STAGES))
  tab <- tab[tab > 0]
  if (length(tab) == 0) return("")
  paste0(names(tab), " ", as.integer(tab), collapse = ", ")
}

# A. One country's cells on competitive position (x) against revealed policy priority (y,
# percentile within the country), point size = strategic need percentile, fill = the recommended
# posture (strategic_posture: secure / compete / deploy), shape = policy_alignment (circle aligned,
# down-triangle under-prioritized, up-triangle over-prioritized, diamond mixed, square no policy
# data), with a dark outline wherever policy and posture disagree. The quadrant lines sit at
# `thresholds` and the grey corner names are the competitive-policy alignment classes read on the
# plotted axes. Cells missing either axis are not drawn and are counted, by stage, in the caption;
# cells without a need score are drawn hollow (outline in the posture hue) at a fixed size
# (missing need is not low need).
#
# position = "within_country" (default) plots cp_within_country_pct, the within-country percentile
# of the cross-country position percentile that competitive_policy_alignment uses by default
# (re-ranked over the country's cells with a position when the column is absent): across ~200
# countries nearly every cell of a large economy sits above the median, so only the
# within-country view separates its relative strengths. "cross_country" plots the cross-country
# percentile itself.
sp_plot_country_matrix <- function(tbl, country, thresholds = c(cp = 0.5, pp = 0.5), n_labels = 12,
                                   position = c("within_country", "cross_country"),
                                   base_size = 12, caption_width = 170) {
  fn <- "sp_plot_country_matrix"
  position <- match.arg(position)
  within <- position == "within_country"
  sp_plot_require(tbl, c(
    "Country", "tech", "supply_chain", "competitive_position_percentile", "policy_priority_percentile",
    "strategic_need_percentile", "strategic_posture", "policy_alignment", "priority_gap"
  ), fn)
  thresholds <- sp_plot_check_thresholds(thresholds, fn)
  d <- sp_plot_country_rows(tbl, country, fn)
  sp_plot_check_unique(d, SP_CELL_KEYS, fn)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d$stage <- sp_plot_stage_factor(d$supply_chain, fn)
  name <- as.character(d$Country[1])

  d$x_plot <- if (!within) {
    d$competitive_position_percentile
  } else if ("cp_within_country_pct" %in% names(d)) {
    d$cp_within_country_pct
  } else {
    sp_midrank_pct(d$competitive_position_percentile)
  }
  x_na <- is.na(d$x_plot)
  y_na <- is.na(d$policy_priority_percentile)
  pts <- d[!x_na & !y_na, , drop = FALSE]

  posture <- sp_plot_posture_values(pts$strategic_posture)
  posture[is.na(posture)] <- "unclassified"
  posture_levels <- c(SP_PLOT_POSTURE_LEVELS, if (any(posture == "unclassified")) "unclassified")
  pts$posture <- factor(posture, levels = posture_levels)
  pa <- as.character(pts$policy_alignment)
  pa[!pa %in% SP_PLOT_POLICY_ALIGNMENT$policy_alignment] <- "not_compared"
  shape_levels <- c(
    SP_PLOT_POLICY_ALIGNMENT$policy_alignment[1:4],
    intersect(c("insufficient_data", "not_compared"), pa)
  )
  pts$policy <- factor(pa, levels = shape_levels)
  pts$disagree <- pa %in% SP_PLOT_POLICY_DISAGREE
  pts$need_scored <- !is.na(pts$strategic_need_percentile)
  pts$label <- paste(pts$tech, "-", pts$stage, recycle0 = TRUE)

  fills <- c(SP_PLOT_POSTURE_COLOURS, unclassified = SP_PLOT_UNCLASSIFIED_COLOUR)
  fill_labels <- c(SP_PLOT_POSTURE_LABELS, unclassified = "Not classified")
  shapes <- c(stats::setNames(SP_PLOT_POLICY_ALIGNMENT$shape, SP_PLOT_POLICY_ALIGNMENT$policy_alignment), not_compared = 21)
  shape_labels <- c(stats::setNames(SP_PLOT_POLICY_ALIGNMENT$label, SP_PLOT_POLICY_ALIGNMENT$policy_alignment), not_compared = "Not compared (no posture)")
  # Outline: dark where policy and posture disagree, white otherwise; the posture hue on hollow
  # (need not scored) points.
  pts$.outline <- ifelse(
    pts$need_scored, ifelse(pts$disagree, "grey10", "white"), unname(fills[as.character(pts$posture)])
  )
  pts$.stroke <- ifelse(pts$need_scored, ifelse(pts$disagree, 0.9, 0.4), 1)
  # Draw Deploy under Compete under Secure, and agreeing cells under disagreeing ones.
  pts <- pts[order(match(as.character(pts$posture), c("unclassified", "deploy", "compete", "secure")), pts$disagree), , drop = FALSE]
  labelled <- sp_matrix_label_cells(pts, thresholds, n_labels, x_col = "x_plot")

  tx <- thresholds[["cp"]]
  ty <- thresholds[["pp"]]
  # Corner names sit outside the data range (y beyond 0-100) so they never cover a point.
  quads <- data.frame(
    lx = c(1, 1, 0, 0), ly = c(1.075, -0.075, 1.075, -0.075), hjust = c(1, 1, 0, 0),
    label = unname(SP_PLOT_ALIGNMENT_LABELS[SP_PLOT_ALIGNMENT_CLASSES]),
    stringsAsFactors = FALSE
  )

  n_cells <- nrow(d)
  n_out <- sum(x_na | y_na)
  out_note <- if (n_out == 0) {
    sprintf("All %d cells plotted.", n_cells)
  } else {
    sprintf(
      "Not plotted: %d of %d cells lack a position or priority percentile (position missing %d, priority missing %d, both %d; by stage: %s).",
      n_out, n_cells, sum(x_na & !y_na), sum(y_na & !x_na), sum(x_na & y_na),
      sp_plot_count_by_stage(d$stage, x_na | y_na)
    )
  }
  n_need_na <- sum(!pts$need_scored)
  need_note <- if (n_need_na > 0) sprintf(" Hollow markers: strategic need not scored (%d).", n_need_na) else ""
  mix_note <- sprintf(
    " Posture of plotted cells: %s. Policy vs posture: %s.",
    sp_plot_count_levels(pts$posture, posture_levels, c(stats::setNames(SP_PLOT_POSTURE_LEVELS, SP_PLOT_POSTURE_LEVELS), unclassified = "not classified")),
    sp_plot_count_levels(pts$policy, shape_levels, tolower(shape_labels))
  )
  basis <- if ("competitive_policy_basis" %in% names(d)) unique(stats::na.omit(as.character(d$competitive_policy_basis))) else character(0)
  basis_note <- if (length(basis) == 1 && basis != position) {
    sprintf(" The table's competitive_policy_alignment uses the %s position percentile, so its classes can differ from these quadrants.", gsub("_", "-", basis))
  } else {
    ""
  }
  source_note <- ""
  if ("policy_priority_sources" %in% names(pts) && nrow(pts) > 0) {
    src <- table(ifelse(is.na(pts$policy_priority_sources), "none", as.character(pts$policy_priority_sources)))
    source_note <- paste0(" Priority sources of plotted cells: ", paste0(names(src), " ", as.integer(src), collapse = ", "), ".")
  }
  boundary_note <- ""
  if ("alignment_boundary" %in% names(pts) && nrow(pts) > 0) {
    boundary_note <- sprintf(
      " %d plotted cells are within the boundary band of a threshold (alignment_boundary).",
      sum(pts$alignment_boundary %in% TRUE)
    )
  }
  caption <- sp_plot_wrap(c(
    paste0(out_note, need_note, mix_note),
    paste0(
      sprintf("Lines at position %.0f and priority %.0f; the grey corner names are the competitive-policy alignment quadrants.", 100 * tx, 100 * ty),
      basis_note, boundary_note, source_note
    ),
    paste(
      "Labelled: policy-posture mismatches first (under-prioritized Secure and Compete, over-prioritized Deploy),",
      "then the largest |priority gap| (position percentile within the country minus priority), the cell nearest each corner, highest need."
    )
  ), caption_width)

  scored <- pts[pts$need_scored, , drop = FALSE]
  unscored <- pts[!pts$need_scored, , drop = FALSE]
  shape_outline <- ifelse(shape_levels %in% SP_PLOT_POLICY_DISAGREE, "grey10", "grey40")
  # Empty layers are left out (NULL in the list): a manual scale trained on no data warns.
  # show.legend = TRUE keeps legend keys for postures the country has no cell in (ggplot2 >= 3.5
  # draws those keys empty otherwise).
  point_layers <- list(
    if (nrow(scored) > 0) {
      ggplot2::geom_point(
        data = scored,
        ggplot2::aes(
          size = .data$strategic_need_percentile, fill = .data$posture, shape = .data$policy,
          colour = .data$.outline, stroke = .data$.stroke
        ),
        alpha = 0.9, show.legend = TRUE
      )
    },
    if (nrow(unscored) > 0) {
      ggplot2::geom_point(
        data = unscored,
        ggplot2::aes(shape = .data$policy, colour = .data$.outline),
        # a lone named TRUE would count for every legend and paint the posture keys white
        fill = "white", size = 2.6, stroke = 1, show.legend = c(shape = TRUE, fill = FALSE, size = FALSE)
      )
    },
    if (nrow(labelled) > 0) {
      ggrepel::geom_text_repel(
        data = labelled,
        ggplot2::aes(label = .data$label),
        size = base_size / 3.9, colour = "grey10", bg.colour = "white", bg.r = 0.12,
        box.padding = 0.45, point.padding = 0.3, min.segment.length = 0, segment.colour = "grey40",
        segment.size = 0.3, max.overlaps = Inf, seed = 1
      )
    }
  )

  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x_plot, y = .data$policy_priority_percentile)) +
    ggplot2::geom_vline(xintercept = tx, colour = "grey45", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = ty, colour = "grey45", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_text(
      data = quads,
      ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$label, hjust = .data$hjust),
      colour = "grey40", fontface = "bold", size = base_size / 2.7, inherit.aes = FALSE
    ) +
    point_layers +
    ggplot2::scale_x_continuous(
      limits = c(-0.02, 1.02), breaks = seq(0, 1, 0.25), labels = function(b) sprintf("%.0f", 100 * b),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-0.11, 1.11), breaks = seq(0, 1, 0.25), labels = function(b) sprintf("%.0f", 100 * b),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      name = "Recommended posture", values = fills, labels = fill_labels, limits = posture_levels,
      guide = ggplot2::guide_legend(override.aes = list(shape = 21, size = 4, alpha = 1, colour = "white", stroke = 0.4), order = 1)
    ) +
    ggplot2::scale_shape_manual(
      name = "Observed policy vs posture", values = shapes, labels = shape_labels, limits = shape_levels,
      guide = ggplot2::guide_legend(
        override.aes = list(size = 3.4, fill = "grey75", alpha = 1, colour = shape_outline, stroke = 0.9), order = 2
      )
    ) +
    ggplot2::scale_size_continuous(
      name = "Strategic need\n(percentile)", range = c(1.6, 7.5), limits = c(0, 1),
      breaks = c(0.1, 0.5, 0.9), labels = c("10", "50", "90"),
      guide = ggplot2::guide_legend(override.aes = list(shape = 21, fill = "grey75", colour = "white"), order = 3)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(
      title = sprintf("%s: competitive position vs revealed policy priority, by recommended posture", name),
      # the subtitle font is larger than the caption's, so it wraps a little sooner
      subtitle = sp_plot_wrap(c(
        paste0(
          "Each point is a technology x stage cell. Position: percentile ",
          if (within) paste0("within ", name, " of its cross-country percentile. ") else "across countries in the cell. ",
          "Priority: percentile within ", name, ". Size: strategic need."
        ),
        paste(
          "Colour: recommended Yards & Fences posture (observed policy never enters it).",
          "Shape and dark outline: observed policy disagrees with that posture."
        ),
        SP_PLOT_PRIORITY_CAVEAT
      ), round(caption_width * 0.9)),
      x = if (within) {
        paste0("Competitive position, percentile within ", name, " (100 = the country's strongest cell)")
      } else {
        "Competitive position (percentile across countries, 100 = strongest)"
      },
      y = "Revealed policy priority (percentile within country)",
      caption = caption
    ) +
    sp_plot_theme(base_size) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey93", linewidth = 0.3))
}

# ---- B. Specialization heatmap ------------------------------------------------------------------

# B. Competitive position percentile by tech (rows) x stage (columns), one facet per country, in
# the order the countries are given. Every requested country must be in the table (filter first
# with sp_review_country_set()). `techs` limits and orders the rows (default: every tech the
# countries have, alphabetically); a tech the table lacks is drawn as an n/a row.
sp_plot_specialization_heatmap <- function(tbl, countries, techs = NULL, ncol = NULL, base_size = 12) {
  fn <- "sp_plot_specialization_heatmap"
  sp_plot_require(tbl, c("Country", "tech", "supply_chain", "competitive_position_percentile"), fn)
  present <- sp_plot_country_set_all(tbl, countries, fn)
  d <- as.data.frame(tbl[as.character(tbl$Country) %in% present, , drop = FALSE], stringsAsFactors = FALSE)
  sp_plot_check_unique(d, SP_CELL_KEYS, fn)
  sp_plot_stage_factor(d$supply_chain, fn)
  tech_levels <- sp_plot_tech_levels(d$tech, techs)
  if (length(tech_levels) == 0) stop(fn, "(): no technologies to draw.")

  g <- sp_plot_complete_grid(d, tech_levels, "competitive_position_percentile", countries = present)
  g$label <- sp_plot_pct_label(g$competitive_position_percentile)
  n_facets <- length(present)
  if (is.null(ncol)) ncol <- sp_plot_facet_ncol(n_facets)
  text_size <- if (ncol > 3) base_size / 5.2 else base_size / 4.4

  na_by_country <- tapply(is.na(g$competitive_position_percentile), g$Country, sum)
  sp_plot_tile_grid(
    g, "competitive_position_percentile", "label",
    fill_title = "Position\npercentile", text_size = text_size, base_size = base_size,
    stage_angle = if (n_facets > 1) 40 else 0
  ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Country), ncol = ncol) +
    ggplot2::labs(
      title = "Competitive position by technology and supply-chain stage",
      subtitle = "Percentile across countries within each technology x stage cell (100 = strongest). n/a: not scored.",
      caption = sp_plot_wrap(paste0(
        "Cells not scored, of ", length(tech_levels) * length(SUPPLY_CHAIN_STAGES), " per country: ",
        paste0(names(na_by_country), " ", as.integer(na_by_country), collapse = ", "), "."
      ), 170)
    ) +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(0.8, "lines"))
}

# ---- C. Alignment heatmaps ------------------------------------------------------------------------

# The right-hand legend of a ggplot as a grob. ggplot2 >= 3.5 names it guide-box-right; asking
# for that name avoids cowplot::get_legend()'s 'multiple components' warning.
sp_plot_legend_grob <- function(p) {
  g <- tryCatch(cowplot::get_plot_component(p, "guide-box-right"), error = function(e) NULL)
  if (is.null(g)) g <- suppressWarnings(cowplot::get_legend(p))
  g
}

# C. One country's tech x stage grids: competitive position, revealed policy priority and
# strategic need percentiles (viridis, values printed) and the recommended posture (secure /
# compete / deploy fill, the policy-alignment code printed on each tile, a diamond where a
# sensitive layer in the cell is Secure; "n/a" where the table gives no posture). With
# combine = TRUE (default) the four are joined in a 2 x 2 cowplot::plot_grid() with one title and
# caption and the result is a ggplot object; with combine = FALSE the four panels come back as a
# named list of ggplots (position, priority, need, posture) for a script to arrange or save
# separately. sensitive_layers_secure is optional (no markers without it).
sp_plot_alignment_heatmap <- function(tbl, country, techs = NULL, combine = TRUE, base_size = 11,
                                      stage_angle = 30) {
  fn <- "sp_plot_alignment_heatmap"
  value_cols <- c(
    "competitive_position_percentile", "policy_priority_percentile",
    "strategic_need_percentile", "strategic_posture", "policy_alignment"
  )
  sp_plot_require(tbl, c("Country", "tech", "supply_chain", value_cols), fn)
  if ("sensitive_layers_secure" %in% names(tbl)) value_cols <- c(value_cols, "sensitive_layers_secure")
  d <- sp_plot_country_rows(tbl, country, fn)
  sp_plot_check_unique(d, SP_CELL_KEYS, fn)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  sp_plot_stage_factor(d$supply_chain, fn)
  name <- as.character(d$Country[1])
  tech_levels <- sp_plot_tech_levels(d$tech, techs)
  if (length(tech_levels) == 0) stop(fn, "(): no technologies to draw.")
  g <- sp_plot_complete_grid(d, tech_levels, value_cols)
  text_size <- base_size / 4

  panel_title <- ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.95)))
  continuous_panel <- function(col, title) {
    g$label <- sp_plot_pct_label(g[[col]])
    sp_plot_tile_grid(
      g, col, "label",
      fill_title = "Percentile", text_size = text_size, base_size = base_size, stage_angle = stage_angle
    ) +
      ggplot2::labs(title = title) +
      panel_title
  }
  panels <- list(
    position = continuous_panel("competitive_position_percentile", "Competitive position (percentile across countries)"),
    priority = continuous_panel("policy_priority_percentile", "Revealed policy priority (percentile within country)"),
    need = continuous_panel("strategic_need_percentile", "Strategic need (percentile across countries)"),
    posture = sp_plot_posture_tiles(g, base_size, text_size, stage_angle) +
      ggplot2::labs(title = "Recommended posture (code: observed policy vs posture)") +
      panel_title
  )
  if (!combine) return(panels)

  # The three percentile panels share one scale, so the combined chart shows one colour bar and
  # the posture legend in a column on the right; per-panel legends would squeeze the grids until
  # the stage names collide.
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      sprintf("%s: competitive position, policy priority, strategic need and recommended posture", name),
      fontface = "bold", x = 0.01, hjust = 0, size = base_size + 3
    )
  n_na <- sum(is.na(sp_plot_posture_values(g$strategic_posture)))
  caption <- cowplot::ggdraw() +
    cowplot::draw_label(
      sp_plot_wrap(c(
        paste0(SP_PLOT_PRIORITY_CAVEAT, "."),
        paste0(
          "Posture: the recommended Yards & Fences posture (observed policy never enters it). Tile code, observed policy vs posture: ",
          SP_PLOT_POLICY_CODE_KEY, ". Diamond: a sensitive layer inside the cell is Secure (the cell keeps its own posture). ",
          "n/a: not scored or not classified (", n_na, " of ", nrow(g), " posture tiles)."
        )
      ), 210),
      x = 0.01, hjust = 0, size = base_size - 2.5, colour = "grey35", lineheight = 1.1
    )
  bare <- function(p) p + ggplot2::theme(legend.position = "none")
  grid <- cowplot::plot_grid(
    bare(panels$position), bare(panels$priority), bare(panels$need), bare(panels$posture),
    ncol = 2, align = "hv", axis = "tblr"
  )
  legends <- cowplot::plot_grid(
    sp_plot_legend_grob(panels$position), sp_plot_legend_grob(panels$posture),
    ncol = 1, rel_heights = c(0.7, 1)
  )
  body <- cowplot::plot_grid(grid, legends, nrow = 1, rel_widths = c(1, 0.22))
  cowplot::plot_grid(title, body, caption, ncol = 1, rel_heights = c(0.05, 1, 0.075)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# ---- D. Evidence heatmap --------------------------------------------------------------------------

# Band an evidence specificity share as the contract does for stage_evidence_quality:
# high >= bands["high"], medium >= bands["medium"], low > 0, none = 0; NA stays NA.
sp_evidence_band <- function(x, bands = c(high = 0.67, medium = 0.33)) {
  out <- ifelse(
    is.na(x), NA_character_,
    ifelse(x >= bands[["high"]], "high", ifelse(x >= bands[["medium"]], "medium", ifelse(x > 0, "low", "none")))
  )
  factor(out, levels = SP_PLOT_EVIDENCE_BANDS)
}

# D. Evidence specificity of one dimension (cp, sn or pp: evidence_specificity_<dimension>) by
# tech x stage, banded high / medium / low / none, the share printed as a percent. With
# country = NULL each tile is the mean over the countries that have a value (the band is then the
# band of that mean, and "none" means no country has stage-specific evidence). Evidence is
# reported next to the scores and never changes them; this chart says how much of a cell's score
# rests on inputs that actually identify the stage.
sp_plot_evidence_heatmap <- function(tbl, country = NULL, dimension = c("cp", "sn", "pp"),
                                     bands = c(high = 0.67, medium = 0.33), techs = NULL, base_size = 12) {
  fn <- "sp_plot_evidence_heatmap"
  dimension <- match.arg(dimension)
  col <- paste0("evidence_specificity_", dimension)
  sp_plot_require(tbl, c("Country", "tech", "supply_chain", col), fn)
  if (!is.numeric(bands) || !all(c("high", "medium") %in% names(bands)) || bands[["medium"]] > bands[["high"]]) {
    stop(fn, "(): bands must be c(high = , medium = ) with medium <= high.")
  }

  if (is.null(country)) {
    d <- as.data.frame(tbl, stringsAsFactors = FALSE)
    sp_plot_check_unique(d, SP_CELL_KEYS, fn)
    sp_plot_stage_factor(d$supply_chain, fn)
    d <- dplyr::summarise(
      dplyr::group_by(d, .data$tech, .data$supply_chain),
      share = sp_mean_available(.data[[col]]),
      n_countries = sum(!is.na(.data[[col]])),
      .groups = "drop"
    )
    where <- sprintf("mean over the countries with a value (%d countries in the table)", length(unique(tbl$Country)))
    heading <- "all countries"
  } else {
    d <- sp_plot_country_rows(tbl, country, fn)
    sp_plot_check_unique(d, SP_CELL_KEYS, fn)
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    sp_plot_stage_factor(d$supply_chain, fn)
    d$share <- d[[col]]
    heading <- as.character(d$Country[1])
    where <- heading
  }
  tech_levels <- sp_plot_tech_levels(d$tech, techs)
  if (length(tech_levels) == 0) stop(fn, "(): no technologies to draw.")
  g <- sp_plot_complete_grid(d, tech_levels, "share")
  g$band <- sp_evidence_band(g$share, bands)
  g$label <- ifelse(is.na(g$share), "n/a", sprintf("%.0f%%", 100 * g$share))

  # Ordinal bands on viridis stops, so high reads light/yellow and none dark, like the
  # percentile heatmaps.
  values <- stats::setNames(rev(scales::viridis_pal()(4)), SP_PLOT_EVIDENCE_BANDS)
  labels <- c(
    high = sprintf("High (>= %.0f%%)", 100 * bands[["high"]]),
    medium = sprintf("Medium (>= %.0f%%)", 100 * bands[["medium"]]),
    low = "Low (> 0%)",
    none = "None (0%)"
  )
  n_na <- sum(is.na(g$share))
  sp_plot_tile_grid(
    g, "band", "label",
    fill_title = "Evidence specificity", text_size = base_size / 3.6, base_size = base_size,
    fill_values = values, fill_labels = labels, na_label = "n/a  No inputs"
  ) +
    ggplot2::labs(
      title = sprintf("Stage evidence behind %s: %s", SP_PLOT_EVIDENCE_DIMENSIONS[[dimension]], heading),
      subtitle = paste0(
        "Share of the score's weight on observed inputs whose source identifies the stage (exact or split); ",
        where, "."
      ),
      caption = sp_plot_wrap(paste0(
        "Evidence is reported next to the score and never changes it. ",
        "n/a: no inputs for the cell (", n_na, " of ", nrow(g), " cells). ",
        "Shared, proxy and technology-wide inputs and filled values do not count as stage evidence."
      ), 170)
    )
}

# ---- E. Posture map and posture mix ---------------------------------------------------------------

# E. Recommended posture by tech (rows) x stage (columns), one facet per country in the order
# given: the country-divergence view (the same tech x stage cell can be Secure for one country and
# Deploy for another, because exposure, coercive supply and capability are the country's own).
# Each tile prints the policy-alignment code; a diamond marks a cell with a Secure sensitive layer
# (when the table has sensitive_layers_secure); n/a grey where the table gives no posture. Every
# requested country must be in the table; `techs` limits and orders the rows.
sp_plot_posture_map <- function(tbl, countries, techs = NULL, ncol = NULL, base_size = 12) {
  fn <- "sp_plot_posture_map"
  value_cols <- c("strategic_posture", "policy_alignment")
  sp_plot_require(tbl, c("Country", "tech", "supply_chain", value_cols), fn)
  if ("sensitive_layers_secure" %in% names(tbl)) value_cols <- c(value_cols, "sensitive_layers_secure")
  present <- sp_plot_country_set_all(tbl, countries, fn)
  d <- as.data.frame(tbl[as.character(tbl$Country) %in% present, , drop = FALSE], stringsAsFactors = FALSE)
  sp_plot_check_unique(d, SP_CELL_KEYS, fn)
  sp_plot_stage_factor(d$supply_chain, fn)
  tech_levels <- sp_plot_tech_levels(d$tech, techs)
  if (length(tech_levels) == 0) stop(fn, "(): no technologies to draw.")

  g <- sp_plot_complete_grid(d, tech_levels, value_cols, countries = present)
  n_facets <- length(present)
  if (is.null(ncol)) ncol <- sp_plot_facet_ncol(n_facets)
  text_size <- if (ncol > 3) base_size / 4.8 else base_size / 4.1

  posture <- sp_plot_posture_values(g$strategic_posture)
  by_country <- vapply(present, function(cn) {
    sp_plot_count_levels(
      ifelse(is.na(posture[g$Country == cn]), "n/a", posture[g$Country == cn]),
      c(SP_PLOT_POSTURE_LEVELS, "n/a")
    )
  }, character(1))
  divergence_note <- ""
  if (n_facets > 1) {
    cell <- paste(g$tech, g$supply_chain)
    n_classified <- tapply(!is.na(posture), cell, sum)
    n_distinct <- tapply(posture, cell, function(p) length(unique(p[!is.na(p)])))
    compared <- n_classified >= 2
    divergence_note <- sprintf(
      " Postures differ across the countries shown in %d of the %d cells classified for at least two of them.",
      sum(n_distinct[compared] > 1), sum(compared)
    )
  }

  sp_plot_posture_tiles(g, base_size, text_size, stage_angle = if (n_facets > 1) 40 else 0) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Country), ncol = ncol) +
    ggplot2::labs(
      title = "Recommended posture by technology and supply-chain stage",
      subtitle = sp_plot_wrap(paste0(
        "Secure, Compete or Deploy for each country's technology x stage cell (observed policy never enters it). ",
        "Code, observed policy vs posture: ", SP_PLOT_POLICY_CODE_KEY, ". n/a: not classified."
      ), 165),
      caption = sp_plot_wrap(c(
        paste0("Cells by posture: ", paste0(present, ": ", by_country, collapse = "; "), "."),
        paste0(
          "Diamond: a sensitive layer inside the cell (control software, a fuel, a critical material) is Secure; ",
          "the cell keeps its own posture.", divergence_note
        )
      ), 190)
    ) +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(0.8, "lines"))
}

# E2. Share of each country's classified cells (those with a recommended posture) that are
# Secure, Compete and Deploy: one horizontal stacked bar per country, in the order given (first on
# top), Secure anchored at zero. Shares of 5% or more are printed in the segment; the bar label
# gives the number of classified cells and the caption the cells not classified.
sp_plot_posture_mix <- function(tbl, countries, base_size = 12) {
  fn <- "sp_plot_posture_mix"
  sp_plot_require(tbl, c("Country", "strategic_posture"), fn)
  present <- sp_plot_country_set_all(tbl, countries, fn)
  d <- as.data.frame(tbl[as.character(tbl$Country) %in% present, , drop = FALSE], stringsAsFactors = FALSE)
  if (all(SP_CELL_KEYS %in% names(d))) sp_plot_check_unique(d, SP_CELL_KEYS, fn)
  posture <- sp_plot_posture_values(d$strategic_posture)
  country <- factor(as.character(d$Country), levels = present)

  counts <- as.data.frame(
    table(Country = country[!is.na(posture)], posture = factor(posture[!is.na(posture)], levels = SP_PLOT_POSTURE_LEVELS)),
    stringsAsFactors = FALSE
  )
  totals <- tapply(counts$Freq, factor(counts$Country, levels = present), sum)
  unclassified <- tapply(is.na(posture), country, sum)
  counts$total <- as.integer(totals[counts$Country])
  counts$share <- ifelse(counts$total > 0, counts$Freq / counts$total, NA_real_)
  counts$posture <- factor(counts$posture, levels = SP_PLOT_POSTURE_LEVELS)
  counts <- counts[order(match(counts$Country, present), counts$posture), , drop = FALSE]
  counts$xmid <- stats::ave(dplyr::coalesce(counts$share, 0), counts$Country, FUN = cumsum) - dplyr::coalesce(counts$share, 0) / 2
  bar_labels <- stats::setNames(sprintf("%s\n%d classified cells", present, as.integer(totals[present])), present)
  counts$bar <- factor(unname(bar_labels[counts$Country]), levels = rev(unname(bar_labels)))
  bars <- counts[counts$Freq > 0, , drop = FALSE]
  text <- bars[bars$share >= 0.05, , drop = FALSE]
  text$label <- sprintf("%.0f%%", 100 * text$share)
  text$.colour <- sp_plot_text_on(unname(SP_PLOT_POSTURE_COLOURS[as.character(text$posture)]))

  ggplot2::ggplot(bars, ggplot2::aes(y = .data$bar)) +
    ggplot2::geom_col(
      ggplot2::aes(x = .data$share, fill = .data$posture),
      position = ggplot2::position_stack(reverse = TRUE), width = 0.62, colour = "white", linewidth = 0.8,
      show.legend = TRUE
    ) +
    ggplot2::geom_text(
      data = text, ggplot2::aes(x = .data$xmid, label = .data$label, colour = .data$.colour),
      size = base_size / 3.4, show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_manual(
      name = NULL, values = SP_PLOT_POSTURE_COLOURS, labels = SP_PLOT_POSTURE_LABELS,
      limits = SP_PLOT_POSTURE_LEVELS, drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.0001), breaks = seq(0, 1, 0.25), labels = function(b) sprintf("%.0f%%", 100 * b),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "Recommended posture mix by country",
      subtitle = "Share of each country's classified technology x stage cells recommended Secure, Compete or Deploy.",
      x = "Share of classified cells", y = NULL,
      caption = sp_plot_wrap(paste0(
        "Classified: cells with a recommended posture. Not classified: ",
        paste0(present, " ", as.integer(unclassified[present]), collapse = ", "), ". ",
        "Shares below 5% are not printed."
      ), 170)
    ) +
    sp_plot_theme(base_size) +
    ggplot2::theme(
      legend.position = "top", legend.justification = "left",
      # room for the "100%" tick label at the panel's right edge
      plot.margin = ggplot2::margin(8, 22, 8, 8),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      axis.text.y = ggplot2::element_text(colour = "grey20", hjust = 1, lineheight = 1)
    )
}

# ---- Synthetic example ---------------------------------------------------------------------------

# A synthetic strategic_position_tbl with the section 6-7 schema, for tests and chart previews only
# (not data). Built the way the contract builds the real one, so its internal logic holds:
# position and need are percentiles across countries in the cell, priority is within the country;
# cp_within_country_pct is the within-country percentile of the position percentile and
# competitive_policy_alignment is read from it and priority at 0.5; priority_gap re-ranks position
# and priority over the cells that have both. The recommended posture follows the engine's
# structure: Deployment cells are always Deploy (no physical supply test, no capability case),
# Secure needs high need, Compete needs a position or a catch-up case, and Deploy is the largest
# share; variant, strategy, confidence and delivery mode agree with the posture; policy_alignment
# compares the posture with priority as sp_policy_alignment() does (matched at 0.5, over-prioritized
# Deploy at 0.67, mixed for a fenced Compete cell without a build). Missing patterns mimic the
# sources: no position at Geothermal / Green Hydrogen manufacturing, no NIPO at Deployment (priority
# from PAMS only, and missing for some countries), scattered missing need, a few cells not
# classified. The global RNG state is restored on exit.
sp_plot_example_tbl <- function(countries = c("United States", "China", "Japan", "South Korea", "India", "Germany"),
                                techs = c(
                                  "Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal",
                                  "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind"
                                ),
                                seed = 42) {
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  iso <- c(
    "United States" = "USA", China = "CHN", Japan = "JPN", "South Korea" = "KOR", India = "IND",
    Germany = "DEU", "United Kingdom" = "GBR", Australia = "AUS"
  )
  d <- expand.grid(Country = countries, tech = techs, supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  d$iso3 <- ifelse(d$Country %in% names(iso), iso[d$Country], toupper(substr(d$Country, 1, 3)))
  d$legacy_stage <- supply_chain_stage_rollup(d$supply_chain)
  n <- nrow(d)
  manufacturing <- d$supply_chain %in% c("Components", "Assembly")

  d$competitive_position_score <- round(stats::runif(n), 3)
  d$competitive_position_score[d$tech %in% c("Geothermal", "Green Hydrogen") & manufacturing] <- NA
  d$competitive_position_score[stats::runif(n) < 0.05] <- NA
  sn_score <- stats::runif(n)
  sn_score[stats::runif(n) < 0.08] <- NA
  pp_score <- stats::runif(n)
  pp_score[d$supply_chain == "Deployment" & d$Country %in% countries[seq(2, length(countries), 2)]] <- NA
  pp_score[stats::runif(n) < 0.04] <- NA

  cell <- paste(d$tech, d$supply_chain)
  d$competitive_position_percentile <- stats::ave(d$competitive_position_score, cell, FUN = sp_midrank_pct)
  d$strategic_need_percentile <- stats::ave(sn_score, cell, FUN = sp_midrank_pct)
  d$policy_priority_percentile <- stats::ave(pp_score, d$Country, FUN = sp_midrank_pct)
  within_country <- function(v) stats::ave(ifelse(is.na(pp_score), NA, v), d$Country, FUN = sp_midrank_pct)
  d$policy_support_priority <- within_country(stats::runif(n))
  d$policy_protection_priority <- within_country(stats::runif(n))
  d$policy_momentum <- within_country(stats::runif(n))
  d$policy_priority_sources <- ifelse(
    is.na(pp_score), NA_character_,
    ifelse(d$supply_chain == "Deployment" | d$tech %in% c("Geothermal", "Green Hydrogen"), "pams", "nipo+pams")
  )

  evidence <- function(p_zero, p_na) {
    v <- round(stats::runif(n), 2)
    v[stats::runif(n) < p_zero] <- 0
    v[stats::runif(n) < p_na] <- NA
    v
  }
  d$evidence_specificity_cp <- evidence(0.15, 0.05)
  d$evidence_specificity_cp[is.na(d$competitive_position_score)] <- NA
  d$evidence_specificity_sn <- evidence(0.10, 0.05)
  d$evidence_specificity_sn[d$supply_chain == "Deployment"] <- 0
  d$evidence_specificity_pp <- evidence(0.10, 0.05)
  d$evidence_specificity_pp[is.na(pp_score)] <- NA
  d$stage_evidence_quality <- as.character(sp_evidence_band(d$evidence_specificity_cp))
  d$stage_evidence_quality[is.na(d$stage_evidence_quality)] <- "none"

  # Competitive-policy alignment on the within-country position percentile (alignment.R default).
  cp <- d$competitive_position_percentile
  pp <- d$policy_priority_percentile
  sn <- d$strategic_need_percentile
  d$cp_within_country_pct <- stats::ave(cp, d$Country, FUN = sp_midrank_pct)
  hx <- d$cp_within_country_pct >= 0.5
  hy <- pp >= 0.5
  d$competitive_policy_alignment <- ifelse(
    is.na(hx) | is.na(hy), "insufficient_data",
    ifelse(hx & hy, "aligned_strength", ifelse(hx, "latent_advantage", ifelse(hy, "policy_led_bet", "low_priority")))
  )
  d$competitive_policy_basis <- "within_country"
  both <- !is.na(cp) & !is.na(pp)
  d$priority_gap <- stats::ave(ifelse(both, cp, NA), d$Country, FUN = sp_midrank_pct) -
    stats::ave(ifelse(both, pp, NA), d$Country, FUN = sp_midrank_pct)
  near <- function(v) !is.na(v) & abs(v - 0.5) < 0.1
  d$alignment_boundary <- near(d$cp_within_country_pct) | near(pp) | near(sn)

  # Recommended posture. Draws are made for every row so the stream does not depend on the branch.
  producing <- d$supply_chain != "Deployment"
  u_secure <- stats::runif(n)
  u_compete <- stats::runif(n)
  u_skip <- stats::runif(n)
  secure <- producing & !is.na(sn) & sn >= 0.75 & u_secure < 0.45
  compete <- producing & !secure & !is.na(cp) &
    ((cp >= 0.6 & u_compete < 0.6) | (cp >= 0.35 & cp < 0.6 & u_compete < 0.15))
  posture <- ifelse(secure, "secure", ifelse(compete, "compete", "deploy"))
  posture[(is.na(cp) & is.na(sn)) | u_skip < 0.03] <- NA_character_
  d$strategic_posture <- posture
  is_p <- function(p) !is.na(posture) & posture == p

  pick <- function(values, prob) sample(values, n, replace = TRUE, prob = prob)
  secure_variant <- pick(
    c("secure_input", "secure_control", "secure_integrated", "secure_whole", "secure_unclear", "secure_both"),
    c(0.55, 0.1, 0.1, 0.05, 0.1, 0.1)
  )
  strategy_draw <- pick(c("adjacent_capability", "catch_up", "leap_ahead"), c(0.5, 0.35, 0.15))
  costly <- stats::runif(n) < 0.12
  u_deploy <- stats::runif(n)
  strategy <- ifelse(is_p("compete"), ifelse(!is.na(cp) & cp >= 0.75, "preserve", strategy_draw), NA_character_)
  d$posture_variant <- dplyr::case_when(
    is.na(posture) ~ NA_character_,
    posture == "secure" ~ secure_variant,
    posture == "compete" & strategy == "preserve" & costly ~ "compete_costly",
    posture == "compete" ~ paste0("compete_", strategy),
    !is.na(sn) & sn >= 0.6 & u_deploy < 0.4 ~ "deploy_diversify",
    producing & !is.na(cp) & cp >= 0.3 & cp < 0.6 & u_deploy > 0.85 ~ "deploy_floor",
    TRUE ~ "deploy"
  )
  d$compete_strategy <- strategy
  conf <- list(
    secure = pick(c("low", "moderate", "high"), c(0.2, 0.5, 0.3)),
    compete = pick(c("low", "moderate", "high"), c(0.3, 0.5, 0.2)),
    deploy = pick(c("low", "moderate", "high"), c(0.2, 0.4, 0.4))
  )
  d$posture_confidence <- ifelse(is_p("secure"), conf$secure, ifelse(is_p("compete"), conf$compete, ifelse(is_p("deploy"), conf$deploy, NA_character_)))
  secure_mode <- pick(c("allied", "mixed", "diversified_global"), c(0.5, 0.3, 0.2))
  compete_mode <- pick(c("domestic", "mixed"), c(0.6, 0.4))
  diversify_mode <- pick(c("allied", "diversified_global"), c(0.5, 0.5))
  d$delivery_mode <- dplyr::case_when(
    is.na(posture) ~ NA_character_,
    posture == "secure" & !is.na(cp) & cp >= 0.7 ~ "domestic",
    posture == "secure" ~ secure_mode,
    posture == "compete" & strategy %in% c("preserve", "leap_ahead") ~ "domestic",
    posture == "compete" ~ compete_mode,
    d$posture_variant == "deploy_diversify" ~ diversify_mode,
    TRUE ~ "diversified_global"
  )

  # Recommended posture vs observed policy (sp_policy_alignment() with the config defaults).
  fence_no_build <- is_p("compete") & !is.na(d$policy_protection_priority) & !is.na(d$policy_support_priority) &
    d$policy_protection_priority >= 0.67 & d$policy_support_priority <= 0.33
  d$policy_alignment <- dplyr::case_when(
    is.na(posture) ~ NA_character_,
    is.na(pp) ~ "insufficient_data",
    posture %in% c("secure", "compete") & fence_no_build ~ "mixed",
    posture %in% c("secure", "compete") & pp >= 0.5 ~ "aligned",
    posture %in% c("secure", "compete") ~ "under_prioritized",
    d$supply_chain == "Deployment" ~ "aligned",
    pp >= 0.67 ~ "over_prioritized",
    TRUE ~ "aligned"
  )

  # Sensitive layers whose own posture is Secure (curated Yards & Fences components); China is the
  # coercive supplier in these chains, so it has none.
  layer_ids <- c(
    "Electric Grid Components" = "grid-inverter-firmware;grid-scada",
    "Batteries Components" = "bess-control",
    "Nuclear Refining" = "nuc-haleu",
    "Wind Components" = "ree-magnets"
  )
  layers <- unname(layer_ids[paste(d$tech, d$supply_chain)])
  layers[d$iso3 == "CHN" | is.na(posture)] <- NA_character_
  layers[paste(d$tech, d$supply_chain) == "Nuclear Refining" & !d$iso3 %in% c("USA", "JPN", "KOR", "DEU", "GBR")] <- NA_character_
  d$sensitive_layers_secure <- layers

  d <- d[order(d$Country, d$tech, match(d$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
  rownames(d) <- NULL
  d
}
