# Figure and table: the HS6 product crosswalk after the October 2026 basket review
# (paper Section 3, fig:hs6_crosswalk, tab:hs6_counts).
#
# Reads the version-controlled master (data/reference/energy_hs6_master.csv) and normalizes it
# with the pipeline's own pure helpers (R/utils/hs6_crosswalk.R), so the counts are the counts
# every trade theme sees. The review kept every row in the master: rows that fail the
# dominant-use test are flagged excluded = TRUE (reason in `note`) and hs6_normalize_master()
# drops them; the rows it added (the copper and aluminium input basket of Electric Grid
# Upstream, which lost its only code, electrical energy HS 2716.00) carry a note that opens with
# "added (basket review 2026-10". The Upstream baskets of Electric Vehicles, Solar, Wind, Nuclear
# and Semiconductors (user decision 2026-10-11) added rows the same way, plus one HS92 bridge row
# (8112.91) whose note names the decision, and MOVED six rows from Midstream to Upstream (a code
# sits in one stage of a technology): their note opens with "moved from <stage> (<old sub-sector>;".
# The pre-review basket is therefore the master without the added rows, with the moved rows put
# back in their former stage and sub-sector, and with the exclusion flag ignored. Those five
# Upstream cells had no basket before the review.
#
# Panel (a): for each technology x stage basket, the distinct codes in the basket, split into
#   codes added by the review (moved-in codes count as added to their new basket), current codes
#   of that basket only, current codes shared with at least one other basket (the many-to-many
#   part of the mapping) and legacy HS bridge codes, with the codes the review excluded, and the
#   codes it moved to Upstream, drawn to the left of zero.
# Panel (b): the share of the pre-review basket's 2024 trade value carried by the codes the
#   review excluded (moved codes are not counted as removed; cells new in the review have no
#   pre-review basket). Trade is the reconciled all-reporter Comtrade dyad matrix of the bilateral
#   and network layers (partnership_strength_build_trade_cells(): importer's declaration first,
#   else the exporter's mirror x CIF/FOB factor), summed over dyads; a code filed under several
#   baskets counts in each.
#
# Also reports which in-basket codes each trade source can see (Atlas HS92 product list, the
# Comtrade energy-trade file of the country-level trade themes, the all-reporter dyad pull).
# Writes paper/figures/fig_hs6_crosswalk.pdf (+ png), paper/results/hs6_crosswalk.json and
# paper/tables/misc_hs6_counts.tex (Table tab:hs6_counts).
#
# Run from the repository root:
#   Rscript scripts/paper/fig_hs6_crosswalk.R

local({
  find_theme <- function() {
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    while (dirname(d) != d) {
      f <- file.path(d, "scripts", "paper", "_paper_theme.R")
      if (file.exists(f)) return(f)
      d <- dirname(d)
    }
    stop("Run from inside the repository.")
  }
  source(find_theme(), local = FALSE)
})

suppressPackageStartupMessages({
  library(dplyr)
  library(cowplot)
})

root <- paper_repo_root()
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
raw_dir <- file.path(root, config$raw_data_dir)
partnership_cfg_path <- file.path(root, "config", "partnership.yml")
partnership_cfg <- if (file.exists(partnership_cfg_path)) yaml::read_yaml(partnership_cfg_path) else list()
cif_fob <- if (is.null(partnership_cfg$trade$cif_fob_factor)) {
  PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR
} else {
  as.numeric(partnership_cfg$trade$cif_fob_factor)
}

# ---------------------------------------------------------------------------
# The master before and after the review
# ---------------------------------------------------------------------------
master_raw <- utils::read.csv(
  file.path(root, "data", "reference", "energy_hs6_master.csv"),
  colClasses = c(hs6 = "character"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
)
stopifnot("excluded" %in% names(master_raw))
excluded_row <- as.logical(master_raw$excluded)
excluded_row[is.na(excluded_row)] <- FALSE
note <- dplyr::coalesce(master_raw$note, "")
# Rows the review moved between stages of a technology: "moved from <stage> (<old sub-sector>; ...".
# (The older "moved from Midstream: ..." notes of Batteries Upstream predate the review.)
moved_match <- regmatches(note, regexec("^moved from (Upstream|Midstream|Downstream) \\(([^;)]+)[;)]", note))
moved_row <- lengths(moved_match) == 3
moved_from_stage <- vapply(moved_match, function(m) if (length(m) == 3) m[2] else NA_character_, character(1))
moved_from_sub <- vapply(moved_match, function(m) if (length(m) == 3) trimws(m[3]) else NA_character_, character(1))
# Added rows: "added (basket review 2026-10..." and the HS92 bridge row of the 2026-10-11 decision.
added_row <- startsWith(note, "added (basket review 2026-10") |
  (grepl("user decision 2026-10-11", note, fixed = TRUE) & !moved_row)
stopifnot(!any(added_row & excluded_row), !any(moved_row & excluded_row), !any(added_row & moved_row),
          all(moved_from_stage[moved_row] != master_raw$supply_chain[moved_row]))
# "kept with caveat" opens the note of most such rows and follows an earlier note on the others.
caveat_row <- grepl("kept with caveat", note, fixed = TRUE)

master <- hs6_normalize_master(master_raw)
master_pre_raw <- master_raw
master_pre_raw$supply_chain[moved_row] <- moved_from_stage[moved_row]
master_pre_raw$sub_sector[moved_row] <- moved_from_sub[moved_row]
master_pre_raw <- master_pre_raw[!added_row, ]
master_pre_raw$excluded <- FALSE
master_pre <- hs6_normalize_master(master_pre_raw)

key3 <- function(d) paste(d$tech, d$supply_chain, d$hs6, sep = "|")
pairs <- unique(master[, c("hs6", "tech", "supply_chain", "legacy_hs")])
pairs_pre <- unique(master_pre[, c("hs6", "tech", "supply_chain", "legacy_hs")])
stopifnot(!anyDuplicated(key3(pairs)), !anyDuplicated(key3(pairs_pre)))
moved_out_keys <- paste(master_raw$tech[moved_row], moved_from_stage[moved_row],
                        hs6_clean_code(master_raw$hs6[moved_row]), sep = "|")
gone_pairs <- pairs_pre[!key3(pairs_pre) %in% key3(pairs), ]
moved_pairs <- gone_pairs[key3(gone_pairs) %in% moved_out_keys, ]
excl_pairs <- gone_pairs[!key3(gone_pairs) %in% moved_out_keys, ]
stopifnot(setequal(key3(moved_pairs), moved_out_keys))
added_pairs <- pairs[!key3(pairs) %in% key3(pairs_pre), ]
added_master <- master_raw[added_row | moved_row, ]
added_master$hs6 <- hs6_clean_code(added_master$hs6)
stopifnot(setequal(key3(added_pairs), key3(added_master)))
kept_pre_pairs <- pairs_pre[key3(pairs_pre) %in% key3(pairs), ]

pairs$cell <- paste(pairs$tech, pairs$supply_chain, sep = " | ")
cells_per_code <- tapply(pairs$cell, pairs$hs6, function(x) length(unique(x)))
pairs$n_cells <- as.integer(cells_per_code[pairs$hs6])
techs_per_code <- tapply(pairs$tech, pairs$hs6, function(x) length(unique(x)))
stages_per_code <- tapply(pairs$supply_chain, pairs$hs6, function(x) length(unique(x)))
stopifnot(all(stages_per_code == 1)) # no code belongs to both stages

class_levels <- c(
  "This basket only",
  "Shared with other baskets",
  "Legacy HS bridge code",
  "Added in the review",
  "Excluded in the review",
  "Moved to Upstream in the review"
)
left_classes <- class_levels[5:6] # drawn to the left of zero
pairs$added <- key3(pairs) %in% key3(added_pairs)
pairs$class <- ifelse(
  pairs$added, class_levels[4],
  ifelse(pairs$legacy_hs, class_levels[3],
         ifelse(pairs$n_cells > 1, class_levels[2], class_levels[1]))
)
excl_pairs$class <- class_levels[5]
moved_pairs$class <- class_levels[6]
stopifnot(all(moved_from_stage[moved_row] == "Midstream"), all(master_raw$supply_chain[moved_row] == "Upstream"))

all_cells <- unique(rbind(pairs_pre[, c("tech", "supply_chain")], pairs[, c("tech", "supply_chain")]))
count_class <- function(d) {
  if (nrow(d) == 0) {
    return(data.frame(tech = character(), supply_chain = character(), class = character(), hs6 = integer()))
  }
  stats::aggregate(hs6 ~ tech + supply_chain + class, data = d, FUN = length)
}
in_counts <- count_class(pairs)
ex_counts <- count_class(excl_pairs)
mv_counts <- count_class(moved_pairs)
counts <- rbind(in_counts, ex_counts, mv_counts)
names(counts)[names(counts) == "hs6"] <- "n"
counts$x <- ifelse(counts$class %in% left_classes, -counts$n, counts$n)

count_cell <- function(d, nm) {
  if (nrow(d) == 0) {
    return(stats::setNames(data.frame(character(), character(), integer()), c("tech", "supply_chain", nm)))
  }
  out <- stats::aggregate(hs6 ~ tech + supply_chain, data = d, FUN = length)
  names(out)[3] <- nm
  out
}
totals <- all_cells
totals <- merge(totals, count_cell(pairs, "n_in"), by = c("tech", "supply_chain"), all.x = TRUE)
totals <- merge(totals, count_cell(excl_pairs, "n_ex"), by = c("tech", "supply_chain"), all.x = TRUE)
totals <- merge(totals, count_cell(added_pairs, "n_add"), by = c("tech", "supply_chain"), all.x = TRUE)
totals <- merge(totals, count_cell(moved_pairs, "n_mv"), by = c("tech", "supply_chain"), all.x = TRUE)
totals$n_in[is.na(totals$n_in)] <- 0L
totals$n_ex[is.na(totals$n_ex)] <- 0L
totals$n_add[is.na(totals$n_add)] <- 0L
totals$n_mv[is.na(totals$n_mv)] <- 0L
totals$n_left <- totals$n_ex + totals$n_mv

# ---------------------------------------------------------------------------
# 2024 basket trade before and after the review (reconciled dyad matrix)
# ---------------------------------------------------------------------------
dyads_path <- file.path(raw_dir, "comtrade_energy_dyads_all.csv")
dyads <- data.table::fread(
  dyads_path,
  select = c("reporter_iso", "partner_iso", "ref_year", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = "cmd_code"),
  showProgress = FALSE
)
dyad_years <- sort(unique(dyads$ref_year))
trade_year <- max(dyad_years, na.rm = TRUE)
dyad_codes <- unique(hs6_clean_code(dyads$cmd_code))
dyad_reporters <- lapply(stats::setNames(dyad_years, dyad_years), function(y) {
  d <- dyads[dyads$ref_year == y, ]
  list(
    reporters_any_flow = length(unique(d$reporter_iso)),
    reporters_imports = length(unique(d$reporter_iso[d$flow_direction == "import"])),
    reporters_exports = length(unique(d$reporter_iso[d$flow_direction == "export"]))
  )
})
dyads <- dyads[dyads$ref_year == trade_year, ]
data.table::setnames(dyads, "ref_year", "period")
dyads <- as.data.frame(dyads)
invisible(gc())

basket_trade <- function(p) {
  m <- data.frame(tech = p$tech, supply_chain = p$supply_chain, sub_sector = "All", hs6 = p$hs6,
                  stringsAsFactors = FALSE)
  cells <- partnership_strength_build_trade_cells(
    dyads, m, years = trade_year, cif_fob_factor = cif_fob
  )
  out <- cells %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(
      value = sum(value_last, na.rm = TRUE),
      value_mirror = sum(value_last[source_last %in% "mirror"], na.rm = TRUE),
      .groups = "drop"
    )
  rm(cells)
  invisible(gc())
  out
}
trade_pre <- basket_trade(pairs_pre)
trade_kept <- basket_trade(kept_pre_pairs)
trade_post <- basket_trade(pairs)
# The pre-review basket less its excluded codes (kept codes plus the codes later moved to another
# stage), reconciled as one cell like the pre-review basket, so that moved codes do not count as
# removed; without moved codes it is the kept part.
retained_pre_pairs <- pairs_pre[!key3(pairs_pre) %in% key3(excl_pairs), ]
trade_retained <- basket_trade(retained_pre_pairs)
trade_moved <- if (nrow(moved_pairs) > 0) basket_trade(moved_pairs) else
  data.frame(tech = character(), supply_chain = character(), value = numeric())
trade <- all_cells %>%
  dplyr::left_join(dplyr::select(trade_pre, tech, supply_chain, value_pre = value),
                   by = c("tech", "supply_chain")) %>%
  dplyr::left_join(dplyr::select(trade_kept, tech, supply_chain, value_kept = value),
                   by = c("tech", "supply_chain")) %>%
  dplyr::left_join(dplyr::select(trade_retained, tech, supply_chain, value_retained = value),
                   by = c("tech", "supply_chain")) %>%
  dplyr::left_join(dplyr::select(trade_moved, tech, supply_chain, value_moved = value),
                   by = c("tech", "supply_chain")) %>%
  dplyr::left_join(dplyr::rename(trade_post, value_post = value, mirror_post = value_mirror),
                   by = c("tech", "supply_chain")) %>%
  dplyr::mutate(
    value_kept = dplyr::coalesce(value_kept, 0),
    value_retained = dplyr::coalesce(value_retained, 0),
    value_moved = dplyr::coalesce(value_moved, 0),
    value_post = dplyr::coalesce(value_post, 0),
    mirror_post = dplyr::coalesce(mirror_post, 0),
    # A basket the review created (the Upstream baskets of 2026-10-11) has no pre-review value.
    new_basket = is.na(value_pre),
    # Share of the pre-review basket's value carried by the excluded codes; the added codes do
    # not offset it (they are reported separately as a share of the post-review basket), and
    # codes moved to another stage are not removals (moved_share reports them).
    removed_share = 1 - value_retained / value_pre,
    moved_share = value_moved / value_pre,
    added_share_post = ifelse(value_post > 0, 1 - value_kept / value_post, NA_real_)
  )
new_cells <- paste(trade$tech, trade$supply_chain)[trade$new_basket]
stopifnot(setequal(new_cells, setdiff(paste(pairs$tech, pairs$supply_chain), paste(pairs_pre$tech, pairs_pre$supply_chain))),
          all(trade$value_pre[!trade$new_basket] > 0), all(trade$removed_share[!trade$new_basket] >= -1e-9),
          all(trade$value_post > 0))
# Baskets without added codes: post-review value equals the kept part of the pre-review basket.
no_add <- !paste(trade$tech, trade$supply_chain) %in% paste(added_pairs$tech, added_pairs$supply_chain)
stopifnot(all(abs(trade$value_post[no_add] - trade$value_kept[no_add]) <= 1e-6 * trade$value_post[no_add]))

# Code-level reconciled 2024 trade of the excluded and added codes, the same rule applied to one
# code at a time (importer's declarations of the code where the importer reports it, else
# exporters' declarations x CIF/FOB), to name the products that carried the removed value.
code_trade <- local({
  d <- dyads[dyads$reporter_iso != dyads$partner_iso & !is.na(dyads$primary_value), ]
  d$cmd_code <- hs6_clean_code(d$cmd_code)
  d <- d[d$cmd_code %in% unique(c(excl_pairs$hs6, added_pairs$hs6)), ]
  imp <- d[d$flow_direction == "import", ]
  imp_val <- stats::aggregate(primary_value ~ cmd_code, data = imp, FUN = sum)
  reporting <- unique(paste(imp$cmd_code, imp$reporter_iso))
  exp <- d[d$flow_direction == "export", ]
  exp <- exp[!paste(exp$cmd_code, exp$partner_iso) %in% reporting, ]
  exp_val <- if (nrow(exp) > 0) {
    stats::aggregate(primary_value ~ cmd_code, data = exp, FUN = sum)
  } else {
    data.frame(cmd_code = character(), primary_value = numeric())
  }
  out <- merge(imp_val, exp_val, by = "cmd_code", all = TRUE, suffixes = c("_imp", "_mirror"))
  out[is.na(out)] <- 0
  out$value <- out$primary_value_imp + cif_fob * out$primary_value_mirror
  out[, c("cmd_code", "value")]
})
rm(dyads)
invisible(gc())

clean_reason <- function(r) {
  r <- sub("^excluded \\(basket review 2026-10\\): ", "", r)
  r <- sub("^added \\(basket review 2026-10[^)]*\\): ", "", r)
  r <- sub("^moved from [A-Za-z]+ \\([^)]*\\): ", "", r)
  r <- sub("\\. Earlier note:.*$", "", r)
  sub(" kept with caveat:.*$", "", r)
}
reason_of <- function(code, tech, stage, flag) {
  r <- master_raw[hs6_clean_code(master_raw$hs6) == code & master_raw$tech == tech &
                    master_raw$supply_chain == stage & flag, "note"]
  clean_reason(r[1])
}
excl_detail <- excl_pairs %>%
  dplyr::left_join(code_trade, by = c("hs6" = "cmd_code")) %>%
  dplyr::mutate(value = dplyr::coalesce(value, 0)) %>%
  dplyr::arrange(tech, supply_chain, dplyr::desc(value))
excl_detail$reason <- unname(mapply(reason_of, excl_detail$hs6, excl_detail$tech,
                                    excl_detail$supply_chain, MoreArgs = list(flag = excluded_row)))
added_detail <- added_pairs %>%
  dplyr::left_join(code_trade, by = c("hs6" = "cmd_code")) %>%
  dplyr::mutate(value = dplyr::coalesce(value, 0)) %>%
  dplyr::left_join(unique(master[, c("tech", "supply_chain", "hs6", "sub_sector")]),
                   by = c("tech", "supply_chain", "hs6")) %>%
  dplyr::arrange(dplyr::desc(value))
added_detail$reason <- unname(mapply(reason_of, added_detail$hs6, added_detail$tech,
                                     added_detail$supply_chain, MoreArgs = list(flag = added_row | moved_row)))
added_detail$moved <- paste(added_detail$tech, added_detail$supply_chain, added_detail$hs6, sep = "|") %in%
  key3(master_raw[moved_row, c("tech", "supply_chain", "hs6")] %>% dplyr::mutate(hs6 = hs6_clean_code(hs6)))
added_detail <- added_detail %>%
  dplyr::left_join(dplyr::select(trade, tech, supply_chain, cell_value_post = value_post),
                   by = c("tech", "supply_chain"))
moved_detail <- moved_pairs %>%
  dplyr::left_join(code_trade, by = c("hs6" = "cmd_code")) %>%
  dplyr::mutate(value = dplyr::coalesce(value, 0)) %>%
  dplyr::arrange(tech, supply_chain, dplyr::desc(value))

# ---------------------------------------------------------------------------
# Figure
# ---------------------------------------------------------------------------
tech_label <- function(t) ifelse(t == "Semiconductors", "Semiconductors (not scored)", t)
# Alphabetical from the top in both stages (the order of Table tab:hs6_counts); the unscored
# semiconductor basket last.
scored_labels <- sort(setdiff(unique(totals$tech), "Semiconductors"))
y_levels <- rev(c(scored_labels, tech_label("Semiconductors")))

stage_levels <- c("Upstream", "Midstream")
prep <- function(d) {
  d$label <- factor(tech_label(d$tech), levels = y_levels)
  d$supply_chain <- factor(d$supply_chain, levels = stage_levels)
  d
}
counts <- prep(counts)
counts$class <- factor(counts$class, levels = class_levels)
counts <- counts[order(counts$class), ]
totals_p <- prep(totals)
trade_p <- prep(trade)

fill_cols <- c(
  "This basket only" = unname(paper_cat["blue"]),
  "Shared with other baskets" = unname(paper_cat["orange"]),
  "Legacy HS bridge code" = paper_ink$muted,
  "Added in the review" = unname(paper_cat["aqua"]),
  "Excluded in the review" = unname(paper_cat["red"]),
  "Moved to Upstream in the review" = unname(paper_cat["violet"])
)

x_max <- max(totals$n_in)
x_min <- -max(totals$n_left)
lab_in <- totals_p[totals_p$n_in > 0, ]
lab_ex <- totals_p[totals_p$n_left > 0, ]

panel_a <- ggplot() +
  geom_col(data = counts, aes(x = x, y = label, fill = class),
           width = 0.7, colour = paper_ink$surface, linewidth = 0.25,
           position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, colour = paper_ink$secondary, linewidth = 0.3) +
  geom_text(data = lab_in, aes(x = n_in, y = label, label = n_in),
            hjust = -0.25, size = 2.5, colour = paper_ink$secondary) +
  geom_text(data = lab_ex, aes(x = -n_left, y = label, label = n_left),
            hjust = 1.25, size = 2.5, colour = paper_ink$secondary) +
  facet_grid(supply_chain ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = fill_cols, breaks = class_levels[class_levels %in% counts$class], name = NULL,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(
    limits = c(x_min - 4, x_max + 5),
    breaks = seq(-20, 60, by = 10),
    labels = function(b) abs(b),
    expand = expansion(mult = 0)
  ) +
  labs(x = "Distinct HS6 codes\n(excluded or moved: left of 0)", y = NULL) +
  theme_paper(grid = "x") +
  theme(strip.text.y.left = element_text(angle = 90, face = "bold", hjust = 0.5),
        strip.placement = "outside",
        axis.line.x = element_blank(),
        legend.margin = margin(t = 0),
        panel.spacing.y = unit(0.6, "lines"),
        plot.margin = margin(14, 8, 6, 6))

# A basket the review created has no pre-review value: no bar, labelled "new".
trade_p$removed_plot <- dplyr::coalesce(trade_p$removed_share, 0)
trade_lab <- trade_p[!trade_p$new_basket & trade_p$removed_share >= 0.05, ]
trade_lab$txt <- sprintf("%.0f%%", 100 * trade_lab$removed_share)
new_lab <- trade_p[trade_p$new_basket, ]

panel_b <- ggplot(trade_p, aes(x = removed_plot, y = label)) +
  geom_col(width = 0.7, fill = paper_ink$secondary) +
  geom_text(data = trade_lab, aes(label = txt), hjust = -0.2, size = 2.5,
            colour = paper_ink$secondary) +
  geom_text(data = new_lab, aes(x = 0, label = "new"), hjust = -0.2, size = 2.5,
            fontface = "italic", colour = paper_ink$muted) +
  facet_grid(supply_chain ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1.42), breaks = c(0, 0.5, 1),
                     labels = c("0", "50", "100"),
                     expand = expansion(mult = 0)) +
  labs(x = sprintf("%% of pre-review %d\nbasket trade removed", trade_year), y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing.y = unit(0.6, "lines"),
        legend.position = "none",
        plot.margin = margin(14, 8, 6, 6))

legend <- cowplot::get_plot_component(panel_a, "guide-box-bottom", return_all = TRUE)
if (is.list(legend) && !inherits(legend, "grob")) legend <- legend[[1]]
row <- cowplot::plot_grid(
  panel_a + theme(legend.position = "none"), panel_b,
  nrow = 1, rel_widths = c(1, 0.34), align = "h", axis = "tb",
  labels = c("(a)", "(b)"), label_size = 9, label_fontface = "bold",
  label_x = c(0, 0), hjust = 0
)
fig <- cowplot::plot_grid(row, legend, ncol = 1, rel_heights = c(1, 0.1))

save_paper_fig(fig, "fig_hs6_crosswalk", width = paper_width_full, height = 4.5)

# ---------------------------------------------------------------------------
# Numbers and table
# ---------------------------------------------------------------------------
count_by <- function(d, nm) {
  if (nrow(d) == 0) {
    return(stats::setNames(data.frame(character(), character(), integer()), c("tech", "supply_chain", nm)))
  }
  out <- stats::aggregate(hs6 ~ tech + supply_chain, data = d, FUN = function(x) length(unique(x)))
  names(out)[3] <- nm
  out
}
shared_pairs <- pairs[pairs$n_cells > 1, ]
ess <- unique(master[master$essential, c("tech", "supply_chain", "hs6")])
caveat_master <- master_raw[caveat_row & !excluded_row, ]
caveat_master$hs6 <- hs6_clean_code(caveat_master$hs6)

by_cell <- all_cells
for (part in list(
  count_by(pairs[!pairs$legacy_hs, ], "codes_current"),
  count_by(pairs[pairs$legacy_hs, ], "codes_legacy"),
  count_by(shared_pairs, "codes_shared"),
  count_by(ess, "codes_essential"),
  count_by(added_pairs, "codes_added"),
  count_by(excl_pairs, "codes_excluded"),
  count_by(moved_pairs, "codes_moved_out"),
  count_by(caveat_master, "codes_kept_with_caveat")
)) {
  by_cell <- merge(by_cell, part, by = c("tech", "supply_chain"), all.x = TRUE)
}
by_cell[is.na(by_cell)] <- 0L
sub_sectors <- stats::aggregate(sub_sector ~ tech + supply_chain, data = master,
                                FUN = function(x) length(unique(x)))
names(sub_sectors)[3] <- "sub_sectors"
by_cell <- merge(by_cell, sub_sectors, by = c("tech", "supply_chain"), all.x = TRUE)
by_cell$sub_sectors[is.na(by_cell$sub_sectors)] <- 0L
by_cell$codes_total <- by_cell$codes_current + by_cell$codes_legacy
by_cell <- merge(by_cell, trade[, c("tech", "supply_chain", "value_pre", "value_post", "mirror_post",
                                    "new_basket", "removed_share", "moved_share", "added_share_post")],
                 by = c("tech", "supply_chain"))
by_cell$trade_post_usd_bn <- by_cell$value_post / 1e9
by_cell$trade_pre_usd_bn <- by_cell$value_pre / 1e9
by_cell$mirror_share_post <- ifelse(by_cell$value_post > 0, by_cell$mirror_post / by_cell$value_post, NA)
by_cell$value_pre <- NULL
by_cell$value_post <- NULL
by_cell$mirror_post <- NULL
by_cell <- by_cell[order(by_cell$tech, match(by_cell$supply_chain, stage_levels)), ]
# Sub-sectors are labels inside a basket: no code sits in two sub-sectors of one basket.
sub_keys <- unique(master[, c("tech", "supply_chain", "sub_sector", "hs6")])
stopifnot(!anyDuplicated(key3(sub_keys)))

chapters <- sort(table(substr(unique(pairs$hs6), 1, 2)), decreasing = TRUE)

headline <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal",
              "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind")
basket_stages <- unique(pairs[pairs$tech %in% headline, c("tech", "supply_chain")])

gas_mid <- pairs[pairs$tech == "Gas" & pairs$supply_chain == "Midstream", "hs6"]
oil_mid <- pairs[pairs$tech == "Oil" & pairs$supply_chain == "Midstream", "hs6"]

# --- Which in-basket codes each trade source can see -----------------------------------------
# Successors of each legacy bridge code, from the master's `note` column (and the HS correlation
# tables it cites). A current code counts as bridged in a basket when a legacy code of the same
# basket is its predecessor. The script stops if the master gains a legacy code not listed here.
legacy_successors <- list(
  "262090" = c("262091", "262099"),
  "271000" = c("271012", "271019", "271091", "271099"),
  "440310" = c("440311", "440312"),
  "711210" = "711291",
  "711220" = "711292",
  "711290" = "711299",
  "722510" = c("722511", "722519"),
  "722610" = c("722611", "722619"),
  "730410" = c("730411", "730419"),
  "730420" = c("730422", "730423", "730424", "730429"),
  "730610" = c("730611", "730619"),
  "730620" = c("730621", "730629"),
  "840619" = c("840681", "840682"),
  "854140" = c("854142", "854143"),
  "854441" = "854442",
  "854451" = "854442",
  "854459" = "854449",
  "870390" = "870380",
  "871190" = "871160",
  # HS92 8112.91 (other base metals: unwrought, waste and scrap, powders), 8112.92 since HS2002;
  # added with Semiconductors Upstream (user decision 2026-10-11).
  "811291" = "811292"
)
legacy_codes <- sort(unique(pairs$hs6[pairs$legacy_hs]))
unlisted <- setdiff(legacy_codes, names(legacy_successors))
if (length(unlisted) > 0) {
  stop("Legacy codes without a successor entry: ", paste(unlisted, collapse = ", "))
}

read_code_columns <- function(path, cols) {
  if (!file.exists(path)) return(NULL)
  data.table::fread(path, select = cols, colClasses = "character", showProgress = FALSE)
}
hs92 <- read_code_columns(file.path(raw_dir, "hs92_product_year_6.csv"), "product_hs92_code")
hs92_codes <- if (is.null(hs92)) NULL else unique(hs6_clean_code(hs92$product_hs92_code))
rm(hs92)
extract <- read_code_columns(file.path(raw_dir, "comtrade_energy_trade.csv"),
                             c("cmd_code", "ref_year", "reporter_iso", "flow_direction", "partner_iso"))

source_coverage <- list()
if (!is.null(hs92_codes)) {
  report <- hs6_match_report(master, hs92_codes)
  cur <- pairs[!pairs$legacy_hs, c("hs6", "tech", "supply_chain")]
  leg <- pairs[pairs$legacy_hs, c("hs6", "tech", "supply_chain")]
  cur$unmatched <- !cur$hs6 %in% hs92_codes
  cur$bridged <- vapply(seq_len(nrow(cur)), function(r) {
    pred <- leg$hs6[leg$tech == cur$tech[r] & leg$supply_chain == cur$supply_chain[r]]
    cur$hs6[r] %in% unlist(legacy_successors[pred], use.names = FALSE)
  }, logical(1))
  cur$unbridged <- cur$unmatched & !cur$bridged
  unbridged_codes <- sort(unique(cur$hs6[cur$unbridged]))
  scored_unbridged <- sort(unique(cur$hs6[cur$unbridged & cur$tech != "Semiconductors"]))
  cur$bridged_unmatched <- cur$unmatched & cur$bridged
  by_cell_hs92 <- stats::aggregate(
    cbind(unmatched, bridged_unmatched, unbridged) ~ tech + supply_chain,
    data = cur, FUN = sum
  )
  source_coverage$atlas_hs92 <- list(
    product_list = "data/raw/hs92_product_year_6.csv (all years)",
    codes_unmatched = report$n_unmatched,
    unmatched_current_codes = length(unique(cur$hs6[cur$unmatched])),
    unmatched_legacy_codes = length(setdiff(legacy_codes, hs92_codes)),
    current_codes_bridged = length(setdiff(unique(cur$hs6[cur$unmatched]), unbridged_codes)),
    current_codes_unbridged = length(unbridged_codes),
    current_codes_unbridged_scored_baskets = length(scored_unbridged),
    unbridged_codes = unbridged_codes,
    added_codes_in_hs92 = sort(intersect(unique(added_pairs$hs6), hs92_codes)),
    by_cell = by_cell_hs92[rowSums(by_cell_hs92[, c("unmatched", "bridged_unmatched", "unbridged")]) > 0, ]
  )
}
basket_codes <- unique(pairs$hs6)
if (!is.null(extract)) {
  extract_codes <- unique(hs6_clean_code(extract$cmd_code))
  absent <- setdiff(basket_codes, extract_codes)
  ex_rows <- extract[extract$flow_direction == "export", ]
  source_coverage$comtrade_energy_trade <- list(
    file = "data/raw/comtrade_energy_trade.csv (country-level trade themes; rebuilt from the dyad pull's World rows)",
    years = sort(unique(as.integer(extract$ref_year))),
    partners = sort(unique(extract$partner_iso)),
    export_reporters_by_year = as.list(tapply(ex_rows$reporter_iso, ex_rows$ref_year,
                                              function(x) length(unique(x)))),
    reporters_by_year = as.list(tapply(extract$reporter_iso, extract$ref_year,
                                       function(x) length(unique(x)))),
    codes_absent = length(absent),
    current_codes_absent = length(setdiff(absent, legacy_codes)),
    current_codes_absent_list = sort(setdiff(absent, legacy_codes)),
    legacy_codes_absent = length(intersect(absent, legacy_codes)),
    legacy_codes_present = sort(intersect(legacy_codes, extract_codes)),
    codes_present_not_in_basket = length(setdiff(extract_codes, basket_codes))
  )
  rm(extract, ex_rows)
}
absent_dyads <- setdiff(basket_codes, dyad_codes)
source_coverage$comtrade_dyads <- list(
  file = "data/raw/comtrade_energy_dyads_all.csv (bilateral and network layers)",
  years = dyad_years,
  reporters = dyad_reporters,
  codes_absent = length(absent_dyads),
  current_codes_absent = length(setdiff(absent_dyads, legacy_codes)),
  current_codes_absent_list = sort(setdiff(absent_dyads, legacy_codes)),
  legacy_codes_absent = length(intersect(absent_dyads, legacy_codes)),
  legacy_codes_present = sort(intersect(legacy_codes, dyad_codes))
)
invisible(gc())

top_excluded <- excl_detail %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::slice_max(value, n = 3, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(tech, supply_chain, hs6, value_usd_bn = value / 1e9, reason)

grid_up <- trade[trade$tech == "Electric Grid" & trade$supply_chain == "Upstream", ]
grid_mid <- trade[trade$tech == "Electric Grid" & trade$supply_chain == "Midstream", ]
code_value <- function(code) {
  v <- code_trade$value[code_trade$cmd_code == code]
  if (length(v) == 0) 0 else v
}

write_paper_results(
  list(
    source = "data/reference/energy_hs6_master.csv",
    review = paste(
      "basket review 2026-10: rows failing the dominant-use test kept with excluded = TRUE;",
      "Electric Grid Upstream given a copper and aluminium input basket (rows noted 'added');",
      "Upstream baskets for Electric Vehicles, Solar, Wind, Nuclear and Semiconductors (user decision",
      "2026-10-11; rows noted 'added', and rows noted 'moved from Midstream (...)' that the pre-review",
      "basket puts back in Midstream)"
    ),
    master_rows = nrow(master_raw),
    rows_in_basket = nrow(master),
    rows_excluded = sum(excluded_row),
    rows_added = sum(added_row),
    rows_moved = sum(moved_row),
    pairs_moved_out = nrow(moved_pairs),
    codes_moved_between_stages = sort(unique(moved_pairs$hs6)),
    baskets_new_in_review = sort(new_cells),
    rows_kept_with_caveat = sum(caveat_row & !excluded_row),
    codes_kept_with_caveat = length(unique(caveat_master$hs6)),
    pairs_excluded = nrow(excl_pairs),
    pairs_added = nrow(added_pairs),
    codes_excluded_somewhere = length(unique(excl_pairs$hs6)),
    codes_dropped_entirely = length(setdiff(unique(pairs_pre$hs6), unique(pairs$hs6))),
    codes_new_to_master = length(setdiff(unique(added_pairs$hs6), unique(pairs_pre$hs6))),
    added_codes_moved_from_other_baskets = sort(intersect(unique(added_pairs$hs6), unique(pairs_pre$hs6))),
    pre_review = list(
      rows = nrow(master_pre),
      distinct_codes = length(unique(pairs_pre$hs6)),
      basket_pairs = nrow(pairs_pre)
    ),
    distinct_codes = length(basket_codes),
    basket_pairs = nrow(pairs),
    current_codes = length(unique(pairs$hs6[!pairs$legacy_hs])),
    legacy_codes = length(legacy_codes),
    legacy_rows = sum(master$legacy_hs),
    legacy_codes_excluded = sort(setdiff(unique(pairs_pre$hs6[pairs_pre$legacy_hs]), legacy_codes)),
    technologies = sort(unique(pairs$tech)),
    stages = sort(unique(pairs$supply_chain)),
    codes_by_stage = as.list(tapply(pairs$hs6, pairs$supply_chain, function(x) length(unique(x)))),
    sub_sectors = length(unique(master$sub_sector)),
    essential_codes = length(unique(master$hs6[master$essential])),
    codes_by_number_of_baskets = as.list(table(as.integer(cells_per_code))),
    codes_in_more_than_one_basket = sum(cells_per_code > 1),
    share_codes_in_more_than_one_basket = mean(cells_per_code > 1),
    codes_by_number_of_technologies = as.list(table(as.integer(techs_per_code))),
    max_baskets_per_code = max(cells_per_code),
    codes_in_max_baskets = sort(names(cells_per_code)[cells_per_code == max(cells_per_code)]),
    gas_midstream = list(codes = length(gas_mid), shared = sum(cells_per_code[gas_mid] > 1),
                         shared_with_oil_midstream = length(intersect(gas_mid, oil_mid))),
    top_chapters = as.list(utils::head(chapters, 5)),
    headline_upstream_midstream_stages_with_basket = nrow(basket_stages),
    headline_upstream_midstream_stages_without_basket = setdiff(
      paste(rep(headline, each = 2), c("Upstream", "Midstream")),
      paste(basket_stages$tech, basket_stages$supply_chain)
    ),
    electric_grid = list(
      upstream_pre_review_code = sort(unique(pairs_pre$hs6[pairs_pre$tech == "Electric Grid" &
                                                              pairs_pre$supply_chain == "Upstream"])),
      upstream_pre_review_trade_usd_bn = grid_up$value_pre / 1e9,
      upstream_post_review_codes = sort(unique(added_pairs$hs6[added_pairs$tech == "Electric Grid"])),
      upstream_post_review_trade_usd_bn = grid_up$value_post / 1e9,
      midstream_removed_share = grid_mid$removed_share,
      hs851762_trade_usd_bn = code_value("851762") / 1e9,
      hs851762_share_of_pre_review_midstream = code_value("851762") / grid_mid$value_pre
    ),
    trade = list(
      source = "data/raw/comtrade_energy_dyads_all.csv, reconciled by partnership_strength_build_trade_cells()",
      year = trade_year,
      cif_fob_factor = cif_fob,
      note = paste(
        "technology-attributed: a code in several baskets counts in each;",
        "removed_share = trade of the excluded codes / pre-review basket trade (NA for a basket the",
        "review created; codes moved to another stage are not removals);",
        "moved_share = trade of the codes moved out of the basket / pre-review basket trade;",
        "added_share_post = trade of the added (incl. moved-in) codes / post-review basket trade"
      )
    ),
    by_cell = by_cell,
    top_excluded_codes = top_excluded,
    excluded_detail = excl_detail %>%
      dplyr::transmute(tech, supply_chain, hs6, legacy_hs, value_usd_bn = value / 1e9, reason),
    added_detail = added_detail %>%
      dplyr::transmute(tech, supply_chain, sub_sector, hs6, moved_in = moved, value_usd_bn = value / 1e9,
                       share_of_basket_code_level = value / cell_value_post, reason),
    moved_out_detail = moved_detail %>%
      dplyr::transmute(tech, from_stage = supply_chain, hs6, value_usd_bn = value / 1e9),
    source_coverage = source_coverage
  ),
  "hs6_crosswalk"
)

# --- Table tab:hs6_counts ---------------------------------------------------------------------
tab <- by_cell
tab$order_tech <- match(tab$tech, c(sort(setdiff(unique(tab$tech), "Semiconductors")), "Semiconductors"))
tab <- tab[order(tab$order_tech, match(tab$supply_chain, stage_levels)), ]
fmt_bn <- function(x) ifelse(x > 0, formatC(x, format = "f", digits = 1, big.mark = ","), "0")
# A basket the review created has no pre-review value, so no removed share ("new").
fmt_pct <- function(x) ifelse(is.na(x), "new", formatC(100 * x, format = "f", digits = 1))
prev_tech <- ""
rows <- character(0)
for (r in seq_len(nrow(tab))) {
  t <- tab$tech[r]
  shown <- if (t == prev_tech) "" else if (t == "Semiconductors") "Semiconductors\\tnote{a}" else t
  prev_tech <- t
  rows <- c(rows, sprintf(
    "%s & %s & %d & %d & %d & %d & %d & %d & %d & %s & %s\\\\",
    shown, tab$supply_chain[r], tab$codes_total[r], tab$codes_legacy[r], tab$codes_shared[r],
    tab$sub_sectors[r], tab$codes_added[r], tab$codes_moved_out[r], tab$codes_excluded[r],
    fmt_bn(tab$trade_post_usd_bn[r]), fmt_pct(tab$removed_share[r])
  ))
}
tex <- c(
  "% Generated by scripts/paper/fig_hs6_crosswalk.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{HS6 baskets by technology and stage after the October 2026 basket review}",
  "\\label{tab:hs6_counts}",
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{3.5pt}",
  "\\begin{tabular}{@{}llrrrrrrrrr@{}}",
  "\\toprule",
  sprintf(paste0(" & & \\multicolumn{4}{c}{Codes in the basket} & \\multicolumn{3}{c}{Review} & ",
                 "\\multicolumn{2}{c}{Basket trade, %d} \\\\"), trade_year),
  "\\cmidrule(lr){3-6}\\cmidrule(lr){7-9}\\cmidrule(l){10-11}",
  "Technology & Stage & All & Legacy & Shared & Sub-sectors & Added & Moved & Excluded & USD bn & \\% removed\\\\",
  "\\midrule",
  rows,
  "\\midrule",
  sprintf("Distinct codes & & %d & %d & %d & %d & %d & %d & %d & & \\\\",
          length(basket_codes), length(legacy_codes), sum(cells_per_code > 1),
          length(unique(master$sub_sector)), length(unique(added_pairs$hs6)),
          length(unique(moved_pairs$hs6)), length(unique(excl_pairs$hs6))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Distinct HS6 codes in each technology--stage basket of ",
    "\\texttt{data/reference/energy\\_hs6\\_master.csv} after the review. ``Legacy'' codes are bridge ",
    "codes of older HS revisions whose successors are all in the basket; ``Shared'' counts the codes ",
    "that also belong to at least one other basket. ``Excluded'' counts the codes the review removed ",
    "from the basket because the technology is not a dominant use of the product, and ``Added'' the ",
    "codes it added (the copper and aluminium inputs that replace electrical energy, HS 2716.00, in ",
    "electric-grid Upstream, and the Upstream baskets of electric vehicles, solar, wind, nuclear and ",
    "semiconductors, which had none); ``Moved'' counts the codes it moved from a technology's ",
    "Midstream to its Upstream basket (rare-earth compounds and metals, natural uranium compounds, ",
    "unwrought gallium, germanium and indium), which count as added there, since a code sits in one ",
    "stage of a technology; the rows stay in the master with the reason. Basket trade is the ",
    trade_year, " value in the reconciled all-reporter Comtrade matrix (the importer's declaration, ",
    "else the exporter's mirror declaration $\\times$ ", formatC(cif_fob, format = "f", digits = 2),
    "), summed over exporter--importer pairs; ``\\% removed'' is the share of the pre-review ",
    "basket's value carried by the excluded codes (moved codes are not removals; ``new'' marks a ",
    "basket the review created). A code in several baskets counts in each, so ",
    "neither codes nor trade values add across rows. Green hydrogen, geothermal and the Downstream ",
    "stage have no basket."
  ),
  "\\item[a] Carried in the master; not a scored technology.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tex, file.path(tab_dir, "misc_hs6_counts.tex"))

message("Wrote fig_hs6_crosswalk, hs6_crosswalk.json and tables/misc_hs6_counts.tex")
