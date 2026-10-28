# Policy Design layer of the strategic posture (docs/strategic_position.md, section 7, "Policy design
# and evaluation"). Turns a node's posture variant and delivery mode into a fence, a gate and a build,
# with a benchmark, success metrics, an exit condition and a review interval.
#
# The content ports the NEIS Yards & Fences policy layer: needsFor(), successMetrics() and
# diagnosedFence() in dashboard/src/fence/engine/policy.ts, the toolkits in
# dashboard/src/fence/content/policy.ts and the posture text in classify.ts. Small yard = secure,
# medium yard = compete, large yard = deploy. It lives in two config tables that scripts read and pass
# in:
#
#   config/posture_policy_design.csv   one row per posture_variant (all 17 must exist)
#   config/posture_delivery_modes.csv  one row per delivery_mode x strategic_posture (all 12 pairs):
#                                      a build_note and a gate_note that adjust the variant's build and
#                                      gate to where capacity is built (allied delivery builds trusted
#                                      capacity with partners, not at home)
#
# Everything here is categorical text. fence_level (none < low < moderate < high) and gate_level
# (closed < selective < open < wide_open) are ordered labels of the diagnosed fence geometry, not a
# score: there is no fence index, yard index or posture score, and nothing is summed or averaged.
#
# needs_<purpose> follow the policy.ts purposes (secure, buffer, build, protect, open, discipline) with
# the policy.ts meaning: `build` is capability building for producers (production subsidy, R&D,
# public finance, workforce). A Deploy node's build_posture (infrastructure, permitting,
# interoperability) is therefore consistent with needs_build = neutral.
#
# Observed policy never enters this layer: the design is what the posture calls for. The comparison
# with observed policy is policy_alignment (R/strategic_position/posture_engine.R).
#
# Pure functions: no IO.

PPD_VARIANTS <- c(
  "secure_control", "secure_integrated", "secure_whole", "secure_unclear", "secure_input", "secure_both",
  "secure_dual_use", "secure_resilience",
  "compete_preserve", "compete_adjacent_capability", "compete_catch_up", "compete_leap_ahead", "compete_costly",
  "compete_partner",
  "deploy", "deploy_floor", "deploy_diversify"
)
PPD_POSTURES <- c("secure", "compete", "deploy")
PPD_FENCE_LEVELS <- c("none", "low", "moderate", "high")
PPD_GATE_LEVELS <- c("closed", "selective", "open", "wide_open")
# What the fence surrounds. Secure scopes follow SmallScope in dashboard/src/fence/engine/types.ts;
# Compete fences cover the node's product (or, for a leap, the successor technology); Deploy fences are
# the cross-cutting safeguards that apply to any good, or a specified capability floor. The security
# routes add critical_end_use (a dual-use node's national-security uses), essential_supply (systemic
# resilience: no import fence) and partner_supply (Compete through trusted partners).
PPD_FENCE_SCOPES <- c(
  "control_layer", "control_plus_integrated", "whole_product", "control_unclear", "input",
  "control_layer_and_input", "critical_end_use", "essential_supply", "product", "successor_technology",
  "partner_supply", "capability_floor", "cross_cutting"
)
PPD_PURPOSES <- c("secure", "buffer", "build", "protect", "open", "discipline")
PPD_NEEDS <- c("required", "helpful", "neutral", "avoid")
PPD_DELIVERY_MODES <- c("domestic", "allied", "diversified_global", "mixed")

PPD_LIST_COLS <- c("fence_instruments", "gate_instruments", "build_instruments", "success_metrics")
PPD_TEXT_COLS <- c("fence_posture", "gate_posture", "build_posture", "benchmark", "exit_condition", "source")
PPD_DESIGN_COLS <- c(
  "posture_variant", "strategic_posture", "fence_level", "fence_scope", "fence_posture", "fence_instruments",
  "gate_level", "gate_posture", "gate_instruments", "build_posture", "build_instruments",
  paste0("needs_", PPD_PURPOSES), "success_metrics", "benchmark", "exit_condition", "review_interval_years", "source"
)
PPD_DELIVERY_COLS <- c("delivery_mode", "strategic_posture", "build_note", "gate_note")

# Columns sp_policy_design() adds; `detail = TRUE` adds the instrument lists and needs as well.
PPD_OUTPUT_COLS <- c(
  "fence_level", "fence_scope", "fence_posture", "gate_level", "gate_posture", "build_posture",
  "benchmark", "success_metrics", "exit_condition", "review_interval_years"
)
PPD_DETAIL_COLS <- c("fence_instruments", "gate_instruments", "build_instruments", paste0("needs_", PPD_PURPOSES))

# Posture of a variant: the prefix before the first underscore ("deploy" is its own posture).
ppd_variant_posture <- function(variant) {
  sub("_.*$", "", as.character(variant))
}

# A semicolon list as a character vector (items trimmed; empty items kept so the check can see them).
ppd_split_list <- function(x) {
  if (is.na(x)) return(character())
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

ppd_blank <- function(x) is.na(x) | !nzchar(trimws(x))

ppd_as_chr_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (col in names(x)) if (is.character(x[[col]]) || is.factor(x[[col]])) x[[col]] <- trimws(as.character(x[[col]]))
  x
}

# Validates the two config tables and returns them normalised (trimmed text, integer review interval).
# Stops with every problem found at once, so a config edit is fixed in one pass.
ppd_read_check <- function(design, delivery) {
  design <- ppd_as_chr_df(design)
  delivery <- ppd_as_chr_df(delivery)
  bad <- character()
  miss_d <- setdiff(PPD_DESIGN_COLS, names(design))
  miss_l <- setdiff(PPD_DELIVERY_COLS, names(delivery))
  if (length(miss_d)) bad <- c(bad, paste("design: missing column(s)", paste(miss_d, collapse = ", ")))
  if (length(miss_l)) bad <- c(bad, paste("delivery: missing column(s)", paste(miss_l, collapse = ", ")))
  if (length(bad)) stop("ppd_read_check(): ", paste(bad, collapse = "; "), call. = FALSE)

  v <- design$posture_variant
  if (anyDuplicated(v)) bad <- c(bad, paste("duplicate posture_variant:", paste(unique(v[duplicated(v)]), collapse = ", ")))
  if (length(setdiff(PPD_VARIANTS, v))) bad <- c(bad, paste("missing posture_variant:", paste(setdiff(PPD_VARIANTS, v), collapse = ", ")))
  if (length(setdiff(v, PPD_VARIANTS))) bad <- c(bad, paste("unknown posture_variant:", paste(setdiff(v, PPD_VARIANTS), collapse = ", ")))
  wrong_post <- v[!is.na(v) & design$strategic_posture != ppd_variant_posture(v)]
  if (length(wrong_post)) bad <- c(bad, paste("strategic_posture does not match the variant:", paste(wrong_post, collapse = ", ")))

  vocab <- function(col, allowed) {
    x <- design[[col]]
    no <- is.na(x) | !x %in% allowed
    if (any(no)) sprintf("%s: invalid value(s) %s (variants %s)", col, paste(unique(x[no]), collapse = ", "), paste(v[no], collapse = ", "))
  }
  bad <- c(bad, vocab("fence_level", PPD_FENCE_LEVELS), vocab("gate_level", PPD_GATE_LEVELS),
           vocab("fence_scope", PPD_FENCE_SCOPES))
  for (p in PPD_PURPOSES) bad <- c(bad, vocab(paste0("needs_", p), PPD_NEEDS))

  for (col in PPD_TEXT_COLS) {
    e <- ppd_blank(design[[col]])
    if (any(e)) bad <- c(bad, sprintf("%s: empty for %s", col, paste(v[e], collapse = ", ")))
  }
  for (col in PPD_LIST_COLS) {
    items <- lapply(design[[col]], ppd_split_list)
    e <- vapply(items, function(i) length(i) == 0 || any(!nzchar(i)), logical(1))
    if (any(e)) bad <- c(bad, sprintf("%s: empty list or list item for %s", col, paste(v[e], collapse = ", ")))
  }
  ri <- suppressWarnings(as.numeric(design$review_interval_years))
  bad_ri <- is.na(ri) | ri < 1 | ri != round(ri)
  if (any(bad_ri)) bad <- c(bad, paste("review_interval_years must be a whole number of years >= 1:", paste(v[bad_ri], collapse = ", ")))

  m <- delivery$delivery_mode
  p <- delivery$strategic_posture
  if (any(is.na(m) | !m %in% PPD_DELIVERY_MODES)) bad <- c(bad, paste("delivery: invalid delivery_mode", paste(unique(m[is.na(m) | !m %in% PPD_DELIVERY_MODES]), collapse = ", ")))
  if (any(is.na(p) | !p %in% PPD_POSTURES)) bad <- c(bad, paste("delivery: invalid strategic_posture", paste(unique(p[is.na(p) | !p %in% PPD_POSTURES]), collapse = ", ")))
  key <- paste(m, p)
  if (anyDuplicated(key)) bad <- c(bad, paste("delivery: duplicate delivery_mode x strategic_posture:", paste(unique(key[duplicated(key)]), collapse = ", ")))
  want <- paste(rep(PPD_DELIVERY_MODES, each = length(PPD_POSTURES)), rep(PPD_POSTURES, length(PPD_DELIVERY_MODES)))
  if (length(setdiff(want, key))) bad <- c(bad, paste("delivery: missing delivery_mode x strategic_posture:", paste(setdiff(want, key), collapse = ", ")))
  for (col in c("build_note", "gate_note")) {
    e <- ppd_blank(delivery[[col]])
    if (any(e)) bad <- c(bad, sprintf("delivery: %s empty for %s", col, paste(key[e], collapse = ", ")))
  }

  if (length(bad)) stop("ppd_read_check(): ", paste(bad, collapse = "; "), call. = FALSE)
  design$review_interval_years <- as.integer(ri)
  list(design = design[, PPD_DESIGN_COLS], delivery = delivery[, PPD_DELIVERY_COLS])
}

ppd_delivery_label <- function(mode) gsub("_", " ", mode, fixed = TRUE)

# Adds the policy design to a posture table (cells or sensitive layers) by posture_variant and
# delivery_mode. `tbl` needs posture_variant; delivery_mode and strategic_posture are used when present.
# A missing variant (an unclassified node) gets a missing design; a missing delivery mode leaves the
# variant's build and gate unadjusted. Existing design columns are replaced, so a re-run is idempotent.
sp_policy_design <- function(tbl, design, delivery, detail = FALSE) {
  if (!"posture_variant" %in% names(tbl)) stop("sp_policy_design(): tbl has no posture_variant column", call. = FALSE)
  cfg <- ppd_read_check(design, delivery)
  v <- as.character(tbl$posture_variant)
  unknown <- setdiff(unique(v[!is.na(v)]), PPD_VARIANTS)
  if (length(unknown)) stop("sp_policy_design(): unknown posture_variant ", paste(unknown, collapse = ", "), call. = FALSE)
  mode <- if ("delivery_mode" %in% names(tbl)) as.character(tbl$delivery_mode) else rep(NA_character_, nrow(tbl))
  bad_mode <- setdiff(unique(mode[!is.na(mode)]), PPD_DELIVERY_MODES)
  if (length(bad_mode)) stop("sp_policy_design(): unknown delivery_mode ", paste(bad_mode, collapse = ", "), call. = FALSE)
  # The design is keyed on the variant; a posture column that disagrees with it means the engine and
  # this table are out of step, which must not pass silently.
  if ("strategic_posture" %in% names(tbl)) {
    clash <- !is.na(v) & !is.na(tbl$strategic_posture) & tbl$strategic_posture != ppd_variant_posture(v)
    if (any(clash)) {
      stop("sp_policy_design(): strategic_posture disagrees with posture_variant in ", sum(clash), " row(s), e.g. ",
           tbl$strategic_posture[clash][1], " / ", v[clash][1], call. = FALSE)
    }
  }

  add_cols <- c(PPD_OUTPUT_COLS, if (detail) PPD_DETAIL_COLS)
  keys <- data.frame(.ppd_row = seq_len(nrow(tbl)), posture_variant = v, .ppd_mode = mode,
                     .ppd_posture = ppd_variant_posture(v), stringsAsFactors = FALSE)
  joined <- keys %>%
    dplyr::left_join(cfg$design[, c("posture_variant", add_cols)], by = "posture_variant", relationship = "many-to-one") %>%
    dplyr::left_join(
      dplyr::rename(cfg$delivery, .ppd_mode = "delivery_mode", .ppd_posture = "strategic_posture"),
      by = c(".ppd_mode", ".ppd_posture"), relationship = "many-to-one"
    ) %>%
    dplyr::arrange(.data$.ppd_row) %>%
    dplyr::mutate(
      .ppd_note = !is.na(.data$build_posture) & !is.na(.data$build_note),
      build_posture = dplyr::if_else(
        .data$.ppd_note,
        paste0(.data$build_posture, " Delivery (", ppd_delivery_label(.data$.ppd_mode), "): ", .data$build_note),
        .data$build_posture
      ),
      gate_posture = dplyr::if_else(
        !is.na(.data$gate_posture) & !is.na(.data$gate_note),
        paste0(.data$gate_posture, " Delivery (", ppd_delivery_label(.data$.ppd_mode), "): ", .data$gate_note),
        .data$gate_posture
      )
    )

  out <- tbl[, setdiff(names(tbl), c(PPD_OUTPUT_COLS, PPD_DETAIL_COLS)), drop = FALSE]
  for (col in add_cols) out[[col]] <- joined[[col]]
  out
}

# Needs of one variant as a named character vector (secure, buffer, build, protect, open, discipline).
ppd_needs <- function(design, variant) {
  r <- design[design$posture_variant == variant, , drop = FALSE]
  if (nrow(r) != 1) stop("ppd_needs(): no single design row for ", variant, call. = FALSE)
  stats::setNames(unlist(r[1, paste0("needs_", PPD_PURPOSES)], use.names = FALSE), PPD_PURPOSES)
}
