assert_required_columns <- function(df, required_cols, df_name = "data.frame") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in ", df_name, ": ",
        paste(missing_cols, collapse = ", "),
        ". Available columns: ",
        paste(names(df), collapse = ", ")
      )
    )
  }
}

compute_ipd_matrix <- function(theta_df, year, spec_name) {
  assert_required_columns(theta_df, c("iso3", "country", "year", "theta", "spec_name"), "theta_df")

  year_df <- theta_df[theta_df$year == year & theta_df$spec_name == spec_name, c("iso3", "country", "theta")]
  if (nrow(year_df) == 0) {
    stop(paste0("No ideal points found for requested year/spec: year=", year, ", spec=", spec_name, "."))
  }

  pairs <- merge(year_df, year_df, by = NULL, suffixes = c("_i", "_j"))
  pairs$year <- year
  pairs$spec_name <- spec_name
  pairs$ipd <- abs(pairs$theta_i - pairs$theta_j)

  theta_range <- max(year_df$theta, na.rm = TRUE) - min(year_df$theta, na.rm = TRUE)
  if (isTRUE(all.equal(theta_range, 0))) stop("Theta range is zero; cannot normalize.")
  pairs$gp_norm <- pairs$ipd / theta_range

  pairs[, c("iso3_i", "iso3_j", "year", "ipd", "gp_norm", "spec_name")]
}

compute_seg_scores <- function(ipd_df, country_map, year, spec_name, us_iso3 = "USA", china_iso3 = "CHN") {
  assert_required_columns(ipd_df, c("iso3_i", "iso3_j", "year", "ipd", "spec_name"), "ipd_df")
  assert_required_columns(country_map, c("iso3", "country"), "country_map")

  year_df <- ipd_df[ipd_df$year == year & ipd_df$spec_name == spec_name, ]

  to_us <- year_df[year_df$iso3_j == us_iso3, c("iso3_i", "ipd")]
  names(to_us) <- c("iso3", "ipd_to_us")

  to_china <- year_df[year_df$iso3_j == china_iso3, c("iso3_i", "ipd")]
  names(to_china) <- c("iso3", "ipd_to_china")

  seg <- merge(to_us, to_china, by = "iso3", all = TRUE)
  seg$seg <- with(seg, ifelse((ipd_to_us + ipd_to_china) == 0, NA_real_,
                              (ipd_to_china - ipd_to_us) / (ipd_to_us + ipd_to_china)))
  seg$year <- year
  seg$spec_name <- spec_name
  seg <- merge(seg, unique(country_map[, c("iso3", "country")]), by = "iso3", all.x = TRUE)
  seg[, c("iso3", "country", "year", "ipd_to_us", "ipd_to_china", "seg", "spec_name")]
}

assign_blocs <- function(seg_df) {
  q1 <- stats::quantile(seg_df$seg, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(seg_df$seg, 0.75, na.rm = TRUE)
  seg_df$bloc <- ifelse(seg_df$seg <= q1, "China-leaning",
                        ifelse(seg_df$seg >= q3, "U.S.-leaning", "Nonaligned"))
  seg_df
}

build_pair_bloc_labels <- function(bloc_df) {
  pairs <- merge(bloc_df[, c("iso3", "bloc", "year", "spec_name")],
                 bloc_df[, c("iso3", "bloc", "year", "spec_name")],
                 by = c("year", "spec_name"), suffixes = c("_i", "_j"))

  pairs$pair_type <- ifelse(pairs$bloc_i == "Nonaligned" | pairs$bloc_j == "Nonaligned", "nonaligned",
                            ifelse(pairs$bloc_i == pairs$bloc_j, "within_bloc", "between_bloc"))
  pairs[, c("iso3_i", "iso3_j", "year", "bloc_i", "bloc_j", "pair_type", "spec_name")]
}

compute_exposure <- function(trade_shares_df, gp_norm_df, averaging_years = NULL) {
  gp <- gp_norm_df
  if (!is.null(averaging_years)) {
    gp <- gp[gp$year %in% averaging_years, ]
  }
  gp_avg <- aggregate(gp_norm ~ iso3_i + iso3_j + spec_name, data = gp, FUN = mean, na.rm = TRUE)

  merged <- merge(trade_shares_df, gp_avg,
                  by.x = c("iso3", "partner_iso3", "spec_name"),
                  by.y = c("iso3_i", "iso3_j", "spec_name"),
                  all.x = TRUE)
  merged$weighted <- merged$share * merged$gp_norm

  exposure <- aggregate(weighted ~ iso3 + country + year + spec_name, data = merged, FUN = sum, na.rm = TRUE)
  names(exposure)[names(exposure) == "weighted"] <- "exposure"
  exposure
}

validate_outputs <- function(ipd_df, seg_df, bloc_df, us_iso3 = "USA", china_iso3 = "CHN") {
  symmetry <- merge(ipd_df, ipd_df, by.x = c("iso3_i", "iso3_j", "year", "spec_name"),
                    by.y = c("iso3_j", "iso3_i", "year", "spec_name"), suffixes = c("", "_rev"))
  list(
    ipd_symmetric = all(abs(symmetry$ipd - symmetry$ipd_rev) < 1e-8, na.rm = TRUE),
    ipd_diagonal_zero = all(ipd_df$ipd[ipd_df$iso3_i == ipd_df$iso3_j] == 0, na.rm = TRUE),
    gp_norm_bounds = all(ipd_df$gp_norm >= -1e-8 & ipd_df$gp_norm <= 1 + 1e-8, na.rm = TRUE),
    seg_us = seg_df$seg[seg_df$iso3 == us_iso3],
    seg_china = seg_df$seg[seg_df$iso3 == china_iso3],
    quartile_check_us = all(bloc_df$seg[bloc_df$bloc == "U.S.-leaning"] >= stats::quantile(bloc_df$seg, 0.75, na.rm = TRUE), na.rm = TRUE),
    quartile_check_china = all(bloc_df$seg[bloc_df$bloc == "China-leaning"] <= stats::quantile(bloc_df$seg, 0.25, na.rm = TRUE), na.rm = TRUE)
  )
}
