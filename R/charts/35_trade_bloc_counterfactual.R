source(local({
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
  }
  if (!nzchar(this_file)) stop("Unable to resolve script path for bootstrap.")
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
}))

source(file.path(repo_root, "R", "utils", "country.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

parse_args <- function(args) {
  out <- list()
  if (length(args) == 0) return(out)
  for (arg in args) {
    if (!startsWith(arg, "--")) next
    kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- kv[1]
    val <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else TRUE
    out[[key]] <- val
  }
  out
}

parse_csv <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) return(character())
  vals <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

pick_col <- function(df, candidates, label = "column", required = TRUE) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) > 0) return(hits[[1]])
  if (required) {
    stop(sprintf("Could not find %s. Tried: %s", label, paste(candidates, collapse = ", ")))
  }
  NULL
}

find_numeric_col <- function(df, candidates = character(), exclude = c("Year", "component_weight", "component_weight_share", "weighted_component", "contribution")) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) > 0) return(hits[[1]])
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, exclude)
  if (length(numeric_cols) == 0) return(NULL)
  numeric_cols[[1]]
}

as_score01 <- function(x, inverse = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(x)
  rng <- range(x, na.rm = TRUE)
  out <- if (isTRUE(rng[1] >= 0 && rng[2] <= 1)) {
    x
  } else if (diff(rng) == 0) {
    rep(0.5, length(x))
  } else {
    (x - rng[1]) / diff(rng)
  }
  out <- pmin(pmax(out, 0), 1)
  if (isTRUE(inverse)) out <- 1 - out
  out
}

safe_row_mean <- function(...) {
  mat <- cbind(...)
  if (is.null(dim(mat))) return(as.numeric(mat))
  rowMeans(mat, na.rm = TRUE)
}

resolve_processed_tbl <- function(name, required = TRUE) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    if (isTRUE(required)) stop("Processed table not found: ", path)
    return(NULL)
  }
  readRDS(path)
}

read_index_outputs_local <- function() {
  outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
  if (!nzchar(outputs_rds_path)) {
    outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
      file.path(repo_root, config$outputs_rds)
    } else {
      file.path(processed_dir, "outputs", "index_outputs.rds")
    }
  }
  if (!file.exists(outputs_rds_path)) {
    stop("Index outputs not found: ", outputs_rds_path)
  }
  readRDS(outputs_rds_path)
}

read_country_info_local <- function() {
  path <- file.path(repo_root, config$raw_data_dir, "wdi_country_info.csv")
  if (!file.exists(path)) return(NULL)
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (exists("standardize_country_info", mode = "function")) {
    out <- tryCatch(standardize_country_info(out), error = function(e) out)
  }
  out
}

to_iso3 <- function(x, country_info = NULL) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  is_iso <- grepl("^[A-Za-z]{3}$", x)
  out[is_iso] <- toupper(x[is_iso])

  if (!is.null(country_info) && nrow(country_info) > 0) {
    possible_name_cols <- c("country", "Country", "country_name", "name", "economy", "Country Name")
    possible_iso_cols <- c("iso3c", "iso3", "ISO3", "country_iso3", "country_code")
    name_col <- pick_col(country_info, possible_name_cols, required = FALSE)
    iso_col <- pick_col(country_info, possible_iso_cols, required = FALSE)
    if (!is.null(name_col) && !is.null(iso_col)) {
      map_tbl <- unique(data.frame(
        key = trimws(as.character(country_info[[name_col]])),
        iso = toupper(as.character(country_info[[iso_col]])),
        stringsAsFactors = FALSE
      ))
      idx <- match(trimws(x), map_tbl$key)
      out[is.na(out) & !is.na(idx)] <- map_tbl$iso[idx[is.na(out) & !is.na(idx)]]
    }
  }

  cc <- suppressWarnings(countrycode::countrycode(x, "country.name", "iso3c", warn = FALSE))
  out[is.na(out)] <- cc[is.na(out)]
  toupper(out)
}

latest_stage_value <- function(tbl, out_name, variable_patterns = NULL, inverse = FALSE) {
  if (is.null(tbl) || !all(c("Country", "tech", "supply_chain") %in% names(tbl))) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      !!out_name := numeric()
    ))
  }

  year_col <- pick_col(tbl, c("Year", "year", "ref_year", "period"), required = FALSE)
  data_type_col <- pick_col(tbl, c("data_type"), required = FALSE)
  variable_col <- pick_col(tbl, c("variable", "Variable"), required = FALSE)
  value_col <- find_numeric_col(
    tbl,
    candidates = c("value", "Economic_Opportunity_Index", "Energy_Security_Index", "Partnership_Strength_Index", "strategic_index")
  )

  if (is.null(value_col)) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      !!out_name := numeric()
    ))
  }

  out <- tbl
  out[[value_col]] <- suppressWarnings(as.numeric(out[[value_col]]))

  if (!is.null(data_type_col)) {
    out <- out[is.na(out[[data_type_col]]) | out[[data_type_col]] %in% c("index", "Index"), , drop = FALSE]
  }

  if (!is.null(variable_col) && !is.null(variable_patterns) && length(variable_patterns) > 0) {
    pat <- paste(variable_patterns, collapse = "|")
    keep <- stringr::str_detect(tolower(as.character(out[[variable_col]])), pat)
    if (any(keep, na.rm = TRUE)) {
      out <- out[keep %in% TRUE, , drop = FALSE]
    }
  }

  if (!is.null(year_col)) {
    out[[year_col]] <- suppressWarnings(as.integer(out[[year_col]]))
    out <- out %>%
      dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
      dplyr::mutate(.latest_year = if (all(is.na(.data[[year_col]]))) NA_integer_ else max(.data[[year_col]], na.rm = TRUE)) %>%
      dplyr::filter(is.na(.data$.latest_year) | is.na(.data[[year_col]]) | .data[[year_col]] == .data$.latest_year) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$.latest_year)
  }

  out <- out %>%
    dplyr::filter(is.finite(.data[[value_col]])) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(!!out_name := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")

  out[[out_name]] <- as_score01(out[[out_name]], inverse = inverse)
  out
}

prepare_partner_affinity <- function(tbl, allies, out_name) {
  empty <- tibble::tibble(
    country_iso = character(),
    tech = character(),
    supply_chain = character(),
    !!out_name := numeric()
  )
  if (is.null(tbl)) return(empty)

  reporter_iso_col <- pick_col(tbl, c("reporter_iso", "Reporter_ISO", "reporter"), required = FALSE)
  partner_iso_col <- pick_col(tbl, c("partner_iso", "Partner_ISO", "partner"), required = FALSE)
  data_type_col <- pick_col(tbl, c("data_type"), required = FALSE)
  value_col <- find_numeric_col(tbl, candidates = c("value", "contribution"))

  if (is.null(reporter_iso_col) || is.null(partner_iso_col) || is.null(value_col)) return(empty)

  out <- tbl
  out[[reporter_iso_col]] <- toupper(as.character(out[[reporter_iso_col]]))
  out[[partner_iso_col]] <- toupper(as.character(out[[partner_iso_col]]))
  out[[value_col]] <- suppressWarnings(as.numeric(out[[value_col]]))

  if (!is.null(data_type_col)) {
    out <- out[out[[data_type_col]] %in% c("index", "Index") | is.na(out[[data_type_col]]), , drop = FALSE]
  }

  out <- out %>%
    dplyr::filter(is.finite(.data[[value_col]]), !is.na(.data[[reporter_iso_col]]), !is.na(.data[[partner_iso_col]])) %>%
    dplyr::mutate(is_ally = .data[[partner_iso_col]] %in% allies) %>%
    dplyr::group_by(country_iso = .data[[reporter_iso_col]], .data$tech, .data$supply_chain, .data$is_ally) %>%
    dplyr::summarise(avg_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$is_ally, values_from = .data$avg_value, names_prefix = "ally_") %>%
    dplyr::mutate(
      ally_true = dplyr::coalesce(.data$ally_true, 0),
      ally_false = dplyr::coalesce(.data$ally_false, 0),
      raw_score = dplyr::if_else(
        ally_true + ally_false > 0,
        ally_true / (ally_true + ally_false),
        NA_real_
      )
    ) %>%
    dplyr::select(.data$country_iso, .data$tech, .data$supply_chain, raw_score)

  names(out)[names(out) == "raw_score"] <- out_name
  out[[out_name]] <- as_score01(out[[out_name]])
  out
}

prepare_trade_clean <- function(comtrade_dyads, subcat, years = 2020:2024) {
  if (is.null(comtrade_dyads) || is.null(subcat)) return(NULL)
  required_trade_cols <- c("reporter_iso", "partner_iso")
  if (!all(required_trade_cols %in% names(comtrade_dyads))) {
    stop("Trade dyads file must contain reporter_iso and partner_iso columns.")
  }

  flow_col <- pick_col(comtrade_dyads, c("flow_direction", "trade_flow", "flow"), "flow", required = FALSE)
  period_col <- pick_col(comtrade_dyads, c("period", "year", "Year", "ref_year"), "period", required = FALSE)
  value_col <- pick_col(comtrade_dyads, c("primary_value", "trade_value", "value"), "trade value", required = FALSE)
  code_col <- pick_col(comtrade_dyads, c("cmd_code", "code", "hs6"), "HS6 code", required = FALSE)
  if (is.null(flow_col) || is.null(period_col) || is.null(value_col) || is.null(code_col)) {
    stop("Trade dyads file is missing one or more required columns for flow, period, value, or code.")
  }

  subcat_code_col <- pick_col(subcat, c("code", "cmd_code", "hs6"), required = FALSE)
  if (is.null(subcat_code_col)) stop("Subcategory file must contain a code/cmd_code/hs6 column.")

  mapping <- subcat %>%
    dplyr::transmute(
      code = stringr::str_pad(as.character(.data[[subcat_code_col]]), width = 6, side = "left", pad = "0"),
      tech = as.character(.data[[pick_col(subcat, c("tech", "Technology"), required = FALSE) %||% "tech"]]),
      supply_chain = as.character(.data[[pick_col(subcat, c("supply_chain", "Value.Chain", "Value Chain"), required = FALSE) %||% "supply_chain"]])
    ) %>%
    dplyr::distinct()

  comtrade_dyads %>%
    dplyr::transmute(
      reporter_iso = toupper(as.character(.data$reporter_iso)),
      partner_iso = toupper(as.character(.data$partner_iso)),
      period = suppressWarnings(as.integer(.data[[period_col]])),
      flow_direction = stringr::str_to_lower(as.character(.data[[flow_col]])),
      cmd_code = stringr::str_pad(as.character(.data[[code_col]]), width = 6, side = "left", pad = "0"),
      primary_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::left_join(mapping, by = c("cmd_code" = "code"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(.data$period), .data$period %in% years, !is.na(.data$tech), !is.na(.data$supply_chain))
}

build_trade_features <- function(trade_clean, allies, clean_techs) {
  empty_stage <- tibble::tibble(
    country_iso = character(),
    tech = character(),
    supply_chain = character(),
    import_total_trade_usd = numeric(),
    export_total_trade_usd = numeric(),
    import_ally_share = numeric(),
    export_ally_share = numeric(),
    import_hhi = numeric(),
    export_hhi = numeric(),
    clean_trade_value_usd = numeric()
  )
  empty_fossil <- tibble::tibble(
    country_iso = character(),
    fossil_import_value_usd = numeric(),
    fossil_import_hhi = numeric(),
    fossil_import_ally_share = numeric()
  )
  if (is.null(trade_clean)) return(list(stage = empty_stage, fossil = empty_fossil))

  clean_trade <- trade_clean %>%
    dplyr::filter(.data$tech %in% clean_techs) %>%
    dplyr::group_by(.data$reporter_iso, .data$partner_iso, .data$tech, .data$supply_chain, .data$flow_direction) %>%
    dplyr::summarise(trade_value_usd = sum(.data$primary_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$reporter_iso, .data$tech, .data$supply_chain, .data$flow_direction) %>%
    dplyr::mutate(
      total_trade_usd = sum(.data$trade_value_usd, na.rm = TRUE),
      share = dplyr::if_else(.data$total_trade_usd > 0, .data$trade_value_usd / .data$total_trade_usd, NA_real_),
      is_ally = .data$partner_iso %in% allies
    ) %>%
    dplyr::summarise(
      total_trade_usd = dplyr::first(.data$total_trade_usd),
      ally_share = sum(.data$share[.data$is_ally], na.rm = TRUE),
      hhi = sum((.data$share)^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$flow_direction,
      values_from = c(.data$total_trade_usd, .data$ally_share, .data$hhi),
      names_glue = "{flow_direction}_{.value}"
    ) %>%
    dplyr::rename(country_iso = .data$reporter_iso) %>%
    dplyr::mutate(
      import_total_trade_usd = dplyr::coalesce(.data$import_total_trade_usd, 0),
      export_total_trade_usd = dplyr::coalesce(.data$export_total_trade_usd, 0),
      import_ally_share = dplyr::coalesce(.data$import_ally_share, 0),
      export_ally_share = dplyr::coalesce(.data$export_ally_share, 0),
      import_hhi = dplyr::coalesce(.data$import_hhi, 1),
      export_hhi = dplyr::coalesce(.data$export_hhi, 1),
      clean_trade_value_usd = .data$import_total_trade_usd + .data$export_total_trade_usd
    )

  fossil_techs <- c("Oil", "Gas", "Coal")
  fossil_trade <- trade_clean %>%
    dplyr::filter(.data$tech %in% fossil_techs, .data$flow_direction == "import") %>%
    dplyr::group_by(.data$reporter_iso, .data$partner_iso) %>%
    dplyr::summarise(trade_value_usd = sum(.data$primary_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$reporter_iso) %>%
    dplyr::mutate(
      total_trade_usd = sum(.data$trade_value_usd, na.rm = TRUE),
      share = dplyr::if_else(.data$total_trade_usd > 0, .data$trade_value_usd / .data$total_trade_usd, NA_real_),
      is_ally = .data$partner_iso %in% allies
    ) %>%
    dplyr::summarise(
      fossil_import_value_usd = dplyr::first(.data$total_trade_usd),
      fossil_import_hhi = sum((.data$share)^2, na.rm = TRUE),
      fossil_import_ally_share = sum(.data$share[.data$is_ally], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(country_iso = .data$reporter_iso) %>%
    dplyr::mutate(
      fossil_import_value_usd = dplyr::coalesce(.data$fossil_import_value_usd, 0),
      fossil_import_hhi = dplyr::coalesce(.data$fossil_import_hhi, 1),
      fossil_import_ally_share = dplyr::coalesce(.data$fossil_import_ally_share, 0)
    )

  list(stage = clean_trade, fossil = fossil_trade)
}

run_bloc_counterfactual <- function(stage_df, params) {
  static_cost_wedge <- max(0, params$extra_bloc_tariff + params$ntb_equivalent - params$intra_bloc_trade_cost_reduction)

  out <- stage_df %>%
    dplyr::mutate(
      transition_weight = dplyr::case_when(
        .data$supply_chain == "Downstream" ~ 1.00,
        .data$supply_chain == "Midstream"  ~ 0.70,
        .data$supply_chain == "Upstream"   ~ 0.50,
        TRUE ~ 0.60
      ),
      bloc_quality_score = safe_row_mean(
        .data$friendshore_ally_score,
        .data$opportunity_ally_score,
        .data$development_ally_score,
        .data$policy_score
      ),
      opportunity_score = safe_row_mean(
        .data$eo_score,
        .data$future_demand_score,
        .data$production_depth_score,
        .data$opportunity_ally_score
      ),
      baseline_risk_score = safe_row_mean(
        .data$import_nonally_share,
        .data$import_hhi_score,
        .data$foreign_dependency_score,
        1 - .data$friendshore_ally_score
      ),
      cost_gap_score = safe_row_mean(
        1 - .data$cost_competitiveness_score,
        .data$overcapacity_score,
        1 - .data$policy_score
      ),
      trade_anchor_usd = dplyr::if_else(
        is.na(.data$clean_trade_value_usd) | .data$clean_trade_value_usd <= 0,
        params$default_trade_anchor_usd * pmax(0.1, safe_row_mean(.data$eo_score, .data$baseline_risk_score)),
        .data$clean_trade_value_usd
      ),
      import_anchor_usd = dplyr::if_else(
        is.na(.data$import_total_trade_usd) | .data$import_total_trade_usd <= 0,
        0.5 * .data$trade_anchor_usd,
        .data$import_total_trade_usd
      ),
      export_anchor_usd = dplyr::if_else(
        is.na(.data$export_total_trade_usd) | .data$export_total_trade_usd <= 0,
        0.5 * .data$trade_anchor_usd,
        .data$export_total_trade_usd
      ),
      fossil_anchor_usd = dplyr::if_else(
        is.na(.data$fossil_import_value_usd) | .data$fossil_import_value_usd <= 0,
        params$default_fossil_anchor_usd * pmax(0.1, .data$fossil_volatility_country),
        .data$fossil_import_value_usd
      ),
      bloc_shift_share = pmin(
        params$trade_diversion_cap,
        params$trade_diversion_elasticity * .data$baseline_risk_score * .data$bloc_quality_score
      ),
      bloc_risk_score = pmax(0, .data$baseline_risk_score * (1 - .data$bloc_shift_share)),
      security_benefit_score = pmax(0, .data$baseline_risk_score - .data$bloc_risk_score),
      export_gain_score = pmin(
        1,
        params$bloc_preference_markup * .data$opportunity_score * .data$export_market_gap * (0.25 + 0.75 * .data$bloc_quality_score)
      ),
      scale_learning_gain_score = pmin(
        1,
        params$learning_elasticity * .data$future_demand_score * .data$production_depth_score * (0.25 + 0.75 * .data$bloc_quality_score)
      ),
      static_cost_score = pmin(
        1,
        static_cost_wedge * .data$cost_gap_score * .data$bloc_shift_share
      ),
      fiscal_cost_score = pmin(
        1,
        params$public_support_rate * (1 - .data$policy_score) * (0.25 + 0.75 * .data$bloc_quality_score)
      ),
      avoided_fossil_volatility_score = pmin(
        1,
        params$fossil_pass_through * .data$fossil_volatility_country * .data$transition_weight * (0.25 + 0.75 * .data$bloc_quality_score)
      ),
      expected_disruption_cost_base_usd = .data$trade_anchor_usd * params$supply_shock_prob * params$supply_shock_loss * .data$baseline_risk_score,
      expected_disruption_cost_bloc_usd = .data$trade_anchor_usd * params$supply_shock_prob * params$supply_shock_loss * .data$bloc_risk_score,
      security_benefit_usd = pmax(0, .data$expected_disruption_cost_base_usd - .data$expected_disruption_cost_bloc_usd),
      export_gain_usd = .data$export_anchor_usd * .data$export_gain_score,
      scale_learning_gain_usd = .data$trade_anchor_usd * .data$scale_learning_gain_score,
      static_cost_premium_usd = .data$import_anchor_usd * .data$static_cost_score,
      fiscal_cost_usd = .data$trade_anchor_usd * .data$fiscal_cost_score,
      avoided_fossil_volatility_usd = .data$fossil_anchor_usd * .data$avoided_fossil_volatility_score,
      gross_benefit_usd = .data$security_benefit_usd + .data$export_gain_usd + .data$scale_learning_gain_usd + .data$avoided_fossil_volatility_usd,
      gross_cost_usd = .data$static_cost_premium_usd + .data$fiscal_cost_usd,
      net_value_usd = .data$gross_benefit_usd - .data$gross_cost_usd,
      net_value_index = (
        .data$security_benefit_score +
          .data$export_gain_score +
          .data$scale_learning_gain_score +
          .data$avoided_fossil_volatility_score -
          .data$static_cost_score -
          .data$fiscal_cost_score
      ),
      scenario_label = params$scenario_label
    )

  out
}

run_monte_carlo <- function(stage_df, params, n_sims = 250L, seed = 123L) {
  if (n_sims <= 0) {
    return(tibble::tibble(
      Country = character(),
      country_iso = character(),
      mean_net_value_usd = numeric(),
      p10_net_value_usd = numeric(),
      p50_net_value_usd = numeric(),
      p90_net_value_usd = numeric()
    ))
  }

  set.seed(seed)
  draws <- tibble::tibble(
    sim = seq_len(n_sims),
    trade_diversion_elasticity = stats::runif(n_sims, 0.35, 0.85),
    trade_diversion_cap = stats::runif(n_sims, 0.20, 0.50),
    bloc_preference_markup = stats::runif(n_sims, 0.03, 0.12),
    learning_elasticity = stats::runif(n_sims, 0.03, 0.12),
    supply_shock_prob = stats::runif(n_sims, 0.05, 0.20),
    supply_shock_loss = stats::runif(n_sims, 0.10, 0.40),
    fossil_pass_through = stats::runif(n_sims, 0.10, 0.35),
    public_support_rate = stats::runif(n_sims, 0.01, 0.06),
    intra_bloc_trade_cost_reduction = stats::runif(n_sims, 0.02, 0.08),
    extra_bloc_tariff = stats::runif(n_sims, 0.10, 0.20),
    ntb_equivalent = stats::runif(n_sims, 0.05, 0.15)
  )

  sim_results <- purrr::map_dfr(seq_len(n_sims), function(i) {
    p <- params
    for (nm in setdiff(names(draws), "sim")) p[[nm]] <- draws[[nm]][i]
    p$scenario_label <- sprintf("mc_%04d", i)

    run_bloc_counterfactual(stage_df, p) %>%
      dplyr::group_by(.data$Country, .data$country_iso) %>%
      dplyr::summarise(net_value_usd = sum(.data$net_value_usd, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(sim = i)
  })

  sim_results %>%
    dplyr::group_by(.data$Country, .data$country_iso) %>%
    dplyr::summarise(
      mean_net_value_usd = mean(.data$net_value_usd, na.rm = TRUE),
      p10_net_value_usd = stats::quantile(.data$net_value_usd, probs = 0.10, na.rm = TRUE),
      p50_net_value_usd = stats::quantile(.data$net_value_usd, probs = 0.50, na.rm = TRUE),
      p90_net_value_usd = stats::quantile(.data$net_value_usd, probs = 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

args <- if (sys.nframe() == 0) parse_args(commandArgs(trailingOnly = TRUE)) else list()
config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
if (is.null(config) || is.null(weights)) stop("Config and weights must be loaded by bootstrap.")

if (!exists("repo_root")) repo_root <- resolve_repo_root()
processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) stop("Processed directory not found: ", processed_dir)

base_outputs_dir <- file.path(repo_root, config$outputs_dir %||% "outputs")
out_dir <- args[["out-dir"]] %||% file.path(base_outputs_dir, "trade_bloc_counterfactual")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

country_info <- read_country_info_local()

tech_focus <- parse_csv(args[["techs"]])
if (length(tech_focus) == 0) tech_focus <- c("Solar", "Wind", "Batteries")

allies <- parse_csv(args[["allies"]])
if (length(allies) == 0) {
  allies <- c(
    "USA", "CAN", "MEX", "GBR", "DEU", "FRA", "ITA", "ESP", "NLD", "BEL",
    "DNK", "SWE", "FIN", "POL", "CZE", "ROU", "JPN", "KOR", "AUS", "IND",
    "VNM", "NOR"
  )
}
allies <- toupper(allies)

country_filter <- parse_csv(args[["countries"]])
country_filter_iso <- toupper(parse_csv(args[["country-iso"]]))

n_sims <- suppressWarnings(as.integer(args[["n-sims"]] %||% 250L))
if (is.na(n_sims)) n_sims <- 250L

params <- list(
  scenario_label = "allied_trade_bloc",
  intra_bloc_trade_cost_reduction = 0.05,
  extra_bloc_tariff = 0.15,
  ntb_equivalent = 0.10,
  trade_diversion_elasticity = 0.60,
  trade_diversion_cap = 0.35,
  bloc_preference_markup = 0.08,
  learning_elasticity = 0.08,
  supply_shock_prob = 0.10,
  supply_shock_loss = 0.30,
  fossil_pass_through = 0.20,
  public_support_rate = 0.03,
  default_trade_anchor_usd = 1e9,
  default_fossil_anchor_usd = 5e9
)

index_outputs <- read_index_outputs_local()
energy_security_index <- index_outputs$energy_security_index
if (is.null(energy_security_index)) energy_security_index <- index_outputs$energy_security_outputs$index %||% NULL

economic_opportunity_index <- index_outputs$economic_opportunity_index
if (is.null(economic_opportunity_index)) economic_opportunity_index <- index_outputs$economic_opportunity_outputs$index %||% NULL

policy_component_tbl <- resolve_processed_tbl("policy_component_tbl", required = FALSE)
policy_index <- index_outputs$policy_index %||% resolve_processed_tbl("policy_index", required = FALSE)
trade_concentration_tbl <- resolve_processed_tbl("trade_concentration_tbl", required = FALSE)
foreign_dependency_tbl <- resolve_processed_tbl("foreign_dependency_tbl", required = FALSE)
critical_minerals_trade_tbl <- resolve_processed_tbl("critical_minerals_trade_tbl", required = FALSE)
future_demand_tbl <- resolve_processed_tbl("future_demand_tbl", required = FALSE)
cost_competitiveness_tbl <- resolve_processed_tbl("cost_competitiveness_tbl", required = FALSE)
production_depth_momentum_tbl <- resolve_processed_tbl("production_depth_momentum_tbl", required = FALSE)
overcapacity_premium_tbl <- resolve_processed_tbl("overcapacity_premium_tbl", required = FALSE)
energy_prices_tbl <- resolve_processed_tbl("energy_prices_tbl", required = FALSE)
import_dependence_tbl <- resolve_processed_tbl("import_dependence_tbl", required = FALSE)
energy_consumption_tbl <- resolve_processed_tbl("energy_consumption_tbl", required = FALSE)
partner_friendshore_tbl <- resolve_processed_tbl("partner_friendshore_tbl", required = FALSE)
partner_opportunity_tbl <- resolve_processed_tbl("partner_opportunity_tbl", required = FALSE)
partner_development_tbl <- resolve_processed_tbl("partner_development_tbl", required = FALSE)

es_stage <- latest_stage_value(energy_security_index, "es_score")
eo_stage <- latest_stage_value(economic_opportunity_index, "eo_score")
policy_stage <- latest_stage_value(policy_index %||% policy_component_tbl, "policy_score")
foreign_dependency_stage <- latest_stage_value(foreign_dependency_tbl, "foreign_dependency_score")
trade_concentration_stage <- latest_stage_value(trade_concentration_tbl, "trade_concentration_score")
critical_minerals_stage <- latest_stage_value(critical_minerals_trade_tbl, "critical_minerals_score")
future_demand_stage <- latest_stage_value(future_demand_tbl, "future_demand_score")
cost_competitiveness_stage <- latest_stage_value(cost_competitiveness_tbl, "cost_competitiveness_score")
production_depth_stage <- latest_stage_value(production_depth_momentum_tbl, "production_depth_score")
overcapacity_stage <- latest_stage_value(overcapacity_premium_tbl, "overcapacity_score")

stage_tables <- list(
  es_stage,
  eo_stage,
  policy_stage,
  foreign_dependency_stage,
  trade_concentration_stage,
  critical_minerals_stage,
  future_demand_stage,
  cost_competitiveness_stage,
  production_depth_stage,
  overcapacity_stage
)

stage_base <- Reduce(
  function(x, y) dplyr::full_join(x, y, by = c("Country", "tech", "supply_chain")),
  stage_tables
)

stage_base <- stage_base %>%
  dplyr::mutate(country_iso = to_iso3(.data$Country, country_info)) %>%
  dplyr::filter(.data$tech %in% tech_focus)

friendshore_affinity <- prepare_partner_affinity(partner_friendshore_tbl, allies, "friendshore_ally_score")
opportunity_affinity <- prepare_partner_affinity(partner_opportunity_tbl, allies, "opportunity_ally_score")
development_affinity <- prepare_partner_affinity(partner_development_tbl, allies, "development_ally_score")

stage_base <- stage_base %>%
  dplyr::left_join(friendshore_affinity, by = c("country_iso", "tech", "supply_chain")) %>%
  dplyr::left_join(opportunity_affinity, by = c("country_iso", "tech", "supply_chain")) %>%
  dplyr::left_join(development_affinity, by = c("country_iso", "tech", "supply_chain"))

raw_trade_path <- file.path(repo_root, config$raw_data_dir, "allied_comtrade_energy_data.csv")
subcat_path <- file.path(repo_root, config$raw_data_dir, "consolidated_hs6_energy_tech_long.csv")
raw_trade <- if (file.exists(raw_trade_path)) utils::read.csv(raw_trade_path, stringsAsFactors = FALSE) else NULL
subcat <- if (file.exists(subcat_path)) utils::read.csv(subcat_path, stringsAsFactors = FALSE) else NULL

trade_clean <- if (!is.null(raw_trade) && !is.null(subcat)) prepare_trade_clean(raw_trade, subcat, years = 2020:2024) else NULL
trade_features <- build_trade_features(trade_clean, allies, tech_focus)

stage_base <- stage_base %>%
  dplyr::left_join(trade_features$stage, by = c("country_iso", "tech", "supply_chain"))

fossil_energy_prices <- latest_stage_value(
  energy_prices_tbl,
  "fossil_price_signal",
  variable_patterns = c("price_volatility", "mean_price_volatility", "overall price volatility", "latest_price", "yoy_price_change")
)

fossil_energy_prices <- fossil_energy_prices %>%
  dplyr::filter(.data$tech %in% c("Oil", "Gas", "Coal")) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(fossil_price_signal = mean(.data$fossil_price_signal, na.rm = TRUE), .groups = "drop")

fossil_import_dependence <- latest_stage_value(import_dependence_tbl, "fossil_import_dependence") %>%
  dplyr::filter(.data$tech %in% c("Oil", "Gas", "Coal")) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(fossil_import_dependence = mean(.data$fossil_import_dependence, na.rm = TRUE), .groups = "drop")

fossil_consumption <- latest_stage_value(energy_consumption_tbl, "fossil_consumption_score") %>%
  dplyr::filter(.data$tech %in% c("Oil", "Gas", "Coal")) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(fossil_consumption_score = mean(.data$fossil_consumption_score, na.rm = TRUE), .groups = "drop")

country_fossil <- Reduce(
  function(x, y) dplyr::full_join(x, y, by = "Country"),
  list(fossil_energy_prices, fossil_import_dependence, fossil_consumption)
) %>%
  dplyr::mutate(
    country_iso = to_iso3(.data$Country, country_info),
    fossil_volatility_country = safe_row_mean(
      .data$fossil_price_signal,
      .data$fossil_import_dependence,
      .data$fossil_consumption_score
    )
  ) %>%
  dplyr::select(.data$Country, .data$country_iso, .data$fossil_volatility_country) %>%
  dplyr::left_join(trade_features$fossil, by = "country_iso")

stage_base <- stage_base %>%
  dplyr::left_join(country_fossil %>% dplyr::select(-.data$Country), by = "country_iso")

if (nrow(stage_base) == 0) stop("No stage rows available after joining indices and filters.")

numeric_cols <- names(stage_base)[vapply(stage_base, is.numeric, logical(1))]
for (col in numeric_cols) {
  if (all(is.na(stage_base[[col]]))) next
  stage_base[[col]][is.na(stage_base[[col]])] <- mean(stage_base[[col]], na.rm = TRUE)
}

stage_base <- stage_base %>%
  dplyr::mutate(
    eo_score = dplyr::coalesce(as_score01(.data$eo_score), 0.5),
    es_score = dplyr::coalesce(as_score01(.data$es_score), 0.5),
    policy_score = dplyr::coalesce(as_score01(.data$policy_score), 0.5),
    foreign_dependency_score = dplyr::coalesce(as_score01(.data$foreign_dependency_score), 0.5),
    trade_concentration_score = dplyr::coalesce(as_score01(.data$trade_concentration_score), 0.5),
    critical_minerals_score = dplyr::coalesce(as_score01(.data$critical_minerals_score), 0.5),
    future_demand_score = dplyr::coalesce(as_score01(.data$future_demand_score), 0.5),
    cost_competitiveness_score = dplyr::coalesce(as_score01(.data$cost_competitiveness_score), 0.5),
    production_depth_score = dplyr::coalesce(as_score01(.data$production_depth_score), 0.5),
    overcapacity_score = dplyr::coalesce(as_score01(.data$overcapacity_score), 0.5),
    friendshore_ally_score = dplyr::coalesce(as_score01(.data$friendshore_ally_score), 0.5),
    opportunity_ally_score = dplyr::coalesce(as_score01(.data$opportunity_ally_score), 0.5),
    development_ally_score = dplyr::coalesce(as_score01(.data$development_ally_score), 0.5),
    import_ally_share = dplyr::coalesce(as_score01(.data$import_ally_share), 0),
    export_ally_share = dplyr::coalesce(as_score01(.data$export_ally_share), 0),
    import_hhi_score = dplyr::coalesce(as_score01(.data$import_hhi), 1),
    export_hhi_score = dplyr::coalesce(as_score01(.data$export_hhi), 1),
    import_nonally_share = 1 - .data$import_ally_share,
    export_market_gap = 1 - .data$export_ally_share,
    fossil_volatility_country = dplyr::coalesce(as_score01(.data$fossil_volatility_country), 0.5)
  )

if (length(country_filter) > 0) {
  stage_base <- stage_base %>% dplyr::filter(.data$Country %in% country_filter)
}
if (length(country_filter_iso) > 0) {
  stage_base <- stage_base %>% dplyr::filter(.data$country_iso %in% country_filter_iso)
}

stage_results <- run_bloc_counterfactual(stage_base, params) %>%
  dplyr::mutate(sector = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
  dplyr::arrange(dplyr::desc(.data$net_value_usd))

country_results <- stage_results %>%
  dplyr::group_by(.data$Country, .data$country_iso) %>%
  dplyr::summarise(
    net_value_usd = sum(.data$net_value_usd, na.rm = TRUE),
    gross_benefit_usd = sum(.data$gross_benefit_usd, na.rm = TRUE),
    gross_cost_usd = sum(.data$gross_cost_usd, na.rm = TRUE),
    security_benefit_usd = sum(.data$security_benefit_usd, na.rm = TRUE),
    export_gain_usd = sum(.data$export_gain_usd, na.rm = TRUE),
    scale_learning_gain_usd = sum(.data$scale_learning_gain_usd, na.rm = TRUE),
    avoided_fossil_volatility_usd = sum(.data$avoided_fossil_volatility_usd, na.rm = TRUE),
    static_cost_premium_usd = sum(.data$static_cost_premium_usd, na.rm = TRUE),
    fiscal_cost_usd = sum(.data$fiscal_cost_usd, na.rm = TRUE),
    mean_net_value_index = mean(.data$net_value_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$net_value_usd))

tech_results <- stage_results %>%
  dplyr::group_by(.data$tech, .data$supply_chain) %>%
  dplyr::summarise(
    net_value_usd = sum(.data$net_value_usd, na.rm = TRUE),
    gross_benefit_usd = sum(.data$gross_benefit_usd, na.rm = TRUE),
    gross_cost_usd = sum(.data$gross_cost_usd, na.rm = TRUE),
    mean_net_value_index = mean(.data$net_value_index, na.rm = TRUE),
    n_countries = dplyr::n_distinct(.data$country_iso),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$net_value_usd))

country_tech_results <- stage_results %>%
  dplyr::group_by(.data$Country, .data$country_iso, .data$tech) %>%
  dplyr::summarise(
    net_value_usd = sum(.data$net_value_usd, na.rm = TRUE),
    gross_benefit_usd = sum(.data$gross_benefit_usd, na.rm = TRUE),
    gross_cost_usd = sum(.data$gross_cost_usd, na.rm = TRUE),
    mean_net_value_index = mean(.data$net_value_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$net_value_usd))

mc_country <- run_monte_carlo(stage_base, params, n_sims = n_sims, seed = 123L)

assumptions_tbl <- tibble::tibble(
  parameter = names(params),
  value = unlist(params, use.names = FALSE)
)

metadata_tbl <- tibble::tibble(
  run_timestamp = as.character(Sys.time()),
  repo_root = repo_root,
  processed_dir = processed_dir,
  out_dir = out_dir,
  raw_trade_available = !is.null(raw_trade),
  subcat_available = !is.null(subcat),
  n_stage_rows = nrow(stage_results),
  n_country_rows = nrow(country_results),
  n_sims = n_sims,
  tech_focus = paste(tech_focus, collapse = ","),
  allies = paste(allies, collapse = ",")
)

readr::write_csv(stage_results, file.path(out_dir, "trade_bloc_counterfactual_stage_results.csv"))
readr::write_csv(country_results, file.path(out_dir, "trade_bloc_counterfactual_country_results.csv"))
readr::write_csv(country_tech_results, file.path(out_dir, "trade_bloc_counterfactual_country_tech_results.csv"))
readr::write_csv(tech_results, file.path(out_dir, "trade_bloc_counterfactual_technology_results.csv"))
readr::write_csv(mc_country, file.path(out_dir, "trade_bloc_counterfactual_monte_carlo_country_results.csv"))
readr::write_csv(assumptions_tbl, file.path(out_dir, "trade_bloc_counterfactual_assumptions.csv"))
readr::write_csv(metadata_tbl, file.path(out_dir, "trade_bloc_counterfactual_metadata.csv"))

saveRDS(
  list(
    stage_results = stage_results,
    country_results = country_results,
    country_tech_results = country_tech_results,
    technology_results = tech_results,
    monte_carlo_country = mc_country,
    assumptions = assumptions_tbl,
    metadata = metadata_tbl,
    params = params
  ),
  file.path(out_dir, "trade_bloc_counterfactual_results.rds")
)

message("Wrote trade bloc counterfactual outputs to: ", out_dir)
