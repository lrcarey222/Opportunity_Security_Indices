technological_readiness_clean <- function(iea_cleantech_all) {
  require_columns(iea_cleantech_all, c("sector", "trl2023", "name"), label = "iea_cleantech_all")

  iea_cleantech_all %>%
    dplyr::mutate(sector_raw = sector) %>%
    tidyr::separate(
      sector,
      into = c("sector1", "sector2", "sector3", "sector4"),
      sep = ",",
      fill = "right",
      extra = "merge"
    ) %>%
    dplyr::mutate(
      trl2023 = dplyr::case_when(
        is.na(trl2023) ~ NA_real_,
        stringr::str_detect(as.character(trl2023), "-") ~ {
          parts <- stringr::str_split(as.character(trl2023), "-")
          purrr::map_dbl(parts, function(values) {
            numeric_values <- readr::parse_number(values)
            numeric_values <- numeric_values[!is.na(numeric_values)]
            if (length(numeric_values) == 0) {
              NA_real_
            } else {
              mean(numeric_values)
            }
          })
        },
        TRUE ~ readr::parse_number(as.character(trl2023))
      )
    )
}

taxonomy_has_token <- function(sector_raw, sector4, token) {
  pattern <- stringr::regex(
    paste0("(^|,)\\s*", stringr::str_replace_all(token, "([\\W])", "\\\\\\1"), "\\s*(,|$)"),
    ignore_case = TRUE
  )

  stringr::str_detect(dplyr::coalesce(as.character(sector_raw), ""), pattern) |
    stringr::str_detect(dplyr::coalesce(as.character(sector4), ""), pattern)
}

technological_readiness_assign_sector <- function(iea_cleantech) {
  iea_cleantech %>%
    dplyr::mutate(
      battery_in_taxonomy = taxonomy_has_token(sector_raw, sector4, "Battery"),
      storage_in_taxonomy = taxonomy_has_token(sector_raw, sector4, "Storage"),
      rmi_sector = dplyr::case_when(
        name == "Composite materials" ~ "Mass Timber",
        battery_in_taxonomy & sector2 != "Mining" ~ "Batteries",
        stringr::str_detect(name, "Lithium") & sector2 != "Mining" ~ "Batteries",
        sector3 == "Biofuels" ~ "Biofuels",
        stringr::str_detect(name, "Biomass") ~ "Biomass",
        sector3 %in% c("CO2 capture", "CO2 transport", "CO2 storage") ~ "Carbon Capture",
        stringr::str_detect(sector4, regex("recycling", ignore_case = TRUE)) ~ "Circular Economy",
        stringr::str_detect(sector4, regex("End-of-life", ignore_case = TRUE)) ~ "Circular Economy",
        sector3 == "Road transport" &
          !battery_in_taxonomy &
          !stringr::str_detect(name, regex("Hydrogen", ignore_case = TRUE)) &
          !stringr::str_detect(name, "Lithium") ~ "Electric Vehicles",
        sector4 == "Appliance" ~ "Energy Efficient Appliances",
        sector3 == "Heating and cooling" ~ "Energy Efficient Heating/Cooling",
        sector3 == "Lighting" ~ "Energy Efficient Lighting",
        storage_in_taxonomy & sector1 == "Supply" & !battery_in_taxonomy ~ "Energy Storage",
        sector3 %in% c("Iron and steel", "Aluminium") ~ "Energy Transition Metals",
        name == "Closed-loop and hybrid deep geothermal systems" ~ "Geothermal",
        stringr::str_detect(name, regex("hydrogen", ignore_case = TRUE)) ~ "Green Hydrogen",
        name %in% c("Pumped storage", "Hydropower") ~ "Hydroelectric",
        stringr::str_detect(name, regex("reactor", ignore_case = TRUE)) ~ "Nuclear",
        stringr::str_detect(name, "Nuclear") ~ "Nuclear",
        stringr::str_detect(name, regex("solar", ignore_case = TRUE)) ~ "Solar",
        stringr::str_detect(name, "PV") ~ "Solar",
        stringr::str_detect(name, "Thin film") ~ "Solar",
        sector4 == "Infrastructure" ~ "Transmission & Distribution",
        stringr::str_detect(name, regex("wave", ignore_case = TRUE)) ~ "Wave Energy",
        stringr::str_detect(name, regex("wind", ignore_case = TRUE)) ~ "Wind Energy",
        stringr::str_detect(name, regex("inverter", ignore_case = TRUE)) ~ "Inverters",
        sector3 == "Critical minerals" ~ "Critical minerals",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(rmi_sector))
}

recode_rmi_sector_to_tech <- function(df,
                                      sector_col = "rmi_sector",
                                      out_col = "tech") {
  if (is.null(df) || !sector_col %in% names(df)) {
    return(df)
  }

  sector <- df[[sector_col]]
  cleaned <- stringr::str_squish(as.character(sector))

  tech <- dplyr::case_when(
    cleaned %in% c("Solar") ~ "Solar",
    cleaned %in% c("Wind Energy") ~ "Wind",
    cleaned %in% c("Nuclear") ~ "Nuclear",
    cleaned %in% c("Batteries") ~ "Batteries",
    cleaned %in% c("Electric Vehicles") ~ "Electric Vehicles",
    cleaned %in% c("Green Hydrogen") ~ "Green Hydrogen",
    cleaned %in% c("Geothermal") ~ "Geothermal",
    cleaned %in% c("Transmission & Distribution") ~ "Electric Grid",
    cleaned %in% c("Energy Storage") ~ "Batteries",
    cleaned %in% c("Hydroelectric") ~ "Hydro",
    cleaned %in% c("Inverters") ~ "Electric Grid",
    cleaned %in% c("Energy Transition Metals", "Critical minerals") ~ "Critical Minerals",
    cleaned %in% c(
      "Energy Efficient Appliances",
      "Energy Efficient Lighting",
      "Energy Efficient Heating/Cooling"
    ) ~ "Energy Efficiency",
    cleaned %in% c("Circular Economy") ~ "Circular Economy",
    cleaned %in% c("Mass Timber") ~ "Mass Timber",
    cleaned %in% c("Carbon Capture") ~ "Carbon Capture",
    cleaned %in% c("Biofuels") ~ "Biofuels",
    cleaned %in% c("Biomass") ~ "Biomass",
    cleaned %in% c("Wave Energy") ~ "Marine Energy",
    TRUE ~ NA_character_
  )

  df[[out_col]] <- tech
  df
}

technological_readiness_build_sector <- function(iea_cleantech) {
  technological_readiness_assign_sector(iea_cleantech) %>%
    dplyr::group_by(rmi_sector) %>%
    dplyr::summarize(trl2023 = mean(trl2023, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(trl2023))
}

technological_readiness_build_tech <- function(iea_cleantech_sector,
                                                techs = c(
                                                 "Electric Vehicles",
                                                 "Nuclear",
                                                 "Coal",
                                                 "Batteries",
                                                 "Green Hydrogen",
                                                 "Wind",
                                                 "Oil",
                                                 "Solar",
                                                 "Gas",
                                                 "Geothermal",
                                                 "Electric Grid"
                                               ),
                                               min_trl = 2,
                                               mu = 6,
                                               max_trl = 11,
                                               gamma = 0.5) {
  technological_readiness_assign_sector(iea_cleantech_sector) %>%
    recode_rmi_sector_to_tech() %>%
    dplyr::filter(.data$tech %in% techs) %>%
    dplyr::group_by(.data$tech) %>%
    dplyr::summarize(
      trl2023 = {
        values <- trl2023[!is.na(trl2023)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      trl_index = {
        scores <- trl_bell_soft(trl2023, min_trl = min_trl, mu = mu, max_trl = max_trl, gamma = gamma)
        scores <- scores[!is.na(scores)]
        if (length(scores) == 0) NA_real_ else mean(scores)
      },
      n_items = sum(!is.na(trl2023)),
      .groups = "drop"
    ) %>%
    dplyr::right_join(tibble::tibble(tech = techs), by = "tech") %>%
    dplyr::arrange(dplyr::desc(trl2023))
}

trl_bell_hard <- function(x, min_trl = 2, mu = 6, max_trl = 11) {
  left_w  <- mu - min_trl
  right_w <- max_trl - mu
  
  y <- dplyr::case_when(
    is.na(x) ~ NA_real_,
    x <= min_trl | x >= max_trl ~ 0,
    x <= mu ~ cos((pi/2) * (mu - x) / left_w),
    TRUE    ~ cos((pi/2) * (x - mu) / right_w)
  )
  
  # Numerical safety
  pmax(0, pmin(1, y))
}

trl_bell_soft <- function(x, min_trl = 2, mu = 6, max_trl = 11, gamma = 0.5) {
  y <- trl_bell_hard(x, min_trl = min_trl, mu = mu, max_trl = max_trl)
  dplyr::if_else(is.na(y), NA_real_, pmax(0, pmin(1, y^gamma)))
}

technological_readiness_build_indices <- function(iea_tech,
                                                  min_trl = 2,
                                                  mu = 6,
                                                  max_trl = 11,
                                                  gamma = 0.5,
                                                  year = 2023L) {
  supply_chain_levels <- c("Upstream", "Midstream", "Downstream")

  iea_tech %>%
    dplyr::mutate(
      trl_index = dplyr::if_else(
        is.na(trl_index),
        trl_bell_soft(trl2023, min_trl = min_trl, mu = mu, max_trl = max_trl, gamma = gamma),
        trl_index
      )
      ) %>%
    tidyr::crossing(supply_chain = supply_chain_levels) %>%
    tidyr::pivot_longer(
      cols = c(trl2023, trl_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::case_when(
        variable == "trl2023" ~ "raw",
        variable == "trl_index" ~ "index",
        TRUE ~ "raw"
      ),
      variable = dplyr::case_when(
        variable == "trl2023" ~ "TRL 2023",
        variable == "trl_index" ~ "TRL Index",
        TRUE ~ variable
      )
    ) %>%
    dplyr::transmute(
      tech,
      supply_chain,
      category = "Technological Readiness",
      variable,
      data_type,
      value,
      Year = as.integer(year),
      source = "IEA Clean Tech Guide",
      explanation = dplyr::case_when(
        variable == "TRL 2023" ~ "Mean technology readiness level from IEA Clean Tech Guide items mapped to each technology",
        variable == "TRL Index" ~ "Mean bell-curve score across item-level TRLs mapped to each technology",
        TRUE ~ NA_character_
      )
    )
}

technological_readiness <- function(iea_cleantech_all,
                                    techs = c(
                                      "Electric Vehicles",
                                      "Nuclear",
                                      "Coal",
                                      "Batteries",
                                      "Green Hydrogen",
                                      "Wind",
                                      "Oil",
                                      "Solar",
                                      "Gas",
                                      "Geothermal",
                                      "Electric Grid"
                                    ),
                                    min_trl = 2,
                                    mu = 6,
                                    max_trl = 11,
                                    gamma = 0.5,
                                    year = 2023L) {
  iea_cleantech <- technological_readiness_clean(iea_cleantech_all)
  iea_tech <- technological_readiness_build_tech(
    iea_cleantech,
    techs = techs,
    min_trl = min_trl,
    mu = mu,
    max_trl = max_trl,
    gamma = gamma
  )
  readiness_tbl <- technological_readiness_build_indices(
    iea_tech,
    min_trl = min_trl,
    mu = mu,
    max_trl = max_trl,
    gamma = gamma,
    year = year
  )
  readiness_tbl %>%
    dplyr::mutate(
      Country = "Global",
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      value = suppressWarnings(as.numeric(value)),
      Year = as.integer(year),
      source = as.character(source),
      explanation = as.character(explanation)
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )
}
