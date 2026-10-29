# CM-FDI, the Cleantech Manufacturing FDI Dataset (P. Bandara and the Net Zero Industrial Policy Lab,
# Johns Hopkins University; https://cmfdi.netzeropolicylab.com; CC BY 4.0): the sector-fdi chunk of
# Sector Insights (contract.ts FDI_SPEC). Pure: the CSV arrives as a data frame of character columns
# (scripts/87 reads data/raw/cm_fdi_projects.csv with every column as text, so the cleaning below is
# the only transformation between the file and the chunk), and the settings arrive as a list
# (scripts/87 reads the `cm_fdi` block of config/partnership.yml, the Partnership indices' settings,
# so the descriptive views and the indices describe the same projects).
#
# Cleaning (sector_fdi_clean(); the parity check in scripts/87 re-applies it to the raw CSV):
#   * text: surrounding white space trimmed; an empty optional text is NA. Nothing else is changed
#     (parent names, cities and source cells ship verbatim).
#   * Company Country (HQ) and Destination Country -> ISO3 with the pipeline's country_name_to_iso3()
#     ("Czechia" and "Czech Republic" -> CZE, "UAE" -> ARE, "Hong Kong" -> HKG, "Taiwan" -> TWN).
#   * Technology -> the cell of settings `technologies` x the first of `stages` (config/partnership.yml
#     cm_fdi stages: Components and Assembly, the two manufacturing stages; Battery ->
#     batteries-components, EV -> electric-vehicles-components, Solar, Wind); the CSV's own label is kept
#     in `technology`. The file has no product field, so a project cannot be placed in one stage: the
#     pipeline counts each project, whole, in every configured stage. The chunk ships each project once
#     (pid unique; counts and sums never doubled) under the first stage's cell and lists the stages in
#     fdi.stages: the other stages' cells carry the same projects (user decision E5, 2026-10-28).
#   * Capital Investment (US$m): a number as published; blank = not disclosed -> value NA with status
#     nq, never 0 (a published 0 stops the export: undisclosed or true zero must be decided first).
#   * Year Completed: a four-digit year, else NA ("N/A" or blank). For projects not yet complete it is
#     the planned year.
#   * 0 / 1 flags -> logical (Joint Venture Local State blank -> NA).
#   * Unknown vocabularies (technology, status, operational state, activity) stop the export: a new
#     CM-FDI edition must be reviewed before it ships.
# Project flags (from the settings; the rules of the pipeline's cm_fdi_flag_index_use()):
#   manufacturing  Industry activity in `manufacturing_activities` (Manufacturing: CM-FDI's 974);
#   active         Project Status in `keep_status` (Cancelled, Paused and Closed dropped, as the CM-FDI
#                  working paper does) and Operational not in `drop_operational` (plants that have
#                  since ceased operations or paused);
#   index_use      manufacturing, active and announced in or after `announced_from` (null = every
#                  year): the projects the Partnership indices use.
#   home_group     the parent's HQ group: china when the HQ is in `china_hq_iso3` (CHN, HKG, MAC: as
#                  the indices' Chinese-parent exposure counts them), else the dims.geo group (core,
#                  partner, rest).
# Source cells ship verbatim in fdi.sources with the http(s) links found in them (`url`, pipe-joined;
# only http:// and https:// links, characters unsafe in an attribute or a pipe list percent-encoded)
# and CM-FDI's "[UNVERIFIED]" mark as a flag.
#
# Aggregates (sector_fdi_tables()), each for two bases: `all` (every project: all statuses and
# activities) and `index_use` (the flag above). Raw counts and disclosed sums: a joint venture counts
# wholly under its listed parent (the indices attribute a share of it; see config/partnership.yml).
# Value sums cover disclosed values only, in project-table order; a sum with no disclosed value is NA
# (n_value = 0), never 0.

SECTOR_FDI_COLUMNS <- c(
  "Sector", "Project ID", "Parent company", "Company Country (HQ)", "Project Status", "Destination Country",
  "Destination city", "Year Announced", "Year Completed", "Technology", "Industry activity",
  "Capital Investment (US$m)", "Expansion", "Phase", "Joint Venture", "Joint Venture Local",
  "Joint Venture Local State", "Joint Venture Company", "Source (Parent company name and HQ)",
  "Source (Project Status)", "Source (Project location, country and city)", "Source (Amount)",
  "Source (Ownership share, where available)", "Source (Date Announced [MM/DD/YYYY])",
  "Source (Date Completed [MM/DD/YYYY])", "Source (Industry Activity)", "Source (Joint Venture)", "Operational"
)
# fdi.sources `field` ids -> the CSV's source columns (contract FDI_SOURCE_FIELDS, same order).
SECTOR_FDI_SOURCE_FIELDS <- c(
  parent = "Source (Parent company name and HQ)", status = "Source (Project Status)",
  location = "Source (Project location, country and city)", amount = "Source (Amount)",
  ownership = "Source (Ownership share, where available)", announced = "Source (Date Announced [MM/DD/YYYY])",
  completed = "Source (Date Completed [MM/DD/YYYY])", activity = "Source (Industry Activity)",
  jv = "Source (Joint Venture)"
)
# The reviewed vocabularies of this edition (contract FDI_PROJECT_STATUSES, ...).
SECTOR_FDI_STATUSES <- c("Announced", "Under Construction", "Completed", "Operational", "Paused", "Cancelled", "Closed")
SECTOR_FDI_OPERATIONAL <- c("Operational", "Announced only", "Under construction", "Ceased operations", "Paused")
SECTOR_FDI_ACTIVITIES <- c("Manufacturing", "R&D & Manufacturing", "Research & Development",
                           "Design, Development & Testing", "Logistics, Distribution & Transportation")
SECTOR_FDI_BASES <- c("all", "index_use")
SECTOR_FDI_GROUPS <- c("china", "core", "partner", "rest")
SECTOR_FDI_VARS <- c("investment.fdi_projects", "investment.fdi_value", "investment.fdi_parent_share")

# The settings the export uses when config/partnership.yml has no cm_fdi block (the pipeline's
# defaults, cm_fdi_default_settings()).
sector_fdi_default_settings <- function() {
  list(
    technologies = c(Battery = "Batteries", Solar = "Solar", Wind = "Wind", EV = "Electric Vehicles"),
    stages = SUPPLY_CHAIN_MANUFACTURING_STAGES,
    keep_status = c("Announced", "Under Construction", "Completed", "Operational"),
    drop_operational = c("Ceased operations", "Paused"),
    manufacturing_activities = "Manufacturing",
    announced_from = NA_integer_,
    china_hq_iso3 = c("CHN", "HKG", "MAC")
  )
}

# The cm_fdi block of config/partnership.yml (a parsed list, or NULL) merged over the defaults; the
# keys the indices alone use (jv_parent_share, exposure_weighting, min_projects, ...) are ignored. The
# old single `stage` key (replaced on 2026-10-27 by `stages`) stops, as the pipeline's reader does.
sector_fdi_settings <- function(cfg = NULL) {
  s <- sector_fdi_default_settings()
  if (!is.null(cfg)) {
    if ("stage" %in% names(cfg)) stop("sector_fdi_settings(): cm_fdi stage was replaced by stages, a list of supply-chain stages (config/partnership.yml cm_fdi: stages: [Components, Assembly])")
    for (k in names(s)) if (k %in% names(cfg) && !is.null(cfg[[k]])) s[[k]] <- cfg[[k]]
    if ("announced_from" %in% names(cfg) && is.null(cfg$announced_from)) s$announced_from <- NA_integer_
  }
  tech <- unlist(s$technologies)
  if (!is.character(tech) || is.null(names(tech)) || anyNA(tech) || !all(tech %in% SECTOR_TECH_LABELS)) {
    stop("sector_fdi_settings(): technologies must map CM-FDI Technology values to tech labels (", paste(SECTOR_TECH_LABELS, collapse = ", "), ")")
  }
  s$technologies <- tech
  s$stages <- as.character(unlist(s$stages))
  if (!length(s$stages) || anyNA(s$stages) || anyDuplicated(s$stages) || !all(s$stages %in% SUPPLY_CHAIN_STAGES)) {
    stop("sector_fdi_settings(): stages must list distinct supply-chain stages (", paste(SUPPLY_CHAIN_STAGES, collapse = ", "), ")")
  }
  for (k in c("keep_status", "drop_operational", "manufacturing_activities", "china_hq_iso3")) s[[k]] <- as.character(unlist(s[[k]]))
  s$announced_from <- suppressWarnings(as.integer(s$announced_from))
  if (length(s$announced_from) != 1) stop("sector_fdi_settings(): announced_from must be one year or null")
  s
}

# The http(s) links in source cells: one character vector per cell. A link runs to the next white
# space; a trailing comma or semicolon (CM-FDI separates links with ", ") is dropped; characters that
# are unsafe in an attribute or a pipe-joined list are percent-encoded.
sector_fdi_urls <- function(text) {
  text[is.na(text)] <- ""
  found <- regmatches(text, gregexpr("https?://[^[:space:]]+", text))
  lapply(found, function(u) {
    if (!length(u)) return(character())
    u <- sub("[,;]+$", "", u)
    enc <- c("|" = "%7C", "\"" = "%22", "<" = "%3C", ">" = "%3E", "\\" = "%5C", "`" = "%60")
    for (k in names(enc)) u <- gsub(k, enc[[k]], u, fixed = TRUE)
    u[grepl("^https?://[^[:space:]]+$", u) & nchar(u) > nchar("https://")]
  })
}

# The project flags of the header from the settings (manufacturing, active, index_use).
sector_fdi_flags <- function(projects, settings) {
  s <- settings
  projects$manufacturing <- projects$activity %in% s$manufacturing_activities
  projects$active <- projects$status %in% s$keep_status & !projects$operational %in% s$drop_operational
  projects$index_use <- projects$manufacturing & projects$active &
    (is.na(s$announced_from) | projects$year_announced >= s$announced_from)
  projects
}

# One clean project table and the long source table from the CSV as read (character columns).
# country_iso3: function(names) -> ISO3 (NA when unknown); settings: sector_fdi_settings(). Returns
# list(projects, sources), keyed by text: item_key ("cell:<tech>-<stage>"), home / dest ISO3. Stops on
# anything not reviewed.
sector_fdi_clean <- function(raw, country_iso3, settings = sector_fdi_settings()) {
  miss <- setdiff(SECTOR_FDI_COLUMNS, names(raw))
  if (length(miss)) stop("CM-FDI: columns missing from the CSV: ", paste(miss, collapse = ", "))
  col <- function(nm) {
    v <- trimws(as.character(raw[[nm]]))
    v[is.na(v)] <- ""
    v
  }
  opt <- function(nm) {
    v <- col(nm)
    v[v == ""] <- NA_character_
    v
  }
  stop_rows <- function(what, bad, values) {
    if (any(bad)) stop("CM-FDI: ", what, " (", sum(bad), " rows, e.g. ", paste(utils::head(unique(values[bad]), 5), collapse = "; "), ")")
  }
  ctl <- vapply(SECTOR_FDI_COLUMNS, function(nm) any(grepl("[[:cntrl:]]", col(nm))), logical(1))
  if (any(ctl)) stop("CM-FDI: control characters in ", paste(SECTOR_FDI_COLUMNS[ctl], collapse = ", "))

  pid <- col("Project ID")
  stop_rows("rows without a Project ID", pid == "", pid)
  stop_rows("duplicate Project IDs", duplicated(pid), pid)
  tech <- col("Technology")
  stop_rows("unknown Technology (config/partnership.yml cm_fdi technologies)", !tech %in% names(settings$technologies), tech)
  sector <- col("Sector")
  stop_rows("Sector differs from Technology", sector != tech, paste(sector, tech))
  parent <- col("Parent company")
  stop_rows("rows without a Parent company", parent == "", pid)
  hq_name <- col("Company Country (HQ)")
  dest_name <- col("Destination Country")
  home <- country_iso3(hq_name)
  dest <- country_iso3(dest_name)
  stop_rows("HQ country without an ISO3 code", is.na(home), hq_name)
  stop_rows("destination country without an ISO3 code", is.na(dest), dest_name)
  stop_rows("a project inside its parent's HQ economy (not cross-border)", home == dest, pid)
  status <- col("Project Status")
  stop_rows("unknown Project Status", !status %in% SECTOR_FDI_STATUSES, status)
  operational <- col("Operational")
  stop_rows("unknown Operational state", !operational %in% SECTOR_FDI_OPERATIONAL, operational)
  activity <- col("Industry activity")
  stop_rows("unknown Industry activity", !activity %in% SECTOR_FDI_ACTIVITIES, activity)
  ya <- col("Year Announced")
  stop_rows("Year Announced is not a year", !grepl("^[0-9]{4}$", ya), ya)
  yc <- col("Year Completed")
  stop_rows("Year Completed is neither a year nor N/A", !(grepl("^[0-9]{4}$", yc) | yc %in% c("", "N/A")), yc)
  cap <- col("Capital Investment (US$m)")
  value <- suppressWarnings(as.double(ifelse(cap == "", NA_character_, cap)))
  stop_rows("Capital Investment is not a number", cap != "" & !is.finite(value), cap)
  stop_rows("Capital Investment is negative", !is.na(value) & value < 0, cap)
  stop_rows("Capital Investment is 0 (undisclosed or a true zero? review the edition)", !is.na(value) & value == 0, pid)
  flag <- function(nm, optional = FALSE) {
    v <- col(nm)
    ok <- v %in% c("0", "1") | (optional & v == "")
    stop_rows(paste(nm, "is not 0 / 1"), !ok, v)
    out <- v == "1"
    out[v == ""] <- NA
    out
  }
  jv <- flag("Joint Venture")
  jv_local <- flag("Joint Venture Local")
  jv_local_state <- flag("Joint Venture Local State", optional = TRUE)
  expansion <- flag("Expansion")
  stop_rows("Joint Venture Local without Joint Venture", jv_local & !jv, pid)

  tech_slug <- names(SECTOR_TECH_LABELS)[match(unname(settings$technologies[tech]), SECTOR_TECH_LABELS)]
  projects <- tibble::tibble(
    pid = pid,
    item_key = paste0("cell:", tech_slug, "-", tolower(settings$stages[1])),
    technology = tech,
    parent = parent,
    home = home,
    dest = dest,
    city = opt("Destination city"),
    status = status,
    operational = operational,
    activity = activity,
    year_announced = as.integer(ya),
    year_completed = as.integer(ifelse(grepl("^[0-9]{4}$", yc), yc, NA_character_)),
    value = value,
    value_status = ifelse(is.na(value), "nq", "obs"),
    expansion = expansion,
    phase = opt("Phase"),
    jv = jv,
    jv_local = jv_local,
    jv_local_state = jv_local_state,
    jv_company = opt("Joint Venture Company")
  )
  projects <- sector_fdi_flags(projects, settings)
  projects <- projects[order(projects$item_key, projects$year_announced, projects$pid, method = "radix"), , drop = FALSE]

  src <- dplyr::bind_rows(lapply(names(SECTOR_FDI_SOURCE_FIELDS), function(f) {
    tibble::tibble(pid = pid, field = f, text = opt(SECTOR_FDI_SOURCE_FIELDS[[f]]))
  }))
  src <- src[!is.na(src$text), , drop = FALSE]
  urls <- sector_fdi_urls(src$text)
  src$url <- vapply(urls, function(u) if (length(u)) paste(u, collapse = "|") else NA_character_, character(1))
  src$unverified <- grepl("[UNVERIFIED]", src$text, fixed = TRUE)
  src <- src[order(match(src$pid, projects$pid), match(src$field, names(SECTOR_FDI_SOURCE_FIELDS)), method = "radix"), , drop = FALSE]
  list(projects = projects, sources = src)
}

# The parent's HQ group of each project: china for an HQ in china_hq, else the dims.geo group.
sector_fdi_home_group <- function(home, geo, china_hq) {
  g <- geo$group[match(home, geo$code)]
  g[home %in% china_hq] <- "china"
  bad <- is.na(g) | !g %in% SECTOR_FDI_GROUPS
  if (any(bad)) stop("CM-FDI: HQ economies without a china / core / partner / rest group: ", paste(unique(home[bad]), collapse = ", "))
  g
}

# Sum of disclosed values in the given order; NA when none is disclosed.
sector_fdi_sum <- function(v) {
  v <- v[!is.na(v)]
  if (length(v)) sum(v) else NA_real_
}

# The aggregates of the chunk from the projects (text keys, with home_group and index_use) and
# dims.geo (code: the row order of economies). Returns list(flows, groups, years) keyed by text.
sector_fdi_tables <- function(projects, geo) {
  if (any(!projects$dest %in% geo$code) || any(!projects$home %in% geo$code)) stop("sector_fdi_tables(): economies not in dims.geo")
  p <- projects
  flows <- list()
  groups <- list()
  years <- list()
  for (b in SECTOR_FDI_BASES) {
    s <- if (b == "all") p else p[p$index_use, , drop = FALSE]
    if (!nrow(s)) next
    k <- paste(s$item_key, s$home, s$dest, sep = "\r")
    ix <- split(seq_len(nrow(s)), factor(k, levels = unique(k)))
    flows[[b]] <- dplyr::bind_rows(lapply(ix, function(i) tibble::tibble(
      item_key = s$item_key[i[1]], home = s$home[i[1]], dest = s$dest[i[1]], basis = b, n = length(i),
      n_value = sum(!is.na(s$value[i])), value = sector_fdi_sum(s$value[i]),
      year_min = min(s$year_announced[i]), year_max = max(s$year_announced[i])
    )))
    # Parent-HQ group split per destination x technology, and over every destination (WLD).
    out <- list()
    for (ck in unique(s$item_key)) {
      sc <- s[s$item_key == ck, , drop = FALSE]
      for (d in c(unique(sc$dest), "WLD")) {
        sd <- if (d == "WLD") sc else sc[sc$dest == d, , drop = FALSE]
        tot_value <- sector_fdi_sum(sd$value)
        out[[length(out) + 1]] <- dplyr::bind_rows(lapply(SECTOR_FDI_GROUPS, function(g) {
          sg <- sd[sd$home_group == g, , drop = FALSE]
          value <- if (!nrow(sg)) 0 else sector_fdi_sum(sg$value)
          tibble::tibble(item_key = ck, dest = d, basis = b, group = g, n = nrow(sg), n_value = sum(!is.na(sg$value)), value = value,
                         share_n = nrow(sg) / nrow(sd),
                         share_value = if (is.na(tot_value) || is.na(value)) NA_real_ else value / tot_value)
        }))
      }
    }
    groups[[b]] <- dplyr::bind_rows(out)
    ky <- paste(s$item_key, s$year_announced, s$home_group, sep = "\r")
    iy <- split(seq_len(nrow(s)), factor(ky, levels = unique(ky)))
    years[[b]] <- dplyr::bind_rows(lapply(iy, function(i) tibble::tibble(
      item_key = s$item_key[i[1]], year = s$year_announced[i[1]], basis = b, group = s$home_group[i[1]], n = length(i),
      n_value = sum(!is.na(s$value[i])), value = sector_fdi_sum(s$value[i])
    )))
  }
  # Rows in the chunk's order: items (cell slugs sort as dims.items does), economies by dims.geo row
  # (WLD after the countries), then basis and group in vocabulary order.
  tidy <- function(x, keys) {
    x <- dplyr::bind_rows(x)
    x$n <- as.integer(x$n)
    x$n_value <- as.integer(x$n_value)
    ord <- lapply(keys, function(k) {
      switch(k, home = , dest = match(x[[k]], geo$code), basis = match(x[[k]], SECTOR_FDI_BASES),
             group = match(x[[k]], SECTOR_FDI_GROUPS), x[[k]])
    })
    x[do.call(order, c(ord, list(method = "radix"))), , drop = FALSE]
  }
  list(
    flows = tidy(flows, c("item_key", "home", "dest", "basis")),
    groups = tidy(groups, c("item_key", "dest", "basis", "group")),
    years = tidy(years, c("item_key", "year", "basis", "group"))
  )
}

# The full fdi part of the bundle: list(projects (+ home_group), sources, flows, groups, years,
# stages: the configured stages, the first being the cell every row is keyed on).
sector_fdi_part <- function(clean, geo, settings = sector_fdi_settings()) {
  p <- clean$projects
  p$home_group <- sector_fdi_home_group(p$home, geo, settings$china_hq_iso3)
  c(list(projects = p, sources = clean$sources), sector_fdi_tables(p, geo), list(stages = settings$stages))
}

# The cell of a project's item at another configured stage ("cell:batteries-components" ->
# "cell:batteries-assembly").
sector_fdi_stage_item <- function(item_key, from, to) {
  sub(paste0("-", tolower(from), "$"), paste0("-", tolower(to)), item_key)
}

# Data keys of the chunk (sector_data_keys(); avail, rosters, section cards, coverage): every project
# under investment.fdi_projects and investment.fdi_parent_share, the disclosed ones under
# investment.fdi_value; year = year announced, geo = the destination; on the cell of every configured
# stage (fdi$stages), which all carry the same projects, so each stage's sector shows the measures.
sector_fdi_data_keys <- function(fdi, kind_of) {
  p <- fdi$projects
  if (is.null(p) || !nrow(p)) return(NULL)
  stages <- if (is.null(fdi$stages)) character() else fdi$stages
  items <- if (length(stages)) lapply(stages, function(s) sector_fdi_stage_item(p$item_key, stages[1], s)) else list(p$item_key)
  one <- function(var, s, ik) tibble::tibble(var = var, item_key = ik[s], chunk = "fdi", year = p$year_announced[s],
                                             geo = p$dest[s], geo_kind = kind_of(p$dest[s]))
  all <- rep(TRUE, nrow(p))
  dplyr::bind_rows(lapply(items, function(ik) dplyr::bind_rows(one("investment.fdi_projects", all, ik), one("investment.fdi_value", !is.na(p$value), ik),
                                                               one("investment.fdi_parent_share", all, ik))))
}

# Dictionary placeholders filled from the projects (sources, caveats, data years).
sector_fdi_fill <- function(fdi, settings = sector_fdi_settings()) {
  p <- fdi$projects
  s <- fdi$sources
  amount <- s[s$field == "amount", , drop = FALSE]
  n_fmt <- function(x) format(x, big.mark = ",")
  list(
    fdi_n = n_fmt(nrow(p)),
    fdi_n_undisclosed = n_fmt(sum(is.na(p$value))),
    fdi_n_mfg = n_fmt(sum(p$manufacturing)),
    fdi_n_index_use = n_fmt(sum(p$index_use)),
    fdi_n_dest = length(unique(p$dest)),
    fdi_n_home = length(unique(p$home)),
    fdi_n_parents = length(unique(p$parent)),
    fdi_n_jv = sum(p$jv),
    fdi_n_ceased = sum(p$manufacturing & p$status %in% settings$keep_status & p$operational %in% settings$drop_operational),
    fdi_n_fdimarkets = sum(!is.na(p$value) & p$pid %in% amount$pid[grepl("fDi Markets", amount$text, fixed = TRUE)]),
    fdi_n_unverified_amount = sum(amount$unverified & amount$pid %in% p$pid[!is.na(p$value)]),
    fdi_years = paste0(min(p$year_announced), "-", max(p$year_announced)),
    fdi_window = if (is.na(settings$announced_from)) "every announcement year" else paste0("announced ", settings$announced_from, " or later"),
    fdi_china_hq = paste(settings$china_hq_iso3, collapse = ", ")
  )
}

# Placeholder values when the chunk is not built (--no-fdi): the dictionary still holds the texts.
sector_fdi_fill_none <- function() {
  keys <- c("fdi_n", "fdi_n_undisclosed", "fdi_n_mfg", "fdi_n_index_use", "fdi_n_dest", "fdi_n_home", "fdi_n_parents",
            "fdi_n_jv", "fdi_n_ceased", "fdi_n_fdimarkets", "fdi_n_unverified_amount", "fdi_years", "fdi_window", "fdi_china_hq")
  stats::setNames(as.list(rep("n/a", length(keys))), keys)
}

# Consistency of the part (text keys): flags and groups by the settings, aggregates recomputed from
# the projects, sources tied to projects, value states, clean links. Returns problems (empty when
# consistent).
sector_fdi_problems <- function(fdi, geo, settings = sector_fdi_settings()) {
  p <- character()
  pr <- fdi$projects
  if (!identical(as.character(fdi$stages), settings$stages)) p <- c(p, "stages differ from the settings")
  if (!all(endsWith(pr$item_key, paste0("-", tolower(settings$stages[1]))))) p <- c(p, "projects not keyed on the first configured stage's cell")
  if (anyDuplicated(pr$pid)) p <- c(p, "duplicate project ids")
  if (any(is.na(pr$value) != (pr$value_status == "nq"))) p <- c(p, "value NA <=> value_status nq is violated")
  if (any(!is.na(pr$value) & pr$value <= 0)) p <- c(p, "a disclosed value is not positive")
  fl <- sector_fdi_flags(pr, settings)
  for (k in c("manufacturing", "active", "index_use")) if (!identical(fl[[k]], pr[[k]])) p <- c(p, paste("project flag", k, "differs from the settings"))
  if (!identical(pr$home_group, sector_fdi_home_group(pr$home, geo, settings$china_hq_iso3))) p <- c(p, "home_group differs from the settings")
  if (!all(fdi$sources$pid %in% pr$pid)) p <- c(p, "sources for unknown projects")
  u <- unlist(strsplit(stats::na.omit(fdi$sources$url), "|", fixed = TRUE))
  if (length(u) && !all(grepl("^https?://[^[:space:]\"<>|`\\\\]+$", u))) p <- c(p, "a source link is not a clean http(s) URL")
  again <- sector_fdi_tables(pr, geo)
  for (nm in c("flows", "groups", "years")) if (!identical(again[[nm]], fdi[[nm]])) p <- c(p, paste0(nm, " differ from the projects"))
  for (b in SECTOR_FDI_BASES) {
    s <- if (b == "all") pr else pr[pr$index_use, , drop = FALSE]
    f <- fdi$flows[fdi$flows$basis == b, , drop = FALSE]
    if (sum(f$n) != nrow(s)) p <- c(p, paste("flows", b, "do not count every project once"))
    y <- fdi$years[fdi$years$basis == b, , drop = FALSE]
    if (sum(y$n) != nrow(s)) p <- c(p, paste("years", b, "do not count every project once"))
    g <- fdi$groups[fdi$groups$basis == b, , drop = FALSE]
    tot <- tapply(g$share_n, paste(g$item_key, g$dest), sum)
    if (length(tot) && max(abs(tot - 1)) > 1e-12) p <- c(p, paste("group shares of", b, "do not sum to 1"))
    w <- g[g$dest == "WLD", , drop = FALSE]
    if (sum(w$n) != nrow(s)) p <- c(p, paste("WLD group rows of", b, "do not count every project once"))
  }
  p
}

# Parity of the decoded chunk with the CSV (8.9 for an X source): every exported project field is
# identical() to the same field of the CSV with the documented cleaning applied independently here,
# the flags and groups follow the settings, and the decoded aggregates equal those recomputed from
# the decoded projects. decoded: the decoded sector-fdi objects; meta: the decoded sector-meta
# objects; raw: the CSV as read (character columns); country_iso3 and settings: as for
# sector_fdi_clean(). Returns tibble(check, status, n, detail).
sector_fdi_parity <- function(decoded, meta, raw, country_iso3, settings = sector_fdi_settings()) {
  geo <- meta$dims$geo
  it <- meta$dims$items
  ik <- paste0(it$axis, ":", it$slug)
  d <- decoded$fdi$projects
  out <- list()
  add <- function(check, ok, n, detail = NA_character_) {
    out[[length(out) + 1]] <<- tibble::tibble(check = check, status = if (ok) "PASS" else "FAIL", n = as.integer(n), detail = detail)
  }
  r <- raw[match(d$pid, trimws(raw[["Project ID"]])), , drop = FALSE]
  add("CM-FDI parity: every decoded project is a CSV row and every CSV row is exported once",
      !anyNA(r[["Project ID"]]) && nrow(d) == nrow(raw) && !anyDuplicated(d$pid), nrow(d))
  tx <- function(nm) {
    v <- trimws(as.character(r[[nm]]))
    v[is.na(v) | v == ""] <- NA_character_
    v
  }
  fl <- function(nm) {
    v <- tx(nm)
    ifelse(is.na(v), NA, v == "1")
  }
  cap <- suppressWarnings(as.double(tx("Capital Investment (US$m)")))
  yc <- tx("Year Completed")
  tech_label <- unname(settings$technologies[tx("Technology")])
  cells <- paste0("cell:", names(SECTOR_TECH_LABELS)[match(tech_label, SECTOR_TECH_LABELS)], "-", tolower(settings$stages[1]))
  cmp <- list(
    `Parent company` = identical(d$parent, tx("Parent company")),
    `Company Country (HQ)` = identical(geo$code[d$home + 1L], country_iso3(tx("Company Country (HQ)"))),
    `Destination Country` = identical(geo$code[d$dest + 1L], country_iso3(tx("Destination Country"))),
    `Destination city` = identical(d$city, tx("Destination city")),
    `Project Status` = identical(d$status, tx("Project Status")),
    Operational = identical(d$operational, tx("Operational")),
    `Industry activity` = identical(d$activity, tx("Industry activity")),
    Technology = identical(d$technology, tx("Technology")) && identical(ik[d$item + 1L], cells),
    `Year Announced` = identical(d$year_announced, as.integer(tx("Year Announced"))),
    `Year Completed` = identical(d$year_completed, as.integer(ifelse(!is.na(yc) & grepl("^[0-9]{4}$", yc), yc, NA))),
    `Capital Investment (US$m)` = identical(d$value, cap) && identical(d$value_status, ifelse(is.na(cap), "nq", "obs")),
    Expansion = identical(d$expansion, fl("Expansion")),
    Phase = identical(d$phase, tx("Phase")),
    `Joint Venture` = identical(d$jv, fl("Joint Venture")),
    `Joint Venture Local` = identical(d$jv_local, fl("Joint Venture Local")),
    `Joint Venture Local State` = identical(d$jv_local_state, fl("Joint Venture Local State")),
    `Joint Venture Company` = identical(d$jv_company, tx("Joint Venture Company"))
  )
  bad <- names(cmp)[!unlist(cmp)]
  add("CM-FDI parity: project fields identical() to the CSV after cleaning (17 fields)", !length(bad), nrow(d),
      if (length(bad)) paste("differ:", paste(bad, collapse = ", ")) else NA_character_)
  s <- decoded$fdi$sources
  key <- paste(d$pid[s$project + 1L], s$field)
  want <- dplyr::bind_rows(lapply(names(SECTOR_FDI_SOURCE_FIELDS), function(f) {
    v <- trimws(as.character(raw[[SECTOR_FDI_SOURCE_FIELDS[[f]]]]))
    tibble::tibble(key = paste(trimws(raw[["Project ID"]]), f), text = v)
  }))
  want <- want[!is.na(want$text) & want$text != "", , drop = FALSE]
  ok_src <- setequal(key, want$key) && identical(s$text, want$text[match(key, want$key)])
  add("CM-FDI parity: source cells identical() to the CSV (verbatim, trimmed)", ok_src, nrow(s))
  # Flags, groups and aggregates recomputed from the decoded projects.
  pt <- tibble::tibble(pid = d$pid, item_key = ik[d$item + 1L], home = geo$code[d$home + 1L], dest = geo$code[d$dest + 1L],
                       status = d$status, operational = d$operational, activity = d$activity, year_announced = d$year_announced,
                       value = d$value)
  pt <- sector_fdi_flags(pt, settings)
  pt$home_group <- sector_fdi_home_group(pt$home, geo, settings$china_hq_iso3)
  ok_flags <- identical(pt$manufacturing, d$manufacturing) && identical(pt$active, d$active) && identical(pt$index_use, d$index_use) &&
    identical(pt$home_group, d$home_group)
  add("CM-FDI parity: project flags and parent HQ groups follow the settings (config/partnership.yml cm_fdi)", ok_flags, nrow(d))
  add("CM-FDI parity: fdi.stages = the configured stages (config/partnership.yml cm_fdi stages); rows keyed on the first stage's cell",
      identical(as.character(decoded$fdi$stages), settings$stages), length(settings$stages),
      paste(as.character(decoded$fdi$stages), collapse = ", "))
  again <- sector_fdi_tables(pt, geo)
  dec_txt <- function(t, cols) {
    o <- tibble::as_tibble(t)
    if ("item" %in% names(o)) o$item_key <- ik[o$item + 1L]
    for (g in intersect(c("home", "dest"), names(o))) o[[g]] <- geo$code[o[[g]] + 1L]
    o[cols]
  }
  same_agg <- vapply(c("flows", "groups", "years"), function(nm) {
    a <- again[[nm]]
    identical(dec_txt(decoded$fdi[[nm]], names(a)), a[names(a)])
  }, logical(1))
  add("CM-FDI parity: flows, group splits and years recomputed from the decoded projects identical()", all(same_agg), 3L,
      if (!all(same_agg)) paste("differ:", paste(names(same_agg)[!same_agg], collapse = ", ")) else NA_character_)
  dplyr::bind_rows(out)
}
