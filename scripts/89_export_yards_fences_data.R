# Export the data of Draw the Fence, the standalone Yards & Fences decision tool (app:
# dashboard/fence/ + dashboard/src/fence/; contract: dashboard/src/fence/data/contract.ts).
#
#   Rscript scripts/89_export_yards_fences_data.R [--data-dir <dir>] [--out-dir <dir>]
#                                                 [--processed-dir <dir>] [--country USA[,JPN...]]
#                                                 [--check-only]
#
# IO only: every rule lives in R/dashboard/yards_fences/yf_build.R. Runs after scripts/85 and 87 and,
# like the Country export, reads their published bundles (so every number shown here is the number
# Sector and Strategic Insights show):
#
#   <data-dir>/manifest.json + core, vars, partner chunks          (scripts/85, Strategic Insights)
#   <data-dir>/sector/manifest.json + meta, facts, trade chunks    (scripts/87, Sector Insights)
#   <processed-dir>/outputs/allied_network_{member_shares,diagnostics,observed_hhi}.csv  (scripts/30)
#   config/yards_fences_metric_map.yml                             (which measures, labels, engine inputs)
#   data/reference/yards_fences_assessments.csv                    (curated component assessments)
#
# and writes, to <out-dir> (default dashboard/fence/public/data/yards-fences, the site's own data):
#
#   reference.<h8>.json        metric map, components, stacks, sources, HHI reference points
#   evidence-<ISO3>.<h8>.json  one country's evidence records, supply views, network and partners
#   manifest.json              vintage, inputs, validation, notes (written last; served no-cache)
#
# <data-dir> defaults to <repo>/dashboard/public/data; <processed-dir> to config$processed_dir.
# --check-only runs every guard and validation check and writes nothing. The build stops on any
# failed check. Restricted sources (BNEF, BCG) cannot enter: the Sector bundle is checked for them.

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))
    candidate_starts <- unique(c(candidate_starts, getwd()))
    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

args <- commandArgs(trailingOnly = TRUE)
arg_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "R", "dashboard", "dash_osb.R"))
source(file.path(repo_root, "R", "dashboard", "yards_fences", "yf_build.R"))

data_dir <- abs_path(arg_value("--data-dir", file.path("dashboard", "public", "data")))
out_dir <- abs_path(arg_value("--out-dir", file.path("dashboard", "fence", "public", "data", "yards-fences")))
processed_dir <- abs_path(arg_value("--processed-dir", config$processed_dir))
countries <- strsplit(arg_value("--country", "USA"), ",")[[1]]
check_only <- arg_flag("--check-only")

read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
read_osb <- function(p) {
  b <- readBin(p, "raw", file.info(p)$size)
  if (length(b) > 1 && b[1] == as.raw(0x1f) && b[2] == as.raw(0x8b)) b <- memDecompress(b, "gzip")
  dash_osb_decode(b)$objects
}
chunk_path <- function(dir, manifest, name) {
  hit <- Filter(function(c) identical(c$name, name), manifest$chunks)
  if (!length(hit)) stop("chunk '", name, "' not listed in ", dir, "/manifest.json")
  file.path(dir, hit[[1]]$file)
}

# ── Inputs ──────────────────────────────────────────────────────────────────────────────────────
step("reading the metric map and curated assessments")
map <- yaml::read_yaml(file.path(repo_root, "config", "yards_fences_metric_map.yml"))
ass <- utils::read.csv(file.path(repo_root, "data", "reference", "yards_fences_assessments.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(), encoding = "UTF-8")
problems <- yf_validate_assessments(ass)
if (length(problems)) stop("yards_fences_assessments.csv:\n  ", paste(problems, collapse = "\n  "))

step("reading the published Sector and Strategic bundles in ", data_dir)
sector_dir <- file.path(data_dir, "sector")
smf <- read_json(file.path(sector_dir, "manifest.json"))
gmf <- read_json(file.path(data_dir, "manifest.json"))
meta <- read_osb(chunk_path(sector_dir, smf, "sector-meta"))
facts <- read_osb(chunk_path(sector_dir, smf, "sector-facts"))$facts
trade_names <- vapply(Filter(function(c) grepl("^sector-trade-", c$name), smf$chunks), function(c) c$name, character(1))
trade <- lapply(trade_names, function(n) read_osb(chunk_path(sector_dir, smf, n)))
core <- read_osb(chunk_path(data_dir, gmf, "core"))
vars <- read_osb(chunk_path(data_dir, gmf, "vars"))
partner <- read_osb(chunk_path(data_dir, gmf, "partner"))

step("reading the allied network outputs in ", processed_dir)
nf <- map$network$files
net_path <- function(k) file.path(processed_dir, nf[[k]])
member_shares <- utils::read.csv(net_path("member_shares"), stringsAsFactors = FALSE)
diagnostics <- utils::read.csv(net_path("diagnostics"), stringsAsFactors = FALSE)
observed <- utils::read.csv(net_path("observed_hhi"), stringsAsFactors = FALSE)
network_built <- format(file.info(net_path("diagnostics"))$mtime, "%Y-%m-%d")

# ── Reference ───────────────────────────────────────────────────────────────────────────────────
lk <- yf_lookup(meta)
metrics <- map$metrics
measures <- map$measures
cav <- as.data.frame(meta$dict$caveats, stringsAsFactors = FALSE)
components <- yf_components(ass)
hhi_refs <- as.data.frame(meta$dict$hhi_refs, stringsAsFactors = FALSE)
ref_sources <- lapply(seq_len(nrow(lk$sources)), function(i) {
  s <- lk$sources[i, ]
  list(id = s$id, short = s$short, citation = s$citation, url = s$url, licence = s$licence, edition = s$edition)
})
reference <- list(
  schema = "osi-yards-fences-reference", schema_version = 1L, bundle_id = NA_character_,
  metrics = lapply(metrics, function(m) list(id = m$id, concept = m$concept, label = m$label, unit = m$unit,
                                              engineInput = m$engine_input %||% NA, normalized = isTRUE(m$normalized), explanation = m$explanation)),
  # I(): jsonlite's auto_unbox would turn a one-item list into a bare string
  measures = measures, enginePriority = lapply(map$engine_measure_priority, function(x) I(unlist(x))), concepts = map$concepts,
  components = components, stacks = lapply(map$stacks, function(s) { s$layers <- I(unlist(s$layers)); s }), sources = ref_sources,
  hhiRefs = lapply(seq_len(nrow(hhi_refs)), function(i) as.list(hhi_refs[i, c("id", "value", "label", "authority", "document", "year")]))
)

# ── Shared tables ───────────────────────────────────────────────────────────────────────────────
summ <- as.data.frame(meta$summary, stringsAsFactors = FALSE)
summ <- summ[summ$eu_mode == "members", ]
summ$var_id <- lk$var_id(summ$var)
summ$slug <- lk$item_slug(summ$item)
obs <- as.data.frame(facts$obs, stringsAsFactors = FALSE)
obs$var_id <- lk$var_id(obs$var)
obs$slug <- lk$item_slug(obs$item)
sub_summ <- do.call(rbind, lapply(trade, function(t) as.data.frame(t$trade$sub_summary, stringsAsFactors = FALSE)))
sub_summ <- sub_summ[sub_summ$eu_mode == "members" & sub_summ$year == 2024L, ]
sub_summ$var_id <- lk$var_id(sub_summ$var)
sub_summ <- sub_summ[sub_summ$var_id == measures$sub_exports$var, ]
sub_summ$slug <- lk$item_slug(sub_summ$item)
cty_sub <- do.call(rbind, lapply(trade, function(t) as.data.frame(t$trade$cty_sub, stringsAsFactors = FALSE)))
cty_sub <- cty_sub[cty_sub$year == 2024L, ]
cty_sub$slug <- lk$item_slug(cty_sub$item)
flows <- do.call(rbind, lapply(trade, function(t) as.data.frame(t$trade$flows, stringsAsFactors = FALSE)))
cells <- yf_cells(meta, trade, lk)
cell_caveats <- setNames(lapply(cells, function(c) c$caveats), vapply(cells, function(c) c$id, character(1)))
psi <- as.data.frame(partner$baseline$partner$psi_dyad, stringsAsFactors = FALSE)
nodes <- as.data.frame(core$dims$nodes, stringsAsFactors = FALSE)
n_top <- map$network$top_suppliers

summary_for <- function(var_id, slug, year) {
  r <- summ[summ$var_id == var_id & summ$slug == slug & summ$year == year, , drop = FALSE]
  if (nrow(r)) r[1, ] else NULL
}
pool_for <- function(var_id, year) summ$hhi[summ$var_id == var_id & summ$year == year]
metric <- function(id) metrics[[which(vapply(metrics, function(x) x$id == id, logical(1)))]]

minerals_used <- unique(unlist(lapply(components, function(c) {
  s <- c(list(c$evidence), c$secondary)
  vapply(Filter(function(x) !is.null(x) && x$scope == "mineral", s), function(x) x$scopeId, character(1))
})))

build_country <- function(home) {
  home_row <- lk$geo[lk$geo$code == home, ]
  if (!nrow(home_row)) stop("unknown country ", home)
  home_name <- home_row$name[1]
  records <- list()
  supply <- list()
  push <- function(x) records[[length(records) + 1]] <<- x
  conc <- function(ctx, mkey, srow, shares, pool) {
    eng <- mkey %in% unlist(map$engine_measure_priority[[ctx$scope]])
    heng <- mkey %in% unlist(metric("home_share")$engine_measures)
    for (r in yf_concentration_records(ctx, measures[[mkey]], srow, shares, metrics, pool, eng, heng)) push(r)
  }

  # stage scope
  for (cl in cells) {
    slug <- cl$id
    base_ctx <- list(home = home, home_name = home_name, technology = cl$technology, stage = cl$stage, scope = "stage",
                     scope_id = slug, scope_label = paste0(cl$label, " (whole stage)"), caveats = I(cl$caveats))
    for (mkey in c("exports", "mfg_capacity", "mfg_pipeline")) {
      md <- measures[[mkey]]
      srow <- summary_for(md$var, slug, md$year)
      if (is.null(srow)) next
      o <- obs[obs$var_id == md$var & obs$slug == slug & obs$year == md$year, ]
      shares <- yf_shares(o$geo, o$value, srow$W, lk, basis_is_share = identical(srow$basis, "one"))
      src <- lk$source_of(md$var)
      ctx <- c(base_ctx, list(measure = mkey, source = src$text, source_id = src$id, home_status = yf_home_status(o$geo, o$value, o$status, home, lk)))
      conc(ctx, mkey, srow, shares, pool_for(md$var, md$year))
      if (mkey == "exports")
        supply[[length(supply) + 1]] <- yf_supply_view(slug, mkey, md, srow, shares, src$text, n_top,
                                                       yf_home_imports(flows, lk$item_index(slug), home, lk, n_top))
    }
    ctx <- c(base_ctx, list(measure = NA_character_))
    cross <- function(var_id, year) {
      o <- obs[obs$var_id == var_id & obs$slug == slug & obs$year == year, ]
      g <- lk$geo[o$geo + 1L, ]
      keep <- g$kind == "country"
      list(all = o$value[keep], home = o$value[keep][g$code[keep] == home][1] %||% NA)
    }
    # trade
    x <- cross("trade.imports", 2024L)
    if (!is.na(x$home)) push(yf_rank_records(ctx, metric("home_imports"), yf_fill(metric("home_imports")$label, list(country = home_name)), x$home, x$all, "usd", 2024L, lk$source_of("trade.imports")))
    xn <- cross("trade.net", 2024L); xe <- cross("trade.exports", 2024L)
    if (!is.na(xn$home) && !is.na(x$home) && !is.na(xe$home) && xe$home > 0) {
      md <- metric("home_net_trade"); src <- lk$source_of("trade.net")
      ratio <- x$home / xe$home
      push(yf_record(ctx, md, yf_fill(md$label, list(country = home_name)), xn$home, "usd", list(
        year = 2024L, source = src$text, sourceId = src$id, caveats = I(cl$caveats),
        explanation = yf_fill(md$explanation, list(value = yf_fmt(xn$home, "usd"), year = 2024L,
                                                   ratio = if (ratio >= 1) sprintf("%.1f times", ratio) else sprintf("%.0f%% of", 100 * ratio))))))
    }
    for (spec in list(list("home_rca", "trade.rca", 2024L, "rca"), list("home_feasibility", "trade.feasibility", 2023L, "index01"))) {
      x <- cross(spec[[2]], spec[[3]])
      if (!is.na(x$home)) push(yf_rank_records(ctx, metric(spec[[1]]), yf_fill(metric(spec[[1]])$label, list(country = home_name)), x$home, x$all, spec[[4]], spec[[3]], lk$source_of(spec[[2]])))
    }
    x0 <- cross("trade.exports", 2020L)
    if (!is.na(x0$home) && x0$home > 0 && !is.na(xe$home)) {
      md <- metric("home_export_growth"); src <- lk$source_of("trade.exports"); g <- xe$home / x0$home - 1
      push(yf_record(ctx, md, yf_fill(md$label, list(country = home_name)), g, "pct_change", list(
        year = "2020-2024", source = src$text, sourceId = src$id, caveats = I(cl$caveats),
        explanation = yf_fill(md$explanation, list(country = home_name, value = yf_fmt(g, "pct_change"))))))
    }
    # investment
    inv <- obs[obs$var_id == "investment.annual" & obs$slug == slug, ]
    if (nrow(inv)) {
      hv <- inv[lk$geo_code(inv$geo) == home & !is.na(inv$value), ]
      if (nrow(hv)) {
        last <- max(hv$year)
        x <- cross("investment.annual", last)
        push(yf_rank_records(ctx, metric("home_investment"), yf_fill(metric("home_investment")$label, list(country = home_name)), x$home, x$all, "usd_bn", last, lk$source_of("investment.annual")))
        pos <- hv[hv$value > 0, ]
        if (nrow(pos) >= 2) {
          f <- pos[which.min(pos$year), ]; l <- pos[which.max(pos$year), ]
          cagr <- (l$value / f$value)^(1 / (l$year - f$year)) - 1
          md <- metric("home_investment_growth"); src <- lk$source_of("investment.annual")
          push(yf_record(ctx, md, yf_fill(md$label, list(country = home_name)), cagr, "pct_pa", list(
            year = paste0(f$year, "-", l$year), source = src$text, sourceId = src$id, caveats = I(cl$caveats),
            explanation = yf_fill(md$explanation, list(value = yf_fmt(cagr, "pct_pa"), first = f$year, last = l$year)))))
        }
      }
    }
    # technology readiness (global)
    for (spec in list(list("trl", "technology.trl", "trl"), list("trl_change", "technology.trl_change", "trl_points"))) {
      o <- obs[obs$var_id == spec[[2]] & obs$slug == slug, ]
      if (nrow(o)) {
        md <- metric(spec[[1]]); src <- lk$source_of(spec[[2]]); v <- o$value[1]
        push(yf_record(c(ctx, list(measure = NA)), md, md$label, v, spec[[3]], list(
          year = o$year[1], source = src$text, sourceId = src$id, caveats = I(cl$caveats),
          engineInput = md$engine_input %||% NA,
          explanation = yf_fill(md$explanation, list(value = yf_fmt(v, spec[[3]]), country = home_name)))))
      }
    }
    # normalised OSI components
    for (mid in c("osi_addressable_market", "osi_input_cost")) {
      md <- metric(mid)
      comp <- yf_osi_component(core, vars, md$pillar, md$category, md$variable, cl$technology, cl$stage)
      if (is.null(comp) || !nrow(comp) || !home %in% comp$iso3) next
      hv <- comp$value[comp$iso3 == home][1]
      rec <- yf_rank_records(ctx, md, md$label, hv, comp$value, "index01", "OSI baseline (Oct 2026)",
                             list(id = "osi", text = paste0("Opportunity & Security Indices, Strategic bundle ", substr(gmf$bundle_id, 1, 8))))
      rec$normalizedValue <- hv
      rec$rawValue <- NULL
      rec$engineInput <- NA
      push(rec)
    }
  }

  # sub-component scope
  for (i in seq_len(nrow(sub_summ))) {
    s <- sub_summ[i, ]
    cl <- cells[[match(s$slug, vapply(cells, function(c) c$id, character(1)))]]
    sub <- Filter(function(x) x$label == s$sub_sector, cl$subcomponents)[[1]]
    cav_sub <- c(cl$caveats, if (isTRUE(sub$shared)) "Some HS6 codes in this sub-component are shared with other technologies, so its trade is attributed to each of them.")
    o <- cty_sub[cty_sub$slug == s$slug & cty_sub$sub_sector == s$sub_sector, ]
    shares <- yf_shares(o$geo, o$exports, s$W, lk)
    src <- lk$source_of("trade.sub_exports")
    sid <- paste0(s$slug, "::", s$sub_sector)
    ctx <- list(home = home, home_name = home_name, technology = cl$technology, stage = cl$stage, scope = "subcomponent",
                scope_id = sid, scope_label = sprintf("%s (HS %s)", s$sub_sector, paste(sub$hs6, collapse = ", ")),
                caveats = I(cav_sub), measure = "sub_exports", source = src$text, source_id = src$id,
                home_status = yf_home_status(o$geo, o$exports, NULL, home, lk))
    conc(ctx, "sub_exports", s, shares, sub_summ$hhi)
    supply[[length(supply) + 1]] <- yf_supply_view(sid, "sub_exports", measures$sub_exports, s, shares, src$text, n_top)
  }

  # mineral scope (only minerals a curated component points to)
  for (mn in minerals_used) {
    label <- lk$items$label[lk$items$slug == mn & lk$items$axis == "mineral"][1]
    for (mkey in c("refining", "mining", "reserves")) {
      md <- measures[[mkey]]
      srow <- summary_for(md$var, mn, md$year)
      if (is.null(srow)) next
      o <- obs[obs$var_id == md$var & obs$slug == mn & obs$year == md$year, ]
      shares <- yf_shares(o$geo, o$value, srow$W, lk)
      src <- lk$source_of(md$var)
      vcav <- lk$vars$caveats[lk$vars$id == md$var][1]
      cav_ids <- if (is.na(vcav)) character() else trimws(strsplit(vcav, "[;,]")[[1]])
      ctx <- list(home = home, home_name = home_name, technology = label, stage = "Upstream", scope = "mineral",
                  scope_id = mn, scope_label = paste0(label, " (mineral)"), caveats = I(unname(stats::na.omit(cav$text[match(cav_ids, cav$id)]))),
                  measure = mkey, source = src$text, source_id = src$id, home_status = yf_home_status(o$geo, o$value, o$status, home, lk))
      conc(ctx, mkey, srow, shares, pool_for(md$var, md$year))
      if (mkey == "refining") supply[[length(supply) + 1]] <- yf_supply_view(mn, mkey, md, srow, shares, src$text, n_top)
    }
  }

  psi_home <- psi[psi$reporter_iso == home, , drop = FALSE]
  psi_home <- psi_home[order(psi_home$rank), , drop = FALSE]
  top_p <- head(psi_home[!is.na(psi_home$psi), ], map$network$top_partners)
  list(
    schema = "osi-yards-fences-evidence", schema_version = 1L, bundle_id = NA_character_,
    country = list(iso3 = home, name = home_name, tier = home_row$tier[1]),
    cells = cells, records = records, supply = supply,
    network = yf_network_views(member_shares, diagnostics, observed, nodes, psi_home, home, network_built),
    partners = lapply(seq_len(nrow(top_p)), function(i) list(
      iso3 = top_p$partner_iso[i], name = lk$geo$name[match(top_p$partner_iso[i], lk$geo$code)] %||% top_p$partner_iso[i],
      tier = lk$geo$tier[match(top_p$partner_iso[i], lk$geo$code)] %||% "none",
      psi = top_p$psi[i], friendshore = top_p$friendshore[i], opportunity = top_p$opportunity[i], rank = top_p$rank[i]))
  )
}

step("building evidence for ", paste(countries, collapse = ", "))
bundles <- setNames(lapply(countries, build_country), countries)

# ── Validate ────────────────────────────────────────────────────────────────────────────────────
validation <- do.call(rbind, lapply(bundles, function(b) yf_validate_bundle(reference, b, smf, gmf)))
validation <- validation[!duplicated(validation$check) | !validation$ok, ]
for (i in seq_len(nrow(validation))) step(if (validation$ok[i]) "  ok    " else "  FAIL  ", validation$check[i], if (!validation$ok[i]) paste0(": ", validation$detail[i]) else "")
if (!all(validation$ok)) stop("validation failed; nothing written")
n_rec <- vapply(bundles, function(b) length(b$records), integer(1))
step("records: ", paste(names(n_rec), n_rec, collapse = ", "))
if (check_only) {
  step("--check-only: nothing written")
  quit(save = "no", status = 0)
}

# ── Write ───────────────────────────────────────────────────────────────────────────────────────
to_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = I(6), pretty = FALSE)
h8 <- function(txt) substr(digest_md5(txt), 1, 8)
digest_md5 <- function(txt) {
  tf <- tempfile(); on.exit(unlink(tf))
  writeBin(charToRaw(enc2utf8(txt)), tf)
  unname(tools::md5sum(tf))
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
old <- list.files(out_dir, pattern = "^(reference|evidence-[A-Z]{3})\\.[0-9a-f]{8}\\.json$", full.names = TRUE)
written <- character()
write_hashed <- function(stem, obj) {
  obj$bundle_id <- NA_character_
  txt <- as.character(to_json(obj))
  id <- digest_md5(txt)
  obj$bundle_id <- id
  txt <- as.character(to_json(obj))
  f <- sprintf("%s.%s.json", stem, substr(id, 1, 8))
  writeLines(txt, file.path(out_dir, f), useBytes = TRUE)
  written <<- c(written, f)
  list(file = f, id = id)
}
ref_out <- write_hashed("reference", reference)
country_files <- lapply(names(bundles), function(iso) {
  w <- write_hashed(paste0("evidence-", iso), bundles[[iso]])
  list(iso3 = iso, name = bundles[[iso]]$country$name, file = w$file)
})
git_commit <- tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE), error = function(e) NA_character_)
manifest <- list(
  schema = "osi-yards-fences", schema_version = 1L,
  bundle_id = digest_md5(paste(c(ref_out$id, vapply(country_files, function(x) x$file, character(1))), collapse = "|")),
  created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), git_commit = git_commit[1] %||% NA,
  generator = "scripts/89_export_yards_fences_data.R",
  inputs = list(sector_bundle_id = smf$bundle_id, sector_created = smf$created, strategic_bundle_id = gmf$bundle_id,
                strategic_created = gmf$created, network_build = network_built, pipeline_marker = smf$pipeline_marker %||% NA),
  countries = country_files, reference = ref_out$file,
  validation = lapply(seq_len(nrow(validation)), function(i) list(check = validation$check[i], ok = validation$ok[i], detail = validation$detail[i])),
  notes = list(
    "OSI metrics describe observed economic, technological and supply-chain conditions. They do not themselves determine yard placement.",
    "Evidence is measured at the technology-stage, HS6 sub-component or mineral level; no measure here is component-level unless its label says so.",
    "Shares are of world supply on the stated measure and year; EU members are counted individually.",
    "Normalised OSI components are relative (ranks), never absolute quantities, and never feed the decision engine."
  )
)
writeLines(as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)), file.path(out_dir, "manifest.json"), useBytes = TRUE)
stale <- setdiff(basename(old), written)
if (length(stale)) file.remove(file.path(out_dir, stale))
step("wrote ", out_dir, ": manifest.json, ", paste(written, collapse = ", "))
