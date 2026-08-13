source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

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

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))

# Render docs/sources.md from config/raw_inputs_manifest.yml so the source inventory
# cannot drift away from what the pipeline actually reads.

render_sources_doc <- function(manifest) {
  esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  cell <- function(x, fallback = "—") {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) fallback else esc(x)
  }

  section <- function(entries, title, blurb) {
    if (length(entries) == 0) return(character())

    rows <- vapply(entries, function(e) {
      name <- cell(e$source_name, cell(e$path))
      name_cell <- if (!is.na(e$url) && nzchar(e$url)) {
        paste0("[", name, "](", e$url, ")")
      } else {
        name
      }

      file_cell <- if (!is.na(e$pattern)) {
        paste0("`", esc(e$pattern), "`<br>newest match")
      } else {
        paste0("`", esc(e$path), "`")
      }

      used_by <- if (length(e$required_by) > 0) {
        paste0("`", basename(e$required_by), "`", collapse = ", ")
      } else {
        "—"
      }

      paste0(
        "| ", name_cell,
        " | ", file_cell,
        " | ", cell(e$cadence),
        " | ", cell(e$vintage),
        " | ", cell(e$owner),
        " | ", used_by, " |"
      )
    }, character(1))

    c(
      paste0("### ", title),
      "",
      blurb,
      "",
      "| Source | File | Cadence | Vintage in use | Owner | Read by |",
      "| --- | --- | --- | --- | --- | --- |",
      rows,
      ""
    )
  }

  by_type <- function(type) {
    entries <- Filter(function(e) identical(e$source_type, type), manifest)
    entries[order(vapply(entries, function(e) tolower(cell(e$source_name, e$id)), character(1)))]
  }

  unassigned <- Filter(function(e) identical(e$owner, "unassigned"), manifest)
  unknown <- Filter(function(e) identical(e$source_type, "unknown"), manifest)

  header <- c(
    "# Sources",
    "",
    "<!-- GENERATED FILE - do not edit by hand.",
    "     Regenerate with: Rscript scripts/02_render_sources_doc.R",
    "     Edit config/raw_inputs_manifest.yml instead. -->",
    "",
    paste0(
      "Every raw input the pipeline reads, grouped by how it arrives. ",
      "There are ", length(manifest), " inputs in total."
    ),
    "",
    "- **api** — fetched automatically during ingestion; no human step.",
    "- **manual** — staged by hand into `sharepoint_raw_dir` before a run.",
    "- **derived** — authored by the project, not fetched from anywhere.",
    "- **generated** — written by the pipeline as bookkeeping.",
    ""
  )

  body <- c(
    section(
      by_type("api"),
      "Automated (api)",
      "These refresh on every ingestion run. Comtrade requires `COMTRADE_API_KEY`."
    ),
    section(
      by_type("manual"),
      "Manual staging required",
      paste(
        "Each of these must be downloaded and placed in `sharepoint_raw_dir` before a run.",
        "Ingestion compares size and mtime, so replacing a file with a newer vintage is enough;",
        "entries with a pattern also pick up a renamed release automatically."
      )
    ),
    section(
      by_type("derived"),
      "Project-authored crosswalks (derived)",
      paste(
        "These are maintained by the team rather than fetched. They are still staged from",
        "SharePoint, which means a collaborator without OneDrive access cannot reproduce a run;",
        "moving them under version control would fix that."
      )
    ),
    section(
      by_type("generated"),
      "Pipeline bookkeeping (generated)",
      "Written by the pipeline to record what a given run used."
    ),
    section(
      by_type("unknown"),
      "Uncurated",
      "Discovered in the pipeline but not yet described. Add metadata in `config/raw_inputs_manifest.yml`."
    )
  )

  footer <- c(
    "## Coverage gaps",
    "",
    paste0("- Inputs with no named owner: **", length(unassigned), "**"),
    paste0("- Inputs with `source_type: unknown`: **", length(unknown), "**"),
    "",
    "## Refreshing",
    "",
    "```bash",
    "# re-copy every staged input even when mtimes look current",
    "OPSI_FORCE_REFRESH=true Rscript scripts/05_ingest_sources.R",
    "```",
    "",
    "Ingestion is mtime- and size-aware, so a newer file in the staging area replaces the",
    "local copy automatically. Pattern-matched inputs resolve to the newest available",
    "vintage, and `data/raw/raw_inputs_resolved.yml` records what each run actually used.",
    ""
  )

  paste(c(header, body, footer), collapse = "\n")
}

manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
doc_path <- file.path(repo_root, "docs", "sources.md")

if (!dir.exists(dirname(doc_path))) {
  dir.create(dirname(doc_path), recursive = TRUE)
}
writeLines(render_sources_doc(manifest), doc_path)

message("Wrote ", doc_path, " from ", length(manifest), " manifest entries.")
