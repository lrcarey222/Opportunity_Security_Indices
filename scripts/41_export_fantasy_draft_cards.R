# Fantasy Draft cards: reads the Explorer's six-stage posture bundle (dashboard/public/data/posture) and writes
# fantasy-draft/data-osi.js, the technology x stage cards and each game country's High / Medium / Low fence
# on them (R/fantasy_draft/fd_cards.R; rules in config/fantasy_draft.yml). Needs only files in the repo, so it
# runs without config/config.yml. Re-run after scripts/91 re-exports the posture bundle.
#   --config FILE   default config/fantasy_draft.yml
#   --out FILE      default the config's `output`

find_repo_root <- function() {
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  starts <- unique(c(if (length(fa)) dirname(sub("^--file=", "", fa[1])), getwd()))
  for (s in starts) {
    d <- normalizePath(s, winslash = "/", mustWork = FALSE)
    while (dirname(d) != d) {
      if (file.exists(file.path(d, "config", "fantasy_draft.yml"))) return(d)
      d <- dirname(d)
    }
  }
  stop("Could not locate the repo root (config/fantasy_draft.yml).")
}
repo_root <- find_repo_root()
`%||%` <- function(a, b) if (is.null(a)) b else a
source(file.path(repo_root, "R", "fantasy_draft", "fd_cards.R"))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) { i <- match(name, args); if (is.na(i) || i == length(args)) default else args[i + 1] }

cfg <- yaml::read_yaml(arg_value("--config", file.path(repo_root, "config", "fantasy_draft.yml")))
posture_dir <- file.path(repo_root, cfg$posture_dir)
manifest <- jsonlite::fromJSON(file.path(posture_dir, "manifest.json"), simplifyVector = FALSE)
core <- jsonlite::fromJSON(file.path(posture_dir, manifest$core), simplifyVector = TRUE)

cells <- fd_core_cells(core)
allies <- unlist(cfg$allies)
built <- fd_build_cards(cells, allies, unlist(cfg$techs), drop_all_low = isTRUE(cfg$drop_all_low))
js <- fd_js_payload(built, list(core = manifest$core, created = manifest$created, git_commit = manifest$git_commit))

out <- arg_value("--out", file.path(repo_root, cfg$output))
writeLines(js, out, useBytes = TRUE)
message(sprintf("Wrote %s: %d cards (%d KB), %d ratings; fences across the game countries: %s",
                out, nrow(built$cards), round(nchar(js, type = "bytes") / 1024), nrow(built$ratings),
                paste(names(table(built$ratings$fence)), table(built$ratings$fence), sep = " ", collapse = ", ")))
