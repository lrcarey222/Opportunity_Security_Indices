# The commit stamp of the code that writes a dashboard bundle (IO: runs git in the repo). The rule is
# posture_commit_stamp() (R/dashboard/posture/posture_export.R): the short sha of HEAD, with "-dirty" where
# `git status --porcelain -- R scripts config` prints anything (the code that wrote the bundle is not the
# commit's); NA when git is not available. scripts/84 (AIB) and scripts/34 (partnership memos) use it;
# scripts/91 (posture) runs the same two commands itself.
#
#   source(file.path(repo_root, "scripts", "utils", "git_stamp.R"))
#   git_commit <- git_commit_stamp(repo_root)

git_commit_stamp <- function(root) {
  stamp_env <- new.env()
  sys.source(file.path(root, "R", "dashboard", "posture", "posture_export.R"), envir = stamp_env)
  sha <- tryCatch(system2("git", c("-C", shQuote(root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
                  error = function(e) NA_character_)
  porcelain <- tryCatch(system2("git", c("-C", shQuote(root), "status", "--porcelain", "--", "R", "scripts", "config"),
                                stdout = TRUE, stderr = FALSE), error = function(e) "unknown")
  stamp_env$posture_commit_stamp(sha, porcelain)
}
