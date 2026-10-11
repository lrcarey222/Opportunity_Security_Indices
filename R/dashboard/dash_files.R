# The R files the dashboard's reference engine needs, relative to the repository root, in the order
# they are sourced (pure). scripts/85 (the data export), scripts/86 (the test fixtures) and the
# tests source them; the browser runs the TypeScript port in dashboard/src/engine/, which is
# parity-tested against fixtures computed by these files.
dash_engine_files <- function() {
  c(
    "R/utils/scurve.R",
    "R/utils/schema.R",
    "R/themes/partnership_strength/partnership_strength_helpers.R",
    "R/indices/allied_network_design.R",
    "R/dashboard/dash_bundle.R",
    "R/dashboard/dash_engine.R",
    "R/dashboard/dash_files.R",
    "R/dashboard/dash_fixture.R",
    "R/dashboard/dash_network.R",
    "R/dashboard/dash_osb.R",
    "R/dashboard/dash_params.R",
    "R/dashboard/dash_partner.R",
    "R/dashboard/dash_pillars.R",
    "R/dashboard/dash_policy.R",
    "R/dashboard/dash_si.R",
    "R/dashboard/dash_solver.R",
    "R/dashboard/dash_web.R"
  )
}
