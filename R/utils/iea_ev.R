# IEA Global EV Outlook reader.
#
# The IEA restaged this dataset between releases. Through the 2025 release it reached the
# repo as the EV Data Explorer workbook ("IEA_EVDataExplorer2025.xlsx", data on the first
# sheet); the Global EV Outlook 2026 publishes the same extract as a flat CSV ("EV data by
# country 2026.csv"). Both carry the identical long layout, so read_iea_ev() accepts
# either form and hands back one frame:
#
#   region_country  country or aggregate region ("China", "Europe", "World")
#   category        "Historical" or a projection scenario
#   parameter       "EV stock", "EV sales", "EV sales share", "EV charging points", ...
#   mode            "Cars", "Vans", "Buses", "Trucks", "EVSE", ...
#   powertrain      "BEV", "PHEV", "FCEV", and an "EV" total
#   year            integer
#   unit            "Vehicles", "percent", "charging points", ...
#   value           numeric
#
# Two shape changes arrived with the 2026 release, and callers rather than this reader
# decide what to do with them, because the answer is theme-specific:
#
#   1. A second projection scenario. The 2025 workbook published only Stated Policies
#      ("Projection-STEPS"); 2026 publishes Current Policies ("Projection-CPS") beside it,
#      so anything summing over `category` now double counts the projection year.
#   2. An "EV" powertrain total. Stock and sales rows used to break out BEV/PHEV/FCEV
#      only; 2026 adds an "EV" row that sums them, so anything summing over `powertrain`
#      now double counts. The components carry full precision while the total is rounded
#      to two significant figures, so summing the components is the more faithful total.
#
# See future_demand_build_ev() for how the Technology Demand theme handles both.

IEA_EV_COLUMNS <- c(
  "region_country",
  "category",
  "parameter",
  "mode",
  "powertrain",
  "year",
  "unit",
  "value"
)

read_iea_ev <- function(path) {
  if (!file.exists(path)) {
    stop("IEA Global EV Outlook dataset not found: ", path)
  }

  ev <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    as.data.frame(readxl::read_excel(path, sheet = 1), check.names = FALSE)
  }

  missing_cols <- setdiff(IEA_EV_COLUMNS, names(ev))
  if (length(missing_cols) > 0) {
    stop(
      "IEA Global EV Outlook dataset is missing columns: ",
      paste(missing_cols, collapse = ", "),
      " (", basename(path), ")"
    )
  }

  ev$year <- as.integer(ev$year)
  ev$value <- as.numeric(ev$value)
  ev
}
