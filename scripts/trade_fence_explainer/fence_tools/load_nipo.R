p <- "data/raw/GTA NIPO - October 2026.xlsx"
sh <- readxl::excel_sheets(p); print(sh)
x <- readxl::read_excel(p, sheet = sh[1], guess_max = 50000)
cat(nrow(x), ncol(x), "\n"); print(names(x))
us <- x[x$`Implementing Jurisdiction` %in% c("United States of America", "United States"), ]
cat("US rows", nrow(us), "\n")
saveRDS(us, "../scratch_twt/nipo_us_oct2026.rds")
write.csv(us, "../scratch_twt/nipo_us_oct2026.csv", row.names = FALSE)
print(table(us$`GTA Intervention Type`)[order(-table(us$`GTA Intervention Type`))][1:40])
print(table(us$`Initial Assessment (Change Relative to 1 Jan 2009)`))
