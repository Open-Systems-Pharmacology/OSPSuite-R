# This file is run after all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = NULL)

# Clean up RDS files created by setup
rds_files <- list.files(
  testthat::test_path("../data"),
  pattern = "\\.rds$",
  full.names = TRUE
)
file.remove(rds_files)
