# This file is run after all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = NULL)

# Clean up RDS files created by setup
rds_files <- file.path(
  testthat::test_path("../data"),
  c(
    "oneObsDC.rds",
    "oneSimDC.rds",
    "manyObsDC.rds",
    "manySimDC.rds",
    "oneObsSimDC.rds",
    "manyObsSimDC.rds",
    "oneObsGeometricDC.rds",
    "customDPC.rds",
    "manyObsSimDCWithFraction.rds"
  )
)
file.remove(rds_files[file.exists(rds_files)])
