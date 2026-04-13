# This file is run before all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = TRUE)

# Create data combined RDS files if they don't already exist.
# Guards against redundant creation when multiple workers run setup.R.
if (!file.exists(testthat::test_path("../data/oneObsDC.rds"))) {
  source(testthat::test_path("../data/create_and_save_data_combined_objects.R"))
}
