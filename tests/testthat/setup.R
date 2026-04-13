# This file is run before all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = TRUE)

# Create global `DataCombined` (DC) objects ----------------------------
source(testthat::test_path("data/create_data_combined_objects.R"))
