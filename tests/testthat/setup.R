# This file is run once before all tests
# Set watermark option globally for all tests
options(ospsuite.plots.watermarkEnabled = TRUE)

# Create global `DataCombined` (DC) objects ----------------------------
source(testthat::test_path("data/create_data_combined_objects.R"))
