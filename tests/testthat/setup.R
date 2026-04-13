# This file is run once before all tests
# Set watermark option globally for all tests
options(ospsuite.plots.watermarkEnabled = TRUE)

# Create data combined objects for testing and save them as RDS files. This is done to avoid the need to create these objects from scratch in each test, which can be time-consuming.
source(testthat::test_path("../data/create_and_save_data_combined_objects.R"))
