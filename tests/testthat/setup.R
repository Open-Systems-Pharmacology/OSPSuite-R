# This file is run once before all tests (inside each worker when parallel)

# Set watermark option globally for all tests
options(ospsuite.plots.watermarkEnabled = TRUE)

# Create data combined objects for testing and save them as RDS files.
# This avoids recreating these objects from scratch in each test.
source(testthat::test_path("../data/create_and_save_data_combined_objects.R"))
