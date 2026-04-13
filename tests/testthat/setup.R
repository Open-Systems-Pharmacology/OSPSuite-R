# This file is run once before all tests

# Set watermark option globally for all tests
options(ospsuite.plots.watermarkEnabled = TRUE)

# Configure parallel test execution
numCores <- parallel::detectCores()
numCores <- if (is.na(numCores)) 1L else numCores
Sys.setenv(TESTTHAT_PARALLEL = "true")
Sys.setenv(TESTTHAT_CPUS = as.character(numCores))

# Create data combined objects for testing and save them as RDS files.
# This avoids recreating these objects from scratch in each test.
source(testthat::test_path("../data/create_and_save_data_combined_objects.R"))
