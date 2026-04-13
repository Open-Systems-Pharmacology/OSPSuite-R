# This file is run once after all tests
# Clean up watermark option
options(ospsuite.plots.watermarkEnabled = NULL)

# Remove files created in "create_and_save_data_combined_objects.R"
rds_files <- list.files(
  testthat::test_path("../data"),
  pattern = "\\.rds$",
  full.names = TRUE
)
file.remove(rds_files)
