# This file is run before all tests (inside each worker when parallel)

# Silence lifecycle deprecation warnings during the test run so they don't
# pollute output. Tests that specifically assert deprecation warnings
# re-enable verbosity locally (see test-1-deprecation-tlf-functions.R).
# Stash the previous value so teardown can restore it instead of clearing it.
options(
  ospsuite.tests.prev_lifecycle_verbosity = getOption("lifecycle_verbosity")
)
options(lifecycle_verbosity = "quiet")

options(ospsuite.plots.watermarkEnabled = TRUE)

# Create global `DataCombined` (DC) objects ----------------------------
source(testthat::test_path("../data/create_data_combined_objects.R"))
