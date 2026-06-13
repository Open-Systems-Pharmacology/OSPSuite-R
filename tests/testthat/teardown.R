# This file is run once after all tests

# Restore lifecycle verbosity to its previous value
options(
  lifecycle_verbosity = getOption("ospsuite.tests.prev_lifecycle_verbosity"),
  ospsuite.tests.prev_lifecycle_verbosity = NULL
)
