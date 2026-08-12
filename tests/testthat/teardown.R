# This file is run after all tests (inside each worker when parallel)

# Restore lifecycle verbosity to its previous value
options(
  lifecycle_verbosity = getOption("ospsuite.tests.prev_lifecycle_verbosity"),
  ospsuite.tests.prev_lifecycle_verbosity = NULL
)
