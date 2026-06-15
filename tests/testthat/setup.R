# This file is run once before all tests

# Silence lifecycle deprecation warnings during the test run so they don't
# pollute output. Tests that specifically assert deprecation warnings
# re-enable verbosity locally (see test-deprecation-tlf-functions.R).
# Stash the previous value so teardown can restore it instead of clearing it.
options(
  ospsuite.tests.prev_lifecycle_verbosity = getOption("lifecycle_verbosity")
)
options(lifecycle_verbosity = "quiet")
