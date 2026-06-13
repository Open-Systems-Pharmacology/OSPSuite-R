# This file is run once before all tests

# Silence lifecycle deprecation warnings during the test run so they don't
# pollute output. Tests that specifically assert deprecation warnings
# re-enable verbosity locally (see test-deprecation-tlf-functions.R).
options(lifecycle_verbosity = "quiet")
