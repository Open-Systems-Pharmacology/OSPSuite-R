# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(ospsuite)

# Set TESTTHAT_CPUS dynamically before test_check() spawns workers.
# Config/testthat/parallel in DESCRIPTION enables parallelism;
# this controls how many workers are used.
numCores <- max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
Sys.setenv(TESTTHAT_CPUS = numCores)

test_check("ospsuite")
