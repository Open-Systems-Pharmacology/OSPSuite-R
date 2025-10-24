# Development setup script for ospsuite package
#
# This script runs the configure script to prepare platform-specific
# library files for development.
#
# Usage:
#   source("tools/setup_dev.R")
#   setup_dev()
#   Then use devtools::load_all() as normal
#
# This will run the configure script that is usually run during installation
#
# After running this setup, you can use devtools::load_all() normally.

setup_dev <- function() {
  cat("Running platform configuration for development...\n")

  source("tools/configure_platform.R")

  cat("\nSetup complete! You can now use devtools::load_all()\n")
}
