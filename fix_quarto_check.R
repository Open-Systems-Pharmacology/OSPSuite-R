# Fix for Quarto error on Windows during devtools::check()

# This is a known issue on Windows where environment variables
# are not set correctly when calling Quarto

# Solution 1: Update packages
# Run these commands to ensure you have the latest versions:
# update.packages(c("devtools", "knitr", "rmarkdown"), ask = FALSE)

# Solution 2: Set Quarto path explicitly (if Quarto is installed)
# Sys.setenv(QUARTO_PATH = "C:/Program Files/Quarto/bin/quarto.exe")

# Solution 3: Disable Quarto checking if you don't need it
# Since your vignettes are .Rmd files (not .qmd), you don't need Quarto
Sys.setenv(QUARTO_ENABLED = "FALSE")

# Solution 4: Run check with specific options to skip vignette building
# devtools::check(build_args = "--no-build-vignettes")

# Solution 5: Use rcmdcheck directly with proper environment
# library(rcmdcheck)
# rcmdcheck::rcmdcheck(env = c(TMPDIR = tempdir()))

# For now, let's disable Quarto since you're not using .qmd files:
message("Disabling Quarto for this R session...")
Sys.setenv(QUARTO_ENABLED = "FALSE")

# Now run the check
message("Running devtools::check()...")
devtools::check()