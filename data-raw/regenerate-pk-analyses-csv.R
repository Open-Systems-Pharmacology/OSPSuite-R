# Rebuilds inst/extdata/PKAnalyses.csv from inst/extdata/Aciclovir.pkml. Run from
# the repository root, after the PKML has been regenerated:
#
#   Rscript data-raw/regenerate-pk-analyses-csv.R
#
# See data-raw/README.md for the background and for what to expect in the diff.

devtools::load_all(".", quiet = TRUE)

pkmlPath <- file.path("inst", "extdata", "Aciclovir.pkml")
csvPath <- file.path("inst", "extdata", "PKAnalyses.csv")

simulation <- loadSimulation(pkmlPath)
results <- runSimulations(simulation)[[1]]
pkAnalyses <- calculatePKAnalyses(results)

# The .NET writer needs an existing absolute path.
exportPKAnalysesToCSV(pkAnalyses, normalizePath(csvPath))
message("Wrote: ", csvPath)

# Report what came out: one block of 14 parameters per output path.
rows <- readLines(csvPath, warn = FALSE)
message("Rows (without header): ", length(rows) - 1)
message(
  "Outputs: ",
  paste(
    vapply(
      simulation$outputSelections$allOutputs,
      function(output) output$path,
      FUN.VALUE = character(1)
    ),
    collapse = " ; "
  )
)
