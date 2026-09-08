# Lifts the test fixtures in PKML format to the format version of the current
# libraries, by loading each file and writing it back. Run from the repository
# root:
#
#   Rscript data-raw/regenerate-pkml-fixtures.R
#
# See data-raw/README.md.

devtools::load_all(".", quiet = TRUE)

# Simulations. tests/data/simple.pkml and inst/extdata/simple.pkml hold the same
# simulation, and the second is written from the first to keep them identical.
simulationFiles <- c(
  file.path("tests", "data", "simple.pkml"),
  file.path("tests", "data", "concentration_based.pkml"),
  file.path("tests", "data", "MoBiProject", "TestSim_2Modules.pkml")
)

# Observed data, loaded and written as `DataSet` rather than as a simulation.
dataSetFiles <- c(
  file.path("tests", "data", "obs_data.pkml"),
  file.path("tests", "data", "obs_data_no_error.pkml")
)

formatVersion <- function(filePath) {
  head <- readLines(filePath, warn = FALSE, n = 3)
  match <- regmatches(head, regexpr('version="[0-9]+"', head))
  match <- unlist(match)
  if (length(match) == 0) NA_character_ else sub('version="([0-9]+)"', "\\1", match[[1]])
}

for (filePath in simulationFiles) {
  before <- formatVersion(filePath)
  simulation <- loadSimulation(filePath, loadFromCache = FALSE, addToCache = FALSE)
  saveSimulation(simulation, filePath)
  message(basename(filePath), ": format version ", before, " -> ", formatVersion(filePath))
}

for (filePath in dataSetFiles) {
  before <- formatVersion(filePath)
  saveDataSetToPKML(loadDataSetFromPKML(filePath), filePath)
  message(basename(filePath), ": format version ", before, " -> ", formatVersion(filePath))
}

# Keep the second copy of the simple simulation in step with the first.
invisible(file.copy(
  file.path("tests", "data", "simple.pkml"),
  file.path("inst", "extdata", "simple.pkml"),
  overwrite = TRUE
))
message("Copied tests/data/simple.pkml to inst/extdata/simple.pkml")
