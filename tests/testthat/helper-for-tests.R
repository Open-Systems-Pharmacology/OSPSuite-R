# Helper function to load a model easily. In the test environment, we do not want to load from cache by default. Instead
# new instances should be created unless specifically specified otherwise
loadTestSimulation <- function(simulationName, loadFromCache = FALSE) {
  dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
  simFile <- file.path(dataPath, paste0(simulationName, ".pkml"), fsep = .Platform$file.sep)
  sim <- loadSimulation(simFile, loadFromCache)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}
