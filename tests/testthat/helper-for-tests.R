loadTestSimulation <- function(simulationName, loadFromCache = TRUE) {
  dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
  simFile <- file.path(dataPath, paste0(simulationName, ".pkml"), fsep = .Platform$file.sep)
  sim <- loadSimulation(simFile, loadFromCache)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}
