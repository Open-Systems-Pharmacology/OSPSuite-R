getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not
# want to load from cache by default. Instead new instances should be created
# unless specifically specified otherwise
loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- loadSimulation(
    simFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

# Helper function to test that a property is read-only
# Expects an error when trying to assign a value to the property
expectPropertyReadOnly <- function(object, propertyName, testValue = "test") {
  expr <- substitute(object$prop <- testValue, list(prop = as.name(propertyName)))
  expect_error(eval(expr))
}

# Helper function to test snapshot of object's print() method
expectSnapshotPrint <- function(object) {
  expect_snapshot(object$print())
}
