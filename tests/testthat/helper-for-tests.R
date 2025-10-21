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


# Helper function to copy package to temp location
.copyPackageToTemp <- function() {
  # Find package root - works in multiple contexts (interactive, testthat, CI)
  # Use testthat::test_path() to find tests/testthat, then go up two levels
  testDir <- testthat::test_path()
  pkgRoot <- normalizePath(file.path(testDir, "../.."))

  # Verify we found the right location
  if (!file.exists(file.path(pkgRoot, "DESCRIPTION"))) {
    stop(
      "Cannot find package root with DESCRIPTION file. ",
      "Expected at: ",
      file.path(pkgRoot, "DESCRIPTION")
    )
  }

  # Create temp directory and copy package
  tempPkgDir <- tempfile(pattern = "temppkg")

  # Copy entire package directory (creates tempPkgDir as a subdirectory)
  # file.copy with recursive=TRUE copies the directory itself, not just contents
  dir.create(dirname(tempPkgDir), showWarnings = FALSE, recursive = TRUE)
  file.copy(pkgRoot, dirname(tempPkgDir), recursive = TRUE)
  file.rename(file.path(dirname(tempPkgDir), basename(pkgRoot)), tempPkgDir)

  # Ensure cleanup: reset permissions and delete temp directory
  # Use withr::defer with parent environment so cleanup happens after the test, not after this function
  withr::defer(
    {
      # Reset permissions recursively so files can be deleted
      if (dir.exists(tempPkgDir)) {
        Sys.chmod(tempPkgDir, mode = "0755", use_umask = FALSE)

        # Now safe to delete
        unlink(tempPkgDir, recursive = TRUE, force = TRUE)
      }
    },
    envir = parent.frame()
  )

  return(tempPkgDir)
}
