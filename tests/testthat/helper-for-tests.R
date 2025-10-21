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
  # Find the package root directory (where DESCRIPTION file is located)
  pkgRoot <- NULL

  # Strategy 1: Check current working directory first
  if (file.exists("DESCRIPTION")) {
    pkgRoot <- getwd()
  }

  # Strategy 2: Search upward from current directory
  if (is.null(pkgRoot)) {
    searchDir <- getwd()
    for (i in 1:10) {
      if (file.exists(file.path(searchDir, "DESCRIPTION"))) {
        pkgRoot <- searchDir
        break
      }
      searchDir <- dirname(searchDir)
      if (searchDir == "/" || searchDir == dirname(searchDir)) {
        break
      }
    }
  }

  # Strategy 3: Search from test directory
  if (is.null(pkgRoot)) {
    testDir <- testthat::test_path()
    searchDir <- testDir
    for (i in 1:10) {
      if (file.exists(file.path(searchDir, "DESCRIPTION"))) {
        pkgRoot <- searchDir
        break
      }
      searchDir <- dirname(searchDir)
      if (searchDir == "/" || searchDir == dirname(searchDir)) {
        break
      }
    }
  }

  # Strategy 4: Try common relative paths from current directory
  if (is.null(pkgRoot)) {
    commonPaths <- c(".", "..", "../..", "../../..")
    for (path in commonPaths) {
      if (file.exists(file.path(path, "DESCRIPTION"))) {
        pkgRoot <- normalizePath(path)
        break
      }
    }
  }

  # Final check
  if (is.null(pkgRoot) || !file.exists(file.path(pkgRoot, "DESCRIPTION"))) {
    stop(
      "Cannot find package root with DESCRIPTION file. ",
      "Current directory: ",
      getwd(),
      ", Test directory: ",
      testthat::test_path(),
      ", Files in current dir: ",
      paste(list.files(".", all.files = TRUE), collapse = ", ")
    )
  }

  # Create temp directory and copy entire package
  tempPkgDir <- tempfile(pattern = "temppkg")

  # Copy all files and directories from package root to temp directory
  dir.create(tempPkgDir, showWarnings = FALSE, recursive = TRUE)
  file.copy(
    list.files(pkgRoot, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    tempPkgDir,
    recursive = TRUE
  )

  # Handle SQLite database restoration if needed
  # Check if there's an original database backup in the source
  originalDbPath <- file.path(
    pkgRoot,
    "inst",
    "lib",
    "PKSimDB.sqlite.ospsuite_original"
  )
  tempDbPath <- file.path(tempPkgDir, "inst", "lib", "PKSimDB.sqlite")
  tempOriginalDbPath <- file.path(
    tempPkgDir,
    "inst",
    "lib",
    "PKSimDB.sqlite.ospsuite_original"
  )

  # If original backup exists in source, restore it in the temp copy
  if (file.exists(tempOriginalDbPath)) {
    # Copy the original backup to temp location
    file.copy(tempOriginalDbPath, tempDbPath, overwrite = TRUE)

    # Delete the original backup
    unlink(tempOriginalDbPath)

    # Remove any existing marker files to ensure fresh state
    markerFile <- file.path(
      tempPkgDir,
      "inst",
      "lib",
      "PKSimDB.sqlite.ospsuite_macos_fixed"
    )
    if (file.exists(markerFile)) {
      unlink(markerFile)
    }
  }

  # Ensure cleanup: reset permissions and delete temp directory
  # Use withr::defer with parent environment so cleanup happens after the test, not after this function
  withr::defer(
    {
      # Reset permissions recursively so files can be deleted
      if (dir.exists(tempPkgDir)) {
        Sys.chmod(tempPkgDir, mode = "0755")

        # Now safe to delete
        unlink(tempPkgDir, recursive = TRUE, force = TRUE)
      }
    },
    envir = parent.frame()
  )

  return(tempPkgDir)
}
