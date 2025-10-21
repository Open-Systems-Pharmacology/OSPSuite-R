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
  # Start from test path and search upward for DESCRIPTION
  testDir <- testthat::test_path()

  # Try different strategies to find the package root
  pkgRoot <- NULL

  # Strategy 1: Go up two levels from tests/testthat (most common case)
  candidate <- normalizePath(file.path(testDir, "../.."))
  if (file.exists(file.path(candidate, "DESCRIPTION"))) {
    pkgRoot <- candidate
  }

  # Strategy 2: In R CMD check, the package is installed in .Rcheck/
  # Look for the installed package directory
  if (is.null(pkgRoot) && grepl("\\.Rcheck", testDir)) {
    # Extract package name from the path and find installed location
    # In .Rcheck, tests run from: pkg.Rcheck/tests/testthat/
    # The installed package is in: pkg.Rcheck/00_pkg_src/pkg/
    rcheckDir <- sub("(/tests/testthat.*|/tests.*)", "", testDir)

    # Try 00_pkg_src subdirectory (used during check)
    srcDir <- file.path(rcheckDir, "00_pkg_src")
    if (dir.exists(srcDir)) {
      # Find the package directory inside 00_pkg_src
      pkgDirs <- list.dirs(srcDir, recursive = FALSE, full.names = TRUE)
      for (dir in pkgDirs) {
        if (file.exists(file.path(dir, "DESCRIPTION"))) {
          pkgRoot <- dir
          break
        }
      }
    }
  }

  # Strategy 3: Search upward from current directory
  if (is.null(pkgRoot)) {
    searchDir <- testDir
    for (i in 1:10) {
      # Limit search depth
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

  # Final check
  if (is.null(pkgRoot) || !file.exists(file.path(pkgRoot, "DESCRIPTION"))) {
    stop(
      "Cannot find package root with DESCRIPTION file. ",
      "Searched from: ",
      testDir
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
        Sys.chmod(tempPkgDir, mode = "0755")

        # Now safe to delete
        unlink(tempPkgDir, recursive = TRUE, force = TRUE)
      }
    },
    envir = parent.frame()
  )

  return(tempPkgDir)
}
