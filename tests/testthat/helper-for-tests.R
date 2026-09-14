aciclovirSimulationPath <- system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
)

getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

globalTestMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("MoBiProject/Test_Project.mbp3")
)

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# No PK-Sim project file is stored in the repository. Tests that need one get it
# from the snapshot in `inst/extdata`, which is the maintained form of that
# project (see `data-raw/README.md`). The conversion is slow and the tests only
# read the file, so it happens on first use and the path is reused afterwards.
# The directory lives in the test process and R removes it on exit.
testProjectPath <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      outputDir <- file.path(tempdir(), "test-project")
      dir.create(outputDir, showWarnings = FALSE)
      loadProjectFromSnapshot(
        system.file("extdata", "test_snapshot.json", package = "ospsuite"),
        output = outputDir
      )
      cached <<- list.files(
        outputDir,
        pattern = "\\.pksim5$",
        full.names = TRUE
      )[[1]]
    }
    cached
  }
})

# Helper function to load a model easily. In the test environment, we do not
# want to load from cache by default. Instead new instances should be created
# unless specifically specified otherwise
loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  simFile <- getSimulationFilePath(simulationName)
  loadSimulation(
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
