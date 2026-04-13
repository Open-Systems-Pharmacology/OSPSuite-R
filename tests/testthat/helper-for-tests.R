aciclovirSimulationPath <- system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
)

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

globalTestMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("MoBiProject/Test_Project.mbp3")
)

# `DataCombined` and `DefaultPlotConfiguration` globals ----------------------
# Built once per worker; shared across all tests in that worker.
# WARNING: do not mutate these objects in tests (R6 reference semantics).
# If a test needs a modified version, create a local copy with $clone().

.dcSim <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
  loadFromCache = FALSE,
  addToCache = FALSE
)

.dcOutputPathSingle <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
.dcOutputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration",
  "Organism|Kidney|Urine|Aciclovir|Fraction excreted to urine"
)
setOutputs(.dcOutputPaths, .dcSim)
.dcSimResults <- runSimulations(.dcSim)[[1]]

.dcObsData <- lapply(
  c(
    "ObsDataAciclovir_1.pkml",
    "ObsDataAciclovir_2.pkml",
    "ObsDataAciclovir_3.pkml"
  ),
  function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
)
names(.dcObsData) <- lapply(.dcObsData, function(x) x$name)

oneObsDC <- DataCombined$new()
oneObsDC$addDataSets(.dcObsData[["Vergin 1995.Iv"]])

oneSimDC <- DataCombined$new()
oneSimDC$addSimulationResults(.dcSimResults, .dcOutputPathSingle, groups = "Aciclovir PVB")

manyObsDC <- DataCombined$new()
manyObsDC$addDataSets(
  c(.dcObsData[["Vergin 1995.Iv"]], .dcObsData[["Laskin 1982.Group C"]]),
  groups = "Aciclovir observed"
)

manySimDC <- DataCombined$new()
manySimDC$addSimulationResults(.dcSimResults, .dcOutputPaths[1:2], groups = "Aciclovir PVB")

oneObsSimDC <- DataCombined$new()
oneObsSimDC$addDataSets(.dcObsData[["Vergin 1995.Iv"]], groups = "Aciclovir PVB")
oneObsSimDC$addSimulationResults(.dcSimResults, .dcOutputPathSingle, groups = "Aciclovir PVB")

manyObsSimDC <- DataCombined$new()
manyObsSimDC$addDataSets(
  c(.dcObsData[["Vergin 1995.Iv"]], .dcObsData[["Laskin 1982.Group C"]]),
  groups = "Aciclovir observed"
)
manyObsSimDC$addSimulationResults(.dcSimResults, .dcOutputPaths[1:2], groups = "Aciclovir PVB")

oneObsGeometricDC <- DataCombined$new()
oneObsGeometricDC$addDataSets(.dcObsData[["Laskin 1982.Group C"]], groups = "Aciclovir PVB")

customDPC <- DefaultPlotConfiguration$new()
customDPC$title <- "My Plot Title"
customDPC$subtitle <- "My Plot Subtitle"
customDPC$caption <- "My Sources"
customDPC$legendPosition <- tlf::LegendPositions$outsideRight
customDPC$yAxisScale <- "log"
customDPC$yAxisLimits <- c(0.01, 1000)

manyObsSimDCWithFraction <- DataCombined$new()
manyObsSimDCWithFraction$addSimulationResults(
  .dcSimResults,
  .dcOutputPaths[[1]],
  names = "Aciclovir Plasma",
  groups = "Aciclovir PVB"
)
manyObsSimDCWithFraction$addSimulationResults(
  .dcSimResults,
  .dcOutputPaths[[3]],
  names = "Aciclovir Fraction excreted",
  groups = "Aciclovir Urine"
)
manyObsSimDCWithFraction$addDataSets(.dcObsData[[1]], groups = "Aciclovir PVB")
