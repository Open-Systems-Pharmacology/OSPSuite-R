# data to be used ---------------------------------------

context("plotIndividualTimeProfile")

# `loadDataSetsFromExcel()` does not work for non-Windows platforms
skip_on_os("linux")

skip_if(getRversion() < "4.1")

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of `DataSet` objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath("ImporterConfiguration.xml"))
)

# both observed and simulated ------------------------

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Organism|Lumen|Stomach|Metformin|Gastric retention"
  )
)
myCombDat$addDataSets(dataSet)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Stevens_2012_placebo.Sita_dist",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Stevens_2012_placebo.Sita_proximal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Stevens_2012_placebo.Sita_total"
  ),
  groups = c("distal", "distal", "proximal", "proximal", "total", "total")
)

test_that("It creates default plots as expected for both observed and simulated", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults - both",
    fig = plotIndividualTimeProfile(myCombDat)
  )
})

test_that("It respects custom plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$subtitle <- "My Plot Subtitle"
  myPlotConfiguration$caption <- "My Sources"
  myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
  myPlotConfiguration$yAxisScale <- tlf::Scaling$log

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom",
    fig = plotIndividualTimeProfile(myCombDat, myPlotConfiguration)
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(myPlotConfiguration$xLabel)
  expect_null(myPlotConfiguration$yLabel)
})


# only observed ------------------------

myCombDat2 <- DataCombined$new()
myCombDat2$addDataSets(dataSet)

test_that("It creates default plots as expected for only observed", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults - obs",
    fig = plotIndividualTimeProfile(myCombDat2)
  )
})

# only simulated ------------------------

myCombDat3 <- DataCombined$new()
myCombDat3$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Organism|Lumen|Stomach|Metformin|Gastric retention"
  )
)

test_that("It creates default plots as expected for only simulated", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults - sim",
    fig = plotIndividualTimeProfile(myCombDat3)
  )
})


# geometric error ------------------------

test_that("It works when geometric error is present", {
  obsData <- loadDataSetFromPKML(system.file("extdata", "ObsDataAciclovir_3.pkml", package = "ospsuite"))

  myDataCombined4 <- DataCombined$new()
  myDataCombined4$addDataSets(obsData, groups = "Aciclovir PVB")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geometric error",
    fig = plotIndividualTimeProfile(myDataCombined4)
  )
})

# multiple datasets per group ------------------------

test_that("It maps multiple observed datasets to different shapes", {
  dataSet1 <- DataSet$new(name = "Dataset1")
  dataSet1$setValues(1, 1)
  dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
  dataSet1$molWeight <- 1

  dataSet2 <- DataSet$new(name = "Dataset2")
  dataSet2$setValues(2, 1)
  dataSet2$yDimension <- ospDimensions$`Concentration (mass)`
  dataSet2$molWeight <- 1

  dataSet3 <- DataSet$new(name = "Dataset3")
  dataSet3$setValues(1, 3)
  dataSet3$yDimension <- ospDimensions$`Concentration (mass)`
  dataSet3$molWeight <- 1

  myCombDat5 <- DataCombined$new()
  myCombDat5$addDataSets(
    c(dataSet1, dataSet2, dataSet3),
    groups = "myGroup"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs",
    fig = plotIndividualTimeProfile(myCombDat5)
  )
})

test_that("It maps simulated datasets to different linetypes", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  outputPath <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  addOutputs(outputPath, sim)
  simResults <- runSimulation(sim)


  myDataCombined6 <- DataCombined$new()

  # Add simulated results
  myDataCombined6$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPath,
    groups = "Aciclovir PVB"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim",
    fig = plotIndividualTimeProfile(myDataCombined6)
  )
})

test_that("It maps multiple observed and simulated datasets to different visual properties", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  outputPath <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  addOutputs(outputPath, sim)
  simResults <- runSimulation(sim)

  obsData <- lapply(
    c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
    function(x) {
      loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
    }
  )

  names(obsData) <- lapply(obsData, function(x) {
    x$name
  })

  myDataCombined5 <- DataCombined$new()

  # Add simulated results
  myDataCombined5$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPath,
    groups = "Aciclovir PVB"
  )

  # Add observed data set
  myDataCombined5$addDataSets(obsData, groups = "Aciclovir observed")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs and sim",
    fig = plotIndividualTimeProfile(myDataCombined5)
  )
})


# edge cases ------------------------

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotIndividualTimeProfile(myCombDat)))
  expect_warning(
    plotIndividualTimeProfile(myCombDat),
    messages$plottingWithEmptyDataCombined()
  )
})
