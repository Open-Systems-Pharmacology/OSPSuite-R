# data to be used ---------------------------------------

context("plotObservedVsSimulated")

skip_if_not_installed("vdiffr")
skip_if(getRversion() < "4.1")
skip_on_ci() # TODO: fix tests once https://github.com/Open-Systems-Pharmacology/TLF-Library/issues/367 is resolved

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

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet)
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )
)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Placebo_distal",
    "Stevens_2012_placebo.Placebo_proximal"
  ),
  groups = c("Solid total", "Solid distal", "Solid proximal", "Solid total", "Solid distal", "Solid proximal")
)

test_that("It creates default plots as expected", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "default plot",
    fig = plotObservedVsSimulated(myCombDat)
  )
})

test_that("It issues warning when scale is linear", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$xAxisScale <- tlf::Scaling$lin
  myPlotConfiguration$yAxisScale <- tlf::Scaling$lin

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "linear scale",
    fig = plotObservedVsSimulated(myCombDat, myPlotConfiguration)
  )
})


test_that("It respects custom plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$subtitle <- "My Plot Subtitle"
  myPlotConfiguration$caption <- "My Sources"
  myPlotConfiguration$legendTitle <- "My legendary title"
  myPlotConfiguration$legendTitleColor <- "red"
  myPlotConfiguration$legendTitleSize <- 16
  myPlotConfiguration$legendKeysColor <- "brown"
  myPlotConfiguration$pointsSize <- 2.5
  myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
  myPlotConfiguration$pointsColor <- tlf::ColorMaps$default
  myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "customized plot",
    fig = plotObservedVsSimulated(
      myCombDat,
      myPlotConfiguration,
      foldDistance = c(1.5, 2)
    )
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(myPlotConfiguration$xLabel)
  expect_null(myPlotConfiguration$yLabel)
})

test_that("It produces expected plot for Aciclovir data", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  simResults <- runSimulation(sim)

  obsData <- lapply(
    c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
    function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  )

  names(obsData) <- lapply(obsData, function(x) x$name)

  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  myDataCombined <- DataCombined$new()

  # Add simulated results
  myDataCombined$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPath,
    groups = "Aciclovir PVB"
  )

  # Add observed data set
  myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "Aciclovir data",
    fig = plotObservedVsSimulated(myDataCombined)
  )
})

# edge cases ------------------------

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotObservedVsSimulated(myCombDat)))
  expect_warning(
    plotObservedVsSimulated(myCombDat),
    messages$plottingWithEmptyDataCombined()
  )
})


test_that("It returns `NULL` when `DataCombined` doesn't have any pairable datasets", {
  dataSet1 <- DataSet$new(name = "Dataset1")
  dataSet1$setValues(1, 1)
  dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
  dataSet1$molWeight <- 1

  myCombDat <- DataCombined$new()
  myCombDat$addDataSets(dataSet1)

  expect_null(suppressWarnings(plotObservedVsSimulated(myCombDat)))
  expect_warning(
    plotObservedVsSimulated(myCombDat),
    messages$plottingWithNoPairedDatasets()
  )
})
