# data to be used ---------------------------------------

context("plotObservedVsSimulated")

skip_if_not_installed("vdiffr")
skip_if(getRversion() < "4.1")

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of DataSet objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath("ImporterConfiguration.xml"))
)

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
  myPlotConfiguration$legendTitleSize <- 8
  myPlotConfiguration$pointsSize <- 2.5
  myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
  myPlotConfiguration$pointsColor <- tlf::ColorMaps$default
  myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom plot config",
    fig = plotObservedVsSimulated(myCombDat, myPlotConfiguration, smoother = "loess")
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(myPlotConfiguration$xLabel)
  expect_null(myPlotConfiguration$yLabel)
})
