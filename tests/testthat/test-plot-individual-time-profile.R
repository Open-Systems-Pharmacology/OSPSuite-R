# data to be used ---------------------------------------

context("plotIndividualTimeProfile")

# `loadDataSetsFromExcel()` does not work for non-Windows platforms
skip_on_os("linux")
skip_on_ci() # TODO: run on Appveyor
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
  # TODO:
  # - legend doesn't match with colors and shapes
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "default plot and units",
    fig = plotIndividualTimeProfile(myCombDat)
  )
})

test_that("It respects custom plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$pointsSize <- 2.5
  myPlotConfiguration$legendTitle <- "My Groupings"
  myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight

  # TODO:
  # - why are custom colors not mapped?
  # - legend title not present
  # myPlotConfiguration$pointsColor <- tlf::ColorMaps$default

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom plot config",
    fig = plotIndividualTimeProfile(myCombDat, myPlotConfiguration)
  )
})
