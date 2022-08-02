# data to be used ---------------------------------------

context("plotPopulationTimeProfile")

skip_on_os("linux") # TODO enable again as soon as `createPopulation()` runs under Linux
skip_if_not_installed("vdiffr")
skip_if(getRversion() < "4.1")

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

populationResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file("extdata", "SimResults_pop.csv", package = "ospsuite")
)

myDataComb <- DataCombined$new()
myDataComb$addSimulationResults(populationResults)

# only simulated ------------------------

test_that("It respects custom plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$subtitle <- "My Plot Subtitle"
  myPlotConfiguration$caption <- "My Sources"

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom plot config",
    fig = plotPopulationTimeProfile(myDataComb, myPlotConfiguration)
  )
})

# edge cases ------------------------

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotPopulationTimeProfile(myCombDat)))
  expect_warning(
    plotPopulationTimeProfile(myCombDat),
    messages$plottingWithEmptyDataCombined()
  )
})
