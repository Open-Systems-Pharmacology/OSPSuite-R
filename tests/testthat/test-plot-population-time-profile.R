# Update the theme to revers settings by ospsuite.plots::setDefaults()
originalTheme <- ggplot2::theme_get()
ggplot2::theme_update(panel.border = ggplot2::element_blank())
ggplot2::reset_geom_defaults()

# data to be used ---------------------------------------

aciclovirSim <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = TRUE,
  addToCache = TRUE
)
populationResults <- importResultsFromCSV(
  simulation = aciclovirSim,
  filePaths = getTestDataFilePath("SimResults_pop.csv")
)

obsData <- lapply(
  c(
    "ObsDataAciclovir_1.pkml",
    "ObsDataAciclovir_2.pkml",
    "ObsDataAciclovir_3.pkml"
  ),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)
names(obsData) <- lapply(obsData, function(x) x$name)

# plotPopulationTimeProfile

# only simulated ------------------------

test_that("It respects custom plot configuration", {
  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults)

  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$subtitle <- "My Plot Subtitle"
  myPlotConfiguration$caption <- "My Sources"

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "only sim",
    fig = plotPopulationTimeProfile(myDataComb, myPlotConfiguration)
  )
})

# both observed and simulated ------------------------

test_that("It produces expected plot for both observed and simulated datasets", {
  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  myDataCombined <- DataCombined$new()

  # Add simulated results
  myDataCombined$addSimulationResults(
    simulationResults = populationResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  # Add observed data set
  myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

  myDataCombined$setDataTransformations(
    forNames = obsData$`Laskin 1982.Group D`$name,
    xOffsets = 2
  )

  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yAxisScale <- "log"
  myPlotConfiguration$yAxisLimits <- c(0.01, 1000)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "obs and sim",
    fig = plotPopulationTimeProfile(myDataCombined, myPlotConfiguration)
  )
})

# multiple datasets per group ---------------------

test_that("It produces expected plot for multple simulated datasets per group", {
  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  myDataCombined <- DataCombined$new()

  myDataCombined$addSimulationResults(
    simulationResults = populationResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim",
    fig = plotPopulationTimeProfile(myDataCombined)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim - dataset legend",
    fig = plotPopulationTimeProfile(myDataCombined, showLegendPerDataset = TRUE)
  )
})

test_that("It produces expected plot for multple simulated and observed datasets per group", {
  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  myDataCombined <- DataCombined$new()

  myDataCombined$addSimulationResults(
    simulationResults = populationResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  myDataCombined$addDataSets(obsData[c(1, 3)], groups = "Aciclovir observed")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim and obs",
    fig = plotPopulationTimeProfile(myDataCombined)
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

# Aggregations ------------------------

test_that("Aggregations are computed and displayed correctly", {
  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults)

  vdiffr::expect_doppelganger(
    title = "default (quantiles)",
    fig = plotPopulationTimeProfile(myDataComb)
  )

  vdiffr::expect_doppelganger(
    title = "modified quantiles",
    fig = plotPopulationTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))
  )

  vdiffr::expect_doppelganger(
    title = "arithmetic mean",
    fig = plotPopulationTimeProfile(myDataComb, aggregation = "arithmetic")
  )

  vdiffr::expect_doppelganger(
    title = "arithmetic mean with 2sd",
    fig = plotPopulationTimeProfile(
      myDataComb,
      aggregation = "arithmetic",
      nsd = 2
    )
  )

  vdiffr::expect_doppelganger(
    title = "geometric mean",
    fig = plotPopulationTimeProfile(myDataComb, aggregation = "geometric")
  )

  vdiffr::expect_doppelganger(
    title = "geometric mean with 2sd",
    fig = plotPopulationTimeProfile(
      myDataComb,
      aggregation = "geometric",
      nsd = 2
    )
  )
})
ggplot2::theme_set(originalTheme)
ospsuite.plots::setDefaults()
