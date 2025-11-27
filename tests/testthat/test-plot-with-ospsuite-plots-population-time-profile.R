# Set defaults
oldDefaults <- ospsuite.plots::setDefaults()
ggplot2::theme_update(legend.title = ggplot2::element_blank())
ggplot2::theme_update(legend.position = c(0.95, 0.05))
ggplot2::theme_update(legend.justification = c("right", "bottom"))

ospsuite.plots::setOspsuite.plots.option(ospsuite.plots::OptionKeys$Percentiles, c(0.05, 0.25, 0.5, 0.75, 0.95))

# only simulated ------------------------

test_that("It respects custom plot configuration", {
  # Load simulation
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  populationResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = system.file(
      "extdata",
      "SimResults_pop.csv",
      package = "ospsuite"
    )
  )

  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "only sim",
    fig = plotTimeProfile(myDataComb)
  )
})

# both observed and simulated ------------------------

test_that("It produces expected plot for both observed and simulated datasets", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = system.file(
      "extdata",
      "SimResults_pop.csv",
      package = "ospsuite"
    )
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

  outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  myDataCombined <- DataCombined$new()

  # Add simulated results
  myDataCombined$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  # Add observed data set
  myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

  myDataCombined$setDataTransformations(
    forNames = obsData$`Laskin 1982.Group D`$name,
    xOffsets = 2
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "obs and sim",
    fig = plotTimeProfile(myDataCombined,
      yScale = "log",
      yScaleArgs = list(limits = c(0.01, 1000))
    )
  )
})

# multiple datasets per group ---------------------

test_that("It produces expected plot for multiple simulated datasets per group", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = system.file(
      "extdata",
      "SimResults_pop.csv",
      package = "ospsuite"
    )
  )

  obsData <- lapply(
    c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
    function(x) {
      loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
    }
  )

  names(obsData) <- lapply(obsData, function(x) x$name)

  myDataCombined <- DataCombined$new()

  myDataCombined$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim",
    fig = plotTimeProfile(myDataCombined)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim - dataset legend",
    fig = plotTimeProfile(myDataCombined,
      mapping = ggplot2::aes(linetype = group)
    )
  )
})

test_that("It produces expected plot for multiple simulated and observed datasets per group", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  outputPaths <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Muscle|Intracellular|Aciclovir|Concentration"
  )

  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = system.file(
      "extdata",
      "SimResults_pop.csv",
      package = "ospsuite"
    )
  )

  obsData <- lapply(
    c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
    function(x) {
      loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
    }
  )

  names(obsData) <- lapply(obsData, function(x) x$name)

  myDataCombined <- DataCombined$new()

  myDataCombined$addSimulationResults(
    simulationResults = simResults,
    quantitiesOrPaths = outputPaths,
    groups = "Aciclovir PVB"
  )

  myDataCombined$addDataSets(obsData, groups = "Aciclovir observed")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim and obs",
    fig = plotTimeProfile(myDataCombined)
  )
})

# edge cases ------------------------

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_error(
    plotTimeProfile(myCombDat),
    messages$plotNoDataAvailable()
  )
})

# Aggregations ------------------------

test_that("Aggregations are computed and displayed correctly", {
  # Load simulation
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  populationResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = system.file(
      "extdata",
      "SimResults_pop.csv",
      package = "ospsuite"
    )
  )

  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults)

  vdiffr::expect_doppelganger(
    title = "default (quantiles)",
    fig = plotTimeProfile(myDataComb)
  )

  vdiffr::expect_doppelganger(
    title = "modified quantiles",
    fig = plotTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))
  )

  vdiffr::expect_doppelganger(
    title = "arithmetic mean",
    fig = plotTimeProfile(myDataComb, aggregation = "arithmetic")
  )

  vdiffr::expect_doppelganger(
    title = "arithmetic mean with 2sd",
    fig = plotTimeProfile(
      myDataComb,
      aggregation = "arithmetic",
      nsd = 2
    )
  )

  vdiffr::expect_doppelganger(
    title = "geometric mean",
    fig = plotTimeProfile(myDataComb, aggregation = "geometric")
  )

  vdiffr::expect_doppelganger(
    title = "geometric mean with 2sd",
    fig = plotTimeProfile(
      myDataComb,
      aggregation = "geometric",
      nsd = 2
    )
  )
})
