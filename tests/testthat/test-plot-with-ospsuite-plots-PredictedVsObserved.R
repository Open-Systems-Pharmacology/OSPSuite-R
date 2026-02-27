# Set defaults
oldDefaults <- ospsuite.plots::setDefaults()
withr::defer(ospsuite.plots::resetDefaults(oldDefaults))
ggplot2::theme_update(legend.title = ggplot2::element_blank())
ggplot2::theme_update(legend.position = c(0.95, 0.05))
ggplot2::theme_update(legend.justification = c("right", "bottom"))

# data to be used ---------------------------------------

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of `DataSet` objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath(
    "ImporterConfiguration.xml"
  ))
)

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet[c(1, 3, 5)])
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
  groups = c(
    "Solid total",
    "Solid distal",
    "Solid proximal",
    "Solid total",
    "Solid distal",
    "Solid proximal"
  )
)

# plot tests --------------

test_that("It creates default plots as expected", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults",
    fig = plotPredictedVsObserved(myCombDat, xyScale = "linear")
  )
})

test_that("It creates default plots as expected without any identity or foldDistance line", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults without lines",
    fig = plotPredictedVsObserved(
      myCombDat,
      comparisonLineVector = NULL,
      xyScale = "linear"
    )
  )
})

test_that("It creates default plots as expected with single fold distance line", {
  set.seed(123)
  # use suppressWarnings instaed of expect_warnings. Main focus is on the plot display
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "default 1 fold dist",
      fig = plotPredictedVsObserved(
        myCombDat,
        comparisonLineVector = unname(
          ospsuite.plots::getFoldDistanceList(
            folds = 2
          )
        )
      )
    )
  )
})

test_that("It creates default plots as expected with multiple fold distance lines", {
  set.seed(123)
  # use suppressWarnings instaed of expect_warnings. Main focus is on the plot display
  suppressWarnings(vdiffr::expect_doppelganger(
    title = "default 3 fold dist",
    fig = plotPredictedVsObserved(
      myCombDat,
      comparisonLineVector = unname(
        ospsuite.plots::getFoldDistanceList(
          folds = c(2, 4, 6)
        )
      )
    )
  ))
})

test_that("It produces expected plot for Aciclovir data", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  simResults <- runSimulations(sim)[[1]]

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

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "Aciclovir",
    fig = plotPredictedVsObserved(
      myDataCombined,
      comparisonLineVector = unname(ospsuite.plots::getFoldDistanceList(
        folds = c(2)
      ))
    )
  )
})

test_that("It throws error when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_error(
    plotPredictedVsObserved(myCombDat),
    messages$plotNoDataAvailable()
  )
})

test_that("Different symbols for data sets within one group", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  simData <- withr::with_tempdir({
    df <- dplyr::tibble(
      IndividualId = c(0, 0, 0),
      `Time [min]` = c(0, 2, 4),
      `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
        0.5,
        4,
        8
      )
    )
    readr::write_csv(df, "SimResults.csv")
    importResultsFromCSV(sim, "SimResults.csv")
  })

  obsData <- DataSet$new(name = "Observed")
  obsData$setValues(
    xValues = c(1, 3, 3.5, 4, 5),
    yValues = c(1.9, 6.1, 7, 8.2, 1)
  )
  obsData$xUnit <- "min"
  obsData$yDimension <- ospDimensions$`Concentration (molar)`

  myDC <- DataCombined$new()
  myDC$addSimulationResults(simData, groups = "myGroup")
  myDC$addDataSets(obsData, groups = "myGroup")

  # Add second obs data
  obsData2 <- DataSet$new(name = "Observed 2")
  obsData2$setValues(
    xValues = c(1, 3, 4, 4.5, 5.5),
    yValues = c(2.9, 5.1, 3, 8.2, 1)
  )
  obsData2$xUnit <- "min"
  obsData2$yDimension <- ospDimensions$`Concentration (molar)`
  myDC$addDataSets(obsData2, groups = "myGroup")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple data sets one group",
    fig = plotPredictedVsObserved(myDC)
  )
})

test_that("LLOQ is plotted", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  simData <- withr::with_tempdir({
    df <- dplyr::tibble(
      IndividualId = c(0, 0, 0),
      `Time [min]` = c(0, 2, 4),
      `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
        0,
        4,
        8
      )
    )
    readr::write_csv(df, "SimResults.csv")
    importResultsFromCSV(sim, "SimResults.csv")
  })

  obsData <- DataSet$new(name = "Observed")
  obsData$setValues(
    xValues = c(1, 3, 3.5, 4, 5),
    yValues = c(1.9, 6.1, 7, 8.2, 1)
  )
  obsData$xUnit <- "min"
  obsData$yDimension <- ospDimensions$`Concentration (molar)`
  obsData$LLOQ <- 3

  myDC <- DataCombined$new()
  myDC$addSimulationResults(simData, groups = "myGroup")
  myDC$addDataSets(obsData, groups = "myGroup")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "lloq vertical",
    fig = plotPredictedVsObserved(myDC)
  )
})

# Test predictedAxis parameter ----
test_that("It swaps axes when predictedAxis is 'x'", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "predicted on x-axis",
    fig = plotPredictedVsObserved(
      myCombDat,
      predictedAxis = "x",
      xyScale = "linear"
    )
  )
})

# 2 y-axis dimensions ----
test_that("Plot throws error with fraction and concentration", {
  manyObsSimDCWithFraction <- readRDS(getTestDataFilePath(
    "manyObsSimDCWithFraction"
  ))

  expect_error(
    plotPredictedVsObserved(manyObsSimDCWithFraction),
    'Data contains too many'
  )
})

# xUnit / yUnit direct parameters ----

test_that("plotPredictedVsObserved converts units when yUnit is provided", {
  # Providing yUnit should override the auto-detected unit %
  expect_no_error(
    resultPlot <- plotPredictedVsObserved(
      myCombDat,
      xyScale = "linear",
      yUnit = ""
    )
  )

  expect_false(
    grepl("%", resultPlot$labels$x, fixed = TRUE),
    info = "default x-axis label should not contain '%'"
  )
})

test_that("plotPredictedVsObserved produces same result as pre-converting with convertUnits", {
  plotDirect <- plotPredictedVsObserved(
    myCombDat,
    xyScale = "linear",
    yUnit = ""
  )
  plotPreConverted <- plotPredictedVsObserved(
    convertUnits(myCombDat, yUnit = ""),
    xyScale = "linear"
  )

  expect_equal(plotDirect$data, plotPreConverted$data)
})
