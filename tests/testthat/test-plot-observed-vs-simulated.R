# data to be used ---------------------------------------

context("plotObservedVsSimulated")
skip_if(getRversion() < "4.1")

oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))
manyObsSimDC <- readRDS(getTestDataFilePath("manyObsSimDC"))

# one dataset per group -----------------------------------

test_that("It creates default plots as expected with one dataset per group", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "one per group",
    fig = plotObservedVsSimulated(oneObsSimDC)
  )
})

# multiple datasets per group -----------------------------------

test_that("It creates default plots as expected with multiple datasets per group", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "many per group - defaults",
    fig = plotObservedVsSimulated(manyObsSimDC)
  )
})


test_that("many per group - defaults", {
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
    title = "many per group - custom",
    fig = plotObservedVsSimulated(
      manyObsSimDC,
      myPlotConfiguration,
      foldDistance = c(1.5, 2)
    )
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(myPlotConfiguration$xLabel)
  expect_null(myPlotConfiguration$yLabel)
})


# edge cases ------------------------

test_that("It issues warning when scale is linear", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$xAxisScale <- tlf::Scaling$lin
  myPlotConfiguration$yAxisScale <- tlf::Scaling$lin

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "linear scale",
    fig = plotObservedVsSimulated(oneObsSimDC, myPlotConfiguration)
  )
})

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotObservedVsSimulated(myCombDat)))
  expect_warning(
    plotObservedVsSimulated(myCombDat),
    messages$plottingWithEmptyDataCombined()
  )
})

test_that("It doesn't extrapolate past maximum simulated time point", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  simData <- withr::with_tempdir({
    df <- dplyr::tibble(
      IndividualId = c(0, 0, 0),
      `Time [min]` = c(0, 2, 4),
      `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(0, 4, 8)
    )
    readr::write_csv(df, "SimResults.csv")
    importResultsFromCSV(sim, "SimResults.csv")
  })

  obsData <- DataSet$new(name = "Observed")
  obsData$setValues(xValues = c(1, 3, 3.5, 4, 5), yValues = c(1.9, 6.1, 7, 8.2, 1))
  obsData$xUnit <- "min"

  myDC <- DataCombined$new()
  myDC$addSimulationResults(simData, groups = "myGroup")
  myDC$addDataSets(obsData, groups = "myGroup")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "NA past max sim time",
    fig = plotObservedVsSimulated(myDC)
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
    messages$residualsCanNotBeComputed()
  )
})
