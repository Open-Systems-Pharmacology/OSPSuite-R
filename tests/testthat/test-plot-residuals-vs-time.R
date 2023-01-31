# data to be used ---------------------------------------

context("plotResidualsVsTime")


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
    title = "defaults",
    fig = plotResidualsVsTime(myCombDat)
  )
})


test_that("It doesn't work with log scale for Y-axis", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yAxisScale <- tlf::Scaling$log
  expect_error(
    plotResidualsVsTime(myCombDat, myPlotConfiguration),
    messages$logScaleNotAllowed()
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
    title = "custom",
    fig = plotResidualsVsTime(
      myCombDat,
      myPlotConfiguration
    )
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(myPlotConfiguration$xLabel)
  expect_null(myPlotConfiguration$yLabel)
})


# edge cases ------------------------

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotResidualsVsTime(myCombDat)))
  expect_warning(
    plotResidualsVsTime(myCombDat),
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

  expect_null(suppressWarnings(plotResidualsVsTime(myCombDat)))
  expect_warning(
    plotResidualsVsTime(myCombDat),
    messages$residualsCanNotBeComputed()
  )
})

test_that("Different symbols for data sets within one group", {
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
  obsData$yDimension <- ospDimensions$`Concentration (molar)`

  myDC <- DataCombined$new()
  myDC$addSimulationResults(simData, groups = "myGroup")
  myDC$addDataSets(obsData, groups = "myGroup")

  # Add second obs data
  obsData2 <- DataSet$new(name = "Observed 2")
  obsData2$setValues(xValues = c(0, 3, 4, 4.5, 5.5), yValues = c(2.9, 5.1, 3, 8.2, 1))
  obsData2$xUnit <- "min"
  obsData2$yDimension <- ospDimensions$`Concentration (molar)`
  myDC$addDataSets(obsData2, groups = "myGroup")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple data sets one group",
    fig = plotResidualsVsTime(myDC)
  )
})
