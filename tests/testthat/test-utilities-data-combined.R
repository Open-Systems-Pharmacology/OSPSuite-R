tolerance <- 0.0001

# load the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0, 0),
    `Time [min]` = c(0, 2, 4, 5),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(0, 4, 8, 0)
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})
obsData <- DataSet$new(name = "Observed")
obsData$setValues(xValues = c(0, 1, 3, 3.5, 4, 5, 6), yValues = c(0, 1.9, 6.1, 7, 8.2, 1, 0))
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`
obsData$LLOQ <- 0.02
myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

test_that("NULL passed to the calculateResiduals function is an error", {
  expect_error(calculateResiduals(NULL))
})

test_that(
  "An empty dataCombined object passed to the calculateResiduals function results in an error",
  {
    expect_error(calculateResiduals(DataCombined$new()))
  }
)

test_that("calculateResiduals returns a dataframe", {
  expect_true(
    "data.frame" %in% class(calculateResiduals(myDC, scaling = "lin"))
  )
})



test_that("calculateResiduals returns expected columns", {
  expected_column_names <- c(
    "group",
    "name",
    "xValues",
    "xUnit",
    "xDimension",
    "yValuesObserved",
    "yUnit",
    "yDimension",
    "yErrorValues",
    "yErrorType",
    "yErrorUnit",
    "lloq",
    "yValuesSimulated",
    "residualValues"
  )

  expect_setequal(
    expected_column_names, colnames(calculateResiduals(myDC, scaling = "lin"))
  )
})

test_that(
  "DataCombined objects keep LLOQ data passed from underlying DataSet objects",
  {
    expect_identical(
      myDC$toDataFrame()$lloq,
      c(rep(as.numeric(NA), nrow(df)), rep(0.02, length(obsData$yValues))),
      tolerance = tolerance
    )
  }
)

test_that(
  "calculateResiduals function keeps passes lloq data through",
  {
    expect_equal(
      calculateResiduals(myDC, scaling = "lin")$lloq,
      rep(0.02, 6),
      tolerance = tolerance
    )
  }
)

test_that(
  "calculateResiduals does not return rows for data outside of the simulation time",
  {
    expect_equal(
      nrow(calculateResiduals(myDC, scaling = "lin")),
      6
    )
  }
)

test_that(
  "calculateResiduals returns a correct vector of linear residuals on example data",
  {
    expect_equal(calculateResiduals(myDC, scaling = "lin")$residualValues,
      c(0, 0.1000000, -0.0999999, 0.0, -0.1999998, -1),
      tolerance = 1e-5
    )
  }
)

test_that(
  "calculateResiduals returns a correct vector of log residuals on example data",
  {
    pairedData <- calculateResiduals(myDC, scaling = "log")
    expectedResiduals <- sapply(seq_along(pairedData$yValuesObserved), function(idx) {
      ospsuite.utils::logSafe(pairedData$yValuesSimulated[[idx]]) -
        ospsuite.utils::logSafe(pairedData$yValuesObserved[[idx]])
    })

    expect_equal(calculateResiduals(myDC, scaling = "log")$residualValues,
      expectedResiduals,
      tolerance = 1e-5
    )
  }
)
