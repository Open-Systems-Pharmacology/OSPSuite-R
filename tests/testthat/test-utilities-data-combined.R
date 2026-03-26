tolerance <- 0.0001

# load the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0, 0),
    `Time [min]` = c(0, 2, 4, 5),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8,
      0
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})
obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(0, 1, 3, 3.5, 4, 5, 6),
  yValues = c(0, 1.9, 6.1, 7, 8.2, 1, 0)
)
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`
obsData$LLOQ <- 0.02
myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

test_that("NULL passed to the calculateResiduals function is an error", {
  expect_error(calculateResiduals(NULL))
})

test_that("An empty dataCombined object passed to the calculateResiduals function results in an error", {
  expect_error(calculateResiduals(DataCombined$new()))
})

test_that("calculateResiduals returns a data frame", {
  expect_s3_class(calculateResiduals(myDC, scaling = "lin"), "data.frame")
})

test_that("calculateResiduals returns expected columns", {
  expected_column_names <- c(
    "group",
    "name",
    "nameSimulated",
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
    expected_column_names,
    colnames(calculateResiduals(myDC, scaling = "lin"))
  )
})

test_that("DataCombined objects keep LLOQ data passed from underlying DataSet objects", {
  expect_identical(
    myDC$toDataFrame()$lloq,
    c(rep(as.numeric(NA), nrow(df)), rep(0.02, length(obsData$yValues))),
    tolerance = tolerance
  )
})

test_that("calculateResiduals function keeps passes lloq data through", {
  expect_equal(
    calculateResiduals(myDC, scaling = "lin")$lloq,
    rep(0.02, 6),
    tolerance = tolerance
  )
})

test_that("calculateResiduals does not return rows for data outside of the simulation time", {
  expect_equal(
    nrow(calculateResiduals(myDC, scaling = "lin")),
    6
  )
})

test_that("calculateResiduals returns a correct vector of linear residuals on example data", {
  expect_equal(
    as.vector(calculateResiduals(myDC, scaling = "lin")$residualValues),
    c(0, 0.1000000, -0.0999999, 0.0, -0.1999998, -1),
    tolerance = 1e-5
  )
})

test_that("calculateResiduals returns a correct vector of log residuals on example data", {
  expect_warning(
    pairedData <-
      calculateResiduals(myDC, scaling = "log"),
    regexp = "NaN"
  )

  # Only rows where both observed and simulated are strictly positive remain.
  # t=0 (sim=0, obs=0) and t=5 (sim=0, obs=1) are excluded.
  expect_equal(nrow(pairedData), 4)

  # For strictly positive values, log() and logSafe() agree.
  expectedResiduals <- log(pairedData$yValuesSimulated) -
    log(pairedData$yValuesObserved)

  expect_equal(
    as.vector(pairedData$residualValues),
    expectedResiduals,
    tolerance = 1e-5
  )
})

test_that("calculateResiduals computes all observed, simulated data pairs correctly", {
  myDC$addSimulationResults(
    simData,
    groups = "myGroup",
    names = "secondSimResults"
  )
  residuals <- calculateResiduals(myDC, scaling = "lin")

  expect_setequal(
    residuals$nameSimulated,
    c(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "secondSimResults"
    )
  )

  expect_equal(
    residuals$residualValues,
    rep(c(0, 0.1000000, -0.0999999, 0.0, -0.1999998, -1), 2),
    tolerance = 1e-5
  )
})

test_that("calculateResiduals handles single-point observed and simulated
          datasets correctly", {
  myDataSet <- DataSet$new("myDataSet")
  myDataSet$setValues(1, 1)

  simDataSet <- DataSet$new("simDataSet")
  simDataSet$setValues(1, 1)

  myDC <- DataCombined$new()
  myDC$addDataSets(c(myDataSet, simDataSet), groups = "myGroup")
  myDC$setDataTypes(
    c("myDataSet", "simDataSet"),
    dataTypes = c("observed", "simulated")
  )

  residuals <- calculateResiduals(myDC, scaling = tlf::Scaling$lin)

  expect_equal(as.vector(residuals$residualValues), 0, tolerance = 1e-5)
})

test_that("calculateResiduals drops points when simulated dataset contains a
          single point with non-matching x-value", {
  myDataSet <- DataSet$new("myDataSet")
  myDataSet$setValues(1, 1)

  simDataSet <- DataSet$new("simDataSet")
  simDataSet$setValues(2, 2)

  myDC <- DataCombined$new()
  myDC$addDataSets(c(myDataSet, simDataSet), groups = "myGroup")
  myDC$setDataTypes(
    c("myDataSet", "simDataSet"),
    dataTypes = c("observed", "simulated")
  )

  residuals <- calculateResiduals(myDC, scaling = 'lin')

  expect_equal(nrow(residuals), 0)
})

# addResidualColumn ----------------

test_that("addResidualColumn adds linear residuals correctly", {
  paired <- data.frame(
    yValuesObserved = c(1, 2, 4),
    yValuesSimulated = c(1.1, 1.9, 3.8)
  )
  result <- addResidualColumn(paired, scaling = "linear")
  expect_equal(
    as.vector(result$residualValues),
    c(0.1, -0.1, -0.2),
    tolerance = 1e-6
  )
})

test_that("addResidualColumn adds log residuals correctly for positive values", {
  paired <- data.frame(
    yValuesObserved = c(1, 2, 4),
    yValuesSimulated = c(1.1, 1.9, 3.8)
  )
  result <- addResidualColumn(paired, scaling = "log")
  expected <- log(c(1.1, 1.9, 3.8)) - log(c(1, 2, 4))
  expect_equal(as.vector(result$residualValues), expected, tolerance = 1e-6)
})

test_that("addResidualColumn sets NaN and warns for log of zero values", {
  paired <- data.frame(
    yValuesObserved = c(0, 1, 2),
    yValuesSimulated = c(1, 0, 2)
  )
  expect_warning(
    result <- addResidualColumn(paired, scaling = "log"),
    regexp = messages$residualsLogNonPositive(2),
    fixed = TRUE
  )
  expect_true(is.nan(result$residualValues[1]))
  expect_true(is.nan(result$residualValues[2]))
  expect_equal(result$residualValues[3], log(2) - log(2), tolerance = 1e-6)
})

test_that("addResidualColumn sets NaN and warns for ratio with non-positive predicted values", {
  paired <- data.frame(
    yValuesObserved = c(3, 1, 2),
    yValuesSimulated = c(1, 0, -1)
  )
  expect_warning(
    result <- addResidualColumn(paired, scaling = "ratio"),
    regexp = messages$residualsRatioPredNonPositive(2),
    fixed = TRUE
  )
  expect_true(is.nan(result$residualValues[2]))
  expect_true(is.nan(result$residualValues[3]))
})


test_that("addResidualColumn adds ratio residuals correctly", {
  paired <- data.frame(
    yValuesObserved = c(2, 4, 8),
    yValuesSimulated = c(1, 2, 4)
  )
  result <- addResidualColumn(paired, scaling = "ratio")
  expect_equal(as.vector(result$residualValues), c(2, 2, 2), tolerance = 1e-6)
})

test_that("addResidualColumn supports 'lin' alias for linear scaling", {
  paired <- data.frame(
    yValuesObserved = c(1, 2),
    yValuesSimulated = c(2, 4)
  )
  result <- addResidualColumn(paired, scaling = "lin")
  expect_equal(as.vector(result$residualValues), c(1, 2))
})

test_that("addResidualColumn errors when observed column is missing", {
  paired <- data.frame(yValuesSimulated = c(1, 2))
  expect_error(
    addResidualColumn(paired, observed = "yValuesObserved", scaling = "log"),
    regexp = "Column 'yValuesObserved' not found"
  )
})

test_that("addResidualColumn errors when predicted column is missing", {
  paired <- data.frame(yValuesObserved = c(1, 2))
  expect_error(
    addResidualColumn(paired, predicted = "yValuesSimulated", scaling = "log"),
    regexp = "Column 'yValuesSimulated' not found"
  )
})

test_that("addResidualColumn respects custom column names", {
  paired <- data.frame(obs = c(1, 2), pred = c(1.1, 1.9))
  result <- addResidualColumn(
    paired,
    observed = "obs",
    predicted = "pred",
    residuals = "res",
    scaling = "linear"
  )
  expect_true("res" %in% names(result))
  expect_equal(as.vector(result$res), c(0.1, -0.1), tolerance = 1e-6)
})
