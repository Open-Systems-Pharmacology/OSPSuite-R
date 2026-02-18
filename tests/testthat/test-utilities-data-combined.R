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

test_that("calculateResiduals validates scaling parameter", {
  expect_error(
    calculateResiduals(myDC, scaling = "invalid"),
    "Invalid scaling parameter"
  )
})

test_that("NULL passed to the calculateResiduals function is an error", {
  expect_error(calculateResiduals(NULL))
})

test_that("An empty dataCombined object passed to the calculateResiduals function results in an error", {
  expect_error(calculateResiduals(DataCombined$new()))
})

test_that("calculateResiduals returns a data frame", {
  result <- suppressWarnings(calculateResiduals(myDC, scaling = "lin"))
  expect_s3_class(result, "data.frame")
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

  result <- suppressWarnings(calculateResiduals(myDC, scaling = "lin"))
  expect_setequal(
    expected_column_names,
    colnames(result)
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
    suppressWarnings(calculateResiduals(myDC, scaling = "lin")$lloq),
    rep(0.02, 6),
    tolerance = tolerance
  )
})

test_that("calculateResiduals does not return rows for data outside of the simulation time but throws a warning", {
  expect_warning(
    result <- calculateResiduals(myDC, scaling = "lin"),
    'residual value set to NA'
  )
  expect_equal(
    nrow(result),
    6
  )
})

test_that("calculateResiduals returns a correct vector of linear residuals on example data", {
  expect_equal(
    suppressWarnings(calculateResiduals(myDC, scaling = "lin")$residualValues),
    c(0, 0.1000000, -0.0999999, 0.0, -0.1999998, -1),
    tolerance = 1e-5
  )
})

test_that("calculateResiduals returns a correct vector of log residuals on example data", {
  # Should emit a warning about undefined residuals
  expect_warning(
    expect_warning(
      pairedData <- calculateResiduals(myDC, scaling = "log"),
      "NA values found in predicted or observed"
    ),
    "non-positive values found for log scaling"
  )

  # Should have 4 rows instead of 6 (excluding NaN and Inf)
  expect_equal(nrow(pairedData), 4)

  # Calculate expected residuals for the valid points
  expectedResiduals <- c(
    log(2) - log(1.9), # time=1
    log(6) - log(6.1), # time=3
    log(7) - log(7), # time=3.5
    log(8) - log(8.2) # time=4
  )

  expect_equal(
    pairedData$residualValues,
    expectedResiduals,
    tolerance = 1e-5
  )
})

test_that("calculateResiduals supports ratio scale", {
  # Ratio scale: observed / simulated
  expect_warning(
    expect_warning(
      pairedData <- calculateResiduals(myDC, scaling = "ratio"),
      'zero observed values found for ratio scaling'
    ),
    'NA values found in predicted or observed'
  )

  # Should have 6 rows (ratio doesn't produce NaN/Inf for zero values the same way)
  # But division by zero would produce Inf, which is filtered
  # time=0: obs=0, sim=0 -> 0/0 = NaN (filtered)
  # time=5: obs=1, sim=0 -> 1/0 = Inf (filtered)

  expect_equal(nrow(pairedData), 5)

  expectedRatios <- c(
    2 / 1.9, # time=1
    6 / 6.1, # time=3
    7 / 7, # time=3.5
    8 / 8.2, # time=4
    0 / 1 # time=5
  )

  expect_equal(
    pairedData$residualValues,
    expectedRatios,
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

  residuals <- suppressWarnings(calculateResiduals(
    myDC,
    scaling = tlf::Scaling$lin
  ))

  expect_equal(residuals$residualValues, 0, tolerance = 1e-5)
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

  residuals <- suppressWarnings(calculateResiduals(
    myDC,
    scaling = tlf::Scaling$lin
  ))

  expect_equal(nrow(residuals), 0)
})
