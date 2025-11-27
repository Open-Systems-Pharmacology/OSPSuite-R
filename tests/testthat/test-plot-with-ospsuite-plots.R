# Test for .getMappingForTimeprofiles function
test_that(".getMappingForTimeprofiles constructs mapping correctly", {

  # Mock plot data
  mockPlotData <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    name = LETTERS[seq(1,5)],
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l"
  )

  mockPlotDataNoGroup <- copy(mockPlotData)[, group := NA]

  # Mock metadata
  mockMetaData <- list(
    xValues = list(dimension = "Time", unit = "h"),
    yValues = list(dimension = "Concentration", unit = "mg/l")
  )

  # Test without user mapping for data with group
  mapping <- .getMappingForTimeprofiles(mockPlotData, mockMetaData, userMapping = NULL)
  expect_true("uneval" %in% class(mapping))
  expect_equal(rlang::as_label(mapping$x), 'xValues')
  expect_equal(rlang::as_label(mapping$y), 'yValues')
  expect_equal(rlang::as_label(mapping$groupby), "group")
  expect_equal(rlang::as_label(mapping$group), "interaction(group, name)")

  # Test without user mapping for data without group
  mapping <- .getMappingForTimeprofiles(mockPlotDataNoGroup, mockMetaData, userMapping = NULL)
  expect_equal(rlang::as_label(mapping$groupby), "name")
  expect_false('group' %in% names(mapping))

  # Test with user mapping for data with group
  userMapping <- ggplot2::aes(color = group, groupby = dataType)
  mappingWithUser <- .getMappingForTimeprofiles(mockPlotData, mockMetaData, userMapping = userMapping)
  expect_equal(rlang::as_label(mappingWithUser$colour), 'group')
  expect_equal(rlang::as_label(mappingWithUser$groupby), "dataType")
  expect_false('group' %in% names(mappingWithUser))

  # Test with yErrorType present
  mockPlotDataError <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l",
    yErrorType = rep("ArithmeticStdDev", 5),
    yErrorValues = c(1, 2, 1.5, 2.5, 3)
  )

  mappingWithError <- .getMappingForTimeprofiles(mockPlotDataError, mockMetaData, userMapping = NULL)
  expect_true("error" %in% names(mappingWithError))

  # Test with yMin and yMax present
  mockPlotDataMinMax <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l",
    yMin = c(8, 18, 12, 22, 28),
    yMax = c(12, 22, 18, 28, 32)
  )

  mappingMinMax <- .getMappingForTimeprofiles(mockPlotDataMinMax, mockMetaData, userMapping = NULL)
  expect_true("ymin" %in% names(mappingMinMax))
  expect_true("ymax" %in% names(mappingMinMax))

  # Mock plot data with a secondary y-axis (y2)
  mockPlotDataWithY2 <- data.table(
    xValues = c(1, 2, 3, 1, 2),
    yValues = c(10, 20, 15, 0.1, 0.2),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = c(rep("mg/l",3), rep("ml",2))
  )

  # Mock metadata indicating presence of secondary y-axis
  mockMetaDataWithY2 <- list(
    xValues = list(dimension = "Time", unit = "h"),
    yValues = list(dimension = "Concentration", unit = "mg/l"),
    y2 = list(dimension = "Volume", unit = "ml")  # Secondary y-axis metadata
  )

  # Test mapping with y2
  mappingWithY2 <- .getMappingForTimeprofiles(mockPlotDataWithY2, mockMetaDataWithY2, userMapping = NULL)
  expect_contains(names(mappingWithY2), 'y2axis')

})

# getMostFrequentUnit ----

# Sample data for testing
sampleData <- data.table(
  group = c("A", "B", "B"),
  name = c("Sample1",  "Sample1", "Sample2"),
  yUnit = c("mg",  "g", "g"),
  xUnit = c("h",  "min", "min"),
  dataType = c("observed",  "simulated", "simulated")
)

test_that("getMostFrequentUnit returns the most frequent observed unit", {
  result <- .getMostFrequentUnit(sampleData, "yUnit")
  expect_equal(result, "mg")  # Expected to return "mg" as it's the most frequent observed unit
})

test_that("getMostFrequentUnit returns the most frequent simulated unit when no observed units are present", {
  dataNoObserved <- data.table(
    group = c("A", "B"),
    name = c("Sample1", "Sample2"),
    yUnit = c("g", "g"),
    xUnit = c("min", "min"),
    dataType = c("simulated", "simulated")
  )

  result <- .getMostFrequentUnit(dataNoObserved, "xUnit")
  expect_equal(result, "min")  # Expected to return "min" as the only available unit
})

test_that("getMostFrequentUnit handles all NA values", {
  naData <- data.table::data.table(
    group = c("A", "B"),
    name = c("Sample1", "Sample2"),
    yUnit = c(NA_character_, NA_character_),
    xUnit = c("h", "h"),
    dataType = c("observed", "observed")
  )

  expect_no_error(result <- .getMostFrequentUnit(naData, "yUnit"))
  expect_true(is.na(result))
})

test_that(".getMostFrequentUnit prioritizes observed when frequencies are tied", {
  mixedData <- data.table(
    group = c("A", "B", "C", "D"),
    name = c("Obs1", "Obs2", "Sim1", "Sim2"),
    yUnit = c("mg", "mg", "g", "g"),
    xUnit = c("h", "h", "h", "h"),
    dataType = c("observed", "observed", "simulated", "simulated")
  )

  result <- .getMostFrequentUnit(mixedData, "yUnit")

  expect_equal(result, "mg")
})

# .convertInconsistentErrorTypes ----------

test_that("Function handles missing yErrorType gracefully", {
  plotData <- data.table(yValues = c(1, 2, 3), yErrorValues = c(0.1, 0.2, 0.3))
  result <- .convertInconsistentErrorTypes(plotData)
  expect_equal(result, plotData)  # Should return the original data.table
})


test_that("Function calculates yMin and yMax correctly", {
  plotData <- data.table(yValues = c(10, 20, 10, 20) ,
                         yErrorValues = c(2, 4, 2, 4),
                         yErrorType = c(DataErrorType$GeometricStdDev,
                                        DataErrorType$GeometricStdDev,
                                        DataErrorType$ArithmeticStdDev,
                                        DataErrorType$ArithmeticStdDev))
  result <- .convertInconsistentErrorTypes(plotData)
  expect_equal(result$yMin, c(5, 5, 8 , 16))
  expect_equal(result$yMax, c(20, 80, 12, 24))
  expect_true(all(is.na(result$yErrorValues)))  # Should be set to NA
  expect_true(all(is.na(result$yErrorType)))  # Should be set to NA
})

# .convertUnitsForPlot ----------------

test_that("Function handles empty data correctly", {
  emptyData <- data.frame()
  result <- .convertUnitsForPlot(emptyData, 2)
  expect_equal(result, emptyData)
})

test_that("Function checks for data.frame input", {
  expect_error(.convertUnitsForPlot(matrix(1:10, nrow = 5), 2),'is of type')
})


test_that("Function checks maxAllowedYDimensions is an integer", {
  validData <- data.frame(yDimension = c("Concentration (mass)", "Concentration (molar)"),
                           xUnit = c("mg/L", "mol/L"),
                           yUnit = c("mg/L", "mol/L"),
                           value = c(10, 0.1))

  expect_error(.convertUnitsForPlot(validData, "two"),
               "is of type")
})

test_that("Function merges dimensions correctly", {
  validData <- data.frame(yDimension = c("Concentration (mass)", "Concentration (molar)"),
                          xUnit = c("h", "min"),
                          xDimension = 'Time',
                          yUnit = c("mg/l", "mol/l"),
                          group = c('group','group'),
                          name = c('A','B'),
                          dataType = c('observed','simulated'),
                          xValues = c(1, 1),
                          yValues = c(10, 0.1),
                          molWeight = 2
                          )

  .convertUnitsForPlot(validData, 2)
  expect_equal(nrow(result), 2)
  expect_true(all(result$yDimension %in% "Concentration (mass)"))
  expect_true(all(result$yUnit %in% "mg/l"))
})

test_that("Function raises error for too many Y dimensions", {
  validData <- data.frame(yDimension = c("Concentration (mass)", "Fraction"),
                          xUnit = c("h", "min"),
                          xDimension = 'Time',
                          yUnit = c("mg/l", ""),
                          group = c('group','group'),
                          name = c('A','B'),
                          dataType = c('observed','simulated'),
                          xValues = c(1, 1),
                          yValues = c(10, 0.1),
                          molWeight = 2
  )

  expect_error(.convertUnitsForPlot(validData, 1),
               substr(messages$plotToManyYDimension(validData$yDimension),1,10))
})

# .calculateResidualsForPlot ----------------

test_that(".calculateResidualsForPlot handles unpaired data", {
  unpairedData <- data.table::data.table(
    xValues = c(1, 2, 3, 4),
    yValues = c(10, 20, 30, 40),
    group = c("A", "B", "C", "D"),
    name = c("Obs1", "Obs2", "Sim1", "Sim2"),
    nameSimulated = c("Sim1", "Sim2", "Sim3", "Sim4"),
    dataType = c("observed", "observed", "simulated", "simulated"),
    yUnit = rep("mg/l", 4),
    xUnit = rep("h", 4),
    yDimension = rep("Concentration (mass)", 4),
    xDimension = rep("Time", 4),
    molWeight = rep(100, 4)
  )

  expect_warning(
    result <- .calculateResidualsForPlot(unpairedData, scaling = "log"),
    regexp = "residuals"
  )
  expect_true(is.null(result))
})
