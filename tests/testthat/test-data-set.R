tolerance <- 0.0001

context("DataSet from scratch")

test_that("it can create a new data set from scratch", {
  dataSet <- DataSet$new()
  expect_identical(dataSet$xValues, numeric(0))
  expect_identical(dataSet$yValues, numeric(0))
  expect_identical(dataSet$yErrorValues, NULL)
  expect_identical(dataSet$metaData, list())
  expect_identical(dataSet$name, "")
  expect_identical(dataSet$xDimension, ospDimensions$Time)
  expect_identical(dataSet$xUnit, ospUnits$Time$h)
  expect_identical(dataSet$yDimension, ospDimensions$`Concentration (mass)`)
  expect_identical(dataSet$yUnit, "mg/l")
  expect_identical(dataSet$yErrorType, NULL)
  expect_identical(dataSet$yErrorUnit, NULL)
  expect_identical(dataSet$yErrorValues, NULL)
  expect_identical(dataSet$lloq, NULL)
  expect_identical(dataSet$yValues, numeric())
  expect_error(capture.output(print(dataSet)), regexp = NA)
})

test_that("it can set the name of the data set", {
  dataSet <- DataSet$new()
  dataSet$name <- "TOTO"
  expect_equal(dataSet$name, "TOTO")
})

test_that("it can update the dimension of the xValues when no values are set", {
  dataSet <- DataSet$new()
  dataSet$xDimension <- ospDimensions$Ampere
  expect_equal(dataSet$xValues, numeric(), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Ampere$A)
})

test_that("it can update the dimension of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  # this will be Time in h
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$xDimension <- ospDimensions$Ampere
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Ampere$A)
})

test_that("it can update the unit of the xValues when no values are set", {
  dataSet <- DataSet$new()
  dataSet$xUnit <- ospUnits$Time$`week(s)`
  expect_equal(dataSet$xValues, numeric(), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Time$`week(s)`)
})

test_that("it can update the unit of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$xUnit <- ospUnits$Time$`week(s)`
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Time$`week(s)`)
})

test_that("it can update the dimension of the yValues when no values are set", {
  dataSet <- DataSet$new()
  dataSet$yDimension <- ospDimensions$Ampere
  expect_equal(dataSet$yValues, numeric(), tolerance)
  expect_equal(dataSet$yUnit, ospUnits$Ampere$A)
})

test_that("it can update the dimension of the yValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yDimension <- ospDimensions$Ampere
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yErrorValues, c(0, 1, 2, 3, 0), tolerance)
  expect_equal(dataSet$yUnit, ospUnits$Ampere$A)
  expect_equal(dataSet$yErrorUnit, ospUnits$Ampere$A)
})

test_that("it does not change the display unit when updating the dimension with the currently set one", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yUnit <- ospUnits$`Concentration (mass)`[[2]]
  expect_equal(dataSet$yUnit, ospUnits$`Concentration (mass)`[[2]])
  dataSet$yDimension <- ospDimensions$`Concentration (mass)`
  expect_equal(dataSet$yUnit, ospUnits$`Concentration (mass)`[[2]])
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yErrorValues, c(0, 1, 2, 3, 0), tolerance)

  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[1]])
})

test_that("it can update the unit of the yValues when no values are set", {
  dataSet <- DataSet$new()
  dataSet$yUnit <- ospUnits$`Concentration (mass)`[[2]]
  expect_equal(dataSet$yValues, numeric(), tolerance)
  expect_equal(dataSet$yUnit, ospUnits$`Concentration (mass)`[[2]])
})

test_that("it can update the unit of the yValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yUnit <- ospUnits$`Concentration (mass)`[[2]]
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yUnit, ospUnits$`Concentration (mass)`[[2]])
  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[1]])
})

test_that("Empty error with defined y values", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(dataSet$yErrorValues, NULL)
})

test_that("it does not crash when setting yErrorType without error values", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  expect_equal(dataSet$yErrorType, NULL)
})

test_that("it does not change the unit of yError when setting to the currently set error type", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yErrorUnit <- ospUnits$`Concentration (mass)`[[2]]
  dataSet$yErrorType <- DataErrorType$ArithmeticStdDev
  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[2]])
})

test_that("arithmetic to geometric error changes the dimension of yError", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  expect_equal(dataSet$yErrorType, DataErrorType$GeometricStdDev)
  expect_equal(dataSet$yErrorValues, c(0, 1, 2, 3, 0), tolerance)
  expect_equal(dataSet$yErrorUnit, "")
})

test_that("geometric to arithmetic error sets the dimension and unit of yError to those of yValues", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$yUnit <- ospUnits$`Concentration (mass)`[[2]]
  expect_equal(dataSet$yUnit, ospUnits$`Concentration (mass)`[[2]])
  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[1]])
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  dataSet$yErrorType <- DataErrorType$ArithmeticStdDev
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  expect_equal(dataSet$yErrorValues, c(0, 1, 2, 3, 0), tolerance)
  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[2]])
})

test_that("it can update x and y values and remove y error", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$setValues(xValues = c(1, 2, 3), yValues = c(10, 20, 30))

  expect_equal(dataSet$yErrorValues, NULL)
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  expect_equal(dataSet$yErrorValues, c(0, 1, 2, 3, 0), tolerance = tolerance)
})

test_that("it can add a new meta data", {
  dataSet <- DataSet$new()
  dataSet$addMetaData("Meta", "Value")
  expect_equal(dataSet$metaData[["Meta"]], "Value")
})

test_that("it can update existing meta data", {
  dataSet <- DataSet$new()
  dataSet$addMetaData("Meta", "Value")
  dataSet$addMetaData("Meta", "Value2")
  expect_equal(dataSet$metaData[["Meta"]], "Value2")
})

test_that("it can remove existing meta data", {
  dataSet <- DataSet$new()
  dataSet$addMetaData("Meta", "Value")
  dataSet$removeMetaData("Meta")
  expect_null(dataSet$metaData[["Meta"]])
})

test_that("it does not crash when removing a meta data that does not exist", {
  dataSet <- DataSet$new()
  dataSet$removeMetaData("Meta")
  expect_null(dataSet$metaData[["Meta"]])
})

test_that("empty molWeight", {
  dataSet <- DataSet$new()
  expect_equal(dataSet$molWeight, NULL)
})

test_that("it can get and set molWeight", {
  dataSet <- DataSet$new()
  dataSet$molWeight <- 123
  expect_equal(dataSet$molWeight, 123)
})

context("DataSet from pkml without error")
obsDataFile <- getTestDataFilePath("obs_data_no_error.pkml")
obsData <- .loadDataRepositoryFromPKML(obsDataFile)

test_that("it can create a new data set from an existing repository", {
  dataSet <- DataSet$new(obsData)
  expect_false(is.null(dataSet))
})

context("DataSet from pkml with error")
obsDataFile <- getTestDataFilePath("obs_data.pkml")

xValues <- c(
  1.79999995231628, 4.86999988555908, 10.1999998092651,
  30, 60, 120, 240
)
yValues <- c(
  0.00100999997254547, 0.000830000013163357, 0.00073000000488932,
  0.000279999995411728, 0.000119999996051057, 3.0499998360245E-05, 5.9299999806417E-06
)
yError <- c(
  1.15000000278087, 1.08999995518388, 1.12999998691521, 1.21999994462385, 1.4099999816608,
  1.18000002657936, 1.48000003719062
)
metaData <- list(
  Source = "C:\\temp\\RanorexTestData\\ObservedData.xlsx",
  Sheet = "Tabelle1",
  DoubleValue = 5,
  StringValues = "hello",
  IntegerValue = 4
)

test_that("it can create a new data set from an existing repository", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  expect_equal(dataSet$xValues, xValues)
  expect_equal(dataSet$yValues, yValues)
  expect_equal(dataSet$yErrorValues, yError)
  expect_equal(dataSet$metaData, metaData)
  expect_equal(dataSet$name, "ObservedData")
  expect_equal(dataSet$xDimension, ospDimensions$Time)
  expect_equal(dataSet$xUnit, ospUnits$Time$min)
  expect_equal(dataSet$yDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(dataSet$yUnit, "mg/l")
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  expect_equal(dataSet$yErrorUnit, "mg/l")
  expect_equal(dataSet$yErrorValues, yError)
  expect_error(capture.output(print(dataSet)), regexp = NA)
})

test_that("it can set the name of the data set", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$name <- "TOTO"
  expect_equal(dataSet$name, "TOTO")
})

test_that("it can update the dimension of the xValues and this does not change the returned value", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$xDimension <- ospDimensions$Ampere
  expect_equal(dataSet$xValues, xValues, tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Ampere$A)
})

test_that("it can update x and y values and remove y error", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yErrorValues, NULL)
})

test_that("it does not change the unit of yError when setting to the currently set error type", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$yErrorUnit <- ospUnits$`Concentration (mass)`[[2]]
  dataSet$yErrorType <- DataErrorType$ArithmeticStdDev
  expect_equal(dataSet$yErrorUnit, ospUnits$`Concentration (mass)`[[2]])
})

test_that("arithmetic to geometric error changes the dimension of yError", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  expect_equal(dataSet$yErrorType, DataErrorType$GeometricStdDev)
  expect_equal(dataSet$yErrorValues, yError, tolerance)
  expect_equal(dataSet$yErrorUnit, "")
})

test_that("it can add a new meta data", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$addMetaData("Meta", "Value")
  expect_equal(dataSet$metaData[["Meta"]], "Value")
})

test_that("it can set the llog value", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  dataSet$LLOQ <- 0.25
  expect_equal(dataSet$LLOQ, 0.25, tolerance)
})

test_that("it can save the data set as pkml", {
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)
  filePath <- getTestDataFilePath("obs_data_save.pkml")

  dataSet$saveToPKML(filePath = filePath)
  #load the saved file and check everything is correct
  obsData <- .loadDataRepositoryFromPKML(obsDataFile)
  dataSet <- DataSet$new(obsData)

  expect_equal(dataSet$xValues, xValues)
  expect_equal(dataSet$yValues, yValues)
  expect_equal(dataSet$yErrorValues, yError)
  expect_equal(dataSet$metaData, metaData)
  expect_equal(dataSet$name, "ObservedData")
  expect_equal(dataSet$xDimension, ospDimensions$Time)
  expect_equal(dataSet$xUnit, ospUnits$Time$min)
  expect_equal(dataSet$yDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(dataSet$yUnit, "mg/l")
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  expect_equal(dataSet$yErrorUnit, "mg/l")
  expect_equal(dataSet$yErrorValues, yError)
  expect_error(capture.output(print(dataSet)), regexp = NA)

  #remove the temp file
  capture.output(file.remove(filePath))
})
