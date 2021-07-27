context("DataSet")

obsDataFile <- getTestDataFilePath("obs_data.pkml")
obsData <- loadDataRepositoryFromPKML(obsDataFile)

tolerance <- 0.0001

test_that("it can set the name of the data set", {
  dataSet <- DataSet$new(obsData)
  dataSet$name <- "TOTO"
  expect_equal(dataSet$name, "TOTO")
})

test_that("it can create a new data set from scratch", {
  dataSet <- DataSet$new()
  expect_false(is.null(dataSet))
  expect_identical(dataSet$xValues, numeric(0))
})

test_that("it can create a new data set from an existing repository", {
  dataSet <- DataSet$new(obsData)
  expect_false(is.null(dataSet))
})

test_that("it can return and set the xValues", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
})

test_that("it can return and set the yValues", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues= c(1, 2, 3, 4), yValues = c(10, 20, 30, 40))
  expect_equal(dataSet$yValues, c(10, 20, 30, 40), tolerance)
})

test_that("it can update the unit of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  # this will be in h
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$xUnit <- ospUnits$Time$min
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
})

test_that("it can update the dimension of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  # this will be Time in h
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$xDimension <- ospDimensions$Ampere
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Ampere$A)
})

test_that("it can update the dimension of the yValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  # this will be Time in min
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$yDimension <- ospDimensions$Ampere
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yUnit, ospUnits$Ampere$A)
})

test_that("it can set the xValues if xUnit is different from base unit", {
  dataSet <- DataSet$new()
  dataSet$xUnit <- ospUnits$Time$`week(s)`
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5), tolerance)
  expect_equal(dataSet$xUnit, ospUnits$Time$`week(s)`)
})

test_that("it can set the yValues if yUnit is different from base unit", {
  dataSet <- DataSet$new()
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$yUnit <- "mg/l"
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yUnit, "mg/l")
})

test_that("it can change yErrorType when no yError is defined", {
  dataSet <- DataSet$new()
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$yUnit <- "mg/l"
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)
  expect_equal(dataSet$yUnit, "mg/l")
})

test_that("empty y values", {
  dataSet <- DataSet$new()
  expect_equal(dataSet$yValues, numeric(0))
})

test_that("empty y error", {
  dataSet <- DataSet$new()
  expect_equal(dataSet$yErrorValues, numeric(0))
})

test_that("it can print a data set", {
  dataSet <- DataSet$new()
  # this will be Time in h
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$xDimension <- ospDimensions$Ampere
  dataSet$yUnit <- "mg/l"
  expect_error(capture.output(print(dataSet)), regexp = NA)
})

test_that("it can set another array for xValues and yValues", {
  dataSet <- DataSet$new()
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  dataSet$yErrorType <- DataErrorType$GeometricStdDev
  dataSet$setValues(xValues= c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(dataSet$yValues, c(10, 20, 30, 40, 50), tolerance)

  dataSet$setValues(xValues= c(1, 2, 3), yValues = c(10, 20, 30))
  expect_equal(dataSet$yValues, c(10, 20, 30), tolerance)
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

