context("dataSetToDataFrame")

dataSet <- DataSet$new()

test_that("It can convert an empty data set", {
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = character(0), xValue = logical(0), yValue = logical(0), yErrorValues = logical(0),
      xDimension = character(0), xUnit = character(0), yDimension = character(0),
      yUnit = character(0), yErrorType = character(0), yErrorUnit = character(0), yMolWeight = logical(0)
    )
  )
})

test_that("It can convert a data set with xValues and yValues set by setValues, other fields are default", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep("", 5), xValue = dataSet$xValues, yValue = dataSet$yValues, yErrorValues = rep(NA, 5),
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(NA_character_, 5), yErrorUnit = rep(NA_character_, 5), yMolWeight = rep(NA, 5)
    )
  )
})

test_that("It can convert a data set with only non-empty fields, except for metaData", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$name <- "Data1"
  dataSet$molWeight <- 123
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep(dataSet$name, 5), xValue = dataSet$xValues,
      yValue = dataSet$yValues, yErrorValues = dataSet$yErrorValues,
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(dataSet$yErrorType, 5), yErrorUnit = rep(dataSet$yErrorUnit, 5),
      yMolWeight = rep(dataSet$molWeight, 5)
    )
  )
})

test_that("It can convert a list of data sets", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$name <- "Data1"
  dataSet$molWeight <- 123
  dataSet2 <- DataSet$new()
  dataSet2$setValues(xValues = c(6, 7, 8), yValues = c(11, 21, 31))
  dataSet2$molWeight <- 456

  expect_equal(
    dataSetToDataFrame(list(dataSet, dataSet2)),
    data.frame(
      name = c(rep(dataSet$name, 5), rep("", 3)), xValue = c(dataSet$xValues, dataSet2$xValues),
      yValue = c(dataSet$yValues, dataSet2$yValues), yErrorValues = c(dataSet$yErrorValues, rep(NA, 3)),
      xDimension = c(rep(dataSet$xDimension, 5), rep(dataSet2$xDimension, 3)),
      xUnit = c(rep(dataSet$xUnit, 5), rep(dataSet2$xUnit, 3)),
      yDimension = c(rep(dataSet$yDimension, 5), rep(dataSet2$yDimension, 3)),
      yUnit = c(rep(dataSet$yUnit, 5), rep(dataSet2$yUnit, 3)),
      yErrorType = c(rep(dataSet$yErrorType, 5), rep(NA_character_, 3)),
      yErrorUnit = c(rep(dataSet$yErrorUnit, 5), rep(NA_character_, 3)),
      yMolWeight = c(rep(dataSet$molWeight, 5), rep(dataSet2$molWeight, 3))
    )
  )
})

test_that("It can convert a single data set with meta data", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  dataSet$name <- "Data1"
  dataSet$molWeight <- 123
  dataSet$addMetaData("City", "Rome")

  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep(dataSet$name, 5), xValue = dataSet$xValues, yValue = dataSet$yValues, yErrorValues = rep(NA, 5),
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(NA_character_, 5), yErrorUnit = rep(NA_character_, 5), yMolWeight = rep(123, 5),
      City = rep("Rome", 5)
    )
  )
})

test_that("It can convert a list of data sets with meta data", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$name <- "Data1"
  dataSet$molWeight <- 123
  dataSet$addMetaData("City", "Rome")
  dataSet2 <- DataSet$new()
  dataSet2$setValues(xValues = c(6, 7, 8), yValues = c(11, 21, 31))
  dataSet2$molWeight <- 456
  dataSet2$addMetaData("City", "Berlin")
  dataSet2$addMetaData("ZIP", "10245")
  dataSet2$addMetaData("Country", "Germany")

  expect_equal(
    dataSetToDataFrame(list(dataSet, dataSet2)),
    data.frame(
      name = c(rep(dataSet$name, 5), rep("", 3)), xValue = c(dataSet$xValues, dataSet2$xValues),
      yValue = c(dataSet$yValues, dataSet2$yValues), yErrorValues = c(dataSet$yErrorValues, rep(NA, 3)),
      xDimension = c(rep(dataSet$xDimension, 5), rep(dataSet2$xDimension, 3)),
      xUnit = c(rep(dataSet$xUnit, 5), rep(dataSet2$xUnit, 3)),
      yDimension = c(rep(dataSet$yDimension, 5), rep(dataSet2$yDimension, 3)),
      yUnit = c(rep(dataSet$yUnit, 5), rep(dataSet2$yUnit, 3)),
      yErrorType = c(rep(dataSet$yErrorType, 5), rep(NA_character_, 3)),
      yErrorUnit = c(rep(dataSet$yErrorUnit, 5), rep(NA_character_, 3)),
      yMolWeight = c(rep(dataSet$molWeight, 5), rep(dataSet2$molWeight, 3)),
      City = c(rep("Rome", 5), rep("Berlin", 3)),
      ZIP = c(rep(NA_character_, 5), rep("10245", 3)),
      Country = c(rep(NA_character_, 5), rep("Germany", 3))
    )
  )
})
