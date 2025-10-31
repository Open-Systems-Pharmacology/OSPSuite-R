# DataColumn

createBaseGrid <- function() {
  DataColumn$new(rSharp::newObjectFromName(
    "OSPSuite.Core.Domain.Data.BaseGrid",
    "baseGrid",
    getDimensionByName(ospDimensions$Time)
  ))
}
createColumn <- function(baseGrid) {
  DataColumn$new(rSharp::newObjectFromName(
    "OSPSuite.Core.Domain.Data.DataColumn",
    "column",
    getDimensionByName(ospDimensions$`Concentration (mass)`),
    baseGrid
  ))
}

test_that("It can print a DataColumn", {
  baseGrid <- createBaseGrid()
  expect_snapshot(print(baseGrid))
})

test_that("it can set and retrieve the dimension of a data column", {
  baseGrid <- createBaseGrid()
  expect_equal(baseGrid$dimension, ospDimensions$Time)
  baseGrid$dimension <- ospDimensions$Amount
  expect_equal(baseGrid$dimension, ospDimensions$Amount)
})

test_that("it can retrieve the name of a data column", {
  baseGrid <- createBaseGrid()
  expect_equal(baseGrid$name, "baseGrid")
})

test_that("it can retrieve the base unit of a data column", {
  baseGrid <- createBaseGrid()
  expect_equal(baseGrid$unit, ospUnits$Time$min)
})

test_that("it can set and retrieve the display unit of a data column", {
  baseGrid <- createBaseGrid()
  baseGrid$dimension <- ospDimensions$Time
  expect_equal(baseGrid$displayUnit, ospUnits$Time$h)
  baseGrid$displayUnit <- ospUnits$Time$`day(s)`
  expect_equal(baseGrid$displayUnit, ospUnits$Time$`day(s)`)
})

test_that("it can set and retrieve single value of the column", {
  baseGrid <- createBaseGrid()
  baseGrid$values <- 1
  expect_equal(baseGrid$values, 1)

  baseGrid2 <- createBaseGrid()
  baseGrid2$values <- c(1)
  expect_equal(baseGrid2$values, 1)
})

test_that("it can set and retrieve the values of the column", {
  baseGrid <- createBaseGrid()
  baseGrid$values <- c(1, 2, 3)
  expect_equal(baseGrid$values, c(1, 2, 3))
})

test_that("it throws an error when setting a unit thast does not exist in the dimension", {
  baseGrid <- createBaseGrid()
  baseGrid$dimension <- ospDimensions$Time
  expect_error(baseGrid$displayUnit <- ospUnits$Amount$mmol)
})
