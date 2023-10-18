# TableFormula

simple <- loadTestSimulation("simple")
tableParameter <- getParameter("Organism|TableParameter", simple)
tableFormula <- tableParameter$formula

test_that("It can retrieve all points defined in the table formula", {
  points <- tableFormula$allPoints
  expect_equal(length(points), 4)
})

test_that("It can retrieve the xDimension of the formula", {
  expect_equal(tableFormula$dimension, "Inversed time")
  expect_equal(tableFormula$xDimension, "Time")
})

test_that("It can set and retrieve the use derivative flag", {
  tableFormula$useDerivedValues <- FALSE
  expect_false(tableFormula$useDerivedValues)

  tableFormula$useDerivedValues <- TRUE
  expect_true(tableFormula$useDerivedValues)
})

test_that("It can add a point to the table at the correct index", {
  tableFormula$addPoints(20, 30)
  points <- tableFormula$allPoints
  expect_equal(length(points), 5)
  expect_equal(points[[3]]$x, 20)
})

test_that("It can remove a point from the table defined with existing x and y", {
  tableFormula$removePoint(20, 30)
  points <- tableFormula$allPoints
  expect_equal(length(points), 4)
})

test_that("It throws an exception when trying to add a point at an existing x with a different y", {
  tableFormula$addPoints(20, 30)
  expect_error(tableFormula$add(20, 40))
})

test_that("It can update the restart solver flag of a given point", {
  point <- tableFormula$allPoints[[2]]
  point$restartSolver <- TRUE
  expect_true(point$restartSolver)
  point$restartSolver <- FALSE
  expect_false(point$restartSolver)
})

test_that("It can clear all points from the table", {
  tableFormula$clearPoints()
  points <- tableFormula$allPoints
  expect_equal(length(points), 0)
})

test_that("It can add multiple points at once", {
  tableFormula$clearPoints()
  tableFormula$addPoints(c(0.1, 0.2), c(20, 30))
  expect_equal(length(tableFormula$allPoints), 2)
})

test_that("It can retrieves the values for existing points or interpolate for missing points", {
  tableFormula$setPoints(c(0.1, 0.3), c(10, 30))
  expect_equal(tableFormula$valueAt(0.1), 10)
  expect_equal(tableFormula$valueAt(0.2), 20)
  expect_equal(tableFormula$valueAt(0.4), 30)
})


test_that("It can update all points at once in a table", {
  tableFormula$clearPoints()
  tableFormula$addPoints(10, 20)
  tableFormula$addPoints(20, 30)
  expect_equal(length(tableFormula$allPoints), 2)
  tableFormula$setPoints(c(0.1, 0.2, 0.3), c(10, 20, 30))
  points <- tableFormula$allPoints
  expect_equal(length(points), 3)
  expect_equal(points[[3]]$x, 0.3)
  expect_equal(points[[3]]$y, 30)
})

test_that("It can print the table", {
  expect_error(capture.output(tableFormula$print()), NA)
})
