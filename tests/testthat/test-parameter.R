# Parameter

sim <- loadTestSimulation("S1")
simple <- loadTestSimulation("simple")

liverPathArray <- c("Organism", "Liver")
liverPath <- toPathString(liverPathArray)
volumePath <- toPathString(c(liverPathArray, "Volume"))

volumeParameter <- getParameter(volumePath, sim)
formulaParameter <- getParameter("Organism|Weight", sim)
constantParameter <- getParameter("Organism|Age", sim)
tableParameter <- getParameter("Organism|TableParameter", simple)
rhsParameter <- getParameter("Organism|RHSParameter", simple)

test_that("It can print parameters", {
  expect_snapshot(volumeParameter$print())
  expect_snapshot(formulaParameter$print())
                  expect_snapshot(constantParameter$print())
                                  expect_snapshot(tableParameter$print())
                                                  expect_snapshot(rhsParameter$print())
})

test_that("It can retrieve name of a parameter", {
  par <- getParameter(toPathString(c(liverPathArray, "Blood flow rate")), sim)
  expect_equal(par$name, "Blood flow rate")
})

test_that("It can display whether a parameter is a constant parameter", {
  expect_false(volumeParameter$isConstant)
  expect_false(formulaParameter$isConstant)
  expect_true(constantParameter$isConstant)
  expect_false(tableParameter$isConstant)
})

test_that("It can display whether a parameter is a formula parameter", {
  expect_false(volumeParameter$isFormula)
  expect_true(formulaParameter$isFormula)
  expect_false(constantParameter$isFormula)
  expect_false(tableParameter$isFormula)
})

test_that("It can display whether a parameter is a table parameter", {
  expect_false(volumeParameter$isTable)
  expect_false(formulaParameter$isTable)
  expect_false(constantParameter$isTable)
  expect_true(tableParameter$isTable)
})

test_that("It can display the formula string of formula parameter or null otherwise", {
  expect_null(volumeParameter$formulaString)
  expect_false(is.null(formulaParameter$formulaString))
  expect_null(constantParameter$formulaString)
})

test_that("It can display whether a parameter is a table parameter", {
  expect_false(volumeParameter$isTable)
  expect_false(formulaParameter$isTable)
  expect_false(constantParameter$isTable)
})

test_that("It can override a formula with a value", {
  formulaParameter$value <- 111
  expect_true(formulaParameter$isFixedValue)
  formulaParameter$reset()
  expect_false(formulaParameter$isFixedValue)
})

test_that("It throws an error when trying to set the name of a parameter", {
  expect_error(volumeParameter$name <- "TOTO")
})

test_that("It can retrieve the id of a parameter", {
  expect_false(is.null(volumeParameter$id))
})

test_that("It can retrieve the full path of a parameter", {
  expect_equal(volumeParameter$fullPath, paste("S1", volumePath, sep = "|"))
})

test_that("It can retrieve the path of a parameter", {
  expect_equal(volumeParameter$path, volumePath)
})

test_that("It throws an error when trying to set the path of a parameter", {
  expect_error(volumeParameter$path <- "TOTO")
})

test_that("It throws an error when trying to set the id of a parameter", {
  expect_error(volumeParameter$id <- "id")
})

test_that("It can retrieve a value and update a value of a parameter", {
  val <- volumeParameter$value
  volumeParameter$value <- val * 2
  expect_equal(volumeParameter$value, val * 2)
})

test_that("It can retrieve the dimension of a parameter", {
  expect_equal(volumeParameter$dimension, "Volume")
})

test_that("It can retrieve the unit of a parameter", {
  expect_equal(volumeParameter$unit, "l")
})

test_that("It throws an error when setting the unit of a parameter", {
  expect_error(volumeParameter$unit <- "ml")
})

test_that("It can retrieve the display unit of a parameter", {
  expect_equal(volumeParameter$displayUnit, "l")
})

test_that("It throws an error when setting the display unit of a parameter", {
  expect_error(volumeParameter$displayUnit <- "ml")
})

test_that("It can set a value in another unit and the value will be updated as expected", {
  volumeParameter$setValue(1, "l")
  expect_equal(volumeParameter$value, 1)

  volumeParameter$setValue(10, "ml")
  expect_equal(volumeParameter$value, 0.01)
})

test_that("It can set a value without the unit specified, thus using the default unit", {
  par <- getParameter(volumePath, sim)
  par$setValue(1, "l")
  expect_equal(par$value, 1)

  par$setValue(10)
  expect_equal(par$value, 10)
})

test_that("It throws an exception when setting a value in a unit that does not exists", {
  expect_error(volumeParameter$setValue(1, "kg"))
})

test_that("it can retrieve all units defined for a quantity", {
  par <- getParameter(volumePath, sim)
  expect_identical(par$allUnits, c("l", "ml", .encodeUnit("µl")))
})

test_that("It can retrieve RHS parameter and returns NULL if the parameter has no RHS", {
  par <- getParameter(volumePath, sim)
  expect_null(par$rhsFormula)
})

test_that("isStateVariable returns false if the parameter has no RHS Formula", {
  par <- getParameter(volumePath, sim)
  expect_false(par$isStateVariable)
})

test_that("isStateVariable returns true if the parameter has a RHS Formula ", {
  expect_true(rhsParameter$isStateVariable)
})

test_that("It can retrieve RHS parameter and returns a formula if the parameter has a  RHS", {
  expect_false(is.null(rhsParameter$rhsFormula))
})

test_that("It can set the isStateVariable to false for a parameter without RHS", {
  par <- getParameter(volumePath, sim)
  par$isStateVariable <- FALSE
  expect_false(par$isStateVariable)
  expect_null(par$rhsFormula)
})

test_that("It throws an exception when setting the isStateVariable to true for a parameter that has no state variable", {
  par <- getParameter(volumePath, sim)
  expect_error(par$isStateVariable <- TRUE)
})

test_that("It can set the isStateVariable to true for a parameter with RHS", {
  rhsParameter$isStateVariable <- TRUE
  expect_true(rhsParameter$isStateVariable)
  expect_false(is.null(rhsParameter$rhsFormula))
})

test_that("It can set the isStateVariable to false for a parameter with RHS", {
  rhsParameter$isStateVariable <- FALSE
  expect_false(rhsParameter$isStateVariable)
  expect_null(rhsParameter$rhsFormula)
})
