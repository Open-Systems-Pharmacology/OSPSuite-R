# SimulationBatchOptions

test_that("It has the expected default value", {
  batchOptions <- SimulationBatchOptions$new()
  expect_null(batchOptions$variableParameters)
  expect_null(batchOptions$variableMolecules)
})

test_that("It can set the variable parameters and variable molecules", {
  batchOptions <- SimulationBatchOptions$new()
  batchOptions$variableParameters <- c("P1", "P2")
  expect_identical(batchOptions$variableParameters, c("P1", "P2"))

  batchOptions$variableMolecules <- c("M1", "M2")
  expect_identical(batchOptions$variableMolecules, c("M1", "M2"))
})

test_that("It can set the single parameter and molecule", {
  batchOptions <- SimulationBatchOptions$new()
  batchOptions$variableParameters <- "P1"
  expect_equal(batchOptions$variableParameters, "P1")

  batchOptions$variableMolecules <- "M1"
  expect_equal(batchOptions$variableMolecules, "M1")
})

test_that("It can print simulation batch option", {
  batchOptions <- SimulationBatchOptions$new()
  batchOptions$variableParameters <- c("P1", "P2")
  batchOptions$variableMolecules <- c("M1", "M2")
  expect_error(capture.output(batchOptions$print()), NA)
})
