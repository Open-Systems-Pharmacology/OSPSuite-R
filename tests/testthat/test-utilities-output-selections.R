sim <- loadTestSimulation("S1")
outputSelections <- sim$outputSelections

# addOutputs

test_that("It can add multiple outputs by path", {
  outputSelections$clear()
  addOutputs(
    c("Organism|Liver|Volume", "Organism|ArterialBlood|Plasma|Caffeine"),
    sim
  )
  expect_equal(length(sim$outputSelections$allOutputs), 2)
})

test_that("It can add single output by path", {
  outputSelections$clear()
  path <- "Organism|ArterialBlood|Plasma|Caffeine"
  addOutputs(path, sim)
  expect_equal(length(sim$outputSelections$allOutputs), 1)
  expect_equal(sim$outputSelections$allOutputs[[1]]$path, path)
})

test_that("It can add multiple outputs by reference", {
  outputSelections$clear()
  parameter <- getParameter("Organism|Liver|Volume", sim)
  quantity <- getAllQuantitiesMatching(
    "Organism|ArterialBlood|Plasma|Caffeine",
    sim
  )[[1]]
  addOutputs(c(parameter, quantity), sim)
  expect_equal(length(sim$outputSelections$allOutputs), 2)
})

test_that("It can add single output by reference", {
  outputSelections$clear()
  parameter <- getParameter("Organism|Liver|Volume", sim)
  addOutputs(parameter, sim)
  expect_equal(length(sim$outputSelections$allOutputs), 1)
  expect_equal(sim$outputSelections$allOutputs[[1]]$path, parameter$path)
})

test_that("It throws an exception if the parameters do not have the expect type", {
  parameter <- getParameter("Organism|Liver|Volume", sim)
  container <- getContainer("Organism|Liver", sim)
  expect_error(addOutputs(sim, parameter))
  expect_error(addOutputs(parameter, container))
  expect_error(addOutputs(parameter, null))
  expect_error(addOutputs(null, sim))
})

test_that("It throws an error for invalid path when stopIfNotFound is TRUE (default)", {
  invalidPath <- "InvalidPath|DoesNotExist"
  expect_error(
    addOutputs(invalidPath, sim),
    'no entity exists for path "InvalidPath|DoesNotExist" located under container <S1>!'
  )
})

test_that("It silently ignores invalid paths when stopIfNotFound is FALSE", {
  invalidPath <- "InvalidPath|DoesNotExist"
  oldLength <- length(sim$outputSelections$allOutputs)
  expect_no_error(addOutputs(invalidPath, sim, stopIfNotFound = FALSE))
  expect_equal(length(sim$outputSelections$allOutputs), oldLength)
})

test_that("It throws error for invalid path and does not add any outputs when stopIfNotFound is TRUE", {
  validPath <- "Organism|Liver|Volume"
  invalidPath <- "InvalidPath|DoesNotExist"
  oldLength <- length(sim$outputSelections$allOutputs)
  paths <- c(validPath, invalidPath)
  expect_error(
    addOutputs(paths, sim),
    'no entity exists for path "InvalidPath|DoesNotExist" located under container <S1>!'
  )
  # Verify that no outputs were added before the error
  expect_equal(length(sim$outputSelections$allOutputs), oldLength)
})

test_that("It adds valid paths and silently skips invalid paths when stopIfNotFound is FALSE", {
  outputSelections$clear()
  validPath <- "Organism|Liver|Volume"
  invalidPath <- "InvalidPath|DoesNotExist"
  oldLength <- length(sim$outputSelections$allOutputs)
  paths <- c(validPath, invalidPath)
  expect_no_error(addOutputs(paths, sim, stopIfNotFound = FALSE))
  expect_equal(length(sim$outputSelections$allOutputs), oldLength + 1)
})


# clearOutputs

test_that("It can clear all outputs of a given simulation", {
  addOutputs(c("Organism|Liver|Volume"), sim)
  expect_gt(length(outputSelections$allOutputs), 0)
  clearOutputs(sim)
  expect_equal(length(outputSelections$allOutputs), 0)
})

# setOutputs

test_that("It can set outputs of a given simulation", {
  addOutputs(
    c("Organism|Liver|Volume", "Organism|ArterialBlood|Plasma|Caffeine"),
    sim
  )
  setOutputs(c("Organism|Liver|Volume"), sim)
  expect_equal(length(outputSelections$allOutputs), 1)
  expect_equal(
    sim$outputSelections$allOutputs[[1]]$path,
    "Organism|Liver|Volume"
  )
})

test_that("setOutputs throws an error for invalid path when stopIfNotFound is TRUE (default)", {
  invalidPath <- "InvalidPath|DoesNotExist"
  expect_error(
    setOutputs(invalidPath, sim),
    'no entity exists for path "InvalidPath|DoesNotExist" located under container <S1>!'
  )
})

test_that("setOutputs clears existing outputs and silently ignores invalid paths when stopIfNotFound is FALSE", {
  oldLength <- length(sim$outputSelections$allOutputs)
  addOutputs("Organism|Pancreas|Volume", sim)
  expect_equal(length(outputSelections$allOutputs), oldLength + 1)
  invalidPath <- "InvalidPath|DoesNotExist"
  expect_no_error(setOutputs(invalidPath, sim, stopIfNotFound = FALSE))
  # All outputs should be cleared and no new ones added
  expect_equal(length(sim$outputSelections$allOutputs), 0)
})
