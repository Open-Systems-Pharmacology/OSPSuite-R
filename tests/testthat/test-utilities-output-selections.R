sim <- loadTestSimulation("S1")
outputSelections <- sim$outputSelections

context("addOutputs")

test_that("It can add multiple outputs by path", {
  outputSelections$clear()
  quantities <- addOutputs(c("Organism|Liver|Volume", "Organism|ArterialBlood|Plasma|Caffeine"), sim)
  expect_equal(length(quantities), 2)
  expect_equal(length(outputSelections$allOutputs), 2)
})

test_that("It can add single output by path", {
  outputSelections$clear()
  path <- "Organism|ArterialBlood|Plasma|Caffeine"
  quantities <- addOutputs(path, sim)
  expect_equal(length(quantities), 1)
  expect_equal(length(outputSelections$allOutputs), 1)
  expect_equal(outputSelections$allOutputs[[1]]$path, path)
})

test_that("It can add multiple outputs by reference", {
  outputSelections$clear()
  parameter <- getParameter("Organism|Liver|Volume", sim)
  quantity <- getAllQuantitiesMatching("Organism|ArterialBlood|Plasma|Caffeine", sim)[[1]]
  quantities <- addOutputs(c(parameter, quantity), sim)
  expect_equal(length(quantities), 2)
  expect_equal(length(outputSelections$allOutputs), 2)
})

test_that("It can add single output by reference", {
  outputSelections$clear()
  parameter <- getParameter("Organism|Liver|Volume", sim)
  quantities <- addOutputs(parameter, sim)
  expect_equal(length(quantities), 1)
  expect_equal(length(outputSelections$allOutputs), 1)
  expect_equal(quantities[[1]]$path, parameter$path)
})

test_that("It throws an exception if the parameters do not have the expect type", {
  parameter <- getParameter("Organism|Liver|Volume", sim)
  container <- getContainer("Organism|Liver", sim)
  expect_that(addOutputs(sim, parameter), throws_error())
  expect_that(addOutputs(parameter, container), throws_error())
  expect_that(addOutputs(parameter, null), throws_error())
  expect_that(addOutputs(null, sim), throws_error())
})


context("clearOutputs")

test_that("It can clear all outputs of a given simulation", {
  addOutputs(c("Organism|Liver|Volume"), sim)
  expect_gt(length(outputSelections$allOutputs), 0)
  clearOutputs(sim)
  expect_equal(length(outputSelections$allOutputs), 0)
})
