
dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)

simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
outputSelections <- sim$settings$outputSelections
context("loadSimulation")


test_that("It can load a valid pkml simulation file", {
  expect_true(!is.null(sim))
})

test_that("It throws an exception if the pkml loaded is not a valid simulation file", {
  simFile <- file.path(dataPath, "molecules.pkml", fsep = .Platform$file.sep)
  expect_that(loadSimulation(simFile), throws_error("Could not load simulation"))
})

context("saveSimulation")

test_that("It can save a valid simulation to file", {
  exportFile <- tempfile()
  saveSimulation(sim, exportFile)
  expect_true(file.exists(exportFile))
  file.remove(exportFile)
})


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
  expect_equal(quantities[[1]], parameter)
})
