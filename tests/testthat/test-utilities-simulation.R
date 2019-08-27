
context("loadSimulation")

test_that("It can load a valid pkml simulation file with 'loadFromCache = TRUE' without previously loaded sim", {
  ospsuiteEnv$loadedSimulations <- new.env(parent = emptyenv())

  sim <- loadTestSimulation("S1",  loadFromCache = TRUE)
  expect_true(!is.null(sim))
})

test_that("It can load a valid pkml simulation file with 'loadFromCache = FALSE' without previously loaded sim", {
  ospsuiteEnv$loadedSimulations <- new.env(parent = emptyenv())

  sim <- loadTestSimulation("S1",  loadFromCache = FALSE)
  expect_true(!is.null(sim))
})

test_that("It can load a simulation from cache", {
  ospsuiteEnv$loadedSimulations <- new.env(parent = emptyenv())

  sim1 <- loadTestSimulation("S1")
  sim2 <- loadTestSimulation("S1")

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)

  setParametersValues(parameters = parameter1, values = 0)
  expect_equal(parameter1$value, parameter2$value)
})

test_that("It can load two simulations not from cache", {
  ospsuiteEnv$loadedSimulations <- new.env(parent = emptyenv())

  sim1 <- loadTestSimulation("S1",  loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1",  loadFromCache = FALSE)

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)

  setParametersValues(parameters = parameter1, values = 0)
  expect_false(isTRUE(identical(parameter1$value, parameter2$value)))
})

test_that("Two sims not from cache and third from cache", {
  ospsuiteEnv$loadedSimulations <- new.env(parent = emptyenv())

  sim1 <- loadTestSimulation("S1")
  sim2 <- loadTestSimulation("S1",  loadFromCache = FALSE)
  sim3 <- loadTestSimulation("S1")

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)
  parameter3 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim3)

  setParametersValues(parameters = parameter1, values = 1)

  expect_false(isTRUE(identical(parameter1$value, parameter3$value)))
  expect_equal(parameter2$value, parameter3$value)
})

test_that("It throws an exception if the pkml loaded is not a valid simulation file", {
  simFile <- file.path(dataPath, "molecules.pkml", fsep = .Platform$file.sep)

  expect_that(loadSimulation(simFile), throws_error("Could not load simulation"))
})

sim <- loadTestSimulation("S1")
outputSelections <- sim$settings$outputSelections

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
