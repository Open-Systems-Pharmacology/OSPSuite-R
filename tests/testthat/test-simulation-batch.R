context("SimulationBatch")
sim <- loadTestSimulation("simple", loadFromCache = TRUE)


test_that("It can run a simulation batch by varying some parameters and molecules", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  res <- simulationBatch$run(parameterValues = c(1.2, 2.4), initialValues = 2.5)
  expect_false(is.null(res))
})

test_that("It can run a simulation batch by varying some parameters only", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  res <- simulationBatch$run(parameterValues = c(1.2, 2.4))
  expect_false(is.null(res))
})

test_that("It can run a simulation batch by varying some molecule only", {
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  res <- simulationBatch$run(initialValues = 1.2)
  expect_false(is.null(res))
})

test_that("It throws an error when the number of values does not match the initialization count for initial values", {
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  expect_that(simulationBatch$run(initialValues = c(1, 2.0)), throws_error())
})

test_that("It throws an error when the number of values does not match the initialization count for parameters", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  expect_that(simulationBatch$run(parameterValues = c(1, 2.0, 3.2)), throws_error())
})
