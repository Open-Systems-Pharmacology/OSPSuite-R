context("SimulationPKAnalyses")

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|Caffeine"
addOutputs(outputs, sim)
results <- runSimulation(sim)
pkAnalyses <- calculatePKAnalyses(results)

test_that("It returns the path of all quantities for which PK-Analyses were calculated", {
  allQuantityPaths <- pkAnalyses$allQuantityPaths
  expect_gt(length(allQuantityPaths), 0)
})

test_that("It returns the name of all pk parameters for which PK-Analyses were calculated", {
  allNames <- pkAnalyses$allPKParameterNames
  expect_gt(length(allNames), 0)
})

test_that("It can print the pk Analysis", {
  expect_error(capture.output(print(pkAnalyses)), NA)
})
