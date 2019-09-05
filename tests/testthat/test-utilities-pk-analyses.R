
context("calculatePKAnalyses")

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|Caffeine"
addOutputs(outputs, sim)
results <- runSimulation(sim)
pkAnalyses <- calculatePKAnalyses(results, sim)

test_that("It should be able to calculate the PK-Analyses each output of a simulation", {

  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor("Organism|VenousBlood|Plasma|Caffeine")
  expect_gt(length(pkAnalysesForOutput), 0)
})

test_that("It should an empty list of parameters for an output that is not part of the calculated results", {
  clearOutputs(sim)
  addOutputs(outputs, sim)
  results <- runSimulation(sim)
  pkAnalyses <- calculatePKAnalyses(results, sim)

  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor("Another output that does not exist")
  expect_equal(length(pkAnalysesForOutput), 0)
})
