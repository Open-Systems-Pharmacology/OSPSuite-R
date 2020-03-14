context("QuantityPKParameter")

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|Caffeine"
addOutputs(outputs, sim)
results <- runSimulation(sim)
pkAnalyses <- calculatePKAnalyses(results)

pkParameter <- pkAnalyses$allPKParametersFor(quantityPath = "Organism|VenousBlood|Plasma|Caffeine")[[1]]

test_that("It can print PK-parameter", {
  expect_error(capture.output(pkParameter$print()), NA)
})
