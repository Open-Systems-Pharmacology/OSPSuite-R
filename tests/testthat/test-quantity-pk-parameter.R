# QuantityPKParameter

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|Caffeine"
addOutputs(outputs, sim)
results <- runSimulations(sim)[[1]]
pkAnalyses <- calculatePKAnalyses(results)

allPKParameters <- pkAnalyses$allPKParametersFor(
  quantityPath = "Organism|VenousBlood|Plasma|Caffeine"
)
pkParameter <- allPKParameters[[1]]

test_that("It can print a quantity PK parameter", {
  expect_snapshot(pkParameter$print())
})

test_that("It can resolve the unit and dimension of a quantity PK-Parameter", {
  for (pkParameter in allPKParameters) {
    expect_false(is.null(pkParameter$unit))
    expect_false(is.null(pkParameter$dimension))
  }
})
