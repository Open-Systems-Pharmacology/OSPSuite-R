sim <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = FALSE,
  addToCache = FALSE
)

outputSelections <- sim$outputSelections
# OutputSelections

test_that("It can print the output selections", {
  expect_snapshot(outputSelections$print())
})

test_that("It remove all outputs when outputs are cleared", {
  addOutputs("Organism|Weight", sim)
  expect_gt(length(outputSelections$allOutputs), 0)

  outputSelections$clear()
  expect_equal(length(outputSelections$allOutputs), 0)
})
