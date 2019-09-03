dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
outputSelections <- sim$settings$outputSelections
context("OutputSelections")

test_that("It remove all outputs when outputs are cleared", {
  addOutputs("Organism|Weight", sim)
  expect_gt(length(outputSelections$allOutputs), 0)

  outputSelections$clear()
  expect_equal(length(outputSelections$allOutputs), 0)
})
