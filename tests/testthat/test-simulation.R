context("Simulation")

sim <- loadTestSimulation("S1")

test_that("It can retrieve the file source of the simulation", {
  sourceFile <- sim$sourceFile
  expect_equal(sourceFile, getSimulationFilePath("S1"))
})

test_that("It throws an error when trying to set file source", {
  expect_that(sim$sourceFile <- "TOTO", throws_error())
})
