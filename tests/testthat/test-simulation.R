context("Simulation")

sim <- loadTestSimulation("S1")

test_that("It can retrieve the file source of the simulation", {
  sourceFile <- sim$sourceFile
  expect_equal(sourceFile, getSimulationFilePath("S1"))
})

test_that("It throws an error when trying to set file source", {
  expect_that(sim$sourceFile <- "TOTO", throws_error())
})

test_that("It can print the simulation", {
  expect_error(sim$print(), NA)
})


test_that("It can retrieve the name of all proteins used in the model", {
  proteins <- sim$allProteinNames()
  expect_equal(proteins, c("CYP3A4", "AADAC", "CYP3A5", "CYP2C9", "CYP1A2", "OATP1B1", "ABCB1"))
})

