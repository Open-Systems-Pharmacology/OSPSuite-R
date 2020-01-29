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
  expect_error(capture.output(sim$print()), NA)
})

test_that("It can retrieve the name of all stationary molecules used in the model", {
  molecules <- sim$allStationaryMoleculeNames()
  expect_equal(molecules, c("CYP3A4", "AADAC", "CYP3A5", "CYP2C9", "CYP1A2", "Caffeine-CYP1A2-MM Metabolite", "OATP1B1", "ABCB1"))
})

test_that("It can retrieve the name of all floating molecule used in the model", {
  molecules <- sim$allFloatingMoleculeNames()
  expect_equal(molecules, c("Caffeine"))
})

test_that("It can retrieve the name of all endogenous stationary molecules used in the model", {
  molecules <- sim$allEndogenousStationaryMoleculeNames()
  expect_equal(molecules, c("CYP3A4", "AADAC", "CYP3A5", "CYP2C9", "CYP1A2", "OATP1B1", "ABCB1"))
})

test_that("It can retrieve the name of all xenobiotic floating molecule used in the model", {
  molecules <- sim$allXenobioticFloatingMoleculeNames()
  expect_equal(molecules, c("Caffeine"))
})
