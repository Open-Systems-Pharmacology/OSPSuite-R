# Simulation

quantityPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"
sim <- loadTestSimulation("S1")

test_that("It can retrieve the file source of the simulation", {
  sourceFile <- sim$sourceFile
  expect_equal(sourceFile, getSimulationFilePath("S1"))
})

test_that("It throws an error when trying to set file source", {
  expect_error(sim$sourceFile <- "TOTO")
})

test_that("It can print the simulation", {
  expect_snapshot(sim$print())
})

test_that("It can retrieve the name of all stationary molecules used in the model", {
  molecules <- sim$allStationaryMoleculeNames()
  expect_equal(
    molecules,
    c(
      "CYP3A4",
      "AADAC",
      "CYP3A5",
      "CYP2C9",
      "CYP1A2",
      "Caffeine-CYP1A2-MM Metabolite",
      "OATP1B1",
      "ABCB1"
    )
  )
})

test_that("It can retrieve the name of all floating molecule used in the model", {
  molecules <- sim$allFloatingMoleculeNames()
  expect_equal(molecules, c("Caffeine"))
})

test_that("It can retrieve the name of all endogenous stationary molecules used in the model", {
  molecules <- sim$allEndogenousStationaryMoleculeNames()
  expect_equal(
    molecules,
    c("CYP3A4", "AADAC", "CYP3A5", "CYP2C9", "CYP1A2", "OATP1B1", "ABCB1")
  )
})

test_that("It can retrieve the name of all xenobiotic floating molecule used in the model", {
  molecules <- sim$allXenobioticFloatingMoleculeNames()
  expect_equal(molecules, c("Caffeine"))
})

test_that("It can retrieve the mol weight of a valid quantity path", {
  molWeight <- sim$molWeightFor(quantityPath)
  expect_equal(molWeight, 1.942e-07)
})

test_that("It returns NA if the path is not valid for mol weight", {
  molWeight <- sim$molWeightFor("A|B|c")
  expect_true(is.na(molWeight))
})

test_that("It returns the applications defined for the simulation", {
  applications <- sim$allApplicationsFor(quantityPath)
  expect_gt(length(applications), 0)
})

test_that("It can set a new name to the simulation", {
  newName <- "NewName"
  sim$name <- newName
  expect_equal(sim$name, newName)
})

test_that("It throws an error when trying to set a new name with illegal characters", {
  newName <- "NewName|"
  expect_error(sim$name <- newName, messages$illegalCharactersInName(newName))
})

# It returns a simulation configuration
test_that("It returns a simulation configuration", {
  sim <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
  config <- sim$configuration
  expect_true(isOfType(config, "SimulationConfiguration"))
})

test_that("It throws an error when the simulation was created with an earlier version of OSPS", {
  sim <- loadTestSimulation("S1")
  expect_error(sim$configuration, regexp = messages$errorFeatureNotSupportedBySimulation("SimulationConfiguration", 9, 17),
               fixed = TRUE)
})

# It throws an error when trying to set a new configuration
test_that("It throws an error when trying to set a new configuration", {
  expect_error(sim$configuration <- "anything", messages$errorPropertyReadOnly("configuration"),
               fixed = TRUE)
})
