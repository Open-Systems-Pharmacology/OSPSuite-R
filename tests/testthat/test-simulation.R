# Simulation

# To speed up the tests, we load the Aciclovir simulation from cache and use it for tests that do not modify it. For tests that need to modify the simulation, we load a simple simulation without using cache to ensure we have a clean instance.
quantityPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
sim <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = TRUE,
  addToCache = TRUE
)
mutableSim <- loadTestSimulation(
  "simple",
  loadFromCache = FALSE,
  addToCache = FALSE
)

test_that("It can retrieve the file source of the simulation", {
  sourceFile <- mutableSim$sourceFile
  expect_equal(sourceFile, getSimulationFilePath("simple"))
})

test_that("It throws an error when trying to set file source", {
  expect_error(sim$sourceFile <- "TOTO")
})

test_that("It can print the simulation", {
  expect_snapshot(mutableSim$print())
})

test_that("It can retrieve the name of all stationary molecules used in the model", {
  expect_snapshot(sim$allStationaryMoleculeNames())
})

test_that("It can retrieve the name of all floating molecule used in the model", {
  expect_snapshot(sim$allFloatingMoleculeNames())
})

test_that("It can retrieve the name of all endogenous stationary molecules used in the model", {
  expect_snapshot(sim$allEndogenousStationaryMoleculeNames())
})

test_that("It can retrieve the name of all xenobiotic floating molecule used in the model", {
  expect_snapshot(sim$allXenobioticFloatingMoleculeNames())
})

test_that("It can retrieve the mol weight of a valid quantity path", {
  molWeight <- sim$molWeightFor(quantityPath)
  molWeightParam <- getParameter("Aciclovir|Molecular weight", sim)$value
  expect_equal(molWeight, molWeightParam)
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
  mutableSim$name <- newName
  expect_equal(mutableSim$name, newName)
})

test_that("It throws an error when trying to set a new name with illegal characters", {
  newName <- "NewName|"
  expect_error(
    mutableSim$name <- newName,
    messages$illegalCharactersInName(newName)
  )
})

test_that("It throws an error when trying to change the name of the simulation to a forbidden name", {
  expect_error(
    mutableSim$name <- "MoleculeProperties",
    regexp = messages$forbiddenSimulationName("MoleculeProperties", mutableSim)
  )
  expect_error(
    mutableSim$name <- "Metformin",
    regexp = messages$forbiddenSimulationName("Metformin", mutableSim)
  )
})

# It returns a simulation configuration
test_that("It returns a simulation configuration", {
  sim <- loadSimulation(system.file(
    "extdata",
    "Aciclovir.pkml",
    package = "ospsuite"
  ))
  config <- sim$configuration
  expect_true(isOfType(config, "SimulationConfiguration"))
})

test_that("It throws an error when the simulation was created with an earlier version of OSPS", {
  sim <- loadTestSimulation("simple_v11")
  expect_error(
    sim$configuration,
    regexp = messages$errorFeatureNotSupportedBySimulation(
      "SimulationConfiguration",
      8,
      12
    ),
    fixed = TRUE
  )
})

# It throws an error when trying to set a new configuration
test_that("It throws an error when trying to set a new configuration", {
  sim <- loadSimulation(system.file(
    "extdata",
    "Aciclovir.pkml",
    package = "ospsuite"
  ))
  expect_error(
    (sim$configuration <- "anything"),
    regexp = "Property 'configuration' is read-only and cannot be modified.",
    fixed = TRUE
  )
})
