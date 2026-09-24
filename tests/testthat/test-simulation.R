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
    regexp = messages$illegalCharactersInName(newName),
    fixed = TRUE
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

# isPopulation / population fields
test_that("A freshly loaded simulation is an individual simulation", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  expect_false(sim$isPopulation)
  expect_null(sim$population)
})

test_that("isPopulation is read-only", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  expect_error(
    (sim$isPopulation <- TRUE),
    regexp = "Property 'isPopulation' is read-only and cannot be modified.",
    fixed = TRUE
  )
})

test_that("Assigning a population turns the simulation into a population simulation", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  population <- loadPopulation(
    system.file("extdata", "pop.csv", package = "ospsuite")
  )

  sim$population <- population

  expect_true(sim$isPopulation)
  expect_true(isOfType(sim$population, "Population"))
  expect_equal(sim$population$count, population$count)
})

test_that("Assigning NULL switches a population simulation back to individual", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  population <- loadPopulation(
    system.file("extdata", "pop.csv", package = "ospsuite")
  )

  sim$population <- population
  expect_true(sim$isPopulation)

  sim$population <- NULL
  expect_false(sim$isPopulation)
  expect_null(sim$population)
})

test_that("Assigning a population validates the input type", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  expect_error(
    (sim$population <- "not a population"),
    regexp = "is of type <.*>, but expected <Population>"
  )
})

test_that("Removing the population also clears any aging data", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  population <- loadPopulation(
    system.file("extdata", "pop.csv", package = "ospsuite")
  )
  sim$population <- population
  # attach aging data on the underlying object
  sim$set("AgingData", AgingData$new())
  expect_false(is.null(sim$get("AgingData")))

  # switching back to an individual simulation clears the aging data too
  sim$population <- NULL
  expect_false(sim$isPopulation)
  expect_true(is.null(sim$get("AgingData")))
})

test_that("Re-assigning a population clears stale aging data", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  population <- loadPopulation(
    system.file("extdata", "pop.csv", package = "ospsuite")
  )
  sim$set("AgingData", AgingData$new())
  expect_false(is.null(sim$get("AgingData")))

  sim$population <- population
  expect_true(is.null(sim$get("AgingData")))
})
