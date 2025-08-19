simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
configuration <- simulation$configuration

testMoBiProject <- loadMoBiProject(filePath = getTestDataFilePath("Test_Project.mbp3"))

test_that("Snapshot test for printing SimulationConfiguration", {
  expect_snapshot(configuration$print())
})

# Test for individual
test_that("SimulationConfiguration can get and set individual", {
  individual <- configuration$individual
  # Expecting NULL because this simulation did not have an individual defined
  expect_null(individual)

  # Now we set a default individual
  individual <- testMoBiProject$getIndividual("DefaultIndividual")
  configuration$individual <- individual
  expect_true(isOfType(configuration$individual, "BuildingBlock"))
  expect_equal(configuration$individual$name, "DefaultIndividual")

  # Set NULL as individual
  configuration$individual <- NULL
  expect_null(configuration$individual)
})

# Error when trying to set a wrong BB
test_that("SimulationConfiguration individual throws an error when wrong BB type is provided for individual", {
  bb <- testMoBiProject$getExpressionProfiles("CYP3A4|Human|Healthy")[[1]]

  expect_error(configuration$individual <- bb,
               regexp = "Building Block with the name 'CYP3A4|Human|Healthy' is of type 'Expression Profile', but expected type is 'Individual'",
               fixed = TRUE)
})

# Test for Expression Profiles

# it can set and get one exp profile
# it can set and get multiple exp profiles
# it throws an error when trying to set a profile for the same enzyme multiple times
