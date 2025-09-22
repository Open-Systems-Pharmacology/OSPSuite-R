# Population
populationFileName <- getTestDataFilePath("pop.csv")
simuation <- loadTestSimulation("S1")
venousBloodVolume <- getParameter("Organism|VenousBlood|Volume", simuation)
values <- c(1:10) * 2.5


test_that("It can return the expected number of individual in the population", {
  population <- loadPopulation(populationFileName)
  expect_equal(population$count, 10)
})

test_that("It returns whether variability is defined for a parameter path", {
  population <- loadPopulation(populationFileName)
  expect_true(population$has("Organism|VenousBlood|Volume [l]"))
  expect_true(population$has("Organism|VenousBlood|Volume"))
  expect_true(population$has(venousBloodVolume))
  expect_false(population$has("NOPE"))
})


test_that("It can add user defined variability using a new parameter path", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameter"
  population$setParameterValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  expect_identical(population$getParameterValues(parameterPath), values)
})

test_that("It can add user defined variability using an existing parameter path without unit", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|VenousBlood|Volume"
  expect_true(population$has(parameterPath))
  population$setParameterValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  expect_identical(population$getParameterValues(parameterPath), values)
})

test_that("It can add user defined variability using an existing parameter", {
  population <- loadPopulation(populationFileName)
  expect_true(population$has(venousBloodVolume))
  population$setParameterValues(venousBloodVolume, values)
  expect_true(population$has(venousBloodVolume))
  expect_identical(population$getParameterValues(venousBloodVolume), values)
})

test_that("It can add user defined variability using values with NaN", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameterWithNaN"
  values_with_NAN <- c(1:10) * 2.5
  values_with_NAN[2] <- NaN
  population$setParameterValues(parameterPath, values_with_NAN)
  expect_true(population$has(parameterPath))
  expect_identical(
    population$getParameterValues(parameterPath),
    values_with_NAN
  )
})

test_that("It can add user defined variability using values with NA", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameterWithNA"
  values_with_NA <- c(1:10) * 2.5
  values_with_NA[2] <- NA
  population$setParameterValues(parameterPath, values_with_NA)
  expect_true(population$has(parameterPath))
  expect_identical(population$getParameterValues(parameterPath), values_with_NA)
})

test_that("It can remove user defined variability", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameterWithNA"
  values <- c(1:10) * 2.5
  population$setParameterValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  population$remove(parameterPath)
  expect_false(population$has(parameterPath))
})

test_that("It throws an exception when adding values that have the wrong number of items", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameter"
  expect_error(population$setParameterValues(parameterPath, c(1:5) * 2.5))
})

test_that("It can retrieve all parameter values for an existing individual id", {
  population <- loadPopulation(populationFileName)
  parameterValues <- population$getParameterValuesForIndividual(7)
  expect_gt(length(parameterValues), 0)
  expect_gt(length(parameterValues$paths), 0)
  expect_gt(length(parameterValues$values), 0)
})

test_that("It throws an exception when retrieving all parameter values for an individual id that does not exist", {
  population <- loadPopulation(populationFileName)
  expect_error(
    parameterValues <- population$getParameterValuesForIndividual(666)
  )
})

# Covariates
population <- loadPopulation(populationFileName)

test_that("It can retrieve the covariates names defined in a population", {
  allCovariateNames <- population$allCovariateNames
  expect_equal(length(allCovariateNames), 3)
})

test_that("It can retrieve the covariate values for given individual", {
  gender <- population$getCovariateValues("Gender")[7]
  expect_equal(gender, "2")
})

test_that("It retrieve an empty string for an non existant covariate", {
  covariates <- population$getCovariateValues("Does not exist")
  expect_null(covariates)
})

test_that("It can print population", {
  population <- loadPopulation(populationFileName)
  expect_snapshot(population$print())
})

test_that("It can retrieve all ids define ina population", {
  population <- loadPopulation(populationFileName)
  expect_length(population$allIndividualIds, 10)
})

# exportPopulationToCSV

test_that("It can export valid population to CSV", {
  executeWithTestFile(function(csvFile) {
    population <- loadPopulation(populationFileName)
    exportPopulationToCSV(population, csvFile)
    expect_true(file.exists(csvFile))
  })
})
