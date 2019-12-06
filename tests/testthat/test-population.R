context("Population")
populationFileName <- getTestDataFilePath("pop_10.csv")
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
  population$setValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  expect_identical(population$getValues(parameterPath), values)
})

test_that("It can add user defined variability using an existing parameter path with unit", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|VenousBlood|Volume [l]"
  expect_true(population$has(parameterPath))
  population$setValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  expect_identical(population$getValues(parameterPath), values)
})

test_that("It can add user defined variability using an existing parameter path without unit", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|VenousBlood|Volume"
  expect_true(population$has(parameterPath))
  population$setValues(parameterPath, values)
  expect_true(population$has(parameterPath))
  expect_identical(population$getValues(parameterPath), values)
})

test_that("It can add user defined variability using an existing parameter", {
  population <- loadPopulation(populationFileName)
  expect_true(population$has(venousBloodVolume))
  population$setValues(venousBloodVolume, values)
  expect_true(population$has(venousBloodVolume))
  expect_identical(population$getValues(venousBloodVolume), values)
})

test_that("It throws an exception when adding values that have the wrong dimension", {
  population <- loadPopulation(populationFileName)
  parameterPath <- "Organism|MyParameter"
  expect_that(population$setValues(parameterPath, c(1:5) * 2.5), throws_error())
})

context("Covariates")
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
  expect_error(population$print(), NA)
})
