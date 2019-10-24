context("Covariates")
populationFileName <- getTestDataFilePath("pop_10.csv")
population <- loadPopulation(populationFileName)


test_that("It can retrieve the covariates names defined in a population", {
  allCovariateNames <- population$allCovariateNames
  expect_equal(length(allCovariateNames), 3)
})

test_that("It can retrieve the covariate values for given individual", {
  covariates <- population$covariatesAt(5)
  expect_equal(covariates$valueFor("Gender"), "2")
})

test_that("It retrieve an empty string for an non existant covariate", {
  covariates <- population$covariatesAt(5)
  expect_equal(covariates$valueFor("DOES NOT EXIST"), "")
})
