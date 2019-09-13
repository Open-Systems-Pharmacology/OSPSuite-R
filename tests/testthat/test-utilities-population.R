context("loadPopulation")

test_that("It can load a valid csv population file", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  expect_true(!is.null(population))
})

test_that("It throws an exception when loading an invalid population file", {
  populationFileName <- getTestDataFilePath("junk.csv")
  expect_that(loadPopulation(populationFileName), throws_error())
})
