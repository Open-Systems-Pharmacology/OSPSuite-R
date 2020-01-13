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


context("splitPopulationFile")
test_that("It can split a valid csv file to split files", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  splitFiles <- splitPopulationFile(populationFileName, 3, tempdir(), "SplitFile")
  expect_equal(length(splitFiles), 3)
})
