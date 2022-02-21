context("loadPopulation")

test_that("It can load a valid csv population file", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(populationFileName)
  expect_true(!is.null(population))
})

test_that("It throws an exception when loading an invalid population file", {
  populationFileName <- getTestDataFilePath("junk.csv")
  expect_that(loadPopulation(populationFileName), throws_error())
})


context("splitPopulationFile")
test_that("It can split a valid csv file to split files", {
  populationFileName <- getTestDataFilePath("pop.csv")
  splitFiles <- splitPopulationFile(populationFileName, 3, tempdir(), "SplitFile")
  expect_equal(length(splitFiles), 3)
})


context("populationToDataFrame")
test_that("It can convert a population to data frame", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(populationFileName)
  df <- populationToDataFrame(population)
  expect_equal(nrow(df), 10)
})
