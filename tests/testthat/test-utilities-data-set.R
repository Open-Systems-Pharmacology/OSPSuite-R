context("loadDataSetFromPKML")

test_that("It can load a valid observed data file and create a DataSet object", {
  file <- getTestDataFilePath("obs_data.pkml")
  dataSet <- loadDataSetFromPKML(file)

  expect_true(isOfType(dataSet, DataSet))
})
