context("loadDataRepositoryFromPKML")

test_that("It can load a valid observed data file", {
  file <- getTestDataFilePath("obs_data.pkml")
  obsData <- loadDataRepositoryFromPKML(file)
  expect_true(!is.null(obsData))
})
