context("DataRepository")
obsDataFile <- getTestDataFilePath("obs_data.pkml")
obsData <- loadObservedData(obsDataFile)

test_that("it can return the base grid", {
  expect_true(!is.null(obsData$baseGrid))
})

test_that("it can return the value of the base grid as a numeric array", {
  expect_length(obsData$baseGrid$values, 7)
})
