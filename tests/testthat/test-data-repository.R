context("DataRepository")
obsDataFile <- getTestDataFilePath("obs_data.pkml")
obsData <- loadObservedData(obsDataFile)

test_that("it can return the base grid", {
  expect_true(!is.null(obsData$baseGrid))
})

test_that("it can return the value of the base grid as a numeric array", {
  expect_length(obsData$baseGrid$values, 7)
})

test_that("it returns the same reference to columns once instantiated", {
  col2 <- obsData$columns[[2]]
  expect_identical(obsData$columns[[2]], col2)
})

test_that("it returns the same reference to base grids once instantiated", {
  expect_identical(obsData$columns[[1]], obsData$baseGrid)
})
