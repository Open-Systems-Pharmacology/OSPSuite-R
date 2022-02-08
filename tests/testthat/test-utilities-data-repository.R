context(".loadDataRepositoryFromPKML")

test_that("It can load a valid observed data file", {
  file <- getExtDataFilePath("obs_data.pkml")
  obsData <- .loadDataRepositoryFromPKML(file)
  expect_true(!is.null(obsData))
})
