# SensitivityAnalysisRunOptions

test_that("it can create a sensitivity analysis run options by passing some null values", {
  options <- SensitivityAnalysisRunOptions$new(numberOfCores = NULL, showProgress = FALSE)
  expect_false(options$showProgress)
  expect_false(is.null(options$numberOfCores))
})

test_that("It can print", {
  options <- SensitivityAnalysisRunOptions$new(numberOfCores = NULL, showProgress = FALSE)
  options$numberOfCores <- 5
  expect_snapshot(options$print())
})
