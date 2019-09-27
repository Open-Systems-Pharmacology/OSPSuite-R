context("SimulationRunOptions")

runOptions <- SimulationRunOptions$new()
test_that("It has the expected default value", {
  expect_true(runOptions$checkForNegativeValues)
  expect_equal(runOptions$numberOfCoresToUse, 1)
})

test_that("It can set the basic options parameters", {
  runOptions$checkForNegativeValues <- FALSE
  expect_false(runOptions$checkForNegativeValues)

  runOptions$numberOfCoresToUse <- 5
  expect_equal(runOptions$numberOfCoresToUse, 5)
})
