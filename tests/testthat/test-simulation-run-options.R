# SimulationRunOptions

runOptions <- SimulationRunOptions$new()
test_that("It has the expected default value", {
  expect_equal(runOptions$numberOfCores, getOSPSuiteSetting("numberOfCores"))
})

test_that("It can set the basic options parameters", {
  runOptions$numberOfCores <- 5
  expect_equal(runOptions$numberOfCores, 5)
})

test_that("It can print simulation run options", {
  expect_snapshot(runOptions$print())
})

test_that("checkForNegativeValues parameter is deprecated", {
  expect_warning(
    SimulationRunOptions$new(checkForNegativeValues = TRUE),
    "deprecated"
  )
})
