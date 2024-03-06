sim <- loadTestSimulation("S1")
outputSchema <- sim$outputSchema

# OutputSchema

test_that("it can retrieve the list of all intervals", {
  expect_gt(length(outputSchema$intervals), 0)
})

test_that("it can add some local time points", {
  outputSchema$addTimePoints(10)
  expect_identical(outputSchema$timePoints, 10)
  expect_identical(outputSchema$timePoints, c(10))

  outputSchema$addTimePoints(c(20, 30))
  expect_identical(outputSchema$timePoints, c(10, 20, 30))
})

test_that("it does not duplicate entries", {
  outputSchema$addTimePoints(c(10, 20, 20, 30, 30))
  expect_identical(outputSchema$timePoints, c(10, 20, 30))
})

test_that("it can remove interval references", {
  count <- length(outputSchema$intervals)
  int <- outputSchema$intervals[[1]]
  outputSchema$removeInterval(int)
  expect_equal(length(outputSchema$intervals), count - 1)
})

test_that("It can print output schema", {
  expect_error(capture.output(outputSchema$print()), NA)
})

test_that("it correctly returns the end time", {
  # Before adding a time point outside of the time intervals
  expect_equal(outputSchema$endTime, 1440)
  # Add a time point outside of the time intervals
  outputSchema$addTimePoints(1441)
  expect_equal(outputSchema$endTime, 1441)
})

test_that(".setEndSimulationTime adds a new time point outside of the current schema", {
  .setEndSimulationTime(sim, 1442)
  expect_equal(outputSchema$endTime, 1442)
})


test_that(".setEndSimulationTime adds a new time point smaller than the current end time without time points", {
  sim <- loadTestSimulation("S1")
  outputSchema <- sim$outputSchema
  .setEndSimulationTime(sim, 1430)

  expect_equal(outputSchema$intervals[[1]]$startTime$value, 0)
  expect_equal(outputSchema$intervals[[1]]$endTime$value, 120)
  expect_equal(outputSchema$intervals[[2]]$startTime$value, 120)
  expect_equal(outputSchema$intervals[[2]]$endTime$value, 1430)
  expect_equal(outputSchema$timePoints, 1430)

  expect_equal(outputSchema$endTime, 1430)
})

test_that(".setEndSimulationTime adds a new time point smaller than the current end time with time points", {
  sim <- loadTestSimulation("S1")
  outputSchema <- sim$outputSchema
  outputSchema$addTimePoints(c(1, 2, 121))
  .setEndSimulationTime(sim, 119)

  expect_equal(outputSchema$intervals[[1]]$startTime$value, 0)
  expect_equal(outputSchema$intervals[[1]]$endTime$value, 119)
  # Second interval is removed
  expect_length(outputSchema$intervals, 1)
  expect_equal(outputSchema$timePoints, c(1, 2, 119))

  expect_equal(outputSchema$endTime, 119)
})


test_that(".setEndSimulationTime sets the end time when the provided value is smaller than the only defined interval", {
  sim <- loadTestSimulation("S1")
  outputSchema <- sim$outputSchema
  # Remove the first interval
  oldIntervals <- outputSchema$intervals
  outputSchema$removeInterval(oldIntervals[[1]])

  # Try to set end time to 100, which is before the first interval
  .setEndSimulationTime(sim, 100)
  expect_equal(outputSchema$endTime, 100)
})
