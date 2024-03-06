sim <- loadTestSimulation("S1")
outputSchema <- sim$outputSchema

# clearOutputIntervals
test_that("It can clear intervals defined in the schema of the simulation", {
  expect_gt(length(outputSchema$intervals), 0)
  clearOutputIntervals(sim)
  expect_equal(length(outputSchema$intervals), 0)
})

# addOutputInterval

test_that("It can add intervals to the output", {
  clearOutputIntervals(sim)
  int1 <- addOutputInterval(sim, 10, 20, 30, "Int1")
  int2 <- addOutputInterval(sim, 20, 30, 40, "Int2")
  expect_equal(length(outputSchema$intervals), 2)
})

test_that("It uses the property specified to create the interval", {
  clearOutputIntervals(sim)
  int1 <- addOutputInterval(sim, 10, 20, 1)
  expect_gt(length(int1$name), 0)
  expect_equal(int1$startTime$value, 10)
  expect_equal(int1$endTime$value, 20)

  int2 <- addOutputInterval(sim, 20, 30, 40, "Int2")
  expect_equal(int2$name, "Int2")
  expect_equal(int2$resolution$value, 40)
})


test_that("It throws an exception when adding two intervals with the same name", {
  clearOutputIntervals(sim)
  int1 <- addOutputInterval(sim, 10, 20, 1, intervalName = "int")
  expect_error(addOutputInterval(sim, 10, 20, 1, intervalName = "int"))
})


# setOutputInterval

test_that("It can set direct output interval into a simulation", {
  clearOutputIntervals(sim)
  addOutputInterval(sim, 10, 20, 1, intervalName = "Int1")
  addOutputInterval(sim, 10, 20, 1, intervalName = "Int2")
  addOutputInterval(sim, 10, 20, 1, intervalName = "Int3")
  expect_equal(length(outputSchema$intervals), 3)

  setOutputInterval(sim, 1, 2, 3, "NEW")
  expect_equal(length(outputSchema$intervals), 1)
  expect_equal(outputSchema$intervals[[1]]$name, "NEW")
})
