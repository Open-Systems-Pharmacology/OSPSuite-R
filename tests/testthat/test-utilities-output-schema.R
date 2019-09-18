sim <- loadTestSimulation("S1")
outputSchema <- sim$outputSchema

context("clearIntervals")
test_that("It can clear intervals defined in the schema of the simulation", {
  expect_gt(length(outputSchema$intervals), 0)
  clearIntervals(sim)
  expect_equal(length(outputSchema$intervals), 0)
})

context("addOutputInterval")

test_that("It can add intervals to the output", {
  clearIntervals(sim)
  int1<- addOutputInterval(sim, 10, 20, 30, "Int1");
  int2<- addOutputInterval(sim, 20, 30, 40, "Int2");
  expect_equal(length(outputSchema$intervals), 2)
})

test_that("It uses the property specified to create the interval", {
  clearIntervals(sim)
  int1<- addOutputInterval(sim, 10, 20, 1);
  expect_gt(length(int1$name), 0)
  expect_equal(int1$startTime$value, 10)
  expect_equal(int1$endTime$value, 20)

  int2<- addOutputInterval(sim, 20, 30, 40, "Int2");
  expect_equal(int2$name, "Int2")
  expect_equal(int2$resolution$value, 40)
})


test_that("It throws an exception when adding two intervals with the same name", {
  clearIntervals(sim)
  int1<- addOutputInterval(sim, 10, 20, 1, intervalName =  "int");
  expect_that(addOutputInterval(sim, 10, 20, 1, intervalName =  "int"), throws_error())
})

