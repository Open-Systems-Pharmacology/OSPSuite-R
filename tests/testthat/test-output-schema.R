sim <- loadTestSimulation("S1")
outputSchema <- sim$outputSchema

context("OutputSchema")

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

test_that("it does nto duplicate entries", {
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
