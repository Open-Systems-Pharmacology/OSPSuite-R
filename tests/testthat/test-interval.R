# Interval

sim <- loadTestSimulation("S1")
interval <- sim$outputSchema$intervals[[1]]

test_that("It can print an interval", {
  expect_error(capture.output(interval$print()), NA)
})
