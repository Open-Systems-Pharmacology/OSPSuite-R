context("QuantitySelection$print")

sim <- loadTestSimulation("S1")

test_that("It can print Quantity", {
  quantitySelection <- sim$settings$outputSelections$allOutputs[[1]]
  expect_error(capture.output(quantitySelection$print()), NA)
})
