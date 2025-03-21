# QuantitySelection$print

sim <- loadTestSimulation("S1")

test_that("It can print Quantity Selection", {
  quantitySelection <- sim$outputSelections$allOutputs[[1]]
  expect_snapshot(quantitySelection$print())
})
