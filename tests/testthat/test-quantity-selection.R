# QuantitySelection$print

sim <- loadTestSimulation("simple", loadFromCache = TRUE)

test_that("It can print Quantity Selection", {
  quantitySelection <- sim$outputSelections$allOutputs[[1]]
  expect_snapshot(quantitySelection$print())
})
