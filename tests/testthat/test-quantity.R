context("Quantity$print")

sim <- loadTestSimulation("S1")

test_that("It can print Quantity", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_error(quantity$print(), NA)
})
