context("Quantity")

sim <- loadTestSimulation("S1")

test_that("It can print Quantity", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_error(capture.output(quantity$print()), NA)
})

test_that("It can retrieve the index of an existing unit", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  allUnits <- quantity$allUnits
  unitIndex <- quantity$unitIndexOf("ml")
  expect_gte(unitIndex, match("ml", allUnits)[1])
})
