context("Quantity")

sim <- loadTestSimulation("S1")

test_that("It can print Quantity", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_error(capture.output(quantity$print()), NA)
})

test_that("It can retrieve the parent of an entity", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  parent <- quantity$parentContainer
  expect_false(is.null(parent))
  expect_equal(parent$name, "Intracellular")
  expect_equal(parent$parentContainer$name, "Liver")
  expect_equal(parent$parentContainer$parentContainer$name, "Organism")
})
