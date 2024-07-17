# Quantity

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


test_that("It prints the Scientific value of the Quantity", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  quantity$value <- 0.001
  expect_equal(capture.output(print(quantity))[3], "   Value: 1.00e-03 [l] ")
  quantity$value <- 2
  expect_equal(capture.output(print(quantity))[3], "   Value: 2.00 [l] ")
  quantity$value <- 10000
  expect_equal(capture.output(print(quantity))[3], "   Value: 1.00e+04 [l] ")
  quantity$value <- 10001.1
  expect_equal(capture.output(print(quantity))[3], "   Value: 1.00e+04 [l] ")
})

test_that("It prints the NaN value of the Quantity", {
  quantity <- getQuantity("AADAC|Lipophilicity", sim)
  expect_error(capture.output(quantity$print()), NA)
})
