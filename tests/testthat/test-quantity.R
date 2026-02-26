# Quantity

sim <- loadTestSimulation("S1")

test_that("It can print Quantity", {
  quantity <- getQuantity(
    toPathString(c("Organism", "Liver", "Intracellular", "Volume")),
    sim
  )
  expect_snapshot(quantity$print())
})

test_that("It can retrieve the parent of an entity", {
  quantity <- getQuantity(
    toPathString(c("Organism", "Liver", "Intracellular", "Volume")),
    sim
  )
  parent <- quantity$parentContainer
  expect_false(is.null(parent))
  expect_equal(parent$name, "Intracellular")
  expect_equal(parent$parentContainer$name, "Liver")
  expect_equal(parent$parentContainer$parentContainer$name, "Organism")
})


test_that("It prints the Scientific value of the Quantity", {
  quantity <- getQuantity(
    toPathString(c("Organism", "Liver", "Intracellular", "Volume")),
    sim
  )
  quantity$value <- 0.001
  expect_snapshot(print(quantity))
  quantity$value <- 2
  expect_snapshot(print(quantity))
  quantity$value <- 10000
  expect_snapshot(print(quantity))
  quantity$value <- 10001.1
  expect_snapshot(print(quantity))
})

test_that("It prints the NaN value of the Quantity", {
  quantity <- getQuantity("AADAC|Lipophilicity", sim)
  expect_snapshot(quantity$print())
})

test_that("It can access valueOrigin property", {
  quantity <- getQuantity(
    toPathString(c("Organism", "Liver", "Intracellular", "Volume")),
    sim
  )
  valueOrigin <- quantity$valueOrigin
  expect_true(is.character(valueOrigin) || is.null(valueOrigin))
})
